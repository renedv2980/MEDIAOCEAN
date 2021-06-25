*          DATA SET ACCAP08    AT LEVEL 115 AS OF 09/20/20                      
*PHASE T61D08A                                                                  
*&&ONLIN SET   Y                                                                
*&&US                                                                           
*INCLUDE AC1RMNT                                                                
*&&                                                                             
*INCLUDE ACSRCHC                                                                
*INCLUDE SRCHCALL                                                               
*INCLUDE ACSRCHP                                                                
*INCLUDE SRCHPASS                                                               
*INCLUDE ACRAPPER                                                               
                                                                                
         TITLE 'T61D08 - PERSON RECORD MAINT/LIST'                              
                                                                                
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCAP08 -- PERSON MAINTENANCE/LIST                   *         
*                                                                     *         
*  COMMENTS:     MAINTAINS PERSON RECORDS                             *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61D00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS ACCAPF9 (MAINTENANCE)                        *         
*                        ACCAPF8 (LIST)                               *         
*                                                                     *         
*  OUTPUTS:      UPDATED PERSON RECORDS, LIST                         *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
* DCUR LVL 39 - IF OFFPOS IS NOT IN 1,2, OR 3 LEVEL OF 1R DEFAULT TO  *         
*               SHOW ALL                                              *         
* DCUR LVL 43 - PUT IN CLEAR OF STDISP IF > MAX STDISP                *         
*               CHANGE MAX # OF ENTRIES TO 19 TO KEEP TO 2 PAGES      *         
* DCUR LVL 47 - CHECK FOR SAL/HRS BUCKET POSTINGS WHEN DELETING LOC   *         
* DCUR LVL 48 - CHECK FOR DUP LOC AND ALLOW DELETION OF LOCATION IF   *         
*               DUP LOC COVERS '32' BALANCE ELEM, SALARY, TIME AND    *         
*               POSTINGS.  ALSO FIXED BUG WHEN ENTERING TERM DATE AND *         
*               CARRY DOWN TO LOC END DATE. (IN PREVEND ROUTINE)      *         
* DCUR LVL 60 - ADD START DATE OF LOC BEING ENDED/DELETED TO PERSON   *         
*               BLOCK FOR HISTORY MOVE/DEL TO MAKE SURE HAVE RIGHT LOC*         
* DCUR LVL 61 - (1)REMOVED POSSIBLE FALL THROUGH'S IN PREVEND ROUTINE *         
*               WHEN CHECKING FOR TIME.(2) USE TMS LOCK DATE AS END DTE         
*               WHEN CHECKING FOR TIME INSTEAD OF NOT CHECKING AT ALL *         
* DCUR LVL 62 - FIX BUG IN DATEUPD ROUTINE WHEN CHECKING FOR SAL/TIME *         
* DCUR LVL 63 - FIX BUG WHEN STORING PSTART. IF TRANSFERRING LOCS WAS *         
*               NOT STORING THE OLD LOC START DATE.                   *         
* DCUR LVL 69 - MAKE BMONVAL CALL COUNTRY SPECIFIC                    *         
* DCUR LVL 73 - DISALLOW WORKCODE '**' AS THIS IS ODERS ONLY (UK ONLY)*         
* DCUR LVL 75 - INCREASED NUMBER OF LOCATIONS FROM 19 TO 29.          *         
* NSHE LVL 78 - ADD FURTHER VALIDATION TO CHANGING END DATE OF A LOC  *         
* NSHE LVL 79 - CHANGE TO UK STRUCTURE OF PIDRECD                     *         
* NSHE LVL 80 - <1014595> BUG WITH PERSON PID CHECK                   *         
* NSHE LVL 81 - <1005751> BUG WITH PERSON PID PASSIVE                 *         
* NSHE LVL 82 - ADD RECORD ACTIVITY POINTERS FOR LOW LEVEL 1R         *         
*               ENHANCEMENT FOR ACCENT                                *         
* YNGX LVL 83 - <LO01-4829> CHANGE TO UK STRUCTURE OF PIDRECD         *         
* DKEL LVL 84 - <LO01-5598> ENSURE PID IS ALWAYS PRESENT FOR USERS OF *         
*               BRANDOCEAN TIMESHEETS                                 *         
* JFOS LVL 85 - <BR10766L> LOOK FOR ETIME ELS BEFORE LOC DELETE ALLOWD*         
* TKLU LVL 86 - <DU01-4633> FEATURE TO ADD HISTORY AUTOMATICALLY      *         
*               <BUG FIX> PREVENT 'CRAPPER' BY BRANCH CONDITION TEST  *         
* SMAN LVL 87 - <LO01-6081> TIGHTEN UP STATUS, END DATE AND TERM DATE *         
*               VALIDATION (INCL. END DATE BROUGHT UP TO TERM DATE)   *         
* YNGX LVL 88 - <BR10752D> CALL ACSRCHP TO CHANGE NAME SEARCH POINTER *         
* NSHE LVL 89 - <BR14732L> FIX BLOCK USED TO IN CALL RAPPER           *         
* TKLU 11APR08 90   <BR17237L> ENSURE CHECKS ON PID REMOVE/CHANGE     *         
* YNGX 26JUN08 91   <BR12638D> GET CORRECT PID NAME                   *         
* TKLU 28JUL08 92   <BR12778D> ALLOW OVERWRITE DELETED PHIRECDS       *         
* MPEN 18JUL08 93  <LO01-7405> ADDED VALIDATION TO CHANGE/REMOVE      *         
* /TFRY         END DATES, MODIFIED CHKTIME TO USE PEND DATE, STOPPED *         
*               READING COMPANY RECORD IN VALIDATE KEY, DISPLAY       *         
*               BRANDOCEAN SWITCHON DATES IN PER2 SCREEN DISPLAY      *         
* TFRY 05NOV08 94  <BR13269D> BRANDOCEAN SWITCH ON DATES, CHECK FOR   *         
*               SAME START AND END DATES                              *         
* TFRY 06NOV08 95  <BR13269D> CHECK FOR MULTIPLE DUPLICATE DATES      *         
* TFRY 16JAN08 96  <BR13269D> FIX FOR SWITCH ON DATES & 2CO           *         
* TFRY 19JAN09 97  <BR22548L> CHECK FOR OUTSTANDING EXPENSES BEFORE   *         
*                             ALLOWING CHANGE TO LOCATION             *         
* MPEN 29OCT08 98   <LO01-8189> NEW RECORD TO CONTROL BRANDOCEAN      *         
*               SWITCHON DATES                                        *         
* MPEN 20FEB09 99  <BR23369L> FIX BAD CURSOR POSITION                 *         
* MPEN 26FEB09 100 <BR23369L> FIX MORE BAD CURSOR POSITIONS           *         
* SMAN 02APR09 101 <BR24232L> CHECK IF ANY UNPROTECTED INPUT IN FIELDS*         
* SMAN 15APR09 102 <BR23661L> DEAL WITH NESTED BRO SWITCH ON/OFF DATES*         
* TFRY 22APR09 103 <BR23995L> FIX TO CHKEXP                           *         
* NSHE 11MAY09 104 CHANGE AUDIT RECORD STRUCTURE                      *         
* TFRY 04JUN09 105 <BR14251D> FIX TO CHKEXP                           *         
* TFRY 25JUN09 106 <BR25610L> FIX TO CHKTIME CALL                     *         
* JFOS 15JUL09     <BR26003L> ENSURE STDISP INIT'D TO 00'S            *         
* MPEN 27JUL09 107 <LO01-8967> RELINK FOR BIGGER COBLOCK              *         
* TFRY 17AUG09 108 <BR266629L> FIX TO CHKEXP CALL WHERE TS LOCK EXISTS*         
* TFRY 14SEP09 109 <BR14421D>  LOCATION DELETE - DELETE ALL HISTORY   *         
* TFRY 15DEC09 110 <BR28788L>  PREVENT DUMP IN COST                   *         
*                  <BR28922L>  PREVENT ERROR, WHEN DATE NOT CHANGED   *         
*                  COPY US CHANGES - REMOVE US SPACEPADDING OF PIDKEY *         
* TFRY 20JAN10 111 <BR30085L> QUICK FIX TO PREVIOUS LEVEL TO PREVENT  *         
*                  TRANSFER OF LOCATION WHERE TIME EXISTS             *         
* MPEN 25JAN10 112 <LO01-9704> CHANGE 1R NAME SORT ORDER              *         
*                  <LO01-9691> CHANGE ORDER NAME IS BUILT             *         
* TFRY 19MAR10 113 <BR31961L> NO ERROR, WHEN MIDPER DATE NOT CHANGED  *         
* TKLU 05JUL10 114 <DU01-7217> SOME NEW PERSON/LIST FILTER OPTIONS, + *         
*                  <PR000207> DISALLOW SALARY TRANSFER AND PID CHANGE *         
*                             IN ONE GO (PASSIVE MAINTENANCE ISSUE)   *         
* NRAK 31MAR11 115 <BR17745D> BAD OFFSET CAUSES DUMPS                 *         
* SMAN 28APR11 116 <BR41404L> IGNORE XFFS END DATE ON CHANGING START  *         
* MPEN 22NOV11 117 <BR45742L> CHECK FOR FIELD INPUT THIS TIME IN NEWS *         
* MPEN 25NOV10 118 <PR0002517> SET THIRD PART FLAG IF PROFILE SET     *         
* MPEN 26MAR12     <PR002113> RELINK FOR LARGER COBLOCK               *         
* MPEN 05MAR12 119 <BR19484D> SHOW PERSON END DATE INSTEAD OF HIGH LVL*         
* JFOS 24JUL12 120 <PR002375> ABLLN2Q -> ABLLN3Q                      *         
* MPEN 07MAR12 121 <PR001190> ENSURE ACCOUNT RECORD IS SET TO LOCKED  *         
*                  <PR000369> IMPROVE BO SWITCHON DATES               *         
*                  <PR002401> AMEND CHKEXP TO BE LESS RESTRICTIVE     *         
* MPEN 30MAY12                ALLOW AMENDMENTS TO HIGH LEVEL SWITCHON *         
* MPEN 12APR13 122 <OT75720L> TRAP TO STOP BAD EMPHIR/EMPTRM          *         
* MPEN 29APR13 123 <BR55322L> FIX TO SWITCHON/SWITCHOFF PAIR + REPLACE*         
*                             STDATE WITH DTSTDATE                    *         
* MPEN 26OCT17 --- <DSRD-16769> UPDATE NAMES ON ALL 1R ACCOUNTS       *         
* VGUP 01APR20 131 <DSPCA-3121> IF PERSON IS TERMINATED THEN UPDATE   *         
*                               ALL 1R ACCOUNTS WITH TERMINATED STATUS*         
* ASAX 24JUN20 131 <DSPCA-3121> FIXED BUG OF BLANK TERMINATION DATE   *         
* ASAX 07JUL20 131 <DSPCA-3121> FIXED BUG TO REINITIATE TERM PERSON   *         
* ASAX 13AUG20 115 <SPEC-48719> PERSON RECORD SHOULD CHECK OFFICE EXIST         
***********************************************************************         
                                                                                
T61D08   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1D08**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T61DFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE                                                      
         USING BLOCKSD,R5                                                       
         ST    R3,RELO                                                          
         ST    RC,SAVERC                                                        
         USING CPXELD,BCCPXEL                                                   
*                                                                               
         GOTO1 =A(SETUP),DMCB,RR=RELO       ANY SETUP                           
*                                                                               
         XC    STADDR(SALDB-STADDR),STADDR  INIT STDISP, ETC TO 00'S            
                                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   MAIN05                                                           
         GOTO1 =A(DK),DMCB,RR=RELO                                              
         B     XIT                                                              
MAIN05   CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+12                                                             
         BRAS  RE,DR                                                            
         B     XIT                                                              
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   MAIN10                                                           
         GOTO1 =A(LR),DMCB,RR=RELO                                              
         B     XIT                                                              
MAIN10   CLI   MODE,XRECADD        AFTER AN ADD                                 
         BE    XP                                                               
         CLI   MODE,XRECPUT        AFTER A PUT                                  
         BE    XP                                                               
         CLI   MODE,XRECDEL        AFTER REC DELETED                            
         BE    XP                                                               
         CLI   MODE,RECDEL         TO DELETE A REC                              
         BE    RDEL                                                             
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        VALIDATE KEY                                                           
***********************************************************************         
*                                                                               
VK       MVI   BIT,0               RESET BITS                                   
         XC    DTADDLOC(DTLENQ),DTADDLOC       CLEAR ADD LINE IN TABLE          
         NI    BIT3,X'FF'-DUPLOC                                                
         NI    BIT3,X'FF'-DELDUP                                                
         CLI   RECNUM,RTPR2        PER2 SCREEN                                  
         BNE   *+8                                                              
         MVI   SCRBOT,0            NEVER NEED THIS FIELD FOR PER2               
         TM    TRANSTAT,RCHANG     REC TYPE CHANGED?                            
         BZ    VK07                                                             
         MVI   BIT3,0              YES, THEN CLEAR BIT                          
         MVI   BIT4,0                                                           
         MVI   CHKSTAT,0                                                        
         MVI   SCRBOT,0            START WITH DEFAULT DISPLAY SCREEN            
*                                                                               
VK07     NI    BIT2,X'FF'-NEWTIME  IS COMPANY ON NEW TIME                       
         TM    COMPYST7,CPYSTMSY   ON NEW TIME                                  
         BNO   *+8                                                              
         OI    BIT2,NEWTIME                                                     
         TM    COMPYST6,CPYSRAPP   USE PASSIVE POINTERS?                        
         BNO   *+8                                                              
         OI    BIT4,POINTERS                                                    
*                                                                               
VK08     NI    BIT3,X'FF'-ONEBYTOF                                              
         GOTO1 GETLDG,DMCB,C'1R'   GET 1R FIELD LENGTHS                         
         GOTO1 =A(VALOPTS),DMCB,RR=RELO                                         
         MVC   LN1RLEVS,ABCDLEN                                                 
         LA    R1,LN1RLEV1                                                      
         LA    R0,L'LN1RLEVS                                                    
         SR    RF,RF                                                            
         CLI   0(R1),0                                                          
         BE    *+16                                                             
         LA    R1,1(,R1)                                                        
         LA    RF,1(,RF)           RF=LOWER LEVEL ACCOUNT                       
         BCT   R0,*-16                                                          
         STC   RF,LN1RLOW                                                       
*                                                                               
         MVC   OFFDISP,LDGROFP     LEDGER OFFICE POSITION                       
         TM    LDGROFP,X'40'                                                    
         BO    *+8                                                              
         OI    BIT3,ONEBYTOF                                                    
         NI    OFFDISP,X'FF'-X'40' TURN OFF 2 CHAR BIT                          
         LLC   RE,OFFDISP          AND SUBTRACT 1 FOR                           
         BCTR  RE,0                DISPLACEMENT INTO ACC                        
         STC   RE,OFFDISP                                                       
*                                                                               
         LA    R6,BIGKEY                                                        
         USING LDGRECD,R6                                                       
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,CMPY        COMPANY                                      
         MVC   LDGKUNT(2),=C'1R'   UNIT/LEDGER                                  
         MVI   SVCPYPCD,0                                                       
         GOTO1 HIGH                                                             
         CLC   LDGKEY,KEYSAVE                                                   
         BNE   VK09                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,LDGELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   VK09                NOT FOUND                                    
         USING LDGELD,R6                                                        
         CLI   LDGLN,LDGLNXQ                                                    
         BL    VK09                                                             
         CLI   LDGDEFPC,0                                                       
         BE    VK09                                                             
         MVC   SVCPYPCD,LDGDEFPC                                                
         OI    BIT4,AUTOHIST                                                    
         DROP  R6                                                               
*                                                                               
VK09     CLI   ACTEQU,ACTLIST      LIST - VALIDATE FILTERS                      
         BE    VK20                                                             
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PERSON CODE                                                   
***********************************************************************         
*                                                                               
         MVC   PERSON,SPACES                                                    
         LA    R2,PEMCODEH                                                      
         CLI   RECNUM,RTPR2        PER2 SCREEN                                  
         BNE   *+8                                                              
         LA    R2,PESCODEH                                                      
         CLI   5(R2),0                                                          
         BE    ERRPLS                        MISSING DATA                       
*                                                                               
         CLC   5(1,R2),LN1RLEV4              SHOULD = LEN LEVEL D ACC           
         BH    ETOOLONG                                                         
         CLI   8(R2),X'40'                   CAN'T START WITH SPACE             
         BE    EINVPER                                                          
*                                                                               
         LLC   R3,5(R2)            LENGTH OF INPUT                              
         LA    R1,8(R2)                                                         
VK15     CLI   0(R1),X'40'         NO INBEDDED SPACES                           
         BE    EINVPER                                                          
         LA    R1,1(R1)                                                         
         BCT   R3,VK15                                                          
*                                                                               
         CLC   8(3,R2),NINES       CAN'T BEGIN WITH 3 NINES                     
         BE    EOVRHED             THAT MEANS OVERHEAD                          
*                                                                               
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   PERSON(0),8(R2)       SAVE PERSON CODE                           
         EX    R1,*-6                                                           
         OC    PERSON,SPACES                                                    
*                                                                               
*                                                                               
         CLI   RECNUM,RTPR2        IF ON PERSON SCREEN                          
         BE    VK50                                                             
         TM    PEMCODEH+4,X'80'    AND FIELD INPUT THIS TIME                    
         BNO   *+8                                                              
         OI    BIT,KEYCHNGE        FLAG CHANGE IN KEY                           
         B     VK50                BUILD KEY                                    
         EJECT                                                                  
***********************************************************************         
*        FILTERS FOR ACTION LIST                                                
***********************************************************************         
*                                                                               
VK20     XC    OFFFILT,OFFFILT     OPTIONAL OFFICE FILTER FOR LIST              
         XC    DEPTFILT,DEPTFILT   OPTIONAL DEPT FILTER FOR LIST                
         XC    SUBFILT,SUBFILT     OPTIONAL SUBDEPT FILTER FOR LIST             
         XC    STRTCODE,STRTCODE   OPTIONAL START AT FILTER                     
         NI    BIT2,X'FF'-STARTAT  LIST START AT                                
*                                                                               
         TM    PELOFFH+4,X'80'     ENTERED THIS TIME                            
         BNO   *+8                                                              
         OI    BIT2,STARTAT        START FROM TOP                               
         CLI   PELOFFH+5,0         ANY OFFICE FILTER                            
         BE    VK25                                                             
         LLC   R1,PELOFFH+5                                                     
         BCTR  R1,0                                                             
         MVC   OFFFILT(0),PELOFF                                                
         EX    R1,*-6                                                           
         OC    OFFFILT,SPACES                                                   
*                                                                               
VK25     LA    R2,PELDEPTH         ANY DEPARTMENT FILTER                        
         TM    PELDEPTH+4,X'80'    ENTERED THIS TIME                            
         BNO   *+8                                                              
         OI    BIT2,STARTAT        START FROM TOP                               
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         CLC   5(1,R2),LN1RLEV2                                                 
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   DEPTFILT(0),8(R2)                                                
         EX    R1,*-6                                                           
         OC    DEPTFILT,SPACES                                                  
*                                                                               
VK30     LA    R2,PELSDPTH         ANY SUB DEPT FILTER                          
         TM    PELSDPTH+4,X'80'    ENTERED THIS TIME                            
         BNO   *+8                                                              
         OI    BIT2,STARTAT        START FROM TOP                               
         CLI   5(R2),0                                                          
         BE    VK35                                                             
         CLC   5(1,R2),LN1RLEV3                                                 
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   SUBFILT(0),8(R2)                                                 
         EX    R1,*-6                                                           
         OC    SUBFILT,SPACES                                                   
*                                                                               
VK35     LA    R2,PELSTRTH         ANY START AT CODE                            
         TM    PELSTRTH+4,X'80'    ENTERED THIS TIME                            
         BNO   *+8                                                              
         OI    BIT2,STARTAT                                                     
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         CLC   5(1,R2),LN1RLEV4                                                 
         BH    ETOOLONG                                                         
         MVC   STRTCODE,8(R2)                                                   
         OC    STRTCODE,SPACES                                                  
*                                                                               
VK40     LA    R2,CONOPTH                                                       
         TM    CONOPTH+4,X'80'     ENTERED THIS TIME                            
         BNO   VK50                                                             
         OI    BIT2,STARTAT                                                     
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        BUILD KEY                                                              
***********************************************************************         
*                                                                               
         USING PERRECD,R4                                                       
VK50     LA    R4,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    PERSON RECORD TYPE                           
         MVC   PERKCPY,CMPY        COMPANY                                      
         MVC   PERKCODE,PERSON     PERSON CODE                                  
*                                                                               
         LA    R0,DTLENQ           LENGTH OF 1 ENTRY                            
         MH    R0,=Y(MAXCOUNT)     MAX # OF ENTRIES                             
         CH    R0,STDISP           STDISP SHOULDN'T BE GREATER                  
         BNL   VK52                SO CLEAR FIELDS                              
         MVC   STDISP,=H'0'                                                     
         MVC   PRVSTDSP,=H'0'                                                   
*                                                                               
VK52     TM    TRANSTAT,RACHANG     HAS THE RECORD/ACT BEEN CHANGED             
         BZ    VK55                                                             
         MVC   STDISP,=H'0'                                                     
         MVC   PRVSTDSP,=H'0'                                                   
         MVC   LSTDISP,=H'0'                                                    
VK55     CLC   SAVEKEY(L'ACTKEY),BIGKEY                                         
         BE    VK60                                                             
         MVC   STDISP,=H'0'                                                     
         MVC   PRVSTDSP,=H'0'                                                   
         MVC   LSTDISP,=H'0'                                                    
         MVC   SAVEKEY,BIGKEY                                                   
         OI    BIT,KEYCHNGE        CHANGE IN KEY                                
         CLI   RECNUM,RTPR2        PER2 SCREEN                                  
         BE    VK60                                                             
         CLI   ACTEQU,ACTLIST      LIST                                         
         BE    VK60                                                             
         BRAS  RE,CLRSCR                                                        
*                                                                               
VK60     CLI   ACTEQU,ACTDEL              ACTION DELETE?                        
         BNE   VKX                                                              
         GOTO1 HIGH                       TO DELETE A RECORD                    
         CLC   BIGKEY(L'ACTKEY),KEYSAVE   MAKE SURE RECORD HAS NO               
         BNE   VKX                        LOCATION ELEMENTS                     
         GOTO1 GETREC                                                           
         BRAS  RE,OKTODEL                                                       
*                                                                               
VKX      XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'SAVEKEY),SAVEKEY  RESET KEY                             
         CLI   ACTEQU,ACTLIST      IF NOT ACTION LIST DO NOT CHECK              
         BNE   XIT                 BIT-BECAUSE CAN'T FIND DAMN BUG!             
*                                                                               
         TM    BIT3,OPTVK       DID WE COME FROM LIST RECS?                     
         BZ    XIT                                                              
         CLI   MODE,LISTRECS                                                    
         BNE   XIT                                                              
         B     LR                  YES                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        RECORD DELETE                                                          
***********************************************************************         
*                                                                               
RDEL     BRAS  RE,OKTODEL          IS IT OK TO DELETE                           
         B     XIT                                                              
*                                                                               
***********************************************************************         
*        VALIDATE AND UPDATE PERSON RECORD                                      
***********************************************************************         
VR       NI    CHKSTAT2,X'FF'-VAL2LOCS                                          
         CLI   SCRBOT,0            IS MESSAGE LOADED?                           
         BE    VR01                NO                                           
*                                                                               
* IF MESSAGE IS THERE MUST VALIDATE USER INPUT FIRST                            
*                                                                               
VRA      NI    CHKSTAT,X'FF'-(MOVESAL+DELTSAL)                                  
         LA    R2,PEMANSWH                                                      
         CLI   5(R2),0             ANY INPUT                                    
         BE    ERRMISS                                                          
*                                                                               
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXCLC R1,8(R2),AC@CDEL    DELETE?                                      
         BNE   *+12                                                             
         OI    CHKSTAT,DELTSAL                                                  
         B     VRC                                                              
         EXCLC R1,8(R2),AC@CQUIT   QUIT?                                        
         BE    VRB                                                              
         EXCLC R1,8(R2),AC@CMOVE   MOVE?                                        
         BNE   ERRINV                                                           
         CLI   SCRBOT,2            MUST BE RIGHT SCREEN FOR MOVE OPTION         
         BNE   ERRINV                                                           
         OI    CHKSTAT,MOVESAL                                                  
         B     VRC                                                              
VRB      OI    CHKSTAT,MSGQUIT     SET STATUS                                   
         MVI   SCRBOT,0                                                         
         NI    CHKSTAT,X'FF'-SALOUT                                             
         GOTO1 =A(SCREENS),DMCB,RR=RELO                                         
         LA    R2,PEMLIN1H                                                      
         TM    BIT2,ADJRATE        THEN GIVE MESSAGE SAL EXISTS                 
         BO    EDFADJ                                                           
         B     EDSALS                                                           
*                                                                               
VRC      NI    CHKSTAT,X'FF'-MSGQUIT  FIRST MUST CHECK SECURITY ACCESS          
         MVI   WORK,ACTDEL        DEFAULT TO ACTION DELETE                      
         LA    R3,WORK                                                          
         TM    CHKSTAT,MOVESAL     WANT TO MOVE SALARY?                         
         BZ    *+12                                                             
         MVI   WORK,8             ACTION MOVE                                   
         LA    R3,WORK                                                          
         MVI   SBYTE,RTHIS                                                      
         ICM   R3,8,SBYTE                                                       
         GOTO1 SECRET,DMCB,('SECPRACT',ASECBLK),(R3)                            
         BE    VRD                                                              
         MVI   SCRBOT,0                                                         
         GOTO1 =A(SCREENS),DMCB,RR=RELO   RELOAD DEFAULT SCREEN                 
         LA    R2,PEMLIN1H                                                      
         B     EINVSECL            SECURITY LOCKOUT                             
*                                                                               
VRD      TM    CHKSTAT2,MOALOCK    NEXT MUST CHECK IF ANY SAL MONTHS            
         BZ    VRE                 ARE LOCKED BY TY97                           
         MVI   SCRBOT,0                                                         
         GOTO1 =A(SCREENS),DMCB,RR=RELO   RELOAD DEFAULT SCREEN                 
         LA    R2,PEMLIN1H                                                      
         B     ERRMLOCK            SECURITY LOCKOUT                             
*                                                                               
VRE      TM    CHKSTAT,MOVESAL     WANT TO MOVE SALARY? (TRANSFERRING)          
         BZ    VRF                                                              
         MVI   PFKEY,16            THEN PF TO HISTORY/MOVE SCREEN               
         CLC   OLDPID,PIDNUM                                                    
         BE    VRG                                                              
         LA    R2,PEMPIDH                                                       
         B     EINVLOC                                                          
*                                                                               
* NEED TO FIGURE OUT WHICH DELETE OPTION WE ARE USING - IF DELELOC BIT          
* IS ON-USER IS DELETING A LOCATION SO DO NOT NEED TO LOAD THE                  
* HISTORY SCREEN-CAN DELETE FUTURE HISTORY RECS FROM THIS PROGRAM.              
* IF USER IS ADDING AN ENDDATE TO A LOCATION-NEED TO LOAD HISTORY/MOVE          
* SCREEN.                                                                       
*                                                                               
VRF      TM    CHKSTAT,DELELOC     IS USER DELETING LOCATION                    
         BO    VR01                                                             
         MVI   PFKEY,15            NO THEN PF TO HISTORY/DELETE SCREEN          
         CLI   LOCBIT,0            ADDING A NEW LOC                             
         BE    VRG                                                              
         MVI   PFKEY,17            THEN PF TO HISTOR/DEL FOR TRNSFR             
*                                                                               
VRG      MVI   SCRBOT,0            SET BACK DEFAULT SCRN SO WILL REDISP         
         GOTO1 =A(PERSINFO),DMCB,RR=RELO  BUILD BLOCK OF INFO TO PASS           
         OI    PRGSTAT,PGMCALL    SET BIT THAT CALLING PGM FROM OVERLAY         
         NI    CHKSTAT,X'FF'-SALOUT                                             
         BRAS  RE,CALLPF                                                        
         DC    H'0'                SHOULDN'T RETURN                             
         EJECT                                                                  
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*                                                                               
VR01     GOTO1 =A(VALOPTS),DMCB,RR=RELO        VALIDATE OPTIONS                 
         NI    BIT3,X'FF'-DUPLOC                                                
         NI    BIT3,X'FF'-DELDUP                                                
         CLI   ACTEQU,ACTDIS                                                    
         BNE   *+12                                                             
         BRAS  RE,DR                                                            
         B     XIT                                                              
         OI    GENSTAT2,RETEQSEL            REDISP BEFORE RET TO LIST           
         XC    DTADDLOC(DTLENQ),DTADDLOC    CLEAR ADD LINE IN TABLE             
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BE    VR02                            FOR ACTION ADD                   
         TM    BIT,KEYCHNGE                    DID THE KEY CHANGE               
         BNO   *+12                                                             
         BRAS  RE,DR                                                            
         B     XIT                                                              
*                                                                               
         CLI   RECNUM,RTPR2                    STATUS SCREEN?                   
         BE    VR03                                                             
         TM    BIT2,REBUILD           REBUILD AFTER PER2 SCREEN                 
         BNO   VR10                                                             
VR02     GOTO1 =A(BLDTABLE),DMCB,RR=RELO       BUILD EMPTY TABLE                
         B     VR10                                                             
*                                                                               
VR03     GOTO1 =A(VALSTSCR),DMCB,RR=RELO       VALIDATE STATUS SCREEN           
         B     VR250                                                            
         EJECT                                                                  
*======================================================================         
* CAN'T FIND BUG WHEN STDISP=SPACES SO PUT IN THIS CHECK                        
*======================================================================         
*                                                                               
VR10     LA    R0,DTLENQ           LENGTH OF 1 ENTRY                            
         MH    R0,=Y(MAXCOUNT)     MAX # OF ENTRIES                             
         CH    R0,STDISP           STDISP SHOULDN'T BE GREATER                  
         BNL   VR10A               SO CLEAR FIELDS                              
         MVC   STDISP,=H'0'                                                     
         MVC   PRVSTDSP,=H'0'                                                   
*======================================================================         
***********************************************************************         
*        VALIDATE PERSONAL ID                                                   
***********************************************************************         
*                                                                               
VR10A    LA    R2,PEMPIDH                                                       
         CLI   PEMPIDH+5,0                                                      
         BNE   VR12                                                             
         XC    ACCNT,ACCNT         TEST AT AGENCY LEVEL                         
         BRAS  RE,REQPID           IS PID REQ'D                                 
         BE    ERRMISS                                                          
*                                                                               
         OC    OLDPID,OLDPID       WAS THERE PREVIOUSLY A PID?                  
         BZ    VR10B               NO                                           
         GOTO1 =A(XREFCHK),DMCB,RR=RELO CHECK FOR TEMPO XREF RECS FIRST         
         BE    EDPOST                   TIME EXISTS-CAN'T DELETE PID            
*                                                                               
VR10B    MVC   PIDNAME,SPACES                                                   
         XC    PIDNUM,PIDNUM                                                    
         B     VR14                NOT REQ'D - MAKE SURE ELEM DELETED           
*                                                                               
VR12     TM    PEMPIDH+4,X'80'     FIELD INPUT THIS TIME                        
         BO    VR13                                                             
         TM    PEMPIDH+4,X'40'     FIELD INPUT PREVIOUSLY                       
         BO    VR13A                                                            
         TM    PEMPIDH+4,X'20'     FIELD PREV VALIDATED                         
         BZ    VR13                                                             
         B     *+20                NO NEED TO CHECK                             
VR13     TM    BIT2,NOACCURR       HAVE ACCESS TO THE CURRENT LOC?              
         BZ    *+12                                                             
         LA    R2,PEMPIDH          NO-CAN'T ADD/CHA PID                         
         B     EINVSECL                                                         
*                                                                               
VR13A    MVC   PIDNAME,PEMPID                                                   
         OC    PIDNAME,SPACES                                                   
*                                                                               
         OC    OLDPID,OLDPID       WAS THERE PREVIOUSLY A PID?                  
         BZ    VR13B               NO THEN LET THEM ADD ONE                     
*                                                                               
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXCLC R1,PEMPID,SVPIDNM   CHANGE IN PID?                               
         BE    VR13B               NO                                           
         GOTO1 =A(XREFCHK),DMCB,RR=RELO CHECK FOR TEMPO XREF RECS FIRST         
         BE    EDPOST                   TIME EXISTS-CAN'T CHANGE PID            
VR13B    GOTO1 =A(VALPID),DMCB,RR=RELO VALIDATE PID IN SECURITY                 
         BNE   ERRINV                                                           
*                                                                               
VR14     OI    PEMPIDH+4,X'20'     VALIDATED BIT                                
         GOTO1 =A(ELEMD8),DMCB,RR=RELO  UPDATE X'D8' PID ELEMENT                
         EJECT                                                                  
***********************************************************************         
*        VALIDATE HIRE DATE                                                     
***********************************************************************         
*                                                                               
VR20     LA    R2,PEMHIREH         HIRE DATE                                    
         GOTO1 ANY                 REQUIRED FIELD                               
         TM    PEMHIREH+4,X'80'    FIELD INPUT THIS TIME                        
         BO    VR21                                                             
         TM    PEMHIREH+4,X'40'    FIELD INPUT PREVIOUSLY                       
         BO    VR21A                                                            
         TM    PEMHIREH+4,X'20'    FIELD PREV VALIDATED?                        
         BZ    VR21                                                             
         B     *+20                NO NEED TO CHECK                             
VR21     TM    BIT2,NOACCURR       HAVE ACCESS TO THE CURRENT LOC?              
         BZ    *+12                                                             
         LA    R2,PEMHIREH         NO CAN'T ADD/CHA HIRE DATE                   
         B     EINVSECL                                                         
*                                                                               
VR21A    GOTO1 =A(VALHIRE),DMCB,RR=RELO                                         
         BE    *+12                                                             
         LA    R2,PEMHIREH                                                      
         B     ERRHIRE                                                          
         MVC   HIREDT,WORK                                                      
         EJECT                                                                  
***********************************************************************         
*        VALIDATE NAMES                                                         
***********************************************************************         
*                                                                               
VR30     MVC   PEMWARN,SPACES                                                   
         OI    PEMWARNH+6,X'80'    XMIT                                         
         MVC   PEMSNAM,SPACES                                                   
         OI    PEMSNAMH+6,X'80'    XMIT                                         
         CLI   ACTEQU,ACTADD       NAMES REQ'D ON ADD                           
         BE    VR32                                                             
         BRAS  RE,FLDSEC           SECURITY TO PRINT NAME                       
*&&US*&& CLI   BYTE,C'W'           DO THEY HAVE WRITE ACCESS?                   
         BE    VR32                CAN'T GET NAME FROM SCREEN SO GET            
         GOTO1 =A(GETNAME),DMCB,RR=RELO FROM EXISTING NAME ELEMENTS             
*&&US                                                                           
         CLI   BYTE,C'R'           DO THEY HAVE READ ACCESS?                    
         BNE   *+16                NO ACCESS                                    
         OI    PEMLNAMH+1,X'20'    PROTECT                                      
         OI    PEMFNAMH+1,X'20'    PROTECT                                      
         B     VR32                                                             
*&&                                                                             
         OI    PEMLNAMH+1,X'0C'    AND SET TO LOW INTENSITY SO HIDDEN           
         OI    PEMFNAMH+1,X'0C'                                                 
*&&UK*&& B     VR40                                                             
VR32     LA    R2,PEMLNAMH         LAST NAME                                    
         GOTO1 ANY                                                              
         TM    PEMLNAMH+4,X'80'    FIELD INPUT THIS TIME                        
         BO    VR34                                                             
         TM    PEMLNAMH+4,X'40'    FIELD INPUT PREVIOUSLY                       
         BO    VR35                                                             
         TM    PEMLNAMH+4,X'20'    FIELD PREV VALIDATED                         
         BZ    VR34                                                             
         B     *+20                NO NEED TO CHECK                             
VR34     TM    BIT2,NOACCURR       HAVE ACCESS TO THE CURRENT LOC?              
         BZ    *+12                                                             
         LA    R2,PEMLNAMH         NO CAN'T ADD/CHA LAST NAME                   
         B     EINVSECL                                                         
         OI    PEMLNAMH+4,X'20'    VALIDATED BIT                                
*                                                                               
VR35     LA    R2,PEMFNAMH         FIRST NAME                                   
         GOTO1 ANY                                                              
         TM    PEMFNAMH+4,X'80'    FIELD INPUT THIS TIME?                       
         BO    VR36                                                             
         TM    PEMFNAMH+4,X'40'    FIELD INPUT PREVIOUSLY                       
         BO    VR37                                                             
         TM    PEMFNAMH+4,X'20'    PREV VALIDATED                               
         BZ    VR36                                                             
         B     *+20                NO NEED TO CHECK                             
VR36     TM    BIT2,NOACCURR       HAVE ACCESS TO CURRENT LOC?                  
         BZ    *+12                                                             
         LA    R2,PEMFNAMH         CAN'T ADD/CHA FIRST NAME                     
         B     EINVSECL                                                         
         OI    PEMFNAMH+4,X'20'    VALIDATED BIT                                
*                                                                               
VR37     CLI   PEMPIDH+5,0         ONLY CHECK IF THERE'S A PID                  
         BE    VR38                                                             
         GOTO1 =A(GETPIDNM),DMCB,RR=RELO  GET NAME FROM SECURITY SIDE           
         LLC   R1,SECLLN           LEN OF LAST NAME IN SECURITY                 
         LLC   RF,PEMLNAMH+5       LEN OF LAST NAME IN =COST                    
         CR    RF,R1               USE WHATEVER LENGTH IS HIGHER                
         BNH   *+6                                                              
         LR    R1,RF                                                            
         AHI   R1,-1                                                            
         BM    VR37A                                                            
         EXCLC R1,SECLAST,PEMLNAM                                               
         BNE   VR37B                                                            
VR37A    LLC   R1,SECFLN          LEN OF FIRST NAME IN SECURITY                 
         LLC   RF,PEMFNAMH+5       LEN OF FIRST NAME IN =COST                   
         CR    RF,R1               USE WHATEVER IS HIGHER                       
         BNH   *+6                                                              
         LR    R1,RF                                                            
         AHI   R1,-1                                                            
         BM    VR38                                                             
         EXCLC R1,SECFIRST,PEMFNAM                                              
         BE    VR38                                                             
VR37B    L     R1,=A(DDNMERR)                                                   
         A     R1,RELO                                                          
         MVC   PEMWARN(37),0(R1)                                                
         GOTO1 DICTATE,DMCB,C'SL  ',PEMWARN,0                                   
*&&US                                                                           
         CLI   BYTE,C'N'           CAN THEY SEE THE NAMES?                      
         BE    VR37C                                                            
*&&                                                                             
         XC    BLOCK(100),BLOCK                                                 
         MVC   BLOCK(L'SECLAST),SECLAST                                         
         MVI   BLOCK+L'SECLAST+2,C','                                           
         MVC   BLOCK+L'SECLAST+5(L'SECFIRST),SECFIRST                           
         GOTO1 SQUASHER,DMCB,BLOCK,100                                          
         MVC   PEMSNAM,BLOCK                                                    
         OI    PEMSNAMH+6,X'80'    TRANSMIT                                     
VR37C    OI    PEMWARNH+6,X'80'                                                 
         B     ERRNAME                                                          
*                                                                               
VR38     GOTO1 =A(ELEM5A),DMCB,RR=RELO  X'5A' GENERAL PURPOSE NAME EL           
         EJECT                                                                  
***********************************************************************         
*        VALIDATE TERMINATION DATE                                              
***********************************************************************         
*                                                                               
VR40     LA    R2,PEMTERMH         TERMINATION DATE                             
         CLI   5(R2),0                                                          
*&&UK*&& BNE   *+14                                                             
*&&US                                                                           
         BNE   *+18                                                             
         NI    BIT,X'FF'-REQRD                                                  
*&&                                                                             
         XC    TERMDT,TERMDT                                                    
         B     VR60                                                             
         TM    PEMTERMH+4,X'80'    FIELD INPUT THIS TIME                        
         BO    VR45                                                             
         TM    PEMTERMH+4,X'40'    FIELD INPUT PREVIOUSLY                       
         BO    VR48                                                             
         TM    PEMTERMH+4,X'20'    PREV VALIDATED?                              
         BZ    VR45                                                             
         B     *+20                NO NEED TO CHECK                             
VR45     TM    BIT2,NOACCURR       ACCESS TO THE CURRENT LOC?                   
         BZ    *+12                                                             
         LA    R2,PEMTERMH         CAN'T ADD/CHA TERM DATE                      
         B     EINVSECL                                                         
*                                                                               
VR48     XC    TERMDT,TERMDT                                                    
*&&US*&& OI    BIT,REQRD           PREVIOUS END DATE REQ'D                      
         LA    R1,PEMTERM                                                       
         ST    R1,DMCB                                                          
         LLC   R1,5(R2)                                                         
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,X'04'                                                     
         BNE   EINVDATE                                                         
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
*&&US                                                                           
         MVC   TEMPDATE,PVALPSTA                                                
         CLC   TEMPDATE,HIREDT     TERM DTE MUST NOT BE BEFORE HIRE DTE         
         BL    ERRTERM                                                          
         GOTO1 =A(READCAL),DMCB,RR=RELO                                         
*        CLC   ENDATE,PVALPSTA     MAKE SURE ITS ON AN ENDDATE                  
*        BNE   ERRLOCBT                                                         
*&&                                                                             
         MVC   TERMDT,PVALPSTA     PWOS                                         
         OI    PEMTERMH+4,X'20'    VALIDATED BIT                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        NEW LOCATION?                                                          
***********************************************************************         
*                                                                               
*  MUST SEE HOW MANY ENTRIES ALREADY EXIST                                      
*                                                                               
VR60     CLI   ACTEQU,ACTADD                                                    
         BE    VR62                                                             
*                                                                               
         LA    R2,PEMOFFH          OFFICE                                       
         CLI   5(R2),0                                                          
         BNE   VR61                                                             
         LA    R2,PEMDEPTH         DEPT                                         
         CLI   5(R2),0                                                          
         BNE   VR61                                                             
         LA    R2,PEMSDPTH         SUBDEPT                                      
         CLI   5(R2),0                                                          
         BE    VR62                                                             
*                                                                               
VR61     LA    R0,DTLENQ           LENGTH OF ONE ENTRY                          
         MHI   R0,8                * 9 DISPLAY ENTRIES PER PAGE                 
         CH    R0,STDISP           IF ON 2ND PAGE OR MORE                       
         BH    *+12                CANNOT ADD LOCATION                          
         LA    R2,PEMOFFH                                                       
         B     ERRLOC                                                           
*                                  COPY AIO -> AIO3, PREVENT OVERWRITE          
         L     R0,AIO3             OF NEW PID                                   
         L     RE,AIO                                                           
         USING PERRECD,RE                                                       
         SR    RF,RF                                                            
         ICM   RF,3,PERRLEN        LENGTH OF REC                                
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 =A(BLDTABLE),DMCB,RR=RELO                                        
*                                                                               
         L     R0,AIO              COPY AIO3 BACK TO AIO                        
         L     RE,AIO3                                                          
         SR    RF,RF                                                            
         ICM   RF,3,PERRLEN        LENGTH OF REC                                
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         DROP  RE                                                               
*                                                                               
         CLC   TABCOUNT,=Y(MAXCOUNT)       CAN'T BE MORE THAN MAX               
         BL    VR62                                                             
         B     ERREC2BG                                                         
         DC    H'0'                                                             
*                                                                               
         USING DISTABD,R4                                                       
VR62     LA    R4,DTADDLOC         ADD NEW LOCATION TO TOP OF TABLE             
         MVI   LOCBIT,0                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        NEW LOCATION - VALIDATE OFFICE                                         
***********************************************************************         
*                                                                               
VR70     LA    R2,PEMOFFH          OFFICE                                       
         CLI   ACTEQU,ACTADD       REQ'D ON ADD                                 
         BE    VR72                                                             
         CLI   5(R2),0                                                          
         BE    VR80                                                             
*                                                                               
VR72     CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         CLC   5(1,R2),LN1RLEV1                                                 
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         MVC   DTOFFICE,PEMOFF                                                  
         OC    DTOFFICE,SPACES                                                  
         OI    LOCBIT,LBOFF        YES ENTERED OFFICE                           
         LA    R3,DTACCNT                                                       
         MVC   DTACCNT,SPACES      BUILD ACCOUNT IN TABLE TOO                   
         LLC   R1,LN1RLEV1                                                      
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   NINES(0),8(R2)      CAN'T HAVE OFFICE OF ALL NINES               
         BE    EOVRHED             THAT'S FOR OVERHEAD                          
         MVC   0(0,R3),PEMOFF      BUILD 1R ACCOUNT IN DTACCNT                  
         EX    R1,*-6                                                           
         LA    R3,1(R1,R3)                                                      
         TM    COMPYST4,CPYSOFF2   TEST IF COMPANY USES NEW OFFICE              
         JNO   VR80                                                             
         BRAS  RE,VALOFF           VALIDATE OFFRECD EXIST FOR OFFICE            
         BNE   EINVOFC                                                          
         EJECT                                                                  
***********************************************************************         
*        NEW LOCATION - VALIDATE DEPARTMENT                                     
***********************************************************************         
*                                                                               
VR80     LA    R2,PEMDEPTH         DEPARTMENT                                   
         TM    LOCBIT,LBOFF        WAS AN OFFICE ENTERED                        
         BO    VR82                                                             
         CLI   5(R2),0                                                          
         BE    VR90                                                             
         LA    R2,PEMOFFH          OFFICE                                       
         B     EMISHIGH            MISSING HIGHER LEVELS                        
*                                                                               
VR82     CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         CLC   5(1,R2),LN1RLEV2                                                 
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         MVC   DTDEPT,PEMDEPT                                                   
         OC    DTDEPT,SPACES                                                    
         OI    LOCBIT,LBDEPT                                                    
         LLC   R1,LN1RLEV2                                                      
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   NINES(0),8(R2)      CAN'T HAVE DEPT OF ALL NINES                 
         BE    EOVRHED             THAT'S FOR OVERHEAD                          
         MVC   0(0,R3),PEMDEPT     BUILD 1R ACCOUNT IN DTACCNT                  
         EX    R1,*-6                                                           
         LA    R3,1(R1,R3)                                                      
         EJECT                                                                  
***********************************************************************         
*        NEW LOCATION - VALIDATE SUB DEPARTMENT                                 
***********************************************************************         
*                                                                               
VR90     LA    R2,PEMSDPTH         SUB DEPARTMENT                               
         TM    LOCBIT,LBOFF+LBDEPT       HIGHER LEVELS EXIST                    
         BNZ   VR92                                                             
         CLI   5(R2),0                                                          
         BE    VR130                                                            
         LA    R2,PEMOFFH          OFFICE                                       
         B     EMISHIGH            MISSING HIGHER LEVELS                        
*                                                                               
VR92     CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         CLC   5(1,R2),LN1RLEV3                                                 
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         MVC   DTSUBDPT,PEMSDPT                                                 
         OC    DTSUBDPT,SPACES                                                  
         OI    LOCBIT,LBSUB                                                     
         LLC   R1,LN1RLEV3                                                      
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   0(0,R3),PEMSDPT     BIULD 1R ACCOUNT IN DTACCNT                  
         EX    R1,*-6                                                           
         EJECT                                                                  
***********************************************************************         
*        NEW LOCATION - VALIDATE OFF/DPT/SUB IN 1R                              
***********************************************************************         
*                                                                               
         LA    R2,PEMOFFH                                                       
         MVC   ACCNT,DTACCNT                                                    
         BRAS  RE,VALLOC           VALIDATE OFFICE,DEPT,SUBDPT WITH 1R          
         BNE   EMISHIGH                                                         
*                                                                               
         LA    R3,1(R1,R3)         MOVE IN PERSON TO COMPLETE                   
         LLC   R1,LN1RLEV4                                                      
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   0(0,R3),PERSON      COMPLETE 1R ACCOUNT IN DTACCNT               
         EX    R1,*-6                                                           
*                                                                               
         XC    WORK,WORK                                                        
         LA    R3,DTACCNT                                                       
         CLI   OFFDISP,0           IS OFFICE IN 1ST LEVEL?                      
         BNE   VR96                                                             
         LLC   RF,LN1RLEV1                                                      
         B     VR98                                                             
*                                                                               
VR96     CLC   OFFDISP,LN1RLEV1    IS OFFICE IN 2ND LEVEL?                      
         BNE   VR97                                                             
         LLC   R0,LN1RLEV1                                                      
         AR    R3,R0                                                            
         LLC   RF,LN1RLEV2                                                      
         B     VR98                                                             
*                                                                               
VR97     LLC   R0,LN1RLEV1         IS OFFICE IN 3RD LEVEL?                      
         LLC   R1,LN1RLEV2                                                      
         AR    R1,R0                R1=LEV1+LEV2                                
         LLC   R0,OFFDISP                                                       
         CR    R0,R1                MUST BE 1ST,2ND OR 3RD LEVEL                
         BE    *+8                                                              
         B     VR100               DON'T RECOGNIZE-DON'T CHECK                  
         AR    R3,R1                                                            
         LLC   RF,LN1RLEV3                                                      
*                                                                               
VR98     BCTR  RF,0                                                             
         EXMVC RF,WORK,0(R3)                                                    
         GOTO1 =A(OFFACC),DMCB,RR=RELO    VALIDATE OFFICE ACCESS                
         BNE   EINVSECL                                                         
         EJECT                                                                  
***********************************************************************         
*        NEW LOCATION - VALIDATE START DATE (AND PREVIOUS END DATE)             
***********************************************************************         
*                                                                               
VR100    LA    R2,PEMSTDTH         DATE LOCATION STARTED                        
         CLI   ACTEQU,ACTADD       USE HIRE DATE ON ADD IF BLANK                
         BNE   VR102                                                            
         CLI   5(R2),0                                                          
         BNE   VR102                                                            
         MVC   DTPREVST,HIREDT                                                  
         MVC   DTSTDATE,HIREDT                                                  
         B     VR104                                                            
*                                                                               
VR102    GOTO1 ANY                                                              
         XC    DMCB,DMCB                                                        
         LA    R1,PEMSTDT                                                       
         ST    R1,DMCB                                                          
         LLC   R1,5(R2)                                                         
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,X'04'                                                     
         BNE   EINVDATE                                                         
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         MVC   DTSTDATE,PVALPSTA     PWOS                                       
         CLC   DTSTDATE,HIREDT     START MUST BE AFTER HIRE DATE                
         BL    ESTHIRE                                                          
         OC    TERMDT,TERMDT                                                    
         BZ    *+14                                                             
         CLC   DTSTDATE,TERMDT     START MUST BE BEFORE TERM DATE               
         BH    EDTTERM                                                          
         MVC   DTPREVST,DTSTDATE                                                
VR104    GOTO1 DATCON,DMCB,(1,DTSTDATE),(DOUT,PEMSTDT)                          
         MVC   STDATE,DTSTDATE                                                  
         MVI   5(R2),8             SET CORRECT LENGTH                           
         OI    BIT,REQRD           PREVIOUS END DATE REQ'D                      
         GOTO1 =A(PREVEND),DMCB,RR=RELO                                         
*                                                                               
* SET DEFAULT SCREEN FOR NOW UNTIL FINISHED VALIDATING                          
*                                                                               
         MVI   SCRBOT,0                                                         
         EJECT                                                                  
***********************************************************************         
*        NEW LOCATION - VALIDATE END DATE                                       
***********************************************************************         
*                                                                               
VR110    LA    R2,PEMENDTH         DATE LOCATION ENDED                          
         CLI   5(R2),0                                                          
         BE    VR130                                                            
         LA    R1,PEMENDT                                                       
         ST    R1,DMCB                                                          
         LLC   R1,5(R2)                                                         
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,X'04'                                                     
         BNE   EINVDATE                                                         
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         MVC   DTENDATE,PVALPSTA   PWOS                                         
         CLC   DTSTDATE,DTENDATE   END MUST BE AFTER START                      
         BNL   ESTEND                                                           
*&&UK                                                                           
         OC    TERMDT,TERMDT                                                    
         BZ    *+14                                                             
         CLC   DTENDATE,TERMDT     CANNOT BE AFTER TERM DATE                    
         BH    EDTTERM                                                          
*&&                                                                             
         BRAS  RE,FUTURSAL         CAN'T END WITH FUTURE SAL                    
         BNE   EDSALS                                                           
         GOTO1 =A(FUTRTIME),DMCB,RR=RELO CAN'T END WITH FUTURE TIME             
         BNE   EDPOST              CAN'T CHANGE - POSTINGS EXIST                
         GOTO1 DATCON,DMCB,(1,PVALPSTA),(DOUT,PEMENDT)                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK FOR DUPLICATE LOCATION (IF ADDING NEW LOCATION ONLY)             
*        VALIDATE PREVIOUS END DATE                                             
***********************************************************************         
*                                                                               
VR130    CLI   LOCBIT,0            ADDING NEW LOCATION                          
         BE    VR132                                                            
         BRAS  RE,DUPMRK           MARK ANY DUPLICATE LOCATIONS                 
         MVC   STDATE,DTSTDATE     THERE IS A NEXT START DATE                   
         OI    BIT,REQRD           PREVIOUS END DATE REQ'D                      
         B     VR134                                                            
VR132    XC    STDATE,STDATE       NO NEXT START DATE                           
*&&UK*&& NI    BIT,X'FF'-REQRD     PREVIOUS END DATE NOT REQ'D                  
VR134    GOTO1 =A(PREVEND),DMCB,RR=RELO                                         
         GOTO1 =A(DATEUPD),DMCB,RR=RELO    ANY OTHER DATES TO VAL               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE ALL SAL LOCK DATES/STATII/FILTERS/SELECT FIELDS               
*        UPDATE TABLE AND X'83' LOCATION ELEM ON PERSON REC                     
***********************************************************************         
*                                                                               
         USING DISTABD,R4                                                       
         LA    R4,DTADDLOC         TOP OF TABLE                                 
         AH    R4,STDISP                                                        
         USING LLINED,R2                                                        
         LA    R2,PEMSELH          FIRST LINE ON SCREEN                         
         XC    STDAY,STDAY                                                      
         XC    TSTDAY,TSTDAY                                                    
         XC    TENDAY,TENDAY                                                    
*&&US*&& OI    BIT,CURRLOC                                                      
*                                                                               
VR150    OC    0(DTLENQ,R4),0(R4)  ANY ENTRY IN TABLE                           
         BZ    VR190NX                                                          
*                                                                               
         GOTO1 =A(VALSALKD),DMCB,RR=RELO   VALIDATE SALARY LOCK DATE            
         GOTO1 =A(VALSTAT),DMCB,RR=RELO     VALIDATE STATUS                     
*&&US*&& NI    BIT,X'FF'-CURRLOC                                                
*&&US*&& BRAS  RE,VALFREE          VALIDATE FREELANCER                          
         BRAS  RE,VALFILT          VALIDATE FILTERS                             
*                                  CHECK WHETHER ACTIVE OFFICE                  
         CLI   DTSTATUS,LOCSACT    REQUIRES PID FOR MCS                         
         BNE   VR150A                                                           
*&&US*&& TM    DTATTR,LOCYFRL      CHECK IF FREELANCER                          
*&&US*&& BO    VR150A                                                           
         TM    DTLSTAT,DTSDEL      DELETED LINE                                 
         BO    VR150A                                                           
         CLI   PEMPIDH+5,0         HAVE THEY ENTERED A PID                      
         BH    VR150A                                                           
         BRAS  RE,REQMCS           CHECKS WHETHER USING MCS TIME                
         BNE   VR150A              NO - DON'T NEED PID                          
         LA    R2,PEMPIDH          OTHERWISE DO NEED PID                        
         B     ERRMISS                                                          
*                                                                               
* IF WE GET HERE AND SALOUT BIT IS ON THAT MEANS THERE IS SALARY PAST           
* THE END DATE AND MUST CHECK TO SEE IF ADDING A NEW LOCATION (LOCBIT)          
* YES-THEN MUST VALIDATE THE PREVIOUS LOCATION BEFORE PROMPTING USER            
* TO DELETE/MOVE                                                                
*                                                                               
VR150A   TM    CHKSTAT,SALOUT      SAL PAST ENDDATE                             
         BZ    VR152               CONTINUE UPDATING                            
         TM    CHKSTAT,DELELOC     IS USER DELETING A LOCATION                  
         BO    VR152             THEN DEL FUTURE HISTORY FROM THIS PGM          
         CLI   LOCBIT,0            ADDING NEW LOCATION                          
         BE    VR151               NO-LOAD MESSAGE SCREEN                       
         TM    CHKSTAT2,VAL2LOCS   HAVE WE VALIDATED THE PREV LOC?              
         BO    VR151               YES THEN LOAD SCREEN                         
         OI    CHKSTAT2,VAL2LOCS                                                
         LA    R4,DTLENQ(R4)       NEXT TABLE ENTRY                             
         LA    R2,LLLEN(R2)        NEXT SCREEN LINE                             
         B     VR150                                                            
*                                                                               
VR151    DS    0H                                                               
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD?                 
         BO    VR151AA             THAN CAN'T USE THE NEW FEATURE               
         NI    CHKSTAT2,X'FF'-VAL2LOCS  FIRST MUST CHECK SEC ACCESS             
         MVI   WORK,ACTDEL        DEFAULT TO ACTION DELETE                      
         LA    R3,WORK                                                          
         CLI   LOCBIT,0            ADDING A LOCATION?                           
         BZ    *+12                                                             
         MVI   WORK,8             ACTION MOVE                                   
         LA    R3,WORK                                                          
         MVI   SBYTE,RTHIS                                                      
         ICM   R3,8,SBYTE                                                       
         GOTO1 SECRET,DMCB,('SECPRACT',ASECBLK),(R3)                            
         BE    VR151A                                                           
         NI    CHKSTAT,X'FF'-SALOUT      SAL PAST ENDDATE                       
VR151AA  LA    R2,PEMPENDH                                                      
         B     EDSALS                   CAN'T CHANGE - SALARY EXISTS            
*                                                                               
VR151A   MVI   SCRBOT,1            LOAD DELETE/QUIT MESSAGE                     
         CLI   LOCBIT,0            ADDING A NEW LOCATION                        
         BE    *+8                                                              
         MVI   SCRBOT,2            THEN LOAD DELETE/MOVE/QUIT MESSAGE           
         GOTO1 =A(SCREENS),DMCB,RR=RELO                                         
         LA    R2,PEMANSWH         POINT TO MESSAGE INPUT FIELD                 
         B     ERRPLS                                                           
*                                                                               
VR152    TM    DTLSTAT,DTSDEL      DELETE LOCATION                              
         BNO   VR160                                                            
VR155    GOTO1 =A(DELLOC),DMCB,RR=RELO                                          
         CLI   SCRBOT,0            IF NOT ZERO-MSG SCREEN LOADED                
         BNE   VR350               AND EXIT BACK TO GENCON                      
         B     VR190NX                                                          
VR160    GOTO1 =A(ELEM83),DMCB,RR=RELO    X'83' LOCATION ELEM                   
*                                                                               
VR190NX  DS    0H                                                               
*&&UK*&& NI    BIT,X'FF'-CURRLOC                                                
         LA    R4,DTLENQ(R4)       NEXT TABLE ENTRY                             
         LA    R1,DISTABND         END OF TABLE                                 
         CR    R4,R1                                                            
         BNL   VR200                                                            
         LA    R2,LLLEN(R2)        NEXT SCREEN LINE                             
         LA    R1,PEMENDH          LAST SCREEN LINE                             
         CR    R2,R1                                                            
         BNH   VR150                                                            
         NI    BIT3,X'FF'-DELDUP   HAVE WE ALREADY DELETED ONE DUP              
         OC    TSTDAY,TSTDAY                                                    
         BZ    VR200                                                            
         LA    R2,PEMTERMH                                                      
         CLI   5(R2),0                                                          
         JNE   VR200                                                            
         TM    PEMTERMH+4,X'40'    FIELD INPUT PREV                             
         BO    VR200                                                            
         LA    R3,DTADDLOC                                                      
T        USING DISTABD,R3                                                       
         OC    0(DTLENQ,R3),0(R3)                                               
         BNZ   *+8                                                              
         LA    R3,DTLENQ(R3)      GET LAST LOCATION                             
         CLI   T.DTSTATUS,LOCSTRM                                               
         BNE   VR200                                                            
         MVC   TERMDT,T.DTENDATE                                                
*                                                                               
         DROP  R4,R2,T                                                          
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'56' ELEM ON PERSON REC WITH MOST RECENT LOCATION              
***********************************************************************         
*                                                                               
VR200    LA    R4,DTADDLOC         TOP OF TABLE                                 
VR202    OC    0(DTLENQ,R4),0(R4)                                               
         BZ    VR202NX                                                          
         GOTO1 =A(ELEM56),DMCB,RR=RELO  UPDATE X'56' EMP HISTORY ELEM           
         B     VR250                                                            
VR202NX  LA    R4,DTLENQ(R4)                                                    
         LA    R1,DISTABND         END OF TABLE                                 
         CR    R4,R1                                                            
         BL    VR202                                                            
*                                                                               
*                                                                               
         USING DISTABD,R4                                                       
VR250    LA    R4,DTADDLOC         UPDATE RECORD STATUS AREA                    
VR252    OC    0(DTLENQ,R4),0(R4)                                               
         BZ    VR254                                                            
*                                                                               
*        USING PERRECD,R6                                                       
*        L     R6,AIO              SEE PERKSTA BELOW                            
*        MVC   PERRSTA+1(5),DTFILT1                                             
*        B     VR300                                                            
*        DROP  R6                                                               
*                                                                               
VR254    AHI   R4,DTLENQ                                                        
         LA    R1,DISTABND         END OF TABLE                                 
         CR    R4,R1                                                            
         BL    VR252                                                            
         DROP  R4                                                               
*                                                                               
*                                                                               
VR300    CLI   ACTEQU,ACTCHA       ACTION CHANGE?                               
         BNE   VR305                                                            
         LA    R1,PEMSELH          FORCE CURSOR TO SELECT LINE                  
         ST    R1,ACURFORC                                                      
*                                                                               
VR305    NI    CHKSTAT,X'FF'-DELELOC                                            
*                                                                               
         USING ACTRECD,R6                                                       
VR350    TM    BIT4,POINTERS       ADD POINTER?                                 
         BNO   VR360                                                            
         GOTO1 =A(ADDPTREL),DMCB,AIO,RAPKRPER,RR=RELO                           
*                                                                               
VR360    CLI   ACTEQU,ACTADD       ADD ACTION?                                  
         BNE   VRX                                                              
         TM    BIT4,AUTOHIST       AUTO ADD HISTORY ENABLED?                    
         BZ    VRX                                                              
         L     RF,=A(GETPCAM)      GET PAY CODE AND MONTHS                      
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         BNE   VRX                                                              
         L     RF,=A(ADDHIST)      AUTO ADD HISTORY                             
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         BE    VRX                                                              
         DC    H'0'                                                             
*                                                                               
VRX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        UPDATE 1R RECORDS AND PASSIVE POINTERS                                 
***********************************************************************         
*                                                                               
XP       GOTO1 =A(PASSIVE),DMCB,RR=RELO       UPDATE PASSIVE KEYS               
*                                                                               
         TM    BIT4,POINTERS                                                    
         BNO   XP10                                                             
*                                                                               
         GOTO1 =A(ADDRAPTR),DMCB,AIO,RAPKRPER,RR=RELO                           
*                                                                               
         USING DISTABD,R4          TABLE                                        
XP10     OI    BIT,CURRLOC         START WITH CURRENT LOCATION                  
*                                                                               
         LA    R4,DTADDLOC         FIRST IN TABLE                               
XP20     OC    0(DTLENQ,R4),0(R4)                                               
         BZ    XP60                                                             
*                                                                               
XP22     TM    DTLSTAT,DTSDEL      DELETE THIS LOCATION                         
         BNO   XP24                                                             
         GOTO1 =A(DEL1R),DMCB,RR=RELO                                           
         B     XP55NX                                                           
XP24     TM    DTSTAT2,LOCSDUP                                                  
         BO    XP60                DO NOT UPDATE 1R FOR DUP LOCS                
*                                                                               
         L     RE,AIO              PRE-CLEAR I/O                                
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
*&&US*&& NI    BIT,X'FF'-NEWREC                                                 
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,BIGKEY           DOES 1R RECORD EXIST                         
         XC    BIGKEY,BIGKEY                                                    
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVI   ACTKUNT,C'1'        UNIT 1                                       
         MVI   ACTKLDG,C'R'        LEDGER R                                     
         MVC   ACTKACT,DTACCNT                                                  
*        MVC   ACTKSAF1(L'DTFILTS),SPACES   PRE-CLEAR FILTERS IN KEY            
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'      PASS BACK DELETED RECS                        
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   XP26                                                             
         LA    R6,BIGKEY                                                        
*&&US                                                                           
         TM    ACTKSTAT,X'80'      IS RECORD MARKED DELETED                     
         BNO   *+12                                                             
         OI    BIT,RESREC          MARK RECORD RESTORED                         
*&&                                                                             
         NI    ACTKSTAT,X'FF'-X'80' UNDELETE IF DELETED                         
*        NI    ACTKSTAT,X'FF'-(X'80'+ACTSLOCK) UNDELETE IF DELETED              
*        TM    DTATTR,LOCALOCK     ACCOUNT IS LOCKED                            
*        BNO   *+8                                                              
*        OI    ACTKSTAT,ACTSLOCK                                                
         MVC   ACTKSAF1,DTFILT1    UPDATE FILTERS                               
         MVC   ACTKSAF2,DTFILT2    IN STATUS OF KEY                             
         MVC   ACTKSAF3,DTFILT3                                                 
         MVC   ACTKSAF4,DTFILT4                                                 
         MVC   ACTKSAF5,DTFILT5                                                 
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         NI    ACTRSTAT,X'FF'-X'80' UNDELETE IF DELETED                         
*        NI    ACTRSTAT,X'FF'-(X'80'+ACTSLOCK) UNDELETE IF DELETED              
*        TM    DTATTR,LOCALOCK     ACCOUNT IS LOCKED                            
*        BNO   *+8                                                              
*        OI    ACTRSTAT,ACTSLOCK                                                
         NI    BIT,X'FF'-NEWREC                                                 
         MVC   ACTRSAF1,DTFILT1    UPDATE FILTERS                               
         MVC   ACTRSAF2,DTFILT2    IN STATUS OF KEY                             
         MVC   ACTRSAF3,DTFILT3                                                 
         MVC   ACTRSAF4,DTFILT4                                                 
         MVC   ACTRSAF5,DTFILT5                                                 
         B     XP28                                                             
*                                                                               
XP26     LA    R6,BIGKEY                                                        
         L     R6,AIO              SET UP KEY TO ADD 1R                         
         MVC   0(L'ACTKEY,R6),KEYSAVE                                           
         MVC   ACTRLEN,=AL2(ACTRFST-ACTKEY)                                     
         MVC   ACTRSAF1,DTFILT1    UPDATE FILTERS                               
         MVC   ACTRSAF2,DTFILT2    IN STATUS OF KEY                             
         MVC   ACTRSAF3,DTFILT3                                                 
         MVC   ACTRSAF4,DTFILT4                                                 
         MVC   ACTRSAF5,DTFILT5                                                 
         OI    ACTRSTAT,ACTSABLP                                                
*        TM    DTATTR,LOCALOCK     ACCOUNT IS LOCKED                            
*        BNO   *+8                                                              
*        OI    ACTRSTAT,ACTSLOCK                                                
         OI    BIT,NEWREC                                                       
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        STATUS SCREEN UPDATES DIFFERENT ELEMENTS                               
***********************************************************************         
*                                                                               
XP28     TM    BIT4,POINTERS       ADD POINTER?                                 
         BNO   XP29                                                             
         GOTO1 =A(ADDPTREL),DMCB,AIO,RAPKR1RD,RR=RELO ADD 14 ELEM               
*                                                                               
         L     R6,AIO3                                                          
         MVC   LRAPBLK,0(R6)                                                    
*                                                                               
XP29     GOTO1 =A(ELEM30),DMCB,RR=RELO UPDATE STATUS ELEM                       
         GOTO1 =A(ELEM56),DMCB,RR=RELO EMPLOYEE HISTORY ELEMENT                 
*&&US*&& GOTO1 =A(ELEME5),DMCB,RR=RELO BRANDO DATE ELEMENT                      
         CLI   RECNUM,RTPR2        ON STATUS SCREEN                             
         BNE   XP30                                                             
         MVC   TEMPOFF,DTOFFICE    SET UP OFFICE FOR NEWSWO READCAL CAL         
         MVC   ACCNT,DTACCNT       SET UP ACCOUNT FOR NEWSWO CHKTIM CAL         
         LA    R1,PESCODEH         MAKE SURE CURSOR POSITION IS SET!            
         ST    R1,ACURFORC                                                      
         GOTO1 =A(ELEM2C),DMCB,RR=RELO    UPDATE SPEC POSINTG ACCNT EL          
         GOTO1 =A(NEWSWO),DMCB,RR=RELO    WAS SWITCHON DATE ENTERED             
         GOTO1 =A(ELEM22),DMCB,RR=RELO    UPDATE ADDRESS ELEM                   
         GOTO1 =A(ELEM3F),DMCB,RR=RELO    UPDATE ON LINE MEMO ELEM              
         B     XP50                                                             
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'5A' AND X'20' NAME ELEMENTS FOR CURRENT LOCATION ONLY         
***********************************************************************         
*                                                                               
XP30     TM    BIT,NEWREC          IF ADDING REC THEN UPDATE NAMES              
         BO    XP32                                                             
         TM    BIT,CURRLOC         ONLY UPDATE FOR MOST CURRENT LOC             
         BNO   XP40                                                             
*        BRAS  RE,FLDSEC           SECURITY TO DISPLAY NAME                     
*        BNE   XP40                                                             
XP32     DS    0H                                                               
*&&UK*&& GOTO1 =A(ELEM20UK),DMCB,RR=RELO     UPDATE '20' FOR UK                 
*&&UK*&& B     XP35                                                             
*                                                                               
         LA    R2,PEMLNAMH         A(LAST NAME)                                 
         LA    R3,PEMFNAMH         A(FIRST NAME)                                
*&&US*&& GOTO1 ELM20,DMCB,(5(R2),PEMLNAM),(5(R3),PEMFNAM)                       
*                                                                               
XP35     GOTO1 =A(ELEM5A),DMCB,RR=RELO       X'5A' NAME ELEMENTS                
         EJECT                                                                  
***********************************************************************         
*        ADD/UPDATE ELEMENTS                                                    
***********************************************************************         
*                                                                               
XP40     GOTO1 =A(ELEM32),DMCB,RR=RELO  ADD BALANCE ELEM IF NEW REC             
         GOTO1 =A(ELEM33),DMCB,RR=RELO  ADD PEEL ELEM IF NEW REC                
         EJECT                                                                  
***********************************************************************         
*        ADD/WRITE RECORD AND GET NEXT TABLE ENTRY                              
***********************************************************************         
*                                                                               
XP50     NI    BIT,X'FF'-CURRLOC   NOT CURRENT LOCATION ANYMORE                 
         TM    BIT,NEWREC                                                       
         BO    XP52                                                             
         GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
* UPDATE NAME SEARCH AND NAME CHANGE POINTER                                    
         GOTO1 =A(CHKNAM),DMCB,RR=RELO                                          
         GOTO1 =A(PUTSRC),DMCB,RR=RELO                                          
*       - - - - - - - - -                                                       
         OC    PIDNUM,PIDNUM       IS THERE A PIDNUM GIVEN?                     
         BZ    XP55NX              NO                                           
         CLC   OLDPID,PIDNUM       IS THERE A CHANGE IN PIDNUM?                 
         BE    XP55NX              NO                                           
         GOTO1 =A(UPTXREF),DMCB,RR=RELO   TIMESHEET TEMPO X-REF RECORDS         
*       - - - - - - - - -                                                       
         B     XP55NX                                                           
XP52     GOTO1 ADDREC                                                           
* ADD NAME SEARCH POINTER                                                       
         GOTO1 =A(PUTSRC),DMCB,RR=RELO                                          
*                                                                               
XP55NX   TM    BIT4,POINTERS       ADD POINTER?                                 
         BNO   XP60                                                             
*                                                                               
         USING RAPPERD,R6                                                       
         L     R6,AIO3                                                          
         MVC   0(L'LRAPBLK,R6),LRAPBLK                                          
*                                                                               
         CLC   RAPCPY,CMPY            CHECK THESE FIELDS TO MAKE SURE           
         BNE   XP60                   BLOCK INFO IS STILL VALID                 
         CLC   RAPACOM,ACOMFACS       IF NOT SKIP ADDING POINTER AS IT          
         BNE   XP60                   WILL NOT BE VALID                         
         CLI   RAPEMU,C'N'                                                      
         BNE   XP60                                                             
*                                                                               
         XC    RAPRDA,RAPRDA                                                    
         MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 =A(ADDRAPTR),DMCB,AIO,RAPKR1RD,RR=RELO ADD FA RAPPER REC         
         DROP  R6                                                               
*                                                                               
XP60     LA    R4,DTLENQ(R4)       NEXT TABLE ENTRY                             
         LA    R1,DISTABND         END OF TABLE                                 
         CR    R4,R1                                                            
         BL    XP20                                                             
         CLI   RECNUM,RTPR2        STATUS SCREEN ONLY UPDATES 1 1R REC          
         BE    XP70                                                             
         GOTOR UPDTHR              UPDATE THIRD PARTY PROFILE SETTINGS          
         B     XPX                                                              
*                                  ARE WE UPDATING THE PROFILE RECORD?          
XP70     TM    SWTCHBI2,UPDPPR1+UPDPPR2+UPDPPR3                                 
         BZ    XPX                                                              
         GOTOR UPDPROF                                                          
*                                                                               
XPX      NI    DMINBTS,X'FF'-X'08'                                              
         BRAS  RE,DR                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ON-SCREEN LIST                                                         
***********************************************************************         
*                                                                               
LR       OI    GENSTAT2,DISTHSPG                                                
         MVI   SCRBOT,0            MESSAGE SCREEN NOT VALID W/LIST              
         NI    BIT3,X'FF'-OPTVK                                                 
         MVI   NLISTS,14                                                        
         MVC   SVPERCDE,SPACES                                                  
         GOTO1 =A(VALOPTS),DMCB,RR=RELO                                         
         MVC   PELNAME,AC@NAME                                                  
         OI    PELNAMEH+6,X'80'                                                 
         TM    OPTSTAT,ALTDATE                                                  
         BZ    LR00                                                             
         MVC   PELNAME,AC@STEND                                                 
LR00     CLC   SVOPTION,OPTSTAT    HAVE THE OPTIONS CHANGED?                    
         BE    LR01                NO                                           
         MVC   SVOPTION,OPTSTAT                                                 
         OI    BIT3,OPTVK                                                       
         B     VK                                                               
*                                                                               
         USING PERRECD,R6                                                       
LR01     LA    R6,BIGKEY                                                        
*                                                                               
         USING LSTLINED,R2                                                      
         LA    R2,LISTAR           LINE DSECT                                   
*                                                                               
         TM    BIT2,STARTAT        START AT FILTER                              
         BO    LR02                                                             
         OC    BIGKEY,BIGKEY       FIRST TIME THROUGH?                          
         BNZ   LRHI                                                             
*                                                                               
LR02     MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    PERSON RECORD TYPE                           
         MVC   PERKCPY,CMPY        COMPANY                                      
         MVC   PERKCODE,STRTCODE   PERSON CODE START AT                         
*                                                                               
LRHI     GOTO1 HIGH                                                             
         B     LR10                                                             
LRSEQ    GOTO1 SEQ                                                              
LR10     CLI   DMCB+8,0                                                         
         BE    LR12                                                             
         DC    H'0'                                                             
LR12     GOTO1 GETREC                                                           
         CLC   BIGKEY(2),SAVEKEY   SAME RECORD TYPE AND COMPANY?                
         BNE   LRX                 NO MORE DATATYPES TO LIST                    
*                                                                               
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-          
*        CHECK TO SEE IF MEETS OPTION REQUIREMENT  (TERM)                       
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-          
*                                                                               
         L     R6,AIO                                                           
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         MVC   SVPERCDE,PERKCODE                                                
         USING EMPELD,R6                                                        
         TM    OPTSTAT,NOTERM+ONLYTERM  NO NEED TO CHECK-LIST ALL               
         BZ    LR14                                                             
         MVI   ELCODE,EMPELQ       X'56' EMPLOYEE HISTORY ELEM                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         OC    EMPTRM,EMPTRM       IS THERE A TERMINATION DATE?                 
         BZ    LR13                                                             
         TM    OPTSTAT,ONLYTERM    TERM=ONLY                                    
         BO    LR14                YES-CONTINUE                                 
         B     LRSEQ               NO-GET NEXT                                  
*                                                                               
LR13     TM    OPTSTAT,NOTERM      TERM=NO                                      
         BO    LR14                YES-CONITNUE                                 
         B     LRSEQ               NO-GET NEXT                                  
         EJECT                                                                  
*                                                                               
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-          
*        CHECK TO SEE IF MEETS OPTION REQUIREMENT  (LOA + ACTIVE)               
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-          
*                                                                               
         USING EMPELD,R6                                                        
LR14     L     R6,AIO              ANY LOA/ACTIVE FILTER SET?                   
         TM    OPTSTA2,NOLOA+ONLYLOA+NOACT+ONLYACT                              
         BZ    LR15                                                             
         MVI   ELCODE,EMPELQ       X'56' EMPLOYEE HISTORY ELEM                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         TM    OPTSTA2,NOLOA                                                    
         BZ    LR14A                                                            
         CLI   EMPCSTAT,EMPCLOA                                                 
         BE    LRSEQ                                                            
LR14A    TM    OPTSTA2,ONLYLOA                                                  
         BZ    LR14B                                                            
         CLI   EMPCSTAT,EMPCLOA                                                 
         BNE   LRSEQ                                                            
LR14B    TM    OPTSTA2,NOACT                                                    
         BZ    LR14C                                                            
         CLI   EMPCSTAT,EMPCLOA                                                 
         BE    LR14C                                                            
         CLI   EMPCSTAT,EMPCTRM                                                 
         BNE   LRSEQ                                                            
LR14C    TM    OPTSTA2,ONLYACT                                                  
         BZ    LR14D                                                            
         CLI   EMPCSTAT,EMPCLOA                                                 
         BE    LRSEQ                                                            
         CLI   EMPCSTAT,EMPCTRM                                                 
         BE    LRSEQ                                                            
LR14D    DS    0H                                                               
         EJECT                                                                  
*                                                                               
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-          
*        CHECK TO SEE IF LOCATION FILTER MATCHES                                
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-          
*                                                                               
LR15     CLC   OPTF15,SPACES       ANY FILTER?                                  
         BE    LR16                                                             
         USING LOCELD,R6                                                        
         L     R6,AIO              FIND LATEST LOCATION                         
         MVI   ELCODE,LOCELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   LRSEQ                                                            
         MVC   DUB(3),LOCSTART                                                  
         LR    R1,R6                                                            
LR15A    LLC   R0,LOCLN                                                         
         AR    R6,R0                                                            
         CLI   LOCEL,0                                                          
         BE    LR15B                                                            
         CLI   LOCEL,LOCELQ                                                     
         BNE   LR15A                                                            
         CLC   LOCSTART,DUB                                                     
         BNH   LR15A                                                            
         MVC   DUB(3),LOCSTART                                                  
         LR    R1,R6                                                            
         B     LR15A                                                            
LR15B    LR    R6,R1               GET AND TEST 1R ACCOUNT                      
         GOTO1 =A(TST1RF15),DMCB,RR=RELO                                        
         BNE   LRSEQ                                                            
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-          
*        CHECK TO SEE IF MEETS OPTION REQUIREMENT  (PID)                        
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-          
*                                                                               
LR16     CLC   OPTPID,SPACES       IS THERE A PID FILTER                        
         BE    LR20                NO-CONTINUE                                  
*                                                                               
         USING PIDELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,PIDELQ       X'D8' PERSON ID ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   LRSEQ                                                            
         TM    OPTSTAT,PIDNOOPT    FILTERING ON PID#                            
         BZ    LR17                                                             
         CLC   PIDNO,OPTPID        MATCH                                        
         BNE   LRSEQ               NO                                           
         TM    OPTSTAT,PIDOPT      CHECKING FOR PID NAME TOO                    
         BZ    LR20                NO                                           
*                                                                               
LR17     OI    BIT3,PIDCHK                                                      
         MVC   PIDNUM,PIDNO        FILL IN PID NUMBER                           
         MVC   PIDNAME,SPACES      CLEAR PID NAME                               
         GOTO1 =A(GETPIDNM),DMCB,RR=RELO   GET PID INFO                         
         NI    BIT3,X'FF'-PIDCHK                                                
         SR    R1,R1               RF=LENGTH OF PID FILTER                      
         LA    RE,OPTPID                                                        
LR18     CLI   0(RE),X'40'                                                      
         BE    LR19                                                             
         LA    R1,1(R1)           BUMP LENGTH                                   
         CHI   R1,8               AND COMPARE TO MAX                            
         BNL   LR19                                                             
         LA    RE,1(RE)           BUMP POINTER TO NAME                          
         B     LR18                                                             
*                                                                               
LR19     LTR   R1,R1                                                            
         BZ    LRSEQ                                                            
         BCTR  R1,0                                                             
         EXCLC R1,PIDNAME,OPTPID       MATCH?                                   
         BNE   LRSEQ               NO                                           
***********************************************************************         
*        PUT NAME IN LIST LINE                                                  
*        IF PASS FILTER AND SECURITY TEST                                       
***********************************************************************         
*                                                                               
LR20     LA    R3,WORK             BUILD NAME IN WORK                           
         MVC   WORK,SPACES                                                      
         USING GPNELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,GPNELQ       X'5A' GENERAL PURPOSE NAME ELEMENTS          
         BAS   RE,GETEL                                                         
         BNE   LR48                                                             
*                                                                               
         ST    R6,STADDR           NEED TO READ ELEMS AGAIN LATER               
         CLC   OPTNAME,SPACES      FILTER ON NAME                               
         BE    LR25                NO-PRINT ALL                                 
         SR    R1,R1               R1=LENGTH OF NAME FILTER                     
         LA    RE,OPTNAME                                                       
LR21     CLI   0(RE),X'40'                                                      
         BE    LR23                                                             
         LA    R1,1(R1)           BUMP LENGTH                                   
         CHI   R1,MAXNAME         AND COMPARE TO MAX                            
         BNL   LR23                                                             
         LA    RE,1(RE)           BUMP POINTER TO NAME                          
         B     LR21                                                             
*                                                                               
LR23     LTR   R1,R1                                                            
         BZ    LRSEQ                                                            
         LLC   RE,GPNLN                                                         
         AHI   RE,-4               3 FOR CODE,LEN,AND TYPE + 1 FOR EX           
         BM    LR25                                                             
         BCTR  R1,0                                                             
LR24     EXCLC R1,OPTNAME,GPNNME   MATCH ON FIRST OR LAST NAME?                 
         BE    LR25                YES                                          
         BAS   RE,NEXTEL                                                        
         BNE   LRSEQ                                                            
         B     LR24                                                             
*                                                                               
LR25     TM    OPTSTAT,ALTDATE     SHOW DATES INSTEAD OF NAME?                  
         BNZ   LR48                                                             
         BRAS  RE,FLDSEC           SECURITY TO DISPLAY NAME                     
         BNE   LR48                                                             
*                                                                               
LR29     L     R6,STADDR           RESET TO BEGINNING OF 1ST '5A' ELEM          
LR30     CLI   GPNEL,GPNELQ        GENERAL PURPOSE NAME ELEMENT?                
         BNE   LR37                                                             
         LLC   R1,GPNLN                                                         
         AHI   R1,-4               3 FOR CODE,LEN,AND TYPE + 1 FOR EX           
         BNM   LR31                                                             
         B     LR36                GET NEXT ELEMENT                             
*                                                                               
LR31     CLI   GPNTYP,GPNTLST                                                   
         BE    LR32                                                             
         CLI   GPNTYP,GPNTFST      DID WE LOOK AT BOTH FIRST AND LAST           
         BE    LR34                YES, SO DONE                                 
         B     LR36                                                             
*                                                                               
LR32     MVC   33(0,R3),GPNNME     STORE LAST NAME AT WORK+32                   
         EX    R1,*-6                                                           
         STC   R1,32(R3)                                                        
         B     LR36                                                             
*                                                                               
LR34     MVC   1(0,R3),GPNNME      STORE 1ST NAME AT WORK+0                     
         EX    R1,*-6                                                           
         STC   R1,0(R3)                                                         
         B     LR36                                                             
*                                                                               
LR36     BAS   RE,NEXTEL           GET FIRST NAME                               
         BE    LR30                                                             
*                                                                               
LR37     LA    R3,LSTNAME                                                       
         XR    RF,RF                                                            
         TM    CPXSTAT6,CPXCNAMO   CHANGING SORT ORDER?                         
         BZ    LR40                                                             
         OC    WORK(1),WORK        ANYTHING STORED?                             
         BNZ   *+12                                                             
         CLI   WORK+1,C' '                                                      
         BNH   LR38                                                             
         IC    RF,WORK                                                          
         MVC   0(0,R3),WORK+1      PUT FIRST NAME                               
         EX    RF,*-6                                                           
         AR    R3,RF                                                            
         AHI   R3,1                                                             
         MVI   0(R3),C','                                                       
         AHI   R3,2                1 FOR , THEN 1 MORE                          
LR38     OC    WORK+32(1),WORK+32  THEN LAST NAME                               
         BNZ   *+12                                                             
         CLI   WORK+32,C' '                                                     
         BNH   LR48                                                             
         IC    RF,WORK+32                                                       
         MVC   0(0,R3),WORK+33                                                  
         EX    RF,*-6                                                           
         B     LR48                                                             
*                                                                               
LR40     OC    WORK+32(2),WORK+32  PUT LAST NAME                                
         BNH   *+12                                                             
         CLI   WORK+32,C' '                                                     
         BNH   LR42                                                             
         IC    RF,WORK+32                                                       
         MVC   0(0,R3),WORK+33                                                  
         EX    RF,*-6                                                           
         AR    R3,RF                                                            
         AHI   R3,1                                                             
         MVI   0(R3),C','                                                       
         AHI   R3,2                1 FOR , THEN 1 MORE                          
*                                                                               
LR42     OC    WORK(1),WORK        THEN FIRST NAME                              
         BNZ   *+12                                                             
         CLI   WORK,C' '                                                        
         BNZ   LR48                                                             
         IC    RF,WORK                                                          
         MVC   0(0,R3),WORK+1                                                   
         EX    RF,*-6                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        PUT START/END DATES INTO NAME COLUMN                                   
***********************************************************************         
*                                                                               
LR48     TM    OPTSTAT,ALTDATE                                                  
         BZ    LR50                                                             
         USING EMPELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,EMPELQ       X'56' EMPLOYEE HISTORY ELEM                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         OC    EMPHIR,EMPHIR                                                    
         BZ    LR48A                                                            
         GOTO1 DATCON,DMCB,(1,EMPHIR),(21,LSTDATS+0)                            
         MVI   LSTDATS+10,C'-'                                                  
         SPACE 1                                                                
LR48A    OC    EMPTRM,EMPTRM                                                    
         BZ    LR48B                                                            
         GOTO1 DATCON,DMCB,(1,EMPTRM),(21,LSTDATS+11)                           
         MVI   LSTDATS+10,C'-'                                                  
         SPACE 1                                                                
LR48B    DS    0H                  (ALL DONE)                                   
         EJECT                                                                  
***********************************************************************         
*        PUT CURRENT LOCATION IN LIST LINE                                      
***********************************************************************         
*                                                                               
         USING LOCELD,R6                                                        
LR50     MVC   LSTCODE,SVPERCDE    PUT PERSON CODE IN LIST LINE                 
         NI    BIT2,X'FF'-OFFICEOK                                              
         L     R6,AIO                                                           
         MVI   ELCODE,LOCELQ       STAFF LOCATION ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   LR100                                                            
LR60     ST    R6,STADDR           KEEP TRACK OF WHERE TO START                 
         TM    BIT2,OFFICEOK       IS THERE AT LEAST 1 OFFICE                   
         BO    LR90                THAT THE USER HAS ACCESS TO                  
*                                                                               
         USING OFFALD,R3                                                        
LR65     L     R3,AOFFBLK                                                       
         TM    BIT3,ONEBYTOF                                                    
         BZ    LR66                                                             
         OC    OFFAOLDA,OFFAOLDA   OLD OFFICE LIMIT ACCESS?                     
         BNZ   LR67                                                             
         OI    BIT2,OFFICEOK       NO-OKAY TO LIST                              
         B     LR90                                                             
LR66     OC    OFFANEWA,OFFANEWA   NEW OFFICE LIMIT ACCESS                      
         BNZ   LR67                                                             
         OI    BIT2,OFFICEOK       NO-OKAY TO LIST                              
         B     LR90                                                             
*                                                                               
LR67     LA    R3,OFFAWORK         LIST OF VALID OFFICES                        
         TM    BIT3,ONEBYTOF       AGENCY HAS ONE BYTE OFFICES?                 
         BO    *+16                # OF OFFICES NOT IN LIST                     
         LH    RF,0(R3)            RF=# OF OFFICES IN LIST                      
         LA    R3,2(R3)            BUMP PAST THIS                               
         B     *+8                                                              
         LA    RF,OLDOFLMX*2       MAX # IN LIST*2 (TWO PAGES)                  
         CLI   OFFDISP,0           IS OFFICE IN 1ST POSITION?                   
         BH    LR70                                                             
         LA    R1,LOCOFF           OFFICE IS IN RIGHT POS                       
         LLC   RE,LN1RLEV1                                                      
         B     *+14                                                             
LR70     LLC   RE,LN1RLEV2         MUST BE IN 2ND POSITION                      
         LA    R1,LOCDEPT          OFFICE IS IN 2ND POS                         
         BCTR  RE,0                                                             
LR80     EXCLC RE,0(R1),0(R3)     IS OFFICE FROM '83' IN LIST                   
         BNE   *+12                                                             
         OI    BIT2,OFFICEOK       YES-NO NEED TO GO ON                         
         B     LR90                                                             
         LA    R3,1(RE,R3)         BUMP TO NEXT ENTRY IN LIST                   
         TM    BIT3,ONEBYTOF       ONE BYTE OFFICES                             
         BO    *+12                MUST CHECK DIFFERENTLY                       
         BCT   RF,LR80                                                          
         B     LR90                                                             
         CLI   0(R3),0             CHECK END OF LIST                            
         BE    LR90                                                             
         BCT   RF,LR80                                                          
*                                                                               
LR90     BAS   RE,NEXTEL           WANT LAST ELEMENT                            
         BE    LR60                DISPLAY THE LAST DATE                        
         TM    BIT2,OFFICEOK       IS IT OKAY TO LIST THIS PERSON               
         BO    *+14                                                             
         MVC   LISTAR,SPACES       CLEAR STUFF I ALREADY PUT IN LINE            
         B     LRSEQ               GET NEXT PERSON                              
*                                                                               
         L     R6,STADDR           ADDRESS OF CURRENT LOCATION                  
         OC    OFFFILT,OFFFILT     OFFICE FILTER                                
         BZ    *+14                                                             
         CLC   OFFFILT,LOCOFF                                                   
         BNE   LRSEQ                                                            
         OC    DEPTFILT,DEPTFILT   DEPARTMENT FILTER?                           
         BZ    *+14                                                             
         CLC   DEPTFILT,LOCDEPT                                                 
         BNE   LRSEQ                                                            
         OC    SUBFILT,SUBFILT     SUB DEPARTMENT FILTER?                       
         BZ    *+14                                                             
         CLC   SUBFILT,LOCSUB                                                   
         BNE   LRSEQ                                                            
*                                                                               
         MVC   LSTOFF,LOCOFF       OK TO LIST                                   
         MVC   LSTDEPT,LOCDEPT                                                  
         MVC   LSTSUB,LOCSUB                                                    
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        LIST LINE                                                              
***********************************************************************         
*                                                                               
LR100    GOTO1 LISTMON             SEND RECORD TO SCREEN                        
         B     LRSEQ               NEXT RECORD                                  
LRX      B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        ERROR MESSAGES                                                         
***********************************************************************         
*                                  GENERAL MESSAGES                             
ERRPLS   MVI   GERROR1,2                                                        
         MVI   GMSGTYPE,C'I'                                                    
         B     ERRX                                                             
ERRINV   MVI   GERROR1,INVALID                                                  
         B     ACCERRX                                                          
*                                  ACC ERROR MESSAGES                           
ERRMISS  MVC   GERROR,=AL2(ACEMISS)                                             
         B     ACCERRX                                                          
E1UPD    MVC   GERROR,=AL2(ACEONLY1)   CAN ONLY UPDATE DATES ON 1 LOC           
         B     ACCERRX                                                          
EVALCHA  MVC   GERROR,=AL2(ACEVALCH)   ONLY VALID WITH ACTION CHANGE            
         B     ACCERRX                                                          
EDTTERM  MVC   GERROR,=AL2(ACEDTTRM)   DATE AFTER TERM DATE                     
         B     ACCERRX                                                          
EDPOST   MVC   GERROR,=AL2(ACEDPOST)                                            
         B     ACCERRX                                                          
EDSALS   MVC   GERROR,=AL2(ACEDSALS)                                            
         B     ACCERRX                                                          
EDLOCS   MVC   GERROR,=AL2(ACEDLOCS)                                            
         B     ACCERRX                                                          
EOVRHED  MVC   GERROR,=AL2(ACENOHED)                                            
         B     ACCERRX                                                          
ESTHIRE  MVC   GERROR,=AL2(ACESTHRD)                                            
         B     ACCERRX                                                          
ENDAFTST MVC   GERROR,=AL2(ACEENAST)     END AFTER NEXT START                   
         B     ACCERRX                                                          
ESTEND   MVC   GERROR,=AL2(ACENDBST)                                            
         B     ACCERRX                                                          
ETOOSHRT MVC   GERROR,=AL2(ACELSHO)                                             
         B     ACCERRX                                                          
ETOOLONG MVC   GERROR,=AL2(ACELLONG)                                            
         B     ACCERRX                                                          
EINVOPT  MVC   GERROR,=AL2(ACEINVOP)                                            
         B     ACCERRX                                                          
EINVPER  MVC   GERROR,=AL2(ACEIVPER)                                            
         B     ACCERRX                                                          
EINVDATE MVC   GERROR,=AL2(ACEIVDTE)                                            
         B     ACCERRX                                                          
EINVSTEN MVC   GERROR,=AL2(ACEINSTE)                                            
         B     ACCERRX                                                          
EINVTIM  MVC   GERROR,=AL2(ACEINTIM)                                            
         B     ACCERRX                                                          
EINVCAL  MVC   GERROR,=AL2(ACENOCAL)                                            
         B     ACCERRX                                                          
EINVDELN MVC   GERROR,=AL2(ACDELETN)                                            
         B     ACCERRX                                                          
ERRNOUP  MVC   GERROR,=AL2(ACENOUPD)                                            
         B     ACCERRX                                                          
EMISHIGH MVC   GERROR,=AL2(ACEHIGH)                                             
         B     ACCERRX                                                          
EINVACCT MVC   GERROR,=AL2(ACEACCT)                                             
         B     ACCERRX                                                          
ERRLOC   MVC   GERROR,=AL2(ACEONLOC)                                            
         B     ACCERRX                                                          
EINVSECL MVC   GERROR,=AL2(ACSELOCK)                                            
         B     ACCERRX                                                          
EDFADJ   MVC   GERROR,=AL2(ACEFADJS)                                            
         B     ACCERRX                                                          
ERREC2BG MVC   GERROR,=AL2(ACREC2BG)                                            
         B     ACCERRX                                                          
ERRIGHT  MVC   GERROR,=AL2(ACERIGHT)                                            
         B     ACCERRX                                                          
ERRHIRE  MVC   GERROR,=AL2(ACEHIRE)                                             
         B     ACCERRX                                                          
ERRSPOST MVC   GERROR,=AL2(ACESPOST)                                            
         B     ACCERRX                                                          
ERRACBAL MVC   GERROR,=AL2(ACEACBAL)                                            
         B     ACCERRX                                                          
ERRDLOC  MVC   GERROR,=AL2(ACEDELOC)                                            
         B     ACCERRX                                                          
EHISTLMT MVC   GERROR,=AL2(ACEHSTLM)                                            
         B     ACCERRX                                                          
ERRSVTMS MVC   GERROR,=AL2(ACESVTMS)                                            
         B     ACCERRX                                                          
ERRDRAFT MVC   GERROR,=AL2(ACEDRTIM)                                            
         B     ACCERRX                                                          
ERRMLOCK MVC   GERROR,=AL2(ACEMOLCK)                                            
         B     ACCERRX                                                          
EINVLOC  MVC   GERROR,=AL2(ACEIVLOC)                                            
         B     ACCERRX                                                          
ENDHIRE  MVC   GERROR,=AL2(ACEENDHI) END DATE HIGHER THAN LOCATION END          
         B     ACCERRX                                                          
ERRCONF  MVC   GERROR,=AL2(ACEDCONF) CONFLICTING DATES                          
         B     ACCERRX                                                          
ERREXPD  MVC   GERROR,=AL2(ACECDECL) CANNOT DELETE EXPENSES EXIST               
         B     ACCERRX                                                          
ERREXPC  MVC   GERROR,=AL2(ACECCLEC) CANNOT CHANGE EXPENSES EXIST               
         B     ACCERRX                                                          
ERRNAME  MVC   GERROR,=AL2(ACESNAME) NAMES DON'T MATCH WITH =SEC                
         B     ACCERRX                                                          
ERRENDDA MVC   GERROR,=AL2(ACEENDDA) PLEASE ENTER AN END DATE                   
         B     ACCERRX                                                          
EINVOFC  MVC   GERROR,=AL2(ACEIVOF)  INVALID OFFICE                             
         B     ACCERRX                                                          
*&&US                                                                           
ERRALTDM MVC   GERROR,=AL2(ACEALTDM) ACTIVE LOC/TERM DATE MIXMATCH              
         B     ACCERRX                                                          
ERRLOCBT MVC   GERROR,=AL2(ACELOCBT) CANT TERM MIDPERIOD                        
         B     ACCERRX                                                          
*&&                                                                             
ERRTERM  MVC   GERROR,=AL2(ACETERM)  TERM DATE BEFORE HIRE DATE                 
         B     ACCERRX                                                          
ERRSTDT  MVC   GERROR,=AL2(ACEMSTDT) START DATE MISSING                         
         B     ACCERRX                                                          
ERRENDT  MVC   GERROR,=AL2(ACEMENDT) END DATE MISSING                           
         B     ACCERRX                                                          
*                                                                               
ACCERRX  MVI   GMSYS,6             ACC MESSAGE SYSTEM                           
         B     *+8                                                              
ERRX     MVI   GMSYS,X'FF'         GENERAL MESSAGE SYSTEM                       
         GOTO1 MYERR                                                            
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND STUFF                                                    
***********************************************************************         
*                                                                               
XHIGH    CLI   *,0                                                              
         B     XIT                                                              
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
NINES    DC    CL3'999'                                                         
CTDISP   DC    Y(L'SAPEKEY+L'SAPELEN+L'SAPESTAT)                                
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         GETEL2 R6,CTDISP,ELCODE                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DATA DICTIONARY                                                        
***********************************************************************         
*                                                                               
DDNMERR  DCDD  AC#SECNM,37         SECURITY NAME IS                             
DDPWARN  DCDD  AC#PWARN,45         OID NAME WARNING                             
DDTRMNT  DCDDL AC#TRM,4            TERM                                         
DDLOA    DCDDL AC#LOA,4            LOA                                          
DDACT    DCDDL AC#ACTV,4           ACTIVE                                       
DDALT    DCDDL AC#ALT,3            ALT                                          
DDNODET  DCDD  AC#NODET,20         NO DETAILS AVAILABLE                         
DDFTSAL1 DCDDL AC#CFTS1,24         FUTURE SALARY EXISTS -                       
DDFTSAL2 DCDDL AC#CFTS2,44         DO YOU WANT TO DEL OR QUIT?                  
DDFTSAL3 DCDDL AC#CFTS3,51         DO YOU WANT TO DEL, MOVE OR QUIT?            
*                                                                               
* INPUT/OUTPUT FOR DICTATE CALL                                                 
*                                                                               
PERLISTI DS    0C                  DATA DICTIONARY INPUT                        
         DCDDL AC#NAME,4           NAME                                         
         DCDDL AC#COP17,3          PID                                          
         DCDDL AC#CPIDN,4          PID#                                         
         DCDDL AC#CMOVE,8          M..M=MOVE                                    
         DCDDL AC#CDEL,8           D..D=DELETE                                  
         DCDDL AC#CQUIT,8          Q..Q=QUIT                                    
         DCDDL AC#ACTV,8           ACTIVE                                       
         DCDDL AC#TRM,8            TERM                                         
         DCDDL AC#XFR,8            TRANSFER                                     
         DCDDL AC#STEND,20         START/END DATES                              
         DC    X'00'                                                            
*                                                                               
PERLISTU DS    0C                  DATA DICTIONARY OUTPUT                       
AC@NAME  DS    CL4                                                              
AC@PID   DS    CL3                                                              
AC@PIDNO DS    CL4                                                              
AC@CMOVE DS    CL8                                                              
AC@CDEL  DS    CL8                                                              
AC@CQUIT DS    CL8                                                              
AC@ACTV  DS    CL8                                                              
AC@TRM   DS    CL8                                                              
AC@XFR   DS    CL8                                                              
AC@STEND DS    CL20                                                             
*&&DO                                                                           
PERLSTLI DS    0C                  DATA DICTIONARY INPUT (LOWER)                
         DCDDL AC#NAME,20          NAME                                         
         DC    X'00'                                                            
*                                                                               
PERLSTLU DS    0C                  DATA DICTIONARY OUTPUT (LOWER)               
ACLNAME  DS    CL20                                                             
ACLDATES DS    CL20                                                             
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
*        ATTRIBUTE TABLE                                                        
***********************************************************************         
*                                                                               
ATTRTAB  DC    AL2(PESEXECH-CONTAGH)      DISP TO FIELD HEADER                  
         DC    AL1(LOCAEXEC)              BIT ON FOR POSITIVE                   
         DC    AL2(PESLOCKH-CONTAGH)                                            
         DC    AL1(LOCALOCK)                                                    
         DC    AL2(PESPRODH-CONTAGH)                                            
         DC    AL1(LOCAPROD)                                                    
         DC    AL2(PESJOBH-CONTAGH)                                             
         DC    AL1(LOCAJOB)                                                     
         DC    AL2(PESACTH-CONTAGH)                                             
         DC    AL1(LOCAACT)                                                     
         DC    AL2(PESPCH-CONTAGH)                                              
         DC    AL1(LOCAPRTS)                                                    
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        STATUS TABLE                                                           
***********************************************************************         
*                                                                               
STATTAB  DS    0H                                                               
DDACTIVE DCDD  AC#ACTV,8           WORD/BYTE                                    
         DC    AL1(LOCSACT)                                                     
DDTERM   DCDD  AC#TRM,8                                                         
         DC    AL1(LOCSTRM)                                                     
DDXFR    DCDD  AC#XFR,8                                                         
         DC    AL1(LOCSTRAN)                                                    
         DCDD  AC#LOA,8                                                         
         DC    AL1(LOCSLOA)                                                     
         DCDD  AC#LEAVE,8                                                       
         DC    AL1(LOCSLOA)                                                     
*&&US*&& DCDD  AC#FGAP,8                                                        
*&&US*&& DC    AL1(LOCFRL)                                                      
         DCDD  AC#OTHER,8                                                       
         DC    AL1(LOCSOTH)                                                     
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        VALID SELECT CODES TABLE                                               
***********************************************************************         
*                                                                               
VSELTAB  DS    0H                                                               
         DCDD  AC#S,3              'S  '                                        
         DC    AL1(DTSSEL),C'N'                                                 
         DCDD  AC#DSPD,3           'DIS'                                        
         DC    AL1(DTSDIS),C'N'                                                 
         DCDD  AC#CHG,3            'CHA'                                        
         DC    AL1(DTSCHA),C'N'                                                 
         DCDD  AC#HIST,3           'HIS'                                        
         DC    AL1(DTSHIS),C'Y'                                                 
         DCDD  AC#CRATS,3          'ARA'                                        
         DC    AL1(DTSCRA),C'Y'                                                 
         DCDD  AC#UPD,3            'UPD'                                        
         DC    AL1(DTSUPD),C'N'                                                 
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        PFKEY TABLE DEFINITIONS - LIST SCREEN                                  
***********************************************************************         
*                                                                               
LPFTABLE DS    0C                                                               
*                                                                               
*        PERSON LAST (PREVIOUS PAGE)                                            
*                                                                               
         DC    AL1(LPF07X-*,07,PFTLIST,0,0)                                     
         DC    CL3' '                                                           
         DC    CL8' '                                                           
         DCDD  AC#LAST,8                                                        
LPF07X   EQU   *                                                                
*                                                                               
*        PERSON NEXT (NEXT PAGE)                                                
*                                                                               
         DC    AL1(LPF08X-*,08,PFTLIST,0,0)                                     
         DC    CL3' '                                                           
         DC    CL8' '                                                           
         DCDD  AC#NXT,8                                                         
LPF08X   EQU   *                                                                
*                                                                               
*        PERSON DISPLAY                                                         
*                                                                               
         DC    AL1(LPF02X-*,02,PFTCPROG,(LPF02X-LPF02)/KEYLNQ,0)                
         DCDD  AC#DSP,3                                                         
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#DSP,8                                                         
LPF02    DC    AL1(KEYTYCUR,L'LSTCODE-1),AL2(LSTCODE-LSTCODE)                   
LPF02X   EQU   *                                                                
*                                                                               
*        RATES DISPLAY                                                          
*                                                                               
         DC    AL1(LPF10X-*,10,PFTCPROG,(LPF10X-LPF10)/KEYLNQ,0)                
         DCDD  AC#CRATS,3                                                       
         DCDD  AC#CRATS,8                                                       
         DCDD  AC#DSP,8                                                         
LPF10    DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYCUR,L'LSTCODE-1),AL2(LSTCODE-LSTCODE)                   
LPF10X   EQU   *                                                                
*                                                                               
*        HISTORY LIST                                                           
*                                                                               
         DC    AL1(LPF03X-*,03,PFTCPROG,(LPF03X-LPF03)/KEYLNQ,0)                
         DCDD  AC#HIST,3                                                        
         DCDD  AC#HIST,8                                                        
         DCDD  AC#LIST,8                                                        
LPF03    DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYCUR,L'LSTCODE-1),AL2(LSTCODE-LSTCODE)                   
LPF03X   EQU   *                                                                
*                                                                               
*        MAD INPUT                                                              
*                                                                               
         DC    AL1(LPF06X-*,06,PFTCPROG,0,0)                                    
         DCDD  AC#MAD,3                                                         
         DCDD  AC#MAD,8                                                         
         DCDD  AC#INP,8                                                         
LPF06X   EQU   *                                                                
*                                                                               
*        RETURN TO CALLER                                                       
*                                                                               
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        PFKEY TABLE DEFINITIONS - MAINT SCREEN                                 
***********************************************************************         
*                                                                               
MPFTABLE DS    0C                                                               
*                                                                               
*        PERSON LIST                                                            
*                                                                               
         DC    AL1(MPF01X-*,01,PFTCPROG,(MPF01X-MPF01)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#LIST,8                                                        
MPF01    DC    AL1(KEYTYTWA,L'PEMCODE-1),AL2(PEMCODE-T61DFFD)                   
MPF01X   EQU   *                                                                
*                                                                               
*        PER2 DISPLAY                                                           
*                                                                               
         DC    AL1(MPF02X-*,02,PFTCPROG,(MPF02X-MPF02)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#RPER2,8                                                       
         DCDD  AC#DSP,8                                                         
MPF02    DC    AL1(KEYTYTWA,L'PEMCODE-1),AL2(PEMCODE-T61DFFD)                   
         DC    AL1(KEYTYCUR,L'OFFICE-1),AL2(LLOFF-LLOFF)                        
         DC    AL1(KEYTYCUR,L'DEPT-1),AL2(LLDEPT-LLOFF)                         
         DC    AL1(KEYTYCUR,L'SUBDPT-1),AL2(LLSUB-LLOFF)                        
MPF02X   EQU   *                                                                
*                                                                               
*        RATES DISPLAY                                                          
*                                                                               
         DC    AL1(MPF10X-*,10,PFTCPROG,(MPF10X-MPF10)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#CRATS,8                                                       
         DCDD  AC#DSP,8                                                         
MPF10    DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYTWA,L'PEMCODE-1),AL2(PEMCODE-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'OFFICE-1),AL2(OFFICE-T61DFFD)                     
         DC    AL1(KEYTYTWA,L'DEPT-1),AL2(DEPT-T61DFFD)                         
         DC    AL1(KEYTYTWA,L'SUBDPT-1),AL2(SUBDPT-T61DFFD)                     
MPF10X   EQU   *                                                                
*                                                                               
*        HISTORY LIST                                                           
*                                                                               
         DC    AL1(MPF03X-*,03,PFTCPROG,(MPF03X-MPF03)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#HIST,8                                                        
         DCDD  AC#LIST,8                                                        
MPF03    DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYTWA,L'PEMCODE-1),AL2(PEMCODE-T61DFFD)                   
         DC    AL1(KEYTYCUR,L'OFFICE-1),AL2(LLOFF-LLOFF)                        
         DC    AL1(KEYTYCUR,L'DEPT-1),AL2(LLDEPT-LLOFF)                         
         DC    AL1(KEYTYCUR,L'SUBDPT-1),AL2(LLSUB-LLOFF)                        
MPF03X   EQU   *                                                                
*                                                                               
*        MAD INPUT                                                              
*                                                                               
         DC    AL1(MPF06X-*,06,PFTCPROG,0,0)                                    
         DC    CL3'   '                                                         
         DCDD  AC#MAD,8                                                         
         DCDD  AC#INP,8                                                         
MPF06X   EQU   *                                                                
*                                                                               
*                                                                               
*        PF9  TO GO TO PER2 SCREEN WITH ACTION CHANGE                           
*                                                                               
         DC    AL1(MPF09X-*,09,PFTCPROG,(MPF09X-MPF09)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#RPER2,8                                                       
         DCDD  AC#CHG,8                                                         
MPF09    DC    AL1(KEYTYTWA,L'PEMCODE-1),AL2(PEMCODE-T61DFFD)                   
MPF09X   EQU   *                                                                
*                                                                               
*        PF11 TO GO TO PER2 SCREEN                                              
*                                                                               
         DC    AL1(MPF11X-*,11,PFTCPROG,(MPF11X-MPF11)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#RPER2,8                                                       
         DCDD  AC#DSP,8                                                         
MPF11    DC    AL1(KEYTYTWA,L'PEMCODE-1),AL2(PEMCODE-T61DFFD)                   
MPF11X   EQU   *                                                                
*                                                                               
*        PF14 TO GO TO PER2 SCREEN WITH ACTION DISPLAY                          
*                                                                               
         DC    AL1(MPF14X-*,14,PFTCPROG,(MPF14X-MPF14)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#RPER2,8                                                       
         DCDD  AC#DSP,8                                                         
MPF14    DC    AL1(KEYTYTWA,L'PEMCODE-1),AL2(PEMCODE-T61DFFD)                   
MPF14X   EQU   *                                                                
*                                                                               
*        PF15 TO GO TO HISTORY/DELETE SCREEN TO END A LOC (INTERNAL)            
*                                                                               
         DC    AL1(MPF15X-*,15,PFTCPROG,(MPF15X-MPF15)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#HIST,8                                                        
         DCDD  AC#DEL,8                                                         
MPF15    DC    AL1(KEYTYTWA,L'PEMCODE-1),AL2(PEMCODE-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'OFFICE-1),AL2(PEMPOFF-T61DFFD)                    
         DC    AL1(KEYTYTWA,L'DEPT-1),AL2(PEMPDPT-T61DFFD)                      
         DC    AL1(KEYTYTWA,L'SUBDPT-1),AL2(PEMPSDT-T61DFFD)                    
         DC    AL1(KEYTYTWA,L'LLENDT-1),AL2(PEMPEND-T61DFFD)                    
         DC    AL1(KEYTYTWA,L'PEMPSAL-1),AL2(PEMPSAL-T61DFFD)                   
MPF15X   EQU   *                                                                
*                                                                               
*        PF16 TO GO TO HISTORY/MOVE SCREEN (INTERNAL)                           
*                                                                               
         DC    AL1(MPF16X-*,16,PFTCPROG,(MPF16X-MPF16)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#HIST,8                                                        
         DCDD  AC#MOVE,8                                                        
MPF16    DC    AL1(KEYTYTWA,L'PEMCODE-1),AL2(PEMCODE-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'OFFICE-1),AL2(PEMPOFF-T61DFFD)                    
         DC    AL1(KEYTYTWA,L'DEPT-1),AL2(PEMPDPT-T61DFFD)                      
         DC    AL1(KEYTYTWA,L'SUBDPT-1),AL2(PEMPSDT-T61DFFD)                    
         DC    AL1(KEYTYTWA,L'LLENDT-1),AL2(PEMPEND-T61DFFD)                    
         DC    AL1(KEYTYTWA,L'PEMPSAL-1),AL2(PEMPSAL-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'PEMOFF-1),AL2(PEMOFF-T61DFFD)                     
         DC    AL1(KEYTYTWA,L'PEMDEPT-1),AL2(PEMDEPT-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'PEMSDPT-1),AL2(PEMSDPT-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'PEMSTDT-1),AL2(PEMSTDT-T61DFFD)                   
MPF16X   EQU   *                                                                
*                                                                               
*        PF17 TO GO TO HISTORY/DELETE SCREEN FOR TRANSFERRING(INTERNAL)         
*                                                                               
         DC    AL1(MPF17X-*,17,PFTCPROG,(MPF17X-MPF17)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#HIST,8                                                        
         DCDD  AC#DEL,8                                                         
MPF17    DC    AL1(KEYTYTWA,L'PEMCODE-1),AL2(PEMCODE-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'OFFICE-1),AL2(PEMPOFF-T61DFFD)                    
         DC    AL1(KEYTYTWA,L'DEPT-1),AL2(PEMPDPT-T61DFFD)                      
         DC    AL1(KEYTYTWA,L'SUBDPT-1),AL2(PEMPSDT-T61DFFD)                    
         DC    AL1(KEYTYTWA,L'LLENDT-1),AL2(PEMPEND-T61DFFD)                    
         DC    AL1(KEYTYTWA,L'PEMPSAL-1),AL2(PEMPSAL-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'PEMOFF-1),AL2(PEMOFF-T61DFFD)                     
         DC    AL1(KEYTYTWA,L'PEMDEPT-1),AL2(PEMDEPT-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'PEMSDPT-1),AL2(PEMSDPT-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'PEMSTDT-1),AL2(PEMSTDT-T61DFFD)                   
MPF17X   EQU   *                                                                
*                                                                               
*        RETURN TO CALLER                                                       
*                                                                               
         DC    AL1(MPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
MPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        PFKEY TABLE DEFINITIONS - PER2 SCREEN                                  
***********************************************************************         
*                                                                               
PPFTABLE DS    0C                                                               
*                                                                               
*        PERSON LIST                                                            
*                                                                               
         DC    AL1(PPF01X-*,01,PFTCPROG,(PPF01X-PPF01)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#LIST,8                                                        
PPF01    DC    AL1(KEYTYTWA,L'PESCODE-1),AL2(PESCODE-T61DFFD)                   
PPF01X   EQU   *                                                                
*                                                                               
*        PERSON                                                                 
*                                                                               
         DC    AL1(PPF02X-*,02,PFTCPROG,(PPF02X-PPF02)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#CPRSN,8                                                       
         DC    CL8' '                                                           
PPF02    DC    AL1(KEYTYTWA,L'PESCODE-1),AL2(PESCODE-T61DFFD)                   
PPF02X   EQU   *                                                                
*                                                                               
*        RATES DISPLAY                                                          
*                                                                               
         DC    AL1(PPF10X-*,10,PFTCPROG,(PPF10X-PPF10)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#CRATS,8                                                       
         DCDD  AC#DSP,8                                                         
PPF10    DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYTWA,L'PESCODE-1),AL2(PESCODE-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'PESOFF-1),AL2(PESOFF-T61DFFD)                     
         DC    AL1(KEYTYTWA,L'PESDEPT-1),AL2(PESDEPT-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'PESSDPT-1),AL2(PESSDPT-T61DFFD)                   
PPF10X   EQU   *                                                                
*                                                                               
*        HISTORY LIST                                                           
*                                                                               
         DC    AL1(PPF03X-*,03,PFTCPROG,(PPF03X-PPF03)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#HIST,8                                                        
         DCDD  AC#LIST,8                                                        
PPF03    DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYTWA,L'PESCODE-1),AL2(PESCODE-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'PESOFF-1),AL2(PESOFF-T61DFFD)                     
         DC    AL1(KEYTYTWA,L'PESDEPT-1),AL2(PESDEPT-T61DFFD)                   
         DC    AL1(KEYTYTWA,L'PESSDPT-1),AL2(PESSDPT-T61DFFD)                   
PPF03X   EQU   *                                                                
*                                                                               
*        INTERNAL                                                               
*                                                                               
         DC    AL1(PPF11X-*,11,0,0,PFTRETRN,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
PPF11X   EQU   *                                                                
*                                                                               
*        RETURN TO CALLER                                                       
*                                                                               
         DC    AL1(PPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
PPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DISPLAY RECORD                                                         
***********************************************************************         
*                                                                               
DR       NTR1  BASE=*,LABEL=*                                                   
         CLI   SCRBOT,0                                                         
         BE    DR02                                                             
         MVI   SCRBOT,0                                                         
         BRAS  RE,CLRSCR                                                        
         GOTO1 =A(SCREENS),DMCB,RR=RELO   CLEAR BOTTOM OF SCREEN                
DR02     GOTO1 =A(VALOPTS),DMCB,RR=RELO       VALIDATE OPTIONS                  
         CLI   RECNUM,RTPR2        IF ON PERSON SCREEN                          
         BE    DR03                MAYBE DON'T REBUILD TABLE                    
         GOTO1 =A(BLDTABLE),DMCB,RR=RELO  REBUILD TABLE                         
*                                                                               
DR03     CLC   DTNAME,=C'*LOCTAB*'    IS THERE A TABLE ALREADY                  
         BNE   DR10                   NO, THEN MAKE ONE                         
         TM    BIT,KEYCHNGE           DID PERSON CHANGE                         
         BO    DR10                   IF YES, REBUILD LOCATIONS TABLE           
*                                                                               
         TM    OPTSTAT,XDOPT       XD=Y OPTION ON                               
         BO    DR10                IS SO REBUILD TABLE                          
*                                                                               
         CLI   RECNUM,RTPR2           IF ON PERSON SCREEN                       
         BE    DR20                                                             
         TM    BIT2,REBUILD           REBUILD AFTER PER2 SCREEN                 
         BNO   DR20                                                             
*                                                                               
DR10     NI    BIT,X'FF'-KEYCHNGE                                               
         CLI   ACTEQU,ACTADD          ALREADY BUILT FOR ADD                     
         BE    DR20                                                             
         CLI   RECNUM,RTPR2        PER2 SCREEN                                  
         BE    DR20                                                             
         GOTO1 =A(BLDTABLE),DMCB,RR=RELO                                        
*                                                                               
DR20     CLI   RECNUM,RTPR2                                                     
         BE    DR70                           GO DISPLAY PER2 SCREEN            
*                                                                               
         GOTO1 =A(VALSELS),DMCB,RR=RELO       VAL SEL FIELDS                    
         OC    DTADDLOC(DTLENQ),DTADDLOC      MOVE OUT OF ADD LINE              
         BZ    DR30                                                             
*                                                                               
         LA    R0,DTLOC2           SHIFT BLOCK OUT OF ADD LINE                  
         L     R1,=A(DTLOC2ND-DTLOC2)                                           
         LA    RE,DTADDLOC                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R0,DTLOCS                                                        
         L     R1,=A(DTLOC2ND-DTLOC2)                                           
         LA    RE,DTLOC2                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         XC    DTADDLOC(DTLENQ),DTADDLOC                                        
*                                                                               
DR30     GOTO1 =A(DISPRSCR),DMCB,RR=RELO  ALWAYS REDISPLAY PERSON FIRST         
*                                                                               
         CLI   PHSCREEN,X'F9'      MAINTENANCE SCREEN?                          
         BNE   DR32                                                             
         CLI   ACTNUM,ACTSEL       CAME FROM LIST SELECTION?                    
         BNE   DR32                                                             
         CLC   SVTHSSEL,AC@CHAU    IS IT CHANGE? IF SO DO NOT FORCE             
         BE    DR32                DISP-WON'T ACCEPT YOUR CHANGES               
         MVC   CONACT,SPACES       FORCE ACTION DISPLAY                         
         MVC   CONACT(7),AC@DISPU                                               
         MVI   ACTNUM,ACTDIS                                                    
*                                                                               
DR32     GOTO1 =A(SELECTS),DMCB,RR=RELO   GO TO SELECTION IF ANY                
*                                                                               
         CLI   ACTEQU,ACTADD              PF9 = GO TO PER2 AFTER ADD            
         BNE   DR40                                                             
         CLI   PFKEY,9                                                          
         BNE   DR40                                                             
         MVC   CONACT,SPACES                                                    
         MVC   CONACT(6),AC@CHAU                                                
DR35     BRAS  RE,CALLPF                                                        
*                                                                               
DR40     TM    BIT,UPDSEL          FOR UPDATIVE SELECTS                         
         BO    DRX                 ACURFORC IS SET TO DATES FOR UPDATE          
         CLI   ACTEQU,ACTDIS       FOR ACT DISPLAY OR SELECT FORCE              
         BE    DR50                CURSOR TO FIRST DISPLAY LINE                 
         CLI   ACTEQU,ACTSEL                                                    
         BNE   *+14                                                             
         CLC   SVTHSSEL,AC@CHAU                                                 
         BNE   DR50                                                             
*                                                                               
         LA    R1,PEMSELH          ELSE FIRST SELECT FIELD                      
         ST    R1,ACURFORC                                                      
         B     DRX                                                              
*                                                                               
DR50     LA    R1,PEMLIN1H                                                      
         ST    R1,ACURFORC                                                      
         B     DRX                                                              
*                                                                               
DR70     CLI   ACTEQU,ACTCHA       IS IT PER2 CHANGE?                           
         BNE   *+14                                                             
         OC    SWTCHBIT,SWTCHBIT   CHECK WHETHER WE CHANGED A                   
         BNZ   DR100               SWITCHON DATE                                
         LA    R1,PESCODEH         MAKE SURE CURSOR POSITION IS SET!            
         ST    R1,ACURFORC                                                      
*                                                                               
DR100    GOTO1 =A(SELECTS),DMCB,RR=RELO      MARK RIGHT SELECTION               
         GOTO1 =A(DISSTSCR),DMCB,RR=RELO     DISPLAY IT                         
*                                                                               
DRX      NI    BIT,X'FF'-KEYCHNGE                                               
DRXX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        OK TO DELETE PERSON REC                                                
***********************************************************************         
*                                                                               
OKTODEL  NTR1  BASE=*,LABEL=*      RECORD CAN HAVE NO LOCATION ELEMENTS         
         LA    R2,CONACTH                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,LOCELQ                                                    
         BAS   RE,GETEL                                                         
         BE    EDLOCS                                                           
         TM    BIT4,POINTERS       ADD POINTER?                                 
         BNO   OKTO10                                                           
         GOTO1 =A(ADDPTREL),DMCB,AIO,RAPKRPER,RR=RELO                           
OKTO10   J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
*        VALIDATE FILTERS AND UPDATE TABLE ENTRY                                
*        R2 POINTS TO SCREEN LINE                                               
*        R4 POINTS TO DISTAB ENTRY                                              
***********************************************************************         
*                                                                               
         USING LLINED,R2                                                        
         USING DISTABD,R4                                                       
VALFREE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   LLFREE,C'Y'         FREELANCER?                                  
         JNE   *+16                                                             
         NI    DTATTR,X'FF'-LOCBFRL    TURN OFF NON-CORRESPONDING BITS          
         OI    DTATTR,LOCYFRL          SET CORRESPONDING BIT                    
         J     VALFR030                                                         
         CLC   LLFREE,SPACES           UNDERFINED                               
         JE    *+12                                                             
         CLI   LLFREE,C'N'             REGULAR USER?                            
         JNE   *+12                                                             
         NI    DTATTR,X'FF'-(LOCBFRL+LOCYFRL)  TURN OFF ALL FRL BITS            
         J     VALFR010                                                         
         CLI   LLFREE,C'B'             BRANDOCEAN FREELANCER?                   
         JNE   VALFREI                                                          
         NI    DTATTR,X'FF'-LOCYFRL    TURN OFF NON-CORRESPONDING BITS          
         OI    DTATTR,LOCBFRL          SET CORRESPONDING BIT                    
*                                                                               
VALFR010 CLI   PEMPIDH+5,0             IS THERE A PID GIVEN?                    
         JNE   VALFR030                                                         
         BRAS  RE,REQMCS           CHECKS WHETHER USING MCS TIME                
         JE    VALFREM                                                          
*                                                                               
VALFR030 MVC   DTFREE,LLFREE                                                    
         OC    DTFREE,SPACES                                                    
*                                                                               
VFREEX   J     XIT                                                              
*                                                                               
VALFREI  LA    R2,LLFREEH          INVALID FREELANCER SETTING                   
         J     ERRINV                                                           
*                                                                               
VALFREM  LA    R2,PEMPIDH          MISSING PID WHEN REQUIRED                    
         J     ERRMISS                                                          
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
*        VALIDATE FILTERS AND UPDATE TABLE ENTRY                                
*        R2 POINTS TO SCREEN LINE                                               
*        R4 POINTS TO DISTAB ENTRY                                              
***********************************************************************         
*                                                                               
         USING LLINED,R2                                                        
         USING DISTABD,R4                                                       
VALFILT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   DTFILT1,LLFILT1                                                  
         OC    DTFILT1,SPACES                                                   
         MVC   DTFILT2,LLFILT2                                                  
         OC    DTFILT2,SPACES                                                   
         MVC   DTFILT3,LLFILT3                                                  
         OC    DTFILT3,SPACES                                                   
         MVC   DTFILT4,LLFILT4                                                  
         OC    DTFILT4,SPACES                                                   
         MVC   DTFILT5,LLFILT5                                                  
         OC    DTFILT5,SPACES                                                   
VFILTX   J     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CHECKS PROFILES TO SEE IF PID IS REQUIRED                              
***********************************************************************         
*                                                                               
REQPID   NTR1  BASE=*,LABEL=*                                                   
         L     R0,AIO3                                                          
         L     R1,=A(COBLOCKX-COBLOCK)                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING COBLOCK,R3                                                       
         L     R3,AIO3                                                          
         MVC   COADM,DATAMGR       PASS A(DATA MANAGER)                         
         MVC   COBKEY(COBKEYLN),SPACES                                          
         MVC   COKCPY,CMPY                                                      
         MVC   COKMTHD,SPACES                                                   
         GOTO1 VGETCAP,DMCB,COBLOCK                                             
         CLI   COSTATUS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PIDREQ,COOPTNQ      =N                                           
         JE    XNO                 NOT REQ'D                                    
         J     XYES                REQ'D                                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECKS PROFILES TO SEE IF USING MCS TIMESHEETS AT 1R LVL(=> NEED PID)         
***********************************************************************         
*                                                                               
REQMCS   NTR1  BASE=*,LABEL=*                                                   
         L     R0,AIO3                                                          
         L     R1,=A(COBLOCKX-COBLOCK)                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING COBLOCK,R3                                                       
         USING LLINED,R2           - CURRENT SCREEN LINE PROCESSING             
         L     R3,AIO3                                                          
         MVC   COADM,DATAMGR       PASS A(DATA MANAGER)                         
         MVC   COBKEY(COBKEYLN),SPACES                                          
         MVC   COKCPY,CMPY                                                      
         MVC   COKMTHD,SPACES                                                   
         SR    R1,R1                    BUILD 1R ACCOUNT FROM SCREEN            
         IC    R1,LN1RLEV1              VALIDATED THAT SCREEN LENGTHS           
         BCTR  R1,R0                    MATCH LEVEL LENGTHS IN VR               
         MVC   COKOFC(0),LLOFF                                                  
         EX    R1,*-6                                                           
         IC    R1,LN1RLEV2                                                      
         BCTR  R1,R0                                                            
         MVC   COKDPT(0),LLDEPT                                                 
         EX    R1,*-6                                                           
         IC    R1,LN1RLEV3                                                      
         BCTR  R1,R0                                                            
         MVC   COKSDT(0),LLSUB                                                  
         EX    R1,*-6                                                           
         IC    R1,LN1RLEV4                                                      
         BCTR  R1,R0                                                            
         MVC   COKPER(0),PERSON                                                 
         EX    R1,*-6                                                           
         GOTO1 VGETCAP,DMCB,COBLOCK                                             
         CLI   COSTATUS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   COMCT,COOPTYQ       =Y, USING MCS TIMESHEETS                     
         JE    XYES                REQ'D                                        
         J     XNO                 NOT REQ'D                                    
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        MAKE SURE NO FUTURE SAL EXISTS IF ENDING LOCATION                      
*        R4 POINTS TO DISTAB ENTRY                                              
***********************************************************************         
*                                                                               
         USING DISTABD,R4                                                       
FUTURSAL NTR1  BASE=*,LABEL=*                                                   
         OC    DTSALKDT,DTSALKDT   SKIP IF SAL LOCK DATE                        
         BNZ   FSYES                                                            
         GOTO1 DATCON,DMCB,(1,DTENDATE),(0,YYMMDD1)                             
         GOTO1 ADDAY,DMCB,YYMMDD1,YYMMDD2,F'1'      NO FUTURE SAL               
         GOTO1 DATCON,DMCB,(0,YYMMDD2),(1,STDATE)   END DATE +1                 
         XC    ENDATE,ENDATE                                                    
         MVC   TEMPOFF,DTOFFICE                                                 
         MVC   TEMPDEPT,DTDEPT                                                  
         MVC   TEMPSDPT,DTSUBDPT                                                
         MVC   ACCNT,DTACCNT                                                    
         GOTO1 =A(CHKSAL),DMCB,RR=RELO                                          
         JNE   XNO                                                              
FSYES    J     XYES                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        GET CURRENT SELECTION FROM TABLE                                       
*        RETURN AT R4                                                           
***********************************************************************         
*                                                                               
         USING DISTABD,R4          TABLE                                        
CURRSEL  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,DTADDLOC         FIND CURRENT SELECTION IN TABLE              
         AH    R4,STDISP                                                        
CUR10    OC    0(DTLENQ,R4),0(R4)                                               
         BZ    CUR10NX                                                          
         TM    DTLSTAT,DTSCURR     CURRENT SELECTION                            
         BO    CURYES                                                           
CUR10NX  LA    R4,DTLENQ(R4)       NEXT TABLE ENTRY                             
         LA    R1,DISTABND         END OF TABLE                                 
         CR    R4,R1                                                            
         BL    CUR10                                                            
         B     CURNO               NO CURRENT SELECTION                         
*                                                                               
CURYES   SR    RC,RC                                                            
CURNO    LTR   RC,RC                                                            
         XIT1  REGS=(R4)                                                        
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OFFICE, DEPT,SUBDPT WITH 1R                                   
***********************************************************************         
*                                                                               
         USING ACTRECD,R4                                                       
VALLOC   NTR1  BASE=*,LABEL=*      VALIDATE LOCATION ENTEREDD                   
         LA    R4,KEY2             BUILD KEY                                    
         XC    ACTKEY,ACTKEY                                                    
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVI   ACTKUNT,C'1'        UNIT 1                                       
         MVI   ACTKLDG,C'R'        LEDGER R                                     
         MVC   ACTKACT,ACCNT                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,AIO2                    
         L     R4,AIO2                                                          
         CLC   KEY2(L'ACTKEY),0(R4)                                             
         JNE   XNO                                                              
         J     XYES                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE FLIST OFFICE RECORD EXIST FOR OFFICE                          
***********************************************************************         
*                                                                               
         USING OFFRECD,R4                                                       
VALOFF   NTR1  BASE=*,LABEL=*                                                   
         CLI   PEMOFFH+5,2         VALIDATE OFFICE LENGTH                       
         JNE   XYES                IF NOT= 2, VALIDATE PROD OFFICE              
         LA    R4,KEY2             BUILD KEY                                    
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ    OFFICE RECORDS                               
         MVC   OFFKCPY,CMPY        COMPANY                                      
         LLC   R1,PEMOFFH+5                                                     
         BCTR  R1,0                                                             
         MVC   OFFKOFF(0),PEMOFF                                                
         EX    R1,*-6                                                           
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,AIO2                    
         L     R4,AIO2                                                          
         CLC   KEY2(L'OFFKEY),0(R4)                                             
         JNE   XNO                                                              
         J     XYES                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CHECK FOR DUPLICATE LOCATION AND MARK AS LOCKED DUPLICATE              
*        R4 POINTS TO DISTAB ENTRY BEING ADDED                                  
***********************************************************************         
*                                                                               
NEW      USING DISTABD,R4          ENTRY BEING ADDED                            
TAB      USING DISTABD,R3          TABLE                                        
DUPMRK   NTR1  BASE=*,LABEL=*                                                   
         LR    R3,R4               START AT ADD                                 
DM10NX   LA    R3,DTLENQ(R3)       NEXT TABLE ENTRY                             
         LA    R1,DISTABND         END OF TABLE                                 
         CR    R3,R1                                                            
         BNL   DMX                                                              
*                                                                               
         CLC   NEW.DTOFFICE,TAB.DTOFFICE     SAME OFF                           
         BNE   DM10NX                                                           
         CLC   NEW.DTDEPT,TAB.DTDEPT         SAME DEPT                          
         BNE   DM10NX                                                           
         CLC   NEW.DTSUBDPT,TAB.DTSUBDPT     SAME SUB DPET                      
         BNE   DM10NX                                                           
*                                                                               
*                         CHECK TO SEE IF MID PERIOD AND TIME EXISTS            
         MVC   TEMPDATE,TAB.DTENDATE                                            
         GOTO1 =A(READCAL),DMCB,RR=RELO                                         
         CLC   ENDATE,TAB.DTENDATE                                              
         BE    DM20                END DATE IS END OF PERIOD CONTINUE           
         CLC   NEW.DTSTDATE,ENDATE                                              
         BH    DM20                                                             
         GOTO1 =A(CHKTIME),DMCB,RR=RELO                                         
         BE    DM20                NO TIME EXISTS CONTINUE AS NORMAL            
         LA    R2,PEMSTDTH                                                      
         B     EDPOST                                                           
                                                                                
*                                                                               
DM20     OI    TAB.DTSTAT2,LOCSDUP           MARK DUPLICATE                     
         MVC   NEW.DTATTR,TAB.DTATTR         KEEP OLD ATTRIBUTES                
DMX      J     XIT                                                              
         DROP  NEW,TAB                                                          
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
*        VALIDATE ACT=Y BY CHECKING FOR STANDARD HOURS RECORD                   
*        R4 SHOULD POINT TO TABLE ENTRY                                         
***********************************************************************         
*                                                                               
         USING DISTABD,R4                                                       
STDHRS   NTR1  BASE=*,LABEL=*      VALIDATE THAT NO STD HOURS REC XISTS         
         MVI   SEQNUM,0                                                         
*                                                                               
         USING STDRECD,R6                                                       
         LA    R6,KEY2             BUILD KEY                                    
         XC    KEY2,KEY2                                                        
         MVC   STDKEY,SPACES                                                    
         MVI   STDKTYP,STDKTYPQ    STANDARD HOURS RECORD                        
         MVI   STDKSUB,STDKSUBQ                                                 
         MVC   STDKCPY,CMPY        COMPANY                                      
         MVC   STDKOFC,DTOFFICE                                                 
         MVC   STDKDPT,DTDEPT                                                   
         MVC   STDKSBD,DTSUBDPT                                                 
         MVC   STDKPER,PERSON                                                   
         MVC   STDKSEQ,SEQNUM                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCDIR ',KEY2,AIO2                    
         L     R6,AIO2                                                          
         CLC   KEY2(L'STDKEY),0(R6)                                             
         JNE   XYES                                                             
         J     XNO                                                              
         DROP  R6                                                               
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CLEAR SELECT FIELDS FOR RETURN                                         
***********************************************************************         
*                                                                               
         USING LLINED,R2                                                        
CLRSELS  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,PEMSELH          FIRST LINE ON SCREEN                         
CSEL10   MVC   LLSEL,SPACES        CLEAR                                        
         OI    LLSELH+6,X'80'      TRANSMIT                                     
         LA    R2,LLLEN(R2)        NEXT SCREEN LINE                             
         LA    R1,PEMENDH          LAST SCREEN LINE                             
         CR    R2,R1                                                            
         BNH   CSEL10                                                           
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CLEARS FIELDS FROM R2 TO R3                                            
***********************************************************************         
*                                                                               
CLRSCR   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,PEMSELH          1ST DISPLAY TABLE LINE                       
         LA    R3,PEMMSG1H         LAST DISPLAY TABLE LINE                      
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BNE   CLRSCR10                                                         
         CLI   MODE,VALKEY                                                      
         BNE   CLRSCR10                                                         
         LA    R2,PEMLIN1H         1ST LINE AFTER ADD LINE                      
*                                                                               
CLRSCR10 LLC   R1,0(R2)            FIELD LENGTH                                 
         AHI   R1,-9               8 FOR HEADER, 1 FOR EX                       
         MVC   8(0,R2),SPACES                                                   
         EX    R1,*-6                                                           
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         TRANSMIT                                     
         LLC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BL    CLRSCR10            NO                                           
CLRSCRX  J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CHECK SECURITY TO VALIDATE UPDATE OPTION                               
***********************************************************************         
*                                                                               
UPDSEC   NTR1  BASE=*,LABEL=*                                                   
         XC    DMCB(24),DMCB                                                    
         MVI   SBYTE,OPTFLDQ                                                    
         LA    R2,SBYTE                                                         
         GOTO1 SECRET,DMCB,('SECPOPTP',ASECBLK),(R2)                            
         CLI   DMCB,SECPYES                                                     
         JE    XYES                                                             
         J     XNO                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK FIELD SECURITY TO VALIDATE DISPLAYING NAME                    *         
*       EXIT - BYTE CONTAINS 'W' 'R' OR 'N'                           *         
***********************************************************************         
*                                                                               
FLDSEC   NTR1  BASE=*,LABEL=*                                                   
         XC    DMCB(24),DMCB                                                    
         MVI   SBYTE,NAMEFLDQ                                                   
         LA    R2,SBYTE                                                         
         GOTO1 SECRET,DMCB,('SECPFLDP',ASECBLK),(R2)                            
*&&US*&& MVI   BYTE,C'W'                                                        
         CLI   DMCB,SECPYES        WRITE ACCESS?                                
         JE    XYES                                                             
*&&US                                                                           
         MVI   BYTE,C'R'                                                        
         CLI   DMCB,SECPREAD       READ ACCESS?                                 
         JE    XYES                                                             
         MVI   BYTE,C'N'           NO ACCESS!                                   
*&&                                                                             
         J     XNO                                                              
         EJECT                                                                  
***********************************************************************         
*        CHECK SECURITY TO EDIT/DISPLAY SWITCHON DATES                *         
***********************************************************************         
*                                                                               
SWOSEC   NTR1  BASE=*,LABEL=*                                                   
         XC    DMCB(24),DMCB                                                    
         MVI   SBYTE,BRANSWO                                                    
         LA    R2,SBYTE                                                         
         GOTO1 SECRET,DMCB,('SECPFLDP',ASECBLK),(R2)                            
         CLI   DMCB,SECPYES     WRITE?                                          
         JE    XYES                                                             
         CLI   DMCB,SECPREAD    READ ONLY?                                      
         JE    XHIGH                                                            
         J     XNO                                                              
         EJECT                                                                  
***********************************************************************         
*        CALL PFKEY                                                             
***********************************************************************         
*                                                                               
CALLPF   NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,RTPR2        PER2 SCREEN                                  
         BNE   CALL10                                                           
         L     R2,=A(PPFTABLE)     PER2 PFKEY TABLE                             
         A     R2,RELO                                                          
         LA    R3,PESPFKYH                                                      
         B     CALL50                                                           
*                                                                               
CALL10   CLI   ACTNUM,ACTLIST      LIST SCREEN                                  
         BNE   CALL20                                                           
         L     R2,=A(LPFTABLE)     LIST PFKEY TABLE                             
         A     R2,RELO                                                          
         LA    R3,PELPFKYH                                                      
         B     CALL50                                                           
*                                                                               
CALL20   L     R2,=A(MPFTABLE)     MAINT PFKEY TABLE                            
         A     R2,RELO                                                          
         LA    R3,PEMPFKYH                                                      
         CLI   PFKEY,11            CAN'T CALL PER2 SCREEN WITH ADD              
         BNE   CALL50                                                           
         CLI   ACTEQU,ACTADD                                                    
         BNE   CALL50                                                           
         MVI   PFKEY,9             CHANGE TO CHANGE                             
*                                                                               
CALL50   GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)  INITIALIZE THE PFKEYS            
CALLX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DISPLAY KEY                                                            
***********************************************************************         
*                                                                               
         USING PERRECD,R4                                                       
DK       LA    R4,BIGKEY                                                        
         MVC   SVTHSSEL,THISLSEL   SAVE SELECTION CHOICE                        
         MVI   BIT,KEYCHNGE                                                     
         MVC   SAVEKEY,BIGKEY                                                   
         MVC   PEMCODE,PERKCODE    PERSON CODE                                  
         OI    PEMCODEH+6,X'80'    TRANSMIT                                     
         LA    R1,PEMOFFH                                                       
         ST    R1,ACURFORC         FORCE CURSOR TO OFFICE                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ADD X'FA' POINTER ELEMENTS                                   *         
*        R1 SHOULD POINT TO RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RAPPERD,R6                                                       
ADDPTREL NMOD1 0,*PTREL*                                                        
         L     RC,SAVERC                                                        
         L     R2,0(R1)                                                         
         L     R6,AIO3                                                          
         XC    RAPBLK(RAPBLKL),RAPBLK                                           
         MVI   RAPACTN,RAPAELEM    'FA' POINTER ELEM                            
         MVC   RAPCPY,CMPY                                                      
         MVI   RAPEMU,C'N'                                                      
         MVC   RAPACOM,ACOMFACS                                                 
         ST    R2,RAPAREC                                                       
         L     R2,4(R1)                                                         
         STCM  R2,1,RAPRTYP                                                     
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    ADDPTRX                                                          
         DC    H'0'                                                             
ADDPTRX  XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ADD RECORD ACTIVITY PASSIVE POINTER                          *         
*        R1 SHOULD POINT TO RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RAPPERD,R6                                                       
ADDRAPTR NMOD1 0,*RAPTR*                                                        
         L     RC,SAVERC                                                        
         L     R2,0(R1)                                                         
         L     R6,AIO3                                                          
*&&UK*&& XC    RAPBLK(RAPBLKL),RAPBLK                                           
         MVC   RAPCPY,CMPY                                                      
         MVI   RAPEMU,C'N'                                                      
         MVC   RAPACOM,ACOMFACS                                                 
         MVI   RAPACTN,RAPAPTR     TYPE '14' PASSIVE POINTER                    
         ST    R2,RAPAREC                                                       
         L     R2,4(R1)                                                         
         STCM  R2,1,RAPRTYP                                                     
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    ADDRAPX                                                          
         DC    H'0'                                                             
ADDRAPX  XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'56' -  EMPLOYEE HISTORY ELEMENT                               
*        R4 SHOULD POINT TO TABLE ENTRY                                         
***********************************************************************         
*                                                                               
         USING LOCELD,R6                                                        
         USING DISTABD,R4                                                       
ELEM56   NMOD1 0,*EL56**                                                        
         L     RC,SAVERC                                                        
*                                                                               
         L     R0,AIO3                                                          
         L     R1,=A(COBLOCKX-COBLOCK)                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         NI    CHKSTAT,X'FF'-THRDPTY                                            
*                                                                               
         L     R6,AIO              PERSON RECORD OR 1R?                         
         CLI   0(R6),PERKTYPQ                                                   
         BE    EL56B                                                            
         CLI   TERMSTAT,LOCSTRM    STATUS IS TERMINATE?                         
         BNE   *+8                                                              
         MVI   DTSTATUS,LOCSTRM    YES, MARK THE FIELD TO TERMINATE             
*                                                                               
*L56A    LA    R4,DTLENQ(R4)       FIX FOR DSSUP-2609/DSSUP-3579                
         MVC   TEMPOFF,SPACES      BUILD 1R ACCOUNT FROM SCREEN                 
         LA    RF,DTACCNT                                                       
         LLC   R1,LN1RLEV1         VALIDATED THAT SCREEN LENGTHS                
         BCTR  R1,0                MATCH LEVEL LENGTHS IN VR                    
         MVC   TEMPOFF(0),0(RF)                                                 
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1                                                            
*                                                                               
         MVC   TEMPDEPT,SPACES                                                  
         LLC   R1,LN1RLEV2                                                      
         BCTR  R1,0                                                             
         MVC   TEMPDEPT(0),0(RF)                                                
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1                                                            
*                                                                               
         MVC   TEMPSDPT,SPACES                                                  
         LLC   R1,LN1RLEV3                                                      
         BCTR  R1,0                                                             
         MVC   TEMPSDPT(0),0(RF)                                                
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1                                                            
*                                                                               
         MVC   TEMPPERS,SPACES                                                  
         LLC   R1,LN1RLEV4                                                      
         BCTR  R1,0                                                             
         MVC   TEMPPERS(0),0(RF)                                                
         EX    R1,*-6                                                           
         B     EL56F                                                            
*                                  PERSON RECORD FIND LOCELD                    
EL56B    LA    R6,PERRFST-PERRECD(R6)                                           
*                                                                               
EL56C    CLI   LOCEL,0                                                          
         BE    EL56H                                                            
         CLI   LOCEL,LOCELQ                                                     
         BE    EL56E                                                            
EL56D    LLC   R0,LOCLN                                                         
         AR    R6,R0                                                            
         B     EL56C                                                            
*                                                                               
         USING COBLOCK,R3                                                       
         USING LLINED,R2           - CURRENT SCREEN LINE PROCESSING             
EL56E    MVC   TEMPOFF,SPACES      BUILD 1R ACCOUNT FROM SCREEN                 
         LLC   R1,LN1RLEV1         VALIDATED THAT SCREEN LENGTHS                
         BCTR  R1,0                MATCH LEVEL LENGTHS IN VR                    
         MVC   TEMPOFF(0),LOCOFF                                                
         EX    R1,*-6                                                           
*                                                                               
         MVC   TEMPDEPT,SPACES                                                  
         LLC   R1,LN1RLEV2                                                      
         BCTR  R1,0                                                             
         MVC   TEMPDEPT(0),LOCDEPT                                              
         EX    R1,*-6                                                           
*                                                                               
         MVC   TEMPSDPT,SPACES                                                  
         LLC   R1,LN1RLEV3                                                      
         BCTR  R1,0                                                             
         MVC   TEMPSDPT(0),LOCSUB                                               
         EX    R1,*-6                                                           
*                                                                               
         MVC   TEMPPERS,SPACES                                                  
         LLC   R1,LN1RLEV4                                                      
         BCTR  R1,0                                                             
         MVC   TEMPPERS(0),PERSON                                               
         EX    R1,*-6                                                           
*                                                                               
EL56F    L     R3,AIO3                                                          
         MVC   COADM,DATAMGR       PASS A(DATA MANAGER)                         
         MVC   COBKEY(COBKEYLN),SPACES                                          
         MVC   COKCPY,CMPY                                                      
         MVC   COKMTHD,SPACES                                                   
         MVC   COKOFC,TEMPOFF                                                   
         MVC   COKDPT,TEMPDEPT                                                  
         MVC   COKSDT,TEMPSDPT                                                  
         MVC   COKPER,TEMPPERS                                                  
         GOTO1 VGETCAP,DMCB,COBLOCK                                             
         CLI   COSTATUS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   COTHR,COOPTYQ       =Y, PERSON SHOULD BE AVAIL IN 3RD PY         
         BE    EL56G               NOT REQ'D                                    
         L     RF,AIO                                                           
         CLI   0(RF),PERKTYPQ                                                   
         BE    EL56D                                                            
         B     EL56H                                                            
*                                                                               
EL56G    OI    CHKSTAT,THRDPTY     SET SENDING TO THIRD PARTY                   
*                                                                               
         USING EMPELD,R6                                                        
EL56H    L     R6,AIO              CHANGE IF THERE                              
         MVI   ELCODE,EMPELQ       X'56'                                        
         BAS   RE,GETEL                                                         
         BNE   EL56J                                                            
*                                                                               
         CLI   EMPLN,EMPLNQ        CORRECT LENGTH                               
         BNE   EL56I                                                            
         MVC   EMPHIR,HIREDT       HIRE DATE PWOS                               
         MVC   EMPTRM,TERMDT       TERM DATE PWOS                               
**TEST CODE                                                                     
         GOTO1 DATCON,DMCB,(1,EMPHIR),(21,WORK)                                 
**TEST CODE                                                                     
         MVC   EMPSALKD,DTSALKDT   SALARY LOCKED DATE                           
         MVC   EMPCSTAT,DTSTATUS   STATUS                                       
         TM    CHKSTAT,THRDPTY     FOR THIRD PART USE                           
         BZ    *+8                                                              
         OI    EMPSTAT,EMPSTHRD                                                 
         CLI   RECNUM,RTPR2                                                     
         BNE   EL56X                                                            
         MVC   EMPLOCK,DTTSLKDT    TIMESHEET LOCK DATE                          
         OC    EMPSTAT,DTATTR      ATTRIBUTES (EXEC=Y,ETC)                      
         B     EL56X                                                            
*                                                                               
EL56I    LLC   R2,EMPLN            MAKE ALL ELEMS THE LONGEST LENGTH            
         AHI   R2,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   BLOCK(0),0(R6)      SAVE ELEM                                    
         EX    R2,*-6                                                           
         MVI   ELCODE,EMPELQ                                                    
         GOTO1 REMELEM             DELETE SHORT ELEM                            
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVC   ELEM(0),BLOCK       RESTORE ELEM (REMELEM CREAMS ELEM)           
         EX    R2,*-6                                                           
         LA    R6,ELEM                                                          
         B     EL56K                                                            
*                                                                               
EL56J    LA    R6,ELEM             ADD IF NOT THERE                             
         XC    ELEM,ELEM                                                        
         MVI   EMPEL,EMPELQ                                                     
EL56K    MVI   EMPLN,EMPLNQ                                                     
         MVC   EMPHIR,HIREDT       HIRE DATE PWOS                               
         MVC   EMPTRM,TERMDT       TERM DATE PWOS                               
**TEST CODE                                                                     
         GOTO1 DATCON,DMCB,(1,EMPHIR),(21,WORK)                                 
**TEST CODE                                                                     
         MVC   EMPSALKD,DTSALKDT   SALARY LOCKED DATE                           
         MVC   EMPCSTAT,DTSTATUS   STATUS                                       
         TM    CHKSTAT,THRDPTY     FOR THIRD PART USE                           
         BZ    *+8                                                              
         OI    EMPSTAT,EMPSTHRD                                                 
         CLI   RECNUM,RTPR2                                                     
         BNE   EL56L                                                            
         MVC   EMPLOCK,DTTSLKDT    TIMESHEET LOCK DATE                          
         OC    EMPSTAT,DTATTR      ATTRIBUTES (EXEC=Y,ETC)                      
EL56L    GOTO1 ADDELEM                                                          
*                                                                               
EL56X    B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
*        UPDATE X'EF' -  GENERAL DATE ELEMENT FOR BRANDO DATES                  
*        R4 SHOULD POINT TO TABLE ENTRY                                         
***********************************************************************         
*                                                                               
         USING DISTABD,R4                                                       
         USING EMPELD,R6                                                        
ELEME5   NMOD1 0,*ELE5**                                                        
         L     RC,SAVERC                                                        
*                                                                               
         TM    DTATTR,LOCYFRL      CHECK IF FREELANCER                          
         B     ELEME5X                                                          
*        BNO   ELEME5X                                                          
*                                                                               
ELE5D    GOTO1 ADDELEM                                                          
*                                                                               
ELEME5X  B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
*        MAKE SURE NO FUTURE TIME EXISTS IF ENDING LOCATION                     
*        R4 POINTS TO DISTAB ENTRY                                              
***********************************************************************         
*                                                                               
         USING DISTABD,R4                                                       
FUTRTIME NMOD1 0,*FTIME**                                                       
         L     RC,SAVERC                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(1,DTENDATE),(0,YYMMDD1)                             
         OC    DTTSLKDT,DTTSLKDT   ANY TMS LOCK DATE?                           
         BZ    FT05                NO-ELSE USE IT                               
         GOTO1 DATCON,DMCB,(1,DTTSLKDT),(0,YYMMDD1)                             
FT05     GOTO1 ADDAY,DMCB,YYMMDD1,YYMMDD2,F'1'      NO FUTURE TIME              
         GOTO1 DATCON,DMCB,(0,YYMMDD2),(1,STDATE)   END DATE +1                 
         XC    ENDATE,ENDATE                                                    
         MVC   TEMPOFF,DTOFFICE                                                 
         MVC   TEMPDEPT,DTDEPT                                                  
         MVC   TEMPSDPT,DTSUBDPT                                                
         MVC   ACCNT,DTACCNT                                                    
         GOTO1 =A(CHKTIME),DMCB,RR=RELO                                         
         BNE   XNO                                                              
         TM    BIT2,NEWTIME        CHECK SAVED TIME FOR TMS USERS               
         BZ    FT10                                                             
         GOTO1 =A(CHKSVTMS),DMCB,RR=RELO CHECK FOR SAVED TMS RECS               
         BNE   XNO                                                              
FT10     TM    BIT2,NEWTIME        ON TMS                                       
         BO    FTYES                                                            
         GOTO1 =A(DRAFTRNS),DMCB,RR=RELO ELSE CHECK FOR DRAFT TIME              
         BE    XNO                                                              
FTYES    B     XYES                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        BUILD BLOCK OF PERSON INFORMATION TO PASS TO HISTORY PROGRAM           
*        NEW LOCATION (IF ANY) AND LOCATION BEING ENDED                         
***********************************************************************         
*                                                                               
         USING DISTABD,R4                                                       
         USING PEBLOCKD,R3                                                      
PERSINFO NMOD1 0,*PINFO*                                                        
*                                                                               
         LA    R0,DTLOC2           CLEAR 2ND BLOCK                              
         L     R1,=A(DTLOC2ND-DTLOC2)                                           
         LA    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R3,DTLOC2           BUILD BLOCK HERE                             
         MVC   PTNAME,=C'*PERBLK*'    PUT TABLE HEADER ON                       
         LA    R4,DTADDLOC         POINT TO NEW LOCATION LINE FIRST             
         OC    DTADDLOC(DTLENQ),DTADDLOC  IS THERE A NEW LOC?                   
         BZ    PERS05                                                           
         OI    PTLSTAT,PTNEWLOC    THIS IS THE NEW LOC ENTRY                    
         LHI   RF,2                MUST BUILD TO ENTRIES                        
         LH    R1,=Y(PT2LENQ)      SET TABLE LENGTH FOR TWO ENTRIES             
         STC   R1,PTTABLN                                                       
         B     PERS10                                                           
*                                                                               
PERS05   NI    PTLSTAT,X'FF'-PTNEWLOC   THIS IS NOT THE NEW LOC ENTRY           
         LA    R4,DTLOCS                                                        
         LHI   RF,1                ONLY ONE LOCATION TO BUILD                   
         LH    R1,=Y(PTLENQ)       SET TABLE LENGTH FOR ONE ENTRY               
         STC   R1,PTTABLN                                                       
*                                                                               
PERS10   LA    R2,PEMLNAMH         LAST NAME FIELD                              
         CLI   5(R2),0                                                          
         BE    PERS15                                                           
         MVC   PLNLNME,5(R2)       LENGTH OF LAST NAME                          
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,PLNAME,8(R2)                                                  
*                                                                               
PERS15   LA    R2,PEMFNAMH         FIRST NAME FIELD                             
         CLI   5(R2),0                                                          
         BE    PERS20                                                           
         MVC   PLNFNME,5(R2)       LENGTH OF FIRST NAME                         
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,PFNAME,8(R2)                                                  
*                                                                               
PERS20   MVC   PTERMDT,TERMDT      TERMINATION DATE                             
         MVC   PHIREDT,HIREDT      HIRE DATE                                    
         LA    R6,PTLOCS           POINT TO FIRST TABLE ENTRY                   
         USING PTLOCS,R6                                                        
PERS25   CHI   RF,1                NEED TO STORE START DT OF ENDING             
         BH    *+10                LOC. IF RF=1 THAN IT'S THE RIGHT 1           
         MVC   PSTART,DTSTDATE     START DATE OF ENDING LOCATON                 
         MVC   PSTATUS,DTSTATUS    STATUS                                       
         MVC   PFILTERS(5),DTFILTS FILTERS                                      
         LA    R4,DTLENQ(R4)                                                    
         LA    R6,PTLOCNQ(R6)                                                   
         BCT   RF,PERS25                                                        
*                                                                               
         LA    R0,DISTABLE         CLEAR DISPLAY TABLE                          
         L     R1,=A(DISTABND-DISTABLE)                                         
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,DISTABLE         SHIFT INFO TO BEGINNING OF BLOCK             
         L     R1,=A(DTLOC2ND-DTLOC2)                                           
         LA    RE,DTLOC2                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         B     XIT                                                              
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
*        VALIDATE THE HIRE DATE                                                 
***********************************************************************         
*                                                                               
VALHIRE  NMOD1 0,*VALHIRE                                                       
         L     RC,SAVERC                                                        
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,PEMHIRE                                                       
         ST    R1,DMCB                                                          
         LLC   R1,5(R2)                                                         
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,X'04'        ONLY ONE DATE INPUT                          
         BE    VHIRE10                                                          
         GOTO1 DATVAL,DMCB,(X'00',PEMHIRE),WORK+10                              
         CLI   DMCB+3,X'00'                                                     
         BE    EINVDATE                                                         
         GOTO1 DATCON,DMCB,(0,WORK+10),(1,WORK)                                 
         B     VHIRE20                                                          
VHIRE10  LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         MVC   WORK,PVALPSTA     PWOS                                           
VHIRE20  TM    PEMHIREH+4,X'20'                                                 
         BO    VHIREYES                                                         
*                                                                               
* MUST GET TO FIRST LOCATION                                                    
*                                                                               
         USING DISTABD,R4                                                       
VHIRE25  LA    R4,DTLOCS           POINT TO TABLE                               
         XC    FULL,FULL                                                        
VHIRE30  OC    DTPREVST(DTLENQ),DTPREVST      ANY ENTRY?                        
         BZ    *+8                                                              
         ST    R4,FULL             SAVE ADDRESS                                 
         LA    R4,DTLENQ(R4)                                                    
         LA    R1,DISTABND                                                      
         CR    R4,R1                                                            
         BL    VHIRE30                                                          
*                                                                               
         OC    FULL,FULL           NO ENTRIES IN TABLE-OKAY                     
         BZ    VHIREYES                                                         
         L     R4,FULL             POINT TO EARLIEST LOCATION                   
         CLC   DTSTDATE,WORK       HIRE DTE CANNOT BE LATER THAN                
         BL    VHIRENO             FIRST LOC START DATE                         
         OI    PEMHIREH+4,X'20'    VALIDATED BIT                                
*                                                                               
VHIREYES B     XYES                                                             
VHIRENO  B     XNO                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*=====================================================================          
*        UPDATE TIMESHEET TEMPO CROSS REFERENCE (X-REF) RECORDS                 
*=====================================================================          
*                                                                               
UPTXREF  NMOD1 0,**UPTX**                                                       
         L     RC,SAVERC                                                        
*                                                                               
         USING TSXRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVI   TSXKTYP,TSXKTYPQ    X'3E'                                        
         MVI   TSXKSUB,TSXKSUBQ    X'13'                                        
         MVC   TSXKCPY,CMPY        COMPANY                                      
         MVC   TSXKPER,PERSON      PERSON CODE                                  
         GOTO1 HIGH                READ HIGH                                    
         B     UPTXR20                                                          
*                                                                               
UPTXR10  GOTO1 SEQ                 READ SEQUENTIAL                              
UPTXR20  CLC   BIGKEY(TSXKEND-TSXKEY),KEYSAVE                                   
         BNE   UPTXRX                                                           
*                                                                               
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING PIDELD,R6                                                        
         MVI   ELCODE,PIDELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   UPTXR30             GO ADD THE ELEMENT                           
*                                                                               
         CLC   PIDNO,PIDNUM        IF NUMBERS ARE THE SAME SKIP                 
         BE    UPTXR10                                                          
         MVC   PIDNO,PIDNUM        PID NUMBER                                   
         GOTO1 PUTREC                                                           
         B     UPTXR10                                                          
*                                                                               
UPTXR30  LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   PIDEL,PIDELQ        X'D8' ELEMENT                                
         MVI   PIDLN,PIDLNQ                                                     
         MVC   PIDNO,PIDNUM        PERSON ID NUMBER                             
         GOTO1 ADDELEM                                                          
         L     R6,AIO                                                           
         GOTO1 PUTREC                                                           
         B     UPTXR10                                                          
*                                                                               
UPTXRX   B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*=====================================================================          
***********************************************************************         
*        UPDATE X'20' -  GENERAL NAME ELEMENT (FROM SCREEN) FOR UK              
*        SAVED AS LAST FIRST IN UK (NO COMMA)                                   
***********************************************************************         
*                                                                               
         USING NAMELD,R6                                                        
ELEM20UK NMOD1 0,**EL20UK                                                       
         L     RC,SAVERC                                                        
*                                                                               
         MVC   OLDACNAM,SPACES                                                  
         MVI   ELCODE,NAMELQ       X'20'                                        
         BAS   RE,GETEL            GET OLD NAME ELEMENT                         
         BNE   EL10UK                                                           
*                                                                               
         SR    RF,RF               SAVE OLD 1R ACCOUNT NAME                     
         IC    RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1        RF=LENGTH OF NAME - 1                        
         EXMVC RF,OLDACNAM,NAMEREC                                              
*                                                                               
         GOTO1 REMELEM             REMOVE ANY IF THERE                          
*                                                                               
EL10UK   MVC   WORK,SPACES         BUILD NAME IN WORK                           
         LA    R4,WORK                                                          
         LA    R3,0                LEN SO FAR                                   
         TM    CPXSTAT6,CPXCNAMO   CHANGING NAME ORDER?                         
         BZ    EL12UK                                                           
         LLC   R1,PEMFNAMH+5                                                    
         AHI   R1,-1                                                            
         BM    ERRMISS                                                          
         MVC   0(0,R4),PEMFNAM     MOVE IN FIRST NAME                           
         EX    R1,*-6                                                           
         B     EL14UK                                                           
*                                                                               
EL12UK   LLC   R1,PEMLNAMH+5       LAST NAME                                    
         AHI   R1,-1                                                            
         BM    ERRMISS                                                          
         MVC   0(0,R4),PEMLNAM     MOVE IN LAST NAME                            
         EX    R1,*-6                                                           
*                                                                               
EL14UK   LA    R3,1(R1,R3)         R3=LENGTH SO FAR                             
         AR    R4,R3               BUMP R4 POINTER IN WORK                      
         LA    R4,1(R4)            LEAVE A SPACE BTN FIRST & LA                 
         LA    R3,1(R3)            ADD 1 TO LEN                                 
         TM    CPXSTAT6,CPXCNAMO   CHANGING NAME ORDER?                         
         BZ    EL16UK                                                           
         LLC   R1,PEMLNAMH+5       LAST NAME                                    
         AHI   R1,-1                                                            
         BM    ERRMISS                                                          
         MVC   0(0,R4),PEMLNAM     MOVE IN LAST NAME                            
         EX    R1,*-6                                                           
         B     EL25UK                                                           
*                                                                               
EL16UK   LLC   R1,PEMFNAMH+5                                                    
         AHI   R1,-1                                                            
         BM    ERRMISS                                                          
         MVC   0(0,R4),PEMFNAM     MOVE IN FIRST NAME                           
         EX    R1,*-6                                                           
         B     EL25UK                                                           
*                                                                               
EL25UK   LA    R3,1(R1,R3)         ADD TO LEN                                   
         CHI   R3,36               MAX LEN                                      
         BNH   *+8                                                              
         LA    R3,36                                                            
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   NAMEL,NAMELQ        X'20'                                        
*        AH    R3,=Y(NAMLN1Q)      ADD HEADER TO LEN                            
         AHI   R3,NAMLN1Q          ADD HEADER TO LEN                            
         STC   R3,NAMLN                                                         
         SH    R3,=Y(NAMLN1Q+1)                                                 
         BM    EL20UKX                                                          
         MVC   NAMEREC(0),WORK                                                  
         EX    R3,*-6                                                           
         GOTO1 ADDELEM                                                          
EL20UKX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'D8' -  PERSON ID ELEMENT                                      
***********************************************************************         
*                                                                               
         USING PIDELD,R6                                                        
ELEMD8   NMOD1 0,*ELEMD8*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         MVI   ELCODE,PIDELQ       X'D8'                                        
         GOTO1 REMELEM             REMOVE OLD ELEM IF ANY                       
*                                                                               
         OC    PIDNUM,PIDNUM       NO NUMBER TO ADD                             
         BZ    ELD8X                                                            
         LA    R6,ELEM             ADD NEW ELEM                                 
         XC    ELEM,ELEM                                                        
         MVI   PIDEL,PIDELQ                                                     
         MVI   PIDLN,PIDLNQ                                                     
         MVC   PIDNO,PIDNUM        PERSON ID NUMBER                             
         GOTO1 ADDELEM                                                          
ELD8X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'22' -  ADDRESS ELEM FROM STATUS SCREEN                        
***********************************************************************         
*                                                                               
         USING ADRELD,R6                                                        
ELEM22   NMOD1 0,**EL22**                                                       
         L     RC,SAVERC                                                        
*                                                                               
         MVI   ELCODE,ADRELQ       X'22'                                        
         GOTO1 REMELEM             REMOVE EXISTING ADDRESS                      
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   ADREL,ADRELQ        X'22'                                        
         MVI   ADRNUM,0            SET # OF LINES TO ZERO                       
         MVC   ADRADD1,SPACES                                                   
         CLI   PESADR1H+5,0        ADDRESS LINE 1                               
         BE    EL22A               NO LINE 1                                    
         BAS   RE,EL22NUM          INCREMENT # OF LINES IN ELEMENT              
         MVC   ADRADD1,PESADR1                                                  
         OC    ADRADD1,SPACES                                                   
         MVI   ADRLN,ADRLN1Q       AT LEAST 1 LINE                              
*                                                                               
EL22A    MVC   ADRADD2,SPACES                                                   
         CLI   PESADR2H+5,0        ADDRESS LINE 2                               
         BE    EL22B               NO LINE 2                                    
         BAS   RE,EL22NUM          INCREMENT # OF LINES IN ELEMENT              
         MVC   ADRADD2,PESADR2                                                  
         OC    ADRADD2,SPACES                                                   
         MVI   ADRLN,ADRLN2Q       AT LEAST 2 LINES                             
*                                                                               
EL22B    MVC   ADRADD3,SPACES                                                   
         CLI   PESADR3H+5,0        ADDRESS LINE 3                               
         BE    EL22C               NO LINE 3                                    
         BAS   RE,EL22NUM          INCREMENT # OF LINES IN ELEMENT              
         MVC   ADRADD3,PESADR3                                                  
         OC    ADRADD3,SPACES                                                   
         MVI   ADRLN,ADRLN3Q       AT LEAST 3 LINES                             
*                                                                               
EL22C    CLI   PESADR4H+5,0        ADDRESS LINE 4                               
         BE    EL22D               NO LINE 4                                    
         BAS   RE,EL22NUM          INCREMENT # OF LINES IN ELEMENT              
         MVC   ADRADD4,PESADR4                                                  
         OC    ADRADD4,SPACES                                                   
         MVI   ADRLN,ADRLN4Q       4 LINES                                      
*                                                                               
EL22D    CLI   ADRLN,0             ANY ADDRESS                                  
         BE    EL22X               NO                                           
         GOTO1 ADDELEM             YES, ADD ELEM                                
EL22X    B     XIT                                                              
*                                                                               
* INCREMENT # OF LINES IN ELEMENT FOR EVERY LINE ENTERED                        
*                                                                               
EL22NUM  NTR1                                                                   
         SR    R1,R1                                                            
         IC    R1,ADRNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,ADRNUM                                                        
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'2C' -  SPECIAL POSTINGS ELEM                                  
***********************************************************************         
*                                                                               
         USING SPAELD,R6                                                        
ELEM2C   NMOD1 0,**EL2C**                                                       
         L     RC,SAVERC                                                        
*                                                                               
         MVI   ELCODE,SPAELQ       X'2C'                                        
         L     R6,AIO                                                           
         BAS   RE,GETEL            DELETE INCOME AND WRITEOFF                   
         B     EL2C10                                                           
EL2C10NX BAS   RE,NEXTEL                                                        
EL2C10   BNE   EL2C30                                                           
*                                                                               
         CLI   SPATYPE,SPATINCO    INCOME ACCOUNT?                              
         BE    EL2C20                                                           
         CLI   SPATYPE,SPATWOFF    WRITEOFF ACCOUNT?                            
         BNE   EL2C10NX                                                         
EL2C20   MVI   0(R6),X'FF'                                                      
         B     EL2C10NX                                                         
*                                                                               
EL2C30   MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         CLC   OWOACCT,SPACES      ANY OVERRIDE WRITEOFF ACCOUNT                
         BNH   EL2C50                                                           
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   SPAEL,SPAELQ        X'2C'                                        
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATWOFF    WRITEOFF ACCOUNT                             
         MVC   SPAAULA,OWOACCT                                                  
         GOTO1 ADDELEM                                                          
*                                                                               
EL2C50   CLC   OINCACCT,SPACES     ANY OVERRIDE INCOME ACCOUNT                  
         BNH   EL2CX                                                            
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   SPAEL,SPAELQ        X'2C'                                        
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATINCO    INCOME ACCOUNT                               
         MVC   SPAAULA,OINCACCT                                                 
         GOTO1 ADDELEM                                                          
*                                                                               
EL2CX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'30' -  RECORD STATUS ELEMENT                                  
*        R4 SHOULD POINT TO TABLE ENTRY                                         
***********************************************************************         
*                                                                               
         USING DISTABD,R4                                                       
         USING RSTELD,R6                                                        
ELEM30   NMOD1 0,**EL30**                                                       
         L     RC,SAVERC                                                        
*                                                                               
         L     R6,AIO              CHANGE IF THERE                              
         MVI   ELCODE,RSTELQ       X'30'                                        
         BAS   RE,GETEL                                                         
         BNE   EL30B                                                            
*                                                                               
         LLC   R2,RSTLN            MAKE ALL ELEMS THE LONGEST LENGTH            
         AHI   R2,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   BLOCK(0),0(R6)      SAVE ELEM                                    
         EX    R2,*-6                                                           
         MVI   ELCODE,RSTELQ                                                    
         GOTO1 REMELEM             DELETE SHORT ELEM                            
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVC   ELEM(0),BLOCK       RESTORE ELEM (REMELEM CREAMS ELEM)           
         EX    R2,*-6                                                           
         MVI   RSTLN,RSTLN3Q       UPDATE ELEM LENGTH                           
         B     EL30D                                                            
*                                                                               
EL30B    LA    R6,ELEM             ADD IF NOT THERE                             
         XC    ELEM,ELEM                                                        
         MVI   RSTEL,RSTELQ                                                     
         MVI   RSTLN,RSTLN3Q                                                    
         MVC   RSTBDATE,TODAYP                                                  
         MVC   RSTTDATE,RSTBDATE                                                
*                                                                               
EL30D    MVC   RSTFILT1,DTFILT1    UPDATE FILTERS                               
         MVC   RSTFILT2,DTFILT2                                                 
         MVC   RSTFILT3,DTFILT3                                                 
         MVC   RSTFILT4,DTFILT4                                                 
         MVC   RSTFILT5,DTFILT5                                                 
*&&US                                                                           
         SR    R1,R1                                                            
         ICM   R1,3,RSTSNUM                                                     
         CHI   R1,100                                                           
         JH    *+12                                                             
         LHI   R1,100                                                           
         STCM  R1,3,RSTSNUM                                                     
*&&                                                                             
*                                                                               
         CLI   RECNUM,RTPR2        STATUS SCREEN                                
         BNE   EL30F                                                            
*                                                                               
         MVC   RSTDFTSK,TASK       DEFAULT TASK CODE                            
         MVC   RSTCOSTG,ANALACCT   COSTING GROUP                                
*                                                                               
         NI    RSTSTAT1,X'FF'-(RSTSACIL+RSTSPIAE)                               
         TM    DTATTR,LOCAEXEC     PERSON IS AN ACCOUNT EXECUTIVE               
         BNO   *+8                                                              
         OI    RSTSTAT1,RSTSPIAE                                                
         TM    DTATTR,LOCALOCK     ACCOUNT IS LOCKED                            
         BNO   *+8                                                              
         OI    RSTSTAT1,RSTSACIL                                                
*                                                                               
         NI    RSTSTAT3,X'FF'-RSTSPRTS                                          
         TM    DTATTR,LOCAPRTS     PROJECT REQ'D ON TIMESHEET                   
         BNO   *+8                                                              
         OI    RSTSTAT3,RSTSPRTS                                                
*                                                                               
         NI    RSTSTAT5,X'FF'-(RSTSPROD+RSTSPRJB)                               
         TM    DTATTR,LOCAPROD     PROD REQ'D ON TIMESHEET                      
         BNO   *+8                                                              
         OI    RSTSTAT5,RSTSPROD                                                
         TM    DTATTR,LOCAJOB      JOB REQ'D ON TIMESHEET                       
         BNO   *+8                                                              
         OI    RSTSTAT5,RSTSPRJB                                                
*                                                                               
EL30F    GOTO1 ADDELEM                                                          
EL30X    B     XIT                                                              
         DROP  R4,R6                                                            
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ADD X'32' ACCOUNT BALANCE ELEM IF ONE DOESN'T EXIST ON REC             
***********************************************************************         
*                                                                               
         USING ABLELD,R6                                                        
ELEM32   NMOD1 0,**EL32**                                                       
         L     RC,SAVERC                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,ABLELQ                                                    
         BAS   RE,GETEL            DO NOTHING IF X'32' EXISTS                   
         BE    EL32X                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   ABLEL,ABLELQ        X'32'                                        
         MVI   ABLLN,ABLLN3Q                                                    
         ZAP   ABLFRWD,=P'0'                                                    
         ZAP   ABLDR,=P'0'                                                      
         ZAP   ABLCR,=P'0'                                                      
         ZAP   ABLURG,=P'0'                                                     
         GOTO1 ADDELEM                                                          
EL32X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ADD X'33' ACCOUNT PEEL-OFF ELEM IF ONE DOESN'T EXIST ON REC            
***********************************************************************         
*                                                                               
         USING APOELD,R6                                                        
ELEM33   NMOD1 0,**EL33**                                                       
         L     RC,SAVERC                                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,APOELQ                                                    
         BAS   RE,GETEL            DO NOTHING IF X'33' EXISTS                   
         BE    EL33X                                                            
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   APOEL,APOELQ        X'33'                                        
         MVI   APOLN,APOLN2Q                                                    
         ZAP   APODR,=P'0'                                                      
         ZAP   APOCR,=P'0'                                                      
         GOTO1 ADDELEM                                                          
EL33X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'3F' -  ONLINE MEMO ELEMENT FROM STATUS SCREEN                 
***********************************************************************         
*                                                                               
         USING OMEELD,R6                                                        
ELEM3F   NMOD1 0,**EL3F**                                                       
         L     RC,SAVERC                                                        
*                                                                               
         MVI   ELCODE,OMEELQ       X'3F'                                        
         GOTO1 REMELEM             REMOVE OLD ELEM IF ANY                       
*                                                                               
         LLC   R1,PESMEMOH+5                                                    
         AHI   R1,-1                                                            
         BM    EL3FX                                                            
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   OMEEL,OMEELQ        X'3F'                                        
         MVC   OMEMO(0),PESMEMO                                                 
         EX    R1,*-6                                                           
         LA    R1,1(R1)                                                         
*        AH    R1,=Y(OMELN1Q)                                                   
         AHI   R1,OMELN1Q                                                       
         STC   R1,OMELN                                                         
         GOTO1 ADDELEM                                                          
EL3FX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'5A' -  GENERAL PURPOSE NAME ELEMENTS (FROM SCREEN)            
***********************************************************************         
*                                                                               
         USING GPNELD,R6                                                        
ELEM5A   NMOD1 0,**EL5A**                                                       
         L     RC,SAVERC                                                        
*                                                                               
         MVI   ELCODE,GPNELQ       X'5A'                                        
         GOTO1 REMELEM             REMOVE ANY IF THERE                          
*                                                                               
         LA    R6,ELEM             ADD LAST NAME                                
         XC    ELEM,ELEM                                                        
         MVI   GPNEL,GPNELQ                                                     
         LLC   R1,PEMLNAMH+5                                                    
*        AH    R1,=Y(GPNLNQ)                                                    
         AHI   R1,GPNLNQ                                                        
         STC   R1,GPNLN                                                         
         LLC   R1,PEMLNAMH+5                                                    
         AHI   R1,-1                                                            
         BM    EL5A10                                                           
         MVC   GPNNME(0),PEMLNAM                                                
         EX    R1,*-6                                                           
         MVI   GPNTYP,GPNTLST                                                   
         GOTO1 ADDELEM                                                          
*                                                                               
EL5A10   LA    R6,ELEM             ADD FIRST NAME                               
         XC    ELEM,ELEM                                                        
         MVI   GPNEL,GPNELQ                                                     
         LLC   R1,PEMFNAMH+5                                                    
*        AH    R1,=Y(GPNLNQ)                                                    
         AHI   R1,GPNLNQ                                                        
         STC   R1,GPNLN                                                         
         LLC   R1,PEMFNAMH+5                                                    
         AHI   R1,-1                                                            
         BM    EL5AX                                                            
         MVC   GPNNME(0),PEMFNAM                                                
         EX    R1,*-6                                                           
         MVI   GPNTYP,GPNTFST                                                   
         GOTO1 ADDELEM                                                          
EL5AX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'83' -  PERSON LOCATION ELEMENT                                
*        R4 SHOULD POINT TO TABLE ENTRY                                         
***********************************************************************         
*                                                                               
         USING DISTABD,R4                                                       
         USING LOCELD,R6                                                        
ELEM83   NMOD1 0,*ELEM83*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         L     R6,AIO              CHANGE IF THERE                              
         MVI   ELCODE,LOCELQ       X'83'                                        
         BAS   RE,GETEL                                                         
         B     EL83A                                                            
EL83ANX  BAS   RE,NEXTEL                                                        
EL83A    BNE   EL83D                                                            
         CLC   LOCOFF,DTOFFICE     MATCH ON OFFICE                              
         BNE   EL83ANX                                                          
         CLC   LOCDEPT,DTDEPT      MATCH ON DEPT                                
         BNE   EL83ANX                                                          
         CLC   LOCSUB,DTSUBDPT     MATCH ON SUBDEPT                             
         BNE   EL83ANX                                                          
         CLC   LOCSTART,DTPREVST   MATCH ON PREV START DATE                     
         BNE   EL83ANX                                                          
         MVC   LOCSTART,DTSTDATE   START DATE                                   
         MVC   LOCEND,DTENDATE     END DATE                                     
         MVC   LOCSTAT,DTSTATUS    STATUS                                       
         MVC   LOCSALKD,DTSALKDT   SAL LOCKED DATE                              
         MVC   LOCSTAT2,DTSTAT2    STATUS 2                                     
*&&US                                                                           
         MVC   LOCATTR,DTATTR      ATTRIBUTES                                   
*&&                                                                             
         CLI   RECNUM,RTPR2        PER2 SCREEN                                  
         BNE   EL83X                                                            
         MVC   LOCLOCK,DTTSLKDT    TIMESHEET LOCK                               
*&&US                                                                           
         MVC   LOCATTR,DTATTR      ATTRIBUTES                                   
*&&                                                                             
         B     EL83X                                                            
*                                                                               
EL83D    LA    R3,ACCNT            GOING TO ADD-BUT FIRST MAKE SURE             
         MVC   ACCNT,SPACES        IT'S A VALID ACCOUNT                         
         LLC   R1,LN1RLEV1                                                      
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   0(0,R3),DTOFFICE    OFFICE                                       
         EX    R1,*-6                                                           
         LA    R3,1(R1,R3)                                                      
*                                                                               
         LLC   R1,LN1RLEV2                                                      
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   0(0,R3),DTDEPT      DEPARTMENT                                   
         EX    R1,*-6                                                           
         LA    R3,1(R1,R3)                                                      
*                                                                               
         LLC   R1,LN1RLEV3                                                      
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   0(0,R3),DTSUBDPT    SUB-DEPARTMENT                               
         EX    R1,*-6                                                           
*                                                                               
         BRAS  RE,VALLOC           MAKE SURE VAL BEFORE ADDING TO EL            
         BE    *+6                                                              
         DC    H'0'                DIE IF NOT VALID                             
*                                                                               
         LA    R6,ELEM             ADD IF NOT THERE                             
         XC    ELEM,ELEM                                                        
         MVI   LOCEL,LOCELQ                                                     
         MVI   LOCLN,LOCLNQ                                                     
         MVC   LOCOFF,DTOFFICE     OFFICE                                       
         MVC   LOCDEPT,DTDEPT      DEPT                                         
         MVC   LOCSUB,DTSUBDPT     SUBDEPT                                      
         MVC   LOCSTART,DTSTDATE   START DATE                                   
         MVC   LOCEND,DTENDATE     END DATE                                     
         MVC   LOCLOCK,DTTSLKDT    TIMESHEET LOCK                               
         MVC   LOCATTR,DTATTR      ATTRIBUTES                                   
         MVC   LOCSTAT,DTSTATUS    STATUS                                       
         MVC   LOCSTAT2,DTSTAT2    STATUS 2                                     
         MVC   LOCSALKD,DTSALKDT   SAL LOCKED DATE                              
         GOTO1 ADDELEM                                                          
EL83X    CLI   DTSTATUS,LOCSTRM    STATUS IS TERMINATE?                         
         BNE   *+8                                                              
         MVI   TERMSTAT,LOCSTRM    YES, MARK THE FIELD TO TERMINATE             
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        FIGURE OUT WHERE THE OFFICE IS IN THE DISPLAY TABLE                    
*        BASED ON OFFDISP IN THE LEDGER                                         
*        RETURN OFFICE IN WORK                                                  
***********************************************************************         
*                                                                               
POSOFF   NMOD1 0,*POSOFF*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         USING DISTABD,R4                                                       
         XC    WORK,WORK                                                        
         CLI   OFFDISP,0           IS OFFICE IN 1ST POSITION (USUALLY)          
         BNE   POS10                                                            
         LA    R3,DTOFFICE                                                      
         LLC   RF,LN1RLEV1                                                      
         B     POS50                                                            
POS10    CLC   OFFDISP,LN1RLEV1      IS OFFICE IN 2ND POS                       
         BNE   POS20                                                            
         LA    R3,DTDEPT                                                        
         LLC   RF,LN1RLEV2                                                      
         B     POS50                                                            
POS20    LLC   R0,LN1RLEV1                                                      
         LLC   R1,LN1RLEV2                                                      
         AR    R1,R0               R1=LEV1+LEV2                                 
         LLC   R0,OFFDISP          R0=OFFICE DISPLACEMENT                       
         CR    R0,R1               IS OFFICE IN 3RD POSITION                    
         BE    *+8                                                              
         B     POSSYES             DON'T RECOGNIZE-SHOW ALL                     
         LA    R3,DTSUBDPT                                                      
         LLC   RF,LN1RLEV3                                                      
*                                                                               
POS50    BCTR  RF,0                                                             
         EXMVC RF,WORK,0(R3)                                                    
*                                                                               
         B     XNO                 MUST CHECK OFFICE                            
POSSYES  B     XYES                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*  CHECK WHETHER ACCOUNT NAME HAS CHANGED AND IF SO ADD POINTER                 
*  FOR ACCOLADE TO ALLOW REFRESH OF CONTRA-ACCOUNT NAMES                        
*  ENTRY - OLDACNAM OLD ACCOUNT NAME                                            
*        - AIO      A(1R ACCOUNT)                                               
*        - BIGKEY   1R ACCOUNT KEY WITH DISK ADDRESS                            
***********************************************************************         
*                                                                               
         USING ACTRECD,R4                                                       
CHKNAM   NMOD1 0,*CHKNAM*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         LA    R4,BIGKEY           1R ACCOUNT KEY                               
         L     R6,AIO                                                           
         MVI   ELCODE,NAMELQ       FIND NAME ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                 SHOULD BE THERE BY NOW, EVEN IF ADD          
         DC    H'0'                                                             
                                                                                
         SR    RF,RF                                                            
         IC    RF,1(R6)            CHECK WE HAVEN'T ADDED IT ALREADY            
         BCTR  RF,0                                                             
         EXCLC RF,0(R6),OLDACNAM                                                
         BE    CNAMOK              OK - NO CHANGE                               
*                                                                               
         LA    R6,KEY2                                                          
         USING ANCRECD,R6                                                       
         XC    ANCKEY(ACCKLEN),ANCKEY                                           
         MVI   ANCKTYP,ANCKTYPQ                                                 
         MVC   ANCKCULA,ACTKCULA                                                
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMREAD'),=C'ACCDIR',(R6),(R6),0           
         BE    CNAMOK              POINTER ALREADY THERE                        
*                                                                               
         XC    ANCKEY(ACCKLEN),ANCKEY  ADD IT TO FILE                           
         MVI   ANCKTYP,ANCKTYPQ                                                 
         MVC   ANCKCULA,ACTKCULA                                                
         OI    ANCKSTAT,ANCSDELT                                                
         MVC   ANCKDA,ACTKDA                                                    
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'ACCDIR',(R6),(R6),0                    
CNAMOK   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        ADD/UPDATE 1R ACCOUNT NAME SEARCH PASSIVE POINTERS                     
*  ENTRY - OLDACNAM OLD ACCOUNT NAME                                            
*        - AIO      A(1R ACCOUNT)                                               
*        - BIGKEY   1R ACCOUNT KEY WITH DISK ADDRESS                            
***********************************************************************         
*                                                                               
         USING ACTRECD,R4                                                       
PUTSRC   NMOD1 0,*PUTSRC*                                                       
         L     RC,SAVERC                                                        
         LA    R4,BIGKEY                                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,NAMELQ       FIND NAME ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                 SHOULD BE THERE BY NOW, EVEN IF ADD          
         DC    H'0'                                                             
                                                                                
         LA    R2,OLDACNAM                                                      
         SR    RF,RF                                                            
         IC    RF,1(R6)            CHECK WE HAVEN'T ADDED IT ALREADY            
         BCTR  RF,0                                                             
         EXCLC RF,0(R6),0(R2)                                                   
         BE    PSRCOK              OK - NAME HAS NOT BEEN CHANGED               
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB(4),=C'CT  '                                                 
*&&UK*&& TM    BIT,NEWREC                                                       
*&&US*&& TM    BIT,NEWREC+RESREC                                                
         BZ    PSRC10                                                           
*                                                                               
         LA    R4,BIGKEY           1R ACCOUNT KEY                               
         L     R6,AIO                                                           
         MVC   ACTKEY,0(R6)                                                     
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMREAD'),=C'ACCDIR',(R4),(R4),0           
         BE    *+6                                                              
         DC    H'0'                ACCOUNT NOT FOUND                            
         XC    DMCB,DMCB                                                        
         MVC   DMCB(4),=C'AT  '                                                 
*&&US                                                                           
         TM    BIT,RESREC                                                       
         BZ    PSRC10                                                           
         MVC   DMCB(4),=C'RT  '                                                 
*&&                                                                             
*                                                                               
PSRC10   GOTO1 VACSRCHP,DMCB,,AIO,ACTKDA,(R2),ACOMFACS,(LN1RLOW,0)              
PSRCOK   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DID USER ENTER NEW SWITCHON DATE                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING DISTABD,R4                                                       
NEWSWO   NMOD1 0,**NEWSWO**                                                     
         L     RC,SAVERC                                                        
         XC    SWTCHBIT,SWTCHBIT                                                
         XC    SWTCHBI2,SWTCHBI2                                                
         XC    GPBKEY,GPBKEY       GPBKEY STORES NEW ENTERED DATES              
         XC    GPBKEY2,GPBKEY2                                                  
         LA    R3,GPBKEY           R3 POINTS TO NEW DATE                        
         LA    R2,STDATES          R2 POINTS TO EXISTING DATE                   
*                                                                               
         TM    PESBOS1H+4,X'80'    ANY INPUT?                                   
         BNZ   NEWS008                                                          
         TM    PESBOS2H+4,X'80'    ANY INPUT?                                   
         BNZ   NEWS008                                                          
         TM    PESBOS3H+4,X'80'    ANY INPUT?                                   
         BNZ   NEWS008                                                          
         TM    PESBOS4H+4,X'80'    ANY INPUT?                                   
         BNZ   NEWS008                                                          
         TM    PESBOS5H+4,X'80'    ANY INPUT?                                   
         BNZ   NEWS008                                                          
         TM    PESBOS6H+4,X'80'    ANY INPUT?                                   
         BNZ   NEWS008                                                          
         B     NEWSX                                                            
*                                                                               
NEWS008  TM    PESBOS1H+1,X'20'    TEST PROTECTED FIELDS                        
         BNZ   NEWSX                                                            
         CLC   PESBOS1,SPACES      ANY DATE?                                    
         BNH   NEWS010                                                          
*                                                                               
         LA    R1,PESBOS1H         SET CURSOR                                   
         ST    R1,ACURFORC                                                      
         LA    R1,PESBOS1                                                       
         ST    R1,DMCB                                                          
         LLC   R1,PESBOS1H+5                                                    
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,X'04'                                                     
         BNE   EINVDATE                                                         
*                                                                               
         LA    RF,BLOCK                                                         
         MVC   0(DATL,R3),PVALPSTA-PERVALD(RF)                                  
         OI    PESBOS1H+4,X'20'    SET VALIDATED BIT                            
*                                                                               
NEWS010  CLC   PESBOS2,SPACES      ANY DATE?                                    
         BNH   NEWS012                                                          
*                                                                               
         LA    R1,PESBOS2H         SET CURSOR                                   
         ST    R1,ACURFORC                                                      
         LA    R1,PESBOS2                                                       
         ST    R1,DMCB                                                          
         LLC   R1,PESBOS2H+5                                                    
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,X'04'                                                     
         BNE   EINVDATE                                                         
*                                                                               
         LA    RF,BLOCK                                                         
         MVC   ENDT(DATL,R3),PVALPSTA-PERVALD(RF)                               
         OI    PESBOS2H+4,X'20'    SET VALIDATED BIT                            
*                                                                               
         CLC   ENDT(DATL,R3),0(R3) CHECK NEW END IS LOWER THAN START            
         BL    ESTEND              START                                        
*                                                                               
NEWS012  LA    R3,DATLVL(R3)       BUMP TO NEXT DATES                           
         CLC   PESBOS3,SPACES      ANY DATE?                                    
         BNH   NEWS014                                                          
*                                                                               
         LA    R1,PESBOS3H         SET CURSOR                                   
         ST    R1,ACURFORC                                                      
         LA    R1,PESBOS3                                                       
         ST    R1,DMCB                                                          
         LLC   R1,PESBOS3H+5                                                    
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,X'04'                                                     
         BNE   EINVDATE                                                         
*                                                                               
         LA    RF,BLOCK                                                         
         MVC   0(DATL,R3),PVALPSTA-PERVALD(RF)                                  
         OI    PESBOS3H+4,X'20'    SET VALIDATED BIT                            
*                                                                               
NEWS014  CLC   PESBOS4,SPACES      ANY DATE?                                    
         BNH   NEWS016                                                          
*                                                                               
         LA    R1,PESBOS4H         SET CURSOR                                   
         ST    R1,ACURFORC                                                      
         LA    R1,PESBOS4                                                       
         ST    R1,DMCB                                                          
         LLC   R1,PESBOS4H+5                                                    
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,X'04'                                                     
         BNE   EINVDATE                                                         
*                                                                               
         LA    RF,BLOCK                                                         
         MVC   ENDT(DATL,R3),PVALPSTA-PERVALD(RF)                               
         OI    PESBOS4H+4,X'20'    SET VALIDATED BIT                            
*                                                                               
         CLC   ENDT(DATL,R3),0(R3) CHECK NEW START IS LOWER THAN                
         BL    ESTEND              NEW END                                      
*                                                                               
NEWS016  LA    R3,DATLVL(R3)                                                    
         CLC   PESBOS5,SPACES      ANY DATE?                                    
         BNH   NEWS018                                                          
*                                                                               
         LA    R1,PESBOS5H         SET CURSOR                                   
         ST    R1,ACURFORC                                                      
         LA    R1,PESBOS5                                                       
         ST    R1,DMCB                                                          
         LLC   R1,PESBOS5H+5                                                    
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,X'04'                                                     
         BNE   EINVDATE                                                         
*                                                                               
         LA    RF,BLOCK                                                         
         MVC   0(DATL,R3),PVALPSTA-PERVALD(RF)                                  
         OI    PESBOS5H+4,X'20'    SET VALIDATED BIT                            
*                                                                               
NEWS018  CLC   PESBOS6,SPACES      ANY DATE?                                    
         BNH   NEWS020                                                          
*                                                                               
         LA    R1,PESBOS6H         SET CURSOR                                   
         ST    R1,ACURFORC                                                      
         LA    R1,PESBOS6                                                       
         ST    R1,DMCB                                                          
         LLC   R1,PESBOS6H+5                                                    
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,X'04'                                                     
         BNE   EINVDATE                                                         
*                                                                               
         LA    RF,BLOCK                                                         
         MVC   ENDT(DATL,R3),PVALPSTA-PERVALD(RF)                               
         OI    PESBOS6H+4,X'20'    SET VALIDATED BIT                            
*                                                                               
         CLC   ENDT(DATL,R3),0(R3) CHECK NEW END IS NOT BEFORE START            
         BL    ESTEND                                                           
*                                                                               
NEWS020  LA    R3,GPBKEY                                                        
         MVI   SWTCHBIT,PBOS1      SET GOT FIRST FIELD                          
         MVC   TEMPDTE2,0(R3)                                                   
         MVC   TEMPDTE3,0(R2)                                                   
         LA    R1,PESBOS1H         SET CURSOR                                   
         ST    R1,ACURFORC                                                      
*                                                                               
         OC    0(DATL,R2),0(R2)    DID WE HAVE A DATE BEFORE?                   
         BNZ   NEWS026                                                          
         OC    0(DATL,R3),0(R3)    DO WE HAVE A DATE NOW?                       
         BNZ   *+12                                                             
         OI    SWTCHBI2,UPDPPR3    TURN THE PROFILE OFF THEN                    
         B     NEWS104             NO THEN MUST HAVE NO DATES                   
         OC    ENDT(DATL,R3),ENDT(R3) ANY END DATE?                             
         BZ    *+12                                                             
         OI    SWTCHBI2,UPDPPR3    TURN THE PROFILE OFF THEN                    
         B     NEWS046                                                          
         OI    SWTCHBI2,UPDPPR2    TURN THE PROFILE ON  THEN                    
         B     NEWS046                                                          
*                                                                               
NEWS026  OC    0(DATL,R3),0(R3)    DO WE HAVE A DATE NOW?                       
         BNZ   NEWS028                                                          
         OC    ENDT(DATL,R3),ENDT(R3) DO WE HAVE AN END DATE?                   
         BNZ   EINVDELN            CAN'T REMOVE START IF WE HAVE AN END         
         OI    SWTCHBI2,UPDPPR3    TURN THE PROFILE OFF THEN                    
         B     NEWS046                                                          
*                                                                               
NEWS028  CLI   DATL(R2),X'01'      IS IT PERSON ALREADY?                        
         BE    NEWS046                                                          
         OI    SWTCHBI2,DUMMYDAT   SET TO BUILD DUMMY DATE                      
         TM    SWTCHBI2,UPDPPR1+UPDPPR2+UPDPPR3                                 
         BNZ   NEWS046                                                          
         OI    SWTCHBI2,UPDPPR1                                                 
         MVC   SWTCHLVL,DATL(R2)   LEVEL TO READ SWITCH PROFILE FROM            
         B     NEWS046                                                          
*                                                                               
NEWS030  MVI   SWTCHBIT,PBOS2      SET GOT 2ND FIELD                            
         MVC   TEMPDTE2,ENDT(R3)                                                
         MVC   TEMPDTE3,ENDT(R2)                                                
         LA    R1,PESBOS2H         SET CURSOR                                   
         ST    R1,ACURFORC                                                      
         CLC   ENDT(DATL,R2),=X'FFFFFF' ANY DATE PREVIOUSLY?                    
         BE    *+14                                                             
         OC    ENDT(DATL,R2),ENDT(R2)  ANY SECOND FIELD?                        
         BNZ   NEWS032                                                          
         OC    ENDT(DATL,R3),ENDT(R3)  IS THERE A DATE NOW?                     
         BZ    NEWS034                                                          
         OI    SWTCHBI2,UPDPPR3    TURN THE PROFILE OFF THEN                    
         B     NEWS046                                                          
*                                                                               
NEWS032  OC    ENDT(DATL,R3),ENDT(R3)  ANY DATE NOW?                            
         BNZ   *+12                                                             
         OI    SWTCHBI2,UPDPPR2    TURN PROFILE ON THEN                         
         B     NEWS046                                                          
         CLI   ENDTL(R2),X'01'     IS IT PERSON ALREADY?                        
         BE    NEWS046             IF NOT THEN UPDATE PERSON LEVEL              
         TM    SWTCHBI2,UPDPPR1+UPDPPR2+UPDPPR3                                 
         BNZ   NEWS046                                                          
         OI    SWTCHBI2,UPDPPR1                                                 
         MVC   SWTCHLVL,ENDTL(R2)  LEVEL TO READ SWITCH PROFILE FROM            
         B     NEWS046                                                          
*                                  DELETING THE DATE                            
NEWS034  LA    R2,DATLVL(R2)                                                    
         LA    R3,DATLVL(R3)                                                    
         MVI   SWTCHBIT,PBOS3      SET GOT 3RD FIELD                            
         MVC   TEMPDTE2,0(R3)                                                   
         MVC   TEMPDTE3,0(R2)                                                   
         LA    R1,PESBOS3H         SET CURSOR                                   
         ST    R1,ACURFORC                                                      
*                                                                               
         OC    0(DATL,R3),0(R3)    ANY START DATE NOW?                          
         BNZ   NEWS036                                                          
         OC    ENDT(DATL,R3),ENDT(R3) IF NO START ANY END DATE?                 
         BNZ   ERRSTDT             MUST HAVE START IF END DATE!                 
         B     NEWS040                                                          
*                               ** NO PREVIOUS START BUT START DATE NOW         
NEWS036  OC    ENDT(DATL,R3),ENDT(R3)                                           
         BNZ   NEWS046                                                          
         LA    R1,PESBOS4H         SET CURSOR                                   
         B     ERRENDT             MUST HAVE END DATE IF START DATE!            
*                                                                               
NEWS038  MVI   SWTCHBIT,PBOS4      SET GOT 4TH FIELD                            
         MVC   TEMPDTE2,ENDT(R3)                                                
         MVC   TEMPDTE3,ENDT(R2)                                                
         LA    R1,PESBOS4H         SET CURSOR                                   
         ST    R1,ACURFORC                                                      
         B     NEWS046                                                          
*                                                                               
NEWS040  LA    R2,DATLVL(R2)                                                    
         LA    R3,DATLVL(R3)                                                    
         MVI   SWTCHBIT,PBOS5      SET GOT 5TH FIELD                            
         MVC   TEMPDTE3,0(R2)                                                   
         MVC   TEMPDTE2,0(R3)                                                   
         LA    R1,PESBOS5H         SET CURSOR                                   
         ST    R1,ACURFORC                                                      
*                                                                               
         OC    0(DATL,R3),0(R3)    ANY START DATE NOW?                          
         BNZ   NEWS042                                                          
         OC    ENDT(DATL,R3),ENDT(R3) IF NO START ANY END DATE?                 
         BNZ   ERRSTDT             MUST HAVE START IF END DATE!                 
         B     NEWS072                                                          
*                               ** NO PREVIOUS START BUT START DATE NOW         
NEWS042  OC    ENDT(DATL,R3),ENDT(R3)                                           
         BNZ   NEWS046                                                          
         LA    R1,PESBOS6H         SET CURSOR                                   
         B     ERRENDT             MUST HAVE END DATE IF START DATE!            
*                                                                               
NEWS044  MVI   SWTCHBIT,PBOS6      SET GOT 6TH FIELD                            
         MVC   TEMPDTE3,ENDT(R2)                                                
         MVC   TEMPDTE2,ENDT(R3)                                                
         LA    R1,PESBOS6H         SET CURSOR                                   
         ST    R1,ACURFORC                                                      
         B     NEWS046                                                          
*                                                                               
NEWS046  CLI   SWTCHBIT,PBOS1      START                                        
         BE    NEWS048                                                          
         CLI   SWTCHBIT,PBOS2      END                                          
         BE    NEWS058                                                          
         CLI   SWTCHBIT,PBOS3      START                                        
         BE    NEWS048                                                          
         CLI   SWTCHBIT,PBOS4      END                                          
         BE    NEWS058                                                          
         CLI   SWTCHBIT,PBOS5      START                                        
         BE    NEWS048                                                          
         CLI   SWTCHBIT,PBOS6      END                                          
         BE    NEWS058                                                          
         DC    H'0'                                                             
*                                                                               
NEWS048  OC    TEMPDTE2,TEMPDTE2   HAS START BEEN REMOVED?                      
         BZ    NEWS054                                                          
         CLC   TEMPDTE2,DTSTDATE   HAS IT BEEN SET TO LOC START?                
         BE    NEWS052             YES OK                                       
         MVC   TEMPDATE,TEMPDTE2                                                
         GOTO1 =A(READCAL),DMCB,RR=RELO                                         
         BNE   EINVCAL             ERROR NO CALENDAR EXISTS                     
         CLC   STDATE,TEMPDTE2     CHECK ENTERED DATE IS START OF PER           
         BNE   EINVSTEN                                                         
         CLC   TEMPDTE2,DTSTDATE   IS IT BEFORE THE LOCATION START?             
         BNL   NEWS050                                                          
*                                                                               
         MVC   TEMPDATE,DTSTDATE                                                
         GOTO1 =A(READCAL),DMCB,RR=RELO                                         
         BNE   EINVCAL             ERROR NO CALENDAR EXISTS                     
         CLC   STDATE,DTSTDATE     IS LOC START MID PERIOD?                     
         BNL   ESTHIRE             NO THEN CAN'T SET BEFORE LOC START           
         CLC   TEMPDTE2,DTSTDATE   IS IT NOW AFTER THE LOC START DATE?          
         BH    NEWS050             THEN OK                                      
         CLC   TEMPDTE2,STDATE     HAS IT CHANGED?                              
         BNE   ESTHIRE             NOT ALLOWED TO CHANGE                        
*                                                                               
NEWS050  OC    TERMDT,TERMDT                                                    
         BZ    *+14                                                             
         CLC   TEMPDTE2,TERMDT     ERROR DATE AFTER TERM DATE                   
         BH    EDTTERM                                                          
         OC    DTENDATE,DTENDATE                                                
         BZ    *+14                                                             
         CLC   TEMPDTE2,DTENDATE   ERROR DATE AFTER LOCATION END                
         BH    EDTTERM                                                          
NEWS052  MVC   0(L'TEMPDTE2,R3),TEMPDTE2     START DATES                        
*                                                                               
         CLC   TEMPDTE2,TEMPDTE3   IS NEW START BEFORE/AFTER PREVIOUS           
         BL    NEWS070             BEFORE IS ALWAYS OK                          
         MVC   STDATE,TEMPDTE3                                                  
         MVC   ENDATE,TEMPDTE2                                                  
         B     NEWS068                                                          
*                                                                               
NEWS054  OC    TEMPDTE3,TEMPDTE3   WAS THERE A START BEFORE?                    
         BZ    NEWS046                                                          
         OC    ENDT(DATL,R2),ENDT(R2) ANY END DATE PREVIOUSLY?                  
         BNZ   NEWS056                                                          
         MVC   STDATE,TEMPDTE3     SEARCH FROM CURRENT START ONWARDS            
         XC    ENDATE,ENDATE                                                    
         B     NEWS068                                                          
*                                                                               
NEWS056  MVC   STDATE,TEMPDTE3     PREVIOUS START AND END BUT NONE NOW          
         MVC   ENDATE,ENDT(R2)     CHECK ANY TIME BETWEEN START/END             
         B     NEWS068                                                          
*                                                                               
NEWS058  OC    TEMPDTE2,TEMPDTE2   ANY END DATE?                                
         BZ    NEWS070             THEN REMOVED ALWAYS OK                       
         MVC   TEMPDATE,TEMPDTE2                                                
         GOTO1 =A(READCAL),DMCB,RR=RELO                                         
         BNE   EINVCAL             ERROR NO CALENDAR EXISTS                     
         CLC   ENDATE,TEMPDTE2     DATE IS END OF PERIOD?                       
         BNE   EINVSTEN                                                         
*                                                                               
         OC    DTENDATE,DTENDATE   IS THERE A LOCATION END DATE?                
         BZ    NEWS060                                                          
         CLC   TEMPDTE2,DTENDATE   IS IT LOC END OR LESS?                       
         BNH   NEWS060                                                          
         MVC   TEMPDATE,DTENDATE   READ TO FIND PERIOD LOC END FALLS            
         GOTO1 =A(READCAL),DMCB,RR=RELO UNDER                                   
         BNE   EINVCAL             ERROR NO CALENDAR EXISTS                     
         CLC   ENDATE,DTSTDATE     IS LOC END MID PERIOD?                       
         BNH   ENDHIRE                                                          
         CLC   TEMPDTE2,DTENDATE   IS IT NOW LESS THAN LOC END DATE             
         BNH   NEWS060             THEN OK                                      
         CLC   TEMPDTE2,ENDATE     HAS IT CHANGED?                              
         BNE   ENDHIRE             NOT ALLOWED TO CHANGE                        
*                                                                               
NEWS060  MVC   WORK(L'TEMPDTE2),TEMPDTE2                                        
         GOTO1 DATCON,DMCB,(1,TEMPDTE2),(0,WORK+10)                             
         GOTO1 ADDAY,DMCB,WORK+10,WORK+20,F'1'                                  
         GOTO1 DATCON,DMCB,(0,WORK+20),(1,TEMPDTE2) CONVERT ENTERED DAT         
*                                                                               
NEWS062  CLC   TEMPDTE2,DTSTDATE   ERROR DATE BEFORE LOCATION START             
         BL    ESTEND                                                           
         OC    TERMDT,TERMDT                                                    
         BZ    *+14                                                             
         CLC   TEMPDTE2,TERMDT     ERROR DATE AFTER TERM DATE                   
         BH    EDTTERM                                                          
         MVC   L'GPBKEY(L'TEMPDTE2,R3),TEMPDTE2    END DATES                    
*                                                                               
NEWS064  CLC   TEMPDTE2,0(R3)      IS THE END DATE BEFORE THE START?            
         BL    ESTEND                                                           
         OC    TEMPDTE3,TEMPDTE3   WAS THERE AN END DATE BEFORE?                
         BZ    NEWS066                                                          
         CLC   TEMPDTE2,TEMPDTE3   IS NEW DATE AFTER                            
         BNL   NEWS070             OR BEFORE OLD (AFTER ALWAYS OK)              
         MVC   STDATE,TEMPDTE2     NO USE OLD DATE                              
         MVC   ENDATE,TEMPDTE3                                                  
         B     NEWS068                                                          
*                                  NO PREVIOUS END DATE                         
NEWS066  MVC   STDATE,TEMPDTE2     CHECK ANY TIME FROM CURRENT END              
         XC    ENDATE,ENDATE       ONWARDS                                      
*                                                                               
NEWS068  GOTO1 =A(CHKTIME),DMCB,RR=RELO  CHECK WHETHER TIME EXISTS              
         BNE   EINVTIM                                                          
*                                                                               
NEWS070  CLI   SWTCHBIT,PBOS1      CHECK WHERE WE CAME FROM SO WE               
         BE    NEWS030             KNOW WHERE TO GO BACK TO                     
         CLI   SWTCHBIT,PBOS2                                                   
         BE    NEWS034                                                          
         CLI   SWTCHBIT,PBOS3                                                   
         BE    NEWS038                                                          
         CLI   SWTCHBIT,PBOS4                                                   
         BE    NEWS040                                                          
         CLI   SWTCHBIT,PBOS5                                                   
         BE    NEWS044                                                          
         CLI   SWTCHBIT,PBOS6                                                   
         BE    NEWS072                                                          
         DC    H'0'                NO IDEA WHERE WE CAME FROM                   
*                               ** POST PROCESSING OF DATES **                  
NEWS072  TM    SWTCHBI2,DUMMYDAT   BUILD DUMMY DATE                             
         BNZ   NEWS074                                                          
         OC    GPBKEY,GPBKEY       ANYTHING ENTERED THIS TIME?                  
         BNZ   NEWS080                                                          
         NI    SWTCHBI2,X'FF'-(UPDPPR1+UPDPPR2+UPDPPR3)                         
         OI    SWTCHBI2,UPDPPR3    THEN ALL DATES HAVE BEEN CLEARED             
         LA    R2,GPBKEY                                                        
         B     NEWS078                                                          
*                                                                               
NEWS074  LA    R2,GPBKEY           FIND SPARE SPACE TO BUILD DUMMY              
         LA    R1,GPBKEY2          ELEMENT                                      
*                                                                               
NEWS076  CR    R2,R1                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
         OC    0(DATL,R2),0(R2)                                                 
         BZ    NEWS078                                                          
         LA    R2,DATLVL(R2)                                                    
         B     NEWS076                                                          
*                                                                               
NEWS078  MVC   0(DATL,R2),DTSTDATE TURN PROFILE OFF AND PUT DUMMY               
         GOTO1 DATCON,DMCB,(1,DTSTDATE),(0,WORK+10)  START END                  
         GOTO1 ADDAY,DMCB,WORK+10,WORK+20,F'-1'     TO TURN OFF                 
         GOTO1 DATCON,DMCB,(0,WORK+20),(1,ENDT(R2))                             
         B     NEWS104                                                          
*                                                                               
NEWS080  LA    R2,GPBKEY           NOW FOR PROCESS THE DATES                    
         LA    R1,GPBKEY2                                                       
*                                                                               
NEWS082  CR    R2,R1               HIT END OF TABLE?                            
         BNL   NEWS088             YES NOW PROCESS THE DATES                    
         OC    0(DATL,R2),0(R2)                                                 
         BZ    NEWS086                                                          
         LR    R3,R2                                                            
*                                                                               
NEWS084  LA    R3,DATLVL(R3)       SET R3 TO POINT TO NEXT START                
         CR    R3,R1                                                            
         BNL   NEWS086             FINISHED FIND MORE DUPES                     
         OC    0(DATL,R3),0(R3)                                                 
         BZ    NEWS084                                                          
         CLC   0(DATL,R3),0(R2)                                                 
         BNE   NEWS084                                                          
         CLC   ENDT(DATL,R2),=X'FFFFFF' ANY END DATE?                           
         BE    NEWS084                                                          
         OC    ENDT(DATL,R2),ENDT(R2)  ANY END DATE?                            
         BZ    NEWS084                                                          
         CLC   ENDT(DATL,R3),ENDT(R2)                                           
         BNE   NEWS084                                                          
         XC    0(DATLVL,R3),0(R3)  FOUND ANOTHER SET OF DUPES                   
         XC    ENDT(DATLVL,R3),ENDT(R3) GET RID OF THEM                         
         B     NEWS084                                                          
*                                                                               
NEWS086  LA    R2,DATLVL(R2)                                                    
         B     NEWS082                                                          
*                                                                               
NEWS088  OC    GPBKEY,GPBKEY       CHECK WHETHER ANY DATES LEFT                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR R2SPAC              CLEAR UP TABLE                               
         LA    R2,GPBKEY           NOW FOR PROCESS THE DATES                    
         LR    R3,R2                                                            
*                                                                               
NEWS090  LA    R3,DATLVL(R3)       SET R3 TO POINT TO NEXT START                
         LA    R1,GPBKEY2          HIT END OF TABLE?                            
         CR    R3,R1               CHECK FOR OVERLAPPING DATES                  
         BNL   NEWS100                                                          
*                                                                               
         OC    0(DATL,R2),0(R2)    IF NO DATES SKIP                             
         BZ    NEWS100                                                          
         OC    0(DATL,R3),0(R3)    IF NEXT START ZERO SKIP                      
         BZ    NEWS100                                                          
*                                                                               
         OC    ENDT(DATL,R2),ENDT(R2) ANY END DATE FOR FIRST LINE?              
         BNZ   NEWS094                                                          
         CLC   0(DATL,R2),ENDT(R3) IS S1>E2? THEN NO OVERLAP                    
         BH    NEWS100                                                          
         BE    NEWS092                                                          
         CLC   0(DATL,R2),0(R3)                                                 
         BH    NEWS092                                                          
         XC    0(DATLVL,R3),0(R3)  S1<S2 REMOVE S2 + E2                         
         XC    ENDT(DATLVL,R3),ENDT(R3)                                         
         B     NEWS100                                                          
*                                                                               
NEWS092  MVC   0(DATL,R2),0(R3)   FOR S1=E2 AND                                 
         XC    0(DATLVL,R3),0(R3) REPLACE S1 WITH S2 AND REMOVE S2/E2           
         XC    ENDT(DATLVL,R3),ENDT(R3)                                         
         B     NEWS100                                                          
*                                                                               
NEWS094  CLC   0(DATL,R2),ENDT(R3) IS S1>E2 THEN NO OVERLAP                     
         BH    NEWS100                                                          
         CLC   ENDT(DATL,R2),ENDT(R3) IS S1<E2 AND E1>E2?                       
         BH    NEWS096                                                          
         CLC   0(DATL,R2),0(R3)    IS S2<S1<E2 AND E1<E2                        
         BL    NEWS098                                                          
         XC    0(DATLVL,R2),0(R2)  1ST DATE PAIR WITHIN 2ND DATE PAIR           
         XC    ENDT(DATLVL,R2),ENDT(R2) REMOVE 1ST DATE PAIR                    
         B     NEWS100                                                          
*                                                                               
NEWS096  CLC   0(DATL,R2),0(R3)   IS S1<S2                                      
         BL    *+10               FOR S1<S2<E2 AND E1>E2 2ND DATE INVAL         
         MVC   0(DATL,R2),0(R3)   FOR S2<S1<E2 AND E1>E2                        
         XC    0(DATLVL,R3),0(R3) REPLACE S1 WITH S2 AND REMOVE S2/E2           
         XC    ENDT(DATLVL,R3),ENDT(R3)                                         
         B     NEWS100                                                          
*                                 FOR S1<S2<E2 AND E1<E2 2ND DATE INVAL         
NEWS098  CLC   ENDT(DATL,R2),0(R3)  IS E1<S2                                    
         BL    NEWS100            THEN NO OVERLAP                               
         MVC   ENDT(DATL,R2),ENDT(R3) REPLACE E1 WITH E2 AND                    
         XC    0(DATLVL,R3),0(R3) REMOVE S2/E2                                  
         XC    ENDT(DATLVL,R3),ENDT(R3)                                         
         B     NEWS100                                                          
*                                                                               
NEWS100  LA    R1,GPBKEY2                                                       
         LA    R2,DATLVL(R2)                                                    
         CR    R2,R1                                                            
         BNL   NEWS102                                                          
         LR    R3,R2               DATES WITH NEXT DATE ALONG                   
         B     NEWS090                                                          
*                                                                               
NEWS102  GOTOR R2SPAC              CLEAN UP DATES AGAIN                         
*                                                                               
NEWS104  LA    R2,GPBKEY           SORT DATES INTO MOST RECENT FIRST            
         MVI   BYTE1,0             SO THEY GET SAVED IN CORRECT ORDER           
         LA    R1,GPBKEY2          FOR BRANDOCEAN                               
*                                                                               
NEWS106  CR    R2,R1                                                            
         BNL   NEWS110                                                          
         OC    0(DATLVL,R2),0(R2)                                               
         BZ    NEWS110                                                          
         OC    DATLVL(DATLVL,R2),DATLVL(R2)                                     
         BZ    NEWS110                                                          
         CLC   0(DATL,R2),DATLVL(R2)                                            
         BNH   NEWS108             COMPARE NEXT START WITH START                
         XC    0(DATLVL,R2),DATLVL(R2)      SWAPPING DATES                      
         XC    DATLVL(DATLVL,R2),0(R2)                                          
         XC    0(DATLVL,R2),DATLVL(R2)                                          
         XC    ENDT(DATLVL,R2),ENDDTLV(R2)                                      
         XC    ENDDTLV(DATLVL,R2),ENDT(R2)                                      
         XC    ENDT(DATLVL,R2),ENDDTLV(R2)                                      
*                                                                               
         MVI   BYTE1,1                                                          
*                                                                               
NEWS108  LA    R2,DATLVL(R2)                                                    
         B     NEWS106                                                          
*                                                                               
NEWS110  CLI   BYTE1,1             ANY SORTING DONE?                            
         BE    NEWS104                                                          
*                                                                               
         L     R6,AIO                                                           
         LA    R6,ACTRFST-ACTRECD(R6)                                           
         MVI   BYTE1,0             BYTE1 FLAG TO SAY IF SWITCON/                
*                                  SWITCHOFF INVISIBLE PAIR ALREADY             
         USING GDAELD,R6           PRESENT                                      
NEWS112  CLI   GDAEL,0                                                          
         BE    NEWS120                                                          
         CLI   GDATYPE,GDATMCST    FIND SWITCHON DATE ELEMENTS                  
         BE    NEWS116                                                          
NEWS114  LLC   R0,GDALN                                                         
         AR    R6,R0                                                            
         B     NEWS112                                                          
*                                                                               
NEWS116  OC    GDADATE2,GDADATE2                                                
         BZ    NEWS118                                                          
         CLC   GDADATE2,GDADATE    IF END<START KEEP THIS ONE                   
         BH    NEWS118                                                          
         CLC   GDADATE2,DTSTDATE   IS IT AFTER THE START DATE?                  
         BH    NEWS118             THEN DELETE IT                               
         MVI   BYTE1,X'01'         AND NOTE THAT WE HAVE ONE                    
         B     NEWS114                                                          
*                                                                               
NEWS118  MVI   GDAEL,X'FF'                                                      
         B     NEWS114                                                          
*                                                                               
NEWS120  GOTO1 VHELLO,DMCB,(C'D',=C'ACCMST'),(X'FF',AIO),0                      
         OC    GPBKEY,GPBKEY       ANYTHING ENTERED THIS TIME?                  
         BNZ   *+14                                                             
         OC    GPBKEY2,GPBKEY2                                                  
         BZ    NEWSX                                                            
         LA    R3,GPBKEY           R3 NEW DATES                                 
*                                  IF END<START DO WE NEED TO ADD IT?           
NEWS122  OC    ENDT(L'GDADATE,R3),ENDT(R3)                                      
         BZ    NEWS124                                                          
         CLI   ENDT(R3),X'FF'      IGNORE IF NO END DATE                        
         BE    NEWS124                                                          
         CLC   ENDT(L'GDADATE,R3),0(R3)                                         
         BH    NEWS124                                                          
         CLI   BYTE1,X'01'         IF ONE ALREADY PRESENT DON'T ADD             
         BE    NEWS126             ANOTHER ONE                                  
*                                                                               
NEWS124  XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALN2Q                                                    
         MVI   GDATYPE,GDATMCST                                                 
         MVC   GDADATE,0(R3)                                                    
         MVC   GDADATE2,ENDT(R3)                                                
         OI    GENSTAT5,ADDEQCOD                                                
         GOTO1 ADDELEM             AND ADD IT AGAIN                             
         NI    GENSTAT5,X'FF'-ADDEQCOD                                          
*                                                                               
NEWS126  LA    R3,DATLVL(R3)                                                    
         OC    0(DATL,R3),0(R3)                                                 
         BNZ   NEWS122                                                          
         B     NEWSX                                                            
*                                                                               
NEWSX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* REMOVE SPACES BETWEEN VALUES IN GPBKEY AND GPBKEY2                  *         
***********************************************************************         
         SPACE 1                                                                
R2SPAC   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
         LA    R3,GPBKEY                                                        
         LA    R2,GPBKEY2         GET RID OF ANY SPACES IN DATES                
*                                                                               
R2SP1    OC    0(DATLVL,R3),0(R3)                                               
         BNZ   R2SP2                                                            
         OC    ENDT(DATLVL,R3),0(R3) FOUND BLANK DATE                           
         BZ    R2SP3                                                            
*                                                                               
R2SP2    AHI   R3,DATLVL                                                        
         CR    R3,R2              HIT END OF TABLE?                             
         BL    R2SP1              NO                                            
         B     R2SP5                                                            
*                                                                               
R2SP3    LR    R4,R3                                                            
R2SP4    AHI   R4,DATLVL                                                        
         CR    R4,R2                                                            
         BNL   R2SP5                                                            
         CLC   0(DATLVL,R4),SPACES FIND NEXT NON BLANK DATE                     
         BNH   R2SP4                                                            
         CLC   ENDT(DATLVL,R4),SPACES                                           
         BNH   R2SP4                                                            
         MVC   0(DATLVL,R3),0(R4)   MOVE NON BLANK DATE INTO BLANK DATE         
         MVC   ENDT(DATLVL,R3),ENDT(R4)                                         
         XC    0(DATLVL,R4),0(R4)                                               
         XC    ENDT(DATLVL,R4),ENDT(R4)                                         
         AHI   R3,DATLVL                                                        
         CR    R3,R2                                                            
         BL    R2SP3                                                            
R2SP5    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE THE PROFILE FOR THE CURRENT PERSON                           *         
* SWTCHLVL CONTAINS LEVEL TO COPY PROFILE FROM                        *         
* ON NTRY P1 = TYPE OF UPDATE                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING DISTABD,R4                                                       
         USING CAPRECD,R6          NO NEED TO UPDATE PERSON LEVEL PROF          
UPDPROF  NMOD1 0,**UPDPRO**                                                     
         L     RC,SAVERC                                                        
*                                                                               
         TM    SWTCHBI2,UPDPPR1    COPYING PROFILE                              
         BZ    UPDPRO14                                                         
*                                                                               
UPDPRO02 XC    BLOCK(256),BLOCK                                                 
         LA    R6,BIGKEY                                                        
         MVC   CAPKEY,SPACES                                                    
         MVI   CAPKTYP,CAPKTYPQ    PAYROLL HISTORY RECORD                       
         MVI   CAPKSUB,CAPKSUBQ                                                 
         MVC   CAPKCPY,CMPY        COMPANY                                      
         CLI   SWTCHLVL,X'05'                                                   
         BE    UPDPRO04                                                         
         MVC   CAPKOFC,DTOFFICE                                                 
         CLI   SWTCHLVL,X'04'      OFFICE LEVEL                                 
         BE    UPDPRO04                                                         
         MVC   CAPKDPT,DTDEPT                                                   
         CLI   SWTCHLVL,X'03'      DEPARTMENT                                   
         BE    UPDPRO04                                                         
         MVC   CAPKSDT,DTSUBDPT                                                 
         CLI   SWTCHLVL,X'02'      SUBDEPARTMENT                                
         BE    UPDPRO04                                                         
*                                                                               
UPDPRO04 GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO                                                           
         LA    R3,CAPRFST-CAPRECD(R3)                                           
*                                                                               
         USING OPDELD,R3                                                        
UPDPRO06 CLI   OPDEL,0                                                          
         BE    UPDPRO12            NOT FOUND AT THIS LEVEL!                     
         CLI   OPDEL,OPDELQ                                                     
         BNE   *+12                                                             
         CLI   OPDNUM,COMCT#                                                    
         BE    UPDPRO10                                                         
UPDPRO08 LLC   R0,OPDLN                                                         
         AR    R3,R0                                                            
         B     UPDPRO06                                                         
*                                                                               
UPDPRO10 LLC   RF,OPDLN                                                         
         BCTR  RF,0                                                             
         MVC   BLOCK(0),OPDELD     SAVE OFF ELEMENT                             
         EX    RF,*-6              TO ADD TO PERSON LEVEL PROFILE               
         B     UPDPRO16                                                         
*                                                                               
UPDPRO12 CLI   SWTCHLVL,X'05'      AT AGENCY LEVEL?                             
         BNE   *+6                                                              
         DC    H'0'                SHOULD BE PROFILE AT LEAST AT                
         LLC   RF,SWTCHLVL         AGENCY LEVEL!                                
         AHI   RF,1                                                             
         STC   RF,SWTCHLVL         BUMP TO NEXT LEVEL                           
         B     UPDPRO02                                                         
*                                  TURNING THE PROFILE ON OR OFF?               
UPDPRO14 TM    SWTCHBI2,UPDPPR2+UPDPPR3                                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R3,BLOCK            BUILD OUR ELEMENT                            
         MVI   OPDEL,OPDELQ                                                     
         MVI   OPDLN,OPDLN1Q+1                                                  
         MVI   OPDNUM,COMCT#                                                    
         L     RF,ASECBLK                                                       
         MVC   OPDPERS,SECPID-SECD(RF)                                          
         MVC   OPDLAST,TODAYP                                                   
         MVI   OPDDATA,C'Y'        SWITCH PROFILE ON?                           
         TM    SWTCHBI2,UPDPPR2                                                 
         BNZ   *+8                                                              
         MVI   OPDDATA,C'N'        SWITCH PROFILE OFF                           
*                                                                               
UPDPRO16 LA    R6,BIGKEY                                                        
         MVC   CAPKEY,SPACES                                                    
         MVI   CAPKTYP,CAPKTYPQ    PAYROLL HISTORY RECORD                       
         MVI   CAPKSUB,CAPKSUBQ                                                 
         MVC   CAPKCPY,CMPY        COMPANY                                      
         MVC   CAPKOFC,DTOFFICE                                                 
         MVC   CAPKDPT,DTDEPT                                                   
         MVC   CAPKSDT,DTSUBDPT                                                 
         MVC   CAPKPER,PERSON                                                   
         GOTO1 HIGH                                                             
*                                                                               
         L     R6,AIO                  SAVE KEY FOR LATER RESTORE               
         MVC   SAVEKEYL,0(R6)          RECORD                                   
*                                                                               
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    UPDPRO18                                                         
*                                                                               
         L     R6,AIO                                                           
         L     R0,AIO                                                           
         LHI   R1,L'IO                                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   0(L'CAPKEY,R6),KEYSAVE                                           
         LHI   R0,CAPRFST-CAPRECD                                               
         STH   R0,CAPRLEN                                                       
         XC    ELEM,ELEM                                                        
         LA    R3,BLOCK                                                         
         LLC   RF,OPDLN                                                         
         BCTR  RF,0                                                             
         MVC   ELEM(0),BLOCK                                                    
         EX    RF,*-6                                                           
         GOTO1 ADDELEM             ADD OPTION ELEMENT                           
         GOTO1 ADDREC                                                           
*                                                                               
         MVC   BIGKEY,SAVEKEYL                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR',BIGKEY,BIGKEY                 
         CLC   BIGKEY(L'SAVEKEYL),SAVEKEYL                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         B     XIT                                                              
*                                                                               
UPDPRO18 GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO                                                           
         LA    R3,CAPRFST-CAPRECD(R3)                                           
*                                                                               
         USING OPDELD,R3                                                        
UPDPRO20 CLI   OPDEL,0             DOES AN ELEMENT ALREADY EXIST?               
         BE    UPDPRO26                                                         
         CLI   OPDEL,OPDELQ                                                     
         BNE   UPDPRO22                                                         
         CLI   OPDNUM,COMCT#       BRO PROFILE?                                 
         BE    UPDPRO24                                                         
UPDPRO22 LLC   R0,OPDLN                                                         
         AR    R3,R0                                                            
         B     UPDPRO20                                                         
*                                                                               
UPDPRO24 MVI   OPDEL,X'FF'                                                      
         MVI   ELCODE,X'FF'        DELETE EXISTING ELEMENT                      
         GOTO1 REMELEM                                                          
*                                                                               
UPDPRO26 XC    ELEM,ELEM                                                        
         LA    R3,BLOCK                                                         
         LLC   RF,OPDLN                                                         
         BCTR  RF,0                                                             
         MVC   ELEM(0),BLOCK                                                    
         EX    RF,*-6                                                           
         GOTO1 ADDELEM             ADD OPTION ELEMENT                           
         GOTO1 PUTREC                                                           
*                                                                               
         MVC   BIGKEY,SAVEKEYL                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR',BIGKEY,BIGKEY                 
         CLC   BIGKEY(L'SAVEKEYL),SAVEKEYL                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         B     XIT                                                              
         EJECT                                                                  
         DROP  R4,R6                                                            
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE THE COST THIRD PARTY PROFILE SETTINGS FOR THE LOCATIONS      *         
***********************************************************************         
         SPACE 1                                                                
         USING DISTABD,R4                                                       
         USING BLOCKSD,R5                                                       
UPDTHR   NMOD1 0,**UPDTHR**                                                     
         L     RC,SAVERC                                                        
*                                                                               
         MVC   AIO,AIO3            SWITCH FROM AIO1 TO AIO3                     
         LA    R4,DTADDLOC                                                      
*                                                                               
UPDTHR02 OC    0(DTLENQ,R4),0(R4)  SKIP EMPTY LOCATIONS                         
         BNZ   UPDTHR06                                                         
UPDTHR04 LA    R4,DTLENQ(R4)       NEXT TABLE ENTRY                             
         LA    R1,DISTABND         END OF TABLE                                 
         CR    R4,R1                                                            
         BL    UPDTHR02                                                         
         B     UPDTHRX                                                          
*                                                                               
UPDTHR06 CLC   DTACCNT,SPACES                                                   
         BNH   UPDTHR04                                                         
*                                                                               
         LA    RF,DTACCNT                                                       
         MVC   TEMPOFF,SPACES      BUILD 1R ACCOUNT FROM RECORD                 
         LLC   R1,LN1RLEV1         VALIDATED THAT SCREEN LENGTHS                
         BCTR  R1,0                MATCH LEVEL LENGTHS IN VR                    
         MVC   TEMPOFF(0),0(RF)                                                 
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1                                                            
*                                                                               
         MVC   TEMPDEPT,SPACES                                                  
         LLC   R1,LN1RLEV2                                                      
         BCTR  R1,0                                                             
         MVC   TEMPDEPT(0),0(RF)                                                
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1                                                            
*                                                                               
         MVC   TEMPSDPT,SPACES                                                  
         LLC   R1,LN1RLEV3                                                      
         BCTR  R1,0                                                             
         MVC   TEMPSDPT(0),0(RF)                                                
         EX    R1,*-6                                                           
         AHI   R1,1                                                             
         AR    RF,R1                                                            
*                                                                               
         MVC   TEMPPERS,SPACES                                                  
         LLC   R1,LN1RLEV4                                                      
         BCTR  R1,0                                                             
         MVC   TEMPPERS(0),0(RF)                                                
         EX    R1,*-6                                                           
*                                                                               
         USING ACTRECD,R3                                                       
         LA    R3,BIGKEY           READ 1R TO CHECK THIRD PARTY SET             
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                                                     
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),=C'1R'                              
         MVC   ACTKACT,DTACCNT                                                  
         GOTO1 READ                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO3                                                          
         LA    R3,ACTRFST                                                       
*                                                                               
         USING EMPELD,R3                                                        
UPDTHR08 CLI   EMPEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                MUST HAVE AN EMPELD                          
         CLI   EMPEL,EMPELQ                                                     
         BE    UPDTHR10                                                         
         LLC   R0,EMPLN                                                         
         AR    R3,R0                                                            
         B     UPDTHR08                                                         
*                                                                               
UPDTHR10 TM    EMPSTAT,EMPSTHRD    IS THIRD PARTY SET?                          
         BZ    UPDTHR04                                                         
*                                                                               
         USING OPDELD,R3                                                        
         LA    R3,BLOCK            BUILD OUR ELEMENT                            
         XC    BLOCK(256),BLOCK                                                 
         MVI   OPDEL,OPDELQ                                                     
         MVI   OPDLN,OPDLN1Q+1                                                  
         MVI   OPDNUM,COTHR#                                                    
         L     RF,ASECBLK                                                       
         MVC   OPDPERS,SECPID-SECD(RF)                                          
         MVC   OPDLAST,TODAYP                                                   
         MVI   OPDDATA,C'Y'                                                     
*                                                                               
         USING CAPRECD,R3                                                       
         LA    R3,BIGKEY           READ COST PROFILE                            
         MVC   CAPKEY,SPACES                                                    
         MVI   CAPKTYP,CAPKTYPQ                                                 
         MVI   CAPKSUB,CAPKSUBQ                                                 
         MVC   CAPKCPY,CMPY                                                     
         MVC   CAPKOFC,TEMPOFF                                                  
         MVC   CAPKDPT,TEMPDEPT                                                 
         MVC   CAPKSDT,TEMPSDPT                                                 
         MVC   CAPKPER,TEMPPERS                                                 
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    UPDTHR12                                                         
*                                                                               
         L     R3,AIO                                                           
         L     R0,AIO              PROFILE DOESN'T EXIST SO ADD IT              
         LHI   R1,L'IO                                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   0(L'CAPKEY,R3),KEYSAVE                                           
         LHI   R0,CAPRFST-CAPRECD                                               
         STH   R0,CAPRLEN                                                       
         XC    ELEM,ELEM                                                        
         LA    R3,BLOCK                                                         
         LLC   RF,OPDLN-OPDELD(R3)                                              
         BCTR  RF,0                                                             
         MVC   ELEM(0),BLOCK                                                    
         EX    RF,*-6                                                           
         GOTO1 ADDELEM             ADD OPTION ELEMENT                           
         GOTO1 ADDREC                                                           
         B     UPDTHR04                                                         
*                                                                               
UPDTHR12 GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO                                                           
         LA    R3,CAPRFST-CAPRECD(R3)                                           
*                                                                               
         USING OPDELD,R3                                                        
UPDTHR14 CLI   OPDEL,0             DOES AN ELEMENT ALREADY EXIST?               
         BE    UPDTHR20                                                         
         CLI   OPDEL,OPDELQ                                                     
         BNE   UPDTHR16                                                         
         CLI   OPDNUM,COTHR#       BRO PROFILE?                                 
         BE    UPDTHR18                                                         
UPDTHR16 LLC   R0,OPDLN                                                         
         AR    R3,R0                                                            
         B     UPDTHR14                                                         
*                                                                               
UPDTHR18 MVI   OPDDATA,C'Y'        UPDATE PROFILE ELEMENT                       
         L     RF,ASECBLK                                                       
         MVC   OPDPERS,SECPID-SECD(RF)                                          
         MVC   OPDLAST,TODAYP                                                   
         GOTO1 PUTREC                                                           
         B     UPDTHR04                                                         
*                                                                               
UPDTHR20 XC    ELEM,ELEM                                                        
         LA    R3,BLOCK                                                         
         LLC   RF,OPDLN                                                         
         MVC   ELEM(0),BLOCK                                                    
         EX    RF,*-6                                                           
         GOTO1 ADDELEM             ADD OPTION ELEMENT                           
         GOTO1 PUTREC                                                           
         B     UPDTHR04                                                         
*                                                                               
UPDTHRX  MVC   AIO,AIO1            RESTORE AIO TO AIO1                          
         B     XIT                                                              
         EJECT                                                                  
         DROP  R3,R4                                                            
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        MAKE SURE USER HAS ACCESS TO THIS OFFICE                               
*        WORK CONTAINS THE OFFICE TO BE TESTED                                  
***********************************************************************         
*                                                                               
OFFACC   NMOD1 0,*OFFACC*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         USING OFFALD,R3                                                        
OFF05    L     R3,AOFFBLK                                                       
         TM    BIT3,ONEBYTOF       ONE BYTE OFFICES?                            
         BZ    *+18                                                             
         OC    OFFAOLDA,OFFAOLDA   OLD OFFICE LIMIT ACCESS                      
         BZ    OFFYES              NO-UNLIMITED ACCESS                          
         B     *+14                                                             
         OC    OFFANEWA,OFFANEWA   NEW OFFICE LIMIT ACCESS                      
         BZ    OFFYES              NO-UNLIMITED ACCESS                          
*                                                                               
         LA    R3,OFFAWORK         LIST OF VALID OFFICE CODES                   
         TM    BIT3,ONEBYTOF       AGENCY HAS ONE BYTE OFFICES?                 
         BO    *+16                                                             
         LH    RF,0(R3)            RF=# OF OFFICES IN LIST                      
         LA    R3,2(R3)            BUMP PAST COUNT OF OFFICES                   
         B     *+8                                                              
         LA    RF,OLDOFLMX*2       MAX # IN LIST*2 (2 PAGES)                    
         CLI   OFFDISP,0           IS OFFICE IN 1ST POSITION?                   
         BH    *+14                                                             
         LLC   R1,LN1RLEV1                                                      
         B     *+10                                                             
         LLC   R1,LN1RLEV2         MUST BE IN 2ND POSITION                      
         BCTR  R1,0                                                             
OFF10    EXCLC R1,WORK,0(R3)                                                    
         BE    OFFYES                                                           
         LA    R3,1(R1,R3)                                                      
         TM    BIT3,ONEBYTOF       ONE BYTE OFFICES?                            
         BO    *+12                                                             
         BCT   RF,OFF10                                                         
         B     OFFNO                                                            
         CLI   0(R3),0             END OF LIST                                  
         BE    OFFNO                                                            
         BCT   RF,OFF10                                                         
*                                                                               
OFFNO    B     XNO                                                              
OFFYES   B     XYES                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*       SEE IF PERSON HAS AT LEAST ONE LOCATION WITH AN OFFICE                  
*       THAT CAN BE ACCESSED BY THE USERID                                      
***********************************************************************         
*                                                                               
DISOFFAC NMOD1 0,*DISOFF*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         CLC   TABCOUNT,=H'0'      ANY ENTRIES IN TABLE                         
         BE    DISOFFY             THAN NO REASON TO CHECK                      
         USING DISTABD,R4                                                       
DIS0     LA    R4,DTLOCS           POINT TO START OF TABLE                      
         AH    R4,STDISP                                                        
         USING OFFALD,R6                                                        
DIS01    L     R6,AOFFBLK                                                       
         TM    BIT3,ONEBYTOF                                                    
         BZ    *+18                                                             
         OC    OFFAOLDA,OFFAOLDA   OLD OFFICE LIMIT ACCESS                      
         BZ    DISOFFY             NO-SHOW ALL                                  
         B     *+14                                                             
         OC    OFFANEWA,OFFANEWA   NEW OFFICE LIMIT ACCESS                      
         BZ    DISOFFY             NO-SHOW ALL                                  
*                                                                               
         LA    R6,OFFAWORK         LST OF VALID OFFICE CODES FOR USERID         
         TM    BIT3,ONEBYTOF       ONE BYTE OFFICES?                            
         BO    *+16                # OF OFFICES IS NOT INCL IN LIST             
         LH    RF,0(R6)            RF=# OF OFFICES WHICH ARE VALID              
         LA    R6,2(R6)            BUMP PAST THIS                               
         B     *+8                                                              
         LA    RF,OLDOFLMX*2       MAX # IN LIST*2 (2 PAGES)                    
         CLI   OFFDISP,0           IS OFFICE IN 1ST LEVEL?                      
         BNE   DIS02                                                            
         LLC   R1,LN1RLEV1         LENGTH TO COMPARE                            
         LA    R3,DTOFFICE                                                      
         B     DIS10                                                            
*                                                                               
DIS02    CLC   OFFDISP,LN1RLEV1    IS OFFICE IN 2ND LEVEL?                      
         BNE   DIS04                                                            
         LA    R3,DTDEPT           OFFICE IS IN THE 2ND POSITION                
         LLC   R1,LN1RLEV2         LENGTH FOR COMPARE                           
         B     DIS10                                                            
*                                                                               
DIS04    LLC   R0,LN1RLEV1                                                      
         LLC   R1,LN1RLEV2                                                      
         AR    R1,R0               R1=LEV1+LEV2                                 
         LLC   R0,OFFDISP                                                       
         CR    R0,R1                                                            
         BE    *+8                                                              
         B     DISOFFY             DON'T RECOGNIZE-SHOW ALL                     
         LA    R3,DTSUBDPT                                                      
         LLC   R1,LN1RLEV3                                                      
*                                                                               
DIS10    BCTR  R1,0                                                             
DIS11    EXCLC R1,0(R3),0(R6)                                                   
         BE    DISOFFY                                                          
         LA    R6,1(R1,R6)         BUMP TO NEXT OFFICE IN LIST                  
         TM    BIT3,ONEBYTOF       ONE BYTE OFFICES                             
         BO    *+12                                                             
         BCT   RF,DIS11                                                         
         B     DIS15                                                            
         CLI   0(R6),0             CHECK END OF LIST                            
         BE    DIS15                                                            
         BCT   RF,DIS11                                                         
DIS15    LA    R4,DTLENQ(R4)       BUMP TO NEXT TABLE ENTRY                     
         LA    R2,DISTABND         END OF TABLE                                 
         CR    R4,R2                                                            
         BL    DIS01                                                            
         B     DISOFFN                                                          
*                                                                               
DIS25    LA    R4,DTLENQ(R4)       BUMP TO NEXT TABLE ENTRY                     
         LA    R2,DISTABND         END OF TABLE                                 
         CR    R4,R2                                                            
         BL    DIS01                                                            
*                                                                               
DISOFFN  B     XNO                                                              
DISOFFY  B     XYES                                                             
         DROP  R6,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATES PID NAME AND GETS PID NUMBER                                 
***********************************************************************         
*                                                                               
VALPID   NMOD1 0,*VALPID*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         USING SAPEREC,R6                                                       
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         MVI   SAPETYP,SAPETYPQ    C'F' - SECURITY PERSON REC                   
         MVI   SAPESUB,SAPESUBQ    X'04'                                        
         MVC   SAPEAGY,SECALPHA    SECURITY ALPHA ID                            
         MVC   SAPEPID,PIDNAME                                                  
         MVC   SAPEDEF,TODAYC      TODAYS DAY (COMPLEMENT)                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',KEY2,AIO2                    
         L     R6,AIO2                                                          
         CLC   KEY2(SAPEDEF-SAPEKEY),0(R6)                                      
         BNE   VPIDXNO                                                          
*                                                                               
         USING SAPWDD,R6                                                        
         MVI   ELCODE,SAPWDELQ     X'C4' - PERSON PASSWORD ELEM                 
         BAS   RE,GETEL2                                                        
         BNE   VPIDXNO                                                          
         MVC   PIDNUM,SAPWDNUM                                                  
*                                                                               
         USING PIDRECD,R6                                                       
         LA    R6,KEY2                                                          
         XC    KEY2,KEY2           VALIDATE NOT USED                            
         MVI   PIDKTYP,PIDKTYPQ    X'3E'                                        
         MVI   PIDKSUB,PIDKSUBQ    X'12'                                        
         MVC   PIDKCPY,CMPY                                                     
         MVC   PIDKPID,PIDNUM      NEW PID                                      
         MVI   PIDKSTYP,PIDKPERQ   SET PERSON PASSIVE FOR UK                    
         MVC   BLOCK(L'ACTKEY),KEY2                                             
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR',KEY2,KEY2                     
         CLI   DMCB+8,0                                                         
         BNE   VPIDXYES            NO KEY = OK                                  
         CLC   KEY2(PIDKPER-PIDKEY),BLOCK                                       
         BNE   VPIDXYES            NO MATCH ON PID = OK                         
*                                                                               
         CLC   PIDKPER,PERSON      SAMEPERSON = OK                              
         BE    VPIDXYES                                                         
         MVC   PERPID,PIDKPER                                                   
         MVC   GERROR,=AL2(ACEIPID)    PID ALREADY EXISTS                       
         MVI   GLTXT,L'PERPID          FOR PERSON CODE PERPID                   
         LA    R1,PERPID                                                        
         STCM  R1,7,GATXT                                                       
         J     ACCERRX                                                          
*                                                                               
VPIDXYES B     XYES                                                             
VPIDXNO  B     XNO                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OPTIONS                                                       
***********************************************************************         
*                                                                               
VALOPTS  NMOD1 0,*VALOPT*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         MVI   OPTSTAT,0                                                        
         MVI   OPTSTA2,0                                                        
         NI    BIT3,X'FF'-MOREOPTS                                              
         MVC   OPTNAME,SPACES                                                   
         MVC   OPTPID,SPACES                                                    
         MVC   OPTF15,SPACES                                                    
         LA    R2,CONOPTH                                                       
         CLI   5(R2),0                                                          
         BE    OPTX                                                             
*                                                                               
OPT05    L     R1,=A(DDTRMNT)                                                   
         A     R1,RELO                                                          
         MVC   DDTERMW,0(R1)                                                    
         GOTO1 DICTATE,DMCB,C'SU  ',DDTERMW,0                                   
         L     R1,=A(DDLOA)                                                     
         A     R1,RELO                                                          
         MVC   DDLOAW,0(R1)                                                     
         GOTO1 DICTATE,DMCB,C'SU  ',DDLOAW,0                                    
         L     R1,=A(DDACT)                                                     
         A     R1,RELO                                                          
         MVC   DDACTW,0(R1)                                                     
         GOTO1 DICTATE,DMCB,C'SU  ',DDACTW,0                                    
         L     R1,=A(DDALT)                                                     
         A     R1,RELO                                                          
         MVC   DDALTW,0(R1)                                                     
         GOTO1 DICTATE,DMCB,C'SU  ',DDALTW,0                                    
*                                                                               
         XC    BLOCK+L'PVALOUTB(200),BLOCK+L'PVALOUTB                           
         GOTO1 VPARSNIP,DMCB,(0,CONOPTH),BLOCK+L'PVALOUTB,             *        
               ('PSNNONLQ',SEPTAB)                                              
         CLI   DMCB+8,0                                                         
         BNE   EINVOPT                                                          
         LA    R3,BLOCK+L'PVALOUTB                                              
         USING PSND,R3                                                          
*                                                                               
OPT10    L     RF,PSNCOMP          A(COMPONENT)                                 
         LLC   R1,PSNLEN                                                        
         AHI   R1,-1                                                            
         BM    OPTX                                                             
         EX    R1,EXOPTERM         OPTION TERMINATED                            
         BE    OPT20                                                            
         EX    R1,EXOPTACT         OPTION ACTIVE                                
         BE    OPT22                                                            
         EX    R1,EXOPTLOA         OPTION LOA                                   
         BE    OPT24                                                            
         EX    R1,EXOPNAME         OPTION NAME                                  
         BE    OPT30                                                            
         EX    R1,EXOPTPID         OPTION PID                                   
         BE    OPT40                                                            
         EX    R1,EXOPTALT         OPTION ALT                                   
         BE    OPT42                                                            
         EX    R1,EXOPPIDN         OPTION PID#                                  
         BE    OPT45                                                            
         EXCLC R1,0(RF),=C'XD'     XD=Y                                         
         BE    OPT15                                                            
         LA    RE,OPTFLT1                                                       
         EXCLC R1,0(RF),=C'F1'     F1=..                                        
         BE    OPT50                                                            
         LA    RE,OPTFLT2                                                       
         EXCLC R1,0(RF),=C'F2'     F2=..                                        
         BE    OPT50                                                            
         LA    RE,OPTFLT3                                                       
         EXCLC R1,0(RF),=C'F3'     F3=..                                        
         BE    OPT50                                                            
         LA    RE,OPTFLT4                                                       
         EXCLC R1,0(RF),=C'F4'     F4=..                                        
         BE    OPT50                                                            
         LA    RE,OPTFLT5                                                       
         EXCLC R1,0(RF),=C'F5'     F5=..                                        
         BE    OPT50                                                            
         MVC   CONOPT,SPACES       CLEAR OUT OPTIONS FIELD                      
         MVI   CONOPTH+5,0                                                      
         OI    CONOPTH+6,X'80'                                                  
         B     OPTX                                                             
OPT15    CLI   ACTEQU,ACTDIS       DISPLAY ONLY FOR XD=Y                        
         BE    OPT80                                                            
         MVC   CONOPT,SPACES       CLEAR OUT OPTIONS FIELD                      
         MVI   CONOPTH+5,0                                                      
         OI    CONOPTH+6,X'80'                                                  
         B     OPTX                                                             
*                                                                               
OPT20    CLI   ACTEQU,ACTLIST                                                   
         BNE   OPTXX                                                            
         OC    PSNFLD,PSNFLD       MORE OPTIONS?                                
         BZ    *+8                                                              
         OI    BIT3,MOREOPTS                                                    
         LA    R3,PSNL(R3)         A(FIRST/NEXT VALUE)                          
         L     RF,PSNCOMP          A(COMPONENT)                                 
         LLC   R1,PSNLEN                                                        
         AHI   R1,-1                                                            
         BM    ERRIGHT                                                          
         EX    R1,EXOPTNO          TERM=NO                                      
         BNE   *+12                                                             
         OI    OPTSTAT,NOTERM                                                   
         B     OPT60                                                            
         EX    R1,EXOPTNLY         TERM=ONLY                                    
         BNE   *+12                                                             
         OI    OPTSTAT,ONLYTERM                                                 
         B     OPT60                                                            
         EX    R1,EXOPTYES         TERM=YES(DEFAULT)                            
         BNE   EINVOPT                                                          
         B     OPT60                                                            
*                                                                               
OPT22    CLI   ACTEQU,ACTLIST                                                   
         BNE   OPTXX                                                            
         OC    PSNFLD,PSNFLD       MORE OPTIONS?                                
         BZ    *+8                                                              
         OI    BIT3,MOREOPTS                                                    
         LA    R3,PSNL(R3)         A(FIRST/NEXT VALUE)                          
         L     RF,PSNCOMP          A(COMPONENT)                                 
         LLC   R1,PSNLEN                                                        
         AHI   R1,-1                                                            
         BM    ERRIGHT                                                          
         EX    R1,EXOPTNO          ACT=NO                                       
         BNE   *+12                                                             
         OI    OPTSTA2,NOACT                                                    
         B     OPT60                                                            
         EX    R1,EXOPTNLY         ACT=ONLY                                     
         BNE   *+12                                                             
         OI    OPTSTA2,ONLYACT                                                  
         B     OPT60                                                            
         EX    R1,EXOPTYES         ACT=YES(DEFAULT)                             
         BNE   EINVOPT                                                          
         B     OPT60                                                            
*                                                                               
OPT24    CLI   ACTEQU,ACTLIST                                                   
         BNE   OPTXX                                                            
         OC    PSNFLD,PSNFLD       MORE OPTIONS?                                
         BZ    *+8                                                              
         OI    BIT3,MOREOPTS                                                    
         LA    R3,PSNL(R3)         A(FIRST/NEXT VALUE)                          
         L     RF,PSNCOMP          A(COMPONENT)                                 
         LLC   R1,PSNLEN                                                        
         AHI   R1,-1                                                            
         BM    ERRIGHT                                                          
         EX    R1,EXOPTNO          LOA=NO                                       
         BNE   *+12                                                             
         OI    OPTSTA2,NOLOA                                                    
         B     OPT60                                                            
         EX    R1,EXOPTNLY         LOA=ONLY                                     
         BNE   *+12                                                             
         OI    OPTSTA2,ONLYLOA                                                  
         B     OPT60                                                            
         EX    R1,EXOPTYES         LOA=YES(DEFAULT)                             
         BNE   EINVOPT                                                          
         B     OPT60                                                            
*                                                                               
OPT30    CLI   ACTEQU,ACTLIST                                                   
         BNE   OPTXX                                                            
         OC    PSNFLD,PSNFLD       MORE OPTIONS                                 
         BZ    *+8                                                              
         OI    BIT3,MOREOPTS                                                    
         LA    R3,PSNL(R3)                                                      
         L     RF,PSNCOMP                                                       
         LLC   R1,PSNLEN                                                        
         AHI   R1,-1                                                            
         BM    ERRIGHT                                                          
         EXMVC R1,OPTNAME,0(RF)    SAVE NAME                                    
         OC    OPTNAME,SPACES                                                   
         OI    OPTSTAT,NAMEOPT                                                  
         B     OPT60                                                            
*                                                                               
OPT40    CLI   ACTEQU,ACTLIST                                                   
         BNE   OPTXX                                                            
         OC    PSNFLD,PSNFLD       MORE OPTIONS                                 
         BZ    *+8                                                              
         OI    BIT3,MOREOPTS                                                    
         LA    R3,PSNL(R3)                                                      
         L     RF,PSNCOMP                                                       
         LLC   R1,PSNLEN                                                        
         AHI   R1,-1                                                            
         BM    ERRIGHT                                                          
         EXMVC R1,OPTPID,0(RF)     SAVE PID                                     
         OC    OPTPID,SPACES                                                    
         OI    OPTSTAT,PIDOPT                                                   
         B     OPT60                                                            
*                                                                               
OPT42    CLI   ACTEQU,ACTLIST      ALT= OPTION                                  
         BNE   OPTXX                                                            
         OC    PSNFLD,PSNFLD       MORE OPTIONS                                 
         BZ    *+8                                                              
         OI    BIT3,MOREOPTS                                                    
         LA    R3,PSNL(R3)                                                      
         L     RF,PSNCOMP          A(COMPONENT)                                 
         LLC   R1,PSNLEN                                                        
         AHI   R1,-1                                                            
         BM    ERRIGHT                                                          
         EX    R1,EXOPTDAT         ALT=DATE                                     
         BNE   EINVOPT                                                          
         OI    OPTSTAT,ALTDATE                                                  
         B     OPT60                                                            
*                                                                               
OPT45    CLI   ACTEQU,ACTLIST                                                   
         BNE   OPTXX                                                            
         CLI   TWAOFFC,C'*'       PID# OPT ONLY VALID FOR DDS TERMINALS         
         BNE   EINVOPT                                                          
         OC    PSNFLD,PSNFLD       MORE OPTIONS                                 
         BZ    *+8                                                              
         OI    BIT3,MOREOPTS                                                    
         LA    R3,PSNL(R3)                                                      
         L     R2,PSNCOMP                                                       
         LLC   R4,PSNLEN                                                        
         GOTO1 HEXIN,DMCB,0(R2),OPTPID,(R4)                                     
         OI    OPTSTAT,PIDNOOPT                                                 
         B     OPT60                                                            
*                                                                               
OPT50    CLI   ACTEQU,ACTLIST      F1-F5                                        
         BNE   OPTXX                                                            
         OC    PSNFLD,PSNFLD       MORE OPTIONS?                                
         BZ    *+8                                                              
         OI    BIT3,MOREOPTS                                                    
         LA    R3,PSNL(R3)         A(FIRST/NEXT VALUE)                          
         L     RF,PSNCOMP          A(COMPONENT)                                 
         LLC   R1,PSNLEN                                                        
         CHI   R1,1                                                             
         BL    ERRIGHT                                                          
         BE    OPT52                                                            
         CLI   0(RF),C'*'                                                       
         BNE   EINVOPT                                                          
         CHI   R1,2                                                             
         BNE   ERRIGHT                                                          
         MVC   0(1,RE),1(RF)                                                    
         NI    0(RE),X'FF'-X'40'                                                
         B     OPT60                                                            
OPT52    MVC   0(1,RE),0(RF)                                                    
         B     OPT60                                                            
*                                                                               
OPT60    TM    BIT3,MOREOPTS                                                    
         BZ    OPTX                                                             
         NI    BIT3,X'FF'-MOREOPTS                                              
         LA    R3,PSNL(R3)                                                      
         B     OPT10                                                            
*                                                                               
OPT80    LA    R3,PSNL(R3)         A(FIRST/NEXT VALUE)                          
         L     RF,PSNCOMP          A(COMPONENT)                                 
         LLC   R1,PSNLEN                                                        
         AHI   R1,-1                                                            
         BM    ERRIGHT                                                          
         EX    R1,EXOPTYES                                                      
         BNE   *+12                                                             
         OI    OPTSTAT,XDOPT                                                    
         B     OPTX                                                             
         EX    R1,EXOPTNO                                                       
         BE    OPTX                                                             
         B     EINVOPT                                                          
*                                                                               
EXOPTERM CLC   DDTERMW(0),0(RF)    TERM                                         
EXOPTACT CLC   DDACTW(0),0(RF)     ACTIVE                                       
EXOPTLOA CLC   DDLOAW(0),0(RF)     LOA                                          
EXOPTNO  CLC   AC@NOU(0),0(RF)        =NO                                       
EXOPTNLY CLC   AC@ONLYU(0),0(RF)      =ONLY                                     
EXOPTYES CLC   AC@YESU(0),0(RF)       =YES (DEFAULT)                            
EXOPTDAT CLC   AC@DATE(0),0(RF)       =DATE                                     
EXOPNAME CLC   AC@NAME(0),0(RF)    NAME                                         
EXOPTPID CLC   AC@PID(0),0(RF)     PID                                          
EXOPTALT CLC   DDALTW(0),0(RF)     ALT                                          
EXOPPIDN CLC   AC@PIDNO(0),0(RF)   PID#                                         
*                                                                               
OPTXX    MVC   CONOPT,SPACES       CLEAR OUT OPTIONS FIELD                      
         MVI   CONOPTH+5,0                                                      
         OI    CONOPTH+6,X'80'                                                  
*                                                                               
OPTX     B     XIT                                                              
*                                                                               
SEPTAB   DC    AL1(1),C'='         FIELD DIVIDER                                
         DC    AL1(3),C',;#'                                                    
         DC    AL1(3),C',;#'                                                    
         DC    AL1(3),C',;#'                                                    
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE SAL LOCK DATE AND UPDATE TABLE ENTRY                          
*        R2 POINTS TO SCREEN LINE                                               
*        R4 POINTS TO DISTAB ENTRY                                              
***********************************************************************         
*                                                                               
         USING LLINED,R2                                                        
         USING DISTABD,R4                                                       
VALSALKD NMOD1 0,*VALSAL*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         XC    SALLOCK,SALLOCK                                                  
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         CLI   LLSALDH+5,0                                                      
         BE    VSAL10                                                           
         LA    R1,LLSALD                                                        
         ST    R1,DMCB                                                          
         LLC   R1,LLSALDH+5                                                     
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,X'04'                                                     
         BE    *+12                                                             
         LA    R2,LLSALDH          SALARY LOCK DATE                             
         B     EINVDATE                                                         
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         MVC   SALLOCK,PVALPSTA   PWOS                                          
*                                                                               
VSAL10   CLC   SALLOCK,DTSALKDT    MUST CHECK FOR SAL IF NEW DATE               
         BNL   VSAL50              IS EARLIER THAN OLD DATE                     
*                                                                               
         OC    DTENDATE,DTENDATE   OK TO REMOVE SAL DATE IF NO END DATE         
         BZ    VSAL50                                                           
         MVC   STDATE,DTENDATE     USE LOC END DATE FOR START                   
         OC    SALLOCK,SALLOCK     IF REMOVING SAL LK DATE                      
         BZ    VSAL20                                                           
*                                                                               
         MVC   YYMMDD1,PVALESTA                                                 
         GOTO1 ADDAY,DMCB,YYMMDD1,YYMMDD2,F'1'      NO SAL BETWEEN              
         GOTO1 DATCON,DMCB,(0,YYMMDD2),(1,STDATE)   NEW DATE +1                 
*                                                                               
VSAL20   MVC   ENDATE,DTSALKDT                      AND OLD DATE                
         MVC   TEMPOFF,DTOFFICE                                                 
         MVC   TEMPDEPT,DTDEPT                                                  
         MVC   TEMPSDPT,DTSUBDPT                                                
         MVC   ACCNT,DTACCNT                                                    
         GOTO1 =A(CHKSAL),DMCB,RR=RELO                                          
         BE    VSAL50                                                           
         LA    R2,LLSALDH                                                       
         B     EDSALS              CAN'T CHANGE - SALARY EXISTS                 
*                                                                               
VSAL50   MVC   DTSALKDT,SALLOCK                                                 
         OC    DTSALKDT,DTSALKDT                                                
         BZ    VSALX                                                            
         CLC   DTSALKDT,DTENDATE                                                
         BNL   VSALX                                                            
         LA    R2,LLSALDH                                                       
         B     ERRSALBE            SALARY LOCK DATE IS BEFORE END               
*                                                                               
VSALX    B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
*        ERROR MESSAGES                                                         
***********************************************************************         
*                                  GENERAL MESSAGES                             
ERRSALBE MVC   GERROR,=AL2(ACESALBE)   SALARY LOCK DATE BEFORE END DATE         
         J     ACCERRX                                                          
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        GET PAY CODE AND MONTHS FOR AUTO ADD HISTORY                 *         
***********************************************************************         
         SPACE 1                                                                
         USING PERRECD,R6                                                       
GETPCAM  NMOD1 0,*08GPC*                                                        
         L     RC,SAVERC                                                        
         L     R6,AIO                                                           
         XC    GPCODE,GPCODE                                                    
         XC    GPMONS,GPMONS                                                    
         MVC   GPM1RS,SPACES                                                    
                                                                                
* CALL OR RETRIVE IF SAVED ALREADY COBLOCK FOR PAY CODE                         
                                                                                
         MVC   GPCODE,SVCPYPCD                                                  
                                                                                
         USING LOCELD,R6                                                        
         MVI   ELCODE,LOCELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         OC    LOCEND,LOCEND       SKIP IF LOCATION NOT VALID                   
         BNZ   GETPCAMN                                                         
         CLI   LOCSTAT,LOCSACT                                                  
         BNE   GETPCAMN                                                         
                                                                                
         MVC   GPMONS,LOCSTART     SET START OF RANGE                           
                                                                                
         MVC   GPM1RS(L'PHIKOFC),LOCOFF                                         
         MVC   GPM1RS+L'PHIKOFC(L'PHIKDPT),LOCDEPT                              
         MVC   GPM1RS+L'PHIKOFC+L'PHIKDPT(L'PHIKSBD),LOCSUB                     
                                                                                
         USING PERRECD,R6                                                       
         L     R6,AIO                                                           
         MVC   GPM1RS+L'PHIKOFC+L'PHIKDPT+L'PHIKSBD(L'PHIKPER),PERKCODE         
                                                                                
GETPCAMY CR    RB,RB                                                            
         B     *+6                                                              
GETPCAMN LTR   RB,RB                                                            
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        AUTO ADD HISTORY RECORDS FOR PERSON                          *         
***********************************************************************         
         SPACE 1                                                                
         USING PHIRECD,R6                                                       
ADDHIST  NMOD1 0,*08ADH*                                                        
         L     RC,SAVERC                                                        
                                                                                
         MVC   DUB,AIO                                                          
         MVI   BYTE,X'12'                                                       
         MVC   GPBKEY,BIGKEY                                                    
                                                                                
ADDHIST2 LA    R6,BIGKEY                                                        
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ                                                 
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,CMPY                                                     
         MVC   PHIKOFC(PHILENQ),GPM1RS                                          
         MVC   HALF(1),GPMONS                                                   
         MVC   HALF+1(1),BYTE                                                   
         XR    R1,R1                                                            
         ICM   R1,3,HALF                                                        
         LNR   R1,R1                                                            
         STCM  R1,3,PHIKMOA                                                     
         MVI   PHIKSEQ,0                                                        
                                                                                
         OI    DMINBTS,X'88'       READ FOR DELETES AND UPDATE                  
         MVI   GPFLAG,0                                                         
                                                                                
         MVC   KEYSAVE(L'PHIKEY),BIGKEY                                         
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(L'PHIKEY),BIGKEY                                         
         BNE   ADDHIST4                                                         
         TM    PHIKSTAT-PHIRECD+BIGKEY,X'80'  DELETED RECORD?                   
         BZ    ADDHISTN                                                         
         MVI   GPFLAG,1                                                         
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
                                                                                
ADDHIST4 MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
         LR    RE,R6                                                            
         L     RF,SIZEIO                                                        
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVC   PHIKEY,KEYSAVE                                                   
         MVC   PHIRLEN,=AL2(PHIRFST-PHIRECD)                                    
                                                                                
         USING PDEELD,R6                                                        
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   PDEEL,PDEELQ                                                     
         MVI   PDELN,PDELNQ                                                     
         MVC   PDEDTE(1),GPMONS                                                 
         MVC   PDEDTE+1(1),BYTE                                                 
         MVI   PDEDTE+2,X'01'                                                   
         MVC   PDENUM,GPCODE                                                    
         ZAP   PDEAMT,=P'1'                                                     
         ZAP   PDEADJ,=P'0'                                                     
         OI    PDESTAT2,PDESHRTE                                                
         DROP  R6                                                               
                                                                                
         MVI   ERROR,0             F1 ELEMENT NOT ADDED IN HERE                 
         GOTO1 ADDELEM                                                          
         CLI   ERROR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING PHIRECD,R6                                                       
         L     R6,AIO                                                           
                                                                                
         CLI   GPFLAG,1            UNDELETE?                                    
         BE    ADDHIST6                                                         
         GOTO1 ADDREC                                                           
         B     ADDHIST8                                                         
                                                                                
ADDHIST6 NI    PHIKSTAT-PHIRECD+BIGKEY,X'FF'-X'80'                              
         GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
                                                                                
ADDHIST8 LLC   RE,BYTE                                                          
         SHI   RE,1                                                             
         CHI   RE,X'0F'                                                         
         BNE   *+8                                                              
         LA    RE,X'09'                                                         
         STC   RE,BYTE                                                          
         CLC   BYTE,GPMONS+1                                                    
         BNL   ADDHIST2                                                         
                                                                                
ADDHISTY CR    RB,RB                                                            
         B     *+6                                                              
ADDHISTN LTR   RB,RB                                                            
         MVC   AIO,DUB                                                          
         MVC   BIGKEY,GPBKEY                                                    
         XIT1                                                                   
         DROP  R6                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE STATUS SCREEN AND UPDATE PERSON RECORD                        
***********************************************************************         
*                                                                               
         USING DISTABD,R4          TABLE                                        
VALSTSCR NMOD1 0,**VSS***          DO NOT USE R7                                
         L     RC,SAVERC                                                        
*                                                                               
         BRAS  RE,CURRSEL          RETURNS CURRENT SELECTION AT R4              
         BNE   VSSX                                                             
*                                                                               
***********************************************************************         
*        VALIDATE TIMESHEET LOCK DATE                                           
***********************************************************************         
*                                                                               
VSS10    XC    TSLOCK,TSLOCK                                                    
         LA    R2,PESTSLDH                                                      
         CLI   5(R2),0                                                          
         BE    VSS12                                                            
         OC    PESTSLD,SPACES                                                   
         CLC   AC@NOU,PESTSLD      NO TO REMOVE                                 
         BE    VSS12                                                            
*                                                                               
         LA    R1,PESTSLD          VALIDATE DATE                                
         ST    R1,DMCB                                                          
         LLC   R1,PESTSLDH+5                                                    
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,X'04'                                                     
         BNE   EINVDATE                                                         
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         MVC   TSLOCK,PVALPSTA     PWOS                                         
         MVC   YYMMDD1,PVALESTA                                                 
         B     VSS14                                                            
*                                                                               
VSS12    OC    DTENDATE,DTENDATE   IF REMOVING TS LOCK DATE MAKE SURE           
         BZ    VSS15               END DATE COVERS TIME                         
         GOTO1 DATCON,DMCB,(1,DTENDATE),(0,YYMMDD1)                             
*                                                                               
VSS14    GOTO1 ADDAY,DMCB,YYMMDD1,YYMMDD2,F'1'      NO FUTURE TIME              
         GOTO1 DATCON,DMCB,(0,YYMMDD2),(1,STDATE)   NEW DATE +1                 
         XC    ENDATE,ENDATE                                                    
         MVC   TEMPOFF,DTOFFICE                                                 
         MVC   TEMPDEPT,DTDEPT                                                  
         MVC   TEMPSDPT,DTSUBDPT                                                
         MVC   ACCNT,DTACCNT                                                    
         GOTO1 =A(CHKTIME),DMCB,RR=RELO                                         
         BNE   EDPOST              CAN'T CHANGE - POSTINGS EXIST                
         TM    BIT2,NEWTIME                                                     
         BZ    VSS14A                                                           
         GOTO1 =A(CHKSVTMS),DMCB,RR=RELO CHECK FOR SAVED TMS RECS               
         BNE   ERRSVTMS                                                         
VSS14A   TM    BIT2,NEWTIME        ON TMS                                       
         BO    VSS15                                                            
         GOTO1 =A(DRAFTRNS),DMCB,RR=RELO ELSE CHECK FOR DRAFT TIME              
         BE    ERRDRAFT                                                         
VSS15    MVC   DTTSLKDT,TSLOCK                                                  
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE ATTRIBUTES                                                    
***********************************************************************         
*                                                                               
         USING ATTRD,R3                                                         
VSS20    L     R3,=A(ATTRTAB)      ATTRIBUTE TABLE                              
         A     R3,RELO                                                          
VSS20NX  CLI   0(R3),0             EOT                                          
*&&UK*&& BE    VSS30                                                            
*&&US*&& BE    VSS28                                                            
*                                                                               
         LA    R2,CONTAGH          DISPLACEMENTS ARE FROM CONTAGH               
         AH    R2,ATTRDISP         DISPLACEMENT TO ATTRIBUTE FIELD              
         CLC   8(1,R2),AC@YESU                                                  
         BNE   *+14                                                             
         OC    DTATTR,ATTRBIT      TURN ON CORRESPONDING BIT                    
         B     VSS25                                                            
         CLC   8(1,R2),AC@NOU                                                   
         BNE   ERRINV                                                           
         MVI   BYTE,X'FF'                                                       
         XC    BYTE,ATTRBIT                                                     
         NC    DTATTR,BYTE         TURN OFF CORRESPONDING BIT                   
*                                                                               
VSS25    LA    R3,ATTRLNQ(R3)                                                   
         B     VSS20NX                                                          
*&&US                                                                           
*                                                                               
* FREELANCER BIT                                                                
*                                                                               
VSS28    CLI   DTSTATUS,LOCSACT    REQUIRES PID FOR MCS                         
         BNE   VSS30                                                            
         LA    R2,PESFREEH         DISPLACEMENT TO ATTRIBUTE FIELD              
         CLC   8(1,R2),AC@YESU     FREELANCER?                                  
         BNE   *+22                                                             
         NI    DTATTR,X'FF'-LOCBFRL    TURN OFF NON-CORRESPONDING BITS          
         OI    DTATTR,LOCYFRL      TURN ON CORRESPONDING BIT                    
         MVC   DTFREE,AC@YESU      SAVE OFF THE VALUE                           
         B     VSS30                                                            
         CLI   8(R2),C'B'          BRANDO FREELANCER?                           
         BNE   *+20                                                             
         NI    DTATTR,X'FF'-LOCYFRL    TURN OFF NON-CORRESPONDING BITS          
         OI    DTATTR,LOCBFRL      TURN ON CORRESPONDING BIT                    
         MVI   DTFREE,C'B'         SAVE OFF THE VALUE                           
         B     VSS30                                                            
         CLC   8(1,R2),AC@NOU                                                   
         BNE   ERRINV                                                           
         MVC   DTFREE,AC@NOU       SAVE OFF THE VALUE                           
         MVI   BYTE,X'FF'                                                       
         XI    BYTE,LOCYFRL+LOCBFRL                                             
         NC    DTATTR,BYTE         TURN OFF CORRESPONDING BIT                   
*&&                                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE ANALYSIS ACCOUNT                                              
***********************************************************************         
*                                                                               
VSS30    LA    R2,PESANALH                                                      
         MVC   ANALACCT,SPACES                                                  
         CLI   PESANALH+5,0        ANY ACCOUNT                                  
         BE    VSS40                                                            
         CLI   PESANALH+5,1        MUST BE LENGTH 1                             
         BNE   ERRINV                                                           
         MVC   ANALACCT,PESANAL                                                 
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,KEY2             VALIDATE IN 14                               
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                          COMPANY                    
         MVI   ACTKUNT,C'1'                          UNIT 1                     
         MVI   ACTKLDG,C'4'                          LEDGER 4                   
         MVC   ACTKACT(L'ANALACCT),ANALACCT                                     
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         CLC   KEY2(L'ACTKEY),KEYSAVE                                           
         BNE   EINVANAL                                                         
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OVERRIDE INCOME ACCOUNT                                       
***********************************************************************         
*                                                                               
VSS40    LA    R2,PESOINH                                                       
         MVC   OINCACCT,SPACES                                                  
         CLI   PESOINH+5,0                                                      
         BE    VSS50                                                            
         MVC   OINCACCT,PESOIN                                                  
         OC    OINCACCT,SPACES                                                  
         CLC   =C'SI',OINCACCT                                                  
         BE    VSS42                                                            
         CLC   =C'SK',OINCACCT                                                  
         BNE   ERRINV                                                           
*                                                                               
VSS42    CLC   =C'SI=',OINCACCT    LOOKING TO SEARCH?                           
         BE    *+14                                                             
         CLC   =C'SK=',OINCACCT                                                 
         BNE   VSS45                                                            
         GOTO1 VACSRCHC,DMCB,(4,(R2)),ATWA,0,ACOMFACS,(0,0)                     
*                                                                               
         USING ACTRECD,R6                                                       
VSS45    LA    R6,KEY2             VALIDATE ACCOUNT                             
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                          COMPANY                    
         MVC   ACTKULA,OINCACCT                                                 
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         CLC   KEY2(L'ACTKEY),KEYSAVE                                           
         BNE   ERRINV                                                           
         LA    R3,KEY2             GET RECORD                                   
         AH    R3,LKEY                                                          
         AH    R3,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST ',(R3),AIO2,WORK               
         L     R6,AIO2                                                          
         MVI   ELCODE,ABLELQ       X'32' BALANCE ELEM                           
         BAS   RE,GETEL            MUST BE LOW LEVEL ACCOUNT                    
         BNE   ERRINV                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OVERRIDE WRITEOFF ACCOUNT                                     
***********************************************************************         
*                                                                               
VSS50    LA    R2,PESOWOH                                                       
         MVC   OWOACCT,SPACES                                                   
         CLI   PESOWOH+5,0                                                      
         BE    VSS60                                                            
         MVC   OWOACCT,PESOWO                                                   
         OC    OWOACCT,SPACES                                                   
         CLC   =C'SI',OWOACCT                                                   
         BNE   ERRINV                                                           
         CLC   =C'SI=',OWOACCT      LOOKING TO SEARCH                           
         BNE   VSS55                                                            
         GOTO1 VACSRCHC,DMCB,(4,(R2)),ATWA,0,ACOMFACS,(0,0)                     
*                                                                               
         USING ACTRECD,R6                                                       
VSS55    LA    R6,KEY2             VALIDATE ACCOUNT                             
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                          COMPANY                    
         MVC   ACTKULA,OWOACCT                                                  
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         CLC   KEY2(L'ACTKEY),KEYSAVE                                           
         BNE   ERRINV                                                           
         LA    R3,KEY2             GET RECORD                                   
         AH    R3,LKEY                                                          
         AH    R3,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST ',(R3),AIO2,WORK               
         L     R6,AIO2                                                          
         MVI   ELCODE,ABLELQ       X'32' BALANCE ELEM                           
         BAS   RE,GETEL            MUST BE LOW LEVEL ACCOUNT                    
         BNE   ERRINV                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE DEFAULT TASK CODE                                             
***********************************************************************         
*                                                                               
VSS60    LA    R2,PESDTSKH                                                      
         MVC   TASK,SPACES                                                      
         CLI   PESDTSKH+5,0                                                     
         BE    VSS70                                                            
*&&UK                                                                           
         CLC   PESDTSK,=CL2'**'    DISALLOW ORDER WORKCODE '**'                 
         BE    ERRINV                                                           
*&&                                                                             
         USING WCORECD,R6                                                       
         LA    R6,KEY2             VALIDATE WORK CODE                           
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ    X'0A'                                        
         MVC   WCOKCPY,CMPY                                                     
         MVC   WCOKUNT(2),=C'SJ'                                                
         MVC   WCOKWRK,PESDTSK                                                  
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         CLC   KEY2(L'WCOKEY),KEYSAVE                                           
         BNE   ERRINV                                                           
         MVC   TASK,PESDTSK                                                     
         OC    TASK,SPACES                                                      
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE PERSON RECORD ELEMENTS FROM TABLE ENTRY                         
***********************************************************************         
*                                                                               
VSS70    GOTO1 =A(ELEM83),DMCB,RR=RELO     UPDATE '83' ELEM                     
         GOTO1 =A(ELEM56),DMCB,RR=RELO   UPDATE '56' ELEM                       
VSSX     XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        ERROR MESSAGES                                                         
***********************************************************************         
*                                  GENERAL MESSAGES                             
EINVANAL MVC   GERROR,=AL2(ACEIANAL)   INVALID ANALYSIS ACCOUNT                 
         J     ACCERRX                                                          
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        MARK THE RIGHT ENTRY IN TABLE AS CURRENT SELECTION                     
***********************************************************************         
                                                                                
         USING DISTABD,R4          TABLE                                        
SELECTS  NMOD1 0,*SELECTS          DO NOT USE R7                                
         L     RC,SAVERC                                                        
*                                                                               
         GOTO1 =A(DISOFFAC),DMCB,RR=RELO  MAKE SURE VALID FOR DIS               
         BE    *+12                                                             
         LA    R2,PESCODEH                                                      
         B     EINVSECL                                                         
         CLI   RECNUM,RTPR2        ON PER2 SCREEN ALREADY                       
         BNE   SEL30               GET FIRST SELECTION                          
         BAS   RE,NEWODS           WAS OFF/DEPT/SUB ENTERED                     
         BE    SEL10               YES, GET MATCHING ENTRY                      
         BAS   RE,OLDODS           OFF/DEPT/SUB ALREADY THERE                   
         BNE   *+12                                                             
         CLI   PFKEY,11            TRYING TO GET NEXT SEL                       
         BNE   SEL10                                                            
         CLI   PFKEY,11            DO THEY WANT NEXT SELECTED                   
         BE    SEL30               GET NEXT SELECTION                           
         BRAS  RE,CURRSEL          IS THERE A CURRENT SELECTION                 
         BNE   SEL20               NO, USE CURRENT LOCATION                     
         B     SELX                YES KEEP CURRENT SELECTED                    
         EJECT                                                                  
***********************************************************************         
*        MARK MATCHING LOCATION AS CURRENT                                      
***********************************************************************         
*                                                                               
SEL10    LA    R4,DTADDLOC         FIND MATCHING LOCATION IN TABLE              
         AH    R4,STDISP                                                        
SEL12    OC    0(DTLENQ,R4),0(R4)                                               
         BZ    SEL12NX                                                          
         CLC   DTOFFICE,PESOFF                                                  
         BNE   SEL12NX                                                          
         CLC   DTDEPT,PESDEPT                                                   
         BNE   SEL12NX                                                          
         CLC   DTSUBDPT,PESSDPT                                                 
         BE    SEL14                                                            
SEL12NX  NI    DTLSTAT,X'FF'-DTSCURR    UNMARK ANY CURR ALONG THE WAY           
         BAS   RE,NEWODS                WAS OFF/DEPT/SUB ENTERED                
         BNE   *+8                                                              
         NI    DTLSTAT,X'FF'-DTSSEL     UNMARK SELECT - CAN'T GET NEXT          
         LA    R4,DTLENQ(R4)       NEXT TABLE ENTRY                             
         LA    R1,DISTABND         END OF TABLE                                 
         CR    R4,R1                                                            
         BL    SEL12                                                            
         LA    R2,PESOFFH          NO MATCH                                     
         B     ERRINV                                                           
SEL14    GOTO1 =A(POSOFF),DMCB,RR=RELO FIGURE OFFICE DISP                       
         BE    SEL15                                                            
         GOTO1 =A(OFFACC),DMCB,RR=RELO CHECK OFFICE ACCESS                      
         BE    *+12                                                             
         LA    R2,PESOFFH                                                       
         B     EINVSECL                                                         
*                                                                               
SEL15    OI    DTLSTAT,DTSCURR          MARK AS CURRENT SELECTION               
         B     SELX                                                             
         EJECT                                                                  
***********************************************************************         
*        MARK MOST RECENT LOCATION AS CURRENT                                   
***********************************************************************         
*                                                                               
SEL20    LA    R4,DTADDLOC         USE FIRST (CURRENT) LOCATION                 
SEL22    OC    0(DTLENQ,R4),0(R4)                                               
         BZ    SEL22NX                                                          
         GOTO1 =A(POSOFF),DMCB,RR=RELO GET OFFICE POSITION                      
         BE    SEL22A                                                           
         GOTO1 =A(OFFACC),DMCB,RR=RELO CHECK OFFICE ACCESS                      
         BNE   SEL22NX                                                          
SEL22A   OI    DTLSTAT,DTSCURR     MARK AS CURRENT SELECTION                    
         B     SELX                                                             
SEL22NX  LA    R4,DTLENQ(R4)       NEXT TABLE ENTRY                             
         LA    R1,DISTABND         END OF TABLE                                 
         CR    R4,R1                                                            
         BL    SEL22                                                            
         LA    R2,PESOFFH          NO ENTRIES EXIST                             
         B     ERRINV                                                           
         EJECT                                                                  
***********************************************************************         
*        UNMARK CURRENT AND LOOK FOR NEXT OR FIRST SELECTED                     
***********************************************************************         
*                                                                               
SEL30    LA    R4,DTADDLOC         USE FIRST (CURRENT) LOCATION                 
         AH    R4,STDISP           ADD DISPLACEMENT                             
SEL32    OC    0(DTLENQ,R4),0(R4)                                               
         BZ    SEL38NX                                                          
*                                                                               
         TM    DTLSTAT,DTSCURR     CURRENT SELECTION                            
         BNO   SEL34                                                            
         TM    DTLSTAT,DTSSEL+DTSCHA+DTSDIS     SELECTED FROM PERSON            
         BZ    SELX                             KEEP CURRENT (NO NEXT)          
         NI    DTLSTAT,X'FF'-(DTSCURR+DTSSEL+DTSCHA+DTSDIS)                     
         B     SEL38NX                                                          
*                                                                               
SEL34    TM    DTLSTAT,DTSHIS      SELECTED FOR HISTORY SCREEN                  
         BO    SEL40                                                            
         TM    DTLSTAT,DTSCRA      SELECTED FOR HISTORY SCREEN                  
         BO    SEL40                                                            
         TM    DTLSTAT,DTSSEL+DTSCHA+DTSDIS  SELECTED/SEL FOR CHANGE            
         BZ    SEL38NX                                                          
         OI    DTLSTAT,DTSCURR     NEW CURRENT SELECTION                        
         CLI   RECNUM,RTPR2        ON PER2 SCREEN                               
         BE    SELX                                                             
         CLI   ACTEQU,ACTADD                                                    
         BNE   SEL36                                                            
         MVI   PFKEY,9             CALL PER2 WITH CHANGE                        
         B     SELX                                                             
SEL36    MVI   PFKEY,11            CALL PER2 REC                                
         TM    DTLSTAT,DTSCHA      ACTION CHANGE                                
         BNO   *+8                                                              
         MVI   PFKEY,9                                                          
         TM    DTLSTAT,DTSDIS      ACTION DISPLAY                               
         BNO   *+8                                                              
         MVI   PFKEY,14                                                         
         BRAS  RE,CLRSELS          CLEAR SELECT FIELDS                          
         OI    BIT2,REBUILD        REBUILD TABLE ON RETURN TO PERSON            
         BRAS  RE,CALLPF                                                        
         DC    H'0'                SHOULDN'T RETURN                             
*                                                                               
SEL38NX  LA    R4,DTLENQ(R4)       NEXT TABLE ENTRY                             
         LA    R1,DISTABND         END OF TABLE                                 
         CR    R4,R1                                                            
         BL    SEL32                                                            
         CLI   RECNUM,RTPR2        ON PER2 SCREEN                               
         BNE   SELX                                                             
         MVI   PFKEY,2             RETURN TO PERSON                             
         CLI   CALLSP,1            ANYTHING IN STACK                            
         BNE   *+8                                                              
         MVI   PFKEY,12            YES, THEN JUST RETURN                        
         BRAS  RE,CALLPF                                                        
         DC    H'0'                SHOULDN'T RETURN                             
*                                                                               
SEL40    MVC   OFFICE,DTOFFICE                                                  
         MVC   DEPT,DTDEPT                                                      
         MVC   SUBDPT,DTSUBDPT                                                  
         MVI   PFKEY,10            CRATES                                       
         TM    DTLSTAT,DTSHIS                                                   
         BNO   *+8                                                              
         MVI   PFKEY,3             HISTORY                                      
         BRAS  RE,CALLPF                                                        
         DC    H'0'                SHOULDN'T RETURN                             
*                                                                               
SELX     XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        DID USER ENTER NEW OFFICE, DEPT, OR SUBDPT                   *         
***********************************************************************         
*                                                                               
NEWODS   NTR1                                                                   
         OC    PESOFF,SPACES                                                    
         OC    PESDEPT,SPACES                                                   
         OC    PESSDPT,SPACES                                                   
         TM    PESOFFH+4,X'80'     FIELD INPUT HIS TIME                         
         BO    XYES                                                             
         TM    PESDEPTH+4,X'80'                                                 
         BO    XYES                                                             
         TM    PESSDPTH+4,X'80'                                                 
         BO    XYES                                                             
         B     XNO                                                              
         EJECT                                                                  
***********************************************************************         
*        IS OFFICE, DEPT OR SUBDEPT FILLED IN                                   
***********************************************************************         
*                                                                               
OLDODS   NTR1                                                                   
         CLI   PESOFFH+5,0         ANYTHING IN THE FIELD                        
         BNE   XYES                                                             
         CLI   PESDEPTH+5,0                                                     
         BNE   XYES                                                             
         CLI   PESSDPTH+5,0                                                     
         BNE   XYES                                                             
         B     XNO                                                              
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE ALL SELECT FIELDS AND UPDATE TABLE ENTRIES                    
***********************************************************************         
*                                                                               
         USING LLINED,R2                                                        
         USING DISTABD,R4                                                       
VALSELS  NMOD1 0,*VALSELS          DO NOT USE R7                                
         L     RC,SAVERC                                                        
*                                                                               
         NI    BIT,X'FF'-UPDSEL    UPDATIVE SELECT                              
         LA    R4,DTADDLOC         TOP OF TABLE                                 
         AH    R4,STDISP                                                        
         LA    R2,PEMSELH          FIRST LINE ON SCREEN                         
*                                                                               
VS10     OC    0(DTLENQ,R4),0(R4)  ANY ENTRY IN TABLE                           
         BZ    VS50NX                                                           
*                                                                               
         CLI   LLSELH+5,0          ANY SELECTION                                
         BE    VS20                                                             
         TM    LLSELH+4,X'80'      ENTERED THIS TIME                            
         BO    *+12                                                             
         TM    LLSELH+4,X'20'     SEL ENTERED PREVIOUSLY                        
         BO    VS20                                                             
         GOTO1 =A(POSOFF),DMCB,RR=RELO FIGURE OFFICE DISP                       
         BE    VS20                                                             
         GOTO1 =A(OFFACC),DMCB,RR=RELO CHECK OFFICE ACCESS                      
         BE    VS20                                                             
         LA    R2,LLSELH                                                        
         B     EINVSECL                                                         
*                                                                               
VS20     OI    LLSELH+4,X'20'      PREV VALIDATED                               
         BAS   RE,VALSEL                                                        
VS50NX   LA    R4,DTLENQ(R4)       NEXT TABLE ENTRY                             
         LA    R1,DISTABND         END OF TABLE                                 
         CR    R4,R1                                                            
         BNL   VSX                                                              
         LA    R2,LLLEN(R2)        NEXT SCREEN LINE                             
         LA    R1,PEMENDH          LAST SCREEN LINE                             
         CR    R2,R1                                                            
         BNH   VS10                                                             
*                                                                               
VSX      TM    BIT,UPDSEL          IF THERE IS AN UPDATE DATES SEL              
         BNO   XIT                                                              
         BAS   RE,REMSELS          THEN REMOVE ALL OTHER SELECTS                
         B     XIT                                                              
         DROP  R4,R2                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE SELECT FIELD AND UPDATE TABLE ENTRY                           
*        R2 POINTS TO SCREEN LINE                                               
*        R4 POINTS TO DISTAB ENTRY                                              
***********************************************************************         
*                                                                               
         USING LLINED,R2                                                        
         USING DISTABD,R4                                                       
VALSEL   NTR1                                                                   
         MVI   DTLSTAT,0                                                        
         CLI   LLSELH+5,0                                                       
         BE    VSELX                                                            
*                                                                               
         USING VSELD,R3            VALID SELECTIONS TABLE                       
         L     R3,=A(VSELTAB)          SELECT TABLE                             
         A     R3,RELO                                                          
         OC    LLSEL,SPACES                                                     
*                                                                               
VSEL05   CLC   LLSEL,VSELWORD      MATCH ON SELECT CODE                         
         BE    VSEL10                                                           
         LA    R3,VSELLNQ(R3)                                                   
         CLI   0(R3),0                                                          
         BE    ERRINV                                                           
         B     VSEL05                                                           
*                                                                               
VSEL10   MVC   DTLSTAT,VSELBIT     MARK ENTRY WITH APPROPRIATE BIT              
*                                                                               
         DROP  R3                                                               
         TM    DTLSTAT,DTSUPD      FOR UPDATE NEED TO VALIDATE MORE             
         BNO   VSELX                                                            
         NI    DTLSTAT,X'FF'-DTSUPD   UNMARK IN CASE INVALID                    
         CLI   ACTEQU,ACTCHA       ONLY VALID WITH ACTION CHANGE                
         BNE   EVALCHA                                                          
         BRAS  RE,UPDSEC           SECURITY FOR UPDATE                          
         BNE   ERRNOUP                                                          
         TM    BIT,UPDSEL          CAN ONLY UPDATE 1                            
         BO    E1UPD                                                            
         GOTO1 =A(POSOFF),DMCB,RR=RELO    FIGURE OFFICE DISP                    
         BE    VSEL20                                                           
         GOTO1 =A(OFFACC),DMCB,RR=RELO MAKE SURE HAVE ACCESS TO OFFCE           
         BNE   EINVSECL                NO                                       
VSEL20   OI    DTLSTAT,DTSUPD      CAN UPDATE                                   
         OI    BIT,UPDSEL                                                       
*                                                                               
VSELX    XIT1                                                                   
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        REMOVE ALL OTHER SELECT BITS EXCEPT UPDATE DATES                       
*        R4 POINTS TO DISTAB ENTRY                                              
***********************************************************************         
*                                                                               
         USING DISTABD,R4                                                       
REMSELS  NTR1                                                                   
         LA    R4,DTADDLOC         TOP OF TABLE                                 
         AH    R4,STDISP                                                        
RSEL10   OC    0(DTLENQ,R4),0(R4)  ANY ENTRY IN TABLE                           
         BZ    RSELNX                                                           
         TM    DTLSTAT,DTSUPD      DON'T UNMARK UPDATE DATES SELECTION          
         BO    RSELNX                                                           
         MVI   DTLSTAT,0           CLEAR SELECT BITS                            
RSELNX   LA    R4,DTLENQ(R4)       NEXT TABLE ENTRY                             
         LA    R1,DISTABND         END OF TABLE                                 
         CR    R4,R1                                                            
         BL    RSEL10                                                           
RSELX    XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DISPLAY PERSON SCREEN                                                  
***********************************************************************         
*                                                                               
DISPRSCR NMOD1 0,*DISPR**          DO NOT USE R7                                
         L     RC,SAVERC                                                        
*                                                                               
         BRAS  RE,CLRSCR                                                        
         GOTO1 =A(DISOFFAC),DMCB,RR=RELO  IS VALID FOR DISPLAY?                 
         BE    *+12                       YES                                   
         LA    R2,PEMCODEH                                                      
         B     EINVSECL                                                         
*                                                                               
         MVC   PEMWARN,SPACES                                                   
         OI    PEMWARNH+6,X'80'    XMIT                                         
         MVC   PEMSNAM,SPACES                                                   
         OI    PEMSNAMH+6,X'80'    XMIT                                         
         TM    OPTSTAT,XDOPT       WANT XTRA DETAILS                            
         BZ    DSP05                                                            
         GOTO1 =A(SHOWXDET),DMCB,RR=RELO                                        
*                                                                               
DSP05    MVC   PEMCODE,PERSON      PERSON CODE                                  
         OI    PEMCODEH+6,X'80'    XMIT                                         
*                                                                               
         MVC   PEMPID,PIDNAME      PERSONAL ID NAME                             
         OI    PEMPIDH+6,X'80'     XMIT                                         
         MVC   OLDPID,PIDNUM                                                    
*                                                                               
         MVC   PEMLNAM,SPACES                                                   
         OI    PEMLNAMH+6,X'80'    XMIT                                         
         MVC   PEMFNAM,SPACES                                                   
         OI    PEMFNAMH+6,X'80'    XMIT                                         
         BRAS  RE,FLDSEC           SECURITY TO PRINT NAME                       
*&&UK*&& BNE   DPS10                                                            
         GOTO1 =A(GETNAME),DMCB,RR=RELO                                         
*&&US                                                                           
         CLI   BYTE,C'W'           ALL ACCESS?                                  
         BE    DPS10               YES - CONTINUE                               
         CLI   BYTE,C'R'           READ ACCESS ONLY?                            
         BNE   *+16                NO - CONTINUE                                
         OI    PEMLNAMH+1,X'20'    PROTECT FOR READ ONLY ACCESS                 
         OI    PEMFNAMH+1,X'20'                                                 
         B     DPS10                                                            
*                                                                               
         OI    PEMLNAMH+1,X'0C'    AND SET TO LOW INTENSITY SO HIDDEN           
         OI    PEMFNAMH+1,X'0C'                                                 
*                                                                               
*&&                                                                             
DPS10    CLC   PIDNAME,SPACES      ANY PID                                      
         BNH   DPS20                                                            
         GOTO1 =A(GETPIDNM),DMCB,RR=RELO     GET PID INFO                       
         MVC   SVPIDNM,PIDNAME                                                  
*                                                                               
         CLC   SECLAST,PEMLNAM     COMPARE NAMES                                
         BNE   DPS15                                                            
         CLC   SECFIRST,PEMFNAM                                                 
         BE    DPS20                                                            
DPS15    L     R1,=A(DDPWARN)                                                   
         A     R1,RELO                                                          
         MVC   PEMWARN(45),0(R1)                                                
         GOTO1 DICTATE,DMCB,C'SL  ',PEMWARN,0                                   
         XC    BLOCK(100),BLOCK                                                 
         MVC   BLOCK(L'SECLAST),SECLAST                                         
         MVI   BLOCK+L'SECLAST+2,C','                                           
         MVC   BLOCK+L'SECLAST+5(L'SECFIRST),SECFIRST                           
         GOTO1 SQUASHER,DMCB,BLOCK,100                                          
         MVC   PEMSNAM,BLOCK                                                    
*                                                                               
DPS20    GOTO1 DATCON,DMCB,(1,HIREDT),(DOUT,PEMHIRE)     HIRE DATE              
         OI    PEMHIREH+6,X'80'                                                 
         OI    PEMHIREH+4,X'20'    VALIDATED                                    
         MVC   PEMTERM,SPACES                                                   
         OI    PEMTERMH+6,X'80'                                                 
         OC    TERMDT,TERMDT                           ANY TERM DATE            
         BZ    DPS50                                                            
         GOTO1 DATCON,DMCB,(1,TERMDT),(DOUT,PEMTERM)                            
         EJECT                                                                  
***********************************************************************         
*        DISPLAY STAFF LOCATION HISTORY FROM TABLE                              
***********************************************************************         
*                                                                               
         USING LLINED,R2                                                        
         USING DISTABD,R4                                                       
DPS50    LA    R2,PEMLIN1H                             1ST SCREEN LINE          
         LA    R4,DTLOCS                               START OF TABLE           
         OI    BIT,FIRST                                                        
*                                                                               
* DOING THIS FOR MOST CURRENT LOCATION ONLY TO SEE WHETHER TO                   
* PROTECT THE END DATE OR NOT                                                   
*                                                                               
         NI    BIT2,X'FF'-NOACCURR                                              
         GOTO1 =A(POSOFF),DMCB,RR=RELO GET OFFICE DISPL                         
         BE    DPS51                                                            
         GOTO1 =A(OFFACC),DMCB,RR=RELO CHECK OFFICE ACCESS                      
         BE    *+8                                                              
         OI    BIT2,NOACCURR                                                    
*                                                                               
DPS51    CLI   PFKEY,0                                                          
         BNE   DPS52                                                            
         CLI   ACTEQU,ACTSEL                                                    
         BE    DPS54                                                            
         B     DPS58                                                            
*                                                                               
DPS52    CLI   PFKEY,7             UP                                           
         BNE   DPS53                                                            
         MVC   STDISP,PRVSTDSP                                                  
         LA    R1,DTLENQ                                                        
         MHI   R1,8               8 LINES                                       
         LH    R0,PRVSTDSP                                                      
         SR    R0,R1                                                            
         CHI   R0,0                                                             
         BNL   *+6                 DISP FROM TOP                                
         SR    R0,R0                                                            
         STH   R0,PRVSTDSP                                                      
         B     DPS58                                                            
*                                                                               
DPS53    CLI   PFKEY,8             DOWN                                         
         BNE   DPS58                                                            
         MVC   PRVSTDSP,STDISP                                                  
         MVC   STDISP,LSTDISP                                                   
         B     DPS58                                                            
*                                                                               
DPS54    MVC   STDISP,=H'0'                                                     
         MVC   PRVSTDSP,=H'0'                                                   
*                                                                               
DPS58    LA    R0,DTLENQ           LENGTH OF ONE ENTRY                          
         MH    R0,TABCOUNT         NUMBER OF ENTRIES                            
         LH    R1,STDISP                                                        
         CR    R0,R1                                                            
         BH    DPS59                                                            
         LA    R1,0                                                             
DPS59    AR    R4,R1               WHERE TO START DISPLAYING                    
*                                                                               
DPS60    LA    R1,DTLOCS                                                        
         LR    R0,R4                                                            
         SR    R0,R1                                                            
         STH   R0,LSTDISP                                                       
*                                                                               
         OC    DTPREVST(DTLENQ),DTPREVST ANY ENTRY?                             
         BNZ   DPS64                                                            
         NI    GENSTAT2,X'FF'-RETEQSEL                                          
         SR    R0,R0               START FROM TOP NEXT ENTER                    
         STH   R0,LSTDISP                                                       
         B     DPSX                                                             
*                                                                               
DPS64    NI    BIT2,X'FF'-NOACCESS                                              
         GOTO1 =A(POSOFF),DMCB,RR=RELO GET OFFICE DISPL                         
         BE    DPS64A                                                           
         GOTO1 =A(OFFACC),DMCB,RR=RELO CHECK OFFICE ACCESS                      
         BE    *+8                                                              
         OI    BIT2,NOACCESS                                                    
*                                                                               
         USING VSELD,R3                                                         
DPS64A   L     R3,=A(VSELTAB)          SELECT TABLE                             
         A     R3,RELO                                                          
DPS65    CLC   DTLSTAT,VSELBIT                                                  
         BE    DPS70                                                            
         LA    R3,VSELLNQ(R3)                                                   
         CLI   0(R3),0                                                          
         BE    DPS80                                                            
         B     DPS65                                                            
*                                                                               
DPS70    CLI   VSELDISP,C'Y'       REDISPLAY SELECT CODE                        
         BNE   DPS80                                                            
         TM    BIT,FIRST           DON'T REDISPLAY FIRST CODE                   
         BO    DPS75                                                            
         MVC   LLSEL,VSELWORD                                                   
         MVI   LLSELH+5,3                                                       
         OI    LLSELH+6,X'80'                                                   
DPS75    NI    BIT,X'FF'-FIRST                                                  
*                                                                               
DPS80    MVC   LLOFF,DTOFFICE                          OFFICE                   
         MVC   LLDEPT,DTDEPT                           DEPARTMENT               
         MVC   LLSUB,DTSUBDPT                          SUB DEPARTMENT           
*                                                                               
         GOTO1 DATCON,DMCB,(1,DTSTDATE),(DOUT,LLDATE)    START DATE             
         MVI   LLDATEH+5,8                                                      
         OI    LLDATEH+1,X'20'                         PROTECT                  
         TM    DTLSTAT,DTSUPD                          UPDATE DATES             
         BNO   DPS85                                                            
         NI    LLDATEH+1,X'FF'-X'20'                   UNPROTECT TO             
         LA    R1,LLDATEH                                                       
         ST    R1,ACURFORC                                                      
         OI    BIT,UPDSEL                                                       
*                                                                               
DPS85    NI    LLENDTH+1,X'FF'-X'20'                   UNPROTECT                
         OC    DTENDATE,DTENDATE                                                
         BNZ   DPS86                                                            
         TM    BIT2,NOACCESS                                                    
         BZ    *+8                                                              
         OI    LLENDTH+1,X'20'     PROTECT-NO ACCESS TO THIS LOC                
         B     DPS90                                                            
*                                                                               
DPS86    DS    0H                                                               
         GOTO1 DATCON,DMCB,(1,DTENDATE),(DOUT,LLENDT)  END DATE                 
         MVI   LLENDTH+5,8                                                      
         TM    DTLSTAT,DTSUPD                          UPDATE DATES             
         BO    *+8                                                              
         OI    LLENDTH+1,X'20'                         PROTECT                  
*                                                                               
DPS90    OC    DTSALKDT,DTSALKDT                                                
         BNZ   DPS91                                                            
         TM    BIT2,NOACCESS                                                    
         BZ    *+8                                                              
         OI    LLSALDH+1,X'20'     PROTECT-NO ACCESS TO THIS LOC                
         B     DPS95                                                            
*                                                                               
DPS91    DS    0H                                                               
         GOTO1 DATCON,DMCB,(1,DTSALKDT),(DOUT,LLSALD)  SAL LOCK DATE            
         TM    BIT2,NOACCESS                                                    
         BZ    *+8                                                              
         OI    LLSALDH+1,X'20'     PROTECT                                      
*                                                                               
DPS95    MVC   LLSTAT,DTSTATNM                         STATUS                   
*&&US*&& NI    LLFREEH+1,X'FF'-X'20'                   UNPROTECT FRL            
         TM    BIT2,NOACCESS                                                    
         BZ    *+8                                                              
         OI    LLSTATH+1,X'20'        PROTECT                                   
*&&US*&& MVC   LLFREE,DTFREE                                                    
         TM    DTSTAT2,LOCSDUP                         DUPLICATE                
         BO    DPS100                                                           
*&&US                                                                           
         CLI   DTSTATUS,LOCSACT    PROTECT FREELANCER UNLESS ACTIVE             
         BE    *+8                                                              
         OI    LLFREEH+1,X'20'                     PROTECT FREELANCERS          
*&&                                                                             
         MVC   LLFILT1,DTFILT1                         FILTERS                  
         MVC   LLFILT2,DTFILT2                                                  
         MVC   LLFILT3,DTFILT3                                                  
         MVC   LLFILT4,DTFILT4                                                  
         MVC   LLFILT5,DTFILT5                                                  
         TM    BIT2,NOACCESS       NO ACCESS TO THIS LOC                        
         BZ    DPSNX                                                            
*&&US*&& OI    LLFREEH+1,X'20'                     PROTECT FREELANCERS          
         OI    LLFILT1H+1,X'20'                    AND PROTECT FILTERS          
         OI    LLFILT2H+1,X'20'                        IF NO ACCESS             
         OI    LLFILT3H+1,X'20'                                                 
         OI    LLFILT4H+1,X'20'                                                 
         OI    LLFILT5H+1,X'20'                                                 
         B     DPSNX                                                            
*                                                                               
DPS100   DS    0H                                                               
*&&US*&& OI    LLFREEH+1,X'20'                     PROTECT FREELANCERS          
         OI    LLFILT1H+6,X'20'                    AND PROTECT FILTERS          
         OI    LLFILT2H+6,X'20'                        IF DUPLICATE             
         OI    LLFILT3H+6,X'20'                                                 
         OI    LLFILT4H+6,X'20'                                                 
         OI    LLFILT5H+6,X'20'                                                 
*                                                                               
DPSNX    LA    R4,DTLENQ(R4)                           NEXT TABLE ENTRY         
         LA    R1,DISTABND                             END OF TABLE             
         CR    R4,R1                                                            
         BNL   DPSX                                                             
         LA    R2,LLLEN(R2)                            NEXT LINE                
         LA    R1,PEMENDH                              LAST LINE                
         CR    R2,R1                                                            
         BNH   DPS60                                                            
*                                                                               
DPSX     LA    R4,DTLOCS           RE-ESTABLISH POINTER TO BEGINNING            
         USING DISTABD,R4                                                       
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,DTENDATE),(10,WORK)                               
         CLC   PEMPEND,WORK    ARE WE ON THE FIRST PAGE?                        
         BNE   DPSXX                                                            
         TM    BIT2,NOACCURR       PROTECT BECAUSE OF LIMITED ACCESS            
         BO    *+8                 YES                                          
         NI    PEMPENDH+1,X'FF'-X'20'    YES-UNPROTECT THE FIRST END            
DPSXX    XIT1                                                                   
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
*        GET NAME FROM '5A' ELEMENT ON PERSON REC                               
***********************************************************************         
*                                                                               
GETNAME  NMOD1 0,*GETNME*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         USING PERRECD,R6                                                       
         LA    R6,KEY2                                                          
         XC    KEY2,KEY2                                                        
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    PERSON RECORD TYPE                           
         MVC   PERKCPY,CMPY        COMPANY                                      
         MVC   PERKCODE,PERSON     PERSON CODE                                  
         MVC   SAVEKEY,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR',KEY2,KEY2,0                   
         CLC   SAVEKEY(L'SAVEKEY),KEY2                                          
         BNE   GETNX                                                            
         LA    R3,KEY2                                                          
         AH    R3,LKEY                                                          
         AH    R3,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST',(R3),AIO2,WORK                
         L     R6,AIO2                                                          
*                                                                               
         USING GPNELD,R6                                                        
         AH    R6,DATADISP                                                      
GETN11   CLI   0(R6),0                                                          
         BE    GETNX                                                            
         CLI   0(R6),GPNELQ                                                     
         BE    *+16                                                             
GETN12   SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     GETN11                                                           
*                                                                               
         CLI   GPNTYP,GPNTLST      LAST NAME                                    
         BNE   GETN15                                                           
         LLC   R1,GPNLN                                                         
         SH    R1,=Y(GPNLNQ+1)     OVERHEAD+1                                   
         BM    GETN12                                                           
         EXMVC R1,PEMLNAM,GPNNME                                                
         LA    R1,1(R1)            GET TRUE LENGTH AGAIN                        
         STC   R1,PEMLNAMH+5       AND SET LENGTH                               
         B     GETN12                                                           
GETN15   LLC   R1,GPNLN                                                         
         SH    R1,=Y(GPNLNQ+1)                                                  
         BM    GETNX                                                            
         EXMVC R1,PEMFNAM,GPNNME                                                
         LA    R1,1(R1)            GET TRUE LENGTH AGAIN                        
         STC   R1,PEMFNAMH+5       AND SET LENGTH                               
*                                                                               
GETNX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY EXTRA DETAILS ON DISPLAY SCREEN                                
***********************************************************************         
*                                                                               
SHOWXDET NMOD1 0,*SHOWXD*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         TM    BIT3,NODETAIL                                                    
         BO    SHOW10                                                           
         MVC   PEMWARN,BLOCK+L'PVALOUTB*2                                       
         OI    PEMWARNH+6,X'80'                                                 
         B     SHOWX                                                            
*                                                                               
SHOW10   L     R1,=A(DDNODET)                                                   
         A     R1,RELO                                                          
         MVC   PEMWARN(20),0(R1)                                                
         GOTO1 DICTATE,DMCB,C'SL  ',PEMWARN,0                                   
         OC    PEMWARN,SPACES                                                   
*                                                                               
SHOWX    NI    BIT3,X'FF'-NODETAIL                                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DISPLAY STATUS SCREEN                                                  
***********************************************************************         
*                                                                               
         USING DISTABD,R4                                                       
DISSTSCR NMOD1 0,*DISST**          DO NOT USE R7                                
         L     RC,SAVERC                                                        
*                                                                               
         XC    SVTSLOCK,SVTSLOCK                                                
         BRAS  RE,CURRSEL          R4 SET TO CURRENT SELECTION IN TABLE         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   PESCODE,PERSON      PERSON CODE                                  
         OI    PESCODEH+6,X'80'    XMIT                                         
         MVC   PESOFF,DTOFFICE     OFFICE                                       
         OI    PESOFFH+6,X'80'                                                  
         MVC   PESDEPT,DTDEPT      DEPARTMENT                                   
         OI    PESDEPTH+6,X'80'                                                 
         MVC   PESSDPT,DTSUBDPT    SUB DEPARTMENT                               
         OI    PESSDPTH+6,X'80'                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(1,DTSTDATE),(DOUT,PESSTDT)   START DATE             
         OI    PESSTDTH+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(1,DTENDATE),(DOUT,PESENDT)   END DATE               
         OI    PESENDTH+6,X'80'                                                 
*                                                                               
         MVC   PESSTAT,DTSTATNM    STATUS                                       
         OI    PESSTATH+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(1,DTTSLKDT),(DOUT,PESTSLD)   TIMESHEET LOCK         
         OI    PESTSLDH+6,X'80'                                                 
         MVC   SVTSLOCK,DTTSLKDT   SAVE THE TMS LOCK DATE                       
         GOTO1 =A(BRANDS),DMCB,RR=RELO DISPLAY SWITCHON DATES                   
*                                                                               
***********************************************************************         
*        DISPLAY ATTRIBUTES  (EXEC,PROD,ETC)                                    
***********************************************************************         
*                                                                               
         USING ATTRD,R3                                                         
DSS10A   L     R3,=A(ATTRTAB)      ATTRIBUTE TABLE                              
         A     R3,RELO                                                          
DSS10    LA    R2,CONTAGH                                                       
         AH    R2,ATTRDISP         DISP TO ATTRIBUTE FIELD                      
         MVC   8(1,R2),AC@NOU                                                   
         MVC   BYTE,ATTRBIT        BIT ON FOR Y                                 
         NC    BYTE,DTATTR                                                      
         CLC   BYTE,ATTRBIT        IS BIT ON                                    
         BNE   *+10                                                             
         MVC   8(1,R2),AC@YESU                                                  
         OI    6(R2),X'80'         XMIT                                         
         LA    R3,ATTRLNQ(R3)                                                   
         CLI   0(R3),0                                                          
         BNE   DSS10                                                            
*&&US                                                                           
*                                                                               
* FREELANCER BIT                                                                
*                                                                               
         LA    R2,PESFREEH         DISP TO ATTRIBUTE FIELD                      
         MVC   8(1,R2),AC@NOU                                                   
         MVI   BYTE,LOCYFRL        BIT ON FOR Y                                 
         NC    BYTE,DTATTR                                                      
         CLI   BYTE,LOCYFRL        IS BIT ON                                    
         BNE   *+10                                                             
         MVC   8(1,R2),AC@YESU                                                  
         MVI   BYTE,LOCBFRL        BIT ON FOR B                                 
         NC    BYTE,DTATTR                                                      
         CLI   BYTE,LOCBFRL        IS BIT ON                                    
         BNE   *+8                                                              
         MVI   8(R2),C'B'                                                       
         OI    6(R2),X'80'         XMIT                                         
*&&                                                                             
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
*        GET 1R FOR REST OF INFO                                                
***********************************************************************         
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,KEY2                                                          
         MVC   ACTKEY,SPACES       DELETE 1R                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVI   ACTKUNT,C'1'        UNIT 1                                       
         MVI   ACTKLDG,C'R'        LEDGER R                                     
         MVC   ACTKACT,DTACCNT     ACCOUNT                                      
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         CLC   KEY2(L'ACTKEY),KEYSAVE                                           
         BE    DSS20                                                            
         LA    R2,PESOFFH                                                       
         B     EINVACCT            MISSING 1R                                   
*                                                                               
DSS20    LA    R2,KEY2             GET RECORD                                   
         AH    R2,LKEY                                                          
         AH    R2,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST ',(R2),AIO2,WORK               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY PERSON NAME FROM X'20' ELEM                                    
***********************************************************************         
*                                                                               
         MVC   PESNAME,SPACES      CLEAR FIELD                                  
         OI    PESNAMEH+6,X'80'                                                 
*                                                                               
         BRAS  RE,FLDSEC           SECURITY TO PRINT NAME                       
         BNE   DSS30               NOT ALLOWED TO PRINT                         
*                                                                               
         USING NAMELD,R6                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,NAMELQ       X'20' GENERAL NAME ELEM                      
         BAS   RE,GETEL                                                         
         BNE   DSS30                                                            
         LLC   R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    DSS30                                                            
         MVC   PESNAME(0),NAMEREC     NAME                                      
         EX    R1,*-6                                                           
*&&US                                                                           
         CLI   BYTE,C'R'           READ ONLY ACCESS?                            
         BNE   *+8                                                              
         OI    PESNAMEH+1,X'20'                                                 
*&&                                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY ADDRESS FROM X'22' ELEM                                        
***********************************************************************         
*                                                                               
         USING ADRELD,R6                                                        
DSS30    MVC   PESADR1,SPACES             CLEAR ADDRESS                         
         OI    PESADR1H+6,X'80'                                                 
         NI    PESADR1H+1,X'FF'-X'20'     UNPROTECT                             
         TM    DTSTAT2,LOCSDUP                                                  
         BNO   *+8                                                              
         OI    PESADR1H+1,X'20'            PROTECT IF DUP                       
*                                                                               
         MVC   PESADR2,SPACES                                                   
         OI    PESADR2H+6,X'80'                                                 
         NI    PESADR2H+1,X'FF'-X'20'                                           
         TM    DTSTAT2,LOCSDUP                                                  
         BNO   *+8                                                              
         OI    PESADR2H+1,X'20'            PROTECT IF DUP                       
*                                                                               
         MVC   PESADR3,SPACES                                                   
         OI    PESADR3H+6,X'80'                                                 
         NI    PESADR3H+1,X'FF'-X'20'                                           
         TM    DTSTAT2,LOCSDUP                                                  
         BNO   *+8                                                              
         OI    PESADR3H+1,X'20'            PROTECT IF DUP                       
*                                                                               
         MVC   PESADR4,SPACES                                                   
         OI    PESADR4H+6,X'80'                                                 
         NI    PESADR4H+1,X'FF'-X'20'                                           
         TM    DTSTAT2,LOCSDUP                                                  
         BNO   *+8                                                              
         OI    PESADR4H+1,X'20'            PROTECT IF DUP                       
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,ADRELQ       X'22' ADDRESS ELEM                           
         BAS   RE,GETEL                                                         
         BNE   DSS40                                                            
         MVC   PESADR1,ADRADD1                                                  
         CLI   ADRLN,ADRLN1Q       ONLY 1 LINE                                  
         BE    DSS40                                                            
         MVC   PESADR2,ADRADD2                                                  
         CLI   ADRLN,ADRLN2Q       ONLY 2 LINES                                 
         BE    DSS40                                                            
         MVC   PESADR3,ADRADD3                                                  
         CLI   ADRLN,ADRLN3Q       ONLY 3 LINES                                 
         BE    DSS40                                                            
         MVC   PESADR4,ADRADD4                                                  
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY MEMO FROM X'3F' ELEM                                           
***********************************************************************         
*                                                                               
         USING OMEELD,R6                                                        
DSS40    MVC   PESMEMO,SPACES                                                   
         OI    PESMEMOH+6,X'80'                                                 
         NI    PESMEMOH+1,X'FF'-X'20'                                           
         TM    DTSTAT2,LOCSDUP                                                  
         BNO   *+8                                                              
         OI    PESMEMOH+1,X'20'            PROTECT IF DUP                       
         L     R6,AIO2                                                          
         MVI   ELCODE,OMEELQ       X'3F' ONLINE MEMO ELEM                       
         BAS   RE,GETEL                                                         
         BNE   DSS50                                                            
         LLC   R1,OMELN                                                         
         SH    R1,=Y(OMELN1Q)                                                   
         AHI   R1,-1                                                            
         BM    DSS50                                                            
         MVC   PESMEMO(0),OMEMO                                                 
         EX    R1,*-6                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY ANALYSIS ACCOUNT AND DEF TASK FROM X'30' STATUS ELEM           
***********************************************************************         
*                                                                               
DSS50    MVC   PESANAL,SPACES      CLEAR FIELD                                  
         OI    PESANALH+6,X'80'                                                 
         NI    PESANALH+1,X'FF'-X'20'                                           
         TM    DTSTAT2,LOCSDUP                                                  
         BNO   *+8                                                              
         OI    PESANALH+1,X'20'     PROTECT IF DUP                              
*                                                                               
         MVC   PESDTSK,SPACES       CLEAR FIELD                                 
         OI    PESDTSKH+6,X'80'                                                 
         NI    PESDTSKH+1,X'FF'-X'20'                                           
         TM    DTSTAT2,LOCSDUP                                                  
         BNO   *+8                                                              
         OI    PESDTSKH+1,X'20'     PROTECT IF DUP                              
*                                                                               
         USING RSTELD,R6                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,RSTELQ       X'30' RECORD STATUS ELEM                     
         BAS   RE,GETEL                                                         
         BNE   DSS60                                                            
         MVC   PESANAL(L'RSTCOSTG),RSTCOSTG                                     
         MVC   PESDTSK,RSTDFTSK                                                 
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY OVERRIDE INCOME ACCOUNT AND OVERRIDE WRITEOFF ACCOUNT          
***********************************************************************         
*                                                                               
DSS60    MVC   PESOIN,SPACES       CLEAR FIELD                                  
         OI    PESOINH+6,X'80'                                                  
         NI    PESOINH+1,X'FF'-X'20'                                            
         TM    DTSTAT2,LOCSDUP                                                  
         BNO   *+8                                                              
         OI    PESOINH+1,X'20'            PROTECT IF DUP                        
         MVC   PESOWO,SPACES                                                    
         OI    PESOWOH+6,X'80'                                                  
         NI    PESOWOH+1,X'FF'-X'20'                                            
         TM    DTSTAT2,LOCSDUP                                                  
         BNO   *+8                                                              
         OI    PESOWOH+1,X'20'            PROTECT IF DUP                        
         USING SPAELD,R6                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,SPAELQ       X'2C' SPECIAL POSTING ACCOUNT ELEM           
         BAS   RE,GETEL                                                         
         B     DSS62                                                            
DSS62NX  BAS   RE,NEXTEL                                                        
DSS62    BNE   DSSX                                                             
         CLI   SPATYPE,SPATINCO    INCOME OVERRIDE                              
         BNE   DSS64                                                            
         MVC   PESOIN,SPAAULA                                                   
         B     DSS62NX                                                          
DSS64    CLI   SPATYPE,SPATWOFF    WRITEOFF OVERRIDE                            
         BNE   DSS62NX                                                          
         MVC   PESOWO,SPAAULA                                                   
         B     DSS62NX                                                          
         DROP  R6                                                               
DSSX     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LTORG                                                              *          
**********************************************************************          
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GET THE BRANDOCEAN SWITCH ON/OFF DATES                             *          
**********************************************************************          
         USING DISTABD,R4                                                       
BRANDS   NMOD1 0,**BRANDS**                                                     
         L     RC,SAVERC                                                        
         XC    SWTCHBI2,SWTCHBI2                                                
         BRAS  RE,SWOSEC           BRANDOCEAN SWITCHON SECURITY                 
         BL    BRANDSX             DON'T DISPLAY SWITCHON DATES                 
         BE    *+8                 FULL WRITE ACCESS                            
         OI    SWTCHBI2,NOEDIT     NO EDITING OF DATES                          
*                                                                               
         USING ACTRECD,R6          START READING RECORDS                        
         XC    KEY2,KEY2                                                        
         MVC   PESBOS1,SPACES                                                   
         MVC   PESBOS2,SPACES                                                   
         MVC   PESBOS3,SPACES                                                   
         MVC   PESBOS4,SPACES                                                   
         MVC   PESBOS5,SPACES                                                   
         MVC   PESBOS6,SPACES                                                   
         MVC   PESDAS1,SPACES                                                   
         MVC   PESDAS2,SPACES                                                   
         MVC   PESDAS3,SPACES                                                   
*                                                                               
         MVC   PESLVL1,SPACES                                                   
         MVC   PESLVL2,SPACES                                                   
         MVC   PESLVL3,SPACES                                                   
         MVC   PESLVL4,SPACES                                                   
         MVC   PESLVL5,SPACES                                                   
         MVC   PESLVL6,SPACES                                                   
         MVC   TEMPDATE,DTSTDATE   IF USER START DATE IS MID PERIOD             
         GOTO1 =A(READCAL),DMCB,RR=RELO  MAKE SURE STDATE HAS LOC START         
*                                                                               
         XC    STDATES(L'STDATES+L'ENDATES),STDATES                             
         XC    KEY2,KEY2                                                        
         NI    PESBOS1H+1,X'FF'-X'20' CLEAR FIELD PROTECTION                    
         NI    PESBOS2H+1,X'FF'-X'20'                                           
         NI    PESBOS3H+1,X'FF'-X'20'                                           
         NI    PESBOS4H+1,X'FF'-X'20' CLEAR FIELD PROTECTION                    
         NI    PESBOS5H+1,X'FF'-X'20'                                           
         NI    PESBOS6H+1,X'FF'-X'20'                                           
         LA    R2,STDATES                                                       
         ST    R2,FULL                                                          
         LA    R6,KEY2                                                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                                                     
         MVC   ACTKUNT(2),=C'1R'                                                
         LA    R2,ACTKACT                                                       
*                                                                               
         LLC   R3,LN1RLEV1         DEAL WITH LEVELS                             
         AHI   R3,-1                                                            
         EXMVC R3,0(R2),PESOFF                                                  
         AHI   R3,1                                                             
         AR    R2,R3                                                            
         LLC   R3,LN1RLEV2                                                      
         AHI   R3,-1                                                            
         EXMVC R3,0(R2),PESDEPT                                                 
         AHI   R3,1                                                             
         AR    R2,R3                                                            
         LLC   R3,LN1RLEV3                                                      
         AHI   R3,-1                                                            
         EXMVC R3,0(R2),PESSDPT                                                 
         AHI   R3,1                                                             
         AR    R2,R3                                                            
         LLC   R3,LN1RLEV4                                                      
         AHI   R3,-1                                                            
         EXMVC R3,0(R2),PESCODE                                                 
         MVC   KEYSAVE,KEY2                                                     
*                                  PERSON                                       
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCDIR',KEY2,KEY2                     
         CLC   KEY2(L'ACTKEY),KEYSAVE                                           
         BE    *+8                                                              
         B     EINVLOC                                                          
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST',ACTKDA,AIO3,WORK              
         MVI   BYTE1,X'01'                                                      
         GOTO1 BRANGET                                                          
         BNE   BS002               TABLE FULL MOVE ON                           
*                                  SUBDEPT                                      
         LA    R2,ACTKACT                                                       
         LLC   R3,LN1RLEV1         MOVE SPACES INTO ACTKACT FOR PERSON          
         AR    R2,R3                                                            
         LLC   R3,LN1RLEV2                                                      
         AR    R2,R3                                                            
         LLC   R3,LN1RLEV3                                                      
         AR    R2,R3                                                            
         LA    R3,ACTKACT+L'ACTKACT                                             
         SR    R3,R2                                                            
         AHI   R3,-1                                                            
         EXMVC R3,0(R2),SPACES                                                  
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCDIR',KEY2,KEY2                     
         CLC   KEY2(L'ACTKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST',ACTKDA,AIO3,WORK              
         MVI   BYTE1,X'02'                                                      
         GOTO1 BRANGET                                                          
         BNE   BS002               TABLE FULL MOVE ON                           
*                                  DEPT                                         
         LA    R2,ACTKACT                                                       
         LLC   R3,LN1RLEV1                                                      
         AR    R2,R3               MOVE SPACES INTO ACTKACT FOR PERSON          
         LLC   R3,LN1RLEV2         AND SUB DEPT                                 
         AR    R2,R3                                                            
         LA    R3,ACTKACT+L'ACTKACT                                             
         SR    R3,R2                                                            
         AHI   R3,-1                                                            
         EXMVC R3,0(R2),SPACES                                                  
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCDIR',KEY2,KEY2                     
         CLC   KEY2(L'ACTKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST',ACTKDA,AIO3,WORK              
         MVI   BYTE1,X'03'                                                      
         GOTO1 BRANGET                                                          
         BNE   BS002               TABLE FULL MOVE ON                           
*                                  OFFICE                                       
         LA    R2,ACTKACT                                                       
         LLC   R3,LN1RLEV1                                                      
         AR    R2,R3               MOVE SPACES INTO ACTKACT FOR PERSON          
         LA    R3,ACTKACT+L'ACTKACT SUB DEPT AND OFFICE                         
         SR    R3,R2                                                            
         AHI   R3,-1                                                            
         EXMVC R3,0(R2),SPACES                                                  
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCDIR',KEY2,KEY2                     
         CLC   KEY2(L'ACTKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST',ACTKDA,AIO3,WORK              
         MVI   BYTE1,X'04'                                                      
         GOTO1 BRANGET                                                          
         BNE   BS002               TABLE FULL MOVE ON                           
*                                  1R                                           
         MVC   ACTKACT,SPACES      MOVE SPACES INTO ACTKACT                     
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCDIR',KEY2,KEY2                     
         CLC   KEY2(L'ACTKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST',ACTKDA,AIO3,WORK              
         MVI   BYTE1,X'05'                                                      
         GOTO1 BRANGET                                                          
         BNE   BS002               TABLE FULL MOVE ON                           
         XC    KEY2,KEY2                                                        
         B     BS002                                                            
         DROP  R6                                                               
*                             **   CHECK FOR PERIOD GAPS AND OVERLAP **         
*                             **   PERIODS                           **         
BS002    OC    STDATES,STDATES     DO WE HAVE ANY DATES?                        
         BZ    BS062               NO MOVE ON                                   
         LA    R2,STDATES          NOW FOR PROCESS THE DATES                    
         LA    R1,ENDATES                                                       
*                                                                               
BS004    CR    R2,R1               HIT END OF TABLE?                            
         BNL   BS010               YES NOW PROCESS THE DATES                    
         OC    0(DATL,R2),0(R2)                                                 
         BZ    BS008                                                            
         LR    R3,R2                                                            
*                                                                               
BS006    LA    R3,DATLVL(R3)       SET R3 TO POINT TO NEXT START                
         CR    R3,R1                                                            
         BNL   BS008               FINISHED FIND MORE DUPES                     
         CLC   0(DATL,R3),0(R2)                                                 
         BNE   BS006                                                            
         CLC   ENDT(DATL,R3),ENDT(R2) CHECK LEVS                                
         BNE   BS006                                                            
         XC    0(DATLVL,R3),0(R3)  FOUND ANOTHER SET OF DUPES                   
         XC    ENDT(DATLVL,R3),ENDT(R3) GET RID OF                              
         B     BS006                                                            
BS008    LA    R2,DATLVL(R2)                                                    
         B     BS004                                                            
*                                                                               
BS010    OC    STDATES,STDATES     CHECK WHETHER ANY DATES LEFT                 
         BZ    BS062                                                            
         GOTO1 RMSPAC              CLEAR UP TABLE                               
         LA    R2,STDATES          NOW FOR PROCESS THE DATES                    
         LR    R3,R2                                                            
*                                                                               
BS012    LA    R3,DATLVL(R3)       SET R3 TO POINT TO NEXT START                
         LA    R1,ENDATES          HIT END OF TABLE?                            
         CR    R3,R1                                                            
         BNL   BS040                                                            
*                                                                               
         OC    0(DATL,R3),0(R3)    IF NEXT START ZERO SKIP                      
         BNZ   BS013                                                            
         OC    ENDT(DATL,R3),ENDT(R3)                                           
         BNZ   BS032                                                            
         B     BS012                                                            
*                                                                               
BS013    CLC   0(DATL,R3),ENDT(R2)                                              
         BH    BS012               IF NEXT START AFTER 1ST END SKIP             
         BNE   BS020                                                            
*                                  IF NEXT START=1ST END                        
         CLC   DATL(L'BYTE1,R2),DATL(R3) CHECK LEVELS                           
         BE    BS014                                                            
         CLC   0(DATL,R2),ENDT(R2)  SAME START AND END BUT DIFF LEVELS?         
         BE    BS018                                                            
         XC    0(DATLVL,R3),0(R3)                                               
         XC    ENDT(DATLVL,R3),ENDT(R3)                                         
         B     BS012                DIFFERENT LEVELS DIFF START AND END         
*                                   SAFE TO REMOVE LOWER LEVEL DATES            
***********************************************************************         
* SAME LEVEL                                                          *         
***********************************************************************         
BS014    CLC   0(DATL,R2),ENDT(R2)  CHECK SAME START/END                        
         BE    BS016                                                            
         MVC   ENDT(DATLVL,R2),ENDT(R3) NOT SAME START/END, SAME LEVEL          
         XC    0(DATLVL,R3),0(R3)                                               
         XC    ENDT(DATLVL,R3),ENDT(R3)                                         
         B     BS012                 CLEAR OUT NEXT START AND NEXT END          
***********************************************************************         
* SAME START/END,SAME LEVEL                                           *         
***********************************************************************         
BS016    XC    0(DATLVL,R2),0(R2)                                               
         XC    ENDT(DATLVL,R2),ENDT(R2) THEN CLEAR OUT FIRST START              
         B     BS040                 AND END                                    
***********************************************************************         
* SAME START/END DIFFERENT LEVELS                                     *         
***********************************************************************         
BS018    XC   0(DATLVL,R2),0(R2)   CLEAR OUT BOTH SET OF DATES                  
         XC   ENDT(DATLVL,R2),ENDT(R2)                                          
         XC   0(DATLVL,R3),0(R3)                                                
         XC   ENDT(DATLVL,R3),ENDT(R3)                                          
         B    BS040                                                             
*                                                                               
BS020    CLC   ENDT(DATL,R3),0(R2)                                              
         BE    BS022               COMPARE NEXT END DATE WITH 1ST START         
         BH    BS026                                                            
         OI    SWTCHBI2,NESTED     NESTED BRO SWITCH ON/OFF DATES               
         B     BS032                                                            
*                                  IF NEXT END=1ST START CHECK LEVELS           
BS022    CLC   DATL(L'BYTE1,R2),DATL(R3) IF NEXT END=1ST START                  
         BNE   BS024                                                            
         XC    0(DATLVL,R2),0(R2)  THEN OK TO GET RID OF FIRST DATES            
         XC    ENDT(DATLVL,R2),0(R2) AND NEXT END                               
         XC    ENDT(DATLVL,R3),ENDT(R3)                                         
         B     BS040                                                            
*                                                                               
BS024    OC    STDATE,STDATE       LEVELS NOT EQUAL CHECK WITH STRTDATE         
         BZ    BS012                                                            
         CLC   0(DATL,R3),STDATE                                                
         BH    BS012                                                            
         XC    ENDT(DATLVL,R3),ENDT(R3) CLEAR OUT NEXT START AND END            
         XC    0(DATLVL,R3),0(R3)                                               
         OI    SWTCHBI2,SETHIRE                                                 
         MVC   TEMPDTE3,0(R2)      SAVE OFF TEMPDATE TO REPLACE LATER           
         MVC   0(DATL,R2),STDATE                                                
         B     BS012                                                            
*                                                                               
BS026    OC    0(DATL,R2),0(R2)    IF FIRST START ZERO REPLACE WITH             
         BZ    BS028               NEXT START                                   
         CLC   ENDT(DATL,R3),XFFS  IF NON ZERO CHECK WHETHER NEXT DATE          
         BNE   *+14                HAS AN END DATE IF NO THEN CHECK             
         CLC   DATL(L'BYTE1,R2),ENDTL(R3) LEVELS DIFFERENT                      
         BNE   BS030               IF LVLS DIFF DON'T REPLACE 1ST DAT!          
*                                                                               
         CLC   0(DATL,R2),0(R3)    COMPARE 1ST START WITH NEXT START            
         BNH   BS030                                                            
         OC    STDATE,STDATE       COMPARE WITH START DATE                      
         BZ    BS028                                                            
         CLC   0(DATL,R3),STDATE                                                
         BNL   BS028               IF START DATE<START DATE THEN DON'T          
         XC    ENDT(DATLVL,R3),ENDT(R3) REPLACE                                 
         B     BS030                                                            
*                                                                               
BS028    MVC   0(DATLVL,R2),0(R3)  THEN REPLACE 1ST START                       
BS030    XC    0(DATLVL,R3),0(R3)  CLEAR OUT NEXT START                         
BS031    NI    SWTCHBI2,X'FF'-NESTED                                            
*                                                                               
BS032    CLC   ENDT(DATL,R2),0(R2) COMPARE END1 WITH START1                     
         BNE   BS034                                                            
         XC    0(DATLVL,R2),0(R2)  REMOVE 1ST DATES IF EQUAL                    
         XC    ENDT(DATLVL,R2),ENDT(R2)                                         
         B     BS040                                                            
*                                                                               
BS034    CLC   ENDT(DATL,R3),XFFS  IF NO END DATE SKIP                          
         BE    BS038                                                            
         CLC   ENDT(DATL,R2),ENDT(R3)                                           
         BH    BS038               COMPARE 1ST END WITH NEXT END                
         CLC   ENDTL(L'BYTE1,R2),ENDTL(R3)                                      
         BNL   BS036               IF LOWER LEVEL END DATE THEN DON'T           
         XC    ENDT(DATLVL,R3),ENDT(R3) REPLACE REMOVE THE HIGHER LEVEL         
         XC    0(DATLVL,R3),0(R3)  ONE                                          
         B     BS012                                                            
*                                                                               
BS036    MVC   ENDT(DATLVL,R2),ENDT(R3) REPLACE CURRENT END WITH NEXT           
*                                                                               
BS038    CLC   ENDTL(L'BYTE1,R2),ENDTL(R3)                                      
         BE    BS012               CHECK TO SEE IF LEVELS ARE DIFF              
         TM    SWTCHBI2,NESTED     NESTED BRO DATES?                            
         BO    BS012                                                            
         XC    ENDT(DATLVL,R3),ENDT(R3)                                         
         B     BS012                                                            
*                                                                               
BS040    LA    R1,ENDATES                                                       
         LA    R2,DATLVL(R2)                                                    
         CR    R2,R1                                                            
         BNL   BS042                                                            
         LR    R3,R2               DATES WITH NEXT DATE ALONG                   
         B     BS012                                                            
*                                                                               
BS042    OC    STDATES,STDATES     ANY DATES LEFT?                              
         BZ    BS062               NOPE                                         
         BAS   RE,SORT             DISREGARD HIGHER LEVEL END DATES             
         LA    R2,STDATES          IF EARLIER LOWER LEVEL EXISTS                
*                                                                               
BS044    LA    R1,ENDATES                                                       
         CR    R2,R1                                                            
         BNL   BS050                                                            
         OC    ENDDTLVL(DATLVL,R2),ENDDTLVL(R2)                                 
         BZ    BS050                                                            
         CLC   0(DATL,R2),DATLVL(R2) COMPARE START WITH NEXT ST                 
         BNE   BS046                                                            
         CLC   DATL(1,R2),DATLVLVL(R2)  CHECK LEVELS PERSON LEVEL DATES         
         BH    BS045                    ALWAYS TAKE PRECEDENCE                  
         BL    BS045A                                                           
         CLC   ENDT(DATL,R2),ENDDTLV(R2) CHECK WHICH END DATE IS LATEST         
         BL    BS045                                                            
         B     BS045A                                                           
*                                                                               
BS045    XC    0(DATLVL,R2),0(R2)                                               
         XC    ENDT(DATLVL,R2),ENDT(R2)                                         
         B     BS048                                                            
*                                                                               
BS045A   XC    ENDDTLV(DATLVL,R2),ENDDTLV(R2) DON'T SHOW DATES FROM DIF         
         XC    DATLVL(DATLVL,R2),DATLVL(R2)   LEVELS BUT SAME START             
         B     BS048                                                            
*                                                                               
BS046    CLC   ENDTL(L'BYTE1,R2),ENDDTLVL(R2)                                   
         BNH   BS048                                                            
         XC    ENDT(DATLVL,R2),ENDT(R2)                                         
         XC    0(DATLVL,R2),0(R2)                                               
*                                                                               
BS048    LA    R2,DATLVL(R2)                                                    
         B     BS044                                                            
*                                                                               
BS050    BAS   RE,SORT             TIDY UP OUR TABLE                            
         LA    R2,STDATES                                                       
         B     *+8                                                              
*                                                                               
BS052    AHI   R2,DATLVL                                                        
         CLC   0(DATLVL,R2),SPACES                                              
         BNH   BS056                                                            
         OC    STDATE,STDATE       MAKE SURE FIRST START AND END OK             
         BZ    BS054                                                            
         CLC   0(DATL,R2),STDATE   CHECK TO SEE IF FIRST DATE IS AFTER          
         BH    *+10                START DATE                                   
         MVC   0(DATL,R2),STDATE                                                
         CLC   ENDT(DATL,R2),0(R2) COMPARE END1 WITH START1                     
         BNL   BS054                                                            
         CLC   ENDT(DATL,R2),DTSTDATE  IS IT SWITCHON/SWITCHOFF PAIR            
         BH    BS054               SAME AS START DATE                           
         XC    0(DATLVL,R2),0(R2)  REMOVE 1ST DATES IF EQUAL                    
         XC    ENDT(DATLVL,R2),ENDT(R2)                                         
*                                                                               
BS054    CLC   TERMDT,SPACES                                                    
         BNH   BS052                                                            
         CLC   ENDT(L'TERMDT,R2),XFFS NO END DATE?                              
         BE    BS052               THEN NO NEED TO CHECK AGAINST TERMDT         
         CLC   ENDT(L'TERMDT,R2),TERMDT                                         
         BL    *+10                CHECK TO SEE IF DATE IS BEFORE               
         MVC   ENDT(L'TERMDT,R2),TERMDT                                         
         B     BS052               TERM DATE                                    
*                                                                               
BS056    OC    STDATES,STDATES     IS OUR TABLE NOW BLANK                       
         BZ    BS062               YES THEN EXIT                                
         BAS   RE,SORT             TIDY UP OUR TABLE                            
*                                                                               
         MVC   TEMPDATE,STDATES                                                 
         GOTO1 =A(READCAL),DMCB,RR=RELO                                         
         CLC   STDATE,TEMPDATE     IF FIRST DATE MID PERIOD                     
         BNL   *+10                THEN SET TO PERIOD START                     
         MVC   STDATES(L'STDATE),STDATE                                         
*                                                                               
         LA    R2,STDATES                                                       
         MVC   YYMMDD3,0(R2)                                                    
         MVC   YYMMDD4,ENDT(R2)                                                 
         CLC   YYMMDD4,XFFS                                                     
         BNE   *+10                                                             
         XC    YYMMDD4,YYMMDD4                                                  
         GOTO1 DATCON,DMCB,(1,YYMMDD3),(10,PESBOS1)                             
         MVI   PESLVL1,C'('                                                     
         MVC   PESLVL1+1(1),DATL(R2)                                            
         OI    PESLVL1+1,X'F0'                                                  
         MVI   PESLVL1+2,C')'                                                   
         MVI   PESDAS1,C'-'                                                     
*                                                                               
         OC    YYMMDD4,YYMMDD4     ANY DATE?                                    
         BZ    BS060                                                            
         GOTO1 DATCON,DMCB,(1,YYMMDD4),(0,YYMMDD1)                              
         GOTO1 ADDAY,DMCB,YYMMDD1,YYMMDD1,F'-1'      SUBTRACT 1 TO GET          
         GOTO1 DATCON,DMCB,(0,YYMMDD1),(10,PESBOS2)  PERIOD END DATE            
         MVI   PESLVL2,C'('                                                     
         MVC   PESLVL2+1(1),ENDTL(R2)                                           
         OI    PESLVL2+1,X'F0'                                                  
         MVI   PESLVL2+2,C')'                                                   
*                                                                               
BS060    DS    0H                                                               
         LA    R2,DATLVL(R2)                                                    
         OC    0(DATLVL,R2),0(R2)  ANY DATES?                                   
         BZ    BS062                                                            
         MVC   YYMMDD3,0(R2)                                                    
         MVC   YYMMDD4,ENDT(R2)                                                 
         GOTO1 DATCON,DMCB,(1,YYMMDD3),(10,PESBOS3)                             
         MVI   PESLVL3,C'('                                                     
         MVC   PESLVL3+1(1),DATL(R2)                                            
         OI    PESLVL3+1,X'F0'                                                  
         MVI   PESLVL3+2,C')'                                                   
         MVI   PESDAS2,C'-'                                                     
         GOTO1 DATCON,DMCB,(1,YYMMDD4),(0,YYMMDD1)                              
         GOTO1 ADDAY,DMCB,(C'D',YYMMDD1),(0,YYMMDD1),F'-1'                      
         GOTO1 DATCON,DMCB,(0,YYMMDD1),(10,PESBOS4)                             
         MVI   PESLVL4,C'('                                                     
         MVC   PESLVL4+1(1),ENDTL(R2)                                           
         OI    PESLVL4+1,X'F0'                                                  
         MVI   PESLVL4+2,C')'                                                   
*                                                                               
         LA    R2,DATLVL(R2)                                                    
         OC    0(DATLVL,R2),0(R2)  ANY DATES?                                   
         BZ    BS062                                                            
         MVC   YYMMDD3,0(R2)                                                    
         MVC   YYMMDD4,ENDT(R2)                                                 
         GOTO1 DATCON,DMCB,(1,YYMMDD3),(10,PESBOS5)                             
         MVI   PESLVL5,C'('                                                     
         MVC   PESLVL5+1(1),DATL(R2)                                            
         OI    PESLVL5+1,X'F0'                                                  
         MVI   PESLVL5+2,C')'                                                   
         MVI   PESDAS3,C'-'                                                     
         GOTO1 DATCON,DMCB,(1,YYMMDD4),(0,YYMMDD1)                              
         GOTO1 ADDAY,DMCB,(C'D',YYMMDD1),(0,YYMMDD1),F'-1'                      
         GOTO1 DATCON,DMCB,(0,YYMMDD1),(10,PESBOS6)                             
         MVI   PESLVL6,C'('                                                     
         MVC   PESLVL6+1(1),ENDTL(R2)                                           
         OI    PESLVL6+1,X'F0'                                                  
         MVI   PESLVL6+2,C')'                                                   
*                                                                               
BS062    TM    SWTCHBI2,NOEDIT                                                  
         BZ    BS070                                                            
         OI    PESBOS1H+1,X'20'    NO EDIT ALLOWED PROTECT EVERYTHING           
         OI    PESBOS2H+1,X'20'                                                 
         OI    PESBOS3H+1,X'20'    IF NOTHING IN TABLE THEN PROTECT             
         OI    PESBOS4H+1,X'20'    EVERYTHING EXCEPT FIRST TWO FIELDS           
         OI    PESBOS5H+1,X'20'                                                 
         OI    PESBOS6H+1,X'20'                                                 
*                                                                               
BS070    LA    R2,STDATES                                                       
         TM    SWTCHBI2,SETHIRE    HAVE WE REPLACED A DATE WITH LOC ST          
         BZ    BS076               DATE?                                        
BS072    CLC   0(DATLVL,R2),SPACES                                              
         BNH   BS076                                                            
         CLC   0(DATL,R2),STDATE   FIND LOC START DATE TO REPLACE               
         BNE   BS074                                                            
         MVC   0(DATL,R2),TEMPDTE3                                              
BS074    AHI   R2,DATLVL                                                        
         B     BS072                                                            
*                                                                               
BS076    OI    PESBOS1H+6,X'80'    TRANSMIT FIELDS                              
         OI    PESBOS2H+6,X'80'                                                 
         OI    PESBOS3H+6,X'80'                                                 
         OI    PESBOS4H+6,X'80'                                                 
         OI    PESBOS5H+6,X'80'                                                 
         OI    PESBOS6H+6,X'80'                                                 
         OI    PESDAS1H+6,X'80'                                                 
         OI    PESDAS2H+6,X'80'                                                 
         OI    PESDAS3H+6,X'80'                                                 
         OI    PESLVL1H+6,X'80'                                                 
         OI    PESLVL2H+6,X'80'                                                 
         OI    PESLVL3H+6,X'80'                                                 
         OI    PESLVL4H+6,X'80'                                                 
         OI    PESLVL5H+6,X'80'                                                 
         OI    PESLVL6H+6,X'80'                                                 
*                                                                               
BRANDSX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET BRANDOCEAN SWITCHON DATES                                       *         
* AIO3 CONTAINS ADDRESS OF PERSON RECORD                              *         
* DATES ARE PLACED INTO STDATES AND ENDATES                           *         
***********************************************************************         
         SPACE 1                                                                
BRANGET  NTR1  BASE=*,LABEL=*      FINDS DATES IN ELEMENTS AND STORES           
         L     RC,SAVERC           START DATES IN FIRST 32 BYTES, END           
         XR    R0,R0               DATES IN LAST 32 BYTES                       
         L     R3,AIO3                                                          
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         XC    TEMPDATE,TEMPDATE                                                
         XC    TEMPDTE2,TEMPDTE2                                                
         USING GDAELD,R3                                                        
*                                                                               
BRAN2    CLI   GDAEL,0                                                          
         BE    BRANX                                                            
         CLI   GDAEL,GDAELQ                                                     
         BE    BRAN6                                                            
*                                                                               
BRAN4    IC    R0,GDALN                                                         
         AR    R3,R0                                                            
         B     BRAN2                                                            
*                                                                               
BRAN6    CLI   GDATYPE,GDATMCST    CHECK IF WE HAVE RIGHT DATE TYPE             
         BNE   BRAN4                                                            
         CLI   GDADATE,0           CHECK START AND END DATES                    
         BNE   *+12                                                             
         CLI   GDADATE2,0          GET SWITCH ON/SWITCH OFF DATES AT            
         BE    BRANX               EACH LEVEL                                   
         CLC   TERMDT,SPACES       ANY TERMINATION DATE?                        
         BNH   BRAN8                                                            
         CLC   GDADATE,TERMDT      CHECK IF START DATE IS BEFORE TERM           
         BH    BRAN4               DATE IF NOT SKIP                             
*                                                                               
BRAN8    CLC   HIREDT,SPACES       ANY HIRE DATE?                               
         BNH   BRAN10                                                           
*&&DO                                                                           
         CLC   GDADATE2,SPACES                                                  
         BNH   BRAN12                                                           
         CLC   GDADATE2,HIREDT     CHECK IF END IS BEFORE HIRE DATE             
         BL    BRAN4               IF SO SKIP                                   
*&&                                                                             
*                                                                               
BRAN10   OC    TEMPDATE,TEMPDATE   FIRST TIME?                                  
         BZ    BRAN12                                                           
         CLC   TEMPDATE,GDADATE    CHECK IF DATES MATCH                         
         BNE   BRAN12                                                           
         CLC   TEMPDTE2,GDADATE2   IF SECOND DATE MATCHES                       
         BE    BRAN4               SKIP THIS-DUPLICATE DATE                     
*                                                                               
BRAN12   L     R2,FULL                                                          
         MVC   TEMPDATE,GDADATE    SAVE OFF DATES FOR DUPLICATE COMPARE         
         MVC   TEMPDTE2,GDADATE2                                                
         LA    R1,ENDATES                                                       
         CR    R2,R1               IF TABLE FULL MOVE ON                        
         BNL   BRANXNE                                                          
         MVC   0(DATL,R2),GDADATE                                               
         MVC   DATL(L'BYTE1,R2),BYTE1 INSERT LEVEL # AFTER DATES                
         CLC   GDADATE2,SPACES                                                  
         BH    *+14                                                             
         MVC   ENDT(DATL,R2),XFFS  PUT F'S IF BLANK END DTE                     
         B     *+10                                                             
         MVC   ENDT(DATL,R2),GDADATE2                                           
         MVC   ENDTL(L'BYTE1,R2),BYTE1                                          
         LA    R2,DATLVL(R2)                                                    
         ST    R2,FULL                                                          
         B     BRAN4                                                            
*                                                                               
BRANXNE  LTR   RB,RB                                                            
BRANX    B     XIT                                                              
         DROP  R3                                                               
***********************************************************************         
* SORT FOR BRANDOCEAN SWITCHON DATES                                  *         
***********************************************************************         
SORT     NTR1  BASE=*,LABEL=*                                                   
         GOTO1 RMSPAC              REMOVE SPACES BETWEEN START                  
*                                  AND ENDATES                                  
SORT2    LA    R2,STDATES          SORT DATES INTO CHRONO ORDER                 
         MVI   BYTE1,0                                                          
         LA    R1,ENDATES                                                       
*                                                                               
SORT4    CR    R2,R1                                                            
         BNL   SORT8                                                            
         OC    0(DATLVL,R2),0(R2)                                               
         BZ    SORT8                                                            
         CLC   0(DATL,R2),DATLVL(R2)                                            
         BNL   SORT6               COMPARE NEXT START WITH START                
         XC    0(DATLVL,R2),DATLVL(R2)      SWAPPING DATES                      
         XC    DATLVL(DATLVL,R2),0(R2)                                          
         XC    0(DATLVL,R2),DATLVL(R2)                                          
         XC    ENDT(DATLVL,R2),ENDDTLV(R2)                                      
         XC    ENDDTLV(DATLVL,R2),ENDT(R2)                                      
         XC    ENDT(DATLVL,R2),ENDDTLV(R2)                                      
*                                                                               
         MVI   BYTE1,1                                                          
*                                                                               
SORT6    LA    R2,DATLVL(R2)                                                    
         B     SORT4                                                            
*                                                                               
SORT8    CLI   BYTE1,1                                                          
         BE    SORT2                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* REMOVE SPACES BETWEEN VALUES IN STDATES AND ENDATES                 *         
***********************************************************************         
RMSPAC   NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
         LA    R3,STDATES                                                       
         LA    R2,ENDATES         GET RID OF ANY SPACES IN DATES                
*                                                                               
RMSP1    OC    0(DATLVL,R3),0(R3)                                               
         BNZ   RMSP2                                                            
         OC    ENDT(DATLVL,R3),0(R3) FOUND BLANK DATE                           
         BZ    RMSP3                                                            
*                                                                               
RMSP2    AHI   R3,DATLVL                                                        
         CR    R3,R2              HIT END OF TABLE?                             
         BL    RMSP1              NO                                            
         B     RMSP5                                                            
*                                                                               
RMSP3    LR    R4,R3                                                            
RMSP4    AHI   R4,DATLVL                                                        
         CR    R4,R2                                                            
         BNL   RMSP5                                                            
         CLC   0(DATLVL,R4),SPACES FIND NEXT NON BLANK DATE                     
         BNH   RMSP4                                                            
         CLC   ENDT(DATLVL,R4),SPACES                                           
         BNH   RMSP4                                                            
         MVC   0(DATLVL,R3),0(R4)   MOVE NON BLANK DATE INTO BLANK DATE         
         MVC   ENDT(DATLVL,R3),ENDT(R4)                                         
         XC    0(DATLVL,R4),0(R4)                                               
         XC    ENDT(DATLVL,R4),ENDT(R4)                                         
         AHI   R3,DATLVL                                                        
         CR    R3,R2                                                            
         BL    RMSP3                                                            
RMSP5    B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
*        LTORG CONSTANTS AND EQUATES                                  *         
***********************************************************************         
         LTORG                                                                  
XFFS     DC    X'FFFFFF'                                                        
**SWITCHON DATES EQUATES                                                        
DATLVLVL EQU   L'GDADATE+L'BYTE1+L'GDADATE                                      
DATLVL   EQU   L'GDADATE+L'BYTE1                                                
DATL     EQU   L'GDADATE                                                        
ENDT     EQU   L'STDATES                                                        
ENDTL    EQU   L'STDATES+L'GDADATE                                              
ENDDTLV  EQU   L'STDATES+L'BYTE1+L'GDADATE                                      
ENDDTLVL EQU   L'STDATES+L'GDADATE+L'BYTE1+L'GDADATE                            
         EJECT                                                                  
***********************************************************************         
*        BUILD DISPLAY TABLE AND GET INFO FROM PERSON RECORD                    
***********************************************************************         
*                                                                               
BLDTABLE NMOD1 0,**BLDT**          DO NOT USE R7                                
         L     RC,SAVERC                                                        
*                                                                               
         MVI   TERMSTAT,0          RESET TERMINATE INDICATOR                    
         NI    BIT3,X'FF'-NODETAIL                                              
         LA    R0,DISTABLE         CLEAR DISPLAY TABLE                          
         L     R1,=A(DISTABND-DISTABLE)                                         
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    OLDPID,OLDPID       SAVE OLD PID NUMBER                          
*                                                                               
         XC    BIGKEY,BIGKEY       MAKE SURE HAVE RIGHT PERSON REC              
         MVC   BIGKEY(L'SAVEKEY),SAVEKEY                                        
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   BLDX                                                             
         LA    R6,BIGKEY                                                        
         USING PERRECD,R6                                                       
         MVC   DISKADDR,PERKDA     SAVE D/A FOR XTRA DETAILS                    
         GOTO1 GETREC                                                           
         USING PERRECD,R6                                                       
         L     R6,AIO              GET GENERAL INFO FROM PERSON REC             
         MVC   PERSON,PERKCODE     PERSON CODE                                  
*                                                                               
         USING EMPELD,R6                                                        
         XC    HIREDT,HIREDT                                                    
         XC    TERMDT,TERMDT                                                    
         L     R6,AIO              GET GENERAL INFO FROM PERSON REC             
         MVI   ELCODE,EMPELQ       X'56' - EMPLOYEE HISTORY ELEM                
         BAS   RE,GETEL                                                         
         BNE   BLD10                                                            
         MVC   HIREDT,EMPHIR       HIRE DATE                                    
         MVC   TERMDT,EMPTRM       TERM DATE                                    
*                                                                               
         USING PIDELD,R6                                                        
BLD10    XC    PIDNUM,PIDNUM       CLEAR SECURITY VALUES                        
         XC    OLDPID,OLDPID                                                    
         MVC   PIDNAME,SPACES                                                   
*                                                                               
         L     R6,AIO              GET GENERAL INFO FROM PERSON REC             
         MVI   ELCODE,PIDELQ       X'D8' - PERSONAL ID ELEM                     
         BAS   RE,GETEL                                                         
         BNE   BLD50                                                            
         MVC   PIDNUM,PIDNO        PERSONAL ID NUMBER                           
         MVC   OLDPID,PIDNO        SAVE OLD PID FOR DELETE PASSIVE KEY          
         GOTO1 =A(GETPIDNM),DMCB,RR=RELO     GET PID INFO                       
         MVC   SVPIDNM,PIDNAME                                                  
         EJECT                                                                  
***********************************************************************         
*        BUILD DISPLAY TABLE FROM PERSON REC AND SORT ON START DATE             
***********************************************************************         
*                                                                               
         USING DISTABD,R4                                                       
BLD50    LA    R4,DTLOCS           START BUILDING TABLE HERE                    
         USING LOCELD,R6                                                        
         L     R6,AIO                                                           
         XC    TABCOUNT,TABCOUNT                                                
*                                                                               
         MVI   ELCODE,LOCELQ                                                    
         BAS   RE,GETEL                                                         
         B     BLD60                                                            
BLD60NX  BAS   RE,NEXTEL                                                        
BLD60    BNE   BLD100                                                           
*                                                                               
         MVC   DTPREVST,LOCSTART   START DATE (NEVER CHANGES)                   
         MVC   DTSTDATE,LOCSTART   START DATE                                   
         MVC   DTENDATE,LOCEND     END DATE                                     
         MVC   DTPREVEN,LOCEND     END DATE (NEVER CHANGES)                     
         MVC   DTOFFICE,LOCOFF     OFFICE                                       
*&&US*&& MVC   TEMPOFF,LOCOFF      SAVE FOR LATER USE                           
         MVC   DTDEPT,LOCDEPT      DEPARTMENT                                   
         MVC   DTSUBDPT,LOCSUB     SUB DEPARTMENT                               
         MVC   DTSALKDT,LOCSALKD   SALARY LOCK DATE                             
         MVC   DTTSLKDT,LOCLOCK    TIMESHEET LOCK DATE                          
         CLI   LOCSTAT,LOCSTRM     STATUS IS TERMINATE?                         
         BNE   *+8                                                              
         MVI   TERMSTAT,LOCSTRM    YES, MARK THE FIELD TO TERMINATE             
         MVC   DTSTATUS,LOCSTAT    STATUS                                       
         MVC   DTSTAT2,LOCSTAT2    STATUS 2                                     
         MVC   DTATTR,LOCATTR      ATTRIBUTES AS IN X'83' ELEM                  
*&&US                                                                           
         MVC   DTFREE,AC@NOU                                                    
         TM    DTATTR,LOCYFRL      FREELANCER?                                  
         BNO   *+10                                                             
         MVC   DTFREE,AC@YESU                                                   
         TM    DTATTR,LOCBFRL      BRANDO FREELANCER?                           
         BNO   *+8                                                              
         MVI   DTFREE,C'B'                                                      
*&&                                                                             
*                                                                               
         USING STATD,R3                                                         
         L     R3,=A(STATTAB)      STATUS TABLE                                 
         A     R3,RELO                                                          
BLD70    CLI   0(R3),X'00'         EOT                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DTSTATUS,STBYTE     MATCH ON STATUS BYTE                         
         BE    BLD74                                                            
         LA    R3,STLNQ(R3)                                                     
         B     BLD70                                                            
BLD74    MVC   DTSTATNM,STNAME     GET NAME                                     
         DROP  R3                                                               
*                                                                               
         MVC   DTACCNT,SPACES      MAKE 1R ACCOUNT                              
         LA    R2,DTACCNT                                                       
         LLC   R1,LN1RLEV1                                                      
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   0(0,R2),LOCOFF                                                   
         EX    R1,*-6                                                           
         LA    R2,1(R1,R2)                                                      
         LLC   R1,LN1RLEV2                                                      
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   0(0,R2),LOCDEPT                                                  
         EX    R1,*-6                                                           
         LA    R2,1(R1,R2)                                                      
         LLC   R1,LN1RLEV3                                                      
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   0(0,R2),LOCSUB                                                   
         EX    R1,*-6                                                           
         LA    R2,1(R1,R2)                                                      
         LLC   R1,LN1RLEV4                                                      
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   0(0,R2),PERSON                                                   
         EX    R1,*-6                                                           
*                                                                               
         LH    R1,TABCOUNT         COUNT OF TABLE ENTRIES                       
         LA    R1,1(R1)                                                         
         STH   R1,TABCOUNT                                                      
*                                                                               
         LA    R4,DTLENQ(R4)                                                    
         LA    R1,DISTABND         END OF TABLE                                 
         CR    R4,R1                                                            
         BL    BLD60NX                                                          
*                                                                               
BLD100   TM    OPTSTAT,XDOPT       XD=Y OPTION ON                               
         BZ    BLD108                                                           
         L     R6,AIO                                                           
         USING ACTVD,R6                                                         
         MVI   ELCODE,X'F1'                                                     
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
         OI    BIT3,NODETAIL                                                    
         B     BLD108                                                           
*                                                                               
         USING XDETD,R3                                                         
         LA    R3,BLOCK+L'PVALOUTB*2                                            
         MVC   BLOCK+L'PVALOUTB*2(100),SPACES                                   
         CLI   TWAOFFC,C'*'        IF NOT DDS TERMINAL SUPPRESS DA              
         BNE   BLD105                                                           
         MVC   XDDAW,=C'DA='                                                    
         GOTO1 HEXOUT,DMCB,DISKADDR,XDDA,L'DISKADDR                             
         MVI   XDCOMMA1,C','                                                    
         MVC   XDDADDW,=C'DADD='                                                
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(X'20',XDDADD)                          
         MVI   XDCOMMA2,C','                                                    
         MVC   XDDCHAW,=C'DCHA='                                                
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(X'20',XDDCHA)                          
         MVI   XDCOMMA3,C','                                                    
         MVC   XDWHOW,=C'WHO='                                                  
         MVC   XDWHO,ACTVSCID                                                   
         B     BLD108                                                           
*                                                                               
BLD105   MVC   XDDADDWN,=C'DADD='                                               
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(X'20',XDDADDN)                         
         MVI   XDCOMM1N,C','                                                    
         MVC   XDDCHAWN,=C'DCHA='                                               
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(X'20',XDDCHAN)                         
         MVI   XDCOMM2N,C','                                                    
         MVC   XDWHOWN,=C'WHO='                                                 
         MVC   XDWHON,ACTVSCID                                                  
*                                                                               
BLD108   LH    R2,TABCOUNT         SORT ON START DATE                           
         GOTO1 VQSORT,DMCB,(C'N',DTLOCS),(R2),DTLENQ,L'DTPREVST,       X        
               DTPREVST-DISTABD                                                 
*                                                                               
         DROP  R4,R6,R3                                                         
         EJECT                                                                  
***********************************************************************         
*        FILL IN FILTERS IN TABLE FROM 1R RECS                                  
***********************************************************************         
*                                                                               
         USING DISTABD,R4                                                       
         LA    R4,DTLOCS           START OF TABLE                               
BLD110   OC    DTPREVST(DTLENQ),DTPREVST ANY ENTRY?                             
         BZ    BLDX                                                             
         MVC   DTFILTS,SPACES                                                   
         TM    DTSTAT2,LOCSDUP     IS THIS A DUPLICATE                          
         BO    BLD150NX            YES, THEN SKIP 1R INFO                       
*                                                                               
         USING ACTRECD,R6                                                       
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         MVC   ACTKEY,SPACES       DELETE 1R                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVI   ACTKUNT,C'1'        UNIT 1                                       
         MVI   ACTKLDG,C'R'        LEDGER R                                     
         MVC   ACTKACT,DTACCNT     ACCOUNT                                      
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         CLC   KEY2(L'ACTKEY),KEYSAVE                                           
         BNE   BLD150NX                                                         
         LA    R2,KEY2             GET RECORD                                   
         AH    R2,LKEY                                                          
         AH    R2,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST ',(R2),AIO2,WORK               
*                                                                               
         USING RSTELD,R6                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,RSTELQ       X'30' - RECORD STATUS ELEM                   
         BAS   RE,GETEL                                                         
         BNE   BLD150NX                                                         
*                                                                               
         MVC   DTFILT1,RSTFILT1    FILTERS                                      
         MVC   DTFILT2,RSTFILT2                                                 
         MVC   DTFILT3,RSTFILT3                                                 
         MVC   DTFILT4,RSTFILT4                                                 
         CLI   RSTLN,RSTLN1Q       DOES ELEM HAVE A FILT5                       
         BNH   *+10                                                             
         MVC   DTFILT5,RSTFILT5                                                 
*                                                                               
BLD150NX LA    R4,DTLENQ(R4)                                                    
         LA    R1,DISTABND         END OF TABLE                                 
         CR    R4,R1                                                            
         BL    BLD110                                                           
*                                                                               
BLDX     MVC   DTNAME,=C'*LOCTAB*' HEADER                                       
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'SAVEKEY),SAVEKEY  RESET KEY                             
         LA    R4,DTLOCS           SAVE                                         
         MVC   OFFICE,DTOFFICE                                                  
         MVC   DEPT,DTDEPT                                                      
         MVC   SUBDPT,DTSUBDPT                                                  
         NI    BIT2,X'FF'-REBUILD                                               
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE STATUS AND UPDATE TABLE ENTRY                                 
*        R3 POINTS TO SCREEN FIELD                                              
*        R4 POINTS TO DISTAB ENTRY                                              
***********************************************************************         
*                                                                               
         USING LLINED,R2                                                        
         USING DISTABD,R4                                                       
VALSTAT  NMOD1 0,*VALSTA*                                                       
         L     RC,SAVERC                                                        
*                                                                               
*&&US                                                                           
         MVC   OPTNAME,SPACES                                                   
         MVC   HALF(1),DTSTATUS    SAVE PREVIOUS STATUS                         
*&&                                                                             
         NI    DTLSTAT,X'FF'-DTSDEL                                             
         CLC   AC@DELU,LLSTAT      DELETE LOCATION                              
         BNE   VST05                                                            
         OI    DTLSTAT,DTSDEL      MARK TO DELETE LOCATION                      
         B     VSTX                                                             
*                                                                               
VST05    DS    0H                                                               
*&&US*&& MVC   BYTE,LLFREE         SAVEOFF FREELANCE STATUS FOR LATER           
         LA    R2,LLSTATH                                                       
         CLC   STDAY,DTSTDATE                                                   
         BH    VST06                                                            
         MVC   STDAY,DTSTDATE      SAVE 'HIGHEST' START DATE                    
VST06    MVI   DTSTATUS,LOCSACT    DEFAULT TO ACTIVE                            
         MVC   DTSTATNM,AC@ACTV    ACTIVE                                       
*&&US                                                                           
         MVI   OPTNAME,LOCSACT                                                  
         MVC   OPTNAME+1(L'AC@ACTV),AC@ACTV                                     
*&&                                                                             
         TM    4(R2),X'80'         INPUT THIS TIME                              
         BO    VST07                                                            
         OC    DTENDATE,DTENDATE   NO END DATE = ACTIVE                         
         BZ    VSTXYES                                                          
VST07    CLI   5(R2),0                                                          
         BE    VST50                                                            
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                R1 = EX CLC LENGTH                           
*                                                                               
         USING STATD,R3                                                         
         L     R3,=A(STATTAB)      STATUS TABLE                                 
         A     R3,RELO                                                          
VST10    CLI   0(R3),X'00'         EOT                                          
         BE    VSTXNO1                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   STNAME(0),8(R2)                                                  
         BE    VST20                                                            
         LA    R3,STLNQ(R3)                                                     
         B     VST10                                                            
*                                                                               
VST20    DS    0H                                                               
*&&UK*&& MVC   DTSTATUS,STBYTE     STATUS BYTE                                  
*&&UK*&& MVC   DTSTATNM,STNAME     STATUS NAME                                  
*&&US                                                                           
         MVC   OPTNAME(L'STBYTE),STBYTE    SAVE IN THIS FIELD SINCE             
         MVC   OPTNAME+1(L'STNAME),STNAME  ONLY USED FOR LIST SCREEN            
*&&                                                                             
*                                                                               
VST50    DS    0H                                                               
*&&US                                                                           
         CLI   OPTNAME,LOCFRL      IS THIS FGAP?                                
         BNE   *+14                                                             
         CLC   BYTE,AC@NOU         THEN IT MUST BE A FREELANCER?                
         BE    VSTXNO1                                                          
*&&                                                                             
         OC    DTENDATE,DTENDATE   IF THERE'S AN END DATE                       
         BZ    VST70                                                            
*&&US                                                                           
         MVC   TEMPOFF,DTOFFICE                                                 
         MVC   TEMPDATE,DTENDATE                                                
         GOTO1 =A(READCAL),DMCB,RR=RELO                                         
         CLI   OPTNAME,LOCSTRM                                                  
         BNE   VST52                                                            
         TM    BIT,CURRLOC         CURRENT LOCATION -CHECK THE DATE             
         BO    VST50A                                                           
         CLI   HALF,LOCSTRM        WAS PREVIOUS STATUS TERM TOO?                
         BNE   VST50A              NO THEN CHECK THE DATE                       
         TM    DTLSTAT,DTSUPD      USING UPD FEATURE                            
         BZ    *+14                CHECK THE DATE                               
VST50A   CLC   ENDATE,DTENDATE     MAKE SURE ITS ON AN ENDDATE                  
         BNE   ERRLOCBT                                                         
*&&                                                                             
*&&UK*&& CLI   DTSTATUS,LOCSTRM                                                 
*&&UK*&& BNE   VST52                                                            
         MVC   TSTDAY,DTSTDATE     SAVE 'TERM' START DATE                       
         MVC   TENDAY,DTENDATE     SAVE 'TERM' END DATE                         
VST52    DS    0H                                                               
*&&US*&& CLI   OPTNAME,LOCSACT     THEN YOU CAN'T HAVE STATUS ACTIVE            
*&&UK*&& CLI   DTSTATUS,LOCSACT    THEN YOU CAN'T HAVE STATUS ACTIVE            
         BE    *+12                                                             
         CLI   DTSTATUS,LOCSTRAN                                                
         BNE   VSTXYES                                                          
         CLI   RECNUM,RTPR2        PER2 SCREEN                                  
         BE    VSTXNO1                                                          
*&&UK*&& CLC   DTENDATE,TERMDT     TERMED LOCATION?                             
*&&UK*&& BNE   VST60                                                            
*&&US                                                                           
         TM    BIT,CURRLOC                                                      
         BZ    *+14                                                             
         CLC   ENDATE,DTENDATE     MAKE SURE ITS ON AN ENDDATE                  
         BNE   ERRLOCBT                                                         
         OC    TERMDT,TERMDT       A TERMED LOCATION                            
         BZ    VST60                                                            
         MVI   OPTNAME,LOCSTRM     THEN MARK AS TERMINATED                      
         MVC   OPTNAME+1(L'DTSTATNM),AC@TRM     TERM                            
         B     VSTXYES                                                          
VST60    MVI   OPTNAME,LOCSTRAN   ELSE MARK AS TRANSFERED                       
         MVC   OPTNAME+1(L'DTSTATNM),AC@XFR     TRANSFER                        
         B     VSTXYES                                                          
VST70    CLI   OPTNAME,LOCSACT     IF NOT ACTIVE, INSIST ON AN END DATE         
         BNE   VSTXNO2                                                          
         OC    TERMDT,TERMDT                                                    
         BNZ   VSTXNO3                                                          
VSTXYES  MVC   DTSTATUS,OPTNAME    STATUS BYTE                                  
         MVC   DTSTATNM,OPTNAME+1  STATUS NAME                                  
         MVC   8(L'DTSTATNM,R2),DTSTATNM                                        
VSTX     B     XYES                                                             
VSTXNO1  B     ERRINV              INVALID STATUS                               
VSTXNO2  LA    R2,PEMPENDH                                                      
         B     ERRENDDA            'PLEASE ENTER AN END DATE'                   
VSTXNO3  LA    R2,PEMTERMH                                                      
         B     ERRALTDM            'ACTIVE LOC/TERMDATE MISMATCH'               
*&&                                                                             
*&&UK                                                                           
         MVI   DTSTATUS,LOCSTRM    THEN MARK AS TERMINATED                      
         MVC   DTSTATNM,DDTERM     TERM                                         
         B     VSTXYES                                                          
VST60    MVI   DTSTATUS,LOCSTRAN   ELSE MARK AS TRANSFERED                      
         MVC   DTSTATNM,DDXFR      TRANSFER                                     
         B     VSTXYES                                                          
VST70    CLI   DTSTATUS,LOCSACT    IF NOT ACTIVE, INSIST ON AN END DATE         
         BNE   VSTXNO2                                                          
VSTXYES  MVC   8(L'DTSTATNM,R2),DTSTATNM                                        
VSTX     B     XYES                                                             
VSTXNO1  B     ERRINV              INVALID STATUS                               
VSTXNO2  LA    R2,PEMPENDH                                                      
         B     ERRENDDA            'PLEASE ENTER AN END DATE'                   
*&&                                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        GETS PID NAME USING PIDNUM                                             
***********************************************************************         
*                                                                               
GETPIDNM NMOD1 0,*GETPID*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         MVC   SECLAST,SPACES                                                   
         MVC   SECFIRST,SPACES                                                  
*                                                                               
         USING SA0REC,R6                                                        
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         MVI   SA0KTYP,SA0KTYPQ    C'0' - PERSONAL AUTH. RECORD                 
         MVC   SA0KAGY,SECALPHA    SECURITY ALPHA ID                            
         MVC   SA0KNUM,PIDNUM                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',KEY2,AIO2                    
         L     R6,AIO2                                                          
         CLC   KEY2(L'SA0KEY),0(R6)                                             
         BNE   GPIDX                                                            
         TM    SA0STAT,X'20'       LOCKED                                       
         BO    GPIDX                                                            
*                                                                               
         USING SAPALD,R6                                                        
         MVI   ELCODE,SAPALELQ     X'C3' - PERSONAL ID ELEM                     
         BAS   RE,GETEL2                                                        
         BNE   GPIDX                                                            
         MVC   PIDNAME,SAPALPID                                                 
         TM    BIT3,PIDCHK         IS THIS COMING FROM THE LIST                 
         BO    GPIDX               YES-ONLY NEEDED THE NAME                     
*                                                                               
         USING SAPEREC,R6                                                       
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         MVI   SAPETYP,SAPETYPQ    C'F' - SECURITY PERSON REC                   
         MVI   SAPESUB,SAPESUBQ    X'04'                                        
         MVC   SAPEAGY,SECALPHA    SECURITY ALPHA ID                            
         MVC   SAPEPID,PIDNAME                                                  
         MVC   SAPEDEF,TODAYC      TODAYS DAY (COMPLEMENT)                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',KEY2,AIO2                    
         L     R6,AIO2                                                          
         CLC   KEY2(SAPEDEF-SAPEKEY),0(R6)                                      
         BNE   GPIDX                                                            
*                                                                               
         USING SANAMD,R6                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,SANAMELQ     X'C5' - PERSON NAME ELEM                     
         BAS   RE,GETEL2                                                        
         BNE   GPIDX                                                            
         LA    R3,SANAMES          NAME SUBELEMS                                
         TM    SANAMIND,SANAMIFN   IS THERE A FIRST NAME                        
         BNO   GPID10                                                           
         LLC   R1,0(R3)                                                         
         STC   R1,SECFLN                                                        
         AHI   R1,-1                                                            
         BM    GPID10                                                           
         MVC   SECFIRST(0),1(R3)                                                
         EX    R1,*-6                                                           
         LA    R4,SECFIRST                                                      
         LA    R2,L'SECFIRST                                                    
         BAS   RE,UPCASE                                                        
         LA    R3,2(R1,R3)                                                      
GPID10   TM    SANAMIND,SANAMIMN   IS THERE A MIDDLE NAME                       
         BNO   GPID20                                                           
         LLC   R1,0(R3)                                                         
         LA    R3,1(R1,R3)                                                      
GPID20   TM    SANAMIND,SANAMILN   IS THERE A LAST NAME                         
         BNO   GPIDX                                                            
         LLC   R1,0(R3)                                                         
         STC   R1,SECLLN                                                        
         AHI   R1,-1                                                            
         BM    GPIDX                                                            
         MVC   SECLAST(0),1(R3)                                                 
         EX    R1,*-6                                                           
         LA    R4,SECLAST                                                       
         LA    R2,L'SECLAST                                                     
         BAS   RE,UPCASE                                                        
GPIDX    XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        CHANGE FIELD AT R4 TO UPPERCASE FOR LEN IN R2                          
***********************************************************************         
*                                                                               
UPCASE   NTR1                                                                   
UP10     CLI   0(R4),X'81'                                                      
         BL    UP20                                                             
         CLI   0(R4),X'A9'                                                      
         BH    UP20                                                             
         OI    0(R4),X'40'         MAKE UPPER CASE                              
UP20     LA    R4,1(R4)                                                         
         BCT   R2,UP10                                                          
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        GET AND TEST 1R ACCOUNT AGAINST FILTERS (LOCEL IN R6)        *         
***********************************************************************         
*                                                                               
         USING LOCELD,R6                                                        
TST1RF15 NMOD1 0,*TST1RF*                                                       
         L     RC,SAVERC                                                        
         SPACE 1                                                                
         USING ACTRECD,R4                                                       
         LA    R4,KEY2             DOES 1R RECORD EXIST                         
         XC    KEY2,KEY2                                                        
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVI   ACTKUNT,C'1'        UNIT 1                                       
         MVI   ACTKLDG,C'R'        LEDGER R                                     
         LA    RF,ACTKACT          ACCOUNT CODE                                 
         LLC   R1,LN1RLEV1                                                      
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   0(0,RF),LOCOFF                                                   
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         LLC   R1,LN1RLEV2                                                      
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   0(0,RF),LOCDEPT                                                  
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         LLC   R1,LN1RLEV3                                                      
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   0(0,RF),LOCSUB                                                   
         EX    R1,*-6                                                           
         LA    RF,1(R1,RF)                                                      
         LLC   R1,LN1RLEV4                                                      
         AHI   R1,-1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         L     RE,AIO                                                           
         MVC   0(0,RF),PERKCODE-PERRECD(RE)                                     
         EX    R1,*-6                                                           
         GOTO1 DATAMGR,DMCB,=CL8'DMREAD',=CL8'ACCDIR',KEY2,KEY2                 
         BNE   TST1RFN                                                          
         SPACE 1                                                                
         LA    R1,OPTFLT1                                                       
         LA    RF,ACTKSAF1                                                      
         BAS   RE,TESTIT                                                        
         BNE   TST1RFN                                                          
         LA    R1,OPTFLT2                                                       
         LA    RF,ACTKSAF2                                                      
         BAS   RE,TESTIT                                                        
         BNE   TST1RFN                                                          
         LA    R1,OPTFLT3                                                       
         LA    RF,ACTKSAF3                                                      
         BAS   RE,TESTIT                                                        
         BNE   TST1RFN                                                          
         LA    R1,OPTFLT4                                                       
         LA    RF,ACTKSAF4                                                      
         BAS   RE,TESTIT                                                        
         BNE   TST1RFN                                                          
         LA    R1,OPTFLT5                                                       
         LA    RF,ACTKSAF5                                                      
         BAS   RE,TESTIT                                                        
         BNE   TST1RFN                                                          
         SPACE 1                                                                
TST1RFY  GOTO1 READ                REREAD BIGKEY (PERKEY)                       
         CR    RB,RB                                                            
         B     TST1RFX                                                          
TST1RFN  GOTO1 READ                REREAD BIGKEY (PERKEY)                       
         LTR   RB,RB                                                            
TST1RFX  XIT1                                                                   
         SPACE 1                                                                
TESTIT   TM    0(R1),X'40'                                                      
         BZ    TESTIT2                                                          
         CLC   0(1,R1),0(RF)                                                    
         BR    RE                                                               
TESTIT2  MVC   DUB+3(1),0(R1)                                                   
         OI    DUB+3,X'40'                                                      
         MVI   DUB+4,0                                                          
         CLC   DUB+3(1),0(RF)                                                   
         BE    TESTIT4                                                          
         MVI   DUB+4,1                                                          
TESTIT4  CLI   DUB+4,1                                                          
         BR    RE                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        MAKE SURE THEY ENTER AN END DATE FOR PREVIOUS LOCATION       *         
***********************************************************************         
*                                                                               
PREV     USING DISTABD,R4                                                       
PREVEND  NMOD1 0,*PREVEND                                                       
         L     RC,SAVERC                                                        
*                                                                               
         NI    CHKSTAT,X'FF'-SALOUT                                             
         CLI   ACTEQU,ACTADD       NO PREVIOUS END DATE ON ADD                  
         BE    XIT                                                              
*                                                                               
         TM    CHKSTAT,DELELOC     DELETING A LOCATION?                         
         BO    *+8                 THEN DON'T CLEAR SCRBOT                      
         MVI   SCRBOT,0            SET DEFAULT SCREEN                           
         LA    R4,DTLOCS             PREVIOUS TABLE ENTRY                       
         AH    R4,STDISP                                                        
         OC    0(DTLENQ,R4),0(R4)    ANY PREVIOUS ENTRY?                        
         BZ    XIT                   NO                                         
         TM    PEMTERMH+4,X'80'    TERM DATE ENTERED THIS TIME?                 
         BO    PE10                                                             
         TM    PEMSTDTH+4,X'80'    START DATE ENTERED THIS TIME?                
         BO    PE10                                                             
         TM    PEMPENDH+4,X'80'    END DATE ENTERED THIS TIME?                  
         BZ    *+8                                                              
PE10     NI    PEMPENDH+4,X'FF'-X'20'                                           
*                                                                               
         XC    TEMPDATE,TEMPDATE                                                
         OC    PREV.DTENDATE,PREV.DTENDATE                                      
         BZ    *+10                                                             
         MVC   TEMPDATE,PREV.DTENDATE                                           
         XC    PREV.DTENDATE,PREV.DTENDATE                                      
         TM    BIT,REQRD           REQUIRED FIELD                               
         BO    PE40                                                             
         CLI   PEMPENDH+5,0                                                     
         BNE   PE50                                                             
*&&UK*&& OC    TERMDT,TERMDT                                                    
*&&UK*&& BNZ   PE20                                                             
         TM    PEMPENDH+4,X'80'    END DATE ENTERED THIS TIME?                  
         BZ    XIT                                                              
         CLC   PREV.DTPREVEN,SPACES                                             
         BNH   XIT                                                              
         MVC   TEMPDATE,PREV.DTPREVEN   CHECK TO SEE IF REMOVED DATE            
         GOTO1 =A(READCAL),DMCB,RR=RELO WAS MID PERIOD                          
         CLC   TEMPDATE,ENDATE                                                  
         BE    XIT                                                              
         MVC   TEMPOFF,PREV.DTOFFICE    CHECK TO SEE IF MID PERIOD              
         MVC   TEMPDEPT,PREV.DTDEPT     TIME EXISTS                             
         MVC   TEMPSDPT,PREV.DTSUBDPT                                           
         MVC   ACCNT,PREV.DTACCNT                                               
         GOTO1 =A(CHKTIME),DMCB,RR=RELO                                         
         BE    XIT                       NO - FINE EXIT                         
         GOTO1 DATCON,DMCB,(1,TEMPDATE),(DOUT,PEMPEND)                          
         LA    R2,PEMPENDH               ERROR - CANNOT REMOVE END DATE         
         OI    PEMPENDH+6,X'80'                                                 
         B     EDPOST                                                           
                                                                                
PE20     GOTO1 DATCON,DMCB,(1,TERMDT),(DOUT,PEMPEND)                            
         MVI   PEMPENDH+5,8                                                     
         B     PE50                                                             
*                                  VALIDATE TERM AS END DATE                    
*                                                                               
PE40     DS    0H                  REQ'D AND NOTHING ENTERED                    
         CLI   PEMPENDH+5,0                                                     
         BNE   PE50                                                             
         GOTO1 DATCON,DMCB,(1,STDATE),(11,WORK)                                 
         MVC   WORK+8(5),=C'(-1D)'                                              
         CLI   LANGCODE,3                                                       
         BNE   *+10                                                             
         MVC   WORK+8(5),=C'(-1T)'                                              
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,(13,WORK),(BYTE,BLOCK)                               
         B     PE52                                                             
*                                                                               
PE50     LA    R1,PEMPEND          VALIDATE ENTERED END DATE                    
         ST    R1,DMCB                                                          
         LLC   R1,PEMPENDH+5                                                    
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
PE52     CLI   DMCB+4,X'04'                                                     
         BE    PE52A                                                            
         MVC   PREV.DTENDATE,TEMPDATE                                           
         LA    R2,PEMPENDH                                                      
         B     EINVDATE                                                         
*                                                                               
         USING PERVALD,R3                                                       
PE52A    LA    R3,BLOCK                                                         
         GOTO1 DATCON,DMCB,(1,PVALPSTA),(DOUT,PEMPEND)                          
         OI    PEMPENDH+6,X'80'                                                 
         CLC   PVALPSTA,PREV.DTSTDATE   MUST BE AFTER START DATE                
         BH    PE52B                                                            
         MVC   PREV.DTENDATE,TEMPDATE                                           
         TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD?                 
         BO    *+12                                                             
         LA    R2,PEMPENDH                                                      
         B     ESTEND                                                           
*                                                                               
         LA    R2,PEMOFFH          THEN POINT TO NEW LOC                        
         B     ERRCONF             AND GIVE A DIFFERENT MESSAGE                 
*                                                                               
PE52B    OC    TERMDT,TERMDT                                                    
         BZ    PE54                                                             
*&&UK                                                                           
         CLC   PVALPSTA,TERMDT          CANNOT BE AFTER TERM DATE               
         BNH   PE54                                                             
         MVC   PREV.DTENDATE,TEMPDATE                                           
         LA    R2,PEMPENDH                                                      
         B     EDTTERM                                                          
*&&                                                                             
*                                                                               
PE54     OC    STDATE,STDATE            NEXT LOC'S START DATE (IF ONE)          
         BZ    PE56                                                             
         CLC   PVALPSTA,STDATE          MUST BE BEFORE NEXT START               
         BL    PE56                                                             
         MVC   PREV.DTENDATE,TEMPDATE                                           
         LA    R2,PEMPENDH                                                      
         B     ENDAFTST                                                         
*                                                                               
*                                                                               
*  CHECK FOR TIME PAST END DATE ENTERED                                         
*                                                                               
*                                  TOOK THESE CHECKS OUT, I THINK THIS          
*                                  MAY BE ALLOWING USERS TO END A LOC           
*                                  WITH TIME ALREADY ENTERED                    
PE56     LA    R2,PEMPENDH                                                      
*        TM    4(R2),X'80'              DATE INPUT THIS TIME                    
*        BO    PE57                     VALIDATE                                
*        TM    4(R2),X'20'         DATE PREV VALIDATED                          
*        BO    PE58                                                             
*        TM    1(R2),X'20'              IS DATE PROTECTED                       
*        BO    PE58                                                             
PE57     MVC   YYMMDD1,PVALESTA    MOVE IN END DATE                             
         GOTO1 DATCON,DMCB,(0,YYMMDD1),(1,YYMMDD4)                              
                                                                                
         GOTO1 ADDAY,DMCB,YYMMDD1,YYMMDD2,F'1'                                  
         GOTO1 DATCON,DMCB,(0,YYMMDD2),(1,STDATE)                               
         XC    ENDATE,ENDATE                                                    
         MVC   TEMPOFF,PREV.DTOFFICE                                            
         MVC   TEMPDEPT,PREV.DTDEPT                                             
         MVC   TEMPSDPT,PREV.DTSUBDPT                                           
         MVC   ACCNT,PREV.DTACCNT                                               
         MVI   EXPBYTE,EXPBYTEN                                                 
         GOTO1 =A(CHKEXP),DMCB,RR=RELO                                          
         BE    *+18                                                             
         MVC   PREV.DTENDATE,TEMPDATE                                           
         LA    R2,PEMPENDH                                                      
         B     ERREXPC                                                          
                                                                                
         OC    PREV.DTTSLKDT,PREV.DTTSLKDT TIMESHEET LOCK DATE                  
         BZ    PE571                       NO ELSE USE IT                       
         MVC   YYMMDD4,PREV.DTTSLKDT                                            
         GOTO1 DATCON,DMCB,(1,PREV.DTTSLKDT),(0,YYMMDD1)                        
                                                                                
         GOTO1 ADDAY,DMCB,YYMMDD1,YYMMDD2,F'1'   NO FUTURE TIME BTN             
         GOTO1 DATCON,DMCB,(0,YYMMDD2),(1,STDATE) NEW DATE + 1                  
         XC    ENDATE,ENDATE                                                    
         MVC   TEMPOFF,PREV.DTOFFICE                                            
         MVC   TEMPDEPT,PREV.DTDEPT                                             
         MVC   TEMPSDPT,PREV.DTSUBDPT                                           
         MVC   ACCNT,PREV.DTACCNT                                               
PE571    GOTO1 =A(CHKTIME),DMCB,RR=RELO  CAN'T END WITH FUTURE TIME             
         BE    PE572                                                            
                                                                                
         USING LOCELD,R6           FUTURE TIME IS OKAY IF THIS DATE             
         L     R6,AIO              ALREADY EXISTS ON THE LOCEL                  
         MVI   ELCODE,LOCELQ       I.E. IF IT IS A MID-PERIOD TIMESHEET         
         BAS   RE,GETEL                                                         
         B     PE571A                                                           
PE571ANX BAS   RE,NEXTEL                                                        
PE571A   BE    *+6                                                              
         DC    H'0'                                                             
         CLC   LOCOFF,PREV.DTOFFICE  MATCH ON OFFICE                            
         BNE   PE571ANX                                                         
         CLC   LOCDEPT,PREV.DTDEPT   MATCH ON DEPT                              
         BNE   PE571ANX                                                         
         CLC   LOCSUB,PREV.DTSUBDPT  MATCH ON SUBDEPT                           
         BNE   PE571ANX                                                         
         CLC   LOCSTART,PREV.DTSTDATE                                           
         BNE   PE571ANX                                                         
         GOTO1 DATCON,DMCB,(0,YYMMDD1),(1,STDATE)                               
         CLC   LOCEND,STDATE                                                    
         BE    PE572                                                            
         MVC   PREV.DTENDATE,LOCEND                                             
         LA    R2,PEMPENDH                                                      
         B     EDPOST                   CAN'T CHANGE - POSTINGS EXIST           
         DROP  R6                                                               
                                                                                
PE572    OC    TEMPDATE,TEMPDATE   DO WE HAVE AN ORIGINAL END DATE              
         BZ    PE57A               NO                                           
         CLC   YYMMDD4,TEMPDATE     CHECK NEW END DATE IS HIGHER                
         BE    PE57A                                                            
         GOTO1 DATCON,DMCB,(1,TEMPDATE),(0,YYMMDD1)                             
         GOTO1 ADDAY,DMCB,YYMMDD1,YYMMDD2,F'1'   NO FUTURE TIME BTN             
         GOTO1 DATCON,DMCB,(0,YYMMDD2),(1,STDATE) NEW DATE + 1                  
         OI    BIT3,MIDPER         SET BIT - AS DATE MAY FALL IN PERIOD         
         GOTO1 =A(CHKTIME),DMCB,RR=RELO  CAN'T END WITH FUTURE TIME             
         BE    PE573                                                            
         MVC   PREV.DTENDATE,TEMPDATE                                           
         LA    R2,PEMPENDH                                                      
         B     EDPOST                   CAN'T CHANGE - POSTINGS EXIST           
*                                                                               
PE573    GOTO1 DATCON,DMCB,(1,YYMMDD4),(0,YYMMDD1)                              
         GOTO1 ADDAY,DMCB,YYMMDD1,YYMMDD2,F'1'   NO FUTURE TIME BTN             
         GOTO1 DATCON,DMCB,(0,YYMMDD2),(1,STDATE) NEW DATE + 1                  
*                                                                               
PE57A    TM    BIT2,NEWTIME                                                     
         BZ    PE57B                                                            
         GOTO1 =A(CHKSVTMS),DMCB,RR=RELO CHECK FOR SAVED TMS RECS               
         BE    PE57A1                                                           
         MVC   PREV.DTENDATE,TEMPDATE                                           
         LA    R2,PEMPENDH                                                      
         B     ERRSVTMS                                                         
*                                                                               
PE57A1   OC    TEMPDATE,TEMPDATE   DO WE HAVE AN ORIGINAL END DATE              
         BZ    PE58                NO                                           
         CLC   YYMMDD4,TEMPDATE    CHECK NEW END DATE IS HIGHER                 
         BE    PE58                                                             
         GOTO1 DATCON,DMCB,(1,TEMPDATE),(0,YYMMDD1)                             
         GOTO1 ADDAY,DMCB,YYMMDD1,YYMMDD2,F'1'   NO FUTURE TIME BTN             
         GOTO1 DATCON,DMCB,(0,YYMMDD2),(1,STDATE) NEW DATE + 1                  
         OI    BIT3,MIDPER         SET BIT - AS DATE MAY FALL IN PERIOD         
         GOTO1 =A(CHKSVTMS),DMCB,RR=RELO  CAN'T END WITH FUTURE TIME            
         BE    PE57A2                                                           
         MVC   PREV.DTENDATE,TEMPDATE                                           
         LA    R2,PEMPENDH                                                      
         B     ERRSVTMS                                                         
*                                                                               
PE57A2   GOTO1 DATCON,DMCB,(1,YYMMDD4),(0,YYMMDD1)                              
         GOTO1 ADDAY,DMCB,YYMMDD1,YYMMDD2,F'1'   NO FUTURE TIME BTN             
         GOTO1 DATCON,DMCB,(0,YYMMDD2),(1,STDATE) NEW DATE + 1                  
         B     PE58                                                             
*                                                                               
PE57B    GOTO1 =A(DRAFTRNS),DMCB,RR=RELO ELSE CHECK FOR DRAFT TIME              
         BNE   PE58                                                             
         MVC   PREV.DTENDATE,TEMPDATE                                           
         LA    R2,PEMPENDH                                                      
         B     ERRDRAFT             CAN'T CHANGE - POSTINGS EXIST               
                                                                                
*  CHECK FOR SALARY PAST END DATE - IF SO SET SCRBOT TO LOAD BOTTOM OF          
*  SCREEN AFTER IT VALIDATES EVERYTHING                                         
*                                                                               
PE58     LA    R2,PEMPENDH                                                      
         TM    4(R2),X'80'              DATE INPUT THIS TIME                    
         BO    PE59                     VALIDATE                                
         TM    4(R2),X'20'              DATE PREV VALIDATED                     
         BO    PE60                                                             
         TM    1(R2),X'20'              IS DATE PROTECTED                       
         BO    PE60                                                             
PE59     OC    PREV.DTSALKDT,PREV.DTSALKDT SAL LOCK DATE                        
         BNZ   PE60                        IF SO SKIP                           
         GOTO1 DATCON,DMCB,(1,PVALPSTA),(DOUT,PEMPEND)                          
         MVC   YYMMDD1,PVALESTA                                                 
         GOTO1 ADDAY,DMCB,YYMMDD1,YYMMDD2,F'1'   NO SAL BETWEEN                 
         GOTO1 DATCON,DMCB,(0,YYMMDD2),(1,STDATE) NEW DATE + 1                  
         XC    ENDATE,ENDATE                                                    
         MVC   TEMPOFF,PREV.DTOFFICE                                            
         MVC   TEMPDEPT,PREV.DTDEPT                                             
         MVC   TEMPSDPT,PREV.DTSUBDPT                                           
         MVC   ACCNT,PREV.DTACCNT                                               
         MVI   SCRBOT,0                                                         
         GOTO1 =A(CHKSAL),DMCB,RR=RELO                                          
         BE    PE60                                                             
         CLI   BYTE,LIMITSAL       OVER THE RECORD LIMIT?                       
         BNH   PE59A               NO                                           
         MVI   SCRBOT,0                                                         
         GOTO1 =A(SCREENS),DMCB,RR=RELO                                         
         B     EHISTLMT                                                         
*                                                                               
PE59A    OI    CHKSTAT,SALOUT                                                   
         MVC   PREV.DTENDATE,PVALPSTA                                           
         B     PEXX                                                             
*                                                                               
PE60     MVC   PREV.DTENDATE,PVALPSTA   FILL IN PREV ENTRY                      
*                                                                               
PEX      OI    PEMPENDH+4,X'20'    END DATE VALIDATED ALREADY                   
PEXX     XIT1                                                                   
         DROP  R3,PREV                                                          
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DELETE LOCATION IF POSSIBLE                                            
*        R4 SHOULD POINT TO TABLE ENTRY                                         
***********************************************************************         
*                                                                               
         USING DISTABD,R4                                                       
DELLOC   NMOD1 0,*DELLOC*                                                       
         L     RC,SAVERC                                                        
*                                                                               
***********************************************************************         
*        1ST CHECK TO SEE IF THERE'S A DUPLICATE LOC                            
*        ON EXIT -- TEMPDATE CONTAINS SAL LOCK DATE OF DUP                      
*                   TEMPDTE2 CONTAINS TMS LOCK DATE OF DUP                      
***********************************************************************         
*                                                                               
         NI    BIT3,X'FF'-DUPLOC                                                
         TM    BIT3,DELDUP         HAVE WE ALREADY DELETED ONE DUP              
         BO    ERRDLOC             YES THEN CANNOT CHECK FOR ANOTHER            
         GOTO1 =A(CHKDUP),DMCB,RR=RELO                                          
*                                                                               
***********************************************************************         
*        TO DELETE = NO 1R RECS WITH CONTRAS (IF NOT ON NEW TIME)               
***********************************************************************         
*                                                                               
         USING ACTRECD,R6                                                       
DL01     LA    R6,KEY2                                                          
         XC    KEY2,KEY2                                                        
         MVC   ACTKEY,SPACES       CAN'T HAVE 1R REC WITH CONTRA                
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVI   ACTKUNT,C'1'        UNIT 1                                       
         MVI   ACTKLDG,C'R'        LEDGER R                                     
         MVC   ACTKACT,DTACCNT                                                  
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         CLC   KEY2(L'ACTKEY),KEYSAVE                                           
         BNE   EINVPER             NO 1R REC FOUND, ERROR                       
         AH    R6,LKEY                                                          
         AH    R6,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST ',(R6),AIO2,WORK               
         L     R6,AIO2                                                          
         MVI   ELCODE,ABLELQ       X'32' BALANCE ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   DL20                NO BALANCE, OK TO DELETE (MAYBE)             
*                                                                               
         USING ABLELD,R6                                                        
         CP    ABLFRWD,=P'0'                                                    
         BNE   DL10                CHECK IF THERE'S A DUP LOC                   
         CP    ABLDR,=P'0'                                                      
         BNE   DL10                                                             
         CP    ABLCR,=P'0'                                                      
         BNE   DL10                                                             
         B     DL20                                                             
*                                                                               
DL10     DS    0H                IS THERE A DUP FOR THIS LOC?                   
         TM    BIT3,DUPLOC       NO-SO THEN CAN'T DELETE                        
         BZ    ERRACBAL          YES-CAN DEL SINCE 1R WILL STILL EXIST          
         DROP  R6                                                               
***********************************************************************         
*        TO DELETE = NO TIME IN PERIOD                                          
***********************************************************************         
*                                                                               
DL20     MVC   STDATE,DTSTDATE                                                  
         MVC   ENDATE,DTENDATE                                                  
         OC    DTTSLKDT,DTTSLKDT   ANY TS LOCK DATE                             
         BZ    *+10                                                             
         MVC   ENDATE,DTTSLKDT     USE THAT FOR END DATE                        
         MVC   ACCNT,DTACCNT                                                    
         GOTO1 =A(CHKTIME),DMCB,RR=RELO                                         
         BNE   EDPOST              CAN'T CHANGE - POSTINGS EXIST                
         MVI   EXPBYTE,EXPBYTEN                                                 
         GOTO1 =A(CHKEXP),DMCB,RR=RELO                                          
         BNE   ERREXPD                                                          
         TM    BIT2,NEWTIME                                                     
         BZ    DL20A                                                            
         GOTO1 =A(CHKSVTMS),DMCB,RR=RELO CHECK FOR SAVED TMS RECS               
         BNE   ERRSVTMS                                                         
         EJECT                                                                  
***********************************************************************         
*        TO DELETE - NO SALARY OR HOURS POSTINGS                                
***********************************************************************         
*                                                                               
DL20A    TM    BIT2,NEWTIME        USER ON NEW TIME                             
         BO    DL20B               THEN CHECK FOR POSTINGS                      
         GOTO1 =A(DRAFTRNS),DMCB,RR=RELO ELSE CHECK FOR DRAFT TIME              
         BNE   DL20B               NO DRAFT TRANSACTIONS-CONTINUE               
         LA    R2,LLSELH                                                        
         B     ERRDRAFT                                                         
         OI    6(R2),X'40'                                                      
*                                                                               
DL20B    ZAP   SALDB,=P'0'         CLEAR COUNTERS                               
         ZAP   SALCR,=P'0'                                                      
         ZAP   HRSDB,=P'0'                                                      
         ZAP   HRSCR,=P'0'                                                      
*                                                                               
         TM    BIT3,DUPLOC       IF THERE IS A DUP LOC                          
         BO    DL30              ALLOW PASS CHECK                               
*                                                                               
         XC    WORK,WORK                                                        
         OC    DTENDATE,DTENDATE   ANY END DATE                                 
         BZ    DL21                NO-USE TODAY'S DATE                          
         GOTO1 DATCON,DMCB,(1,DTENDATE),(0,WORK)                                
         B     DL21A                                                            
*                                                                               
DL21     GOTO1 DATCON,DMCB,(5,0),(0,WORK)   TODAY'S DATE                        
DL21A    TM    BIT3,DUPLOC         IS THIS A DUP LOCATION?                      
         BZ    DL21B               THEN ONLY CHECK BTN START AND END            
         GOTO1 DATCON,DMCB,(0,WORK),(1,FULL)                                    
         B     DL22                                                             
*                                                                               
DL21B    LA    R6,YEARCHKQ         # OF YEARS BACK TO CHECK                     
         LNR   R6,R6               MAKE NEGATIVE                                
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+10,(R6)                              
         XC    WORK(10),WORK                                                    
         GOTO1 DATCON,DMCB,(0,WORK+10),(1,FULL)                                 
*                                                                               
         USING CACRECD,R6                                                       
DL22     LA    R6,KEY2                                                          
         XC    KEY2,KEY2                                                        
         MVC   CACKEY,SPACES       CONTRA-ACCOUNT RECORD                        
         MVC   CACKCPY,CMPY        COMPANY                                      
         MVI   CACKUNT,C'1'        UNIT 1                                       
         MVI   CACKLDG,C'R'        LEDGER R                                     
         MVC   CACKACT,DTACCNT                                                  
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         B     DL23                                                             
DLNXT    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCDIR ',KEY2,KEY2                    
DL23     CLC   KEYSAVE(CACKWRK-CACKEY),KEY2                                     
         BNE   DL26                                                             
         LA    R6,KEY2                                                          
         CLC   CACKOFF,SPACES      WORK CODE=SPACES                             
         BNE   DLNXT                                                            
         CLC   CACKCULC(CACKSPAC-CACKULC),SPACES  MUST HAVE                     
         BE    DLNXT                              CONTRA ACCNT                  
         CLC   CACKBTYP,SPACES     ANYTHING IN BUCKET TYPE                      
         BE    DLNXT                                                            
         CLC   CACKSTYP+1(2),SPACES   LAST 2 BYTES OF BUCKET SUB-               
         BNE   DLNXT                  TYPE MUST BE SPACES                       
         MVC   BYTE,CACKBTYP      SVE BUCKET TYP (ONLY NEED 1ST BYTE)           
*                                                                               
         LA    R2,KEY2                                                          
         AH    R2,LKEY                                                          
         AH    R2,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST ',(R2),AIO2,WORK               
*                                                                               
         USING BUKELD,R6                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,BUKELQ       X'45' BUCKET ELEMENT                         
         BAS   RE,GETEL                                                         
         B     *+8                 NO BALANCE, OK TO DELETE (MAYBE)             
DL24NX   BAS   RE,NEXTEL                                                        
         BNE   DLNXT                                                            
*                                                                               
         TM    BIT3,DUPLOC         ARE WE CHECKING BTN START AND END            
         BZ    DL24A                                                            
         CLC   BUKYEAR(2),DTSTDATE CLC W/ LOC START DATE                        
         BL    DL24NX                                                           
         CLC   BUKYEAR(2),FULL     CLC W/ LOC END DATE                          
         BH    DL24NX                                                           
         B     DL24B                                                            
*                                                                               
DL24A    CLC   BUKYEAR(2),FULL     COMPARE YYMM                                 
         BL    DL24NX              IF < CHECK DATE SKIP                         
*                                                                               
DL24B    CLI   BYTE,C'H'           HOURS BUCKET?                                
         BE    DL25                                                             
         AP    SALDB(8),BUKDR(6)    SALARY DEBITS                               
         AP    SALCR(8),BUKCR(6)    SALARY CREDITS                              
         B     DL24NX                                                           
DL25     AP    HRSDB(8),BUKDR(6)    HOURS DEBITS                                
         AP    HRSCR(8),BUKCR(6)    HOURS CREDITS                               
         B     DL24NX                                                           
*                                                                               
DL26     CP    SALDB,=P'0'         NO SALARY DEBITS                             
         BNE   *+14                                                             
         CP    SALCR,=P'0'      NO SALARY CREDITS                               
         BE    DL30                                                             
         LA    R2,LLSELH                                                        
         B     ERRSPOST                                                         
         OI    6(R2),X'40'                                                      
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        CHECK FOR SALARY WITHIN LOCATION DATES AND FUTURE            *         
*        MAY HAVE THE OPTION TO DELETE SALARY ALONG WITH LOCATION     *         
*        IF # OF HISTORY RECS IS NOT GREATER THAN LIMIT ALLOWED       *         
***********************************************************************         
*                                                                               
DL30     MVC   TEMPOFF,DTOFFICE                                                 
         MVC   TEMPDEPT,DTDEPT                                                  
         MVC   TEMPSDPT,DTSUBDPT                                                
         MVC   STDATE,DTSTDATE                                                  
         MVC   ENDATE,DTENDATE                                                  
         MVC   ACCNT,DTACCNT                                                    
         OC    DTSALKDT,DTSALKDT   ANY SAL LOCK DATE                            
         BZ    *+10                                                             
         MVC   ENDATE,DTSALKDT     USE THAT FOR END DATE                        
         GOTO1 =A(CHKSAL),DMCB,RR=RELO                                          
         BNE   DL35                SEE IF WE CAN DELETE THE SALARY              
         TM    BIT2,ADJRATE                                                     
         BO    DL35                SEE IF WE CAN DELETE THE SALARY              
*                                                                               
* CHECK FOR FUTURE SAL WITH AN ADJ RATE                                         
*                                                                               
         OC    DTENDATE,DTENDATE   ANY END DATE                                 
         BZ    DL32                                                             
         GOTO1 =A(FUTURSAL),DMCB,RR=RELO                                        
DL32     TM    BIT2,ADJRATE                                                     
         BO    DL35                SEE IF WE CAN DELETE THE SALARY              
         B     DL100               OKAY TO DELETE THE PERSON-NO SALARY          
*                                                                               
* LOAD BOTTOM OF SCREEN WITH SALARY MESSAGE AND EXIT                            
*                                                                               
DL35     TM    GFACTST6,X'40'      RUNNING UNDER SCRIPT UPLOAD?                 
         BO    EDSALS              THAN CAN'T USE NEW FEATURE                   
         MVI   WORK,ACTDEL        DEFAULT TO ACTION DELETE                      
         LA    R3,WORK                                                          
         TM    CHKSTAT,MOVESAL     WANT TO MOVE SALARY?                         
         BZ    *+12                                                             
         MVI   WORK,8             ACTION MOVE                                   
         LA    R3,WORK                                                          
         MVI   SBYTE,RTHIS                                                      
         ICM   R3,8,SBYTE                                                       
         GOTO1 SECRET,DMCB,('SECPRACT',ASECBLK),(R3)                            
         BE    DL35A                                                            
         TM    BIT2,ADJRATE        THEN GIVE MESSAGE SAL EXISTS                 
         BO    EDFADJ                                                           
         B     EDSALS                                                           
DL35A    CLI   SCRBOT,1            BOTTOM OF SCREEN ALREADY LOADED              
         BE    DL45                GO DELETE SALARY                             
         MVI   SCRBOT,1                                                         
         OI    CHKSTAT,DELELOC     USER IS DELETING LOCATION                    
         GOTO1 =A(SCREENS),DMCB,RR=RELO                                         
         LA    R2,PEMANSWH         POINT TO MESSAGE INPUT FIELD                 
         B     ERRPLS                                                           
*                                                                               
* CAN ONLY USE THE SALARY/DELETE FEATURE IF (1) THE # OF HISTORY RECS           
* THAT NEED TO BE DELETED IS <= THE LIMIT. (2) IF USER HAS ACCESS TO            
* HISTORY/DELETE ACTION.                                                        
*                                                                               
DL45     TM    CHKSTAT,MSGQUIT     DID USER NOT WANT TO DEL SALARY              
         BZ    DL47                                                             
         NI    CHKSTAT,X'FF'-MSGQUIT TURN OFF BIT AND                           
         MVI   SCRBOT,0                                                         
         GOTO1 =A(SCREENS),DMCB,RR=RELO                                         
         TM    BIT2,ADJRATE        THEN GIVE MESSAGE SAL EXISTS                 
         BO    EDFADJ                                                           
         B     EDSALS                                                           
DL47     CLI   BYTE,LIMITSAL                                                    
         BNH   DL50                                                             
         MVI   SCRBOT,0                                                         
         GOTO1 =A(SCREENS),DMCB,RR=RELO                                         
         B     EHISTLMT                                                         
*                                                                               
DL50     MVC   TEMPOFF,DTOFFICE                                                 
         MVC   TEMPDEPT,DTDEPT                                                  
         MVC   TEMPSDPT,DTSUBDPT                                                
         MVC   STDATE,DTSTDATE                                                  
         MVC   ENDATE,DTENDATE                                                  
         OC    DTSALKDT,DTSALKDT   ANY SAL LOCK DATE                            
         BZ    *+10                                                             
         MVC   ENDATE,DTSALKDT     USE THAT FOR END DATE                        
         GOTO1 =A(DELSAL),DMCB,RR=RELO DELETE SALARY                            
         MVI   SCRBOT,0            CLEAR BOTTOM OF SCREEN                       
         GOTO1 =A(SCREENS),DMCB,RR=RELO                                         
         EJECT                                                                  
***********************************************************************         
*        PASSED CHECKS - DELETE ELEM FROM PERSON REC                            
***********************************************************************         
*                                                                               
         USING LOCELD,R6                                                        
DL100    L     R6,AIO                                                           
         MVI   ELCODE,LOCELQ       X'83' LOCATION ELEM                          
         BAS   RE,GETEL                                                         
         B     DL110                                                            
DL100NX  BAS   RE,NEXTEL                                                        
DL110    BNE   DLX                                                              
*                                                                               
         CLC   DTOFFICE,LOCOFF                                                  
         BNE   DL100NX                                                          
         CLC   DTDEPT,LOCDEPT                                                   
         BNE   DL100NX                                                          
         CLC   DTSUBDPT,LOCSUB                                                  
         BNE   DL100NX                                                          
         CLC   DTPREVST,LOCSTART                                                
         BNE   DL100NX                                                          
*                                                                               
         MVI   0(R6),X'FF'         DELETE ELEMENT                               
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         OI    BIT2,REBUILD        REBUILD TABLE                                
         NI    BIT3,X'FF'-DELDUP                                                
         TM    BIT3,DUPLOC         IS THERE A DUPLICATE LOC                     
         BZ    *+8                                                              
         OI    BIT3,DELDUP         SET BIT THAT DUP WAS DELETED                 
*                                                                               
         BAS   RE,DUPUNMRK         CHECK TO UNMARK PREV DUP                     
         BNE   DLX                 NO DUPS -MARKED DEL FOR 1R UPDATE            
         NI    DTLSTAT,X'FF'-DTSDEL   DUPS -DON'T DELETE 1R                     
DLX      B     XIT                                                              
         DROP  R6,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        UNMARK PREVIOUS DUP (IF ONE) WHEN DELETING LOCATION                    
*        R4 POINTS TO ENTRY BEING DELETED                                       
***********************************************************************         
*                                                                               
DEL      USING DISTABD,R4          R4 POINTS TO DELETED ENTRY                   
TAB      USING DISTABD,R3                                                       
DUPUNMRK NTR1                                                                   
         LR    R3,R4               START AT DELETION                            
DU10NX   LA    R3,DTLENQ(R3)       NEXT TABLE ENTRY                             
         LA    R1,DISTABND         END OF TABLE                                 
         CR    R3,R1                                                            
         BNL   DUXNO                                                            
*                                                                               
         TM    DEL.DTSTAT2,LOCSDUP           IF DELETING DUP                    
         BO    DUXYES                        DON'T DELETE 1R                    
*                                                                               
         CLC   DEL.DTOFFICE,TAB.DTOFFICE     SAME OFF                           
         BNE   DU10NX                                                           
         CLC   DEL.DTDEPT,TAB.DTDEPT         SAME DEPT                          
         BNE   DU10NX                                                           
         CLC   DEL.DTSUBDPT,TAB.DTSUBDPT     SAME SUB DPET                      
         BNE   DU10NX                                                           
         NI    TAB.DTSTAT2,X'FF'-LOCSDUP     UNMARK MOST CURR DUP               
DUXYES   B     XYES                                                             
DUXNO    B     XNO                                                              
         DROP  DEL,TAB                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CHECK FOR DRAFT TIME TRANSACTIONS IF USER IS NOT ON NEW TIME           
***********************************************************************         
*                                                                               
         USING DISTABD,R4                                                       
         USING TIMRECD,R6                                                       
DRAFTRNS NMOD1 0,*DRTRANS                                                       
         L     RC,SAVERC                                                        
*                                                                               
         LA    R6,KEY2                                                          
         XC    KEY2,KEY2                                                        
         MVC   TIMKEY,SPACES       TRANSACTION RECORD                           
         MVC   TIMKCPY,CMPY        COMPANY                                      
         MVI   TIMKUNT,C'1'        UNIT 1                                       
         MVI   TIMKLDG,C'R'        LEDGER R                                     
         MVC   TIMKACT,DTACCNT                                                  
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         B     DRF10                                                            
DRFNXT   GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCDIR ',KEY2,KEY2                    
DRF10    CLC   KEYSAVE(TIMKOFF-TIMKEY),KEY2                                     
         BNE   DRAFTNO                                                          
         LA    R6,KEY2                                                          
         CLC   TIMKCULC(TIMKPEDT-TIMKULC),SPACES  MUST HAVE                     
         BE    DRFNXT                             CONTRA ACCNT                  
         OC    TIMKPEDT,TIMKPEDT   MUST HAVE A TRANSACTION DATE                 
         BZ    DRFNXT                                                           
         TM    TIMKSTA,X'40'      IS THIS A DRAFT TRANSACTION                   
         BZ    DRFNXT                                                           
*                                                                               
         CLC   TIMKPEDT,STDATE                                                  
         BL    DRFNXT                                                           
         OC    ENDATE,ENDATE                                                    
         BZ    DRAFTYN                                                          
         CLC   TIMKPEDT,ENDATE                                                  
         BH    DRFNXT                                                           
*                                                                               
DRAFTYN  B     XYES                                                             
DRAFTNO  B     XNO                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        MANIPULATE SCREENS                                                     
***********************************************************************         
*                                                                               
SCREENS  NMOD1 0,*SCRNS*                                                        
         L     RC,SAVERC                                                        
*                                                                               
         CLI   SCRBOT,0            ZERO-CLEAR BOTTOM OF SCREEN                  
         BNE   SCR50               ELSE LOAD MESSAGE SCREEN                     
*                                                                               
         LA    R2,PEMMSG1H         START OF MESSAGE                             
         LA    R3,PEMPFKYH         END OF MESSAGE                               
SCRCLR   DS    0H                                                               
         LLC   R1,0(R2)            FIELD LENGTH                                 
         AHI   R1,-9               8 FOR HEADER, 1 FOR EX                       
         XC    8(0,R2),8(R2)                                                    
         EX    R1,*-6                                                           
         MVI   5(R2),X'00'                                                      
         OI    6(R2),X'80'         TRANSMIT                                     
         LLC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BL    SCRCLR              NO                                           
         B     SCRX                                                             
*                                                                               
SCR50    LA    R2,PEMMSG1H         LINE 21                                      
         LLC   R1,0(R2)                                                         
         AHI   R1,-10              8 FOR HDR 1 FOR EX & 1 ALREADY DONE          
         LA    R2,PEMMSG1                                                       
         MVI   0(R2),C'*'                                                       
         EXMVC R1,1(R2),0(R2)                                                   
         LA    R2,PEMMSG1H                                                      
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         LA    R2,PEMMSG2H         LINE 22                                      
         L     R1,=A(DDFTSAL1)                                                  
         A     R1,RELO                                                          
         MVC   PEMMSG2(23),0(R1)                                                
         GOTO1 DICTATE,DMCB,C'SL  ',PEMMSG2,0                                   
         OI    6(R2),X'80'                                                      
         LA    R2,PEMMSG3H                                                      
         CLI   SCRBOT,2            2=DELETE,MOVE OR QUIT OPTIONS                
         BE    SCR55                                                            
         L     R1,=A(DDFTSAL2)     1=DELETE OR QUIT OPTIONS                     
         A     R1,RELO                                                          
         MVC   PEMMSG3(43),0(R1)                                                
         B     SCR60                                                            
SCR55    L     R1,=A(DDFTSAL3)                                                  
         A     R1,RELO                                                          
         MVC   PEMMSG3(50),0(R1)                                                
SCR60    GOTO1 DICTATE,DMCB,C'SL  ',PEMMSG3,0                                   
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,PEMMSG4H         LINE 23                                      
         LLC   R1,0(R2)                                                         
         AHI   R1,-10              8 FOR HDR 1 FOR EX & 1 ALREADY DONE          
         LA    R2,PEMMSG4                                                       
         MVI   0(R2),C'*'                                                       
         EXMVC R1,1(R2),0(R2)                                                   
         LA    R2,PEMMSG4H                                                      
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
SCRX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*         DELETE FUTURE SALARY                                                  
***********************************************************************         
*                                                                               
DELSAL   NMOD1 0,*DELSAL*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         TM    CHKSTAT2,MOALOCK    IS ALL THE SAL WITHIN THE VALID MOA          
         BO    ERRMLOCK                                                         
*                                                                               
         NI    CHKSTAT,X'FF'-HISTELEM                                           
         MVC   KEY2,BIGKEY         SAVE KEY                                     
*                                                                               
         USING PHIRECD,R6                                                       
         LA    R6,BIGKEY           CHECK FOR ANY SALARY AT LOCATION             
         XC    BIGKEY,BIGKEY                                                    
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ    X'3E05' PAYROLL HISTORY RECORD               
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,CMPY        COMPANY                                      
         MVC   PHIKOFC,TEMPOFF                                                  
         MVC   PHIKDPT,TEMPDEPT                                                 
         MVC   PHIKSBD,TEMPSDPT                                                 
         MVC   PHIKPER,PERSON                                                   
         MVC   PHIKMOA,=X'0000'     GET MOST CURRENT                            
         MVC   SAVEKEY,BIGKEY                                                   
         GOTO1 HIGH                                                             
         B     DSL10                                                            
*                                                                               
DSLSEQ   NI    CHKSTAT,X'FF'-HISTELEM                                           
         GOTO1 SEQ                                                              
DSL10    CLC   BIGKEY(PHIKMOA-PHIKEY),KEYSAVE                                   
         BNE   DSLX                                                             
*                                                                               
         LA    R6,BIGKEY                                                        
         ZICM  R1,PHIKMOA,2        MOA                                          
         LNR   R1,R1                                                            
         STH   R1,WORK                                                          
         CLC   WORK(2),STDATE      BEFORE START DATE IS OK                      
         BL    DSLX                                                             
         OC    ENDATE,ENDATE       NO END DATE                                  
         BZ    DSL14                                                            
         CLC   WORK(2),ENDATE      AFTER END DATE, GET NEXT                     
         BH    DSLSEQ                                                           
*                                                                               
DSL14    MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         USING PDEELD,R6                                                        
         L     R6,AIO2             CHECK DATES                                  
         MVI   ELCODE,PDEELQ       X'86' PAYROLL DETAIL ELEM                    
         BAS   RE,GETEL                                                         
         B     DSL16                                                            
DSL16NX  BAS   RE,NEXTEL                                                        
DSL16    BNE   DSL30                                                            
*                                                                               
DSL17    CLI   0(R6),PDEELQ        STILL '86' ELEM                              
         BNE   DSL30                                                            
         CLC   PDEDTE(2),STDATE       IS DATE WITHIN PERIOD                     
         BL    DSL20                                                            
*                                                                               
DSL18    OC    TEMPDATE,TEMPDATE   IS THERE A SALLOCK DTE FROM A DUPLOC         
         BZ    DSL25                                                            
         CLC   TEMPDATE,PDEDTE                                                  
         BNH   DSL25                                                            
*                                                                               
* SET BIT THAT AN '83' ELEM STILL EXISTS SO DON'T DELETE REC                    
*                                                                               
DSL20    OI    CHKSTAT,HISTELEM                                                 
         B     DSL16NX                                                          
*                                                                               
DSL25    MVI   0(R6),X'FF'         REMOVE '83' ELEM                             
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM             REMELEM SHIFTS ELEMENTS AFTER DEL            
         B     DSL17                                                            
*                                                                               
DSL30    TM    CHKSTAT,HISTELEM    DO ANY ELEMS REMAIN ON THIS REC              
         BO    DSL35               THEN DON'T DELETE IT BUT WRITE BACK          
         USING PHIRECD,R6                                                       
         L     R6,AIO2                                                          
         OI    PHIRSTAT,X'80'      MARK REC DELETED                             
         LA    R6,BIGKEY                                                        
         OI    PHIKSTAT,X'80'      MARK KEY DELETED                             
DSL35    GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
         B     DSLSEQ                                                           
*                                                                               
DSLX     MVC   BIGKEY,KEY2         RESET KEY                                    
         MVC   SAVEKEY,KEY2         RESET KEY                                   
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                REESTABLISH SEQUENCE                         
         GOTO1 GETREC                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CHECK FOR DUPLICATE LOCATION WHEN DELETING A LOC                       
***********************************************************************         
*                                                                               
DUP      USING DISTABD,R4          ENTRY BEING DELETED                          
CHK      USING DISTABD,R3                                                       
CHKDUP   NMOD1 0,*CHKDUP*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         NI    BIT3,X'FF'-DUPLOC                                                
         XC    TEMPDATE,TEMPDATE                                                
         XC    TEMPDTE2,TEMPDTE2                                                
         ST    R4,FULL             SAVE ADDRESS OF ENTRY BEING DEL              
         LH    RF,TABCOUNT         # OF ENTRIES FOR BCT                         
         LA    R3,DTLOCS           POINT R3 TO BEGINNING OF TABLE               
         B     CH15                                                             
CH10NX   LA    R3,DTLENQ(R3)       NEXT TABLE ENTRY                             
         LA    R1,DISTABND         END OF TABLE                                 
         CR    R3,R1                                                            
         BNL   CHX                                                              
*                                                                               
CH15     CLC   DUP.DTOFFICE,CHK.DTOFFICE     SAME OFF                           
         BNE   CH10NX                                                           
         CLC   DUP.DTDEPT,CHK.DTDEPT         SAME DEPT                          
         BNE   CH10NX                                                           
         CLC   DUP.DTSUBDPT,CHK.DTSUBDPT     SAME SUBDEPT                       
         BNE   CH10NX                                                           
         C     R3,FULL             THE LOC BEING DELETED DOESN'T COUNT          
         BE    CH10NX                                                           
*                                                                               
         OI    BIT3,DUPLOC                                                      
         OC    TEMPDATE,TEMPDATE   SAL LOCK DATE ALREADY FOUND?                 
         BNZ   CH20                YES NO NEED TO GO FURTHUR                    
         OC    CHK.DTSALKDT,CHK.DTSALKDT    SAL LOCK DATE?                      
         BZ    *+10                                                             
         MVC   TEMPDATE,CHK.DTSALKDT        SAVE SAL LOCK DATE                  
*                                                                               
CH20     OC    TEMPDTE2,TEMPDTE2   TMS LOCK DATE ALREADY FOUND?                 
         BNZ   CH25                YES NO NEED TO GO FURTHUR                    
         OC    CHK.DTTSLKDT,CHK.DTTSLKDT    TMS LOCK DATE?                      
         BZ    *+10                                                             
         MVC   TEMPDTE2,CHK.DTTSLKDT          SAVE TMS LOCK DATE                
CH25     BCT   RF,CH10NX                                                        
*                                                                               
CHX      B     XIT                                                              
         DROP  DUP,CHK                                                          
***********************************************************************         
*        DELETE 1R AND ANY PROFILE OR STD HOURS REC FOR PERSON                  
*        R4 SHOULD POINT TO TABLE ENTRY BEING DELETED                           
***********************************************************************         
*                                                                               
         USING DISTABD,R4                                                       
DEL1R    NMOD1 0,*DEL1R**                                                       
         L     RC,SAVERC                                                        
         MVC   AIO,AIO2            USE AIO2                                     
*                                                                               
         USING ACTRECD,R6                                                       
         XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY                                                        
         MVC   ACTKEY,SPACES       DELETE 1R                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVI   ACTKUNT,C'1'        UNIT 1                                       
         MVI   ACTKLDG,C'R'        LEDGER R                                     
         MVC   ACTKACT,DTACCNT                                                  
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   D1RX                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         OI    ACTRSTAT,X'80'      MARK FOR DELETION                            
         LA    R6,BIGKEY                                                        
         OI    ACTKSTAT,X'80'      MARK FOR DELETION                            
         GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
*                                                                               
* DELETE NAME SEARCH POINTER                                                    
*                                                                               
         GOTO1 VACSRCHP,DMCB,C'DT  ',AIO,ACTKDA,0,ACOMFACS,(LN1RLOW,0)          
                                                                                
* DELETE RATE AND ADJUST RATE RECORDS                                           
*                                                                               
*&&US*&& GOTO1 =V(AC1RMNT),DMCB,(C'D',AIO),DATAMGR,RR=RELO                      
*                                                                               
* ADD 'FA' POINTER ELEMENT AND RECORD ACTIVITY PASSIVE POINTER                  
*                                                                               
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        PASSED CHECKS - DELETE PROFILE OR STD HOURS RECS                       
***********************************************************************         
*                                                                               
         USING CAHRECD,R6                                                       
         LA    R6,BIGKEY           MAKE TABLE OF ALL METHODS                    
         XC    BIGKEY,BIGKEY       MUST DELETE FOR ALL METHODS                  
         LA    R3,METHTAB                                                       
         XC    METHTAB,METHTAB                                                  
*                                                                               
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    METHOD BY NUMBER RECORD                      
         MVI   CAHKSUB,CAHKSUBQ                                                 
         MVC   CAHKCPY,CMPY        COMPANY                                      
         MVC   KEY2,BIGKEY                                                      
         GOTO1 HIGH                                                             
         B     D1R10                                                            
D1R10NX  GOTO1 SEQ                                                              
D1R10    CLC   BIGKEY(3),KEY2                                                   
         BNE   D1R20                                                            
         GOTO1 GETREC                                                           
         MVC   0(1,R3),CAHKMTHD    SAVE METHOD NUMBER IN TABLE                  
         LA    R3,1(R3)                                                         
         B     D1R10NX                                                          
*                                                                               
         USING CAPRECD,R6                                                       
D1R20    LA    R3,METHTAB          METHOD TABLE                                 
D1R20NX  LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   CAPKEY,SPACES                                                    
         MVI   CAPKTYP,CAPKTYPQ    PAYROLL HISTORY RECORD                       
         MVI   CAPKSUB,CAPKSUBQ                                                 
         MVC   CAPKCPY,CMPY        COMPANY                                      
         MVC   CAPKMTHD,0(R3)      CHECK FOR ALL METHODS                        
         MVC   CAPKOFC,DTOFFICE                                                 
         MVC   CAPKDPT,DTDEPT                                                   
         MVC   CAPKSDT,DTSUBDPT                                                 
         MVC   CAPKPER,PERSON                                                   
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    D1R30                                                            
D1R25    LA    R3,1(R3)            CHECK NEXT METHOD                            
         CLI   0(R3),X'00'                                                      
         BE    D1R40                                                            
         B     D1R20NX                                                          
*                                                                               
D1R30    XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY,KEYSAVE                                                   
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         OI    CAPRSTAT,X'80'      DELETE                                       
         LA    R6,BIGKEY                                                        
         OI    CAPKSTAT,X'80'      DELETE                                       
         GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
         B     D1R25                                                            
*                                                                               
         USING STDRECD,R6                                                       
D1R40    LA    R6,BIGKEY           CHECK FOR STD HOURS REC TO DELETE            
         XC    BIGKEY,BIGKEY                                                    
         MVC   STDKEY,SPACES                                                    
         MVI   STDKTYP,STDKTYPQ    PAYROLL HISTORY RECORD                       
         MVI   STDKSUB,STDKSUBQ                                                 
         MVC   STDKCPY,CMPY        COMPANY                                      
         MVC   STDKOFC,DTOFFICE                                                 
         MVC   STDKDPT,DTDEPT                                                   
         MVC   STDKSBD,DTSUBDPT                                                 
         MVC   STDKPER,PERSON                                                   
         MVI   STDKSEQ,X'00'                                                    
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   D1RX                                                             
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY,KEYSAVE                                                   
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         OI    STDRSTAT,X'80'      DELETE                                       
         LA    R6,BIGKEY                                                        
         OI    STDKSTAT,X'80'      DELETE                                       
         GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
D1RX     MVC   AIO,AIO1                                                         
         B     XYES                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE AND UPDATE DATES ON SELECTED LINE                             
***********************************************************************         
*                                                                               
DATEUPD  NMOD1 0,*DATEUP*                                                       
         L     RC,SAVERC                                                        
*                                                                               
PREV     USING DISTABD,R3          R3 = PREVIOUS LOC (CHRONOLOGICALLY)          
UPD      USING DISTABD,R4          R4 = LOCATION BEING UPDATED                  
NXT      USING DISTABD,R6          R6 = NEXT LOC (CHRONOLOGICALLY)              
         USING LLINED,R2                                                        
         SR    R3,R3                                                            
         SR    R6,R6                                                            
         LA    R4,DTADDLOC         TOP OF TABLE                                 
         AH    R4,STDISP           ADD START DISPLACEMENT                       
         LA    R2,PEMSELH          FIRST LINE ON SCREEN                         
DATE10   OC    0(DTLENQ,R4),0(R4)  ANY ENTRY IN TABLE                           
         BZ    DATE10NX                                                         
         TM    UPD.DTLSTAT,DTSUPD      UPDATE LOCATION DATE                     
         BO    DATE20                                                           
         LR    R6,R4               R6 = NEXT LOCATION (CHRONOLOGICALLY)         
DATE10NX LA    R4,DTLENQ(R4)       NEXT TABLE ENTRY                             
         LA    R1,DISTABND         END OF TABLE                                 
         CR    R4,R1                                                            
         BNL   DATEX                                                            
         LA    R2,LLLEN(R2)        NEXT SCREEN LINE                             
         LA    R1,PEMENDH          LAST SCREEN LINE                             
         CR    R2,R1                                                            
         BNH   DATE10                                                           
         B     DATEX                                                            
*                                                                               
DATE20   ST    R4,FULL                                                          
         LA    R4,DTLENQ(R4)       NEXT TABLE ENTRY                             
         LA    R1,DISTABND         END OF TABLE                                 
         CR    R4,R1                                                            
         BH    DATE22                                                           
         OC    0(DTLENQ,R4),0(R4)  ANY ENTRY IN TABLE                           
         BZ    DATE22                                                           
         LR    R3,R4               R3 = PREVIOUS LOCATION IN TIME               
DATE22   L     R4,FULL             RESTORE R4 TO LINE BEING UPDATED             
         EJECT                                                                  
***********************************************************************         
*        VALIDATE NEW START AND END DATES                                       
***********************************************************************         
*                                                                               
         XC    NEWSTDT,NEWSTDT     VALIDATE NEW START AND END DATES             
         XC    NEWENDT,NEWENDT                                                  
         XC    PREVENDT,PREVENDT   START-1                                      
         XC    NEXTSTDT,NEXTSTDT   END+1                                        
*                                                                               
         XC    DMCB,DMCB                                                        
         LA    R1,LLDATE           START DATE                                   
         ST    R1,DMCB                                                          
         LLC   R1,LLDATEH+5                                                     
         STC   R1,DMCB                                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,X'04'                                                     
         BE    *+12                                                             
         LA    R2,LLDATEH                                                       
         B     EINVDATE                                                         
         LA    R1,BLOCK                                                         
         USING PERVALD,R1                                                       
         MVC   NEWSTDT,PVALPSTA    PWOS                                         
*                                                                               
         CLC   NEWSTDT,HIREDT                                                   
         BNL   *+12                                                             
         LA    R2,LLDATEH                                                       
         B     ESTHIRE             DATE BEFORE HIRE DATE                        
*                                                                               
         OC    TERMDT,TERMDT                                                    
         BZ    DATE24                                                           
         CLC   NEWSTDT,TERMDT                                                   
         BL    DATE24                                                           
         LA    R2,LLDATEH                                                       
         B     EDTTERM             DATE AFTER TERM DATE                         
*                                                                               
DATE24   MVC   YYMMDD1,PVALESTA    EBDIDIC                                      
         DROP  R1                                                               
         GOTO1 ADDAY,DMCB,YYMMDD1,YYMMDD2,F'-1'                                 
         GOTO1 DATCON,DMCB,(0,YYMMDD2),(1,PREVENDT) START -1 = PREV             
*                                                                               
         LTR   R3,R3               IS THERE A PREVIOUS LOCATION                 
         BZ    DATE29                                                           
         CLC   PREVENDT,PREV.DTSTDATE   NEW PREV END AFTER PREV START           
         BNL   DATE29                                                           
         LA    R2,LLDATEH                                                       
         B     EBFPREV             DATE BEFORE PREVIOUS LOCATION START          
*                                                                               
DATE29   CLI   LLENDTH+5,0         ANY END DATE                                 
         BNE   *+10                                                             
         LTR   R6,R6               IS THERE A NEXT LOCATION                     
         BZ    DATE40                                                           
*                                                                               
         TM    LLENDTH+4,X'20'             VALIDATED PREVIOUSLY?                
         BZ    DATE30              NO                                           
         LA    R1,LLENDT           END DATE                                     
         ST    R1,DMCB                                                          
         LA    R1,8                LENGTH                                       
         STC   R1,DMCB                                                          
         B     DATE30A                                                          
*                                                                               
DATE30   XC    DMCB,DMCB                                                        
         LA    R1,LLENDT           END DATE                                     
         ST    R1,DMCB                                                          
         LLC   R1,LLENDTH+5                                                     
         STC   R1,DMCB                                                          
DATE30A  MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'60'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,BLOCK)                                        
         CLI   DMCB+4,X'04'                                                     
         BE    *+12                                                             
         LA    R2,LLENDTH                                                       
         B     EINVDATE                                                         
         LA    R1,BLOCK                                                         
         USING PERVALD,R1                                                       
         MVC   NEWENDT,PVALPSTA    PWOS                                         
*                                                                               
DATE31   OC    TERMDT,TERMDT                                                    
         BZ    DATE32                                                           
         CLC   NEWENDT,TERMDT                                                   
         BNH   DATE32                                                           
         LA    R2,LLENDTH                                                       
         B     EDTTERM             DATE AFTER TERM DATE                         
*                                                                               
DATE32   MVC   YYMMDD1,PVALESTA    EBDIDIC                                      
         DROP  R1                                                               
         GOTO1 ADDAY,DMCB,YYMMDD1,YYMMDD2,F'1'                                  
         GOTO1 DATCON,DMCB,(0,YYMMDD2),(1,NEXTSTDT) END +1 =NEXT START          
*                                                                               
         OC    TERMDT,TERMDT                                                    
         BZ    DATE35                                                           
         CLC   NEXTSTDT,TERMDT                                                  
         BL    DATE35                                                           
         LA    R2,LLENDTH                                                       
         B     EDTTERM             DATE AFTER TERM DATE                         
*                                                                               
DATE35   LTR   R6,R6             IS THERE A NEXT LOCATION (CHRONOLOG.)          
         BZ    DATE40            NO - NOTHING TO CHECK                          
         OC    NXT.DTENDATE,NXT.DTENDATE -IS THERE AN END DATE (ACTIVE)         
         BZ    DATE40                                                           
         CLC   NEXTSTDT,NXT.DTENDATE  NEW END DATE < NEXT END DATE              
         BNH   DATE40                                                           
         LA    R2,LLENDTH             POSITION CURSOR IN MISSING FIELD          
         B     EBFNEXT  - DATE AFTER NEXT LOCATION'S END                        
         EJECT                                                                  
***********************************************************************         
*        MAKE SURE NO SAL/TIME RECS EXIST OUTSIDE RESPECTIVE PERIODS            
***********************************************************************         
*                                                                               
DATE40   LTR   R3,R3                IS THERE A PREVIOUS LOC LINE?               
         BNZ   *+14                                                             
         MVC   STDATE,=3X'FF'                                                   
         B     *+10                                                             
         MVC   STDATE,PREV.DTENDATE  BETWEEN PREVIOUS END DATE                  
         MVC   ENDATE,NEWSTDT        AND NEW START DATE                         
         MVC   TEMPOFF,UPD.DTOFFICE                                             
         MVC   TEMPDEPT,UPD.DTDEPT                                              
         MVC   TEMPSDPT,UPD.DTSUBDPT                                            
         MVC   ACCNT,UPD.DTACCNT                                                
         MVI   EXPBYTE,EXPBYTES                                                 
         GOTO1 =A(CHKEXP),DMCB,RR=RELO                                          
         BNE   ERREXPC                                                          
         MVC   STDATE,NEWENDT        BETWEEN NEW END DATE                       
         MVC   ENDATE,NXT.DTSTDATE   AND NEXT START DATE                        
         MVC   TEMPOFF,NXT.DTOFFICE                                             
         MVC   TEMPDEPT,NXT.DTDEPT                                              
         MVC   TEMPSDPT,NXT.DTSUBDPT                                            
         MVC   ACCNT,NXT.DTACCNT                                                
         MVI   EXPBYTE,EXPBYTEE                                                 
         GOTO1 =A(CHKEXP),DMCB,RR=RELO                                          
         BNE   ERREXPC                                                          
*                                                                               
         DS    0H                                                               
         OC    NEWENDT,NEWENDT                                                  
         BZ    DATE45                                                           
         CLC   NEWSTDT,NEWENDT     ALSO MAKE SURE NEW ENDATE IS >               
         BL    *+12                NEW STDATE                                   
         LA    R2,LLENDTH                                                       
         B     ESTEND                                                           
*                                                                               
DATE45   OI    CHKSTAT,CHKSSAL+CHKSTIME                                         
         CLC   NEWSTDT,UPD.DTSTDATE                                             
         BL    DATE60                   NEW START < OLD START DATE              
         BH    DATE70                   NEW START > OLD START DATE              
*                                                                               
DATE50   OI    CHKSTAT,CHKSSAL+CHKSTIME                                         
         OC    NEWENDT,NEWENDT          NO END DATE                             
         BZ    DATE100                                                          
         CLC   NEWENDT,UPD.DTENDATE                                             
         BL    DATE80                   NEW END < OLD END                       
         BH    DATE90                   NEW END > OLD END                       
         B     DATE95                   =                                       
*                                                                               
DATE60   LTR   R3,R3                    IS THERE A PREVIOUS LOCATION            
         BZ    DATE50                   NO, THEN NOTHING TO CHECK               
*                                                                               
         MVC   TEMPOFF,PREV.DTOFFICE    NEW START < OLD                         
         MVC   TEMPDEPT,PREV.DTDEPT     NO RECS FOR PREVIOUS LOCATION           
         MVC   TEMPSDPT,PREV.DTSUBDPT                                           
         MVC   ACCNT,PREV.DTACCNT                                               
         MVC   STDATE,NEWSTDT           BETWEEN NEW START DATE                  
         MVC   ENDATE,UPD.DTSTDATE      AND OLD START DATE (UPD LINE)           
         BAS   RE,CHKRECS               CHECK FOR SAL/TIME RECS                 
*                                                                               
*                                  NOW CHK FOR TIME PAST THE NEW END DT         
         XC    ENDATE,ENDATE                                                    
         BAS   RE,SETDATES                                                      
         BAS   RE,CHKRECS               CHECK FOR SAL/TIME RECS                 
         B     DATE50                   GO CHECK END DATE                       
*                                                                               
DATE70   MVC   TEMPOFF,UPD.DTOFFICE     NEW START > OLD                         
         MVC   TEMPDEPT,UPD.DTDEPT      NO RECS FOR UPDATING LOCATION           
         MVC   TEMPSDPT,UPD.DTSUBDPT                                            
         MVC   ACCNT,UPD.DTACCNT                                                
         MVC   STDATE,UPD.DTSTDATE      BETWEEN OLD START                       
         MVC   ENDATE,PREVENDT          AND NEW START -1 DAY                    
         BAS   RE,CHKRECS               CHECK FOR SAL/TIME RECS                 
         B     DATE50                   GO CHECK END DATE                       
*                                                                               
DATE80   OC    UPD.DTSALKDT,UPD.DTSALKDT     IS THERE A SAL LOCK                
         BZ    *+8                      YES, THEN ANYTHING IS VALID             
         NI    CHKSTAT,X'FF'-CHKSSAL                                            
         OC    UPD.DTTSLKDT,UPD.DTTSLKDT     TIMESHEET LOCK DATE                
         BZ    *+8                      YES, THEN ANYTHING IS VALID             
         NI    CHKSTAT,X'FF'-CHKSTIME                                           
*                                                                               
         MVC   TEMPOFF,UPD.DTOFFICE     NEW END < OLD                           
         MVC   TEMPDEPT,UPD.DTDEPT      NO RECS FOR UPDATING LOCATION           
         MVC   TEMPSDPT,UPD.DTSUBDPT                                            
         MVC   ACCNT,UPD.DTACCNT                                                
         MVC   STDATE,NEXTSTDT          BETWEEN NEW END +1 DAY                  
         MVC   ENDATE,UPD.DTENDATE      AND OLD END                             
         BAS   RE,CHKRECS               CHECK FOR SAL/TIME RECS                 
         B     DATE95                   DONE                                    
*                                                                               
DATE90   LTR   R6,R6                    IS THERE A NEXT LOCATION                
         BZ    DATE100                  NO, THEN NOTHING TO CHECK               
         MVC   TEMPOFF,NXT.DTOFFICE     NEW END > OLD                           
         MVC   TEMPDEPT,NXT.DTDEPT      NO RECS FOR NEXT LOCATION               
         MVC   TEMPSDPT,NXT.DTSUBDPT                                            
         MVC   ACCNT,NXT.DTACCNT                                                
         MVC   STDATE,UPD.DTENDATE      BETWEEN OLD END                         
         MVC   ENDATE,NEWENDT           AND NEW END                             
         BAS   RE,CHKRECS               CHECK FOR SAL/TIME RECS                 
         B     DATE95                   DONE                                    
*                                                                               
                                                                                
DATE95   MVC   TEMPDATE,PREVENDT             NEW START -1                       
         GOTO1 =A(READCAL),DMCB,RR=RELO      GET PERIOD END DATE                
         BNE   DATE100                                                          
         CLC   TEMPDATE,ENDATE               COMPARE PERIOD WITH PREV           
         BE    DATE100                       END DATE                           
         XC    STDATE,STDATE                                                    
         LTR   R3,R3                                                            
         BZ    *+10                                                             
         MVC   STDATE(L'DTENDATE),PREV.DTENDATE                                 
         GOTO1 =A(CHKTIME),DMCB,RR=RELO                                         
         BNE   EDPOST              CAN'T CHANGE - POSTINGS EXIST                
         EJECT                                                                  
***********************************************************************         
*        PASSED CHECKS - REPLACE DATES IF NECESSARY                             
***********************************************************************         
*                                                                               
DATE100  MVC   UPD.DTSTDATE,NEWSTDT     START DATE                              
         MVC   UPD.DTENDATE,NEWENDT     END DATE                                
         LTR   R3,R3                    IS THERE A PREVIOUS LOCATION            
         BZ    DATE110                  NO, THEN NOTHING TO CHECK               
         CLC   PREVENDT,PREV.DTENDATE   IS NEW PREV END DATE OK                 
         BNL   DATE110                  >= IS OK                                
         MVC   PREV.DTENDATE,PREVENDT   ELSE, REPLACE                           
DATE110  LTR   R6,R6                    IS THERE A NEXT LOCATION                
         BZ    DATE120                  NO, THEN NOTHING TO CHECK               
         CLC   NEXTSTDT,NXT.DTSTDATE    IS NEW NEXT START DATE OK               
         BNH   DATE120                  <= IS OK                                
         MVC   NXT.DTSTDATE,NEXTSTDT    ELSE, REPLACE                           
DATE120  OI    BIT2,REBUILD                                                     
*                                                                               
DATEX    XIT1                                                                   
         DROP  R2,PREV,UPD,NXT                                                  
         EJECT                                                                  
***********************************************************************         
*        ERROR MESSAGES                                                         
***********************************************************************         
*                                  GENERAL MESSAGES                             
EBFPREV  MVC   GERROR,=AL2(ACEDBPRE)   DATE BEFORE PREV LOC'S START             
         J     ACCERRX                                                          
EBFNEXT  MVC   GERROR,=AL2(ACEDBNEX)   DATE AFTER NEXT LOC'S END                
         J     ACCERRX                                                          
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET UP DATES TO PASS TO CHKRECS ROUTINE                                       
* WORK WILL CONTAIN THE LOCATION END DATE OR SAL LOCK DATE                      
* WORK+10 WILL CONTAIN THE LOCATION END DATE OR TMS LOCK DATE                   
***********************************************************************         
         SPACE 1                                                                
PREV     USING DISTABD,R3          R3 = PREVIOUS LOC (CHRONOLOGICALLY)          
UPD      USING DISTABD,R4          R4 = LOCATION BEING UPDATED                  
NXT      USING DISTABD,R6          R6 = NEXT LOC (CHRONOLOGICALLY)              
SETDATES NTR1                                                                   
         OI    CHKSTAT,CHKSSAL+CHKSTIME                                         
         XC    WORK,WORK                                                        
         MVC   WORK(L'DTENDATE),UPD.DTENDATE  MOVE IN LOC END DTE (OLD)         
         OC    PREV.DTSALKDT,PREV.DTSALKDT     IS THERE A SAL LOCK              
         BZ    *+10                     YES, THEN USE THAT INSTEAD              
         MVC   WORK(L'DTSALKDT),PREV.DTSALKDT                                   
         MVC   WORK+10(L'DTENDATE),UPD.DTENDATE                                 
         OC    PREV.DTTSLKDT,PREV.DTTSLKDT     IS THERE A T/S LOC DATE          
         BZ    *+10                     YES, THEN USE THAT INSTEAD              
         MVC   WORK+10(L'DTTSLKDT),PREV.DTTSLKDT                                
         B     XIT                                                              
         DROP  PREV,UPD,NXT                                                     
         SPACE 2                                                                
***********************************************************************         
*        CHECKS FOR RECORDS                                                     
*        WORK CONTAINS THE LOCATION END DATE OR SALARY LOCK DATE                
*        WORK+10 CONTAINS THE LOCATION END DATE OR TMS LOCK DATE                
***********************************************************************         
*                                                                               
CHKRECS  NTR1                                                                   
         TM    CHKSTAT,CHKSSAL     CHECK FOR SALARY RECS                        
         BNO   CHK10                                                            
         GOTO1 =A(CHKSAL),DMCB,RR=RELO                                          
         BNE   EDSALS              CAN'T CHANGE - SALARY EXISTS                 
CHK10    TM    CHKSTAT,CHKSTIME    CHECK FOR SALARY RECS                        
         BNO   CHKX                                                             
         GOTO1 =A(CHKTIME),DMCB,RR=RELO                                         
         BNE   EDPOST              CAN'T CHANGE - POSTINGS EXIST                
         TM    BIT2,NEWTIME                                                     
         BZ    CHK20                                                            
         GOTO1 =A(CHKSVTMS),DMCB,RR=RELO                                        
         BNE   ERRSVTMS                                                         
CHK20    TM    BIT2,NEWTIME        ON TMS?                                      
         BO    CHKX                                                             
         GOTO1 =A(DRAFTRNS),DMCB,RR=RELO CHECK FOR SAVED TMS RECS               
         BE    ERRDRAFT                                                         
CHKX     NI    CHKSTAT,X'FF'-CHKSSAL                                            
         NI    CHKSTAT,X'FF'-CHKSTIME                                           
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CHECKS FOR ANY SALARY FROM STDATE TO ENDATE                            
*        USING LOCATION IN TEMPOFF, TEMPDEPT, AND TEMPSDPT                      
***********************************************************************         
*                                                                               
CHKSAL   NMOD1 0,*CHKSAL*                                                       
         L     RC,SAVERC                                                        
         NI    BIT2,X'FF'-ADJRATE                                               
         NI    CHKSTAT2,X'FF'-MOALOCK                                           
         XC    BYTE,BYTE           HOLDS COUNT OF RECS FOUND SO FAR             
*                                                                               
         OC    STDATE,STDATE                                                    
         BNZ   CS01                NO DATES                                     
         OC    ENDATE,ENDATE                                                    
         BZ    CSYES                                                            
*                                                                               
         USING PHIRECD,R6                                                       
CS01     LA    R6,KEY2             CHECK FOR ANY SALARY AT LOCATION             
         XC    KEY2,KEY2                                                        
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ    X'3E05' PAYROLL HISTORY RECORD               
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,CMPY        COMPANY                                      
         MVC   PHIKOFC,TEMPOFF                                                  
         MVC   PHIKDPT,TEMPDEPT                                                 
         MVC   PHIKSBD,TEMPSDPT                                                 
         MVC   PHIKPER,PERSON                                                   
         MVC   PHIKMOA,=X'0000'    GET MOST CURRENT                             
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         B     CS10                                                             
CS10SEQ  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCDIR ',KEY2,KEY2                    
CS10     CLC   KEY2(PHIKMOA-PHIKEY),KEYSAVE                                     
         BNE   CS30                                                             
*                                                                               
         LA    R6,KEY2                                                          
         ZICM  R1,PHIKMOA,2        MOA                                          
         LNR   R1,R1                                                            
         STH   R1,WORK                                                          
         CLC   WORK(2),STDATE      BEFORE START DATE IS OK                      
         BL    CS30                                                             
         OC    ENDATE,ENDATE       NO END DATE                                  
         BZ    CS14                                                             
         CLC   WORK(2),ENDATE      AFTER END DATE, GET NEXT                     
         BH    CS10SEQ                                                          
*                                                                               
CS14     LA    R2,KEY2             GET RECORD                                   
         AH    R2,LKEY                                                          
         AH    R2,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST ',(R2),AIO2,WORK               
*                                                                               
         USING PDEELD,R6                                                        
         L     R6,AIO2             CHECK DATES                                  
         MVI   ELCODE,PDEELQ       X'86' PAYROLL DETAIL ELEM                    
         BAS   RE,GETEL                                                         
         B     CS16                                                             
CS16NX   BAS   RE,NEXTEL                                                        
CS16     BNE   CS10SEQ                                                          
*                                                                               
         TM    CHKSTAT,DELELOC                                                  
         BO    *+14                                                             
         CLC   PDEDTE,STDATE       IS DATE WITHIN PERIOD                        
         B     *+10                                                             
         CLC   PDEDTE(2),STDATE    IS DATE WITHIN PERIOD                        
         BL    CS16NX                                                           
         OC    ENDATE,ENDATE       NO END DATE                                  
         BZ    CS18                CHECK FOR $$                                 
         CLC   PDEDTE,ENDATE                                                    
         BH    CS16NX                                                           
*                                                                               
CS18     ZAP   WORK(8),PDEAMT                                                   
         AP    WORK(8),PDEADJ                                                   
         CP    WORK(8),=P'0'                                                    
         BE    CS16NX                                                           
         TM    BIT3,DUPLOC         IS THERE A DUP LOC BEING DEL                 
         BZ    CS20                                                             
         OC    TEMPDATE,TEMPDATE   TEMPDATE=SAL LOCK DTE OF DUP LOC             
         BZ    CS20                CHECK IF SAL LOCK DATE COVERS SAL            
         CLC   TEMPDATE,PDEDTE     FOUND FOR LOC BEING DELETED                  
         BNL   CS16NX                                                           
*                                                                               
CS20     MVC   TEMPDATE(2),PDEDTE  SET UP DATE FOR MONVAL CALL                  
         BAS   RE,MONOPEN          CHECK IF MOA IS OPEN                         
         BE    CS22                                                             
         CLC   GERROR,=AL2(ACESPAN) IF THE ERROR IS 'DATE OUTSIDE               
         BE    *+8                  PERMITTED SPAN' IGNORE IT                   
         OI    CHKSTAT2,MOALOCK                                                 
* RE-ESTABLISH KEY                                                              
CS22     GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCDIR ',KEY2,KEY2                    
         TM    PDESTAT2,PDESADJ    IS THERE AN ADJ RATE                         
         BZ    CS25                                                             
         OI    BIT2,ADJRATE                                                     
         B     CS16NX              DON'T COUNT AS REGULAR SALARY                
*                                                                               
CS25     LLC   R1,BYTE             INCREMENT COUNTER FOR RECS FOUND             
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         B     CS10SEQ             CONTINUE IF =<                               
*                                                                               
CS30     OC    BYTE,BYTE           DID WE FIND ANY RECORDS W/ SALARY?           
         BZ    CSYES                                                            
*                                                                               
CSNO     B     XNO                 CAN'T DELETE - SALARY EXISTS                 
CSYES    B     XYES                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        IS MOA OPEN                                                            
*        NTRY - INDATE CONTAINS YYMM OF MOA                                     
***********************************************************************         
*                                                                               
MONOPEN  NTR1                                                                   
         MVI   TEMPDATE+2,X'01'   FINISH BUILDING INDATE                        
         GOTO1 DATCON,DMCB,(1,TEMPDATE),(10,WORK)                               
*&&US*&& GOTO1 VBMONVAL,DMCB,(8,WORK),(97,ACOMFACS),(0,BLOCK),(CMPY,0)          
*&&UK*&& GOTO1 VBMONVAL,DMCB,(8,WORK),(97,ACOMFACS),(0,BLOCK),         X        
               (CMPY,TWAACCS)                                                   
         LA    R1,BLOCK                                                         
         USING BMONVALD,R1                                                      
         CLI   BMOERR,BMOEOKQ      EVERYTHING OK?                               
         BE    XYES                                                             
         MVC   GERROR,BMOMSG       SET ERROR MESSAGE                            
         B     XNO                                                              
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* CHECK FOR OUTSTANDING EXPENSES                                      *         
**********************************************************************          
CHKEXP   NMOD1 0,**CHKEXP**                                                     
         L     RC,SAVERC                                                        
         GOTO1 DATCON,DMCB,(1,STDATE),(2,TWOSTA)                                
         GOTO1 DATCON,DMCB,(1,ENDATE),(2,TWOEND)                                
         XR    R1,R1                                                            
         ICM   R1,3,TWOSTA         GET TWOS COMP OF START AND END DATES         
         LNR   R1,R1                                                            
         STCM  R1,3,TWOSTA                                                      
         XR    R1,R1                                                            
         ICM   R1,3,TWOEND                                                      
         LNR   R1,R1                                                            
         STCM  R1,3,TWOEND                                                      
*                                                                               
         USING EXCRECD,R2                                                       
         XC    KEY2,KEY2                                                        
         LA    R2,KEY2                                                          
         XC    EXCKEY,EXCKEY                                                    
         MVI   EXCKTYP,EXCKTYPQ                                                 
         MVI   EXCKSUB,EXCKSUBQ                                                 
         MVC   EXCKCPY,CMPY                                                     
         MVC   EXCKPIDB,PIDNUM                                                  
         MVC   EXCKDATE,TWOEND                                                  
         CLC   EXCKDATE,TWOSTA     SET LOWER OF TWO DATES                       
         BL    *+10                                                             
         MVC   EXCKDATE,TWOSTA                                                  
         MVC   KEYSAVE,EXCKEY                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR',EXCKEY,EXCKEY                 
         BE    CE02                                                             
*                                                                               
CE01     GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCDIR',EXCKEY,EXCKEY                 
*                                                                               
CE02     CLC   EXCKEY(EXCKPIDB-EXCRECD),KEYSAVE                                 
         BNE   XYES                                                             
         CLC   EXCKPIDB,PIDNUM     FIRST CLAIM FOUND FOR DIFFERENT PID?         
         BNE   XYES                                                             
         TM    EXCKSTAT,EXCSCOMP   POSTED ALSO OK                               
         BNZ   CE01                                                             
         TM    EXCKSTAT,EXCSLOGD   LOGICALLY DELETED OK TOO                     
         BNZ   CE01                                                             
*                                                                               
         CLC   EXCKDATE,TWOSTA     COMPARE EXP DATE TO START DATE               
         BL    CE05                AFTER START DATE?                            
         BE    CE03                SAME AS START DATE                           
         CLC   TWOSTA,TWOEND       FOR BEFORE START, CHECK THAT START           
         BH    XYES                IS BEFORE END                                
*                                                                               
CE03     CLI   EXPBYTE,EXPBYTEN    IS IT OKAY TO BE THE SAME AS START?          
         BE    XNO                                                              
         CLC   TWOSTA,TWOEND       COMPARE START TO END DATE                    
         BH    CE04                START BEFORE END?                            
         CLC   EXCKDATE,TWOSTA                                                  
         BNE   CE04                                                             
         CLI   EXPBYTE,EXPBYTES    HAS START DATE BEEN CHANGED?                 
         BE    CE06                                                             
*                                                                               
CE04     CLC   EXCK1RAC,ACCNT      CHECK THAT LOCATION OF EXPENSE               
         BNE   XYES                IS NOT THE SAME AS LOCATION AT END           
         B     XNO                                                              
*                                                                               
CE05     CLI   EXPBYTE,EXPBYTEN    IS IT OKAY FOR EXPENSE TO EXIST              
         BE    XNO                 AFTER START DATE?                            
         CLC   EXCKDATE,TWOEND     COMPARE EXP DATE TO END DATE                 
         BH    XNO                 BEFORE END? NOT OKAY                         
         CLC   EXCK1RAC,ACCNT                                                   
         BNE   XNO                                                              
         B     CE01                READ FOR NEXT EXPENSE                        
*                                                                               
CE06     CLC   EXCK1RAC,ACCNT                                                   
         BNE   XNO                                                              
         B     XYES                                                             
         EJECT                                                                  
**********************************************************************          
*        READCAL GETS CALENDER RECORD PERIOD END                                
**********************************************************************          
READCAL  NMOD1 0,**READCAL**                                                    
         L     RC,SAVERC                                                        
GT1      XR    RE,RE                                                            
         ICM   RE,1,COMPFST                                                     
*&&US                                                                           
         JNZ   *+8                 CHECK TO SEE IF FISCAL START SET             
         AHI   RE,X'F1'               IF NOT, SET IT TO JAN                     
*&&                                                                             
         CHI   RE,X'F0'                                                         
         BH    *+8                                                              
         AHI   RE,X'F0'-X'C0'+X'09'                                             
         SHI   RE,X'F0'                                                         
         STC   RE,BYTE1                                                         
         CLI   BYTE1,X'0A'         CONVERT FISCAL START TO ACTUAL MONTH         
         BL    GT1A                                                             
         CLI   BYTE1,X'0A'         0A =====> 10                                 
         BNE   *+12                                                             
         MVI   BYTE1,X'10'                                                      
         B     GT1A                                                             
         CLI   BYTE1,X'0B'         0B =====> 11                                 
         BNE   *+12                                                             
         MVI   BYTE1,X'11'                                                      
         B     GT1A                                                             
         CLI   BYTE1,X'0C'         0C =====> 12                                 
         BNE   *+8                                                              
         MVI   BYTE1,X'12'                                                      
*                                                                               
GT1A     MVC   TS#CDAT,TEMPDATE                                                 
         MVC   TS#CDAT+1(1),BYTE1                                               
*                                                                               
         CLC   TEMPDATE+1(1),BYTE1                                              
         BNL   GT2                                                              
GT1B     XR    R1,R1                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(1,TS#CDAT),(0,WORK)                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,F'-1'                              
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,TS#CDAT)                               
         B     GT2                                                              
*                                                                               
GT1C     GOTO1 DATCON,DMCB,(1,TS#CDAT),(0,WORK)                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,F'1'                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,TS#CDAT)                               
*                                                                               
GT2      GOTO1 DATCON,DMCB,(1,TS#CDAT),(0,WORK)                                 
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,F'11'                              
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,TS#CEND)                               
         GOTO1 =A(RECCAL),DMCB,RR=RELO                                          
         BNE   XNO                                                              
         MVI   BYTE1,0             INIT FOR CALENDAR READ.                      
         USING CASRECD,R2                                                       
         USING TMRELD,R3                                                        
GT3      L     R2,AIO2                                                          
         LA    R3,CASRFST                                                       
         XR    R0,R0                                                            
                                                                                
GT4      CLI   TMREL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   TMREL,TMPELQ                                                     
         BE    GT6                                                              
         CLI   TMREL,TMRELQ                                                     
         BNE   GT5                                                              
         CLI   BYTE1,1                                                          
         BE    GT6                                                              
         CLC   TEMPDATE,TMRSTART                                                
         BL    GT1B                                                             
         CLC   TEMPDATE,TMREND                                                  
         BH    GT1C                                                             
         MVI   BYTE1,1                                                          
         B     GT6                                                              
                                                                                
GT5      IC    R0,TMRLN                                                         
         AR    R3,R0                                                            
         B     GT4                                                              
         DROP  R3                                                               
                                                                                
         USING TMPELD,R3                                                        
GT6      CLC   TEMPDATE,TMPEND                                                  
         BH    GT5                                                              
         CLC   TEMPDATE,TMPSTART                                                
         BL    GT5                                                              
         MVC   ENDATE,TMPEND                                                    
         MVC   STDATE,TMPSTART                                                  
         B     XYES                                                             
         XIT1                                                                   
*********************************************************************           
* RECCAL-GETS THE CALENDAR RECORD                                               
***********************************************************************         
RECCAL   NMOD1 0,**RECCAL**                                                     
         L     RC,SAVERC                                                        
         USING CASRECD,R2                                                       
         XC    KEY2,KEY2                                                        
         LA    R2,KEY2                                                          
         XC    CASKEY,CASKEY                                                    
         MVI   CASKTYP,CASKTYPQ                                                 
         MVI   CASKSUB,CASKSUBQ                                                 
         MVC   CASKCPY,CMPY                                                     
         MVC   CASKEMOA,TS#CEND                                                 
         MVC   CASKSMOA,TS#CDAT                                                 
         MVC   CASKOFC,TEMPOFF                                                  
         MVC   KEYSAVE,CASKEY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCDIR',CASKEY,CASKEY                 
         BE    RECCAL2                                                          
*                                                                               
         MVC   CASKEY,KEYSAVE                                                   
         MVC   CASKOFC,SPACES                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCDIR',CASKEY,CASKEY                 
         BNE   XNO                 SET CC NOT EQUAL                             
*                                                                               
RECCAL2  DS    0H                  CALENDAR FOUND                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST ',CASKDA,AIO2,WORK             
         BE    XYES                SET CC EQUAL                                 
         DC    H'0'                FATAL ERROR                                  
*                                                                               
         DROP  RB,R2                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECKS FOR ANY TIME FROM STDATE TO ENDATE                              
*        USING LOCATION IN ACCNT                                                
***********************************************************************         
*                                                                               
CHKTIME  NMOD1 0,*CHKTIM*                                                       
         L     RC,SAVERC                                                        
*                                                                               
*&&US                                                                           
         TM    BIT2,NEWTIME                                                     
         BO    CT30                                                             
*                                                                               
         ZAP   COUNTP,=P'0'        CLEAR HOURS COUNTER                          
*                                                                               
         USING TTHRECD,R6                                                       
         LA    R6,KEY2                                                          
         XC    KEY2,KEY2                                                        
         MVI   TTHKTYP,TTHKTYPQ    X'3E'                                        
         MVI   TTHKSUB,TTHKSUBQ    X'0E'                                        
         MVC   TTHKCPY,CMPY                                                     
         MVI   TTHKUNT,C'1'        UNIT 1                                       
         MVI   TTHKLDG,C'R'        LEDGER R                                     
         MVC   TTHKACT,ACCNT                                                    
         MVI   TTHKTIME,TTHKTTOT   READ FOR TOTAL TIME RECS                     
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         B     CT10                                                             
CTNXT    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCDIR ',KEY2,KEY2                    
CT10     CLC   KEY2(TTHKTIME-TTHKEY),KEYSAVE  CLC UP TO ACCNT ONLY              
         BNE   CT22                CHECK FOR HOURS NOW                          
         LA    R3,KEY2             GET RECORD                                   
         AH    R3,LKEY                                                          
         AH    R3,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST ',(R3),AIO2,WORK               
         L     R6,AIO2                                                          
*                                                                               
         USING PTHELD,R6                                                        
         AH    R6,DATADISP                                                      
CT12     CLI   0(R6),0                                                          
         BE    CTNXT                                                            
         CLI   0(R6),PTHELQ        X'8D'                                        
         BE    CT18                                                             
         CLI   0(R6),PTHELQA       X'8E'                                        
         BE    CT18                                                             
CT15     SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     CT12                                                             
CT18     LLC   R1,PTHLN                                                         
         SH    R1,=Y(PTHBLNQ)                                                   
         SRL   R1,3                DIVIDE BY 8 TO GET # OF SUBELEMS             
*                                                                               
         CLC   PTHEDT,STDATE       BEFORE START DATE IS OK                      
         BL    CT15                                                             
         OC    ENDATE,ENDATE       NO END DATE                                  
         BZ    CT20                CHECK FOR HOURS                              
         CLC   PTHEDT,ENDATE       AFTER END DATE, GET NEXT                     
         BH    CT15                                                             
*                                                                               
CT20     LA    RF,PTHMOA                                                        
         AP    COUNTP,PTHTHRS-PTHMOA(L'PTHTHRS,RF)    KEEP TRACK OF HRS         
         LA    RF,L'PTHSLNQ(RF)    BUMP TO NEXT SUBELEM                         
         BCT   R1,CT20                                                          
         B     CT15                                                             
*                                                                               
CT22     CP    COUNTP,=P'0'        ANY HOURS FOUND?                             
         BZ    CTYES               NO SO OKAY TO END LOC/DELETE LOC             
*                                                                               
CT25     TM    BIT3,DUPLOC         ELSE CHECK FOR DUPLICATE LOCATION            
         BZ    CTNO                                                             
         OC    TEMPDTE2,TEMPDTE2   TEMPDTE2=TMS LOCK DTE OF DUP LOC             
         BZ    CTNO                CHECK IF TMS LOCK DATE COVERS TIME           
         CLC   TEMPDTE2,PTHEDT     FOUND FOR LOC BEING DELETED                  
         BNL   CT15                                                             
         B     CTNO                CAN'T CHANGE - TIME EXISTS                   
*                                                                               
*&&                                                                             
         USING TSWRECD,R6                                                       
CT30     LA    R6,KEY2             CHECK FOR ANY TIME AT LOCATION               
         XC    KEY2,KEY2                                                        
         MVC   TSWKEY,SPACES                                                    
         MVI   TSWKTYP,TSWKTYPQ    X'3E0F' TIMESHEET WEEKLY POINTER             
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,CMPY        COMPANY                                      
         MVC   TSWKPER,PERSON                                                   
         XC    TSWKEND,TSWKEND                                                  
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         B     CT40                                                             
CT40SEQ  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCDIR ',KEY2,KEY2                    
CT40     CLC   KEY2(TSWKEND-TSWKEY),KEYSAVE                                     
         BNE   CTYES                                                            
         LA    R6,KEY2             CHECK FOR ANY TIME AT LOCATION               
         LLC   R1,LN1RLEV1                                                      
         LLC   R0,LN1RLEV2                                                      
         AR    R1,R0                                                            
         LLC   R0,LN1RLEV3                                                      
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TSWKODS(0),ACCNT       SAME LOCATION                             
         BNE   CT40SEQ                                                          
*                                                                               
         ZICM  R1,TSWKEND,3        PERIOD END DATE                              
         LNR   R1,R1                                                            
         STCM  R1,7,YYMMDD3                                                     
         CLC   YYMMDD3,STDATE      BEFORE START DATE IS OK                      
         BL    CT40SEQ                                                          
         TM    BIT3,MIDPER         IF LOOKING FOR MID PERIOD                    
         BNZ   CT48                SKIP CHECKING OF TIMELS                      
                                                                                
         LA    R3,KEY2                                                          
         AH    R3,LKEY                                                          
         AH    R3,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST',(R3),AIO2,WORK                
*                                                                               
         USING TIMELD,R6                                                        
         L     R6,AIO2                                                          
         AH    R6,DATADISP                                                      
CT42     CLI   TIMEL,0             EOR                                          
*&&UK*&& BE    CT40SEQ                                                          
*&&US*&& BE    CT50                                                             
         CLI   TIMEL,TIMELQ        TIMEL                                        
         BE    CT46                                                             
CT44     SR    R1,R1                                                            
         IC    R1,TIMLN                                                         
         AR    R6,R1                                                            
         B     CT42                                                             
*                                                                               
CT46     CLI   TIMETYP,TIMEXTRA    LOOKING FOR TEMPO TIMELS                     
         BNE   CT47                                                             
         CLC   TIMXTPDT,STDATE     BEFORE START DATE IS OK                      
         BL    CT44                                                             
         B     CT48                                                             
*                                                                               
CT47     CLI   TIMETYP,TIMETIME    OR ETIME TIMELS                              
         BNE   CT44                GET NEXT ELEMENT                             
         CLC   YYMMDD3,STDATE       BEFORE START DATE IS OK                     
         BL    CT44                                                             
*                                                                               
CT48     OC    ENDATE,ENDATE       NO END DATE                                  
         BZ    CT50                                                             
         CLC   YYMMDD3,ENDATE      AFTER END DATE, GET NEXT                     
         BH    CT40SEQ                                                          
*                                                                               
CT50     TM    BIT3,DUPLOC         IS THERE A DUP LOC BEING DEL                 
         BZ    CTNO                                                             
         OC    TEMPDTE2,TEMPDTE2   TEMPDTE2=TMS LOCK DTE OF DUP LOC             
         BZ    CTNO                CHECK IF TMS LOCK DATE COVERS TIME           
         CLC   TEMPDTE2,YYMMDD3    FOUND FOR LOC BEING DELETED                  
         BNL   CTYES                                                            
CTNO     NI    BIT3,X'FF'-MIDPER                                                
         B     XNO                                                              
CTYES    NI    BIT3,X'FF'-MIDPER                                                
         B     XYES                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECKS FOR ANY SAVED TIME FROM STDATE TO ENDATE                        
*        USING LOCATION IN ACCNT                                                
***********************************************************************         
*                                                                               
CHKSVTMS NMOD1 0,*CHKSVTM                                                       
         L     RC,SAVERC                                                        
*                                                                               
         USING TSSRECD,R6                                                       
         LA    R6,KEY2                                                          
         XC    KEY2,KEY2                                                        
         MVC   TSSKEY,SPACES                                                    
         MVI   TSSKTYP,TSSKTYPQ    X'3E11' SAVED TMS RECORDS                    
         MVI   TSSKSUB,TSSKSUBQ                                                 
         MVC   TSSKCPY,CMPY        COMPANY                                      
         MVC   TSSKPER,PERSON                                                   
         XC    TSSKEND,TSSKEND                                                  
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         B     CTS10                                                            
CTS10SQ  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCDIR ',KEY2,KEY2                    
CTS10    CLC   KEY2(TSSKEND-TSSKEY),KEYSAVE                                     
         BNE   CTSYES                                                           
         LA    R6,KEY2                                                          
         LLC   R1,LN1RLEV1                                                      
         LLC   R0,LN1RLEV2                                                      
         AR    R1,R0                                                            
         LLC   R0,LN1RLEV3                                                      
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TSSKODS(0),ACCNT       SAME LOCATION                             
         BNE   CTS10SQ                                                          
*                                                                               
         ZICM  R1,TSSKEND,3        PERIOD END DATE                              
         LNR   R1,R1                                                            
         STCM  R1,7,YYMMDD3                                                     
         CLC   YYMMDD3,STDATE      BEFORE START DATE IS OK                      
         BL    CTS10SQ                                                          
         TM    BIT3,MIDPER         LOOKING FOR MID PERIOD ERRORS                
         BNZ   CTS18               SKIP LOOKING AT ELEMENTS                     
*                                                                               
         LA    R3,KEY2                                                          
         AH    R3,LKEY                                                          
         AH    R3,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST',(R3),AIO2,WORK                
*                                                                               
         USING TIMELD,R6                                                        
         L     R6,AIO2                                                          
         AH    R6,DATADISP                                                      
CTS12    CLI   TIMEL,0             EOR                                          
         BE    CTS10SQ                                                          
         CLI   TIMEL,TIMELQ        TIMEL                                        
         BE    CTS16                                                            
CTS14    SR    R1,R1                                                            
         IC    R1,TIMLN                                                         
         AR    R6,R1                                                            
         B     CTS12                                                            
*                                                                               
CTS16    CLI   TIMETYP,TIMEXTRA    LOOKING FOR TEMPO TIMELS                     
         BNE   CTS17                                                            
         CLC   TIMXTPDT,STDATE     BEFORE START DATE IS OK                      
         BL    CTS14                                                            
         B     CTS18                                                            
*                                                                               
CTS17    CLI   TIMETYP,TIMETIME    OR ETIME TIMELS                              
         BNE   CTS14               GET NEXT ELEMENT                             
         CLC   TIMETPDT,STDATE     BEFORE START DATE IS OK                      
         BL    CTS14                                                            
*                                                                               
CTS18    OC    ENDATE,ENDATE       NO END DATE                                  
         BZ    CTS20                                                            
         CLC   YYMMDD3,ENDATE      AFTER END DATE, GET NEXT                     
         BH    CTS10SQ                                                          
*                                                                               
CTS20    TM    BIT3,DUPLOC         IS THERE A DUP LOC BEING DEL                 
         BZ    CTSNO                                                            
         OC    TEMPDTE2,TEMPDTE2   TEMPDTE2=TMS LOCK DTE OF DUP LOC             
         BZ    CTSNO               CHECK IF TMS LOCK DATE COVERS TIME           
         CLC   TEMPDTE2,YYMMDD3    FOUND FOR LOC BEING DELETED                  
         BNL   CTSYES                                                           
CTSNO    NI    BIT3,X'FF'-MIDPER                                                
         B     XNO                                                              
CTSYES   NI    BIT3,X'FF'-MIDPER                                                
         B     XYES                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CHECK IF ANY TEMPO XREF RECORDS EXIST FOR THIS PERSON                  
*        OR ANY AUDRECDS EXIST FOR PERSON (OLDPID)                              
***********************************************************************         
*                                                                               
         USING TSXRECD,R6                                                       
XREFCHK  NMOD1 0,*XREFCHK                                                       
         L     RC,SAVERC                                                        
*                                                                               
         LA    R6,KEY2                                                          
         XC    KEY2,KEY2                                                        
         MVI   TSXKTYP,TSXKTYPQ    X'3E'                                        
         MVI   TSXKSUB,TSXKSUBQ    X'13'                                        
         MVC   TSXKCPY,CMPY        COMPANY                                      
         MVC   TSXKPER,PERSON      PERSON CODE                                  
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         B     XREF10                                                           
XREF05   GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCDIR ',KEY2,KEY2                    
XREF10   CLC   KEY2(TSXKEND-TSXKEY),KEYSAVE CLC UP TO PERSON                    
         BNE   XREF20              NO TIME EXISTS                               
*                                                                               
         LA    R3,KEY2                                                          
         AH    R3,LKEY                                                          
         AH    R3,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST ',(R3),AIO2,WORK               
*                                                                               
         USING PIDELD,R6                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,PIDELQ                                                    
         BAS   RE,GETEL                                                         
         B     *+8                 GET NEXT REC                                 
XREF15   BAS   RE,NEXTEL                                                        
         BNE   XREF05                                                           
         CLC   PIDNO,OLDPID        MAKE SURE SAME PID NUMBER                    
         BNE   XREF15              TIME EXISTS                                  
         B     TIMEY                                                            
*                                                                               
         USING AUDRECD,R6                                                       
XREF20   OC    OLDPID,OLDPID                                                    
         BZ    TIMEN                                                            
         LA    R6,KEY2                                                          
         XC    AUDKEY,AUDKEY                                                    
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVI   AUDKAUDT,AUDKTIME                                                
         MVC   AUDKCPY,CMPY                                                     
         MVC   AUDKPIDB,OLDPID                                                  
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR ',KEY2,KEY2                    
         BNE   TIMEN                                                            
         CLC   AUDRECD(AUDKPEDT-AUDKEY),KEYSAVE                                 
         BE    TIMEY                                                            
*                                                                               
TIMEN    B     XNO                                                              
TIMEY    B     XYES                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE PERSON PASSIVE KEYS                                             
***********************************************************************         
*                                                                               
PASSIVE  NMOD1 0,*PASSIVE          DO NOT USE R7                                
         L     RC,SAVERC                                                        
*                                                                               
         CLI   ACTEQU,ACTDEL       DELETING A PERSON?                           
         BNE   PAS02                                                            
         XC    PIDNUM,PIDNUM       CLEAR PID SO WILL DELETE PASSIVE             
         B     PAS04                                                            
*                                                                               
PAS02    DS    0H                                                               
*                                                                               
*        USING PERRECD,R6                                                       
*        L     R1,AIO              FILTERS IN PERREC NEED ACCAP01, TOO          
*                                  AND WORK IN FLIST WHEN 1R FILTER IS          
*                                  CHANGED                                      
*        CLI   PERKTYP-PERRECD(R1),PERKTYPQ                                     
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        XC    BIGKEY,BIGKEY                                                    
*        LA    R6,BIGKEY                                                        
*        MVC   PERKEY,PERKEY-PERRECD(R1)                                        
*        GOTO1 DATAMGR,DMCB,(X'88',=C'DMREAD'),=C'ACCDIR',BIGKEY,BIGKEY         
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        L     R1,AIO                                                           
*        MVC   PERKSTA,PERRSTA-PERRECD(R1)                                      
*        GOTO1 DATAMGR,DMCB,(X'00',=C'DMWRT'),=C'ACCDIR',BIGKEY,BIGKEY          
*        BE    PAS04                                                            
*        DC    H'0'                                                             
*        DROP  R6                                                               
*                                                                               
         USING PIDRECD,R6                                                       
PAS04    CLC   OLDPID,PIDNUM       ANY CHANGE IN PID                            
         BE    PASX                NO                                           
*                                                                               
         OC    OLDPID,OLDPID       DELETE OLD PID NUMBER KEY                    
         BZ    PAS10               NO                                           
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVI   PIDKTYP,PIDKTYPQ    X'3E'                                        
         MVI   PIDKSUB,PIDKSUBQ    X'12'                                        
         MVC   PIDKCPY,CMPY                                                     
         MVI   PIDKSTYP,PIDKPERQ   SET PERSON PASSIVE                           
         MVC   PIDKPID,OLDPID                                                   
         MVC   PIDKPER,PERSON                                                   
         GOTO1 DATAMGR,DMCB,(X'88',=C'DMREAD'),=C'ACCDIR',BIGKEY,BIGKEY         
         CLI   DMCB+8,0                                                         
         BNE   PAS10               IF THERE DELETE THEN ADD NEW POINTER         
         OI    PIDKSTAT,X'80'      DELETE IT                                    
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMWRT'),=C'ACCDIR',BIGKEY,BIGKEY          
         CLI   DMCB+8,0                                                         
         BE    PAS10                                                            
         DC    H'0'                                                             
*                                                                               
PAS10    OC    PIDNUM,PIDNUM       ADD NEW PID NUMBER KEY                       
         BZ    PASX                NO                                           
         LA    R6,BIGKEY           GET DA OF REC JUST ADDED                     
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'ACTKEY),SAVEKEY                                         
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCDIR',BIGKEY,BIGKEY                 
         CLI   DMCB+8,0                                                         
         BNE   PASX                                                             
         MVC   DISKADDR,PIDKDA                                                  
*                                                                               
         XC    BIGKEY,BIGKEY                                                    
         MVI   PIDKTYP,PIDKTYPQ    X'3E'                                        
         MVI   PIDKSUB,PIDKSUBQ    X'12'                                        
         MVC   PIDKCPY,CMPY                                                     
         MVC   PIDKPID,PIDNUM      NEW PID                                      
         MVI   PIDKSTYP,PIDKPERQ   SET PERSON PASSIVE                           
         MVC   PIDKPER,PERSON                                                   
         MVC   KEY2,BIGKEY         SAVE TO ADD                                  
         GOTO1 DATAMGR,DMCB,(X'88',=C'DMREAD'),=C'ACCDIR',BIGKEY,BIGKEY         
         CLI   DMCB+8,X'10'        RECORD NOT FOUND                             
         BE    PAS20               NO, GO ADD                                   
         NI    PIDKSTAT,X'FF'-X'80' UNDELETE                                    
         MVC   PIDKDA,DISKADDR                                                  
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMWRT'),=C'ACCDIR',BIGKEY,BIGKEY          
         CLI   DMCB+8,0                                                         
         BE    PASX                                                             
         DC    H'0'                                                             
*                                                                               
PAS20    XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'ACTKEY),KEY2                                            
         MVC   PIDKDA,DISKADDR                                                  
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMADD'),=C'ACCDIR',BIGKEY,BIGKEY          
         CLI   DMCB+8,0                                                         
         BE    PASX                                                             
         DC    H'0'                                                             
*                                                                               
PASYES   SR    RC,RC                                                            
PASNO    LTR   RC,RC                                                            
PASX     XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SET UP STUFF                                                           
***********************************************************************         
*                                                                               
SETUP    NMOD1 0,*SETUP**          DO NOT USE R7                                
         L     RC,SAVERC                                                        
*                                                                               
         TM    TRANSTAT,ACHANG     DID THE ACTION CHANGE?                       
         BZ    SU05                                                             
         CLI   ACTEQU,ACTADD       AND IS THE ACTION ADD?                       
         BNE   SU05                                                             
         MVC   PEMPID,SPACES       CLEAR PID, HIRE & TERM DATES,                
         MVI   PEMPIDH+5,0         FIRST & LAST NAMES                           
         OI    PEMPIDH+6,X'80'                                                  
         MVC   PEMLNAM,SPACES                                                   
         MVI   PEMLNAMH+5,0                                                     
         OI    PEMLNAMH+6,X'80'                                                 
         MVC   PEMHIRE,SPACES                                                   
         MVI   PEMHIREH+5,0                                                     
         OI    PEMHIREH+6,X'80'                                                 
         MVC   PEMFNAM,SPACES                                                   
         MVI   PEMFNAMH+5,0                                                     
         OI    PEMFNAMH+6,X'80'                                                 
         MVC   PEMTERM,SPACES                                                   
         MVI   PEMTERMH+5,0                                                     
         OI    PEMTERMH+6,X'80'                                                 
*                                                                               
SU05     NI    GENSTAT3,X'FF'-OKVALSEL                                          
         OI    GENSTAT2,RETEQSEL                                                
         BAS   RE,INITPF           CHECK FOR PFKEYS                             
         L     R1,=V(ACSRCHC)                                                   
         A     R1,RELO                                                          
         ST    R1,VACSRCHC                                                      
*                                                                               
         L     R1,=V(ACSRCHP)                                                   
         A     R1,RELO                                                          
         ST    R1,VACSRCHP                                                      
*                                                                               
         L     R1,=V(ACRAPPER)                                                  
         A     R1,RELO                                                          
         ST    R1,VRAPPER                                                       
*                                                                               
         MVI   DOUT,10             DATCON OUTPUT TYPE 10                        
         CLI   LANGCODE,3          GERMANY                                      
         BNE   *+8                                                              
         MVI   DOUT,17             DD.MM.YY FOR GERMANY                         
*                                                                               
         USING STATD,R3            TRANSLATE STATUS TABLE                       
         L     R3,=A(STATTAB)                                                   
         A     R3,RELO                                                          
         CLI   STNAME,X'40'        ALREADY DONE?                                
         BNL   SU50                                                             
SU10     GOTO1 DICTATE,DMCB,C'SU  ',STNAME,0                                    
         LA    R3,STLNQ(R3)                                                     
         CLI   0(R3),0                                                          
         BNE   SU10                                                             
*                                                                               
         USING VSELD,R3            TRANSLATE SELECTION TABLE                    
SU50     L     R3,=A(VSELTAB)          SELECT TABLE                             
         A     R3,RELO                                                          
         CLI   VSELWORD,X'40'      ALREADY DONE?                                
         BNL   SU70                                                             
SU60     GOTO1 DICTATE,DMCB,C'SU  ',VSELWORD,0                                  
         LA    R3,VSELLNQ(R3)                                                   
         CLI   0(R3),0                                                          
         BNE   SU60                                                             
         DROP  R3                                                               
SU70     LA    R2,PERLISTI                                                      
         LA    R3,PERLISTU                                                      
         GOTO1 DICTATE,DMCB,C'LU  ',(R2),(R3)                                   
*        LA    R2,PERLSTLI                                                      
*        LA    R3,PERLSTLU                                                      
*        GOTO1 DICTATE,DMCB,C'LL  ',(R2),(R3)                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,WORK)                                       
         MVC   TODAYC,=X'FFFF'     GET TODAYS DATE                              
         XC    TODAYC,WORK         AND 2S COMPLEMENT                            
         GOTO1 DATCON,DMCB,(5,0),(1,TODAYP)                                     
*                                                                               
SUX      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE PFKEYS                                                      
***********************************************************************         
*                                                                               
INITPF   NTR1                                                                   
         CLI   RECNUM,RTPR2        PER2 SCREEN                                  
         BNE   INIT10                                                           
         LA    R3,PESPFKYH                                                      
         SR    R2,R2                                                            
         CLI   PFKEY,11                                                         
         BE    INIT50                                                           
         L     R2,=A(PPFTABLE)     PER2 PFKEY TABLE                             
         A     R2,RELO                                                          
         B     INIT50                                                           
*                                                                               
INIT10   CLI   ACTNUM,ACTLIST      LIST SCREEN                                  
         BNE   INIT20                                                           
         LA    R3,PELPFKYH                                                      
         L     R2,=A(LPFTABLE)     LIST PFKEY TABLE                             
         A     R2,RELO                                                          
         B     INIT50                                                           
*                                                                               
INIT20   SR    R2,R2                                                            
         LA    R3,PEMPFKYH                                                      
         CLI   PFKEY,7             UP                                           
         BE    INIT50                                                           
         CLI   PFKEY,8             DOWN                                         
         BE    INIT50                                                           
         SR    R2,R2                                                            
         LA    R3,PEMPFKYH                                                      
         CLI   PFKEY,9                                                          
         BE    INIT50                                                           
         CLI   PFKEY,14                                                         
         BE    INIT50                                                           
         L     R2,=A(MPFTABLE)     MAINT PFKEY TABLE                            
         A     R2,RELO                                                          
*                                                                               
INIT50   DS    0H                                                               
         GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)  INITIALIZE THE PFKEYS            
INITX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
                                                                                
         EJECT                                                                  
***********************************************************************         
*        INCLUDES                                                               
***********************************************************************         
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* DDSCANBLKD                                                                    
* FASECRETD                                                                     
* ACCAPWORKD                                                                    
* ACCAPDSECT                                                                    
* ACGENFILE                                                                     
* ACGENBOTH                                                                     
* ACDDEQUS                                                                      
* SEACSFILE                                                                     
* ACCAPBLOCK                                                                    
* ACOFFALD                                                                      
* DDPARSNIPD                                                                    
* DDACTIVD                                                                      
* ACBMONVALD                                                                    
* ACRAPPERD                                                                     
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE ACCAPBLOCK                                                     
       ++INCLUDE ACOFFALD                                                       
       ++INCLUDE DDPARSNIPD                                                     
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE ACBMONVALD                                                     
RAPPERD  DSECT                                                                  
       ++INCLUDE ACRAPPERD                                                      
*        PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*        SCREENS                                                                
***********************************************************************         
                                                                                
       ++INCLUDE ACCAPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPFBD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPF8D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPF9D                                                       
         EJECT                                                                  
***********************************************************************         
*        REMAINING WORK AREA                                                    
***********************************************************************         
*                                                                               
RELO     DS    A                                                                
SAVERC   DS    F                                                                
VACSRCHC DS    F                                                                
VACSRCHP DS    F                                                                
VRAPPER  DS    F                                                                
STADDR   DS    F                   START ADDRESS                                
*                                                                               
SALDB    DS    PL8                 SALARY DEBITS                                
SALCR    DS    PL8                 SALARY CREDITS                               
HRSDB    DS    PL8                 HOURS DEBITS                                 
HRSCR    DS    PL8                 HOURS CREDITS                                
COUNTP   DS    PL8                 PACKED WORK AREA                             
*                                                                               
PERSON   DS    CL8                 PERSON CODE                                  
PERPID   DS    CL8                 PERSON CODE FOR INVALID PID                  
PIDNUM   DS    XL2                 PERSONAL ID NUMBER                           
OLDPID   DS    XL2                 OLD PERSONAL ID NUMBER                       
PIDNAME  DS    CL8                 PERSONAL ID NAME                             
SVPIDNM  DS    CL8                                                              
OFFICE   DS    CL2                                                              
DEPT     DS    CL6                                                              
SUBDPT   DS    CL6                                                              
HIREDT   DS    CL3                 HIRE DATE                                    
TERMDT   DS    CL3                 TERM DATE                                    
STDATE   DS    PL3                 START DATE MANIPULATION                      
ENDATE   DS    PL3                 END DATE                                     
NEWSTDT  DS    PL3                 NEW START DATE (UPDATE SELECT)               
NEWENDT  DS    PL3                 NEW END DATE                                 
SALLOCK  DS    PL3                 SALARY LOCKED DATE                           
TSLOCK   DS    PL3                 TIMESHEET LOCK DATE                          
PREVENDT DS    PL3                 PREVIOUS END DATE                            
NEXTSTDT DS    PL3                 NEXT START DATE                              
SVTSLOCK DS    PL3                 SAVED TMS LOCK DATE                          
YYMMDD1  DS    CL6                                                              
YYMMDD2  DS    CL6                                                              
YYMMDD3  DS    PL3                                                              
YYMMDD4  DS    PL3                                                              
ANALACCT DS    CL1                 ANALYSIS ACCOUNT                             
OWOACCT  DS    CL14                OVERRIDE WRITEOFF ACCOUNT                    
OINCACCT DS    CL14                OCERRIDE INCOME ACCOUNT                      
TASK     DS    CL2                 DEFAULT TASK CODE                            
STRTCODE DS    CL8                 START AT CODE                                
OFFFILT  DS    CL2                 OFFICE FILTER                                
DEPTFILT DS    CL6                 DEPARTMENT FILTER                            
SUBFILT  DS    CL6                 SUB DEPT FILTER                              
*                                                                               
TERMSTAT DS    XL1                 TERMINATE STATUS                             
BIT      DS    XL1                                                              
REQRD    EQU   X'80'               PREVIOUS END DATE REQ'D                      
RESREC   EQU   X'40'               RECORD NEED TO BE RESTORED                   
*CLRPROT  EQU   X'40'               CLEAR ONLY PROTECTED FIELDS                 
CURRLOC  EQU   X'20'               CURRENT LOCATION                             
NEWREC   EQU   X'10'               RECORD NEEDS TO BE ADDED                     
FIRST    EQU   X'08'                                                            
NODATES  EQU   X'04'               DON'T UPDATE HIRE/TERM DATES                 
KEYCHNGE EQU   X'02'               KEY HAS CHANGED                              
UPDSEL   EQU   X'01'               THERE IS AN UPDATIVE SELECT                  
*                                                                               
BIT2     DS    XL1                                                              
NEWTIME  EQU   X'80'               COMPANY ON NEW TIME                          
REBUILD  EQU   X'40'               REBUILD TABLE ON RETURN FROM PER2            
STARTAT  EQU   X'20'               LIST START AT                                
OFFICEOK EQU   X'10'               OFFC IS VALID TO SHOW FOR THIS               
PROTEND  EQU   X'08'               PROT CURR LOC END-DATE                       
NOACCESS EQU   X'04'               PROTECT DISPLAY LINE - NO ACCESS             
NOACCURR EQU   X'02'               NO ACCESS TO THE CURRENT LOCATION            
ADJRATE  EQU   X'01'               HISTORY IS AN ADJ RATE                       
*                                                                               
CHKSTAT  DS    XL1                                                              
CHKSTIME EQU   X'80'               CHECK FOR TIME RECS                          
CHKSSAL  EQU   X'40'               CHECK FOR SALARY RECS                        
HISTELEM EQU   X'20'               HISTORY ELEM EXISTS                          
MSGQUIT  EQU   X'10'               USER DID NOT WANT TO DELETE SALARY           
DELELOC  EQU   X'08'               USER IS DELETING LOCATION                    
MOVESAL  EQU   X'04'               USER IS TRANSFERRING LOCATIONS AND           
THRDPTY  EQU   X'02'               THIS PERSON IS AVAILABLE FOR 3RD PAR         
*                                  WANTS TO MOVE FUTURE SAL TO NEW LOC          
DELTSAL  EQU   X'02'               USER IS ENDING LOCATION AND WANTS            
*                                  TO DELETE FUTURE SALARY                      
SALOUT   EQU   X'01'               FOUND SALARY PAST END DATE                   
*                                                                               
CHKSTAT2 DS    XL1                                                              
VAL2LOCS EQU   X'80'               IF SAL IS PAST ENDDATE THEN VALIDATE         
*                                  BOTH NEW LOC AND OLD BEFORE                  
*                                  PROMPTING USER TO DELETE/MOVE                
MOALOCK  EQU   X'40'               SALARY IS OUTSIDE OF MOA RANGE               
DOUT     DS    XL1                 DATCON OUTPUT TYPE (10 OR GER=17)            
SCRBOT   DS    XL1                                                              
*                                                                               
OPTSTAT  DS    XL1                                                              
NOTERM   EQU   X'80'               DO NOT LIST TERMINATED EMPLOYEES             
ONLYTERM EQU   X'40'               LIST ONLY TERMINATED EMPLOYEES               
NAMEOPT  EQU   X'20'               NAME= OPTION ON                              
PIDOPT   EQU   X'10'               PID= OPTION ON                               
XDOPT    EQU   X'08'               XD=Y OPTION ON                               
PIDNOOPT EQU   X'04'               PID#= OPTION ON                              
ALTDATE  EQU   X'02'               ALT=D OPTION ON                              
*                                                                               
OPTSTA2  DS    XL1                                                              
NOLOA    EQU   X'80'               DO NOT LIST LOA EMPLOYEES                    
ONLYLOA  EQU   X'40'               LIST ONLY LOA EMPLOYEES                      
NOACT    EQU   X'20'               DO NOT LIST ACTIVE EMPLOYEES                 
ONLYACT  EQU   X'10'               LIST ONLY ACTIVE EMPLOYEES                   
*                                                                               
BIT3     DS    XL1                                                              
OPTVK    EQU   X'80'               CALL VALKEY TO RESET REC POINTER             
MOREOPTS EQU   X'40'               MORE OPTIONS TO PROCESS                      
PIDCHK   EQU   X'20'               CHECKING FOR PID                             
NODETAIL EQU   X'10'               NO EXTRA DETAILS (NO 'F1' ELEM)              
ONEBYTOF EQU   X'08'               AGENCY HAS ONE BYTE OFFICE CODES             
DUPLOC   EQU   X'04'               THERE IS A DUP LOCATION                      
DELDUP   EQU   X'02'               DUP LOC HAS BEEN DELETED                     
MIDPER   EQU   X'01'               MID PERIOD READ FOR CHKTIME CHKSVTMS         
*                                                                               
BIT4     DS    XL1                                                              
POINTERS EQU   X'80'               COMPANY USES PASSIVE POINTERS                
AUTOHIST EQU   X'40'               COMPANY USES AUTO ADD HISTORY                
*&&US                                                                           
NOPIDREQ EQU   X'20'               NO PID IS REQUIRED                           
*&&                                                                             
*                                                                               
SWTCHBIT DS    XL1                 BRANDOCEAN SWITCHON DATES                    
PBOS1    EQU   C'1'                1ST DATE FIELD                               
PBOS2    EQU   C'2'                2ND DATE FIELD                               
PBOS3    EQU   C'3'                3RD DATE FIELD                               
PBOS4    EQU   C'4'                4TH DATE FIELD                               
PBOS5    EQU   C'5'                5TH DATE FIELD                               
PBOS6    EQU   C'6'                6TH DATE FIELD                               
*                                                                               
SWTCHBI2 DS    XL1                 BRANDOCEAN SWITCHON DATES                    
NOEDIT   EQU   X'80'               NO EDITING OF SWITCHON DATES                 
SETHIRE  EQU   X'40'               CHANGED A DATE TO HIRE DATE                  
NESTED   EQU   X'20'               NESTED BRO SWITCH ON/OFF DATES               
UPDPPR1  EQU   X'10'               COPY HIGH LEVEL PROFILE TO PERSON            
UPDPPR2  EQU   X'08'               SWITCH PERSON LEVEL PROFILE ON               
UPDPPR3  EQU   X'04'               SWITCH PERSON LEVEL PROFILE OFF              
DUMMYDAT EQU   X'02'               BUILD A DUMMY SWITCHON DATE PAIR             
*                                                                               
SWTCHLVL DS    XL1                 FOR UPDPROF LEVEL OF PROFILE TO COPY         
*                                                                               
LOCBIT   DS    XL1                                                              
LBOFF    EQU   X'80'               NEW OFFICE ENTERED                           
LBDEPT   EQU   X'40'               NEW DEPT ENTERED                             
LBSUB    EQU   X'20'               NEW SUBDPT ENTERED                           
*                                                                               
SBYTE    DS    XL1                 SECURITY OPTION NUMBER                       
OPTFLDQ  EQU   1                   ACCESS TO OPTION UPDATE                      
NAMEFLDQ EQU   1                   ACCESS TO NAME FIELD                         
BRANSWO  EQU   4                   BRANDOCEAN SWITCHON DATES                    
*                                                                               
OLDACNAM DS    CL(NAMLN1Q+L'NAMEREC)                                            
LN1RLEVS DS    0XL4                1R LEVELS                                    
LN1RLEV1 DS    XL1                                                              
LN1RLEV2 DS    XL1                                                              
LN1RLEV3 DS    XL1                                                              
LN1RLEV4 DS    XL1                                                              
LN1RLOW  DS    XL1                 LOWEST LEVEL 1R ACCOUNT LENGTH               
*                                                                               
METHTAB  DS    CL10                LIST OF METHOD NUMBERS                       
SEQNUM   DS    XL1                 SEQ NUMBER FOR STD HOURS RECS                
ACCNT    DS    CL12                MY TEMP ACCOUNT FIELD                        
SECLAST  DS    CL20                LAST NAME FROM SECURITY                      
OPTNAME  DS    CL36                SAVED NAME IN OPTION                         
OPTPID   DS    CL8                 SAVED PID NAME IN OPTION                     
OPTF15   DS    0CL5                SAVED F1-F5 IN OPTIONS                       
OPTFLT1  DS    CL1                                                              
OPTFLT2  DS    CL1                                                              
OPTFLT3  DS    CL1                                                              
OPTFLT4  DS    CL1                                                              
OPTFLT5  DS    CL1                                                              
SECLLN   DS    XL1                 LENGTH OF LAST NAME                          
SECFIRST DS    CL20                FIRST NAME FROM SECURITY                     
SVOPTION DS    XL(L'OPTSTAT+L'OPTSTA2)                                          
SECFLN   DS    XL1                 LENGH OF FIRST NAME                          
SVTHSSEL DS    CL1                 SAVED SELECTION FROM LIST                    
SVPERCDE DS    CL8                 SAVED PERSON CODE                            
DDTERMW  DS    CL4                 'TERM' FROM DICTATE CALL                     
DDLOAW   DS    CL4                 'LOA' FROM DICTATE CALL                      
DDACTW   DS    CL4                 'ACTIVE' FROM DICTATE CALL                   
DDALTW   DS    CL3                 'ALT' FROM DICTATE CALL                      
OFFDISP  DS    CL1                 OFFICE POSITION IN LEDGER                    
SCUNLD   DS    CL2                 UNIT/LEDGER TO SEARCH ON                     
MAXNAME  EQU   36                  MAX LENGTH FOR NAME FILTER IN LIST           
OLDOFLMX EQU   16                  MAX # OF OLD OFFICES ON A PAGE               
MAXCOUNT EQU   29                  MAX # OF ENTRIES IN TABLE                    
YEARCHKQ EQU   3                   # OF YEARS TO CHECK FOR $'S & HRS            
LIMITSAL EQU   12                  MAX # OF MONTHS OF SALARY TO DELETE          
SVCPYPCD DS    XL1                                                              
GPCODE   DS    XL1                                                              
GPFLAG   DS    XL1                                                              
GPMONS   DS    XL2                                                              
GPM1RS   DS    XL(PHILENQ)                                                      
PHILENQ  EQU   L'PHIKOFC+L'PHIKDPT+L'PHIKSBD+L'PHIKPER                          
STDAY    DS    PL3                 SAVED 'HIGHEST' START DATE                   
TSTDAY   DS    PL3                 SAVED START DATE FOR 'TERM' STATUS           
TENDAY   DS    PL3                 SAVED END DATE FOR 'TERM' STATUS             
EXPDTE   DS    PL3                 DATE FOR EXPENSES VALIDATION                 
TS#CDAT  DS    XL3                 -TIME SHEET END DATE+FISCAL MTH              
TS#CEND  DS    XL3                 -END OF FISCAL YEAR                          
BYTE1    DS    XL1                                                              
TODAYC   DS    XL2                 TODAYS DATE (COMPLEMENT)                     
TODAYP   DS    XL3                 TODAYS DATE PL3                              
TWOSCOMP DS    XL2                 DATE IN 2'S COMPLEMENT                       
TWOSTA   DS    XL2                                                              
TWOEND   DS    XL2                                                              
EXPBYTE  DS    XL1                 BYTE FOR EXPENSE CHECK                       
EXPBYTEN EQU   C'N'                NOT OK FOR EXP TO BE SAME AS ST DATE         
EXPBYTES EQU   C'S'                START DATE GIVEN                             
EXPBYTEE EQU   C'E'                END DATE GIVEN                               
*                                                                               
       ++INCLUDE DDGENTWA                                                       
*                                                                               
BLOCKSD  DSECT                                                                  
DISTABLE DS    0H                  DISPLAY TABLE                                
DTNAME   DS    CL8                                                              
DTADDLOC DS    CL(DTLENQ)          LEAVE 1 LINE FOR ADDED LOCATIONS             
DTLOCS   DS    29CL(DTLENQ)        29 ENTRIES  *INCREASED FROM 19 10/02         
DISTABND DS    0C                  * ONLY 4 MORE ENTRIES WILL FIT               
DTLOC2   DS    20CL(DTLENQ)        I HAVE THE SPACE SO THIS IS THE              
DTLOC2ND DS    0C                   BLOCK USED TO SHIFT THE REAL BLOCK          
GPBKEY   DS    XL64                                                             
GPBKEY2  DS    XL64                                                             
STDATES  DS    XL64                BRANDOCEAN SWITCHON TABLES                   
ENDATES  DS    XL64                                                             
KEY2     DS    XL70                ACCFILE KEY                                  
*                                                                               
DISKADDR DS    F                                                                
TABCOUNT DS    H                                                                
STDISP   DS    H                                                                
PRVSTDSP DS    H                                                                
LSTDISP  DS    H                                                                
*                                                                               
SAVEKEY  DS    XL42                ACCFILE KEY                                  
*                                                                               
SAVEKEYL DS    XL42                NEED KEY FOR RECORD RESTORE                  
LRAPBLK  DS    CL(RAPBLKL)         MAINTAIN SETTINGS RAPPER BLOCK               
*                                                                               
TEMPDATE DS    CL3                                                              
TEMPDTE2 DS    CL3                                                              
TEMPDTE3 DS    CL3                                                              
*                                                                               
TEMPOFF  DS    CL2                 TEMPORARY OFFICE FIELD                       
TEMPDEPT DS    CL6                     "     DEPT     "                         
TEMPSDPT DS    CL6                     "     SUBDPT   "                         
TEMPPERS DS    CL8                     "     PERSON   "                         
         EJECT                                                                  
                                                                                
*                                                                               
**********************************************************************          
*        LOCATIONS TABLE DSECT                                                  
***********************************************************************         
*                                                                               
DISTABD  DSECT                     DISPLAY TABLE                                
DTPREVST DS    PL3                 PREVIOUS START DATE                          
DTSTDATE DS    PL3                 START DATE                                   
DTENDATE DS    PL3                 END DATE                                     
DTPREVEN DS    PL3                                                              
DTOFFICE DS    CL2                 OFFICE                                       
DTDEPT   DS    CL6                 DEPARTMENT                                   
DTSUBDPT DS    CL6                 SUB DEPARTMENT                               
DTTSLKDT DS    PL3                 TIMESHEET LOCKED DATE                        
DTSALKDT DS    PL3                 SALARY LOCKED DATE                           
DTSTATUS DS    XL1                 STATUS                                       
DTSTATNM DS    CL8                 STATUS NAME                                  
*&&US                                                                           
DTFREE   DS    CL1                 FREELANCER                                   
*&&                                                                             
DTFILTS  DS    0XL5                                                             
DTFILT1  DS    XL1                 FILTER 1                                     
DTFILT2  DS    XL1                 FILTER 2                                     
DTFILT3  DS    XL1                 FILTER 3                                     
DTFILT4  DS    XL1                 FILTER 4                                     
DTFILT5  DS    XL1                 FILTER 5                                     
DTATTR   DS    XL1                 ATTRIBUTES AS IN X'83' ELEM                  
DTSTAT2  DS    XL1                 STATUS 2 AS IN X'83' ELEM                    
DTLSTAT  DS    XL1                 STATUS OF THIS LINE                          
DTSCHA   EQU   X'80'               LINE HAS BEEN SELECTED FOR CHANGE            
DTSSEL   EQU   X'40'               LINE HAS BEEN SELECTED                       
DTSCURR  EQU   X'20'               CURRENT SELECTION ON STAT SCREEN             
DTSHIS   EQU   X'10'               GO TO HISTORY SCREEN                         
DTSCRA   EQU   X'08'               GO TO CRATES  SCREEN                         
DTSDEL   EQU   X'04'               DELETE LOCATION                              
DTSUPD   EQU   X'02'               UPDATE LOCATION DATES                        
DTSDIS   EQU   X'01'               LINE HAS BEEN SELECTED FOR DISPLAY           
DTACCNT  DS    CL12                1R ACCOUNT                                   
DTLENQ   EQU   *-DISTABD                                                        
*                                                                               
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*                     
*  DSECT BLOCK OF INFO TO BE PAST TO HISTORY DELETE/MOVE                        
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*                     
*                                                                               
PEBLOCKD DSECT                                                                  
PTNAME   DS    CL8                 TABLE HEADER NAME                            
PTTABLN  DS    XL1                 TABLE LENGTH                                 
PLNLNME  DS    XL1                 LENGTH OF LAST NAME                          
PLNAME   DS    CL36                LAST NAME                                    
PLNFNME  DS    XL1                 LENGTH OF FIRST NAME                         
PFNAME   DS    CL36                FIRST NAME                                   
PHIREDT  DS    XL3                 HIRE DATE                                    
PTERMDT  DS    XL3                 TERMINATION DATE                             
PSTART   DS    XL3                 START DATE OF ENDING LOC                     
PTLOCS   DS    0C                                                               
PSTATUS  DS    XL1                 STATUS                                       
PFILTERS DS    0XL5                                                             
PTFILT1  DS    XL1                 FILTER 1                                     
PTFILT2  DS    XL1                 FILTER 2                                     
PTFILT3  DS    XL1                 FILTER 3                                     
PTFILT4  DS    XL1                 FILTER 4                                     
PTFILT5  DS    XL1                 FILTER 5                                     
PTLSTAT  DS    XL1                                                              
PTNEWLOC EQU   X'80'               THIS ENTRY IS THE NEW LOCATION               
PTLOCNQ  EQU   *-PTLOCS                                                         
PTLENQ   EQU   *-PTNAME            LEN OF TABLE WITH 1 LOCATION ENTRY           
PT2LENQ  EQU   (*-PTNAME)+PTLOCNQ  LEN OF TABLE WITH 2 LOCATION ENTRIES         
         EJECT                                                                  
***********************************************************************         
*        STATUS AND ATTRIBUTE AND VALID SELECTS TABLE DSECTS                    
***********************************************************************         
*                                                                               
STATD    DSECT                     STATTAB DSECT                                
STNAME   DS    CL8                 STATUS NAME                                  
STBYTE   DS    XL1                 BYTE AS IN X'83' AND X'56' ELEMS             
STLNQ    EQU   *-STATD                                                          
*                                                                               
ATTRD    DSECT                     ATTRTAB DSECT                                
ATTRDISP DS    XL2                 DISP TO ATTRIBUTE FIELD                      
ATTRBIT  DS    XL1                 BIT ON IF Y IN FIELD                         
ATTRLNQ  EQU   *-ATTRD                                                          
*                                                                               
VSELD    DSECT                     VSELTAB DSECT                                
VSELWORD DS    CL3                 SELECT CODE                                  
VSELBIT  DS    XL1                 BIT ON                                       
VSELDISP DS    CL1                 Y TO REDISP SELECT CODE                      
VSELLNQ  EQU   *-VSELD                                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        PERSON SCREEN LOCATION LINE DSECT                                      
***********************************************************************         
*                                                                               
LLINED   DSECT                     LOCATION HISTORY LINE                        
LLSELH   DS    CL8                 FIELD HEADER                                 
LLSEL    DS    CL3                 SELECTION                                    
LLOFFH   DS    CL8                 FIELD HEADER                                 
LLOFF    DS    CL2                 OFFICE                                       
LLDEPTH  DS    CL8                                                              
LLDEPT   DS    CL6                 DEPARTMENT                                   
LLSUBH   DS    CL8                                                              
LLSUB    DS    CL6                 SUB DEPARTMENT                               
LLDATEH  DS    CL8                                                              
LLDATE   DS    CL8                 DATE LOCATION STARTED                        
LLENDTH  DS    CL8                                                              
LLENDT   DS    CL8                 DATE LOCATION ENDED                          
LLSALDH  DS    CL8                                                              
LLSALD   DS    CL8                 DATE SALARY LOCKED                           
LLSTATH  DS    CL8                                                              
LLSTAT   DS    CL8                 STATUS                                       
*&&US                                                                           
LLFREEH  DS    CL8                                                              
LLFREE   DS    CL1                 FREELANCER                                   
*&&                                                                             
LLFILT1H DS    CL8                                                              
LLFILT1  DS    CL1                 FILTERS                                      
LLFILT2H DS    CL8                                                              
LLFILT2  DS    CL1                 FILTERS                                      
LLFILT3H DS    CL8                                                              
LLFILT3  DS    CL1                 FILTERS                                      
LLFILT4H DS    CL8                                                              
LLFILT4  DS    CL1                 FILTERS                                      
LLFILT5H DS    CL8                                                              
LLFILT5  DS    CL1                 FILTERS                                      
LLLEN    EQU   *-LLINED                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LIST LINE DSECT                                                        
***********************************************************************         
*                                                                               
LSTLINED DSECT                                                                  
LSTCODE  DS    CL8                 PERSON CODE                                  
         DS    CL1                                                              
LSTNAME  DS    0CL36               NAME                                         
LSTDATS  DS    CL21                                                             
         DS    CL15                                                             
         DS    CL3                                                              
LSTOFF   DS    CL2                                                              
         DS    CL4                                                              
LSTDEPT  DS    CL6                                                              
         DS    CL3                                                              
LSTSUB   DS    CL6                                                              
*                                                                               
***********************************************************************         
*        EXTRA DETAILS LINE DSECT                                               
***********************************************************************         
*                                                                               
XDETD    DSECT                                                                  
XDLINE   DS    0C                                                               
XDDAW    DS    CL3                 DA=                                          
XDDA     DS    XL8                 DISK ADDRESS                                 
XDCOMMA1 DS    CL1                 COMMA                                        
XDDADDW  DS    CL5                 DADD=                                        
XDDADD   DS    XL6                 DATE ADDED (YYMMDD)                          
XDCOMMA2 DS    CL1                                                              
XDDCHAW  DS    CL5                 DCHA=                                        
XDDCHA   DS    XL6                 DATE CHANGED (YYMMDD)                        
XDCOMMA3 DS    CL1                                                              
XDWHOW   DS    CL4                 WHO=                                         
XDWHO    DS    CL8                 LAST PERSON TO MAKE CHANGE                   
*                                                                               
*                                                                               
* DETAILS FOR NON DDS TERMINALS                                                 
*                                                                               
         ORG   XDLINE                                                           
XDDADDWN DS    CL5                 DADD=                                        
XDDADDN  DS    XL6                 DATE ADDED (YYMMDD)                          
XDCOMM1N DS    CL1                                                              
XDDCHAWN DS    CL5                 DCHA=                                        
XDDCHAN  DS    XL6                 DATE CHANGED (YYMMDD)                        
XDCOMM2N DS    CL1                                                              
XDWHOWN  DS    CL4                 WHO=                                         
XDWHON   DS    CL8                 LAST PERSON TO MAKE CHANGE                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'115ACCAP08   09/20/20'                                      
         END                                                                    
