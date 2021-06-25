*          DATA SET ACPRO71    AT LEVEL 087 AS OF 04/28/15                      
*PHASE T60B71A                                                                  
*INCLUDE BINSRCH2                                                               
*                                                                               
* AMAD 002 18JUN02 ERRONEOUS BRANCHES FIXED                                     
* AMAD 003 21JUN02 VARIOUS BUG FIXES                                            
* MWHE 004 21JUN02 DOWNLOAD CHANGE AS THE DEFAULT ACTION FOR UPLOAD             
* AMAD 005 26JUN02 MAP TABLE INDS TO PREVENT SENDING BLANK CHARS                
* AMAD 006 26JUN02 DON'T SEND BLANK USER CODES                                  
* AMAD 007 27JUN02 COMMENTS BUG FIX                                             
* MWHE 008 02JUL02 DONT CLEAR DEBTORS/COSTING IF ALREADY SET                    
* AMAD 009 02JUL02 FIX COMMENTS BUG                                             
* MWHE 010 25SEP02 JOB AUTO                                                     
* MWHE 011 01OCT02 JOB AUTO BUG FIX                                             
* DPEA 012 08APR03 DON'T SET AERRAREA - NOW DONE IN ACPRO70                     
* AMAD 013 09APR03 SET CORRECT ERROR MESSAGE FOR INVALID DATE                   
* AMAD 014 11APR03 CORRECT JOB COMMENT/P.O.B FIELD LENGTHS                      
* MWHE 015 11APR03 MAKE SURE JOB AUTO ERRORS ACTUALLY EXIT                      
* MWHE 016 23APR03 PUT BACK IN LINE AT USET40                                   
* AMAD 017 01JUL03 CLEAR AIO BEFORE (RE)BUILDING THE JOB RECORD                 
* TKLU 018 24JUL03 ENSURE ALL ADDRESS LINES TAKEN ON EXTRA JOB SCREEN           
* MWHE 019 04AUG03 FIX EXIT BUG IN GETUSR ROUTINE (RESTORE AIO1)                
* MWHE 020 17SEP03 JOB COMMENTS LENGTH IS 50 NOT 60                             
* MWHE 021 17JUN04 MAKE SURE FULL LENGTH OF RSTEL IS CLEARED                    
* MWHE 022 16JUL04 LO01-3460 SET RECORD ADDED INFO IN XJOB                      
* MWHE 023 26AUG04 PHASE CARD CHANGE FOR PANAPT                                 
* MWHE 024 12OCT04 NEW FIELDS FOR DESKTOP <LO01-3568> (LO01-3446)               
* MWHE 025 25MAY05 AUTO JOB NO REQUEST  & AUTO CLOSE DATE <LO01-4362>           
* TKLU 026 29JUN05 <LO01-4376> FILTER NAME/VALUES VALIDATION                    
* DPEA 027 31AUG05 <LO01-4465> USE ACSRCHPASS DEFINED IN ROOT                   
* TKLU 028 06JAN06 <TK1024285> BUG FIX - USE FIELD NOT SCREEN FIELD             
* TKLU 029 12JAN06 <DU01-5098> LONG JOB DESCRIPTION FOR MCS/JOBS                
*          20JAN06 <TK1006801> AUTO JOB NO./OFFICE SECURITY BUG FIX             
*          01FEB06 <DU01-5106> JOB SOURCE STATUS                                
*          03FEB06 <TK1025525> AUTO JOB NUMBERING - MOVE UPDATE                 
* DPEA     10FEB06 <ACCQ 1027345> AUTO JOB NO FIX IF NO ALREADY EXISTS          
* YNGX     03MAR06 <LO01-4326> ADD JOB TO APPROVER RECORD                       
* TKLU     20MAR06 <DU01-4941> JOBSNEST/JOBSMCSE FLAG                           
*          06APR06 CLIENT AND PRODUCT NAME RETURN                               
*          13APR06 MEDIA CODE AND NAME                                          
* TKLU 030 24APR06 <DU01-5098> LONG DESCRIPTION BUG FIX                         
* TKLU 031 10MAY06 <UKCR0000671> ADD CREATED DATE FOR JOB DISPLAY               
* TKLU 032 05JUL06 <DU01-5577> 'ADD ACCOUNT AS DRAFT' FEATURE                   
*                  AND CREATE DRAPASD PASSIVE, FIND40 BUG FIX                   
*          14SEP06 IGNORE OFFICE PASSED IN UPLOAD                               
*                  ADD STATUS TO DOWNLOAD                                       
*          27SEP06 ADD ID NUMBER TO JOB UP/DOWNLOAD                             
* TKLU 033 17NOV06 <DU01-5577> STATUS RETURN ON ADD/MAINT CALLS                 
* TKLU 034 15DEC06 LEVEL *33 BUG FIX TO SND MODE                                
* TKLU 035 18DEC06 <DU01-5577> SUPPORT FOR AUTO-APPROVAL (2 STAGES)             
* TKLU 036 08JAN07 <DU01-5577> APPROVE/REJECT COMMENTS ON DISPLAY               
* TKLU 037 15JAN07 <DU01-5577> BUG FIX TO DISPLAY APPROVER'S STATUS             
*                  AND W/S CHANGES AS GETTING SHORT OF IT                       
*                  ADD RACELD/RACTCHA ON ANY CHANGE FOR AUDIT                   
* NSHE 038 08FEB07 <LO01-6033> ADD ALTERNATIVE NAMES                            
* TKLU 039 26APR07 <UKCR00012419> JOB CREATOR CANNOT BE ITS APPROVER            
* TKLU 040 27JUL07 <DU01-6661> MULTIPLE RACTCHA ELEMENT SUPPORT                 
*          06AUG08 <UKCR00013619> DEFAULT JOB APPR. MUST EXIST ON ADD           
* TKLU     23AUG07 <LO01-6336> CAMPAIGN CODE ON JOB                             
*          03SEP07 <UKCR00014002> SUPPORT SELF-APPROVAL SCENARIO                
*          10SEP07 <BR11314D> COMPANY LEVEL AUTO JOB CLOSE DATE BUG             
* TKLU 041 18SEP07 <UKCR00014182> STCELD ON SELF APPROVAL                       
* TKLU 042 31OCT07 US MERGER (FOR RGUP)                                         
* TKLU 043 15NOV07 <BR11615D> AUTO NUMBER OVERFLOW - CATCH AND ERROR            
* TKLU 044 21NOV07 JOB APPROVALS - BRING IN SYNC WITH ACBRA19                   
* TKLU 045 11MAR08 <LO01-7431> ENLARGE DESC. PASSED + LONGER LINKL              
* TKLU 046 01APR08 <DU01-7432> AEXRECD USAGE PREPARATIONS: JLDELD ON            
*                  AEXRECD IN DISPLAY MODE DONE, REMOVING AEXRECD ON            
*                  CHANGE DONE, TOO, GENERATION OF AEXRECD O/S                  
* TKLU 047 10JUL08 <UKCR00017741> APPROVALS FIX (LIDELD CHECK BUG FIX)          
* NSHE 048 10AUG08 <LO01-7743> SEPARATE CLIENT APPROVERS                        
* NSHE 049 10OCT08 <BR20591L> FIX JOB APPROVERS BUG                             
* TKLU 050 27OCT08 <LO01-8119> ENLARGE LINKL LOCAL STORAGE BY 1K                
* SMAN 051 22SEP08 <LO01-7385> STATUS REPORT KEY ROLES                          
* TKLU     25SEP08 <DU01-8171> 'JOB USES LANGUAGE' FROM OPT/MAINT               
* TKLU     27OCT08 <LO01-8119> CLIENT PO TABLE                                  
* TKLU     27OCT08 <LO01-8274> FEE BUDGET AMOUNT                                
* TKLU     06NOV08 <DU01-8304> EXPANDED JOB LOCKS                               
* NSHE     21NOV08 <LO01-8345> CLIENT PURCHASE ORDER AMOUNT                     
* SMAN 052 03APR09 <LO01-7385> STATUS REPORT KEY ROLES REMOVE TST LINES         
* NSHE 053 11MAY09 CHANGE TO APPROVER RECORDS                                   
*      054 ???                                                                  
* NRAK 055 18SEP09 CHECK IN ADVANCE WHETHER RECORD EXISTS ON ADD                
* NRAK 057 14APR10 <OT11582D> ALLOW FOR STUPID LENGTH OF 'KEY' LABEL            
* TKLU 058 15APR10 <LO01-9776> DRAFT/LIVE TYPE AND ACGENBOTH SUBST.             
* YNGX 059 23sep10 <BR16591D> BUG FIX FOR BRANDOCEAN JOB UPLOAD                 
* SMAN 060 12MAY11 <UKCR00032096> Add Cli/Med and Cli lvls to APRTAB            
* NRAK 061 13JUN11 <BR18133D> MAKE SURE JOBS HAVE JOBELS!                       
* JFOS 062 24JUL12 <PR002375> ABLLN2Q -> ABLLN3Q                                
* TKLU 063 27JUN14 <RD003081> JOB ADDED DATE (PIDKADAT) FOR PIDKJSA/SQ          
         TITLE 'T60B71 - FALINK JOB MAINT FOR NEW PRODUCTION'                   
*                                                                               
T60B71   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T60B71*,R4,R8,RR=R2                                           
         USING LINKD,R7                                                         
         L     RC,AGEND                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING T60BFFD,RA                                                       
                                                                                
         ST    R2,RELO                                                          
         BASR  R2,0                                                             
         AHI   R2,FINDJOB-*                                                     
         ST    R2,AFINDJOB                                                      
         BASR  R2,0                                                             
         AHI   R2,GETUSR-*                                                      
         ST    R2,AGETUSR                                                       
         BASR  R2,0                                                             
         AHI   R2,VALJCOM-*                                                     
         ST    R2,AVALJCOM                                                      
         BASR  R2,0                                                             
         AHI   R2,ADDPROF-*                                                     
         ST    R2,AADDPROF                                                      
         BASR  R2,0                                                             
         AHI   R2,ADDASTA-*                                                     
         ST    R2,AADDASTA                                                      
         BASR  R2,0                                                             
         AHI   R2,BALPEEL-*                                                     
         ST    R2,ABALPEEL                                                      
         BASR  R2,0                                                             
         AHI   R2,GETLDGS-*                                                     
         ST    R2,AGETLDGS                                                      
         BASR  R2,0                                                             
         AHI   R2,GETPID-*                                                      
         ST    R2,AGETPID                                                       
         BASR  R2,0                                                             
         AHI   R2,ADDDRAP-*                                                     
         ST    R2,AADDDRAP                                                      
         BASR  R2,0                                                             
         AHI   R2,VALNAME-*                                                     
         ST    R2,AVALNAME                                                      
         BASR  R2,0                                                             
         AHI   R2,VALSTAT-*                                                     
         ST    R2,AVALSTAT                                                      
         BASR  R2,0                                                             
         AHI   R2,VCLOSE-*                                                      
         ST    R2,AVCLOSE                                                       
         BASR  R2,0                                                             
         AHI   R2,ADDEST-*                                                      
         ST    R2,AADDEST                                                       
         BASR  R2,0                                                             
         AHI   R2,ADDPASS-*                                                     
         ST    R2,AADDPASS                                                      
         BASR  R2,0                                                             
         AHI   R2,GETAPP-*                                                      
         ST    R2,AGETAPP                                                       
         BASR  R2,0                                                             
         AHI   R2,VALACADR-*                                                    
         ST    R2,AVALACAD                                                      
         BASR  R2,0                                                             
         AHI   R2,VALEXTCM-*                                                    
         ST    R2,AVALEXTC                                                      
         BASR  R2,0                                                             
         AHI   R2,VALCAMPC-*                                                    
         ST    R2,AVALCAMP                                                      
         BASR  R2,0                                                             
         AHI   R2,VALTEAJ-*                                                     
         ST    R2,AVALTEAJ                                                      
         BASR  R2,0                                                             
         AHI   R2,VALLAL-*                                                      
         ST    R2,AVALLAL                                                       
         BASR  R2,0                                                             
         AHI   R2,NAMESRCH-*                                                    
         ST    R2,ANAMSRCH                                                      
         BASR  R2,0                                                             
         AHI   R2,ADD2JOB-*                                                     
         ST    R2,AADD2JOB                                                      
         BASR  R2,0                                                             
         AHI   R2,AEXAREA-*                                                     
         ST    R2,AAEXAREA                                                      
         ST    R2,AAEXANXT                                                      
                                                                                
*&&US                                                                           
         L     R2,=A(GOXBLK)                                                    
         A     R2,RELO                                                          
         ST    R2,AGOXBLK                                                       
*&&                                                                             
         MVI   BLANKS,C' '                                                      
         MVC   BLANKS+1(L'BLANKS-1),BLANKS                                      
                                                                                
         ZAP   PZERO,=P'0'                                                      
                                                                                
         LHI   RE,SVCTEAM-LINKD                                                 
         LA    RE,LINKD(RE)                                                     
         ST    RE,ASVCTEAM                                                      
                                                                                
*        LA    R1,CONTAGH ** NOW SET IN ACPRO70 **                              
*        ST    R1,AERRAREA         SET A(OUTPUT AREA FOR ERR MSGS)              
                                                                                
         MVI   RUNINDS,0                                                        
                                                                                
         CLI   LINKMODE,ROUTSN                                                  
         BNL   EXITY                                                            
         XR    RF,RF                                                            
         IC    RF,LINKMODE                                                      
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     SETMAP              SET A(MAP TABLE)                             
         B     RCVFST              FIRST FOR RECEIVE                            
         B     RCVHDRF             FIRST FOR MAP HEADER RECEIVE                 
         B     RCVDATA             DATA RECEIVE                                 
         B     RCVHDRL             LAST FOR MAP HEADER RECEIVE                  
         B     RCVLST              LAST FOR RECEIVE                             
         B     SND                 SEND                                         
ROUTSN   EQU   (*-ROUTS)/L'ROUTS                                                
         SPACE 1                                                                
***********************************************************************         
* SET A(MAP TABLE)                                                    *         
***********************************************************************         
         SPACE 1                                                                
SETMAP   DS    0H                                                               
         BASR  RF,0                                                             
         AHI   RF,MAPTAB-*                                                      
         ST    RF,AMAPTAB                                                       
         GOTO1 AVALTAB                                                          
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR RECEIVE                                                   *         
***********************************************************************         
         SPACE 1                                                                
RCVFST   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR MAP HEADER RECEIVE                                        *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRF  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* ELEMENT DATA RECEIVE                                                *         
***********************************************************************         
         SPACE 1                                                                
RCVDATA  DS    0H                                                               
         ICM   RF,15,ORCVDATA                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR MAP HEADER RECEIVE                                         *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRL  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR RECEIVE                                                    *         
***********************************************************************         
         SPACE 1                                                                
RCVLST   DS    0H                                                               
*                                                                               
         GOTO1 GETFACT,DMCB,0,0                                                 
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   CMPALPHA,FATAGYSC   SECURITY ALPHA ID                            
         DROP  R1                                                               
         MVC   AIO,AIO2            SWAP IO AREA                                 
         GOTO1 AGETLDGS,(RC)                                                    
         MVC   KEY,BLANKS                                                       
         MVC   KEY(1),CUL                                                       
         GOTO1 READ                                                             
         MVI   ELCODE,ACMPELQ                                                   
         BAS   RE,GETELIO                                                       
         USING CPYELD,R6                                                        
         OC    CMPALPHA,CMPALPHA                                                
         BNZ   *+10                                                             
         MVC   CMPALPHA,CPYALPHA   ALPHA ID                                     
         MVC   SAVOFF,BLANKS                                                    
         CLI   TWAACCS,0                                                        
         JE    RCVLST04                                                         
         TM    CPYSTATC,CPYSROFF OFFICES FOR RESOURCES                          
         JZ    RCVLST04                                                         
         MVC   SAVOFF,TWAACCS+2                                                 
         USING OFFALD,R1                                                        
RCVLST02 L     R1,AOFFBLK                                                       
         TM    CPYSTAT4,CPYSOFF2                                                
         JNZ   RCVLST04                                                         
         MVC   SAVOFF,OFFAWORK                                                  
         MVI   SAVOFF+1,C' '                                                    
RCVLST04 MVC   AIO,AIO1            SWAP BACK                                    
                                                                                
         CLI   FAACTION,FQACTDIS   ONLY FOR ADD, AUTO, CHANGE OR NUM            
         BE    EXITY                                                            
*                                                                               
         LHI   RF,POINTERS-T60BFFD  PRESTO ACTIVITY POINTER BLOCK               
         AR    RF,RA                                                            
         ST    RF,APOINTRS                                                      
         GOTO1 VSAVPTRS,DMCB,(X'80',0),APOINTRS   CLEAR POINTER BLOCK           
*                                                                               
         GOTO1 VKEY                VALIDATE KEY FIELDS                          
         BNE   EXITN                                                            
         CLI   FAACTION,FQACTNUM   IF WE JUST WANTED A NUMBER THEN              
         BE    EXITY               WE'RE FINISHED FOR NOW                       
*                                                                               
         CLI   FAACTION,FQACTCHA                                                
         BNE   RCVLST10                                                         
         GOTO1 VSAVPTRS,DMCB,(X'80',AIO),APOINTRS SAVE POINTER BLOCK            
*                                                                               
RCVLST10 GOTO1 VREC                VALIDATE OTHER FIELDS & UPDATE REC           
         BNE   EXITN                                                            
*                                                                               
         GOTO1 VCHGPTRS,DMCB,(X'80',AIO),APOINTRS UPDATE POINTERS               
*                                                                               
         CLI   FAACTION,FQACTADD   FOR ADD ONLY, ARE WE DOING NEW               
         BE    RCVLST15             ESTIMATE                                    
         CLI   FAACTION,FQACTAUT                                                
         BNE   RCVLST30                                                         
*&&US                                                                           
         USING ACKEYD,R6                                                        
RCVLST15 L     R6,AIO                                                           
         TM    ACSTATUS,ACTSDRFT   IS JOB DRAFT?                                
         BO    RCVLST20            YES, ADD PASSIVE FOR DRAFT JOB               
*&&                                                                             
*&&UK                                                                           
RCVLST15 TM    MYLSTA2,LDGSDRFT    DRAFT FEATURE ACTIVATED?                     
         BZ    RCVLST25                                                         
         CLC   SVPASSWD,MYAPPST                                                 
         BNE   RCVLST20                                                         
*&&                                                                             
         GOTO1 AADDPASS,(RC)       NO, ADD PID PASSIVE IF LIVE                  
         B     RCVLST25                                                         
*                                                                               
RCVLST20 DS    0H                                                               
         GOTO1 AADDDRAP,(RC)                                                    
*                                                                               
RCVLST25 CLI   GOESTTYP,C'N'                                                    
         BNE   RCVLST30            NO, DON'T ADD THEM                           
         BAS   RE,PREADD           YES, ADD ESTIMATES                           
*                                                                               
*                                  COMMON EXIT FOR RECEIVE/LAST                 
*                                                                               
RCVLST30 CLI   FAACTION,FQACTAUT   IF JOB AUTO                                  
         BE    RCVLST35                                                         
         CLI   FAACTION,FQACTADD   OR JOB ADD                                   
         BE    RCVLST35                                                         
         CLI   FAACTION,FQACTCHA   OR CHANGE                                    
         BNE   EXITY                                                            
*                                                                               
RCVLST35 MVI   FAACTION,FQACTDIS   SEND NEW JOB BACK WITH NUM/DATE              
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* SEND                                                                *         
***********************************************************************         
         SPACE 1                                                                
SND      DS    0H                                                               
         ICM   RF,15,OSND                                                       
         BNZR  RF                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SEND JOB                                                            *         
***********************************************************************         
         SPACE 1                                                                
SNDJOB   DS    0H                                                               
         CLI   FAACTION,FQACTNUM   JUST SEND BACK THE NEW KEY                   
         BE    SND04                                                            
         CLI   FAACTION,FQACTDIS                                                
         BNE   EXITY                                                            
                                                                                
         LA    R6,KEY                                                           
         USING ACTRECD,R6                                                       
         MVC   ACTKEY,BLANKS                                                    
         MVC   ACTKCULA(L'CUL),CUL                                              
         GOTO1 SETHEIR                                                          
                                                                                
         MVC   CLPROFF,BLANKS                                                   
         LHI   RE,FAROLE1-LINKD                                                 
         LA    RE,LINKD(RE)                                                     
         LA    RF,FARENDL                                                       
         XCEF                                                                   
                                                                                
         MVC   ACTKEY,BLANKS                                                    
         MVC   ACTKCULA(L'CUL),CUL                                              
         MVC   ACTKACT(L'FACLIENT),FACLIENT                                     
         MVC   SCLIKHD,KEY                                                      
         GOTO1 READ                                                             
         ST    R6,SAVER6                                                        
         MVI   ELCODE,PPRELQ       GET OFFICE FROM CLIENT                       
         BAS   RE,GETELIO                                                       
         BNE   SND00                                                            
         USING PPRELD,R6                                                        
         MVC   CLPROFF,PPRGAOFF                                                 
*                                                                               
SND00    MVI   ELCODE,LIDELQ       GET TEAM LIST DATA (CLIENT TEAM)             
         BAS   RE,GETELIO                                                       
         BNE   SND02                                                            
         USING LIDELD,R6                                                        
         CLI   LIDTYPE,LIDTTEAJ                                                 
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     SND02                                                            
         LA    R0,LIDDATA-LIDELD                                                
         XR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         SR    R1,R0                                                            
         XR    RE,RE                                                            
         IC    RE,LIDITLN                                                       
         XR    R0,R0                                                            
         DR    R0,RE           R1=NUMBER OF ITEMS                               
         STC   R1,BYTE                                                          
         LA    R2,LIDDATA                                                       
         USING FATEAJD,R3                                                       
         LHI   R3,FAROLE1-LINKD                                                 
         LA    R3,LINKD(R3)                                                     
         MVC   AIO,AIO2                                                         
SND01    MVC   FAROLE,0(R2)    (CONVERT TO DECIMAL?)                            
         MVC   HALF,0(R2)                                                       
         BAS   RE,GETRLNM                                                       
         MVC   FARLNM,WORK                                                      
*                                                                               
         MVC   HALF,L'LIDROLN(R2)                                               
         GOTO1 AGETPID,(RC)                                                     
         BNE   *+10                                                             
         MVC   FAPCOD,WORK                                                      
*                                                                               
         MVI   FARLEV,C'C'   'C' FOR CLIENT LEVEL                               
         AHI   R3,FARSTRT                                                       
         AHI   R2,L'LIDROLN+L'LIDBPID                                           
         XR    R1,R1                                                            
         IC    R1,BYTE                                                          
         SHI   R1,1                                                             
         STC   R1,BYTE                                                          
         LTR   R1,R1                                                            
         BNZ   SND01                                                            
         MVC   AIO,AIO1                                                         
         DROP  R3                                                               
*                                                                               
         USING ACTRECD,R6                                                       
SND02    L     R6,SAVER6                                                        
         XR    RE,RE                                                            
         IC    RE,LCLI                                                          
         LA    RF,ACTKACT                                                       
         AR    RF,RE                                                            
         MVC   0(L'FAPROD,RF),FAPROD                                            
         MVC   SPROKHD,KEY                                                      
         GOTO1 READ                                                             
         ST    R6,SAVER6                                                        
         MVI   ELCODE,PPRELQ       GET OFFICE FROM PRODUCT                      
         BAS   RE,GETELIO                                                       
         BNE   SND03                                                            
         USING PPRELD,R6                                                        
         CLI   PPRGAOFF,C' '                                                    
         BNH   SND03                                                            
         MVC   CLPROFF,PPRGAOFF                                                 
*                                                                               
         USING ACTRECD,R6                                                       
SND03    L     R6,SAVER6                                                        
         XR    RE,RE                                                            
         IC    RE,LCLI                                                          
         LA    RF,ACTKACT                                                       
         AR    RF,RE                                                            
         MVC   0(L'FAPROD,RF),FAPROD                                            
         MVC   SPROKHD,KEY                                                      
         IC    RE,LPRO                                                          
         AR    RF,RE                                                            
         MVC   0(L'FAJOB,RF),FAJOB                                              
         MVC   SAVEJOB,ACTKACT                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
                                                                                
SND04    GOTO1 ASNDHDR,DMCB,1      SEND JOB INFO                                
         MVI   BYTE,FQACTCHA                                                    
         GOTO1 ASNDDATA,DMCB,1,BYTE                                             
         IC    R2,LCLI                                                          
         GOTO1 (RF),(R1),2,((R2),FACLIENT)                                      
         IC    R2,LPRO                                                          
         GOTO1 (RF),(R1),3,((R2),FAPROD)                                        
         IC    R2,LJOB                                                          
         GOTO1 (RF),(R1),4,((R2),FAJOB)                                         
         CLI   FAACTION,FQACTNUM   FINISHED IF IT WAS A NUM REQUEST             
         BE    EXITY                                                            
*                                                                               
         MVI   ELCODE,LIDELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   SND05                                                            
         USING LIDELD,R6                                                        
         CLI   LIDTYPE,LIDTTEAJ                                                 
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     SND05                                                            
         LA    R0,LIDDATA-LIDELD                                                
         XR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         SR    R1,R0                                                            
         XR    RE,RE                                                            
         IC    RE,LIDITLN                                                       
         XR    R0,R0                                                            
         DR    R0,RE           R1=NUMBER OF ITEMS ON LIDTTEAJ                   
         STC   R1,BYTE                                                          
         USING FATEAJD,R3                                                       
         LA    R2,LIDDATA                                                       
SNDROL00 LHI   R3,FAROLE1-LINKD                                                 
         LA    R3,LINKD(R3)                                                     
         MVI   MYBYTE,14             LIMIT STORAGE TO 14 ITEMS                  
         MVC   AIO,AIO2                                                         
SNDROL01 OC    FAROLE,FAROLE                                                    
         BZ    SNDROL04                                                         
         CLC   FAROLE,0(R2)                                                     
         BNE   SNDROL03                                                         
         L     R6,AIO                                                           
         USING SAPEREC,R6                                                       
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ      C'F'                                       
         MVI   SAPESUB,SAPESUBQ      X'04'                                      
         MVC   SAPEAGY,CMPALPHA                                                 
         MVC   SAPEPID,FAPCOD                                                   
         MVC   SAVEKEY,SAPEKEY                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,(R6),(R6)                             
         CLC   SAVEKEY(SAPEDEF-SAPEKEY),SAPEKEY                                 
         BNE   SNDROL03                                                         
         LA    RF,SAPEDATA                                                      
         USING SAPWDD,RF                                                        
         XR    R0,R0                                                            
SNDROL02 CLI   SAPWDEL,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   SAPWDEL,SAPWDELQ      X'C4'                                      
         BE    *+14                                                             
         IC    R0,SAPWDLN                                                       
         AR    RF,R0                                                            
         B     SNDROL02                                                         
         CLC   SAPWDNUM,L'LIDROLN(R2)                                           
         BE    SNDROL06            YES, ALREADY STORED THEN                     
SNDROL03 AHI   R3,FATEAJLN                                                      
         LA    RE,MYBYTE                                                        
         SHI   RE,1                                                             
         STC   RE,MYBYTE                                                        
         CLI   MYBYTE,0                                                         
         BH    SNDROL01                                                         
         B     SNDROL07            HAVE STORED 14 ALREADY                       
         DROP  RF                                                               
SNDROL04 MVC   FAROLE,0(R2)        (CONVERT TO DECIMAL?)                        
         MVC   HALF,0(R2)                                                       
         BAS   RE,GETRLNM          GET ROLE NAME                                
         MVC   FARLNM,WORK                                                      
*                                                                               
         MVC   HALF,L'LIDROLN(R2)                                               
         GOTO1 AGETPID,(RC)                                                     
         BNE   *+10                                                             
         MVC   FAPCOD,WORK                                                      
*                                                                               
         MVI   FARLEV,C'J'             'J' FOR JOB LEVEL                        
SNDROL06 XR    R1,R1                                                            
         IC    R1,BYTE                                                          
         SHI   R1,1                                                             
         LTR   R1,R1                                                            
         BNZ   SNDROL08                                                         
SNDROL07 MVC   AIO,AIO1                                                         
         B     SND05                                                            
SNDROL08 STC   R1,BYTE                                                          
         AHI   R2,L'LIDROLN+L'LIDBPID                                           
         B     SNDROL00                                                         
         DROP  R3                                                               
*                                                                               
SND05    CLI   SNDMODE,FQACTCHA    FROM CHANGE - SHOW STATUS                    
         BE    SND14                                                            
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   SND06                                                            
         USING NAMELD,R6                                                        
         XR    R2,R2                                                            
         IC    R2,NAMLN                                                         
         SHI   R2,2                                                             
         BM    SND06                                                            
         GOTO1 ASNDDATA,DMCB,5,((R2),NAMEREC)                                   
                                                                                
         USING RACELD,R6                                                        
SND06    XC    SVJCREAT,SVJCREAT                                                
         MVI   ELCODE,RACELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   SND10                                                            
         XR    R0,R0                                                            
                                                                                
SND07    CLI   RACTYPE,RACTADD     ADDED=SUBMITTED?                             
         BE    SND08                                                            
         IC    R0,RACLN                                                         
         AR    R6,R0                                                            
         CLI   RACEL,0                                                          
         BE    SND10                                                            
         B     SND07                                                            
                                                                                
SND08    MVC   SVJCREAT,RACPERS                                                 
                                                                                
SND10    MVI   CNTAEX,0                                                         
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   SND12                                                            
         USING RSTELD,R6                                                        
SNDFLT1  CLI   RSTFILT1,C' '                                                    
         BNH   SNDFLT2                                                          
         GOTO1 ASNDDATA,DMCB,6,RSTFILT1   FILTERS                               
SNDFLT2  CLI   RSTFILT2,C' '                                                    
         BNH   SNDFLT3                                                          
         GOTO1 ASNDDATA,DMCB,7,RSTFILT2                                         
SNDFLT3  CLI   RSTFILT3,C' '                                                    
         BNH   SNDFLT4                                                          
         GOTO1 ASNDDATA,DMCB,8,RSTFILT3                                         
SNDFLT4  CLI   RSTFILT4,C' '                                                    
         BNH   SNDFLT5                                                          
         GOTO1 ASNDDATA,DMCB,9,RSTFILT4                                         
SNDFLT5  CLI   RSTFILT5,C' '                                                    
         BNH   SND11                                                            
         GOTO1 ASNDDATA,DMCB,10,RSTFILT5                                        
                                                                                
SND11    MVI   FAJBSTAT,C' '        JOB STATUS                                  
         TM    RSTSTAT1,RSTSACIL                                                
         BNO   *+8                                                              
         MVI   FAJBSTAT,C'L'                                                    
         TM    RSTSTAT1,RSTSACIC                                                
         BNO   *+8                                                              
         MVI   FAJBSTAT,C'C'                                                    
                                                                                
         MVI   FAEXCTMP,0           EXCLUDE FROM TEMPO                          
         MVI   FAJULDEF,C'N'        JOB=LANGUAGE                                
         MVI   FAJLEST,C'N'         JOB LOCKS                                   
         MVI   FAJLORD,C'N'                                                     
         MVI   FAJLEXT,C'N'                                                     
         MVI   FAJLTSI,C'N'                                                     
         MVI   FAJLADJ,C'N'                                                     
         MVI   FAJLBIL,C'N'                                                     
         MVI   FAPRIOTY,0           PRIORITY                                    
         CLI   RSTLN,RSTLN3Q                                                    
         BL    SND12                                                            
                                                                                
         MVC   CNTAEX,RSTEXCNT                                                  
                                                                                
         TM    RSTLSTAT,RSTLSESQ                                                
         BZ    *+8                                                              
         MVI   FAJLEST,C'Y'                                                     
         TM    RSTLSTAT,RSTLSORQ                                                
         BZ    *+8                                                              
         MVI   FAJLORD,C'Y'                                                     
         TM    RSTLSTAT,RSTLSBIQ                                                
         BZ    *+8                                                              
         MVI   FAJLBIL,C'Y'                                                     
         TM    RSTLSTAT,RSTLSTIQ                                                
         BZ    *+8                                                              
         MVI   FAJLTSI,C'Y'                                                     
         TM    RSTLSTAT,RSTLSADQ                                                
         BZ    *+8                                                              
         MVI   FAJLADJ,C'Y'                                                     
         TM    RSTLSTAT,RSTLSEXQ                                                
         BZ    *+8                                                              
         MVI   FAJLEXT,C'Y'                                                     
                                                                                
         TM    RSTSTAT5,RSTSNOTS                                                
         BNO   *+8                                                              
         MVI   FAEXCTMP,1                                                       
         TM    RSTSTAT6,RSTSDTOP                                                
         BNO   *+8                                                              
         MVI   FAPRIOTY,1                                                       
                                                                                
SND12    MVI   ELCODE,ASTELQ       JOB=LANGUAGE                                 
         BAS   RE,GETELIO                                                       
         BNE   SND12A                                                           
         USING ASTELD,R6                                                        
         TM    ASTSTAT1,ASTISFOR                                                
         BZ    SND12A                                                           
         MVI   FAJULDEF,C'Y'                                                    
                                                                                
SND12A   MVI   ELCODE,PPRELQ       OFFICE                                       
         BAS   RE,GETELIO                                                       
         BNE   SND13                                                            
         USING PPRELD,R6                                                        
         CLI   PPRGAOFF,C' '       ANY OFFICE?                                  
         BNH   SND13                                                            
         MVC   CLPROFF,PPRGAOFF                                                 
                                                                                
SND13    CLC   CLPROFF,BLANKS                                                   
         BNH   SND14                                                            
         LA    R2,1                                                             
         CLI   CLPROFF+1,C' '                                                   
         BNH   *+8                                                              
         LA    R2,2                                                             
         GOTO1 ASNDDATA,DMCB,11,((R2),CLPROFF)                                  
                                                                                
SND14    MVI   ELCODE,JOBELQ       JOB OPEN AND CLOSE DATES                     
         XC    JADDATE,JADDATE                                                  
         XC    FAJBIDNO,FAJBIDNO                                                
         MVI   FAJARSTA,C'A'                                                    
         L     R1,AIO                                                           
         TM    ACCOSTAT(R1),ACTSDRFT                                            
         BZ    *+8                                                              
         MVI   FAJARSTA,C'D'                                                    
         BAS   RE,GETELIO                                                       
         BNE   SND16                                                            
         USING JOBELD,R6                                                        
         CLI   SNDMODE,FQACTCHA    FROM CHANGE - SHOW STATUS/ID NO              
         BE    SNDCDTX                                                          
SNDCDT   GOTO1 ASNDDATA,DMCB,12,JOBCDATE                                        
SNDCDTX  CLI   JOBLN,JOBLN1Q       ANY OPEN DATE?                               
         BNH   SND16                                                            
         XOUT  JOBREVNO,FAJBIDNO,2                                              
         MVC   JADDATE,JOBADATE                                                 
         CLI   JOBODATE,X'00'      OPEN DATE BINARY ZEROS?                      
         BNH   SND15                                                            
         CLI   SNDMODE,FQACTCHA    FROM CHANGE - SHOW STATUS ONLY               
         BE    SND15                                                            
SNDODT   GOTO1 ASNDDATA,DMCB,13,JOBODATE                                        
SND15    CLI   JOBLN,JOBLN3Q                                                    
         BL    SND16                                                            
         TM    JOBSTA2,JOBSREJ                                                  
         BZ    SND16                                                            
         MVI   FAJARSTA,C'R'                                                    
                                                                                
SND16    CLI   FAJBSTAT,C' '                                                    
         BNH   SND20                                                            
         CLI   SNDMODE,FQACTCHA    FROM CHANGE - SEND STATUS/ID NO              
         BE    SND150                                                           
         GOTO1 ASNDDATA,DMCB,14,FAJBSTAT                                        
                                                                                
SND20    MVI   FAARCOML,0                                                       
         CLI   FAJARSTA,C'R'                                                    
         BE    SND22                                                            
         CLI   FAJARSTA,C'A'                                                    
         BNE   SND30                                                            
SND22    MVI   ELCODE,STCELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    SND26                                                            
         B     SND30                                                            
         USING STCELD,R6                                                        
SND24    XR    R0,R0                                                            
         IC    R0,STCLN                                                         
         AR    R6,R0                                                            
         CLI   STCEL,0                                                          
         BE    SND30                                                            
         CLI   STCEL,STCELQ                                                     
         BNE   SND24                                                            
SND26    CLI   STCIND,STCIJOB                                                   
         BNE   SND24                                                            
         CLC   STCDTO,FAJARSTA                                                  
         BNE   SND24                                                            
         CLI   STCLN,STCLN1Q                                                    
         BNH   SND30                                                            
         XR    R2,R2                                                            
         IC    R2,STCLN                                                         
         SHI   R2,STCLN1Q+1                                                     
         MVC   FAARCOMM,BLANKS                                                  
         MVC   FAARCOMM(0),STCCOMM                                              
         EX    R2,*-6                                                           
         AHI   R2,1                                                             
         STC   R2,FAARCOML                                                      
                                                                                
SND30    MVI   ELCODE,PPRELQ       PRINT ON BILL                                
         BAS   RE,GETELIO                                                       
         BNE   SND50                                                            
         USING PPRELD,R6                                                        
                                                                                
         LA    R2,L'PPRBILLP       R2 = LENGTH OF DATA                          
         GOTO1 ASNDDATA,DMCB,15,((R2),PPRBILLP)                                 
                                                                                
         XR    R5,R5               OTHER INFO 1                                 
         IC    R5,PPRLN                                                         
         LA    RF,PPRLN1Q                                                       
         SR    R5,RF               R5 = TOTAL LENGTH OF OTHER INF DATA          
         BNH   SND50               NO DATA                                      
         LA    R2,50                                                            
         GOTO1 ASNDDATA,DMCB,16,((R2),PPRNARRP)                                 
                                                                                
SND32    XR    R5,R5               OTHER INFO 2                                 
         IC    R5,PPRLN                                                         
         LA    RF,PPRLN1Q                                                       
         SR    R5,RF               R5 = TOTAL LENGTH OF OTHER INF DATA          
         CHI   R5,50               2 LINES OF TEXT?                             
         BNH   SND50                                                            
         LA    R2,50               R2 = LENGTH                                  
         GOTO1 ASNDDATA,DMCB,17,((R2),PPRNARRP+50)                              
                                                                                
SND34    XR    R5,R5               OTHER INFO 3                                 
         IC    R5,PPRLN                                                         
         LA    RF,PPRLN1Q                                                       
         SR    R5,RF               R5 = TOTAL LENGTH OF OTHER INF DATA          
         CHI   R5,100              3 LINES OF TEXT?                             
         BNH   SND50                                                            
         LA    R2,50               R2 = LENGTH                                  
         GOTO1 ASNDDATA,DMCB,18,((R2),PPRNARRP+100)                             
                                                                                
SND50    DS    0H                  COMMENTS                                     
         MVI   ELCODE,SCMELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   SND60               END                                          
         USING SCMELD,R6                                                        
         LA    R0,3                NUMBER OF LINES                              
         MVI   DUB,C'N'                                                         
         MVI   DUB+1,1                                                          
         LA    R3,FAJOBCM1         R3 = 1ST COMMENTS LINE                       
         LA    R2,FAJOBCM1         R3 = 1ST COMMENTS LINE                       
                                                                                
SND51    CLI   SCMTYPE,0           NORMAL COMMENTS?                             
         BNE   SND53                                                            
         CLI   DUB,C'N'                                                         
         BE    SND52                                                            
         BAS   RE,SND57                                                         
         MVI   DUB,C'N'                                                         
                                                                                
SND52    XR    R1,R1                                                            
         IC    R1,SCMLN                                                         
         SHI   R1,5                                                             
         MVC   0(0,R3),SCMNARR                                                  
         EX    R1,*-6                                                           
         BAS   RE,SND57                                                         
         B     SND56                                                            
                                                                                
SND53    LA    RE,SCMCODE                                                       
         LA    R1,6                                                             
                                                                                
SND54    CLI   0(RE),C' '                                                       
         BNE   SND55                                                            
         AHI   RE,1                                                             
         BCT   R1,SND54                                                         
         B     SND56                                                            
                                                                                
SND55    MVI   DUB,C'Y'            MARK AS NEW STYLE COMMENTS                   
         STC   R1,DUB+2            R1 = L'COMMENT                               
         ST    RE,DUB+4            RE = A(1ST SIGNIFICANT BYTE)                 
         AHI   R1,5                R1 = TOTAL LENGTH DISPLAYED ITEM             
         AR    R1,R3               R1 = A(LAST BYTE + 1 OF DSPLYD ITEM)         
         LR    RE,R2               RE = A(HEADER)                               
         SR    R1,RE                                                            
         CHI   R1,58                                                            
         BNH   *+8                 IF NOT SUFFICIENT ROOM ON THIS LINE          
         BAS   RE,SND57                                                         
         CLI   DUB+1,1                                                          
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         MVI   DUB+1,2                                                          
         MVC   0(4,R3),=C'EST='    SET TYPE OF COMMENT                          
         TM    SCMTYPE,X'40'                                                    
         BO    *+10                                                             
         MVC   0(3,R3),=C'BIL'                                                  
         TM    SCMTYPE,X'C0'                                                    
         BM    *+10                                                             
         MVC   0(3,R3),=C'B+E'                                                  
         L     RE,DUB+4                                                         
         XR    R1,R1                                                            
         IC    R1,DUB+2                                                         
         BCTR  R1,0                                                             
         MVC   4(0,R3),0(RE)      DISPLAY NUMBER                                
         EX    R1,*-6                                                           
         LA    R3,5(R1,R3)                                                      
                                                                                
SND56    XR    R1,R1                                                            
         IC    R1,SCMLN                                                         
         AR    R6,R1                                                            
         CLI   SCMEL,0                                                          
         BE    SNDCOM                                                           
         CLI   SCMEL,SCMELQ                                                     
         BE    SND51                                                            
         B     SND56                                                            
                                                                                
SND57    LA    R2,L'FAJOBCM1(R2)                                                
         LA    R2,L'FLJOBCM1(R2)                                                
         LR    R3,R2                                                            
         MVI   DUB+1,1                                                          
         BCT   R0,*+8                                                           
         B     *+6                                                              
         BR    RE                                                               
                                                                                
SNDCOM   LA    R2,50                                                            
                                                                                
         GOTO1 ASNDDATA,DMCB,19,((R2),FAJOBCM1)                                 
         GOTO1 ASNDDATA,DMCB,20,((R2),FAJOBCM2)                                 
         GOTO1 ASNDDATA,DMCB,21,((R2),FAJOBCM3)                                 
                                                                                
SND60    MVI   ELCODE,UFSELQ       USER FIELD DATA                              
         BAS   RE,GETELIO                                                       
         BNE   SND70                                                            
         USING UFSELD,R6                                                        
                                                                                
         XR    R2,R2                                                            
         IC    R2,UFSLN                                                         
         LA    RF,UFSLN1Q                                                       
         SR    R2,RF                                                            
                                                                                
         CHI   R2,0                                                             
         BNH   SNDUSX1                                                          
*&&US*&& LA    R3,UFSDATA                                                       
*&&US*&& BAS   RE,SND61                                                         
SNDUSR1  GOTO1 ASNDDATA,DMCB,22,UFSCODE                                         
         GOTO1 ASNDDATA,DMCB,23,((R2),(R3))                                     
SNDUSX1  BAS   RE,SND62                                                         
                                                                                
         CHI   R2,0                                                             
         BNH   SNDUSX2                                                          
*&&US*&& LA    R3,UFSDATA                                                       
*&&US*&& BAS   RE,SND61                                                         
SNDUSR2  GOTO1 ASNDDATA,DMCB,24,UFSCODE                                         
         GOTO1 ASNDDATA,DMCB,25,((R2),(R3))                                     
SNDUSX2  BAS   RE,SND62                                                         
                                                                                
         CHI   R2,0                                                             
         BNH   SNDUSX3                                                          
*&&US*&& LA    R3,UFSDATA                                                       
*&&US*&& BAS   RE,SND61                                                         
SNDUSR3  GOTO1 ASNDDATA,DMCB,26,UFSCODE                                         
         GOTO1 ASNDDATA,DMCB,27,((R2),(R3))                                     
SNDUSX3  BAS   RE,SND62                                                         
                                                                                
         CHI   R2,0                                                             
         BNH   SNDUSX4                                                          
*&&US*&& LA    R3,UFSDATA                                                       
*&&US*&& BAS   RE,SND61                                                         
SNDUSR4  GOTO1 ASNDDATA,DMCB,28,UFSCODE                                         
         GOTO1 ASNDDATA,DMCB,29,((R2),(R3))                                     
SNDUSX4  BAS   RE,SND62                                                         
                                                                                
         CHI   R2,0                                                             
         BNH   SNDUSX5                                                          
*&&US*&& LA    R3,UFSDATA                                                       
*&&US*&& BAS   RE,SND61                                                         
SNDUSR5  GOTO1 ASNDDATA,DMCB,30,UFSCODE                                         
         GOTO1 ASNDDATA,DMCB,31,((R2),(R3))                                     
SNDUSX5  BAS   RE,SND62                                                         
                                                                                
         CHI   R2,0                                                             
         BNH   SNDUSX6                                                          
*&&US*&& LA    R3,UFSDATA                                                       
*&&US*&& BAS   RE,SND61                                                         
SNDUSR6  GOTO1 ASNDDATA,DMCB,32,UFSCODE                                         
         GOTO1 ASNDDATA,DMCB,33,((R2),(R3))                                     
SNDUSX6  BAS   RE,SND62                                                         
                                                                                
         CHI   R2,0                                                             
         BNH   SNDUSX7                                                          
*&&US*&& LA    R3,UFSDATA                                                       
*&&US*&& BAS   RE,SND61                                                         
SNDUSR7  GOTO1 ASNDDATA,DMCB,34,UFSCODE                                         
         GOTO1 ASNDDATA,DMCB,35,((R2),(R3))                                     
SNDUSX7  BAS   RE,SND62                                                         
                                                                                
         CHI   R2,0                                                             
         BNH   SNDUSX8                                                          
*&&US*&& LA    R3,UFSDATA                                                       
*&&US*&& BAS   RE,SND61                                                         
SNDUSR8  GOTO1 ASNDDATA,DMCB,36,UFSCODE                                         
         GOTO1 ASNDDATA,DMCB,37,((R2),(R3))                                     
SNDUSX8  BAS   RE,SND62                                                         
                                                                                
         CHI   R2,0                                                             
         BNH   SNDUSX9                                                          
*&&US*&& LA    R3,UFSDATA                                                       
*&&US*&& BAS   RE,SND61                                                         
SNDUSR9  GOTO1 ASNDDATA,DMCB,38,UFSCODE                                         
         GOTO1 ASNDDATA,DMCB,39,((R2),(R3))                                     
SNDUSX9  BAS   RE,SND62                                                         
                                                                                
         CHI   R2,0                                                             
         BNH   SND70                                                            
*&&US*&& LA    R3,UFSDATA                                                       
*&&US*&& BAS   RE,SND61                                                         
SNDUSR10 GOTO1 ASNDDATA,DMCB,40,UFSCODE                                         
         GOTO1 ASNDDATA,DMCB,41,((R2),(R3))                                     
         B     SND70                                                            
                                                                                
*&&US                                                                           
SND61    CLI   UFSEDIT,C'D'        IS IT A DATE TYPE ?                          
         BNER  RE                                                               
         ST    RE,SAVERE           SAVE RETURN ADDRESS                          
         GOTO1 DATVAL,DMCB,(0,UFSDATA),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(0,WORK),(5,WORK+8)                                  
         LA    R3,WORK+8                                                        
         LA    R2,8                                                             
         L     RE,SAVERE           RESTORE RETURN ADDRESS                       
         BR    RE                                                               
*&&                                                                             
                                                                                
SND62    XR    R2,R2                                                            
         IC    R2,UFSLN                                                         
                                                                                
         AR    R6,R2                                                            
         CLI   UFSEL,0                                                          
         BE    SND70                                                            
         CLI   UFSEL,UFSELQ                                                     
         BNE   SND62                                                            
                                                                                
         IC    R2,UFSLN                                                         
         LA    RF,UFSLN1Q                                                       
         SR    R2,RF               R2 = DATA LENGTH                             
         BR    RE                                                               
                                                                                
SND70    DS    0H                                                               
         MVI   ELCODE,ADRELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   SND88                                                            
         USING ADRELD,R6                                                        
                                                                                
SND72    LA    R3,ADRADD1          R3 -> START OF DATA                          
         LA    R2,L'ADRADD1        R2 = LENGTH                                  
         GOTO1 ASNDDATA,DMCB,42,((R2),ADRADD1)                                  
                                                                                
         CLI   ADRNUM,2                                                         
         BL    SND88                                                            
                                                                                
SND74    LA    R3,ADRADD2          R3 -> START OF DATA                          
         LA    R2,L'ADRADD2        R2 = LENGTH                                  
         GOTO1 ASNDDATA,DMCB,43,((R2),ADRADD2)                                  
                                                                                
         CLI   ADRNUM,3                                                         
         BL    SND88                                                            
                                                                                
SND76    LA    R3,ADRADD3          R3 -> START OF DATA                          
         LA    R2,L'ADRADD3        R2 = LENGTH                                  
         GOTO1 ASNDDATA,DMCB,44,((R2),ADRADD3)                                  
                                                                                
         CLI   ADRNUM,4                                                         
         BL    SND88                                                            
                                                                                
SND78    LA    R3,ADRADD4          R3 -> START OF DATA                          
         LA    R2,L'ADRADD4        R2 = LENGTH                                  
         GOTO1 ASNDDATA,DMCB,45,((R2),ADRADD4)                                  
                                                                                
         CLI   ADRNUM,5                                                         
         BL    SND88                                                            
                                                                                
SND80    LA    R3,ADRADD5          R3 -> START OF DATA                          
         LA    R2,L'ADRADD5        R2 = LENGTH                                  
         GOTO1 ASNDDATA,DMCB,46,((R2),ADRADD5)                                  
*                                                                               
SND88    GOTOR DOCPOS              130-169/CLIENT POS                           
         GOTOR DOFAMT              170/FEE BUDGET AMOUNT                        
*                                                                               
SND90    GOTO1 ASNDDATA,DMCB,47,FAEXCTMP                                        
         GOTO1 ASNDDATA,DMCB,129,FAJULDEF                                       
         GOTO1 ASNDDATA,DMCB,171,FAJLEST                                        
         GOTO1 ASNDDATA,DMCB,172,FAJLORD                                        
         GOTO1 ASNDDATA,DMCB,173,FAJLBIL                                        
         GOTO1 ASNDDATA,DMCB,174,FAJLTSI                                        
         GOTO1 ASNDDATA,DMCB,175,FAJLADJ                                        
         GOTO1 ASNDDATA,DMCB,176,FAJLEXT                                        
         GOTO1 ASNDDATA,DMCB,56,FAPRIOTY                                        
*                                                                               
         MVI   ELCODE,PACELQ                                                    
         GOTO1 GETELIO                                                          
         BNE   SND100                                                           
         USING PACELD,R6                                                        
         LA    R2,L'PACPERS                                                     
         GOTO1 ASNDDATA,DMCB,53,((R2),PACPERS)                                  
         GOTO1 ASNDDATA,DMCB,54,PACDATE                                         
                                                                                
SND100   XC    FACAMPCD,FACAMPCD                                                
         MVI   ELCODE,FFTELQ                                                    
         GOTO1 GETELIO                                                          
         BNE   SND110                                                           
         XR    RF,RF                                                            
         USING FFTELD,R6                                                        
SND102   CLI   FFTEL,0             EOR ?                                        
         BE    SND110                                                           
         CLI   FFTEL,FFTELQ        IS IT FFTEL ?                                
         BNE   SND106                                                           
         CLI   FFTTYPE,FFTTPCOM    IS IT RIGHT TYPE ?                           
         BNE   SND104                                                           
         XR    R2,R2                                                            
         IC    R2,FFTDLEN                                                       
         GOTO1 ASNDDATA,DMCB,55,((R2),FFTDATA)                                  
         B     SND106                                                           
                                                                                
SND104   CLI   FFTTYPE,FFTTCAMP                                                 
         BNE   SND106                                                           
         MVC   FACAMPCD,FFTDATA                                                 
                                                                                
SND106   XR    RF,RF               BUMP TO NEXT ELEMENT                         
         IC    RF,FFTLN                                                         
         AR    R6,RF                                                            
         B     SND102                                                           
                                                                                
SND110   DS    0H                                                               
         BAS   RE,SNDJLD           (SEE ALSO FOR AEXRECD)                       
                                                                                
         USING ACTRECD,R6                                                       
SND120   DS    0H                  READ CLIENT NAME                             
         LA    R6,KEY                                                           
         MVC   ACTKEY,BLANKS                                                    
         MVC   ACTKEY(L'SCLIKHD),SCLIKHD                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         TM    COMPSTAC,CPYSALTN                                                
         BZ    SND122                                                           
         MVI   ELCODE,XNMELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   SND128                                                           
         USING XNMELD,R6                                                        
         XR    R2,R2                                                            
         IC    R2,XNMSUBL                                                       
         GOTO1 ASNDDATA,DMCB,58,((R2),XNMSUBN)                                  
         B     SND130                                                           
*                                                                               
SND122   TM    COMPSTAC,CPYSMEDN                                                
         BZ    SND128                                                           
         MVI   ELCODE,SNMELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   SND128                                                           
         USING SNMELD,R6                                                        
         XR    R2,R2                                                            
         IC    R2,SNMLN                                                         
         SHI   R2,SNMLN1Q                                                       
         BM    SND128                                                           
         GOTO1 ASNDDATA,DMCB,58,((R2),SNMNAME)                                  
         B     SND130                                                           
*                                                                               
SND128   MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   SND130                                                           
         USING NAMELD,R6                                                        
         XR    R2,R2                                                            
         IC    R2,NAMLN                                                         
         SHI   R2,2                                                             
         BM    SND130                                                           
         GOTO1 ASNDDATA,DMCB,58,((R2),NAMEREC)                                  
                                                                                
         USING ACTRECD,R6                                                       
SND130   DS    0H                  READ PRODUCT NAME                            
         LA    R6,KEY                                                           
         MVC   ACTKEY,BLANKS                                                    
         MVC   ACTKEY(L'SPROKHD),SPROKHD                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         TM    COMPSTAC,CPYSALTN                                                
         BZ    SND132                                                           
         MVI   ELCODE,XNMELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   SND138                                                           
         USING XNMELD,R6                                                        
         XR    R2,R2                                                            
         IC    R2,XNMSUBL                                                       
         GOTO1 ASNDDATA,DMCB,58,((R2),XNMSUBN)                                  
         B     SND140                                                           
*                                                                               
SND132   TM    COMPSTAC,CPYSMEDN                                                
         BZ    SND138                                                           
         MVI   ELCODE,SNMELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   SND138                                                           
         USING SNMELD,R6                                                        
         XR    R2,R2                                                            
         IC    R2,SNMLN                                                         
         SHI   R2,SNMLN1Q                                                       
         BM    SND138                                                           
         GOTO1 ASNDDATA,DMCB,58,((R2),SNMNAME)                                  
         B     SND140                                                           
*                                                                               
SND138   MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   SND140                                                           
         USING NAMELD,R6                                                        
         XR    R2,R2                                                            
         IC    R2,NAMLN                                                         
         SHI   R2,2                                                             
         BM    SND140                                                           
         GOTO1 ASNDDATA,DMCB,59,((R2),NAMEREC)                                  
                                                                                
         USING PMDRECD,R6                                                       
SND140   DS    0H                  READ MEDIA CODE NAME                         
         LA    R6,KEY                                                           
         MVC   PMDKEY,BLANKS                                                    
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,SPROKHD                                                  
         MVC   PMDKMED,FAJOB                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVI   ELCODE,PMDELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   SND142                                                           
         LA    R2,L'PMDKMED        SEND MEDIA CODE                              
         GOTO1 ASNDDATA,DMCB,60,((R2),FAJOB)                                    
         USING PMDELD,R6                                                        
         LA    R2,L'PMDDESC        SEND MEDIA DESCRITION                        
         GOTO1 ASNDDATA,DMCB,61,((R2),PMDDESC)                                  
                                                                                
SND142   OC    JADDATE,JADDATE                                                  
         BZ    SND150                                                           
         GOTO1 ASNDDATA,DMCB,62,JADDATE                                         
                                                                                
SND150   GOTO1 ASNDDATA,DMCB,63,FAJARSTA                                        
         GOTO1 ASNDDATA,DMCB,64,FAJBIDNO                                        
                                                                                
         MVI   FAJBAPPR,C'N'                                                    
                                                                                
         CLI   FAJARSTA,C'A'                                                    
         BE    SND180                                                           
                                                                                
         USING FACTSD,R1                                                        
         GOTO1 GETFACT,DMCB,0,0                                                 
         L     R1,0(R1)                                                         
         MVC   DUB(2),FAPASSWD                                                  
         DROP  R1                                                               
*&&DO                                                                           
         OC    SVJCREAT,SVJCREAT                                                
         BZ    SND152                                                           
         CLC   SVJCREAT,DUB        SUBMITTER CANNOT BE APPROVER                 
         BE    SND180                                                           
*&&                                                                             
         USING APPRECD,R6                                                       
SND152   LA    R6,KEY                                                           
         XC    APPKEY,APPKEY                                                    
         MVI   APPKTYP,APPKTYPQ                                                 
         MVI   APPKSUB,APPKSUBQ                                                 
         MVC   APPKCPY,SPROKHD                                                  
         MVC   APPKPIDB,DUB                                                     
         MVI   APPKSEQ,0                                                        
         MVI   RDUPDATE,C'N'                                                    
         MVI   FAJBAPPR,C'N'                                                    
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,APPRECD,APPRECD                       
         BNE   SND180                                                           
         TM    APPKSTA2,APPSRJAQ+APPSDJAQ                                       
         BZ    SND162              SKIP IF NO JOB APPROVER                      
         MVI   FAJBAPPR,C'Y'                                                    
         B     SND180                                                           
                                                                                
SND162   MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVI   ELCODE,LIDELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   SND180                                                           
         USING LIDELD,R6                                                        
         XR    R0,R0                                                            
                                                                                
SND164   CLI   LIDEL,0                                                          
         BE    SND180              EXIT NEQ IF NO MATCHING SJ ENTRY             
         CLI   LIDEL,LIDELQ                                                     
         BNE   SND176                                                           
         CLI   LIDTYPE,LIDTAPSJ                                                 
         BNE   SND176                                                           
         CLI   LIDLN,LIDDATA-LIDELD                                             
         BNH   SND176                                                           
         XR    R5,R5                                                            
         IC    R5,LIDLN                                                         
         LA    R5,LIDELD(R5)       POINT BEYOND LAST ENTRY                      
         LA    R2,LIDDATA          POINT TO FIRST ENTRY                         
                                                                                
SND166   CR    R2,R5               END OF ELEMENT?                              
         BNL   SND176                                                           
         TM    0(R2),LIDAJOBY+LIDAJOBD                                          
         BZ    SND174                                                           
         MVI   FAJBAPPR,C'N'                                                    
         DS    0H                  CHECK CURRENT JOB AGAINST ENTRY              
         LLC   R1,LCLI             DETERMINE LIMLIST ENTRY LEVEL                
         LR    RF,R1                                                            
         AR    RF,R2                                                            
         CLI   1(RF),C' '                                                       
         BE    SND170                                                           
         LLC   RF,LPRO                                                          
         AR    RF,R1                                                            
         LR    R1,RF                                                            
         AR    RF,R2                                                            
         CLI   1(RF),C' '                                                       
         BE    SND170                                                           
         LA    R1,L'ACTKACT                                                     
                                                                                
SND170   SHI   R1,1                                                             
         EX    R1,SND172                                                        
         BNE   SND174                                                           
         MVI   FAJBAPPR,C'Y'                                                    
         B     SND180              EXIT OK IF MATCH FOUND                       
                                                                                
SND172   CLC   1(0,R2),SAVEJOB                                                  
                                                                                
SND174   AHI   R2,L'LIDASTAT+L'LIDASJAC                                         
         B     SND166                                                           
                                                                                
SND176   IC    R0,LIDLN                                                         
         AR    R6,R0                                                            
         B     SND164                                                           
                                                                                
SND180   GOTO1 ASNDDATA,DMCB,65,FAJBAPPR                                        
         XR    R2,R2                                                            
         IC    R2,FAARCOML                                                      
         LTR   R2,R2                                                            
         BNP   SND182                                                           
         GOTO1 ASNDDATA,DMCB,66,((R2),FAARCOMM)                                 
                                                                                
SND182   OC    FACAMPCD,FACAMPCD                                                
         BZ    SND184                                                           
         GOTO1 ASNDDATA,DMCB,67,((R2),FACAMPCD)                                 
                                                                                
         USING CPNPASD,R6                                                       
         LA    R6,KEY                                                           
         XC    CPNPAS(ACCKLEN),CPNPAS                                           
         MVI   CPNPTYP,CPNPTYPQ                                                 
         MVI   CPNPSUB,CPNPSUBQ                                                 
         MVC   CPNPCPY,SPROKHD                                                  
         MVC   CPNPCODE,FACAMPCD                                                
         XC    CPNPCODE,=X'FFFFFFFF'                                            
         MVC   SAVEKEY,KEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,CPNPASD,CPNPASD                       
         BNE   SND184                                                           
         CLC   CPNPAS(CPNPACT-CPNPASD),SAVEKEY                                  
         BNE   SND184                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   SND184                                                           
         USING NAMELD,R6                                                        
         XR    R2,R2                                                            
         IC    R2,NAMLN                                                         
         SHI   R2,2                                                             
         GOTO1 ASNDDATA,DMCB,68,((R2),NAMEREC)                                  
                                                                                
         USING AEXRECD,R6                                                       
SND184   CLI   CNTAEX,0            ANY EXTENSION RECORDS?                       
         BE    SND190                                                           
         LLC   R4,CNTAEX                                                        
                                                                                
SND186   LA    R6,KEY                                                           
         XC    AEXKEY,AEXKEY                                                    
         MVI   AEXKTYP,AEXKTYPQ                                                 
         MVI   AEXKSUB,AEXKSUBQ                                                 
         MVC   AEXKCPY,CUL                                                      
         MVC   AEXKULC,CUL+1                                                    
         MVC   AEXKACC,SAVEJOB                                                  
         STC   R4,AEXKSEQ                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         BNE   SND188                                                           
         BAS   RE,SNDJLD           JLDELD ON EXTENSION RECORD                   
         DS    0H                  -> OTHER DATA HERE                           
                                                                                
SND188   BCT   R4,SND186                                                        
                                                                                
SND190   LA    R2,73                 DEAL WITH DATA FIELDS 73 TO 128            
         LA    R3,FARNUM                                                        
         LHI   RF,FAROLE1-LINKD                                                 
         LA    RF,LINKD(RF)                                                     
SND192   OC    0(L'FAROLE1,RF),0(RF)                                            
         BZ    SND198                                                           
         GOTO1 ASNDDATA,DMCB,(R2),(RF)                                          
         AHI   RF,L'FAROLE1                                                     
         AHI   R2,1                                                             
         GOTO1 ASNDDATA,DMCB,(R2),(RF)                                          
         AHI   RF,L'FARLNM1                                                     
         AHI   R2,1                                                             
         GOTO1 ASNDDATA,DMCB,(R2),(RF)                                          
         AHI   RF,L'FAPCOD1                                                     
         AHI   R2,1                                                             
         GOTO1 ASNDDATA,DMCB,(R2),(RF)                                          
         AHI   RF,L'FARLEV1                                                     
         AHI   R2,1                                                             
         BCT   R3,SND192                                                        
                                                                                
SND198   DS    0H                  (NEXT HERE)                                  
                                                                                
SND200   B     EXITY                                                            
         DROP  R6                                                               
                                                                                
         USING JLDELD,R6                                                        
SNDJLD   NTR1                                                                   
         MVI   ELCODE,JLDELQ       LONG JOB DESCRIPTION                         
         GOTO1 GETELIO                                                          
         BNE   SNDJLDX                                                          
                                                                                
SNDJLD2  LLC   R2,JLDLN                                                         
         SHI   R2,JLDLNQ                                                        
         LA    RF,57                                                            
         CLI   JLDSEQ,0                                                         
         BE    SNDJLD4                                                          
         LA    RF,69                                                            
         CLI   JLDSEQ,1                                                         
         BE    SNDJLD4                                                          
         LA    RF,70                                                            
         CLI   JLDSEQ,2                                                         
         BE    SNDJLD4                                                          
         LA    RF,71                                                            
         CLI   JLDSEQ,3                                                         
         BE    SNDJLD4                                                          
         LA    RF,72                                                            
         CLI   JLDSEQ,4                                                         
         BNE   SNDJLD6                                                          
                                                                                
SNDJLD4  GOTO1 ASNDDATA,DMCB,(RF),((R2),JLDDESC)                                
                                                                                
SNDJLD6  LLC   RE,JLDLN                                                         
         AR    R6,RE                                                            
         CLI   JLDEL,0                                                          
         JE    SNDJLDX                                                          
         CLI   JLDEL,JLDELQ                                                     
         BE    SNDJLD2                                                          
         B     SNDJLD6                                                          
                                                                                
SNDJLDX  B     EXITY                                                            
         DROP  R6                                                               
                                                                                
GETRLNM  ST    RE,SAVERE                                                        
         L     R6,AIO                                                           
         USING ROLRECD,R6                                                       
         XC    ROLKEY,ROLKEY                                                    
         MVI   ROLKTYP,ROLKTYPQ                                                 
         MVI   ROLKSUB,ROLKSUBQ                                                 
         MVC   ROLKCPY,CUL                                                      
         MVC   ROLKOFF,SAVOFF                                                   
         MVC   ROLKNUM,HALF                                                     
         MVC   SAVEKEY,0(R6)                                                    
         GOTO1 DATAMGR,DMCB,DMREAD,ACCOUNT,(R6),(R6),DMWORK                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,ACCORFST(R6)                                                  
         USING NAMELD,R6                                                        
         XR    R0,R0                                                            
GETRL02  CLI   NAMEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NAMEL,NAMELQ                                                     
         BE    *+14                                                             
         IC    R0,NAMLN                                                         
         AR    R6,R0                                                            
         B     GETRL02                                                          
         MVC   WORK(L'FARLNM),BLANKS                                            
         SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         MVC   WORK(0),NAMEREC                                                  
         EX    RF,*-6                                                           
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R6                                                               
                                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                        *         
***********************************************************************         
VKEY     NTR1                                                                   
         GOTO1 GETFACT,DMCB,0,0                                                 
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   SVPASSWD,FAPASSWD                                                
         MVC   SVTIME,FATIME                                                    
*&&US*&& AP    SVTIME,=P'60000'                                                 
         DROP  R1                                                               
         MVC   AIO,AIO2            SWAP IO AREA                                 
         GOTO1 AGETLDGS,(RC)                                                    
         MVC   KEY,BLANKS                                                       
         MVC   KEY(1),CUL                                                       
         GOTO1 READ                                                             
         MVI   ELCODE,ACMPELQ                                                   
         BAS   RE,GETELIO                                                       
         USING CPYELD,R6                                                        
         MVC   PRODLEDG,CPYPROD    GET LEDGERS AND STATUS                       
         MVC   CMPSTAT1,CPYSTAT1                                                
         XC    CMPDJCDO,CMPDJCDO                                                
         CLI   CPYLN,CPYLN4Q                                                    
         BL    *+10                                                             
         MVC   CMPDJCDO,CPYDJCDO                                                
*                                                                               
         MVC   DUMFLDHL,FLCLIENT                                                
         MVC   DUMFLD,BLANKS                                                    
         MVC   DUMFLD(L'FACLIENT),FACLIENT                                      
                                                                                
         LA    R2,DUMFLDH          ADDRESS CLIENT                               
         MVI   NOLIMTST,X'FF'      DONT CHECK LIMIT ACCESS SECURITY             
         GOTO1 VALCLI              VALIDATE CLIENT, MOVE NAME,                  
         MVI   NOLIMTST,0                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'ACTKEY),BLANKS                                             
         MVC   KEY(L'CUL),CUL                                                   
         MVC   KEY+L'CUL(L'CLICODE),CLICODE                                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                REREAD THE CLIENT                            
         MVI   CLPRLK,0            INITIALIZE FLAG                              
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSTELD,R6                                                        
         TM    RSTSTAT1,RSTSACIL   TEST FOR LOCKED CLIENT                       
         BNO   *+8                 NO                                           
         OI    CLPRLK,CLILOKD      YES - SET FLAG                               
                                                                                
         MVC   DUMFLDHL,FLPROD                                                  
         MVC   DUMFLD,BLANKS                                                    
         MVC   DUMFLD(L'FAPROD),FAPROD                                          
                                                                                
         LA    R2,DUMFLDH          ADDRESS PRODUCT                              
         MVI   NOLIMTST,X'FF'      DONT CHECK LIMIT ACCESS                      
*&&UK*&& CLI   AGYCNTRY,CTRYGER    UNLESS GERMANY:                              
*&&US*&& CLI   AGYCTRY,CTRYGER                                                  
         BNE   *+8                                                              
         MVI   NOLIMTST,0          CHECK LIMIT ACCESS ON PRODUCT                
         GOTO1 VALPROD             VALIDATE PRODUCT, MOVE NAME,                 
         MVI   NOLIMTST,0                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'ACTKEY),BLANKS                                             
         MVC   KEY(L'CUL),CUL                                                   
         MVC   KEY+L'CUL(L'CLICODE),CLICODE                                     
         XR    RF,RF                                                            
         IC    RF,LCLI                                                          
         LA    RF,KEY+L'CUL(RF)                                                 
         MVC   0(L'PRODCODE,RF),PRODCODE                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                REREAD THE PRODUCT                           
         NI    CLPRLK,255-PROLOKD  INITIALIZE FLAG                              
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSTELD,R6                                                        
         TM    RSTSTAT1,RSTSACIL   TEST FOR LOCKED PRODUCT                      
         BNO   *+8                 NO                                           
         OI    CLPRLK,PROLOKD      YES - SET FLAG                               
         MVC   SAVEOFC,EFFOFFC     SAVE EFFECTIVE OFFICE FOR LABELS             
*                                                                               
         CLI   FAACTION,FQACTADD  IF 'DRAFT' AND ADD THEN ENSURE THAT           
         BE    VKEY0              DEFAULT JOB APPROVER EXISTS                   
         CLI   FAACTION,FQACTAUT                                                
         BNE   VKEY1                                                            
*                                                                               
VKEY0    TM    MYLSTA2,LDGSDRFT                                                 
         BZ    VKEY1                                                            
         GOTO1 AGETAPP,(RC)                                                     
         OC    MYAPPST,MYAPPST                                                  
         BNZ   VKEY1                                                            
         MVI   ERROR,NODEFAPP                                                   
         B     EXITN                                                            
*                                                                               
VKEY1    CLI   FAACTION,FQACTADD                                                
         BE    *+8                                                              
         CLI   FAACTION,FQACTAUT                                                
         BE    *+8                                                              
         CLI   FAACTION,FQACTNUM                                                
         BNE   VKEY2                                                            
         CLI   CLPRLK,0            TEST FOR CLIENT OR PRODUCT LOCKED            
         BE    VKEY2               NO                                           
         MVI   ERROR,CANTADDP      PRODUCT IS LOCKED                            
         TM    CLPRLK,PROLOKD                                                   
         BO    EXITN                                                            
         MVI   ERROR,CANTADDC      CLIENT IS LOCKED                             
         B     EXITN                                                            
                                                                                
VKEY2    GOTO1 DATCON,DMCB,(5,0),(0,TODAY)                                      
         XC    OPEN,OPEN                                                        
         XC    MYOPEN,MYOPEN                                                    
         XC    EFFDATE,EFFDATE                                                  
         OC    FAOPENDT,FAOPENDT                                                
         BZ    *+10                                                             
         MVC   MYOPEN,FAOPENDT                                                  
         OC    EFFDATE,MYOPEN                                                   
         BNZ   *+10                                                             
         OC    EFFDATE,TODAYP                                                   
         GOTO1 DATCON,DMCB,(1,EFFDATE),(0,OPEN)                                 
*                                                                               
         MVC   DUMFLDHL,FLJOB                                                   
         MVC   DUMFLD,BLANKS                                                    
         MVC   DUMFLD(L'FAJOB),FAJOB                                            
         LA    R2,DUMFLDH          ADDRESS JOB                                  
                                                                                
         MVI   ERROR,BADMEDIA                                                   
         CLI   FAJOB,C' '          DOES JOB START WITH A BLANK ?                
         BE    EXITN               YES, THIS IS AN ERROR                        
*&&UK                                                                           
         CLI   AGYCNTRY,CTRYGER    DISALLOW GERMAN CHARS                        
         BNE   VKEY2B                                                           
         MVI   ERROR,INVCHAR                                                    
         ZIC   R0,DUMFLDHL         R0=L'ACC CODE                                
         LA    RE,FAJOB            RE=A(ACC CODE)                               
VKEY2A2  LA    R1,L'CHARTAB        R1=L'CHARTAB                                 
         LA    RF,CHARTAB          RF=A(CHARTAB)                                
VKEY2A4  CLC   0(1,RE),0(RF)       IS CHARACTER DISALLOWED?                     
         BE    EXITN                                                            
         LA    RF,1(RF)            TRY NEXT FORBIDDEN CHAR                      
         BCT   R1,VKEY2A4                                                       
         LA    RE,1(RE)            TRY NEXT CHAR IN ACC CODE                    
         BCT   R0,VKEY2A2                                                       
*&&                                                                             
VKEY2B   CLI   FAACTION,FQACTADD                                                
         BE    VKEY2C                                                           
         CLI   FAACTION,FQACTAUT                                                
         BE    VKEY2C                                                           
         CLI   FAACTION,FQACTNUM                                                
         BE    VKEY2C                                                           
         MVI   NOLIMTST,0                                                       
         MVI   OPTION,C' '         DON'T GET NAME                               
         GOTO1 VALJOB                                                           
                                                                                
VKEY2C   GOTO1 VALMED                                                           
*                                                                               
         BRAS  RE,SETGET                                                        
         LA    R1,LGOBBLK                                                       
         STCM  R1,15,GOABEXT                                                    
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         ICM   R1,15,GOABEXT                                                    
*&&UK                                                                           
         MVC   PRODCURR,GOBILCUR-GOBBLOCK(R1)                                   
         TM    COMPSTA7,CPYSSCNV   IF EUROZONE AGENCY                           
         BNO   VKEY2D                                                           
         CLC   PRODCURR,COMPCURS   AND ESTIMATE CURRENCY NOT 2ND CURR           
         BE    VKEY2D                                                           
         MVC   PRODCURR,COMPCURP   MAKE SURE IT'S PRIMARY CURR                  
*&&                                                                             
VKEY2D   XC    GOABEXT,GOABEXT                                                  
         CLI   FAACTION,FQACTAUT   AUTO NUMBERING?                              
         BE    *+12                                                             
         CLI   FAACTION,FQACTNUM   AUTO NUMBERING?                              
         BNE   VKEY3               NO                                           
*&&UK*&& MVI   ERROR,JNUMNACT                                                   
*&&US*&& MVI   ERROR,NOTAUTO                                                    
         CLI   GOAUTNUM,C'Y'                                                    
         BNE   EXITN                                                            
         GOTO1 AFINDJOB,(RC)                                                    
         BNE   EXITN                                                            
         MVC   FAJOB,WORK                                                       
         MVI   FLJOB,6                                                          
                                                                                
VKEY3    XC    KEY,KEY             PREPARE KEY FOR ADD/WRITE                    
         MVC   KEY(L'ACTKEY),BLANKS                                             
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(6),CLICODE                                                 
         XR    R1,R1                                                            
         IC    R1,LCLI                                                          
         LA    R1,KEY+3(R1)                                                     
         MVC   0(6,R1),PRODCODE                                                 
         XR    R1,R1                                                            
         IC    R1,LCLIPRO                                                       
         LA    R1,KEY+3(R1)                                                     
         MVC   0(6,R1),WORK                                                     
         MVC   JOBNUM,WORK                                                      
         CLI   KEY+15,C' '         GUARD AGAINST KEY TOO LONG                   
         BNH   *+12                                                             
         MVI   ERROR,TOOLNG                                                     
         B     EXITN                                                            
*                                                                               
         CLI   FAACTION,FQACTADD                                                
         BNE   VKEY3B                                                           
*                                                                               
         MVC   IOKEY(L'KEY),KEY    IT'S AN ADD - RECORD ON FILE?                
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,KEY,KEY                               
         TM    DMCB+8,X'10'+X'80'                                               
         BNZ   VKEY3A                                                           
         CLI   DMCB+8,X'40'                                                     
         BNE   *+6                                                              
         DC    H'0'                DISK ERROR                                   
         MVI   ERROR,RECXIST       ELSE ASSUME DUPE                             
         B     EXITN                                                            
                                                                                
VKEY3A   MVC   KEY,IOKEY                                                        
*                                                                               
VKEY3B   BAS   RE,CLEARU           YES, CLEAR USER FIELDS                       
         XC    SVSTART,SVSTART                                                  
*                                                                               
VKEY040  MVC   AIO,AIO1            SWAP IO BACK                                 
         MVI   CNTAEX,0                                                         
         CLI   FAACTION,FQACTADD   TEST FOR ACTION=ADD                          
         BE    VKEY050                                                          
         CLI   FAACTION,FQACTAUT   TEST FOR ACTION=AUTO                         
         BE    VKEY050                                                          
         CLI   FAACTION,FQACTNUM   TEST FOR ACTION=GET AUTO NUMBER              
         BE    VKEY050                                                          
         MVI   RDUPDATE,C'Y'       CHANGE, SO GET REC FOR UPDATE                
         GOTO1 READ                                                             
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   VKEYX                                                            
         USING RSTELD,R6                                                        
         CLI   RSTLN,RSTLN3Q                                                    
         BL    VKEYX                                                            
         CLI   RSTEXCNT,0                                                       
         BE    VKEYX                                                            
         MVC   CNTAEX,RSTEXCNT                                                  
         GOTOR DELAEX                                                           
         B     VKEYX                                                            
*                                                                               
VKEY050  CLI   GOPROD,C'N'         TEST NO PRODUCTION JOBS                      
         BNE   VKEYX               NO-OK TO ADD THEM                            
         MVI   ERROR,JOBADDER                                                   
         B     EXITN                                                            
*                                                                               
VKEYX    MVI   AEXIND,0                                                         
         B     EXITY                                                            
*                                                                               
CHARTAB  DC    C'\~!'             INVALID CHARACTERS FOR GERMANY               
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
VREC     NTR1                                                                   
*                                  VALIDATE MAIN JOB SCREEN                     
         MVI   NEWDATE,0           CLEAR NEW DATE SWITCH                        
         CLI   FAACTION,FQACTADD                                                
         BE    *+12                                                             
         CLI   FAACTION,FQACTAUT                                                
         BNE   VALSJ                                                            
*                                                                               
         L     R2,AIO              CLEAR AIO AREA                               
         LA    R3,2000                                                          
         XR    R1,R1                                                            
         MVCL  R2,R0                                                            
*                                                                               
         USING ACKEYD,R6           BUILD ACC RECORD                             
         L     R6,AIO                                                           
         MVC   0(L'KEY,R6),KEY     MOVE IN KEY                                  
         MVC   ACLENGTH,=H'50'                                                  
         TM    MYLSTA2,LDGSDRFT    DRAFT FEATURE ACTIVATED?                     
         BZ    VREC002                                                          
         CLC   SVPASSWD,MYAPPST                                                 
         BE    VREC002                                                          
         MVI   ACSTATUS,ACTSDRFT   ADD ACCOUNT AS DRAFT                         
*                                                                               
VREC002  GOTO1 ABALPEEL,(RC)                                                    
*                                                                               
VALSJ    MVC   DUMFLDHL,FLJBNAME                                                
         MVC   DUMFLD,BLANKS                                                    
         MVC   DUMFLD(L'FAJBNAME),FAJBNAME                                      
         LA    R2,DUMFLDH                                                       
         GOTO1 ANY                                                              
         CLI   FAACTION,FQACTCHA                                                
         BNE   VSJ010                                                           
         GOTO1 SAVENAM,DMCB,OLDJOBNM  SAVE OLD NAME                             
VSJ010   GOTO1 AVALNAME,(RC)          VALIDATE NAME                             
         GOTO1 AVALSTAT,(RC)          VALIDATE FILTERS & EXCLUDE TEMPO          
         BNE   EXITN                                                            
*                                                                               
         USING PPRELD,R6                                                        
VSJ020   MVI   ELCODE,PPRELQ                                                    
         BAS   RE,GETELIO          GET PPREL                                    
         BE    VOFFICE                                                          
         GOTO1 AADDPROF,(RC)       ADD IF NOT THERE                             
         B     VSJ020                                                           
*                                                                               
VOFFICE  DS    0H                                                               
*&&UK                                                                           
         MVC   DUMFLDHL,FLOFFIC                                                 
         MVC   DUMFLD,BLANKS                                                    
         MVI   DUMFLDHL,0          SKIP ANY OFFICE PASSED                       
         B     VOFFC00             ----------------------                       
         MVC   DUMFLD(L'FAOFFIC),FAOFFIC                                        
         LA    R2,DUMFLDH                                                       
         CLI   FAACTION,FQACTCHA   ACTION = CHANGE?                             
         BNE   VOFFC00                                                          
*                                                                               
         CLI   5(R2),0                                                          
         BE    VOFFC00                                                          
         CLI   TWAACCS,0           TEST LIMIT ACCESS                            
         BE    VOFFC00                                                          
         GOTO1 ANY                                                              
         GOTO1 VTSTOFF,WORK        TEST LIMIT ACCESS                            
         BE    VOFFC00                                                          
         B     EXITN                                                            
*                                                                               
VOFFC00  CLI   5(R2),0                                                          
         BE    VOFFC20                                                          
         TM    CMPSTAT1,X'20'                                                   
         BZ    VOFFC20             NO VALIDATION FOR NON-OFFICE COMP.           
         MVC   AIO,AIO2            USE IO2 FOR OFFICE REC                       
         LA    R6,KEY                                                           
         USING OGRRECD,R6                                                       
         XC    OGRKEY,OGRKEY       BUILD OFFICE KEY                             
         MVI   OGRKTYP,OGRKTYPQ                                                 
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY(L'CUL),CUL                                               
         GOTO1 ANY                                                              
         CLI   TWAACCS,0                                                        
         BE    VOFFC10                                                          
         GOTO1 VTSTOFF,WORK                                                     
         BNE   EXITN                                                            
VOFFC10  MVC   OGRKOFC,WORK                                                     
         GOTO1 HIGH                READ FOR OFFICE.                             
         MVC   AIO,AIO1            CHANGE BUFFERS BACK                          
         CLC   OGRKEY,KEYSAVE                                                   
         BE    VOFFC20             FOUND THE OFFICE.                            
         MVI   ERROR,INVALID       OFFICE NOT FOUND.                            
         B     EXITN                                                            
*                                                                               
         USING PPRELD,R6                                                        
VOFFC20  MVI   ELCODE,PPRELQ                                                    
         BAS   RE,GETELIO                                                       
         CLI   5(R2),0                                                          
         BNE   VOFFC40                                                          
         MVI   PPRUFORA,0                                                       
         MVC   PPRGAOFF,=C'  '                                                  
         B     VALPBIL                                                          
*                                                                               
VOFFC40  GOTO1 ANY                                                              
         MVC   PPRUFORA,WORK                                                    
         MVC   PPRGAOFF,WORK                                                    
*&&                                                                             
VALPBIL  CLI   FAACTION,FQACTCHA                                                
         BE    *+16                                                             
         XC    PPRRECV,PPRRECV     NO RECEIVABLE AT JOB LEVEL                   
         XC    PPRCOST,PPRCOST     NO COSTING AT JOB LEVEL                      
         XC    PPRBILLP,PPRBILLP                                                
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,1,FLPRTBIL                                                    
         BZ    VALINFO                                                          
         BCTR  R1,0                                                             
         MVC   PPRBILLP(0),FAPRTBIL                                             
         EX    R1,*-6                                                           
*                                                                               
VALINFO  MVI   ELEMENT+PPRLN1Q,C' '   CLEAR NARR PORTION OF NEW LMNT.           
         MVC   ELEMENT+PPRLN1Q+1(L'PPRNARRP-1),ELEMENT+PPRLN1Q                  
         MVC   ELEMENT(PPRLN1Q),PPREL PRESERVE START OF OLD ELEMENT.            
         LA    R6,ELEMENT                                                       
         GOTO1 REMELEM                                                          
*                                                                               
         MVC   PPRNARRP,BLANKS                                                  
         LA    RF,PPRNARRP                                                      
         LA    R3,FAOTHIN1         R3=A(OTHER INFO 1)                           
         LA    R0,3                R0 = N'INFO LINES                            
VINFO40  XR    R1,R1                                                            
         ICM   R1,1,L'FAOTHIN1(R3) R1 = L'THIS LINE.                            
         BZ    VINF060                                                          
         BCTR  R1,0                                                             
         MVC   0(0,RF),0(R3)                                                    
         EX    R1,*-6                                                           
         LA    RF,L'FAOTHIN1(RF)   RF = A(NEXT LINE POST'N IN EL).              
         LA    R3,L'FLOTHIN1+L'FAOTHIN1(R3)                                     
         BCT   R0,VINFO40                                                       
*                                                                               
VINF060  LA    R2,3                DERIVE N'LINES I/P.                          
         SR    R2,R0               TOTAL L'INPUT IS N'LINES * 50.               
         MHI   R2,L'FAOTHIN1                                                    
         LA    R2,PPRNARRP-PPRELD(R2) COMPUTE EL LEN                            
         STC   R2,PPRLN                                                         
         GOTO1 ADDELEM              ADD THE ELEMENT.                            
         DROP  R6                                                               
*                                                                               
*                                                                               
*   START UPDATING/BUILDING JOBEL, ON ADD JOBEL KEPT IN ELEMENT UNTIL           
*     VCLOSE - PRESERVE!!!!!                                                    
*                                                                               
*                                                                               
         USING JOBELD,R6                                                        
VALEST   MVI   ELCODE,JOBELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    VEST015                                                          
         CLI   FAACTION,FQACTADD    ELEMENT MUST BE THERE UNLESS ADD            
         BE    VEST010                                                          
         CLI   FAACTION,FQACTAUT                                                
         BE    VEST010                                                          
         DC    H'0'                                                             
VEST010  LA    R6,ELEMENT          SETUP TO ADD NEW ELEMENT                     
         XC    ELEMENT,ELEMENT                                                  
         MVI   JOBEL,JOBELQ                                                     
         MVI   JOBLN,JOBLN4Q                                                    
         MVC   JOBADATE,TODAYP                                                  
         MVI   JOBSRCES,JOBSFALQ   ADDED BY PROD/FALINK                         
         CLI   FAACTION,FQACTAUT                                                
         BNE   VEST025                                                          
         OI    JOBSRCES,JOBSAJNQ   USED AUTO JOB NUMBERING                      
*                                                                               
VEST015  CLC   FAJBIDNO,BLANKS     ID NUMBER CHECK REQUIRED?                    
         BNH   VEST020                                                          
         XOUT  JOBREVNO,DUB,2                                                   
         CLC   FAJBIDNO,DUB                                                     
         BE    VEST020                                                          
         MVI   ERROR,BADCOMM                                                    
         B     EXITN                                                            
*                                                                               
VEST020  XR    RE,RE                                                            
         ICM   RE,3,JOBREVNO                                                    
         AHI   RE,1                                                             
         STCM  RE,3,JOBREVNO                                                    
*                                                                               
VEST025  CLI   FAACTION,FQACTADD                                                
         BE    VEST030                                                          
         CLI   FAACTION,FQACTAUT                                                
         BNE   VALTYPE                                                          
VEST030  LA    R1,LGOBBLK                                                       
         STCM  R1,15,GOABEXT                                                    
*&&US*&& MVC   GOAEXT,AGOXBLK                                                   
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         OI    JOBSTA1,JOBSUEST    INDICATE JOB HAS UNAPPROVED ESTIMATE         
         CLI   GONEWJUN,C'Y'                                                    
         BE    VALTYPE                                                          
         NI    JOBSTA1,X'FF'-JOBSUEST                                           
*                                                                               
VALTYPE  CLI   FAACTION,FQACTADD   ARE WE ADDING ?                              
         BE    VALT010                                                          
         CLI   FAACTION,FQACTAUT                                                
         BNE   VALCLOSE            NO                                           
VALT010  OI    JOBSTA1,JOBSNEST    NO, MUST BE NEW                              
*&&UK*&& CLI   GOESTTYP,C'M'       SET NEW MCS FLAG?                            
*&&UK*&& BNE   VALCLOSE                                                         
*&&US*&& CLI   GOESTTYP,C'B'                                                    
*&&US*&& BNE   VALT020                                                          
         NI    JOBSTA1,X'FF'-JOBSNEST                                           
         OI    JOBSTA1,JOBSMCSE                                                 
*                                                                               
*&&US                                                                           
VALT020  L     R2,AGOXBLK                                                       
         USING GOXBLKD,R2                                                       
         CLI   GOAWOO,C'Y'         IS AUTO WRITEOFF SET?                        
         BNE   VALCLOSE            NO                                           
         OI    JOBSTA1,JOBSXJOB    YES                                          
*&&                                                                             
VALCLOSE GOTO1 AVCLOSE,(RC)                                                     
         BNE   EXITN                                                            
*                                                                               
*                                                                               
*  VCLOSE ADDS JOBEL IF BUILDING IN ELEMENT (ON ADD), SO IT'S NOW SAFE          
*       TO RE-USE 'ELEMENT'. NOT BEFORE!!!                                      
*                                                                               
*                                                                               
*                                                                               
VALCJUL  GOTO1 AVALLAL,DMCB,(RC),(R7)                                           
*                                                                               
*&&US                                                                           
         CLI   FAACTION,FQACTADD   ARE WE ADDING ?                              
         BE    VALSTUD                                                          
         CLI   FAACTION,FQACTAUT                                                
         BNE   VALJC1              NO                                           
VALSTUD  CLI   GOSTUDIO,C'Y'       IS THIS A STUDIO JOB?                        
         BNE   VALJC1              NO                                           
         OC    GOTYPE,GOTYPE       YES, DO WE HAVE A DEFAULT TYPE?              
         BZ    VALJC1              NO                                           
*                                                                               
         USING STURECD,R3                                                       
         MVC   AIO,AIO2            SWAP BUFFERS FOR READ                        
         LA    R3,KEY                                                           
         XC    STUKEY,STUKEY                                                    
         MVI   STUKTYP,STUKTYPQ                                                 
         MVI   STUKSUB,STUKSUBQ                                                 
         MVC   STUKCPY,CUL                                                      
         MVC   STUKCODE,GOTYPE                                                  
         GOTO1 READ                                                             
                                                                                
         USING STUELD,R6                                                        
         MVI   ELCODE,STUELQ                                                    
         MVI   ERROR,NOTDEFN                                                    
         BAS   RE,GETELIO                                                       
         BNE   EXITN                                                            
                                                                                
         CLC   STUMED,MEDIA        FIND THE RIGHT MEDIA                         
         BE    VALST02                                                          
         CLC   STUMED,BLANKS                                                    
         BNH   VALST02                                                          
         MVI   ERROR,NOTMED                                                     
         B     EXITN                                                            
*                                                                               
VALST02  SR    R1,R1                                                            
         IC    R1,STULN            GET LENGTH OF ELEMENT                        
         SH    R1,=Y(STUOFF-STUELD)                                             
         BZ    EXITN               NO OFFICE/CLIENTS                            
*                                                                               
         LA    R5,STUOFF                                                        
*                                                                               
VALST04  CLC   2(3,R5),BLANKS      BLANK IS AS GOOD AS FIND                     
         BE    VALST06                                                          
         CLC   2(3,R5),CLICODE                                                  
         BE    VALST06                                                          
         LA    R5,L'STUOCLN(R5)                                                 
         SH    R1,=Y(L'STUOCLN)                                                 
         BNZ   VALST04                                                          
         B     EXITN               CAN'T FIND CLIENT                            
*                                                                               
VALST06  MVC   AIO,AIO1            VALID, SWAP BACK AND UPDATE STATUS           
         DROP  R3                                                               
*                                                                               
         USING LNKELD,R6                                                        
         MVI   ELCODE,LNKELQ                                                    
         BAS   RE,GETELIO          DO WE HAVE A LINK ELEMENT?                   
         BE    VALST08             YES                                          
*                                                                               
         USING ACTRECD,R3                                                       
         L     R3,AIO                                                           
         LA    R6,ELEMENT          NO, BUILD ONE                                
         XC    ELEMENT,ELEMENT                                                  
         MVI   LNKEL,LNKELQ                                                     
         MVI   LNKLN,LNKLNQ                                                     
         MVC   LNKSTUD,GOTYPE                                                   
         MVC   LNKSTJB,ACTKACT                                                  
         DROP  R2,R3                                                            
*                                                                               
         USING JOBELD,R6                                                        
VALST08  GOTO1 ADDELEM                                                          
         MVI   ELCODE,JOBELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   VALJC1                                                           
         OI    JOBSTA1,JOBSTUD                                                  
*&&                                                                             
VALJC1   LA    R2,DUMFLDH                                                       
         MVI   ELCODE,SCMELQ                                                    
         GOTO1 REMELEM                                                          
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT,SCMELQ                                                   
         MVC   DUMFLD,FAJOBCM1                                                  
         MVC   DUMFLDHL,FLJOBCM1                                                
         CLI   DUMFLDHL,0                                                       
         BE    VALJC2                                                           
         GOTO1 AVALJCOM,(RC)                                                    
         BNE   EXITN                                                            
*                                                                               
VALJC2   MVC   DUMFLD,FAJOBCM2                                                  
         MVC   DUMFLDHL,FLJOBCM2                                                
         CLI   DUMFLDHL,0                                                       
         BE    VALJC3                                                           
         GOTO1 AVALJCOM,(RC)                                                    
         BNE   EXITN                                                            
*                                                                               
VALJC3   MVC   DUMFLD,FAJOBCM3                                                  
         MVC   DUMFLDHL,FLJOBCM3                                                
         CLI   DUMFLDHL,0                                                       
         BE    VADDR                                                            
         GOTO1 AVALJCOM,(RC)                                                    
         BNE   EXITN                                                            
*                                                                               
VADDR    MVC   DUMFLD,FAADDR1                                                   
         MVC   DUMFLDHL,FLADDR1                                                 
         GOTO1 AVALACAD,(RC)       VALIDATE BILLING ADDRESS                     
*                                                                               
VEXTCOM  GOTO1 AVALEXTC,(RC)       VALIDATE EXTRA COMMENTS                      
*                                                                               
VLCAMPC  GOTO1 AVALCAMP,(RC)       VALIDATE CAMPAIGN CODE                       
*                                                                               
VLTEAJ   GOTO1 AVALTEAJ,(RC)       VALIDATE TEAM LIST IN JOBS                   
*                                                                               
VALUSER  GOTO1 AGETUSR,(RC)        GET USER FLD DEFINITIONS INTO ELMTAB         
         MVI   ELCODE,UFSELQ                                                    
         GOTO1 REMELEM             REMOVE OLD USER ELEMENTS                     
         CP    ELMCNT,PZERO                                                     
         BE    VALUSERX                                                         
         LA    R0,10               R0=N'POSSIBLE USER FIELDS                    
         LA    R3,ELMTAB           R3=A(ELMTAB)                                 
*                                                                               
VUS020   XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(UFSLN1Q),0(R3)                                           
         MVI   ELEMENT+UFSSEQ-UFSEL,X'00'  REMOVE SEQUENCE NUMBER               
         MVC   ELEMENT+UFSLN1Q(L'FAUSER1),BLANKS                                
         LA    RE,10                       WAS A USER FIELD UPLOADED            
         LA    R5,FAUSCD1                  WITH THIS CODE ?                     
         USING FAUSERD,R5                                                       
VUS030   CLC   FAUSCODE,UFSCODE-UFSELD(R3)                                      
         BE    VUS040                                                           
         LA    R5,FQUSERLN(R5)                                                  
         BCT   RE,VUS030                                                        
         B     VUS044              NO, CHECK IF IT'S REQUIRED                   
                                                                                
VUS040   CLI   FAUSDTLN,0          MOVE DATA IF ENTERED                         
         BNE   VUS060                                                           
VUS044   MVI   ERROR,REQFLD        PREPARE FOR MISSING REQUIRED FIELD           
         TM    UFSSTAT-UFSELD(R3),UFSSREQD                                      
         BO    EXITN                                                            
         B     VUS180              NOT ENTERED, ADD WITH BLANK DATA             
*                                                                               
         USING UFSELD,R6                                                        
VUS060   LA    R6,ELEMENT                                                       
         XR    RF,RF                                                            
         IC    RF,FAUSDTLN         GET LENGTH OF INPUT DATA                     
         XR    R1,R1                                                            
         IC    R1,UFSMXLN          GET MAXIMUM LENGTH ALLOWED                   
         CR    RF,R1                                                            
         MVI   ERROR,INP2LONG      ERROR IF INPUT LONGER THAN MAX               
         BH    EXITN                                                            
         CLI   UFSEDIT-UFSELD(R3),C'N' VALIDATE FOR NUMERIC IF                  
         BNE   VUS100               EDIT = N                                    
         LR    R1,RF               SAVE INPUT LENGTH                            
         XC    WORK,WORK                                                        
         MVI   ERROR,NOTNUM        PREPARE ERROR MESSAGE                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),FAUSDATA    MOVE IN JUST ZONES                           
         LA    RE,WORK                                                          
         LR    R1,RF               RESET LENGTH                                 
*                                                                               
VUS080   CLI   0(RE),X'F0'                                                      
         BNE   EXITN                                                            
         LA    RE,1(0,RE)                                                       
         BCT   R1,VUS080                                                        
*                                                                               
VUS100   CLI   UFSEDIT-UFSELD(R3),C'D'                                          
         BNE   VUS120                                                           
         MVI   ERROR,INVDATE                                                    
         MVC   DATELEN,FAUSDTLN    LENGTH OF DATE INPUT                         
         MVC   DATEFORM,AGYLANG    LANGUAGE                                     
         OI    DATEFORM,PVINSGLO   SINGLE DATE ONLY                             
         OI    DATEFORM,PVINSGLS   TREAT SINGLE DATE AS SINGLE                  
         GOTO1 PERVAL,DMCB,(DATELEN,FAUSDATA),(DATEFORM,PERVBLK)                
         CLI   4(R1),PVRCONE       ONLY ONE DATE INPUT?                         
         BNE   EXITN                                                            
         MVC   FAUSDATA(L'PVALCPER),PERVBLK+PVALCPER-PERVALD                    
         LA    RF,8                                                             
*                                                                               
VUS120   CLI   UFSEDIT-UFSELD(R3),C'C'                                          
         BNE   VUS160                                                           
         LR    R1,RF               SAVE INPUT LENGTH                            
         XC    WORK,WORK                                                        
         MVI   ERROR,NOTCHAR       PREPARE ERROR MESSAGE                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),FAUSDATA    MOVE IN JUST ZONES                           
         LR    R1,RF               RESET LENGTH                                 
         LA    RE,WORK                                                          
*                                                                               
VUS140   CLI   0(RE),X'F0'                                                      
         BE    EXITN                                                            
         LA    RE,1(0,RE)                                                       
         BCT   R1,VUS140                                                        
*                                                                               
VUS160   IC    R1,UFSLN             GET LENGTH OF RECORD                        
         AR    R1,RF                ADD INPUT LENGTH TO RECORD TO               
         STC   R1,UFSLN             GET NEW RECORD LENGTH                       
         BCTR  RF,0                 MOVE TO RECORD BASED ON                     
         MVC   UFSDATA(0),FAUSDATA  MOVE DATA TO RECORD                         
         EX    RF,*-6               INPUT LENGTH                                
*                                                                               
VUS180   GOTO1 ADDELEM              ADD THE NEW ELEMENT                         
*                                                                               
VUS200   LA    R3,ELMLNG(R3)        BUMP TO NEXT ELMTAB ENTRY                   
         BCT   R0,VUS020            DO FOR ALL 10 FIELDS                        
         DROP  R6,R5                                                            
*                                                                               
VALUSERX DS    0H                                                               
*                                                                               
VLJDESC  BAS   RE,VALLDESC         VALIDATE LONG JOB DESCRIPTION                
         GOTOR VALCLIPO            VALIDATE CLIENT POS                          
         GOTOR VALFEEBA            VALIDATE FEE BUDGET AMOUNT                   
*                                                                               
VWRITE   CLI   FAACTION,FQACTADD   IF THIS IS A NEW RECORD ADD                  
         BE    VWRITE00            RACEL WITH CREATION DETAILS                  
         CLI   FAACTION,FQACTAUT                                                
         BNE   VWRITE01                                                         
*                                                                               
         USING RACELD,R6                                                        
VWRITE00 XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         MVI   RACEL,RACELQ                                                     
         MVI   RACLN,RACLNQ                                                     
         MVI   RACTYPE,RACTADD                                                  
         MVC   RACUSER,TWAORIG                                                  
         MVC   RACPERS,SVPASSWD                                                 
         MVC   RACDATE,TODAYP                                                   
         MVC   RACTIME,SVTIME                                                   
         MVC   DMCB(4),=X'FFFFFFFF'                                             
         GOTO1 SWITCH,DMCB                                                      
         L     R1,0(R1)                                                         
         MVC   RACTERM,TCFN-UTLD(R1)                                            
         MVC   SVTERMNO,RACTERM                                                 
         MVI   ELCODE,RACELQ                                                    
         GOTO1 ADDELEM                                                          
         CLC   SVPASSWD,MYAPPST    SELF APPROVAL?                               
         BNE   VWRITE09                                                         
         MVI   RACTYPE,RACTAPP                                                  
         MVI   ELCODE,RACELQ                                                    
         GOTO1 ADDELEM                                                          
         USING STCELD,R6                                                        
         XC    ELEMENT,ELEMENT     NO CHANGE RACEL ON ADD BUT STCEL             
         LA    R6,ELEMENT                                                       
         MVI   STCEL,OCAELQ        (USE FE TO MOVE IT TO THE END)               
         MVI   STCLN,STCLN1Q                                                    
         MVI   STCIND,STCIJOB                                                   
         MVC   STCUSER,TWAORIG                                                  
         MVC   STCPERS,SVPASSWD                                                 
         MVC   STCTERM,SVTERMNO                                                 
         MVC   STCDATE,TODAYP                                                   
         MVC   STCTIME,SVTIME                                                   
         MVI   STCDFR,C'D'                                                      
         MVI   STCDTO,C'A'                                                      
         GOTO1 ADDELEM                                                          
         MVI   ELCODE,OCAELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   STCEL,STCELQ        (AND RENAME FE TO STCEL)                     
         B     VWRITE09                                                         
         DROP  R6                                                               
                                                                                
         USING RACELD,R6                                                        
VWRITE01 MVI   ELCODE,RACELQ       CHANGE: ADD/REPLACE RACTCHA ELEMENT          
         BAS   RE,GETELIO                                                       
         BNE   VWRITE07                                                         
         XR    R0,R0                                                            
         XR    RE,RE                                                            
         XC    WORK,WORK                                                        
         CLI   RACTYPE,RACTCHA                                                  
         BNE   VWRITE02                                                         
         MVC   WORK+4(L'RACDATE+L'RACTIME),RACDATE                              
         ST    R6,WORK                                                          
         AHI   RE,1                                                             
                                                                                
VWRITE02 IC    R0,RACLN                                                         
         AR    R6,R0                                                            
         CLI   RACEL,0                                                          
         BE    VWRITE05                                                         
                                                                                
VWRITE03 CLI   RACEL,RACELQ                                                     
         BNE   VWRITE02                                                         
         CLI   RACTYPE,RACTCHA                                                  
         BNE   VWRITE02                                                         
         AHI   RE,1                                                             
         OC    WORK(4),WORK                                                     
         BZ    VWRITE04                                                         
         CLC   WORK+4(L'RACDATE+L'RACTIME),RACDATE                              
         BL    VWRITE02                                                         
                                                                                
VWRITE04 MVC   WORK+4(L'RACDATE+L'RACTIME),RACDATE                              
         ST    R6,WORK                                                          
         B     VWRITE02                                                         
                                                                                
VWRITE05 CHI   RE,RACMAXQ                                                       
         BNL   VWRITE06                                                         
         LA    R1,2000                                                          
         SHI   R1,RACMAXQ*50                                                    
         L     RF,AIO                                                           
         AHI   RF,ACCORLEN                                                      
         CLM   R1,3,0(RF)                                                       
         BH    VWRITE07                                                         
                                                                                
VWRITE06 L     R6,WORK           REPLACE ELEMENT                                
         LTR   R6,R6                                                            
         BZ    VWRITE07                                                         
         MVI   BYTE,0                                                           
         B     VWRITE08                                                         
                                                                                
VWRITE07 LA    R6,ELEMENT        ADD NEW CHANGE RACELD                          
         MVI   BYTE,1                                                           
                                                                                
VWRITE08 XC    RACEL(RACLNQ),RACEL                                              
         MVI   RACEL,RACELQ                                                     
         MVI   RACLN,RACLNQ                                                     
         MVI   RACTYPE,RACTCHA                                                  
         MVC   RACUSER,TWAORIG                                                  
         MVC   RACPERS,SVPASSWD                                                 
         MVC   RACDATE,TODAYP                                                   
         MVC   RACTIME,SVTIME                                                   
         MVC   DMCB(4),=X'FFFFFFFF'                                             
         GOTO1 SWITCH,DMCB                                                      
         L     R1,0(R1)                                                         
         MVC   RACTERM,TCFN-UTLD(R1)                                            
         MVI   ELCODE,RACELQ                                                    
         CLI   BYTE,0                                                           
         BE    VWRITE09                                                         
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
                                                                                
VWRITE09 CLC   TWAALIAS,BLANKS     ANY PERSON HERE?                             
         BE    *+10                YES                                          
         MVC   TWAALIAS,PERSONID   NO, USE PERSON                               
         GOTO1 PERSIN              ADD PERSON/ACTIVITY                          
                                                                                
         MVI   ELCODE,JOBELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                NO JOBEL, BROKEN RECORD!                     
         CLI   JOBLN-JOBELD(R6),JOBLN1Q                                         
         JNL   *+6                                                              
         DC    H'0'                BAD LENGTH!!!                                
*                                                                               
         CLI   FAACTION,FQACTADD                                                
         BE    VWRITE10                                                         
         CLI   FAACTION,FQACTAUT                                                
         BE    VWRITE10                                                         
         MVC   FUNCTION,=CL8'DMWRT'                                             
         B     *+10                                                             
VWRITE10 MVC   FUNCTION,=CL8'DMADD'                                             
                                                                                
         GOTOR MANAGER             WRITE BACK JOB RECORD                        
         BNE   EXITN                                                            
*&&US                                                                           
         USING JOBELD,R6                                                        
         MVI   ELCODE,JOBELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MYJSTA2,JOBSTA2                                                  
         DROP  R6                                                               
*&&                                                                             
         GOTO1 CHKNAME,DMCB,AIO,OLDJOBNM  ADD NAME CHANGE POINTERS              
         GOTO1 ANAMSRCH,(RC)       UPDATE NAME SEARCH PASSIVE RECORDS           
*                                                                               
TLABEL   CLI   FAACTION,FQACTADD   IS THIS AN ADD ?                             
         BE    VLABEL              YES, ALL ADDS NEED LABELS                    
         CLI   FAACTION,FQACTAUT                                                
         BE    VLABEL                                                           
         TM    NAMEFLAG,NFCHANGE   HAS NAME CHANGED ?                           
         BO    VLABEL              YES, REQUEST LABELS                          
         CLI   NEWDATE,0           HAS CLOSE DATE CHANGED ?                     
         BE    EXITY               NO, EXIT                                     
*                                                                               
VLABEL   GOTO1 GETOPT,DMCB,GOBLOCK                                              
         CLI   GOSUPSTI,C'Y'                                                    
         BE    EXITY               SEE IF DATA HAS CHANGED                      
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT+10,12                                                    
         MVC   ELEMENT+26(80),BLANKS                                            
         MVC   ELEMENT+26(2),=C'12'                                             
         MVC   ELEMENT+28(1),CUL                                                
         MVC   ELEMENT+29(1),SAVEOFC                                            
         MVI   ELCODE,PPRELQ                                                    
         BAS   RE,GETELIO                                                       
         USING PPRELD,R6                                                        
         CLC   PPRGAOFF,BLANKS                                                  
         BNH   *+10                                                             
         MVC   ELEMENT+29(1),PPRGAOFF                                           
         L     RE,AIO                                                           
         MVC   ELEMENT+35(15),0(RE)                                             
         GOTO1 DATAMGR,DMCB,DMADD,=C'ACCREQS',ELEMENT,ELEMENT,0                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXITY                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PREPARE FOR ESTIMATES TO BE ADDED                                   *         
***********************************************************************         
         SPACE 1                                                                
PREADD   NTR1                                                                   
         MVC   AIO,AIO2            USE IO2 FOR THIS                             
         LA    R5,1                GET READY TO ADD SOMETHING                   
         CLI   GOADDREV,C'Y'       ADD AN 'R' ESTIMATE ?                        
         BNE   PREADD2             NO                                           
         MVI   ADDTYPE,C'R'        YES, AGO ADD IT                              
         GOTO1 AADDEST,(RC)                                                     
*                                                                               
PREADD2  XR    R5,R5                                                            
         ICM   R5,1,GOADDPLN                                                    
         BZ    PREADDX             NO 'P' ESTIMATES TO ADD                      
         MVI   ADDTYPE,C'P'                                                     
         GOTO1 AADDEST,(RC)                                                     
*                                                                               
PREADDX  MVC   AIO,AIO1            RESTORE IO AREA                              
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* CLEAR ELMTAB (USER SELECT FIELD DETAILS)                            *         
***********************************************************************         
         SPACE 1                                                                
CLEARU   NTR1                                                                   
         LA    R2,ELMTAB           CLEAR TABLE OF USER ELEMENTS                 
         LA    R3,320                                                           
         XR    R1,R1                                                            
         MVCL  R2,R0                                                            
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK WHETHER ACCOUNT NAME HAS CHANGED AND IF SO ADD POINTER        *         
* FOR ACCOLADE TO ALLOW REFRESH OF CONTRA-ACCOUNT NAMES               *         
*              P1=A(RECORD)                                           *         
*              P2=A(OLD NAME)                                         *         
***********************************************************************         
         SPACE 1                                                                
CHKNAME  NTR1                                                                   
         CLI   EMULATE,C'Y'                                                     
         BNE   CHKNX               NOT EMULATING                                
         L     R6,0(R1)            R6=A(RECORD)                                 
         ST    R6,FULL                                                          
         L     R2,4(R1)            R2=A(OLD NAME)                               
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   1(1,R2),1(R6)                                                    
         BNE   CHKN20                                                           
         XR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(R6)                                                    
         BE    CHKNX               NO CHANGE                                    
CHKN20   OI    NAMEFLAG,NFCHANGE                                                
         L     R6,FULL                                                          
         USING ACTRECD,R6                                                       
         L     R2,AIO3                                                          
         USING ANCRECD,R2                                                       
         MVI   ANCKTYP,ANCKTYPQ                                                 
         MVC   ANCKCULA,ACTKCULA                                                
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,(R2),(R2),0                   
         BE    CHKNX               POINTER ALREADY THERE                        
         MVC   0(L'ACTKEY,R2),ACTKEY                                            
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,(R2),(R2),0                   
         BE    *+6                                                              
         DC    H'0'                ACCOUNT NOT FOUND                            
         MVC   FULL,ACTKDA-ACTRECD(R2)                                          
         MVI   ANCKEY,C' '                                                      
         MVC   ANCKEY+1(L'ANCKEY-1),ANCKEY                                      
         MVI   ANCKTYP,ANCKTYPQ                                                 
         MVC   ANCKCULA,ACTKCULA                                                
         XC    ANCKSTA,ANCKSTA                                                  
         OI    ANCKSTA,ACTSDELT                                                 
         GOTO1 DATAMGR,DMCB,DMADD,ACCDIR,(R2),(R2),0                            
CHKNX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SAVE NAME FROM CURRENT RECORD INTO P1                               *         
***********************************************************************         
         SPACE 1                                                                
SAVENAM  NTR1                      SAVE NAME ELEMENT FROM REC IN AIO            
         L     R2,0(R1)                                                         
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   EXIT                                                             
         XR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     EXIT                                                             
         MVC   0(0,R2),0(R6)                                                    
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD LONG JOB DESCRIPTION                                 *         
***********************************************************************         
         SPACE 1                                                                
VALLDESC NTR1                                                                   
         MVI   ELCODE,JLDELQ       LOOK FOR JOB LONG DESCRIPTION                
         BAS   RE,GETELIO                                                       
         BNE   VALLD04                                                          
*                                                                               
         USING JLDELD,R6                                                        
VALLD02  MVI   JLDEL,X'FF'                                                      
         MVI   ELCODE,X'FF'        DELETE IT                                    
         GOTO1 REMELEM                                                          
         MVI   ELCODE,JLDELQ       LOOK FOR NEXT                                
         BAS   RE,GETELIO                                                       
         BE    VALLD02                                                          
*                                                                               
VALLD04  LHI   R4,FLJBDESC-LINKD   LONG JOB DESCRIPTION PASSED?                 
         LA    R4,LINKD(R4)                                                     
         XR    R2,R2                                                            
*                                                                               
VALLD06  CLI   0(R4),0                                                          
         BE    VALLDX                                                           
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         MVI   JLDEL,JLDELQ        BUILD ELEMENT                                
         LLC   RF,0(R4)                                                         
         LR    R1,RF                                                            
         SHI   RF,1                                                             
         MVC   JLDDESC(0),1(R4)                                                 
         EX    RF,*-6                                                           
         AHI   R1,JLDLNQ                                                        
         STC   R1,JLDLN                                                         
         STC   R2,JLDSEQ                                                        
         MVI   ELCODE,JLDELQ                                                    
         GOTO1 ADDELEM             ADD IT                                       
         AHI   R4,FLJBDEX1-FLJBDESC                                             
         AHI   R2,1                                                             
         CHI   R2,5                                                             
         BL    VALLD06                                                          
*                                                                               
VALLDX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET ELEMENT                                                         *         
***********************************************************************         
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         B     EXITY                                                            
*                                                                               
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
*RREXIT  LA    R2,CONTAGH                                                       
*        GOTO1 VERRCUR                                                          
         SPACE 1                                                                
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
EXITY    CR    RB,RB                                                            
EXIT     XIT1  ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
EFFS     DC    8XL1'FF'                                                         
DATADIS2 DC    X'0038'                                                          
ACCOUNT  DC    C'ACCOUNT'                                                       
ACCDIR   DC    C'ACCDIR'                                                        
ACCMST   DC    C'ACCMST'                                                        
CTFILE   DC    C'CTFILE'                                                        
DMREAD   DC    C'DMREAD'                                                        
DMRSEQ   DC    C'DMRSEQ'                                                        
DMRDHI   DC    C'DMRDHI'                                                        
DMWRT    DC    C'DMWRT'                                                         
DMADD    DC    C'DMADD'                                                         
PTREC    DC    C'PUTREC'                                                        
GTREC    DC    C'GETREC'                                                        
         SPACE 1                                                                
***********************************************************************         
* JOB MAINT FALINK DATA RECIEVE                                       *         
***********************************************************************         
*                                                                               
RCVACT   DS    0H                                                               
         MVC   FAACTION,DATA                                                    
         MVC   SNDMODE,FAACTION                                                 
         J     EXITY                                                            
*                                                                               
RCVCLI   DS    0H                                                               
         MVC   FACLIENT,DATA                                                    
         MVC   FLCLIENT,DATALEN+1                                               
         J     EXITY                                                            
*                                                                               
RCVPRO   DS    0H                                                               
         MVC   FAPROD,DATA                                                      
         MVC   FLPROD,DATALEN+1                                                 
         J     EXITY                                                            
*                                                                               
RCVJOB   DS    0H                                                               
         MVC   FAJOB,DATA                                                       
         MVC   FLJOB,DATALEN+1                                                  
         J     EXITY                                                            
*                                                                               
RCVJNM   DS    0H                                                               
         MVC   FAJBNAME,DATA                                                    
         MVC   FLJBNAME,DATALEN+1                                               
         J     EXITY                                                            
*                                                                               
RCVJF1   MVC   FAFILT1,DATA                                                     
         J     EXITY                                                            
RCVJF2   MVC   FAFILT2,DATA                                                     
         J     EXITY                                                            
RCVJF3   MVC   FAFILT3,DATA                                                     
         J     EXITY                                                            
RCVJF4   MVC   FAFILT4,DATA                                                     
         J     EXITY                                                            
RCVJF5   MVC   FAFILT5,DATA                                                     
         J     EXITY                                                            
*                                                                               
RCVJOF   DS    0H                                                               
         MVC   FAOFFIC,DATA                                                     
         MVC   FLOFFIC,DATALEN+1                                                
         J     EXITY                                                            
*                                                                               
RCVCDT   DS    0H                                                               
         MVC   FACLOSDT,DATA                                                    
         J     EXITY                                                            
*                                                                               
RCVODT   DS    0H                                                               
         MVC   FAOPENDT,DATA                                                    
         J     EXITY                                                            
*                                                                               
RCVPOB   DS    0H                                                               
         MVC   FAPRTBIL,DATA                                                    
         MVC   FLPRTBIL,DATALEN+1                                               
         J     EXITY                                                            
*                                                                               
RCVOTH1  DS    0H                                                               
         MVC   FAOTHIN1,DATA                                                    
         MVC   FLOTHIN1,DATALEN+1                                               
         J     EXITY                                                            
*                                                                               
RCVOTH2  DS    0H                                                               
         MVC   FAOTHIN2,DATA                                                    
         MVC   FLOTHIN2,DATALEN+1                                               
         J     EXITY                                                            
*                                                                               
RCVOTH3  DS    0H                                                               
         MVC   FAOTHIN3,DATA                                                    
         MVC   FLOTHIN3,DATALEN+1                                               
         J     EXITY                                                            
*                                                                               
RCVCOM1  DS    0H                                                               
         MVC   FAJOBCM1,DATA                                                    
         MVC   FLJOBCM1,DATALEN+1                                               
         J     EXITY                                                            
*                                                                               
RCVCOM2  DS    0H                                                               
         MVC   FAJOBCM2,DATA                                                    
         MVC   FLJOBCM2,DATALEN+1                                               
         J     EXITY                                                            
*                                                                               
RCVCOM3  DS    0H                                                               
         MVC   FAJOBCM3,DATA                                                    
         MVC   FLJOBCM3,DATALEN+1                                               
         J     EXITY                                                            
*                                                                               
RCVUCD1  MVC   FAUSCD1,DATA                                                     
         J     EXITY                                                            
RCVUSR1  MVC   FAUSER1,DATA                                                     
         MVC   FLUSER1,DATALEN+1                                                
         J     EXITY                                                            
                                                                                
RCVUCD2  MVC   FAUSCD2,DATA                                                     
         J     EXITY                                                            
RCVUSR2  MVC   FAUSER2,DATA                                                     
         MVC   FLUSER2,DATALEN+1                                                
         J     EXITY                                                            
                                                                                
RCVUCD3  MVC   FAUSCD3,DATA                                                     
         J     EXITY                                                            
RCVUSR3  MVC   FAUSER3,DATA                                                     
         MVC   FLUSER3,DATALEN+1                                                
         J     EXITY                                                            
                                                                                
RCVUCD4  MVC   FAUSCD4,DATA                                                     
         J     EXITY                                                            
RCVUSR4  MVC   FAUSER4,DATA                                                     
         MVC   FLUSER4,DATALEN+1                                                
         J     EXITY                                                            
                                                                                
RCVUCD5  MVC   FAUSCD5,DATA                                                     
         J     EXITY                                                            
RCVUSR5  MVC   FAUSER5,DATA                                                     
         MVC   FLUSER5,DATALEN+1                                                
         J     EXITY                                                            
                                                                                
RCVUCD6  MVC   FAUSCD6,DATA                                                     
         J     EXITY                                                            
RCVUSR6  MVC   FAUSER6,DATA                                                     
         MVC   FLUSER6,DATALEN+1                                                
         J     EXITY                                                            
                                                                                
RCVUCD7  MVC   FAUSCD7,DATA                                                     
         J     EXITY                                                            
RCVUSR7  MVC   FAUSER7,DATA                                                     
         MVC   FLUSER7,DATALEN+1                                                
         J     EXITY                                                            
                                                                                
RCVUCD8  MVC   FAUSCD8,DATA                                                     
         J     EXITY                                                            
RCVUSR8  MVC   FAUSER8,DATA                                                     
         MVC   FLUSER8,DATALEN+1                                                
         J     EXITY                                                            
                                                                                
RCVUCD9  MVC   FAUSCD9,DATA                                                     
         J     EXITY                                                            
RCVUSR9  MVC   FAUSER9,DATA                                                     
         MVC   FLUSER9,DATALEN+1                                                
         J     EXITY                                                            
                                                                                
RCVUC10  MVC   FAUSCD10,DATA                                                    
         J     EXITY                                                            
RCVUSR10 MVC   FAUSER10,DATA                                                    
         MVC   FLUSER10,DATALEN+1                                               
         J     EXITY                                                            
*                                                                               
RCVAD1   DS    0H                                                               
         MVC   FAADDR1,DATA                                                     
         MVC   FLADDR1,DATALEN+1                                                
         J     EXITY                                                            
RCVAD2   MVC   FAADDR2,DATA                                                     
         MVC   FLADDR2,DATALEN+1                                                
         J     EXITY                                                            
RCVAD3   MVC   FAADDR3,DATA                                                     
         MVC   FLADDR3,DATALEN+1                                                
         J     EXITY                                                            
RCVAD4   MVC   FAADDR4,DATA                                                     
         MVC   FLADDR4,DATALEN+1                                                
         J     EXITY                                                            
RCVAD5   MVC   FAADDR5,DATA                                                     
         MVC   FLADDR5,DATALEN+1                                                
         J     EXITY                                                            
*                                                                               
RCVTMP   DS    0H                                                               
         MVC   FAEXCTMP,DATA                                                    
         J     EXITY                                                            
*                                                                               
RCVJUL   DS    0H                                                               
         MVC   FAJULDEF,DATA                                                    
         J     EXITY                                                            
*                                                                               
RCVJLEST DS    0H                                                               
         MVC   FAJLEST,DATA                                                     
         J     EXITY                                                            
*                                                                               
RCVJLEXT DS    0H                                                               
         MVC   FAJLEXT,DATA                                                     
         J     EXITY                                                            
*                                                                               
RCVJLBIL DS    0H                                                               
         MVC   FAJLBIL,DATA                                                     
         J     EXITY                                                            
*                                                                               
RCVJLADJ DS    0H                                                               
         MVC   FAJLADJ,DATA                                                     
         J     EXITY                                                            
*                                                                               
RCVJLTSI DS    0H                                                               
         MVC   FAJLTSI,DATA                                                     
         J     EXITY                                                            
*                                                                               
RCVJLORD DS    0H                                                               
         MVC   FAJLORD,DATA                                                     
         J     EXITY                                                            
*                                                                               
RCVFBA   DS    0H                                                               
         ZAP   FAFEEAMT,PZERO                                                   
         CLC   DATA(16),BLANKS                                                  
         JNH   EXITY                                                            
         LA    RE,DATA+16-1                                                     
         LA    RF,16                                                            
RCVFBA2  CLI   0(RE),C'0'                                                       
         JNL   RCVFBA4                                                          
         SHI   RE,1                                                             
         JCT   RF,RCVFBA2                                                       
         DC    H'0'                                                             
RCVFBA4  XR    R1,R1                                                            
         LA    RE,DATA                                                          
         CLI   DATA,C'-'                                                        
         BNE   RCVFBA6                                                          
         LA    R1,1                                                             
         SHI   RF,1                                                             
         AHI   RE,1                                                             
RCVFBA6  AHI   RF,X'6F'                                                         
         EX    RF,*+8                                                           
         J     RCVFBA8                                                          
         PACK  FAFEEAMT(0),0(0,RE)                                              
RCVFBA8  LTR   R1,R1                                                            
         JZ    EXITY                                                            
         MP    FAFEEAMT,PMINUS1                                                 
         J     EXITY                                                            
*                                                                               
RCVNCY   DS    0H                                                               
         MVC   FANXTCYC,DATA                                                    
         J     EXITY                                                            
*                                                                               
RCVART   DS    0H                                                               
         MVC   FAADJRAT,DATA                                                    
         J     EXITY                                                            
*                                                                               
RCVAWO   DS    0H                                                               
         MVC   FAAUTOWO,DATA                                                    
         J     EXITY                                                            
*                                                                               
RCVTMD   DS    0H                                                               
         MVC   FATALMED,DATA                                                    
         J     EXITY                                                            
*                                                                               
RCVSTU   DS    0H                                                               
         MVC   FASTUDIO,DATA                                                    
*&&US*&& MVC   FLSTUDIO,DATALEN+1                                               
         J     EXITY                                                            
*                                                                               
RCVXTCM  MVC   FAEXTRCM,DATA                                                    
         MVC   FLEXTRCM,DATALEN+1                                               
         J     EXITY                                                            
*                                                                               
RCVLDSC  LHI   RE,FLJBDESC-LINKD                                                
         J     RCVDALL                                                          
RCVLDE1  LHI   RE,FLJBDEX1-LINKD                                                
         J     RCVDALL                                                          
RCVLDE2  LHI   RE,FLJBDEX2-LINKD                                                
         J     RCVDALL                                                          
RCVLDE3  LHI   RE,FLJBDEX3-LINKD                                                
         J     RCVDALL                                                          
RCVLDE4  LHI   RE,FLJBDEX4-LINKD                                                
RCVDALL  LA    RE,LINKD(RE)                                                     
         MVC   1(L'FAJBDESC,RE),DATA                                            
         MVC   0(L'FLJBDESC,RE),DATALEN+1                                       
         J     EXITY                                                            
*                                                                               
RCVCPA01 LHI   RE,FACPOA01-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA02 LHI   RE,FACPOA02-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA03 LHI   RE,FACPOA03-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA04 LHI   RE,FACPOA04-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA05 LHI   RE,FACPOA05-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA06 LHI   RE,FACPOA06-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA07 LHI   RE,FACPOA07-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA08 LHI   RE,FACPOA08-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA09 LHI   RE,FACPOA09-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA10 LHI   RE,FACPOA10-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA11 LHI   RE,FACPOA11-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA12 LHI   RE,FACPOA12-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA13 LHI   RE,FACPOA13-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA14 LHI   RE,FACPOA14-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA15 LHI   RE,FACPOA15-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA16 LHI   RE,FACPOA16-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA17 LHI   RE,FACPOA17-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA18 LHI   RE,FACPOA18-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA19 LHI   RE,FACPOA19-LINKD                                                
         J     RCVCPAAL                                                         
RCVCPA20 LHI   RE,FACPOA20-LINKD                                                
RCVCPAAL LA    RE,LINKD(RE)                                                     
         ZAP   0(6,RE),PZERO                                                    
         CLC   DATA(16),BLANKS                                                  
         JNH   EXITY                                                            
         LR    R2,RE                                                            
         LA    RE,DATA+16-1                                                     
         LA    RF,16                                                            
RCVCPA2  CLI   0(RE),C'0'                                                       
         JNL   RCVCPA4                                                          
         SHI   RE,1                                                             
         JCT   RF,RCVCPA2                                                       
         DC    H'0'                                                             
RCVCPA4  XR    R1,R1                                                            
         LA    RE,DATA                                                          
         CLI   DATA,C'-'                                                        
         JNE   RCVCPA6                                                          
         LA    R1,1                                                             
         SHI   RF,1                                                             
         AHI   RE,1                                                             
RCVCPA6  AHI   RF,X'6F'                                                         
         EX    RF,*+8                                                           
         J     RCVCPA8                                                          
         PACK  0(0,R2),0(0,RE)                                                  
RCVCPA8  LTR   R1,R1                                                            
         JZ    EXITY                                                            
         MP    0(6,R2),PMINUS1                                                  
         J     EXITY                                                            
PMINUS1  DC    P'-1'                                                            
                                                                                
RCVCPO01 LHI   RE,FLCLPO01-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO02 LHI   RE,FLCLPO02-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO03 LHI   RE,FLCLPO03-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO04 LHI   RE,FLCLPO04-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO05 LHI   RE,FLCLPO05-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO06 LHI   RE,FLCLPO06-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO07 LHI   RE,FLCLPO07-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO08 LHI   RE,FLCLPO08-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO09 LHI   RE,FLCLPO09-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO10 LHI   RE,FLCLPO10-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO11 LHI   RE,FLCLPO11-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO12 LHI   RE,FLCLPO12-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO13 LHI   RE,FLCLPO13-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO14 LHI   RE,FLCLPO14-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO15 LHI   RE,FLCLPO15-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO16 LHI   RE,FLCLPO16-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO17 LHI   RE,FLCLPO17-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO18 LHI   RE,FLCLPO18-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO19 LHI   RE,FLCLPO19-LINKD                                                
         J     RCVCPOAL                                                         
RCVCPO20 LHI   RE,FLCLPO20-LINKD                                                
RCVCPOAL LA    RE,LINKD(RE)                                                     
         MVC   1(L'FLCLPO01,RE),DATA                                            
         MVC   0(L'FACLPO01,RE),DATALEN+1                                       
         J     EXITY                                                            
*                                                                               
RCVIDNO  MVC   FAJBIDNO,DATA                                                    
         J     EXITY                                                            
*                                                                               
RCVCAMP  MVC   FACAMPCD,DATA                                                    
         J     EXITY                                                            
*                                                                               
RCVPRIO  DS    0H                                                               
         MVC   FAPRIOTY,DATA                                                    
         J     EXITY                                                            
*                                                                               
RCVROL1  LHI   RE,FAROLE1-LINKD                                                 
         J     RCVROLE                                                          
RCVPCD1  LHI   RE,FAPCOD1-LINKD                                                 
         J     RCVPCOD                                                          
RCVRLV1  LHI   RE,FARLEV1-LINKD                                                 
         J     RCVRLEV                                                          
*                                                                               
RCVROL2  LHI   RE,FAROLE2-LINKD                                                 
         J     RCVROLE                                                          
RCVPCD2  LHI   RE,FAPCOD2-LINKD                                                 
         J     RCVPCOD                                                          
RCVRLV2  LHI   RE,FARLEV2-LINKD                                                 
         J     RCVRLEV                                                          
*                                                                               
RCVROL3  LHI   RE,FAROLE3-LINKD                                                 
         J     RCVROLE                                                          
RCVPCD3  LHI   RE,FAPCOD3-LINKD                                                 
         J     RCVPCOD                                                          
RCVRLV3  LHI   RE,FARLEV3-LINKD                                                 
         J     RCVRLEV                                                          
*                                                                               
RCVROL4  LHI   RE,FAROLE4-LINKD                                                 
         J     RCVROLE                                                          
RCVPCD4  LHI   RE,FAPCOD4-LINKD                                                 
         J     RCVPCOD                                                          
RCVRLV4  LHI   RE,FARLEV4-LINKD                                                 
         J     RCVRLEV                                                          
*                                                                               
RCVROL5  LHI   RE,FAROLE5-LINKD                                                 
         J     RCVROLE                                                          
RCVPCD5  LHI   RE,FAPCOD5-LINKD                                                 
         J     RCVPCOD                                                          
RCVRLV5  LHI   RE,FARLEV5-LINKD                                                 
         J     RCVRLEV                                                          
*                                                                               
RCVROL6  LHI   RE,FAROLE6-LINKD                                                 
         J     RCVROLE                                                          
RCVPCD6  LHI   RE,FAPCOD6-LINKD                                                 
         J     RCVPCOD                                                          
RCVRLV6  LHI   RE,FARLEV6-LINKD                                                 
         J     RCVRLEV                                                          
*                                                                               
RCVROL7  LHI   RE,FAROLE7-LINKD                                                 
         J     RCVROLE                                                          
RCVPCD7  LHI   RE,FAPCOD7-LINKD                                                 
         J     RCVPCOD                                                          
RCVRLV7  LHI   RE,FARLEV7-LINKD                                                 
         J     RCVRLEV                                                          
*                                                                               
RCVROL8  LHI   RE,FAROLE8-LINKD                                                 
         J     RCVROLE                                                          
RCVPCD8  LHI   RE,FAPCOD8-LINKD                                                 
         J     RCVPCOD                                                          
RCVRLV8  LHI   RE,FARLEV8-LINKD                                                 
         J     RCVRLEV                                                          
*                                                                               
RCVROL9  LHI   RE,FAROLE9-LINKD                                                 
         J     RCVROLE                                                          
RCVPCD9  LHI   RE,FAPCOD9-LINKD                                                 
         J     RCVPCOD                                                          
RCVRLV9  LHI   RE,FARLEV9-LINKD                                                 
         J     RCVRLEV                                                          
*                                                                               
RCVROL10 LHI   RE,FAROLE10-LINKD                                                
         J     RCVROLE                                                          
RCVPCD10 LHI   RE,FAPCOD10-LINKD                                                
         J     RCVPCOD                                                          
RCVRLV10 LHI   RE,FARLEV10-LINKD                                                
         J     RCVRLEV                                                          
*                                                                               
RCVROL11 LHI   RE,FAROLE11-LINKD                                                
         J     RCVROLE                                                          
RCVPCD11 LHI   RE,FAPCOD11-LINKD                                                
         J     RCVPCOD                                                          
RCVRLV11 LHI   RE,FARLEV11-LINKD                                                
         J     RCVRLEV                                                          
*                                                                               
RCVROL12 LHI   RE,FAROLE12-LINKD                                                
         J     RCVROLE                                                          
RCVPCD12 LHI   RE,FAPCOD12-LINKD                                                
         J     RCVPCOD                                                          
RCVRLV12 LHI   RE,FARLEV12-LINKD                                                
         J     RCVRLEV                                                          
*                                                                               
RCVROL13 LHI   RE,FAROLE13-LINKD                                                
         J     RCVROLE                                                          
RCVPCD13 LHI   RE,FAPCOD13-LINKD                                                
         J     RCVPCOD                                                          
RCVRLV13 LHI   RE,FARLEV13-LINKD                                                
         J     RCVRLEV                                                          
*                                                                               
RCVROL14 LHI   RE,FAROLE14-LINKD                                                
         J     RCVROLE                                                          
RCVPCD14 LHI   RE,FAPCOD14-LINKD                                                
         J     RCVPCOD                                                          
RCVRLV14 LHI   RE,FARLEV14-LINKD                                                
         J     RCVRLEV                                                          
*                                                                               
RCVROLE  LA    RE,LINKD(RE)                                                     
         MVC   0(L'FAROLE1,RE),DATA                                             
         J     EXITY                                                            
*                                                                               
RCVPCOD  LA    RE,LINKD(RE)                                                     
         MVC   0(L'FAPCOD1,RE),DATA                                             
         J     EXITY                                                            
*                                                                               
RCVRLEV  LA    RE,LINKD(RE)                                                     
         MVC   0(L'FARLEV1,RE),DATA                                             
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SETUP FOR GETOPTS                                                   *         
***********************************************************************         
         SPACE 1                                                                
SETGET   LR    R0,RE                                                            
         MVC   GOADM,DATAMGR                                                    
         XC    GOACLI(L'GOACLI+L'GOAPRO+L'GOAJOB),GOACLI                        
         XC    GOACOMP,GOACOMP                                                  
         MVC   GOSELCUL,CUL                                                     
*&&UK*&& MVC   GOCTRY,AGYCNTRY                                                  
         MVC   GOSELCLI,CLICODE                                                 
         MVC   GOSELPRO,PRODCODE                                                
         MVI   GOWHICH,0                                                        
         MVI   GOANYMED,0                                                       
         MVI   GOSELLEV,0                                                       
*        CLI   FAACTION,FQACTADD   IF ADD THERE IS NO OPTION AT JOB             
*        BE    EXIT                 LEVEL                                       
         MVC   GOSELJOB,BLANKS                                                  
         LA    RF,DUMFLDH                                                       
         USING FLDHDRD,RF                                                       
         XR    R1,R1                                                            
         IC    R1,FLDILEN                                                       
         BCTR  R1,0                                                             
         LARL  RE,SETSEL                                                        
         EX    R1,0(RE)                                                         
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  RF                                                               
SETSEL   MVC   GOSELJOB(0),FLDDATA-FLDHDRD(RF)                                  
         EJECT                                                                  
***********************************************************************         
* DO CLIENT POS (FIELDS 130 - 169)                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R2                                                        
DOCPOS   LR    R0,RE                                                            
         L     R2,AIO                                                           
         AHI   R2,ACCORFST                                                      
         J     DOCPOS4                                                          
                                                                                
DOCPOS2  LLC   R1,FFTLN                                                         
         AR    R2,R1                                                            
                                                                                
DOCPOS4  CLI   FFTEL,0                                                          
         JE    DOCPOSX                                                          
         CLI   FFTEL,FFTELQ                                                     
         JNE   DOCPOS2                                                          
         CLI   FFTTYPE,FFTTCLPO                                                 
         JNE   DOCPOS2                                                          
         LLC   R3,FFTDLEN                                                       
         SHI   R3,L'FFTPOAM                                                     
         LLC   RF,FFTSEQ                                                        
         SLL   RF,1                                                             
         AHI   RF,130                                                           
         GOTO1 ASNDDATA,DMCB,(RF),((R3),FFTPO#)                                 
         LLC   RF,FFTSEQ                                                        
         SLL   RF,1                                                             
         AHI   RF,131                                                           
         GOTO1 ASNDDATA,DMCB,(RF),((R3),FFTPOAM)                                
         J     DOCPOS2                                                          
                                                                                
DOCPOSX  LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DO FEE BUDGET AMOUNT (FIELD 170)                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING SCIELD,R2                                                        
DOFAMT   LR    R0,RE                                                            
         L     R2,AIO                                                           
         AHI   R2,ACCORFST                                                      
         J     DOFAMT4                                                          
                                                                                
DOFAMT2  LLC   R1,SCILN                                                         
         AR    R2,R1                                                            
                                                                                
DOFAMT4  CLI   SCIEL,0                                                          
         JE    DOFAMTX                                                          
         CLI   SCIEL,SCIELQ                                                     
         JNE   DOFAMT2                                                          
         CLI   SCITYPE,SCITFEEB                                                 
         JNE   DOFAMT2                                                          
         GOTO1 ASNDDATA,DMCB,170,(L'SCIAMNT,SCIAMNT)                            
                                                                                
DOFAMTX  LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD CLIENT POS                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R6                                                        
VALCLIPO NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    CL8'VALCLIPO'                                                    
         MVI   ELCODE,FFTELQ       REMOVE EXISTING ELEMENTS                     
         MVI   BYTE,0                                                           
         BAS   RE,GETELIO                                                       
         JNE   VCLIPO10                                                         
*                                                                               
VCLIPO02 CLI   FFTTYPE,FFTTCLPO                                                 
         JNE   VCLIPO04                                                         
         MVI   FFTEL,X'FF'                                                      
         MVI   BYTE,1                                                           
         J     VCLIPO06                                                         
*                                                                               
VCLIPO04 LLC   R1,FFTLN                                                         
         AR    R6,R1                                                            
         CLI   FFTEL,0                                                          
         JE    VCLIPO06                                                         
         CLI   FFTEL,FFTELQ                                                     
         JE    VCLIPO02                                                         
         J     VCLIPO04                                                         
*                                                                               
VCLIPO06 CLI   BYTE,1              DELETE THEM                                  
         JNE   VCLIPO10                                                         
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
VCLIPO10 LHI   R4,FLCLPO01-LINKD   CHECK FOR DATA PRESENT                       
         LA    R4,LINKD(R4)                                                     
         XR    R2,R2                                                            
*                                                                               
VCLIPO12 CLI   0(R4),0                                                          
         JE    VCLIPO14                                                         
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         MVI   FFTEL,FFTELQ        BUILD ELEMENT                                
         MVI   FFTTYPE,FFTTCLPO                                                 
         LLC   RF,0(R4)                                                         
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         EX    RF,4(R1)                                                         
         MVC   FFTPO#(0),1(R4)                                                  
         MVC   FFTPOAM,L'FLCLPO01+L'FACLPO01(R4)                                
         AHI   RF,L'FFTPOAM+1                                                   
         STC   RF,FFTDLEN                                                       
         AHI   RF,FFTLN1Q                                                       
         STC   RF,FFTLN                                                         
         STC   R2,FFTSEQ                                                        
         MVI   ELCODE,FFTELQ                                                    
         GOTO1 ADDELEM             ADD IT                                       
*                                                                               
VCLIPO14 AHI   R4,FLCLPO02-FLCLPO01                                             
         AHI   R2,1                NEXT ENTRY                                   
         CHI   R2,20                                                            
         JL    VCLIPO12                                                         
         J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE AEXRECD ON CHANGE                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING AEXRECD,R6                                                       
DELAEX   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    CL8'*DELAEX*'                                                    
         MVC   AIO,AIO2            USE IO2 HERE                                 
                                                                                
         LLC   R4,CNTAEX                                                        
         LA    R6,KEY                                                           
DELAEX2  XC    AEXKEY,AEXKEY                                                    
         MVI   AEXKTYP,AEXKTYPQ                                                 
         MVI   AEXKSUB,AEXKSUBQ                                                 
         MVC   AEXKCPY,CUL                                                      
         MVC   AEXKULC,CUL+1                                                    
         MVC   AEXKACC,SAVEJOB                                                  
         STC   R4,AEXKSEQ                                                       
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 READ                                                             
         JNE   DELAEX4                                                          
         L     R1,AIO                                                           
         OI    ACSTATUS-ACKEYD(R1),AEXSDELT                                     
         GOTOR MANAGER                                                          
                                                                                
DELAEX4  JCT   R4,DELAEX2                                                       
                                                                                
DELAEXX  MVC   AIO,AIO1            RESET IO AREA                                
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD FEE BUDGET AMOUNT                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING SCIELD,R6                                                        
VALFEEBA NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    CL8'VALFEEBA'                                                    
         MVI   ELCODE,SCIELQ       REMOVE EXISTING ELEMENT                      
         BAS   RE,GETELIO                                                       
         JNE   VFEEBA10                                                         
*                                                                               
VFEEBA02 CLI   SCITYPE,SCITFEEB                                                 
         JNE   VFEEBA04                                                         
         MVI   SCIEL,X'FF'                                                      
         MVI   ELCODE,X'FF'        DELETE IT                                    
         GOTO1 REMELEM                                                          
         J     VFEEBA10                                                         
*                                                                               
VFEEBA04 LLC   R1,SCILN                                                         
         AR    R6,R1                                                            
         CLI   SCIEL,0                                                          
         JE    VFEEBA10                                                         
         CLI   SCIEL,SCIELQ                                                     
         JE    VFEEBA02                                                         
         J     VFEEBA04                                                         
*                                                                               
VFEEBA10 OC    FAFEEAMT,FAFEEAMT                                                
         JZ    EXIT                                                             
         CP    FAFEEAMT,PZERO                                                   
         JE    EXIT                                                             
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         MVI   SCIEL,SCIELQ        BUILD ELEMENT                                
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITFEEB                                                 
         ZAP   SCIAMNT,FAFEEAMT                                                 
         MVI   ELCODE,SCIELQ                                                    
         GOTO1 ADDELEM             ADD IT                                       
         J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ADD OR UPDATE RECORD, DEPENDING ON FUNCTION                         *         
***********************************************************************         
         SPACE 1                                                                
MANAGER  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    CL8'71MANAG'                                                     
         L     R6,AIO                                                           
         GOTO1 DATAMGR,DMCB,FUNCTION,ACCOUNT,(R6),(R6),DMWORK                   
         CLI   DMCB+8,0                                                         
         JE    EXITY                                                            
                                                                                
         CLI   DMCB+8,X'80'      END OF FILE                                    
         JNE   *+8                                                              
         MVI   ERROR,EOF                                                        
                                                                                
         CLI   DMCB+8,X'40'      DISK ERROR                                     
         JNE   *+8                                                              
         MVI   ERROR,DISKERR                                                    
                                                                                
         CLI   DMCB+8,X'20'      DUPLICATE KEY ON ADD                           
         JNE   *+8                                                              
         MVI   ERROR,RECXIST                                                    
*&&US                                                                           
         TM    TRNRSTAT-TRNKEY(R6),TRNSDELT    IS RECORD DELETED?               
         JNO   *+8                                                              
         MVI   ERROR,DELEXIST                                                   
*&&                                                                             
                                                                                
         CLI   DMCB+8,X'10'      RECORD NOT FOUND                               
         JNE   *+8                                                              
         MVI   ERROR,NOTFOUND                                                   
                                                                                
         CLI   DMCB+8,X'02'      RECORD IS DELETED                              
         JNE   *+8                                                              
         MVI   ERROR,RECISDEL                                                   
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* STATUS BYTE TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
STATTAB  DC    AL2(RSTFILT1-RSTELD)                                             
         DC    AL2(RSTFILT2-RSTELD)                                             
         DC    AL2(RSTFILT3-RSTELD)                                             
         DC    AL2(RSTFILT4-RSTELD)                                             
         DC    AL2(RSTFILT5-RSTELD)                                             
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* EXTENDED JOB AREA                                                   *         
***********************************************************************         
         SPACE 1                                                                
AEXAREA  DC    (AEXAMAXQ)X'00'                                                  
AEXAMAXQ EQU   3000                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD A BLANK PROFILE ELEMENT                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING PPRELD,R6                                                        
ADDPROF  NMOD1 0,*ADDPRO*                                                       
         LR    RC,R1                                                            
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   PPREL,PPRELQ                                                     
         MVI   PPRLN,PPRNARRP-PPRELD                                            
*&&UK*&& MVC   PPRUWRK,BLANKS                                                   
*&&US*&& MVC   PPRUWRK(L'PPRUWRK*PPRUWRKN),BLANKS                               
         GOTO1 ADDELEM                                                          
         B     EXITY                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD A BLANK AST ELEMENT                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING ASTELD,R6                                                        
ADDASTA  NMOD1 0,*ADDAST*                                                       
         LR    RC,R1                                                            
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ASTEL,ASTELQ                                                     
         MVI   ASTLN,ASTLN1Q                                                    
         GOTO1 ADDELEM                                                          
         B     EXITY                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ADD BALANCE AND PEEL ELEMENTS                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING ABLELD,R6                                                        
BALPEEL  NMOD1 0,*BALPE**                                                       
         LR    RC,R1                                                            
         XC    ELEMENT(ABLLN3Q),ELEMENT                                         
         LA    R6,ELEMENT                                                       
         MVI   ABLEL,ABLELQ                                                     
         MVI   ABLLN,ABLLN3Q                                                    
         ZAP   ABLFRWD,PZERO                                                    
         ZAP   ABLDR,PZERO                                                      
         ZAP   ABLCR,PZERO                                                      
         ZAP   ABLURG,PZERO                                                     
         GOTO1 ADDELEM                                                          
         USING APOELD,R6                                                        
         XC    ELEMENT(APOLN1Q),ELEMENT                                         
         MVI   APOEL,APOELQ                                                     
         MVI   APOLN,APOLN1Q                                                    
         ZAP   APODR,PZERO                                                      
         ZAP   APOCR,PZERO                                                      
         GOTO1 ADDELEM                                                          
         B     EXITY                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* GET LEDGER ELEMENT STATUS 2                                         *         
***********************************************************************         
         SPACE 1                                                                
GETLDGS  NMOD1 0,*GTLDG**                                                       
         LR    RC,R1                                                            
         LA    R6,KEY                                                           
         USING ACTRECD,R6                                                       
         MVC   ACTKEY,BLANKS                                                    
         MVC   ACTKCULA(L'CUL),CUL                                              
         GOTO1 READ                                                             
         MVI   ELCODE,LDGELQ                                                    
         BAS   RE,GETELIO                                                       
         USING LDGELD,R6                                                        
         MVC   MYLSTA2,LDGSTAT2                                                 
         B     EXITY                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* GET PERSON CODE FROM BINARY PID                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING SA0REC,R5                                                        
GETPID   NMOD1 0,*GTPID**                                                       
         LR    RC,R1                                                            
                                                                                
         L     R5,AIO                                                           
         XC    WORK,WORK                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,CMPALPHA                                                 
         MVC   SA0KNUM,HALF                                                     
         MVC   SAVEKEY,SA0KEY                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,(R5),(R5)                             
         CLC   SAVEKEY(L'SA0KEY),SA0KEY                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,SA0DATA                                                       
         USING SAPALD,RF                                                        
         XR    R0,R0                                                            
GTPID02  CLI   SAPALEL,0                                                        
         BE    EXITN                                                            
         CLI   SAPALEL,SAPALELQ                                                 
         BE    *+14                                                             
         IC    R0,SAPALLN                                                       
         AR    RF,R0                                                            
         B     GTPID02                                                          
         MVC   WORK(L'SAPALPID),SAPALPID                                        
         B     EXITY                                                            
         DROP  R5,RF                                                            
         EJECT                                                                  
***********************************************************************         
* ADD DRAPASD PASSIVE FOR DRAFT JOB                                   *         
***********************************************************************         
         SPACE 1                                                                
ADDDRAP  NMOD1 0,*ADDDR**                                                       
         LR    RC,R1                                                            
         USING DRAPASD,R2                                                       
         L     R6,AIO                                                           
         L     R2,AIO3                                                          
         MVC   0(L'ACTKEY,R2),0(R6)                                             
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,(R2),(R2),0                   
         BE    *+6                                                              
         DC    H'0'                ACCOUNT NOT FOUND                            
                                                                                
         MVC   FULL,ACTKDA-ACTRECD(R2)                                          
         MVC   DUB,ACTKSTA-ACTRECD(R2)                                          
                                                                                
         XC    DRAPASD(ACCKLEN),DRAPASD                                         
         MVI   DRAPTYP,DRAPTYPQ                                                 
         MVI   DRAPSUB,DRAPSUBQ                                                 
         MVC   DRAPCPY,ACTKCPY-ACTRECD(R6)                                      
         MVI   DRAPAPS,DRAPDRA     DRAFT INDICATOR                              
         MVC   DRAPULA,ACTKULA-ACTRECD(R6)                                      
         MVC   DRAPPID,SVPASSWD    PERSON ADDED                                 
         MVC   DRAPDA,FULL                                                      
         MVC   DRAPSTA,DUB                                                      
         MVC   DRAPSOFF,EFFOFFC                                                 
                                                                                
         GOTO1 DATAMGR,DMCB,DMADD,ACCDIR,DRAPASD,DRAPASD,0                      
                                                                                
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACCOUNT NAME                                               *         
***********************************************************************         
         SPACE 1                                                                
VALNAME  NMOD1 0,*VNAME**                                                       
         LR    RC,R1                                                            
         USING NAMELD,R6                                                        
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   NAME020                                                          
         CLI   FLJBNAME,0                                                       
         BE    EXITY               DON'T ERASE THE NAME IF NONE SENT            
         XC    ELEMENT,ELEMENT                                                  
         GOTO1 REMELEM                                                          
*                                                                               
NAME020  XR    R1,R1                                                            
         ICM   R1,1,FLJBNAME                                                    
         BZ    EXITY                                                            
         BCTR  R1,0                                                             
         LA    R6,ELEMENT          SET UP TO BUILD NEW ELEMENT.                 
         MVI   NAMEL,NAMELQ                                                     
         MVC   NAMEREC(0),FAJBNAME                                              
         EX    R1,*-6                                                           
         AHI   R1,NAMLN1Q+1                                                     
         STC   R1,NAMLN                                                         
         GOTO1 ADDELEM             ADD THE NEW ELEMENT.                         
         B     EXITY                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE JOB NAME SEARCH PASSIVE RECORDS                              *         
***********************************************************************         
         SPACE 1                                                                
NAMESRCH NMOD1 0,**NAMSRC                                                       
         LR    RC,R1                                                            
         L     R2,AIO                                                           
         MVC   DMCB+1(3),=CL3'TF '                                              
         MVI   DMCB,C'A'           ADD                                          
         CLI   FAACTION,FQACTADD                                                
         BE    NSRCH04                                                          
         CLI   FAACTION,FQACTAUT                                                
         BE    NSRCH04                                                          
         MVI   DMCB,C'C'           CHANGE                                       
         TM    NAMEFLAG,NFCHANGE                                                
         BZ    NSRCHX                                                           
         NI    NAMEFLAG,X'FF'-NFCHANGE                                          
*                                                                               
NSRCH04  GOTO1 VACSRCHP,DMCB,,AIO,,OLDJOBNM,ACOMFACS,AACCFACS                   
*                                                                               
         TM    COMPSTAB,CPYSAJLA   TEST COMPANY USES AUTO JOB APPROVER          
         BZ    NSRCH20                                                          
         CLI   FAACTION,FQACTADD   ADD                                          
         BE    *+12                                                             
         CLI   FAACTION,FQACTAUT   ADD WITH AUTO NUMBERING                      
         BNE   NSRCH20                                                          
*                                                                               
         GOTO1 VJOBAPP,DMCB,(C'A',AIO),ACOMFACS                                 
         CLI   0(R1),3                                                          
         BNE   *+6                                                              
         DC    H'0'                DATAMGR ERROR                                
*                                                                               
         DS    0H                                                               
*                                                                               
NSRCH20  CLI   FAACTION,FQACTAUT   IF JOB AUTO  G?                              
         BNE   NSRCHX                                                           
         OC    SAVE1ST,SAVE1ST                                                  
         BZ    NSRCHX                                                           
*                                                                               
         MVC   IOKEY,SVJNKEY                                                    
         XC    DMCB(24),DMCB                                                    
         TM    RUNINDS,RUNIREAD                                                 
         BNZ   NSRCH25                                                          
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),ACCDIR,IOKEY,IOKEY                   
         CLI   DMCB+8,0            NOT FOUND                                    
         BE    NSRCH30                                                          
         DC    H'0'                                                             
NSRCH25  GOTO1 DATAMGR,DMCB,(X'00',DMREAD),ACCDIR,IOKEY,IOKEY                   
         CLI   DMCB+8,0            NOT FOUND                                    
         BE    NSRCH30                                                          
         DC    H'0'                                                             
NSRCH30  MVC   DISKDA,ACCKDA-ACCRECD+IOKEY                                      
*                                                                               
         XC    DMCB(24),DMCB                                                    
         L     R6,AIO2                                                          
         TM    RUNINDS,RUNIREAD                                                 
         BNZ   NSRCH35                                                          
         GOTO1 DATAMGR,DMCB,(X'80',GTREC),ACCMST,DISKDA,(R6),DMWORK             
         CLI   DMCB+8,X'00'                                                     
         BE    NSRCH40                                                          
         DC    H'0'                                                             
NSRCH35  GOTO1 DATAMGR,DMCB,(X'00',GTREC),ACCMST,DISKDA,(R6),DMWORK             
         CLI   DMCB+8,X'00'                                                     
         BE    NSRCH40                                                          
         DC    H'0'                                                             
*                                                                               
NSRCH40  MVC   AIO,AIO2                                                         
         USING JNAELD,R6                                                        
         MVI   ELCODE,JNAELQ       YES, GET ELEMENT                             
         MVC   HALF,DATADISP                                                    
         MVC   DATADISP,DATADIS2                                                
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DATADISP,HALF                                                    
         MVC   JNALNUM,LATECPJ                                                  
*                                                                               
         XC    DMCB(24),DMCB                                                    
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,PTREC,ACCMST,DISKDA,(R6),DMWORK                     
         CLI   DMCB+8,X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DISKDA,IOKEY,IOKEY                     
         CLI   DMCB+8,X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
*                                                                               
NSRCHX   B     EXITY                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE JOB=LANGUAGE AND EXPANDED JOB LOCKS                        *         
***********************************************************************         
         SPACE 1                                                                
VALLAL   NMOD1 0,**VLAL**                                                       
         L     RC,0(R1)                                                         
         L     R7,4(R1)                                                         
         ICM   R1,15,GOABEXT                                                    
         USING GOBBLOCK,R1                                                      
         MVC   MYVAL0,FAJULDEF                                                  
         MVC   MYVAL1,FAJLEST                                                   
         MVC   MYVAL2,FAJLORD                                                   
         MVC   MYVAL3,FAJLBIL                                                   
         MVC   MYVAL4,FAJLTSI                                                   
         MVC   MYVAL5,FAJLADJ                                                   
         MVC   MYVAL6,FAJLEXT                                                   
                                                                                
         CLI   FAACTION,FQACTADD   JOB USES FOREIGN LANGUAGE?                   
         BE    VALLAL2                                                          
         CLI   FAACTION,FQACTAUT                                                
         BNE   VALLAL4                                                          
VALLAL2  DS    0H                                                               
*&&UK                                                                           
         CLI   FAJULDEF,C' '       ANY OVERRIDES ON ADD?                        
         BH    *+10                                                             
         MVC   MYVAL0,GOJULDEF                                                  
*&&                                                                             
         CLI   FAJLEST,C' '                                                     
         BH    *+10                                                             
         MVC   MYVAL1,GOJLDEST                                                  
         CLI   FAJLORD,C' '                                                     
         BH    *+10                                                             
         MVC   MYVAL2,GOJLDORD                                                  
         CLI   FAJLBIL,C' '                                                     
         BH    *+10                                                             
         MVC   MYVAL3,GOJLDBIL                                                  
         CLI   FAJLTSI,C' '                                                     
         BH    *+10                                                             
         MVC   MYVAL4,GOJLDTSI                                                  
         CLI   FAJLADJ,C' '                                                     
         BH    *+10                                                             
         MVC   MYVAL5,GOJLDADJ                                                  
         CLI   FAJLEXT,C' '                                                     
         BH    *+10                                                             
         MVC   MYVAL6,GOJLDEXT                                                  
         DROP  R1                                                               
                                                                                
         USING RSTELD,R6                                                        
VALLAL4  MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   RSTLN,RSTLN3Q                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   MYVAL1,C' '                                                      
         BNH   *+20                                                             
         NI    RSTLSTAT,X'FF'-RSTLSESQ                                          
         CLI   MYVAL1,C'Y'                                                      
         BNE   *+8                                                              
         OI    RSTLSTAT,RSTLSESQ                                                
                                                                                
         CLI   MYVAL2,C' '                                                      
         BNH   *+20                                                             
         NI    RSTLSTAT,X'FF'-RSTLSORQ                                          
         CLI   MYVAL2,C'Y'                                                      
         BNE   *+8                                                              
         OI    RSTLSTAT,RSTLSORQ                                                
                                                                                
         CLI   MYVAL3,C' '                                                      
         BNH   *+20                                                             
         NI    RSTLSTAT,X'FF'-RSTLSBIQ                                          
         CLI   MYVAL3,C'Y'                                                      
         BNE   *+8                                                              
         OI    RSTLSTAT,RSTLSBIQ                                                
                                                                                
         CLI   MYVAL4,C' '                                                      
         BNH   *+20                                                             
         NI    RSTLSTAT,X'FF'-RSTLSTIQ                                          
         CLI   MYVAL4,C'Y'                                                      
         BNE   *+8                                                              
         OI    RSTLSTAT,RSTLSTIQ                                                
                                                                                
         CLI   MYVAL5,C' '                                                      
         BNH   *+20                                                             
         NI    RSTLSTAT,X'FF'-RSTLSADQ                                          
         CLI   MYVAL5,C'Y'                                                      
         BNE   *+8                                                              
         OI    RSTLSTAT,RSTLSADQ                                                
                                                                                
         CLI   MYVAL6,C' '                                                      
         BNH   *+20                                                             
         NI    RSTLSTAT,X'FF'-RSTLSEXQ                                          
         CLI   MYVAL6,C'Y'                                                      
         BNE   *+8                                                              
         OI    RSTLSTAT,RSTLSEXQ                                                
                                                                                
         USING ASTELD,R6                                                        
VALLAL6  MVI   ELCODE,ASTELQ                                                    
         BAS   RE,GETELIO          GET ASTEL                                    
         BE    VALLAL8                                                          
         CLI   MYVAL0,C'Y'                                                      
         BNE   VALLALX                                                          
         GOTO1 AADDASTA,(RC)       ADD IF NOT THERE                             
         B     VALLAL6                                                          
                                                                                
VALLAL8  CLI   MYVAL0,C' '                                                      
         BNH   VALLALX                                                          
         OI    ASTSTAT1,ASTISFOR                                                
         CLI   MYVAL0,C'Y'                                                      
         BE    VALLALX                                                          
         NI    ASTSTAT1,X'FF'-ASTISFOR                                          
                                                                                
VALLALX  DS    0H                                                               
         B     EXITY                                                            
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE STATUS ELEMENT                                             *         
***********************************************************************         
         SPACE 1                                                                
VALSTAT  NMOD1 0,*VSTAT**                                                       
         LR    RC,R1                                                            
         USING RSTELD,R6                                                        
         MVI   ELCODE,RSTELQ                                                    
         XC    ELEMENT(RSTLN3Q),ELEMENT                                         
         BAS   RE,GETELIO                                                       
         BNE   VSTAT010                                                         
         XR    RF,RF                                                            
         ICM   RF,1,RSTLN                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         MVC   ELEMENT(0),0(R6)                                                 
         EX    RF,*-6                                                           
         GOTO1 REMELEM                                                          
         LA    R6,ELEMENT                                                       
         OI    RSTFILT5,X'40'                                                   
         B     VSTAT020                                                         
*                                                                               
VSTAT010 LA    R6,ELEMENT                                                       
         MVI   RSTFILT1,C' '       FILTER 1                                     
         MVI   RSTFILT2,C' '       FILTER 2                                     
         MVI   RSTFILT3,C' '       FILTER 3                                     
         MVI   RSTFILT4,C' '       FILTER 4                                     
         MVI   RSTFILT5,C' '       FILTER 5                                     
*                                                                               
VSTAT020 L     RE,=A(STATTAB)      FOR ACCOUNTS OTHER THAN CLI/PRO/JOB,         
         A     RE,RELO                                                          
         LA    R2,FAFILT1          EXISTING FILTERS REMAIN IF THERE             
         MVI   MYCOUNT,1                                                        
         MVC   THISCOMP,CUL                                                     
*                                                                               
VSTAT040 CLI   0(RE),X'FF'                                                      
         BE    VSTAT100                                                         
*&&UK*&& TM    COMPSTA8,CPYSFNAM   COMPANY USES NAMES?                          
*&&UK*&& BZ    VSTAT050                                                         
         CLI   0(R2),C' '          ANY INPUT?                                   
         BNH   VSTAT050                                                         
         ST    RE,MYSVRE                                                        
         MVI   ERROR,INVFILT                                                    
         GOTO1 VPROFLT,DMCB,THISCOMP,ACOMFACS                                   
         ORG   *-2                                                              
         MVC   4(1,R1),MYCOUNT     PASS LEVEL                                   
         MVC   0(1,R1),0(R2)       AND FILTER VALUE                             
         BASR  RE,RF                                                            
         BNE   EXITN                                                            
         L     RE,MYSVRE                                                        
*                                                                               
VSTAT050 XR    R1,R1                                                            
         ICM   R1,3,0(RE)                                                       
         LA    R1,0(R1,R6)         R1 = A(FILTER IN STATUS ELEMENT).            
         CLI   0(R2),0                                                          
         BE    VSTAT060            NO INPUT                                     
         MVI   ERROR,INVALID                                                    
         CLI   0(R2),C'.'          PERIOD IS INVALID                            
         BE    EXITN                                                            
         MVC   0(1,R1),0(R2)       MOVE VALUE TO ELEMENT.                       
         OI    0(R1),X'40'         MAKE SURE UPPERCASE                          
         B     VSTAT080                                                         
*                                                                               
VSTAT060 MVI   0(R1),C' '                                                       
*                                                                               
VSTAT080 LA    R2,L'FAFILT1(R2)                                                 
         LA    RE,2(RE)            RE = A(NEXT FILTER TABLE ENTRY).             
         XR    R1,R1                                                            
         IC    R1,MYCOUNT                                                       
         AHI   R1,1                                                             
         STC   R1,MYCOUNT                                                       
         B     VSTAT040                                                         
*                                                                               
VSTAT100 MVI   RSTEL,RSTELQ        NEW ELEMENT                                  
         MVI   RSTLN,RSTLN3Q       COMPLETE CONSTRUCTION                        
         MVI   RSTCOSTG,C' '                                                    
         OC    RSTTDATE,RSTTDATE   TEST ANY DATE ALREADY                        
         BNZ   *+16                THEN DO NOT SET TODAY                        
         MVC   RSTTDATE,TODAYP                                                  
         MVC   RSTBDATE,RSTTDATE                                                
*                                   VALIDATE TIME SHEET FIELD                   
         NI    RSTSTAT5-RSTELD(R6),255-RSTSNOTS                                 
         CLI   FAEXCTMP,1                                                       
         BNE   *+8                                                              
         OI    RSTSTAT5-RSTELD(R6),RSTSNOTS      NO TIME SHEETS                 
*                                   VALIDATE PRIORITY FIELD                     
         NI    RSTSTAT6-RSTELD(R6),255-RSTSDTOP                                 
         CLI   FAPRIOTY,1                                                       
         BNE   *+8                                                              
         OI    RSTSTAT6-RSTELD(R6),RSTSDTOP      PRIORITY                       
         GOTO1 ADDELEM                                                          
         B     EXITY                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE CLOSING DATE                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING JOBELD,R6                                                        
VCLOSE   NMOD1 0,*VCLOS**                                                       
         LR    RC,R1                                                            
         CLI   JOBEL,JOBELQ                                                     
         JE    *+6                                                              
         DC    H'0'                R6 BUGGERED                                  
*                                                                               
         MVC   JOBCDATE,FACLOSDT   MOVE IN CLOSE DATE (PL3)                     
         XC    SVSTART,SVSTART                                                  
         MVC   SVSTART,JOBADATE    SAVE START DATE                              
         CLI   JOBLN,JOBLN2Q                                                    
         BL    VCLO005             ELEMENT TOO SMALL FOR OPENED DATE?           
         NC    JOBODATE,JOBODATE                                                
         BZ    VCLO005             NO OPEN DATE                                 
         MVC   SVSTART,JOBODATE                                                 
*                                                                               
VCLO005  OC    FACLOSDT,FACLOSDT   IF NO CLOSE DATE SENT USE AUTO               
         BNZ   VCLO020                                                          
         MVI   ERROR,NOAUTO        AUTO ONLY VALID WHEN ADDING JOBS             
         CLI   FAACTION,FQACTADD                                                
         BE    VCLO010                                                          
         CLI   FAACTION,FQACTAUT                                                
         BNE   EXITN                                                            
*                                                                               
VCLO010  LH    R3,GOAUTADD                                                      
         LTR   R3,R3                                                            
         BNZ   VCLO015                                                          
         ICM   R3,3,CMPDJCDO                                                    
*                                                                               
VCLO015  GOTO1 ADDAY,DMCB,OPEN,CLOSED,(R3)                                      
         GOTO1 DATCON,DMCB,(0,CLOSED),(1,JOBCDATE)                              
*                                                                               
VCLO020  OC    FAOPENDT,FAOPENDT                                                
         BZ    VCLO025                                                          
         MVC   MYOPEN,FAOPENDT     JUST MOVE IN OPEN DATE (PL3)                 
*                                                                               
VCLO025  DS    0H                                                               
*                                                                               
VCLO090  CLC   MYOPEN,JOBCDATE     ERROR IF OPENED LOWER THAN CLOSED            
         MVI   ERROR,INVDATE                                                    
         BH    EXITN                                                            
         CLI   JOBLN,0             IF LENGTH IS ZERO, WE ARE ADDING             
         BE    VCLO120                                                          
         OC    MYOPEN,MYOPEN                                                    
         BZ    VCLO100             NO OPEN DATE                                 
         CLI   JOBLN,JOBLN2Q       IS ELEMENT LONG ENOUGH FOR BOTH ?            
         BL    VCLO110             NO, LONGER ELEMENT IS NEEDED                 
         MVC   JOBODATE,MYOPEN     SET OPENED DATE TO HEX ZEROES                
         B     VCLO130                                                          
         SPACE 1                                                                
VCLO100  CLI   JOBLN,JOBLN2Q       IS THERE ROOM FOR OPENED DATE?               
         BL    VCLO130             NO, BUT THAT'S OK                            
         XC    JOBODATE,JOBODATE   SET OPENED DATE TO HEX ZEROES                
         B     VCLO130                                                          
         SPACE 1                                                                
VCLO110  XC    ELEMENT,ELEMENT                                                  
         XR    R1,R1                                                            
         IC    R1,JOBLN                                                         
         MVC   ELEMENT(0),JOBELD   MOVE OLD ELEMENT                             
         EX    R1,*-6                                                           
         MVI   ELCODE,ACJBELQ                                                   
         GOTO1 REMELEM             AND REMOVE IT                                
         LA    R6,ELEMENT                                                       
                                                                                
VCLO120  MVI   JOBLN,JOBLN4Q       MAKE NEW ELEMENT LARGER SIZE                 
         MVC   JOBODATE,MYOPEN                                                  
VCLO130  GOTO1 ADDELEM             ADD THE NEW ONE                              
                                                                                
         OC    MYOPEN,MYOPEN                                                    
         BZ    VCLO140             NO OPEN DATE USE START THEN                  
         CLC   SVSTART,MYOPEN      YES, HAS IT CHANGED ?                        
         BE    EXITY                                                            
         B     VCLO145                                                          
*                                                                               
VCLO140  CLC   SVSTART,JOBADATE                                                 
         BE    EXITY               NO, EXIT                                     
*                                                                               
VCLO145  MVI   NEWDATE,1           YES, SET FLAG                                
         B     EXITY                                                            
         SPACE 1                                                                
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE JOB (FOOT) COMMENTS                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING DUMFLDH,R2                                                       
         USING SCMELD,R6                                                        
VALJCOM  NMOD1 0,**VCOM**                                                       
         LR    RC,R1                                                            
         LA    R6,ELEMENT                                                       
*        CLI   FLJOBCM1,0          ANY I/P IN LINE1 ?                           
*        BE    EXITY                                                            
         MVI   SCMTYPE,0                                                        
         XC    BLOCK,BLOCK                                                      
         GOTO1 SCANNER,DMCB,DUMFLDH,BLOCK,0                                     
         XR    R0,R0                                                            
         ICM   R0,1,DMCB+4                                                      
         BZ    VJC020                                                           
         CLI   BLOCK+1,0                                                        
         BNE   VJC040                                                           
*                                                                               
VJC020   XR    R1,R1                                                            
         IC    R1,DUMFLDH+5                                                     
         BCTR  R1,0                                                             
         MVC   SCMNARR(0),DUMFLD                                                
         EX    R1,*-6                                                           
         LA    R1,5(R1)                                                         
         STC   R1,SCMLN                                                         
         GOTO1 ADDELEM                                                          
         XR    R1,R1                                                            
         IC    R1,SCMSEQ                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SCMSEQ                                                        
         B     EXITY                                                            
*                                                                               
VJC040   LA    R5,BLOCK                                                         
         XC    KEY,KEY                                                          
         USING SCMRECD,KEY                                                      
         MVI   SCMKTYP,SCMKTYPQ                                                 
         MVC   SCMKCPY,CUL                                                      
*                                                                               
VJC060   MVI   SCMLN,SCMLN2Q                                                    
         MVI   SCMTYPE,SCMTPRES+SCMTPRAD                                        
         CLC   12(3,R5),=C'EST='                                                
         BE    VJC080                                                           
         MVI   SCMTYPE,SCMTPRBI+SCMTPRAD                                        
         CLC   12(3,R5),=C'BIL'                                                 
         BE    VJC080                                                           
         CLC   12(3,R5),=C'B+E'                                                 
         BNE   VJC100                                                           
         OI    SCMTYPE,SCMTPRES                                                 
*                                                                               
VJC080   MVC   SCMNARR,BLANKS                                                   
         XR    R1,R1                                                            
         ICM   R1,1,1(R5)                                                       
         BZ    VJC100                                                           
         CLI   1(R5),6             MUST BE 1-6 BYTES LONG                       
         BH    VJC100                                                           
         LA    RF,6                                                             
         SR    RF,R1                                                            
         LA    RF,SCMNARR(RF)                                                   
         BCTR  R1,0                                                             
         MVC   0(0,RF),22(R5)                                                   
         EX    R1,*-6                                                           
         MVC   SCMKCODE,SCMNARR                                                 
         MVC   AIO,AIO2              USE OTHER BUFFER TO READ                   
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(42),KEY                                                  
         BNE   VJC100                                                           
         MVC   AIO,AIO1              GET JOB RECORD BACK                        
         GOTO1 ADDELEM                                                          
         XR    R1,R1                                                            
         IC    R1,SCMSEQ                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SCMSEQ                                                        
         LA    R5,32(R5)                                                        
         BCT   R0,VJC060                                                        
         B     EXITY                                                            
*                                                                               
VJC100   MVI   ERROR,INVALID                                                    
         B     EXITN                                                            
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* GET USER SELECT FIELD DEFINITIONS                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING USERD,R5                                                         
GETUSR   NMOD1 0,**GUSE**                                                       
         LR    RC,R1                                                            
         MVC   AIO,AIO2                                                         
         ZAP   ELMCNT,PZERO        CLEAR COUNTER                                
         LA    R5,USERKEY          LOOK FOR COMPANY LEVEL                       
         XC    USERKEY,USERKEY                                                  
         BAS   RE,READHIU                                                       
         CLC   KEYSAVE(UFSKOFG-UFSRECD),KEY  EXIT IF NO RECS FOR THIS           
         BNE   GETUSRX                       COMPANY                            
         CLC   KEYSAVE(L'UFSKEY),KEY      SAVE HEADERS IF RECORD FOUND          
         BNE   GETMG                                                            
         BAS   RE,USERTABL                                                      
                                                                                
GETMG    OC    MGROUP,MGROUP       LOOK FOR COMPANY, MEDIA GROUP                
         BZ    GETMED                                                           
         XC    USERKEY,USERKEY                                                  
         MVC   USERMG,MGROUP                                                    
         BAS   RE,READHIU                                                       
         BNE   GETMED                                                           
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
                                                                                
GETMED   OC    MEDIA,MEDIA         LOOK FOR COMPANY, MEDIA                      
         BZ    GETOGR                                                           
         XC    USERKEY,USERKEY                                                  
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIU                                                       
         BNE   GETOGR                                                           
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
                                                                                
GETOGR   OC    EFFOFG,EFFOFG       LOOK FOR COMPANY, OFFICE GROUP               
         BZ    GETOFF                                                           
         XC    USERKEY,USERKEY                                                  
         MVC   USEROFG,EFFOFG                                                   
         BAS   RE,READHIU                                                       
         CLC   KEYSAVE(UFSKOFC-UFSRECD),KEY  SKIP IF NO OGR RECS                
         BNE   GETOFF                                                           
         CLC   KEYSAVE(L'UFSKEY),KEY     SAVE HEADERS IF RECORD FOUND           
         BNE   GUOG020                                                          
         BAS   RE,USERTABL                                                      
                                                                                
GUOG020  OC    MGROUP,MGROUP       LOOK FOR COMPANY, OFFICE GROUP,              
         BZ    GUOG040              MEDIA GROUP                                 
         MVC   USERMG,MGROUP                                                    
         BAS   RE,READHIU                                                       
         BNE   GUOG040             SAVE HEADERS IF RECORD FOUND                 
         BAS   RE,USERTABL                                                      
                                                                                
GUOG040  OC    MEDIA,MEDIA         LOOK FOR COMPANY, OFFICE GROUP,              
         BZ    GETOFF               MEDIA                                       
         XC    USERMG,USERMG                                                    
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIU                                                       
         BNE   GETOFF              SAVE HEADERS IF RECORD FOUND                 
         BAS   RE,USERTABL                                                      
                                                                                
GETOFF   CLC   EFFOFFC,BLANKS      LOOK FOR COMPANY, OFFICE                     
         BNH   GETCLI                                                           
         XC    USERKEY,USERKEY                                                  
         MVC   USEROFF,EFFOFFC                                                  
         BAS   RE,READHIU                                                       
         CLC   KEYSAVE(UFSKCLI-UFSRECD),KEYSAVE                                 
         BNE   GETCLI                 SKIP IF NO OFFICE RECORDS                 
         CLC   KEYSAVE(L'UFSKEY),KEY  SAVE HEADER IF REC FOUND                  
         BNE   GUOF020                                                          
         BAS   RE,USERTABL                                                      
                                                                                
GUOF020  OC    MGROUP,MGROUP       LOOK FOR COMPANY, OFFICE,                    
         BZ    GUOF040              MEDIA GROUP                                 
         MVC   USERMG,MGROUP                                                    
         BAS   RE,READHIU                                                       
         BNE   *+8                                                              
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
         XC    USERMG,USERMG                                                    
                                                                                
GUOF040  OC    MEDIA,MEDIA         LOOK FOR COMPANY, OFFICE,                    
         BZ    GETCLI               MEDIA                                       
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIU                                                       
         CLC   KEYSAVE(L'UFSKEY),KEY     SAVE HEADERS IF RECORD FOUND           
         BNE   GETCLI                                                           
         BAS   RE,USERTABL                                                      
                                                                                
GETCLI   XC    USERKEY,USERKEY     LOOK FOR COMPANY, CLIENT                     
         MVC   USERCLI,CLICODE                                                  
         BAS   RE,READHIU                                                       
         CLC   KEYSAVE(UFSKPRO-UFSRECD),KEY                                     
         BNE   GETUSRX                SKIP IF NO CLIENT RECS                    
         CLC   KEYSAVE(L'UFSKEY),KEY  SAVE HEADERS IF REC FOUND                 
         BNE   GUCLI020                                                         
         BAS   RE,USERTABL                                                      
                                                                                
GUCLI020 OC    MGROUP,MGROUP       LOOK FOR COMPANY, CLIENT, MEDIA              
         BZ    GUCLI040             GROUP                                       
         MVC   USERMG,MGROUP                                                    
         BAS   RE,READHIU                                                       
         BNE   *+8                                                              
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
         XC    USERMG,USERMG                                                    
                                                                                
GUCLI040 OC    MEDIA,MEDIA         LOOK FOR COMPANY, CLIENT, MEDIA              
         BZ    GUCLI060                                                         
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIU                                                       
         BNE   *+8                                                              
         BAS   RE,USERTABL         SAVE HEADERS IF KEYS MATCH                   
         XC    USERMED,USERMED                                                  
                                                                                
GUCLI060 MVC   USERPRO,PRODCODE    LOOK FOR COMPANY, CLIENT,                    
         BAS   RE,READHIU           PROGUCT                                     
         BNE   *+8                                                              
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
         OC    MGROUP,MGROUP       LOOK FOR COMPANY, CLIENT,                    
         BZ    GUCLI080             PRODUCT, MEDIA GROUP                        
         MVC   USERMG,MGROUP                                                    
         BAS   RE,READHIU                                                       
         BNE   *+8                                                              
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
         XC    USERMG,USERMG                                                    
                                                                                
GUCLI080 OC    MEDIA,MEDIA         LOOK FOR COMPANY, CLIENT,                    
         BZ    GUCLI100             PRODUCT, MEDIA                              
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIU                                                       
         BNE   *+8                                                              
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
         XC    USERMED,USERMED                                                  
                                                                                
GUCLI100 MVC   USERJOB,FAJOB       LOOK FOR COMPANY, CLIENT,                    
         OC    USERJOB,BLANKS      PRODUCT, JOB                                 
         BAS   RE,READHIU                                                       
         BNE   *+8                                                              
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
                                                                                
GETUSRX  MVC   AIO,AIO1                                                         
         B     EXITY                                                            
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* FORMAT KEY FOR UFSRECD AND READ HI                                  *         
***********************************************************************         
         SPACE 1                                                                
READHIU  NTR1                                                                   
         LA    R6,KEY                                                           
         USING UFSRECD,R6                                                       
         MVC   UFSKEY,USERKEY                                                   
         MVI   UFSKTYP,UFSKTYPQ                                                 
         MVI   UFSKSUB,UFSKSUBQ                                                 
         MVC   UFSKCPY,CUL                                                      
         MVC   UFSKUNT(2),PRODLEDG                                              
         GOTO1 HIGH                                                             
         CLC   UFSKEY,KEYSAVE                                                   
         XIT1                                                                   
         DROP  R6                                                               
***********************************************************************         
* GET USER SELECT DEFINITION INTO TABLE                               *         
***********************************************************************         
         SPACE 1                                                                
         USING ELMTABD,R3                                                       
         USING UFSELD,R6                                                        
USERTABL NTR1                                                                   
         MVI   ELCODE,UFSELQ                                                    
         BAS   RE,GETELIO                                                       
         B     *+8                                                              
                                                                                
USET020  BAS   RE,NEXTEL                                                        
         BNE   EXITY                                                            
         CLC   UFSDESC,BLANKS                                                   
         BE    USET020                                                          
         LA    R3,ELMTAB                                                        
         LA    R0,10                                                            
                                                                                
USET040  OC    ELMDATA,ELMDATA     IS THIS SPOT FILLED?                         
         BZ    USET080             NO, KEEP LOOKING                             
         CLC   UFSCODE,ELMCODE     YES, DO THE CODES MATCH ?                    
         BNE   USET080             NO                                           
         OC    UFSCUT,UFSCUT       YES, DO WE HAVE A CUTOFF DATE ?              
         BZ    USET060             NO, REPLACE ELEMENT                          
         CLC   UFSCUT,SVSTART      YES, CAN WE TAKE IT ?                        
         BH    USET060             YES                                          
*                                                                               
         BCTR  R0,0                                                             
         MVC   ELMDATA,ELMLNG(R3)                                               
         LA    R3,ELMLNG(R3)                                                    
         BCT   R0,*-10                                                          
         XC    ELMDATA,ELMDATA     CLEAR LAST SPOT                              
         SP    ELMCNT,=P'1'        DECREASE THE COUNT BY 1                      
         B     USET020             GET NEXT ELEMENT                             
*                                                                               
USET060  MVC   ELMDATA,0(R6)       REPLACE ELEMENT                              
         B     USET020             NEXT NEXT ONE                                
                                                                                
USET080  LA    R3,ELMLNG(R3)       GET NEXT                                     
         BCT   R0,USET040                                                       
                                                                                
         LA    R3,ELMTAB           NO MATCH, LOOK FOR VACANT SPOT               
         LA    R0,10                                                            
                                                                                
USET100  OC    ELMDATA,ELMDATA     IS THIS SPOT FILLED ?                        
         BNZ   USET120             YES, KEEP LOOKING                            
         OC    UFSCUT,UFSCUT       NO, DO WE HAVE A CUTOFF DATE ?               
         BZ    USET110             NO, ADD ELEMENT                              
         CLC   UFSCUT,SVSTART      YES, CAN WE TAKE IT ?                        
         BNH   USET020             NO                                           
                                                                                
USET110  MVC   ELMDATA,0(R6)       ADD ELEMENT                                  
         AP    ELMCNT,=P'1'        KEEP A COUNT                                 
         B     USET020             GET NEXT ELEMENT                             
                                                                                
USET120  LA    R3,ELMLNG(R3)       GET NEXT                                     
         BCT   R0,USET100                                                       
         B     EXITY                                                            
         LTORG                                                                  
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ADD PIDKJSAQ/PIDKJSSQ PASSIVES FOR SELF APPROVAL                    *         
***********************************************************************         
         SPACE 1                                                                
         USING PIDRECD,R2                                                       
ADDPASS  NMOD1 0,**ADDPAS                                                       
         LR    RC,R1                                                            
         L     R6,AIO                                                           
         L     R2,AIO3                                                          
         MVC   0(L'ACTKEY,R2),0(R6)                                             
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,(R2),(R2),0                   
         BE    *+6                                                              
         DC    H'0'                ACCOUNT NOT FOUND                            
                                                                                
         MVC   FULL,ACTKDA-ACTRECD(R2)                                          
         MVC   DUB,ACTKSTA-ACTRECD(R2)                                          
                                                                                
         XC    PIDRECD(ACCKLEN),PIDRECD                                         
         MVI   PIDKTYP,PIDKTYPQ    CREATE THE PASSIVE POINTER                   
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,ACTKCPY-ACTRECD(R6)                                      
         MVI   PIDKSTYP,PIDKJOBQ                                                
         MVI   PIDKJST,PIDKJSAQ                                                 
         MVC   PIDKACC,ACTKACT-ACTRECD(R6)                                      
         MVC   PIDKPID,SVPASSWD    FOR PERSON APPROVED                          
         GOTO1 DATCON,DMCB,(5,0),(2,PIDKADAT)                                   
         MVC   PIDKDA,FULL                                                      
         MVC   PIDKSTA,DUB                                                      
*&&UK*&& LA    RE,PIDRECD+ACTKSUFA-ACTRECD                                      
*&&UK*&& MVC   0(2,RE),SAVEOFC     ASSUME JOB HAS NO OFFICE SET                 
                                                                                
         GOTO1 DATAMGR,DMCB,DMADD,ACCDIR,PIDRECD,PIDRECD,0                      
                                                                                
         XC    PIDRECD(ACCKLEN),PIDRECD                                         
         MVI   PIDKTYP,PIDKTYPQ    CREATE THE PASSIVE POINTER                   
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,ACTKCPY-ACTRECD(R6)                                      
         MVI   PIDKSTYP,PIDKJOBQ                                                
         MVI   PIDKJST,PIDKJSSQ                                                 
         MVC   PIDKACC,ACTKACT-ACTRECD(R6)                                      
         MVC   PIDKPID,SVPASSWD    FOR PERSON ADDED/CREATED                     
         GOTO1 DATCON,DMCB,(5,0),(2,PIDKADAT)                                   
         MVC   PIDKDA,FULL                                                      
         MVC   PIDKSTA,DUB                                                      
*&&UK*&& LA    RE,PIDRECD+ACTKSUFA-ACTRECD                                      
*&&UK*&& MVC   0(2,RE),SAVEOFC     ASSUME JOB HAS NO OFFICE SET                 
                                                                                
         GOTO1 DATAMGR,DMCB,DMADD,ACCDIR,PIDRECD,PIDRECD,0                      
                                                                                
         B     EXITY                                                            
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ADD ESTIMATES, IF REQUIRED                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING ACKEYD,R6                                                        
ADDEST   NMOD1 0,**ADDEST                                                       
         LR    RC,R1                                                            
*                                                                               
ADDEST2  LR    R6,R2               SAVE CURSOR POSITION                         
         L     R2,AIO              CLEAR AIO                                    
         LA    R3,1000                                                          
         XR    R1,R1                                                            
         MVCL  R2,R0                                                            
         LR    R2,R6               RESTORE CURSOR POSITION                      
*                                                                               
         USING ACEVKEY,R6                                                       
         L     R6,AIO                                                           
         XC    ACEVKEY,ACEVKEY                                                  
         MVI   ACEVRTYP,ACEVEQU                                                 
         MVI   ACEVSREC,ACEVSEQU                                                
         MVC   ACEVCUL,CUL                                                      
         MVC   ACEVCLI,CLICODE                                                  
         MVC   ACEVPROD,PRODCODE                                                
         MVC   ACEVJOB,JOBNUM                                                   
*                                                                               
         MVC   ACEVTYPE,ADDTYPE                                                 
         STC   R5,ACEVERS          R5 IS ESTIMATE NUMBER                        
         MVC   ACLENGTH,=Y(ACRECORD-ACKEYD+1) INITIALIZE RECORD LENGTH          
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),ACCDIR,(R6),IOKEY                    
         TM    DMCB+8,X'10'        NOT FOUND                                    
         BO    ADDEST8                                                          
                                                                                
K        USING ACCRECD,IOKEY                                                    
         NI    K.ACCKSTA,X'FF'-X'80' MAKE SURE NOT DELETED                      
         MVC   DISKDA,K.ACCKDA                                                  
         DROP  K                                                                
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,(R6),IOKEY                             
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,(X'80',GTREC),ACCMST,DISKDA,(R6),DMWORK             
         CLI   DMCB+8,X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
R        USING ACCRECD,R6                                                       
         NI    R.ACCRSTA,X'FF'-X'80'    MAKE SURE NOT DELETED                   
         LA    R2,R.ACCRFST             CLEAR THE RECORD                        
         XR    R3,R3                                                            
         ICM   R3,3,R.ACCRLEN                                                   
         XR    R1,R1                                                            
         MVCL  R2,R0                                                            
                                                                                
         MVC   R.ACCRLEN,=Y(ACCRFST-ACCRECD+1)  INITIALISE LENGTH               
         MVC   FUNCTION,=CL8'PUTREC'                                            
         MVC   ACCT,=C'ACCMST '                                                 
         DROP  R                                                                
                                                                                
         USING ACEUD,R1                                                         
ADDEST8  XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         MVI   ACEUEL,ACEUELQ      BUILD AN ESTIMATE UPDATE ELEMENT             
         MVI   ACEULEN,ACEULENQ                                                 
         MVC   ACEUADD,TODAYP                                                   
         MVC   ACEUPERS,TWAALIAS                                                
         MVC   ACEULAST,TODAYP                                                  
*        GOTO1 ADDELEM                                                          
         GOTO1 HELLO,DMCB,(C'P',ACCT),ACEVKEY,ELEM                              
*&&UK                                                                           
         USING FFTELD,R1                                                        
         TM    COMPSTA7,CPYSSCNV   TEST AGENCY IN EURO ZONE                     
         BNO   ADDEST10                                                         
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         MVI   FFTEL,FFTELQ        BUILD AN ESTIMATE UPDATE ELEMENT             
         MVI   FFTLN,FFTLN1Q+4                                                  
         MVI   FFTTYPE,FFTTACUR                                                 
         MVI   FFTDLEN,L'PRODCURR                                               
         MVC   FFTDATA(L'PRODCURR),PRODCURR                                     
*        GOTO1 ADDELEM                                                          
         GOTO1 HELLO,DMCB,(C'P',ACCT),ACEVKEY,ELEM                              
*&&                                                                             
ADDEST10 CLC   FUNCTION,=CL8'PUTREC'                                            
         BE    ADDEST12                                                         
         MVC   FUNCTION,=CL8'DMADD'                                             
         GOTOR MANAGER                                                          
         BCT   R5,ADDEST2                                                       
         B     ADDESTX                                                          
*                                                                               
ADDEST12 XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,FUNCTION,ACCMST,DISKDA,(R6),DMWORK                  
         CLI   DMCB+8,X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         BCT   R5,ADDEST2                                                       
                                                                                
ADDESTX  B     EXITY                                                            
         LTORG                                                                  
         DROP  R1,R6                                                            
ACCT     DC    C'ACCOUNT'                                                       
         EJECT                                                                  
***********************************************************************         
* GET APPROVER                                                        *         
***********************************************************************         
         SPACE 1                                                                
GETAPP   NMOD1 0,**GETAPP                                                       
         LR    RC,R1                                                            
         XC    MYBYTE2,MYBYTE2                                                  
         XC    MYAPPST,MYAPPST                                                  
                                                                                
         LA    R2,APRTAB                                                        
         USING APRTABD,R2                                                       
SJ       USING JOBPASD,IOKEY       Find job, product or client approver         
GETAPP02 XC    SJ.JOBPAS,SJ.JOBPAS                                              
         MVI   SJ.JOBPTYP,JOBPTYPQ                                              
         MVI   SJ.JOBPSUB,JOBPSUBQ                                              
         MVC   SJ.JOBPCPY,CUL                                                   
         MVI   SJ.JOBPAPPL,JOBPAJOB                                             
         MVI   SJ.JOBPVIEW,JOBPVOFF                                             
         MVC   SJ.JOBPCOFF,BLANKS                                               
         MVC   SJ.JOBPCMED,BLANKS                                               
         MVC   SJ.JOBPCPJ,BLANKS                                                
         OC    APRSTAT,APRSTAT                                                  
         JNZ   GETAPP04                                                         
         MVI   SJ.JOBPCODE,X'FF'                                                
         MVC   SJ.JOBPCODE+1(L'JOBPCODE-1),SJ.JOBPCODE                          
         J     GETAPP12                                                         
*                                                                               
GETAPP04 TM    APRSTAT,APRPRO                                                   
         JZ    GETAPP06                                                         
         MVC   SJ.JOBPCPJ(L'CLICODE),CLICODE                                    
         XR    RE,RE                                                            
         IC    RE,LCLI                                                          
         LA    RE,SJ.JOBPCPJ(RE)                                                
         MVC   0(L'PRODCODE,RE),PRODCODE                                        
         J     GETAPP08                                                         
GETAPP06 TM    APRSTAT,APRCLI                                                   
         JZ    GETAPP08                                                         
         MVC   SJ.JOBPCPJ(L'CLICODE),CLICODE                                    
                                                                                
GETAPP08 TM    APRSTAT,APRMED                                                   
         JZ    GETAPP10                                                         
         CLC   MEDIA,BLANKS                                                     
         JNH   GETAPP28                                                         
         MVC   SJ.JOBPCMED,MEDIA                                                
                                                                                
GETAPP10 TM    APRSTAT,APROFF                                                   
         JZ    GETAPP12                                                         
         CLC   EFFOFFC,BLANKS                                                   
         JNH   GETAPP28                                                         
         MVC   SJ.JOBPCOFF,EFFOFFC                                              
                                                                                
GETAPP12 MVC   SAVEKEY,SJ.JOBPAS                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,SJ.JOBPASD,SJ.JOBPASD                 
         B     GETAPP26                                                         
                                                                                
GETAPP24 GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,SJ.JOBPASD,SJ.JOBPASD                 
                                                                                
GETAPP26 CLC   SJ.JOBPAS(JOBPPIDB-JOBPASD),SAVEKEY                              
         JE    GETAPP30                                                         
         CLI   MYBYTE2,1           Have we found approvers                      
         JE    GETAPPX             Yes - exit                                   
GETAPP28 LA    R2,APRTABL(R2)                                                   
         CLI   0(R2),X'FF'                                                      
         JNE   GETAPP02                                                         
         J     GETAPPX                                                          
                                                                                
GETAPP30 MVI   MYBYTE2,1           Set we found approver level                  
         TM    SJ.JOBPSTAT,JOBPDFLT                                             
         JZ    GETAPP24                                                         
         MVC   MYAPPST,SJ.JOBPPIDB    client level approver found               
         DROP  SJ,R2                                                            
                                                                                
GETAPPX  B     EXITY                                                            
                                                                                
APRTAB   DS    0XL1                                                             
         DC    B'10000000'         Client                                       
         DC    B'10010000'         Client/media                                 
         DC    B'11011000'         Office/client/product/media                  
         DC    B'10011000'         Office/client/media                          
         DC    B'00011000'         Office/media                                 
         DC    B'00010000'         Media                                        
         DC    B'11001000'         Office/client/product                        
         DC    B'10001000'         Office/client                                
         DC    B'00001000'         Office                                       
         DC    B'00000000'         Agency                                       
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                  FIND APPROPRIATE JOB NUMBER RECORD                 *         
***********************************************************************         
*                                                                               
         USING USERD,R5                                                         
FINDJOB  NMOD1 0,**FJOB**                                                       
         LR    RC,R1                                                            
         MVC   AIO,AIO2                                                         
         LA    R5,USERKEY                                                       
         XC    USERKEY,USERKEY                                                  
         XC    EFFDATE,EFFS                                                     
*                                                                               
         MVC   USERCLI,CLICODE     LOOK FOR COMPANY, CLIENT,                    
         MVC   USERPRO,PRODCODE     PRODUCT, MEDIA                              
         OC    MEDIA,MEDIA                                                      
         BZ    FIND02                                                           
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIJ                                                       
         BE    FIND26                                                           
         XC    USERMED,USERMED                                                  
*                                                                               
FIND02   OC    MGROUP,MGROUP       LOOK FOR COMPANY, CLIENT                     
         BZ    FIND04               PRODUCT, MEDIA GROUP                        
         MVC   USERMG,MGROUP                                                    
         BAS   RE,READHIJ                                                       
         BE    FIND26                                                           
         XC    USERMG,USERMG                                                    
*                                                                               
FIND04   BAS   RE,READHIJ          LOOK FOR COMPANY, CLIENT                     
         BE    FIND26                PRODUCT                                    
*                                                                               
         XC    USERPRO,USERPRO     LOOK FOR COMPANY, CLIENT                     
         OC    MEDIA,MEDIA          MEDIA                                       
         BZ    FIND06                                                           
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIJ                                                       
         BE    FIND26                                                           
         XC    USERMED,USERMED                                                  
*                                                                               
FIND06   OC    MGROUP,MGROUP       LOOK FOR COMPANY, CLIENT                     
         BZ    FIND08               MGROUP                                      
         MVC   USERMG,MGROUP                                                    
         BAS   RE,READHIJ                                                       
         BE    FIND26                                                           
         XC    USERMG,USERMG                                                    
*                                                                               
FIND08   BAS   RE,READHIJ          LOOK FOR COMPANY, CLIENT                     
         BE    FIND26                                                           
*                                                                               
         XC    USERCLI,USERCLI     LOOK FOR COMPANY, OFFICE                     
         OC    MEDIA,MEDIA          MEDIA                                       
         BZ    FIND10                                                           
         MVC   USERMED,MEDIA                                                    
         MVC   USEROFF,EFFOFFC                                                  
         BAS   RE,READHIJ                                                       
         BE    FIND26                                                           
         XC    USERMED,USERMED                                                  
*                                                                               
FIND10   OC    MGROUP,MGROUP       LOOK FOR COMPANY, OFFICE                     
         BZ    FIND12               MGROUP                                      
         MVC   USERMG,MGROUP                                                    
         BAS   RE,READHIJ                                                       
         BE    FIND26                                                           
         XC    USERMG,USERMG                                                    
*                                                                               
FIND12   BAS   RE,READHIJ          LOOK FOR COMPANY, OFFICE                     
         BE    FIND26                                                           
         XC    USEROFF,USEROFF                                                  
*                                                                               
         OC    EFFOFG,EFFOFG       LOOK FOR COMPANY, OFFICE GROUP               
         BZ    FIND18               MEDIA                                       
         MVC   USEROFG,EFFOFG                                                   
         OC    MEDIA,MEDIA                                                      
         BZ    FIND14                                                           
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIJ                                                       
         BE    FIND26                                                           
         XC    USERMED,USERMED                                                  
*                                                                               
FIND14   OC    MGROUP,MGROUP       LOOK FOR COMPANY, OFFICE GROUP,              
         BZ    FIND16               MEDIA GROUP                                 
         MVC   USERMG,MGROUP                                                    
         BAS   RE,READHIJ                                                       
         BE    FIND26                                                           
         XC    USERMG,USERMG                                                    
*                                                                               
FIND16   BAS   RE,READHIJ          LOOK FOR COMPANY, OFFICE GROUP               
         BE    FIND26                                                           
         XC    USEROFG,USEROFG                                                  
*                                                                               
FIND18   OC    MEDIA,MEDIA         LOOK FOR COMPANY, MEDIA                      
         BZ    FIND20                                                           
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIJ                                                       
         BE    FIND26                                                           
         XC    USERMED,USERMED                                                  
*                                                                               
FIND20   OC    MGROUP,MGROUP       LOOK FOR COMPANY, MGROUP                     
         BZ    FIND22                                                           
         MVC   USERMG,MGROUP                                                    
         BAS   RE,READHIJ                                                       
         BE    FIND26                                                           
         XC    USERMG,USERMG                                                    
*                                                                               
FIND22   BAS   RE,READHIJ          LOOK FOR COMPANY                             
         BE    FIND26                                                           
*                                                                               
FIND24   LA    R2,CONACTH                                                       
         ST    R2,ACURFORC                                                      
*&&UK*&& MVI   ERROR,NOJNUMRC                                                   
*&&US*&& MVI   ERROR,NOJNUM                                                     
         B     EXITN                                                            
*                                                                               
         USING AJNRECD,R6                                                       
FIND26   ST    RE,SAVERE                                                        
*                                                                               
FIND26A  L     R6,AIO                                                           
         TM    AJNRSTAT,AJNNDACT   IS THE RECORD ACTIVE ?                       
         BNZ   FIND26B                                                          
         CLI   AJNKTYPE,AJNKLIVQ   IS THE RECORD 'LIVE NOT DRAFT'?              
         BE    FIND27              YES, SEE IF WE HAVE THE RIGHT DATE           
FIND26B  GOTO1 SEQ                 NO, KEEP READING                             
         CLC   AJNKEY(AJNKEFF-AJNKTYP),KEYSAVE                                  
         BE    FIND26A             SAME KEY                                     
         L     RE,SAVERE           NEW KEY, GET NEXT LEVEL                      
         B     4(RE)                                                            
*                                                                               
FIND27   CLC   AJNKEFF,EFFDATE     ACTIVE RECORD, DATE OK ?                     
         BNL   FIND28              YES                                          
*                                                                               
FIND27A  GOTO1 SEQ                 NO, KEEP LOOKING                             
         CLC   AJNKEY(AJNKEFF-AJNKTYP),KEYSAVE                                  
         BNE   FIND24              NO MORE AT THIS LEVEL ERROR                  
         TM    AJNRSTAT,AJNNDACT   IS THE RECORD ACTIVE ?                       
         BZ    FIND27              YES, CHECK THE DATE                          
         B     FIND27A                                                          
*                                                                               
         USING JNAELD,R6                                                        
FIND28   MVC   IOKEY,0(R6)         SAVE KEY OF JOB NUMBER                       
         MVC   SVJNKEY,0(R6)                                                    
         MVI   ELCODE,JNAELQ       YES, GET ELEMENT                             
         BAS   RE,GETELIO                                                       
         BNE   FIND24                                                           
         XC    DMCB(24),DMCB                                                    
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),ACCDIR,IOKEY,SVJNKEY                 
         CLI   DMCB+8,0            NOT FOUND                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DISKDA,ACCKDA-ACCRECD+SVJNKEY                                    
*                                                                               
         XC    DMCB(24),DMCB                                                    
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,(X'80',GTREC),ACCMST,DISKDA,(R6),DMWORK             
         CLI   DMCB+8,X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    RUNINDS,RUNIREAD                                                 
         MVC   AIO,AIO2                                                         
         MVI   ELCODE,JNAELQ       YES, GET ELEMENT                             
         MVC   HALF,DATADISP                                                    
         MVC   DATADISP,DATADIS2                                                
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DATADISP,HALF                                                    
*                                                                               
         ST    R6,SAVER6                                                        
         LA    R3,WORK+1           GET SECOND POSITION OF JOB                   
         LA    R0,5                ONLY 5 POSITIONS LEFT                        
         XC    ASEQNUM,ASEQNUM     CLEAR STARTING SEQUENCE #                    
         XC    SAVEPY,SAVEPY       SET TO  NOT IN USE                           
         MVI   ROYIND,0            SET TO FIRST TIME                            
*                                                                               
         USING NUMTABD,R5                                                       
FIND30   LA    R5,NUMTAB                                                        
*                                                                               
FIND32   CLI   NUMVAL,X'FF'        END OF TABLE ?                               
         BE    FIND34              MUST BE CONSTANT                             
         CLC   JNAP2,NUMVAL        DO VALUES MATCH ?                            
         BE    FIND34              YES                                          
         LA    R5,NUMLEN(R5)       NO, KEEP LOOKING                             
         B     FIND32                                                           
*                                                                               
FIND34   OC    NUMADDR,NUMADDR     DO WE HAVE A SPECIAL ROUTINE ?               
         BNZ   FIND36              YES                                          
         CLI   0(R3),C' '          NO, FIELD MUST BE BLANK TO START             
*&&UK*&& MVI   ERROR,INVALID                                                    
*&&US*&& MVI   ERROR,AJNCHKD       IF AUTO JOB NUMBERING CHECKED DO NOT         
         BH    EXITN                                                            
*        BH    BADNUM                                                           
         MVC   *+10(2),NUMMOVE     MOVE IN REQUIRED DATA                        
         MVC   0(1,R3),BLANKS                                                   
*&&UK                                                                           
         CLI   PRODPROF+2,C'R'     RESETTING OF YEAR/SEQ NUM IN USE?            
         BNE   FIND38                                                           
         CLC   NUMVAL,NUMOYQ       CURRENT YEAR CHOSEN?                         
         BNE   FIND38                                                           
         ST    R3,SAVEPY           SAVE ADDRESS AND SET TO  IN USE              
*&&                                                                             
         B     FIND38                                                           
*                                                                               
FIND36   MVC   *+8(2),NUMADDR      GET ADDRESS OF SPECIAL ROUTINE               
         BAS   RE,FIND36           AND BRANCH TO IT                             
*                                                                               
FIND38   LA    R6,L'JNAP2+L'JNAP2N(R6)     GET NEXT POSITION IN ELEM            
         LA    R3,1(R3)            GET NEXT POSITION IN JOB                     
         BCT   R0,FIND30                                                        
*        XC    SAVE1ST,SAVE1ST                                                  
*                                                                               
FIND40   L     R6,SAVER6           GET BACK TO CORRECT SPOT                     
         BAS   RE,GETSEQ                                                        
***      BNE   EXITN               ALTHOUGH IN 03 DON'T USE HERE                
         BE    FIND41                                                           
         CLI   ERROR,JNUMCYCL      CANNOT RECOVER FROM THIS ONE BUT             
         BE    EXITN               TO EXIT WITH ERROR HERE                      
*                                                                               
FIND41   XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVC   LATECPJ(L'CLICODE),CLICODE                                       
         IC    RE,LCLI                                                          
         LA    RE,LATECPJ(RE)                                                   
         MVC   0(L'PRODCODE,RE),PRODCODE                                        
         XR    RE,RE                                                            
         IC    RE,LCLIPRO                                                       
         LA    RE,LATECPJ(RE)                                                   
         IC    RF,LJOB                                                          
         BCTR  RF,0                                                             
         MVC   0(0,RE),WORK                                                     
         EX    RF,*-6                                                           
         MVC   JNALNUM,LATECPJ     GETSEQ NEEDS JNALNUM TO BE UPDATED           
         OC    SAVE1ST,SAVE1ST     DID WE SAVE THE FIRST JOB # ?                
         BZ    FIND42              NO                                           
         CLC   SAVE1ST,WORK                                                     
         BNE   FIND44                                                           
         MVI   ERROR,NOMORE#                                                    
         B     EXITN                                                            
*                                                                               
FIND42   MVC   SAVE1ST,WORK                                                     
FIND44   MVC   AIO,AIO3                                                         
         MVC   KEY,BLANKS                                                       
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(L'LATECPJ),LATECPJ                                         
         GOTO1 HIGH                                                             
         CLC   KEY(ACKEYWRK-ACKEYD),KEYSAVE                                     
         BE    FIND40                                                           
         MVC   AIO,AIO1                                                         
         B     EXITY                                                            
*                                                                               
GETOFG   MVI   ERROR,NOOFG         GET READY FOR ERROR                          
         OC    EFFOFG,EFFOFG       DO WE HAVE AN OFFICE GROUP                   
         BZ    EXITN               NO                                           
         MVC   0(1,R3),EFFOFG      YES, USE IT                                  
         BR    RE                                                               
*                                                                               
GETOF2   MVI   ERROR,NOOFF2        GET READY FOR ERROR                          
         CLI   EFFOFFC+1,C' '      DO WE HAVE A SECOND CHARACTER ?              
         BNH   EXITN               NO                                           
         MVC   0(1,R3),EFFOFFC+1   YES, USE IT                                  
         BR    RE                                                               
*                                                                               
USERSUP  CLI   0(R3),C' '          USER SUPPLIED FIELD, MUST HAVE DATA          
         MVI   ERROR,INVALID                                                    
         BHR   RE                                                               
         B     EXITN                                                            
*                                                                               
NEXTNUM  CLI   0(R3),C' '          SEQUENTIAL FIELD, MUST NOT HAVE DATA         
         MVI   ERROR,INVALID                                                    
         BH    EXITN                                                            
         OC    ASEQNUM,ASEQNUM     IS THIS THE FIRST TIME ?                     
         BNZR  RE                  NO                                           
         ST    R3,ASEQNUM          YES, SAVE THIS STOP                          
         BR    RE                                                               
         DROP  R5,R6                                                            
*                                                                               
OKXIT    CR    R8,R8                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              FORMAT KEY AND READ HIGH FOR JOB NUMBER RECORDS        *         
***********************************************************************         
         SPACE 1                                                                
READHIJ  NTR1                                                                   
         LA    R6,KEY                                                           
         USING AJNRECD,R6                                                       
         MVC   AJNKEY,USERKEY                                                   
         MVI   AJNKTYP,AJNKTYPQ                                                 
         MVI   AJNKSUB,AJNKSUBQ                                                 
         MVC   AJNKCUL,CUL                                                      
         GOTO1 HIGH                                                             
         CLC   AJNKEY(AJNKEFF-AJNRECD),KEYSAVE                                  
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*              GET NEXT SEQUENTIAL NUMBER FOR JOB                     *         
***********************************************************************         
         SPACE 1                                                                
         USING JNAELD,R6                                                        
GETSEQ   NTR1                                                                   
         MVC   AIO,AIO2            USE SECOND IO BUFFER                         
         L     R3,ASEQNUM          GET STARTING POSTION IN JOB KEY              
         OC    SAVEPY,SAVEPY       RESETTING IN USE?                            
         BZ    GETS02                                                           
         L     RE,SAVEPY           ENSURE CURR YEAR BEFORE SEQ NUMBER           
         LR    RF,R3                                                            
         SR    RF,RE                                                            
         CHI   RF,1                                                             
         BE    GETS02              OK                                           
         XC    SAVEPY,SAVEPY       NOT OK (SET TO   NOT IN USE)                 
*                                                                               
GETS02   MVC   START#,JNASTRT      NEED START TO DETERMINE LENGTH               
         GOTO1 SQUASHER,DMCB,START#,L'START#                                    
         XR    R2,R2                                                            
         IC    R2,DMCB+7           LENGTH OF SEQUENTIAL PORTION                 
         BCTR  R2,0                                                             
*                                                                               
         CLC   JNALNUM,BLANKS      IS THIS OUR FIRST TIME ?                     
         BH    GETS06              NO, NEXT LAST # USED                         
*                                                                               
GETS04   MVC   0(0,R3),START#      YES, USE  STARTING #                         
         EX    R2,*-6                                                           
         B     GETS18                                                           
*                                                                               
GETS06   XR    R1,R1                                                            
         IC    R1,LCLIPRO                                                       
         AR    R1,R3                                                            
         LA    RF,WORK                                                          
         SR    R1,RF                                                            
         LA    R1,JNALNUM(R1)                                                   
         MVC   NEXT#,BLANKS                                                     
         MVC   NEXT#(0),0(R1)                                                   
         EX    R2,*-6                                                           
*                                                                               
         OC    SAVEPY,SAVEPY       RESETTING  IN USE?                           
         BZ    GETS10                                                           
         TM    ROYIND,ROYIREP      FIRST TIME?                                  
         BNZ   GETS10                                                           
         OI    ROYIND,ROYIREP                                                   
         LR    RE,R1                                                            
         BCTR  RE,0                POINT TO YEAR                                
         CLC   0(1,RE),OPEN+1      AND TEST CURRENT YEAR                        
         BE    GETS10              (IF CURRENT GO ON)                           
         B     GETS04              THEN USE START NUMBER                        
*                                  CHECK FOR OVERFLOW                           
GETS10   EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   NEXT#(0),=CL5'99999'  THIS IS ALWAYS OVERFLOW                    
         BE    GETS12                                                           
         OC    JNAEND,JNAEND                                                    
         BZ    GETS14              NO END TO SEQUENCE                           
         EX    R2,*+8              IF NEXT WILL BE OVERFLOW - ERROR             
         B     *+10                                                             
         CLC   NEXT#(0),JNAEND                                                  
         BNE   GETS14                                                           
* TS12   LA    R2,JBRSJACH                                                      
GETS12   MVI   ERROR,JNUMCYCL                                                   
         B     EXITN                                                            
*                                                                               
GETS14   EX    R2,*+8               ADD ONE TO SEQUENTIAL                       
         B     *+10                                                             
         PACK  DUB,NEXT#(0)                                                     
         AP    DUB,=P'1'                                                        
*                                                                               
         UNPK  WORK5,DUB           SHIFT OUTPUT TO LEFT                         
         LA    R1,WORK5+L'WORK5-1                                               
         SR    R1,R2                                                            
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   NEXT#(0),0(R1)                                                   
*                                                                               
         LA    R1,NEXT#(R2)        FIX THE SIGN                                 
         OI    0(R1),X'F0'                                                      
*                                                                               
         EX    R2,*+8              MUST NOT BE IN MANUAL RANGE                  
         B     *+10                                                             
         CLC   NEXT#(0),JNAMANS                                                 
         BL    GETS16                                                           
         BE    GETS10                                                           
*                                                                               
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   NEXT#(0),JNAMANE                                                 
         BNH   GETS10                                                           
*                                                                               
GETS16   EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),NEXT#       VALID NUMBER, UPDATE JNUM RECORD             
         EX    R2,*+8              IF NEXT WILL BE OVERFLOW - ERROR             
         B     *+10                                                             
         CLC   NEXT#(0),JNAEND     IF USING THE LAST ONE                        
         BNE   GETS18                                                           
         L     RE,AIO2             SET RECORD AS INACTIVE                       
         OI    AJNRSTAT-AJNRECD(RE),AJNNDACT                                    
         B     EXITN                                                            
*                                                                               
GETS18   DS    0H                                                               
         B     EXITY                                                            
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
NUMTAB   DC    C'US',S(0),S(USERSUP)                                            
         DC    C'SN',S(0),S(NEXTNUM)                                            
         DC    C'OF',S(EFFOFFC),S(0)                                            
         DC    C'O2',S(0),S(GETOF2)                                             
         DC    C'OG',S(0),S(GETOFG)                                             
         DC    C'CY',S(TODAY+1),S(0)                                            
NUMOYQ   DC    C'OY',S(OPEN+1),S(0)                                             
         DC    C'NA',S(BLANKS),S(0)                                             
         DC    X'FF',S(20(R6)),S(0)                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD ADDRESS ELEMENT                                    *         
***********************************************************************         
         SPACE 1                                                                
VALACADR NMOD1 0,**VCAD**                                                       
         LR    RC,R1                                                            
         LA    R2,DUMFLDH                                                       
         LA    R3,FAADDR5              R3=A(ADDRESS 5 DATA)                     
         USING FLDHDRD,R2                                                       
*                                                                               
VALACA1  MVI   ELCODE,ADRELQ       ADD AN ADDRESS ELEMENT                       
         GOTO1 REMELEM                                                          
         LA    R6,ELEMENT                                                       
         USING ADRELD,R6                                                        
         MVI   ADREL,ADRELQ                                                     
         LA    RF,ADRADD5                                                       
         XR    RE,RE                                                            
         LA    R0,5                5 ADDRESS LINES                              
*                                                                               
VALACA2  CLI   FLDILEN,0                                                        
         BNE   VALACA3                                                          
         SHI   RF,L'ADRADD1                                                     
         SHI   R2,L'FAADDR1+L'FLADDR1                                           
         BCT   R0,VALACA2                                                       
         B     VALACAX                                                          
*                                                                               
VALACA3  MVC   DUMFLD,BLANKS                                                    
         MVC   DUMFLD(L'FAADDR1),0(R3)     MOVE DATA INTO DUMMY FIELD           
         MVC   DUMFLDHL,L'FAADDR1(R3)      SET LENGTH IN DUMMY FIELD            
         MVC   0(L'ADRADD1,RF),BLANKS                                           
         AHI   RE,1                                                             
         CLI   FLDILEN,0                                                        
         BE    VALACA4                                                          
         XR    R1,R1                                                            
         IC    R1,FLDILEN          (INPUT LENGTH)                               
         BCTR  R1,0                                                             
         MVC   0(0,RF),FLDDATA                                                  
         EX    R1,*-6                                                           
VALACA4  SHI   R3,L'FAADDR1+L'FLADDR1     BUMP TO NEXT DATA FIELDS              
         SHI   RF,L'ADRADD1               BUMP TO NEXT ELEMENT FIELD            
         BCT   R0,VALACA3                                                       
         SPACE 1                                                                
         STC   RE,ADRNUM                                                        
         MHI   RE,L'ADRADD1                                                     
         AHI   RE,3                                                             
         STC   RE,ADRLN                                                         
         GOTO1 ADDELEM                                                          
VALACAX  B     EXITY                                                            
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD EXTRA COMMENTS                                       *         
***********************************************************************         
         SPACE 1                                                                
VALEXTCM NMOD1 0,**VEXT**                                                       
         LR    RC,R1                                                            
         MVI   ELCODE,FFTELQ       LOOK FOR DESKTOP COMMENTS ON FFTEL           
         BAS   RE,GETELIO                                                       
         BNE   VALEXT04                                                         
         USING FFTELD,R6                                                        
         XR    RF,RF                                                            
*                                                                               
VALEXT01 CLI   FFTEL,0             EOR ?                                        
         BE    VALEXT03                                                         
         CLI   FFTEL,FFTELQ        IS IT FFTEL ?                                
         BNE   VALEXT02                                                         
         CLI   FFTTYPE,FFTTPCOM    IS IT RIGHT TYPE ?                           
         BNE   VALEXT02                                                         
         MVI   FFTEL,X'FF'         MARK FOR DELETION                            
         B     VALEXT03                                                         
                                                                                
VALEXT02 IC    RF,FFTLN            BUMP TO NEXT ELEMENT                         
         AR    R6,RF                                                            
         B     VALEXT01                                                         
*                                                                               
VALEXT03 MVI   ELCODE,X'FF'        DELETE IT                                    
         GOTO1 REMELEM                                                          
*                                                                               
VALEXT04 CLI   FLEXTRCM,0          DESKTOP COMMENTS                             
         BE    VALEXTX                                                          
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         MVI   FFTEL,FFTELQ        BUILD FFTEL                                  
         MVI   FFTTYPE,FFTTPCOM                                                 
         MVI   FFTSEQ,1                                                         
         MVC   FFTDLEN,FLEXTRCM   SET DATA LENGTH                               
         XR    RF,RF                                                            
         IC    RF,FLEXTRCM                                                      
         BCTR  RF,0                                                             
         MVC   FFTDATA(0),FAEXTRCM MOVE IN COMMENT                              
         EX    RF,*-6                                                           
         XR    RF,RF                                                            
         IC    RF,FFTDLEN                                                       
         LA    RF,FFTLN1Q+L'FFTDLEN(RF)                                         
         STC   RF,FFTLN            SET ELEMENT LENGTH                           
         MVI   ELCODE,FFTELQ                                                    
         GOTO1 ADDELEM             ADD IT                                       
                                                                                
VALEXTX  B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD CAMPAIGN CODE                                        *         
***********************************************************************         
         SPACE 1                                                                
VALCAMPC NMOD1 0,**VCAM**                                                       
         LR    RC,R1                                                            
         MVI   ELCODE,FFTELQ       LOOK FOR FFTTCAMP                            
         BAS   RE,GETELIO                                                       
         BNE   VALCA10                                                          
         XR    R0,R0                                                            
*                                                                               
         USING FFTELD,R6                                                        
VALCA02  CLI   FFTEL,FFTELQ                                                     
         BNE   VALCA04                                                          
         CLI   FFTTYPE,FFTTCAMP                                                 
         BE    VALCA06                                                          
VALCA04  IC    R0,FFTLN                                                         
         AR    R6,R0                                                            
         CLI   FFTEL,0                                                          
         BNE   VALCA02                                                          
         B     VALCA10                                                          
*                                                                               
VALCA06  OC    FACAMPCD,FACAMPCD                                                
         BZ    VALCA08                                                          
         MVC   FFTDATA,FACAMPCD    REPLACE EXISTING ONE                         
         B     VALCAX                                                           
*                                                                               
VALCA08  MVI   FFTEL,X'FF'         DELETE EXISTING ONE                          
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
VALCA10  OC    FACAMPCD,FACAMPCD   CAMPAIGN PASSED?                             
         BZ    VALCAX                                                           
         XC    ELEMENT,ELEMENT     ADD NEW ONE                                  
         LA    R6,ELEMENT                                                       
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FACAMPCD+1                                       
         MVI   FFTTYPE,FFTTCAMP                                                 
         MVI   FFTDLEN,L'FACAMPCD                                               
         MVC   FFTDATA(L'FACAMPCD),FACAMPCD                                     
         MVI   ELCODE,FFTELQ                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
VALCAX   B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD ELEMENT TO JOB (INCL: AEXRECD MAINTENANCE)           *         
* (WHEN ENABLED THIS SUPERCEDES THE ADDELEM CALLS)                    *         
***********************************************************************         
         SPACE 1                                                                
ADD2JOB  NMOD1 0,**71A2JO                                                       
         LR    RC,R1                                                            
                                                                                
         DC    H'0'                >>> NOT TESTED YER                           
                                                                                
         TM    AEXIND,AEXIUPDQ     UPDATE AEX DATA?                             
         JNZ   ADD2J70                                                          
                                                                                
ADD2J02  L     R3,AIO              GET RECORD LENGTH                            
         XR    RF,RF                                                            
         ICM   RF,B'0011',ACCORLEN(R3)                                          
         AHI   RF,250              ADD 250 BYTES 'SPARE'                        
         LLC   RE,ELEMENT+1                                                     
         AR    RF,RE               ADD ELEMENT LENGTH TO BE ADDED               
         CHI   RF,2000             ENOUGH ROOM?                                 
         BH    ADD2J10                                                          
         GOTO1 ADDELEM             ADD TO JOB ACTRECD                           
         J     EXIT                                                             
                                                                                
ADD2J10  LA    R2,AEXTAB           IS ELEMENT ALLOWED FOR AEX?                  
         LA    R3,ELEMENT                                                       
                                                                                
ADD2J12  CLI   0(R2),0             EOT?                                         
         BE    ADD2J20                                                          
         CLC   0(1,R2),ELEMENT                                                  
         BNE   ADD2J14                                                          
         CLI   1(R2),0             ANY SUB ELEMENT CODE?                        
         BE    ADD2J20             THEN ADD TO AEX                              
         LLC   RF,1(R2)                                                         
         LA    RF,ELEMENT(RF)                                                   
         CLC   2(1,R2),0(RF)       CHECK SUB CODE                               
         BE    ADD2J50             (AND ADD)                                    
                                                                                
ADD2J14  AHI   R2,3                                                             
         B     ADD2J12                                                          
                                                                                
ADD2J20  TM    AEXIND,AEXIDELQ     HAVE WE MOVED ALREADY?                       
         BZ    *+6                                                              
         DC    H'0'                >>> RECORD FULL                              
                                                                                
         L     R3,AIO              ANY ELEMENTS THAT CAN MOVED TO AEX?          
         AHI   R3,ACCORFST                                                      
         B     ADD2J24                                                          
                                                                                
ADD2J22  LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
                                                                                
ADD2J24  CLI   0(R4),0             EOR?                                         
         BE    ADD2J40                                                          
         LA    R2,AEXTAB                                                        
                                                                                
ADD2J26  CLI   0(R2),0             EOT?                                         
         BE    ADD2J22                                                          
         CLC   0(2,R1),0(R3)                                                    
         BNE   ADD2J28                                                          
         CLI   1(R2),0             ANY SUB ELEMENT CODE?                        
         BE    ADD2J30             THEN ADD TO AEX                              
         LLC   RF,1(R2)                                                         
         AR    RF,R3                                                            
         CLC   2(1,R2),0(RF)       CHECK SUB CODE                               
         BE    ADD2J30             (AND ADD)                                    
                                                                                
ADD2J28  AHI   R2,3                                                             
         B     ADD2J26                                                          
                                                                                
ADD2J30  OI    AEXIND,AEXIRETQ                                                  
         OI    AEXIND,AEXIDELQ                                                  
         B     ADD2J50             THIS ...                                     
                                                                                
ADD2J32  MVI   0(R3),X'FF'         ... RETURNS HERE                             
         B     ADD2J22             PROCESS NEXT ELEMENT                         
                                                                                
ADD2J40  TM    AEXIND,AEXIDELQ     ANY DATA TO MOVED?                           
         BNZ   *+6                                                              
         DC    H'0'                >>> RECORD FULL                              
         GOTO1 HELLO,DMCB,(C'D',AC2FIL),(X'FF',AIO),0                           
         B     ADD2J02                                                          
                                                                                
ADD2J50  OI    AEXIND,AEXIDATQ     SET 'DATA' AND ADD INTO AEX AREA             
         L     R1,AAEXANXT                                                      
         LLC   RF,1(R3)                                                         
         CHI   RF,1                                                             
         BH    *+6                                                              
         DC    H'0'                >>> BAD ELEMENT LENGTH                       
         SHI   RF,1                                                             
         MVC   0(0,R1),0(R3)                                                    
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         AR    R1,RF                                                            
         ST    R1,AAEXANXT         CHECK FOR AEXAREA OVERFLOW                   
         L     RF,AAEXAREA                                                      
         AHI   RF,AEXAMAXQ                                                      
         CR    R1,RF                                                            
         BNH   *+6                                                              
         DC    H'0'                >>> AEX AREA FULL                            
                                                                                
         TM    AEXIND,AEXIRETQ     RETURN?                                      
         JZ    EXIT                                                             
         NI    AEXIND,X'FF'-AEXIRETQ                                            
         J     ADD2J32                                                          
                                                                                
         USING AEXRECD,R2                                                       
ADD2J70  TM    AEXIND,AEXIDATQ     UPDATE JOB AND AEX RECORD                    
         JZ    EXIT                (EXIT IF NO DATA)                            
         MVI   CNTAEX,1                                                         
         L     R4,AAEXAREA         POINT TO ELEMENT AREA                        
         MVC   AIO,AIO2                                                         
         L     R2,AIO              CLEAR AIO AREA                               
         LA    R3,2000                                                          
         XR    R1,R1                                                            
         MVCL  R2,R0                                                            
         MVI   AEXKTYP,AEXKTYPQ                                                 
         MVI   AEXKSUB,AEXKSUBQ                                                 
         MVC   AEXKCPY,CUL                                                      
         MVC   AEXKULC,CUL+1                                                    
         MVC   AEXKACC,SAVEJOB                                                  
         MVC   AEXKSEQ,CNTAEX                                                   
         LHI   RF,AEXRFST-AEXRECD                                               
         AHI   RF,1                                                             
         STCM  RF,3,AEXRLEN        SET LENGTH                                   
                                                                                
ADD2J72  CLI   0(R4),0             END OF ELEMENTS?                             
         BE    ADD2J80                                                          
                                                                                
         XR    RF,RF                                                            
         ICM   RF,B'0011',AEXRLEN                                               
         AHI   RF,250              ADD 50 BYTES 'SPARE'                         
         LLC   RE,1(R4)                                                         
         AR    RF,RE               ADD ELEMENT LENGTH TO BE ADDED               
         CHI   RF,2000             ENOUGH ROOM?                                 
         BH    ADD2J76                                                          
                                                                                
ADD2J74  LA    RF,=CL8'ADD=END'                                                 
         OI    AEXIND,AEXIRECQ                                                  
         GOTO1 HELLO,DMCB,(C'P',AC2MST),AEXRECD,0(R4),(RF)                      
         CLI   12(R1),0                                                         
         BE    ADD2J78                                                          
         DC    H'0'                                                             
                                                                                
ADD2J76  BAS   RE,UPDAEX                                                        
                                                                                
         NI    AEXIND,X'FF'-AEXIRECQ                                            
         LLC   R1,CNTAEX                                                        
         AHI   R1,1                                                             
         STC   R1,CNTAEX                                                        
                                                                                
         L     R2,AIO              BUILD NEXT RECORD                            
         LA    R3,2000                                                          
         XR    R1,R1                                                            
         MVCL  R2,R0                                                            
         MVI   AEXKTYP,AEXKTYPQ                                                 
         MVI   AEXKSUB,AEXKSUBQ                                                 
         MVC   AEXKCPY,CUL                                                      
         MVC   AEXKULC,CUL+1                                                    
         MVC   AEXKACC,SAVEJOB                                                  
         MVC   AEXKSEQ,CNTAEX                                                   
         LHI   RF,AEXRFST-AEXRECD                                               
         AHI   RF,1                                                             
         STCM  RF,3,AEXRLEN        SET LENGTH                                   
         B     ADD2J74                                                          
                                                                                
ADD2J78  LLC   R1,1(R4)            NEXT ELEMENT                                 
         AR    R4,R1                                                            
         B     ADD2J72                                                          
                                                                                
ADD2J80  TM    AEXIND,AEXIRECQ     END OF ELEMENTS, ANY IO PENDING?             
         JZ    ADD2J90                                                          
                                                                                
         BAS   RE,UPDAEX                                                        
         NI    AEXIND,X'FF'-AEXIRECQ                                            
                                                                                
JOB      USING ACTRECD,R3                                                       
ADD2J90  LA    R3,IOKEY            UPDATE AEX COUNTER ON JOB                    
         MVC   JOB.ACTKEY,BLANKS                                                
         MVC   JOB.ACTKCPY(3),CUL                                               
         MVC   JOB.ACTKACT,SAVEJOB                                              
         MVC   AIO,AIO1                                                         
                                                                                
         GOTO1 DATAMGR,DMCB,(X'80',D2READ),AC2DIR,JOB.ACTRECD,         *        
               JOB.ACTRECD                                                      
         BE    *+6                                                              
         DC    H'0'                >>> NO JOB?                                  
         GOTO1 DATAMGR,DMCB,(X'80',G2REC),AC2MST,JOB.ACTKDA,AIO,DMWORK          
         BE    *+6                                                              
         DC    H'0'                >>> NO JOB?                                  
                                                                                
         USING RSTELD,R4                                                        
         XR    R0,R0                                                            
         L     R3,AIO1                                                          
         LA    R4,JOB.ACTRFST                                                   
                                                                                
ADD2J92  CLI   RSTEL,RSTELQ                                                     
         BE    ACC2J94                                                          
         CLI   RSTEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                >>> NO RSTELD ON JOB                         
         IC    R0,RSTLN                                                         
         AR    R4,R0                                                            
         B     ADD2J92                                                          
                                                                                
ACC2J94  CLI   RSTLN,RSTLN3Q                                                    
         BNL   *+6                                                              
         DC    H'0'                >>> RSTELD ON JOB TOO SHORT                  
         MVC   RSTEXCNT,CNTAEX                                                  
                                                                                
         LA    R3,IOKEY                                                         
         GOTO1 DATAMGR,DMCB,P2REC,AC2MST,JOB.ACTKDA,AIO,DMWORK                  
         BE    ACC2J96                                                          
         DC    H'0'                >>> CANNOT UPDATE?                           
                                                                                
ACC2J96  DS    0H                  EXIT ADD2JOB                                 
         J     EXIT                                                             
         DROP  JOB                                                              
                                                                                
UPDAEX   ST    RE,AAEXANXT                                                      
                                                                                
         L     R1,AIO              READ FOR AEXRECD                             
         LA    R2,IOKEY                                                         
         MVC   AEXKEY,0(R1)                                                     
         TM    AEXIND,AEXIADDQ     CAN ADD STRAIGHT?                            
         BNZ   UPDAEX05                                                         
                                                                                
         GOTO1 DATAMGR,DMCB,(X'88',D2READ),AC2DIR,AEXRECD,AEXRECD               
         BE    UPDAEX10                                                         
         TM    DMCB+8,X'02'        RECORD DELETED?                              
         BNZ   UPDAEX10                                                         
         TM    DMCB+8,X'10'        RECORD NOT FOUND?                            
         BNZ   *+6                                                              
         DC    H'0'                >>> OTHER IO ERROR                           
                                                                                
         OI    AEXIND,AEXIADDQ     ... ADD FROM HERE ON ...                     
                                                                                
UPDAEX05 XC    DISKDA,DISKDA                                                    
         GOTO1 DATAMGR,DMCB,A2REC,AC2MST,DISKDA,AIO,DMWORK                      
         BE    UPDAEXX                                                          
         DC    H'0'                >>> ADD ERROR?                               
                                                                                
UPDAEX10 L     R0,AIO3             COPY RECORD TO AIO3                          
         LA    R1,2000                                                          
         L     RE,AIO                                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   DISKDA,AEXKDA                                                    
         GOTO1 DATAMGR,DMCB,(X'80',G2REC),AC2MST,DISKDA,AIO,DMWORK              
         BE    *+6                                                              
         DC    H'0'                >>> GET IO ERROR?                            
                                                                                
         L     R0,AIO              COPY RECORD BACK TO AIO                      
         LA    R1,2000                                                          
         L     RE,AIO3                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R1,AIO              SET STATUS                                   
         NI    AEXRSTAT-AEXRECD(R1),X'FF'-AEXSDELT                              
                                                                                
         GOTO1 DATAMGR,DMCB,P2REC,AC2MST,DISKDA,AIO,DMWORK                      
         BE    *+6                                                              
         DC    H'0'                >>> CANNOT UPDATE?                           
                                                                                
         LA    R2,IOKEY                                                         
         NI    AEXKSTAT,X'FF'-AEXSDELT                                          
         GOTO1 DATAMGR,DMCB,D2WRT,AC2DIR,DISKDA,AEXRECD,AEXRECD                 
         BE    UPDAEXX                                                          
         DC    H'0'                >>> CANNOT UPDATE?                           
                                                                                
UPDAEXX  L     RE,AAEXANXT                                                      
         BR    RE                                                               
         DROP  R2,R4                                                            
                                                                                
* AEX ELEMENT TABLE (NEEDS VREC/SND SUPPORT ...)                                
AEXTAB   DC    AL1(JLDELQ,0,0)                                                  
         DC    AL1(0)                                                           
                                                                                
AC2FIL   DC    CL8'ACCOUNT'                                                     
AC2DIR   DC    CL8'ACCDIR'                                                      
AC2MST   DC    CL8'ACCMST'                                                      
D2READ   DC    CL8'DMREAD'                                                      
G2REC    DC    CL8'GETREC'                                                      
P2REC    DC    CL8'PUTREC'                                                      
A2REC    DC    CL8'ADDREC'                                                      
D2WRT    DC    CL8'DMWRT'                                                       
                                                                                
         LTORG                                                                  
                                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD TEAM LIST DATA                                                 
***********************************************************************         
         SPACE 1                                                                
VALTEAJ  NMOD1 0,**VTEA**                                                       
         LR    RC,R1                                                            
         MVI   BYTE,0                                                           
         L     RF,AIO                                                           
         MVC   SJOBCOD,0(RF)                                                    
                                                                                
         L     RF,ASVCTEAM             CLEAR CLIENT TEAM TABLE                  
         XC    0(L'SVCTEAM,RF),0(RF)                                            
         AHI   RF,L'SVCTEAM                                                     
         ST    RF,SAVERF                                                        
                                                                                
         MVI   ELCODE,LIDELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   *+8                                                              
         BAS   RE,VALTDEL      DELETE EXISTING LIDTTEAJ AT JOB LEVEL            
*&&US                                                                           
         CLC   FACLIENT,=C'KEL  '       ''''''''''''''''''''''                  
         BNE   VALT02                                                           
         CLC   FAPROD,=C'02 '               JWTBRO                              
         BNE   VALT02                                                           
         CLC   FAJOB,=C'T00065'                                                 
         BNE   VALT02                                                           
*                                                                               
         LHI   RF,FAROLE1-LINKD                                                 
         LA    RF,LINKD(RF)                                                     
         MVC   0(2,RF),=X'000A'                                                 
         AHI   RF,L'FAROLE1+L'FARLNM1                                           
         MVC   0(8,RF),=C'JHUM    '                                             
         AHI   RF,L'FAPCOD1                                                     
         MVI   0(RF),C'J'                                                       
*                                                                               
         LHI   RF,FAROLE2-LINKD                                                 
         LA    RF,LINKD(RF)                                                     
         MVC   0(L'FAROLE14,RF),=X'000A'                                        
         AHI   RF,L'FAROLE1+L'FARLNM1                                           
         MVC   0(L'FAPCOD14,RF),=C'SSMITH  '                                    
         AHI   RF,L'FAPCOD1                                                     
         MVI   0(RF),C'C'                                                       
*                                                                               
         LHI   RF,FAROLE10-LINKD                                                
         LA    RF,LINKD(RF)                                                     
         MVC   0(L'FAROLE14,RF),=X'000B'                                        
         AHI   RF,L'FAROLE1+L'FARLNM1                                           
         MVC   0(L'FAPCOD14,RF),=C'PCOLD   '                                    
         AHI   RF,L'FAPCOD1                                                     
         MVI   0(RF),C'J'            '''''''''''''''''''''''''                  
*&&                                                                             
VALT02   BAS   RE,VALTADD      ADD/REPLACE LIDTTEAJ                             
*                                                                               
         MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
         USING ACTRECD,R6                                                       
         MVC   ACTKEY,BLANKS                                                    
         MVC   ACTKCPY(3),CUL                                                   
         MVC   ACTKACT(L'FACLIENT),FACLIENT                                     
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),ACCOUNT,(R6),(R6),DMWORK             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,ACCORFST(R6)                                                  
         MVI   BYTE,1                                                           
         MVI   CTEACNT,0       INITIALIZE COUNTER FOR SVCTEAM                   
         BAS   RE,VALTDEL      DELETE EXISTING LIDTTEAJ AT CLIENT LEVEL         
         BAS   RE,VALTADD      ADD/REPLACE LIDTTEAJ                             
         XR    RE,RE                                                            
         IC    RE,CTEACNT                                                       
         LTR   RE,RE           ANYTHING IN SVCTEAM?                             
         BZ    VALT04          NO                                               
         L     R2,ASVCTEAM                                                      
         LA    RF,CTLEN                                                         
         GOTO1 XSORT,DMCB,(R2),(RE),(RF),(RF),0                                 
         BAS   RE,UPDJOBS      UPDATE ALL OTHER JOBS                            
VALT04   MVC   AIO,AIO1        CHANGE BUFFERS BACK                              
VALTEX   B     EXITY                                                            
                                                                                
                                                                                
** DELETE LIDTTEAJ **                                                           
                                                                                
VALTDEL  ST    RE,MYSVRE                                                        
         XR    R0,R0                                                            
         USING LIDELD,R6                                                        
VDEL02   CLI   LIDEL,0                                                          
         BE    VALTDELX                                                         
         CLI   LIDEL,LIDELQ                                                     
         BNE   VDEL04                                                           
         CLI   LIDTYPE,LIDTTEAJ                                                 
         BE    VDEL06                                                           
VDEL04   IC    R0,LIDLN                                                         
         AR    R6,R0                                                            
         B     VDEL02                                                           
VDEL06   CLI   BYTE,1                                                           
         BNE   VDEL10                                                           
         LA    R2,LIDDATA-LIDELD                                                
         XR    R3,R3                                                            
         IC    R3,LIDLN                                                         
         SR    R3,R2                                                            
         XR    RE,RE                                                            
         IC    RE,LIDITLN                                                       
         XR    R2,R2                                                            
         DR    R2,RE           R3=NUMBER OF ITEMS                               
         STC   R3,CTEACNT                                                       
         LA    R2,LIDDATA                                                       
         USING CTEAMD,RF                                                        
         L     RF,ASVCTEAM                                                      
VDEL08   MVI   0(RF),C'-'                                                       
         MVC   1(L'LIDROLN,RF),0(R2)                                            
         MVC   3(L'LIDBPID,RF),L'LIDROLN(R2)                                    
         AHI   RF,CTLEN                                                         
         AHI   R2,L'LIDROLN+L'LIDBPID                                           
         BCT   R3,VDEL08                                                        
         DROP  RF                                                               
VDEL10   MVI   LIDEL,X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         GOTO1 DATAMGR,DMCB,DMWRT,ACCOUNT,AIO,AIO,DMWORK                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
VALTDELX L     RE,MYSVRE                                                        
         BR    RE                                                               
                                                                                
                                                                                
** ADD LIDTTEAJ **                                                              
                                                                                
VALTADD  ST    RE,MYSVRE                                                        
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         MVI   LIDEL,LIDELQ                                                     
         MVI   LIDITLN,L'LIDROLN+L'LIDBPID                                      
         MVI   LIDTYPE,LIDTTEAJ                                                 
         LA    RE,LIDDATA                                                       
         ST    RE,FULL                                                          
         USING FATEAJD,R3                                                       
         LHI   R3,FAROLE1-LINKD                                                 
         LA    R3,LINKD(R3)                                                     
         LA    R5,14                                                            
         MVC   SAVAIO,AIO                                                       
         OC    FAROLE,FAROLE                                                    
         BZ    VADD04                                                           
VADD02   CLI   BYTE,1                                                           
         BNE   *+12                                                             
         CLI   FARLEV,C'C'                                                      
         BNE   VADD04                                                           
         MVC   AIO,AIO3                                                         
         L     R2,AIO                                                           
         USING SAPEREC,R2                                                       
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ      C'F'                                       
         MVI   SAPESUB,SAPESUBQ      X'04'                                      
         MVC   SAPEAGY,CMPALPHA                                                 
         MVC   SAPEPID,FAPCOD                                                   
         MVC   SAVEKEY,SAPEKEY                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,(R2),(R2)                             
         CLC   SAVEKEY(SAPEDEF-SAPEKEY),SAPEKEY                                 
         BNE   VADD04                                                           
         LA    RF,SAPEDATA                                                      
         USING SAPWDD,RF                                                        
         XR    R0,R0                                                            
VADD03   CLI   SAPWDEL,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   SAPWDEL,SAPWDELQ      X'C4'                                      
         BE    *+14                                                             
         IC    R0,SAPWDLN                                                       
         AR    RF,R0                                                            
         B     VADD03                                                           
         L     RE,FULL                                                          
         MVC   0(L'LIDROLN,RE),FAROLE ROLE NUMBER                               
         MVC   L'LIDROLN(L'LIDBPID,RE),SAPWDNUM  BINARY PID                     
         DROP  R2,RF                                                            
         AHI   RE,L'LIDROLN+L'LIDBPID                                           
         ST    RE,FULL                                                          
VADD04   AHI   R3,FATEAJLN                                                      
         BCT   R5,VADD16                                                        
         LA    R0,LIDDATA                                                       
         L     RE,FULL                                                          
         SR    RE,R0                                                            
         LTR   RE,RE                                                            
         BZ    VALTADDX                                                         
         MVC   AIO,SAVAIO                                                       
         LR    RF,RE                                                            
         AHI   RE,LIDDATA-LIDEL                                                 
         STC   RE,LIDLN                                                         
         CLI   BYTE,1                                                           
         BNE   VADD14                                                           
         LA    R0,L'LIDROLN+L'LIDBPID                                           
         XR    RE,RE                                                            
         DR    RE,R0                                                            
         LR    R5,RF                                                            
         LA    R2,LIDDATA                                                       
         USING CTEAMD,RE                                                        
VADD06   L     RE,ASVCTEAM                                                      
VADD08   CLC   1(L'CTROLE,RE),0(R2)                                             
         BNE   VADD10                                                           
         CLC   3(L'CTBPID,RE),L'LIDROLN(R2)                                     
         BNE   VADD10                                                           
                                                                                
VADD09   MVI   0(RE),C' '   REPLACE '-' WITH ' '                                
         AHI   R2,L'LIDROLN+L'LIDBPID                                           
         BCT   R5,VADD06                                                        
         B     VADD14                                                           
                                                                                
VADD10   OC    0(L'CTROLE+1,RE),0(RE)                                           
         BZ    VADD12                                                           
         AHI   RE,CTLEN                                                         
         L     RF,SAVERF                                                        
         CR    RE,RF                                                            
         BL    *+6                                                              
         DC    H'0'               CANNOT FIT ONTO A SINGLE ELEMENT              
         OC    0(L'CTROLE+1,RE),0(RE)    ANY MORE ENTRIES TO LOOK AT?           
         BNZ   VADD08                    YES                                    
                                                                                
VADD12   MVI   0(RE),C'+'                NO, SO ADD IT                          
         MVC   1(L'CTROLE,RE),0(R2)                                             
         MVC   3(L'CTBPID,RE),L'LIDROLN(R2)                                     
         AHI   R2,L'LIDROLN+L'LIDBPID                                           
         XR    R0,R0                                                            
         IC    R0,CTEACNT                                                       
         AHI   R0,1                                                             
         STC   R0,CTEACNT                                                       
         BCT   R5,VADD06                                                        
         DROP  RE                                                               
                                                                                
VADD14   LA    RF,=C'ADD=END'                                                   
         GOTO1 HELLO,DMCB,(C'P',ACCOUNT),AIO,ELEMENT,(RF)                       
         GOTO1 DATAMGR,DMCB,DMWRT,ACCOUNT,AIO,AIO,DMWORK                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,MYSVRE                                                        
         BR    RE                                                               
*                                                                               
VADD16   OC    FAROLE,FAROLE                                                    
         BZ    VADD04                                                           
         B     VADD02                                                           
                                                                                
VALTADDX L     RE,MYSVRE                                                        
         BR    RE                                                               
         DROP  R3,R6                                                            
                                                                                
                                                                                
** UPDATE ALL OTHER JOBS **                                                     
                                                                                
UPDJOBS  ST    RE,MYSVRE                                                        
         L     R6,AIO                                                           
         USING ACTRECD,R6                                                       
         MVC   ACTKEY,BLANKS                                                    
         MVC   ACTKCPY(3),CUL                                                   
         MVC   ACTKACT(L'FACLIENT),FACLIENT                                     
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,(R6),(R6)                             
UPDJ02   CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ACTKCPY(3),CUL                                                   
         BNE   UPDJX                                                            
         XR    RE,RE                                                            
         IC    RE,LCLI                                                          
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         BNE   UPDJX                                                            
         CLC   ACTKACT(0),FACLIENT                                              
         IC    RE,LCLIPRO                                                       
         LA    RF,ACTKACT                                                       
         AR    RF,RE                                                            
         CLI   0(RF),C' '        ARE WE AT A JOB?                               
         BH    UPDJ10                                                           
UPDJ04   GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,(R6),(R6)                             
         B     UPDJ02                                                           
                                                                                
UPDJ10   CLC   SJOBCOD,0(R6)                                                    
         BE    UPDJ04                                                           
         MVC   DISKDA,ACTKDA                                                    
         GOTO1 DATAMGR,DMCB,(X'80',GTREC),ACCMST,DISKDA,(R6),DMWORK             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,ACTRFST                                                       
         USING LIDELD,R5                                                        
         XR    R0,R0                                                            
UPDJ12   CLI   LIDEL,0                                                          
         BE    UPDJ20               ADD LIDELD IF NECESSARY                     
         CLI   LIDEL,LIDELQ                                                     
         BNE   UPDJ14                                                           
         CLI   LIDTYPE,LIDTTEAJ                                                 
         BE    UPDJ40               ADJUST LIDELD AS NECESSARY                  
UPDJ14   IC    R0,LIDLN                                                         
         AR    R5,R0                                                            
         B     UPDJ12                                                           
                                                                                
UPDJ20   XC    ELEMENT,ELEMENT                                                  
         LA    R5,ELEMENT                                                       
         LA    R2,LIDDATA                                                       
         L     RE,ASVCTEAM                                                      
         IC    R0,CTEACNT                                                       
UPDJ22   CLI   0(RE),C'-'                                                       
         BE    UPDJ24                                                           
         MVC   0(L'LIDROLN+L'LIDBPID,R2),L'CTSIGN(RE)                           
         AHI   R2,L'LIDROLN+L'LIDBPID                                           
UPDJ24   AHI   RE,CTLEN                                                         
         BCT   R0,UPDJ22                                                        
         LA    RF,LIDDATA                                                       
         CR    RF,R2                                                            
         BNL   UPDJ30                                                           
         MVI   LIDEL,LIDELQ                                                     
         MVI   LIDITLN,L'LIDROLN+L'LIDBPID                                      
         MVI   LIDTYPE,LIDTTEAJ                                                 
         SR    R2,R5                                                            
         STC   R2,LIDLN                                                         
                                                                                
         LA    RF,=C'ADD=END'                                                   
         GOTO1 HELLO,DMCB,(C'P',ACCMST),ACTRECD,ELEMENT,(RF)                    
         GOTO1 DATAMGR,DMCB,PTREC,ACCMST,DISKDA,(R6),DMWORK                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
UPDJ30   XR    RE,RE                                                            
         IC    RE,ACTKACT+L'ACTKACT-1                                           
         LA    RE,1(,RE)                                                        
         STC   RE,ACTKACT+L'ACTKACT-1                                           
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,(R6),(R6)                             
         B     UPDJ02                                                           
                                                                                
UPDJ40   XR    R3,R3                                                            
         IC    R3,LIDLN                                                         
         AR    R3,R5             R3=A(END OF LIDTTEAJ)                          
         LA    R2,LIDDATA                                                       
         XC    ELEMENT,ELEMENT                                                  
         LA    RF,ELEMENT                                                       
         AHI   RF,LIDDATA-LIDEL                                                 
         ST    RF,FULL                                                          
UPDJ42   CR    R2,R3                                                            
         BNL   UPDJ48                                                           
         L     RE,ASVCTEAM                                                      
         IC    R0,CTEACNT                                                       
UPDJ44   CLC   L'CTSIGN(L'CTROLE+L'CTBPID,RE),0(R2)                             
         BE    UPDJ46                                                           
         AHI   RE,CTLEN                                                         
         BCT   R0,UPDJ44                                                        
         L     RF,SAVERF                                                        
         CR    RE,RF                                                            
         BL    *+6                                                              
         DC    H'0'               CANNOT FIT                                    
         L     RF,FULL                                                          
         MVC   0(L'LIDROLN+L'LIDBPID,RF),0(R2)                                  
         AHI   RF,L'LIDROLN+L'LIDBPID                                           
         ST    RF,FULL                                                          
UPDJ46   AHI   R2,L'LIDROLN+L'LIDBPID                                           
         B     UPDJ42                                                           
                                                                                
UPDJ48   MVI   LIDEL,X'FF'                                                      
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(X'FF',(R6)),0,0                        
                                                                                
         LA    R5,ELEMENT                                                       
         MVI   LIDEL,LIDELQ                                                     
         MVI   LIDITLN,L'LIDROLN+L'LIDBPID                                      
         MVI   LIDTYPE,LIDTTEAJ                                                 
         L     R2,FULL                                                          
         L     R3,ASVCTEAM                                                      
         XR    RE,RE                                                            
         IC    RE,CTEACNT                                                       
UPDJ50   CLI   0(R3),C'-'                                                       
         BE    UPDJ52                                                           
         MVC   0(L'LIDROLN,R2),L'CTSIGN(R3)                                     
         MVC   L'LIDROLN(L'LIDBPID,R2),3(R3)                                    
         AHI   R2,L'LIDROLN+L'LIDBPID                                           
UPDJ52   AHI   R3,CTLEN                                                         
         BCT   RE,UPDJ50                                                        
         LA    RF,LIDDATA                                                       
         OC    0(L'LIDROLN,RF),0(RF)                                            
         BZ    UPDJ54                                                           
         SR    R2,R5                                                            
         STC   R2,LIDLN                                                         
         LA    RF,=C'ADD=END'                                                   
         GOTO1 HELLO,DMCB,(C'P',ACCMST),ACTRECD,ELEMENT,(RF)                    
UPDJ54   GOTO1 DATAMGR,DMCB,PTREC,ACCMST,DISKDA,(R6),DMWORK                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     UPDJ30                                                           
         DROP  R5,R6                                                            
                                                                                
UPDJX    L     RE,MYSVRE                                                        
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
*&&US                                                                           
GOXBLK   DS    CL(L'GOXBLOCK)      GETOPT EXTENSION BLOCK                       
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* MAP TABLES                                                          *         
***********************************************************************         
         SPACE 1                                                                
MAPTAB   DS    0X                                                               
*                                  ** CLIENT PRODUCT LIST **                    
JBMEL    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(01)             ELEMENT CODE                                 
         DC    AL2(JBMELX+1-JBMEL) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(SNDJOB-T60B71)  SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE FOR ACTION                      
         DC    CL5'ACTN '          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAACTION)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVACT-T60B71)   RECEIVE ROUTINE                             
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE FOR CLIENT CODE                 
         DC    CL5'CLINT'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVCLI-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE FOR PRODUCT CODE                
         DC    CL5'PROD.'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVPRO-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE FOR JOB CODE                    
         DC    CL5'JOB  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVJOB-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE FOR JOB NAME                    
         DC    CL5'JOBNM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVJNM-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(06)             MAPPING CODE FOR FILTER 1                    
         DC    CL5'JOBF1'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAFILT1)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVJF1-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(07)             MAPPING CODE FOR FILTER 2                    
         DC    CL5'JOBF2'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAFILT2)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVJF2-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(08)             MAPPING CODE FOR FILTER 3                    
         DC    CL5'JOBF3'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAFILT3)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVJF3-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(09)             MAPPING CODE FOR FILTER 4                    
         DC    CL5'JOBF4'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAFILT4)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVJF4-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(10)             MAPPING CODE FOR FILTER 5                    
         DC    CL5'JOBF5'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAFILT5)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVJF5-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(11)             MAPPING CODE FOR OFFICE CODE                 
         DC    CL5'JOBOF'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAOFFIC)       DATA LENGTH                                 
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVJOF-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(12)             MAPPING CODE FOR CLOSE DATE                  
         DC    CL5'CLSDT'          TEXT IDENTIFIER                              
         DC    AL1(MDTDTQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FACLOSDT)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVCDT-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(13)             MAPPING CODE FOR OPEN DATE                   
         DC    CL5'OPEDT'          TEXT IDENTIFIER                              
         DC    AL1(MDTDTQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAOPENDT)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVODT-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(14)             MAPPING CODE FOR JOB STATUS                  
         DC    CL5'JBSTA'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAJBSTAT)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(15)             MAPPING CODE FOR PRINT ON BILLS              
         DC    CL5'PRTBL'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVPOB-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(16)             MAPPING CODE FOR OTHER 1                     
         DC    CL5'OTHR1'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVOTH1-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(17)             MAPPING CODE FOR OTHER 2                     
         DC    CL5'OTHR2'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVOTH2-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(18)             MAPPING CODE FOR OTHER 3                     
         DC    CL5'OTHR3'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVOTH3-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(19)             MAPPING CODE FOR JOB COMMENT 1               
         DC    CL5'JBCM1'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVCOM1-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(20)             MAPPING CODE FOR JOB COMMENT 2               
         DC    CL5'JBCM2'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVCOM2-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(21)             MAPPING CODE FOR JOB COMMENT 3               
         DC    CL5'JBCM3'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVCOM3-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(22)             MAPPING CODE FOR USER CODE1                  
         DC    CL5'USCD1'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'UFSCODE)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUCD1-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(23)             MAPPING CODE FOR USER FIELD 1                
         DC    CL5'USER1'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUSR1-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(24)             MAPPING CODE FOR USER CODE 2                 
         DC    CL5'USCD2'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'UFSCODE)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUCD2-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(25)             MAPPING CODE FOR USER FIELD 2                
         DC    CL5'USER2'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUSR2-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(26)             MAPPING CODE FOR USER CODE 3                 
         DC    CL5'USCD3'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'UFSCODE)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUCD3-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(27)             MAPPING CODE FOR USER FIELD 3                
         DC    CL5'USER3'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUSR3-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(28)             MAPPING CODE FOR USER CODE 4                 
         DC    CL5'USCD4'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'UFSCODE)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUCD4-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(29)             MAPPING CODE FOR USER FIELD 4                
         DC    CL5'USER4'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUSR4-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(30)             MAPPING CODE FOR USER CODE 5                 
         DC    CL5'USCD5'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'UFSCODE)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUCD5-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(31)             MAPPING CODE FOR USER FIELD 5                
         DC    CL5'USER5'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUSR5-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(32)             MAPPING CODE FOR USER CODE 6                 
         DC    CL5'USCD6'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'UFSCODE)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUCD6-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(33)             MAPPING CODE FOR USER FIELD 6                
         DC    CL5'USER6'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUSR6-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(34)             MAPPING CODE FOR USER CODE 7                 
         DC    CL5'USCD7'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'UFSCODE)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUCD7-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(35)             MAPPING CODE FOR USER FIELD 7                
         DC    CL5'USER7'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUSR7-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(36)             MAPPING CODE FOR USER CODE 8                 
         DC    CL5'USCD8'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'UFSCODE)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUCD8-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(37)             MAPPING CODE FOR USER FIELD 8                
         DC    CL5'USER8'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUSR8-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(38)             MAPPING CODE FOR USER CODE 9                 
         DC    CL5'USCD9'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'UFSCODE)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUCD9-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(39)             MAPPING CODE FOR USER FIELD 9                
         DC    CL5'USER9'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUSR9-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(40)             MAPPING CODE FOR USER CODE 10                
         DC    CL5'USC10'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'UFSCODE)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUC10-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(41)             MAPPING CODE FOR USER FIELD 10               
         DC    CL5'USE10'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVUSR10-T60B71)  RECEIVE ROUTINE                            
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(42)             MAPPING CODE FOR ADDRESS LINE 1              
         DC    CL5'ADDR1'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVAD1-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(43)             MAPPING CODE FOR ADDRESS LINE 2              
         DC    CL5'ADDR2'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVAD2-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(44)             MAPPING CODE FOR ADDRESS LINE 3              
         DC    CL5'ADDR3'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVAD3-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(45)             MAPPING CODE FOR ADDRESS LINE 4              
         DC    CL5'ADDR4'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVAD4-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(46)             MAPPING CODE FOR ADDRESS LINE 5              
         DC    CL5'ADDR5'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVAD5-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(47)             MAPPING CODE FOR PRODUCT CODE                
         DC    CL5'EXTMP'          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAEXCTMP)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVTMP-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(48)             MAPPING CODE FOR NEXT CYCLE                  
         DC    CL5'NXTCY'          TEXT IDENTIFIER                              
         DC    AL1(MDTPKQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FANXTCYC)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVNCY-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(49)             MAPPING CODE FOR ADJUST RATE                 
         DC    CL5'ADJRT'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAADJRAT)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVART-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(50)             MAPPING CODE FOR AUTO WRITE-OFF              
         DC    CL5'AUTWO'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAAUTOWO)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVAWO-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(51)             MAPPING CODE FOR TALENT MEDIA                
         DC    CL5'TALMD'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FATALMED)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVTMD-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(52)             MAPPING CODE FOR STUDIO CODE                 
         DC    CL5'STDIO'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FASTUDIO)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVSTU-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(53)             MAPPING CODE FOR LAST PERSON                 
         DC    CL5'LAPER'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(54)             MAPPING CODE FOR LAST ACTIVITY DATE          
         DC    CL5'LADTE'          TEXT IDENTIFIER                              
         DC    AL1(MDTDTQ)         DATA TYPE (DATE)                             
         DC    AL1(L'PACDATE)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(55)             MAPPING CODE FOR EXTRA COMMENTS              
         DC    CL5'XTRCM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVXTCM-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(56)             MAPPING CODE FOR PRIORITY                    
         DC    CL5'PRIOR'          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAPRIOTY)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVPRIO-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(57)             MAPPING CODE FOR LONG DESCRIPTION            
         DC    CL5'LDESC'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVLDSC-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(58)             MAPPING CODE FOR CLIENT NAME                 
         DC    CL5'CLINM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(59)             MAPPING CODE FOR PRODUCT NAME                
         DC    CL5'PRONM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(60)             MAPPING CODE FOR MEDIA CODE                  
         DC    CL5'MEDCD'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(61)             MAPPING CODE FOR MEDIA NAME                  
         DC    CL5'MEDNM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(62)             MAPPING CODE FOR JOB ADD DATE                
         DC    CL5'ADDDT'          TEXT IDENTIFIER                              
         DC    AL1(MDTDTQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'JADDATE)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(63)             MAPPING CODE FOR APPR/REJ STATUS             
         DC    CL5'ARSTA'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAJARSTA)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(64)             MAPPING CODE FOR ID NUMBER                   
         DC    CL5'IDNUM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAJBIDNO)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVIDNO-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(65)             MAPPING CODE FOR JOB APPROVER                
         DC    CL5'JBAPP'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAJBAPPR)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(66)             MAPPING CODE FOR APPR/REJ COMMENTS           
         DC    CL5'ARCOM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(67)             MAPPING CODE FOR CAMPAIGN CODE               
         DC    CL5'CAMPC'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (BINARY)                           
         DC    AL1(L'FACAMPCD)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVCAMP-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(68)             MAPPING CODE FOR CAMPAIGN NAME               
         DC    CL5'CAMPN'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(69)             MAPPING CODE FOR LONG DESC EXT 1             
         DC    CL5'LDEX1'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVLDE1-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(70)             MAPPING CODE FOR LONG DESC EXT 2             
         DC    CL5'LDEX2'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVLDE2-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(71)             MAPPING CODE FOR LONG DESC EXT 3             
         DC    CL5'LDEX3'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVLDE3-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(72)             MAPPING CODE FOR LONG DESC EXT 4             
         DC    CL5'LDEX4'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVLDE4-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(73)             MAPPING CODE FOR ROLE 1 NUMBER               
         DC    CL5'ROLE1'          ID NUMBER                                    
         DC    AL1(MDTBIQ)         DATA TYPE (BINARY)                           
         DC    AL1(L'FAROLE1)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVROL1-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(74)             MAPPING CODE FOR ROLE 1 NAME                 
         DC    CL5'RLNM1'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(75)             MAPPING CODE FOR PERSON 1 CODE               
         DC    CL5'PCOD1'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAPCOD1)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVPCD1-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(76)             MAPPING CODE FOR ROLE 1 LEVEL                
         DC    CL5'RLEV1'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FARLEV1)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVRLV1-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(77)             MAPPING CODE FOR ROLE 2 CODE                 
         DC    CL5'ROLE2'          ID NUMBER                                    
         DC    AL1(MDTBIQ)         DATA TYPE (BINARY)                           
         DC    AL1(L'FAROLE2)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVROL2-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(78)             MAPPING CODE FOR ROLE 2 NAME                 
         DC    CL5'RLNM2'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(79)             MAPPING CODE FOR PERSON 2 CODE               
         DC    CL5'PCOD2'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAPCOD2)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVPCD2-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(80)             MAPPING CODE FOR ROLE 2 LEVEL                
         DC    CL5'RLEV2'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FARLEV2)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVRLV2-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(81)             MAPPING CODE FOR ROLE 3 NUMBER               
         DC    CL5'ROLE3'          ID NUMBER                                    
         DC    AL1(MDTBIQ)         DATA TYPE (BINARY)                           
         DC    AL1(L'FAROLE3)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVROL3-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(82)             MAPPING CODE FOR ROLE 3 NAME                 
         DC    CL5'RLNM3'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(83)             MAPPING CODE FOR PERSON 3 CODE               
         DC    CL5'PCOD3'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAPCOD3)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVPCD3-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(84)             MAPPING CODE FOR ROLE 3 LEVEL                
         DC    CL5'RLEV3'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FARLEV3)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVRLV3-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(85)             MAPPING CODE FOR ROLE 4 CODE                 
         DC    CL5'ROLE4'          ID NUMBER                                    
         DC    AL1(MDTBIQ)         DATA TYPE (BINARY)                           
         DC    AL1(L'FAROLE4)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVROL4-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(86)             MAPPING CODE FOR ROLE 4 NAME                 
         DC    CL5'RLNM4'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(87)             MAPPING CODE FOR PERSON 4 CODE               
         DC    CL5'PCOD4'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAPCOD4)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVPCD4-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(88)             MAPPING CODE FOR ROLE 4 LEVEL                
         DC    CL5'RLEV4'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FARLEV4)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVRLV4-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(89)             MAPPING CODE FOR ROLE 5 NUMBER               
         DC    CL5'ROLE5'          ID NUMBER                                    
         DC    AL1(MDTBIQ)         DATA TYPE (BINARY)                           
         DC    AL1(L'FAROLE5)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVROL5-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(90)             MAPPING CODE FOR ROLE 5 NAME                 
         DC    CL5'RLNM5'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(91)             MAPPING CODE FOR PERSON 5 CODE               
         DC    CL5'PCOD5'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAPCOD5)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVPCD5-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(92)             MAPPING CODE FOR ROLE 5 LEVEL                
         DC    CL5'RLEV5'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FARLEV5)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVRLV5-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(93)             MAPPING CODE FOR ROLE 6 NUMBER               
         DC    CL5'ROLE6'          ID NUMBER                                    
         DC    AL1(MDTBIQ)         DATA TYPE (BINARY)                           
         DC    AL1(L'FAROLE6)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVROL6-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(94)             MAPPING CODE FOR ROLE 6 NAME                 
         DC    CL5'RLNM6'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(95)             MAPPING CODE FOR PERSON 6 CODE               
         DC    CL5'PCOD6'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAPCOD5)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVPCD6-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(96)             MAPPING CODE FOR ROLE 6 LEVEL                
         DC    CL5'RLEV6'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FARLEV6)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVRLV6-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(97)             MAPPING CODE FOR ROLE 7 NUMBER               
         DC    CL5'ROLE7'          ID NUMBER                                    
         DC    AL1(MDTBIQ)         DATA TYPE (BINARY)                           
         DC    AL1(L'FAROLE7)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVROL7-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(98)             MAPPING CODE FOR ROLE 7 NAME                 
         DC    CL5'RLNM7'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(99)             MAPPING CODE FOR PERSON 7 CODE               
         DC    CL5'PCOD7'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAPCOD7)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVPCD7-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(100)            MAPPING CODE FOR ROLE 7 LEVEL                
         DC    CL5'RLEV7'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FARLEV7)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVRLV7-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(101)            MAPPING CODE FOR ROLE 8 NUMBER               
         DC    CL5'ROLE8'          ID NUMBER                                    
         DC    AL1(MDTBIQ)         DATA TYPE (BINARY)                           
         DC    AL1(L'FAROLE8)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVROL8-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(102)            MAPPING CODE FOR ROLE 8 NAME                 
         DC    CL5'RLNM8'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(103)            MAPPING CODE FOR PERSON 8 CODE               
         DC    CL5'PCOD8'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAPCOD8)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVPCD8-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(104)            MAPPING CODE FOR ROLE 8 LEVEL                
         DC    CL5'RLEV8'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FARLEV8)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVRLV8-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(105)            MAPPING CODE FOR ROLE 9 NUMBER               
         DC    CL5'ROLE9'          ID NUMBER                                    
         DC    AL1(MDTBIQ)         DATA TYPE (BINARY)                           
         DC    AL1(L'FAROLE9)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVROL9-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(106)            MAPPING CODE FOR ROLE 9 NAME                 
         DC    CL5'RLNM9'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(107)            MAPPING CODE FOR PERSON 9 CODE               
         DC    CL5'PCOD9'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAPCOD9)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVPCD9-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(108)            MAPPING CODE FOR ROLE 9 LEVEL                
         DC    CL5'RLEV9'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FARLEV9)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVRLV9-T60B71) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(109)            MAPPING CODE FOR ROLE 10 NUMBER              
         DC    CL5'ROL10'          ID NUMBER                                    
         DC    AL1(MDTBIQ)         DATA TYPE (BINARY)                           
         DC    AL1(L'FAROLE10)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVROL10-T60B71) RECEIVE ROUTINE                             
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(110)            MAPPING CODE FOR ROLE 10 NAME                
         DC    CL5'RLN10'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(111)            MAPPING CODE FOR PERSON 10 CODE              
         DC    CL5'PCD10'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAPCOD10)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVPCD10-T60B71) RECEIVE ROUTINE                             
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(112)            MAPPING CODE FOR ROLE 10 LEVEL               
         DC    CL5'RLV10'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FARLEV10)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVRLV10-T60B71) RECEIVE ROUTINE                             
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(113)            MAPPING CODE FOR ROLE 11 NUMBER              
         DC    CL5'ROL11'          ID NUMBER                                    
         DC    AL1(MDTBIQ)         DATA TYPE (BINARY)                           
         DC    AL1(L'FAROLE11)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVROL11-T60B71) RECEIVE ROUTINE                             
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(114)            MAPPING CODE FOR ROLE 11 NAME                
         DC    CL5'RLN11'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(115)            MAPPING CODE FOR PERSON 11 CODE              
         DC    CL5'PCD11'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAPCOD11)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVPCD11-T60B71) RECEIVE ROUTINE                             
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(116)            MAPPING CODE FOR ROLE 11 LEVEL               
         DC    CL5'RLV11'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FARLEV11)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVRLV11-T60B71) RECEIVE ROUTINE                             
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(117)            MAPPING CODE FOR ROLE 12 NUMBER              
         DC    CL5'ROL12'          ID NUMBER                                    
         DC    AL1(MDTBIQ)         DATA TYPE (BINARY)                           
         DC    AL1(L'FAROLE12)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVROL12-T60B71) RECEIVE ROUTINE                             
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(118)            MAPPING CODE FOR ROLE 12 NAME                
         DC    CL5'RLN12'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(119)            MAPPING CODE FOR PERSON 12 CODE              
         DC    CL5'PCD12'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAPCOD12)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVPCD12-T60B71) RECEIVE ROUTINE                             
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(120)            MAPPING CODE FOR ROLE 12 LEVEL               
         DC    CL5'RLV12'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FARLEV12)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVRLV12-T60B71) RECEIVE ROUTINE                             
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(121)            MAPPING CODE FOR ROLE 13 NUMBER              
         DC    CL5'ROL13'          ID NUMBER                                    
         DC    AL1(MDTBIQ)         DATA TYPE (BINARY)                           
         DC    AL1(L'FAROLE13)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVROL13-T60B71) RECEIVE ROUTINE                             
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(122)            MAPPING CODE FOR ROLE 13 NAME                
         DC    CL5'RLN13'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(123)            MAPPING CODE FOR PERSON 13 CODE              
         DC    CL5'PCD13'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAPCOD13)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVPCD13-T60B71) RECEIVE ROUTINE                             
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(124)            MAPPING CODE FOR ROLE 13 LEVEL               
         DC    CL5'RLV13'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FARLEV13)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVRLV13-T60B71) RECEIVE ROUTINE                             
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(125)            MAPPING CODE FOR ROLE 14 NUMBER              
         DC    CL5'ROL14'          ID NUMBER                                    
         DC    AL1(MDTBIQ)         DATA TYPE (BINARY)                           
         DC    AL1(L'FAROLE14)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVROL14-T60B71) RECEIVE ROUTINE                             
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(126)            MAPPING CODE FOR ROLE 14 NAME                
         DC    CL5'RLN14'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(127)            MAPPING CODE FOR PERSON 14 CODE              
         DC    CL5'PCD14'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAPCOD14)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVPCD14-T60B71) RECEIVE ROUTINE                             
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(128)            MAPPING CODE FOR ROLE 14 LEVEL               
         DC    CL5'RLV14'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FARLEV14)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(RCVRLV14-T60B71) RECEIVE ROUTINE                             
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(129)            MAPPING CODE FOR JOB=LANGUAGE                
         DC    CL5'JULDF'          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'FAJULDEF)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVJUL-T60B71)  RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 01                     
         DC    AL2(130)              NUMBER                                     
         DC    CL5'CPO01'            TEXT IDENTIFIER                            
         DC    AL1(MDTCHQ)           DATA TYPE (CHARACTER)                      
         DC    AL1(MDLENV)           DATA LENGTH                                
         DC    AL1(0)                DATA DISPLACEMENT                          
         DC    AL1(MDINNULL)         INDICATORS                                 
         DC    AL2(RCVCPO01-T60B71)  RECEIVE ROUTINE                            
         DC    AL2(0)                SEND ROUTINE                               
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 01 AMOUNT              
         DC    AL2(131)                                                         
         DC    CL5'CPA01'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA01-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 02                     
         DC    AL2(132)                                                         
         DC    CL5'CPO02'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO02-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 02 AMOUNT              
         DC    AL2(133)                                                         
         DC    CL5'CPA02'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA02-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 03                     
         DC    AL2(134)                                                         
         DC    CL5'CPO03'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO03-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 03 AMOUNT              
         DC    AL2(135)                                                         
         DC    CL5'CPA03'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA03-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 04                     
         DC    AL2(136)                                                         
         DC    CL5'CPO04'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO04-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 04 AMOUNT              
         DC    AL2(137)                                                         
         DC    CL5'CPA04'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA04-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 05                     
         DC    AL2(138)                                                         
         DC    CL5'CPO05'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO05-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 05 AMOUNT              
         DC    AL2(139)                                                         
         DC    CL5'CPA05'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA05-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 06                     
         DC    AL2(140)                                                         
         DC    CL5'CPO06'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO06-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 06 AMOUNT              
         DC    AL2(141)                                                         
         DC    CL5'CPA06'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA06-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 07                     
         DC    AL2(142)                                                         
         DC    CL5'CPO07'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO07-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 07 AMOUNT              
         DC    AL2(143)                                                         
         DC    CL5'CPA07'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA07-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 08                     
         DC    AL2(144)                                                         
         DC    CL5'CPO08'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO08-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 08 AMOUNT              
         DC    AL2(145)                                                         
         DC    CL5'CPA08'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA08-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 09                     
         DC    AL2(146)                                                         
         DC    CL5'CPO09'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO09-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 09 AMOUNT              
         DC    AL2(147)                                                         
         DC    CL5'CPA09'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA09-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 10                     
         DC    AL2(148)                                                         
         DC    CL5'CPO10'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO10-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 10 AMOUNT              
         DC    AL2(149)                                                         
         DC    CL5'CPA10'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA10-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 11                     
         DC    AL2(150)                                                         
         DC    CL5'CPO11'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO11-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 11 AMOUNT              
         DC    AL2(151)                                                         
         DC    CL5'CPA11'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA11-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 12                     
         DC    AL2(152)                                                         
         DC    CL5'CPO12'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO12-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 12 AMOUNT              
         DC    AL2(153)                                                         
         DC    CL5'CPA12'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA12-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 13                     
         DC    AL2(154)                                                         
         DC    CL5'CPO13'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO13-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 13 AMOUNT              
         DC    AL2(155)                                                         
         DC    CL5'CPA13'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA13-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 14                     
         DC    AL2(156)                                                         
         DC    CL5'CPO14'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO14-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 14 AMOUNT              
         DC    AL2(157)                                                         
         DC    CL5'CPA14'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA14-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 15                     
         DC    AL2(158)                                                         
         DC    CL5'CPO15'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO15-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 15 AMOUNT              
         DC    AL2(159)                                                         
         DC    CL5'CPA15'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA15-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 16                     
         DC    AL2(160)                                                         
         DC    CL5'CPO16'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO16-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 16 AMOUNT              
         DC    AL2(161)                                                         
         DC    CL5'CPA16'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA16-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 17                     
         DC    AL2(162)                                                         
         DC    CL5'CPO17'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO17-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 17 AMOUNT              
         DC    AL2(163)                                                         
         DC    CL5'CPA17'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA17-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 18                     
         DC    AL2(164)                                                         
         DC    CL5'CPO18'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO18-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 18 AMOUNT              
         DC    AL2(165)                                                         
         DC    CL5'CPA18'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA18-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 19                     
         DC    AL2(166)                                                         
         DC    CL5'CPO19'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO19-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 19 AMOUNT              
         DC    AL2(167)                                                         
         DC    CL5'CPA19'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA19-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 20                     
         DC    AL2(168)                                                         
         DC    CL5'CPO20'                                                       
         DC    AL1(MDTCHQ)                                                      
         DC    AL1(MDLENV)                                                      
         DC    AL1(0)                                                           
         DC    AL1(MDINNULL)                                                    
         DC    AL2(RCVCPO20-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR CLIENT PO 19 AMOUNT              
         DC    AL2(169)                                                         
         DC    CL5'CPA20'                                                       
         DC    AL1(MDTPKQ)                                                      
         DC    AL1(16)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVCPA20-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        MAPPING FOR FEE BUDGET AMOUNT                
         DC    AL2(170)              NUMBER                                     
         DC    CL5'FBAMT'            TEXT IDENTIFIER                            
         DC    AL1(MDTPKQ)           DATA TYPE (CHARACTER)                      
         DC    AL1(16)               DATA LENGTH                                
         DC    AL1(0)                DATA DISPLACEMENT                          
         DC    AL1(0)                INDICATORS                                 
         DC    AL2(RCVFBA-T60B71)    RECEIVE ROUTINE                            
         DC    AL2(0)                SEND ROUTINE                               
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(171)            MAPPING CODE FOR JOB LOCKS                   
         DC    CL5'JLEST'                                                       
         DC    AL1(MDTHXQ)                                                      
         DC    AL1(L'FAJLEST)                                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVJLEST-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(172)                                                         
         DC    CL5'JLORD'                                                       
         DC    AL1(MDTHXQ)                                                      
         DC    AL1(L'FAJLORD)                                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVJLORD-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(173)                                                         
         DC    CL5'JLBIL'                                                       
         DC    AL1(MDTHXQ)                                                      
         DC    AL1(L'FAJLBIL)                                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVJLBIL-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(174)            MAPPING CODE FOR JOB LOCKS                   
         DC    CL5'JLTSI'                                                       
         DC    AL1(MDTHXQ)                                                      
         DC    AL1(L'FAJLTSI)                                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVJLTSI-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(175)            MAPPING CODE FOR JOB LOCKS                   
         DC    CL5'JLADJ'                                                       
         DC    AL1(MDTHXQ)                                                      
         DC    AL1(L'FAJLADJ)                                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVJLADJ-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(176)            MAPPING CODE FOR JOB LOCKS                   
         DC    CL5'JLEXT'                                                       
         DC    AL1(MDTHXQ)                                                      
         DC    AL1(L'FAJLEXT)                                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(RCVJLEXT-T60B71)                                             
         DC    AL2(0)                                                           
         DC    AL1(0,0,0)                                                       
*                                                                               
JBMELX   DC    XL1'00'                                                          
                                                                                
MAPTABX  DC    XL1'00'                                                          
         EJECT                                                                  
USERD    DSECT                                                                  
         DS    CL1                                                              
         DS    CL1                                                              
         DS    CL3                                                              
USEROFG  DS    CL1                                                              
USEROFF  DS    CL2                                                              
USERCLI  DS    CL6                                                              
USERPRO  DS    CL6                                                              
USERMG   DS    CL1                                                              
USERMED  DS    CL1                                                              
USERJOB  DS    CL6                                                              
         EJECT                                                                  
ELMTABD  DSECT                                                                  
ELMDATA  DS    0CL32                                                            
         DS    CL4                                                              
ELMCODE  DS    CL2                                                              
ELMDESC  DS    CL12                                                             
ELMEDIT  DS    CL8                                                              
         DS    CL1                                                              
ELMSTAT  DS    XL1                                                              
         DS    CL4                                                              
ELMLNG   EQU   *-ELMDATA                                                        
*                                                                               
FAUSERD  DSECT                                                                  
FAUSCODE DS    CL2                                                              
FAUSDATA DS    CL30                                                             
FAUSDTLN DS    XL1                                                              
FQUSERLN EQU   *-FAUSERD                                                        
*                                                                               
FATEAJD  DSECT                                                                  
FAROLE   DS    XL2                 ROLE NUMBER                                  
FARLNM   DS    CL36                ROLE NAME                                    
FAPCOD   DS    CL8                 PERSON CODE                                  
FARLEV   DS    CL1                 TEAM LIST LEVEL                              
FATEAJLN EQU   *-FATEAJD                                                        
*                                                                               
NUMTABD  DSECT                                                                  
NUMVAL   DS    CL2                 VALUE FIELD                                  
NUMMOVE  DS    S                   A(MOVE FIELD)                                
NUMADDR  DS    S                   A(ROUTINE)                                   
NUMLEN   EQU   *-NUMTABD                                                        
*                                                                               
CTEAMD   DSECT                                                                  
CTSIGN   DS    CL1                 REMOVING (-) OR ADDING (+)                   
CTROLE   DS    XL2                 ROLE NUMBER                                  
CTBPID   DS    XL2                 PERSON BINARY PID                            
CTLEN    EQU   *-CTEAMD                                                         
         EJECT                                                                  
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
APRTABL  EQU   *-APRTABD                                                        
         EJECT                                                                  
* ACPROWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*                                                                               
         ORG   T60BFFD+4096                                                     
POINTERS DS    XL(8*54+1)          PASSIVE POINTER BLOCK                        
         ORG                                                                    
*                                                                               
* ACPROLINK                                                                     
       ++INCLUDE ACPROLINK                                                      
         SPACE 1                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
LINKD    DSECT                                                                  
         ORG   OVRWS                                                            
*                                                                               
DUMFLDD  DS    0XL68                                                            
DUMFLDH  DS    0XL8                                                             
         DS    XL5                                                              
DUMFLDHL DS    XL1                                                              
         DS    XL2                                                              
DUMFLD   DS    XL60                MAX FIELD SIZE USED IS 60                    
*                                                                               
PZERO    DS    PL1                                                              
*                                                                               
FAACTION DS    XL1                 ACTION                                       
FQACTDIS EQU   1                   DISPLAY / GET JOB RECORD                     
FQACTADD EQU   2                   ADD JOB RECORD WITH MANUAL NUMBER            
FQACTCHA EQU   3                   CHANGE EXISTING JOB RECORD                   
FQACTAUT EQU   4                   ADD JOB RECORD WITH AUTO NUMBER              
FQACTNUM EQU   5                   GET AUTO NUMBER WITHOUT ADDING JOB           
*                                                                               
FACLIENT DS    CL5                 CLIENT CODE                                  
FLCLIENT DS    XL1                 I/P LENGTH                                   
*                                                                               
FAPROD   DS    CL3                 PRODUCT                                      
FLPROD   DS    XL1                 I/P LENGTH                                   
*                                                                               
FAJOB    DS    CL6                 JOB CODE                                     
FLJOB    DS    XL1                 I/P LENGTH                                   
*                                                                               
FAJBNAME DS    CL36                JOB NAME                                     
FLJBNAME DS    XL1                 I/PLENGTH                                    
*                                                                               
FAFILT1  DS    CL1                 JOB FILTERS 1-5 & OFFICE                     
FAFILT2  DS    CL1                                                              
FAFILT3  DS    CL1                                                              
FAFILT4  DS    CL1                                                              
FAFILT5  DS    CL1                                                              
FAOFFIC  DS    CL2                                                              
FLOFFIC  DS    XL1                                                              
*                                                                               
FAJBSTAT DS    CL1                 JOB STATUS                                   
*                                                                               
FAJARSTA DS    CL1                 REJECT/APPROVE STATUS                        
FAJBAPPR DS    CL1                 JOB APPROVER?                                
FACAMPCD DS    XL4                 CAMPAIGN CODE                                
*                                                                               
FACLOSDT DS    PL3                 CLOSE DATE                                   
FAOPENDT DS    PL3                 OPEN DATE                                    
*                                                                               
FAPRTBIL DS    CL49                PRINT ON BILL                                
FLPRTBIL DS    XL1                 I/P LENGTH                                   
*                                                                               
FAOTHIN1 DS    CL50                OTHER INFO 1                                 
FLOTHIN1 DS    XL1                 I/P LENGTH                                   
FAOTHIN2 DS    CL50                OTHER INFO 2                                 
FLOTHIN2 DS    XL1                 I/P LENGTH                                   
FAOTHIN3 DS    CL50                OTHER INFO 3                                 
FLOTHIN3 DS    XL1                 I/P LENGTH                                   
*                                                                               
FAJOBCM1 DS    CL50                JOB COMMENTS 1                               
FLJOBCM1 DS    XL1                 I/P LENGTH                                   
FAJOBCM2 DS    CL50                JOB COMMENTS 2                               
FLJOBCM2 DS    XL1                 I/P LENGTH                                   
FAJOBCM3 DS    CL50                JOB COMMENTS 3                               
FLJOBCM3 DS    XL1                 I/P LENGTH                                   
*                                                                               
FAUSCD1  DS    CL2                 USER FIELD CODE                              
FAUSER1  DS    CL30                USER FIELD DATA                              
FLUSER1  DS    XL1                 USER FIELD DATA LENGTH                       
*                                                                               
FAUSCD2  DS    CL2                                                              
FAUSER2  DS    CL30                                                             
FLUSER2  DS    XL1                                                              
*                                                                               
FAUSCD3  DS    CL2                                                              
FAUSER3  DS    CL30                                                             
FLUSER3  DS    XL1                                                              
*                                                                               
FAUSCD4  DS    CL2                                                              
FAUSER4  DS    CL30                                                             
FLUSER4  DS    XL1                                                              
*                                                                               
FAUSCD5  DS    CL2                                                              
FAUSER5  DS    CL30                                                             
FLUSER5  DS    XL1                                                              
*                                                                               
FAUSCD6  DS    CL2                                                              
FAUSER6  DS    CL30                                                             
FLUSER6  DS    XL1                                                              
*                                                                               
FAUSCD7  DS    CL2                                                              
FAUSER7  DS    CL30                                                             
FLUSER7  DS    XL1                                                              
*                                                                               
FAUSCD8  DS    CL2                                                              
FAUSER8  DS    CL30                                                             
FLUSER8  DS    XL1                                                              
*                                                                               
FAUSCD9  DS    CL2                                                              
FAUSER9  DS    CL30                                                             
FLUSER9  DS    XL1                                                              
*                                                                               
FAUSCD10 DS    CL2                                                              
FAUSER10 DS    CL30                                                             
FLUSER10 DS    XL1                                                              
*                                                                               
FAADDR1  DS    CL26                                                             
FLADDR1  DS    XL1                                                              
FAADDR2  DS    CL26                                                             
FLADDR2  DS    XL1                                                              
FAADDR3  DS    CL26                                                             
FLADDR3  DS    XL1                                                              
FAADDR4  DS    CL26                                                             
FLADDR4  DS    XL1                                                              
FAADDR5  DS    CL26                                                             
FLADDR5  DS    XL1                                                              
*                                                                               
FAEXTRCM DS    CL60                (DESKTOP) EXTRA COMMENTS                     
FLEXTRCM DS    XL1                                                              
FAPRIOTY DS    XL1                 (DESKTOP) PRIORITY                           
*                                                                               
FAEXCTMP DS    XL1                 EXCLUDE FROM TEMPO                           
FAJULDEF DS    CL1                 JOB=LANGUAGE                                 
FANXTCYC DS    PL1                 NEXT CYCLE                                   
FAADJRAT DS    XL1                 ADJUST RATE                                  
FAAUTOWO DS    XL1                 AUTO WRITEOFF                                
FATALMED DS    CL1                 TALENT MEDIA                                 
FASTUDIO DS    CL4                 STUDIO                                       
FLSTUDIO DS    XL1                                                              
FAJLEST  DS    CL1                 JOB LOCKS                                    
FAJLORD  DS    CL1                                                              
FAJLBIL  DS    CL1                                                              
FAJLTSI  DS    CL1                                                              
FAJLADJ  DS    CL1                                                              
FAJLEXT  DS    CL1                                                              
*                                                                               
FALAPER  DS    CL8                 LAST ACTIVITY PERSON                         
FLLAPER  DS    XL1                 I/P LENGTH                                   
FALADTE  DS    XL3                 LAST ACTIVITY DATE                           
*                                                                               
FAJBIDNO DS    CL4                 ID NUMBER                                    
FAFEEAMT DS    PL6                 FEE BUDGET AMOUNT                            
*                                                                               
FAARCOML DS    XL1                 APPROVE/REJECT LENGTH/COMMENT                
FAARCOMM DS    CL50                                                             
*                                                                               
APOINTRS DS    A                   A(POINTER BLOCK)                             
SAVER6   DS    A                   SAVE AREA FOR R6                             
SAVERE   DS    A                   SAVE AREA FOR RE                             
SAVERF   DS    A                   SAVE AREA FOR RF                             
SAVEPY   DS    A                   SAVE ADDRESS OF YEAR POSITION IN JOB         
ASEQNUM  DS    A                   A(STARTING SEQUENCE NUMBER)                  
ASVCTEAM DS    A                   A(SAVED ELEMENT)                             
RELO     DS    A                   RELOCATION FACTOR                            
AFINDJOB DS    A                   A(JOB NUMBER ROUTINE)                        
AGETUSR  DS    A                   A(GET USER DETAIL ROUTINE)                   
AVALJCOM DS    A                   A(VALIDATE JOB COMMENTS)                     
AADDPROF DS    A                   A(ADDPROF)                                   
AADDASTA DS    A                   A(ADDASTA)                                   
ABALPEEL DS    A                   A(BALPEEL)                                   
AGETLDGS DS    A                   A(GETLDGS)                                   
AGETPID  DS    A                   A(GETPID)                                    
AADDDRAP DS    A                   A(ADDDRAP)                                   
AVALNAME DS    A                   A(VALNAME)                                   
AVALSTAT DS    A                   A(VALIDATE STATUS)                           
AVCLOSE  DS    A                   A(VCLOSE)                                    
ANAMSRCH DS    A                   A(NAMESRCH)                                  
AVALLAL  DS    A                   A(VALLAL)                                    
AADDEST  DS    A                   A(ADDEST)                                    
AADDPASS DS    A                   A(ADDPASS)                                   
AGETAPP  DS    A                   A(GETAPP)                                    
AVALACAD DS    A                   A(VALACADR)                                  
AVALEXTC DS    A                   A(VALEXTCM)                                  
AVALCAMP DS    A                   A(VALCAMPC)                                  
AVALTEAJ DS    A                   A(VALTEAJ)                                   
AADD2JOB DS    A                   A(ADD2JOB)                                   
AAEXAREA DS    A                   A(AEXAREA)                                   
AAEXANXT DS    A                   A(NEXT ENTRY)                                
*&&US                                                                           
AGOXBLK  DS    A                   A(GOXBLK)                                    
*&&                                                                             
FUNCTION DS    CL8                 SAVE DATAMGR COMMAND                         
USERKEY  DS    CL42                                                             
TODAY    DS    CL6                                                              
EFFDATE  DS    PL3                                                              
START#   DS    CL5                 JOB NUMBER                                   
NEXT#    DS    CL5                 JOB NUMBER                                   
WORK5    DS    CL5                 JOB NUMBER                                   
DATELEN  DS    CL1                 DATE LENGTH FOR PERVAL CALL                  
DATEFORM DS    CL1                 DATE FORMAT FOR PERVAL CALL                  
MYLSTA2  DS    XL1                                                              
MYAPPST  DS    XL2                                                              
ROYIND   DS    XL1                 RESET CURR YEAR SEQ JOBNO. INDICATOR         
ROYIREP  EQU   X'80'               2ND TIME (00 = FIRST TIME)                   
CNTAEX   DS    XL1                                                              
AEXIND   DS    XL1                                                              
AEXIDATQ EQU   X'80'                                                            
AEXIRETQ EQU   X'40'                                                            
AEXIDELQ EQU   X'20'                                                            
AEXIUPDQ EQU   X'10'                                                            
AEXIRECQ EQU   X'01'                                                            
AEXIADDQ EQU   X'02'                                                            
RUNINDS  DS    XL1                                                              
RUNIREAD EQU   X'20'                                                            
SAVE1ST  DS    CL6                 SAVE AREA FOR 1ST JOB NUMBER                 
CLPRLK   DS    XL1                 CLIENT OR PRODUCT LOCKED?                    
CLILOKD  EQU   X'80'               CLIENT IS LOCKED                             
PROLOKD  EQU   X'40'               PRODUCT IS LOCKED                            
PERVBLK  DS    CL50                STORAGE BLOCK FOR PERVAL                     
IOKEY    DS    XL64                THIS WAS XL80                                
DISKDA   DS    XL4                                                              
BLANKS   DS    CL256                                                            
         DS    0F                                                               
ELMCNT   DS    PL2                 COUNT OF ELMTAB ENTRIES                      
ELMTAB   DS    CL320               32 (ELEMENT) X 10                            
MYCOUNT  DS    XL1                                                              
SNDMODE  DS    XL1                                                              
MYSVRE   DS    A                                                                
THISCOMP DS    XL1                                                              
PRODLEDG DS    CL2                 PRODUCTION UNIT/LEDGER                       
CMPSTAT1 DS    XL1                 COMPANY STATUS BYTE 1                        
CMPDJCDO DS    XL2                 COMPANY DEF JOB CLOSE DATE OFFSET            
CMPALPHA DS    CL2                 COMPANY ALPHA ID                             
SAVEOFC  DS    CL2                 SAVED EFFECTIVE OFFICE                       
SVSTART  DS    PL3                 SAVE OPEN/START DATE                         
NEWDATE  DS    X                   FLAG FOR OPENED DATE CHANGE                  
SAVEJOB  DS    CL12                                                             
SAVEKEY  DS    CL42                SAVE AREA FOR KEY                            
ADDTYPE  DS    CL1                 ESTIMATE TYPE TO BE ADDED                    
MYOPEN   DS    PL3                                                              
JADDATE  DS    PL3                                                              
OPEN     DS    CL6                                                              
CLOSED   DS    CL6                                                              
OLDJOBNM DS    CL(NAMLN1Q+L'NAMEREC)                                            
NAMEFLAG DS    XL1                                                              
NFCHANGE EQU   X'80'                                                            
SVPASSWD DS    XL2                                                              
SVTERMNO DS    XL2                                                              
SVTIME   DS    F                                                                
SVJCREAT DS    XL2                                                              
LATECPJ  DS    XL12                LATEST CLIENT PRODUCT JOB                    
SVJNKEY  DS    CL64                JOB NUMBER KEY                               
SCLIKHD  DS    XL15                                                             
SPROKHD  DS    XL15                                                             
SJOBCOD  DS    XL15                                                             
CLPROFF  DS    CL2                 CLIENT/PRODUCT OFFICE                        
SAVOFF   DS    CL(L'TRNOFFC)                                                    
SAVAIO   DS    A                                                                
MYBYTE   DS    XL1                                                              
MYBYTE2  DS    XL1                                                              
CTEACNT  DS    XL1                                                              
MYVAL0   DS    CL1                                                              
MYVAL1   DS    CL1                                                              
MYVAL2   DS    CL1                                                              
MYVAL3   DS    CL1                                                              
MYVAL4   DS    CL1                                                              
MYVAL5   DS    CL1                                                              
MYVAL6   DS    CL1                                                              
*&&US                                                                           
MYJSTA2  DS    CL1                                                              
*&&                                                                             
*                                                                               
       ++INCLUDE ACGOBLOCK                                                      
         DS    0F                                                               
LGOBBLK  DS    XL600                                                            
*                                                                               
* JOB LONG DESCRIPTION + EXTENSIONS 1-4                                         
FLJBDESC DS    XL1                                                              
FAJBDESC DS    CL200                                                            
FLJBDEX1 DS    XL1                                                              
FAJBDEX1 DS    CL200                                                            
FLJBDEX2 DS    XL1                                                              
FAJBDEX2 DS    CL200                                                            
FLJBDEX3 DS    XL1                                                              
FAJBDEX3 DS    CL200                                                            
FLJBDEX4 DS    XL1                                                              
FAJBDEX4 DS    CL200                                                            
*                                                                               
* CLIENT PO NUMBERS 1-20                                                        
FLCLPO01 DS    XL1                                                              
FACLPO01 DS    CL20                                                             
FACPOA01 DS    PL6                                                              
FLCLPO02 DS    XL1                                                              
FACLPO02 DS    CL20                                                             
FACPOA02 DS    PL6                                                              
FLCLPO03 DS    XL1                                                              
FACLPO03 DS    CL20                                                             
FACPOA03 DS    PL6                                                              
FLCLPO04 DS    XL1                                                              
FACLPO04 DS    CL20                                                             
FACPOA04 DS    PL6                                                              
FLCLPO05 DS    XL1                                                              
FACLPO05 DS    CL20                                                             
FACPOA05 DS    PL6                                                              
FLCLPO06 DS    XL1                                                              
FACLPO06 DS    CL20                                                             
FACPOA06 DS    PL6                                                              
FLCLPO07 DS    XL1                                                              
FACLPO07 DS    CL20                                                             
FACPOA07 DS    PL6                                                              
FLCLPO08 DS    XL1                                                              
FACLPO08 DS    CL20                                                             
FACPOA08 DS    PL6                                                              
FLCLPO09 DS    XL1                                                              
FACLPO09 DS    CL20                                                             
FACPOA09 DS    PL6                                                              
FLCLPO10 DS    XL1                                                              
FACLPO10 DS    CL20                                                             
FACPOA10 DS    PL6                                                              
FLCLPO11 DS    XL1                                                              
FACLPO11 DS    CL20                                                             
FACPOA11 DS    PL6                                                              
FLCLPO12 DS    XL1                                                              
FACLPO12 DS    CL20                                                             
FACPOA12 DS    PL6                                                              
FLCLPO13 DS    XL1                                                              
FACLPO13 DS    CL20                                                             
FACPOA13 DS    PL6                                                              
FLCLPO14 DS    XL1                                                              
FACLPO14 DS    CL20                                                             
FACPOA14 DS    PL6                                                              
FLCLPO15 DS    XL1                                                              
FACLPO15 DS    CL20                                                             
FACPOA15 DS    PL6                                                              
FLCLPO16 DS    XL1                                                              
FACLPO16 DS    CL20                                                             
FACPOA16 DS    PL6                                                              
FLCLPO17 DS    XL1                                                              
FACLPO17 DS    CL20                                                             
FACPOA17 DS    PL6                                                              
FLCLPO18 DS    XL1                                                              
FACLPO18 DS    CL20                                                             
FACPOA18 DS    PL6                                                              
FLCLPO19 DS    XL1                                                              
FACLPO19 DS    CL20                                                             
FACPOA19 DS    PL6                                                              
FLCLPO20 DS    XL1                                                              
FACLPO20 DS    CL20                                                             
FACPOA20 DS    PL6                                                              
*                                                                               
* ROLE DETAILS 1-14                                                             
FAROLE1  DS    XL2                 ROLE NUMBER                                  
FARLNM1  DS    CL36                ROLE NAME                                    
FAPCOD1  DS    CL8                 PERSON CODE (FOR ROLE)                       
FARLEV1  DS    CL1                 LEVEL  C/P/J (FOR ROLE)                      
FARSTRT  EQU   *-FAROLE1                                                        
FAROLE2  DS    XL2                                                              
FARLNM2  DS    CL36                                                             
FAPCOD2  DS    CL8                                                              
FARLEV2  DS    CL1                                                              
FAROLE3  DS    XL2                                                              
FARLNM3  DS    CL36                                                             
FAPCOD3  DS    CL8                                                              
FARLEV3  DS    CL1                                                              
FAROLE4  DS    XL2                                                              
FARLNM4  DS    CL36                                                             
FAPCOD4  DS    CL8                                                              
FARLEV4  DS    CL1                                                              
FAROLE5  DS    XL2                                                              
FARLNM5  DS    CL36                                                             
FAPCOD5  DS    CL8                                                              
FARLEV5  DS    CL1                                                              
FAROLE6  DS    XL2                                                              
FARLNM6  DS    CL36                                                             
FAPCOD6  DS    CL8                                                              
FARLEV6  DS    CL1                                                              
FAROLE7  DS    XL2                                                              
FARLNM7  DS    CL36                                                             
FAPCOD7  DS    CL8                                                              
FARLEV7  DS    CL1                                                              
FAROLE8  DS    XL2                                                              
FARLNM8  DS    CL36                                                             
FAPCOD8  DS    CL8                                                              
FARLEV8  DS    CL1                                                              
FAROLE9  DS    XL2                                                              
FARLNM9  DS    CL36                                                             
FAPCOD9  DS    CL8                                                              
FARLEV9  DS    CL1                                                              
FAROLE10 DS    XL2                                                              
FARLNM10 DS    CL36                                                             
FAPCOD10 DS    CL8                                                              
FARLEV10 DS    CL1                                                              
FAROLE11 DS    XL2                                                              
FARLNM11 DS    CL36                                                             
FAPCOD11 DS    CL8                                                              
FARLEV11 DS    CL1                                                              
FAROLE12 DS    XL2                                                              
FARLNM12 DS    CL36                                                             
FAPCOD12 DS    CL8                                                              
FARLEV12 DS    CL1                                                              
FAROLE13 DS    XL2                                                              
FARLNM13 DS    CL36                                                             
FAPCOD13 DS    CL8                                                              
FARLEV13 DS    CL1                                                              
FAROLE14 DS    XL2                                                              
FARLNM14 DS    CL36                                                             
FAPCOD14 DS    CL8                                                              
FARLEV14 DS    CL1                                                              
*                                                                               
FARENDL  EQU   *-FAROLE1                                                        
FARNUM   EQU   FARENDL/FARSTRT                                                  
*                                                                               
SVCTEAM  DS    XL256                                                            
         DS    (L'OVRWS-(*-OVRWS))X                                             
*                                                                               
         EJECT                                                                  
*&&US                                                                           
GOXBLKD  DSECT                                                                  
       ++INCLUDE ACGOXBLOCK                                                     
*&&                                                                             
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*  ACGENBOTH                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*  ACGENFILE                                                                    
*        PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*                                                                               
*        ORG   T60BFFD+4096                                                     
*                                                                               
*OINTERS DS    XL(8*54+1)          PASSIVE POINTER BLOCK                        
         ORG                                                                    
         SPACE 2                                                                
         ORG                                                                    
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
* ACMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
* ACGOBBLOCK                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGOBBLOCK                                                     
         PRINT ON                                                               
* DDCTRYEQUS                                                                    
        PRINT OFF                                                               
       ++INCLUDE DDCTRYEQUS                                                     
         PRINT ON                                                               
* FAFACTS                                                                       
        PRINT OFF                                                               
       ++INCLUDE FAFACTS                                                        
        PRINT ON                                                                
* FAUTL                                                                         
        PRINT OFF                                                               
       ++INCLUDE FAUTL                                                          
        PRINT ON                                                                
* ACGENRAC                                                                      
        PRINT OFF                                                               
       ++INCLUDE ACGENRAC                                                       
        PRINT ON                                                                
* SEACSFILE                                                                     
        PRINT OFF                                                               
       ++INCLUDE SEACSFILE                                                      
        PRINT ON                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'087ACPRO71   04/28/15'                                      
         END                                                                    
