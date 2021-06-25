*          DATA SET ACPRO03    AT LEVEL 063 AS OF 08/03/17                      
*PHASE T60B03B                                                                  
*INCLUDE ACSRCHP                                                                
*INCLUDE SRCHPASS                                                               
*INCLUDE BINSRCH2                                                               
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B03 - JOB RECORD MAINTENANCE'                                
                                                                                
***********************************************************************         
* WHEN AMENDING THIS BOOK PLEASE CHECK IF CHANGES ARE ALSO REQUIRED   *         
* IN ACPRO71, THE FALINK JOB MAINTENANCE OVERLAY FOR @TRACKER         *         
***********************************************************************         
                                                                                
* GHOA 063 05MAY17 (MERGE UK CODE | LEVELS 17, 18, 19, AND 21)                  
*   MPEN 017 09JAN15 <RD001822> RELAX =PROD DELETE TO MATCH AURA                
*   MPEN 018 03MAR17 <PCA02609> FIX FOR THE ABOVE                               
*   TKLU 019 05MAY17 <SP012738> FIX TO PREVIOUS LEVELS                          
*   MPEN 021 10JUL17 <RD012112> BUG FIXES FOR RESTORING A JOB                   
                                                                                
T60B03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (MYDEND-MYD),T60B03**,R7,R8,RR=R2,CLEAR=YES                      
         LR    R4,RC                                                            
         USING MYD,R4                                                           
         ST    R2,MYRELO                                                        
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     RE,=V(ACSRCHP)                                                   
         A     RE,MYRELO                                                        
         ST    RE,ACSRCHP                                                       
*                                                                               
         GOTO1 VMODPTRS,DMCB,(X'80',POINTERS)                                   
*                                                                               
         L     R3,ACOMFACS                                                      
         USING COMFACSD,R3                                                      
         L     RF,CSWITCH                                                       
         MVC   DMCB(4),=XL4'FFFFFFFF'         A(UTL ENTRY)                      
         GOTO1 (RF),DMCB                                                        
         L     R3,0(R1)            ADDRESS OF UTL                               
         MVI   USCRIPT,C'N'                                                     
         TM    TSTAT6-UTLD(R3),TST6SCRP                                         
         BZ    *+8                                                              
         MVI   USCRIPT,C'Y'                                                     
         DROP  R3                                                               
*                                                                               
         TM    GENSTAT6,GES$LINK   TEST DDLINK UPLOAD IN PROCESS                
         BNO   MODE1                                                            
         OI    GENSTAT2,USMYOK     TELL GEGENEW USING MY OWN MSG                
         MVC   TWAALIAS,PERSONID   USE PERSONID USER LOGGED IN WITH             
         CLC   =C'JOB@@@@',CONREC  SPECIAL RECORD FOR SCRIPT TO KNOW            
         BNE   MODE1               IT'S JOB/AUTO SO...                          
         MVC   CONACT(4),=C'AUTO'  CHANGE ACTION FROM ADD TO AUTO               
*                                                                               
MODE1    NI    GENSTAT1,X'FF'-OKADDEL                                           
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY             VALIDATE COMPANY, CLIENT, PRODUCT            
         CLC   CONACT(6),=C'DELETE'                                             
         BE    DELETIT                                                          
         CLC   CONACT(7),=C'RESTORE'                                            
         BE    RESTIT                                                           
         CLI   ACTNUM,ACTADD       AND JOB                                      
         BNE   OKEXIT                                                           
         GOTOR PROCPF,DMCB,(RC)    FOR ACTION=ADD,LOOK FOR PF KEYS              
         GOTOR PROCPF8,DMCB,(RC)                                                
         B     OKEXIT                                                           
                                                                                
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE4                                                            
         CLC   CONACT(4),=C'OPEN'                                               
         BE    OPENIT                                                           
         CLC   CONACT(5),=C'CLOSE'                                              
         BE    CLOSEIT                                                          
         BAS   RE,VKEY             VALIDATE COMPANY, CLIENT, PRODUCT,           
         CLI   ACTNUM,ACTCHA       FOR ACTION=CHANGE, LOOK FOR PF KEYS          
         BNE   MODE20                                                           
         GOTOR PROCPF,DMCB,(RC)    FOR ACTION=ADD,LOOK FOR PF KEYS              
         GOTOR PROCPF8,DMCB,(RC)                                                
*                                                                               
MODE20   CLI   RECNUM,RECNXJOB                                                  
         BNE   MODE2A                                                           
         BAS   RE,VREC2            VALIDATE EXTRA DETAILS                       
         BAS   RE,DREC2            DISPLAY EXTRA DETAILS                        
         B     OKEXIT                                                           
*                                                                               
MODE2A   BAS   RE,VREC             JOB AND RECORD                               
         TM    GENSTAT6,GES$LINK   TEST DDLINK UPLOAD IN PROCESS                
         BO    MODEE0              DON'T DISPLAY RECORD, ADD ESTIMATES          
***      BO    OKEXIT              DON'T DISPLAY RECORD, DONE!                  
         BAS   RE,DREC             DISPLAY NEW RECORD                           
         CLI   RECNUM,RECNXJOB     XTRA JOB DISPLAY?                            
         BE    OKEXIT                                                           
*                                                                               
MODE2B   CLI   ACTNUM,ACTADD       IS THIS AN ADD ?                             
         BNE   MODE2C              NO, MUST BE CHANGE                           
*                                                                               
         CP    ELMCNT,=P'0'        ANY USER FIELDS ?                            
         BE    MODEE0              NO, ADD ESTIMATES                            
         CLI   VERUSER,1           YES, USER FIELDS BEEN VERIFIED?              
         BE    MODEE0              YES, ADD ESTIMATES                           
*                                                                               
         NI    JBRTMEH+4,X'FF'-X'20'   TURN OFF PREV VALIDATION                 
         NI    JBRAWOH+4,X'FF'-X'20'                                            
*                                                                               
         LA    R2,JBRUSEDH         SETUP CURSOR                                 
         MVI   MYMSGNO1,UPUSER                                                  
         B     INFEXIT                                                          
*                                                                               
MODE2C   CP    ELMCNT,=P'0'        ANY USER FIELDS ?                            
         BE    OKEXIT              NO, DONE                                     
         CLI   NEWDATE,0           YES, CLOSING DATE CHANGED ?                  
         BNE   MODE3               YES                                          
*                                                                               
         CLI   VERUSER,1           NO, USER FIELDS VERIFIED ?                   
         BE    OKEXIT              YES, ALL DONE                                
*                                                                               
MODE3    LA    R2,JBRUSEDH         SETUP CURSOR                                 
         ST    R2,ACURFORC                                                      
         MVI   MYMSGNO1,UPUSER     SETUP CURSOR AND MESSAGE                     
         B     INFEXIT                                                          
*                                                                               
MODE4    CLI   MODE,DISPKEY                                                     
         BNE   MODE6                                                            
         BAS   RE,DKEY             DISPLAY KEY                                  
         BAS   RE,VKEY             VALIDATE COMPANY, CLIENT, PRODUCT,           
         B     OKEXIT               AND JOB                                     
*                                                                               
MODE6    CLI   MODE,DISPREC                                                     
         BNE   MODEA0                                                           
*                                                                               
         CLC   CONACT(5),=C'CLOSE'                                              
         BE    CLOSEIT                                                          
         CLC   CONACT(4),=C'OPEN'                                               
         BE    OPENIT                                                           
         GOTOR PROCPF,DMCB,(RC)    FOR ACTION=ADD,LOOK FOR PF KEYS              
         GOTOR PROCPF8,DMCB,(RC)                                                
*                                                                               
         CLI   RECNUM,RECNXJOB                                                  
         BNE   MODE6A                                                           
         BAS   RE,DREC2            DISPLAY EXTRA DETAILS                        
         B     OKEXIT                                                           
MODE6A   BAS   RE,DREC             DISPLAY RECORD                               
         B     OKEXIT                                                           
*                                                                               
*                                                                               
MODEA0   CLI   MODE,XRECPUT        DID I PUT A RECORD?                          
         BNE   MODEA5                                                           
         GOTO1 =A(CHKNAME),DMCB,(RC),AIO,OLDJOBNM,RR=MYRELO                     
         B     MODEC0              YES                                          
MODEA5   CLI   MODE,XRECADD        NO                                           
         BE    MODEB0                                                           
         CLI   MODE,XRECDEL                                                     
         BE    MODEB0                                                           
         CLI   MODE,XRECREST                                                    
         BNE   OKEXIT                                                           
*                                                                               
MODEB0   GOTO1 SAVNAM,DMCB,OLDJOBNM  SAVE JOB NAME                              
MODEC0   GOTOR MANDRAP,DMCB,(RC)                                                
         CLI   MODE,XRECDEL                                                     
         BE    MODED0                                                           
         CLI   MODE,XRECREST                                                    
         BE    MODED0                                                           
         GOTOR MANJDTP,DMCB,(RC)                                                
MODED0   GOTOR MANPIDP,DMCB,(RC)                                                
         GOTOR ADDAPRV,DMCB,(RC)                                                
         GOTOR NAMESRCH,DMCB,(RC)                                               
*                                                                               
MODEE0   CLI   ACTNUM,ACTADD       IS THIS AN ADD ?                             
         BNE   OKEXIT              NO, SO DON'T ADD ESTIMATES                   
         CLI   GOESTTYP,C'B'       BRAND OCEAN TYPE ESTIMATES?                  
         BE    MODEE9              YES, SKIP ADD OF ESTIMATES                   
         GOTOR PREADD,DMCB,(RC)    ADD ESTIMATES                                
*                                                                               
MODEE9   GOTOR PROCPF8,DMCB,(RC)                                                
         B     OKEXIT                                                           
***********************************************************************         
* SAVE NAME OF RECORD                                                 *         
***********************************************************************         
SAVNAM   NTR1                                                                   
         L     R2,0(R1)                                                         
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   OKEXIT                                                           
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         BCTR  R1,0                                                             
         MVC   0(0,R2),0(R6)                                                    
         EX    R1,*-6                                                           
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                          DISPLAY KEY                                *         
***********************************************************************         
*                                                                               
DKEY     NTR1                                                                   
         MVC   AIO,AIO2            SWAP IO AREA                                 
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR                                                          
         MVC   JBRCLACH+5(1),LCLI                                               
         OI    JBRCLACH+6,X'80'                                                 
         MVC   JBRPRACH+5(1),LPRO                                               
         OI    JBRPRACH+6,X'80'                                                 
         MVC   JBRSJACH+5(1),LJOB                                               
         OI    JBRSJACH+6,X'80'                                                 
         MVC   AIO,AIO1            SWAP IT BACK                                 
         L     R6,AIO                                                           
         LA    R6,3(R6)            BREAK KEY DOWN INTO CLIENT,                  
         SR    R1,R1                PRODUCT AND JOB                             
         IC    R1,LCLI                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   JBRCLAC(0),0(R6)                                                 
         LA    R6,1(R1,R6)                                                      
         IC    R1,LPRO                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   JBRPRAC(0),0(R6)                                                 
         LA    R6,1(R1,R6)                                                      
         IC    R1,LJOB                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     OKEXIT                                                           
         MVC   JBRSJAC(0),0(R6)                                                 
         EJECT                                                                  
***********************************************************************         
*                           VALIDATE KEY                              *         
***********************************************************************         
*                                                                               
VKEY     NTR1                                                                   
         MVI   KEYCHG,C'N'                                                      
         MVC   AIO,AIO2            SWAP IO AREA                                 
         MVC   KEY,BLANKS                                                       
         MVC   KEY(1),CUL                                                       
         GOTO1 READ                                                             
         MVI   ELCODE,ACMPELQ                                                   
         BAS   RE,GETELIO                                                       
         USING ACCOMPD,R6                                                       
         MVC   PRODLEDG,ACMPJOB    GET LEDGERS AND STATUS                       
         MVC   RECVLEDG,ACMPRECV                                                
*                                                                               
         LA    R2,JBRCLACH         ADDRESS CLIENT                               
         TM    4(R2),X'20'         WAS THIS VALIDATED ALREADY ?                 
         BO    VKEY03              YES                                          
         LA    R2,JBRSJACH         ADDRESS JOB                                  
         NI    4(R2),X'FF'-X'20'   TURN OFF PREVIOUSLY VALID BIT                
         LA    R2,JBRCLACH         RE-ADDRESS CLIENT                            
                                                                                
VKEY03   BAS   RE,TSTKEY                                                        
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'                                                      
         MVI   OPTION,C'Y'         TURN OPTION ON TO GET NAME                   
         GOTO1 VALCLI              VALIDATE CLIENT, MOVE NAME,                  
         MVC   JBRCLOF,CLIOFFC                                                  
         OI    JBRCLOFH+6,X'80'                                                 
*                                                                               
         LA    R2,JBRCLF1H                                                      
         LA    R0,NFLTS                                                         
         LA    R1,CLFLTS                                                        
*                                                                               
VKEY02   MVC   8(1,R2),0(R1)                                                    
         OI    6(R2),X'80'                                                      
         LA    R1,1(R1)                                                         
         BAS   RE,BUMP                                                          
         BCT   R0,VKEY02                                                        
*                                                                               
         LA    R2,JBRPRACH         ADDRESS PRODUCT                              
         TM    4(R2),X'20'         WAS THIS VALIDATED ALREADY ?                 
         BO    VKEY05              YES                                          
         LA    R2,JBRSJACH         ADDRESS JOB                                  
         NI    4(R2),X'FF'-X'20'   TURN OFF PREVIOUSLY VALID BIT                
         LA    R2,JBRPRACH         RE-ADDRESS PRODUCT                           
                                                                                
VKEY05   BAS   RE,TSTKEY                                                        
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'                                                      
         GOTO1 VALPROD             VALIDATE PRODUCT, MOVE NAME,                 
         MVC   JBRPROF,PRODOFFC                                                 
         OI    JBRPROFH+6,X'80'                                                 
                                                                                
         LA    R2,JBRPRF1H                                                      
         LA    R0,NFLTS                                                         
         LA    R1,PRFLTS                                                        
*                                                                               
VKEY04   MVC   8(1,R2),0(R1)                                                    
         OI    6(R2),X'80'                                                      
         LA    R1,1(R1)                                                         
         BAS   RE,BUMP                                                          
         BCT   R0,VKEY04                                                        
*                                                                               
         MVC   SAVEOFC,EFFOFFC     SAVE EFFECTIVE OFFICE FOR LABELS             
*                                                                               
         CLI   RECNUM,RECNXJOB                                                  
         BE    VKEY06                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,TODAY)                                      
         XC    OPEN,OPEN           SET OPEN DATE, IF ENTERED                    
         BAS   RE,VOPEN                                                         
*                                                                               
VKEY06   LA    R2,JBRSJACH         ADDRESS JOB                                  
         BAS   RE,TSTKEY                                                        
         MVI   ERROR,INVALID                                                    
         SR    R1,R1                                                            
         ICM   R1,1,JBRSJACH+5                                                  
         CLC   CONACT(4),=C'AUTO'  IS THIS AUTO NUMBERING ?                     
         BE    *+12                YES, SKIP THIS TEST                          
         CHI   R1,1                                                             
         BNH   ERREXIT                                                          
         CLI   8(R2),C' '          DOES JOB START WITH A BLANK ?                
         BE    ERREXIT             YES, THIS IS AN ERROR                        
         OI    6(R2),X'80'                                                      
         GOTO1 VALMED                                                           
*                                                                               
         USING ACMEDIAD,R6                                                      
         MVI   ELCODE,ACMDELQ      SAVE MEDIA STATUS BYTE                       
         BAS   RE,GETELIO                                                       
         MVC   SVMEDSTA,ACMDSTAT                                                
*                                                                               
         GOTOR SETGET,DMCB,(RC)    SETUP FOR GETOPT                             
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
*                                                                               
         TM    4(R2),X'20'         WAS THIS VALIDATED ALREADY ?                 
         BO    VKEY08              YES                                          
         CLC   CONACT(4),=C'AUTO'  IS THIS AUTO NUMBERING ?                     
         BNE   VKEY08              NO, NEED MORE  THAN 1 DIGIT                  
         LA    R2,CONACTH                                                       
         MVI   ERROR,NOTAUTO                                                    
         CLI   GOAUTNUM,C'Y'                                                    
         BNE   ERREXIT                                                          
*                                                                               
         MVI   ERROR,NOPROD                                                     
         CLI   GOPROD,C'N'         TEST NO PRODUCTION JOBS                      
         BE    ERREXIT                                                          
*                                                                               
         MVC   EFFDATE,OPEN        SET DATE FOR 'FINDJOB'                       
         OC    EFFDATE,EFFDATE                                                  
         BNZ   *+10                                                             
         MVC   EFFDATE,TODAYP                                                   
         GOTO1 DATCON,DMCB,(1,EFFDATE),(0,JDATE)                                
*                                                                               
         TM    GENSTAT6,GES$LINK   TEST DDLINK UPLOAD IN PROCESS                
         BNO   *+12                                                             
         CLI   MODE,VALREC         IF DDLINK UPLOAD AND MODE VALREC             
         BE    VKEY08                                                           
         LA    R2,JBRSJACH                                                      
         GOTOR FINDJOB,DMCB,(RC)                                                
         MVC   8(L'JBRSJAC,R2),WORK                                             
         MVI   5(R2),L'JBRSJAC                                                  
*                                                                               
VKEY08   OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'                                                      
         MVC   KEY,BLANKS          PREPARE KEY FOR GENCON                       
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(6),CLICODE                                                 
         SR    R1,R1                                                            
         IC    R1,LCLI                                                          
         LA    R1,KEY+3(R1)                                                     
         MVC   0(6,R1),PRODCODE                                                 
         SR    R1,R1                                                            
         IC    R1,LCLIPRO                                                       
         LA    R1,KEY+3(R1)                                                     
         MVC   0(6,R1),WORK                                                     
         MVC   JOBNUM,WORK                                                      
*                                                                               
         CLC   SAVEKEY,KEY                                                      
         MVC   SAVEKEY,KEY                                                      
         BNE   VKEY10                                                           
         CLI   CALLER,X'00'        DID SOMEONE CALL ME ?                        
         BE    VKEY12              NO                                           
*                                                                               
VKEY10   BAS   RE,CLEARU           YES, CLEAR USER FIELDS                       
         XC    SVSTART,SVSTART                                                  
*                                                                               
VKEY12   MVC   AIO,AIO1            SWAP IO BACK                                 
         CLI   ACTNUM,ACTADD       TEST FOR ACTION=ADD                          
         BNE   VKEYX               NO                                           
*                                                                               
         CLI   GOPROD,C'N'         TEST NO PRODUCTION JOBS                      
         BNE   VKEYX               NO-OK TO ADD THEM                            
         MVI   ERROR,NOPROD                                                     
         LA    R2,CONRECH                                                       
         ST    R2,ACURFORC                                                      
         B     ERREXIT                                                          
*                                                                               
VKEYX    CLI   KEYCHG,C'Y'         DID ANYTHING CHANGE ?                        
         BNE   OKEXIT              NO, EXIT                                     
         TM    GENSTAT6,GES$LINK   TEST DDLINK UPLOAD IN PROCESS                
         BNO   *+12                                                             
         CLI   MODE,VALREC         IF DDLINK UPLOAD AND MODE VALREC             
         BE    OKEXIT              THEN EXIT, AND DON'T RELOAD BUFFER           
         CLI   RECNUM,RECNXJOB                                                  
         BE    OKEXIT                                                           
         GOTOR LOAD,DMCB,(RC)      YES, RELOAD BUFFER                           
         GOTOR PUTTAB,DMCB,(RC)                                                 
         B     OKEXIT                                                           
*                                                                               
TSTKEY   TM    4(R2),X'20'         HAS FIELD CHANGED ?                          
         BOR   RE                  NO                                           
         MVI   KEYCHG,C'Y'         YES, INDICATE SO                             
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                        DISPLAY RECORD                               *         
***********************************************************************         
*                                                                               
DREC     NTR1                                                                   
         TWAXC JBRSJNMH                                                         
         MVC   CUL+1(2),=C'SJ'     RESET IN CASE LOST                           
         GOTOR SETGET,DMCB,(RC)    SETUP FOR GETOPT                             
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
*                                                                               
         USING JFNELD,R6                                                        
         XC    SVJFUND,SVJFUND     CLEAR JOB FUNDING SAVE AREA                  
         LA    R3,REGPFS           GET PFKEYS READY                             
         MVI   ELCODE,JFNELQ                                                    
         BAS   RE,GETELIO          DO WE HAVE A JOB FUNDING ELEMENT?            
         BNE   DPFKEY              NO                                           
         MVC   SVJFUND,JFNKEY      YES, SAVE KEY OF RECORD                      
         LA    R3,AUTPFS           AND CHANGE PFKEYS                            
*                                                                               
DPFKEY   LA    R2,JBRTAGH          R2=A(START OF PFKEYS)                        
         MVI   ELCODE,1                                                         
         SR    R1,R1                                                            
         LR    R6,R3               SWAP REGISTER FOR NEXTEL                     
*                                                                               
         USING TWAELEMD,R6                                                      
DPFK02   IC    R1,TWAEFLN          MOVE PFKEYS TO SCREEN                        
         STC   R1,5(R2)            STORE FIELD LENGTH                           
         BCTR  R1,0                MOVE FIELD DATA                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),TWAEDTA                                                  
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         OI    1(R2),X'08'         HIGH INTENSITY                               
         BAS   RE,BUMP                                                          
         BAS   RE,NEXTEL                                                        
         BE    DPFK02                                                           
*                                                                               
DISSJ    MVC   THISLEDG,PRODLEDG                                                
         LA    R2,JBRSJNMH                                                      
         BAS   RE,DISFILT          FILTERS AND STATUS                           
         GOTO1 NAMEOUT                                                          
         OI    JBRSJNMH+4,X'20'    INDICATE FIELD VALIDATED                     
         MVI   JBRSRACH+5,0        CLEAR RECEIVABLE AND COSTING                 
         MVI   JBR1CACH+5,0         ACCOUNT LENGTHS                             
         MVI   ELCODE,ACPRELQ      GET PROFILE ELEMENT                          
         BAS   RE,GETELIO                                                       
         BNE   DISLAST                                                          
*                                                                               
         USING ACPROFD,R6                                                       
DISSJOF  MVC   JBRSJOF,ACPROFFC    DISPLAY OFFICE                               
         OI    JBRSJOFH+6,X'80'    TRANSMIT THIS PROTECTED FIELD                
*                                                                               
DGETSRAC CLI   ACPRRECV,C' '       IF NO RECEIVABLE ACCOUNT -                   
         BE    DGET1CAC             LOOK FOR COSTING                            
         CLI   ACPRRECV,0                                                       
         BE    DGET1CAC                                                         
         MVC   SAVESRLG,ACPRRECV+1 SAVE THE LEDGER                              
         MVC   JBRSRAC,ACPRRECV+3  MOVE KEY TO SCREEN                           
         MVI   JBRSRACH+5,12       SET UP LENGTH FOR VALACCT                    
*                                                                               
DGET1CAC OI    JBRSRACH+4,X'20'                                                 
         CLI   ACPRCOST,C' '       IF NO COSTING ACCOUNT -                      
         BE    DISPBIL              DISPLAY DATA TO PRINT ON BILL               
         CLI   ACPRCOST,0                                                       
         BE    DISPBIL                                                          
         MVC   SAVE1CLG,ACPRCOST+1                                              
         MVC   JBR1CAC,ACPRCOST+3                                               
         MVI   JBR1CACH+5,12       SET LENGTH FOR VALACCT                       
*                                                                               
DISPBIL  OI    JBR1CACH+4,X'20'                                                 
         MVC   JBRPBIL,ACPRBLPR    GET DATA TO PRINT ON BILL                    
*                                                                               
DISINFO  CLI   ACPRLEN,105         GET ADDITIONAL INFORMATION                   
         BE    DISLAST                                                          
         MVC   JBRHED1,ACPRNARR                                                 
         CLI   ACPRLEN,155                                                      
         BE    DISLAST                                                          
         MVC   JBRHED2,ACPRNARR+50                                              
         CLI   ACPRLEN,205                                                      
         BE    DISLAST                                                          
         MVC   JBRHED3,ACPRNARR+100                                             
*                                                                               
DISLAST  GOTO1 PERSOUT             DATE OF LAST ACTIVITY                        
         LA    R2,JBRLACTH                                                      
         MVC   8(20,R2),WORK+20                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
DISCDATE GOTOR DISCLOSE,DMCB,(RC)                                               
         GOTOR DISOPEN,DMCB,(RC)                                                
*                                                                               
DISJOBA  L     R6,AIO                                                           
         MVI   JBRJOBA,C'Y'                                                     
         TM    ACTRSTAT-ACTRECD(R6),ACTSDRFT                                    
         BZ    *+8                                                              
         MVI   JBRJOBA,C'N'                                                     
         OI    JBRJOBAH+6,X'80'                                                 
*                                                                               
         USING JOBELD,R6                                                        
DISCYCLE MVI   ELCODE,JOBELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   DISFOOT                                                          
         CLI   JOBLN,JOBLN3Q                                                    
         BL    DISTYPE                                                          
         LA    R2,JBRNCYCH                                                      
         LA    R0,3                                                             
         TM    JOBBIST,X'40'                                                    
         BO    DISC020                                                          
         BCTR  R0,0                                                             
         TM    JOBBIST,X'80'                                                    
         BO    DISC020                                                          
         BCTR  R0,0                                                             
         CLI   JOBBIST,X'00'                                                    
         BNE   DISTYPE                                                          
*                                                                               
DISC020  CVD   R0,DUB                                                           
         UNPK  8(1,R2),DUB+7(1)                                                 
         OI    8(R2),X'F0'                                                      
*                                                                               
DISTYPE  MVI   JBRTYP,C'N'         IF BIT ON, TYPE IS NEW                       
         TM    JOBSTA1,JOBSNEST                                                 
         BNZ   DISADJT                                                          
         MVI   JBRTYP,C'B'         IF BIT ON, TYPE IS BRAND OCEAN               
         TM    JOBSTA1,JOBSMCSE                                                 
         BNZ   DISADJT                                                          
         MVI   JBRTYP,C'O'         SET UP FOR OLD TYPE                          
*                                                                               
DISADJT  OI    JBRTYPH+4,X'20'                                                  
         MVI   JBRRAT,C'N'         SETUP FOR NO ADJUSTMENT RATE                 
         TM    JOBSTA1,JOBSART                                                  
         BZ    DISAWO                                                           
         MVI   JBRRAT,C'Y'         IF BIT ON, ADJUSTMENT IS YES                 
*                                                                               
DISAWO   OI    JBRRATH+4,X'20'                                                  
         CLI   EMULATE,C'Y'        IS THIS AN EMULATED FILE ?                   
         BE    *+8                 YES                                          
         OI    JBRAWOH+1,X'20'     NO, DON'T ALLOW AUTO WRITEOFF ENTRY          
         MVI   JBRAWO,C'N'         SETUO FOR NOT AUTO WRITEOFF                  
         TM    JOBSTA1,JOBSXJOB                                                 
         BZ    DISTMED                                                          
         MVI   JBRAWO,C'Y'         IF BIT ON, AUTO WRITEOFF IS YES              
*                                                                               
DISTMED  OI    JBRAWOH+4,X'20'                                                  
         MVI   JBRTME,C' '         TALENT MEDIA                                 
*                                                                               
         TM    JOBSTA1,JOBSTV                                                   
         BNO   *+8                                                              
         MVI   JBRTME,C'T'         TALENT MEDIA IS TV                           
         TM    JOBSTA1,JOBSRAD                                                  
         BZ    DISSTUD                                                          
         MVI   JBRTME,C'R'         TALENT MEDIA IS RADIO                        
*                                                                               
DISSTUD  OI    JBRTMEH+4,X'20'                                                  
*                                                                               
         USING LNKELD,R6                                                        
         MVI   ELCODE,LNKELQ                                                    
         BAS   RE,GETELIO          DO WE HAVE A LINK ELEMENT?                   
         BNE   DISFOOT             NO                                           
         MVC   JBRSTU,LNKSTUD      YES, DISPLAY STUDIO                          
*                                                                               
         USING ACOMMD,R6                                                        
DISFOOT  OI    JBRSTUH+4,X'20'                                                  
         MVI   ELCODE,X'3E'                                                     
         BAS   RE,GETELIO                                                       
         BNE   DISDATA                                                          
         LA    R2,JBRFT1H                                                       
         LA    R0,3                                                             
         MVI   DUB,C'N'            N=NORMAL COMMENTS ON THIS LINE               
         MVI   DUB+1,1             NOTHING YET IN LINE                          
         LA    R3,8(R2)            ADDRESS FIRST BYTE IN LINE                   
*                                                                               
DISF020  CLI   ACOMTYPE,C'M'                                                    
         BE    *+12                                                             
         CLI   ACOMTYPE,0                                                       
         BNE   DISF060                                                          
         CLI   DUB,C'N'                                                         
         BE    DISF040                                                          
         BAS   RE,BUMPFT           GET NEXT LINE                                
         MVI   DUB,C'N'                                                         
*                                                                               
DISF040  SR    R1,R1                                                            
         IC    R1,ACOMLEN                                                       
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),ACOMMENT                                                 
         BAS   RE,BUMPFT                                                        
         B     DISF120                                                          
*                                                                               
DISF060  LA    RE,ACOMMENT                                                      
         LA    R1,6                                                             
*                                                                               
DISF080  CLI   0(RE),C' '                                                       
         BNE   DISF100                                                          
         LA    RE,1(RE)                                                         
         BCT   R1,DISF080                                                       
         B     DISF120                                                          
*                                                                               
DISF100  MVI   DUB,C'Y'            MARK AS NEW STYLE COMMENTS                   
         STC   R1,DUB+2            R1 = L'COMMENT                               
         ST    RE,DUB+4            RE = A(1ST SIGNIFICANT BYTE)                 
         AHI   R1,5                R1 = TOTAL LENGTH DISPLAYED ITEM             
         AR    R1,R3               R1 = A(LAST BYTE + 1 OF DSPLYD ITEM)         
         LR    RE,R2               RE = A(HEADER)                               
         SR    R1,RE                                                            
         CHI   R1,58                                                            
         BNH   *+8                 IF NOT SUFFICIENT ROOM ON THIS LINE          
         BAS   RE,BUMPFT            BUMP TO NEXT                                
         CLI   DUB+1,1                                                          
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         MVI   DUB+1,2                                                          
         MVC   0(4,R3),=C'EST='    SET TYPE OF COMMENT                          
         TM    ACOMTYPE,X'40'                                                   
         BO    *+10                                                             
         MVC   0(3,R3),=C'BIL'                                                  
         TM    ACOMTYPE,X'C0'                                                   
         BM    *+10                                                             
         MVC   0(3,R3),=C'B+E'                                                  
         L     RE,DUB+4                                                         
         SR    R1,R1                                                            
         IC    R1,DUB+2                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R3),0(RE)      DISPLAY NUMBER                                
         LA    R3,5(R1,R3)                                                      
*                                                                               
DISF120  SR    R1,R1              SEE IF MORE ELEMENTS                          
         IC    R1,ACOMLEN                                                       
         AR    R6,R1                                                            
         CLI   ACOMEL,0                                                         
         BE    DISDATA                                                          
         CLI   ACOMEL,X'3E'                                                     
         BE    DISF020                                                          
         B     DISF120                                                          
*                                                                               
BUMPFT   IC    R1,0(R2)           BUMP TO NEXT LINE                             
         AR    R2,R1                                                            
         LA    R3,8(R2)                                                         
*                                                                               
         TM    1(R2),X'20'         PROTECTED                                    
         BNO   BUMPFT20            YES, SKIP TWO                                
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         LA    R3,8(R2)                                                         
         B     BUMPFT                                                           
*                                                                               
BUMPFT20 MVI   DUB+1,1                                                          
         BCT   R0,*+8                                                           
         B     *+6                                                              
         BR    RE                                                               
*                                                                               
DISDATA  BAS   RE,CLEARU                                                        
         BAS   RE,DISUSER          DISPLAY USER HEADERS                         
         MVC   KEY,SAVEKEY                                                      
         BAS   RE,USERDATA          AND USER SELECT DATA                        
         GOTOR PUTTAB,DMCB,(RC)    SAVE ELEMENT TABLE                           
*                                                                               
DSWAP2   MVC   AIO,AIO2            USE AIO2 FROM NOW UNTIL XIT                  
*                                                                               
DISSRAC  CLI   JBRSRACH+5,0                                                     
         BE    DIS1CAC                                                          
         MVC   THISLEDG,SAVESRLG                                                
         LA    R2,JBRSRACH         VALIDATE ACCOUNT, DISPLAY NAME,              
         BAS   RE,VALACCT           OFFICE, FILTERS AND SUB-DEPT                
         LA    R2,JBRSRNMH                                                      
         BAS   RE,DISFILT                                                       
         GOTO1 NAMEOUT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
DIS1CAC  CLI   JBR1CACH+5,0                                                     
         BE    DSWAP1                                                           
         MVC   THISLEDG,SAVE1CLG                                                
         LA    R2,JBR1CACH         VALIDATE ACCOUNT, DISPLAY NAME,              
         BAS   RE,VALACCT           OFFICE, FILTERS AND SUB-DEPT                
         LA    R2,JBR1CNMH                                                      
         BAS   RE,DISFILT                                                       
         GOTO1 NAMEOUT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
DSWAP1   MVC   AIO,AIO1            USE AIO1 NOW                                 
         MVC   CUL+1(2),=C'SJ'     RESET IN CASE LOST                           
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                        DISPLAY EXTRA DETAILS                        *         
***********************************************************************         
*                                                                               
DREC2    NTR1                                                                   
         TWAXC JBXSJNMH                                                         
         MVC   THISLEDG,PRODLEDG                                                
         LA    R2,JBXSJNMH                                                      
         BAS   RE,DISFIL2          GET FILTERS                                  
         GOTO1 NAMEOUT             DISPLAY JOB NAME                             
         OI    4(R2),X'20'                                                      
*                                                                               
         MVI   ELCODE,ACPRELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   DISNAME                                                          
         USING ACPROFD,R6                                                       
         MVC   JBXSJOF,ACPROFFC    DISPLAY OFFICE                               
         OI    JBXSJOFH+6,X'80'                                                 
*                                                                               
DISNAME  GOTO1 PERSOU2             DATE OF LAST ACTIVITY                        
         LA    R2,JBXLACTH                                                      
         MVC   8(20,R2),WORK+20                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
DISADDR  LA    R2,JBXADR1H         GET BILLING ADDRESS                          
         GOTO1 ADDROUT                                                          
*                                                                               
DISEQU   TWAXC JBXLAB1H,JBXEQU5H   CLEAR ALL ACCOUNT EQUIVALENCIES              
         GOTO1 =A(GETEQU),DMCB,(RC),RR=MYRELO                                   
*                                                                               
DISLOCK  MVC   AIO,AIO1            RESTORE IN CASE SWAPPED                      
         MVI   ELCODE,RSTELQ       GET STATUS ELEMENT                           
         BAS   RE,GETELIO                                                       
         BNE   DISACCM                                                          
         USING RSTELD,R6                                                        
         MVI   JBXLEST,C'N'                                                     
         TM    RSTLSTAT,RSTLSESQ   LOCKED FROM ESTIMATES?                       
         BNO   *+8                 NO                                           
         MVI   JBXLEST,C'Y'                                                     
*                                                                               
         MVI   JBXLORD,C'N'                                                     
         TM    RSTLSTAT,RSTLSORQ   LOCKED FROM ORDERS?                          
         BNO   *+8                 NO                                           
         MVI   JBXLORD,C'Y'                                                     
*                                                                               
         MVI   JBXLTSS,C'N'                                                     
         TM    RSTLSTAT,RSTLSTIQ   LOCKED FROM TIMESHEETS?                      
         BNO   *+8                 NO                                           
         MVI   JBXLTSS,C'Y'                                                     
*&&DO                                                                           
         MVI   JBXLBIL,C'N'                                                     
         TM    RSTLSTAT,RSTLSBIQ   LOCKED FROM BILLINGS?                        
         BNO   *+8                 NO                                           
         MVI   JBXLBIL,C'Y'                                                     
*                                                                               
         MVI   JBXLADJ,C'N'                                                     
         TM    RSTLSTAT,RSTLSADQ   LOCKED FROM ADJUSTMENTS?                     
         BNO   *+8                 NO                                           
         MVI   JBXLADJ,C'Y'                                                     
*                                                                               
         MVI   JBXLEXT,C'N'                                                     
         TM    RSTLSTAT,RSTLSEXQ   LOCKED FROM EXTERNAL POSTINGS?               
         BNO   *+8                 NO                                           
         MVI   JBXLEXT,C'Y'                                                     
*&&                                                                             
DISACCM  MVC   AIO,AIO1            RESTORE IN CASE SWAPPED                      
         MVI   ELCODE,OMEELQ       ACCOUNT MEMO                                 
         BAS   RE,GETELIO                                                       
         BNE   DREC2XIT                                                         
         USING OMEELD,R6                                                        
         ZIC   RF,OMELN            ON-LINE MEMO DATA?                           
         AHI   RF,-3                                                            
         CHI   RF,L'JBXACM-1       CHECK IF WE EXCEEDED FIELD LENGTH            
         BL    *+8                                                              
         LHI   RF,L'JBXACM-1       SET TO MAX FIELD LENGTH                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   JBXACM(0),OMEMO                                                  
         OC    JBXACM,BLANKS                                                    
         OI    JBXACMH+6,X'80'                                                  
*                                                                               
DREC2XIT B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
*                                                                               
VREC     NTR1                                                                   
         MVI   NEWDATE,0           CLEAR NEW DATE SWITCH                        
         MVI   VERUSER,0           CLEAR USER FIELD VERIFIED SWITCH             
         XC    SVDATES,SVDATES                                                  
*                                                                               
         LA    R2,JBRSTATH                                                      
         CLI   8(R2),C'C'          IS THE STATUS CLOSE?                         
         BNE   VREC02              NO                                           
         MVI   ERROR,INVALID       YES, DID THEY ENTER IT?                      
         TM    4(R2),X'80'                                                      
         BO    ERREXIT             YES, ERROR                                   
         CLI   ACTNUM,ACTCHA       NO, ARE WE IN ACTION CHANGE?                 
         BNE   VREC02              NO                                           
         CLC   CONACT(5),=C'CLOSE' YES, IS IT A CLOSE?                          
         BE    VREC02              YES                                          
         MVI   ERROR,CLOSERR       NO, MUST BE CHANGE THEN                      
         LA    R2,CONACTH                                                       
         B     ERREXIT                                                          
*                                                                               
VREC02   LA    R2,JBRSJACH                                                      
         MVC   THISLEDG,PRODLEDG                                                
         CLI   ACTNUM,ACTADD                                                    
         BNE   VALSJ                                                            
         USING ACKEYD,R6           AIO HAS RECORD KEY ALREADY, JUST             
         L     R6,AIO               INSERT LENGTH AND ELEMENTS                  
         MVC   ACLENGTH,=H'50'                                                  
*                                                                               
         TM    GENSTAT6,GES$LINK   IF DDLINK UPLOAD IN PROCESS                  
         BNO   *+10                                                             
         MVC   0(42,R6),SAVEKEY    THEN, NEED TO MOVE KEY IN AIO THEN           
*                                                                               
         BAS   RE,BALPEEL                                                       
*                                                                               
VALSJ    LA    R2,JBRSJNMH                                                      
         GOTO1 ANY                                                              
*                                                                               
         CLI   ACTNUM,ACTCHA                                                    
         BNE   VSJ010                                                           
         GOTO1 =A(SVNAME),DMCB,(RC),OLDJOBNM,RR=MYRELO                          
*                                                                               
VSJ010   BAS   RE,VALNAME                                                       
         BAS   RE,VALSTAT                                                       
*                                                                               
         USING ACPROFD,R6                                                       
VSJ020   MVI   ELCODE,ACPRELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    VGETSRAC                                                         
         BAS   RE,ADDPROF                                                       
         B     VSJ020                                                           
*                                                                               
VGETSRAC SR    R1,R1                                                            
         ICM   R1,1,JBRSRACH+5                                                  
         BNZ   VGSR020                                                          
         XC    ACPRRECV,ACPRRECV   IF NO RECEIVABLE, CLEAR SCREEN               
         XC    JBRSRNM,JBRSRNM                                                  
         LA    R0,5                                                             
         LA    R2,JBRSRF1H                                                      
VGSR010  MVI   8(R2),0                                                          
         BAS   RE,BUMP                                                          
         BCT   R0,VGSR010                                                       
         B     VGET1CAC                                                         
*                                                                               
VGSR020  BCTR  R1,0                                                             
         MVC   ACPRRECV,BLANKS                                                  
         MVC   ACPRRECV(1),CUL                                                  
         MVC   ACPRRECV+1(2),RECVLEDG                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACPRRECV+3(0),JBRSRAC MOVE IN RECEIVABLE ACCOUNT CODE            
*                                                                               
VGET1CAC SR    R1,R1                                                            
         ICM   R1,1,JBR1CACH+5                                                  
         BNZ   VG1C020                                                          
         XC    ACPRCOST,ACPRCOST                                                
         XC    JBR1CNM,JBR1CNM                                                  
         LA    R0,5                                                             
         LA    R2,JBR1CF1H                                                      
VG1C010  MVI   8(R2),0                                                          
         BAS   RE,BUMP                                                          
         BCT   R0,VG1C010                                                       
         B     VALPBIL                                                          
*                                                                               
VG1C020  MVC   ACPRCOST,BLANKS                                                  
         MVC   ACPRCOST(1),CUL                                                  
         MVC   ACPRCOST+1(2),=C'1C'                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACPRCOST+3(0),JBR1CAC MOVE IN COSTING ACCOUNT CODE.              
*                                                                               
VALPBIL  MVC   ACPRBLPR,BLANKS                                                  
         SR    R1,R1                                                            
         ICM   R1,1,JBRPBILH+5                                                  
         BZ    VALINFO                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACPRBLPR(0),JBRPBIL                                              
*                                                                               
VALINFO  MVI   ELEMENT+105,C' '    CLEAR NARRATIVE PORTION OF NEW LMNT.         
         MVC   ELEMENT+106(149),ELEMENT+105                                     
         MVC   ELEMENT(105),ACPREL PRESERVE START OF OLD ELEMENT.               
         LA    R6,ELEMENT                                                       
         GOTO1 REMELEM                                                          
         MVC   ACPRNARR,BLANKS                                                  
         LA    RF,ACPRNARR         RF = A(START OF NARRATIVE).                  
         LA    R3,HEDTAB           R3=A(HEAD COMMENT DISPLACEMENTS)             
         LA    R0,HEDTABN          R0 = N'INFO LINES POSSIBLE.                  
*                                                                               
VINF040  LH    R2,0(R3)            GET DISP TO FIELD                            
         LA    R2,CONTAGH(R2)      COMPUTE ACTUAL ADDRESS                       
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)            R1 = L'THIS LINE.                          
         BZ    VINF060                                                          
         BCTR  R1,0                                                             
         EX    R1,VINFMVC          MVC   0(0,R6),8(R2)                          
         LA    RF,50(RF)           RF = A(NEXT LYN POST'N IN EL).               
         LA    R3,L'HEDTAB(R3)                                                  
         BCT   R0,VINF040                                                       
*                                                                               
VINF060  LA    R2,HEDTABN          DERIVE N'LINES I/P.                          
         SR    R2,R0               TOTAL L'INPUT IS N'LINES * 50.               
         MH    R2,=H'50'                                                        
         LA    R2,ACPRNARR-ACPROFD(R2) COMPUTE EL LEN                           
         STC   R2,ACPRLEN                                                       
         LA    R2,JBRHED1H                                                      
         GOTO1 ADDELEM              ADD THE ELEMENT.                            
         B     *+10                                                             
*                                                                               
VINFMVC  MVC   0(0,RF),8(R2)                                                    
         DROP  R6                                                               
*                                                                               
VALLAST  GOTO1 PERSIN                                                           
         CLI   ACTNUM,ACTADD      IF THIS IS A NEW RECORD ADD                   
         BNE   VALEST             RACEL WITH CREATION DETAILS                   
         XC    ELEMENT,ELEMENT                                                  
         USING RACELD,R6                                                        
         MVI   RACEL,RACELQ                                                     
         MVI   RACLN,RACLNQ                                                     
         MVI   RACTYPE,RACTADD                                                  
         GOTO1 GETFACT,DMCB,0,0                                                 
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   RACUSER,TWAORIG                                                  
         MVC   RACPERS,FAPASSWD                                                 
         MVC   SVPASSWD,FAPASSWD                                                
         MVC   RACDATE,TODAYP                                                   
         MVC   RACTIME,FATIME                                                   
         DROP  R1                                                               
         MVC   DMCB(4),=X'FFFFFFFF'                                             
         GOTO1 SWITCH,DMCB                                                      
         L     R1,0(R1)                                                         
         MVC   RACTERM,TCFN-UTLD(R1)                                            
         MVI   ELCODE,RACELQ                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
         USING JOBELD,R6                                                        
VALEST   MVI   ELCODE,JOBELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    VALDATES                                                         
         CLI   ACTNUM,ACTADD        ELEMENT MUST BE THERE UNLESS ADD            
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,ELEMENT          SETUP TO ADD NEW ELEMENT                     
         XC    ELEMENT,ELEMENT                                                  
         MVI   JOBEL,JOBELQ                                                     
         MVC   JOBADATE,TODAYP                                                  
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
*                                                                               
VALDATES GOTO1 DATCON,DMCB,(1,JOBCDATE),(2,SVDTINCL)                            
         CLI   JOBLN,JOBLN2Q                                                    
         BL    VALTYPE                                                          
         GOTO1 DATCON,DMCB,(1,JOBODATE),(2,SVDTINOP)                            
*                                                                               
VALCYCLE LA    R2,JBRNCYCH                                                      
         CLI   5(R2),0                                                          
         BE    VALTYPE                                                          
         CLI   JOBLN,JOBLN3Q       DOES NOT HAVE CYCLE BILLING                  
         BL    VALTYPE                                                          
         MVI   ERROR,INVALID       CYCLE MUST BE BLANK,1, 2 OR 3                
         MVI   JOBBIST,X'C0'                                                    
         CLI   8(R2),C'3'                                                       
         BE    VALTYPE                                                          
         MVI   JOBBIST,X'80'                                                    
         CLI   8(R2),C'2'                                                       
         BE    VALTYPE                                                          
         MVI   JOBBIST,X'00'                                                    
         CLI   8(R2),C'1'                                                       
         BNE   ERREXIT                                                          
*                                                                               
VALTYPE  CLI   ACTNUM,ACTADD       ARE WE ADDING ?                              
         BNE   VALTY010            NO                                           
         CLI   GOESTTYP,C'O'       YES, OLD TYPE ESTIMATES ?                    
         BE    VALRATE             YES, LEAVE FIELD AS BINARY ZERO              
         OI    JOBSTA1,JOBSNEST    NO, CAN BE NEW                               
         CLI   GOESTTYP,C'B'       SET NEW BRAND OCEAN TYPE?                    
         BNE   VALRATE                                                          
         NI    JOBSTA1,X'FF'-JOBSNEST                                           
         OI    JOBSTA1,JOBSMCSE                                                 
         B     VALRATE                                                          
*                                                                               
VALTY010 LA    R2,JBRTYPH                                                       
         CLI   5(R2),0                                                          
         BE    VALRATE             NOTHING TO CHANGE                            
         TM    4(R2),X'20'                                                      
         BO    VALRATE                                                          
*                                                                               
         TM    JOBSTA1,JOBSNEST    NEW ESTIMATE ?                               
         BZ    VALTY020            NO, SEE IF OLD                               
         CLI   8(R2),C'N'          YES, IS THAT WHAT WAS ENTERED ?              
         BE    VALRATE             YES, NO CHANGE NEEDED                        
         CLI   8(R2),C'B'          CHANGING TO BRAND OCEAN?                     
         BNE   VALRATE             NO                                           
         GOTOR CHKNEW,DMCB,(RC)    CHECK FOR NEW ESTIMATES                      
         BNE   VALTY030                                                         
         NI    JOBSTA1,X'FF'-JOBSNEST                                           
         OI    JOBSTA1,JOBSMCSE                                                 
         B     VALTY040                                                         
*                                                                               
VALTY020 TM    JOBSTA1,JOBSMCSE    MCS ESTIMATE?                                
         BZ    VALTY080            NO, SEE IF OLD                               
         CLI   8(R2),C'B'          IS THAT WHAT WAS ENTERED?                    
         BE    VALRATE             YES,NO CHANGE NEEDED                         
         CLI   8(R2),C'N'          CHANGED TO NEW?                              
         BNE   VALRATE                                                          
         GOTOR CHKMCS,DMCB,(RC)    CHECK FOR MCS ESTIMATES                      
         BNE   VALTY030                                                         
         NI    JOBSTA1,X'FF'-JOBSMCSE                                           
         OI    JOBSTA1,JOBSNEST                                                 
         B     VALTY040                                                         
*                                                                               
VALTY030 MVI   ERROR,HAVEEST                                                    
         B     ERREXIT                                                          
*                                                                               
VALTY040 MVI   ERROR,HAVEBILL      SEE IF OK TO CHANGE TYPE                     
         MVC   AIO,AIO2                                                         
         USING ACKEYD,R3                                                        
         LA    R3,KEY                                                           
         MVC   KEY,BLANKS          CAN'T ADD IF BILLING EXISTS                  
         MVC   ACKEYACC,SAVEKEY                                                 
         MVC   ACKEYWRK,=C'99'                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(ACKEYWRK-ACKEYD+2),KEYSAVE                                   
         BE    ERREXIT                                                          
         MVC   AIO,AIO1                                                         
*                                                                               
VALTY060 NI    JOBSTA1,X'FF'-(JOBSMCSE+JOBSNEST)                                
         CLI   8(R2),C'O'          SET UP TYPE ENTERED: OLD                     
         BE    VALRATE                                                          
         OI    JOBSTA1,JOBSNEST    OR NEW                                       
         CLI   8(R2),C'N'                                                       
         BE    VALRATE                                                          
         NI    JOBSTA1,X'FF'-JOBSNEST                                           
         OI    JOBSTA1,JOBSMCSE    OR BRAND OCEAN                               
         CLI   8(R2),C'B'                                                       
         BNE   ERREXIT                                                          
         B     VALRATE             DONE                                         
*                                                                               
VALTY080 CLI   8(R2),C'O'          IS THIS TYPE OLD?                            
         BNE   VALTY040            NO, SEE IF CHANGE OKAY                       
         DROP  R3                                                               
*                                                                               
VALRATE  LA    R2,JBRRATH                                                       
         CLI   5(R2),0                                                          
         BE    VALAWO              NOTHING TO CHANGE OR ADD                     
         TM    4(R2),X'20'                                                      
         BO    VALAWO                                                           
*                                                                               
         NI    JOBSTA1,X'FF'-JOBSART    SETUP AS NO RATE ADJUSTMENT             
         CLI   8(R2),C'Y'                                                       
         BNE   VALAWO                                                           
         OI    JOBSTA1,JOBSART          UNLESS 'Y' SPECIFIED                    
*                                                                               
VALAWO   LA    R2,JBRAWOH                                                       
         CLI   ACTNUM,ACTADD       ARE WE ADDING ?                              
         BNE   VALAWO10            NO                                           
*                                                                               
         CLI   5(R2),0             ANYTHING I/P                                 
         BE    VALAWO05            NO, USE PROFILE VALUE                        
         TM    4(R2),X'80'         INPUT THIS TIME                              
         BO    VALAWO07            YES, USE SCREEN VALUE                        
         TM    4(R2),X'20'         PREVIOUS VALIDATION                          
         BNO   VALAWO07            NO, USE SCREEN VALUE                         
*                                                                               
VALAWO05 NI    JOBSTA1,X'FF'-JOBSXJOB                                           
         CLI   GOAWOO,C'Y'         CHECK PROFILE? AUTO WRITEOFF ?               
         BNE   VALTMED             NO, LEAVE OFF                                
         OI    JOBSTA1,JOBSXJOB    YES, SET BIT ON                              
         B     VALTMED                                                          
*                                                                               
VALAWO07 MVI   ERROR,INVALID                                                    
         NI    JOBSTA1,X'FF'-JOBSXJOB   TURN OFF A/WO BIT                       
         CLI   8(R2),C'N'        AUTO WRITEOFF?                                 
         BE    VALTMED           NO                                             
         OI    JOBSTA1,JOBSXJOB                                                 
         CLI   8(R2),C'Y'        AUTO WRITEOFF?                                 
         BE    VALTMED           YES                                            
         B     ERREXIT           NO                                             
*                                                                               
VALAWO10 CLI   5(R2),0             ARE THEY CHANGING THE OPTION ?               
         BE    VALTMED             NO                                           
         TM    4(R2),X'20'                                                      
         BO    VALTMED                                                          
*                                                                               
         TM    JOBSTA1,JOBSXJOB    IS THIS AN X-JOB ?                           
         BZ    VALAWO50            NO                                           
         CLI   8(R2),C'Y'          YES, IS THAT WHAT WAS ENTERED ?              
         BE    VALTMED             YES, NO CHANGE NEEDED                        
*                                                                               
VALAWO20 MVI   ERROR,XJOBDATA      SEE IF IT'S OKAY                             
         MVC   AIO,AIO2                                                         
         USING ACKEYD,R3                                                        
         LA    R3,KEY                                                           
         MVC   KEY,BLANKS          CAN'T CHANGE IF TRANSACTION EXIST            
         MVC   ACKEYACC,SAVEKEY                                                 
         GOTO1 HIGH                                                             
*                                                                               
VALAWO30 GOTO1 SEQ                                                              
         L     R3,AIO                                                           
         CLC   ACKEYACC,KEYSAVE    STILL SAME KEY ?                             
         BNE   VALAWO40            NO, ALL DONE                                 
         CLI   ACRECORD,X'44'                                                   
         BNE   VALAWO30                                                         
         CLC   ACKEYWRK,=C'**'                                                  
         BE    VALAWO30                                                         
         B     ERREXIT                                                          
*                                                                               
VALAWO40 MVC   AIO,AIO1                                                         
         NI    JOBSTA1,X'FF'-JOBSXJOB                                           
         CLI   8(R2),C'N'                                                       
         BE    VALTMED                                                          
         OI    JOBSTA1,JOBSXJOB                                                 
         CLI   8(R2),C'Y'                                                       
         BNE   ERREXIT                                                          
         B     VALTMED                                                          
*                                                                               
VALAWO50 CLI   8(R2),C'N'          DID THEY CHANGE IT TO NO WRITEOFF ?          
         BNE   VALAWO20            NO, SEE IF THE CHANGE IS OK                  
*                                                                               
VALTMED  CLI   ACTNUM,ACTADD       ADDING?                                      
         BNE   VTMED20                                                          
         LA    R2,JBRTMEH                                                       
         CLI   5(R2),0             ANYTHING I/P                                 
         BE    VTMED10             NO, USE MEDIA VALUE                          
         TM    4(R2),X'80'         INPUT THIS TIME                              
         BO    VTMED20             YES, USE SCREEN VALUE                        
         TM    4(R2),X'20'         PREVIOUS VALIDATION                          
         BNO   VTMED20             NO, USE SCREEN VALUE                         
*                                  EXTRACT TALENT MEDIA FROM MEDIA STAT         
VTMED10  MVI   BYTE,ACMDSTV+ACMDSRAD                                            
         NC    BYTE,SVMEDSTA                                                    
         OC    JOBSTA1,BYTE                                                     
         B     VALCLOSE                                                         
*                                                                               
VTMED20  NI    JOBSTA1,X'FF'-JOBSTV-JOBSRAD TURN OFF TALENT MEDIA               
         CLI   JBRTMEH+5,0         ANYTHING HERE                                
         BE    VALCLOSE                                                         
         MVI   ERROR,INVALID                                                    
         LA    R2,JBRTMEH                                                       
         CLI   JBRTME,C'T'         TV INPUT                                     
         BNE   *+12                NO                                           
         OI    JOBSTA1,JOBSTV                                                   
         B     VALCLOSE                                                         
*                                                                               
         CLI   JBRTME,C'R'         RADIO?                                       
         BNE   ERREXIT             NO                                           
         OI    JOBSTA1,JOBSRAD                                                  
         NI    JOBSTA1,X'FF'-JOBSTUD    TURN OFF STUDIO BIT                     
*                                                                               
VALCLOSE MVC   STATUS26,JOBSTA1                                                 
         BAS   RE,VCLOSE           FINISH AND WRITE STATUS ELEMENT              
*                                                                               
VALSTUD  LA    R2,JBRSTUH                                                       
         GOTOR SETGET,DMCB,(RC)    SETUP FOR GETOPT                             
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         CLI   ACTNUM,ACTADD       ADDING?                                      
         BNE   VALST04             NO                                           
         CLI   GOSTUDIO,C'Y'       YES, STUDIO?                                 
         BE    VALST02             YES                                          
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0             NO, WAS A TYPE ENTERED?                      
         BE    VALFT1              NO                                           
         TM    6(R2),X'80'         WAS IT ENTERED THIS TIME?                    
         BO    ERREXIT             YES, ERROR                                   
         XC    8(L'JBRSTU,R2),8(R2)                                             
         OI    6(R2),X'80'         CLEAR THE FIELD                              
         MVI   5(R2),0                                                          
         B     VALFT1              DO NEXT FIELD                                
*                                                                               
VALST02  TM    4(R2),X'80'         WAS FIELD INPUT THIS TIME?                   
         BO    VALST04             YES, LEAVE IT                                
         OC    GOTYPE,GOTYPE       NO, DO WE HAVE A DEFAULT TYPE ?              
         BZ    VALFT1              NO, GO TO NEXT FIELD                         
         MVC   8(L'GOTYPE,R2),GOTYPE                                            
         MVC   5(1,R2),=YL1(L'GOTYPE)                                           
         OI    6(R2),X'80'         MOVE GOTYPE TO SCREEN                        
         B     VALST06                                                          
*                                                                               
VALST04  TM    4(R2),X'20'         NO, WAS FIELD CHANGED?                       
         BO    VALFT1              NO                                           
*                                                                               
         USING LNKELD,R6                                                        
         MVI   ELCODE,LNKELQ                                                    
         BAS   RE,GETELIO          YES, SEE IF WE CAN DELETE THE LINK           
         BNE   VALST05                                                          
         MVI   ERROR,LINKED                                                     
         OC    LNKAGJB,LNKAGJB     IT THIS LINKED?                              
         BNZ   ERREXIT             YES, CAN'T REMOVE                            
         GOTO1 REMELEM             NO, DELETE THE LINK                          
*                                                                               
VALST05  CLI   5(R2),0             DO WE HAVE A TYPE?                           
         BE    VALFT1                                                           
*                                                                               
         USING STURECD,R3                                                       
VALST06  MVI   ERROR,INVALID                                                    
         CLI   GOSTUDIO,C'Y'       IS STUDIO ALLOWED?                           
         BNE   ERREXIT             NO                                           
         GOTO1 ANY                 YES, SEE IF VALID                            
         MVC   AIO,AIO2                                                         
         LA    R3,KEY                                                           
         XC    STUKEY,STUKEY                                                    
         MVI   STUKTYP,STUKTYPQ                                                 
         MVI   STUKSUB,STUKSUBQ                                                 
         MVC   STUKCPY,CUL                                                      
         MVC   STUKCODE,WORK                                                    
         GOTO1 READ                                                             
*                                                                               
         USING STUELD,R6                                                        
         MVI   ELCODE,STUELQ                                                    
         MVI   ERROR,NOTDEFN                                                    
         BAS   RE,GETELIO                                                       
         BNE   ERREXIT                                                          
*                                                                               
         CLC   STUMED,MEDIA        FIND THE RIGHT MEDIA                         
         BE    VALST08                                                          
         CLC   STUMED,BLANKS                                                    
         BNH   VALST08                                                          
         MVI   ERROR,NOTMED                                                     
         B     ERREXIT                                                          
*                                                                               
VALST08  SR    R1,R1                                                            
         IC    R1,STULN GET LENGTH OF ELEMENT                                   
         SH    R1,=Y(STUOFF-STUELD)                                             
         BZ    ERREXIT             NO OFFICE/CLIENTS                            
*                                                                               
         LA    R5,STUOFF                                                        
*                                                                               
VALST10  CLC   2(3,R5),BLANKS      BLANK IS AS GOOD AS FIND                     
         BE    VALST12                                                          
         CLC   2(3,R5),CLICODE                                                  
         BE    VALST12                                                          
         LA    R5,L'STUOCLN(R5)                                                 
         SH    R1,=Y(L'STUOCLN)                                                 
         BNZ   VALST10                                                          
         B     ERREXIT             CAN'T FIND CLIENT                            
*                                                                               
VALST12  MVC   AIO,AIO1            VALID, SWAP BACK AND UPDATE STATUS           
         DROP  R3                                                               
*                                                                               
         USING LNKELD,R6                                                        
         MVI   ELCODE,LNKELQ                                                    
         BAS   RE,GETELIO          DO WE HAVE A LINK ELEMENT?                   
         BE    VALST14             YES                                          
*                                                                               
         LA    R6,ELEMENT          NO, BUILD ONE                                
         XC    ELEMENT,ELEMENT                                                  
         MVI   LNKEL,LNKELQ                                                     
         MVI   LNKLN,LNKLNQ                                                     
*                                                                               
         CLI   ACTNUM,ACTADD       ARE WE ADDING ?                              
         BE    VALST18             YES                                          
*                                                                               
VALST14  TM    4(R2),X'20'                                                      
         BO    VALST20                                                          
*                                                                               
         OC    8(L'JBRSTU,R2),BLANKS                                            
         CLC   8(L'JBRSTU,R2),LNKSTUD  IS STUDIO THE SAME?                      
         BE    VALST20                 YES, NO CHANGE NEEDED                    
*                                                                               
         MVI   ERROR,HAVEBILL                                                   
         CLC   LNKAGJB,LNKAGJB     CAN WE CHANGE IT?                            
         BNZ   ERREXIT             NO                                           
*                                                                               
         MVC   AIO,AIO2            YES, SO FAR. LOOK FOR BILLING                
         USING ACKEYD,R3                                                        
         LA    R3,KEY                                                           
         MVC   KEY,BLANKS                                                       
         MVC   ACKEYACC,SAVEKEY                                                 
         GOTO1 HIGH                                                             
*                                                                               
VALST16  GOTO1 SEQ                                                              
         L     R3,AIO                                                           
         CLC   ACKEYACC,KEYSAVE    STILL SAME KEY ?                             
         BNE   VALST18             NO, ALL DONE                                 
         CLI   ACRECORD,X'44'                                                   
         BNE   VALST16                                                          
         CLC   ACKEYWRK,=C'99'                                                  
         BNE   VALST16                                                          
         B     ERREXIT                                                          
*                                                                               
VALST18  MVC   LNKSTUD,WORK                                                     
         MVC   LNKSTJB,SAVEKEY+3                                                
         MVC   AIO,AIO1                                                         
*                                                                               
         USING JOBELD,R6                                                        
VALST20  GOTO1 ADDELEM                                                          
         MVI   ELCODE,JOBELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   VALFT1                                                           
         OI    JOBSTA1,JOBSTUD                                                  
*                                                                               
VALFT1   LA    R2,JBRFT1H                                                       
         MVI   ELCODE,X'3E'                                                     
         GOTO1 REMELEM                                                          
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT,X'3E'                                                    
         BAS   RE,VALFOOT                                                       
*                                                                               
VALFT2   LA    R2,JBRFT2H                                                       
         BAS   RE,VALFOOT                                                       
*                                                                               
VALFT3   LA    R2,JBRFT3H                                                       
         BAS   RE,VALFOOT                                                       
*                                                                               
VSWAP2   MVC   AIO,AIO2            USE AIO2 FROM NOW UNTIL XIT                  
*                                                                               
VALSRAC  LA    R2,JBRSRACH                                                      
         MVC   THISLEDG,RECVLEDG                                                
         BAS   RE,VALSEC           VALIDATE SECURITY, IF NEEDED                 
         CLI   5(R2),0                                                          
         BE    VAL1CAC                                                          
         BAS   RE,GETFUNC                                                       
         LA    R2,JBRSRNMH                                                      
         BAS   RE,VALUPDT                                                       
*                                                                               
VAL1CAC  LA    R2,JBR1CACH                                                      
         MVC   THISLEDG,=C'1C'                                                  
         BAS   RE,VALSEC           VALIDATE SECURITY, IF NEEDED                 
         CLI   5(R2),0                                                          
         BE    VAL29AC                                                          
         BAS   RE,GETFUNC                                                       
         LA    R2,JBR1CNMH                                                      
         BAS   RE,VALUPDT                                                       
*                                                                               
VAL29AC  LA    R2,JBR1CACH                                                      
         MVC   THISLEDG,=C'29'                                                  
         BAS   RE,VALSEC           VALIDATE SECURITY, IF NEEDED                 
         CLI   5(R2),0                                                          
         BE    VSWAP1                                                           
         BAS   RE,GETFUNC                                                       
         LA    R2,JBR1CNMH                                                      
         BAS   RE,VALUPDT                                                       
*                                                                               
VSWAP1   MVC   AIO,AIO1            USE AIO1 NOW                                 
*                                                                               
VALUSER  CLI   NEWDATE,0           NO, WAS CLOSING DATE CHANGED ?               
         BE    VUS010              NO                                           
         TM    GENSTAT6,GES$LINK   IF DDLINK UPLOAD IN PROCESS,                 
         BNO   TLABEL              NO, DON'T VALIDATE USER FIELDS               
         CLI   RECNUM,ACTCHA       DON'T CLEAR USER FIELDS ON CHANGE            
         BE    *+8                                                              
         BAS   RE,CLEARU           CLEAR USER FIELDS                            
         BAS   RE,DISUSER          DISPLAY USER HEADERS HERE SO DONE IN         
*                                   ONE TRANSACTION                             
*                                                                               
VUS010   MVI   ELCODE,ACUFELQ                                                   
         GOTO1 REMELEM             REMOVE OLD 'A2' ELEMENTS                     
         TM    GENSTAT6,GES$LINK   IF DDLINK UPLOAD IN PROCESS, DON'T           
         BO    VUS015              NEED TABLE, ALREADY HAVE IT.                 
         GOTOR GETTAB,DMCB,(RC)    GET ELEMENT TABLE                            
*                                                                               
VUS015   LA    R0,10                                                            
         L     R3,AELMTAB                                                       
         LA    R5,JBRUSEHH                                                      
                                                                                
         CP    ELMCNT,=P'0'                                                     
         BE    VUS020                                                           
         CLI   0(R3),0                                                          
         BE    TLABEL                                                           
                                                                                
VUS020   SR    R2,R2                                                            
         IC    R2,0(R5)                                                         
         AR    R2,R5                                                            
         USING JBRUSEHH,R5                                                      
         CLC   JBRUSEH,BLANKS      SKIP IF NO HEADER FOR USER FIELD             
         BH    VUS040                                                           
         DROP  R5                                                               
*                                                                               
         TM    GENSTAT6,GES$LINK   TEST DDLINK UPLOAD IN PROCESS                
         BO    VUS030                                                           
         CLI   USCRIPT,C'Y'        EXECUTING SCRIPT?                            
         BNE   VUS200                                                           
*                                                                               
         USING JBRUSEHH,R5                                                      
VUS030   CLC   JBRUSED,BLANKS      DATA W/ NO HEADER?   (SCRIPT ONLY)           
         BNH   VUS200              NO, BUMP TO NEXT USER FIELD                  
         MVI   ERROR,UDNEXIST      YES, FLAG ERROR                              
         B     ERREXIT                                                          
*                                                                               
VUS040   XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(ACUFDATA-ACUFD),0(R3)                                    
         MVI   ELEMENT+2,X'00'     REMOVE SEQUENCE NUMBER                       
         MVC   ELEMENT+ACUFDATA-ACUFD(L'JBRUSED),BLANKS                         
         CLI   5(R2),0             MOVE DATA IF ENTERED                         
         BNE   VUS060                                                           
         MVI   ERROR,REQFLD        PREPARE FOR MISSING REQUIRED FIELD           
         TM    ACUFSTAT-ACUFD(R3),X'80'                                         
         BO    ERREXIT                                                          
         B     VUS180              NOT ENTERED, ADD WITH BLANK DATA             
*                                                                               
         USING ACUFD,R6                                                         
VUS060   LA    R6,ELEMENT                                                       
         SR    RF,RF                                                            
         IC    RF,5(R2)            GET LENGTH OF INPUT DATA                     
         CLI   ACUFEDIT-ACUFD(R3),C'D'                                          
         BNE   *+8                                                              
         MVI   ACUFMXLN,8                                                       
         SR    R1,R1                                                            
         IC    R1,ACUFMXLN         GET MAXIMUM LENGTH ALLOWED                   
         CR    RF,R1                                                            
         MVI   ERROR,INP2LONG      ERROR IF INPUT LONGER THAN MAX               
         BH    ERREXIT                                                          
         CLI   ACUFEDIT-ACUFD(R3),C'N'  VALIDATE FOR NUMERIC IF                 
         BNE   VUS100               EDIT = N                                    
         LR    R1,RF               SAVE INPUT LENGTH                            
         XC    WORK,WORK                                                        
         MVI   ERROR,NOTNUM        PREPARE ERROR MESSAGE                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),8(R2)       MOVE IN JUST ZONES                           
         LA    RE,WORK                                                          
         LR    R1,RF               RESET LENGTH                                 
*                                                                               
VUS080   CLI   0(RE),X'F0'                                                      
         BNE   ERREXIT                                                          
         LA    RE,1(0,RE)                                                       
         BCT   R1,VUS080                                                        
*                                                                               
VUS100   CLI   ACUFEDIT-ACUFD(R3),C'D'                                          
         BNE   VUS120                                                           
         MVI   ERROR,INVDATE                                                    
         ST    RF,SAVERF                                                        
         GOTO1 DATVAL,DMCB,(0,8(R2)),DUB   VALIDATE FOR DATE IF                 
         OC    DMCB,DMCB                    EDIT = D                            
         BZ    ERREXIT                                                          
         L     R1,SAVERF           SAVE INPUT LENGTH                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         GOTO1 DATCON,DMCB,DUB,(5,8(R2))                                        
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         XC    SAVERF,SAVERF                                                    
         MVI   SAVERF+3,8                                                       
         L     RF,SAVERF                                                        
         STC   RF,ACUFMXLN                                                      
*                                                                               
VUS120   CLI   ACUFEDIT-ACUFD(R3),C'C'                                          
         BNE   VUS160                                                           
         LR    R1,RF               SAVE INPUT LENGTH                            
         XC    WORK,WORK                                                        
         MVI   ERROR,NOTCHAR       PREPARE ERROR MESSAGE                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),8(R2)       MOVE IN JUST ZONES                           
         LR    R1,RF               RESET LENGTH                                 
         LA    RE,WORK                                                          
*                                                                               
VUS140   CLI   0(RE),X'F0'                                                      
         BE    ERREXIT                                                          
         LA    RE,1(0,RE)                                                       
         BCT   R1,VUS140                                                        
*                                                                               
VUS160   IC    R1,ACUFLEN          GET LENGTH OF RECORD                         
         AR    R1,RF               ADD INPUT LENGTH TO RECORD TO                
         STC   R1,ACUFLEN           GET NEW RECORD LENGTH                       
         BCTR  RF,0                MOVE TO RECORD BASED ON                      
         EX    RF,*+8               INPUT LENGTH                                
         B     *+10                                                             
         MVC   ACUFDATA(0),8(R2)   MOVE DATA TO RECORD                          
*                                                                               
VUS180   GOTO1 ADDELEM             ADD THE NEW ELEMENT                          
*                                                                               
VUS200   SR    RF,RF               BUMP TO NEXT HEADER                          
         IC    RF,0(R5)                                                         
         AR    R5,RF                                                            
         IC    RF,0(R5)                                                         
         AR    R5,RF                                                            
         LA    R3,ELMLNG(R3)                                                    
         BCT   R0,VUS020           DO FOR ALL 10 FIELDS                         
         DROP  R5,R6                                                            
*                                                                               
         MVI   VERUSER,1           USER FIELDS VERIFIED                         
*                                                                               
TLABEL   CLI   ACTNUM,ACTADD       IS THIS AN ADD ?                             
         BE    VLABEL              YES, ALL ADDS NEED LABELS                    
         GOTOR ACTMAIN,DMCB,(RC)                                                
         CLI   GOSUPSTI,C'C'       NO, SUPPRESS THEM FOR CHANGES?               
         BE    VLABELX             YES                                          
         TM    JBRSJNMH+4,X'20'    FIELD CHANGED, TEST FOR LABELS               
         BNO   VLABEL                                                           
         TM    JBRCLOSH+4,X'20'                                                 
         BO    OKEXIT                                                           
*                                                                               
VLABEL   GOTO1 GETOPT,DMCB,GOBLOCK                                              
         CLI   GOSUPSTI,C'Y'       SUPPRESS ALL STICKY LABELS                   
         BE    VLABELX             YES                                          
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT+10,12                                                    
         MVC   ELEMENT+26(80),BLANKS                                            
         MVC   ELEMENT+26(2),=C'12'                                             
         MVC   ELEMENT+28(1),CUL                                                
         MVC   ELEMENT+29(1),SAVEOFC                                            
         MVI   ELCODE,ACPRELQ                                                   
         BAS   RE,GETELIO                                                       
         USING ACPROFD,R6                                                       
         CLC   ACPROFFC,BLANKS                                                  
         BNH   *+10                                                             
         MVC   ELEMENT+29(1),ACPROFFC                                           
         L     RE,AIO                                                           
         MVC   ELEMENT+35(15),0(RE)                                             
         GOTO1 DATAMGR,DMCB,DMADD,=C'ACCREQS',ELEMENT,ELEMENT,0                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VLABELX  CLC   CONACT(5),=C'CLOSE'    EXIT WITH NO RETURN IF DOING              
         BE    CLOSEX                 CLOSE, OPEN, DELETE OR RESTORE            
         CLC   CONACT(4),=C'OPEN'                                               
         BE    CLOSEX                                                           
         CLC   CONACT(6),=C'DELETE'                                             
         BE    CLOSEX                                                           
         CLC   CONACT(7),=C'RESTORE'                                            
         BE    CLOSEX                                                           
*                                                                               
* SEND THE JOB CODE BACK TO PRESTO                                              
         TM    GENSTAT6,GES$LINK   TEST DDLINK UPLOAD IN PROCESS                
         BNO   OKEXIT                                                           
         MVC   CONHEAD(9),=C'JOB CODE='                                         
         MVC   CONHEAD+9(L'JBXSJAC),JBXSJAC                                     
         OI    CONHEADH+6,X'80'    TRANSMIT FIELD                               
         B     OKEXIT                                                           
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE EXTRA DETAILS                        *         
***********************************************************************         
*                                                                               
VREC2    NTR1                                                                   
*                                                                               
         LA    R2,JBXADR1H         VALIDATE BILLING ADDRESS                     
         GOTO1 ADDRIN                                                           
*                                                                               
         GOTO1 PERSIN2                                                          
*                                                                               
         USING RSTELD,R6                                                        
         MVI   ELCODE,RSTELQ       VALIDATE @TRACKER                            
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE BY NOW                         
*                                                                               
         CLI   RSTLN,RSTLN3Q                                                    
         BL    VALACM              ELEMENT SHOULD NEVER BE TOO SHORT            
*&&DO                                                                           
         LA    R2,JBXTRAKH                                                      
         NI    RSTSTAT6,X'FF'-RSTSXF@T                                          
         CLI   8(R2),C'Y'                                                       
         BNE   *+8                                                              
         OI    RSTSTAT6,RSTSXF@T   INCLUDE IN @TRACKER                          
*&&                                                                             
VALACM   LA    R2,JBXACMH          ACCOUNT MEMO                                 
         MVI   ELCODE,OMEELQ                                                    
         GOTO1 REMELEM                                                          
         XC    ELEMENT,ELEMENT                                                  
         CLI   5(R2),0                                                          
         BE    VREC2XIT                                                         
*                                                                               
         USING OMEELD,R6                                                        
         LA    R6,ELEMENT                                                       
         MVI   ELEMENT,OMEELQ                                                   
         ZIC   RF,JBXACMH+5                                                     
         AHI   RF,2                                                             
         STC   RF,OMELN                                                         
         AHI   RF,-3                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   OMEMO(0),JBXACM                                                  
         GOTO1 ADDELEM                                                          
*                                                                               
VREC2XIT B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE THE ACCOUNT CODE, LEVEL ETC.                            
***********************************************************************         
*                                                                               
VALACCT  NTR1                                                                   
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C' '          DOES ACCOUNT START WITH A BLANK ?            
         BE    ERREXIT             YES, THIS IS AN ERROR                        
         MVC   CUL+1(2),THISLEDG                                                
         MVI   OPTION2,C'N'        DON'T CHECK SECURITY HERE                    
         GOTO1 SETHEIR                                                          
         MVI   OPTION2,C'Y'        RESET FOR NEXT TIME                          
         LA    R0,1                                                             
         LA    R6,LLEVA                                                         
*                                                                               
VAC020   CLC   5(1,R2),0(R6)       FIND HIERARCHY LEVEL FOR ACCT CODE.          
         BNH   VAC040                                                           
         LA    R6,1(R6)                                                         
         AHI   R0,1                                                             
         CHI   R0,4                                                             
         BNH   VAC020                                                           
         MVI   ERROR,TOOLONG                                                    
         B     ERREXIT                                                          
*                                                                               
VAC040   CHI   R0,4                MUST BE A LOW LEVEL ACCOUNT.                 
         BE    VAC080                                                           
         CLC   THISLEDG,PRODLEDG                                                
         BNE   VAC060              SALES ANALYSIS ACCOUNT MUST BE               
         CHI   R0,2                AT PRODUCT LEVEL.                            
         BE    VAC080                                                           
         B     *+12                                                             
*                                                                               
VAC060   CLI   1(R6),0                                                          
         BE    VAC080                                                           
         MVI   ERROR,WRNGLVAC      IT ISN'T.                                    
         B     ERREXIT                                                          
*                                                                               
VAC080   LA    R6,LLEVA            MAKE SURE HIGHER LEVEL ACCOUNTS XIST         
*                                                                               
VAC100   BCT   R0,*+8                                                           
         B     VAC120              NO HIGHER LEVELS.                            
         MVC   KEY+1(L'KEY-1),BLANKS                                            
         MVC   KEY+1(2),THISLEDG                                                
         LA    R6,LLEVA            FIND LENGTH OF THIS LEVEL.                   
         AR    R6,R0                                                            
         BCTR  R6,0                                                             
         SR    R1,R1                                                            
         IC    R1,0(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)      R2 = A(ACCT CODE FIELD HEADER).              
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(42),KEY                                                  
         BE    VAC100              FOUND THIS LEVEL OK.                         
         MVI   ERROR,NOHIGHER                                                   
         B     ERREXIT                                                          
*                                                                               
VAC120   MVC   KEY+3(L'KEY-3),BLANKS                                            
         SR    R1,R1               NOW READ THE ACCOUNT                         
         ICM   R1,1,5(R2)                                                       
         BZ    VAC140              NO INPUT HERE                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)      MOVE IN KEY VALUE LESS CO/UNIT/LEDG.         
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(42),KEY                                                  
         BNE   EXIT                                                             
*                                                                               
VAC140   L     R6,AIO                                                           
         NI    (ACSTATUS-ACKEYD)(R6),X'7F'                                      
         CLC   THISLEDG,PRODLEDG                                                
         BE    OKEXIT                                                           
         MVI   ELCODE,ACBLELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    VAC160                                                           
         MVI   ERROR,INVPOST                                                    
         B     ERREXIT                                                          
*                                                                               
VAC160   GOTO1 =A(SVNAME),DMCB,(RC),OLDACCNM,RR=MYRELO                          
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                           DISPLAY FILTERS AND STATUS                *         
***********************************************************************         
*                                                                               
         USING RSTELD,R6                                                        
         USING SCREEND,R2                                                       
DISFILT  NTR1                                                                   
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   DISFILTX                                                         
*                                                                               
         LA    R2,ACF1H                                                         
         GOTO1 SETFLTS                                                          
         DROP  R2                  PRODLEDG SHOULD BE ADDRESSED BY RA           
*                                                                               
         CLC   THISLEDG,PRODLEDG                                                
         BNE   DISFILTX                                                         
*                                                                               
         XC    JBRSTAT,JBRSTAT     SET CLOSED/LOCKED                            
         LA    R1,JBRSTAT                                                       
         OI    JBRSTATH+6,X'80'                                                 
         TM    RSTSTAT,X'60'                                                    
         BZ    DISFILTX                                                         
         TM    RSTSTAT,X'40'       IS JOB CLOSED?                               
         BZ    DISFILT4            NO, MUST BE LOCKED THEN                      
*                                                                               
         MVC   0(2,R1),=C'C,'      NO                                           
         TM    RSTSTAT,X'20'       IS IT LOCKED ALSO?                           
         BO    DISFILT2            YES                                          
         MVI   1(R1),C' '                                                       
         B     DISFILTX                                                         
*                                                                               
DISFILT2 LA    R1,2(R1)                                                         
*                                                                               
DISFILT4 MVI   0(R1),C'L'                                                       
*                                                                               
DISFILTX B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                           DISPLAY FILTERS                           *         
***********************************************************************         
*                                                                               
         USING RSTELD,R6                                                        
         USING SCREEND,R2                                                       
DISFIL2  NTR1                                                                   
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   DISFIL2X                                                         
*                                                                               
         LA    R2,ACF1H                                                         
         GOTO1 SETFLTS                                                          
*&&DO                                                                           
         MVI   JBXTRAK,C'N'        DEFAULT IS NO                                
         CLI   RSTLN,RSTLN3Q                                                    
         BL    DISFIL2X            ELEMENT SHOULD NEVER BE TOO SHORT            
         TM    RSTSTAT6,RSTSXF@T                                                
         BZ    *+8                                                              
         MVI   JBXTRAK,C'Y'                                                     
         OI    JBXTRAKH+6,X'80'                                                 
*&&                                                                             
DISFIL2X B     OKEXIT                                                           
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*                    CLEAR USER SELECT HEADERS AND DATA               *         
***********************************************************************         
*                                                                               
CLEARU   NTR1                                                                   
         GOTOR GETTAB,DMCB,(RC)    GET ELEMENT TABLE                            
         L     R2,AELMTAB          CLEAR TABLE OF USER ELEMENTS                 
         LA    R3,(32*20)                                                       
         SR    R1,R1                                                            
         MVCL  R2,R0                                                            
         LA    R2,JBRUSEHH                                                      
         LA    R0,10               CLEAR USER SELECT HEADERS AND DATA           
         TM    GENSTAT6,GES$LINK   IF DDLINK UPLOAD IN PROCESS,                 
         BO    OKEXIT              DON'T CLEAR SCREEN, ONLY TABLE               
*                                                                               
DCLEAR   XC    8(L'JBRUSEH,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         MVI   5(R2),0                                                          
         BAS   RE,UBUMP            SKIP TO NEXT FIELD                           
         XC    8(L'JBRUSED,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         MVI   5(R2),0                                                          
         BAS   RE,UBUMP                                                         
         BCT   R0,DCLEAR                                                        
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                     VALIDATE 'AUTO' FUNCTION                        *         
***********************************************************************         
*                                                                               
VALAUTO  NTR1                                                                   
         MVI   ERROR,NOAUTO        PREPARE FOR ERROR                            
         CLI   ACTNUM,ACTADD       'AUTO' VALID ONLY FOR NEW                    
         BE    VALAUT2                                                          
         CLC   CONACT(4),=C'OPEN'    REOPEN                                     
         BE    VALAUT2                                                          
         CLC   CONACT(4),=C'AUTO'    OR AUTO                                    
         BNE   ERREXIT                                                          
*                                                                               
VALAUT2  GOTO1 GETOPT,DMCB,GOBLOCK  AT JOB LEVEL                                
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                     DISPLAY USER  SELECT HEADERS                    *         
***********************************************************************         
*                                                                               
         USING USERD,R5                                                         
DISUSER  NTR1                                                                   
         MVC   AIO,AIO2                                                         
         ZAP   ELMCNT,=P'0'        CLEAR COUNTER                                
         LA    R5,USERKEY          LOOK FOR COMPANY LEVEL                       
         XC    USERKEY,USERKEY                                                  
         BAS   RE,READHIU                                                       
         CLC   KEYSAVE(ACUFOG-ACUFKEY),KEY  EXIT IF NO RECS FOR THIS            
         BNE   OKEXIT               COMPANY                                     
         CLC   KEYSAVE(L'ACUFKEY),KEY  SAVE HEADERS IF RECORD FOUND             
         BNE   DISMG                                                            
         BAS   RE,USERTABL                                                      
*                                                                               
DISMG    OC    MGROUP,MGROUP       LOOK FOR COMPANY, MEDIA GROUP                
         BZ    DISMED                                                           
         XC    USERKEY,USERKEY                                                  
         MVC   USERMG,MGROUP                                                    
         BAS   RE,READHIU                                                       
         BNE   DISMED                                                           
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
*                                                                               
DISMED   OC    MEDIA,MEDIA         LOOK FOR COMPANY, MEDIA                      
         BZ    DISOFG                                                           
         XC    USERKEY,USERKEY                                                  
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIU                                                       
         BNE   DISOFG                                                           
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
*                                                                               
DISOFG   OC    EFFOFG,EFFOFG       LOOK FOR COMPANY, OFFICE GROUP               
         BZ    DISOFF                                                           
         XC    USERKEY,USERKEY                                                  
         MVC   USEROFG,EFFOFG                                                   
         BAS   RE,READHIU                                                       
         CLC   KEYSAVE(ACUFOFC-ACUFKEY),KEY  SKIP IF NO OGR RECS                
         BNE   DISOFF                                                           
         CLC   KEYSAVE(L'ACUFKEY),KEY     SAVE HEADERS IF RECORD FOUND          
         BNE   DUOG020                                                          
         BAS   RE,USERTABL                                                      
*                                                                               
DUOG020  OC    MGROUP,MGROUP       LOOK FOR COMPANY, OFFICE GROUP,              
         BZ    DUOG040              MEDIA GROUP                                 
         MVC   USERMG,MGROUP                                                    
         BAS   RE,READHIU                                                       
         BNE   DUOG040             SAVE HEADERS IF RECORD FOUND                 
         BAS   RE,USERTABL                                                      
*                                                                               
DUOG040  OC    MEDIA,MEDIA         LOOK FOR COMPANY, OFFICE GROUP,              
         BZ    DISOFF               MEDIA                                       
         XC    USERMG,USERMG                                                    
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIU                                                       
         BNE   DISOFF              SAVE HEADERS IF RECORD FOUND                 
         BAS   RE,USERTABL                                                      
*                                                                               
DISOFF   CLC   EFFOFFC,BLANKS      LOOK FOR COMPANY, OFFICE                     
         BNH   DISCLI                                                           
         XC    USERKEY,USERKEY                                                  
         MVC   USEROFF,EFFOFFC                                                  
         BAS   RE,READHIU                                                       
         CLC   KEYSAVE(ACUFCLI-ACUFKEY),KEYSAVE                                 
         BNE   DISCLI              SKIP IF NO OFFICE RECORDS                    
         CLC   KEYSAVE(L'ACUFKEY),KEY  SAVE HEADER IF REC FOUND                 
         BNE   DUOF020                                                          
         BAS   RE,USERTABL                                                      
*                                                                               
DUOF020  OC    MGROUP,MGROUP       LOOK FOR COMPANY, OFFICE,                    
         BZ    DUOF040              MEDIA GROUP                                 
         MVC   USERMG,MGROUP                                                    
         BAS   RE,READHIU                                                       
         BNE   *+8                                                              
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
         XC    USERMG,USERMG                                                    
*                                                                               
DUOF040  OC    MEDIA,MEDIA         LOOK FOR COMPANY, OFFICE,                    
         BZ    DISCLI               MEDIA                                       
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIU                                                       
         CLC   KEYSAVE(L'ACUFKEY),KEY     SAVE HEADERS IF RECORD FOUND          
         BNE   DISCLI                                                           
         BAS   RE,USERTABL                                                      
*                                                                               
DISCLI   XC    USERKEY,USERKEY     LOOK FOR COMPANY, CLIENT                     
         MVC   USERCLI,CLICODE                                                  
         BAS   RE,READHIU                                                       
         CLC   KEYSAVE(ACUFPRO-ACUFKEY),KEY                                     
         BNE   DISHEAD             SKIP IF NO CLIENT RECS                       
         CLC   KEYSAVE(L'ACUFKEY),KEY  SAVE HEADERS IF REC FOUND                
         BNE   DUCLI020                                                         
         BAS   RE,USERTABL                                                      
*                                                                               
DUCLI020 OC    MGROUP,MGROUP       LOOK FOR COMPANY, CLIENT, MEDIA              
         BZ    DUCLI040             GROUP                                       
         MVC   USERMG,MGROUP                                                    
         BAS   RE,READHIU                                                       
         BNE   *+8                                                              
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
         XC    USERMG,USERMG                                                    
*                                                                               
DUCLI040 OC    MEDIA,MEDIA         LOOK FOR COMPANY, CLIENT, MEDIA              
         BZ    DUCLI060                                                         
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIU                                                       
         BNE   *+8                                                              
         BAS   RE,USERTABL         SAVE HEADERS IF KEYS MATCH                   
         XC    USERMED,USERMED                                                  
*                                                                               
DUCLI060 MVC   USERPRO,PRODCODE    LOOK FOR COMPANY, CLIENT,                    
         BAS   RE,READHIU           PRODUCT                                     
         BNE   *+8                                                              
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
         OC    MGROUP,MGROUP       LOOK FOR COMPANY, CLIENT,                    
         BZ    DUCLI080             PRODUCT, MEDIA GROUP                        
         MVC   USERMG,MGROUP                                                    
         BAS   RE,READHIU                                                       
         BNE   *+8                                                              
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
         XC    USERMG,USERMG                                                    
*                                                                               
DUCLI080 OC    MEDIA,MEDIA         LOOK FOR COMPANY, CLIENT,                    
         BZ    DUCLI100             PRODUCT, MEDIA                              
         MVC   USERMED,MEDIA                                                    
         BAS   RE,READHIU                                                       
         BNE   *+8                                                              
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
         XC    USERMED,USERMED                                                  
*                                                                               
DUCLI100 MVC   USERJOB,JBRSJAC     LOOK FOR COMPANY, CLIENT,                    
         OC    USERJOB,BLANKS       PRODUCT, JOB                                
         BAS   RE,READHIU                                                       
         BNE   *+8                                                              
         BAS   RE,USERTABL         SAVE HEADERS IF RECORD FOUND                 
*                                                                               
DISHEAD  BAS   RE,USERHEAD                                                      
         MVC   AIO,AIO1                                                         
         B     OKEXIT                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*                        VALIDATE CLOSING DATE                        *         
***********************************************************************         
*                                                                               
         USING JOBELD,R6                                                        
VCLOSE   NTR1                                                                   
         LA    R2,JBRCLOSH                                                      
         GOTO1 ANY                                                              
         XC    WORK(20),WORK                                                    
         MVC   STARTP,OPEN         DETERMINE CORRECT START DATE                 
         OC    STARTP,STARTP                                                    
         BNZ   *+10                                                             
         MVC   STARTP,JOBADATE                                                  
         GOTO1 DATCON,DMCB,(1,STARTP),(0,STARTD)                                
*                                                                               
VCLO010  CLI   8(R2),C'+'                                                       
         BE    VCLO040                                                          
         CLC   8(4,R2),=C'AUTO'                                                 
         BNE   VCLO080                                                          
*                                                                               
         BAS   RE,VALAUTO          SEE IF AUTO VALID FOR THIS FUNCTION          
         LH    R3,GOAUTADD                                                      
*                                                                               
VCLO020  GOTO1 ADDAY,DMCB,STARTD,CLOSED,(R3)                                    
         GOTO1 DATCON,DMCB,(0,CLOSED),(1,JOBCDATE)                              
         GOTO1 DATCON,DMCB,(1,JOBCDATE),(2,SVDTOUCL)  JOBCDATE                  
         B     VCLO090                                                          
*                                                                               
VCLO040  LR    R1,R2               GET STARTING POINT                           
         SR    RF,RF               SUBTRACT LENGTH OF ENTIRE FIELD              
         ICM   RF,1,5(R2)                                                       
         MVI   ERROR,NOTNUM        GET READY FOR ERROR                          
         BCTR  RF,0                MINUS 1 FOR THE '+'                          
         LTR   RF,RF               + ONLY, ERROR                                
         BNP   ERREXIT                                                          
         LR    R1,RF               SAVE THIS LENGTH                             
         BCTR  RF,0                MINUS 1 FOR THE EXECUTE                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVZ   WORK+3(0),9(R2)                                                  
         LA    RE,WORK+3                                                        
         CLI   0(RE),X'F0'                                                      
         BNE   ERREXIT                                                          
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,9(0,R2)                                                      
         CVB   R3,DUB                                                           
         B     VCLO020                                                          
*                                                                               
VCLO080  MVI   ERROR,INVDATE       VALIDATE DATE                                
         GOTO1 DATVAL,DMCB,8(R2),CLOSED                                         
         OC    DMCB,DMCB           ERROR IF NOT DATE ENTERED                    
         BZ    ERREXIT                                                          
         GOTO1 DATCON,DMCB,(0,CLOSED),(1,JOBCDATE) CONVERT DATE                 
         GOTO1 DATCON,DMCB,(1,JOBCDATE),(2,SVDTOUCL)  JOBCDATE                  
*                                                                               
VCLO090  MVI   ERROR,CLOSLOW       PREPARE FOR ERROR                            
         CLC   OPEN,JOBCDATE       OPEN MUST BE LOWER THAN CLOSED               
         BNL   ERREXIT                                                          
         CLI   JOBLN,0             NO LENGTH, WE MUST BE ADDING                 
         BE    VCLO100                                                          
         OC    OPEN,OPEN                                                        
         BZ    VCLO120                                                          
         CLI   JOBLN,JOBLN2Q       ELEMENT BIG ENOUGH FOR OPEN DATE ?           
         BL    VCLO100             NO, ADD LONGER ELEMENT                       
         MVC   JOBODATE,OPEN                                                    
         GOTO1 DATCON,DMCB,(1,JOBODATE),(2,SVDTOUOP) JOBODATE                   
         B     VCLO120                                                          
*                                                                               
VCLO100  MVI   JOBLN,JOBLN4Q                                                    
         MVC   JOBODATE,OPEN                                                    
         GOTO1 DATCON,DMCB,(1,JOBODATE),(2,SVDTOUOP) JOBODATE                   
         GOTO1 ADDELEM                                                          
*                                                                               
VCLO120  OC    JOBODATE,JOBODATE   IS THERE AN OPEN DATE ?                      
         BZ    VCLO140             NO, USE START THEN                           
         CLC   SVSTART,JOBODATE    YES, HAS IT CHANGED ?                        
         B     *+10                                                             
*                                                                               
VCLO140  CLC   SVSTART,JOBADATE                                                 
         BE    OKEXIT              NO, EXIT                                     
         TM    GENSTAT6,GES$LINK   IF DDLINK UPLOAD IN PROCESS,                 
         BNO   *+10                                                             
         MVC   SVSTART,JOBODATE    SAVE THE NEW START DATE                      
         MVI   NEWDATE,1           YES, SET FLAG                                
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                        VALIDATE OPEN DATE                           *         
***********************************************************************         
*                                                                               
         USING JOBELD,R6                                                        
VOPEN    NTR1                                                                   
         LA    R2,JBROPENH                                                      
         CLI   5(R2),0             DO WE HAVE A DATE ?                          
         BNE   VOPEN4              YES                                          
         CLI   ACTNUM,ACTADD       NO, ARE WE ADDING A NEW JOB?                 
         BNE   VOPENX              NO, LEAVE IT ALONE                           
*                                                                               
VOPEN2   GOTO1 DATCON,DMCB,(5,0),(1,OPEN)                                       
         B     VOPENX                                                           
*                                                                               
VOPEN4   SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'TODAY'                                                
         BE    VOPEN2                                                           
         MVI   ERROR,INVDATE       VALIDATE DATE AND PUT IN OPEN                
         GOTO1 DATVAL,DMCB,8(R2),WORK                                           
         OC    DMCB,DMCB           ERROR IF NOT DATE ENTERED                    
         BZ    ERREXIT                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(1,OPEN)                                    
*                                                                               
VOPENX   B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*              BUILD A BLANK PROFILE ELEMENT                                    
***********************************************************************         
*                                                                               
         USING ACPROFD,R6                                                       
ADDPROF  NTR1                                                                   
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACPREL,ACPRELQ                                                   
         MVI   ACPRLEN,ACPRNARR-ACPROFD                                         
         MVC   ACPRUNBL,BLANKS                                                  
         GOTO1 ADDELEM                                                          
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE STATUS ELEMENT                       *         
***********************************************************************         
*                                                                               
         USING SCREEND,R2                                                       
         USING RSTELD,R6                                                        
VALSTAT  NTR1                                                                   
         MVI   DRFTITMS,C'N'       SET NO DRAFT TRANSACTIONS                    
         MVI   ELCODE,ASTELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   *+18                                                             
         OC    ASTDRAFT-ASTELD(L'ASTDRAFT,R6),ASTDRAFT-ASTELD(R6)               
         BZ    *+8                                                              
         MVI   DRFTITMS,C'Y'                                                    
*                                                                               
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETELIO                                                       
         BE    VSTAT20                                                          
*                                                                               
         LA    R6,ELEMENT                                                       
         XC    ELEMENT(RSTLN3Q),ELEMENT                                         
         MVI   RSTLN,RSTLN3Q       VALFLTS CHECKS LENGTH                        
         CLI   GOJLDEST,C'Y'       LOCKED FOR ESTIMATES?                        
         BNE   *+8                 NO                                           
         OI    RSTLSTAT,RSTLSESQ   YES                                          
         CLI   GOJLDORD,C'Y'       LOCKED FOR ORDERS?                           
         BNE   *+8                 NO                                           
         OI    RSTLSTAT,RSTLSORQ   YES                                          
         CLI   GOJLDBIL,C'Y'       LOCKED FOR BILLING?                          
         BNE   *+8                 NO                                           
         OI    RSTLSTAT,RSTLSBIQ   YES                                          
         CLI   GOJLDTSI,C'Y'       LOCKED FOR TIMESHEETS?                       
         BNE   *+12                NO                                           
         OI    RSTLSTAT,RSTLSTIQ   YES                                          
         OI    RSTSTAT5,RSTSNOTS                                                
         CLI   GOJLDADJ,C'Y'       LOCKED FOR ADJUSTMENTS?                      
         BNE   *+8                 NO                                           
         OI    RSTLSTAT,RSTLSADQ   YES                                          
         CLI   GOJLDEXT,C'Y'       LOCKED FOR EXTERNAL POSTINGS?                
         BNE   *+8                 NO                                           
         OI    RSTLSTAT,RSTLSEXQ   YES                                          
         B     VSTAT40                                                          
*                                                                               
VSTAT20  CLI   RSTLN,RSTLN3Q       IS THIS BIG ENOUGH FOR FILTER VALS           
         BNL   VSTAT40             YES                                          
*                                                                               
         MVI   ELCODE,RSTELQ       EXTRACT RSTEL TO "ELEMENT"                   
         GOTO1 REMELEM                                                          
         LA    R6,ELEMENT          UPDATE LENGTH                                
         MVI   RSTLN,RSTLN3Q                                                    
         GOTO1 ADDELEM                                                          
         BAS   RE,GETELIO                                                       
         BE    VSTAT40                                                          
         DC    H'0'                                                             
*                                                                               
VSTAT40  LA    R2,ACF1H                                                         
*                                                                               
         XR    R1,R1               ZERO HOB OF R1 FOR VALIDATE ALL              
         GOTO1 VALFLTS                                                          
*                                                                               
         CLC   THISLEDG,PRODLEDG   IS THIS THE SJ ACCOUNT?                      
         BNE   VSTAT80             NO, DON'T TOUCH LOCK THEN                    
*                                                                               
         MVC   CHGRSTAT,RSTSTAT                                                 
         MVI   ERROR,INVALID                                                    
         LA    R2,JBRSTATH                                                      
         CLI   5(R2),0                                                          
         BE    VSTAT80                                                          
         CLI   5(R2),1                                                          
         BH    ERREXIT                                                          
         OI    RSTSTAT,X'20'      SET IT TO LOCK                                
         CLI   8(R2),C'L'                                                       
         BNE   VSTAT60                                                          
         CLI   DRFTITMS,C'Y'       DO WE HAVE DRAFT TRANSACTIONS?               
         BNE   VSTAT80             NO                                           
         MVI   ERROR,LOCDRFT       YES, ERROR                                   
         B     ERREXIT                                                          
*                                                                               
VSTAT60  NI    RSTSTAT,X'FF'-X'20' UNLOCK IT                                    
         CLI   8(R2),C'U'                                                       
         BE    VSTAT80                                                          
         CLI   8(R2),C' '                                                       
         BNE   ERREXIT                                                          
*                                                                               
VSTAT80  XC    CHGRSTAT,RSTSTAT    CHGRSTAT = CHANGE IN STATUS                  
         NI    CHGRSTAT,RSTSACIC+RSTSACIL                                       
         CLI   RSTEL,0                                                          
         BNE   OKEXIT              STATUS EL ALREADY EXISTS                     
         MVI   RSTEL,ACSTELQ       NEW ELEMENT                                  
         MVI   RSTCOSTG,C' '                                                    
         MVC   RSTTDATE,TODAYP                                                  
         MVC   RSTBDATE,TODAYP                                                  
         GOTO1 ADDELEM                                                          
         B     OKEXIT                                                           
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*                         VALIDATE LEDGER SECURITY                    *         
***********************************************************************         
*                                                                               
VALSEC   NTR1                                                                   
         CLI   MODE,VALREC         ARE WE VALIDATING A FIELD ?                  
         BNE   VALSECX             NO, RETURN                                   
         CLI   5(R2),0             YES, IS ANYTHING THERE ?                     
         BNE   VALSEC2             YES                                          
         CLI   ACTNUM,ACTADD       NO, IS THIS AN ADD ?                         
         BE    VALSECX             YES, NOTHING TO VALIDATE THEN                
*                                                                               
VALSEC2  TM    4(R2),X'20'         WAS THIS FIELD CHANGED ?                     
         BO    VALSECX             NO, RETURN                                   
         MVC   CUL+1(2),THISLEDG   YES, CHECK THE SECURITY                      
         GOTO1 SETHEIR                                                          
*                                                                               
VALSECX  B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                         VALIDATE ACCOUNT NAME                       *         
***********************************************************************         
*                                                                               
         USING SCREEND,R2                                                       
         USING ACNAMED,R6                                                       
VALNAME  NTR1                                                                   
         MVI   ELCODE,ACNMELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   NAME020                                                          
         CLI   ACNAMH+5,0                                                       
         BE    NAME040                                                          
         XC    ELEMENT,ELEMENT                                                  
         GOTO1 REMELEM                                                          
*                                                                               
NAME020  SR    R1,R1                                                            
         ICM   R1,1,ACNAMH+5                                                    
         BZ    NAME060                                                          
         BCTR  R1,0                                                             
         LA    R6,ELEMENT          SET UP TO BUILD NEW ELEMENT.                 
         MVI   ACNMEL,ACNMELQ                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACNMNAME(0),ACNAM                                                
         AHI   R1,3                L'ELEMENT = L'NAME + 3.                      
         STC   R1,ACNMLEN                                                       
         GOTO1 ADDELEM             ADD THE NEW ELEMENT.                         
         B     OKEXIT                                                           
*                                                                               
NAME040  SR    R1,R1                                                            
         IC    R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     OKEXIT                                                           
         MVC   ACNAM(0),ACNMNAME                                                
*                                                                               
         USING JBRSRNMH,R2                                                      
NAME060  LA    RF,JBRSJNMH         IF IT'S CLI/PRO/JOB, EXIT NOW                
         CR    R2,RF                                                            
         BE    OKEXIT                                                           
         SR    R1,R1                                                            
         IC    R1,JBRSJNMH+5       IF THERE'S BEEN NO NAME INPUT TO             
         STC   R1,JBRSRNMH+5       THIS ACCOUNT, AND IT HAD NO NAME             
         OI    JBRSRNMH+6,X'80'    ELEMENT, USE THE NAME INPUT TO               
         BCTR  R1,0                THE CLIENT/PRODUCT/JOB.                      
         EX    R1,*+8                                                           
         B     NAME020                                                          
         MVC   JBRSRNM(0),JBRSJNM                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*                      VALIDATE FOOT COMMENTS                         *         
***********************************************************************         
*                                                                               
         USING JBRFT1H,R2                                                       
         USING ACOMMD,R6                                                        
VALFOOT  NTR1                                                                   
         LA    R6,ELEMENT                                                       
         CLI   JBRFT1H+5,0                                                      
         BE    OKEXIT                                                           
         MVI   ACOMTYPE,0                                                       
         GOTO1 SCANNER,DMCB,JBRFT1H,BLOCK,0                                     
         SR    R0,R0                                                            
         ICM   R0,1,DMCB+4                                                      
         BZ    VFT020                                                           
         CLI   BLOCK+1,0                                                        
         BNE   VFT040                                                           
*                                                                               
VFT020   SR    R1,R1                                                            
         IC    R1,JBRFT1H+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACOMMENT(0),JBRFT1                                               
         LA    R1,5(R1)                                                         
         STC   R1,ACOMLEN                                                       
         GOTO1 ADDELEM                                                          
         SR    R1,R1                                                            
         IC    R1,ACOMSEQ                                                       
         LA    R1,1(R1)                                                         
         STC   R1,ACOMSEQ                                                       
         B     OKEXIT                                                           
*                                                                               
VFT040   LA    R5,BLOCK                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'                                                        
         MVC   KEY+1(1),CUL                                                     
*                                                                               
VFT060   MVI   ACOMLEN,10                                                       
         MVI   ACOMTYPE,X'44'                                                   
         CLC   12(3,R5),=C'EST='                                                
         BE    VFT080                                                           
         MVI   ACOMTYPE,X'84'                                                   
         CLC   12(3,R5),=C'BIL'                                                 
         BE    VFT080                                                           
         CLC   12(3,R5),=C'B+E'                                                 
         BNE   VFT100                                                           
         OI    ACOMTYPE,X'40'                                                   
*                                                                               
VFT080   MVC   ACOMMENT,BLANKS                                                  
         SR    R1,R1                                                            
         ICM   R1,1,1(R5)                                                       
         BZ    VFT100                                                           
         CLI   1(R5),6             MUST BE 1-6 BYTES LONG                       
         BH    VFT100                                                           
         LA    RF,6                                                             
         SR    RF,R1                                                            
         LA    RF,ACOMMENT(RF)                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),22(R5)                                                   
         MVC   KEY+2(6),ACOMMENT                                                
         MVC   AIO,AIO2              USE OTHER BUFFER TO READ                   
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(42),KEY                                                  
         BNE   VFT100                                                           
         MVC   AIO,AIO1              GET JOB RECORD BACK                        
         GOTO1 ADDELEM                                                          
         SR    R1,R1                                                            
         IC    R1,ACOMSEQ                                                       
         LA    R1,1(R1)                                                         
         STC   R1,ACOMSEQ                                                       
         LA    R5,32(R5)                                                        
         BCT   R0,VFT060                                                        
         B     OKEXIT                                                           
*                                                                               
VFT100   MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*              BUILD A BLANK ACCOUNT RECORD IN THIS RECORD AREA.                
***********************************************************************         
*                                                                               
         USING ACKEYD,R6                                                        
RECBUILD NTR1                                                                   
         LR    R6,R2               SAVE CURSOR POSITION                         
         L     R2,AIO              CLEAR AIO                                    
         LA    R3,1000                                                          
         SR    R1,R1                                                            
         MVCL  R2,R0                                                            
         LR    R2,R6                                                            
         L     R6,AIO                                                           
         SR    R1,R1                                                            
         IC    R1,5(R2)            R1 = L'ACCOUNT CODE INPUT.                   
         BCTR  R1,0                                                             
         MVC   ACKEYACC(42),BLANKS   INIT KEY.                                  
         MVC   ACLENGTH,=H'50'                                                  
         MVC   ACKEYACC(1),CUL                                                  
         MVC   ACKEYACC+1(2),THISLEDG                                           
         EX    R1,*+8                                                           
         B     OKEXIT                                                           
         MVC   ACKEYACC+3(0),8(R2)                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                 ADD BALANCE AND PEEL ELEMENTS                       *         
***********************************************************************         
*                                                                               
         USING ACBALD,R6                                                        
BALPEEL  NTR1                                                                   
         XC    ELEMENT(ACBLLNQ),ELEMENT                                         
         MVI   ELEMENT,ACBLELQ                                                  
         MVI   ELEMENT+1,ACBLLNQ                                                
         LA    R6,ELEMENT                                                       
         ZAP   ACBLFRWD,=P'0'                                                   
         ZAP   ACBLDR,=P'0'                                                     
         ZAP   ACBLCR,=P'0'                                                     
         ZAP   ACBLURG,=P'0'                                                    
         GOTO1 ADDELEM                                                          
         USING APOELD,R6                                                        
         XC    ELEMENT(20),ELEMENT                                              
         MVI   APOEL,APOELQ                                                     
         MVI   APOLN,APOLN1Q                                                    
*        MVC   ELEMENT(2),=X'3314'                                              
         ZAP   APODR,=P'0'                                                      
         ZAP   APOCR,=P'0'                                                      
         GOTO1 ADDELEM                                                          
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                 DO MY OWN I/O FOR SR, 1C AND 29 RECORDS             *         
***********************************************************************         
*                                                                               
GETFUNC  NTR1                                                                   
         MVC   FUNCTION,DMWRT                                                   
         BAS   RE,VALACCT                                                       
         BE    OKEXIT                                                           
         BAS   RE,RECBUILD                                                      
         BAS   RE,BALPEEL                                                       
*        GOTOR ACTDRFT,DMCB,(RC)                                                
         MVC   FUNCTION,DMADD                                                   
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*           VALIDATE NAME AND STATUS AND ADD/UPDATE RECORD            *         
***********************************************************************         
*                                                                               
VALUPDT  NTR1                                                                   
         USING SCREEND,R2                                                       
         BAS   RE,VALNAME                                                       
         BAS   RE,VALSTAT                                                       
                                                                                
         CLC   FUNCTION,DMWRT         DID I UPDATE THE ACCOUNT                  
         BNE   VALUP02             NO, MUST BE AN ADD                           
         TM    4(R2),X'20'         WAS THE NAME CHANGED?                        
         BZ    VALUP02             YES, ADD THE PERSON ELEMENT                  
         LA    R0,5                CHECK FILTERS NOW                            
         LA    R2,ACF1H                                                         
         TM    4(R2),X'20'                                                      
         BZ    VALUP02                                                          
         SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R0,*-16                                                          
         B     VALUP06                                                          
         DROP  R2                                                               
                                                                                
         USING SCREEND,RF                                                       
VALUP02  LA    RF,JBR1CNMH                                                      
         CR    R2,RF                                                            
         BE    VALUP04                                                          
         LA    RF,ACF1H                                                         
         CR    R2,RF                                                            
         BNE   VALUP06                                                          
                                                                                
VALUP04  GOTO1 PERSIN                                                           
         BAS   RE,MANAGER                                                       
         GOTO1 VSAVPTRS,DMCB,(X'80',0),POINTERS                                 
         GOTO1 VCHGPTRS,DMCB,(X'80',AIO),POINTERS                               
         B     VALUP08                                                          
                                                                                
VALUP06  BAS   RE,MANAGER                                                       
                                                                                
VALUP08  CLC   FUNCTION,DMWRT         DID I UPDATE THE ACCOUNT                  
         BNE   VALUPX              NO                                           
         GOTO1 =A(CHKNAME),DMCB,(RC),AIO,OLDACCNM,RR=MYRELO                     
                                                                                
VALUPX   B     OKEXIT                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
*             ADD OR UPDATE RECORD, DEPENDING ON FUNCTION             *         
***********************************************************************         
*                                                                               
MANAGER  NTR1                                                                   
         L     R6,AIO                                                           
         GOTO1 DATAMGR,DMCB,FUNCTION,=C'ACCFIL ',(R6),(R6),DMWORK               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                 BUMP UP TO NEXT USER SELECT FIELD                   *         
***********************************************************************         
*                                                                               
UBUMP    SR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*                      FORMAT KEY AND READ HIGH                       *         
***********************************************************************         
*                                                                               
READHIU  NTR1                                                                   
         LA    R6,KEY                                                           
         USING ACUFKEY,R6                                                       
         MVC   ACUFKEY,USERKEY                                                  
         MVI   ACUFRTYP,ACUFEQU                                                 
         MVI   ACUFSREC,ACUFSEQU                                                
         MVC   ACUFCUL,CUL                                                      
         MVC   ACUFCUL+1(2),=C'SJ'                                              
         GOTO1 HIGH                                                             
         CLC   ACUFKEY,KEYSAVE                                                  
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                  GET USER SELECT HEADINGS INTO TABLE                *         
***********************************************************************         
*                                                                               
         USING ELMTABD,R3                                                       
         USING ACUFD,R6                                                         
USERTABL NTR1                                                                   
         MVI   ELCODE,ACUFELQ                                                   
         BAS   RE,GETELIO                                                       
         B     *+8                                                              
*                                                                               
USET020  BAS   RE,NEXTEL                                                        
         BNE   OKEXIT                                                           
         TM    ACUFSTAT,X'04'      DISPLAY ON AUTH?                             
         BO    USET020             SKIP IT, WE PRINT THESE ELSEWHERE            
         CLC   ACUFDESC,BLANKS                                                  
         BE    USET020                                                          
         L     R3,AELMTAB                                                       
         LA    R0,20                                                            
*                                                                               
USET040  OC    ELMDATA,ELMDATA     IS THIS SPOT FILLED?                         
         BZ    USET080             NO, KEEP LOOKING                             
         CLC   ACUFCODE,ELMCODE    YES, DO THE CODES MATCH ?                    
         BNE   USET080             NO                                           
         OC    ACUFCUT,ACUFCUT     YES, DO WE HAVE A CUTOFF DATE ?              
         BZ    USET060             NO, REPLACE ELEMENT                          
         CLC   ACUFCUT,SVSTART     YES, CAN WE TAKE IT ?                        
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
*                                                                               
USET080  LA    R3,ELMLNG(R3)       GET NEXT                                     
         BCT   R0,USET040                                                       
*                                                                               
         L     R3,AELMTAB          NO MATCH, LOOK FOR VACANT SPOT               
         LA    R0,20                                                            
*                                                                               
USET100  OC    ELMDATA,ELMDATA     IS THIS SPOT FILLED ?                        
         BNZ   USET120             YES, KEEP LOOKING                            
         OC    ACUFCUT,ACUFCUT     NO, DO WE HAVE A CUTOFF DATE ?               
         BZ    USET110             NO, ADD ELEMENT                              
         CLC   ACUFCUT,SVSTART     YES, CAN WE TAKE IT ?                        
         BNH   USET020             NO                                           
*                                                                               
USET110  MVC   ELMDATA,0(R6)       ADD ELEMENT                                  
         AP    ELMCNT,=P'1'        KEEP A COUNT                                 
         B     USET020             GET NEXT ELEMENT                             
*                                                                               
USET120  LA    R3,ELMLNG(R3)       GET NEXT                                     
         BCT   R0,USET100                                                       
         B     OKEXIT                                                           
*                                                                               
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*                DISPLAY USER SELECT HEADINGS FROM TABLE              *         
***********************************************************************         
*                                                                               
USERHEAD NTR1                                                                   
         LA    R0,10                                                            
         LA    R2,JBRUSEHH                                                      
         L     R3,AELMTAB                                                       
*                                                                               
         USING JBRUSEHH,R2                                                      
         USING ELMTABD,R3                                                       
USEH020  OC    ELMDATA,ELMDATA                                                  
         BZ    USEH040                                                          
         MVC   JBRUSEH,6(R3)       MOVE HEADER TO SCREEN                        
         OI    JBRUSEHH+6,X'80'    TRANSMIT FIELD                               
         NI    JBRUSEHH+1,X'FF'-X'08'    MAKE SURE LOW INTENSITY                
         TM    ELMSTAT,X'80'       IS FIELD REQUIRED ?                          
         BZ    *+8                 NO                                           
         OI    JBRUSEHH+1,X'08'    YES, MAKE HIGH INTENSITY                     
         BAS   RE,UBUMP                                                         
         BAS   RE,UBUMP                                                         
*                                                                               
USEH040  LA    R3,ELMLNG(R3)                                                    
         BCT   R0,USEH020                                                       
         B     OKEXIT                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*                        GET USER SELECT DATA                         *         
***********************************************************************         
*                                                                               
         USING ACUFD,R6                                                         
USERDATA NTR1                                                                   
         MVI   ELCODE,ACUFELQ                                                   
         BAS   RE,GETELIO                                                       
         B     *+8                                                              
*                                                                               
USED020  BAS   RE,NEXTEL                                                        
         BNE   OKEXIT                                                           
         CP    ELMCNT,=P'0'                                                     
         BZ    OKEXIT                                                           
         LA    R0,10                                                            
         LA    R2,JBRUSEHH                                                      
         L     R3,AELMTAB                                                       
*                                                                               
         USING JBRUSEHH,R2                                                      
USED040  CLC   ACUFCODE,ACUFCODE-ACUFD(R3)  MATCH ON USER CODE                  
         BNE   USED060                                                          
         SR    R1,R1                                                            
         IC    R1,ACUFLEN          GET RECORD LENGTH                            
         SH    R1,=AL2(ACUFDATA-ACUFEL) SUBTRACT LENGTH LESS DATA               
         BZ    USED020             NO DATA, GET NEXT ELEMENT                    
         BCTR  R1,0                RESULT IS LENGTH OF DATA                     
         EX    R1,*+8                                                           
         B     USED020                                                          
         MVC   JBRUSED(0),ACUFDATA                                              
*                                                                               
USED060  BAS   RE,UBUMP            SKIP TO DATA                                 
         BAS   RE,UBUMP            SKIP TO NEXT HEADER                          
         LA    R3,ELMLNG(R3)       SKIP TO NEXT TABLE ENTRY                     
         BCT   R0,USED040                                                       
         B     USED020                                                          
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
*                         DO CLOSING                                  *         
***********************************************************************         
*                                                                               
CLOSEIT  LA    R2,JBRSJACH                                                      
         BAS   RE,VKEY                                                          
         GOTO1 JOBCLS                                                           
*                                                                               
         GOTO1 READ                REREAD UPDATED RECORD                        
         GOTOR MANDRAP,DMCB,(RC)                                                
         GOTOR MANJDTP,DMCB,(RC)                                                
         GOTOR MANPIDP,DMCB,(RC)                                                
         BAS   RE,DREC             REDISPLAY UPDATED RECORD                     
         GOTO1 VACSRCHP,DMCB,C'FTF ',AIO,0,0,ACOMFACS,AACCFACS                  
*                                                                               
         MVI   MYMSGNO1,IJCLSD     SET UP MESSAGE                               
*                                                                               
         B     VLABEL              SEE IF WE NEED LABELS                        
*                                                                               
CLOSEX   B     INFEXIT             PRINT THE MESSAGE                            
         EJECT                                                                  
***********************************************************************         
*                        REOPEN JOB                                   *         
***********************************************************************         
*                                                                               
OPENIT   LA    R2,JBRSJACH                                                      
         BAS   RE,VKEY                                                          
         GOTO1 JOBOPN                                                           
*                                                                               
         GOTO1 READ                                                             
         GOTOR MANDRAP,DMCB,(RC)                                                
         GOTOR MANJDTP,DMCB,(RC)                                                
         GOTOR MANPIDP,DMCB,(RC)                                                
         BAS   RE,DREC             REDISPLAY UPDATED RECORD                     
         GOTO1 VACSRCHP,DMCB,C'FTF ',AIO,0,0,ACOMFACS,AACCFACS                  
*                                                                               
         MVI   MYMSGNO1,IJOPND     SET UP MESSAGE                               
*                                                                               
         B     VLABEL              SEE IF WE NEED LABELS                        
         EJECT                                                                  
***********************************************************************         
*                        DELETE JOB                                   *         
***********************************************************************         
*                                                                               
DELETIT  LA    R2,JBRSJACH                                                      
         BAS   RE,VKEY                                                          
         GOTO1 JOBDEL                                                           
*                                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         GOTO1 READ                GET RECORD IN BUFFER                         
         NI    DMINBTS,X'F7'                                                    
*                                                                               
         BAS   RE,DREC             DISPLAY UPDATED RECORD                       
         GOTOR NAMESRCH,DMCB,(RC)                                               
*                                                                               
         MVI   MYMSGNO1,IJDELD     SET UP MESSAGE                               
*                                                                               
         B     VLABEL              SEE IF WE NEED LABELS                        
         EJECT                                                                  
***********************************************************************         
*                       RESTORE JOB                                   *         
***********************************************************************         
*                                                                               
RESTIT   LA    R2,JBRSJACH                                                      
         BAS   RE,VKEY                                                          
         GOTO1 JOBRES                                                           
*                                                                               
         GOTOR MANDRAP,DMCB,(RC)                                                
         GOTOR MANPIDP,DMCB,(RC)                                                
*                                                                               
         BAS   RE,DREC             DISPLAY UPDATED RECORD                       
         GOTOR NAMESRCH,DMCB,(RC)                                               
*                                                                               
         MVI   MYMSGNO1,IJRESD     SET UP MESSAGE                               
*                                                                               
         B     VLABEL              SEE IF WE NEED LABELS                        
         EJECT                                                                  
* SUB-ROUTINE TO SET KEY CLIENT, PRODUCT, AND JOB FIELDS FROM                   
* AN ACCOUNT KEY--CALLED FROM DELETE AND RESTORE S/RS                           
*                                                                               
* AT ENTRY, R1=A(CLIENT/PRODUCT/JOB) AND SAVEKEY=JOB KEY                        
*           R3=NEXT POSITION IN SAVEKEY                                         
MVCCLI   LA    R3,SAVEKEY+3        RE=A(CLIENT)                                 
         ZIC   RF,LCLI                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R3)                                                    
         LA    R1,6(R1)            POINT TO PRODUCT                             
         LA    R3,1(RF,R3)         ADVANCE IN KEY                               
         BR    RE                                                               
*                                                                               
MVCPRO   ZIC   RF,LPRO                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R3)                                                    
         LA    R1,6(R1)            POINT TO JOB                                 
         LA    R3,1(RF,R3)         ADVANCE IN JOB KEY                           
         BR    RE                                                               
*                                                                               
MVCJOB   ZIC   RF,LJOB                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R3)                                                    
         BR    RE                                                               
         EJECT                                                                  
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
OKEXIT   CR    R8,R8                                                            
EXIT     XIT1                                                                   
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
NOREST   MVI   ERROR,RECNTDEL                                                   
         B     ERREXIT                                                          
*                                                                               
NOMORE   MVI   ERROR,NOMORE#                                                    
         B     ERREXIT                                                          
*                                                                               
BADNUM   MVI   ERROR,INVALID                                                    
*                                                                               
BADOFG   LA    R5,WORK                                                          
         SR    R3,R5                                                            
         STC   R3,ERRNDX                                                        
*                                                                               
         LA    R0,L'JBRSJAC                                                     
         LA    R3,8(R2)                                                         
*                                                                               
BADNUM2  CLI   0(R3),X'00'         NEED TO FILL ZEROS WITH SPACES               
         BNE   *+8                                                              
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         BCT   R0,BADNUM2                                                       
*                                                                               
         MVI   6(R2),X'80'                                                      
         B     ERREXIT                                                          
*                                                                               
NODELETE MVI   ERROR,CANTDEL                                                    
*                                                                               
ERREXIT  GOTO1 VERRCUR                                                          
*                                                                               
INFEXIT  ST    R2,ACURFORC                                                      
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 INFOXIT                                                          
*                                                                               
MYRELO   DS    A                   RELO                                         
         EJECT                                                                  
HEDTAB   DC    Y(JBRHED1H-CONTAGH)      DISP TO HEAD COMMENT FIELDS             
         DC    Y(JBRHED2H-CONTAGH)                                              
         DC    Y(JBRHED3H-CONTAGH)                                              
HEDTABN  EQU   (*-HEDTAB)/L'HEDTAB                                              
*                                                                               
STATTAB  DC    AL2(ACSTFILT-ACSTATD)                                            
         DC    AL2(ACSTFILT+1-ACSTATD)                                          
         DC    AL2(ACSTANAL-ACSTATD)                                            
         DC    AL2(ACSTSUB-ACSTATD)                                             
         DC    X'FF'                                                            
*                                                                               
DMADD    DC    CL8'DMADD'                                                       
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
BLANKS   DC    CL255' '                                                         
NINES    DC    CL5'99999'                                                       
*                                                                               
EFFS     DC    8XL1'FF'                                                         
*                                                                               
REGPFS   DC    X'01',AL1(7+78),AL1(1),AL1(02),AL1(78),X'28',AL1(0)              
         DC    CL78'PF1=OPTM,PF2=OPTL,PF3=EST,PF4=ELIST,PF5=SUM,PF6=LINX        
               K,PF8=JOB2,PF12=RETURN'                                          
         DC    X'00'                                                            
*                                                                               
AUTPFS   DC    X'01',AL1(7+78),AL1(1),AL1(02),AL1(78),X'28',AL1(0)              
         DC    CL78'PF1=OPTM,PF2=OPTL,PF3=EST,PF4=ELIST,PF5=SUM,PF6=LINX        
               K,PF7=AUT,PF8=JOB2,PF12=RET'                                     
         DC    X'00'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* UPDATE JOB NAME SEARCH PASSIVE RECORDS                                        
*********************************************************************           
*                                                                               
NAMESRCH NMOD1 0,*NMESRC*                                                       
         L     RC,0(R1)                                                         
         L     R2,AIO                                                           
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+1(3),=CL3'TF '                                              
         MVI   DMCB,C'A'           ADD                                          
         CLI   ACTNUM,ACTADD                                                    
         BE    NSRCH04                                                          
         MVI   DMCB,C'R'           RESTORE                                      
         CLI   ACTNUM,ACTNRES                                                   
         BE    NSRCH04                                                          
         MVI   DMCB,C'D'           DELETE                                       
         TM    ACCOSTAT(R2),X'80'                                               
         BO    NSRCH04                                                          
         MVI   DMCB,C'C'           CHANGE                                       
         TM    NAMEFLAG,NFCHANGE                                                
         BNZ   NSRCH04                                                          
         MVI   DMCB,C'F'           REFRESH STATUS                               
         TM    ACSTATUS-ACKEYD(R2),ACTSDRFT                                     
         BZ    *+12                HAS IT BEEN CHANGED TO APPROVED?             
         TM    CHGRSTAT,RSTSACIC+RSTSACIL                                       
         BZ    NSRCHX                                                           
*                                                                               
NSRCH04  GOTO1 ACSRCHP,DMCB,,AIO,,OLDJOBNM,ACOMFACS,AACCFACS                    
         NI    NAMEFLAG,X'FF'-NFCHANGE                                          
*                                                                               
NSRCHX   XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                         DISPLAY CLOSING DATE                        *         
***********************************************************************         
*                                                                               
DISCLOSE NMOD1 0,*DCLOSE*                                                       
         L     RC,0(R1)                                                         
         USING JOBELD,R6                                                        
         MVI   ELCODE,JOBELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   DCLOSEX                                                          
         MVC   SVSTART,JOBADATE                                                 
         OI    JBRCLOSH+4,X'20'    INDICATE VALIDATED                           
         MVC   JBRCLOS,BLANKS      CLEAR OUTPUT FIELD                           
         GOTO1 DATCON,DMCB,(1,JOBCDATE),(8,JBRCLOS)                             
*                                                                               
DCLOSEX  XMOD1                                                                  
         LTORG                                                                  
*                                                                               
***********************************************************************         
*                         DISPLAY OPEN DATE                           *         
***********************************************************************         
*                                                                               
DISOPEN  NMOD1 0,*DOPEN**                                                       
         L     RC,0(R1)                                                         
         USING JOBELD,R6                                                        
         MVI   ELCODE,JOBELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   DOPENX                                                           
         OI    JBROPENH+4,X'20'     INDICATE VALIDATED                          
         MVC   JBROPEN,BLANKS       CLEAR OUTPUT FIELD                          
         CLI   JOBLN,JOBLN2Q       ELEMENT TOO SMALL FOR OPENED DATE?           
         BL    DOPENX              YES                                          
         NC    JOBODATE,JOBODATE   IS THERE AN OPENED DATE ?                    
         BZ    DOPENX              NO                                           
         MVC   SVSTART,JOBODATE                                                 
         GOTO1 DATCON,DMCB,(1,JOBODATE),(8,JBROPEN)                             
*                                                                               
DOPENX   XMOD1                                                                  
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*                CHECK FOR NEW ESTIMATES EXISTING                               
*********************************************************************           
         USING EVERECD,R2                                                       
CHKNEW   NMOD1 0,*CHKNEW*                                                       
         L     RC,0(R1)                                                         
         LA    R2,KEY                                                           
         XC    EVEKEY,EVEKEY                                                    
         MVI   EVEKTYP,EVEKTYPQ                                                 
         MVI   EVEKSUB,EVEKSUBQ                                                 
         MVC   EVEKCPY(3),CUL                                                   
         MVC   EVEKCLI,CLICODE                                                  
         MVC   EVEKPRO,PRODCODE                                                 
         MVC   EVEKJOB,JBRSJAC                                                  
         OC    EVEKJOB,BLANKS                                                   
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         CLC   KEYSAVE(EVEKTYPE-EVERECD),EVEKEY                                 
         BNE   CHKNEWER                                                         
                                                                                
CHKNEWOK MVI   DUB,0                                                            
         B     *+8                                                              
                                                                                
CHKNEWER MVI   DUB,1                                                            
         CLI   DUB,1                                                            
         XMOD1                                                                  
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*                CHECK FOR MCS ESTIMATES EXISTING                               
*********************************************************************           
         USING ESTRECD,R2                                                       
CHKMCS   NMOD1 0,*CHKMCS*                                                       
         L     RC,0(R1)                                                         
         LA    R2,KEY                                                           
         XC    ESTKEY,ESTKEY                                                    
         MVI   ESTKTYP,ESTKTYPQ                                                 
         MVI   ESTKSUB,ESTKSUBQ                                                 
         MVC   ESTKCPY,CUL                                                      
         MVC   ESTKCLI,CLICODE                                                  
         MVC   ESTKPRO,PRODCODE                                                 
         MVC   ESTKJOB,JBRSJAC                                                  
         OC    ESTKJOB,BLANKS                                                   
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         CLC   KEYSAVE(ESTKLNO-ESTRECD),ESTKEY                                  
         BNE   CHKMCSER                                                         
                                                                                
CHKMCSOK MVI   DUB,0                                                            
         B     *+8                                                              
                                                                                
CHKMCSER MVI   DUB,1                                                            
         CLI   DUB,1                                                            
         XMOD1                                                                  
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                           PREPARE FOR ESTIMATES TO BE ADDED         *         
***********************************************************************         
*                                                                               
PREADD   NMOD1 0,*GETTAB*                                                       
         L     RC,0(R1)                                                         
         MVC   AIO,AIO2            USE IO2 FOR THIS                             
         LA    R5,1                GET READY TO ADD SOMETHING                   
         CLI   GOADDREV,C'Y'       ADD AN 'R' ESTIMATE ?                        
         BNE   PREADD2             NO                                           
         MVI   ADDTYPE,C'R'        YES, AGO ADD IT                              
         BAS   RE,ADDEST                                                        
*                                                                               
PREADD2  SR    R5,R5                                                            
         ICM   R5,1,GOADDPLN                                                    
         BZ    PREADDX             NO 'P' ESTIMATES TO ADD                      
         MVI   ADDTYPE,C'P'                                                     
         BAS   RE,ADDEST                                                        
*                                                                               
PREADDX  MVC   AIO,AIO1            RESTORE IO AREA                              
         XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                           ADD ESTIMATES, IF REQUIRED                *         
***********************************************************************         
*                                                                               
         USING ACKEYD,R6                                                        
ADDEST   NTR1  ,                                                                
*                                                                               
ADDEST2  LR    R6,R2               SAVE CURSOR POSITION                         
         L     R2,AIO              CLEAR AIO                                    
         LA    R3,1000                                                          
         SR    R1,R1                                                            
         MVCL  R2,R0                                                            
         LR    R2,R6               RESTORE CURSOR POSITION                      
*                                                                               
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
         USING ACEUD,R1                                                         
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         MVI   ACEUEL,ACEUELQ      BUILD AN ESTIMATE UPDATE ELEMENT             
         MVI   ACEULEN,ACEULENQ                                                 
         MVC   ACEUADD,TODAYP                                                   
         MVC   ACEUPERS,TWAALIAS                                                
         MVC   ACEULAST,TODAYP                                                  
         GOTO1 ADDELEM                                                          
*                                                                               
         MVC   FUNCTION,DMADD                                                   
         BAS   RE,MANAGER                                                       
         BCT   R5,ADDEST2                                                       
*                                                                               
ADDESTX  B     OKEXIT                                                           
         DROP  R1,R6                                                            
         EJECT                                                                  
***********************************************************************         
*              LOAD THE BUFFER WHERE ELMTAB WILL BE SAVED             *         
***********************************************************************         
*                                                                               
LOAD     NMOD1 0,**LOAD**                                                       
         L     RC,0(R1)                                                         
         L     RE,=V(DUMMY)                                                     
         A     RE,MYRELO                                                        
         ST    RE,DMCB             SET LOAD POINT FOR BUFFER                    
         MVC   DMCB+4(3),SYSPHASE                                               
         MVI   DMCB+7,X'50'        SET SET BUFFER OVERLAY NUMBER                
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'        IS LOAD OK ?                                 
         BNE   *+6                 YES                                          
         DC    H'0'                NO, BLOW UP                                  
         MVC   AELMTAB,DMCB        SAVE THE ADDRESS                             
         XMOD1                                                                  
*                                                                               
***********************************************************************         
*              PUT THE ELMTAB TO TWA 2                                          
***********************************************************************         
*                                                                               
PUTTAB   NMOD1 0,*PUTTAB*                                                       
         L     RC,0(R1)                                                         
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,2            PAGE = TWA2                                  
         MVC   DMCB+10(2),TERM     TERMINAL NUMBER                              
         GOTO1 DATAMGR,DMCB,DMWRT,=C'TEMPSTR',,AELMTAB                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XMOD1                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
*              GET THE ELMTAB BACK FROM TWA2                          *         
***********************************************************************         
*                                                                               
GETTAB   NMOD1 0,*GETTAB*                                                       
         L     RC,0(R1)                                                         
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,2                                                         
         MVC   DMCB+10(2),TERM                                                  
         GOTO1 DATAMGR,DMCB,DMREAD,=C'TEMPSTR',,BUFF                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,BUFF                                                          
         ST    RE,AELMTAB                                                       
         XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                     FIND APPROPRIATE JOB NUMBER RECORD              *         
***********************************************************************         
*                                                                               
         USING USERD,R5                                                         
FINDJOB  NMOD1 0,*FNDJOB*                                                       
         L     RC,0(R1)                                                         
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
         MVI   ERROR,NOJNUM        NO JNUMBER RECORD                            
         B     ERREXIT                                                          
*                                                                               
         USING ACJNKEY,R6                                                       
FIND26   ST    RE,SAVERE                                                        
*                                                                               
FIND26A  L     R6,AIO                                                           
         TM    ACJNSTAT,ACJNDACT   IS THE RECORD ACTIVE ?                       
         BZ    FIND27              YES, SEE IF WE HAVE THE RIGHT DATE           
         GOTO1 SEQ                 NO, KEEP READING                             
         CLC   ACJNKEY(ACJNEFF-ACJNRTYP),KEYSAVE                                
         BE    FIND26A             SAME KEY                                     
         L     RE,SAVERE           NEW KEY, GET NEXT LEVEL                      
         B     4(RE)                                                            
*                                                                               
FIND27   CLC   ACJNEFF,EFFDATE     ACTIVE RECORD, DATE OK ?                     
         BNL   FIND28              YES                                          
*                                                                               
FIND27A  GOTO1 SEQ                 NO, KEEP LOOKING                             
         CLC   ACJNKEY(ACJNEFF-ACJNRTYP),KEYSAVE                                
         BNE   FIND24              NO MORE AT THIS LEVEL ERROR                  
         TM    ACJNSTAT,ACJNDACT   IS THE RECORD ACTIVE ?                       
         BZ    FIND27              YES, CHECK THE DATE                          
         B     FIND27A                                                          
*                                                                               
         USING ACJND,R6                                                         
FIND28   MVI   ELCODE,ACJNELQ      YES, GET ELEMENT                             
         BAS   RE,GETELIO                                                       
         BNE   FIND24                                                           
         ST    R6,SAVER6                                                        
         LA    R3,WORK+1           GET SECOND POSITION OF JOB                   
         LA    R0,5                ONLY 5 POSITIONS LEFT                        
         XC    ASEQNUM,ASEQNUM     CLEAR STARTING SEQUENCE #                    
*                                                                               
         USING NUMTABD,R5                                                       
FIND30   LA    R5,NUMTAB                                                        
*                                                                               
FIND32   CLI   NUMVAL,X'FF'        END OF TABLE ?                               
         BE    FIND34              MUST BE CONSTANT                             
         CLC   ACJNP2,NUMVAL       DO VALUES MATCH ?                            
         BE    FIND34              YES                                          
         LA    R5,NUMLEN(R5)       NO, KEEP LOOKING                             
         B     FIND32                                                           
*                                                                               
FIND34   OC    NUMADDR,NUMADDR     DO WE HAVE A SPECIAL ROUTINE ?               
         BNZ   FIND36              YES                                          
         CLI   0(R3),C' '          NO, FIELD MUST BE BLANK TO START             
         BH    BADNUM                                                           
         MVC   *+10(2),NUMMOVE     MOVE IN REQUIRED DATA                        
         MVC   0(1,R3),0(R0)                                                    
         B     FIND38                                                           
*                                                                               
FIND36   MVC   *+8(2),NUMADDR      GET ADDRESS OF SPECIAL ROUTINE               
         BAS   RE,0(,0)            AND BRANCH TO IT                             
*                                                                               
FIND38   LA    R6,L'ACJNP2+L'ACJNP2N(R6)   GET NEXT POSITION IN ELEM            
         LA    R3,1(R3)            GET NEXT POSITION IN JOB                     
         BCT   R0,FIND30                                                        
         XC    SAVE1ST,SAVE1ST                                                  
*                                                                               
FIND40   L     R6,SAVER6           GET BACK TO CORRECT SPOT                     
         BAS   RE,GETSEQ                                                        
         MVC   ACJNLNUM(3),CLICODE                                              
         MVC   ACJNLNUM+3(3),PRODCODE                                           
         MVC   ACJNLNUM+6(L'JOBNUM),WORK                                        
         OC    SAVE1ST,SAVE1ST     DID WE SAVE THE FIRST JOB # ?                
         BZ    FIND42              NO                                           
         CLC   SAVE1ST,WORK                                                     
         BE    NOMORE                                                           
         B     *+10                                                             
*                                                                               
FIND42   MVC   SAVE1ST,WORK                                                     
         MVC   AIO,AIO3                                                         
         MVC   KEY,BLANKS                                                       
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(3),CLICODE                                                 
         MVC   KEY+6(3),PRODCODE                                                
         MVC   KEY+9(6),WORK                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(ACKEYWRK-ACKEYD),KEYSAVE                                     
         BE    FIND40                                                           
         MVC   AIO,AIO2                                                         
         GOTO1 WRITE                                                            
         MVC   AIO,AIO1                                                         
         XMOD1                                                                  
*                                                                               
GETOFG   MVI   ERROR,NOOFG         GET READY FOR ERROR                          
         OC    EFFOFG,EFFOFG       DO WE HAVE AN OFFICE GROUP                   
         BZ    BADOFG              NO                                           
         MVC   0(1,R3),EFFOFG      YES, USE IT                                  
         BR    RE                                                               
*                                                                               
GETOF2   MVI   ERROR,NOOFF2        GET READY FOR ERROR                          
         CLI   EFFOFFC+1,C' '      DO WE HAVE A SECOND CHARACTER ?              
         BNH   BADOFG              NO                                           
         MVC   0(1,R3),EFFOFFC+1   YES, USE IT                                  
         BR    RE                                                               
*                                                                               
USERSUP  CLI   0(R3),C' '          USER SUPPLIED FIELD, MUST HAVE DATA          
         BNH   BADNUM                                                           
         BR    RE                                                               
*                                                                               
NEXTNUM  CLI   0(R3),C' '          SEQUENTIAL FIELD, MUST NOT HAVE DATA         
         BH    BADNUM                                                           
         OC    ASEQNUM,ASEQNUM     IS THIS THE FIRST TIME ?                     
         BNZR  RE                  NO                                           
         ST    R3,ASEQNUM          YES, SAVE THIS STOP                          
         BR    RE                                                               
         DROP  R5,R6                                                            
*                                                                               
         DS    0D                                                               
NUMTAB   DC    C'US',S(0),S(USERSUP)                                            
         DC    C'SN',S(0),S(NEXTNUM)                                            
         DC    C'OC',S(EFFOFFC),S(0)                                            
         DC    C'O2',S(0),S(GETOF2)                                             
         DC    C'OG',S(0),S(GETOFG)                                             
         DC    C'CY',S(TODAY+1),S(0)                                            
         DC    C'OY',S(JDATE+1),S(0)                                            
         DC    C'NA',S(BLANKS),S(0)                                             
         DC    X'FF',S(20(R6)),S(0)                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                      FORMAT KEY AND READ HIGH FOR JOB NUMBER RECORDS*         
***********************************************************************         
*                                                                               
READHIJ  NTR1                                                                   
         LA    R6,KEY                                                           
         USING ACJNKEY,R6                                                       
         MVC   ACJNKEY,USERKEY                                                  
         MVI   ACJNRTYP,ACJNEQU                                                 
         MVI   ACJNSREC,ACJNSEQU                                                
         MVC   ACJNCUL,CUL                                                      
         GOTO1 HIGH                                                             
         CLC   ACJNKEY(ACJNEFF-ACJNRTYP),KEYSAVE                                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                      GET NEXT SEQUENTIAL NUMBER FOR JOB             *         
***********************************************************************         
*                                                                               
         USING ACJND,R6                                                         
GETSEQ   NTR1                                                                   
         MVC   AIO,AIO2            USE SECOND IO BUFFER                         
         L     R3,ASEQNUM          GET STARTING POSTION IN JOB KEY              
         MVC   START#,ACJNSTRT     NEED START TO DETERMINE LENGTH               
         GOTO1 SQUASHER,DMCB,START#,L'START#                                    
         SR    R2,R2                                                            
         IC    R2,DMCB+7           LENGTH OF SEQUENTIAL PORTION                 
         BCTR  R2,0                                                             
*                                                                               
         CLC   ACJNLNUM,BLANKS     IS THIS OUR FIRST TIME ?                     
         BH    GETS04              NO, NEXT LAST # USED                         
*                                                                               
GETS02   EX    R2,*+8              YES, USE  STARTING #                         
         B     *+10                                                             
         MVC   0(0,R3),START#                                                   
         B     GETS10                                                           
*                                                                               
GETS04   LR    R1,R3                                                            
         LA    RF,WORK                                                          
         SR    R1,RF                                                            
         LA    R1,ACJNLNUM+6(R1)                                                
         MVC   NEXT#,BLANKS                                                     
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   NEXT#(0),0(R1)                                                   
*                                                                               
GETS06   EX    R2,*+8              IF NEXT WILL BE OVERFLOW, START OVER         
         B     *+10                                                             
         CLC   NEXT#(0),NINES                                                   
         BE    GETS02                                                           
*                                                                               
         EX    R2,*+8               ADD ONE TO SEQUENTIAL                       
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
         CLC   NEXT#(0),ACJNMANS                                                
         BL    GETS08                                                           
         BE    GETS06                                                           
*                                                                               
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   NEXT#(0),ACJNMANE                                                
         BNH   GETS06                                                           
*                                                                               
GETS08   EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),NEXT#       VALID NUMBER, UPDATE JNUM RECORD             
*                                                                               
GETS10   B     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
GETEQU   NMOD1 0,*GETEQU*                                                       
         L     RC,0(R1)                                                         
         USING EQUIVD,R2                                                        
         LA    R2,JBXLAB1H                                                      
         LA    R1,EQUMAX                                                        
*                                                                               
GETE02   OI    EQUACCH+1,X'20'     PROTECT THE SCREEN FIRST                     
         LA    R2,EQULNQ(R2)                                                    
         BCT   R1,GETE02                                                        
*                                                                               
         MVC   AIO,AIO2                                                         
         MVC   KEY,BLANKS          CLEAR KEY                                    
         MVC   KEY(3),CUL          COMPANY, UNIT, LEDGER                        
         GOTO1 READ                                                             
*                                                                               
         MVI   ELCODE,APRELQ       ANY RULES RECORDS?                           
         BAS   RE,GETELIO                                                       
         BNE   GETEXIT             NO, DO ACCOUNT MEMO                          
*                                                                               
         USING APRELD,R6                                                        
GETE04   LA    R2,JBXEQU1H         YES, FIND CORRECT ENTRY                      
         SR    R1,R1               SEQUENCE NUMBER                              
         IC    R1,APRSEQ           TIMES LENGTH OF AN ENTRY                     
         MHI   R1,JBXEQU2H-JBXEQU1H                                             
         AR    R2,R1               ADDED TO FIRST ENTRY                         
         MVC   8(L'APRDESC,R2),APRDESC                                          
         OI    6(R2),X'80'         TRANSMIT IT                                  
         BAS   RE,BUMP             GET TO DATA FIELD                            
*                                                                               
GETE06   ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         BE    GETE08                                                           
         CLI   0(R6),APRELQ                                                     
         BE    GETE04                                                           
         B     GETE06                                                           
         DROP  R6                                                               
*                                                                               
GETE08   MVC   KEY,BLANKS                                                       
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(6),CLICODE    PRINT CLIENT DATA FIRST                      
         GOTO1 READ                                                             
         MVI   ELCODE,FFTELQ       READ THE DATA AND FILL IN THE SCREEN         
         BAS   RE,GETELIO                                                       
         BNE   GETE16                                                           
*                                                                               
         USING FFTELD,R6                                                        
GETE10   CLI   FFTTYPE,FFTTEPTR    TYPE 71                                      
         BE    GETE14                                                           
*                                                                               
GETE12   SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
*                                                                               
         CLI   0(R6),0                                                          
         BE    GETE16                                                           
         CLI   0(R6),FFTELQ        X'DB' FREE FORM TEXT                         
         BE    GETE10                                                           
         B     GETE12                                                           
*                                                                               
GETE14   LA    R2,JBXACC1H         POINT WHERE TO PUT THE ACCOUNT               
         SR    R1,R1                                                            
*                                                                               
         IC    R1,FFTSEQ                                                        
         MHI   R1,JBXACC2H-JBXACC1H                                             
         AR    R2,R1               ADD DISPLACEMENT TO IT                       
         LR    RE,R2                                                            
*                                                                               
         MVI   8(RE),C'('          PUT BRACKETS FOR THIS LEVEL                  
         LA    RE,1(RE)                                                         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,FFTDLEN          GET LENGTH OF ACCOUNT TO OUTPUT              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RE),FFTDATA                                                  
         LA    R1,1(R1)                                                         
         AR    RE,R1                                                            
         MVI   8(RE),C')'                                                       
         OI    6(R2),X'80'         TRANSMIT IT                                  
*        OI    1(R2),X'20'         PROTECT IT                                   
         B     GETE12              CHECK IF MORE EQUIVALENT ACCOUNTS            
*                                                                               
GETE16   CLC   KEYSAVE+6(3),BLANKS CLIENT OR PRODUCT                            
         BNE   GETEXIT             PRODUCT, DONE                                
         MVC   KEY,BLANKS          CLIENT, READ PRODUCT NOW                     
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(3),CLICODE                                                 
         MVC   KEY+6(3),PRODCODE                                                
         GOTO1 READ                                                             
         MVI   ELCODE,FFTELQ       READ THE DATA AND FILL IN THE SCREEN         
         BAS   RE,GETELIO                                                       
         BE    GETE10                                                           
*                                                                               
GETEXIT  XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*             SAVE THE NAME ELEMENT OF THE ACCOUNT IN AIO IN 0(P2)    *         
***********************************************************************         
         SPACE 1                                                                
SVNAME   NMOD1 0,SVNAME                                                         
         L     RC,0(R1)                                                         
         L     R3,4(R1)                                                         
*                                                                               
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETELIO1                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R6)                                                    
         XMOD1                                                                  
GETELIO1 L     R6,AIO                                                           
         GETELN (R6),DATADISP,ELCODE,1                                          
         SPACE 3                                                                
***********************************************************************         
*        IF I JUST UPDATED AN ACCOUNT, SEE IF I NEED CREATE NEW NAME  *         
*        POINTER (ANC) RECORD.                                       *          
*        P1 IS ASSUMED TO POINT TO THE RECORD YOU JUST WROTE          *         
***********************************************************************         
CHKNAME  NMOD1 0,CHKNAME                                                        
         L     RC,0(R1)                                                         
         CLI   EMULATE,C'Y'        AM I ONLY EMULATING IS ACC                   
         BNE   CHKNX               NO                                           
*                                                                               
         L     R6,4(R1)            P2 IS A( ACCOUNT JUST ADDED)                 
         ST    R6,FULL             SAVE IN CASE I NEED DA                       
         L     R2,8(R1)            P3 IS WHERE I SAVED THE OLD NAME             
*                                                                               
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETELIO2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   1(1,R2),1(R6)       WAS THE LENGTH CHANGED                       
         BNE   CHKN20              YES                                          
*                                                                               
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(R6)                                                    
         BE    CHKNX                                                            
*                                                                               
CHKN20   OI    NAMEFLAG,NFCHANGE                                                
         L     R6,FULL             RESET R6                                     
         BAS   RE,PUTANC                                                        
*                                                                               
CHKNX    XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
*        POINTER MAINTENANCE - THE NAME ELEMENT OF THE RECORD IN R6   *         
*        HAS BEEN CHANGED, CREATE A POINTER SO THE DUMP AND LOAD      *         
*        WILL FIND THE NEW NAME                                       *         
***********************************************************************         
PUTANC   NTR1                                                                   
         USING ACTRECD,R6                                                       
         L     R5,AIO3                                                          
         USING ANCRECD,R5                                                       
         MVI   ANCKEY,C' '                                                      
         MVC   ANCKEY+1(L'ANCKEY-1),ANCKEY                                      
         MVI   ANCKTYP,ANCKTYPQ                                                 
         MVC   ANCKCULA,ACTKCULA                                                
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),=CL8'ACCDIR',(R5),(R5),0             
         BE    PUTANCX             POINTER ALREADY EXISTS                       
*                                                                               
         MVC   0(L'ACTKEY,R5),ACTKEY   READ ACCOUNT KEY                         
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMREAD'),=CL8'ACCDIR',(R5),(R5),0         
         BE    *+6                                                              
         DC    H'0'                ACCOUNT NOT FOUND !!                         
         MVC   SVDA,ACTKDA-ACTRECD(R5) SAVE DISK ADDRESS                        
*                                                                               
         MVI   ANCKEY,C' '         CREATE POINTER TO NEW DA                     
         MVC   ANCKEY+1(L'ANCKEY-1),ANCKEY                                      
         MVI   ANCKTYP,ANCKTYPQ                                                 
         MVC   ANCKCULA,ACTKCULA                                                
         XC    ANCKSTA,ANCKSTA                                                  
         OI    ANCKSTA,ACTSDELT    WRITE AS DELETED FOR DUMP/LOAD               
         GOTO1 DATAMGR,DMCB,DMADD,=CL8'ACCDIR',(R5),(R5),0                      
*                                                                               
PUTANCX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
GETELIO2 L     R6,AIO                                                           
         GETELN (R6),DATADISP,ELCODE,2                                          
         EJECT                                                                  
***********************************************************************         
*                      SETUP FOR GETOPTS                              *         
***********************************************************************         
*                                                                               
SETGET   NMOD1 0,SETGET                                                         
         L     RC,0(R1)                                                         
         MVC   GOADM,DATAMGR                                                    
         LA    R1,GOXBLOCK                                                      
         ST    R1,GOAEXT                                                        
         LA    R1,GOBBLOCK                                                      
         ST    R1,GOABEXT                                                       
         XC    GOACLI,GOACLI                                                    
         XC    GOAPRO,GOAPRO                                                    
         XC    GOAJOB,GOAJOB                                                    
         XC    GOACOMP,GOACOMP                                                  
         XC    GOSELJOB,GOSELJOB                                                
         MVC   GOSELCUL,CUL                                                     
         MVC   GOSELCLI,BLANKS                                                  
         MVC   GOSELCLI,CLICODE                                                 
         MVC   GOSELPRO,BLANKS                                                  
         MVC   GOSELPRO,PRODCODE                                                
         MVI   GOWHICH,0                                                        
         MVI   GOANYMED,0                                                       
         MVI   GOSELLEV,0                                                       
         MVC   GOSELJOB,BLANKS                                                  
         MVI   GOSELMED,0                                                       
*                                                                               
         CLI   MODE,VALKEY         WHEN VALKEY AND ACTION AUTO                  
         BNE   SETG50              JOB FIELD NOT VALIDATED                      
*                                                                               
         CLC   CONACT(4),=C'AUTO'  IS THIS AUTO NUMBERING ?                     
         BNE   SETG50              NO, ASSUME JOB ENTERED                       
*                                                                               
         MVC   GOSELMED,JBRSJAC                                                 
         B     SETGX                                                            
*                                                                               
SETG50   SR    R1,R1                                                            
         IC    R1,JBRSJACH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELJOB(0),JBRSJAC                                              
*                                                                               
SETGX    XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
*                 ADD ACTIVITY AND DRAFT STATUS IF NECESSARY          *         
***********************************************************************         
*                                                                               
         USING ACKEYD,R6                                                        
ACTDRFT  NMOD1 0,ACTDRF                                                         
         L     RC,0(R1)                                                         
         L     R6,AIO                                                           
         CLC   1(2,R6),=C'SR'                                                   
         BE    ACTDRFT2                                                         
         CLC   1(2,R6),=C'SQ'                                                   
         BNE   ACTDRFTX                                                         
*                                                                               
         USING RACELD,R6                                                        
ACTDRFT2 XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         MVI   RACEL,RACELQ        BUILD NEW ACTIVITY ELEMENT                   
         MVI   RACLN,RACLNQ                                                     
         MVI   RACTYPE,RACTADD                                                  
         GOTO1 GETFACT,DMCB,0,0                                                 
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   RACUSER,TWAORIG                                                  
         MVC   RACPERS,FAPASSWD                                                 
         MVC   SVPASSWD,FAPASSWD                                                
         MVC   RACDATE,TODAYP                                                   
         MVC   RACTIME,FATIME                                                   
         MVC   DMCB(4),=X'FFFFFFFF'                                             
         GOTO1 SWITCH,DMCB                                                      
         L     R1,0(R1)                                                         
         MVC   RACTERM,TCFN-UTLD(R1)                                            
         DROP  R1                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
ACTDRFTX XMOD1                                                                  
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                 MAINTAIN ACTIVITY ON CHANGE JOB                     *         
***********************************************************************         
*                                                                               
         USING RACELD,R6                                                        
ACTMAIN  NMOD1 0,ACTMAN                                                         
         L     RC,0(R1)                                                         
         L     R6,AIO                                                           
         CLC   1(2,R6),=C'SJ'                                                   
         BNE   ACTMAINX                                                         
*                                                                               
         MVI   ELCODE,RACELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   ACTMAIN5                                                         
         XR    R0,R0                                                            
         XR    RE,RE                                                            
         XC    WORK,WORK                                                        
         CLI   RACTYPE,RACTCHA                                                  
         BNE   ACTMAIN0                                                         
         MVC   WORK+4(L'RACDATE+L'RACTIME),RACDATE                              
         ST    R6,WORK                                                          
         AHI   RE,1                                                             
                                                                                
ACTMAIN0 IC    R0,RACLN                                                         
         AR    R6,R0                                                            
         CLI   RACEL,0                                                          
         BE    ACTMAIN3                                                         
                                                                                
ACTMAIN1 CLI   RACEL,RACELQ                                                     
         BNE   ACTMAIN0                                                         
         CLI   RACTYPE,RACTCHA                                                  
         BNE   ACTMAIN0                                                         
         AHI   RE,1                                                             
         OC    WORK(4),WORK                                                     
         BZ    ACTMAIN2                                                         
         CLC   WORK+4(L'RACDATE+L'RACTIME),RACDATE                              
         BL    ACTMAIN0                                                         
                                                                                
ACTMAIN2 MVC   WORK+4(L'RACDATE+L'RACTIME),RACDATE                              
         ST    R6,WORK                                                          
         B     ACTMAIN0                                                         
                                                                                
ACTMAIN3 CHI   RE,RACMAXQ                                                       
         BNL   ACTMAIN4                                                         
         LA    R1,2000                                                          
         SHI   R1,RACMAXQ*50                                                    
         L     RF,AIO                                                           
         AHI   RF,ACCORLEN                                                      
         CLM   R1,3,0(RF)                                                       
         BH    ACTMAIN5                                                         
                                                                                
ACTMAIN4 L     R6,WORK           REPLACE ELEMENT                                
         LTR   R6,R6                                                            
         BZ    ACTMAIN5                                                         
         MVI   BYTE,0                                                           
         B     ACTMAIN6                                                         
                                                                                
ACTMAIN5 LA    R6,ELEMENT        ADD NEW CHANGE RACELD                          
         MVI   BYTE,1                                                           
                                                                                
ACTMAIN6 XC    RACEL(RACLNQ),RACEL                                              
         MVI   RACEL,RACELQ      BUILD NEW ACTIVITY ELEMENT                     
         MVI   RACLN,RACLNQ                                                     
         MVI   RACTYPE,RACTCHA                                                  
         GOTO1 GETFACT,DMCB,0,0                                                 
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   RACUSER,TWAORIG                                                  
         MVC   RACPERS,FAPASSWD                                                 
         MVC   SVPASSWD,FAPASSWD                                                
         MVC   RACDATE,TODAYP                                                   
         MVC   RACTIME,FATIME                                                   
         MVC   DMCB(4),=X'FFFFFFFF'                                             
         GOTO1 SWITCH,DMCB                                                      
         L     R1,0(R1)                                                         
         MVC   RACTERM,TCFN-UTLD(R1)                                            
         DROP  R1                                                               
         CLI   BYTE,0                                                           
         BE    ACTMAINX                                                         
         MVI   ELCODE,RACELQ                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
ACTMAINX MVI   DUB,1                                                            
         CLI   DUB,1                                                            
         XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                 ADD ACTIVITY APPROVAL ELEMENT TO JOB                *         
***********************************************************************         
*                                                                               
         USING RACELD,R6                                                        
ADDAPRV  NMOD1 0,ADDPRV                                                         
         L     RC,0(R1)                                                         
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         MVI   RACEL,RACELQ        BUILD NEW ACTIVITY ELEMENT                   
         MVI   RACLN,RACLNQ                                                     
         MVI   RACTYPE,RACTAPP                                                  
         GOTO1 GETFACT,DMCB,0,0                                                 
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   RACUSER,TWAORIG                                                  
         MVC   RACPERS,FAPASSWD                                                 
         MVC   SVPASSWD,FAPASSWD                                                
         MVC   RACDATE,TODAYP                                                   
         MVC   RACTIME,FATIME                                                   
         MVC   DMCB(4),=X'FFFFFFFF'                                             
         GOTO1 SWITCH,DMCB                                                      
         L     R1,0(R1)                                                         
         MVC   RACTERM,TCFN-UTLD(R1)                                            
         DROP  R1                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
ADDAPRVX MVI   DUB,1                                                            
         CLI   DUB,1                                                            
         XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              MANAGE JDTPASD PASSIVES                                *         
***********************************************************************         
*                                                                               
MANJDTP  NMOD1 0,MANJDT                                                         
         L     RC,0(R1)                                                         
         USING ACKEYD,R6                                                        
         L     R6,AIO                                                           
         CLI   RECNUM,RECNXJOB    NO NEED ON EXTRA JOB                          
         JE    MANJDTPX                                                         
                                                                                
         XC    SVDA,SVDA                                                        
                                                                                
         OC    SVDTINOP,SVDTINOP  ANY 'IN' OPEN DATE?                           
         JZ    MANJDTP2                                                         
         GOTOR MJEXTJB            GET JOB RECORD DETAILS                        
         GOTOR MJBLDPA,INOPENQ    BUILD IN/OPEN PASSIVE                         
         LHI   R1,DELETEQ         DELETE THIS POINTER IF EXISTING               
         OC    SVDTOUOP,SVDTOUOP                                                
         JZ    *+14                                                             
         CLC   SVDTINOP,SVDTOUOP  ... OR ...                                    
         JNE   MANJDTP1                                                         
         LHI   R1,STATUSQ         CHECK FOR STATUS CHANGE AND UPDATE            
                                                                                
MANJDTP1 GOTOR MJIOPAS                                                          
                                                                                
MANJDTP2 OC    SVDTOUOP,SVDTOUOP  ANY 'OUT' OPEN DATE?                          
         JZ    MANJDTP3                                                         
         CLC   SVDTINOP,SVDTOUOP  STATUS CHANGE HANDLED ABOVE                   
         JE    MANJDTP3                                                         
         GOTOR MJEXTJB            GET JOB RECORD DETAILS                        
         GOTOR MJBLDPA,OUOPENQ    BUILD OUT/OPEN PASSIVE                        
         GOTOR MJIOPAS,UPDATEQ    ADD/CHANGE RECORD                             
                                                                                
MANJDTP3 OC    SVDTINCL,SVDTINCL  ANY 'IN' CLOSE DATE?                          
         JZ    MANJDTP5                                                         
         GOTOR MJEXTJB            GET JOB RECORD DETAILS                        
         GOTOR MJBLDPA,INCLONQ    BUILD IN/CLOSE PASSIVE                        
         LHI   R1,DELETEQ         DELETE THIS POINTER IF EXISTING               
         OC    SVDTOUCL,SVDTOUCL                                                
         JZ    *+14                                                             
         CLC   SVDTINCL,SVDTOUCL  ... OR ...                                    
         JNE   MANJDTP4                                                         
         LHI   R1,STATUSQ         CHECK FOR STATUS CHANGE AND UPDATE            
                                                                                
MANJDTP4 GOTOR MJIOPAS                                                          
MANJDTP5 OC    SVDTOUCL,SVDTOUCL  ANY 'OUT' CLOSE DATE?                         
         JZ    MANJDTP6                                                         
         CLC   SVDTINCL,SVDTOUCL  STATUS CHANGE HANDLED ABOVE                   
         JE    MANJDTP6                                                         
         GOTOR MJEXTJB            GET JOB RECORD DETAILS                        
         GOTOR MJBLDPA,OUCLONQ    BUILD OUT/CLOSE PASSIVE                       
         GOTOR MJIOPAS,UPDATEQ    ADD/CHANGE RECORD                             
                                                                                
MANJDTP6 MVC   SVDTINOP,SVDTOUOP  SWAP OUT TO IN AND CLEAR OUT ...              
         XC    SVDTOUOP,SVDTOUOP                                                
         MVC   SVDTINCL,SVDTOUCL                                                
         XC    SVDTOUCL,SVDTOUCL                                                
*                                                                               
MANJDTPX MVI   DUB,1                                                            
         CLI   DUB,1                                                            
         XIT1                                                                   
                                                                                
MJEXTJB  DS    0H                 EXTRACT JOB RECORD DETAILS                    
                                                                                
         OC    SVDA,SVDA          ALREADY DONE?                                 
         BNZR  RE                                                               
         ST    RE,SAVERE                                                        
         USING ACTRECD,R2                                                       
         L     R2,AIO3            READ ACCDIR FOR D/A AND STATUS                
         MVC   0(L'ACTKEY,R2),0(R6)                                             
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),=C'ACCDIR',(R2),(R2),0               
         JE    MJEXTJB1                                                         
         CLI   8(R1),X'02'        RECORD IS DELETED                             
         JE    MJEXTJB1                                                         
         DC    H'0'               RECORD NOT THERE                              
                                                                                
MJEXTJB1 MVC   SVDA,ACTKDA        DSAVE DETAILS                                 
         MVC   SVJBRSTA,ACTKSTA                                                 
         DROP  R2                                                               
*                                                                               
         CLI   EFFOFFC,C' '       OFFICE SET?                                   
         JH    MJEXTJB2                                                         
         MVC   EFFOFFC,JBRSJOF    SET FROM JOB                                  
         CLI   EFFOFFC,C' '                                                     
         JH    MJEXTJB2                                                         
         MVC   EFFOFFC,JBRPROF    SET FROM PRODUCT                              
         CLI   EFFOFFC,C' '                                                     
         JH    MJEXTJB2                                                         
         MVC   EFFOFFC,JBRCLOF    SET FROM CLIENT                               
*                                                                               
MJEXTJB2 L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
         USING JDTPASD,R2                                                       
MJBLDPA  DS    0H                  BUILD PASSIVE KEY                            
                                                                                
         ST    RE,SAVERE                                                        
         STC   R1,BYTE                                                          
         LA    RF,SVDTINOP                                                      
         LHI   R0,JDTPDTOQ                                                      
         CHI   R1,INOPENQ                                                       
         JE    MJBLDPA1                                                         
         LA    RF,SVDTOUOP                                                      
         CHI   R1,OUOPENQ                                                       
         JE    MJBLDPA1                                                         
         LHI   R0,JDTPDTCQ                                                      
         LA    RF,SVDTINCL                                                      
         CHI   R1,INCLONQ                                                       
         JE    MJBLDPA1                                                         
         LA    RF,SVDTOUCL                                                      
         CHI   R1,OUCLONQ                                                       
         JE    MJBLDPA1                                                         
         DC    H'0'                                                             
                                                                                
MJBLDPA1 L     R2,AIO3                                                          
         XC    JDTPASD(ACCKLEN),JDTPASD                                         
         MVI   JDTPTYP,JDTPTYPQ                                                 
         MVI   JDTPSUB,JDTPSUBQ                                                 
         MVC   JDTPCPY,ACTKCPY-ACTRECD(R6)                                      
         STC   R0,JDTPDTYP                                                      
         MVC   JDTPDATE,0(RF)                                                   
         MVC   JDTPOFFC,EFFOFFC                                                 
         MVC   JDTPJACT,ACTKACT-ACTRECD(R6)                                     
         MVC   JDTPDA,SVDA                                                      
         MVC   JDTPSTA,SVJBRSTA                                                 
                                                                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R2                                                               
                                                                                
         USING JDTPASD,R2                                                       
MJIOPAS  DS    0H                  IO HANDLING                                  
                                                                                
         ST    RE,SAVERE                                                        
         STC   R1,BYTE                                                          
         L     R2,AIO3                                                          
         MVC   ELEMENT(ACCKLEN),JDTPASD  SAVE OFF KEY                           
                                                                                
         MVI   OPTION2,1           RECORD UNDELETED                             
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),=C'ACCDIR',(R2),(R2),0               
         JE    MJIOP002                                                         
         MVI   OPTION2,2                                                        
         CLI   8(R1),X'02'         RECORD DELETED                               
         JE    MJIOP002                                                         
         MVI   OPTION2,3           RECORD NOT THERE                             
                                                                                
MJIOP002 LLC   R1,BYTE                                                          
         CHI   R1,STATUSQ          PUT ON STATUS CHANGE                         
         JE    MJIOP100                                                         
         CHI   R1,UPDATEQ          PUT OR ADD                                   
         JE    MJIOP200                                                         
         CHI   R1,DELETEQ          DELETE                                       
         JE    MJIOP300                                                         
         DC    H'0'                                                             
                                                                                
MJIOP100 DS    0H                  (STATUSQ)                                    
         CLI   OPTION2,3           IF NOT THERE NO ACTION REQUIRED              
         JE    MJIOPASX                                                         
                                                                                
         CLC   SVDA,JDTPDA         SAME D/A - ???                               
         JNE   *+2                                                              
         CLC   SVJBRSTA,JDTPSTA    SAME STATUS NO ACTION REQUIRED               
         JE    MJIOPASX                                                         
                                                                                
         MVC   JDTPAS,ELEMENT      RESTORE KEY, STATUS, D/A                     
         MVC   JDTPSTA,SVJBRSTA                                                 
         MVC   JDTPDA,SVDA                                                      
         GOTO1 DATAMGR,DMCB,DMWRT,=C'ACCDIR',JDTPASD,JDTPASD,0                  
         JE    MJIOPASX                                                         
         DC    H'0'                                                             
                                                                                
MJIOP200 DS    0H                  (UPDATEQ)                                    
         MVC   JDTPAS,ELEMENT      RESTORE KEY, STATUS, D/A                     
         MVC   JDTPSTA,SVJBRSTA                                                 
         MVC   JDTPDA,SVDA                                                      
         CLI   OPTION2,3           ADD IT ELSE PUT IT                           
         JE    MJIOP202                                                         
                                                                                
         GOTO1 DATAMGR,DMCB,DMWRT,=C'ACCDIR',JDTPASD,JDTPASD,0                  
         JE    MJIOPASX                                                         
         DC    H'0'                                                             
                                                                                
MJIOP202 GOTO1 DATAMGR,DMCB,DMADD,=C'ACCDIR',JDTPASD,JDTPASD,0                  
         JE    MJIOPASX                                                         
         DC    H'0'                                                             
                                                                                
MJIOP300 DS    0H                  (DELETEQ)                                    
         CLI   OPTION2,1           ONLY IF UNDELETED EXISTS                     
         JNE   MJIOPASX                                                         
         MVC   JDTPAS,ELEMENT      RESTORE KEY, STATUS, D/A                     
         MVC   JDTPSTA,SVJBRSTA                                                 
         MVC   JDTPDA,SVDA                                                      
         OI    JDTPSTA,ACTSDELT    AND SET DELETED                              
                                                                                
         GOTO1 DATAMGR,DMCB,DMWRT,=C'ACCDIR',JDTPASD,JDTPASD,0                  
         JE    MJIOPASX                                                         
         DC    H'0'                                                             
                                                                                
MJIOPASX L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R2                                                               
INOPENQ  EQU   1                                                                
OUOPENQ  EQU   2                                                                
INCLONQ  EQU   3                                                                
OUCLONQ  EQU   4                                                                
STATUSQ  EQU   1                                                                
UPDATEQ  EQU   2                                                                
DELETEQ  EQU   3                                                                
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*              MANAGE PIDKJOBQ PASSIVES                               *         
***********************************************************************         
*                                                                               
MANPIDP  NMOD1 0,MANPID                                                         
         L     RC,0(R1)                                                         
         USING ACKEYD,R6                                                        
         L     R6,AIO                                                           
*        CLI   RECNUM,RECNXJOB     NO NEED ON EXTRA JOB                         
*        JE    MANPIDX                                                          
                                                                                
         JAS   RE,MJE2TJB          MAY HAVE BEEN CALLED AT MANJDTP              
         MVC   SVJBRSTA+ACTKSOFF-ACTKSTA(2),EFFOFFC                             
                                                                                
         TM    ACSTATUS,ACTSDRFT   APPROVED ACCOUNTS ONLY                       
         JNZ   MANPIDX                                                          
                                                                                
         USING JOBELD,R2                                                        
         XC    MPIDDATA,MPIDDATA   ESTABLISH PIDS AND DATES                     
         LR    R2,R6                                                            
         AH    R2,DATADISP                                                      
                                                                                
MANPIDP1 CLI   JOBEL,0                                                          
         JE    MANPIDP6                                                         
         CLI   JOBEL,JOBELQ                                                     
         JE    MANPIDP3                                                         
         CLI   JOBEL,RACELQ                                                     
         JE    MANPIDP4                                                         
                                                                                
MANPIDP2 LLC   R0,JOBLN                                                         
         AR    R2,R0                                                            
         J     MANPIDP1                                                         
                                                                                
MANPIDP3 MVC   MPIDDATA+4(3),JOBADATE                                           
         J     MANPIDP2                                                         
                                                                                
         USING RACELD,R2                                                        
MANPIDP4 CLI   RACTYPE,RACTADD                                                  
         JNE   MANPIDP5                                                         
         MVC   MPIDDATA+0(2),RACPERS                                            
         MVC   MPIDDATA+4(3),RACDATE                                            
         J     MANPIDP2                                                         
                                                                                
MANPIDP5 CLI   RACTYPE,RACTAPP                                                  
         JNE   MANPIDP2                                                         
         MVC   MPIDDATA+2(2),RACPERS                                            
         J     MANPIDP2                                                         
         DROP  R2                                                               
                                                                                
MANPIDP6 OC    MPIDDATA+2(2),MPIDDATA+2                                         
         JNZ   MANPIDP7                                                         
         MVC   MPIDDATA+2(2),MPIDDATA    SET APPROVER FROM SUBMITTER            
                                                                                
         USING PIDRECD,R2                                                       
MANPIDP7 L     R2,AIO3                                                          
                                                                                
         OC    MPIDDATA+2(2),MPIDDATA+2  ANY APPROVER?                          
         JZ    MANPIDP9                                                         
         XC    PIDRECD(ACCKLEN),PIDRECD                                         
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,ACTKCPY-ACTRECD(R6)                                      
         MVI   PIDKSTYP,PIDKJOBQ                                                
         MVI   PIDKJST,PIDKJSAQ                                                 
         MVC   PIDKACC,ACTKACT-ACTRECD(R6)                                      
         MVC   PIDKPID,MPIDDATA+2                                               
         OC    MPIDDATA+4(3),MPIDDATA+4                                         
         JZ    MANPIDP8                                                         
         GOTO1 DATCON,DMCB,(1,MPIDDATA+4),(2,PIDKADAT)                          
                                                                                
MANPIDP8 JAS   RE,MPIDIO                                                        
                                                                                
MANPIDP9 OC    MPIDDATA+0(2),MPIDDATA+0  ANY SUBMITTER (CREATOR)?               
         JZ    MANPIDX                                                          
         XC    PIDRECD(ACCKLEN),PIDRECD                                         
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKCPY,ACTKCPY-ACTRECD(R6)                                      
         MVI   PIDKSTYP,PIDKJOBQ                                                
         MVI   PIDKJST,PIDKJSSQ                                                 
         MVC   PIDKACC,ACTKACT-ACTRECD(R6)                                      
         MVC   PIDKPID,MPIDDATA+0                                               
         OC    MPIDDATA+4(3),MPIDDATA+4                                         
         JZ    MANPIDPA                                                         
         GOTO1 DATCON,DMCB,(1,MPIDDATA+4),(2,PIDKADAT)                          
                                                                                
MANPIDPA JAS   RE,MPIDIO                                                        
                                                                                
MANPIDX  MVI   DUB,1                                                            
         CLI   DUB,1                                                            
         XIT1                                                                   
                                                                                
MPIDIO   DS    0H                  *** IO HANDLING ***                          
                                                                                
         ST    RE,SAVERE                                                        
         MVC   ELEMENT(ACCKLEN),PIDRECD                                         
                                                                                
         MVI   OPTION2,1           RECORD UNDELETED                             
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),=C'ACCDIR',(R2),(R2),0               
         JE    MPIDIO2                                                          
         MVI   OPTION2,2                                                        
         CLI   8(R1),X'02'         RECORD DELETED                               
         JE    MPIDIO2                                                          
         MVI   OPTION2,3           RECORD NOT THERE                             
         J     MPIDIO4                                                          
                                                                                
MPIDIO2  CLC   SVDA,PIDKDA         SAME D/A - ???                               
         JNE   *+2                                                              
         CLC   SVJBRSTA,PIDKSTA    MATCH ON STATUS?                             
         JE    MPIDIOX                                                          
                                                                                
         MVC   PIDKEY,ELEMENT      RESTORE KEY, STATUS, D/A                     
         MVC   PIDKSTA,SVJBRSTA                                                 
         MVC   PIDKDA,SVDA                                                      
         GOTO1 DATAMGR,DMCB,DMWRT,=C'ACCDIR',PIDRECD,PIDRECD,0                  
         JE    MPIDIOX                                                          
         DC    H'0'                                                             
                                                                                
MPIDIO4  MVC   PIDKEY,ELEMENT      RESTORE KEY, STATUS, D/A AND ADD IT          
         MVC   PIDKSTA,SVJBRSTA                                                 
         MVC   PIDKDA,SVDA                                                      
                                                                                
         GOTO1 DATAMGR,DMCB,DMADD,=C'ACCDIR',PIDRECD,PIDRECD,0                  
         JE    MPIDIOX                                                          
         DC    H'0'                                                             
                                                                                
MPIDIOX  L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
MJE2TJB  DS    0H                 EXTRACT JOB RECORD DETAILS                    
                                                                                
         OC    SVDA,SVDA          ALREADY DONE?                                 
         BNZR  RE                                                               
         ST    RE,SAVERE                                                        
         USING ACTRECD,R2                                                       
         L     R2,AIO3            READ ACCDIR FOR D/A AND STATUS                
         MVC   0(L'ACTKEY,R2),0(R6)                                             
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),=C'ACCDIR',(R2),(R2),0               
         JE    MJE2TJB1                                                         
         CLI   8(R1),X'02'        RECORD IS DELETED                             
         JE    MJE2TJB1                                                         
         DC    H'0'               RECORD NOT THERE                              
                                                                                
MJE2TJB1 MVC   SVDA,ACTKDA        DSAVE DETAILS                                 
         MVC   SVJBRSTA,ACTKSTA                                                 
         DROP  R2                                                               
*                                                                               
         CLI   EFFOFFC,C' '       OFFICE SET?                                   
         JH    MJE2TJB2                                                         
         MVC   EFFOFFC,JBRSJOF    SET FROM JOB                                  
         CLI   EFFOFFC,C' '                                                     
         JH    MJE2TJB2                                                         
         MVC   EFFOFFC,JBRPROF    SET FROM PRODUCT                              
         CLI   EFFOFFC,C' '                                                     
         JH    MJE2TJB2                                                         
         MVC   EFFOFFC,JBRCLOF    SET FROM CLIENT                               
*                                                                               
MJE2TJB2 L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                      PROCESS PF KEYS                                *         
***********************************************************************         
*                                                                               
PROCPF   NMOD1 0,PROCPF                                                         
         L     RC,0(R1)                                                         
         LA    R2,JBRSJACH                                                      
         CLI   PFKEY,PF7                                                        
         BE    PROCPF7                                                          
         CLI   PFKEY,PF6                                                        
         BE    PROCPF6                                                          
         CLI   PFKEY,PF5                                                        
         BE    PROCPF5                                                          
         CLI   PFKEY,PF4                                                        
         BE    PROCPF4                                                          
         CLI   PFKEY,PF3                                                        
         BE    PROCPF3                                                          
         CLI   PFKEY,PF2                                                        
         BE    PROCPF2                                                          
         CLI   PFKEY,PF1                                                        
         BNE   PROCPFX                                                          
         LA    R1,=C'MAINT'                                                     
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'05'                                                     
*                                                                               
PROCPF1  MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'OPTION',,=C',',=C',',(6,CLICODE),(6,PRODCOX        
               DE),(6,JOBNUM),0                                                 
*                                                                               
PROCPF2  LA    R1,=C'LIST'                                                      
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'04'                                                     
         B     PROCPF1                                                          
*                                                                               
PROCPF3  MVI   ERROR,BOESTERR      PREPARE FOR ERROR                            
         CLI   ACTNUM,ACTADD       IS THIS AN ADD ?                             
         BNE   PROCPF3A            NO                                           
         LA    RF,JBRSJAC          YES, ADDRESS JOB NUMBER                      
         TM    STATUS26,X'40'      TEST FOR NEW FORMAT                          
         B     PROCPF3B                                                         
*                                                                               
PROCPF3A GOTO1 SETJOB                                                           
         LA    RF,JOBNUM                                                        
         TM    JOBJSTAT,X'40'      IS THIS A NEW FORMAT ESTIMATE ?              
*                                                                               
PROCPF3B BZ    ERREXIT             NO, ISSUE MESSAGE                            
         MVI   PFKEY,0             YES, CALL ESTIMATE                           
         GOTO1 VCALL,WORK,=C'JOB',=C'ESTIMATE',(6,CLICODE),(6,PRODCODE)X        
               ,(6,(RF)),0                                                      
*                                                                               
PROCPF4  MVI   ERROR,BOESTERR      PREPARE FOR ERROR                            
         CLI   ACTNUM,ACTADD       IS THIS AN ADD ?                             
         BNE   PROCPF4A            NO                                           
         LA    RF,JBRSJAC          YES, ADDRESS JOB NUMBER                      
         TM    STATUS26,X'40'      TEST FOR NEW ESTIMATE                        
         B     PROCPF4B                                                         
*                                                                               
PROCPF4A GOTO1 SETJOB                                                           
         LA    RF,JOBNUM                                                        
         TM    JOBJSTAT,X'40'      IS THIS A NEW FORMAT ESTIMATE ?              
*                                                                               
PROCPF4B BZ    ERREXIT             NO, ISSUE MESSAGE                            
         MVI   PFKEY,0             YES, CALL ESTIMATE                           
         GOTO1 VCALL,WORK,=C'JOB',=C'ELIST',(6,CLICODE),(6,PRODCODE),(6X        
               ,(RF)),0                                                         
*                                                                               
PROCPF5  MVI   ERROR,BOESTERR      PREPARE FOR ERROR                            
         CLI   ACTNUM,ACTADD       IS THIS AN ADD ?                             
         BNE   PROCPF5A            NO                                           
         LA    RF,JBRSJAC          YES, ADDRESS JOB NUMBER                      
         TM    STATUS26,X'40'      TEST FOR NEW ESTIMATE                        
         B     PROCPF5B                                                         
*                                                                               
PROCPF5A GOTO1 SETJOB                                                           
         LA    RF,JOBNUM                                                        
         TM    JOBJSTAT,X'40'      IS THIS A NEW FORMAT ESTIMATE ?              
*                                                                               
PROCPF5B BZ    ERREXIT             NO, ISSUE MESSAGE                            
         MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'JOB',=C'SUMMARY',(6,CLICODE),(6,PRODCODE),X        
               (6,(RF)),0                                                       
*                                                                               
PROCPF6  MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'LINK',=C'MAINT',(6,CLICODE),(6,PRODCODE),(X        
               6,JOBNUM),0                                                      
*                                                                               
PROCPF7  MVI   ERROR,PFKFUND       GET ERROR MESSAGE READY                      
         OC    SVJFUND,SVJFUND     IS JOB FUNDED?                               
         BZ    ERREXIT             NO, ISSUE MESSAGE                            
         LA    RF,SVJFUND                                                       
         USING JFNKEY,RF                                                        
         GOTO1 VCALL,WORK,=C'AUTH',=C'DIS',(1,JFNOGR),(2,JFNOFC),(6,JFNX        
               CLI),(6,JFNPRO),(1,JFNMGR),(1,JFNMED),(20,JFNNUM)                
         DROP  RF                                                               
*                                                                               
PROCPFX  XMOD1                                                                  
         EJECT                                                                  
PROCPF8  NMOD1 0,PROCP8                                                         
         L     RC,0(R1)                                                         
         CLI   PFKEY,PF8                                                        
         BNE   PROCPF8X                                                         
         LA    R2,JBRSJACH                                                      
         MVI   PFKEY,0                                                          
*                                                                               
         CLI   ACTNUM,ACTCHA       ARE WE CHANGING THE RECORD?                  
         BNE   PROCPF8A            NO                                           
         CP    ELMCNT,=P'0'        YES, IF USER FIELDS                          
         BE    PROCPF8A                                                         
         CLI   VERUSER,1           MAKE SURE THEY ARE RESOLVED                  
         BNE   PROCPF8X                                                         
*                                                                               
PROCPF8A CLI   ACTNUM,ACTADD       ARE WE ADDING?                               
         BNE   *+8                 NO                                           
         MVI   ACTNUM,ACTCHA       YES, MAKE IT A CHANGE                        
         SR    RF,RF                                                            
         IC    RF,ACTNUM                                                        
         GOTO1 VCALL,WORK,RECNXJOB,(RF),(6,CLICODE),(6,PRODCODE),(6,JOBX        
               NUM),0                                                           
*                                                                               
PROCPF8X XMOD1                                                                  
         EJECT                                                                  
**********************************************************************          
*              MANAGE DRAPASD PASSIVE (ADD OR UPDATE)                           
**********************************************************************          
         SPACE 1                                                                
         USING DRAPASD,R2                                                       
         USING ACKEYD,R6                                                        
MANDRAP  NMOD1 0,MANDRA                                                         
         L     RC,0(R1)                                                         
         L     R6,AIO                                                           
                                                                                
         TM    ACSTATUS,ACTSDRFT   PASSIVE FRO DRAFT ACCOUNTS ON ADD            
         BZ    MANDRAPX                                                         
                                                                                
         L     R2,AIO3                                                          
         MVC   0(L'ACTKEY,R2),0(R6)                                             
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMREAD'),=C'ACCDIR',(R2),(R2),0           
         BE    *+6                                                              
         DC    H'0'                ACCOUNT NOT FOUND                            
         MVC   SVDA,ACTKDA-ACTRECD(R2)                                          
         MVC   SVJBRSTA,ACTKSTA-ACTRECD(R2)                                     
*                                                                               
         CLI   ACTNUM,ACTADD       IF NOT ADD THEN EXTRACT PID                  
         BE    MANDRAP6            FROM RACELD                                  
*                                                                               
         USING RACELD,R3                                                        
         LR    R3,R6                                                            
         AH    R3,DATADISP                                                      
                                                                                
MANDRAP2 CLI   RACEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   RACEL,RACELQ                                                     
         BNE   *+12                                                             
         CLI   RACTYPE,RACTADD     ADD TYPE RACELD                              
         BE    MANDRAP4                                                         
         LLC   R0,RACLN                                                         
         AR    R3,R0                                                            
         B     MANDRAP2                                                         
                                                                                
MANDRAP4 MVC   SVPASSWD,RACPERS    EXTRACT PID                                  
*                                                                               
MANDRAP6 XC    DRAPASD(ACCKLEN),DRAPASD                                         
         MVI   DRAPTYP,DRAPTYPQ                                                 
         MVI   DRAPSUB,DRAPSUBQ                                                 
         MVC   DRAPCPY,ACTKCPY-ACTRECD(R6)                                      
         MVI   DRAPAPS,DRAPDRA     DRAFT INDICATOR                              
         MVC   DRAPULA,ACTKULA-ACTRECD(R6)                                      
         MVC   DRAPPID,SVPASSWD    PERSON ADDED                                 
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'88',=C'DMREAD'),=C'ACCDIR',DRAPASD,     X        
               DRAPASD,0                                                        
         CLI   8(R1),0             FOUND OK                                     
         BE    *+12                                                             
         CLI   8(R1),X'02'         RECORD DELETED                               
         BNE   MANDRAP8                                                         
         MVC   DRAPSTA,SVJBRSTA    UPDATE THE STATUS                            
         MVC   DRAPDA,SVDA                                                      
         MVC   DRAPSOFF,EFFOFFC                                                 
         CLC   EFFOFFC,BLANKS                                                   
         JH    *+10                                                             
         MVC   DRAPSOFF,JBRSJOF                                                 
         OC    DRAPSOFF,BLANKS                                                  
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'ACCDIR',DRAPASD,DRAPASD,0              
         BE    MANDRAPX                                                         
         DC    H'0'                                                             
                                                                                
MANDRAP8 MVC   DRAPASD,ELEMENT                                                  
         MVC   DRAPDA,SVDA                                                      
         MVC   DRAPSTA,SVJBRSTA                                                 
         MVC   DRAPSOFF,EFFOFFC                                                 
         CLC   EFFOFFC,BLANKS                                                   
         JH    *+10                                                             
         MVC   DRAPSOFF,JBRSJOF                                                 
         OC    DRAPSOFF,BLANKS                                                  
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'ACCDIR',DRAPASD,DRAPASD,0              
         B     MANDRAPX                                                         
                                                                                
MANDRAPX MVI   DUB,1                                                            
         CLI   DUB,1                                                            
         XIT1                                                                   
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
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
         EJECT                                                                  
NUMTABD  DSECT                                                                  
NUMVAL   DS    CL2                 VALUE FIELD                                  
NUMMOVE  DS    S                   ADDRESS OF MOVE FIELD                        
NUMADDR  DS    S                   ADDRESS OF ROUTINE                           
NUMLEN   EQU   *-NUMTABD                                                        
         EJECT                                                                  
MYD      DSECT                                                                  
ACSRCHP  DS    A                                                                
KEYCHG   DS    C                                                                
SAVER6   DS    A                   SAVE AREA FOR R6                             
SAVERE   DS    A                   SAVE AREA FOR RE                             
SAVERF   DS    A                   SAVE AREA FOR RF                             
ASEQNUM  DS    A                   A(STARTING SEQUENCE NUMBER)                  
SAVESRLG DS    CL2                 SAVE AREA FOR RECEIVABLE LEDGER              
SAVE1CLG DS    CL2                 SAVE AREA FOR COSTING LEDGER                 
SAVE1ST  DS    CL6                 SAVE AREA FOR 1ST JOB NUMBER                 
THISLEDG DS    CL2                 LEDGER BEING PROCESSED                       
FUNCTION DS    CL8                 SAVE DATAMGR COMMAND                         
USERKEY  DS    CL42                                                             
CLOSED   DS    CL6                                                              
OPEN     DS    PL3                                                              
TODAY    DS    CL6                                                              
STARTD   DS    CL6                                                              
STARTP   DS    PL3                                                              
JDATE    DS    CL6                                                              
EFFDATE  DS    PL3                                                              
START#   DS    CL5                 STARTING SEQUENTIAL NUMBER                   
NEXT#    DS    CL5                 NEXT SEQUENTIAL NUMBER                       
WORK5    DS    CL5                 WORKAREA                                     
DRFTITMS DS    C                   DRAFT TRANSACTION INDICATOR                  
MPIDDATA DS    XL7                                                              
                                                                                
TWAPARM  DS    CL(TWAPARML)                                                     
NAMEFLAG DS    XL1                                                              
NFCHANGE EQU   X'80'                                                            
CHGRSTAT DS    XL1                 CHANGE IN RSTSTAT VALUE                      
CHGRACIC EQU   RSTSACIC            CLOSED STATUS CHANGED                        
CHGRACIL EQU   RSTSACIL            LOCKED STATUS CHANGED                        
       ++INCLUDE ACGOBLOCK                                                      
       ++INCLUDE ACGOXBLOCK                                                     
       ++INCLUDE ACGOBBLOCK                                                     
MYDEND   EQU   *                                                                
         EJECT                                                                  
SCREEND  DSECT                                                                  
ACNAMH   DS    CL8                                                              
ACNAM    DS    CL(L'JBRSJNM)                                                    
         ORG   ACNAMH+(JBRSJOFH-JBRSJNMH)                                       
ACOFH    DS    CL8                                                              
ACOF     DS    CL(L'JBRSJOF)                                                    
         ORG   ACNAMH+(JBRSJF1H-JBRSJNMH)                                       
ACF1H    DS    CL8                                                              
ACF1     DS    CL(L'JBRSJF1)                                                    
         ORG   ACNAMH+(JBRSJF2H-JBRSJNMH)                                       
ACF2H    DS    CL8                                                              
ACF2     DS    CL(L'JBRSJF2)                                                    
         ORG   ACNAMH+(JBRSJF3H-JBRSJNMH)                                       
ACF3H    DS    CL8                                                              
ACF3     DS    CL(L'JBRSJF3)                                                    
         ORG   ACNAMH+(JBRSJF4H-JBRSJNMH)                                       
ACF4H    DS    CL8                                                              
ACF4     DS    CL(L'JBRSJF4)                                                    
         ORG   ACNAMH+(JBRSJF5H-JBRSJNMH)                                       
ACF5H    DS    CL8                                                              
ACF5     DS    CL(L'JBRSJF5)                                                    
         EJECT                                                                  
EQUIVD   DSECT                                                                  
EQULABH  DS    CL8                                                              
EQULAB   DS    CL(L'JBXLAB1)                                                    
         ORG   EQULABH+(JBXEQU1H-JBXLAB1H)                                      
EQUEQUH  DS    CL8                                                              
EQUEQU   DS    CL(L'JBXEQU1)                                                    
         ORG   EQULABH+(JBXACC1H-JBXLAB1H)                                      
EQUACCH  DS    CL8                                                              
EQUACC   DS    CL(L'JBXACC1)                                                    
EQULNQ   EQU   *-EQUIVD                                                         
         EJECT                                                                  
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
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDTWABLDD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDTWABLDD                                                      
         PRINT ON                                                               
         EJECT                                                                  
* ACPROWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPRODCD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROF3D                                                       
SVDATA   DS    0F                                                               
ELMCNT   DS    PL8                 COUNT OF ELMTAB ENTRIES                      
AELMTAB  DS    A                   A(ELEMENT TABLE)                             
PRODLEDG DS    CL2                 PRODUCTION UNIT/LEDGER                       
RECVLEDG DS    CL2                 RECEIVABLE UNIT/LEDGER                       
SAVEOFC  DS    CL2                 SAVED EFFECTIVE OFFICE                       
SVSTART  DS    PL3                 SAVE OPEN/START DATE                         
NEWDATE  DS    X                   FLAG FOR OPENED DATE CHANGE                  
VERUSER  DS    X                   FLAG FOR USER FIELD VERIFICATION             
SAVEKEY  DS    CL42                SAVE AREA FOR KEY                            
AMOUNT   DS    PL8                 TOTAL BILLED AMOUNT                          
ADDTYPE  DS    CL1                 ESTIMATE TYPE TO BE ADDED                    
STATUS26 DS    CL1                 JOBSTA1                                      
SVMEDSTA DS    CL1                 SAVED MEDIA STATUS BYTE                      
USCRIPT  DS    CL1                 USING SCRIPT?                                
SVJFUND  DS    CL(L'JFNKEY)        AUTHORIZATION KEY FROM JOB RECORD            
SVPASSWD DS    XL2                 SAVE PERSON BINARY PID                       
SVJOBST1 DS    XL1                 SAVED JOBSTA1                                
SVRSTST6 DS    XL1                 SAVED RSTSTAT6                               
SVDATES  DS    0XL8                SAVED OPEN/CLOSE IN/OUT DATES                
SVDTINOP DS    XL2                                                              
SVDTINCL DS    XL2                                                              
SVDTOUOP DS    XL2                                                              
SVDTOUCL DS    XL2                                                              
         DS    0F                                                               
SVDA     DS    CL4                 SAVED DISK ADDRESS                           
SVJBRSTA DS    XL(L'ACTRSTA)       SAVED JOB RECORD STATUS                      
         DS    XL103               >>> SPARE STORAGE - IS IT REALLY?            
         EJECT                                                                  
SUBSYSD  DSECT                                                                  
         ORG   ACIOBLOK                                                         
OLDACCNM DS    CL(NAMLN1Q+L'NAMEREC)                                            
OLDJOBNM DS    CL(NAMLN1Q+L'NAMEREC)                                            
POINTERS DS    XL(8*54+1)          PASSIVE POINTER BLOCK                        
         EJECT                                                                  
* ACGENRAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENRAC                                                       
         PRINT ON                                                               
* FAFACTS                                                                       
       PRINT OFF                                                                
       ++INCLUDE FAFACTS                                                        
       PRINT ON                                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063ACPRO03   08/03/17'                                      
         END                                                                    
