*          DATA SET CTMAD15    AT LEVEL 113 AS OF 12/27/12                      
*PHASE TA0C15A,*                                                                
*INCLUDE PRESENT                                                                
*INCLUDE PRESTO                                                                 
*INCLUDE ACJOBCOL                                                               
*INCLUDE VATICAN                                                                
         TITLE 'TA0C15 - $MAD PRESTO MONITOR SUPPORT RECORDS'                   
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* NOTE: IF YOU MAKE ANY CHANGES TO THIS PROGRAM, PLEASE CHECK IF THEY           
*       SHOULD GO IN PRESTO'S VERSION (ACPRF02 - JOB DOWNLOAD ONLY)             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
TA0C15   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C15,RA,RR=R4                                                
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         ST    R4,APRELO           SAVE RELOCATION CONSTANT                     
         EJECT                                                                  
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
*                                                                               
MAIN     DS    0H                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   M10                                                              
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         B     MX                                                               
*                                                                               
M10      CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   M20                                                              
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
M20      BAS   RE,PROCEND          ELSE CALL PROCEND                            
*                                                                               
MX       B     EXIT                                                             
         EJECT                                                                  
* THIS ROUTINE INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                  
* INITIALIZATION.  CURRENTLY, NOTHING NEEDS INITIALIZATION.                     
*                                                                               
INIT     NTR1                                                                   
*                                                                               
*        INITIALIZE LOCAL WORKING STORAGE                                       
*                                                                               
         GOTO1 SETSYS,DMCB,=C'ACCOUNT',=CL8'ACCDIR',=CL8'ACCMST'                
         BE    INIT05                                                           
* IF THERE WAS AN ERROR, CHECK IF ERROR SWITCHING SYSTEMS                       
* IF ERROR SWITCHING SYSTEMS, CHANGE TO FILE READ-ONLY                          
         CLC   MDACTION,=Y(ERSWITCH)                                            
         BNE   EXIT                                                             
         XC    MDACTION,MDACTION                                                
         B     ERRUPRO                                                          
*                                                                               
INIT05   MVI   LENKEY,L'ACCKEY     DATAMGR VALUES                               
         MVI   DISPDSKA,ACCKDA-ACCRECD                                          
         MVI   WRKISCRE,C'N'       WORKER FILE NOT YET CREATED                  
         BAS   RE,BUILDDT          BUILD DATE/TIME OBJECT                       
*                                                                               
*        SET UP GLOBAL VALUES FOR THE COMPANY/UNIT/LEDGER.                      
*                                                                               
         L     RF,AUTL             FIND COMPANY IN UTL                          
         USING UTLD,RF                                                          
         MVC   CUL(1),TAGYB                                                     
         MVC   CTRY,TCTRY                                                       
         DROP  RF                                                               
*                                                                               
         LA    R6,BIGKEY                                                        
         USING CPYRECD,R6                                                       
         MVI   CPYKEY,C' '                                                      
         MVC   CPYKEY+1(L'CPYKEY-1),CPYKEY                                      
         MVC   CPYKCPY,CUL         READ COMPANY RECORD                          
         GOTO1 HIGH                                                             
         CLC   CPYKEY(CPYKEND),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
         LA    R3,CPYELQ                                                        
         GOTO1 GETELEM,DMCB,(R3),0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CPYELD,R6                                                        
         MVC   CUL+1(2),CPYPROD                                                 
         CLI   CPYLN,CPYLN3Q                                                    
         BL    INIT10                                                           
*                                                                               
         MVC   COMPCUR,CPYCURR                                                  
*&&UK*&& MVC   COMPCURS,CPYCURRS                                                
         MVC   COMPSTA7,CPYSTAT7                                                
         MVC   COMPSTA9,CPYSTAT9                                                
*                                                                               
INIT10   GOTO1 SETHEIR             EXTRACT ACCOUNT LEDGER HEIRARCHY             
         BNE   ERROBJ                                                           
*                                                                               
         MVC   HALF,CUL+1                                                       
         MVC   CUL+1(2),=C'1R'     REPLACE LEDGER WITH 1R                       
         GOTO1 SETHEIR             GET 1R LEDGER LENGTHS                        
         BNE   ERROBJ                                                           
         MVC   CUL+1(2),HALF       RESTORE LEDGER AGAIN                         
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
*&&UK*&& MVC   TOBACCO,CTOBACCO                                                 
         DROP  RF                                                               
*                                                                               
INX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE START MODE.                                        
*                                                                               
PROCSTRT NTR1                                                                   
         MVI   EMPFLAG,C'Y'        TEMP FILE IS EMPTY SO FAR                    
*                                                                               
         BAS   RE,VALREQ           VALIDATE SCREEN REQUEST                      
*                                                                               
         CLI   MONRTYP,PRRQRTJO    IF JOB OPEN                                  
         BNE   PS10                                                             
         BAS   RE,FILLTMP          THEN ADD JOB OPEN OBJS TO TEMPSTR            
         B     PS50                                                             
*                                                                               
PS10     CLI   MONRTYP,PRRQRTOD    ELSE IF ORDER DOWNLOAD                       
         BNE   PS20                                                             
         BAS   RE,FILLTMP          THEN ADD ORDER OBJS TO TEMPSTR               
         B     PS50                                                             
*                                                                               
PS20     DS    0H                                                               
         GOTO1 GETFACT,DMCB,(X'80',0),F#MIOST                                   
         BAS   RE,CALLPSNT         ELSE USE ACPRESENT TO FILL TEMPSTR           
*                                                                               
PS50     CLI   EMPFLAG,C'Y'        IF NO RECORDS ADDED TO TEMP FILE             
         BE    EMPOBJ              THEN RETURN GENERAL EMPTY OBJ                
*                                                                               
         LA    RF,WRKREC           ELSE PUT EOD ITEM TO TEMP FILE               
         XC    0(4,RF),0(RF)                                                    
         MVC   0(2,RF),=H'8'                                                    
         MVC   4(4,RF),=F'999'                                                  
         GOTO1 MYPUT,DMCB,WRKREC                                                
*                                                                               
         MVI   MDLAST,C'Y'         FIRST TRANSACTION IS ALSO LAST               
*                                                                               
PSX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE MIDDLE MODE.  IT CURRENTLY DOES NOTHING.           
*                                                                               
PROCMID  NTR1                                                                   
*                                                                               
PMX      B     XIT                                                              
         SPACE 3                                                                
* THIS ROUTINE PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.              
*                                                                               
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE GETS THE REQUEST NUMBER OBJECT FROM THE INPUT FRAME AND          
* MAKES SURE IT IS VALID.                                                       
*                                                                               
VALREQ   NTR1                                                                   
         GOTO1 GETITEM             GET FIRST ITEM - FILTER OBJECT               
         BNE   EXIT                                                             
*                                  MUST BE REQUEST NUMBER OBJECT                
         CLC   TYPENUM,=A(ITPRREQ)                                              
         BNE   ERRROBJ                                                          
*                                                                               
         L     R2,ADATA            CONVERT RECORD AND FILTER TYPES              
         USING PRRQOBJD,R2             INTO BINARY                              
         GOTO1 HEXIN,DMCB,PRRQRTYP,MONRTYP,4  SET TYPE/FILTER                   
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRROBJ                                                          
*                                                                               
         CLI   MONFTYP,PRRQFTDT    READ BY ACTIVITY POINTERS                    
         BNE   VR50                NO                                           
*                                                                               
         GOTO1 VALDTTM,DMCB,PRRQDATE,PRRQTIME    VALIDATE DATE/TIME             
*                                                                               
         GOTO1 SETDTTM,DMCB,PRRQDATE,PRRQTIME    SET MONDATE/MONTIME            
         B     VRX                                                              
*                                                                               
VR50     CLI   MONRTYP,PRRQRTJO   JOB OPEN                                      
         BNE   VRX                                                              
*                                                                               
         GOTO1 =A(BLDJOBKY),RR=APRELO                                           
*                                                                               
         USING ACTRECD,R3                                                       
         LA    R3,BIGKEY                                                        
         GOTO1 HIGH                                                             
         CLC   ACTKCULA,KEYSAVE                                                 
         BNE   ERRROBJ                                                          
*                                                                               
VRX      B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* THIS ROUTINE GETS THE CURRENT DATE/TIME AND BUILDS A DATE/TIME OBJECT         
* LATER TO BE PUT TO THE TEMP FILE.                                             
*                                                                               
BUILDDT  NTR1                                                                   
         L     R3,ACOMFACS         R3 = A(COMFACS)                              
         USING COMFACSD,R3                                                      
*                                                                               
         LA    R4,DTREC            BUILD TEMP RECORD IN WRKREC                  
         XC    0(4,R4),0(R4)           RECORD LENGTH = 4 + 4 + 12               
         MVC   0(2,R4),=H'20'                 = LENGTH + TYPE + DATA            
*                                                                               
*                                  DATE/TIME OBJECT TYPE IN BYTES 4-7           
         MVC   4(4,R4),=A(ITPRDTIM)                                             
*                                  CURRENT DATE IN BYTES 8-13                   
         GOTO1 CDATCON,DMCB,(5,0),(X'20',8(R4))                                 
*                                                                               
         TIME  DEC                 BINARY CURRENT TIME IN FULL                  
         ST    R0,FULL                                                          
*                                                                               
         XC    DUB,DUB             R2 = HOUR + 8                                
         ZIC   RF,FULL                                                          
         SLL   RF,4                                                             
         ST    RF,DUB+4                                                         
         OI    DUB+7,X'0F'                                                      
         CVB   R2,DUB                                                           
*&&US*&& LA    R2,6(R2)            US USES 6AM BASED TIME                       
*&&UK*&& LA    R2,0(R2)            UK USES REAL TIME                            
*                                                                               
         C     R2,=F'24'           IF HOUR IS PAST 24 THEN SUBTRACT             
         BL    PDT50                   24 AND ADD 1 DAY                         
         S     R2,=F'24'                                                        
         GOTO1 CADDAY,DMCB,8(R4),(X'20',8(R4)),1                                
*                                                                               
PDT50    CVD   R2,DUB              STORE R2 BACK TO BINARY HOUR                 
         L     RF,DUB+4                                                         
         SRL   RF,4                                                             
         STC   RF,FULL                                                          
*                                  CURRENT TIME IN BYTES 14-19                  
         GOTO1 CHEXOUT,DMCB,FULL,14(R4),3                                       
*                                                                               
PDTX     B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* THIS ROUTINE LOOPS THROUGH THE RECORDS FOR THE SPECIFIED RECORD               
* AND FILTER TYPE AND ADDS MAD OBJECTS TO THE TEMP FILE.                        
*                                                                               
FILLTMP  NTR1                                                                   
         LA    R2,BIGKEY           R2 = A(BIGKEY)                               
         L     R3,=A(RECFILT)      R3 = A(RECORD/FILTER/ROUTINES TABLE)         
         A     R3,APRELO                                                        
         USING RECTABD,R3                                                       
         MVI   MATCHFND,C'N'       NOT YET FOUND A MATCH                        
*                                                                               
FT10     CLI   RECRTYP,0           IF END OF TABLE REACHED THEN DONE            
         BE    FT200                                                            
*                                                                               
         CLC   MONRTYP,RECRTYP     IF RECORD/FILTER TYPES DON'T MATCH           
         BNE   FT100                   THEN BUMP TO NEXT TABLE ENTRY            
         CLC   MONFTYP,RECFTYP                                                  
         BNE   FT100                                                            
*                                                                               
FT50     MVI   MATCHFND,C'Y'       FOUND A MATCH - PROCESS ENTRY                
*                                                                               
         L     RF,RECAFST          GET FIRST KEY                                
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
*                                                                               
FT60     BNE   FT100               IF END OF KEYS THEN NEXT RECFILT             
*                                                                               
         GOTO1 GETREC              GET THE RECORD                               
*                                                                               
FT65     LA    R4,OBJECTS          SET AREA TO BUILD OBJECTS                    
         TM    RECSTAT,RECSUTIA    TEST TO USE TIA                              
         BZ    *+12                                                             
         L     R4,ATIA             USE ATIA TO BUILD OBJECTS                    
         A     R4,=A(LENGOOPT)                                                  
*                                                                               
         L     RF,RECABLD          BUILD MAD OBJECT BLOCK FROM RECORD           
         A     RF,APRELO                                                        
         GOTO1 (RF),DMCB,AIO,(R4)                                               
*                                                                               
         BNE   FT90                SKIP THIS RECORD IF ERROR                    
*                                                                               
         TM    RECSTAT,RECSNOBL    TEST TO SKIP BUILDING OBJECTS                
         BO    FT90                YES                                          
*                                                                               
FT70     OC    0(2,R4),0(R4)       IF END OF OBJECTS THEN DONE                  
         BZ    FT90                                                             
*                                                                               
         LA    RF,WRKREC           PUT OBJECT TO TEMP FILE                      
         XC    0(4,RF),0(RF)                                                    
         SR    RE,RE                                                            
         ICM   RE,3,0(R4)                                                       
         LA    RE,4(RE)                                                         
         STH   RE,0(RF)                                                         
         SHI   RE,5                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   4(0,RF),2(R4)                                                    
         GOTO1 MYPUT,DMCB,WRKREC                                                
         BNE   EXIT                                                             
*                                                                               
         MVI   EMPFLAG,C'N'        TEMP FILE IS NO LONGER EMPTY                 
*                                                                               
         SR    R5,R5               BUMP TO NEXT OBJECT                          
         ICM   R5,3,0(R4)                                                       
         LA    R4,2(R4,R5)                                                      
         B     FT70                AND LOOP BACK                                
*                                                                               
FT90     L     RF,RECANXT          GET NEXT KEY                                 
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         B     FT60                AND LOOP BACK                                
*                                                                               
FT100    LA    R3,RECTABL(R3)      BUMP TO NEXT RECFILT TABLE ENTRY             
         B     FT10                AND LOOP BACK                                
*                                                                               
FT200    CLI   MATCHFND,C'Y'       IF NO MATCH FOUND THEN ERROR                 
         BNE   ERRRF                                                            
*                                                                               
FTX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PASSES THE RECORD AND FILTER TYPE TO ACPRESENT WHICH             
* IN TURN READS THE RECORDS AND FEEDS THEM TO MASTHOOK.  MASTHOOK               
* WILL THEN WRITE MAD OBJECTS TO TEMPSTR FOR EACH RECORD.                       
*                                                                               
CALLPSNT NTR1  ,                                                                
         LA    R1,PSNTBLK                                                       
         USING PSNTBLKD,R1                                                      
         XC    PSNTBLK,PSNTBLK                                                  
         MVC   PSNTRTYP,MONRTYP                                                 
         MVC   PSNTFTYP,MONFTYP                                                 
         MVC   PSNTDATE,MONDATE                                                 
         MVC   PSNTTIME,MONTIME                                                 
         MVC   PSNTCUL,CUL                                                      
         MVC   PSNTLCLI,LCLI                                                    
         MVC   PSNTLPRO,LPRO                                                    
         MVC   PSNTLJOB,LJOB                                                    
         MVC   PSNTLOFF,LOFF       1R OFFICE CODE LENGTH                        
         MVC   PSNTLDEP,LDEP       1R DEP CODE LENGTH                           
         MVC   PSNTLSUB,LSUB       1R SUB-DEP CODE LENGTH                       
         MVC   PSNTCOMF,ACOMFACS                                                
         MVC   PSNTAIO,AIO                                                      
         LA    RF,MASTHOOK                                                      
         ST    RF,PSNTHOOK                                                      
*                                                                               
         L     RF,=V(PRESENT)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
*                                                                               
         CLI   PSNTMATF,C'Y'       IF NO MATCH FOUND THEN ERROR                 
         BNE   ERRRF                                                            
         DROP  R1                                                               
*                                                                               
CPX      B     XIT                                                              
         EJECT                                                                  
* THIS IS THE HOOK FOR MASTER FILE RECORDS PASSED BY ACPRESENT.  IT             
* CALLS ACPRESTO TO BUILD A BLOCK OF MAD OBJECTS AND WRITES THEM                
* TO THE TEMP FILE.                                                             
*                                                                               
MASTHOOK NTR1                                                                   
         LA    R4,OBJECTS          SET AREA TO BUILD OBJECTS                    
*                                                                               
         LA    R1,BLOCK            BUILD PARM BLOCK AND CALL ACPRESTO           
         USING ACPRESTD,R1            TO BUILD THE OBJECT BLOCK                 
         XC    ACPRESTD(ACPRL),ACPRESTD                                         
         LA    R2,PSNTBLK                                                       
         USING PSNTBLKD,R2                                                      
         MVC   ACPRREC,AIO                                                      
         ST    R4,ACPROBJ                                                       
         MVC   ACPRTYP,PSNTBTYP                                                 
         MVC   ACPRCOMF,ACOMFACS                                                
         MVC   ACPRLCLI,LCLI                                                    
         MVC   ACPRLPRO,LPRO                                                    
         MVC   ACPRLJOB,LJOB                                                    
         MVC   ACPRLOFF,LOFF       1R OFFICE CODE LENGTH                        
         MVC   ACPRLDEP,LDEP       1R DEP CODE LENGTH                           
         MVC   ACPRLSUB,LSUB       1R SUB-DEP CODE LENGTH                       
         MVC   ACPRSTA7,COMPSTA7                                                
         MVC   ACPRCUR,COMPCUR                                                  
         MVC   ACPRCURS,COMPCURS                                                
         L     RF,=V(PRESTO)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         DROP  R1,R2                                                            
*                                                                               
         BNE   MH90                SKIP THIS RECORD IF ERROR                    
*                                                                               
MH50     OC    0(2,R4),0(R4)       IF END OF OBJECTS THEN DONE                  
         BZ    MH90                                                             
*                                                                               
         LA    RE,WRKREC           PUT OBJECT TO TEMP FILE                      
         XC    0(4,RE),0(RE)                                                    
         SR    RF,RF                                                            
         ICM   RF,3,0(R4)                                                       
         LA    RF,4(RF)                                                         
         STH   RF,0(RE)                                                         
         SHI   RF,4                RF = LENGTH OF OBJECT                        
         AHI   RE,4                RE = ADDRESS OF DESTINATION                  
         LR    R1,RF               R1 = LENGTH OF OBJECT                        
         LA    R0,2(R4)            R0 = ADDRESS OF OBJECT                       
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 MYPUT,DMCB,WRKREC                                                
         BNE   EXIT                                                             
*                                                                               
         MVI   EMPFLAG,C'N'        TEMP FILE IS NO LONGER EMPTY                 
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,3,0(R4)          GET OBJECT LENGTH                            
         LA    R4,2(R4,R5)         BUMP TO NEXT OBJECT                          
         B     MH50                AND LOOP BACK                                
*                                                                               
MH90     DS    0H                                                               
*                                                                               
MHX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE READS THE KEY FOR THE ORDER NUMBER PASSED IN THE                 
* REQUEST OBJECT.  IF THE KEY IS NOT FOUND THEN IT GIVES AN ERROR.              
*                                                                               
FSTODSP  NTR1                                                                   
         LA    R2,BIGKEY           BUILD ORDER KEY FROM REQUEST OBJECT          
         USING ORDRECD,R2                                                       
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUL                                                      
         L     RF,ADATA                                                         
         USING PRRQOBJD,RF                                                      
         MVC   ORDKORD,PRRQORD                                                  
         DROP  RF                                                               
*                                                                               
         GOTO1 HIGH                READ FOR KEY                                 
*                                                                               
         CLC   ORDKEY,KEYSAVE      ERROR IF NOT FOUND                           
         BNE   ERRONF                                                           
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO              CHECK THAT ORDER IS A PRESTO ORDER           
         MVI   ELCODE,ORDELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   ERRONF                                                           
         USING ORDELD,R6                                                        
         CLI   ORDLN,ORDSTAT-ORDELD  TEST ELEMENT LONG ENOUGH                   
         BNH   ERRONF                                                           
         TM    ORDSTAT,ORDSPRES    TEST FOR PRESTO ORDER                        
         BZ    ERRONF                                                           
*                                                                               
FODX     B     YES                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
* THIS ROUTINE CALLS ACPRESTO TO FILL THE OBJECT BLOCK WITH ORDER               
* OBJECTS FROM THE ORDER RECORD IN AIO.                                         
*                                                                               
BUILDOD  NTR1                                                                   
         LA    R4,OBJECTS          SET AREA TO BUILD OBJECTS                    
*                                                                               
         LA    R1,BLOCK            BUILD PARM BLOCK AND CALL ACPRESTO           
         USING ACPRESTD,R1            TO BUILD THE OBJECT BLOCK                 
         XC    ACPRESTD(ACPRL),ACPRESTD                                         
         MVC   ACPRREC,AIO                                                      
         ST    R4,ACPROBJ                                                       
         MVI   ACPRTYP,ACPRBODQ                                                 
         MVC   ACPRCOMF,ACOMFACS                                                
         MVC   ACPRLCLI,LCLI                                                    
         MVC   ACPRLPRO,LPRO                                                    
         MVC   ACPRLJOB,LJOB                                                    
         MVC   ACPRSTA7,COMPSTA7                                                
         MVC   ACPRCUR,COMPCUR                                                  
         MVC   ACPRCURS,COMPCURS                                                
         L     RF,=V(PRESTO)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         DROP  R1                                                               
*                                                                               
         BNE   ERROAP              ERROR IF ACPRESENT HAD A PROBLEM             
*                                                                               
BODX     B     YES                                                              
         EJECT                                                                  
*                                                                               
*        JOB OPEN                                                               
*        SAVE REQUESTED JOB                                                     
*        CALL GETOPT IN PREPARATION FOR BUILDING JOB OPEN OBJECT                
*        NOTE: JOB IS VALIDATED IN VALREQ                                       
*                                                                               
FSTJOSP  NTR1                                                                   
*&&UK*&& GOTO1 GETFACT,DMCB,(X'80',0),F#MIOST                                   
         L     RF,ADATA                                                         
         USING PRRQOBJD,RF                                                      
         MVC   SPCLI,PRRQCLI       SAVE REQUESTED JOB                           
         MVC   SPPRO,PRRQPRD                                                    
         MVC   SPJOB,PRRQJOB                                                    
         DROP  RF                                                               
*                                                                               
         LH    RF,=Y(GOBBLKX-GOBLOCKD)   CLEAR GOBLOCK AND GOBBLOCK             
         LA    RE,GOBLK                                                         
         ST    RE,AGOBLOCK                                                      
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     R5,AGOBLOCK                                                      
         USING GOBLOCKD,R5                                                      
         LA    RE,GOBBLK           SET POINTER TO CBILL BLOCK                   
         ST    RE,GOABEXT                                                       
*                                  CALL GETOPT                                  
         GOTO1 =A(RDOPT),DMCB,(RC),(R5),RR=APRELO                               
*                                                                               
         CLC   GOSCHEME,BLANKS     CHECK FOR MISSING SCHEME                     
         BNH   BJWNOSCH                                                         
*                                                                               
         XC    GOABEXT,GOABEXT     DON'T NEED CBILL OPTIONS ANY MORE            
         GOTO1 =A(BLDJOBKY),RR=APRELO  GET JOB RECORD IN AIO                    
         USING ACTRECD,R3                                                       
         LA    R3,BIGKEY                                                        
         GOTO1 HIGH                                                             
         CLC   ACTKCULA,KEYSAVE    SET CONDITION CODE FOR XIT                   
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
*                                                                               
* BUILD A JOB ESTIMATE KEY BASED ON REQUEST JOB                                 
*        RETURNS CC NEQ IF NO JOB ESTIMATES ARE FOUND                           
*                                                                               
FSTJESP  NTR1                                                                   
         LA    R2,BIGKEY                                                        
         USING EVERECD,R2                                                       
         XC    EVEKEY,EVEKEY       BUILD ESTIMATE KEY                           
         MVI   EVEKTYP,EVEKTYPQ                                                 
         MVI   EVEKSUB,EVEKSUBQ                                                 
         MVC   EVEKCPY(3),CUL                                                   
*                                                                               
         L     R3,ADATA            FILL IN CLIENT PRODUCT JOB                   
         USING PRRQOBJD,R3         FROM REQUEST VALUES                          
         MVC   EVEKCLI,PRRQCLI                                                  
         MVC   EVEKPRO,PRRQPRD                                                  
         MVC   EVEKJOB,PRRQJOB                                                  
         MVI   EVEKTYPE,EVEKTPLN                                                
*                                                                               
         GOTO1 HIGH                GET FIRST KEY                                
         B     NJESTX              TEST FOR END                                 
*                                                                               
* THIS ROUTINE READS THE NEXT ESTIMATE RECORD FOR A JOB                         
* RETURNS NOT EQUAL IF NO MORE ESTIMATED FOR A JOB ARE FOUND                    
*                                                                               
NXTJESP  NTR1                                                                   
*                                                                               
NXTJESQ  GOTO1 SEQ                 GET NEXT KEY                                 
         CLC   EVEKEY(EVEKJOB-EVEKEY+L'EVEKJOB),KEYSAVE                         
         BNE   NJESTXX                                                          
         OC    EVEKWC,EVEKWC       IS THIS A TIME ESTIMATE RECORD?              
         BNZ   NXTJESQ             YES, SKIP IT                                 
*                                                                               
NJESTX   CLC   EVEKEY(EVEKJOB-EVEKEY+L'EVEKJOB),KEYSAVE                         
NJESTXX  B     XIT                                                              
*                                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
*                                                                               
*     PREP BUILDING THE JOB WORKCODE OBEJCTS                                    
*        JUST GET THE JOB IN AIO                                                
*                                                                               
FSTJWSP  NTR1                                                                   
         GOTO1 =A(BLDJOBKY),RR=APRELO                                           
*                                                                               
         USING ACTRECD,R3                                                       
         LA    R3,BIGKEY                                                        
         GOTO1 HIGH                                                             
         CLC   ACTKCULA,KEYSAVE                                                 
         B     XIT                 JOB VALIDATED IN VALREQ                      
         EJECT                                                                  
* THIS ROUTINE IS ONLY NECESSARY UNTIL WE RIP THE TABLE DRIVEN CODE             
* FROM THIS MODULE.                                                             
*                                                                               
NXTNO    NTR1                                                                   
         B     NO                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*     VALREQ SUPPORTING SUBROUTINES                                             
*---------------------------------------------------------------------          
*     VALIDATE THE DATE/TIME SENT IN P1 AND P2                                  
*     CALLS ERRDTTM IF THERE IS AN ERROR                                        
*---------------------------------------------------------------------          
*                                                                               
         USING DTTMD,R2                                                         
VALDTTM  NTR1  WORK=(R2,DTTMLN)                                                 
         L     RF,DMCB                                                          
         MVC   DTTMDATE,0(RF)      SAVE DATE                                    
         L     RF,DMCB+4                                                        
         MVC   DTTMTIME,0(RF)      SAVE TIME                                    
*                                                                               
         MVC   WORK(10),BLANKS                                                  
*&&US                                                                           
         MVC   WORK(2),DTTMDATE+2        MM                                     
         MVC   WORK+3(2),DTTMDATE+4      DD                                     
*&&                                                                             
*&&UK                                                                           
         MVC   WORK(2),DTTMDATE+4        DD                                     
         MVC   WORK+3(2),DTTMDATE+2      MM                                     
*&&                                                                             
         MVC   WORK+6(2),DTTMDATE        YY                                     
         USING COMFACSD,R3                                                      
         L     R3,ACOMFACS                                                      
         GOTO1 CDATVAL,DMCB,(0,WORK),WORK+10                                    
         CLI   DMCB+3,0            LENGTH RETURNED?                             
         BE    ERRDTTM             NO, DATE TIME ERROR                          
*                                                                               
         GOTO1 DECIN,DMCB,DTTMTIME,6                                            
         BNE   ERRDTTM                                                          
*                                                                               
         B     XIT                                                              
*                                                                               
* SET MONDATE/MONTIME FROM DMCB AND DMCB+4                                      
* INPUT PARM 1 IS A(DATE IN CL6'YYMMDD'), FROM PC                               
* INPUT PARM 2 IS A(TIME IN CL6'HHMMSS'), FROM PC                               
* MONDATE AND MONTIME ARE RETURNED AS EQUIVALENT MAINFRAME VALUES               
*                                                                               
*                                                                               
*                                                                               
SETDTTM  NTR1  WORK=(R2,DTTMLN)                                                 
         L     RF,DMCB                                                          
         MVC   DTTMDATE,0(RF)      SAVE DATE                                    
         L     RF,DMCB+4                                                        
         MVC   DTTMTIME,0(RF)      SAVE TIME                                    
         MVC   WORK,BLANKS                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(0,DTTMDATE),(2,MONDATE) START DATE                  
*                                                                               
         PACK  DUB,DTTMTIME(2)     CONVERT START TIME TO BINARY                 
         CVB   RF,DUB                                                           
         STC   RF,WORK+20          BINARY HOURS                                 
         PACK  DUB,DTTMTIME+2(2)                                                
         CVB   RF,DUB                                                           
         STC   RF,WORK+21          BINARY MIN'S                                 
         PACK  DUB,DTTMTIME+4(2)                                                
         CVB   RF,DUB                                                           
         STC   RF,WORK+22          BINARY SECONDS                               
*                                                                               
         XR    RF,RF                                                            
         IC    RF,WORK+20         CONVERT TO MAINFRAME VALUE                    
*&&US*&& SHI   RF,6               IS DATE BEFORE 6:00 AM                        
*&&UK*&& SHI   RF,0                                                             
         STC   RF,WORK+20                                                       
         BNM   SETD30              NO, DATE IS FINE AS IS                       
*                                                                               
*                                  ADJUST DATE WHEN TIME REQUESTED IS           
*                                  BEFORE 6:00 A.M.                             
*                                                                               
         AHI   RF,24               ADD 24 HOURS BACK TO TIME                    
         STC   RF,WORK+20          SAVE                                         
*                                                                               
         USING COMFACSD,R3                                                      
         L     R3,ACOMFACS                                                      
         XR    RF,RF               SUBTRACT 1 FROM DATE                         
         BCTR  RF,0                                                             
         ST    RF,DMCB+8                                                        
         GOTO1 CADDAY,DMCB,DTTMDATE,WORK                                        
         GOTO1 DATCON,DMCB,(0,WORK),(2,MONDATE) SAVE DATE                       
*                                                                               
SETD30   XR    R1,R1               CONVERT TIME TO BINARY SECONDS               
         ZIC   R1,WORK+20          GET BINARY HOURS                             
         MH    R1,=Y(SECSINHR)     CONVERT TO BINARY SECONDS                    
*                                                                               
         XR    RF,RF                                                            
         IC    RF,WORK+21          GET BINARY MIN'S                             
         MH    RF,=Y(SECSINMN)     CONVERT TO BINARY SECONDS                    
         AR    R1,RF               ACCUMULATE                                   
*                                                                               
         XR    RF,RF                                                            
         IC    RF,WORK+22                                                       
         AR    R1,RF               ADD BINARY SECONDS TO SUM OF HRS/MN          
         STCM  R1,7,MONTIME        SAVE 3 BYTE BINARY TIME                      
         B     XIT                                                              
*                                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
*---------------------------------------------------------------------          
*        SET R3 TO THE DATA OF THE OBJECT AT 0(R3)                              
*        USES  R1 AND RF !                                                      
*---------------------------------------------------------------------          
*                                                                               
OBJDATA  EQU   *                                                                
         ZIC   RF,0(R3)            GET L'LEN                                    
         ZIC   R1,1(R3)            GET L'TYPE                                   
         LA    R3,2(R3)            BUMP PAST L'LEN AND L'TYPE                   
         LA    R3,0(RF,R3)         BUMP PAST LEN                                
         LA    R3,0(R1,R3)         BUMP PAST TYPE                               
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* BUILD JOB OPEN OBJECT, GETOPT CALLED IN FSTJOSP                               
*                                                                               
BUILDJO  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         L     R5,AGOBLOCK                                                      
         USING GOBLOCKD,R5                                                      
         MVC   0(2,R3),=Y(PRJOOBJL+4)   SET JOB OPEN OBJ HEADERS                
         MVC   2(4,R3),=A(ITPRRTJO)                                             
*                                                                               
         USING PRJOOBJD,R3         BUMP TO OBJECT DATA AND PRECLEAR             
         LA    R3,6(R3)                                                         
         MVC   0(PRJOOBJL,R3),BLANKS                                            
*                                                                               
         MVC   PRJOCLCD,SPCLI      CLIENT                                       
         MVC   PRJOPRCD,SPPRO      PRODUCT                                      
         MVC   PRJOJBCD,SPJOB      JOB                                          
         MVC   PRJOSCH,GOSCHEME    SCHEME                                       
         MVC   PRJONAE,GONEEDAE    NEED ESTIMATE APPROVAL FLAG                  
         MVC   PRJOBT,GOBILTYP     BILL TYPE                                    
         EDIT  (P4,GOOVRPER),(5,PRJOMPE),FILL=0  MAX OVER EST PCT               
         MVC   JOBCUR,GOBILCUR     BILLING CURRENCY                             
         MVI   SECCURR,C'N'        SET SECOND CURRENCY FLAG TO 'NO'             
*&&UK                                                                           
         TM    COMPSTA7,CPYSSCNV   TEST CONVERTED AGENCY                        
         BZ    BJO20                                                            
*                                                                               
         CLC   JOBCUR,COMPCUR      TEST PRIMARY CURRENCY                        
         BE    BJO20               YES-ALL DONE                                 
         CLC   JOBCUR,COMPCURS     TEST SECONDARY CURRENCY                      
         BE    *+14                YES-SET FLAG ACCORDINGLY                     
         MVC   JOBCUR,COMPCUR                                                   
         B     BJO20                                                            
*                                                                               
         MVI   SECCURR,C'Y'                                                     
*&&                                                                             
*                                                                               
BJO20    MVC   PRJOBCUR,JOBCUR                                                  
         MVI   ELCODE,JOBELQ       GET NEXT BILLING PERCENTAGE                  
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING JOBELD,R6                                                        
         MVI   XJOB,C'N'           SET XJOB FLAG                                
         TM    JOBSTA1,JOBSXJOB    TEST FOR XJOB                                
         BZ    *+8                                                              
         MVI   XJOB,C'Y'           YES                                          
*                                                                               
         CLI   GOBILTYP,C'E'       PCT OF EST BILLING                           
         BNE   BJO50                                                            
         L     R1,GOBILAM1                                                      
         TM    JOBBIST,JOBB1BIL    FIRST BILLING DONE?                          
         BZ    BJO30               NO, CURRENT PCT IN R1                        
*                                                                               
         A     R1,GOBILAM2                                                      
         TM    JOBBIST,JOBB2BIL    SECOND?                                      
         BZ    BJO30               NO, CURRENT PCT IN R1                        
*                                                                               
         A     R1,GOBILAM3                                                      
*                                                                               
BJO30    CVD   R1,DUB                                                           
         EDIT  (P8,DUB),(6,PRJOBPCT),FILL=0                                     
*                                                                               
BJO50    LA    R3,PRJOOBJL(R3)     BUMP R3                                      
         BAS   RE,BUILDUF          BUILD ANY USER FIELD OBJECTS                 
*                                                                               
BJOX     XC    0(2,R3),0(R3)       SET END OF OBJECT BLOCK                      
         B     YES                                                              
         DROP  R3,R5,R6                                                         
         EJECT                                                                  
*                                                                               
*        BUILD JOB USER FIELD OBJECTS                                           
*              AT ENTRY, R3=A(OBJECTS), AIO = A(JOB)                            
*              CALLED BY BUILDJO, ON EXIT, RETURN R3 TO CALLER                  
*                                                                               
BUILDUF  NTR1  ,                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,UFSELQ       USER FIELD ELEMENT                           
         BAS   RE,GETEL                                                         
*                                                                               
BUF10    BNE   BUFX                ALL DONE WITH PROCESSING                     
         USING UFSELD,R6                                                        
*                                                                               
         CLI   UFSLN,UFSDATA-UFSELD TEST FOR NO DATA                            
         BE    BUF20                YES-SKIP THE ELEMENT                        
*                                                                               
         MVC   0(2,R3),=Y(PRUFOBJL+4)                                           
         MVC   2(4,R3),=A(ITPRJMRT)                                             
         LA    R3,6(R3)            BUMP POINTER TO OBJECT DATA                  
         USING PRUFOBJD,R3                                                      
*                                                                               
         MVC   PRUFOBJD(PRUFOBJL),BLANKS                                        
         MVC   PRUFCODE,UFSCODE                                                 
         MVC   PRUFNAME,UFSDESC                                                 
         GOTO1 HEXOUT,DMCB,UFSSEQ,PRUFSEQ,L'UFSSEQ,0                            
*                                                                               
         MVI   PRUFSHES,C'0'       SHOW ON ESTIMATES FLAG= 0 OR 1               
         TM    UFSSTAT,UFSSSHES                                                 
         BZ    *+8                                                              
         MVI   PRUFSHES,C'1'                                                    
*                                                                               
         ZIC   R1,UFSLN                                                         
         SH    R1,=Y(UFSDATA-UFSELD+1)                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRUFDATA(0),UFSDATA                                              
         LA    R3,PRUFOBJL(R3)     BUMP TO NEXT OBJECT POSITION                 
*                                                                               
BUF20    BAS   RE,NEXTEL                                                        
         B     BUF10                                                            
*                                                                               
BUFX     XIT1  REGS=(R3)                                                        
         DROP  R3,R6                                                            
         EJECT                                                                  
*                                                                               
*        BUILD JOB ESTIMATE OBJECT FOR THE ESTIMATE RECORD IN PARM 1            
*              BUILD CELL OBJECTS FOR EACH WORKCODE ON THER ESTIMATE            
*                                                                               
BUILDJE  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVC   0(2,R3),=Y(PREROBJL+4)                                           
         MVC   2(4,R3),=A(ITPRRTER)                                             
*                                                                               
         USING EVERECD,R2                                                       
         USING PREROBJD,R3         BUMP TO OBJECT DATA AND PRECLEAR             
         LA    R3,6(R3)                                                         
         MVC   0(PREROBJL,R3),BLANKS                                            
         MVC   PRERETYP,EVEKTYPE                                                
         EDIT  EVEKVER,(3,PREREVER),FILL=0                                      
*                                                                               
         MVI   BYTE,FFTTACUR       LOOK FOR CURRENCY ELEMENT                    
         GOTO1 SRCHGET,DMCB,FFTELQ,(1,BYTE)                                     
         BNE   *+10                                                             
         USING FFTELD,R6                                                        
         MVC   PRERCUR,FFTDATA     EXTRACT CURRENCY                             
*                                                                               
         LR    R6,R2                                                            
*                                                                               
         MVI   ELCODE,ENAELQ       GET ESTIMATE NAME DATA                       
         BAS   RE,GETEL                                                         
         BNE   BJE010                                                           
*                                                                               
         USING ENAELD,R6                                                        
         ZIC   R1,ENALN            SET ESTIMATE NAME                            
         SHI   R1,3                                                             
         CHI   R1,L'PRERNAM1                                                    
         BNH   *+8                                                              
         LA    R1,L'PRERNAM1                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRERNAM1(0),ENAME                                                
*                                                                               
         BAS   RE,NEXTEL           IS THERE A SECOND ESTIMATE NAME              
         BNE   BJE010              NO                                           
*                                                                               
         ZIC   R1,ENALN            WRITE NAME 2                                 
         SHI   R1,3                                                             
         CHI   R1,L'PRERNAM2                                                    
         BNH   *+8                                                              
         LA    R1,L'PRERNAM2                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRERNAM2(0),ENAME                                                
*                                                                               
BJE010   LR    R6,R2                                                            
*                                                                               
         MVI   ELCODE,EUPELQ       GET ESTIMATE DATE/TIME                       
         BAS   RE,GETEL                                                         
         BNE   BJE020                                                           
*                                                                               
         USING EUPELD,R6                                                        
         MVC   PRERPERS,EUPERS     LAST CHANGE PERSON                           
         GOTO1 DATCON,DMCB,(1,EUPLAST),(2,MONDATE)  SET MONDATE/TIME            
         MVC   MONTIME,EUPTIME                      FOR MAIN2PC                 
*                                                                               
         GOTO1 =A(MAIN2PC),DMCB,(RC),PRERDATE,PRERTIME,RR=APRELO                
*                                                                               
BJE020   LR    R6,R2                                                            
         MVI   ELCODE,EAPELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   BJE030                                                           
         USING EAPELD,R6                                                        
*                                                                               
         MVC   PRERAPPR,EAPPBY                                                  
         GOTO1 DATCON,DMCB,(1,EAPDATE),(X'20',PRERAPDT)                         
*                                                                               
BJE030   LR    R6,R2                                                            
         MVI   ELCODE,EPRELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   BJE040                                                           
         USING EPRELD,R6                                                        
*                                                                               
         MVC   PRERPREP,EPRPREP                                                 
         GOTO1 DATCON,DMCB,(1,EPRDATE),(X'20',PRERPRDT)                         
*                                                                               
BJE040   LA    R3,PREROBJL(R3)     BUMP PAST ESTIMATE OBJECT                    
*                                                                               
         USING PRCEOBJD,R3         BUILD CELL OBJECTS                           
         USING EDAELD,R6           FROM ESTIMATE DATA ELEMENTS                  
         LR    R6,R2                                                            
         MVI   ELCODE,EDAELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   BJEX                NO DATA ON ESTIMATE                          
*                                                                               
BJE050   TM    EDATYPE,EDATWORK    TEST FOR WORKCODE ESTIMATE                   
         BZ    BJE090              NO                                           
*                                                                               
         MVC   0(2,R3),=Y(PRCEOBJL+4)                                           
         MVC   2(4,R3),=A(ITPRRTCE)                                             
         LA    R3,6(R3)                                                         
*                                                                               
         MVC   0(PRCEOBJL,R3),BLANKS                                            
         MVC   PRCEWCOD,EDAWORK    CELL WORKCODE                                
*                                                                               
         ZAP   DUB,EDACOMM         GET ESTIMATE AMOUNT                          
         TM    EDATYPE,EDATCOHE    COMMISSION OVERRIDE?                         
         BO    BJE060              YES, IT'S IN NON-COMM                        
         CLI   EDALN,EDALNQ2       NO, ANY NON-COMM AMOUNT?                     
         BL    *+10                NO                                           
         AP    DUB,EDANCOM         YES, ADD TO COMMISSIONABLE                   
*                                                                               
*                                  PROTECT AGAINST BAD DATA(TEST FILES)         
BJE060   CP    DUB,MAXEST          TEST FOR FULLWORD OVERFLOW                   
         BNH   *+10                                                             
         ZAP   DUB,MAXEST          YES-SET VALUE TO MAXIMUM                     
         CP    DUB,MINEST                                                       
         BNL   *+10                                                             
         ZAP   DUB,MINEST                                                       
*                                                                               
         CVB   R1,DUB              CONVERT ESTIMATE & MOVE TO CELL              
         ST    R1,FULL                                                          
         GOTO1 HEXOUT,DMCB,FULL,PRCEAMNT,4                                      
*                                                                               
         TM    EDATYPE,EDATCOHE    COMMISSION OVERRIDE?                         
         BZ    BJE070              NO                                           
         ZAP   DUB,EDANCOM         YES, GET AMOUNT                              
         CVB   R1,DUB              CONVERT IT & MOVE TO CELL                    
         ST    R1,FULL                                                          
         GOTO1 HEXOUT,DMCB,FULL,PRCECOMM,4                                      
         MVI   PRCECOVR,C'Y'       SET THE FLAG                                 
*                                                                               
BJE070   MVC   PRCESUFF,=C'00'     SET SUFFIX=0                                 
         TM    EDATYPE,EDATSUB     TEST FOR SUB-WORKCODE                        
         BZ    BJE080              NO                                           
         GOTO1 HEXOUT,DMCB,EDASUBC,PRCESUFF,L'EDASUBC                           
*                                                                               
BJE080   LA    R3,PRCEOBJL(R3)                                                  
*                                                                               
BJE090   BAS   RE,NEXTEL                                                        
         BE    BJE050                                                           
*                                                                               
BJEX     XC    0(2,R3),0(R3)       SET END OF OBJECT BLOCK                      
         B     YES                                                              
         DROP  R2,R3,R6                                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
*    BUILD JOB WORKCODE OBJECTS FOR THE LIST OF WORKCODES WHICH IS              
*    DEFINED AS THE UNION OF WORKCODES IN THE JOBS SCHEME, WORKCODES            
*    WHICH HAVE BEEN ESTIMATED AGAINST, AND WORKCODES AGAINST WHICH             
*    CHARGES HAVE BEEN BATCHED.                                                 
*    THE LIST OF WORKCODES IS BUILT USING TSAR BUFFERING.                       
*    FOR EACH WORKCODE IN THE LIST, ACTUAL AMOUNTS, COMMISSION RATES            
*    AND ACTUAL CHARGES ARE MAINTAINED                                          
*----------------------------------------------------------------------         
*    JWBUF IS STORAGE NEEDED TO FOR VATICAN CALL                                
*----------------------------------------------------------------------         
BUILDJW  NTR1  WORK=(R2,JWBUFSZ)                                                
         ST    R2,ATAXTAB                                                       
         XC    0(TAXTABSZ,R2),0(R2)                                             
         LA    R5,TAXTABSZ(R2)                                                  
         ST    R5,AVATBUFF                                                      
*                                                                               
         LA    R5,GOBLK                                                         
         ST    R5,AGOBLOCK                                                      
*                                                                               
         LH    RF,=Y(GOBLOCKX-GOBLOCKD)  CLEAR GOBLOCK                          
         L     RE,AGOBLOCK                                                      
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
*                                                                               
         GOTO1 =A(SETTSAR),DMCB,(RC),RR=APRELO                                  
*                                                                               
         GOTO1 =A(RDOPT),DMCB,(RC),AGOBLOCK,RR=APRELO                           
*                                                                               
*&&US                                                                           
         CLI   CTRY,CTRYCAN        CANADA                                       
         BNE   *+8                                                              
*&&                                                                             
         OI    RUNOPT,NEEDTAX                                                   
*                                                                               
*        CALL JOBBER, WRITE SCHEME WORKCODES TO TSAR                            
*                                                                               
         GOTO1 =A(JWPUTSCH),DMCB,(RC),RR=APRELO                                 
*                                                                               
*        PUT WORKCODES WHICH HAVE BEEN ESTTIMATED TO TSAR                       
*                                                                               
         GOTO1 =A(JWPUTEST),DMCB,(RC),RR=APRELO                                 
*                                                                               
*        PUT WORKCODES WHICH ARE IN OPEN ORDERS TO TSAR                         
*                                                                               
         GOTO1 =A(JWPUTORD),DMCB,(RC),RR=APRELO                                 
*                                                                               
*        PUT ACTUAL WORKCODES TO TSAR                                           
*                                                                               
         GOTO1 =A(JWPUTACT),RR=APRELO                                           
*                                                                               
*        BUILD JOB WORKCODE OBJECTS FROM TSAR                                   
*                                                                               
         GOTO1 =A(BLDJWOBJ),RR=APRELO                                           
*                                                                               
         B     YES                                                              
*                                                                               
         EJECT                                                                  
* THIS ROUTINE PASSES THE CALLER'S PARAMETERS ALONG TO WRKPUT BUT WILL          
* CALL WRKCRE IF IT HAS NOT BEEN CALLED YET THIS TRANSACTION.                   
*                                                                               
MYPUT    NTR1                                                                   
         L     R2,0(R1)            SAVE PARM 1                                  
*                                                                               
         CLI   WRKISCRE,C'N'       IF WORKER FILE NOT YET CREATED               
         BNE   MY10                                                             
         GOTO1 WRKCRE,DMCB,0                                                    
         MVI   WRKISCRE,C'Y'       CREATE IT                                    
         GOTO1 WRKPUT,DMCB,DTREC   PUT DATE/TIME OBJECT TO IT                   
*                                                                               
MY10     GOTO1 WRKPUT,DMCB,(R2)    PUT THE RECORD AS REQUESTED                  
*                                                                               
MYX      B     XIT                                                              
         EJECT                                                                  
*              INVALID REQUEST OBJECT                                           
ERRROBJ  MVC   APPLERR,=Y(PRERROBJ)                                             
         B     ERROBJ                                                           
*              INVALID REQUEST RECORD/FILTER TYPES                              
ERRRF    MVC   APPLERR,=Y(PRERRF)                                               
         B     ERROBJ                                                           
*                                                                               
ERRDTTM  MVC   APPLERR,=Y(PRERDTTM)                                             
         B     ERROBJ                                                           
*                                                                               
ERRONF   MVC   APPLERR,=Y(PRERONF)                                              
         B     ERROBJ                                                           
*                                                                               
ERROAP   MVC   APPLERR,=Y(PREROAP)                                              
         B     ERROBJ                                                           
*                                                                               
BJWNOSCH MVC   APPLERR,=Y(PRERSCH) MISSING SCHEME ON JOB                        
         B     ERROBJ                                                           
*                                  UPLOAD TO READ ONLY FILE                     
ERRUPRO  MVC   APPLERR,=Y(PRERUPRO)                                             
         B     ERROBJ                                                           
*                                  RETURN APPL ERRORS IN ERR OBJECT             
ERROBJ   GOTO1 HEXOUT,DMCB,APPLERR,FULL,2                                       
         GOTO1 PUTITEM,DMCB,A(ITGENERR),4,FULL                                  
         BNE   EXIT                                                             
         B     SETMDLST                                                         
*                                  RETURN APPL ERRORS IN ERR OBJECT             
EMPOBJ   GOTO1 PUTITEM,DMCB,A(ITGENEMP),0                                       
         BNE   EXIT                                                             
         B     SETMDLST                                                         
*                                                                               
SETMDLST MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
         B     EXIT                                                             
*                                                                               
EXIT     CLI   WRKISCRE,C'N'       IF TEMP FILE OPENED                          
         BE    EXIT10                                                           
         GOTO1 WRKCLOS             CLOSE TEMP FILE                              
*                                                                               
EXIT10   L     RD,SAVEDRD          RESTORE RD                                   
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*                                                                               
*        CONSTANTS AND EQUATES                                                  
*                                                                               
BLANKS   DC    CL256' '                                                         
DATADISP DC    Y(ACCRFST-ACCRECD)                                               
*                                                                               
MAXEST   DC    P'2147000000'                                                    
MINEST   DC    P'-2147000000'                                                   
*                                                                               
SECSINMN EQU   60                  SECONDS IN A MIN                             
SECSINHR EQU   60*SECSINMN         NUMBER OF SECONDS IN AN HOUR                 
EIGHTHRS EQU   8*SECSINHR          SECONDS IN EIGHT HOURS                       
TWO4HRS  EQU   24*SECSINHR         SECONDS IN A DAY                             
*                                                                               
LENTIA   EQU   18432                                                            
LENGOOPT EQU   4000                                                             
LENJBBUF EQU   LENTIA-LENGOOPT                                                  
JWBUFSZ  EQU   TAXTABSZ+VTCLNQ                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
RECFILT  DS    0A                                                               
         DC    AL1(PRRQRTJO,PRRQFTSP,0,0)                                       
         DC    A(FSTJOSP,NXTNO,BUILDJO)                                         
*                                                                               
         DC    AL1(PRRQRTJO,PRRQFTSP,0,0)                                       
         DC    A(FSTJESP,NXTJESP,BUILDJE)                                       
*                                                                               
         DC    AL1(PRRQRTJO,PRRQFTSP,RECSNOBL+RECSUTIA,0)                       
         DC    A(FSTJWSP,NXTNO,BUILDJW)                                         
*                                                                               
         DC    AL1(PRRQRTOD,PRRQFTSP,0,0)                                       
         DC    A(FSTODSP,NXTNO,BUILDOD)                                         
*                                                                               
         DC    AL1(0)                                                           
         DROP  RA                  DROP SECOND BSE REG BEFORE NMODS             
         EJECT                                                                  
*---------------------------------------------------------------------          
*        READ JOB CHARGES, SET ACTUAL BUCKETS, PUT RECORD TO TSAR               
*---------------------------------------------------------------------          
*                                                                               
JWPUTACT NMOD1 0,**JWPU**                                                       
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         GOTO1 =A(BLDJOBKY),RR=APRELO  SET JOB KEY                              
         GOTO1 HIGH                                                             
*                                                                               
         USING TRNRECD,R6                                                       
JWPA010  LA    R6,BIGKEY                                                        
         MVC   MYKEYSV,BIGKEY      SAVE KEY IN CASE OF INTERVENING IO'S         
*                                                                               
         CLC   TRNKCULA,KEYSAVE    READING SAME JOB?                            
         BNE   JWPAX               NO, DONE                                     
*                                                                               
         CLC   TRNKREF,=CL15' '    IS THIS A TRANSACTION                        
         BNH   JWPA150             NO, GET NEXT                                 
*                                                                               
         CLC   TRNKWORK,=C'**'     PURCHASE ORDER                               
         BE    JWPA150             YES                                          
         CLC   TRNKWORK,=C'99'     BILLING?                                     
         BE    JWPA150             YES                                          
*                                                                               
         BAS   RE,JWCRCLR          CLEAR BLOCK TO BUILD TSAR RECORD             
         LA    R5,BLOCK                                                         
         USING JWTABD,R5                                                        
         USING JWCABD,BLOCK2                                                    
*                                                                               
         MVC   JWCCONTR,TRNKULC    SAVE THE CONTRA IN BLOCK2                    
*                                                                               
         GOTO1 GETREC              GET THE TRANSACTION RECORD                   
*                                                                               
         L     R6,AIO                                                           
         TM    TRNRSTAT,TRNSDRFT   SKIP DRAFT ITEMS FOR NOW                     
         BO    JWPA150                                                          
*                                                                               
         GOTO1 GETELEM,DMCB,TRNELQ,0                                            
         BNE   JWPA150             NOT A TRAN, GET NEXT                         
         USING TRNELD,R6                                                        
         ST    R6,ATRNEL           SAVE A(TRANSACTION ELEMENT)                  
         MVC   WORKCODE,TRNANAL    SET WORKCODE FOR GETWCRAT CALL               
         GOTO1 =A(GETWCRAT),DMCB,(RC),RR=APRELO                                 
*                                                                               
         MVC   JWTWC,TRNANAL                                                    
         ZAP   JWTCRATE,COMRATE    COMRATE RETURNED BY GETWCRAT                 
         OI    JWTSTAT,JWTSRATE                                                 
*                                                                               
         TM    RUNOPT,NEEDTAX      DO I NEED TO GET VAT/GST RATE                
         BNO   JWPA020                                                          
         GOTO1 =A(GETRATE),DMCB,(RC),RR=APRELO RETURN TAX RATE IN PL16          
         ZAP   JWTTRATE,PL16       GET VAT RATE                                 
*                                                                               
JWPA020  DS    0H                                                               
*&&UK                                                                           
         TM    COMPSTA7,CPYSSCNV   TEST EURO ZONE COMPANY                       
         BZ    JWPA030             NO                                           
         BAS   RE,EURACT                                                        
         B     JWPA050                                                          
*&&                                                                             
JWPA030  LA    R4,JWTBUCKS         ADDRESS BUCKETS                              
         USING BUCKD,R4                                                         
*                                                                               
         L     R6,ATRNEL           RESTORE R6=A(TRANSACTION)                    
         USING TRNELD,R6                                                        
         ZAP   DUB,TRNAMNT                                                      
*                                                                               
         CLI   XJOB,C'Y'           TEST FOR XJOB                                
         BNE   JWPA040             NO                                           
*                                                                               
*&&US                                                                           
         MVI   BYTE,SCITSJXP       LOOK FOR 'S' BUCKETS                         
         GOTO1 SRCHGET,DMCB,SCIELQ,(1,BYTE)                                     
         BNE   *+10                COULD NOT FIND IT                            
         USING SCIELD,R6                                                        
         ZAP   DUB,SCIAMNT         GET MEMO AMOUNT                              
*&&                                                                             
*                                                                               
JWPA040  BAS   RE,BUCKET                                                        
*                                                                               
JWPA050  L     R6,ATRNEL              RESTORE TRANSACTION                       
         USING TRNELD,R6                                                        
         LR    R4,R6                  SAVE TRANSACTION ADDRESS                  
         LA    R2,JWTTBUCK            GET TIME BUCKETS                          
         CLI   TRNTYPE,49             TYPE 49 IS ALWAYS TIME                    
         BE    JWPA055                                                          
*&&UK                                                                           
         CLI   TRNTYPE,34             SOME TYPE 34'S ARE TIME                   
         BNE   JWPA110                                                          
         B     JWPA060                THEY ARE ONLY B TIME W/SCIELS             
*&&                                                                             
JWPA055  GOTO1 GETELEM,DMCB,PRTELQ,0                                            
         BNE   JWPA110                 NO PERSONNEL ELEMENT                     
         USING PRTELD,R6                                                        
*&&US                                                                           
         TM    PRTSTAT,PRTSBILQ        IS IT 'B' TIME?                          
         BZ    *+10                    NO                                       
         ZAP   0(BUCKLN,R2),PRTHOUR                                             
         LA    R2,BUCKLN(0,R2)                                                  
         TM    PRTSTAT,PRTSNOTQ        IS IT 'N' TIME?                          
         BZ    *+10                    NO                                       
         ZAP   0(BUCKLN,R2),PRTHOUR                                             
         LA    R2,BUCKLN(0,R2)                                                  
         TM    PRTSTAT,PRTSRTEQ        MUST BE 'R' TIME                         
         BZ    JWPA110                                                          
         ZAP   0(BUCKLN,R2),PRTHOUR                                             
         LA    R2,BUCKLN(0,R2)                                                  
         ZAP   PL16,PRTRATE                                                     
         MP    PL16,PRTHOUR                                                     
         SRP   PL16,64-2,5                                                      
         ZAP   0(BUCKLN,R2),PL16                                                
*&&                                                                             
*&&UK                                                                           
         CP    TRNAMNT-TRNEL(L'TRNAMNT,R4),=P'0' ANY AMOUNT?                    
         BNE   JWPA060                YES, MUST BE 'B' TIME                     
*                                                                               
         LA    R2,BUCKLN(0,R2)        NO, IS THERE A RATE?                      
         CP    PRTRATE,=P'0'                                                    
         BE    JWPA060                NO, IT'S 'N' TIME                         
*                                                                               
         LA    R2,BUCKLN(0,R2)        MUST BE 'R' TIME                          
*                                                                               
JWPA060  MVI   BYTE,SCITSJHR          LOOK FOR 'T' BUCKETS                      
         GOTO1 SRCHGET,DMCB,SCIELQ,(1,BYTE)                                     
         BNE   JWPA110                COULD NOT FIND IT                         
         USING SCIELD,R6                                                        
         ZAP   0(BUCKLN,R2),SCIAMNT                                             
*                                                                               
         LA    RE,JWTRHR              DO WE HAVE 'R' TIME?                      
         CR    R2,RE                  NOPE, ALL DONE                            
         BNE   JWPA110                                                          
*                                                                               
         LA    R2,BUCKLN(0,R2)        YES, GET THE RVALUE                       
         MVI   BYTE,SCITCRAT          LOOK FOR 'C' BUCKETS                      
         GOTO1 SRCHGET,DMCB,SCIELQ,(1,BYTE)                                     
         BNE   JWPA110                COULD NOT FIND IT                         
         USING SCIELD,R6                                                        
         ZAP   0(BUCKLN,R2),SCIAMNT                                             
*                                                                               
         LA    R2,BUCKLN(0,R2)                                                  
         GOTO1 GETELEM,DMCB,OCAELQ,0  GET SECOND CURRENCY                       
         BNE   JWPA110                                                          
         USING OCAELD,R6                                                        
         SR    R0,R0                                                            
         IC    R0,OCANUM              GET NUMBER OF ENTRIES                     
         LA    RE,OCANTRY             GET FIRST ENTRY                           
*                                                                               
         USING OCANTRY,RE                                                       
JWPA080  CLI   OCANTYPE,QSCITCOMM     IS IT THE EXTENDED COST?                  
         BE    JWPA100                YES                                       
         CLI   OCANTYPE,QPRTRATE      IS IT THE RATE?                           
         BE    *+8                    YES, ONLY MOVE 6 BYTES                    
*                                                                               
JWPA090  LA    RE,2(RE)               NO, MOVE 2 + 6                            
         LA    RE,OCANTRYL-2(RE)                                                
         BCT   R0,JWPA080                                                       
         B     JWPA110                                                          
*                                                                               
JWPA100  CLI   OCANSEQN,2             MUST BE SEQUENCE NUMBER 2                 
         BNE   JWPA090                                                          
         ZAP   0(BUCKLN,R2),OCANCASH  GET THE 2ND CURRENCY VALUE                
*&&                                                                             
*                                                                               
JWPA110  GOTO1 =A(PUTTSAR),DMCB,(RC),BLOCK,RR=APRELO                            
*                                                                               
         LA    R2,JWTTBUCK         LOOK FOR DATA IN TIME BUCKETS                
         LA    R0,JWCTADDS                                                      
*                                                                               
JWPA120  CP    0(BUCKLN,R2),=P'0'     ONLY WANT THIS IF TIME                    
         BNE   JWPA130                                                          
         LA    R2,BUCKLN(0,R2)                                                  
         BCT   R0,JWPA120                                                       
         B     JWPA140                                                          
*                                                                               
JWPA130  MVC   JWCWC,JWTWC            SET WC AND BUCKETS                        
         MVC   JWCNBKT(JWCNBKTL),JWTBUCKS                                       
         MVC   JWCSBKT(JWCSBKTL),JWTSBUCK                                       
         MVC   JWCTBKT(JWCTBKTL),JWTTBUCK                                       
         GOTO1 =A(PUTTSA2),DMCB,(RC),BLOCK2,RR=APRELO                           
*                                                                               
JWPA140  XC    BIGKEY,BIGKEY       RESTORE READ SEQ                             
         LA    R6,BIGKEY           BUMP KEY                                     
         USING TRNRECD,R6                                                       
         MVC   TRNKEY(TRNKEND),MYKEYSV                                          
*                                                                               
* BUMP KEY TO GET NEXT (USE 2 BYTES BECAUSE TRNKSBR CAN REACH 255               
         ZICM  RF,TRNKREF+L'TRNKREF-1,(3)     BUMP KEY TO GET NEXT              
         LA    RF,1(RF)                                                         
         STCM  RF,3,TRNKREF+L'TRNKREF-1                                         
         GOTO1 HIGH                                                             
         B     JWPA010                                                          
*                                                                               
JWPA150  GOTO1 SEQ                 READ SEQ NOT BROKEN                          
         B     JWPA010                                                          
*                                                                               
JWPAX    XIT1                                                                   
*                                                                               
*        INITIALIZE PACKED BUCKETS IN BLOCK & BLOCK2                            
*                                                                               
         DROP  R4,R5,R6                                                         
         USING JWTABD,R5                                                        
JWCRCLR  NTR1                                                                   
         LA    R5,BLOCK                                                         
         XC    JWTREC(JWTABLN),JWTREC                                           
         LA    R1,JWTFBUCK                                                      
         LA    R0,JWTNBUCK                                                      
JWC40    ZAP   0(BUCKLN,R1),=P'0'                                               
         LA    R1,BUCKLN(R1)                                                    
         BCT   R0,JWC40                                                         
*                                                                               
         LA    R5,BLOCK2                                                        
         XC    JWCREC(JWCABLN),JWCREC                                           
         LA    R1,JWCNBKT                                                       
         LA    R0,JWCNADDS                                                      
JWC60    ZAP   0(BUCKLN,R1),=P'0'                                               
         LA    R1,BUCKLN(R1)                                                    
         BCT   R0,JWC60                                                         
         XIT1                                                                   
         DROP  R5                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO GET EURO-ZONE ACTUALS                                          
*                                                                               
*&&UK                                                                           
EURACT   NTR1  ,                                                                
         L     R6,ATRNEL                                                        
         USING TRNELD,R6                                                        
         LA    R5,BLOCK                                                         
         USING JWTABD,R5                                                        
         ZAP   DUB,TRNAMNT         GET PRIMARY CURRENCY AMOUNT                  
*                                                                               
         MVC   WORK(L'COMPCUR),COMPCUR                                          
         MVC   WORK+L'COMPCUR(L'COMPCURS),COMPCURS                              
         GOTO1 TOBACCO,DMCB,('TOBAAOUT',WORK),AIO,ACOMFACS,0,0,0                
         GOTO1 GETELEM,DMCB,TRNELQ,0                                            
         ZAP   TAMT,TRNAMNT        GET SECONDARY CURRENCY AMOUNT                
*                                                                               
* SET PRIMARY AND SECONDARY AMOUNTS BASED ON WHICH CURRENCY REQUESTED           
*                                                                               
EURACT10 ZAP   PRIMAMT,DUB                                                      
         ZAP   SECAMT,TAMT                                                      
         CLI   SECCURR,C'N'        TEST PRIMARY CURRENCY REQUESTED              
         BE    EURACT20            YES                                          
         ZAP   PRIMAMT,TAMT        NO-SECONDARY CURRENCY WANTED                 
         ZAP   SECAMT,DUB                                                       
*                                                                               
EURACT20 LA    R4,JWTBUCKS         PRIMARY CURRENCY BUCKETS                     
         ZAP   DUB,PRIMAMT                                                      
         BAS   RE,BUCKET                                                        
*                                                                               
         LA    R4,JWTSBUCK         SECONDARY CURRENCY BUCKETS                   
         ZAP   DUB,SECAMT                                                       
         BAS   RE,BUCKET                                                        
*                                                                               
EURACTX  XIT1                                                                   
         DROP  R5,R6                                                            
*&&                                                                             
         SPACE 2                                                                
* SUB-ROUTINE TO COMPUTE BUCKETS FOR POSTING                                    
* AT ENTRY, DUB CONTAINS TRANSACTION AMOUNT AND R4=A(BUCKETS)                   
*                                                                               
BUCKET   NTR1  ,                                                                
         USING JWTABD,R5                                                        
         USING BUCKD,R4                                                         
         L     R6,ATRNEL                                                        
         USING TRNELD,R6                                                        
         TM    TRNSTAT,TRNSNOCM    TEST NON-COMMISSIONABLE                      
         BNO   BUCKET2                                                          
*                                                                               
         ZAP   BUCKNCOM,DUB                                                     
         B     BUCKET5                                                          
*                                                                               
BUCKET2  ZAP   BUCKNET,DUB         COMPUTE COMMISSION                           
         ZAP   PL16,DUB                                                         
         MP    PL16,COMRATE                                                     
         SRP   PL16,64-6,5                                                      
         ZAP   BUCKCOM,PL16                                                     
         ZAP   DUB,BUCKNET                                                      
*                                                                               
BUCKET5  TM    RUNOPT,NEEDTAX                                                   
         BNO   BUCKETX                                                          
*                                                                               
*        ZAP   DUB,BUCKNET         APPLY TAX RATE TO NET+COMMISSION             
         AP    DUB,BUCKCOM                                                      
         ZAP   PL16,JWTTRATE                                                    
         MP    PL16,DUB                                                         
         SRP   PL16,64-4,5                                                      
         ZAP   BUCKTAX,PL16                                                     
*                                                                               
BUCKETX  XIT1                                                                   
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
*        CONVERT TSAR RECORDS TO MAD OBJECTS                                    
*        BUILD OBJECTS IN TIA+GETOPPT OPTIMIZATION TABLE                        
*        AT ENTRY, R4=A(OBJECT AREA)                                            
*        ROUTINE BUILDS OBJECTS AND PUT THEM TO TEMPFILE IMMEDIATELY            
*----------------------------------------------------------------------         
*                                                                               
         USING JWTABD,R5                                                        
BLDJWOBJ NMOD1 JWTABLN,**BDJW**    BUILD OBJECTS FROM TSAR RECORDS              
         LR    R5,RC                                                            
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
*                                                                               
         XC    JWTREC(JWTKEYLN),JWTREC                                          
*                                                                               
         USING TSARD,R1                                                         
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSARDH                                                    
         ST    R5,TSAREC                                                        
         GOTO1 VTSAR                                                            
         TM    TSERRS,TSEEOF       ANY TSAR RECORDS                             
         BO    BJW0X               NO, NO OBJECTS                               
*                                                                               
         USING PRJWOBJD,R4                                                      
         LR    R6,R4               SAVE A(OBJECT BUFFER) IN R6                  
*                                                                               
BJW010   MVC   SVTSRNUM,TSRNUM   SAVE RECORD NUMBER IN CASE RATE                
*                                  NEEDED                                       
*                                                                               
         CLI   JWTREC,0            IF FIRST BYTE IS ZERO, USE JWCABD            
         BE    BJW100                                                           
*                                                                               
         MVC   0(2,R6),=Y(PRJWOBJL+4)                                           
         MVC   2(4,R6),=A(ITPRRTJW)                                             
         LA    R4,6(R6)                                                         
         MVC   0(PRJWOBJL,R4),=CL255' '                                         
*                                                                               
         TM    JWTSTAT,JWTSRATE    IS RATE SET                                  
         BO    BJW020              YES                                          
         MVC   WORKCODE,JWTWC                                                   
         GOTO1 =A(GETWCRAT),DMCB,(RC),RR=APRELO                                 
         ZAP   JWTCRATE,COMRATE                                                 
         OI    JWTSTAT,JWTSRATE    FLAG RATE IS SET                             
*                                                                               
BJW020   MVC   PRJWWC,JWTWC        WORKCODE                                     
         GOTO1 HEXOUT,DMCB,JWTSUFF,PRJWSUFF,L'JWTSUFF                           
*                                                                               
         LA    R2,JWTCRATE         FIRST PACKED BUCKET                          
         LA    R3,PRJWCRAT         FIRST OBJECT BUCKET                          
         LA    R0,JWTNBUCK                                                      
*                                                                               
BJW030   ZAP   DUB,0(BUCKLN,R2)                                                 
*                                  PROTECT AGAINST BAD DATA(TEST FILES)         
         CP    DUB,MAXBUCK         TEST FOR FULLWORD OVERFLOW                   
         BNH   *+10                                                             
         ZAP   DUB,MAXBUCK         YES-SET VALUE TO MAXIMUM                     
         CP    DUB,MINBUCK                                                      
         BNL   *+10                                                             
         ZAP   DUB,MINBUCK                                                      
*                                                                               
         CVB   R1,DUB                                                           
         ST    R1,FULL                                                          
         GOTO1 HEXOUT,DMCB,FULL,WORK,4,FULL                                     
         MVC   0(8,R3),WORK                                                     
*                                                                               
         LA    R2,BUCKLN(R2)       NEXT PACKED BUCKED                           
         LA    R3,9(R3)            NEXT OBJECT BUCKET                           
         BCT   R0,BJW030                                                        
*                                                                               
         CLI   WRKISCRE,C'N'       IF WORKER FILE NOT YET CREATED               
         BNE   BJW040                                                           
         GOTO1 WRKCRE,DMCB,0                                                    
         MVI   WRKISCRE,C'Y'       CREATE IT                                    
         GOTO1 WRKPUT,DMCB,DTREC   PUT DATE/TIME OBJECT TO IT                   
*                                                                               
BJW040   LA    RF,WRKREC           PUT OBJECT TO TEMP FILE                      
         XC    0(4,RF),0(RF)                                                    
         SR    RE,RE                                                            
         ICM   RE,3,0(R6)                                                       
         LA    RE,4(RE)                                                         
         STH   RE,0(RF)                                                         
         SHI   RE,5                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   4(0,RF),2(R6)                                                    
         GOTO1 WRKPUT,DMCB,WRKREC                                               
*                                                                               
         MVI   EMPFLAG,C'N'        TEMP FILE IS NO LONGER EMPTY                 
*                                                                               
         USING TSARD,R1                                                         
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSANXT       GET THE NEXT RECORD NUMBER                   
         MVC   TSRNUM,SVTSRNUM                                                  
         ST    R5,TSAREC                                                        
         GOTO1 VTSAR                                                            
         TM    TSERRS,TSEEOF       ANY TSAR RECORDS                             
         BNO   BJW010              NO, BO OBJECTS                               
         B     BJW0X                                                            
*                                                                               
         USING PRCOOBJD,R4                                                      
         USING JWCABD,R5                                                        
BJW100   MVC   0(2,R6),=Y(PRCOOBJL+4)                                           
         MVC   2(4,R6),=A(ITPRRTCO)                                             
         LA    R4,6(R6)                                                         
         MVC   0(PRCOOBJL,R4),=CL255' '                                         
*                                                                               
         MVC   PRCOWC,JWCWC        WORKCODE                                     
         GOTO1 HEXOUT,DMCB,JWCSUFF,PRCOSUFF,L'JWCSUFF                           
         MVC   PRCOCONT,JWCCONTR                                                
*                                                                               
         LA    R2,JWCNBKT          FIRST PACKED BUCKET                          
         LA    R3,PRCONET          FIRST OBJECT BUCKET                          
         LA    R0,JWCNADDS                                                      
         B     BJW030                                                           
*                                                                               
BJW0X    XIT1                                                                   
         DROP  R1,R4,R5                                                         
         SPACE 2                                                                
MAXBUCK  DC    P'2147000000'                                                    
MINBUCK  DC    P'-2147000000'                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*        BUILD A 15 BYTE JOB KEY FROM THE CL6 FIELDS IN ADATA (REQUEST)         
*----------------------------------------------------------------------         
*                                                                               
BLDJOBKY NMOD1 0,**BDJK**                                                       
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         L     R2,ADATA                                                         
         USING PRRQOBJD,R2                                                      
         MVC   BIGKEY,=CL60' '                                                  
         LA    R3,BIGKEY                                                        
         MVC   0(3,R3),CUL                                                      
         LA    R3,3(R3)            BUMP PAST CUL IN JOB KEY                     
         SR    R1,R1                                                            
         IC    R1,LCLI                                                          
         LA    R4,PRRQCLI                                                       
         BAS   RE,MOVELEV                                                       
*                                                                               
         IC    R1,LPRO                                                          
         LA    R4,PRRQPRD                                                       
         BAS   RE,MOVELEV                                                       
*                                                                               
         IC    R1,LJOB                                                          
         LA    R4,PRRQJOB                                                       
         BAS   RE,MOVELEV                                                       
         XIT1                                                                   
         DROP  R2                                                               
*                                                                               
*                                                                               
*        MOVE THE ACCOUNT DATA FROM 0(R4) TO 0(R3) FOR A LENGTH OF R1           
*        BUMP R3 TO NEXT ACCOUNT FIELD                                          
*                                                                               
MOVELEV  EQU   *                                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)                                                    
         LA    R3,1(R1,R3)                                                      
         BR    RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*        CALL GETOPT, P1 IS A(18 BYTE CL6 CLIENT, CL6 PRODUCT AND A             
*              CL6 JOB)                                                         
*             P2 IS A(SPACE TO BUILD GOBLOCK)                                   
*----------------------------------------------------------------------         
RDOPT    NMOD1 0,**RDOP**                                                       
         L     RC,0(R1)                                                         
         L     R5,4(R1)                                                         
         USING PRRQOBJD,R2                                                      
         L     R2,ADATA                                                         
         USING GOBLOCKD,R5         BUILD GETOPT KEY                             
         MVC   GOADM,DATAMGR                                                    
         MVC   GOSELCUL,CUL                                                     
         MVC   GOSELCLI,PRRQCLI                                                 
         MVC   GOSELPRO,PRRQPRD                                                 
         MVC   GOSELJOB,PRRQJOB                                                 
*                                                                               
         L     RE,ATIA             CLEAR GETOPT OPT TABLE                       
         L     RF,=A(LENGOOPT)                                                  
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RF,ATIA             USE 2K OF TIA FOR OPT TABLE                  
         ST    RF,GOABUFF                                                       
         MVC   GOLBUFF,=A(LENGOOPT)                                             
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         XIT1                                                                   
         DROP  R2,R5                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*        READ ESTIMATE RECORDS FOR JOB WORKCODE BUILD                           
*        PUT WORKCODE VALUES TO TSAR                                            
*---------------------------------------------------------------------          
*                                                                               
JWPUTEST NMOD1 0,**PEST**                                                       
         L     RC,0(R1)                                                         
         USING EVERECD,R6                                                       
         LA    R6,BIGKEY           BUILD ESTIMATE KEY                           
         XC    EVEKEY,EVEKEY                                                    
         MVI   EVEKTYP,EVEKTYPQ                                                 
         MVI   EVEKSUB,EVEKSUBQ                                                 
         MVC   EVEKCPY(3),CUL                                                   
         L     R2,ADATA                                                         
         USING PRRQOBJD,R2                                                      
         MVC   EVEKCLI,PRRQCLI                                                  
         MVC   EVEKPRO,PRRQPRD                                                  
         MVC   EVEKJOB,PRRQJOB                                                  
         MVI   EVEKTYPE,EVEKTPLN                                                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
JWPE10   CLC   BIGKEY(EVEKTYPE-EVEKEY),KEYSAVE                                  
         BNE   JWPEX               ALL DONE WITH ESTIMATES                      
*                                                                               
         GOTO1 GETREC              GET THE ESTIMATE RECORD                      
*                                                                               
         USING EVERECD,R6                                                       
         L     R6,AIO                                                           
         LA    R6,EVERFST          EXTRACT ELEMENTS FROM ESTIMATE REC           
*                                                                               
         USING EDAELD,R6                                                        
JWPE30   CLI   0(R6),0             END OF RECORD                                
         BE    JWPE50              GET NEXT ESTIMATE                            
*                                                                               
         CLI   0(R6),EDAELQ        ESTIMATE DATA ELEMENT                        
         BNE   JWPE40              GET NEXT ELEMENT                             
         TM    EDATYPE,EDATWORK    TEST FOR WORKCODE ESTIMATE                   
         BZ    JWPE40              NO                                           
*                                                                               
         USING JWTABD,R5           BUILD BINSRCH RECORD IN BLOCK                
         BAS   RE,JWPECLR          INITIALIZE BLOCK AS TABLE ENTRY              
         LA    R5,BLOCK                                                         
         MVC   JWTWC,EDAWORK                                                    
         TM    EDATYPE,EDATSUB     TEST FOR SUB-WORKCODE ESTIMATE               
         BZ    *+10                                                             
         MVC   JWTSUFF,EDASUBC     EXTRACT SUFFIX                               
         GOTO1 =A(PUTTSAR),DMCB,(RC),BLOCK,RR=APRELO                            
*                                                                               
JWPE40   ZIC   R1,1(R6)            GET NEXT ESTIMATE DATA ELEMENT               
         LA    R6,0(R1,R6)                                                      
         B     JWPE30                                                           
*                                                                               
JWPE50   GOTO1 SEQ                 READ NEXT ESTIMATE RECORD                    
         B     JWPE10                                                           
*                                                                               
JWPEX    XIT1                                                                   
*                                                                               
*        CLEAR PACKED BUCKETS IN TSAR INPUT RECORDS                             
*                                                                               
         USING JWTABD,R5                                                        
JWPECLR  EQU   *                                                                
         LA    R5,BLOCK                                                         
         XC    JWTREC(JWTABLN),JWTREC                                           
         LA    R1,JWTFBUCK                                                      
         LA    R0,JWTNBUCK                                                      
JWPEC40  ZAP   0(BUCKLN,R1),=P'0'                                               
         LA    R1,BUCKLN(R1)                                                    
         BCT   R0,JWPEC40                                                       
         BR    RE                                                               
*                                                                               
         DROP  R2,R5,R6                                                         
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*        READ OPEN ORDER RECORDS FOR JOB - BUILD JOB WORKCODE DATA              
*        PUT WORKCODE VALUES TO TSAR                                            
*---------------------------------------------------------------------          
*                                                                               
JWPUTORD NMOD1 0,**PORD**                                                       
         L     RC,0(R1)                                                         
         GOTO1 =A(BLDJOBKY),RR=APRELO                                           
*                                                                               
         LA    R6,BIGKEY           BUILD TRANSACTION KEY                        
         USING TRNRECD,R6                                                       
         MVC   TRNKWORK,=C'**'     ORDERS ARE UNDER WORKCODE = **               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
JWPO10   LA    R6,BIGKEY                                                        
         CLC   TRNKEY(TRNKCULC-TRNKEY),KEYSAVE                                  
         BNE   JWPOX               ALL DONE WITH ORDERS                         
*                                                                               
         CLC   TRNKDATE,=CL3' '    TEST FOR VALID DATE                          
         BE    JWPO50              NO-GET NEXT RECORD                           
*                                                                               
         GOTO1 GETREC              GET THE ORDER RECORD                         
*                                                                               
*&&UK                                                                           
         TM    COMPSTA7,CPYSSCNV   TEST EURO ZONE COMPANY                       
         BNO   JWPO15                                                           
*                                                                               
         BAS   RE,EURORD                                                        
         GOTO1 =A(PUTTSAR),DMCB,(RC),BLOCK,RR=APRELO                            
         B     JWPO45              GET NEXT RECORD                              
*&&                                                                             
JWPO15   L     R6,AIO                                                           
         LA    R6,TRNRFST          EXTRACT ELEMENTS FROM ORDER REC              
         USING OAMELD,R6                                                        
*                                                                               
JWPO20   CLI   0(R6),0             END OF RECORD                                
         BE    JWPO45              GET NEXT ORDER                               
*                                                                               
         CLI   0(R6),OAMELQ        ORDER AMOUNT ELEMENT                         
         BNE   JWPO40              GET NEXT ELEMENT                             
*                                                                               
         ST    R6,AOAMEL                                                        
         USING JWTABD,R5           BUILD BINSRCH RECORD IN BLOCK                
         BAS   RE,JWPOCLR          INITIALIZE BLOCK AS TABLE ENTRY              
         LA    R5,BLOCK                                                         
         MVC   JWTWC,OAMWORK                                                    
         ZAP   OPENAMT,OAMAMNT     GET ORDER AMOUNT                             
         SP    OPENAMT,OAMIVAL     SUBTRACT INVOICED AMOUNT                     
         BP    *+10                (CAN'T HAVE NEGATIVE RESULT)                 
         ZAP   OPENAMT,=P'0'       TO DERIVE OPEN ORDER AMOUNT                  
*                                                                               
         GOTO1 POSTORD,POSTOPRI                                                 
*                                                                               
JWPO36   GOTO1 =A(PUTTSAR),DMCB,(RC),BLOCK,RR=APRELO                            
*                                                                               
JWPO40   ZIC   R1,1(R6)            GET NEXT ORDER AMOUNT ELEMENT                
         LA    R6,0(R1,R6)                                                      
         B     JWPO20                                                           
*                                                                               
JWPO45   GOTO1 READ                RESTORE KEY SEQUENCE FIRST                   
*                                                                               
JWPO50   GOTO1 SEQ                 READ NEXT PSEUDO ORDER RECORD                
         B     JWPO10                                                           
*                                                                               
JWPOX    XIT1                                                                   
*                                                                               
*        CLEAR PACKED BUCKETS IN TSAR INPUT RECORDS                             
*                                                                               
         USING JWTABD,R5                                                        
JWPOCLR  EQU   *                                                                
         LA    R5,BLOCK                                                         
         XC    JWTREC(JWTABLN),JWTREC                                           
         LA    R1,JWTFBUCK                                                      
         LA    R0,JWTNBUCK                                                      
JWPOC40  ZAP   0(BUCKLN,R1),=P'0'                                               
         LA    R1,BUCKLN(R1)                                                    
         BCT   R0,JWPOC40                                                       
         BR    RE                                                               
*                                                                               
         DROP  R5,R6                                                            
         SPACE 2                                                                
* SUB-ROUTINE TO POST OPEN ORDER AMOUNT AND DERIVATIVES TO BUCKETS              
* AT ENTRY, OPENAMT = OPEN ORDER AMOUNT, R1=BUCKET TYPE                         
*                                                                               
POSTORD  NTR1  ,                                                                
         STC   R1,BYTE                                                          
         LA    R5,BLOCK                                                         
         USING JWTABD,R5                                                        
         L     R6,AOAMEL                                                        
         USING OAMELD,R6                                                        
*                                                                               
         LA    R1,JWTONET                                                       
*&&UK                                                                           
         CLI   BYTE,POSTOPRI                                                    
         BE    *+8                                                              
         LA    R1,JWTOSNT                                                       
*&&                                                                             
         ZAP   0(L'JWTONET,R1),OPENAMT                                          
*                                                                               
         TM    OAMSTAT,OAMSNOCM    TEST NON-COMMISSIONABLE WC                   
         BNO   POSTORD4            NO                                           
*                                                                               
         LA    R1,JWTOCOM          SET COMMISSION BUCKET TO ZERO                
*&&UK                                                                           
         CLI   BYTE,POSTOPRI                                                    
         BE    *+8                                                              
         LA    R1,JWTOSCM                                                       
*&&                                                                             
         ZAP   0(L'JWTOCOM,R1),=P'0'                                            
         B     POSTORD6                                                         
*                                                                               
* LOOK UP WC COMMISSION RATE AND CALCULATE COMMISSION                           
*                                                                               
POSTORD4 MVC   WORKCODE,OAMWORK                                                 
         GOTO1 =A(GETWCRAT),DMCB,(RC),RR=APRELO                                 
         TM    JWTSTAT,JWTSRATE    TEST RATE ALREADY LOOKED UP                  
         BO    *+14                YES                                          
         OI    JWTSTAT,JWTSRATE    NO-SET INDICATOR                             
         ZAP   JWTCRATE,COMRATE    SAVE COMMISSION RATE                         
*                                                                               
         ZAP   PL16,OPENAMT                                                     
         MP    PL16,COMRATE        OPEN ORDER AMT X COMMISSION RATE             
         SRP   PL16,64-6,5                                                      
         LA    R1,JWTOCOM                                                       
*&&UK                                                                           
         CLI   BYTE,POSTOPRI                                                    
         BE    *+8                                                              
         LA    R1,JWTOSCM                                                       
*&&                                                                             
         ZAP   0(L'JWTOCOM,R1),PL16                                             
*                                                                               
* LOOK UP VAT RATE AND CALCULATE OPEN ORDER TAX                                 
*                                                                               
POSTORD6 TM    RUNOPT,NEEDTAX      TEST TO COMPUTE VAT TAX                      
         BNO   POSTORDX                                                         
*                                                                               
         GOTO1 =A(GETRATE),DMCB,(RC),RR=APRELO                                  
         ZAP   JWTTRATE,PL16       GET VAT RATE (2 DECIMAL PLACES)              
         LA    R1,JWTONET                                                       
*&&UK                                                                           
         CLI   BYTE,POSTOPRI                                                    
         BE    *+8                                                              
         LA    R1,JWTOSNT                                                       
*&&                                                                             
*                                                                               
         ZAP   DUB,0(L'JWTONET,R1)                                              
         AP    DUB,L'JWTONET(L'JWTOCOM,R1)                                      
         ZAP   PL16,JWTTRATE                                                    
         MP    PL16,DUB                                                         
         SRP   PL16,64-4,5                                                      
         ZAP   L'JWTONET+L'JWTOCOM(L'JWTOTAX,R1),PL16                           
*                                                                               
POSTORDX XIT1                                                                   
         DROP  R5,R6                                                            
         SPACE 2                                                                
POSTOPRI EQU   0                                                                
POSTOSEC EQU   1                                                                
         SPACE 2                                                                
*&&UK                                                                           
* SUB-ROUTINE TO PROCESS EURO-ZONE ORDERS                                       
*                                                                               
EURORD   NTR1  ,                                                                
         BAS   RE,JWPOCLR          INITIALIZE BLOCK AS TABLE ENTRY              
         LA    R5,BLOCK                                                         
         USING JWTABD,R5           BUILD BINSRCH RECORD IN BLOCK                
*                                                                               
* FIRST GET AGENCY PRIMARY CURRENCY OPEN ORDER AMOUNT                           
*                                                                               
EURORD05 GOTO1 GETELEM,DMCB,OAMELQ,0                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         USING OAMELD,R6                                                        
         MVC   JWTWC,OAMWORK                                                    
         ZAP   DUB,OAMAMNT         GET ORDER AMOUNT                             
         SP    DUB,OAMIVAL         SUBTRACT INVOICED AMOUNT                     
         BP    *+10                (CAN'T HAVE NEGATIVE RESULT)                 
         ZAP   DUB,=P'0'           TO DERIVE OPEN ORDER AMOUNT                  
*                                                                               
* NOW USE TOBACCO TO GET SECONDARY CURRENCY OPEN ORDER AMOUNT                   
*                                                                               
EURORD10 MVC   WORK(L'COMPCUR),COMPCUR                                          
         MVC   WORK+L'COMPCUR(L'COMPCURS),COMPCURS                              
         GOTO1 TOBACCO,DMCB,('TOBAAOUT',WORK),AIO,ACOMFACS,0,0,0                
*                                                                               
         GOTO1 GETELEM,DMCB,OAMELQ,0                                            
         ST    R6,AOAMEL                                                        
         ZAP   OPENAMT,OAMAMNT     GET ORDER AMOUNT                             
         SP    OPENAMT,OAMIVAL     SUBTRACT INVOICED AMOUNT                     
         BP    *+10                                                             
         ZAP   OPENAMT,=P'0'                                                    
*                                                                               
* SET PRIMARY (PRIMAMT) AND SECONDARY (SECAMT) AMOUNTS BASED ON                 
* REQUESTED CURRENCY                                                            
*                                                                               
EURORD20 ZAP   PRIMAMT,DUB                                                      
         ZAP   SECAMT,OPENAMT                                                   
         CLI   SECCURR,C'N'        TEST PRIMARY CURRENCY REQUESTED              
         BE    EURORD30            YES                                          
*                                                                               
         ZAP   PRIMAMT,OPENAMT                                                  
         ZAP   SECAMT,DUB                                                       
*                                                                               
EURORD30 ZAP   OPENAMT,PRIMAMT                                                  
         GOTO1 POSTORD,POSTOPRI                                                 
         ZAP   OPENAMT,SECAMT                                                   
         GOTO1 POSTORD,POSTOSEC                                                 
*                                                                               
EURORDX  XIT1                                                                   
         DROP  R5,R6                                                            
         SPACE 2                                                                
*&&                                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        JOB WORKCODE - SAVE SCHEME WORKCODES TO TSAR                           
*                                                                               
JWPUTSCH NMOD1 JBLOCKL,**SCHE**                                                 
*                                                                               
         LR    R2,RC                                                            
         L     RC,0(R1)                                                         
         LR    RE,R2               CLEAR JOB BLOCK                              
         LA    RF,JBLOCKL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,ATIA             CLEAR TIA                                    
         A     RE,=A(LENGOOPT)     LESS SPACE RESERVED FOR GETOPT               
         L     RF,=A(LENTIA-LENGOOPT)                                           
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    BLOCK,BLOCK                                                      
*&&US*&& GOTO1 =V(ACJOBCOL),DMCB,FLDH,BLOCK,ACOMFACS,RR=APRELO                  
*&&UK*&& GOTO1 =V(ACJOBCOL),DMCB,(X'FF',COLLIST),BLOCK,ACOMFACS,       X        
               RR=RELO                                                          
         USING JBLOCKD,R2                                                       
         L     RF,ATIA             USE FIRST HALF OF TIA FOR COLUMNS            
         A     RF,=A(LENGOOPT)     LESS SPACE RESERVED FOR GETOPT               
         ST    RF,JBACOLTB                                                      
         L     RE,=A(LENJBBUF)                                                  
         SRL   RE,1                DIVIDE LENGTH BY 2                           
         ST    RE,JBLCOLTB                                                      
         ST    RE,JBLOPVTB                                                      
         LA    RF,0(RF,RE)                                                      
         ST    RF,JBAOPVTB         SECOND HALF FOR OPERANDS                     
*                                                                               
*                                  CREATE OLD ACC RECORD FOR JOBBER             
*                                                                               
         MVC   KEY,BIGKEY                                                       
         GOTO1 SETSYS,DMCB,=C'ACCOUNT',(0,0),=CL8'ACCOUNT'                      
         BE    JWPS10                                                           
         DC    H'0'                                                             
*                                                                               
JWPS10   MVI   LENKEY,L'ACCKEY                                                  
         MVI   DISPDSKA,ACCKDA-ACCRECD                                          
         GOTO1 READ                                                             
         GOTO1 SETSYS,DMCB,=C'ACCOUNT',=CL8'ACCDIR',=CL8'ACCMST'                
         MVI   LENKEY,L'ACCKEY                                                  
         MVI   DISPDSKA,ACCKDA-ACCRECD                                          
*                                                                               
         MVC   JBAJOB,AIO          JOB IS IN AIO                                
*                                                                               
         LA    RE,BLOCK                                                         
         ST    RE,JBACOLS                                                       
         MVC   JBACOM,ACOMFACS                                                  
         MVC   JBAIO,AIO2                                                       
         L     R1,AGOBLOCK                                                      
         ST    R1,JBAGOBLK                                                      
*                                                                               
*                                                                               
         XC    WCTAB,WCTAB                                                      
         LA    R1,WCTAB                                                         
         ST    R1,JBAWCTB          PASS JOBBER A WORKCODE FORMULA TABLE         
         LA    R1,L'WCTAB                                                       
         ST    R1,JBAWCTBL                                                      
*                                                                               
*                                                                               
         USING GOBLOCKD,R5                                                      
         L     R5,AGOBLOCK                                                      
         MVC   JBSELSCH,GOSCHEME                                                
         MVC   JBGETOPT,GETOPT                                                  
*                                                                               
*&&UK                                                                           
         GOTO1 DATCON,DMCB,(5,0),(1,JBTODAYP)                                   
         L     RE,=V(VATICAN)                                                   
         A     RE,APRELO                                                        
         ST    RE,JBVATICN                                                      
*&&                                                                             
         GOTO1 JOBBER,DMCB,JBLOCK                                               
         CLI   JBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,JBACOLTB                                                      
         USING JBCOLD,R4                                                        
         LH    R0,JBNROWS                                                       
*                                                                               
         USING JWTABD,R5           BUILD TSAR RECORD IN BLOCK                   
         LA    R5,BLOCK                                                         
JWPS50   CLI   JBCOLTYP,JBCOLTWC   WORKCODE ROW?                                
         BNE   JWPS60              NO                                           
*                                                                               
         BAS   RE,JWPCRCLR         INITIALIZE RECORD                            
*                                                                               
         MVC   JWTWC,JBCOLWC                                                    
         MVC   JWTSUFF,JBCOLSUF                                                 
         ZAP   JWTCRATE,JBCOLVAL                                                
         OI    JWTSTAT,JWTSRATE    FLAG TABLE ENTRY AS HAVING RATE SET          
*                                                                               
*&&UK                                                                           
         ZAP   JWTTRATE,JBCOLVAL+6 VAT RATE                                     
*&&                                                                             
*                                                                               
         GOTO1 GETPCAT,DMCB,JBCOLD                                              
*                                                                               
         GOTO1 =A(PUTTSAR),DMCB,(RC),BLOCK,RR=APRELO                            
*                                                                               
JWPS60   AH    R4,JBLCOL           GET NEXT SCHEME WORKCODE                     
         BCT   R0,JWPS50                                                        
*                                                                               
JWPSX    XIT1                                                                   
*                                                                               
*        CLEAR THE TSAR INPUT RECORD                                            
*                                                                               
         USING JWTABD,R5                                                        
JWPCRCLR NTR1                                                                   
         LA    R5,BLOCK                                                         
         XC    JWTREC(JWTABLN),JWTREC                                           
         LA    R1,JWTFBUCK                                                      
         LA    R0,JWTNBUCK                                                      
JWP40    ZAP   0(BUCKLN,R1),=P'0'                                               
         LA    R1,BUCKLN(R1)                                                    
         BCT   R0,JWP40                                                         
         B     JWPSX                                                            
*                                                                               
*                                                                               
* SUB-ROUTINE TO GET THE PERCENT OF CATEGORY PERCENTAGE FOR A WORKCODE          
* AT ENTRY, R2=A(JOBBER BLOCK), R5=A(JOB WORKCODE BUCKETS)                      
*                                                                               
GETPCAT  NTR1  ,                                                                
         USING JBCOLD,R4                                                        
         USING JWTABD,R5                                                        
         L     R4,0(R1)            GET A(JOBBER COL)                            
         L     RE,JBAWCTB          RE=A(JOBBER WORKCODE TABLE)                  
         USING JBWCTABD,RE                                                      
*                                                                               
GETPCAT2 CLI   JBWCKEY,0           TEST FOR EOT                                 
         BE    GETPCATX            YES                                          
*                                                                               
         CLC   JBCOLCOD,JBWCKEY    MATCH ON WORKCODE/SUFFIX                     
         BE    GETPCAT4            YES                                          
         LA    RE,JBWCTABL(RE)                                                  
         B     GETPCAT2                                                         
*                                                                               
GETPCAT4 L     R3,AGOBLOCK                                                      
         USING GOBLOCKD,R3                                                      
         MVC   GOSELWC,JBWCODE                                                  
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         XC    GOSELWC,GOSELWC                                                  
         ZAP   JWTPCAT,GOESTPER                                                 
*                                                                               
GETPCATX B     JWPSX                                                            
         DROP  R3,RE                                                            
*                                                                               
*                                                                               
*&&US                                                                           
FLDH     DC    AL1(8+L'FLD),4X'00',AL1(L'FLD),AL2(0)                            
FLD      DC    C'RATE'                                                          
*&&                                                                             
*&&UK                                                                           
COLLIST  DC    AL2(AC#CRATE,AC#VATRJ,0)                                         
*&&                                                                             
*                                                                               
         DROP  R2,R4,R5                                                         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        RETURN, IN COMRATE, THE COMMISSION RATE OF WORKCODE                    
*                                                                               
         USING JWTABD,R5                                                        
GETWCRAT NMOD1 JWTABLN,**CRTE**,CLEAR=YES                                       
         LR    R5,RC                                                            
         L     RC,0(R1)                                                         
         MVC   JWTWC,WORKCODE                                                   
         USING TSARD,R1                                                         
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSARDH                                                    
         ST    R5,TSAREC                                                        
         GOTO1 VTSAR                                                            
         TM    TSERRS,TSERNF       NOT FOUND                                    
         BO    GWR50               YES, GETOPT THE RATE.                        
*                                                                               
         TM    JWTSTAT,JWTSRATE    MAKE SURE RATE HAS BEEN SET IN TABLE         
         BNO   GWR50                                                            
*                                                                               
         ZAP   COMRATE,JWTCRATE    RETURN RATE IN COMRATE                       
         B     GWRX                                                             
*                                                                               
GWR50    L     R4,AGOBLOCK         GET RATE FROM GETOPT                         
         USING GOBLOCKD,R4         WORKCODE NOT FOUND IN TSAR                   
         MVC   GOSELWC,WORKCODE                                                 
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         XC    GOSELWC,GOSELWC                                                  
         ZAP   COMRATE,GOAGYCOM                                                 
*                                                                               
GWRX     XIT1                                                                   
         LTORG                                                                  
         DROP  R1,R4,R5                                                         
         EJECT                                                                  
*----------------------------------------------------------------------         
*        ADD AN ITEM TO TSAR                                                    
*        P2 IS ITEM,                                                            
*----------------------------------------------------------------------         
         USING JWTABD,R5                                                        
PUTTSAR  NMOD1  JWTABLN,**PTSA**                                                
         LR    R4,RC               SPACE TO READ HI INTO                        
         L     RC,0(R1)                                                         
         L     R5,4(R1)            A(RECORD TO PUT TO TSAR)                     
         USING TSARD,R1                                                         
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSARDH                                                    
         MVC   0(JWTKEYLN,R4),0(R5)  SET KEY FOR READ HIGH                      
         ST    R4,TSAREC                                                        
         GOTO1 VTSAR                                                            
         TM    TSERRS,TSERNF       WAS RECORD FOUND?                            
         BO    PTT70               NO                                           
*                                                                               
         LA    R1,JWTNADDS         ADD ITEM BUCKETS TO TSAR BUCKETS             
         LA    R3,JWTBUCKS-JWTABD(R5)  A(I/P RECORD BUCKETS)                    
         LA    R2,JWTBUCKS-JWTABD(R4)  A(TSAR BUCKETS)                          
*                                                                               
PTT50    AP    0(BUCKLN,R2),0(BUCKLN,R3)                                        
         LA    R2,BUCKLN(R2)                                                    
         LA    R3,BUCKLN(R3)                                                    
         BCT   R1,PTT50                                                         
*                                                                               
         USING TSARD,R1                                                         
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSAWRT       WRITE RECORD BACK TO TSAR                    
         B     PTT90                                                            
*                                                                               
PTT70    LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSAADD       ADD THE INPUT RECORD TO TSAR                 
         ST    R5,TSAREC                                                        
*                                                                               
PTT90    GOTO1 VTSAR                                                            
         CLI   TSERRS,0            SET CONDITION CODE                           
         BE    *+6                                                              
         DC    H'0'                                                             
PTTX     XIT1                                                                   
         DROP  R1,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
*----------------------------------------------------------------------         
*        ADD AN ITEM TO TSAR BUT USE DIFFERENT DSECT                            
*        P2 IS ITEM,                                                            
*----------------------------------------------------------------------         
         USING JWCABD,R5                                                        
PUTTSA2  NMOD1 JWCABLN,**PTS2**                                                 
         LR    R4,RC               SPACE TO READ HI INTO                        
         L     RC,0(R1)                                                         
         L     R5,4(R1)            A(RECORD TO PUT TO TSAR)                     
         USING TSARD,R1                                                         
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSARDH                                                    
         MVC   0(JWCKEYLN,R4),0(R5)  SET KEY FOR READ HIGH                      
         ST    R4,TSAREC                                                        
         GOTO1 VTSAR                                                            
         TM    TSERRS,TSERNF       WAS RECORD FOUND?                            
         BO    PUTS4               NO                                           
*                                                                               
         LA    R1,JWCNADDS         ADD ITEM BUCKETS TO TSAR BUCKETS             
         LA    R3,JWCNBKT-JWCABD(R5)   A(I/P RECORD BUCKETS)                    
         LA    R2,JWCNBKT-JWCABD(R4)   A(TSAR BUCKETS)                          
*                                                                               
PUTS2    AP    0(BUCKLN,R2),0(BUCKLN,R3)                                        
         LA    R2,BUCKLN(R2)                                                    
         LA    R3,BUCKLN(R3)                                                    
         BCT   R1,PUTS2                                                         
*                                                                               
         USING TSARD,R1                                                         
         LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSAWRT       WRITE RECORD BACK TO TSAR                    
         B     PUTS6                                                            
*                                                                               
PUTS4    LA    R1,TSARBLK                                                       
         MVI   TSACTN,TSAADD       ADD THE INPUT RECORD TO TSAR                 
         ST    R5,TSAREC                                                        
*                                                                               
PUTS6    GOTO1 VTSAR                                                            
         CLI   TSERRS,0            SET CONDITION CODE                           
         BE    *+6                                                              
         DC    H'0'                                                             
PUTSX    XIT1                                                                   
         DROP  R1,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        ESTABLISH VAT RATE OR GET FROM TABLE                                   
*        RETURN RATE IN PL16                                                    
*                                                                               
         USING VTCD,R2                                                          
         USING TAXTABD,R4                                                       
GETRATE  NMOD1 0,**RATE**                                                       
         L     RC,0(R1)                                                         
         ZAP   PL16,=P'0'                                                       
         L     R2,AVATBUFF         LOOK FOR TAXRATE IN TAXTAB                   
         XC    VTCD(VTCLNQ),VTCD                                                
         L     R4,ATAXTAB                                                       
         LA    R5,TAXMAX                                                        
         USING GOBLOCKD,R3                                                      
         L     R3,AGOBLOCK                                                      
         OC    GOTAXCOD,GOTAXCOD                                                
         BZ    GETRX               NO TAXCODE, NO RATE                          
*                                                                               
GETR02   CLI   TTTAXCOD,0          NOTHING IN TABLE OR END                      
         BE    GETR04              CALL VATICAN FOR THE RATE                    
*                                                                               
         CLC   TTTAXCOD,GOTAXCOD   DOES CODE MATCH ?                            
         BNE   GETR03              NO, TRY NEXT                                 
         ZAP   PL16,TTTAXRTE                                                    
         B     GETRX                                                            
*                                                                               
GETR03   LA    R4,TAXTABLN(R4)                                                  
         BCT   R5,GETR02                                                        
         DC    H'0'                TOO MANY RATES                               
*                                                                               
GETR04   MVC   VTCTYPE,GOTAXCOD    PASS VAT TYPE CODE                           
         MVI   VTCACTN,VTCALOOK                                                 
         MVC   VTCCPY,CUL                                                       
         MVC   VTCCOMF,ACOMFACS                                                 
         MVC   VTCOFFC,GOEFFOFC                                                 
         GOTO1 DATCON,DMCB,(5,0),(1,VTCINVD) AS OF TODAY                        
         GOTO1 =V(VATICAN),(R2),RR=APRELO                                       
*                                                                               
         TM    VTCINDS,VTCINA      IS TAX APPLICABLE ?                          
         BO    GETRX               NO                                           
         CLI   VTCERR,X'00'        YES, FIND TYPE ?                             
         BNE   GETRX               NO                                           
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,VTCRATE                                                     
         CVD   RE,DUB                                                           
         ZAP   TTTAXRTE,DUB        UPDATE TAXTAB                                
         MVC   TTTAXCOD,GOTAXCOD                                                
*                                                                               
         ZAP   PL16,DUB            RETURN RATE                                  
*                                                                               
GETRX    XIT1                                                                   
         LTORG                                                                  
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*        CONVERT THE MAINFRAME DATE/TIMES IN MONTIME/DATE TO CL6                
*        PC STYLE VALUES ADDRESSED BY P1 AND P2                                 
*                                                                               
         USING DTTMD,R6                                                         
MAIN2PC  NMOD1 DTTMLN,**M2PC**                                                  
         LR    R6,RC                                                            
         L     RC,0(R1)                                                         
         L     R2,4(R1)            A(WHERE TO WRITE THE DATE)                   
         L     R3,8(R1)            A(WHERE TO WRITE THE TIME)                   
         GOTO1 DATCON,DMCB,(2,MONDATE),(X'20',DTTMDATE)                         
*                                                                               
         XR    R5,R5                                                            
         ICM   R5,7,MONTIME                                                     
*&&US*&& AH    R5,=Y(EIGHTHRS)     ADD 8 HOURS IN BINARY SCEONDS                
*&&UK*&& AH    R5,=H'0'            NO ADJUSTMENT NEEDED IN UK                   
         C     R5,=A(TWO4HRS)      MORE THAN 24                                 
         BNH   M2P50                                                            
         S     R5,=A(TWO4HRS)                                                   
*                                                                               
         XR    RF,RF               SUBTRACT 1 FROM DATE                         
         BCTR  RF,0                                                             
         ST    RF,DMCB+8                                                        
         L     RF,ACOMFACS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,DTTMDATE,(X'20',WORK)                                  
         MVC   DTTMDATE,WORK                                                    
*                                                                               
M2P50    XR    R4,R4               R4 IS E OF E/O PAIR                          
         D     R4,=F'3600'         SECONDS/HOUR                                 
         CVD   R5,DUB                                                           
         EDIT  (P8,DUB),(2,DTTMHRS),FILL=0                                      
         LR    R5,R4               DIVIDE REMAINDER BY MINUTES                  
         XR    R4,R4                                                            
         D     R4,=F'60'           SECONDS/MIN                                  
         CVD   R5,DUB                                                           
         EDIT  (P8,DUB),(2,DTTMMIN),FILL=0                                      
         CVD   R4,DUB                      REMAINDER IS SECONDS                 
         EDIT  (P8,DUB),(2,DTTMSEC),FILL=0                                      
         MVC   0(L'DTTMDATE,R2),DTTMDATE   RETURN DATE/TIME                     
         MVC   0(L'DTTMTIME,R3),DTTMTIME                                        
         XIT1                                                                   
         DROP  R6                                                               
*                                                                               
SETTSAR  NMOD1 0,**ITSR**                                                       
         L     RC,0(R1)                                                         
         GOTO1 CALLOV,DMCB,0,X'D9000A5D'                                        
         CLI   4(R1),X'FF'         CAN'T GET TSAR                               
         BNE   *+6                 GOT OT                                       
         DC    H'0'                DIE IF TSAR UNAVAILABLE                      
         MVC   VTSAR,0(R1)                                                      
*                                                                               
         USING TSARD,R1                                                         
         LA    R1,TSARBLK                                                       
         XC    0(TSARDL,R1),0(R1)                                               
         MVI   TSACTN,TSAINI                                                    
         OI    TSINDS,TSINODSK                                                  
         MVI   TSKEYL,JWTKEYLN                                                  
         MVC   TSRECL,=Y(JWTABLN)                                               
         MVI   TSPAGN,TSNMAX       REQUEST MAX N' DISK PAGES                    
         MVC   TSACOM,ACOMFACS                                                  
         GOTO1 VTSAR                                                            
         XMOD1                                                                  
*                                                                               
         EJECT                                                                  
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
APRELO   DS    A                   RELOCATION CONSTANT                          
TOBACCO  DS    V                                                                
ELCODE   DS    X                   USED FOR GETEL CALLS                         
WRKISCRE DS    C                   WRKCRE CALLED THIS TRANS (Y/N)               
*                                                                               
CTRY     DS    X                   COUNTRY CODE FROM UTL                        
*                                                                               
COMPCUR  DS    CL3                 PRIMARY CURRENCY                             
COMPCURS DS    CL3                 SECONDARY CURRENCY                           
JOBCUR   DS    CL3                 JOB BILLING CURRENCY                         
SECCURR  DS    CL1                 SECONDARY CURRENCY FOR JOB (Y/N)             
COMPSTA7 DS    XL1                 COMPANY STATUS BYTE 7                        
COMPSTA9 DS    XL1                 COMPANY STATUS BYTE 9                        
*                                                                               
MONRTYP  DS    X                   MONITOR REQUESTED RECORD TYPE                
MONFTYP  DS    X                   MONITOR REQUESTED FILTER TYPE                
MONDATE  DS    XL2                 MONITOR REQUESTED FILTER BY DATE             
MONTIME  DS    XL3                 MONITOR REQUESTED FILTER BY TIME             
MATCHFND DS    C                   RECFILT TABLE MATCH FOUND (Y/N)              
EMPFLAG  DS    C                   REQUESTED SET OF DATA IS EMPTY (Y/N)         
*                                                                               
PSNTBLK  DS    XL(PSNTBLKL)        ACPRESENT PARAMETER BLOCK                    
*                                                                               
SPCLI    DS    CL6                 REQUEST JOB                                  
SPPRO    DS    CL6                                                              
SPJOB    DS    CL6                                                              
*                                                                               
AGOBLOCK DS    A                                                                
AWCTAB   DS    A                                                                
ATAXTAB  DS    A                                                                
AVATBUFF DS    A                   VATICAN INTERFACE BLOCK                      
ATRNEL   DS    A                   A(TRANSACTION ELEMENT)                       
AOAMEL   DS    A                   A(ORDER AMOUNT ELEMENT)                      
*                                                                               
VTSAR    DS    V                   A(TSAR MODULE)                               
*                                                                               
PL16     DS    PL16                                                             
RUNOPT   DS    CL1                                                              
NEEDTAX  EQU   1                                                                
XJOB     DS    CL1                                                              
COMRATE  DS    PL6                                                              
WORKCODE DS    CL2                                                              
TAMT     DS    PL8                                                              
PRIMAMT  DS    PL8                                                              
SECAMT   DS    PL8                                                              
OPENAMT  DS    PL8                                                              
*                                                                               
*                                                                               
WCTAB    DS    XL(10*JBWCTABL)       JOBBER WORKCODE TABLE                      
*                                                                               
*                                                                               
BLOCK2   DS    CL(L'BLOCK)        BUILD SECOND TSAR RECORD HERE                 
*                                                                               
SVTSRNUM DS    CL2                                                              
MYKEYSV  DS    CL(L'BIGKEY)                                                     
*                                                                               
         DS    0D                                                               
GOBLK    DS    XL(L'GOBLOCK)                                                    
GOBBLK   DS    XL(GOBBLKX-GOBBLOCK)                                             
*                                                                               
         DS    0D                                                               
TSARBLK  DS    CL(TSARDL)                                                       
*                                                                               
DTREC    DS    XL30                                                             
WRKREC   DS    XL1000                                                           
OBJECTS  DS    XL8000              AREA TO BUILD MAD OBJECTS                    
         EJECT                                                                  
*                                                                               
* RECORD/FILTER TABLE DSECT                                                     
*                                                                               
RECTABD  DSECT                                                                  
RECRTYP  DS    XL1                                                              
RECFTYP  DS    XL1                                                              
RECSTAT  DS    XL1                                                              
RECSNOBL EQU   X'80'               DO NOT BUILD OBJECTS                         
RECSUTIA EQU   X'40'               USE TIA AREA FOR OBJECT BUILDING             
         DS    XL1                 SPARE                                        
RECAFST  DS    A                   A(FIRST KEY ROUTINE)                         
RECANXT  DS    A                   A(NEXT KEY ROUTINE)                          
RECABLD  DS    A                   A(BUILD OBJECT ROUTINE)                      
RECTABL  EQU   *-RECTABD                                                        
         EJECT                                                                  
*        WORKING STORAGE FOR SETDTTM AND VALDTTM                                
*                                                                               
DTTMD    DSECT                                                                  
DTTMDATE DS    CL6                                                              
DTTMTIME DS    0CL6                                                             
DTTMHRS  DS    CL2                                                              
DTTMMIN  DS    CL2                                                              
DTTMSEC  DS    CL2                                                              
*                                                                               
DTPDATE  DS    XL2                                                              
DTBSECS  DS    XL3                                                              
DTTMLN   EQU   *-DTTMD                                                          
         EJECT                                                                  
*                                                                               
*        JOB WORKCODE DOWNLOAD BINSRCH TABLE                                    
*                                                                               
JWTABD   DSECT                                                                  
JWTREC   DS    0C                                                               
JWTWC    DS    CL2                                                              
JWTSUFF  DS    XL1                 WORKCODE SUFFIX (SUBWORKCODES)               
         DS    CL15                USE MAX KEY SIZE                             
JWTKEYLN EQU   *-JWTABD                                                         
JWTSTAT  DS    XL1                                                              
JWTSRATE EQU   1                   COMMISSION RATE HAS BEEN SET                 
JWTFBUCK DS    0C                                                               
JWTCRATE DS    PL6                 COMMISSION RATE                              
*                                                                               
JWTTRATE DS    PL6                 TAX RATE                                     
JWTPCAT  DS    PL6                 PERCENT OF CATEGORY                          
*                                                                               
JWTBUCKS DS    0C                                                               
JWTNET   DS    PL6                 NET                                          
JWTNCN   DS    PL6                 NON COMMISSIONABLE NET                       
JWTCOM   DS    PL6                 COMMISSION                                   
JWTTAX   DS    PL6                 TAX                                          
*                                                                               
JWTONET  DS    PL6                 OPEN ORDER NET AMOUNT                        
JWTOCOM  DS    PL6                 OPEN ORDER COMMISSION AMOUNT                 
JWTOTAX  DS    PL6                 OPEN ORDER TAX AMOUNT                        
*                                                                               
JWTSBUCK DS    0C                  SECONDARY CURRENCY BUCKETS                   
JWTSNET  DS    PL6                 SECONDARY NET                                
JWTSNCN  DS    PL6                 SECONDARY NON-COMMISSIONABLE                 
JWTSCOM  DS    PL6                 SECONDARY COMMISSION                         
JWTSTAX  DS    PL6                 SECONDARY TAX                                
*                                                                               
JWTOSNT  DS    PL6                 SECONDARY OPEN ORDER NET                     
JWTOSCM  DS    PL6                 SECONDARY OPEN ORDER COMM                    
JWTOSTX  DS    PL6                 SECONDARY OPEN ORDER TAX                     
*                                                                               
JWTTBUCK DS    0C                  TIME BUCKETS                                 
JWTBHR   DS    PL6                 'B' TIME                                     
JWTNHR   DS    PL6                 'N' TIME                                     
JWTRHR   DS    PL6                 'R' TIME                                     
JWTRMEM  DS    PL6                 'R' HOURS VALUE (MEMO ITEM)                  
JWTRSME  DS    PL6                 'R' HOURS SECOND CURR VALUE                  
*                                                                               
JWTABLN  EQU   *-JWTABD                                                         
JWTNBUCK EQU   (*-JWTFBUCK)/6                                                   
JWTNADDS EQU   (*-JWTBUCKS)/6   NUMBER OF BUCKETS TO ACCUMULATE                 
BUCKLN   EQU   6                                                                
         EJECT                                                                  
*        JOB WORKCODE-CONTRA DOWNLOAD BINSRCH TABLE                             
*                                                                               
JWCABD   DSECT                                                                  
JWCREC   DS    0C                                                               
JWCTYP   DS    CL1                 RECORD TYPE 0                                
JWCWC    DS    CL2                                                              
JWCSUFF  DS    XL1                 WORKCODE SUFFIX (SUBWORKCODES)               
JWCCONTR DS    CL14                CONTRA ACCOUNT                               
JWCKEYLN EQU   *-JWCABD                                                         
*                                                                               
JWCNBKT  DS    0C                                                               
JWCNET   DS    PL6                 NET                                          
JWCNCN   DS    PL6                 NON COMMISSIONABLE NET                       
JWCCOM   DS    PL6                 COMMISSION                                   
JWCTAX   DS    PL6                 TAX                                          
JWCNBKTL EQU   *-JWCNBKT                                                        
*                                                                               
JWCSBKT  DS    0C                  SECONDARY CURRENCY BUCKETS                   
JWCSNET  DS    PL6                 SECONDARY NET                                
JWCSNCN  DS    PL6                 SECONDARY NON-COMMISSIONABLE                 
JWCSCOM  DS    PL6                 SECONDARY COMMISSION                         
JWCSTAX  DS    PL6                 SECONDARY TAX                                
JWCSBKTL EQU   *-JWCSBKT                                                        
*                                                                               
JWCTBKT  DS    0C                  TIME BUCKETS                                 
JWCBHR   DS    PL6                 'B' TIME                                     
JWCNHR   DS    PL6                 'N' TIME                                     
JWCRHR   DS    PL6                 'R' TIME                                     
JWCRMEM  DS    PL6                 'R' HOURS VALUE (MEMO ITEM)                  
JWCRSME  DS    PL6                 'R' HOURS SECOND CURR VALUE                  
JWCTBKTL EQU   *-JWCTBKT                                                        
JWCBLN   EQU   *-JWCNBKT        LENGTH OF ALL BUCKETS                           
JWCNADDS EQU   (*-JWCNBKT)/6    NUMBER OF BUCKETS TO ACCUMULATE                 
JWCTADDS EQU   (*-JWCTBKT)/6    NUMBER OF TIME BUCKETS TO ACCUMULATE            
         DS    CL55                MAKE SAME SIZE AS JWTABD                     
JWCABLN  EQU   *-JWCABD         LENGTH OF RECORD                                
         EJECT                                                                  
BUCKD    DSECT                                                                  
BUCKNET  DS    PL6                 NET                                          
BUCKNCOM DS    PL6                 NON COMMISSIONABLE NET                       
BUCKCOM  DS    PL6                 COMMISSION                                   
BUCKTAX  DS    PL6                 TAX                                          
*                                                                               
TAXTABD  DSECT                     TABLE OF TAX RATES                           
TTTAXCOD DS    CL1                                                              
TTTAXRTE DS    PL6                                                              
TAXTABLN EQU   *-TAXTABD                                                        
TAXTABSZ EQU   TAXTABLN*TAXMAX                                                  
TAXMAX   EQU   7                                                                
         SPACE 2                                                                
         EJECT                                                                  
* CTMADPREST                                                                    
       ++INCLUDE CTMADPREST                                                     
         EJECT                                                                  
* ACPRESTOD                                                                     
       ++INCLUDE ACPRESTOD                                                      
         EJECT                                                                  
* ACPRESTOQ                                                                     
       ++INCLUDE ACPRESTOQ                                                      
         EJECT                                                                  
* ACPRESENTD                                                                    
       ++INCLUDE ACPRESENTD                                                     
         EJECT                                                                  
* ACPRESENTQ                                                                    
       ++INCLUDE ACPRESENTQ                                                     
         EJECT                                                                  
* CTMADWORKD                                                                    
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
* CTMADEQUS                                                                     
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
* CTMADDSECT                                                                    
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
* DDCOMFACSD                                                                    
       ++INCLUDE DDCOMFACS                                                      
* DDTSARD                                                                       
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
* ACGENFILE                                                                     
* FAUTL                                                                         
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE FAUTL                                                          
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* ACJOBBLOCK                                                                    
JBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACJOBBLOCK                                                     
         PRINT ON                                                               
* ACGOBLOCKD ACGOBBLOCK                                                         
GOBLOCKD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
       ++INCLUDE ACGOBBLOCK                                                     
         PRINT ON                                                               
* ACJOBERD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
* ACVATICAND                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACVATICAND                                                     
         PRINT ON                                                               
* DDCTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* ACTOBACCOD (UK ONLY)                                                          
         PRINT OFF                                                              
*&&UK                                                                           
       ++INCLUDE ACTOBACCOD                                                     
*&&                                                                             
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'113CTMAD15   12/27/12'                                      
         END                                                                    
