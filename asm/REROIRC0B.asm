*          DATA SET REROIRC0B  AT LEVEL 003 AS OF 05/01/02                      
*          DATA SET REROIRC0   AT LEVEL 177 AS OF 03/19/98                      
*PHASE REROIR0A,*                                                               
*INCLUDE STXITER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE HEXIN                                                                  
ROIR     TITLE 'REROIRC0 -- REP OFFLINE INFO - RECOVERY TAPE READER'            
*********************************************************************           
* HISTORY OF CHANGES                                                *           
*********************************************************************           
* NOV08/94 (BU ) --- UPGRADE TO MAKE SYSTEM SOFT                    *           
*                                                                   *           
* MAR19/98 (BU ) --- UPDATE FOR 4K RECORDS                          *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*  SOURCE:  REROIRC0                                                            
*  PHASE:   REROIR0                                                             
*                                                                               
*        REP OFFLINE INFORMATION SYSTEM - STAND-ALONE PROGRAM                   
*        RECOVERAY TAPE READER                                                  
*                                                                               
*        READS DAILY RECOVERY TAPE FOR SELECTED RECORDS,                        
*        COMPRESSES UNWANTED DATA FROM RECORDS, AND WRITES TO                   
*        REP SYSTEM WORKER FILE.                                                
*                                                                               
*        WORKER FILE ENTRIES ARE 1 PER DAY.                                     
*                                                                               
*        CONTROL CARDS:                                                         
*                                                                               
*   *    DATE=NORMAL - ASSUMES RECOVERY TAPE DATE TO BE                         
*                      CURRENT SYSTEM DATE.                                     
*        DATE=YYMMDD - OVERRIDE DATE FOR RECOVERY TAPE.                         
*                                                                               
*              *NOTE* RECOVERY TAPE MUST AGREE WITH GIVEN RUN DATE              
*                                                                               
*                                                                               
*   *    RESTART=NO  - NORMAL CASE.  PREVENTS RUN IF WORKER                     
*                      FILES FOUND FOR RUN DATE.                                
*        RESTART=YES - ALLOWS RERUN OF JOB FOR A GIVEN DATE.                    
*                                                                               
*        WRITE=NO    - DON'T WRITE WORKER FILE RECORDS                          
*                                                                               
*  '*' = CARDS EXPECTED FOR NORMAL RUN.                                         
*                                                                               
*        CONTROL CARDS MUST BE SPECIFIED.  ! NO DEFAULTING !                    
*                                                                               
*                                                                               
*        RECORDS ACCEPTED FROM RECOVERY TAPE:                                   
*                                                                               
*        ID    RECORD              PROCESSING                                   
*        --    ------              ----------                                   
*        0B    BUY                 REMOVE COMMENTS                              
*        0C    CONTRACT            REMOVE COMMENTS                              
*        14    AVAIL               REMOVE COMMENTS                              
*        16    PROPOSAL            REMOVE COMMENTS                              
*                                                                               
*        IN ORDER TO DECREASE NUMBER OF RECORDS WRITTEN TO WORKER FILE          
*        ON A DAILY BASIS, ONLY REPS USING THE OFFLINE INFO FILE(S)             
*        WILL BE INCLUDED.  PROFILE 1 ON THE REP RECORD HAS BEEN                
*        DEFINED AS 'OFFLINE REPORTING ENABLED'.  THIS PROFILE MUST             
*        BE 'Y' TO GET OFFLINE INFO RECORDS.                                    
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*  09/18/89  PJS  ORIGINAL DEVELOPMENT                                          
*                                                                               
*                                                                               
*  JOB STEPS                                                                    
*        A. HOUSEKEEPING                                                        
*              READ IN CONTROL CARDS.  STOP IF ANY ARE MISSING.                 
*              OPEN WORKER FILE.                                                
*              CHECK FOR EXISTING WORKER FILES FOR RUN DATE.                    
*              CREATE WORKER FILE ENTRY FOR RUN DAY.                            
*              OPEN RECOVERY TAPE AS INPUT FILE                                 
*              READ REP RECORDS & BUILD TABLE INDICATING ROI STATUS             
*                                                                               
*        B. PROCESS RECOVERY TAPE                                               
*              ON 1ST RECORD, VERIFY TAPE DATE -VS- RUN DATE                    
*              ACCEPT CONTRACT, BUY, AVAIL, AND PROPOSAL RECORDS                
*                (ADDS AND CHANGES ONLY)                                        
*              CHECK REP FOR ROI ENABLED PROFILE (PROFILE 1 = 'Y')              
*              DO SPECIAL PROCESSING FOR THE RECORD                             
*                (STRIP COMMENTS, ETC)                                          
*              WRITE TO WORKER FILE                                             
*                                                                               
*        C. PRINT RUN STATS AND RECORD COUNTS                                   
*                                                                               
*                                                                               
*        RETURN CODES                                                           
*                                                                               
*              ZERO (0) = GOOD RUN....NO ERRORS                                 
*                                                                               
*              NON-ZERO = ERRORS FOUND                                          
*                                                                               
*                                                                               
*  REGISTER USAGE                                                               
*  --------------                                                               
*        R9 = 2ND PROGRAM BASE REGISTER                                         
*        RA = THIRD PROGRAM BASE REG                                            
*        RB = 1ST PROGRAM BASE REGISTER                                         
*                                                                               
         EJECT                                                                  
ROIR0    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**ROIRC0,VREGSAVE                                              
*                                                                               
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         LA    RA,2048(R9)                                                      
         LA    RA,2048(RA)                                                      
*                                                                               
         USING ROIR0+4096,R9,RA                                                 
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         B     MAIN100                                                          
STATMSG  DC    CL20' '             WINDOW FOR DUMPS                             
         SPACE 2                                                                
DUMPADDR DS    0F'0'               LIST OF ADDRESSES TO DUMP                    
         DC    A(WORKD),V(HEXIN)                                                
         DC    X'00017428',X'8001A000'                                          
         DC    2F'0'                                                            
         DC    2F'0'                                                            
         SPACE 2                                                                
VREGSAVE DC    V(REGSAVE)                                                       
         SPACE 2                                                                
MAIN100  EQU   *                                                                
         ST    RB,DUMPADDR                                                      
         GOTO1 =V(STXITER),DMCB,DUMPADDR                                        
*                                                                               
         BAS   RE,INITIAL          OPENING HOUSEKEEPING                         
         BNZ   ERREXIT                                                          
*                                                                               
         BAS   RE,RECOVIN          PROCESS RECOVERY INPUT                       
         BNZ   ERREXIT                                                          
*                                                                               
         BAS   RE,RUNSTAT          PRINT RUN STATS AND COUNTS                   
*                                                                               
         XC    RETCODE,RETCODE     0 = GOOD RETURN                              
         B     EXITBASE                                                         
*                                                                               
ERREXIT  MVC   RETCODE,=F'999'     ERROR RETURN                                 
*                                                                               
EXITBASE XBASE RC=RETCODE                                                       
         SPACE 2                                                                
EXIT     XIT1                      GENERIC SUBROUTINE EXIT                      
         TITLE 'OPENING HOUSEKEEPING'                                           
*                                                                               
*- INITIAL HOUSEKEEPING                                                         
*                                                                               
*        MAJOR STEPS                                                            
*              SET UP HEADINGS FOR REPORTING                                    
*              PROCESS CONTROL CARDS                                            
*              OPEN RECOVERY TAPE AS INPUT FILE                                 
*              OPEN DATAMGR                                                     
*              READ REP RECORDS & BUILD TABLE WITH PROFILE 1                    
*                                                                               
INITIAL  NTR1                                                                   
*                                                                               
         MVC   LINE,=PL2'99'       FORCE HEADLINE                               
         MVC   TITLE(60),=CL60'REP OFFLINE INFORMATION SYSTEM'                  
         MVC   HEADUSER(19),=CL19'DAILY WORKER UPDATE'                          
*                                                                               
*- VALIDATE CONTROL CARDS.                                                      
         BAS   RE,CTLCARDS                                                      
         BNZ   INITERR             PROBLEM WITH CONTROL CARDS. STOP.            
*                                                                               
         GOTO1 VPRINTER                                                         
         MVC   P(50),=CL50'* CONTROL CARDS PASS VALIDATION *'                   
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
*   OPEN CONTROL SYSTEM TO ACCESS CONTROL FILE IDENTIFICATION                   
*                                                                               
         LA    R1,IOAREA                                                        
         ST    R1,ADREC                                                         
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',ADREC,0                                            
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'           FIND CONTROL FILE ID RECORD                  
         MVC   WORK+15(10),SAVNAME LOAD AGENCY NAME                             
         OC    WORK+15(10),SPACES  SET REMAINDER TO SPACES                      
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',WORK,ADREC                
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         L     R1,ADREC                                                         
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                DIFFERS - DUMP IT OUT                        
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
CTRL0010 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   CTRL0020            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    CTRL0030            YES                                          
CTRL0020 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   CTRL0010            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
CTRL0030 EQU   *                                                                
         MVC   UTL+4(1),3(R1)      OVERRIDE CONTROL FILE UTL                    
*                                     WITH REP UTL CODE                         
*                                                                               
*- DETERMINE INDEX FILE NAME.                                                   
*  NAME = 'SJ' (USER -- ALL INFO STORED IN 'SJ' USER)                           
*         'R'  (SYSTEM = REP)                                                   
*         'OI' (PROGRAM = OFFLINE INFO)                                         
*        X'0'  (SUBPROGRAM)                                                     
*          D   PWOS DAY OF RUN (DD VALUE FROM RUN DATE)                         
*         'R'  (CLASS REP)                                                      
*                                                                               
         XC    INDEX,INDEX         READ 1ST INDEX                               
         XC    REC,REC                                                          
         LA    R7,INDEX                                                         
         USING UKRECD,R7                                                        
         MVC   UKUSRID,=C'SJ'                                                   
         MVC   UKSYSPRG,=C'ROI'                                                 
         MVI   UKSUBPRG,0                                                       
         PACK  FULL(4),RUNDD(2)    RUN DAY OF MONTH                             
         L     RF,FULL                                                          
         SRL   RF,4                BAG THE SIGN NIBBLE                          
         STC   RF,UKDAY                                                         
         MVI   UKCLASS,C'R'                                                     
*                                                                               
         MVC   KEYSAVE(8),INDEX                                                 
*                                                                               
         MVC   P(18),=C'USING WORKER INDEX'                                     
         MVC   P+20(6),UKUSRID                                                  
         MVC   P+26(2),RUNDD                                                    
         MVC   P+28(1),UKCLASS                                                  
         GOTO1 VPRINTER                                                         
*                                                                               
*- LOOK FOR INDEX ALREADY ON FILE.                                              
         MVC   STATMSG,=CL20'INDEX READ'                                        
INIT100  EQU   *                                                                
         GOTO1 DATAMGR,DMCB,=C'INDEX',=C'WKFILE',INDEX,REC,AWRKRB               
*                                                                               
         TM    P3,X'80'            END OF INDEX                                 
         BO    INIT200                                                          
*                                                                               
         CLI   P3,0                                                             
         BE    *+6                                                              
         DC    H'0'                DISK ERROR ON READ                           
*                                                                               
         CLC   INDEX(8),KEYSAVE    USER THRU CLASS                              
         BNE   INIT100             LOOK FOR NEXT INDEX                          
*                                                                               
         MVC   STATMSG,=CL20'INDEX ON FILE'                                     
         MVC   P(21),=C'INDEX ALREADY ON FILE'                                  
         GOTO1 VPRINTER                                                         
*                                                                               
*- INDEX WAS FOUND ON FILE.                                                     
*  WE MUST BE IN RESTART MODE, ELSE WE'RE RUNNING THIS JOB TWICE                
         CLI   RESTART,C'Y'                                                     
         BE    INIT120                                                          
         MVC   P(40),=CL40'** JOB ALREADY RUN FOR THIS DATE **'                 
         MVC   P+40(6),RUNDATE                                                  
         GOTO1 VPRINTER                                                         
         B     INITERR                                                          
*                                                                               
*- WE ARE RESTART MODE.  SET FILE STATUS TO ACTIVE, NOKEEP.                     
*  PURGE ANY DATA RECORDS IN THE FILE.                                          
INIT120  EQU   *                                                                
         MVC   P(21),=C'RE-USE EXISTING INDEX'                                  
         GOTO1 VPRINTER                                                         
*                                                                               
         TM    UKSTAT,X'80'        ACTIVE STATUS?                               
         BO    INIT130                                                          
*                                                                               
         MVC   P(21),=C'RESTORE ACTIVE STATUS'                                  
         GOTO1 VPRINTER                                                         
         GOTO1 DATAMGR,P1,=C'RESTORE',=C'WKFILE',INDEX,REC,AWRKRB               
*                                                                               
INIT130  TM    UKSTAT,X'08'        IN KEEP STATUS?                              
         BZ    INIT140                                                          
*                                                                               
         MVC   P(6),=C'UNKEEP'                                                  
         GOTO1 VPRINTER                                                         
         GOTO1 DATAMGR,P1,=C'UNKEEP',=C'WKFILE',INDEX,REC,AWRKRB                
*                                                                               
INIT140  EQU   *                                                                
         MVC   P(28),=C'PURGE OLD RECORDS FROM INDEX'                           
         GOTO1 VPRINTER                                                         
         GOTO1 DATAMGR,P1,=C'PURGE',=C'WKFILE',INDEX,REC,AWRKRB                 
         B     INIT220                                                          
*                                                                               
*- OPEN NEW INDEX FILE                                                          
INIT200  XC    INDEX,INDEX                                                      
         MVC   INDEX(8),KEYSAVE                                                 
*                                                                               
         MVC   STATMSG,=CL20'NEW INDEX'                                         
         MVC   P(16),=C'CREATE NEW INDEX'                                       
         GOTO1 VPRINTER                                                         
*                                                                               
         GOTO1 DATAMGR,P1,=C'OPEN',=C'WKFILE',INDEX,REC,AWRKRB                  
         CLI   P3,0                                                             
         BE    *+6                                                              
         DC    H'0'                ERROR ON OPEN                                
*                                                                               
INIT220  EQU   *                                                                
         MVI   OPENFILE,C'Y'       INDEX FILE IS OPEN                           
         DROP  R7                                                               
*                                                                               
*- OPEN RECOVERY TAPE AS INPUT FILE                                             
         OPEN  (RECVIN,(INPUT))                                                 
*                                                                               
*- OPEN DATAMGR                                                                 
         GOTO1 DATAMGR,P1,=C'DMOPEN',=C'REP',FLIST,IOAREA                       
*                                                                               
*- POINT TO CURRENT IO AREA                                                     
         LA    RF,IOAREA                                                        
         ST    RF,AIOAREA          POINT TO I/O BUFFER                          
*                                                                               
*- READ ALL REPS ON THE FILE & SAVE IN REPTABLE.  ALSO SAVE                     
*  REP PROFILE 1 (PWC ENABLE PROFILE)                                           
         MVC   STATMSG,=CL20'BUILD REP TBL'                                     
         SR    R7,R7               COUNT # TBL ENTRIES HERE                     
         LA    R8,REPTABLE                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           REP KEY ID                                   
         BAS   RE,HIGH                                                          
         B     INIT420                                                          
*                                                                               
INIT400  BAS   RE,SEQ                                                           
INIT420  CLI   KEY,X'01'           A REP KEY?                                   
         BNE   INIT450                                                          
*                                                                               
         BAS   RE,GREC             READ IN REP RECORD                           
*                                                                               
*- BUILD TABLE ENTRY                                                            
         MVC   RTREP(2,R8),RREPKREP    REP POWER CODE                           
         MVC   RTPR1(1,R8),RREPPROF    1ST PROFILE                              
*                                                                               
*- POINT TO NEXT ENTRY....BUMP TABLE COUNT                                      
         LA    R7,1(R7)            BUMP TABLE COUNT                             
         LA    R8,RTELEN(R8)       POINT TO NEXT ENTRY                          
         B     INIT400                                                          
*                                                                               
INIT450  STH   R7,NUMREPS          SAVE # REPS IN TABLE                         
*                                                                               
INITOK   EQU   *                                                                
         MVC   STATMSG,=CL20'INITIAL IS OK'                                     
         SR    R0,R0               SET 0 (GOOD) CC.                             
         B     INITEXIT                                                         
*                                                                               
INITERR  LTR   RD,RD               SET ^0 (BAD) CC                              
*                                                                               
INITEXIT XIT1                                                                   
         TITLE 'PROCESS CONTROL CARDS'                                          
*                                                                               
*- CTLCARDS -- READ CONTROL CARDS FROM USER'S JCL.                              
*                                                                               
*        LOG CONTROL CARDS TO PRINTER                                           
*        VALIDATE ALL PASSED CARDS                                              
*        INSURE ALL REQUIRED CARDS ARE PRESENT                                  
*                                                                               
CTLCARDS NTR1                                                                   
         MVI   STOP,C'N'                                                        
*                                                                               
         MVC   P(13),=CL13'CONTROL CARDS'                                       
         GOTO1 VPRINTER                                                         
         MVC   P(13),=CL13'-------------'                                       
         GOTO1 VPRINTER                                                         
*                                                                               
*- READ IN CONTROL CARDS                                                        
CARD100  GOTO1 =V(CARDS),DMCB,IOAREA,=C'RE00'                                   
         CLC   =C'/*',IOAREA                                                    
         BE    CARD900             END OF CARD INPUT                            
*                                                                               
         MVC   P(80),IOAREA        DISPLAY CONTROL CARDS                        
         GOTO1 VPRINTER                                                         
*                                                                               
         CLC   =C'DATE=',IOAREA                                                 
         BE    CARD200                                                          
*                                                                               
         CLC   =C'RESTART=',IOAREA                                              
         BE    CARD300                                                          
*                                                                               
         CLC   =C'ID=',IOAREA                                                   
         BE    CARD400                                                          
*                                                                               
         CLC   =C'WRITE=',IOAREA                                                
         BE    CARD500                                                          
*                                                                               
*- INVALID CONTROL CARD.  STOP AFTER ALL CARDS PROCESSED.                       
         MVC   P(26),=CL26'** INVALID CONTROL CARD **'                          
CARDERR  GOTO1 VPRINTER                                                         
         MVI   STOP,C'Y'                                                        
         B     CARD100             PROCESS ALL CARDS                            
         SPACE 2                                                                
*                                                                               
*- DATE CARD.  NORMAL OR YYMMDD DATE.                                           
CARD200  EQU   *                                                                
         CLI   CTLDATE,C'Y'                                                     
         BNE   CARD220                                                          
         MVC   P(30),=CL30'** ONLY 1 DATE CARD ALLOWED **'                      
         B     CARDERR                                                          
*                                                                               
CARD220  MVI   CTLDATE,C'Y'        PROCESSED THE DATE CARD                      
*                                                                               
         CLC   =C'NORMAL',IOAREA+5                                              
         BE    CARD240                                                          
*                                                                               
*- DATE SPECIFIED                                                               
         GOTO1 =V(DATVAL),DMCB,(0,IOAREA+5),RUNDATE                             
         ICM   RF,15,P1                                                         
         BNZ   CARD100             DATE OK.  NEXT CARD                          
         MVC   P(40),=CL40'** INVALID DATE. MUST BE MMMDD/YY **'                
         B     CARDERR                                                          
*                                                                               
*- NORMAL RUN.  GET TODAY'S DATE                                                
CARD240  EQU   *                   GET SYSTEM DATE, YYMMDD FORMAT               
         GOTO1 =V(DATCON),DMCB,(5,IOAREA+5),(0,RUNDATE)                         
         B     CARD100             GET NEXT CARD.                               
         SPACE 2                                                                
*                                                                               
*- RESTART CARD.  'Y' OR 'N'                                                    
CARD300  EQU   *                                                                
         CLI   CTLRESTR,C'Y'                                                    
         BNE   CARD320                                                          
         MVC   P(30),=CL40'** ONLY 1 RESTART CARD ALLOWED **'                   
         B     CARDERR                                                          
*                                                                               
CARD320  MVI   CTLRESTR,C'Y'       PROCESSED RESTART CARD                       
*                                                                               
         CLI   IOAREA+8,C'Y'                                                    
         BE    CARD340                                                          
         CLI   IOAREA+8,C'N'                                                    
         BE    CARD340                                                          
         MVC   P(50),=CL50'** RESTART IS A ''Y'' OR ''N'' QUESTION **'          
         B     CARDERR                                                          
*                                                                               
CARD340  MVC   RESTART(1),IOAREA+8                                              
         B     CARD100             NEXT CARD                                    
         SPACE 2                                                                
*                                                                               
*- ID CARD.                                                                     
CARD400  EQU   *                                                                
*                                                                               
         MVC   SAVNAME,IOAREA+3    SAVE SYSTEM USER NAME                        
         MVI   CTLUSER,C'Y'        SET FLAG TO 'RECEIVED'                       
*                                                                               
         B     CARD100             NEXT CARD                                    
         SPACE 2                                                                
*                                                                               
*- WRITE = NO CARD                                                              
CARD500  EQU   *                                                                
*                                                                               
         MVC   SAVWRIT,IOAREA+6    SAVE WRITE=NO FLAG                           
         CLC   =C'NO',IOAREA+6                                                  
         BE    CARD100             OKAY                                         
         MVC   P(50),=CL50'** FORMAT IS ''WRITE=NO''                 '          
         B     CARDERR                                                          
*                                                                               
         SPACE 2                                                                
*                                                                               
*- MAKE SURE ALL CONTROL CARDS WERE PASSED                                      
CARD900  EQU   *                                                                
         CLI   CTLDATE,C'Y'                                                     
         BE    CARD920                                                          
         MVC   P(50),=CL50'** MISSING DATE CONTROL CARD **'                     
         GOTO1 VPRINTER                                                         
         MVI   STOP,C'Y'                                                        
*                                                                               
CARD920  CLI   CTLRESTR,C'Y'                                                    
         BE    CARD940                                                          
         MVC   P(50),=CL50'** MISSING RESTART CONTROL CARD **'                  
         GOTO1 VPRINTER                                                         
         MVI   STOP,C'Y'                                                        
*                                                                               
CARD940  EQU   *                                                                
         CLI   CTLUSER,C'Y'                                                     
         BE    CARD960                                                          
         MVC   P(50),=CL50'** MISSING ID=USER CONTROL CARD **'                  
         GOTO1 VPRINTER                                                         
         MVI   STOP,C'Y'                                                        
*                                                                               
CARD960  EQU   *                                                                
CARDEXIT CLI   STOP,C'N'                                                        
         XIT1                                                                   
         TITLE 'PROCESS RECOVERY TAPE AS INPUT. WRITE TO WORKER FILE'           
*                                                                               
*- RECOVIN -- READ RECOVERY RECORDS & BUILD SORT FILE.                          
*                                                                               
*  ACCEPT REP FILE BUY/CONTRACT/AVAIL/PROPOSAL RECORDS (ADD/CHANGES)            
*  CHECK REP CODE FOR ROI ENABLED PROFILE (TABLE LOOKUP)                        
*                                                                               
RECOVIN  NTR1                                                                   
         SPACE                                                                  
         MVC   STATMSG,=CL20'IN RECOVIN'                                        
*                                                                               
*- GET RECOVERY RECORD FROM INPUT TAPE                                          
IPT20    LA    R0,RECVHDR-4                                                     
         GET   RECVIN,(0)                                                       
*                                                                               
*- PUT X'0' AT END OF RECORD FOR GETEL                                          
         LA    RE,RECVHDR-4                                                     
         AH    RE,0(RE)                                                         
         XC    0(2,RE),0(RE)                                                    
*                                                                               
*- RECOVERY RECORD FILTERING                                                    
*                                                                               
*  ACCEPT REP ADD/CHANGES FROM CONTRACT OR SFM                                  
*                                                                               
         CLI   RFILTY,X'82'        REPFILE                                      
         BNE   IPT200                                                           
         CLI   RRECTY,X'02'        CHANGE?                                      
         BE    IPT40                                                            
         CLI   RRECTY,X'03'        ADD?                                         
         BNE   IPT200                                                           
IPT40    CLI   RPRG,X'02'          PROGRAM, 2=CONTRACT                          
         BE    IPT50                                                            
         CLI   RPRG,X'18'          SFM PROGRAM?                                 
         BNE   IPT200                                                           
*                                                                               
*- ONLY ACCEPT RECORDS IN RECORD LIST.                                          
IPT50    EQU   *                                                                
         SPACE                                                                  
         CLC   =X'0CC1C301010363061501',RKEY  THIS THE BAD RECORD?              
         BE    IPT20                           YES, BYPASS                      
         SPACE                                                                  
         L     R2,=A(RECLIST)                                                   
IPT55    CLI   0(R2),0             END OF LIST?                                 
         BE    IPT200                                                           
         CLC   RKEY(1),RLID(R2)    RECOV REC ID = LIST?                         
         BE    IPT56                                                            
         LA    R2,RECLNTRY(R2)                                                  
         B     IPT55                                                            
*                                                                               
*- CHECK TAPE DATE AGAINST RUN DATE.                                            
*  PRINT WARNING MESSAGE IF DATES ARE OUT OF WHACK                              
*   (RECORDS ADDED TO WORKER FILE ANYWAY)                                       
IPT56    EQU   *                                                                
         GOTO1 =V(DATCON),P1,(3,RDATE),(0,TAPEDATE)                             
         CLC   RUNDATE,TAPEDATE                                                 
         BE    IPT60               DATES ARE OK                                 
*                                                                               
*- TAPE DATE COULD BE FROM PRIOR DAY, AFTER 1500 HOURS                          
*  (AFTER 11PM RECOVERY TAPE DUMP)                                              
         CLC   RTIME(3),=X'150000'                                              
         BL    IPT57               BEFORE 11PM=ERROR                            
*                                                                               
         GOTO1 =V(ADDAY),P1,TAPEDATE,TAPEDATE,1                                 
         CLC   RUNDATE,TAPEDATE                                                 
         BE    IPT60                                                            
*                                                                               
IPT57    MVC   P(42),=CL42'** DATE MISMATCH. TAPE=XXXXXX   RUN=XXXXXX'          
         MVC   P+23(6),TAPEDATE                                                 
         MVC   P+36(6),RUNDATE                                                  
         GOTO1 =V(HEXOUT),P1,RTIME,P+45,4,=C'STD'                               
         GOTO1 VPRINTER                                                         
*                                                                               
*- CHECK REP CODE FOR ROI ENABLED                                               
IPT60    EQU   *                                                                
*                                                                               
         ST    R2,ARECNTRY         SAVE A(REC LIST ENTRY)                       
*                                                                               
         LH    R0,NUMREPS          # ENTRIES IN REP TABLE                       
         LA    R1,RKEY                                                          
         ZIC   RF,RLREP(R2)                                                     
         AR    R1,RF               R1=A(REP CODE IN KEY)                        
         LA    RE,REPTABLE                                                      
*                                                                               
IPT65    CLC   RTREP(2,RE),0(R1)                                                
         BNE   IPT70                                                            
*                                                                               
         CLI   RTPR1(RE),C'Y'      ENABLED?                                     
         BNE   IPT200              NO                                           
         B     IPT75               YES                                          
*                                                                               
IPT70    LA    RE,RTELEN(RE)                                                    
         BCT   R0,IPT65                                                         
         DC    H'0'                REP NOT IN REP LIST?                         
         SPACE                                                                  
*                                                                               
*- RECOVERY RECORD PASSES VALIDATION.                                           
*                                                                               
*  CALL SPECIAL PROCESS ROUTINE TO COMPRESS UNWANTED DATA FROM                  
*  RECORD, THEN WRITE TO WORKER FILE.                                           
*                                                                               
IPT75    EQU   *                                                                
         MVI   FIRSTSW,C'N'        NOT 1ST ANYMORE                              
*                                                                               
         MVC   LENBEFR,WRKREC                                                   
*                                                                               
         MVC   STATMSG,=CL20'SPECIAL PROCESS'                                   
         L     RF,RLPROC(R2)                                                    
         BASR  RE,RF                                                            
*                                                                               
         MVC   LENAFTR,WRKREC                                                   
*                                                                               
*- ADD BYTE COMPRESSION TO RUNNING COMPRESSION COUNT.                           
         LH    RF,LENBEFR                                                       
         LH    RE,LENAFTR                                                       
         SR    RF,RE               BYTES COMPRESSED THIS REC                    
         A     RF,COMPRESS                                                      
         ST    RF,COMPRESS         ADD TO RUNNING COUNT                         
*                                                                               
*- ADD UP NUMBER OF BYTES WRITTEN TO FILE                                       
*  (RE = LENGTH OF CURRENT RECORD)                                              
         LR    RF,RE                                                            
         A     RF,DATALEN                                                       
         ST    RF,DATALEN                                                       
*                                                                               
*- STATISTICS COUNTING:  SMALLEST REC, LARGEST REC, AVG LEN                     
         C     RE,RLSMALL(R2)                                                   
         BH    IPT100                                                           
         ST    RE,RLSMALL(R2)      SAVE NEW SMALLEST                            
*                                                                               
IPT100   C     RE,RLBIG(R2)                                                     
         BL    IPT120                                                           
         ST    RE,RLBIG(R2)        SAVE NEW BIGGEST                             
*                                                                               
IPT120   A     RE,RLTOT(R2)                                                     
         ST    RE,RLTOT(R2)        KEEP RUNNING LEN FOR AVG                     
*                                                                               
         L     RF,RLCOUNT(R2)      COUNT # RECS ADDED                           
         A     RF,ONE                                                           
         ST    RF,RLCOUNT(R2)                                                   
*                                                                               
*- WRITE OUT WORKER RECORD                                                      
         MVC   STATMSG,=CL20'ADDING TO WORKER'                                  
*                                                                               
         CLC   SAVWRIT,=C'NO'      SKIP FILE WRITE?                             
         BE    IPT200              YES                                          
*                                                                               
         GOTO1 DATAMGR,P1,=C'ADD',=C'WKFILE',INDEX,WRKREC,AWRKRB                
         CLI   P3,0                                                             
         BE    *+6                                                              
         DC    H'0'                WORKER ERROR ON ADD                          
*                                                                               
IPT200   EQU   *                                                                
         B     IPT20                                                            
*                                                                               
*- CLOSE INPUT FILE                                                             
IPT900   EQU   *                                                                
         MVI   STOP,C'N'           NORMAL EXIT                                  
IPTEXIT  EQU   *                                                                
         CLOSE (RECVIN,)           CLOSE TAPE FILE                              
*                                                                               
         CLI   OPENFILE,C'Y'       CLOSE WORKER FILE IF OPEN                    
         BNE   IPTEXIT2                                                         
         MVC   P(20),=CL20'CLOSING WORKER FILE'                                 
         GOTO1 VPRINTER                                                         
         GOTO1 DATAMGR,P1,=C'CLOSE',=C'WKFILE',INDEX,REC,AWRKRB                 
*                                                                               
IPTEXIT2 CLI   STOP,C'N'           SET RETURN CODE                              
         XIT1                                                                   
         TITLE 'SPECIAL PROCESS ROUTINES'                                       
*                                                                               
*- SPECIAL PROCESS ROUTINES.                                                    
*  * ALL ROUTINES DELETE COMMENTS FROM PASSED RECORDS *                         
*                                                                               
BUYPROC  NTR1                                                                   
         LA    R2,BUYCMT           LIST OF ELEMENTS TO DELETE                   
         BAS   RE,NOEL                                                          
         B     EXIT                                                             
         SPACE 2                                                                
CONPROC  NTR1                                                                   
         LA    R2,CONCMT           LIST OF ELEMENTS TO DELETE                   
         BAS   RE,NOEL                                                          
         B     EXIT                                                             
         SPACE 2                                                                
AVLPROC  NTR1                                                                   
         LA    R2,AVLCMT           LIST OF ELEMENTS TO DELETE                   
         BAS   RE,NOEL                                                          
         B     EXIT                                                             
         SPACE 2                                                                
PRPPROC  NTR1                                                                   
         LA    R2,PRPCMT           LIST OF ELEMENTS TO DELETE                   
         BAS   RE,NOEL                                                          
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
*- REMOVE ELEMENTS FROM RECOVERY RECORD                                         
*  R2 = A(LIST OF ELEMENTS TO DELETE)  END W/X'00'                              
*                                                                               
*- INTERNAL REGISTER USAGE                                                      
*  R3 = RECOVERY RECORD LENGTH                                                  
*  R4 = SUB-RECORD LENGTH (REP FILE RECORD)                                     
*  R5 = A(DESTINATION)                                                          
*  R6 = A(SOURCE ELEMENT)                                                       
*                                                                               
NOEL     NTR1                                                                   
         CLI   0(R2),0             NULL LIST?                                   
         BE    NOELEXIT                                                         
*                                                                               
*- LOAD LENGTHS/POINTERS                                                        
         ZICM  R3,WRKREC,(3)                                                    
         ZICM  R4,RKEY+27,(3)                                                   
         LA    R5,RKEY+34                                                       
         LR    R6,R5                                                            
*                                                                               
NOEL050  EQU   *                                                                
         LR    RF,R2               SOURCE ELEMENT IN LIST                       
NOEL100  CLC   0(1,RF),0(R6)                                                    
         BE    NOEL200             YES.  DELETE                                 
         LA    RF,1(RF)                                                         
         CLI   0(RF),0             END OF LIST                                  
         BNE   NOEL100                                                          
*                                                                               
*- ELEMENT NOT IN LIST.  SAVE TO DESTINATION. GET NEXT SOURCE.                  
         ZIC   RF,1(R6)                                                         
         LR    RE,RF               SAVE ACTUAL LENGTH                           
         BCTR  RF,0                                                             
         EX    RF,NOELKEEP                                                      
         AR    R5,RE               NEXT SOURCE                                  
         AR    R6,RE                                                            
NOEL150  CLI   0(R6),0                                                          
         BNE   NOEL050             PROCESS NEXT SOURCE.                         
*                                                                               
*- END OF RECORD.  PUT BACK UPDATED RECORD LENGTHS.                             
         STCM  R3,X'3',WRKREC                                                   
         STCM  R4,X'3',RKEY+27                                                  
*                                                                               
*- STUFF IN ENDING 0'S J.I.C.                                                   
         LA    RF,WRKREC                                                        
         AR    RF,R3                                                            
         XC    0(2,RF),0(RF)                                                    
*                                                                               
NOELEXIT XIT1                                                                   
*                                                                               
*- DELETE ELEMENT.                                                              
*  SUBTRACT SOURCE LENGTH FROM RECORD LENGTHS                                   
*  POINT TO NEXT SOURCE.                                                        
NOEL200  EQU   *                                                                
         ZIC   RF,1(R6)            SOURCE LENGTH                                
         SR    R3,RF                                                            
         SR    R4,RF                                                            
         AR    R6,RF               NEXT SOURCE                                  
         B     NOEL150                                                          
*                                                                               
*- MOVE SOURCE ELEM TO DESTINATION ELEMENT                                      
NOELKEEP MVC   0(0,R5),0(R6)                                                    
         TITLE 'PRINT OUT END OF RUN REPORT'                                    
*                                                                               
*- PRINT OUT END OF RUN REPORT.                                                 
RUNSTAT  NTR1                                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(28),=C'RECORDS ADDED TO WORKER FILE'                           
         MVC   P+35(30),=CL30'  SMALLEST   LARGEST   AVERAGE'                   
         GOTO1 VPRINTER                                                         
         MVC   P(28),=C'----------------------------'                           
         MVC   P+35(30),=CL30'  --------   -------   -------'                   
         GOTO1 VPRINTER                                                         
*                                                                               
         L     R2,=A(RECLIST)                                                   
RSTAT100 CLI   0(R2),0                                                          
         BE    RSTAT200                                                         
*                                                                               
         MVC   P(8),RLTAG(R2)                                                   
         MVC   FULL(4),RLCOUNT(R2)                                              
         EDIT  (4,FULL),(12,P+10),COMMAS=YES                                    
*                                                                               
*- IF WE ADDED AT LEAST 1 RECORD, PRINT OUT OTHER STATS                         
         CLC   FULL,ZERO                                                        
         BE    RSTAT190                                                         
*                                                                               
         MVC   FULL,RLSMALL(R2)                                                 
         EDIT  (4,FULL),(10,P+35),COMMAS=YES                                    
*                                                                               
         MVC   FULL,RLBIG(R2)                                                   
         EDIT  (4,FULL),(10,P+45),COMMAS=YES                                    
*                                                                               
         SR    RE,RE                                                            
         L     RF,RLTOT(R2)                                                     
         D     RE,RLCOUNT(R2)                                                   
         ST    RF,FULL                                                          
         EDIT  (4,FULL),(10,P+55),COMMAS=YES                                    
*                                                                               
RSTAT190 GOTO1 VPRINTER                                                         
         LA    R2,RECLNTRY(R2)                                                  
         B     RSTAT100                                                         
*                                                                               
RSTAT200 GOTO1 VPRINTER                                                         
         MVC   P(26),=C'NUMBER OF BYTES COMPRESSED'                             
         EDIT  (4,COMPRESS),(14,P+27),COMMAS=YES,ALIGN=LEFT                     
         GOTO1 VPRINTER                                                         
         MVC   P(26),=C'NUMBER OF BYTES WRITTEN   '                             
         EDIT  (4,DATALEN),(14,P+27),COMMAS=YES,ALIGN=LEFT                      
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         MVC   P(20),=CL20'** END OF RUN **'                                    
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         TITLE 'DATA MANAGER INTERFACE -- INCLUDE CODE'                         
       ++INCLUDE RGENIO                                                         
         TITLE 'LITERALS AND CONSTANTS'                                         
         LTORG                                                                  
         SPACE 2                                                                
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=4048,             X        
               MACRF=GM,EODAD=IPT900                                            
         SPACE 2                                                                
FLIST    DS    0H                  DATAMGR FILE LIST                            
         DC    CL8'UREPFILE'                                                    
         DC    CL8' REPDIR '                                                    
         DC    CL8'X       '                                                    
*                                                                               
*- ELEMENT DELETE LISTS.  ELEMENT CODES IN THESE LISTS ARE NOT                  
*  PASSED TO WORKER FILE RECORD.  LIST ENDS WITH X'00'                          
*                                                                               
*  * ALL COMMENT ELEMENT CODES BELONG IN THIS LIST *                            
*                                                                               
BUYCMT   DC    X'04',X'84',X'00'                                                
*                                                                               
CONCMT   DC    X'02',X'07',X'11',X'82',X'92',X'00'                              
*                                                                               
AVLCMT   DC    X'02',X'00'                                                      
*                                                                               
PRPCMT   DC    X'02',X'00'                                                      
         EJECT                                                                  
*                                                                               
*- REC LIST TABLE.  ONE ENTRY FOR EACH RECORD ACCEPTED FROM TAPE.               
*                                                                               
*  TABLE ENDS WITH X'00'.                                                       
*                                                                               
*  TABLE DOES NOT NEED TO BE COVERED BY BASE REGISTER.                          
*                                                                               
RLID     EQU   0   XL1             KEY ID                                       
RLREP    EQU   1   XL1             REP CODE DISP IN KEY                         
*        EQU   2   XL2             SPARE                                        
RLPROC   EQU   4   A               A(PROCESS ROUTINE)                           
RLCOUNT  EQU   8   F               NUMBER RECS WRITTEN TO WORKER                
RLTAG    EQU   12  CL8             LABELING TAG                                 
RLSMALL  EQU   20  F               SMALLEST REC LEN FOUND                       
RLBIG    EQU   24  F               BIGGEST RECORD LENGTH FOUND                  
RLTOT    EQU   28  F               TOTAL SPACE USED BY THESE RECORDS            
         SPACE                                                                  
RECLIST  DS    0F                  FULL WORD BOUNDARY                           
*                                                                               
*- BUY                                                                          
         DC    X'0B',AL1(RBUYKREP-RBUYKEY),H'0',A(BUYPROC),F'0'                 
         DC    CL8'BUY',F'1000',2F'0'                                           
         SPACE                                                                  
RECLNTRY EQU   *-RECLIST           LENGTH OF 1 ENTRY                            
*                                                                               
*- CONTRACT                                                                     
         DC    X'0C',AL1(RCONKREP-RCONKEY),H'0',A(CONPROC),F'0'                 
         DC    CL8'CONTRACT',F'1000',2F'0'                                      
*                                                                               
*- AVAIL                                                                        
         DC    X'14',AL1(RAVLKREP-RAVLKEY),H'0',A(AVLPROC),F'0'                 
         DC    CL8'AVAIL',F'1000',2F'0'                                         
*                                                                               
*- PROPOSAL                                                                     
         DC    X'16',AL1(RPRPKREP-RPRPKEY),H'0',A(PRPPROC),F'0'                 
         DC    CL8'PROPOSAL',F'1000',2F'0'                                      
*                                                                               
         DC    H'0'                EOT                                          
*                                                                               
ONE      DC    F'1'                                                             
ZERO     DC    F'0'                                                             
         TITLE 'WORK AREA'                                                      
         DS    0F                                                               
WORKD    EQU   *                                                                
         DC    C'DMCB'                                                          
DMCB     DS    0CL24               PARAMETER LIST                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
         DC    CL4'DUB*'                                                        
DUB      DS    D                                                                
DOUBLE   DS    D                                                                
         DC    C'WORK'                                                          
WORK     DS    CL64                                                             
         DC    C'FULL'                                                          
FULL     DS    F                                                                
WORD     DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
THREE    DS    CL3                                                              
BYTE     DS    CL1                                                              
         SPACE 1                                                                
* FILE HANDLING AREAS *                                                         
         SPACE 1                                                                
         DC    C'KEYS'                                                          
KEY      DS    CL32                KEY                                          
KEYSAVE  DS    CL32                KEY SAVED BEFORE READ HIGH                   
DMWORK   DS    CL96                                                             
         SPACE 2                                                                
DMREAD   DC    CL8'DMREAD'         COMMANDS                                     
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMADD    DC    CL8'DMADD'                                                       
DMWRT    DC    CL8'DMWRT'                                                       
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
         SPACE 2                                                                
LASTFILE DS    F                   BETWEEN I/O CONTROLS                         
LASTDA   DS    F                                                                
LASTIO   DS    F                                                                
LASTLEN  DS    H                                                                
LASTKYST DS    H                                                                
DATADISP DS    H                   DISPLACEMENT TO FIRST ELEMENT                
DMINBTS  DS    CL1                 SET TO X'80'                                 
         EJECT                                                                  
* EXTERNAL ADDRESS DIRECTORY  *                                                 
         DC    0F'0'                                                            
         DC    C'ADDR'                                                          
DATAMGR  DC    V(DATAMGR)                                                       
PRINT    DC    V(PRINT)                                                         
DATCON   DC    V(DATCON)                                                        
DWRKRB   DC    A(MY4KBUF)          WORKER BUFFER                                
VPRINTER DC    V(PRINTER)                                                       
         SPACE 2                                                                
COMMAND  DS    CL8                 COMMAND FOR RGENIO                           
AIOAREA  DS    A                   FOR RGENIO                                   
*                                                                               
ARECNTRY DS    A                   A(RECLIST) ENTRY                             
*                                                                               
RETCODE  DS    F                   RETURN CODE                                  
*                                                                               
         ENTRY UTL                 FOR DATAMGR                                  
UTL      DC    F'0',X'08'          REP FILE                                     
*                                                                               
*- CONTROL CARD VERIFICATION.                                                   
*  MAKE SURE ALL REQUIRED CARDS WERE SPECIFIED IN JCL DECK.                     
*                                                                               
CTLDATE  DC    C'N'                DATE CARD PROCESSED                          
CTLRESTR DC    C'N'                RESTART CARD PROCESSED                       
CTLUSER  DC    C'N'                ID=USER CARD PROCESSED                       
STOP     DC    C'N'                TERMINATE RUN                                
*                                                                               
FIRSTSW  DC    C'Y'                C'Y' = 1ST PASS.                             
RESTART  DC    C'N'                ASSUME NORMAL (NO RESTART) RUN               
OPENFILE DC    C'N'                WORKER FILE STATUS                           
*                                                                               
RUNDATE  DS    0CL6                YYMMDD RUN DATE                              
RUNYY    DS    CL2                                                              
RUNMM    DS    CL2                                                              
RUNDD    DS    CL2                                                              
TAPEDATE DS    CL6                 YYMMDD RECOVERY TAPE DATE                    
*                                                                               
SAVNAME  DS    CL10                USER NAME FOR SYSTEM ASSIGNMENT              
SAVWRIT  DS    CL2                 WRITE=NO FLAG                                
ADREC    DS    A                                                                
*                                                                               
*- WORKER FILE STUFF                                                            
         DC    C'INDEX*'                                                        
INDEX    DS    CL16                                                             
*                                                                               
         DC    C'REC*'                                                          
REC      DS    CL96                                                             
*                                                                               
         SPACE                                                                  
*                                                                               
*- REP TABLE -- PRE-READ ALL REPS UP FRONT & SAVE PROFILE 1                     
*                                                                               
RTREP    EQU   0    CL2            REP CODE                                     
RTPR1    EQU   2    CL1            PROF. 1 ('1' = ENABLED)                      
RTELEN   EQU   3                   ENTRY LENGTH                                 
RTMAX    EQU   150                 MAX NUMBER OF ENTRIES                        
*                                                                               
         DC    C'*NUMREPS*'                                                     
         DS    0H                                                               
NUMREPS  DS    H                   NUMBER OF REPS ACTIVE IN TABLE               
*                                                                               
REPTABLE DS    (RTMAX*RTELEN)X                                                  
         SPACE                                                                  
IOAREA   DS    4200X                                                            
         SPACE                                                                  
*                                                                               
*- THE FOLLOWING RECORD DSECTS ARE ORG'D TO IOAREA.                             
*        REP, BUY, CON, AVL, PRP                                                
         PRINT OFF                                                              
         ORG   IOAREA                                                           
       ++INCLUDE REGENREPA         NEW REP RECORD DSECT                         
         SPACE                                                                  
         ORG   IOAREA                                                           
       ++INCLUDE REGENBUY                                                       
         ORG   IOAREA                                                           
       ++INCLUDE REGENCON                                                       
         ORG   IOAREA                                                           
       ++INCLUDE REGENAVL                                                       
         ORG   IOAREA                                                           
       ++INCLUDE REGENPRP                                                       
         PRINT ON                                                               
         ORG   IOAREA+4200                                                      
*                                                                               
         DC    CL4'LEN*'                                                        
LENBEFR  DS    F                                                                
LENAFTR  DS    F                                                                
COMPRESS DS    F                   COMPRESSION COUNTER                          
DATALEN  DS    F                   AMOUNT WRITTEN TO FILE                       
AWRKRB   DC    A(MY4KBUF)                                                       
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
WRKREC   DS    XL4                 RECOVERY RECORD HEADER                       
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 2                                                                
RKEY     DS    0CL27                                                            
         DS    4200C                                                            
*                                                                               
LWORK    EQU   *-WORKD             LENGTH OF WORK AREA                          
         SPACE                                                                  
MY4KBUF  DS    4096X               4K WORKER BUFFER                             
         SPACE                                                                  
*                                                                               
         SPACE 2                                                                
       ++INCLUDE REDPRINTD                                                      
         SPACE 2                                                                
       ++INCLUDE DMWRKRK                                                        
         SPACE 2                                                                
       ++INCLUDE DMWRKRD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003REROIRC0B 05/01/02'                                      
         END                                                                    
