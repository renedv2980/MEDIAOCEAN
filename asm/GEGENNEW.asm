*          DATA SET GEGENNEW   AT LEVEL 004 AS OF 10/11/06                      
*PHASE T00AE3A                                                                  
GENNEW   TITLE '- GENERAL SPOOL/MAINT CONTROLLER'                               
GENNEW   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GENNEW                                                       
         L     R8,0(R1)                                                         
         USING SPOOLD,R8           R8=A(SPOOL WORKING STORAGE)                  
         LA    RC,SPOOLEND                                                      
         USING GEND,RC             RC=A(GLOBAL WORKING STORAGE)                 
         ST    R8,ASPOOLD                                                       
         ST    RB,BASERB                                                        
         ST    RD,BASERD                                                        
         BASR  R9,0                                                             
         AHI   R9,GLOBALS-*                                                     
         USING GLOBALS,R9          R9=A(GLOBAL LITERALS)                        
                                                                                
         GOTOR GENINI              INITIALIZE GEND                              
         CLI   MODE,TESTGLOB       TEST SPECIAL CALL TO TEST GLOBALS            
         JE    XITY                YES - EXIT FOR CALLER TO PROCESS             
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
         DROP  RB                                                               
                                                                                
INIT02   CLI   TWASCR,FF           TEST REQUEST DISPLAY ACTIVE                  
         JE    INIT04                                                           
         TM    GENSTAT4,SVTWA0FF   DO WE SKIP THE RESTORE?                      
         JO    *+8                                                              
         GOTOR RESWRK              NO - RESTORE SYSTEM SAVED AREAS              
         GOTOR GETUSER             SYSTEM SPECIFIC USER NAME & ADDRESS          
                                                                                
INIT04   CLI   GCMODE,GCMSLAVQ     EXIT HERE IF SLAVED MODE                     
         JE    XIT                                                              
         MVC   SVUSEIO,USEIO       SAVE FOR POSSIBLE GCMODE 4                   
         MVC   SVSYSDIR,SYSDIR     SYSTEM SWITCH                                
         MVC   SVSYSFIL,SYSFIL                                                  
         MVC   SVDATADI,DATADISP                                                
         MVC   SVLKEY,LKEY                                                      
         J     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE TWA HEADER FIELDS - LOAD SCREEN AND OVERLAY                *         
***********************************************************************         
                                                                                
MAIN     GOTOR RECVAL              VALIDATE RECORD FIELD                        
         JE    MAIN02                                                           
         TM    GENSTAT6,GES$LINK   TEST DDLINK UPLOAD IN PROCESS                
         JZ    XIT                                                              
         TM    GENSTAT6,GES$EOIF   TEST END OF INPUT FILE                       
         JZ    MAIN                                                             
         L     RD,4(RD)            YES - DO DOUBLE EXIT TO MONITOR              
         J     XIT                                                              
                                                                                
MAIN02   BASE  ,                   ESTABLISH CODE ADDRESSABILITY                
         MVI   OKNO,0                                                           
         GOTOR VALHDR              VALIDATE OTHER HEADER FIELDS                 
         JL    INIT02                                                           
         JH    XIT                                                              
                                                                                
         L     R2,EFHACT           RESET CURSOR TO ACTION                       
         MVI   OVERLAY,2                                                        
*&&US*&& CLI   TWAFIRST,FF         CHECK FOR RUNLAST                            
*&&US*&& BNE   MAIN04                                                           
*&&US*&& L     RF,TWADCONS                                                      
*&&US*&& MVC   AGO,TAOVER-TWADCOND(RF)                                          
*&&US*&& B     REPIT                                                            
                                                                                
MAIN04   GOTOR LOADSP              LOAD SCREEN PHASE                            
         JNE   MODEX                                                            
                                                                                
         XC    CONHEAD,CONHEAD     PRE-CLEAR MESSAGE AREA                       
         MVC   AOVERLAY,SYSDUMMY                                                
         CLI   PHDTADCT,0          OPTION TO LOAD IN DICTIONARY                 
         BE    MAIN06                                                           
         MVC   OVERLAY,PHDTADCT                                                 
         GOTOR LOADEM,DMCB,0                                                    
         ST    R3,ADTADICT                                                      
         MVC   AGO,AOVERLAY                                                     
                                                                                
MAIN06   CLI   PHEDIT,0            IS AN EDIT REQUIRED?                         
         BE    REPIT                                                            
         MVC   OVERLAY,PHEDIT                                                   
         MVC   SVSYSPH,SYSPHASE+1  (KEY TO PROGRAM REC)                         
         GOTOR LOADEM,DMCB,0                                                    
         ST    R3,AGO              SET OVERLAY ENTRY POINT                      
         MVC   KEY,TWAKEYSV                                                     
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+16                                                             
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'TWAKEYSV),TWAKEYSV                                      
                                                                                
         CLI   ONLYSPUL,YESQ       ALWAYS FOR SPOOL                             
         BE    MAIN08                                                           
         CLI   ACTNUM,ACTSEL       NEVER FOR SELECT                             
         BE    MAIN10                                                           
         TM    GENSTAT6,GES$LINK   ALWAYS FOR DDLINK UPLOAD                     
         BNZ   MAIN08                                                           
         CLC   TWALREC,RECNUM      ALWAYS FOR NEW RECORD TYPE                   
         BNE   MAIN08                                                           
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         BNE   MAIN08                                                           
         CLI   TWALACT,ACTADD      VALIDATE IF LAST WAS ADD                     
         BE    MAIN08                                                           
         CLI   TWALACT,ACTSEL      OR SELECT                                    
         BE    MAIN08                                                           
         GOTOR TSTKEY              OR IF ANY KEY FIELDS ENTERED                 
         BE    MAIN08                                                           
         TM    TWASTAT1,VALKEYOK   IF RETURNED LAST TIME, DON'T BOTHER          
         BO    MAIN10                                                           
                                                                                
MAIN08   NI    TWASTAT1,FF-(VALKEYOK)                                           
         MVI   MODE,VALKEY         GO AND VALIDATE KEYS                         
         GOTOR GOOVER                                                           
         MVC   TWAKEYSV,KEY                                                     
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+10                                                             
         MVC   TWAKEYSV,BIGKEY                                                  
         OI    TWASTAT1,VALKEYOK   KEY HAS BEEN VALIDATED                       
                                                                                
MAIN10   TM    WHENOK,WOK$USER     USER NOW HAS THE OPTION                      
         BO    OTHIT               OF DOING HIS OWN MAINTENANCE                 
         CLI   ACTNUM,ACTLIST      SELECT ROUTINE FROM ACTION                   
         BE    LSTIT                                                            
         CLI   ACTNUM,ACTSEL                                                    
         BE    SELIT                                                            
         CLI   ACTNUM,ACTCHA                                                    
         BNE   MAIN12                                                           
         CLI   TWALACT,ACTSEL                                                   
         BE    MAIN14                                                           
                                                                                
MAIN12   XC    LISTDIR,LISTDIR     CLEAR LIST DIRECTORY IF ACTION               
         MVI   THISLSEL,0          IS NOT SELECT OR LIST                        
         L     RE,AREPINFO         CLEAR SAVED REPORT INFO                      
         XC    0(L'REPINFO,RE),0(RE)                                            
                                                                                
MAIN14   L     R2,AFRSTKEY         GO TO APPROPRIATE ACTION ROUTINE             
         CLI   ACTNUM,ACTLIST                                                   
         BH    REPIT                                                            
         SR    RF,RF                                                            
         IC    RF,ACTNUM                                                        
         MHI   RF,L'ACTIT                                                       
         B     ACTIT-L'ACTIT(RF)                                                
                                                                                
ACTIT    DS    0XL4                                                             
         B     ADDIT               ADD A RECORD                                 
         B     CHAIT               CHANGE A RECORD                              
         B     DISIT               DISPLAY A RECORD                             
         B     DELIT               DELETE A RECORD                              
         B     SELIT               SELECT A RECORD FROM LIST                    
         B     RESIT               RESTORE A RECORD                             
         B     OTHIT               APPLICATION DEFINED ACTION 1                 
         B     OTHIT               APPLICATION DEFINED ACTION 2                 
         B     OTHIT               APPLICATION DEFINED ACTION 3                 
         EJECT                                                                  
***********************************************************************         
* ADD A RECORD IN MAINTENANCE MODE                                    *         
***********************************************************************         
                                                                                
ADDIT    GOTOR ADDVAL              VALIDATE KEY OF RECORD TO BE ADDED           
         BE    ADDIT02             RETURNS CC EQUAL IF OK TO ADD                
         L     R1,AFRSTKEY         ELSE POINT CURSOR TO FIRST KEY FIELD         
         OI    FLDOINDD(R1),FOUTTRN+FOUTMOD                                     
         J     ERRXIT              MAKE MODIFIED KEY AND DO DISPLAY             
                                                                                
ADDIT02  TM    GENSTAT6,GES$LINK   TEST DDLINK UPLOAD IN PROCESS                
         BZ    *+8                                                              
         GOTOR GETDTA              YES - SET DATA FIELDS                        
                                                                                
         L     R2,AFRSTREC                                                      
         USING FLDHDRD,R2                                                       
ADDIT04  TM    FLDATB,FATBPROT                                                  
         BO    *+12                                                             
         CLI   FLDILEN,0           ANY DATA YET IN ANY RECORD FIELD             
         BNE   ADDIT06                                                          
         GOTOR BUMP                                                             
         BNE   ADDIT04                                                          
         L     R2,AFRSTREC                                                      
         MVI   OKNO,2              IF NOT USER NEEDS TO FILL IN DATA            
         J     MODEX                                                            
                                                                                
ADDIT06  MVI   MODE,VALREC         SET MODE FOR RECORD VALIDATION               
         GOTOR DOADD               ADD THE RECORD                               
         L     R2,AFRSTKEY                                                      
         MVI   OKNO,6                                                           
         J     OKEX                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CHANGE A RECORD IN MAINTENANCE MODE                                 *         
***********************************************************************         
                                                                                
CHAIT    TM    GENSTAT3,MULTFILS                                                
         BZ    *+12                                                             
         MVI   MODE,SETFILE                                                     
         GOTOR GOOVER                                                           
         GOTOR READUP                                                           
         GOTOR READ                                                             
         CLI   USEIO,YESQ                                                       
         BE    CHAIT02                                                          
         GOTOR READUP                                                           
         GOTOR GETREC                                                           
         MVC   GLOBDA,DMDSKADD                                                  
                                                                                
CHAIT02  CLC   TWALREC,RECNUM      IF NEW RECORD TYPE                           
         BNE   *+12                ALWAYS DISPLAY FIRST                         
         GOTOR TSTKEY              SEE IF ANY KEY WAS INPUT                     
         BNE   CHAIT04                                                          
         MVI   MODE,DISPREC        YES - SO DISPLAY RECORD                      
         GOTOR GOOVER                                                           
         TM    GENSTAT6,GES$LINK   TEST DDLINK UPLOAD IN PROCESS                
         BZ    *+12                                                             
         GOTOR GETDTA              YES - MOVE CHANGES TO SCREEN                 
         B     CHAIT04                                                          
         L     R2,AFRSTREC                                                      
         MVI   OKNO,4              AND ASK USER TO ENTER CHANGES                
         J     OKEX                                                             
                                                                                
CHAIT04  MVI   MODE,VALREC         NO KEY FIELDS CHANGED                        
         GOTOR DOCHA               SO VALIDATE RECORD FIELDS                    
         L     R2,AFRSTKEY                                                      
         MVI   OKNO,5                                                           
         CLI   TWALACT,ACTSEL      ARE WE STILL IN SELECT MODE?                 
         JNE   OKEX                                                             
         L     R1,EFHACT                                                        
         MVC   FLDHDRL(#SELPLQ,R1),LP@SELP                                      
         MVI   FLDILEND(R1),6                                                   
         MVI   ACTNUM,ACTSEL                                                    
         J     OKEX                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD IN MAINTENENACE MODE                               *         
***********************************************************************         
                                                                                
DISIT    GOTOR READ                                                             
         CLI   USEIO,YESQ                                                       
         BE    DISIT02                                                          
         GOTOR GETREC                                                           
                                                                                
DISIT02  CLI   ACTIVSW,0                                                        
         BNE   DISIT04                                                          
         MVI   MODE,DISPREC                                                     
         GOTOR GOOVER                                                           
         L     R2,AFRSTKEY                                                      
         MVI   OKNO,3                                                           
         J     OKEX                                                             
                                                                                
DISIT04  GOTOR DISACT              DISPLAY ACTIVITY DATA                        
         MVI   OKNO,14                                                          
         J     MODEX                                                            
         EJECT                                                                  
***********************************************************************         
* DELETE A RECORD IN MAINTENANCE MODE                                 *         
***********************************************************************         
                                                                                
DELIT    OI    DMINBTS,X'08'                                                    
         GOTOR READUP                                                           
         GOTOR READ                                                             
         TM    GENSTAT6,GES$LINK   UPLOAD DOESN'T REQUIRE CONFIRMATION          
         BNZ   DELIT04                                                          
         TM    GENSTAT4,CONFDEL    TEST CONFIRMATION OF DELETE REQ'D            
         BZ    DELIT04                                                          
         CLC   TWALREC,RECNUM      IF NEW RECORD TYPE                           
         BNE   *+12                ALWAYS DISPLAY FIRST                         
         GOTOR TSTKEY              SEE IF ANY KEY WAS INPUT                     
         BNE   DELIT04                                                          
         CLI   USEIO,YESQ                                                       
         BE    DELIT02                                                          
         GOTOR GETREC                                                           
                                                                                
DELIT02  MVI   MODE,DISPREC        YES - SO DISPLAY RECORD                      
         GOTOR GOOVER                                                           
         L     R2,EFHACT                                                        
         MVI   OKNO,24             AND ASK USER TO HIT ENTER TO DELETE          
         J     OKEX                                                             
                                                                                
DELIT04  GOTOR DODEL               GO DELETE THE RECORD                         
         JH    ERRXIT                                                           
         L     R2,AFRSTKEY                                                      
         MVI   OKNO,7                                                           
         J     OKEX                                                             
         EJECT                                                                  
***********************************************************************         
* RESTORE A RECORD IN MAINTENANCE MODE                                *         
***********************************************************************         
                                                                                
RESIT    OI    DMINBTS,X'08'                                                    
         GOTOR READUP                                                           
         GOTOR READ                                                             
         GOTOR DORES                                                            
         JH    ERRXIT                                                           
         L     R2,AFRSTKEY                                                      
         MVI   OKNO,8                                                           
         J     OKEX                                                             
                                                                                
***********************************************************************         
* LIST RECORDS FOR SELECTION                                          *         
***********************************************************************         
                                                                                
LSTIT    XC    KEY,KEY             LIST                                         
         XC    BIGKEY,BIGKEY                                                    
         TM    WHEN,WOK$SCR        ONLY HANDLING ON-SCREEN HERE                 
         BNO   REPIT                                                            
         GOTOR DOLST                                                            
         BL    MAIN02                                                           
         J     MODEX                                                            
                                                                                
***********************************************************************         
* SELECT A RECORD                                                     *         
***********************************************************************         
                                                                                
SELIT    GOTOR DOSEL                                                            
         BL    MAIN02                                                           
         JH    ERRXIT                                                           
         J     MODEX                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT REPORTS                                                       *         
***********************************************************************         
                                                                                
REPIT    GOTOR DOREP,0                                                          
         JE    REPXIT                                                           
         J     ERRXIT                                                           
                                                                                
***********************************************************************         
* OTHER APPLICATION DEFINED ACTIONS                                   *         
***********************************************************************         
                                                                                
OTHIT    MVI   MODE,VALREC                                                      
         GOTOR GOOVER                                                           
         CLI   CONHEAD,0           ANY MESSAGE SET                              
         JNE   EXIT                                                             
         MVI   OKNO,17             NO - USE A GENERAL MESSAGE                   
         J     OKEX                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* GENERAL CALLABLE ROUTINES                                           *         
***********************************************************************         
                                                                                
ROUTS    NTR1  BASE=*,LABEL=*                                                   
         L     R8,ASPOOLD                                                       
         L     RA,ATWA                                                          
         BASR  R9,0                                                             
         AHI   R9,GLOBALS-*                                                     
         SRL   RF,24                                                            
         B     ROUTGO(RF)                                                       
                                                                                
ROUTGO   B     ANY$                                                             
         B     HIGH$                                                            
         B     SEQ$                                                             
         B     READ$                                                            
         B     WRITE$                                                           
         B     ADD$                                                             
         B     GETREC$                                                          
         B     PUTREC$                                                          
         B     ADDREC$                                                          
         B     VALDATE$                                                         
*&&UK*&& B     VALPER$                                                          
*&&US*&& DC    AL4(0)                                                           
         B     VALNUM$                                                          
         B     VALDEC$                                                          
         B     ERREX$                                                           
         B     ERREX08$                                                         
         B     ADDELEM$                                                         
         B     REMELEM$                                                         
         B     LSTMON$                                                          
         B     DSPSEC$                                                          
         B     VALSEC$                                                          
         B     TSTSEC$                                                          
         B     OPNPQ$                                                           
         J     LOADEM$                                                          
         J     SAVWRK$                                                          
         B     REQ$                                                             
         J     INIREP$                                                          
         B     EREP$                                                            
         B     CATCHIO$                                                         
ROUT1#   EQU   (*-ROUTGO)/4                                                     
                                                                                
ROUT2    B     RFP$                                                             
         J     RESLIOB$                                                         
         J     SAVLIOB$                                                         
         J     XIT                                                              
         J     XIT                                                              
         J     XIT                                                              
ROUT2#   EQU   (*-ROUT2)/4                                                      
         EJECT                                                                  
REQ$     LHI   R1,-1                                                            
         GOTOR DOREP                                                            
         JE    REPXIT                                                           
         J     ERRXIT                                                           
                                                                                
EREP$    CLI   OFFLINE,YESQ                                                     
         JE    XIT                                                              
         GOTOR CLSREP                                                           
         J     REPXIT                                                           
                                                                                
ANY$     CLI   FLDILEND(R2),0                                                   
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         J     ERRXIT                                                           
         MVC   WORK,SPACES                                                      
         SR    R1,R1                                                            
         IC    R1,FLDILEND(R2)                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     XIT                                                              
         MVC   WORK(0),FLDHDRL(R2)                                              
         EJECT                                                                  
***********************************************************************         
* I/O HANDLING ROUTINES - DIRECTORY                                   *         
***********************************************************************         
                                                                                
HIGH$    MVC   COMMAND,DMRDHI                                                   
         MVC   KEYSAVE,KEY                                                      
         TM    GENSTAT4,USEBIGKY   FOR LARGE KEYS USE BIGKEY                    
         BZ    DIRIO                                                            
         MVC   KEYSAVE,BIGKEY                                                   
         B     DIRIO                                                            
                                                                                
SEQ$     MVC   COMMAND,DMRSEQ                                                   
         B     DIRIO                                                            
                                                                                
READ$    MVC   COMMAND,DMREAD                                                   
         MVC   KEYSAVE,KEY                                                      
         TM    GENSTAT4,USEBIGKY   FOR LARGE KEYS USE BIGKEY                    
         BZ    DIRIO                                                            
         MVC   KEYSAVE,BIGKEY                                                   
         B     DIRIO                                                            
                                                                                
WRITE$   MVC   COMMAND,DMWRTL                                                   
         L     R1,AIO                                                           
         CLI   USEIO,YESQ                                                       
         BNE   *+12                                                             
         GOTOR CHAACT                                                           
         B     WRITE02                                                          
         LH    RE,LKEY                                                          
         BCTR  RE,0                                                             
         LA    RF,KEY                                                           
         TM    GENSTAT4,USEBIGKY   TEST USING BIG KEY                           
         BZ    *+8                                                              
         LA    RF,BIGKEY                                                        
         EX    RE,*+8                                                           
         B     WRITE02                                                          
         MVC   0(0,R1),0(RF)                                                    
WRITE02  CLI   TWAWRITE,NOQ                                                     
         JE    XIT                                                              
         B     DIRIO                                                            
                                                                                
ADD$     MVC   COMMAND,DMADD                                                    
         CLI   USEIO,YESQ                                                       
         BNE   *+8                                                              
         GOTOR ADDACT                                                           
         CLI   TWAWRITE,NOQ                                                     
         JE    XIT                                                              
                                                                                
DIRIO    MVC   DMFILE,FILENAME                                                  
         CLI   DMFILE,0            IF USER DID NOT PROVIDE FILE NAME            
         BNE   *+10                                                             
         MVC   DMFILE,SYSDIR       USE SYSTEM DIRECTORY NAME                    
         LA    R0,KEY              READ INTO KEY                                
         TM    GENSTAT4,USEBIGKY   FOR LARGE KEYS USE BIGKEY                    
         BZ    *+8                                                              
         LA    R0,BIGKEY                                                        
         CLI   USEIO,YESQ          OR, OPTIONALLY I/O AREA                      
         BNE   DIRIO02                                                          
         L     R0,AIO                                                           
                                                                                
DIRIO02  GOTOR SETRUP              SET READ FOR UPDATE                          
                                                                                
         LA    RF,KEY              DEFAULT DATAMGR P3                           
         TM    GENSTAT4,USEBIGKY   FOR LARGE KEYS USE BIGKEY                    
         BZ    *+8                                                              
         LA    RF,BIGKEY                                                        
         TM    GENSTAT3,USEKEYSV                                                
         BZ    *+14                                                             
         MVC   KEYSAVE,0(RF)       PASS CURRENT KEY                             
         LA    RF,KEYSAVE          NEW DATAMGR P3                               
         GOTOR DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,(RF),(R0),0                
         CLI   USEIO,YESQ                                                       
         BNE   CHKIO                                                            
         L     R1,AIO                                                           
         MVC   KEY,0(R1)                                                        
         TM    GENSTAT4,USEBIGKY                                                
         BZ    *+10                                                             
         MVC   BIGKEY,0(R1)                                                     
         B     CHKIO                                                            
         EJECT                                                                  
***********************************************************************         
* I/O HANDLING ROUTINES - FILE                                        *         
***********************************************************************         
                                                                                
GETREC$  MVC   COMMAND,DMGETR                                                   
         LA    R3,KEY                                                           
         TM    GENSTAT4,USEBIGKY   FOR LARGE KEYS USE BIGKEY                    
         BZ    *+8                                                              
         LA    R3,BIGKEY                                                        
         AH    R3,LKEY                                                          
         AH    R3,LSTATUS          R3=A(D/A)                                    
         B     ALLREC                                                           
                                                                                
PUTREC$  MVC   COMMAND,DMPUTR                                                   
         GOTOR CHAACT              UPDATE ACTIVITY                              
         CLI   TWAWRITE,NOQ                                                     
         JE    XIT                                                              
         LA    R3,DMWORK+4         R3=A(D/A)                                    
         B     ALLREC                                                           
                                                                                
ADDREC$  MVC   COMMAND,DMADDR                                                   
         GOTOR ADDACT              ADD AN ACTIVITY ELEMENT                      
         CLI   TWAWRITE,NOQ                                                     
         JE    XIT                                                              
         LA    R3,KEY              R3=A(WHERE DM WILL RETURN D/A)               
                                                                                
ALLREC   MVC   DMFILE,FILENAME                                                  
         CLI   DMFILE,0            IF USER DID NOT PROVIDE FILENAME             
         BNE   *+10                                                             
         MVC   DMFILE,SYSFIL       USE STANDARD SYSTEM FILE NAME                
         GOTOR SETRUP              SET READ FOR UPDATE                          
         GOTOR DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,(R3),AIO,DMWORK            
         MVC   DMDSKADD,0(R3)                                                   
                                                                                
CHKIO    MVI   RDUPDATE,YESQ       RESET READ FOR UPDATE                        
         OI    DMINBTS,X'80'                                                    
         TM    GENSTAT1,RDUPAPPL                                                
         BZ    *+12                                                             
         MVI   RDUPDATE,NOQ        UNLESS CONTROLLED BY APPLICATION             
         NI    DMINBTS,X'7F'                                                    
                                                                                
         TM    GENSTAT4,NODUPDIE   DON'T DIE ON DUPLICATE KEY ON ADD            
         BO    CHKIO04                                                          
         CLC   COMMAND,DMADDR                                                   
         BE    CHKIO02                                                          
         CLC   COMMAND,DMPUTR                                                   
         BE    CHKIO02                                                          
         CLC   COMMAND,DMADD                                                    
         BE    CHKIO02                                                          
         CLC   COMMAND,DMWRTL                                                   
         BNE   CHKIO04                                                          
                                                                                
CHKIO02  CLI   DMCB+8,0                                                         
         BE    CHKIO04                                                          
         DC    H'0'                DIE ON NON-ZERO RETURN CODE                  
                                                                                
CHKIO04  MVC   DUB(1),DMCB+8                                                    
         NC    DUB(1),DMOUTBTS                                                  
         JZ    XIT                                                              
         MVI   ERROR,0                                                          
         J     ERRXIT                                                           
                                                                                
SETRUP   CLI   ACTNUM,ACTLIST      IF ACTION IS LIST                            
         BE    *+12                                                             
         CLI   RDUPDATE,NOQ        OR READ FOR UPDATE IS OFF                    
         BNE   *+8                                                              
         NI    DMINBTS,X'7F'       TURN IT OFF                                  
         TM    GENSTAT1,RDUPAPPL   IF APPLIC. CONTROLLING READ UPDATES          
         BZR   RE                                                               
         CLI   RDUPDATE,YESQ       AND READ FOR UPDATE IS REQUESTED             
         BNER  RE                                                               
         OI    DMINBTS,X'80'       DO IT                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DATE, PERIOD                                               *         
***********************************************************************         
                                                                                
VALDATE$ L     R3,0(R1)                                                         
         GOTOR ANY                                                              
         CLC   FLDHDRL(#TODAYLQ,R2),LP@TODAY                                    
         BNE   VALDAT$2                                                         
         GOTOR DATCON,DMCB,(5,0),(0,0(R3))                                      
         MVI   ACTUAL,5                                                         
         J     XIT                                                              
                                                                                
VALDAT$2 GOTOR DATVAL,DMCB,(0,FLDHDRL(R2)),(R3)                                 
         MVC   ACTUAL,DMCB+3                                                    
         CLI   ACTUAL,0                                                         
         JNE   XIT                                                              
         GOTOR DATCON,DMCB,(5,0),(0,0(R3))                                      
         LH    R0,0(R3)            SAVE THIS YEAR                               
         GOTOR DATVAL,DMCB,(1,FLDHDRL(R2)),(R3)                                 
         STH   R0,0(R3)            AND INSERT INTO OUTPUT                       
         MVC   ACTUAL,DMCB+3                                                    
         CLI   ACTUAL,0                                                         
         JNE   XIT                                                              
         MVI   ERROR,INVDATE                                                    
         J     ERRXIT                                                           
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* THIS ROUTINE VALIDATES A DATE PAIR AND RETURNS A 12 BYTE            *         
* FIELD CONTAINING THE TWO DATES IN YYMMDD EBCDIC FORM. IF ONLY       *         
* ONE DATE ENTERED THIS WILL APPEAR IN BOTH SIDES                     *         
*        DDMMMYY-DDMMMYY      DDMMM-DDMMMYY                           *         
*        MMMYY-MMMYY          MMM-MMMYY                               *         
*        DDMMM-DDMMM          MMMYY                                   *         
*        DDMMM                                                        *         
*        MMM-MMM              MMM                                     *         
* EITHER THE TO OR FROM DATE MAY BE OMITTED BY ENTERING THE DASH      *         
* TO INDICATE AN UPTO OR FROM. ANY OF THE ABOVE FORMATS IS VALID      *         
*  E.G.  DDMMMYY-             -DDMMMYY                                *         
*        MMMYY-               -MMMYY    ETC.                          *         
* ON ENTRY R2 ADDRESSES THE DATE FLD HDR AND R1 POINTS TO THE         *         
* ADDRESS OF THE 12 BYTE OUTPUT AREA.                                 *         
***********************************************************************         
                                                                                
VALPER$  L     R3,0(R1)            0(R1)=A(OUTPUT AREA)                         
         MVI   ERROR,MISSING                                                    
         CLI   FLDILEND(R2),0                                                   
         JE    ERRXIT                                                           
         SR    R1,R1                                                            
         IC    R1,BTODAY                                                        
         CVD   R1,DUB                                                           
         UNPK  HALF,DUB+6(2)                                                    
         OI    HALF+1,C'0'         SAVE CURRENT YR IN HALF                      
         XC    BLOCK(32*3),BLOCK                                                
         GOTOR SCANNER,DMCB,(R2),(3,BLOCK),C',=-='                              
         MVI   ERROR,INVDATE                                                    
         LA    R4,BLOCK                                                         
         CLI   4(R1),0                                                          
         JE    ERRXIT                                                           
         CLI   4(R1),2                                                          
         JH    ERRXIT                                                           
         BNE   VALPER02                                                         
         SR    RF,RF                                                            
         IC    RF,32(R4)           CHECK 2ND DATE FOR YEAR INPUT                
         LA    RF,32+10(R4,RF)                                                  
         TM    0(RF),X'F0'         LAST 2 CHARS NUMERIC                         
         BNO   VALPER02                                                         
         TM    1(RF),X'F0'                                                      
         BNO   VALPER02                                                         
         MVC   HALF,0(RF)          IF FOUND OVERWRITE CURRENT YEAR              
                                                                                
VALPER02 NC    0(2,R4),0(R4)       ANY DATA LEFT OF -                           
         BNZ   VALPER04            YES - VALIDATE IT                            
         MVC   4(6,R4),EZEROES                                                  
         NC    32(2,R4),32(R4)                                                  
         JZ    ERRXIT              MUST BE A RIGHT IF NO LEFT                   
         B     VALPER10                                                         
VALPER04 GOTOR VALDAT                                                           
         JNE   ERRXIT                                                           
         TM    2(R4),2             DAY NOT GIVEN                                
         BNO   VALPER06                                                         
         MVC   8(2,R4),=C'01'      SET TO 1ST OF MONTH                          
                                                                                
VALPER06 NC    32(2,R4),32(R4)     NO 2ND DATE                                  
         BNZ   VALPER10                                                         
         SR    R1,R1                                                            
         IC    R1,0(R4)                                                         
         LA    R1,8(R2,R1)                                                      
         CLI   0(R1),C'-'          SEE IF IT WAS DDMMMYY-                       
         BNE   VALPER08                                                         
         MVC   32+12(7,R4),=C'31DEC99'                                          
         MVI   32(R4),7            IF SO SET MAX DATE                           
         B     VALPER10                                                         
VALPER08 MVC   32(32,R4),0(R4)     OTHERWISE PROPAGATE 1ST ENTRY                
         TM    2(R4),2             IF NONE AND 1ST DATE INCLUDED DAY            
         BNO   VALPER12            THEN LEAVE START AND END SAME                
                                                                                
VALPER10 AHI   R4,32               VALIDATE 2ND DATE                            
         GOTOR VALDAT                                                           
         JNE   ERRXIT                                                           
         TM    2(R4),2             DAY NOT GIVEN                                
         BNO   VALPER12            SET TO LAST OF MONTH                         
         MVC   8(2,R4),=C'01'                                                   
         GOTOR ADDAY,DMCB,4(R4),32(R4),35                                       
         MVC   36(2,R4),=C'01'                                                  
         GOTOR (RF),(R1),32(R4),4(R4),-1                                        
                                                                                
VALPER12 MVI   ERROR,INVEBFRS      CHECK FOR START GTR THAN END                 
         LA    R4,BLOCK                                                         
         CLC   4(6,R4),32+4(R4)                                                 
         JH    ERRXIT                                                           
         MVC   0(6,R3),4(R4)       MOVE TO OUTPUT                               
         MVC   6(6,R3),32+4(R4)                                                 
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO VALIDATE A SCANNER ENTRY AT R2                        *         
***********************************************************************         
                                                                                
VALDAT   NTR1  LABEL=NO                                                         
         LR    R2,R4                                                            
         CLI   1(R4),0                                                          
         BNE   VALDATER            '=' PRESENT                                  
         XC    1(11,R2),1(R2)                                                   
         SR    R0,R0                                                            
         CLC   12(#TODAYLQ,R2),LP@TODAY                                         
         BNE   VALDAT02                                                         
         GOTOR DATCON,DMCB,(3,BTODAY),4(R2)                                     
         B     VALDAT06                                                         
VALDAT02 LHI   R4,1                CHECK FOR MY,DM AND DMY                      
         LHI   R5,2                                                             
VALDAT04 GOTOR DATVAL,DMCB,((R0),12(R2)),4(R2)                                  
         OC    DMCB,DMCB                                                        
         BNZ   VALDAT06                                                         
         BXLE  R0,R4,VALDAT04                                                   
         CLI   0(R2),3             CHECK FOR JUST MONTH                         
         BNE   VALDATER                                                         
         MVC   15(2,R2),HALF                                                    
         GOTOR (RF),(R1),(2,12(R2)),4(R2)                                       
         OC    DMCB,DMCB                                                        
         BZ    VALDATER                                                         
         LHI   R0,2                                                             
VALDAT06 STC   R0,2(R2)            SAVE INDICATORS                              
         TM    2(R2),1             IF YEAR MISSING SET TO CURRENT OR            
         BNO   *+10                YEAR OF 2ND DATE                             
         MVC   4(2,R2),HALF                                                     
         J     XITY                                                             
                                                                                
VALDATER J     XITN                                                             
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE NUMERIC                                                    *         
***********************************************************************         
                                                                                
VALNUM$  GOTOR ANY                                                              
         SR    R1,R1                                                            
         IC    R1,FLDILEND(R2)                                                  
         CHI   R1,6                                                             
         JH    ERRNOTN                                                          
         BCTR  R1,0                                                             
         MVC   WORK(6),EZEROES                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),8(R2)                                                    
         CLC   WORK(6),EZEROES                                                  
         JNE   ERRNOTN                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         JZ    ERRNOTN                                                          
         CHI   R1,255                                                           
         JH    ERRNOTN                                                          
         STC   R1,ACTUAL                                                        
         J     XIT                                                              
                                                                                
***********************************************************************         
* VALIDATE CASH VALUE                                                 *         
***********************************************************************         
                                                                                
VALDEC$  GOTOR ANY                                                              
         SR    R0,R0                                                            
         IC    R0,FLDILEND(R2)                                                  
         GOTOR CASHVAL,DMCB,(MAX,FLDHDRL(R2)),(R0)                              
         MVC   FULL,4(R1)                                                       
         CLI   0(R1),0                                                          
         JE    XIT                                                              
         J     ERRNOTN                                                          
         EJECT                                                                  
***********************************************************************         
* ELEMENT MANIPULATION                                                *         
***********************************************************************         
                                                                                
ADDELEM$ CLI   ELEMENT,0                                                        
         JE    XIT                                                              
         SR    RF,RF                                                            
         TM    GENSTAT5,ADDEQCOD                                                
         BZ    *+8                                                              
         LA    RF,=C'ADD=CODE'                                                  
                                                                                
         GOTOR HELLO,DMCB,(C'P',SYSFIL),AIO,ELEMENT,(RF)                        
         CLI   12(R1),0                                                         
         JE    XIT                                                              
         MVI   ERROR,TOOLONG       DID RECORD GET TOO LONG                      
         CLI   12(R1),5                                                         
         JE    ERRXIT                                                           
         DC    H'0'                OTHER ERRORS UNACCEPTABLE                    
                                                                                
REMELEM$ L     R6,AIO                                                           
         XC    ELEMENT,ELEMENT                                                  
         GOTOR GETEL                                                            
         JNE   XIT                                                              
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R6)                                                 
         GOTOR HELLO,DMCB,(C'D',SYSFIL),(ELCODE,AIO),0                          
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO MONITOR LIST ACTIVITY                                    *         
***********************************************************************         
                                                                                
LSTMON$  TM    WHEN,WOK$SCR        ONLY OF INTEREST FOR ON SCREEN               
         JNO   XITY                                                             
         CLC   LISTNUM,NLISTS      IF MAX HAVE ALREADY BEEN INPUT               
         BL    LSTMON02                                                         
         MVI   OKNO,15             SET MESSAGE ASSUMING NO SELECT FIELD         
         GOTOR TSTSEL              IF THIS IS SELECT FIELD                      
         BNE   *+8                                                              
         MVI   OKNO,9              SET MESSAGE WITH SELECT PROMPT               
         TM    GLSTSTAT,CHNGLIST   IF CHANGES ALLOWED ON LIST SCREEN            
         BZ    *+8                                                              
         MVI   OKNO,32             GIVE SPECIAL MESSAGE                         
         B     LSTMONX                                                          
                                                                                
LSTMON02 SR    R1,R1                                                            
         IC    R1,LISTNUM          UPDATE NUMBER IN LIST                        
         AHI   R1,1                                                             
         STC   R1,LISTNUM                                                       
         BCTR  R1,0                                                             
         MHI   R1,6                                                             
         LA    R1,LISTDIR(R1)                                                   
         XC    0(6,R1),0(R1)                                                    
         CLI   USEIO,YESQ                                                       
         BE    LSTMON04                                                         
         MVC   2(4,R1),DMDSKADD    SAVE DISK ADDRESS                            
         MVC   LASTLIST,DMDSKADD                                                
         OC    PAGEDAS+60(4),PAGEDAS+60                                         
         BNZ   *+10                                                             
         MVC   PAGEDAS+60(4),DMDSKADD                                           
         OC    VERYFRST,VERYFRST                                                
         BNZ   *+10                                                             
         MVC   VERYFRST,DMDSKADD                                                
         B     LSTMON06                                                         
                                                                                
LSTMON04 LA    RF,LISTKEYS         FOR IS FILES SAVE KEY                        
         SR    RE,RE                                                            
         IC    RE,LISTNUM                                                       
         BCTR  RE,0                                                             
         MH    RE,LKEY                                                          
         AR    RF,RE                                                            
         LH    R1,LKEY                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),KEY                                                      
         MVC   LASTSELK,KEY                                                     
                                                                                
LSTMON06 L     R2,ATHISLST                                                      
         TM    GLSTSTAT,APPLCDSP   TEST APPLIC ALREADY DISPLAYED DATA           
         BO    LSTMON08                                                         
         SR    R1,R1                                                            
         IC    R1,FLDLEND(R2)                                                   
         SHI   R1,FLDHDRL+1                                                     
         TM    FLDATBD(R2),FATBXHDR                                             
         BZ    *+8                                                              
         SHI   R1,FLDHDRL                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLDHDRL(0,R2),LISTAR                                             
         BE    LSTMON08                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLDHDRL(0,R2),LISTAR                                             
         OI    FLDOINDD(R2),FOUTTRN                                             
                                                                                
LSTMON08 OC    LLIST,LLIST         TEST HAVE L'LIST LINE                        
         BZ    *+12                                                             
         AH    R2,LLIST            USE IT TO BUMP TO NEXT                       
         B     *+8                                                              
         GOTOR BUMP                ELSE SIMPLY BUMP TO NEXT FIELD               
                                                                                
         MVI   OKNO,15             SET MESSAGE ASSUMING NO SELECT FIELD         
         GOTOR TSTSEL              IF THIS IS SELECT FIELD                      
         BNE   *+12                                                             
         MVI   OKNO,9              SET MESSAGE WITH SELECT PROMPT               
         GOTOR BUMP                AND BUMP PAST IT                             
                                                                                
         TM    GLSTSTAT,CHNGLIST   IF CHANGES ALLOWED ON LIST SCREEN            
         BZ    *+8                                                              
         MVI   OKNO,32             GIVE SPECIAL MESSAGE                         
                                                                                
         ST    R2,ATHISLST         SAVE A(NEXT LIST LINE)                       
         CLC   LISTNUM,NLISTS      IF HAVEN'T DISPLAYED ALL YET                 
         JL    XITY                RETURN TO CALLER WITH CC EQUAL               
         TM    GLSTSTAT,RETEXTRA   IF APPLIC WANTS EXTRA CALL                   
         JO    XITN                RETURN TO CALLER WITH CC NOT EQUAL           
                                                                                
LSTMONX  L     R2,AFRSTREC         SET TO PLACE CURSOR AT FIRST FIELD           
         J     MODEX               AND EXIT TO USER                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE SECURITY                                                   *         
***********************************************************************         
                                                                                
VALSEC$  MVI   ELCODE,X'F3'                                                     
         GOTOR REMELEM                                                          
         CLI   FLDILEND(R2),0                                                   
         JE    XIT                                                              
         XC    ELEM,ELEM                                                        
         GOTOR SCANNER,DMCB,(R2),(3,BLOCK),0                                    
         LA    R6,ELEM                                                          
         USING SECURED,R6                                                       
         MVI   ERROR,INVALID                                                    
         LA    R3,BLOCK                                                         
         SR    R4,R4                                                            
         ICM   R4,1,4(R1)                                                       
         JZ    ERRXIT                                                           
                                                                                
VALSEC02 CLC   12(#IDLQ,R3),LP@ID  STAMP WITH ID                                
         BNE   VALSEC04                                                         
         MVC   SECURID,TWAORIG                                                  
         B     VALSEC08                                                         
                                                                                
VALSEC04 CLC   12(#MELQ,R3),LP@ME  PASSWORD PROTECTION                          
         BE    VALSEC06                                                         
         CLC   12(#PASSLQ,R3),LP@PASS                                           
         BE    VALSEC06                                                         
         OC    4(4,R3),4(R3)       ELSE NUMERIC LEVEL                           
         JZ    ERRXIT                                                           
         MVC   SECURLEV,7(R3)                                                   
         B     VALSEC08                                                         
                                                                                
VALSEC06 GOTOR GETFACT,DMCB,0      FROM GETFACT                                 
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   SECURPAS,FAPASSWD   PICK UP PASSWORD                             
         MVC   SECURFLG,FATFLAG    AND FLAG                                     
                                                                                
VALSEC08 AHI   R3,32                                                            
         BCT   R4,VALSEC02                                                      
         MVI   SECUREL,X'F3'                                                    
         MVI   SECURLEN,12                                                      
         GOTOR ADDELEM                                                          
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SECURITY DISPLAY AND TEST                                           *         
***********************************************************************         
                                                                                
DSPSEC$  MVC   BLOCK(60),SPACES    DISPLAY                                      
         L     R6,AIO                                                           
         SR    R4,R4               NUMBER OF FIELDS                             
         MVI   ELCODE,X'F3'                                                     
         GOTOR GETEL                                                            
         BNE   DSPSEC02                                                         
         USING SECURED,R6                                                       
         LA    R3,BLOCK                                                         
                                                                                
         OC    SECURID,SECURID     ID                                           
         BZ    *+18                                                             
         MVC   0(#IDLQ,R3),LP@ID                                                
         AHI   R3,20                                                            
         AHI   R4,1                                                             
                                                                                
         OC    SECURPAS,SECURPAS   PASSWORD                                     
         BZ    *+18                                                             
         MVC   0(#PASSWLQ,R3),LP@PASSW                                          
         AHI   R3,20                                                            
         AHI   R4,1                                                             
                                                                                
         CLI   SECURLEV,0          LEVEL                                        
         BZ    DSPSEC02                                                         
         EDIT  (1,SECURLEV),(3,(R3)),ALIGN=LEFT                                 
         AHI   R4,1                                                             
                                                                                
DSPSEC02 GOTOR UNSCAN,DMCB,((R4),BLOCK),(R2),0                                  
         OI    FLDOINDD(R2),FOUTTRN                                             
         J     XIT                                                              
                                                                                
TSTSEC$  L     R6,AIO              TEST SECURITY                                
         MVI   ELCODE,X'F3'                                                     
         GOTOR GETEL                                                            
         BNE   TSTSECY                                                          
         USING SECURED,R6                                                       
         OC    SECURID,SECURID     ID STAMPED                                   
         BZ    TSTSEC02                                                         
         CLC   SECURID,TWAORIG                                                  
         BNE   TSTSECN                                                          
                                                                                
TSTSEC02 OC    SECURPAS,SECURPAS   PASSWORD STAMPED                             
         BZ    TSTSEC04                                                         
         GOTOR GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLC   SECURPAS,FAPASSWD                                                
         BNE   TSTSECN                                                          
                                                                                
TSTSEC04 CLI   SECURLEV,0                                                       
         BE    TSTSECY                                                          
         CLC   SECURLEV,TWAACCS+2                                               
         BL    TSTSECN                                                          
         CLC   SECURLEV,TWAACCS+3                                               
         BH    TSTSECN                                                          
                                                                                
TSTSECY  J     XITY                                                             
                                                                                
TSTSECN  MVI   ERROR,SECLOCK                                                    
         J     XITN                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALIZE SPOOL AND PRINT QUEUE                                    *         
***********************************************************************         
                                                                                
OPNPQ$   MVI   PQSW,2                                                           
         CLI   REMUSER+2,C' '      PAD USER                                     
         BNE   *+8                                                              
         MVI   REMUSER+2,C'*'                                                   
         CLI   REMUSER+1,C' '                                                   
         BNE   *+8                                                              
         MVI   REMUSER+1,C'*'                                                   
         CLI   REMUSER,C' '                                                     
         BNE   *+8                                                              
         MVI   REMUSER,C'*'                                                     
         MVC   SPOOLID,REMUSER                                                  
         TM    GENSTAT3,NOCLRSPK   USER HAS INITIALIZED SPOOLKEY?               
         BO    *+10                                                             
         XC    SPOOLKEY,SPOOLKEY                                                
         LA    R2,SPOOLKEY         KEY FOR PRINT Q                              
         USING PQPLD,R2                                                         
*&&US*&& CLC   =C'MLR',TWAOUT      SPECIAL FOR MAILERS                          
*&&US*&& BNE   *+10                                                             
*&&US*&& MVC   PLDESC(6),=C'MAILER'                                             
*&&US*&& CLC   PLDESC(4),=C'TRSB'  SPECIAL FOR LABELS                           
*&&US*&& BNE   *+12                                                             
*&&US*&& MVI   PLLPP,X'80'         TO SUPPRESS PAGING                           
*&&US*&& OI    SPOOLIND,X'40'      AND INDICATE PARMS PRESENT                   
         MVC   USERLANG,LANG                                                    
         MVC   PLSUBID,SPOOLID                                                  
         MVC   PLUSER,TWADEST      OPTIONAL DESTINATION                         
         OC    PLUSER,PLUSER                                                    
         BNZ   *+10                                                             
         MVC   PLUSER,TWAORIG                                                   
         MVC   SPOOLDM,DATAMGR                                                  
         MVC   RCDATCON,DATCON                                                  
         MVC   RCCOMFAC,ACOMFACS                                                
         MVC   SPOOLBUF,ATIA                                                    
         MVI   SPMODE,0            INITIALIZE SPOOL DSECT                       
         TM    GENSTAT1,MLTPQOK    TEST MULTIPLE PQ INTERVALS ALLOWED           
         BZ    *+8                 NO                                           
         MVI   SPMODE,X'80'                                                     
         MVC   VPRINT,TWAVPRNT                                                  
         MVC   ABOX,TWAVBOX                                                     
         GOTOR SPOOL,PARAS,SPOOLD                                               
         MVC   SPOOLRPN,PLREPNO                                                 
         MVC   SPCONSYS,CONNSYS    PASS CONNECT SYSTEM TO SPOOL                 
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* TEST APPLICATION IS ABOUT TO EXCEED MAX IO COUNT                    *         
***********************************************************************         
                                                                                
CATCHIO$ CLI   OFFLINE,YESQ        DON'T BOTHER IF OFFLINE                      
         JE    XIT                                                              
         GOTOR GETFACT,DMCB,0      GET A(SYSTEM INFO BLOCK)                     
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         ICM   R3,3,FATMAXIO       MAXIMUM ALLOWABLE IOS                        
         MHI   R3,9                                                             
         LHI   R0,10                                                            
         DR    R2,R0               90 PERCENT OF MAX IOS IN R3                  
         CLM   R3,3,FATIOCNT       TEST RUNNING OUT OF IOS                      
         JH    XIT                 NO - STILL WITHIN 90 PERCENT                 
         MVI   ERROR,NOTONLIN      JOB CANNOT BE RUN ONLINE                     
         TM    GENSTAT1,CATCHIOR   TEST USER WANTS TO REGAIN CONTROL            
         JO    XIT                 YES                                          
         J     ERRXIT                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR HANDLING                                                      *         
***********************************************************************         
                                                                                
ERREX$   L     R4,AERRAREA         ERRORS EXIT HERE                             
         CLI   ERROPT,YESQ         OPTION TO GO BACK TO USER                    
         MVI   ERROPT,0            SWITCHING OFF TO SHOW ERROR                  
         JE    XIT                                                              
                                                                                
         TM    GENSTAT2,USGETTXT   TEST GENERATE A GETTXT CALL                  
         BO    ERREX04                                                          
         SR    R0,R0                                                            
         IC    R0,GETMSYS          MESSAGES UNDER THIS SYSTEM                   
         CLI   ERROR,60            UNLESS ITS A GENNEW MESSAGE                  
         BH    ERREX02                                                          
         TM    GENSTAT2,USMYERSY   OR USER OVERRIDES                            
         BO    ERREX02                                                          
         LHI   R0,255              GENNEW MESSAGES ON SYS0 (GENERAL)            
                                                                                
ERREX02  GOTOR GETMSG,PARAS,(ERROR,8(R4)),('FF',DMCB),((R0),DATAMGR)            
         B     ERREX08$                                                         
                                                                                
ERREX04  CLI   OFFLINE,YESQ        ENSURE A(OUTPUT) SET OFFLINE                 
         BNE   ERREX06                                                          
         LA    R1,GETTXTCB+(GTAOUT-GETTXTD)                                     
         OC    0(L'GTAOUT,R1),0(R1)                                             
         BNZ   ERREX06                                                          
         LA    RF,CONHEADH                                                      
         STCM  RF,7,0(R1)                                                       
                                                                                
ERREX06  TM    GENSTAT6,GES$LINK   TEST RUNNING DDLINK UPLOAD                   
         JZ    *+12                                                             
         NI    GETTXTCB+(GT1INDS-GETTXTD),FF-(GT1NOREF)                         
         OI    GETTXTCB+(GT1INDS-GETTXTD),GT1REF                                
         GOTOR GETTXT,GETTXTCB     CONTROL BLOCK MUST BE DEFINED                
                                                                                
ERREX08$ CLI   ACTNUM,ACTCHA       IS THIS A CHANGE?                            
         BNE   ERREX10             NO - SKIP                                    
         CLI   TWALACT,ACTSEL      DID CHANGE FOLLOW SELECT                     
         BNE   ERREX10             NO - SKIP                                    
         MVI   ACTNUM,ACTSEL       PRETEND THIS IS SELECT FOR NEXT TRY          
                                                                                
ERREX10  CLI   PQSW,2              TEST PQ HAS BEEN OPENED                      
         BE    ERREX12             YES                                          
         CLI   OFFLINE,YESQ        TEST OFFLINE                                 
         JNE   EXIT                NO - EXIT                                    
         GOTOR OPENPQ                                                           
                                                                                
ERREX12  CLI   GCMODE,GCMDPRPQ     TEST DELETE PARTIAL REPORT                   
         BNE   ERREX14                                                          
         MVI   SPMODE,X'FE'        DELETE PARTIALLY GENERATED REPORT            
         GOTOR SPOOL,PARAS,SPOOLD  WRAP UP PRINT CONTROL INTERVAL               
         GOTOR OPENPQ              REOPEN PQ FOR ERROR PRINTING                 
                                                                                
ERREX14  OC    VPRINT,VPRINT                                                    
         BZ    ERREX16                                                          
         ICM   R0,B'1111',ABOX                                                  
         BZ    *+8                                                              
         ICM   R0,B'1000',PASSABOX                                              
         GOTOR REQTWA,DMCB,(3,TWAD),('FF',ACOMFACS),VPRINT,(R0)                 
                                                                                
         L     R0,ACURFORC         FORCE CURSOR TO THIS ADDRESS?                
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         LR    R2,R0                                                            
         LTR   R2,R2                                                            
         BNZ   *+8                                                              
         L     R2,EFHREC                                                        
                                                                                
         MVCDD P(#CURADLQ),GE#CURAD                                             
         LH    R0,2(R2)            FIELD ADDRESS                                
         SRDL  R0,32                                                            
         LHI   RE,80                                                            
         DR    R0,RE                                                            
         AHI   R0,1                R0 = COL NUMBER  (01-80)                     
         AHI   R1,1                R1 = ROW NUMBER  (01-24)                     
         CVD   R1,DUB              MOVE CURSOR ROW NUMBER                       
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVI   P+#CURADLQ+1,C'('                                                
         MVI   P+#CURADLQ+4,C','                                                
         MVI   P+#CURADLQ+7,C')'                                                
         MVC   P+#CURADLQ+2(2),DUB+1                                            
         CVD   R0,DUB              MOVE CURSOR COLUMN NUMBER                    
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   P+#CURADLQ+5(2),DUB+1                                            
         GOTOR VPRINT,DMCB,SPACES-1,BL01                                        
         GOTOR VPRINT,DMCB,P-1,BL01                                             
         MVC   P,SPACES                                                         
                                                                                
ERREX16  MVI   SPMODE,FF                                                        
         GOTOR SPOOL,PARAS,SPOOLD  WRAP UP PRINT CONTROL INTERVAL               
                                                                                
         TM    GENSTAT3,DIEONERR   IF REQUIRED TO DIE ON ERRORS                 
         BZ    ERREXX                                                           
         CLI   OFFLINE,YESQ        INSURE WE'RE OFFLINE                         
         BNE   ERREXX                                                           
         DC    H'0'                NOW DIE (SO THAT WE GET DUMP)                
                                                                                
ERREXX   J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INTERFACE TO RFPIO                                                  *         
***********************************************************************         
                                                                                
RFP$     CLI   OFFLINE,YESQ        SKIP IF OFFLINE                              
         JE    XIT                                                              
         L     R3,0(R1)                                                         
         USING QRFPD,R3                                                         
         L     R4,ARFPBLK          ESTABLISH RFP CONTROL BLOCK                  
         USING RFPBLK,R4                                                        
         OC    ARFPIO,ARFPIO       TEST RFPIO ADDRESS RESOLVED                  
         BNZ   RFP02                                                            
         LHI   R0,QRFPIO                                                        
         ICM   R0,B'1110',T00A                                                  
         GOTOR CALLOV,DMCB,0,(R0),0                                             
         OC    ARFPIO,0(R1)        SET ADDRESS OF RFPIO IN GEND                 
         BNZ   RFP02                                                            
         DC    H'0'                                                             
                                                                                
RFP02    LHI   R0,RFPTABN          LOCATE ENTRY IN RFP MODE TABLE               
         LA    R1,RFPTAB                                                        
         USING RFPTABD,R1                                                       
         CLC   QRFPMODE,RFPTMODE                                                
         BE    *+14                                                             
         AHI   R1,RFPTABL                                                       
         BCT   R0,*-14                                                          
         DC    H'0'                INVALID MODE                                 
         SR    RE,RE                                                            
         ICM   RE,3,RFPTADDR                                                    
         A     RE,BASERB                                                        
         BR    RE                  BRANCH TO ROUTINE                            
         DROP  R1,R3,R4,RB                                                      
         EJECT                                                                  
***********************************************************************         
* INITIALIZE GEND AND TEST FOR UPLOAD METHOD IF REQUIRED              *         
***********************************************************************         
                                                                                
GENINI   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         TM    GENSTAT6,GES$INIT   TEST ALREADY INITIALIZED                     
         BNZ   GENINIX                                                          
         OI    GENSTAT6,GES$INIT                                                
                                                                                
         MVC   DUB-8(8),=C'**DUB***'                                            
         MVC   ADDAY-8(8),=C'*EXTRNS*'                                          
         MVC   BOOKVAL-8(8),=C'*CORERES'                                        
         MVC   DATADISP-8(8),=C'*SYSCON*'                                       
         MVC   KEY-8(8),=C'**KEYS**'                                            
                                                                                
         LA    R2,IO-8             SET UP IO AREAS                              
         LA    R3,AIO1                                                          
         LHI   R4,X'F1'                                                         
         SR    R0,R0                                                            
         ICM   R0,1,MAXIOS                                                      
         BNZ   GENINI04                                                         
         LHI   R0,1                                                             
                                                                                
GENINI04 MVC   0(8,R2),=C'**I/O1**'                                             
         STC   R4,5(R2)                                                         
         AHI   R2,8                                                             
         ST    R2,0(R3)                                                         
         A     R2,SIZEIO                                                        
         AHI   R3,4                                                             
         AHI   R4,1                                                             
         BCT   R0,GENINI04                                                      
         MVC   AIO,AIO1            DEFAULT IS IO AREA 1                         
                                                                                
         MVC   0(8,R2),=C'**PAD***'                                             
         AHI   R2,8                                                             
         ST    R2,ASYSD                                                         
         LA    R1,SPOOLD           COMPUTE SPACE AVAILABLE                      
         A     R1,LWORK                                                         
         SR    R1,R2                                                            
         ST    R1,LSYSD                                                         
                                                                                
         L     R1,SYSPARMS                                                      
         L     RA,4(R1)                                                         
         CLI   SYSTEM,CTLLETQ      IF CONTROL SYSTEM                            
         BNE   *+8                                                              
         L     RA,20(R1)                                                        
         ST    RA,ATWA                                                          
         MVC   AGENCY,TWAAGY                                                    
         MVI   OFFLINE,NOQ                                                      
         OC    TWAVPRNT,TWAVPRNT                                                
         BZ    GENINI06                                                         
         MVI   OFFLINE,YESQ                                                     
         L     RF,TWADCONS                                                      
         TM    TWACFLAG,TWACFIDF   TEST NEW VERSION OF SPOOF                    
         BZ    *+10                                                             
         MVC   VADUMMY,TDUMMY-TWADCOND(RF)                                      
         MVC   VPRINT,TWAVPRNT     OFF-LINE ADDRESSES IN TWA                    
         MVC   ABOX,TWAVBOX                                                     
         MVC   BUFFALO,TWAVBUFF                                                 
         MVC   SORTER,TWAVSORT                                                  
         MVC   WORKER,TWAVWORK                                                  
                                                                                
GENINI06 LA    R2,CONHEADH                                                      
         ST    R2,AERRAREA                                                      
         LHI   R2,REPINFO-TWATASK                                               
         LA    R2,TWAD(R2)           SET UP A(REPORT ID INFO)                   
         ST    R2,AREPINFO                                                      
                                                                                
         TM    GENSTAT1,NOSETEFH     DON'T BOTHER - APPLIC DID IT               
         BO    GENINI10                                                         
         LA    R2,CONSERV+L'CONSERV  SET UP ADCONS FOR ACCESS                   
         LA    R3,EFHREC             TO FIELDS WITH EXTENDED                    
         LHI   R4,4                  FIELD HEADERS                              
         LA    R5,EFHOTH                                                        
GENINI08 SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         ST    R2,0(R3)                                                         
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BXLE  R3,R4,GENINI08                                                   
         ST    R2,EFHTAG                                                        
                                                                                
GENINI10 L     R1,SYSPARMS                                                      
         LM    R2,R4,8(R1)         A(SYSLIST) A(TIA) A(COMFACS)                 
         CLI   SYSTEM,CTLLETQ      IF CONTROL SYSTEM                            
         BNE   *+12                                                             
         LM    R2,R3,0(R1)         SYSPARMS IS DIFFERENT                        
         L     R4,12(R1)                                                        
         ST    R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         ST    R3,ATIA                                                          
         ST    R4,RCCOMFAC                                                      
         MVC   ADDAY,CADDAY        EXTRACT COMFACS ADDRESSES                    
         MVC   SWITCH,CSWITCH                                                   
         MVC   CALLOV,CCALLOV                                                   
         MVC   CASHVAL,CCASHVAL                                                 
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   DATCON,CDATCON                                                   
         MVC   RCDATCON,DATCON                                                  
         MVC   DATVAL,CDATVAL                                                   
         MVC   GETDAY,CGETDAY                                                   
         MVC   GETFACT,CGETFACT                                                 
         MVC   GETMSG,CGETMSG                                                   
         MVC   GETTXT,CGETTXT                                                   
         MVC   GETPROF,CGETPROF                                                 
         MVC   HELLO,CHELLO                                                     
         MVC   HEXIN,CHEXIN                                                     
         MVC   HEXOUT,CHEXOUT                                                   
         MVC   SCANNER,CSCANNER                                                 
         MVC   SCUNKEY,CSCUNKEY                                                 
         MVC   UNSCAN,CUNSCAN                                                   
         MVC   REQTWA,CREQTWA                                                   
         MVC   PERVAL,CPERVAL                                                   
         MVC   DICTATE,CDICTATE                                                 
         MVC   CUREDIT,CCUREDIT                                                 
         MVC   PERVERT,CPERVERT                                                 
         MVC   SECRET,CSECRET                                                   
         MVC   PROTON,CPROTON                                                   
         MVC   PROTOFF,CPROTOFF                                                 
         DROP  R4                                                               
                                                                                
         LA    R2,BOOKVAL          SET A(CORE RESIDENT PHASES)                  
         LA    R3,CRESLIST                                                      
         LHI   R4,CRESLISL                                                      
         ICM   R0,B'1110',T00A                                                  
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
GENINI12 XC    0(4,R2),0(R2)                                                    
         CLI   0(R3),FF                                                         
         BE    GENINI14                                                         
         ICM   R0,B'0001',0(R3)                                                 
         GOTOR (RF),(R1),0,(R0),0                                               
         MVC   0(4,R2),0(R1)                                                    
GENINI14 AHI   R2,4                                                             
         AHI   R3,L'CRESLIST                                                    
         BCT   R4,GENINI12                                                      
                                                                                
         BASR  RE,0                                                             
         AHI   RE,ROUTS-*                                                       
         SR    RF,RF                                                            
         LA    R1,ANY                                                           
         LHI   R0,ROUT1#           SET ADDRESSES OF CALLABLE ROUTINES           
GENINI16 ST    RE,0(R1)            A(COMMON)                                    
         STC   RF,0(R1)            ROUTINE NUMBER                               
         AHI   RF,4                                                             
         AHI   R1,4                                                             
         BCT   R0,GENINI16                                                      
                                                                                
         LHI   R0,ROUT2#                                                        
         LA    R1,RFP                                                           
GENINI18 ST    RE,0(R1)            A(COMMON)                                    
         STC   RF,0(R1)            ROUTINE NUMBER                               
         AHI   RF,4                                                             
         AHI   R1,4                                                             
         BCT   R0,GENINI18                                                      
                                                                                
         MVI   DMINBTS,X'C0'       PRESET VALUES                                
         TM    GENSTAT1,RDUPAPPL                                                
         BZ    *+8                                                              
         NI    DMINBTS,X'7F'       READ FOR UPDATE CONTROLLED BY APPLIC         
         MVI   DMOUTBTS,X'7D'                                                   
         MVI   NLISTS,15                                                        
         CLI   LRECACT,0           L'RECACT TABLE ENTRIES                       
         BNE   *+8                                                              
         MVI   LRECACT,12          DEFAULT IS 12                                
         GOTOR GETFACT,DMCB,0                                                   
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         MVC   SVSYS,FASYS         SAVE SE NUMBER                               
         MVC   LANG,FALANG         .....LANGUAGE CODE                           
         MVC   CTRY,FACTRY         .....COUNTRY CODE                            
         DROP  R1                                                               
         LA    R1,TWAKEYSV         MAKE TWAKEYSV ADDRESSABLE                    
         ST    R1,ATWAKYSV                                                      
         GOTOR DATCON,DMCB,(5,0),(10,RCDATE)                                    
         ICM   R1,15,TWAMASTC      IF RUNNING OFFLINE                           
         BZ    GENINI20                                                         
         USING MASTD,R1                                                         
         CLC   MCDATE,SPACES       AND DATE CARD WAS PROCESSED                  
         BE    GENINI20                                                         
         MVC   RCDATE,MCDATE       USE THIS DATE                                
                                                                                
GENINI20 GOTOR DATCON,DMCB,(4,RCDATE),(3,BTODAY)                                
                                                                                
         GOTOR DICTATE,DMCB,C'LU  ',DICUPR,LPUCASE                              
         GOTOR (RF),(R1),C'LL  ',DICMIX,LPMCASE                                 
                                                                                
         CLI   OFFLINE,YESQ        TEST OFFLINE                                 
         BE    GENINI22            YES                                          
         L     R1,SYSPARMS                                                      
         L     RF,0(R1)            A(TIOB)                                      
         CLI   SYSTEM,CTLLETQ      TEST CONTROL SYSTEM                          
         BNE   *+8                 NO                                           
         L     RF,28(R1)           SYSPARMS IS DIFFERENT                        
         USING TIOBD,RF                                                         
         MVC   PFAID,TIOBAID       AID BYTE (0=ENTER, 1-24=PFKEY#)              
         DROP  RF                                                               
                                                                                
GENINI22 CLI   GCMODE,GCMSLAVQ     EXIT NOW IF SLAVED MODE                      
         BE    GENINI24                                                         
                                                                                
         LHI   R0,QDICTCON                                                      
         ICM   R0,B'1110',T00A                                                  
         GOTOR CALLOV,DMCB,0,(R0)                                               
         L     RF,DMCB                                                          
         GOTOR (RF),DMCB,GEND      GO TO INITIALIZE DATA DICT. CONTROL          
                                                                                
GENINI24 CLI   OFFLINE,YESQ        TEST OFFLINE                                 
         BE    GENINIX             YES                                          
                                                                                
         TM    GENSTAT6,GES$UPLD   TEST AUTO UPLOAD SUPPORTED                   
         BNZ   *+12                                                             
         CLI   MODE,TESTGLOB       OR CALLED TO TEST GLOBALS                    
         BNE   GENINIX                                                          
                                                                                
         L     R0,ATIA             CLEAR THE TIA                                
         SR    R1,R1                                                            
         ICM   R1,3,LIOBTLEN       FOR THE LENGTH OF THE LIOB                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         NI    GENSTAT6,FF-(GES$SAVE)                                           
                                                                                
         L     R2,ATIA                                                          
         USING LIOBD,R2            R2=A(LIOB)                                   
         ST    R2,ALIOB            SET A(LIOB) FOR CALLER                       
         LA    R0,LIOBD                                                         
         AHI   R0,L'LIOB                                                        
         ST    R0,LIOBAREC                                                      
         AHI   R0,4*ONEK-L'LIOB                                                 
         ST    R0,LIOBABUF                                                      
         LA    R0,LINKMAP                                                       
         ST    R0,LIOBAMAP                                                      
         LA    R0,GEND                                                          
         ST    R0,LIOBASB1                                                      
         MVC   LIOBACOM,ACOMFACS                                                
         MVI   LIOBMSYS,FF                                                      
         MVI   LIOBINDS,LIOBISUB                                                
         L     RF,ACOMFACS                                                      
         L     RF,CLINKIO-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('LIOAINI',LIOBD)                                      
         JNE   GENINI26                                                         
         OI    GENSTAT6,GES$LINK   SET DDLINK UPLOAD                            
         TM    GENSTAT6,GES$UPLD   TEST USING AUTO UPLOAD                       
         BZ    GENINI26                                                         
         MVI   MODE,0              YES - ASSUME CONTROL NOW                     
         L     R1,EFHREC           PUT SOMETHING INTO RECORD FIELD              
         MVI   FLDILEND(R1),L'UPLOAD                                            
         MVC   FLDHDRL(L'UPLOAD,R1),UPLOAD                                      
         B     GENINIX             AND EXIT WITH CC EQUAL TO PROCESS            
                                                                                
GENINI26 CLI   MODE,TESTGLOB       TEST CALLED TO TEST GLOBALS                  
         BNE   GENINIX                                                          
         MVI   MODE,0              RESET MODE FOR NEXT TIME CALL                
         CLI   0(R1),LIOCNOXC      TEST TRANSFER CONTROL GLOBAL FOUND           
         BE    GENINIX                                                          
         OI    GENSTAT6,GES$GLOB   YES - SET FLAG FOR CALLER                    
                                                                                
GENINIX  J     XITY                                                             
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* GET NEXT INPUT RECORD VIA LINKIO AND SET RECORD/ACTION AND OTHER    *         
* HEADER FIELDS                                                       *         
***********************************************************************         
                                                                                
GETHDR   NTR1  LABEL=NO                                                         
         GOTOR RESLIO              RESTORE LIOB IF SAVED                        
         L     R2,ALIOB                                                         
         USING LIOBD,R2            R2=A(LIOB)                                   
                                                                                
         L     RF,ACOMFACS         CALL LINKIO TO GET NEXT RECORD               
         L     RF,CLINKIO-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('LIOAGET',LIOBD)                                      
         JNE   GETHDR10            EXIT IF NOT GOOD COMPLETION CODE             
         TM    LIOBFLG2,LIOBFEOR   MUST NOT BE END OF RECORD                    
         JZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,ARECACT          POINT TO RECORD/ACTION TABLE                 
         SR    R0,R0                                                            
         IC    R0,LRECACT                                                       
GETHDR02 CLI   0(R1),FF            TEST END OF RECORD/ACTION TABLE              
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),1             TEST RECORD TYPE ENTRY                       
         JNE   *+14                                                             
         CLC   LINK$REC,9(R1)      MATCH RECORD NUMBER TO TABLE                 
         JE    *+10                                                             
         AR    R1,R0                                                            
         J     GETHDR02                                                         
                                                                                
         L     RF,EFHREC                                                        
         USING FLDHDRD,RF                                                       
         MVC   FLDDATA(8),1(R1)    SET RECORD NAME                              
         MVI   FLDILEN,8           LENGTH                                       
         MVI   FLDIIND,FINPTHIS    AND INPUT INDICATOR                          
                                                                                
         L     R1,ARECACT          POINT TO RECORD/ACTION TABLE                 
GETHDR04 CLI   0(R1),FF            TEST END OF RECORD/ACTION TABLE              
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),2             TEST ACTION TYPE ENTRY                       
         JNE   *+14                                                             
         CLC   LINK$ACT,9(R1)      MATCH ACTION NUMBER TO TABLE                 
         JE    *+10                                                             
         AR    R1,R0                                                            
         J     GETHDR04                                                         
                                                                                
         L     RF,EFHACT                                                        
         MVC   FLDDATA(8),1(R1)    SET ACTION NAME                              
         MVI   FLDILEN,8           LENGTH                                       
         MVI   FLDIIND,FINPTHIS    AND INPUT INDICATOR                          
                                                                                
         OC    LINK$DTA,LINK$DTA   MUST HAVE SOME UPLOAD DATA PRESENT           
         JNZ   GETHDR08                                                         
         DC    H'0'                                                             
GETHDR06 L     RF,ACOMFACS         GET NEXT DATA FIELD                          
         L     RF,CLINKIO-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('LIOAGET',LIOBD)                                      
         JE    GETHDR08                                                         
         DC    H'0'                                                             
                                                                                
GETHDR08 GOTOR LOCFLD              LOCATE TWA FIELD FOR THIS DATA               
         JZ    XITE                EXIT IF CAN'T FIND A FIELD                   
         C     R1,EFHTAG                                                        
         JNL   XITE                EXIT IF NOT A HEADER FIELD                   
         GOTOR SETDTA,(R1)         SET FIELD DATA                               
         GOTOR SETHDR,(R1)         SET FIELD HEADER                             
         J     GETHDR06            LOOK FOR MORE HEADER FIELDS                  
                                                                                
GETHDR10 CLI   0(R1),LIOCDONE      TEST ALL RECORDS PROCESSED                   
         JE    *+6                                                              
         DC    H'0'                WHAT ELSE COULD IT BE?                       
         OI    GENSTAT6,GES$EOIF   SET END OF INPUT FILE                        
         J     XITH                EXIT WITH CC=HIGH                            
         DROP  R2,RF                                                            
         EJECT                                                                  
***********************************************************************         
* ESTABLISH RECORD KEY FIELDS IN DDLINK UPLOAD MODE                   *         
***********************************************************************         
                                                                                
GETKEY   NTR1  LABEL=NO                                                         
         L     R2,ALIOB                                                         
         USING LIOBD,R2            R2=A(LINKIO CONTROL BLOCK)                   
         MVI   LINK$KEY,0          SET NO KEY FIELDS RESOLVED                   
         J     GETKEY04                                                         
                                                                                
GETKEY02 L     RF,ACOMFACS                                                      
         L     RF,CLINKIO-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('LIOAGET',LIOBD)                                      
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    LIOBFLG2,LIOBFEOR   EXIT IF END OF RECORD                        
         JZ    GETKEY04                                                         
         OI    GENSTAT6,GES$EOIR   SET END OF INPUT RECORD DETECTED             
         J     GETKEY06                                                         
                                                                                
GETKEY04 OC    LINK$DTA,LINK$DTA   MUST HAVE SOME DATA PRESENT                  
         JNZ   GETKEY06                                                         
         DC    H'0'                                                             
                                                                                
GETKEY06 GOTOR LOCFLD              LOCATE TWA FIELD FOR THIS DATA               
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLM   R1,15,AFRSTKEY      TEST BEFORE FIRST KEY FIELD                  
         JNL   *+6                                                              
         DC    H'0'                                                             
         CLM   R1,15,AFRSTREC      TEST KEY FIELD                               
         JNL   GETKEY08                                                         
         GOTOR SETDTA,(R1)         SET FIELD DATA                               
         GOTOR SETHDR,(R1)         SET FIELD HEADER                             
         SR    R1,R1                                                            
         IC    R1,LINK$KEY         BUMP NUMBER OF KEY FIELDS RESOLVED           
         AHI   R1,1                                                             
         STC   R1,LINK$KEY                                                      
         TM    LIOBFLG2,LIOBFEOR   TEST END OF RECORD ENCOUNTERED               
         JZ    GETKEY02                                                         
                                                                                
GETKEY08 CLI   LINK$KEY,0          TEST ANY KEY FIELDS RESOLVED                 
         JNE   XITY                                                             
         DC    H'0'                RECORD KEY NOT PROVIDED                      
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ESTABLISH RECORD DATA FIELDS IN DDLINK UPLOAD MODE                  *         
***********************************************************************         
                                                                                
GETDTA   NTR1  LABEL=NO                                                         
         L     R2,ALIOB                                                         
         USING LIOBD,R2            R2=A(LINKIO CONTROL BLOCK)                   
         MVI   LINK$DAT,0          SET NO DATA FIELDS RESOLVED                  
         J     GETDTA04                                                         
                                                                                
GETDTA02 L     RF,ACOMFACS                                                      
         L     RF,CLINKIO-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('LIOAGET',LIOBD)                                      
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    LIOBFLG2,LIOBFSBK   TEST SUB-RECORD BREAK SET                    
         JZ    GETDTA06                                                         
                                                                                
GETDTA04 OC    LINK$DTA,LINK$DTA   MUST HAVE DATA PRESENT                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         OC    LINK$LOC,LINK$LOC   SPECIAL LOCATION "0,0" ?                     
         JNZ   *+12                YES: CONTINUE PUTTING DATA IN TWA            
         OI    LIOBINDS,LIOBINXT   FORCE END OF RECORD                          
         J     GETDTA10                                                         
                                                                                
         GOTOR LOCFLD              LOCATE TWA FIELD FOR THIS DATA               
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLM   R1,15,AFRSTREC      TEST DATA FIELD                              
         JNL   *+6                                                              
         DC    H'0'                                                             
         GOTOR SETDTA,(R1)         SET FIELD DATA                               
         SR    R1,R1                                                            
         IC    R1,LINK$DAT         BUMP NUMBER OF DATA FIELDS RESOLVED          
         AHI   R1,1                                                             
         STC   R1,LINK$DAT                                                      
                                                                                
GETDTA06 TM    LIOBFLG2,LIOBFEOR   TEST END OF RECORD ENCOUNTERED               
         JZ    GETDTA02            NO - GET NEXT PIECE OF DATA                  
                                                                                
GETDTA08 CLI   LINK$DAT,0          TEST ANY KEY FIELDS RESOLVED                 
         JNE   *+6                                                              
         DC    H'0'                RECORD DATA NOT PROVIDED                     
                                                                                
GETDTA10 GOTOR SETHDR,0            SET ALL TWA FIELD HEADER VALUES              
         J     XITY                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SEND MESSAGE TEXT TO DDLINK FOR DOWNLOADING                         *         
***********************************************************************         
                                                                                
PUTMSG   NTR1  LABEL=NO                                                         
         GOTOR RESLIO              RESTORE LIOB IF SAVED                        
         L     R2,ALIOB                                                         
         USING LIOBD,R2            R2=A(LINKIO CONTROL BLOCK)                   
         L     RF,ACOMFACS                                                      
         L     RF,CLINKIO-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('LIOAPUT',LIOBD),('LIOTRAW',1),              *        
               ('LD_CHARQ',CONHEAD),(L'CONHEAD,0)                               
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOCATE TWA INPUT FIELD FOR LINK$FLD/LINK$REL                        *         
*                                                                     *         
* NOTE:  IF LINK$FLD IS SET THE FIRST OCCURRENCE OF THAT FIELD WILL   *         
*        BE LOCATED IN THE TWA - IF LINK$REL IS ALSO SET IT MEANS THE *         
*        NTH OCCURRENCE OF LINK$FLD IN THE TWA ELSE THE FIRST IS USED *         
*                                                                     *         
*        IF LINK$REL IS ONLY SET IT MEANS THE NTH UNPROTECTED FIELD   *         
*        IN THE TWA (WHERE THE SERVICE REQUEST FIELD IS FIELD #1)     *         
*                                                                     *         
* EXIT WITH R1 POINTING TO TWA FIELD AND CC=NOT EQUAL IF THE FIELD    *         
* WAS LOCATED ELSE R1 WILL BE ZERO AND CC=EQUAL                       *         
***********************************************************************         
                                                                                
LOCFLD   STM   RE,R0,12(RD)                                                     
                                                                                
         SR    R1,R1                                                            
         CLI   LINK$FLD,0          TEST EXTENDED FIELD HEADER NUMBER            
         JE    LOCFLD06                                                         
         LA    R1,CONHEADH         YES - FIND FIRST FIELD WITH THAT             
         USING FLDHDRD,R1          EXTENDED FIELD HEADER NUMBER                 
         SR    R0,R0                                                            
LOCFLD02 CLI   FLDLEN,0            TEST END OF TWA                              
         JNE   *+10                                                             
         SR    R1,R1                                                            
         J     LOCFLDX                                                          
         TM    FLDATB,FATBPROT     TEST PROTECTED                               
         JNZ   LOCFLD04                                                         
         TM    FLDATB,FATBXHDR     TEST EXTENDED FIELD HEADER                   
         JZ    LOCFLD04                                                         
         IC    R0,FLDLEN                                                        
         LA    RF,FLDHDRD                                                       
         AR    RF,R0                                                            
         SHI   RF,FLDHDRL                                                       
         CLC   LINK$FLD,FLDXNUM-FLDDATA(RF)                                     
         JE    LOCFLD06                                                         
LOCFLD04 IC    R0,FLDLEN                                                        
         AR    R1,R0                                                            
         J     LOCFLD02                                                         
                                                                                
LOCFLD06 SR    RE,RE               HANDLE RELATIVE FIELD NUMBER                 
         ICM   RE,1,LINK$REL                                                    
         JZ    LOCFLDX                                                          
         LTR   R1,R1               TEST ALREADY POSITIONED TO FIRST             
         JNZ   *+8                                                              
         LA    R1,CONHEADH                                                      
         SR    R0,R0                                                            
LOCFLD08 CLI   FLDLEN,0            TEST END OF TWA                              
         JNE   *+10                                                             
         SR    R1,R1                                                            
         J     LOCFLDX                                                          
         TM    FLDATB,FATBPROT     TEST PROTECTED                               
         JNZ   LOCFLD12                                                         
         CLI   LINK$FLD,0          TEST RELATIVE TO FIELD NUMBER                
         JE    LOCFLD10                                                         
         IC    R0,FLDLEN           YES - MATCH NUMBER TOO                       
         LA    RF,FLDHDRD                                                       
         AR    RF,R0                                                            
         SHI   RF,FLDHDRL                                                       
         CLC   LINK$FLD,FLDXNUM-FLDDATA(RF)                                     
         JNE   LOCFLD12                                                         
LOCFLD10 BRCT  RE,LOCFLD12         LOOP TO CORRECT TWA FIELD                    
         J     LOCFLDX                                                          
LOCFLD12 IC    R0,FLDLEN           BUMP TO NEXT TWA FIELD                       
         AR    R1,R0                                                            
         J     LOCFLD08                                                         
                                                                                
LOCFLDX  LTR   R1,R1               TEST FIELD LOCATED                           
         LM    RE,R0,12(RD)                                                     
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* MOVE DATA ADDRESSED BY LINK$DTA TO A TWA INPUT FIELD                *         
***********************************************************************         
                                                                                
SETDTA   STM   RE,R2,12(RD)                                                     
                                                                                
         USING FLDHDRD,R1                                                       
         SR    RF,RF               NO - CLEAR TWA FIELD                         
         ICM   RF,1,FLDLEN                                                      
         SHI   RF,FLDHDRL                                                       
         TM    FLDATB,FATBXHDR     TEST EXTENDED FIELD HEADER                   
         JZ    *+8                                                              
         SHI   RF,FLDHDRL                                                       
         SHI   RF,1                                                             
         JNM   *+6                                                              
         DC    H'0'                INVALID TWA FIELD                            
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         XC    FLDDATA(0),FLDDATA  CLEAR TWA INPUT FIELD                        
         LA    R0,1(RF)            R0=MAXIMUM FIELD LENGTH                      
         SR    R2,R2                                                            
         ICM   R2,7,LINK$DTA+1     POINT TO DATA ELEMENT                        
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING LQ_D,R2                                                          
         CLI   LQ_TYPE,LQ_TSINQ    MUST BE A SINGLE VALUE                       
         JE    *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         ICM   RF,3,LQ_LN                                                       
         SHI   RF,LQ_VALUE-LQ_D                                                 
         CR    RF,R0                                                            
         JNH   *+6                                                              
         DC    H'0'                DATA TOO LONG FOR TWA FIELD                  
         STC   RF,FLDILEN                                                       
         SHI   RF,1                                                             
         JM    SETDTAX             EXIT IF NO INPUT DATA                        
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     SETDTAX                                                          
         MVC   FLDDATA(0),LQ_VALUE SET TWA INPUT FIELD DATA                     
                                                                                
SETDTAX  LM    RE,R2,12(RD)                                                     
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* SET FIELD HEADER VALUES OF ONE OR ALL UNPROTECTED FIELDS IN THE     *         
* TWA - ENTER WITH R1 POINTING TO SPECIFIC FIELD HEADER FOR A SINGLE  *         
* FIELD ELSE ZERO FOR ALL TWA FIELD HEADERS TO BE SET                 *         
***********************************************************************         
                                                                                
SETHDR   STM   RE,R2,12(RD)                                                     
                                                                                
         LTR   R2,R1               R2=A(SINGLE FIELD) OR ZERO                   
         JNZ   *+8                                                              
         LA    R1,CONHEADH                                                      
         USING FLDHDRD,R1                                                       
         SR    R0,R0                                                            
SETHDR02 TM    FLDATB,FATBPROT     TEST PROTECTED                               
         JNZ   SETHDR12                                                         
         MVI   FLDIIND,0                                                        
         MVI   FLDILEN,0                                                        
         IC    R0,FLDLEN                                                        
         LR    RF,R0               NO - CLEAR TWA FIELD                         
         SHI   RF,FLDHDRL                                                       
         TM    FLDATB,FATBXHDR     TEST EXTENDED FIELD HEADER                   
         JZ    *+8                                                              
         SHI   RF,FLDHDRL                                                       
         LR    R0,RF                                                            
         LA    RF,FLDDATA-1(RF)                                                 
         CLI   0(RF),C' '          LOCATE END OF INPUT DATA                     
         JH    *+14                                                             
         BCTR  RF,0                                                             
         BRCT  R0,*-10                                                          
         J     SETHDR12            FIELD IS EMPTY                               
                                                                                
         STC   R0,FLDILEN                                                       
         OI    FLDIIND,FINPNUM+FINPALF+FINPHEX+FINPTHIS                         
         TM    FLDILEN,1           NOT HEX IF ODD INPUT LENGTH                  
         JZ    *+8                                                              
         NI    FLDIIND,FF-(FINPHEX)                                             
                                                                                
         LA    RF,FLDDATA          SET FIELD VALIDITY BITS                      
SETHDR04 CLI   0(RF),C'0'                                                       
         JNL   *+8                                                              
         NI    FLDIIND,FF-(FINPNUM)                                             
         TM    FLDATB,FATBLC       TEST LOWER CASE ALLOWED                      
         JZ    SETHDR06                                                         
         CLI   0(RF),C'A'-X'40'                                                 
         JL    SETHDR08                                                         
         CLI   0(RF),C'Z'-X'40'                                                 
         JH    SETHDR06                                                         
         NI    FLDIIND,FF-(FINPNUM+FINPHEX)                                     
         J     SETHDR10                                                         
SETHDR06 CLI   0(RF),C'A'                                                       
         JNL   *+12                                                             
SETHDR08 NI    FLDIIND,FF-(FINPALF+FINPHEX+FINPNUM)                             
         J     SETHDR10                                                         
         CLI   0(RF),C'Z'                                                       
         JNH   *+12                                                             
         NI    FLDIIND,FF-(FINPALF)                                             
         J     SETHDR10                                                         
         NI    FLDIIND,FF-(FINPNUM)                                             
         CLI   0(RF),C'F'                                                       
         JNH   SETHDR10                                                         
         NI    FLDIIND,FF-(FINPHEX)                                             
SETHDR10 TM    FLDIIND,FINPALF+FINPNUM+FINPHEX                                  
         JZ    SETHDR12                                                         
         AHI   RF,1                BUMP TO NEXT INPUT CHARACTER                 
         BRCT  R0,SETHDR04                                                      
                                                                                
SETHDR12 LTR   R2,R2               TEST CALLED FOR SINGLE FIELD                 
         JNZ   SETHDRX                                                          
         IC    R0,FLDLEN           BUMP TO NEXT FIELD                           
         AR    R1,R0                                                            
         CLI   FLDLEN,0            TEST END OF TWA                              
         JNE   SETHDR02                                                         
                                                                                
SETHDRX  LM    RE,R2,12(RD)                                                     
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* SAVE LIOB TO WSSVR BUFFER - TURN ON SAVED INDICATOR                 *         
***********************************************************************         
                                                                                
SAVLIO   TM    GENSTAT6,GES$LINK+GES$SAVE                                       
         BNMR  RE                                                               
         NTR1  LABEL=NO                                                         
         J     SAVLIO02                                                         
                                                                                
SAVLIOB$ TM    GENSTAT6,GES$LINK+GES$SAVE                                       
         JNM   XITY                                                             
                                                                                
         USING FAWSSVRD,BLOCK                                                   
SAVLIO02 XC    FAWSSVRD(FAWSSVRL),FAWSSVRD                                      
         MVC   FAWSTOKN,LKIOTOKN                                                
         MVC   FAWSADR,ALIOB                                                    
         MVC   FAWSLEN,LIOBTLEN                                                 
         MVI   FAWSACTN,FAWSASVE                                                
         L     RF,ACOMFACS                                                      
         L     RF,CWSSVR-COMFACSD(RF)                                           
         GOTOR (RF),FAWSSVRD                                                    
         CLI   FAWSRTN,0                                                        
         JE    *+6                                                              
         DC    H'0'                DIE IF CAN'T SAVE                            
         OI    GENSTAT6,GES$SAVE   SET LIOB VALUES SAVED INDICATOR              
         J     XITY                                                             
                                                                                
***********************************************************************         
* RESTORE LIOB FROM WSSVR BUFFER IF SAVED - TURN OFF SAVED INDICATOR  *         
***********************************************************************         
                                                                                
RESLIO   TM    GENSTAT6,GES$LINK+GES$SAVE                                       
         BNOR  RE                                                               
         NTR1  LABEL=NO                                                         
         J     RESLIO02                                                         
                                                                                
RESLIOB$ TM    GENSTAT6,GES$LINK+GES$SAVE                                       
         JNO   XITY                                                             
                                                                                
RESLIO02 XC    FAWSSVRD(FAWSSVRL),FAWSSVRD                                      
         MVC   FAWSTOKN,LKIOTOKN                                                
         MVC   FAWSADR,ALIOB                                                    
         MVI   FAWSACTN,FAWSARST                                                
         MVC   FAWSLEN,LIOBTLEN                                                 
         L     RF,ACOMFACS                                                      
         L     RF,CWSSVR-COMFACSD(RF)                                           
         GOTOR (RF),FAWSSVRD                                                    
         CLI   FAWSRTN,0                                                        
         JE    *+6                                                              
         DC    H'0'                DIE IF CAN'T RESTORE                         
         MVI   FAWSACTN,FAWSADEL   DELETE THE SAVED BUFFER                      
         GOTOR (RF),(R1)                                                        
         NI    GENSTAT6,FF-(GES$SAVE)                                           
         J     XITY                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE OUTPUT TYPE FIELD                               *         
***********************************************************************         
                                                                                
OUTVAL   NTR1  BASE=*,LABEL=*                                                   
         GOTOR ANY                                                              
         GOTOR CHKRFP              CHECK OUTPUT FOR RFP FILE KEYWORD            
         BNE   *+12                                                             
         OI    GENSTAT7,GES$ORFP   SET GROUP IN DESTINATION FIELD               
         B     OUTVALX                                                          
                                                                                
         ICM   R2,15,EFHDEST                                                    
         BZ    OUTVAL02                                                         
         GOTOR CHKRFP              CHECK DESTINATION FOR RFP KEYWORD            
         BNE   OUTVAL02                                                         
         OI    GENSTAT7,GES$DRFP   SET GROUP IN OUTPUT FIELD                    
                                                                                
OUTVAL02 L     R2,EFHOUT                                                        
         OC    ARFPBLK,ARFPBLK     IF DOING RFP THEN GROUP ID HERE              
         BZ    *+12                                                             
         GOTOR VALGRP              VALIDATE RFP GROUP CODE                      
         B     OUTVALX                                                          
                                                                                
         CLI   WORK,C'0'           TEST STARTS WITH A-I,J-R,S-Z,0-9             
         BL    *+12                                                             
         CLI   WORK,C'9'                                                        
         BNH   OUTVAL06                                                         
         CLI   WORK,C'S'                                                        
         BL    *+12                                                             
         CLI   WORK,C'Z'                                                        
         BNH   OUTVAL06                                                         
         CLI   WORK,C'J'                                                        
         BL    *+12                                                             
         CLI   WORK,C'R'                                                        
         BNH   OUTVAL06                                                         
         CLI   WORK,C'A'                                                        
         BL    *+12                                                             
         CLI   WORK,C'I'                                                        
         BNH   OUTVAL06                                                         
         CLI   WORK,C'+'           TEST OUTPUT TYPE SPECIALS +,&,#              
         BE    OUTVAL06                                                         
         CLI   WORK,C'&&'                                                       
         BE    OUTVAL06                                                         
         CLI   WORK,C'#'                                                        
         BE    OUTVAL06                                                         
                                                                                
OUTVAL04 MVC   TWAOUT,WORK         SAVE SPECIAL CODE IN OUTPUT TYPE             
         CLI   WORK,C'@'           TEST IF SQL TRANSFORM                        
         BE    OUTVAL12                                                         
         CLI   WORK,C'/'           TEST IF REMOTE AFP NAME                      
         BE    OUTVAL10                                                         
         MVI   ERROR,INVOUT                                                     
         J     ERRXIT                                                           
                                                                                
OUTVAL06 LA    R4,KEY              VALIDATE OUTPUT TYPE                         
         CLI   OFFLINE,YESQ                                                     
         BE    OUTVAL10            DONT BOTHER IF OFFLINE                       
         XC    KEY,KEY                                                          
         USING CTOREC,R4                                                        
         MVI   CTOKTYP,CTOKTYPQ                                                 
         MVC   CTOKID(6),WORK                                                   
         MVC   FILENAME,CTFILE                                                  
         MVC   SAVUSEIO,USEIO                                                   
         MVI   USEIO,YESQ                                                       
         GOTOR HIGH                                                             
         MVI   ERROR,INVOUT                                                     
         CLC   KEY(20),KEYSAVE                                                  
         JNE   ERRXIT                                                           
         LA    R4,IO                                                            
         CLI   TWAOFFC,C'*'        UNLESS THIS IS A DDS TERMINAL                
         BE    OUTVAL08                                                         
         CLI   OFFLINE,YESQ                                                     
         BE    OUTVAL08                                                         
         LA    R6,CTODATA                                                       
         MVI   ELCODE,X'38'                                                     
         GOTOR FIRSTEL                                                          
         USING CTOUTD,R6                                                        
         TM    CTOUTSTA,X'80'      CHECK OUTPUT IS ALLOWED                      
         JNO   ERRXIT                                                           
                                                                                
OUTVAL08 XC    FILENAME,FILENAME                                                
         MVC   USEIO,SAVUSEIO                                                   
                                                                                
OUTVAL10 MVC   TWAOUT,WORK                                                      
*&&US*&& CLC   =C'MLR',TWAOUT      MAILER                                       
*&&US*&& BNE   OUTVALX                                                          
*&&US*&& CLC   QCRDCODE,=C'TM'     ONLY ALLOWED FOR TM                          
*&&US*&& BE    OUTVALX                                                          
*&&US*&& CLC   QCRDCODE,=C'TA'     OR TA                                        
*&&US*&& BE    OUTVALX                                                          
*&&US*&& J     ERRXIT                                                           
*&&UK*&& B     OUTVALX                                                          
                                                                                
OUTVAL12 MVI   ERROR,0             VALIDATE SQL TRANSFORM @ABCDEF               
         CLI   OFFLINE,YESQ                                                     
         BE    OUTVAL14            DONT BOTHER IF OFFLINE                       
                                                                                
         LA    R4,KEY              SET KEY FOR SQL REFORM RECORD                
         USING GREFD,R4                                                         
         XC    GREFKEY,GREFKEY                                                  
         MVI   GREFKREC,GREFRECQ                                                
         MVC   GREFAGY,TWAAGY                                                   
         MVC   GREFID,WORK+1                                                    
         GOTOR DATAMGR,DMCB,(0,DMREAD),GENDIR,KEY,KEY                           
         BE    *+12                                                             
         MVI   ERROR,INVSQL                                                     
         J     ERRXIT                                                           
         MVC   TWAOUT,WORK         SAVE SQL FORMULA IN OUTPUT TYPE              
         B     OUTVALX                                                          
                                                                                
OUTVAL14 CLI   FLDILEND(R2),3      REMOTE AFP INPUT AS /XX                      
         BE    OUTVAL16                                                         
         CLI   FLDILEND(R2),6      REMOTE AFP INPUT AS /SPPXX                   
         BE    OUTVAL16                                                         
         MVI   ERROR,INVOUT                                                     
         J     ERRXIT                                                           
                                                                                
OUTVAL16 MVC   TWAOUT,WORK         SAVE SQL FORMULA IN OUTPUT TYPE              
                                                                                
OUTVALX  J     XITY                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE DESTINATION FIELD                               *         
***********************************************************************         
                                                                                
DSTVAL   NTR1  BASE=*,LABEL=*                                                   
         TM    GENSTAT7,GES$DRFP   TEST RFP GROUP IN OUTPUT FIELD               
         BNZ   DSTVALX                                                          
         GOTOR ANY                                                              
         TM    GENSTAT7,GES$ORFP   TEST RFP KEYWORD IN OUTPUT FIELD             
         BNZ   DSTVAL02                                                         
                                                                                
         GOTOR CHKRFP              CHECK FOR RFP IN DESTINATION FIELD           
         BNE   DSTVAL02                                                         
         CLI   OFFLINE,YESQ        SKIP IF OFFLINE                              
         BE    DSTVALX                                                          
                                                                                
         ICM   R1,15,EFHOUT        ERROR IF NO OUTPUT FIELD                     
         BNZ   *+12                                                             
         MVI   ERROR,INVALID       NO - RFP NOT ALLOWED                         
         J     ERRXIT              GO TO ERREX                                  
                                                                                
         CLI   FLDILEND(R1),0                                                   
         BNE   DSTVALX                                                          
         LR    R2,R1               CURSOR TO OUTPUT FIELD                       
         MVI   ERROR,MISSING                                                    
         J     ERRXIT              GO TO ERREX                                  
                                                                                
DSTVAL02 OC    ARFPBLK,ARFPBLK     IF DOING RFP THEN GROUP ID HERE              
         BZ    *+12                                                             
         GOTOR VALGRP              VALIDATE RFP GROUP CODE                      
         B     DSTVALX                                                          
                                                                                
         USING CTIREC,R4                                                        
         LA    R4,KEY                                                           
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,TWAORIG                                                  
         CLC   WORK(L'CTIKID),=CL10'DDS'                                        
         BE    DSTVAL04                                                         
*&&US*&& CLC   WORK(L'CTIKID),=CL10'SJR'                                        
*&&US*&& BE    DSTVAL04                                                         
         B     DSTVAL06                                                         
DSTVAL04 MVC   CTIKID,WORK                                                      
                                                                                
DSTVAL06 GOTOR CTREAD              READ ID RECORD                               
         JNE   ERRDEST                                                          
                                                                                
         CLC   FAXPFX,WORK         TEST FOR FAX ID PREFIX                       
         BNE   *+12                                                             
         GOTOR VALFAX              GO VALIDATE                                  
         B     DSTVALX             SKIP THE REST                                
                                                                                
         LHI   R0,QGETIDS                                                       
         ICM   R0,B'1110',T00A                                                  
         GOTOR CALLOV,DMCB,0,(R0)  GET A(GETIDS)                                
         L     RF,0(R1)                                                         
         GOTOR (RF),DMCB,(C'D',AIO),0,DATAMGR                                   
         CLI   0(R1),FF            TEST DISK ERROR                              
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,1,0(R1)          R5=NUMBER OF DESTINATION IDS                 
         JZ    ERRDEST             NONE FOUND                                   
         L     R1,4(R1)            R6=(DESTINATION ID LIST)                     
                                                                                
DSTVAL08 CLI   0(R1),FF            SEARCH TABLE OF VALID IDS                    
         JE    ERRDEST                                                          
         CLC   WORK(L'CTIKID),0(R1)                                             
         BE    DSTVAL10                                                         
         AHI   R1,L'CTIKID+L'CTIKNUM                                            
         BCT   R0,DSTVAL08                                                      
         J     ERRDEST                                                          
                                                                                
DSTVAL10 MVC   TWADEST,L'CTIKID(R1)                                             
                                                                                
DSTVALX  J     XITY                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE A FAX ID (SET UP FAXINFO BLOCK IF OFFLINE)                 *         
***********************************************************************         
                                                                                
VALFAX   NTR1  LABEL=NO                                                         
         L     R4,AIO                                                           
         USING CTIREC,R4                                                        
         LA    R3,CTIDATA                                                       
         SR    R1,R1                                                            
VALFAX02 CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),CTDSCELQ      GET DESCRIPTION ELEMENT                      
         BE    *+14                                                             
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     VALFAX02                                                         
         USING CTDSCD,R3           R3 = A(DESCRIPTION ELEMENT)                  
                                                                                
         XC    KEY,KEY             BUILD KEY FOR EDICT RECORD                   
         USING EDIKEYD,R4                                                       
         LA    R4,KEY                                                           
         XC    EDIKEY,EDIKEY                                                    
         MVI   EDIKSYS,EDIKSYSQ    SYSTEM                                       
         MVI   EDITYPE,EDITYPEQ    TYPE                                         
         MVC   EDINAME,CTDSC       EBCDIC USER ID FROM ID RECORD                
         GOTOR CTREAD              READ THE RECORD                              
         JNE   ERREDICT                                                         
                                                                                
         USING CTFXKEY,R4                                                       
         XC    CTFXKEY,CTFXKEY                                                  
         MVI   CTFXKTYP,CTFXEQU    TYPE                                         
         MVC   CTFXAGY,TWAAGY      AGENCY                                       
         MVC   CTFXCODE(5),FLDHDRL+L'FAXPFX(R2)                                 
         OC    CTFXCODE,SPACES                                                  
         GOTOR CTREAD              READ THE RECORD                              
         JNE   ERRFAX                                                           
                                                                                
         OC    TWAOUT,TWAOUT       IF OUTPUT TYPE NOT INPUT                     
         BNZ   VALFAX04                                                         
         MVC   TWAOUT,FAXOUT       SET OUTPUT TYPE TO 'FAX'                     
         ICM   R1,15,EFHOUT        IF OUTPUT TYPE FIELD EXISTS                  
         BZ    VALFAX04                                                         
         MVC   FLDHDRL(L'TWAOUT,R1),TWAOUT                                      
         OI    FLDOINDD(R1),FOUTTRN                                             
                                                                                
VALFAX04 CLI   OFFLINE,YESQ        IF WE'RE NOT OFFLINE                         
         BNE   VALFAX06            THEN DONE                                    
                                                                                
         L     R3,TWADCONS         ELSE INITIALIZE FAXLINK'S INFO BLOCK         
         USING TWADCOND,R3                                                      
         L     R3,TFAXINFO         R3 = A(FAX INFO BLOCK)                       
         USING FAXINFOD,R3                                                      
         MVI   FXISTAT,FXISPEND     SET FAX PENDING                             
         MVC   FXISIDNO,TWAORIG     ORIGIN ID NUMBER                            
         MVC   FXISAGY,TWAAGY       ALPHA AGENCY CODE                           
         MVC   FXISFXCD,CTFXCODE    FAX ID CODE                                 
         MVC   FXISRQST,REMUSER     REQUESTOR                                   
         MVC   FXITRSYS,RCPROG      SYSTEM                                      
         MVC   FXITRPRG(2),RCPROG+2 REPORT ID                                   
                                                                                
VALFAX06 J     XITY                                                             
                                                                                
CTREAD   LR    R0,RE               ** READ CTFILE RECORD **                     
         GOTOR DATAMGR,DMCB,DMREAD,CTFILE,KEY,AIO                               
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK FOR RFP 'FILE' KEYWORD INPUT IN A FIELD                       *         
***********************************************************************         
                                                                                
CHKRFP   NTR1  LABEL=NO                                                         
         CLI   FLDILEND(R2),0      TEST ANY INPUT AT ALL                        
         JE    XITN                                                             
         MVCDD DUB,GE#FILE         TRANSLATE 'FILE'                             
         GOTOR DICTATE,DMCB,C'SU  ',DUB,0                                       
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEND(R2)                                                
         BCTR  RF,0                DECREMENT INPUT LENGTH FOR EXECUTE           
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         JNE   XITN                                                             
         CLC   DUB(0),FLDHDRL(R2)                                               
         CLI   OFFLINE,YESQ        SKIP IF OFFLINE                              
         JE    XITY                                                             
         TM    WHEN,WOK$OV+WOK$DDS ONLY ALLOWED FOR OV & DDS                    
         JNZ   *+12                                                             
         MVI   ERROR,INVALID       NO - RFP NOT ALLOWED                         
         J     ERRXIT              GO TO ERREX                                  
         MVC   ARFPBLK,ATIA        SET A(RFPIO CONTROL BLOCK)                   
         J     XITY                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE AN RFP GROUP CODE                                          *         
***********************************************************************         
                                                                                
VALGRP   NTR1  LABEL=NO                                                         
         CLI   OFFLINE,YESQ        SKIP IF OFFLINE                              
         JE    XITY                                                             
                                                                                
         GOTOR SAVLIO              SAVE THE LIOB IF UPLOADING                   
                                                                                
         LA    R4,WORK                                                          
         USING QRFPD,R4            R4=A(RFP INTERFACE CONTROL BLOCK)            
         XC    QRFPD(QRFPDLEN),QRFPD                                            
         MVI   QRFPMODE,QRFPINIT   INITIALIZE RFP                               
         GOTOR RFP,DMCB,QRFPD                                                   
         MVI   QRFPMODE,QRFPGVAL   VALIDATE GROUP ID                            
         SR    R1,R1                                                            
         IC    R1,FLDILEND(R2)                                                  
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   QRFPWORK(0),FLDHDRL(R2)                                          
         GOTOR RFP,DMCB,QRFPD                                                   
         MVI   BYTE,0              SET RETURN CONDITION                         
         JE    *+8                                                              
         MVI   BYTE,1                                                           
                                                                                
         GOTOR RESLIO              RESTORE THE LIOB IF SAVED                    
                                                                                
         CLI   BYTE,0              TEST RFPIO RETURN CONDITION                  
         JE    XIT                 OKAY - EXIT WITH CC=EQUAL                    
         J     ERRXIT              ELSE LET ERREX HANDLE AND PERCOLATE          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK IF DELETED RECORD EXISTS ON FILE                   *         
***********************************************************************         
                                                                                
CHKDEL   NTR1  BASE=*,LABEL=*,WORK=(R4,CDWORKL)                                 
         USING CDWORKD,R4                                                       
         MVC   CDSAVEIO,AIO                                                     
         LA    R1,CDIO                                                          
         ST    R1,AIO                                                           
         MVI   RDUPDATE,YESQ       ALWAYS WANT READ FOR UPDATE HERE             
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTOR HIGH                CHECK IF RECORD ALREADY EXISTS               
         NI    DMINBTS,X'F7'                                                    
         MVC   AIO,CDSAVEIO                                                     
         LA    R2,KEY                                                           
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+8                                                              
         LA    R2,BIGKEY                                                        
         LH    R1,LKEY                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),KEYSAVE                                                  
         BNE   CHKDELN                                                          
         TM    DMCB+8,X'02'        IS EXISTING RECORD DELETED                   
         BNO   CHKDELN                                                          
         GOTOR WRITE               YES - WRITE BACK NEW ONE                     
         J     XITY                                                             
                                                                                
CHKDELN  MVC   KEY,KEYSAVE                                                      
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+16                                                             
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'KEYSAVE),KEYSAVE                                        
         J     XITN                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK IF DELETED RECORD EXISTS ON FILE                   *         
***********************************************************************         
                                                                                
CHKADR   NTR1  BASE=*,LABEL=*,WORK=(R4,CDWORKL)                                 
         USING CDWORKD,R4                                                       
         MVC   CDSAVEIO,AIO                                                     
         LA    R1,CDIO                                                          
         ST    R1,AIO                                                           
         MVI   RDUPDATE,YESQ       ALWAYS WANT READ FOR UPDATE HERE             
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTOR HIGH                CHECK IF RECORD ALREADY EXISTS               
         NI    DMINBTS,X'F7'                                                    
         LA    R2,KEY                                                           
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+8                                                              
         LA    R2,BIGKEY                                                        
         LH    R1,LKEY                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),KEYSAVE                                                  
         BE    CHKADR02                                                         
         MVC   KEY,KEYSAVE                                                      
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+16                                                             
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'KEYSAVE),KEYSAVE                                        
         B     CHKADRN                                                          
                                                                                
CHKADR02 TM    DMCB+8,X'02'        IS EXISTING RECORD DELETED                   
         BZ    CHKADRN                                                          
         MVI   RDUPDATE,YESQ       YES - WRITE BACK NEW KEY/REC                 
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTOR GETREC                                                           
         L     R6,AIO                                                           
         LR    R5,R6                                                            
         AH    R6,LKEY                                                          
         NI    2(R6),X'7F'         CLEAR DELETED BIT                            
         MVC   CDSAVERL,0(R6)                                                   
         MVC   AIO,CDSAVEIO                                                     
         L     R6,AIO                                                           
         LH    R1,LKEY                                                          
         LA    R3,0(R1,R6)                                                      
         MVC   CDSAVERL,0(R3)      SAVE NEW LEN                                 
         AH    R1,LSTATUS                                                       
         LHI   R1,2+4-1            LENGTH PLUS LINK -1                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R5)       COPY KEY,LEN,STAT,LINK TO NEW                
         MVC   0(2,R3),CDSAVERL    RESTORE NEW LEN                              
         GOTOR PUTREC                                                           
         LA    R6,KEY                                                           
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+8                                                              
         LA    R6,BIGKEY                                                        
         AH    R6,LKEY                                                          
         NI    0(R6),X'7F'         TURN OFF DELETE BIT                          
         GOTOR WRITE                                                            
         NI    DMINBTS,X'F7'                                                    
         MVC   AIO,CDSAVEIO                                                     
         J     XITY                AND RETURN CC EQUAL                          
                                                                                
CHKADRN  MVC   AIO,CDSAVEIO                                                     
         J     XITN                                                             
         DROP  RB                                                               
                                                                                
CDWORKD  DSECT                     ** CHKDEL/CHKADR S/R LOCAL W/S **            
CDSAVEIO DS    AL(L'AIO)                                                        
CDSAVERL DS    XL2                                                              
CDIO     DS    XL2000                                                           
CDWORKL  EQU   *-CDWORKD                                                        
GENNEW   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD FIELD                                               *         
***********************************************************************         
                                                                                
RECVAL   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         TM    GENSTAT6,GES$LINK   TEST RUNNING IN DDLINK UPLOAD MODE           
         BZ    *+12                                                             
         GOTOR GETHDR              GET NEXT RECORD - SET HEADER VALUES          
         JNE   XIT                 EXIT IF ALL UPLOAD RECORDS PROCESSED         
                                                                                
         L     R2,EFHREC           A(RECORD TWA FIELD HEADER)                   
         MVI   ERROR,INVREC                                                     
         MVI   ONLYSPUL,NOQ                                                     
         L     R1,EFHKEY           TEST SPOOL ONLY APPLICATION                  
         TM    FLDATBD(R1),FATBPROT                                             
         BNO   RECVAL02            THEY HAVE KEY FIELD PROTECTED                
         GOTOR ANY                                                              
         MVC   REMUSER,WORK        THEIR FIRST FIELD IS REQUESTOR               
         L     R2,EFHACT           AND SECOND FIELD IS REPORT                   
         MVI   ERROR,INVREP                                                     
         MVI   ONLYSPUL,YESQ                                                    
         B     RECVAL04                                                         
                                                                                
RECVAL02 MVI   DUB,LUPPRGQ         LOOK UP TYPE PROGRAM RECORD                  
         MVI   GCMODE,GCMPROGQ     PRE-SET TO HANDLE PROGRAM RECORDS            
         ICM   R3,15,ARECACT1      ALTERNATIVE START FOR RECORD TABLE           
         BNZ   *+8                                                              
         L     R3,ARECACT                                                       
         GOTOR LOOKUP,1                                                         
         BE    RECVAL06            FOUND IT - CONTINUE ON                       
                                                                                
RECVAL04 MVI   GCMODE,GCMNONEQ     RESET PROGRAM RECORD MODE                    
         MVI   DUB,LUPRECQ         LOOK UP RECORD TABLE ENTRY                   
         ICM   R3,15,ARECACT1      ALTERNATIVE START FOR TABLE                  
         BNZ   *+8                                                              
         L     R3,ARECACT                                                       
         GOTOR LOOKUP,1                                                         
                                                                                
RECVAL06 L     R3,WORK                                                          
         MVC   RECNUM,9(R3)        EXTRACT RECORD NUMBER                        
         MVC   PHDTADCT,10(R3)     DATA DICTIONARY PHASE                        
         MVC   PHHELP,11(R3)       HELP PHASE                                   
         GOTOR ANYHELP             POSSIBLE HELP EXIT                           
         CLC   TWALREC,RECNUM      CLEAR LIST DIRECTORY                         
         BE    RECVALX             ON CHANGE OF RECORD                          
         XC    LISTDIR,LISTDIR                                                  
         L     RE,AREPINFO         CLEAR SAVED REPORT INFO                      
         XC    0(L'REPINFO,RE),0(RE)                                            
         MVI   TWALACT,0                                                        
                                                                                
RECVALX  J     XITY                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE OTHER HEADER FIELDS                                        *         
***********************************************************************         
                                                                                
VALHDR   NTR1  BASE=*,LABEL=*                                                   
                                                                                
ACTVAL   L     R2,EFHACT           ACTION VALIDATION                            
         CLI   ONLYSPUL,YESQ                                                    
         BNE   ACTVAL02                                                         
         MVI   ACTNUM,12                                                        
         MVI   ACTEQU,12                                                        
         B     VALRAC                                                           
                                                                                
ACTVAL02 MVI   ACTIVSW,0                                                        
         CLC   8(#ACTLQ,R2),LP@ACT ACTIVE IS SPECIAL DISPLAY                    
         BNE   ACTVAL04                                                         
         MVI   ACTIVSW,1                                                        
         MVC   FLDHDRL(#DISLLQ,R2),LP@DISL                                      
         MVI   FLDIINDD(R2),#DISLLQ                                             
         OI    FLDOINDD(R2),FOUTTRN                                             
         B     ACTVAL12                                                         
                                                                                
ACTVAL04 CLI   LISTSW,FF           TEST RETURNING FROM SELECT SCREEN            
         BNE   ACTVAL06                                                         
         MVI   LISTSW,LISTSNXT     SET TO CONTINUE TO NEXT PAGE                 
         TM    GENSTAT2,DISTHSPG                                                
         BZ    *+8                                                              
         MVI   LISTSW,LISTSCUR     USER WANTS THIS PAGE RE-DISPLAYED            
         B     ACTVAL12                                                         
                                                                                
ACTVAL06 MVI   LISTSW,0            CHECK FOR SPECIAL LIST ACTIONS               
         GOTOR ANY                                                              
                                                                                
         LA    R3,LISTLST                                                       
         USING LISTLD,R3                                                        
         LHI   R0,LISTLSTN                                                      
ACTVAL08 SR    RF,RF                                                            
         ICM   RF,3,LISTSNAM                                                    
         LA    RF,GEND(RF)                                                      
         CLC   WORK(8),0(RF)                                                    
         BE    ACTVAL10                                                         
         AHI   R3,LISTLL                                                        
         BCT   R0,ACTVAL08                                                      
         B     ACTVAL12                                                         
                                                                                
ACTVAL10 MVC   LISTSW,LISTSET                                                   
         MVC   FLDHDRL(#LISTPLQ,R2),LP@LISTP                                    
         MVI   FLDILEND(R2),#LISTPLQ                                            
         OI    FLDOINDD(R2),FOUTTRN                                             
         DROP  R3                                                               
                                                                                
ACTVAL12 MVI   DUB,LUPACTQ         LOOK UP ACTION TABLE ENTRY                   
         MVI   ERROR,INVACT                                                     
         MVI   ACTNUM,0                                                         
         CLI   FLDILEND(R2),3                                                   
         BL    *+8                                                              
         MVI   FLDILEND(R2),3                                                   
         ICM   R3,15,ARECACT2      ALTERNATIVE START FOR ACTION TABLE           
         BNZ   *+8                                                              
         L     R3,ARECACT                                                       
         GOTOR LOOKUP,1                                                         
         L     R3,WORK                                                          
         CLI   9(R3),ACTSEL        FOR ACTION SELECT                            
         BNE   *+12                                                             
         CLI   TWALACT,ACTSEL      LAST ACTION MUST ALSO HAVE BEEN SEL.         
         JNE   ERRXIT              OR INTERNALLY SWITCHED TO SELECT             
         MVC   ACTNUM,9(R3)        EXTRACT ACTION NUMBER                        
         MVC   ACTEQU,10(R3)       AND ACTION EQUATE                            
         GOTOR ANYHELP                                                          
         CLI   USEIO,YESQ          TEST CTFILE I/O (NOT GENDIR/GENFIL)          
         BNE   VALRAC                                                           
         CLI   TWALACT,ACTLIST     TEST PREVIOUS ACTION WAS LIST                
         BE    *+12                                                             
         CLI   TWALACT,ACTSEL      OR SELECT                                    
         BNE   VALRAC                                                           
         CLI   ACTNUM,ACTLIST      BUT CURRENT ACTION ISN'T                     
         BE    VALRAC                                                           
         CLI   ACTNUM,ACTSEL       EITHER OF THEM                               
         BE    VALRAC                                                           
         XC    LASTSELK,LASTSELK   CLEAR SAVED LIST KEY                         
         EJECT                                                                  
***********************************************************************         
* CHECK RECORD/ACTION COMBINATION - SET UP PHASES                     *         
***********************************************************************         
                                                                                
VALRAC   MVC   WORK+2(1),ACTEQU    SET ACTION EQUATE FOR LOOKUP                 
         MVI   ERROR,0             PRESET NO 'WHEN' ERROR                       
         SR    R3,R3                                                            
                                                                                
VALRAC02 GOTOR LUPRAC              LOOKUP RECORD/ACTION ENTRY IN TABLE          
         BE    VALRAC06                                                         
         CLI   ERROR,0             WAS THERE A 'WHEN' ERROR                     
         JNE   *+8                                                              
         MVI   ERROR,INVRCACT      NO - RETURN INVALID RECORD/ACTION            
         J     ERRXIT                                                           
                                                                                
VALRAC04 MVC   WORK(3),0(R3)       IF WHEN DOESN'T MATCH, CONTINUE              
         B     VALRAC02            SEARCH                                       
                                                                                
VALRAC06 L     R3,WORK+4           R3=A(ENTRY) FROM LOOKUP                      
         MVC   PHSCREEN(4),3(R3)   EXTRACT SCREEN/EDIT/SPECS/REPORT             
         MVC   WHENOK,7(R3)        AVAILABLE PRINT OPTIONS                      
         MVC   RCPROG+2(2),8(R3)   HEADLINE PROGRAM CODE                        
         MVC   QCRDCODE,10(R3)     EOD PROGRAM HANDLE                           
                                                                                
         ICM   R2,15,EFHOTH        POINT TO 'OTHER'                             
         BZ    VALRAC10                                                         
         CLI   FLDILEND(R2),0                                                   
         BE    VALRAC10                                                         
         MVI   ERROR,INVALID                                                    
         CLI   FLDILEND(R2),3                                                   
*&&US*&& JNE   ERRXIT                                                           
*&&UK*&& BNE   VALRAC10                                                         
         CLC   FLDHDRL(#REQLQ,R2),LP@REQ                                        
*&&US*&& JNE   ERRXIT                                                           
*&&UK*&& BNE   VALRAC10                                                         
         TM    WHENOK,WOK$OV+WOK$DDS                                            
         BZ    VALRAC04            GO SEE IF MORE MATCHING RECACTS              
                                                                                
         MVC   TWALREC,RECNUM      SAVE RECORD NUMBER                           
         MVC   AOVERLAY,SYSDUMMY                                                
         GOTOR GETFACT,DMCB,0      GET A(PROGRAMS AREA)                         
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         TM    GENSTAT1,APPLIC     USE IT IF NORMALLY REQUIRE APPLIC            
         BZ    VALRAC08                                                         
         NI    GENSTAT1,FF-APPLIC  ENSURE MAINTENANCE CALL                      
         L     RF,FAPGMADR         AND SET TO LOAD IN PGMAREA                   
         AHI   RF,20000            ALLOW ROOM FOR ROOT PHASE                    
         ST    RF,AOVERLAY                                                      
                                                                                
VALRAC08 MVC   SYSPHASE,=X'D90FD800' SET PHASE ID                               
         GOTOR LOADEM,DMCB,0                                                    
         LR    RF,R3                                                            
         GOTOR (RF),DMCB,GEND                                                   
         J     XITY                                                             
         DROP  R1                                                               
                                                                                
VALRAC10 CLI   TWASCR,FF           TEST REQUEST DISPLAY SCREEN LOADED           
         BNE   VALRAC12            NO                                           
         MVI   TWASCR,0            RESET IT NOW                                 
         XC    LISTDIR,LISTDIR     CLEAR REMAINS - ITS GONE NOW                 
         J     XITL                GO RESTORE/CALL GETUSER                      
                                                                                
VALRAC12 CLI   GCMODE,GCMPROGQ     TEST PROGRAM RECORDS                         
         BNE   VALRAC16            NO                                           
         CLI   ACTNUM,ACTREP                                                    
         BE    VALRAC16                                                         
         CLI   LANG,LANGGER        ENGLISH LANGUAGE FIX FOR PRG RECS            
         BNL   VALRAC14                                                         
         CLI   ACTNUM,ACTSEL       TEST REPORT WAS SELECTED                     
         BNE   VALRAC14                                                         
         CLC   THISLSEL,LP@RESS    R=REP NOT R=RES                              
         BE    VALRAC16                                                         
                                                                                
VALRAC14 MVI   WHENOK,WOK$SCR                                                   
         CLI   ACTNUM,ACTLIST                                                   
         BE    *+8                                                              
         MVI   WHENOK,WOK$SCR+WOK$NOW+WOK$SOON+WOK$OV+WOK$DDS                   
         GOTOR SWTOCON             SWITCH TO CONTROL SYSTEM                     
         BE    VALRAC16            TEST SWITCH WAS SUCCESSFUL                   
         MVI   OKNO,19                                                          
         J     ERRXIT              TELL USER TO START CONTROL                   
                                                                                
VALRAC16 TM    GENSTAT5,VRAHOOK    TEST APPLIC WANTS MODE VALRA PASSED          
         BZ    VALRACX                                                          
         CLI   ACTNUM,ACTSEL       SELECT ACTION?                               
         BE    VALRACX             SKIP CALL, DO IT LATER FOR SEL CODE          
         MVI   MODE,VALRA          APPLIC CONTROLLER GETUSER MODE               
         GOTOR GETUSER                                                          
VALRACX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRINT OPTIONS (REQUESTOR AND RUN WHEN)                     *         
***********************************************************************         
                                                                                
VALPOP   L     R2,EFHWHEN                                                       
         MVI   PHHELP,X'EF'                                                     
         GOTOR ANYHELP                                                          
         MVI   TWAWHEN,TWW$NOW                                                  
         MVI   WHEN,WOK$SCR        DEFAULT IS ON SCREEN                         
         CLI   ONLYSPUL,YESQ       SPOOL ONLY DEFAULT TO NOW                    
         BNE   *+8                                                              
         MVI   WHEN,WOK$NOW        NOW                                          
         CLI   FLDILEND(R2),0                                                   
         BE    VALPOPX                                                          
                                                                                
         GOTOR SCANNER,DMCB,(R2),(1,BLOCK),X'6B7EFF6B'                          
         CLI   4(R1),1             MUST ONLY BE ONE ENTRY                       
         BNE   VPINVAL                                                          
         LA    R3,BLOCK                                                         
         USING SCANBLKD,R3                                                      
         LA    RE,PRTOPTS          RE=A(VALID PRINT OPTIONS)                    
         USING PRTOPTD,RE                                                       
         CLI   GCMODE,GCMPROGQ     PROGRECS GET EXTENDED LIST                   
         BNE   VALPOP02                                                         
         CLI   ACTNUM,ACTSEL       ONLY IF ACTION IS SELECT (FOR                
         BNE   VALPOP02            REPORT)                                      
         LA    RE,PRGOPTS                                                       
                                                                                
VALPOP02 CLI   PRTONAME,FF         TEST EOT                                     
         BE    VPINVAL                                                          
         SR    RF,RF                                                            
         ICM   RF,3,PRTONAME                                                    
         LA    RF,GEND(RF)                                                      
         CLC   SC1STFLD(WHENLQ),0(RF)                                           
         BE    VALPOP06                                                         
VALPOP04 AHI   RE,PRTOPTL                                                       
         B     VALPOP02                                                         
                                                                                
VALPOP06 LA    R1,PRTONSPO         R1=A(OPTION BRANCH (NON-SPOOL))              
         CLI   ONLYSPUL,YESQ                                                    
         BNE   *+8                                                              
         LA    R1,PRTOYSPO         DITTO SPOOL-ONLY                             
         SR    RF,RF                                                            
         ICM   RF,3,0(R1)                                                       
         BZ    VPINVAL                                                          
         A     RF,BASERB                                                        
         MVC   WHEN,PRTOWHEN                                                    
         MVC   TWAWHEN,PRTOWVAL                                                 
         BR    RF                                                               
         DROP  RE                                                               
                                                                                
VPINVAL  MVI   ERROR,INVPRINT                                                   
         J     ERRXIT                                                           
                                                                                
VPREMU   MVC   REMUSER,SC2NDFLD    USER ID FOR NOW/SOON/ON                      
         DROP  R3                                                               
                                                                                
VALPOPX  CLI   OFFLINE,YESQ        DON'T CHECK IF OFF LINE                      
         BE    VALOUT                                                           
         MVC   DUB,WHEN                                                         
         NC    DUB(1),WHENOK                                                    
         MVI   ERROR,POPTNTOK                                                   
         CLI   DUB,0                                                            
         BE    VALRAC04            GO SEE IF MORE MATCHING RECACTS              
         TM    WHEN,WOK$SOON+WOK$OV                                             
         BZ    VALOUT                                                           
         CLC   REMUSER,SPACES                                                   
         BH    VALOUT                                                           
         MVI   ERROR,NOUSRNAM      NO USER NAME ENTERED                         
         J     ERRXIT                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE OUTPUT TYPE                                                *         
***********************************************************************         
                                                                                
VALOUT   XC    TWAOUT,TWAOUT                                                    
         ICM   R2,15,EFHOUT        IGNORE IF NO OUTPUT FIELD                    
         BZ    VALOUTX                                                          
         CLI   FLDILEND(R2),0                                                   
         BNE   VALOUT02                                                         
         OC    ARFPBLK,ARFPBLK     IF RFP PROCESSING                            
         BZ    VALOUTX                                                          
         MVI   ERROR,MISSING       THEN INPUT REQUIRED                          
         J     ERRXIT                                                           
VALOUT02 GOTOR OUTVAL                                                           
VALOUTX  DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE DESTINATION                                                *         
***********************************************************************         
                                                                                
VALDST   XC    TWADEST,TWADEST     INIT DESTINATION                             
         ICM   R2,15,EFHDEST       SKIP IF NO DESTINATION FIELD                 
         BZ    VALDSTX                                                          
         CLI   FLDILEND(R2),0                                                   
         BNE   VALDST02                                                         
         OC    ARFPBLK,ARFPBLK     IF RFP PROCESSING                            
         BZ    VALDSTX                                                          
         MVI   ERROR,MISSING       THEN INPUT REQUIRED                          
         J     ERRXIT                                                           
VALDST02 GOTOR DSTVAL                                                           
VALDSTX  DS    0H                                                               
                                                                                
VALHDRX  J     XITY                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* MERGE IN KEY FIELDS FROM SAVED SCREEN                               *         
***********************************************************************         
                                                                                
KEYMRG   NTR1  BASE=*,LABEL=*                                                   
         TM    GENSTAT1,USKYMRG    UNLESS OVERRIDDEN,                           
         BNZ   *+12                                                             
         CLI   ACTNUM,ACTLIST      DON'T MERGE FOR LIST                         
         BE    KEYMRGX                                                          
         TM    GENSTAT1,EFHKYMRG   MERGE ON EXTENDED HEADERS                    
         BNZ   KEYMRG10                                                         
                                                                                
***********************************************************************         
* R3 HAS NEW SCREEN AFSTKEY                                           *         
* R4 IS USED TO WALK THROUGH OLD SCREEN KEY AREA                      *         
*       NEW SCREEN KEY FIELDS WILL BE FILLED FROM OLD IF:             *         
*            1) TAG FIELD (PRECEEDING PROTECTED FIELD) MATCHES        *         
*               IN LENGTH AND CONTENTS                                *         
*            2) DATA FIELD (FOLLOWING UNPROTECTED FIELD) MATCHES      *         
*               IN LENGTH                                             *         
*            3) IF MORE THAN ONE MATCH, FIRST FOUND IN OLD            *         
*               KEY FIELDS IS USED                                    *         
*            4) IF EFHKYMRG IS ON USE EXTENDED FIELD HEADER NUMBERS   *         
*               TO MATCH FIELDS                                       *         
***********************************************************************         
                                                                                
KEYMRG02 CLI   0(R3),0             TEST END-OF-(NEW)SCREEN                      
         BE    KEYMRG22                                                         
         TM    FLDATBD(R3),FATBPROT                                             
         BNO   KEYMRG08                                                         
         CLI   FLDLEND(R3),FLDHDRL+1                                            
         BE    KEYMRG22                                                         
         SR    RF,RF                                                            
         IC    RF,0(R3)            RF=L'TAG FIELD                               
         LA    R6,0(RF,R3)         R6=A(NEW DATA FIELD)                         
         TM    FLDATBD(R6),FATBPROT                                             
         BO    KEYMRG08            MUST BE UNPROTECTED                          
         TM    GENSTAT3,IGNNONXK   IGNORE NON-EXTENDED 'KEY' FIELDS             
         BZ    *+12                                                             
         TM    FLDATBD(R6),FATBXHDR                                             
         BZ    KEYMRG08                                                         
                                                                                
         LA    R4,IO               SET UP TO LOOP THROUGH OLD KEYS              
KEYMRG04 CLI   FLDLEND(R4),0                                                    
         BE    KEYMRG08                                                         
         TM    FLDATBD(R4),FATBPROT                                             
         BNO   KEYMRG06                                                         
         CLI   FLDLEND(R4),FLDHDRL+1                                            
         BE    KEYMRG08                                                         
         CLC   0(1,R3),0(R4)       TAG LENGTHS MUST MATCH                       
         BNE   KEYMRG06                                                         
         LR    R1,RF                                                            
         SHI   R1,FLDHDRL+1        CONTENTS OF TAG FIELD                        
         TM    FLDATBD(R3),FATBXHDR                                             
         BZ    *+8                                                              
         SHI   R1,FLDHDRL                                                       
         EX    R1,KEYCLC                                                        
         BNE   KEYMRG06                                                         
                                                                                
         LA    R5,0(RF,R4)         R5=A(OLD DATA FIELD)                         
         TM    FLDATBD(R5),FATBPROT                                             
         BO    KEYMRG06                                                         
         TM    GENSTAT3,IGNNONXK   IGNORE NON-EXTENDED 'KEY' FIELDS             
         BZ    *+12                                                             
         TM    FLDATBD(R5),FATBXHDR                                             
         BZ    KEYMRG06                                                         
         SR    R0,R0                                                            
         IC    R0,0(R5)            LENGTH OF DATA FIELDS MUST MATCH             
         TM    FLDATBD(R5),FATBXHDR                                             
         BZ    *+8                                                              
         SHI   R0,8                R0=L'OLD DATA FIELD + IT'S HEADER            
         SR    R1,R1                                                            
         IC    R1,0(R6)                                                         
         TM    FLDATBD(R6),FATBXHDR                                             
         BZ    *+8                                                              
         SHI   R1,8                R1=L'NEW DATA FIELD + IT'S HEADER            
         CR    R0,R1                                                            
         BNE   KEYMRG06                                                         
                                                                                
         LR    R3,R6               MOVE IN KEY DATA                             
         LR    R4,R5                                                            
         SHI   R1,5                MOVING SOME INDICES AND DATA                 
         EX    R1,KEYMVC                                                        
         OI    FLDIINDD(R3),FINPTHIS                                            
         B     KEYMRG08                                                         
                                                                                
KEYMRG06 SR    R1,R1                                                            
         IC    R1,0(R4)            MOVE POINTER TO NEXT OLD FIELD               
         AR    R4,R1                                                            
         B     KEYMRG04                                                         
                                                                                
KEYMRG08 SR    R1,R1                                                            
         IC    R1,0(R3)            MOVE POINTER TO NEXT NEW FIELD               
         AR    R3,R1                                                            
         B     KEYMRG02                                                         
                                                                                
KEYMRG10 CLI   FLDLEND(R3),0                                                    
         BE    KEYMRG22                                                         
         TM    FLDATBD(R3),FATBPROT                                             
         BZ    *+16                                                             
         CLI   FLDLEND(R3),FLDHDRL+1                                            
         BE    KEYMRG22                                                         
         B     KEYMRG12                                                         
         TM    FLDATBD(R3),FATBXHDR                                             
         BNZ   KEYMRG14                                                         
                                                                                
KEYMRG12 SR    RF,RF                                                            
         IC    RF,0(R3)            BUMP TO NEXT NEW FIELD                       
         AR    R3,RF                                                            
         B     KEYMRG10            LOOK FOR NEXT UNPROT FIELD                   
                                                                                
KEYMRG14 LA    R4,IO               SET UP TO LOOP THROUGH OLD KEYS              
                                                                                
KEYMRG16 CLI   FLDLEND(R4),0                                                    
         BE    KEYMRG12                                                         
         TM    FLDATBD(R4),FATBPROT                                             
         BZ    *+16                                                             
         CLI   FLDLEND(R4),FLDHDRL+1                                            
         BE    KEYMRG12            GET NEXT FIELD FROM NEW SCREEN               
         B     KEYMRG18                                                         
         TM    FLDATBD(R4),FATBXHDR                                             
         BNZ   KEYMRG20                                                         
                                                                                
KEYMRG18 SR    R1,R1                                                            
         IC    R1,0(R4)            MOVE POINTER TO NEXT OLD FIELD               
         AR    R4,R1                                                            
         B     KEYMRG16                                                         
                                                                                
KEYMRG20 SR    RF,RF                                                            
         IC    RF,FLDLEND(R3)                                                   
         LR    R1,RF               R1=NEW FIELD LENGTH                          
         AR    RF,R3                                                            
         SHI   RF,FLDHDRL                                                       
         SR    RE,RE                                                            
         IC    RF,FLDLEND(R4)                                                   
         LR    R0,RE               R0=OLD FIELD LENGTH                          
         AR    RE,R4                                                            
         SHI   RE,FLDHDRL                                                       
         CLC   0(1,RE),0(RF)       DO FIELD NUMBERS MATCH                       
         BNE   KEYMRG18                                                         
         CR    R1,R0               IF L'NEW FIELD GT L'OLD FIELD                
         BNH   *+6                                                              
         LR    R1,R0               MERGE USING L'OLD FIELD                      
         SHI   R1,FLDHDRL+4+1      EX LENGTH OF DATA                            
         EX    R1,KEYMVC                                                        
         OI    FLDIINDD(R3),FINPTHIS                                            
         B     KEYMRG12            GET NEXT FIELD FROM NEW SCREEN               
                                                                                
KEYMRG22 LA    R0,IO               CLEAR SCREEN FROM I/O AREA                   
         LHI   R1,L'IO                                                          
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
KEYMRGX  J     XIT                                                              
                                                                                
KEYCLC   CLC   FLDHDRL(0,R3),FLDHDRL(R4)                                        
KEYMVC   MVC   FLDIINDD(0,R3),FLDIINDD(R4)                                      
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* MAINTAIN ACTIVITY ELEMENTS                                          *         
***********************************************************************         
                                                                                
ADDACT   NTR1  LABEL=NO                                                         
         CLI   ACTELOPT,NOQ        OPTION NOT TO MONITOR ACTIVITY               
         JE    XIT                                                              
         LA    R6,ELEMENT                                                       
         USING ACTVD,R6                                                         
         MVI   ELCODE,X'F1'                                                     
         GOTOR REMELEM                                                          
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACTVEL,X'F1'                                                     
         MVI   ACTVLEN,20                                                       
         MVC   ACTVADDT,BTODAY     SET DATE                                     
         MVC   ACTVCHDT,BTODAY                                                  
         LA    R3,ACTVADID                                                      
         J     CHAACT08                                                         
                                                                                
CHAACT   NTR1  LABEL=NO                                                         
         LA    R6,ELEMENT                                                       
         USING ACTVD,R6                                                         
         CLI   ACTELOPT,NOQ        OPTION NOT TO MONITOR ACTIVITY               
         JE    XIT                                                              
         MVI   ELCODE,X'F1'                                                     
         GOTOR REMELEM                                                          
         CLI   ELEMENT,0                                                        
         JNE   CHAACT02                                                         
         MVI   ACTVEL,X'F1'        REPAIRING IF NO ELEMENT YET                  
         MVI   ACTVLEN,20                                                       
         MVC   ACTVADDT,BTODAY     SET DATE                                     
         MVC   ACTVADID,TWAORIG    AND ID                                       
         MVC   ACTVCHDT,BTODAY                                                  
                                                                                
CHAACT02 LA    R3,ACTVCHID                                                      
         CLC   ACTVCHDT,BTODAY     WAS RECORD CHANGED TODAY                     
         JNE   CHAACT04                                                         
         CLI   ACTVCHNM,0          (ADDED TODAY)                                
         JNE   CHAACT06                                                         
                                                                                
CHAACT04 MVC   ACTVCHDT,BTODAY     NO SO SET TODAYS DATE                        
         AI    ACTVCHNM,1          UP THE CHANGE NUMBER                         
         MVC   ACTVCHRE,CHREASON   AND MOVE IN THE REASON                       
         J     CHAACT08                                                         
                                                                                
CHAACT06 OC    ACTVCHRE,CHREASON                                                
                                                                                
CHAACT08 MVC   0(2,R3),TWAORIG     MOVE IN ID                                   
         GOTOR GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       IS PASSWORD PROTECT ACTIVE                   
         JZ    CHAACT10                                                         
         MVC   0(2,R3),FAPASSWD    YES SO USE THIS ID                           
         OI    2(R3),X'80'                                                      
         DROP  R1                                                               
                                                                                
CHAACT10 OC    ASECBLK,ASECBLK     IS NEW SECURITY ACTIVE                       
         JZ    CHAACT12                                                         
         L     R1,ASECBLK          NEW SECURITY BLOCK                           
         USING SECD,R1                                                          
         MVC   ACTVSCID,SECPID     USER'S PERSONAL ID                           
         MVI   ACTVLEN,ACTVLENQ    NEW LENGTH                                   
         DROP  R1                                                               
                                                                                
CHAACT12 GOTOR ADDELEM                                                          
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE CLEARS FIELD AND OPTIONALLY TRANSMITS FIELD AT R2           *         
***********************************************************************         
                                                                                
CLRFLD   STM   RE,R1,12(RD)                                                     
                                                                                
         USING FLDHDRD,R2                                                       
         SR    R1,R1                                                            
         ICM   R1,1,FLDLEN         CALCULATE L'FIELD                            
         JZ    CLRFLDX                                                          
         SHI   R1,FLDHDRL+1                                                     
         CLI   FLDATB,FF           NOP FIELD - CAN'T HAVE EXTENDED HDR          
         JE    *+16                                                             
         TM    FLDATB,FATBXHDR     TEST FOR EXTENDED HEADER                     
         JNO   *+8                                                              
         SHI   R1,FLDHDRL                                                       
         BASR  RF,0                                                             
         EX    R1,8(RF)            TEST IF ANYTHING IN FIELD                    
         JZ    CLRFLDX                                                          
         OC    FLDDATA(0),FLDDATA                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT IT                                  
         MVI   FLDILEN,0                                                        
         BASR  RF,0                                                             
         EX    R1,8(RF)            GO AHEAD AND CLEAR IT                        
         J     CLRFLDX                                                          
         XC    FLDDATA(0),FLDDATA                                               
                                                                                
CLRFLDX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY FOR ADD                                                *         
***********************************************************************         
                                                                                
ADDVAL   NTR1  LABEL=NO                                                         
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTOR HIGH                CHECK RECORD DOES NOT YET EXIST              
         NI    DMINBTS,X'F7'                                                    
         LH    R1,LKEY                                                          
         BCTR  R1,0                                                             
         LA    RF,BIGKEY                                                        
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         JNZ   *+8                                                              
         LA    RF,KEY              USE LITTLE KEY                               
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         JNE   ADDVAL02                                                         
         CLC   KEYSAVE(0),0(RF)                                                 
         MVI   ERROR,RECEXIST                                                   
         TM    DMCB+8,X'02'        IS EXISTING RECORD DELETED                   
         JZ    XITN                                                             
         TM    GENSTAT1,OKADDEL    IS IT OK TO ADD DELETED                      
         JO    ADDVAL02                                                         
         MVI   ERROR,DELEXIST      DIFFERENT ERROR MESSAGE                      
         J     XITN                                                             
                                                                                
ADDVAL02 MVC   KEY,KEYSAVE         OK TO ADD - RESTORE KEY                      
         TM    GENSTAT4,USEBIGKY                                                
         JZ    XITY                                                             
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'KEYSAVE),KEYSAVE                                        
         J     XITY                RETURN CC EQUAL                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE CONTROLS ADDING A RECORD                                    *         
***********************************************************************         
                                                                                
DOADD    NTR1  BASE=*,LABEL=*                                                   
         L     RE,AIO              PRE-CLEAR I/O                                
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         L     R0,AIO                                                           
         LH    R1,LKEY                                                          
         LA    RE,KEY                                                           
         TM    GENSTAT4,USEBIGKY                                                
         BZ    *+8                                                              
         LA    RE,BIGKEY                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR GOOVER              PASS REC. VALIDATION MODE (PASSED)           
         MVI   MODE,RECADD                                                      
         GOTOR GOOVER                                                           
         CLI   IOOPT,YESQ                                                       
         BE    DOADD08                                                          
         CLI   USEIO,YESQ                                                       
         BE    DOADD04                                                          
                                                                                
         TM    GENSTAT1,OKADDEL    OK TO ADD DELETED RECORDS                    
         BZ    DOADD02                                                          
         GOTOR CHKADR              YES - TEST RECORD IS DELETED                 
         BE    DOADD08                                                          
DOADD02  GOTOR ADDREC              ADD RECORD TO FILE                           
         MVC   TWAKEYSV,KEY        KEY MAY HAVE CHANGED                         
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+10                                                             
         MVC   TWAKEYSV,BIGKEY                                                  
         B     DOADD08                                                          
                                                                                
DOADD04  TM    GENSTAT1,OKADDEL    OK TO WRITE BACK DELETED RECORDS             
         BZ    DOADD06             ON AN ADD                                    
         GOTOR CHKDEL              RETURNS CC EQUAL ONLY IF DELETED             
         BE    DOADD08             RECORD ON FILE                               
DOADD06  GOTOR ADD                                                              
                                                                                
DOADD08  MVI   MODE,XRECADD                                                     
         GOTOR GOOVER                                                           
         J     XIT                                                              
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE CONTROLS CHANGING A RECORD                                  *         
***********************************************************************         
                                                                                
DOCHA    NTR1  BASE=*,LABEL=*                                                   
         GOTOR GOOVER              PASS VALIDATE RECORD MODE (PASSED)           
         MVI   MODE,RECPUT                                                      
         GOTOR GOOVER                                                           
         CLI   IOOPT,YESQ                                                       
         BE    DOCHA04                                                          
         CLI   USEIO,YESQ                                                       
         BE    DOCHA02                                                          
         CLC   GLOBDA,DMWORK+4     ENSURE DISK ADDRESS NOT CHANGED              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR PUTREC              AND WRITE BACK                               
         B     DOCHA04                                                          
                                                                                
DOCHA02  GOTOR WRITE                                                            
                                                                                
DOCHA04  MVI   MODE,XRECPUT                                                     
         GOTOR GOOVER                                                           
         J     XIT                                                              
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE CONTROLS DELETING A RECORD                                  *         
***********************************************************************         
                                                                                
DODEL    NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,RECISDEL                                                   
         CLI   USEIO,YESQ                                                       
         BE    DODEL06                                                          
         LA    R6,KEY              DELETE FOR DA FILES                          
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+8                                                              
         LA    R6,BIGKEY                                                        
         AH    R6,LKEY                                                          
         CLI   IOOPT,YESQ                                                       
         BE    *+12                                                             
         TM    0(R6),X'80'                                                      
         JO    XITH                                                             
         GOTOR READUP                                                           
         GOTOR GETREC                                                           
         MVI   MODE,RECDEL                                                      
         GOTOR GOOVER                                                           
         CLI   IOOPT,YESQ                                                       
         BE    DODEL02                                                          
         L     R6,AIO                                                           
         AH    R6,LKEY                                                          
         OI    2(R6),X'80'                                                      
         GOTOR PUTREC                                                           
         LA    R6,KEY                                                           
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+8                                                              
         LA    R6,BIGKEY                                                        
         AH    R6,LKEY                                                          
         OI    0(R6),X'80'                                                      
         GOTOR WRITE                                                            
                                                                                
DODEL02  MVI   MODE,XRECDEL                                                     
         GOTOR GOOVER                                                           
         NI    DMINBTS,X'F7'                                                    
         J     XIT                                                              
                                                                                
DODEL06  L     R6,AIO              DELETE FOR IS FILES                          
         AH    R6,LKEY                                                          
         CLI   IOOPT,YESQ                                                       
         BE    *+12                                                             
         TM    2(R6),X'80'                                                      
         JO    XITH                                                             
         MVI   MODE,RECDEL                                                      
         GOTOR GOOVER                                                           
         CLI   IOOPT,YESQ                                                       
         BE    DODEL08                                                          
         OI    2(R6),X'80'                                                      
         GOTOR WRITE                                                            
DODEL08  MVI   MODE,XRECDEL                                                     
         GOTOR GOOVER                                                           
         NI    DMINBTS,X'F7'                                                    
         J     XITY                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE CONTROLS RESTORING A RECORD                                 *         
***********************************************************************         
                                                                                
DORES    NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,RECNTDEL                                                   
         CLI   USEIO,YESQ                                                       
         BE    DORES04                                                          
         LA    R6,KEY              RESTORE FOR DA FILES                         
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+8                                                              
         LA    R6,BIGKEY                                                        
         AH    R6,LKEY                                                          
         CLI   IOOPT,YESQ                                                       
         BE    *+12                                                             
         TM    0(R6),X'80'                                                      
         JZ    XITH                                                             
         GOTOR READUP                                                           
         GOTOR GETREC                                                           
         MVI   MODE,RECREST                                                     
         GOTOR GOOVER                                                           
         CLI   IOOPT,YESQ                                                       
         BE    DORES02                                                          
         L     R6,AIO                                                           
         AH    R6,LKEY                                                          
         NI    2(R6),X'7F'                                                      
         GOTOR PUTREC                                                           
         LA    R6,KEY                                                           
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+8                                                              
         LA    R6,BIGKEY                                                        
         AH    R6,LKEY                                                          
         NI    0(R6),X'7F'         TURN OFF DELETE BIT                          
         GOTOR WRITE                                                            
                                                                                
DORES02  MVI   MODE,XRECREST                                                    
         GOTOR GOOVER                                                           
         NI    DMINBTS,X'F7'                                                    
         J     XIT                                                              
                                                                                
DORES04  L     R6,AIO              RESTORE FOR IS FILES                         
         AH    R6,LKEY                                                          
         CLI   IOOPT,YESQ                                                       
         BE    *+12                                                             
         TM    2(R6),X'80'                                                      
         JZ    XITH                                                             
         MVI   MODE,RECREST                                                     
         GOTOR GOOVER                                                           
         CLI   IOOPT,YESQ                                                       
         BE    DORES06                                                          
         NI    2(R6),X'7F'         TURN OFF DELETE BIT                          
         GOTOR WRITE                                                            
                                                                                
DORES06  MVI   MODE,XRECREST                                                    
         GOTOR GOOVER                                                           
         NI    DMINBTS,X'F7'                                                    
         J     XITY                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* MAINTENANCE ROUTINES - SELECT                                       *         
***********************************************************************         
                                                                                
DOSEL    NTR1  BASE=*,LABEL=*                                                   
         CLI   PFAID,0             TEST PFKEY PRESSED                           
         BE    DOSEL02                                                          
         MVI   MODE,PROCPFK        SET PFKEY MODE                               
         GOTOR GOOVER              GIVE APPLICATION A SHOT                      
                                                                                
DOSEL02  LA    R3,LISTDIR          ADDRESS DIRECTORY                            
         SR    R0,R0                                                            
         ICM   R0,1,LISTNUM        (MAX 18)                                     
         JZ    XITH                                                             
                                                                                
DOSEL04  TM    0(R3),FF-X'40'      LOOK FOR FIRST/NEXT SELECTION                
         BNZ   DOSEL06                                                          
         AHI   R3,6                                                             
         BCT   R0,DOSEL04                                                       
         ICM   R0,15,LASTSEL                                                    
         MVC   KEY,LASTSELK                                                     
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+16                                                             
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'LASTSELK),LASTSELK                                      
         GOTOR RESLST              NO MORE - RESTORE LIST SCREEN                
         JNE   XITH                UNLESS NONE SAVED                            
         GOTOR XMTSCR              TURN ON TRANSMIT BITS                        
         MVI   LISTSW,FF           FORCE FOR RECAP                              
         STCM  R0,15,LASTSEL                                                    
         MVC   LASTSELK,KEY                                                     
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+10                                                             
         MVC   LASTSELK,BIGKEY                                                  
         MVC   LASTLIST,LASTSEL    RE-START LIST AFTER LAST SEL.                
         J     XITL                GO BACK TO LOAD APPLIC                       
                                                                                
DOSEL06  SR    RE,RE                                                            
         IC    RE,LISTNUM          MOVE OUT DISK ADDRESS                        
         SR    RE,R0                                                            
         STC   RE,SELLISTN         SET RELATIVE LINE NUM                        
         MVC   THISLSEL,0(R3)      PASS THIS LIST SELECT CODE                   
         MVC   THISLARG,1(R3)      PASS THIS LIST ARGUMENT                      
                                                                                
         TM    GENSTAT5,VRAHOOK    TEST APPLIC WANTS MODE VALRA PASSED          
         BZ    DOSEL08                                                          
         CLI   THISLSEL,X'40'      HAVE A SELECT ACTION                         
         BNH   DOSEL08             NO - NOT 1ST PASS                            
         MVI   MODE,VALRA          APPLIC CONTROLLER GETUSER MODE               
         GOTOR GETUSER                                                          
                                                                                
DOSEL08  CLI   0(R3),REPSELQ       TEST REPORT WAS SELECTED                     
         BE    DOSEL44                                                          
         GOTOR SELGET              READ RECORD                                  
                                                                                
         CLC   0(1,R3),LP@CHAS                                                  
         BE    DOSEL30                                                          
         TM    GENSTAT4,CONFDEL                                                 
         BZ    *+14                                                             
         CLC   0(1,R3),LP@DELS                                                  
         BE    DOSEL32                                                          
         CLI   0(R3),CHASELQ                                                    
         BE    DOSEL36                                                          
         CLI   0(R3),DELSELQ                                                    
         BE    DOSEL12                                                          
         CLI   GCMODE,GCMPROGQ     TEST PROGRECS                                
         BNE   DOSEL10                                                          
         CLC   0(1,R3),LP@REPS     TEST REPORT ACTION                           
         BE    DOSEL22                                                          
         CLI   LANG,LANGGER        ENGLISH LANGUAGE FIX FOR PRG RECS            
         BNL   DOSEL10                                                          
         CLC   0(1,R3),LP@RESS     R=REP NOT R=RES                              
         BE    DOSEL22                                                          
                                                                                
DOSEL10  MVI   MODE,DISPKEY        SELECT AND DISPLAY                           
         GOTOR GOOVER                                                           
         MVC   TWAKEYSV,KEY                                                     
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+10                                                             
         MVC   TWAKEYSV,BIGKEY                                                  
         MVI   MODE,DISPREC                                                     
         GOTOR GOOVER                                                           
         L     R2,EFHACT                                                        
         CLC   0(1,R3),LP@DELS                                                  
         BE    DOSEL12                                                          
         CLC   0(1,R3),LP@RESS                                                  
         BE    DOSEL18                                                          
         CLC   0(1,R3),LP@REPS                                                  
         BE    DOSEL16                                                          
         B     DOSEL24                                                          
                                                                                
DOSEL12  GOTOR SELCLR              DELETE                                       
         GOTOR DODEL                                                            
         MVI   OKNO,13                                                          
         LA    R3,LISTDIR                                                       
         SR    R0,R0                                                            
         IC    R0,LISTNUM                                                       
                                                                                
DOSEL14  CLI   0(R3),0             SEE IF THERE'S ANY MORE TO COME              
         BNE   DOSEL52                                                          
         AHI   R3,6                                                             
         BCT   R0,DOSEL14          NO                                           
         MVI   OKNO,12             (LAST SELECTION)                             
         B     DOSEL52                                                          
                                                                                
DOSEL16  GOTOR SELCLR              PRINT REPORT                                 
         L     R2,EFHREC                                                        
         MVC   REMUSER,FLDHDRL(R2)                                              
         GOTOR STRTPRNT                                                         
         L     R2,AFRSTKEY                                                      
         GOTOR DOPRNT              CONTROL PRINTING                             
         B     DOSEL02                                                          
                                                                                
DOSEL18  GOTOR SELCLR              RESTORE                                      
         GOTOR DORES                                                            
         MVI   OKNO,18                                                          
         LA    R3,LISTDIR                                                       
         SR    R0,R0                                                            
         IC    R0,LISTNUM                                                       
                                                                                
DOSEL20  CLI   0(R3),0             SEE IF THERE'S ANY MORE TO COME              
         BH    DOSEL52                                                          
         AHI   R3,6                                                             
         BCT   R0,DOSEL20          NO                                           
         MVI   OKNO,12             (LAST SELECTION)                             
         B     DOSEL52                                                          
                                                                                
DOSEL22  MVI   MODE,DISPKEY        DISPLAYING FOR REPORT                        
         GOTOR GOOVER                                                           
         MVC   TWAKEYSV,KEY                                                     
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+10                                                             
         MVC   TWAKEYSV,BIGKEY                                                  
         MVI   MODE,DISPREC                                                     
         GOTOR GOOVER                                                           
         MVI   0(R3),REPSELQ                                                    
         L     R2,EFHWHEN                                                       
         CLC   FLDHDRL(#NEXTLQ,R2),LP@NEXT                                      
         BNE   *+14                                                             
         XC    FLDHDRL(4,R2),FLDHDRL(R2)                                        
         OI    FLDOINDD(R2),FOUTTRN                                             
         MVI   OKNO,30             (HIT ENTER TO GENERATE REPORT)               
         CLI   PQSW,2              TEST PQ HAS BEEN OPENED                      
         JNE   XITR2               NO                                           
         MVI   PQSW,FF                                                          
         MVI   SPMODE,FF                                                        
         GOTOR SPOOL,PARAS,SPOOLD  WRAP UP PRINT CONTROL INTERVAL               
         J     XITR2                                                            
                                                                                
DOSEL24  GOTOR SELCLR                                                           
         MVI   OKNO,11             (SELECTION DISPLAYED)                        
                                                                                
DOSEL26  LA    R3,LISTDIR                                                       
         SR    R0,R0                                                            
         IC    R0,LISTNUM                                                       
                                                                                
DOSEL28  CLI   0(R3),0             SEE IF THERE'S ANY MORE TO COME              
         BH    DOSEL52                                                          
         AHI   R3,6                                                             
         BCT   R0,DOSEL28          NO                                           
         MVI   OKNO,12             (LAST SELECTION)                             
         B     DOSEL52                                                          
                                                                                
DOSEL30  MVI   0(R3),CHASELQ       DISPLAYING FOR CHANGE                        
         MVI   OKNO,4              (RECORD DISPLAYED)                           
         B     DOSEL34                                                          
                                                                                
DOSEL32  MVI   0(R3),DELSELQ       DISPLAYING FOR DELETE                        
         MVI   OKNO,24             (REC DISPLAYED - ENTER TO DELETE)            
                                                                                
DOSEL34  MVI   MODE,DISPKEY                                                     
         GOTOR GOOVER                                                           
         MVC   TWAKEYSV,KEY                                                     
         TM    GENSTAT4,USEBIGKY   USE BIGKEY                                   
         BNO   *+10                                                             
         MVC   TWAKEYSV,BIGKEY                                                  
         MVI   MODE,DISPREC                                                     
         GOTOR GOOVER                                                           
         L     R2,AFRSTREC                                                      
         B     DOSEL52                                                          
                                                                                
DOSEL36  TM    GENSTAT5,GENSELVR   ALWAYS GO TO VALREC                          
         BO    DOSEL38                                                          
         OC    AFRSTREC,AFRSTREC   ARE THERE ANY FIELDS TO CHANGE               
         BZ    DOSEL40                                                          
         L     R1,SYSPARMS         SEE IF ANY CHANGES WERE MADE                 
         L     RE,0(R1)            A(TIOB)                                      
         CLI   SYSTEM,CTLLETQ      TEST CONTROL SYSTEM                          
         BNE   *+8                 NO                                           
         L     RE,28(R1)           SYSPARMS IS DIFFERENT                        
         L     R2,ATWA                                                          
         USING TIOBD,RE                                                         
         AH    R2,TIOBLAST         A(LAST FIELD INPUT)                          
         DROP  RE                                                               
         C     R2,AFRSTREC         IF IT'S ON/AFTER 1ST DATA FIELD              
         BL    DOSEL40                                                          
                                                                                
DOSEL38  MVI   MODE,VALREC         SET VALIDATE RECORD MODE                     
         GOTOR DOCHA               AND GO HANDLE CHANGES                        
                                                                                
DOSEL40  TM    GENSTAT2,RETEQSEL                                                
         BZ    DOSEL42                                                          
         NI    GENSTAT2,FF-(RETEQSEL)                                           
         L     R2,AFRSTREC                                                      
         MVI   OKNO,5              (RECORD CHANGED)                             
         B     DOSEL52                                                          
                                                                                
DOSEL42  GOTOR SELCLR              GO AND GET ANOTHER SELECTION                 
         B     DOSEL02                                                          
                                                                                
DOSEL44  CLI   WHEN,WOK$NO         TEST PRINT OPTION 'NO'                       
         BE    DOSEL50             YES - IGNORE SELECTION                       
         GOTOR DOREP,1             GENERATE REPORT REQUEST                      
         L     RF,EFHWHEN          RF=A(PRINT FIELD HEADER)                     
         TM    WHEN,WOK$SCR        IF REPORT IS ON SCREEN                       
         BZ    DOSEL46                                                          
         MVC   FLDHDRL(#NEXTLQ,RF),LP@NEXT                                      
         OI    FLDOINDD(RF),FOUTTRN                                             
         J     XITR2               RETURN TO USER NOW                           
                                                                                
DOSEL46  L     RE,AREPINFO         A(REPORT ID INFO TABLE)                      
         SR    R1,R1                                                            
         IC    R1,SELLISTN         RELATIVE SELECT NO.                          
         MHI   R1,6                DISP TO THIS SELECTION IN TABLE              
         AR    RE,R1               A(THIS SELECTION)                            
                                                                                
         CLI   OKNO,22             TEST 'NO DATA GENERATED'                     
         BNE   *+12                NO                                           
         MVI   0(RE),C'X'                                                       
         B     DOSEL50                                                          
                                                                                
         MVC   0(1,RE),8(RF)       'WHEN' CODE (N, S, O, D)                     
         MVC   1(3,RE),REMUSER     3-CHAR REQUESTOR                             
         CLI   3(RE),C' '          PAD WITH ASTERISKS                           
         BNE   *+8                                                              
         MVI   3(RE),C'*'                                                       
         CLI   2(RE),C' '                                                       
         BNE   *+8                                                              
         MVI   2(RE),C'*'                                                       
         CLI   1(RE),C' '                                                       
         BNE   *+8                                                              
         MVI   1(RE),C'*'                                                       
         CLI   TWAWHEN,TWW$OV      TEST 'OVERNIGHT'                             
         BE    DOSEL50             YES                                          
         CLI   TWAWHEN,TWW$NOW     TEST 'NOW'                                   
         BNE   *+14                NO                                           
         MVC   4(2,RE),SPOOLRPN    REPORT NUMBER                                
         B     DOSEL50                                                          
         CLI   TWAWHEN,TWW$UPSO    UPDATIVE SOON?                               
         BE    DOSEL48                                                          
         CLI   TWAWHEN,TWW$SOON    TEST 'SOON'                                  
         BE    DOSEL48                                                          
         DC    H'0'                                                             
                                                                                
DOSEL48  MVC   4(2,RE),HALF        REPORT NUMBER                                
                                                                                
DOSEL50  GOTOR SWTOCON             SWITCH TO CONTROL SYSTEM                     
         BE    DOSEL42                                                          
         DC    H'0'                                                             
                                                                                
DOSEL52  TM    GENSTAT2,NEXTSEL    UNLESS REQUESTED TO DO OTHERWISE             
         JZ    XITR2               RETURN CONTROL TO USER                       
         NI    GENSTAT2,FF-(NEXTSEL+RETEQSEL)                                   
         GOTOR SELCLR              CLEAR THIS SELECTION                         
         B     DOSEL02             APPLIC. WANTS NEXT SELECTION                 
                                                                                
SELCLR   TM    GENSTAT2,RETEQSEL   UNLESS REQUESTED TO DO OTHERWISE             
         BO    *+8                                                              
         MVI   0(R3),0             CLEAR THIS SELECTION                         
         NI    GENSTAT2,FF-(RETEQSEL)                                           
         BR    RE                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* MAINTENANCE ROUTINES - LIST                                         *         
***********************************************************************         
                                                                                
DOLST    NTR1  BASE=*,LABEL=*                                                   
         CLI   TWALACT,ACTSEL      WAS THE LAST ACTION SELECT?                  
         BNE   DOLST02                                                          
         CLC   TWALREC,RECNUM      (UNLESS CHANGE OF RECORD)                    
         BNE   DOLST02                                                          
         GOTOR RESLST              YES - NEED TO RESTORE LIST SCREEN            
         BNE   DOLST02             UNLESS NONE SAVED                            
         GOTOR XMTSCR                                                           
         B     DOLST04                                                          
                                                                                
DOLST02  CLI   LISTSW,0                                                         
         BNE   DOLST04                                                          
         GOTOR TSTKEY              IF KEY GOT INPUT AGAIN                       
         BNE   DOLST04                                                          
         MVI   LISTSW,LISTSFST     CONSIDER THIS TO BE FIRST                    
         XC    VERYFRST,VERYFRST                                                
                                                                                
DOLST04  MVI   TWALACT,ACTLIST                                                  
         CLI   LISTSW,LISTSFST     ADJUST D/A DIRECTORIES FOR SPECIAL           
         BE    DOLST16             LIST FUNCTIONS                               
         CLI   LISTSW,LISTSLST                                                  
         BE    DOLST14                                                          
         CLI   LISTSW,LISTSNXT                                                  
         BE    DOLST06                                                          
         CLI   LISTSW,LISTSCUR                                                  
         BE    DOLST18                                                          
                                                                                
         GOTOR ANYCHG              IF ANY CHANGES MADE TO LIST DATA             
         BNE   *+16                                                             
         GOTOR VALLST              HANDLE VALIDATION OF LISTED RECORDS          
         MVI   LISTSW,LISTSCUR     SET TO RE-DISPLAY THIS PAGE                  
         B     DOLST04                                                          
                                                                                
         GOTOR ANYSEL                                                           
         BE    DOLST28             GO HANDLE SELECTED RECORDS                   
         MVI   LISTSW,LISTSNXT                                                  
                                                                                
DOLST06  L     RF,AREPINFO         TEST ANY REPORT INFO SAVED                   
         OC    0(L'REPINFO,RF),0(RF)                                            
         BZ    DOLST12             NO                                           
         L     R2,AFRSTREC                                                      
                                                                                
DOLST08  GOTOR BUMP                BUMP TO LISTAR AREA                          
         LA    R1,58(R2)           BUMP PAST HEADER, NAME, DESCRIPTION          
         XC    0(11,R1),0(R1)      CLEAR EXISTING DATA ON SCREEN                
         CLI   0(RF),0             TEST ANY SAVED INFO FOR THIS ITEM            
         BE    DOLST10             NO                                           
         CLI   0(RF),C'X'          TEST 'NO DATA GENERATED'                     
         BNE   *+14                NO                                           
         MVCDD 0(#NODATLQ,R1),GE#NODAT                                          
         B     DOLST10                                                          
                                                                                
         MVC   0(1,R1),0(RF)       MOVE IN 'WHEN' INDICATOR                     
         MVI   1(R1),C'/'                                                       
         MVC   2(3,R1),1(RF)       REQUESTOR INITIALS                           
         CLC   0(1,RF),LP@O        TEST OVERNIGHT                               
         BE    DOLST10             YES                                          
         MVI   5(R1),C','          FILL IN REPORT NUMBER                        
         EDIT  (2,4(RF)),(5,6(R1)),ALIGN=LEFT                                   
                                                                                
DOLST10  AHI   RF,6                BUMP TO NEXT ENTRY IN REPORT TABLE           
         GOTOR BUMP                BUMP TO NEXT SELECT FIELD                    
         CLI   FLDLEND(R2),FLDHDRL+1                                            
         BH    DOLST08             YES                                          
         L     RF,AREPINFO         CLEAR SAVED REPORT INFO                      
         XC    0(L'REPINFO,RF),0(RF)                                            
         L     R2,AFRSTREC                                                      
         MVI   OKNO,9              LIST DISPLAYED - SELECT OR HIT ENTER         
         CLI   PQSW,2              TEST PQ HAS BEEN OPENED                      
         JNE   XITR2               NO                                           
         MVI   PQSW,FF                                                          
         MVI   SPMODE,FF                                                        
         GOTOR SPOOL,PARAS,SPOOLD  WRAP UP PRINT CONTROL INTERVAL               
         J     XITR2                                                            
                                                                                
DOLST12  MVC   PAGEDAS,PAGEDAS+4   NEXT                                         
         MVC   PAGEDAS+60(4),LASTLIST                                           
         B     DOLST18                                                          
                                                                                
DOLST14  MVC   WORK,PAGEDAS        LAST                                         
         MVC   PAGEDAS+4(60),WORK                                               
         XC    PAGEDAS(4),PAGEDAS                                               
         XC    LASTSELK,LASTSELK                                                
         B     DOLST18                                                          
                                                                                
DOLST16  XC    PAGEDAS,PAGEDAS     FIRST                                        
         XC    LASTSELK,LASTSELK                                                
         MVC   PAGEDAS+60(4),VERYFRST                                           
                                                                                
DOLST18  XC    LISTDIR(108),LISTDIR                                             
         MVI   LISTNUM,0                                                        
         L     RE,AREPINFO         CLEAR SAVED REPORT INFO                      
         XC    0(L'REPINFO,RE),0(RE)                                            
         MVI   THISLSEL,0          ENSURE NO SELECT CODE AROUND                 
         XC    KEY,KEY                                                          
         XC    BIGKEY,BIGKEY                                                    
         TM    GENSTAT3,MULTFILS   IF SYSTEM HAS MULTIPLE FILES                 
         BZ    *+12                                                             
         MVI   MODE,SETFILE        GIVE APPLIC. A CHANCE TO SWITCH              
         GOTOR GOOVER                                                           
         CLI   USEIO,YESQ                                                       
         BNE   DOLST20                                                          
         OC    LASTSELK,LASTSELK                                                
         BZ    DOLST22                                                          
         MVC   KEY,LASTSELK        RESTORE FOR IS FILES                         
         OI    DMINBTS,X'08'       IN CASE LAST RECORD WAS JUST DELETED         
         GOTOR READ                                                             
         NI    DMINBTS,X'F7'                                                    
         B     DOLST22                                                          
                                                                                
DOLST20  OC    PAGEDAS+60(4),PAGEDAS+60                                         
         BZ    DOLST22                                                          
         LA    R4,KEY              SET R4 = A(KEY)                              
         TM    GENSTAT4,USEBIGKY   FOR LARGE KEYS USE BIGKEY                    
         BZ    *+8                                                              
         LA    R4,BIGKEY                                                        
         LR    R1,R4               RESTORE THIS RECORD                          
         AH    R1,LKEY                                                          
         AH    R1,LSTATUS                                                       
         MVC   0(4,R1),PAGEDAS+60                                               
                                                                                
         OI    DMINBTS,X'08'                                                    
         GOTOR GETREC                                                           
         L     R6,AIO                                                           
         LH    R1,LKEY                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R6)               AND KEY                              
         MVI   MODE,LISTKEY                                                     
         GOTOR GOOVER                                                           
         GOTOR READ                                                             
         NI    DMINBTS,X'F7'                                                    
         CLI   LISTSW,LISTSNXT                                                  
         BNE   DOLST22                                                          
         GOTOR SEQ                                                              
                                                                                
DOLST22  CLI   GCMODE,GCMPROGQ     DON'T LOAD IF PROG REC                       
         BE    *+8                                                              
         GOTOR REPLOAD                                                          
         L     R2,AFRSTREC                                                      
         GOTOR TSTSEL              IF SELECT FIELD PRESENT                      
         BNE   *+8                                                              
         GOTOR BUMP                BUMP PAST IT                                 
         ST    R2,ATHISLST         NOTE FIRST LIST                              
         MVI   MODE,LISTRECS                                                    
         GOTOR GOOVER                                                           
         XC    LASTLIST,LASTLIST                                                
         XC    LASTSELK,LASTSELK                                                
         SR    R4,R4                                                            
         IC    R4,NLISTS           CLEAR FIELDS REMAINING FIELDS                
         SR    R0,R0                                                            
         IC    R0,LISTNUM                                                       
         SR    R4,R0               R4=NUMBER OF FIELDS LEFT                     
         BNP   DOLST26                                                          
         L     R2,ATHISLST         R2=A(1ST REMAINING LINE)                     
                                                                                
DOLST24  LR    R3,R2                                                            
         AH    R3,LLIST            R3=A(1ST FIELD ON NEXT LINE)                 
         GOTOR CLRFLD              CLEAR ALL FIELDS ON THIS LINE                
         GOTOR BUMP                                                             
         CR    R2,R3                                                            
         BL    *-10                                                             
         GOTOR TSTSEL              IF SELECT FIELD PRESENT                      
         BNE   *+8                                                              
         GOTOR BUMP                BUMP PAST IT                                 
         BCT   R4,DOLST24                                                       
                                                                                
DOLST26  MVI   OKNO,16             END OF LIST - HIT ENTER FOR ...              
         L     R2,ATHISLST                                                      
         GOTOR BUMP                                                             
         GOTOR TSTSEL              TEST IF SELECT FIELD PRESENT                 
         BNE   *+8                                                              
         MVI   OKNO,10             END OF LIST - SELECT OR HIT ENTER...         
         TM    GLSTSTAT,CHNGLIST   IF CHANGES ALLOWED ON LIST SCREEN            
         BZ    *+8                                                              
         MVI   OKNO,33             END OF LIST - INPUT CHANGES OR ...           
         L     R2,AFRSTREC                                                      
         J     XITR2                                                            
                                                                                
DOLST28  GOTOR SAVLST              USER SELECTED SOME RECORD(S)                 
         L     R1,EFHACT                                                        
         MVC   FLDHDRL(#SELPLQ,R1),LP@SELP                                      
         MVI   FLDILEND(R1),#SELPLQ                                             
         OI    FLDOINDD(R1),FOUTTRN                                             
         MVI   TWALACT,ACTSEL                                                   
         J     XITL                GO BACK TO LOAD SCREEN & APPLIC              
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* MAINTENANCE ROUTINES - REPORT                                       *         
***********************************************************************         
                                                                                
DOREP    NTR1  BASE=*,LABEL=*                                                   
         LTR   R1,R1                                                            
         BZ    DOREP02                                                          
         BM    DOREP06                                                          
         L     R2,EFHACT                                                        
         MVC   FLDHDRL(#REPLQ,R2),LP@REP                                        
         MVI   FLDILEND(R2),#REPLQ                                              
                                                                                
DOREP02  CLI   GCMODE,GCMPROGQ     IF PROG RECS REPORT. . .                     
         BNE   DOREP04             . . . THEN LET USER VALIDATE REQUEST         
         MVI   MODE,VALREC                                                      
         GOTOR GOOVER                                                           
                                                                                
DOREP04  CLI   TWAWHEN,TWW$NOW     IF OVERNIGHT OR SOON WAS REQUESTED           
         BE    DOREP26             WRITE OUT REQUESTS NOW                       
         CLI   OFFLINE,YESQ        UNLESS WE ARE OFFLINE NOW                    
         BE    DOREP26                                                          
                                                                                
DOREP06  XC    IO(26),IO           ENTER HERE TO JUST GENERATE REQUEST          
         MVC   IO+26(80),SPACES                                                 
         LA    R4,IO+26                                                         
         MVC   0(2,R4),QCRDCODE    COMPLETE REQUEST                             
*&&US*&& MVC   2(2,R4),AGENCY                                                   
*&&UK*&& MVC   2(2,R4),TWAAGY                                                   
         CLI   RCPROG,C'A'         FOR ACCOUNTING SYSTEMS                       
         BNE   *+8                                                              
         MVI   3(R4),C'*'          SHOW THIS IS A SPOOL REQUEST                 
         GOTOR GETFACT,DMCB,0                                                   
         L     R1,DMCB                                                          
         ICM   R0,15,FASIN-FACTSD(R1)                                           
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  IO+31(6),DUB                                                     
         SR    R1,R1                                                            
         IC    R1,REQSEQNO                                                      
         AHI   R1,1                                                             
         STC   R1,REQSEQNO                                                      
         LA    R1,SEQTAB(R1)                                                    
         MVC   IO+37(1),0(R1)                                                   
         LA    R4,IO                                                            
         USING REQOFFC,R4                                                       
         MVC   REQOUT,TWAOUT                                                    
         MVC   REQDEST,TWADEST                                                  
                                                                                
         USING SPOOK,BLOCK         SET SPOOK FOR SOON PROCESSING                
         XC    SPOOK(SPOOKXL),SPOOK                                             
         MVC   SPOOKUID,TWAORIG                                                 
         MVC   SPOOKDES,TWADEST                                                 
         MVC   SPOOKTID,TWATRM                                                  
         MVC   SPOOKSEN,SYSPHASE+1                                              
         MVC   SPOOKERN,GETMSYS                                                 
         MVC   SPOOKAGY,TWAAGY                                                  
         L     RE,SYSPARMS                                                      
         MVC   SPOOKAGX,0(RE)                                                   
         MVC   SPOOKDID,REMUSER                                                 
         MVC   SPOOKSYS,RCPROG                                                  
         MVC   SPOOKEOD,RCPROG+2                                                
         MVC   SPOOKJCL,QCRDCODE                                                
         MVC   SPOOKPR1,REQPRI1                                                 
         MVC   SPOOKPR2,REQPRI2                                                 
         MVC   SPOOKSML,REQSML                                                  
         MVC   SPOOKWEN,TWAWHEN                                                 
         MVC   SPOOKXT,=C'XT='                                                  
         MVC   SPOOKTY,REQRTYP                                                  
         CLI   SPOOKDID+2,C' '                                                  
         BNE   DOREP08                                                          
         MVI   SPOOKDID+2,C'*'                                                  
         CLI   SPOOKDID+1,C' '                                                  
         BNE   DOREP08                                                          
         MVI   SPOOKDID+1,C'*'                                                  
         CLI   SPOOKDID+0,C' '                                                  
         BNE   DOREP08                                                          
         XC    SPOOKDID,SPOOKDID                                                
                                                                                
DOREP08  CLI   TWAOUT,C'@'                                                      
         BNE   DOREP10                                                          
         MVC   SPOOKSQL,TWAOUT+1                                                
         OI    SPOOKTY,X'08'                                                    
         B     DOREP12                                                          
                                                                                
DOREP10  CLI   TWAOUT,C'/'                                                      
         BNE   DOREP12                                                          
         CLC   TWAOUT+3(3),SPACES                                               
         BNE   *+14                                                             
         MVC   SPOOKSUB,TWAOUT+1   /XX INPUT                                    
         B     DOREP12                                                          
         MVC   SPOOKSUB,TWAOUT+4   /SPPXX INPUT                                 
         B     DOREP12                                                          
                                                                                
DOREP12  OC    ARFPBLK,ARFPBLK     IF RFP REQUESTED                             
         BZ    DOREP24                                                          
         L     R2,EFHWHEN          SCAN FOR REQUESTOR INITIALS                  
         GOTOR SCANNER,DMCB,(R2),(1,WORK),X'6B7EFF6B'                           
         SR    RE,RE                                                            
         ICM   RE,1,WORK+1         GET REQUESTOR LENGTH                         
         BNZ   DOREP14                                                          
         LA    RF,FLDHDRL(R2)                                                   
         CLI   0(RF),C' '          INSERT COMMA                                 
         BNH   *+12                                                             
         AHI   RF,1                                                             
         B     *-12                                                             
         MVI   0(RF),C','                                                       
         LHI   RF,4                AND LEAVE ROOM FOR 3 CHARS                   
         B     *+8                                                              
DOREP14  LHI   RF,3                MAX REQUESTOR LENGTH                         
         SR    RF,RE               EXCESS TO BE ADDED                           
         IC    RE,FLDILEND(R2)                                                  
         AR    RE,RF               NEW LENGTH OF WHEN FIELD                     
         STC   RE,FLDILEND(R2)                                                  
                                                                                
         ICM   RE,15,EFHDEST       PRE-FILL DESTINATION FIELD                   
         BZ    DOREP16                                                          
         SR    RF,RF                                                            
         IC    RF,FLDLEND(RE)                                                   
         SHI   RF,FLDHDRL+1                                                     
         TM    FLDATBD(RE),FATBXHDR                                             
         BZ    *+8                                                              
         SHI   RF,FLDHDRL                                                       
         STC   RF,WORK+00                                                       
         MVC   WORK+01(19),FLDHDRL(RE)                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDHDRL(0,RE),DSTLIT                                             
         AHI   RF,1                                                             
         STC   RF,FLDILEND(RE)                                                  
                                                                                
DOREP16  ICM   RE,15,EFHOUT        PRE-FILL OUTPUT FIELD                        
         BZ    DOREP18                                                          
         SR    RF,RF                                                            
         IC    RF,FLDLEND(RE)                                                   
         SHI   RF,FLDHDRL+1                                                     
         TM    FLDATBD(RE),FATBXHDR                                             
         BZ    *+8                                                              
         SHI   RF,FLDHDRL                                                       
         STC   RF,WORK+20                                                       
         MVC   WORK+21(19),FLDHDRL(RE)                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDHDRL(0,RE),OUTLIT                                             
         AHI   RF,1                                                             
         STC   RF,FLDILEND(RE)                                                  
                                                                                
DOREP18  GOTOR REQTWA,DMCB,TWAD,IO,ARFPBLK,(X'80',ACOMFACS),SPOOK,     *        
               EFHWHEN             ADD TO REQUEST FILE                          
         ICM   R2,15,EFHDEST                                                    
         BZ    DOREP20                                                          
         GOTOR CLRFLD                                                           
         SR    RF,RF                                                            
         IC    RF,WORK+00                                                       
         EX    RF,*+8                                                           
         B     DOREP20                                                          
         MVC   FLDHDRL(0,R2),WORK+01                                            
                                                                                
DOREP20  ICM   R2,15,EFHOUT                                                     
         BZ    DOREP22                                                          
         GOTOR CLRFLD                                                           
         IC    RF,WORK+20                                                       
         EX    RF,*+8                                                           
         B     DOREP22                                                          
         MVC   FLDHDRL(0,R2),WORK+21                                            
                                                                                
DOREP22  L     RE,ARFPBLK          POINT TO RFP CONTROL BLOCK                   
         CLI   RFPERROR-RFPBLK(RE),RFPNOERR OKAY IF NO ERRORS                   
         BNE   *+12                                                             
         MVI   OKNO,131            GROUP UPDATED WITH REQUEST                   
         J     XITY                DONE                                         
                                                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         MVI   GTMSYS,FF           GENERAL MSG SYSTEM                           
         OI    GENSTAT2,USGETTXT                                                
         CLI   RFPERROR-RFPBLK(RE),RFPNOROO   MAX REQUESTS                      
         BNE   *+12                                                             
         MVI   GTMSGNO1,251        NO ROOM IN GROUP RECORD                      
         J     XITH                                                             
                                                                                
         CLI   RFPERROR-RFPBLK(RE),RFPNOADD   GROUP SUBMITTED?                  
         BNE   DOREP24                                                          
         MVI   GTMSGNO1,254        CAN'T ADD REQS TO SUBMITTED GROUP            
         J     XITH                                                             
         DROP  R1                                                               
                                                                                
DOREP24  GOTOR REQTWA,DMCB,TWAD,IO,DATAMGR,ACOMFACS,SPOOK                       
         MVI   OKNO,20             REP WILL BE PROCESSED O/NIGHT                
         CLI   TWAWHEN,TWW$UPSO    UPDATIVE SOON?                               
         BE    *+12                                                             
         CLI   TWAWHEN,TWW$SOON                                                 
         JNE   XITY                                                             
         CLI   8(R1),X'FE'                                                      
         BNE   *+12                                                             
         MVI   ERROR,TQUEFULL      TERMINAL QUEUE IS FULL                       
         J     XITH                                                             
                                                                                
         CLI   8(R1),FF                                                         
         BNE   *+12                                                             
         MVI   ERROR,PQUEFULL PRINT QUEUE FULL                                  
         J     XITH                                                             
                                                                                
         MVI   OKNO,21             REPORT &T WILL BE PROCESSED SOON             
         L     RE,8(R1)            GET ADDRESS OF PRTQUE KEY                    
         MVC   BLOCK(3),2(RE)      BUILD XXX,9999 FOR &T SUBSTITUTION           
         SR    R0,R0                                                            
         ICM   R0,3,6(RE)                                                       
         BNZ   *+12                                                             
         MVI   ERROR,NOJCL         JCL NOT FOUND                                
         J     XITH                                                             
                                                                                
         STH   R0,HALF             SAVE REPORT NUMBER IN HALF                   
         LA    R4,BLOCK                                                         
         MVI   3(R4),C','                                                       
         EDIT  (R0),(5,4(R4)),ALIGN=LEFT                                        
         LA    R1,GETTXTCB         SET A(TEXT) AND L'TEXT (XXX,99999)           
         STCM  R4,7,GTATXT-GETTXTD(R1)                                          
         MVI   GTLTXT-GETTXTD(R1),9                                             
         J     XITY                                                             
                                                                                
DOREP26  MVC   AOVERLAY,AGO                                                     
         CLI   PHSPECS,0           DO WE NEED TO LOAD A SPEC PHASE              
         BE    DOREP28                                                          
         MVC   OVERLAY,PHSPECS                                                  
         GOTOR LOADEM,DMCB,0       YES                                          
         ST    R3,SPECS                                                         
                                                                                
DOREP28  GOTOR REPLOAD             LOAD REPORT PHASE IF NEEDED                  
         MVC   BUFFALO,TWAVBUFF    XTRNS FOR OFF-LINE VERSIONS                  
         MVC   SORTER,TWAVSORT                                                  
         MVC   WORKER,TWAVWORK                                                  
         ICM   RE,15,TWAVBOX       REQUIRE DICTIONARY SUPPORT                   
         BZ    DOREP30                                                          
*&&UK*&& OI    BOXDDCTL-BOXD(RE),BOXDDREQ+BOXDDLC                               
*&&US*&& OI    BOXDDCTL-BOXD(RE),BOXDDREQ                                       
         ICM   R1,15,TWAMASTC                                                   
         BZ    DOREP30                                                          
         MVC   BOXSYS-BOXD(,RE),MCOVSYS-MASTD(R1)                               
         MVC   BOXLANG-BOXD(,RE),MCLANG-MASTD(R1)                               
         MVC   BOXCTRY-BOXD(,RE),MCCTRY-MASTD(R1)                               
                                                                                
DOREP30  CLI   TWAFIRST,0          FIRST TIME HOOK                              
         BNE   DOREP34                                                          
         ICM   RF,15,TWAMASTC      IF A(MASTER) RESOLVED (OFFLINE)              
         BZ    DOREP32                                                          
         USING MASTD,RF                                                         
         TM    MCPRTIND,MCPRTINL   AND LOGOS DEFERRED/DISABLED                  
         BZ    DOREP32                                                          
         GOTOR MCVLOGO,DMCB,MCVLOGOC  ATTEMPT TO DO THEM NOW                    
         DROP  RF                                                               
                                                                                
DOREP32  CLI   FRSTLAST,NOQ        UNLESS SUPPRESSED                            
         BE    DOREP34                                                          
         MVI   TWAFIRST,1                                                       
         MVI   MODE,RUNFRST                                                     
         GOTOR GOOVER                                                           
         B     DOREP36                                                          
                                                                                
DOREP34  CLI   TWAFIRST,FF         LAST TIME HOOK                               
         BNE   DOREP36                                                          
         MVI   MODE,RUNLAST                                                     
         GOTOR GOOVER                                                           
         J     XITY                                                             
                                                                                
DOREP36  CLI   PQSW,0              UNLESS USER OPTS TO OPEN PQ                  
         BNE   DOREP38                                                          
         TM    WHEN,WOK$SCR        OR REPORT IS ON SCREEN                       
         BO    DOREP38                                                          
         GOTOR OPENPQ              OPEN PQ                                      
                                                                                
DOREP38  OC    VPRINT,VPRINT                                                    
         BZ    DOREP42                                                          
         L     R1,TWAMASTC                                                      
         MVC   SPOOLRPN,MCREMPQK+5-MASTD(R1)                                    
         GOTOR PRTDET              TEST PRINTING REQUEST DETAILS                
         BNE   DOREP40                                                          
         ICM   R0,B'1111',ABOX                                                  
         BZ    *+8                                                              
         ICM   R0,B'1000',PASSABOX                                              
         GOTOR REQTWA,DMCB,(3,TWAD),('FF',ACOMFACS),VPRINT,(R0)                 
                                                                                
DOREP40  GOTOR TWAPTCHR,DMCB,BASERB,SPECS                                       
                                                                                
DOREP42  L     R2,AFRSTKEY                                                      
         XC    KEY,KEY                                                          
         GOTOR DOPRNT                                                           
         CLI   OFFLINE,YESQ                                                     
         JE    XITY                                                             
         GOTOR CLSREP                                                           
         J     XITY                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD REPORT OVERLAY IF NECESSARY                                    *         
***********************************************************************         
                                                                                
REPLOAD  NTR1  LABEL=NO            LOAD REPORT PHASE IF REQUIRED                
         CLI   TWAFIRST,FF         NOT NEEDED FOR RUNLAST                       
         JE    XIT                                                              
         CLI   PHREPORT,0          LOAD IN REPORT PHASE IF NEEDED               
         JE    REPLOAD2                                                         
         CLC   PHREPORT,PHEDIT     UNLESS IT WAS SAME AS EDIT                   
         JE    REPLOAD2                                                         
         MVC   OVERLAY,PHREPORT                                                 
         GOTOR LOADEM,DMCB,0                                                    
         ST    R3,AGO                                                           
REPLOAD2 ICM   RF,15,TWADCONS                                                   
         JZ    *+10                                                             
         MVC   TAOVER-TWADCOND(,RF),AGO                                         
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD ACTIVITY                                             *         
***********************************************************************         
                                                                                
DISACT   NTR1  BASE=*,LABEL=*                                                   
         MVI   OVERLAY,X'DF'       ACTIVITY - LOAD SCREEN                       
         L     R3,AFRSTRCH                                                      
         GOTOR LOADEM,DMCB,1                                                    
*&&UK*&& GOTOR XMTSCR                                                           
         MVI   TWASCR,X'DF'                                                     
         LR    R2,R3                                                            
         USING ACDD,R2                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'F1'        GET ACTIVITY ELEMENT                         
         GOTOR GETEL                                                            
         BNE   DISACT06                                                         
         USING ACTVD,R6                                                         
                                                                                
         MVC   ACDTXT1(#RECADLQ),LP@RECAD                                       
         LA    R3,ACTVADDT         ADD DETAILS ON LINE 1                        
         GOTOR DISWHO                                                           
         GOTOR BUMP                                                             
         CLI   ACTVCHNM,0          SHOW CHANGES ON LINE 2                       
         BE    DISACT02                                                         
         MVC   ACDTXT1(#CHGNOLQ),LP@CHGNO                                       
         EDIT  (1,ACTVCHNM),(3,ACDCHNO),ALIGN=LEFT                              
         LA    R3,ACTVCHDT                                                      
         GOTOR DISWHO                                                           
                                                                                
DISACT02 GOTOR BUMP                REASONS ON LINE 3                            
         OC    ACTVCHRE,ACTVCHRE                                                
         BZ    DISACT04                                                         
         MVC   CHREASON,ACTVCHRE                                                
         MVI   MODE,XPREASON                                                    
         MVC   WORK,SPACES                                                      
         GOTOR GOOVER                                                           
         CLC   WORK,SPACES                                                      
         BE    DISACT04                                                         
         MVC   ACDTXT1(#CHGRLQ),LP@CHGR                                         
         MVC   ACDCHRSN,WORK                                                    
                                                                                
DISACT04 CLI   TWAOFFC,C'*'        DDS TERMINALS GET MORE                       
         BNE   DISACT06                                                         
         GOTOR BUMP                DISK ADDRESS AND RECORD LENGTH               
         MVC   ACDTXT1(#DSKADLQ),LP@DSKAD                                       
         GOTOR HEXOUT,DMCB,DMDSKADD,ACDDSKAD,4,0                                
         GOTOR BUMP                                                             
         MVC   ACDTXT1(#RECLNLQ),LP@RECLN                                       
         L     R3,AIO                                                           
         AH    R3,LKEY                                                          
         EDIT  (2,0(R3)),(4,ACDRECL),ALIGN=LEFT                                 
                                                                                
DISACT06 MVI   ELCODE,X'F3'        SHOW SECURITY STAMPING                       
         GOTOR NEXTEL                                                           
         BNE   DISACTX                                                          
         GOTOR BUMP                                                             
         USING SECURED,R6                                                       
         MVC   ACDTXT1(#SECLQ),LP@SEC                                           
         CLI   SECURLEV,0                                                       
         BE    DISACT08                                                         
         MVC   ACDTXT1(#SECLVLQ),LP@SECLV                                       
         MVI   ACDTXT1+#SECLVLQ+1,C'='                                          
         EDIT  (1,SECURLEV),(3,ACDSECLV),ALIGN=LEFT                             
                                                                                
DISACT08 OC    SECURID,SECURID                                                  
         BZ    DISACT10                                                         
         MVC   ACDTXT2(#IDLQ),LP@ID                                             
         MVI   ACDTXT2+#IDLQ+1,C'='                                             
         EDIT  (2,SECURID),(4,ACDID),ALIGN=LEFT                                 
                                                                                
DISACT10 OC    SECURPAS,SECURPAS                                                
         BZ    DISACTX                                                          
         MVC   ACDTXT2(#PASSWLQ),LP@PASSW                                       
                                                                                
DISACTX  J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SHOW DATE AND ID OF ACTIVITY                                        *         
***********************************************************************         
                                                                                
DISWHO   NTR1  LABEL=NO                                                         
         GOTOR DATCON,DMCB,(3,(R3)),(8,ACDDATE)                                 
         MVC   ACDTXT2(#BYIDLQ),LP@BYID                                         
         EDIT  (2,3(R3)),(4,ACDIDNO),ALIGN=LEFT                                 
         MVC   AIO,AIO2            GET ID RECORD FROM CONTROL FILE              
         MVC   GLOBKEY,KEY                                                      
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,3(R3)                                                    
         GOTOR CTREAD                                                           
         BNE   DISWHO02                                                         
         L     R6,AIO                                                           
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'02'                                                     
         GOTOR FIRSTEL                                                          
         BNE   DISWHO02                                                         
         USING CTDSCD,R6                                                        
         MVC   ACDID,CTDSC                                                      
                                                                                
DISWHO02 MVC   AIO,AIO1                                                         
         XC    FILENAME,FILENAME                                                
         MVC   USEIO,SAVUSEIO                                                   
         MVC   KEY,GLOBKEY                                                      
         J     XIT                                                              
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK SCREEN FOR SELECTIONS                              *         
***********************************************************************         
                                                                                
ANYSEL   NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0                                                            
         ICM   R0,1,LISTNUM                                                     
         JZ    XITN                                                             
         L     R2,AFRSTREC         FIRST SIMPLY VALIDATE SELECT CODES           
         USING FLDHDRD,R2                                                       
         TM    GENSTAT5,SEL1BYTE   TEST APPL SUPPORTS 1 BYTE SELECT             
         BNZ   ANYSEL02                                                         
         CLI   FLDLEN,FLDHDRL+1    ONE BYTE SELECT FIELDS ARE PHONY             
         JE    XITN                                                             
         TM    GLSTSTAT,NOSELFLD   DON'T BOTHER IF NO SELECT FIELD              
         JO    XITN                                                             
                                                                                
ANYSEL02 TM    FLDATB,FATBPROT     IGNORE PROTECTED FIELDS                      
         BO    ANYSEL10                                                         
         TM    FLDATB,FATBLOW      AND ZERO INTENSITY FIELDS                    
         BO    ANYSEL10                                                         
         CLI   FLDILEN,0           TEST ANY INPUT                               
         BE    ANYSEL10                                                         
                                                                                
         LA    R4,SELTAB           LOOK UP SELECT CODE IN TABLE                 
         USING SELTABD,R4                                                       
         LHI   RE,SELTABN                                                       
ANYSEL04 SR    RF,RF                                                            
         ICM   RF,3,SELTCHAR                                                    
         LA    RF,GEND(RF)                                                      
         CLC   FLDDATA(1),0(RF)                                                 
         BE    *+16                                                             
         AHI   R4,SELTABL                                                       
         BCT   RE,ANYSEL04                                                      
         J     ERRINV                                                           
                                                                                
         CLC   FLDDATA(1),LP@CHAS  IF SELECTING FOR CHANGE                      
         BNE   *+12                                                             
         TM    GENSTAT5,NOCHGLST   AND IF CHANGE FROM LIST NOT ALLOWED          
         JO    ERRINV              THEN GIVE ERROR                              
                                                                                
         CLC   FLDDATA(1),LP@DELS  IF SELECTING FOR DELETE                      
         BNE   ANYSEL06                                                         
         TM    GENSTAT4,NODELLST   AND IF DELETE FROM LIST NOT ALLOWED          
         JO    ERRINV              THEN GIVE ERROR                              
         TM    GENSTAT5,NODLST     OR IF 'D' FROM LIST NOT ALLOWED              
         BZ    ANYSEL06            ONLY 'DE'/'DEL' ALLOWED                      
         CLC   FLDDATA(2),LP@DEL                                                
         BE    ANYSEL06                                                         
         CLC   FLDDATA(3),LP@DEL                                                
         JNE   ERRINV              THEN GIVE ERROR                              
                                                                                
ANYSEL06 TM    GENSTAT3,OKVALSEL   TEST OK TO VALIDATE SELECT CODES             
         BZ    ANYSEL10                                                         
         MVI   BLOCK,X'02'         LOOK UP X'02' (ACTION) ENTRY                 
         SR    RF,RF                                                            
         ICM   RF,3,SELTNAME                                                    
         LA    RF,GEND(RF)                                                      
         MVC   BLOCK+1(3),0(RF)    USE CORRESPONDING ACTION FROM TABLE          
                                                                                
         CLI   LANG,LANGGER        ENGLISH LANGUAGE FIX FOR PRG RECS            
         BNL   ANYSEL08                                                         
         CLI   FLDDATA,C'R'        IF SELECT CODE IS 'R' (IE RESTORE)           
         BNE   ANYSEL08                                                         
         CLI   GCMODE,GCMPROGQ     AND THIS IS PROG RECS                        
         BNE   ANYSEL08                                                         
         MVC   BLOCK+1(#REPLQ),LP@REP   FORCE TO ACTION REPORT                  
         DROP  R4                                                               
                                                                                
ANYSEL08 ICM   R3,15,ARECACT2      ALTERNATIVE START FOR 02 ENTRIES             
         BNZ   *+8                                                              
         L     R3,ARECACT                                                       
         MVI   ERROR,INVACT                                                     
         GOTOR LOOKUP,0            LOOK UP ACTION                               
                                                                                
         L     R3,WORK             RETURNS A(ACTION ENTRY) IN WORK              
         MVC   WORK+2(1),10(R3)    NOW HAVE ACTION EQUATE                       
         SR    R3,R3                                                            
         GOTOR LUPRAC              LOOKUP RECORD/ACTION ENTRY                   
         BE    ANYSEL10                                                         
         MVI   ERROR,INVRCACT      ERROR IF RETURNS CC NOT EQUAL                
         J     ERRXIT                                                           
                                                                                
ANYSEL10 GOTOR BUMP                BUMP TO NEXT FIELD                           
         AH    R2,LLIST            USE L'LINE IF DEFINED TO GET TO NEXT         
         CLI   FLDLEN,0                                                         
         BNE   ANYSEL02                                                         
                                                                                
         L     R2,AFRSTREC         ALL OK - NOW DO IT FOR REAL                  
         LA    R3,LISTDIR                                                       
         SR    R5,R5               COUNT SELECTIONS                             
ANYSEL12 TM    FLDATB,FATBPROT                                                  
         BO    ANYSEL16                                                         
         TM    FLDATB,FATBLOW                                                   
         BO    ANYSEL16                                                         
         CLI   FLDILEN,0                                                        
         BE    ANYSEL14                                                         
         MVC   0(2,R3),FLDDATA     SAVE ACTION AND ARGUMENT                     
         AHI   R5,1                ADD TO SELECTIONS                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN           CALCULATE L'FIELD                            
         SHI   R1,FLDHDRL+1                                                     
         CLI   FLDATB,FF           NOP FIELD - CAN'T HAVE EXTENDED HDR          
         BE    *+16                                                             
         TM    FLDATB,FATBXHDR     TEST FOR EXTENDED HEADER                     
         BNO   *+8                                                              
         SHI   R1,FLDHDRL                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    FLDDATA(0),FLDDATA  TEST IF ANYTHING IN FIELD                    
         BZ    ANYSEL14            DON'T BOTHER IF NOTHING THERE                
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    FLDDATA(0),FLDDATA  CLEAR FIELD                                  
         OI    FLDOIND,FOUTTRN     TRANSMIT IT                                  
         MVI   FLDILEN,0           AND SIMULATE NO INPUT LENGTH                 
                                                                                
ANYSEL14 AHI   R3,6                                                             
         BCT   R0,ANYSEL16                                                      
         LTR   R5,R5                                                            
         JZ    XITN                                                             
         J     XITY                                                             
                                                                                
ANYSEL16 GOTOR BUMP                BUMP TO NEXT TWA FIELD                       
         AH    R2,LLIST            USE L'LINE IF DEFINED TO GET TO NEXT         
         CLI   FLDLEN,0                                                         
         BNE   ANYSEL12                                                         
         J     XITN                                                             
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE RECORD/ACTION COMBINATIONS                      *         
* R3 MAY BE PRESET WITH A(START) - WORK+2 HAS ACTION EQUATE           *         
***********************************************************************         
                                                                                
LUPRAC   NTR1  BASE=*,LABEL=*                                                   
         MVI   WORK,LUPRACQ        FINISH BUILDING SEARCH ARGUMENT              
         MVC   WORK+1(1),RECNUM                                                 
         CLI   GCMODE,GCMPROGQ     TEST PROG RECS                               
         BNE   LUPRAC02                                                         
         OC    ASECBLK,ASECBLK     IF NEW SECURITY IN USE                       
         BNZ   LUPRAC02            THEN VALIDATE REAL RECORD/ACTION             
         TM    GENSTAT4,LEAVEACT   SHOULD WE CHANGE THE ACTION ?                
         BO    LUPRAC02            YES, LEAVE IT AS IS                          
         CLI   ACTEQU,ACTREST      MAINTENANCE ACTION?                          
         BNH   *+12                                                             
         CLI   ACTEQU,ACTLIST      LIST ACTION?                                 
         BNE   LUPRAC02                                                         
         MVI   WORK+2,ACTREP       ACTION IS ALWAYS REPORT                      
                                                                                
LUPRAC02 LTR   R3,R3               TEST R3 PRESET                               
         BNZ   LUPRAC08            BUMP TO NEXT AND KEEP TRYING                 
         ICM   R3,15,ARECACT3      ALTERNATIVE START FOR COMBO TABLE            
         BNZ   LUPRAC04                                                         
         L     R3,ARECACT                                                       
                                                                                
LUPRAC04 CLC   0(3,R3),WORK        NOW LOOK UP COMBO ENTRY                      
         BNE   LUPRAC08                                                         
         GOTOR TSTRAS,(R3)         TEST SECURITY BLOCK                          
         BNE   LUPRAC06                                                         
         CLI   LRECACT,12          IF L'RECACT ENTRY IS GT 12                   
         BNH   LUPRACX                                                          
         OC    SECMASKS,SECMASKS   AND SECURITY BIT MASK(S) DEFINED             
         BZ    LUPRACX                                                          
         ICM   RE,15,12(R3)        AND VALID BITS DEFINED                       
         BZ    LUPRACX             (NOTE THIS ASSUMES L'SECMASKS=4)             
         MVC   DUB(L'SECMASKS),SECMASKS DETERMINE IF VALID FOR MASK             
         NC    DUB(L'SECMASKS),12(R3)                                           
         BNZ   LUPRACX                                                          
LUPRAC06 MVI   ERROR,SECLOCK       NO - SECURITY LOCKOUT                        
         J     ERRXIT                                                           
                                                                                
LUPRAC08 SR    R0,R0                                                            
         IC    R0,LRECACT          BUMP TO NEXT ENTRY IN TABLE                  
         AR    R3,R0                                                            
         CLI   0(R3),FF                                                         
         BNE   LUPRAC04                                                         
         J     XITN                NOT FOUND - RETURN CC NOT EQUAL              
                                                                                
LUPRACX  ST    R3,WORK+4           RETURN A(ENTRY)                              
         J     XITY                AND CC EQUAL                                 
         EJECT                                                                  
***********************************************************************         
* LOAD SCREEN PHASE                                                   *         
***********************************************************************         
                                                                                
LOADSP   NTR1  BASE=*,LABEL=*                                                   
         CLI   GCMODE,GCMPROGQ     CHECK FOR PROG RECORDS LIST                  
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         MVI   PHSCREEN,1                                                       
         CLI   PHSCREEN,0          DO WE NEED A SCREEN                          
         BE    LOADSP12                                                         
         CLC   PHSCREEN,TWASCR     DO WE HAVE IT YET                            
         BE    LOADSP12                                                         
         CLI   ACTNUM,ACTLIST      IF THIS IS A LIST ACTION                     
         BNE   LOADSP04                                                         
         CLI   TWALACT,ACTSEL      AND LAST ACTION WAS SELECT                   
         BNE   LOADSP04                                                         
         CLC   TWALREC,RECNUM      FOR SAME RECORD TYPE                         
         BNE   LOADSP04                                                         
         L     RF,EFHKEY           RF=A(KEY FIELD)                              
         MVC   BLOCK(88),0(RF)     SAVE HEADER AND CONTENTS OF FIELD            
         SR    R1,R1                                                            
         IC    R1,LISTSW           WE NEED TO SAVE SPECIAL SWITCH               
         GOTOR RESLST              WE NEED TO RESTORE LIST SCREEN               
         BNE   LOADSP04            UNLESS NONE SAVED                            
         GOTOR XMTSCR                                                           
         STC   R1,LISTSW           NOW REPLACE SWITCH                           
         ICM   R1,1,BLOCK+5        RESTORE CONTENTS OF KEY FIELD                
         BZ    LOADSP12                                                         
         STC   R1,FLDILEND(RF)                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLDHDRL(0,RF),BLOCK+8                                            
         B     LOADSP12                                                         
                                                                                
LOADSP04 L     R3,EFHTAG           SAVE CURRENT SCREEN FOR KEYMRG               
         LA    R0,IO                                                            
         LHI   R1,L'IO             ENOUGH TO COVER ALL KEY FIELDS               
         LR    RE,R3                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         CLI   GCMODE,GCMPROGQ     CHECK FOR PROG RECORDS LIST                  
         BNE   *+12                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    LOADSP06                                                         
         MVC   TWASCR,PHSCREEN                                                  
         MVC   OVERLAY,PHSCREEN                                                 
         GOTOR LOADEM,DMCB,1                                                    
         CLI   TWACOMMN,1          EXIT NOW IF FIRST SPOOF CALL                 
         JE    XIT                                                              
         TM    GENSTAT4,NEWSCRM    TEST APPLIC WANTS NEW SCREEN PASSED          
         BZ    LOADSP08                                                         
         MVI   MODE,NEWSCR         APPLIC CONTROLLER GETUSER MODE               
         GOTOR GETUSER                                                          
         B     LOADSP08                                                         
                                                                                
LOADSP06 XC    DMCB(12),DMCB       LOAD PROG RECORDS LIST SCREEN                
         GOTOR CALLOV,DMCB,(R3),X'D90FD8FE',0                                   
         OC    9(3,R1),9(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   TWASCR,PHSCREEN                                                  
         MVC   OVERLAY,PHSCREEN                                                 
         MVI   NLISTS,16                                                        
                                                                                
LOADSP08 GOTOR KEYMRG              MERGE IN KEYS FROM SAVED SCREEN              
         GOTOR SETTWA              ESTABLISH ADDRESSES                          
         GOTOR XMTSCR              RETRANSMIT ORIGINAL PART OF SCREEN           
         B     LOADSP12                                                         
                                                                                
LOADSP10 L     R2,EFHKEY                                                        
         ST    R2,DMCB             THEN USE SCUNKEY TO OUTPUT KEYS              
         MVI   DMCB,C','           WE USE COMMAS AS SEPARATOR                   
         CLI   MYSEP,C' '          IF OVERRIDE SEPARATOR DEFINED                
         BNH   *+10                                                             
         MVC   DMCB(1),MYSEP       USE IT                                       
         MVC   DMCB+4(4),AFRSTKEY                                               
         GOTOR SCUNKEY,DMCB                                                     
         GOTOR CLRFLD              CLEAR KEY FIELD                              
         B     LOADSPX                                                          
                                                                                
LOADSP12 GOTOR SETTWA                                                           
         ICM   R2,15,AFRSTKEY      ARE THERE ANY FIELDS                         
         BZ    LOADSPX             NO, SO IGNORE "KEY" FIELD                    
         CLI   ACTNUM,ACTSEL       IGNORE KEY FIELD IF ACTION SELECT            
         BE    *+16                                                             
         L     R1,EFHKEY                                                        
         CLI   FLDILEND(R1),0                                                   
         BNE   LOADSP10            GO BACK FOR SCUNKEY                          
         CLI   OFFLINE,YESQ        DON'T CHECK IF OFFLINE                       
         BE    LOADSPX                                                          
                                                                                
LOADSP14 TM    FLDATBD(R2),FATBPROT                                             
         BNZ   *+12                                                             
         CLI   FLDHDRL(R2),0                                                    
         BNE   LOADSPX                                                          
         GOTOR BUMP                                                             
         BE    LOADSP18                                                         
         C     R2,AFRSTREC         TEST END OF KEY                              
         BL    LOADSP14            FIELDS                                       
         CLC   AFRSTREC,AFRSTKEY   IF ALL FIELDS KEY FIELDS                     
         BE    LOADSP14            CHECK ALL FIELDS                             
                                                                                
LOADSP18 CLI   ACTNUM,ACTSEL       DONT NEED ANY FOR SELECT                     
         BE    LOADSPX                                                          
         CLC   OVERLAY,PHSCREEN    CHECK IF REENTRY-IF NOT ASK                  
         BE    LOADSP20            FOR INPUT                                    
         CLI   ACTNUM,ACTLIST      OK IF LIST                                   
         BE    LOADSPX                                                          
         CLI   ACTNUM,ACTREP       OR REPORT                                    
         BNL   LOADSPX                                                          
                                                                                
LOADSP20 TM    GENSTAT6,GES$LINK   TEST DDLINK UPLOAD IN PROCESS                
         BNZ   LOADSPX                                                          
         L     R2,AFRSTKEY                                                      
         MVI   OKNO,2              WE NEED HELP FROM USER                       
         J     XITN                                                             
                                                                                
LOADSPX  TM    GENSTAT6,GES$LINK   TEST DDLINK UPLOAD IN PROCESS                
         JZ    *+8                                                              
         GOTOR GETKEY              YES - SET KEY FIELDS                         
         J     XITY                                                             
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* LOOK UP RECORD AND ACTION DIRECTORY FROM SELECT CODE               *          
*                                                                    *          
* NTRY:- R2=A(TWA FIELD HEADER)                                      *          
*        R3=A(LOOKUP TABLE)                                          *          
*        DUB=TYPE OF ENTRY TO SEARCH FOR                             *          
*        R1=0 FOR SELECT FIELD LOOKUP, NON-ZERO FOR REGULAR CALL     *          
**********************************************************************          
                                                                                
LOOKUP   NTR1  BASE=*,LABEL=*                                                   
         LTR   R1,R1               TEST SELECT FIELD LOOKUP                     
         BNZ   LOOKUP02                                                         
         MVI   BYTE,1              YES - SET FLAG                               
         LHI   R5,3                L'EX COMPARE (TYPE+3 CHR TEXT)               
         TM    GENSTAT3,USEDICT                                                 
         JZ    LOOKUP06                                                         
         BCTR  R5,0                R5=L'EX COMPARE (3 CHR TEXT)                 
         B     LOOKUP08                                                         
                                                                                
LOOKUP02 MVI   BYTE,0                                                           
         GOTOR ANY                                                              
         CLI   WORK,C'?'           QUESTION MARK = HELP                         
         BNE   LOOKUP04                                                         
         MVC   FLDHDRL(#HELPPLQ,R2),LP@HELPP                                    
         OI    FLDOINDD(R2),FOUTTRN                                             
         MVI   FLDILEND(R2),#HELPPLQ                                            
         MVC   WORK(#HELPPLQ),LP@HELPP                                          
                                                                                
LOOKUP04 MVC   BLOCK(1),DUB                                                     
         MVC   BLOCK+1(8),WORK                                                  
         SR    R5,R5                                                            
         IC    R5,FLDILEND(R2)                                                  
         LR    R0,R5                                                            
         LA    RE,BLOCK+1                                                       
         CLI   0(RE),C' '          VALIDATE UP TO FIRST BLANK                   
         BE    *+12                                                             
         AHI   RE,1                                                             
         BCT   R0,*-12                                                          
         SR    R5,R0                                                            
         TM    GENSTAT3,USEDICT    R5=EX'LEN OF KEYWORD IF DICT REF             
         BZ    LOOKUP06                                                         
         BCTR  R5,0                                                             
                                                                                
LOOKUP06 TM    GENSTAT3,USEDICT    REC/ACT MAY BE DICTIONARY REFERENCES         
         BNZ   LOOKUP08                                                         
         EX    R5,*+8                                                           
         BNE   LOOKUP12                                                         
         CLC   0(0,R3),BLOCK       MATCH ON TYPE/DATA                           
         MVC   DUB(8),1(R3)                                                     
         B     LOOKUP14                                                         
                                                                                
LOOKUP08 CLC   0(1,R3),BLOCK       MATCH ON TYPE FIRST                          
         BNE   LOOKUP12                                                         
         CLI   BLOCK+1,GE#ESCL2    TEST INPUT IS AN ESCAPE SEQUENCE             
         BL    LOOKUP10                                                         
         CLI   BLOCK+1,GE#ESCL                                                  
         BH    LOOKUP10                                                         
         CLC   1(3,R3),BLOCK+1     YES - MATCH ON 3 BYTES                       
         BE    LOOKUP14                                                         
         B     LOOKUP12                                                         
                                                                                
LOOKUP10 MVC   DUB(8),1(R3)        EXTRACT DICTIONARY REF                       
         GOTOR DICTATE,DMCB,C'SU  ',DUB,0                                       
         EX    R5,*+8                                                           
         BE    LOOKUP14                                                         
         CLC   DUB(0),BLOCK+1                                                   
                                                                                
LOOKUP12 SR    R0,R0                                                            
         IC    R0,LRECACT                                                       
         AR    R3,R0                                                            
         CLI   0(R3),FF                                                         
         BNE   LOOKUP06                                                         
         CLI   BLOCK,X'04'         IF LOOKING FOR PROGRAM RECORDS               
         JE    XITN                RETURN CC NOT EQUAL                          
         J     ERRXIT              ELSE THIS IS AN ERROR                        
                                                                                
LOOKUP14 GOTOR TSTRAS,(R3)         TEST SECURITY BLOCK                          
         BNE   LOOKUP12                                                         
         CLI   LRECACT,12          IF L'RECACT ENTRY IS GT 12                   
         BNH   LOOKUP16                                                         
         OC    SECMASKS,SECMASKS   AND SECURITY BIT MASK(S) DEFINED             
         BZ    LOOKUP16                                                         
         ICM   RE,15,12(R3)        AND VALID BITS DEFINED                       
         BZ    LOOKUP16            (NOTE THIS ASSUMES L'SECMASKS=4)             
         MVC   WORK(L'SECMASKS),SECMASKS DETERMINE IF VALID FOR MASK            
         NC    WORK(L'SECMASKS),12(R3)                                          
         BZ    LOOKUP12                                                         
                                                                                
LOOKUP16 CLI   BYTE,0              IF THIS IS NOT A SELECT FIELD                
         BNE   LOOKUPX                                                          
         CLI   BLOCK+1,GE#ESCL2    TEST INPUT IS AN ESCAPE SEQUENCE             
         BL    *+12                                                             
         CLI   BLOCK+1,GE#ESCL                                                  
         BNH   LOOKUPX                                                          
         CLC   FLDHDRL(8,R2),DUB                                                
         BE    LOOKUPX             SET IT IF IT'S NOT THERE ALREADY             
         MVC   FLDHDRL(8,R2),DUB                                                
         OI    FLDOINDD(R2),FOUTTRN                                             
         CLI   FLDILEND(R2),3                                                   
         BNL   LOOKUPX                                                          
         MVI   FLDILEND(R2),3                                                   
                                                                                
LOOKUPX  ST    R3,WORK                                                          
         J     XITY                                                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* PASS CONTROL TO OVERLAY                                             *         
***********************************************************************         
                                                                                
GOOVER   NTR1  BASE=*,LABEL=*                                                   
         CLI   GCMODE,GCMPROGQ     HANDLE PROGRAM RECORDS INTERNALLY            
         BNE   GOOVER02                                                         
         CLI   ACTNUM,ACTREP                                                    
         BE    GOOVER02                                                         
         CLI   ACTNUM,ACTSEL       TEST REPORT WAS SELECTED                     
         BNE   *+12                                                             
         CLI   THISLSEL,REPSELQ                                                 
         BE    GOOVER02                                                         
                                                                                
         GOTOR GOPROGS,DMCB,GEND                                                
         MVC   USEIO,SVUSEIO                                                    
         MVC   SYSDIR,SVSYSDIR                                                  
         MVC   SYSFIL,SVSYSFIL                                                  
         MVC   DATADISP,SVDATADI                                                
         MVC   LKEY,SVLKEY                                                      
         MVC   FILENAME,SVFILENM                                                
         ICM   R0,B'1000',SVSYS                                                 
         ICM   R0,B'0111',EFFS                                                  
         GOTOR SWITCH,DMCB,(R0),0                                               
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR GOGO                OFF TO APPLICATION                           
         MVC   SVFILENM,FILENAME                                                
         XC    FILENAME,FILENAME                                                
         GOTOR SWTOCON             SWITCH TO CONTROL SYSTEM                     
         JE    XIT                                                              
         DC    H'0'                                                             
                                                                                
GOOVER02 GOTOR GOGO                OFF TO APPLICATION                           
         J     XIT                                                              
                                                                                
GOGO     NTR1  LABEL=NO                                                         
         GOTOR SAVLIO              SAVE THE LIOB IF UPLOADING                   
         LHI   RB,-1                                                            
         LHI   R9,-1                                                            
         GOTOR AGO,DMCB,GEND                                                    
         BASR  R9,0                                                             
         AHI   R9,GLOBALS-*                                                     
         GOTOR RESLIO              RESTORE THE LIOB IF SAVED                    
         J     XIT                                                              
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SEE IF THERE WAS INPUT IN A KEY FIELD                    *         
***********************************************************************         
                                                                                
TSTKEY   NTR1  LABEL=NO                                                         
*&&UK*&& L     R1,EFHOTH                                                        
*&&UK*&& LTR   R1,R1                                                            
*&&UK*&& JZ    *+12                                                             
*&&UK*&& TM    FLDIINDD(R1),FINPTHIS                                            
*&&UK*&& JO    XITY                                                             
         LA    R2,CONHEADH                                                      
         USING FLDHDRD,R2                                                       
                                                                                
TSTKEY02 GOTOR BUMP                BUMP TO NEXT TWA FIELD                       
         JE    XITN                                                             
         TM    FLDATB,FATBPROT     LOOK FOR UNPROTECTED FIELD                   
         JO    TSTKEY02                                                         
         TM    GENSTAT3,IGNNONXK   IGNORE NON-EXTENDED 'KEY' FIELDS             
         JZ    *+12                                                             
         TM    FLDATB,FATBXHDR                                                  
         JZ    TSTKEY02                                                         
                                                                                
         TM    FLDIIND,FINPTHIS    THAT WAS INPUT THIS TIME                     
         JZ    TSTKEY02                                                         
         C     R2,AFRSTKEY         DOES IF FALL BETWEEN FIRST KEY               
         JL    TSTKEY02                                                         
                                                                                
         TM    GENSTAT4,USEAFRCH   USE AFRSTRCH NOT AFRSTREC                    
         JZ    TSTKEY04                                                         
         C     R2,AFRSTRCH         AND FIRST FIELD AFTER KEY                    
         JNL   XITN                                                             
         J     XITY                                                             
                                                                                
TSTKEY04 C     R2,AFRSTREC         AND FIRST RECORD FIELD                       
         JNL   XITN                                                             
         J     XITY                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK LIST SCREEN FOR CHANGES                            *         
***********************************************************************         
                                                                                
ANYCHG   NTR1  LABEL=NO                                                         
         TM    GLSTSTAT,CHNGLIST   TEST CHANGES ALLOWED ON LIST SCREEN          
         JZ    XITN                DON'T BOTHER IF NOT                          
                                                                                
         SR    R0,0                                                             
         IC    R0,LISTNUM          R0=N'LISTED RECORDS                          
         TM    GLSTSTAT,OKTOADD    IF OK TO ADD NEW RECORDS FROM LIST           
         JZ    *+8                                                              
         IC    R0,NLISTS           CHECK ENTIRE SCREEN                          
         LTR   R0,R0                                                            
         JZ    XITN                                                             
         L     R2,AFRSTREC         R2=A(1ST DATA LINE)                          
         USING FLDHDRD,R2                                                       
ANYCHG02 GOTOR TSTSEL              UNLESS THERE'S NO SELECT FIELD               
         JNE   *+12                                                             
         GOTOR BUMP                BUMP PAST IT TO 1ST FIELD                    
         JE    XITN                                                             
         LR    R3,R2                                                            
         AH    R3,LLIST            R3=A(1ST FIELD AFTER THIS DATA LINE)         
                                                                                
ANYCHG04 TM    FLDATB,FATBPROT     IGNORE PROTECTED FIELDS                      
         JNZ   ANYCHG06                                                         
         TM    FLDIIND,FINPVAL     LOOK FOR FIELDS NOT PREV. VALIDATED          
         JZ    XITY                                                             
                                                                                
ANYCHG06 GOTOR BUMP                BUMP TO NEXT FIELD IN RECORD                 
         JE    XITN                                                             
         CR    R2,R3               TEST PAST END                                
         JL    ANYCHG04            NO                                           
         J     ANYCHG02            YES - BUMP TO NEXT RECORD                    
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE VALIDATION OF LISTED RECORDS                      *         
***********************************************************************         
                                                                                
VALLST   NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0                                                            
         IC    R0,LISTNUM          R0=N'LISTED RECORDS                          
         TM    GLSTSTAT,OKTOADD    IF OK TO ADD NEW RECORDS FROM LIST           
         BZ    *+8                                                              
         IC    R0,NLISTS           CHECK ENTIRE SCREEN                          
         LA    R3,LISTDIR          R3=A(DISK ADDRESS TABLE)                     
         L     R2,AFRSTREC         R2=A(1ST DATA LINE)                          
                                                                                
VALLST02 GOTOR TSTSEL              UNLESS THERE'S NO SELECT FIELD               
         BNE   *+12                                                             
         GOTOR BUMP                BUMP PAST IT TO 1ST FIELD                    
         BE    XITN                                                             
         ST    R2,ATHISLST         SAVE A(THIS LIST LINE) FOR APPLIC            
         LR    R4,R2                                                            
         AH    R4,LLIST            R4=A(1ST FIELD AFTER THIS DATA LINE)         
         MVI   BYTE,0              CLEAR LINE STATUS BYTE                       
                                                                                
VALLST04 TM    FLDATBD(R2),FATBPROT                                             
         BO    VALLST14                                                         
         TM    FLDIINDD(R2),FINPVAL                                             
         BO    VALLST06                                                         
         OI    BYTE,X'80'          SET FOUND UN-VALIDATED FIELD                 
         TM    GLSTSTAT,EMPTYLIN   NEED TO LOOK FOR FIELDS WITH INPUT?          
         BNZ   VALLST08            NO, WE HAVE A CHANGED LINE                   
                                                                                
VALLST06 CLI   FLDILEND(R2),0                                                   
         BE    *+8                                                              
         OI    BYTE,X'40'          SET FOUND A FIELD WITH INPUT                 
         TM    BYTE,X'C0'          IF WE'VE HAVEN'T SEEN BOTH YET               
         BNO   VALLST14            KEEP ON LOOKING                              
                                                                                
VALLST08 SR    R1,R1                                                            
         IC    R1,LISTNUM          RECORD CHANGED - SET RELATIVE NUMBER         
         TM    GLSTSTAT,OKTOADD                                                 
         BZ    *+8                                                              
         IC    R1,NLISTS                                                        
         SR    R1,R0                                                            
         CLM   R1,1,LISTNUM        IF RELATIVE NUMBER >= LISTNUM                
         BNL   VALLST10            THEN THIS MUST BE ADD                        
                                                                                
         STC   R1,SELLISTN         SAVE RELATIVE NUMBER                         
         MVI   0(R3),CHASELQ       MAKE IT APPEAR SELECTED FOR CHANGE           
         GOTOR SELGET              READ RECORD                                  
         MVI   0(R3),0                                                          
         MVI   MODE,LVALREC        SET VALIDATE LISTED RECORD MODE              
         GOTOR DOCHA               HANDLE CHANGES                               
         MVI   SELLISTN,0                                                       
         B     VALLST12                                                         
                                                                                
VALLST10 MVI   MODE,LVALKEY        SET VALIDATE KEY FROM LIST MODE              
         GOTOR GOOVER              LET APPLICATION VALIDATE/BUILD KEY           
         GOTOR ADDVAL              VALIDATE KEY FOR ADD                         
         JNE   ERRXIT              RETURNS CC NOT EQUAL ON ERROR                
         MVI   MODE,LVALREC        SET VALIDATE RECORD FROM LIST MODE           
         GOTOR DOADD               HANDLE ADD                                   
         GOTOR LISTMON             RECOGNIZE NEW ITEM IN LIST                   
                                                                                
VALLST12 LR    R2,R4               BUMP TO NEXT RECORD                          
         B     VALLST16                                                         
                                                                                
VALLST14 GOTOR BUMP                BUMP TO NEXT FIELD IN RECORD                 
         JE    XITN                                                             
         CR    R2,R4               TEST PAST END                                
         BL    VALLST04            NO                                           
                                                                                
VALLST16 AHI   R3,6                YES - BUMP TO ENTRY IN D/A TABLE             
         BCT   R0,VALLST02         BUMP TO NEXT RECORD                          
         J     XIT                                                              
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* TEMPSTR I/O                                                         *         
***********************************************************************         
                                                                                
SAVRST   NTR1  LABEL=NO                                                         
         SR    R0,R0                                                            
         IC    R0,TWANSAVE                                                      
         SLL   R0,27               CRUNCH OFF X'E0' BITS                        
         SRL   R0,27                                                            
         LTR   R0,R0               ANY TO SAVE/RESTORE                          
         JZ    SAVRSTX                                                          
         CLI   OFFLINE,YESQ        NOT APPLICABLE OFFLINE                       
         JE    SAVRSTX                                                          
         CHI   R0,3                MAX 3                                        
         JNH   *+6                                                              
         DC    H'0'                                                             
         LHI   R3,1                TWA NUMBER                                   
         L     R2,ASYSD            START OF STORAGE                             
         NC    ASTARTSV,ASTARTSV                                                
         JZ    *+8                                                              
         L     R2,ASTARTSV         USE OPTIONAL AREA IF SET                     
         AH    R2,LSVTWA0          SKIP ANYTHING SAVED IN TWA0                  
                                                                                
SAVRST02 GOTOR TEMPIO                                                           
         AH    R2,DMCB+22          LENGTH IS IN P6+2 OF TEMPSTR CALL            
         AHI   R3,1                                                             
         BRCT  R0,SAVRST02                                                      
                                                                                
SAVRSTX  XC    DMCB+20(4),DMCB+20                                               
         J     XIT                                                              
                                                                                
TEMPIO   NTR1  LABEL=NO            P6 OF DMCB MAY BE SET BY CALLER              
         GOTOR DATAMGR,DMCB,COMMAND,TEMPSTR,((R3),0),(R2),0                     
         CLI   8(R1),0             BLOW ON ANY ERROR HERE                       
         JE    XIT                                                              
         DC    H'0'                                                             
                                                                                
BUMP     SR    R1,R1               BUMP TO NEXT TWA FIELD OFF R2                
         IC    R1,FLDLEND(R2)                                                   
         AR    R2,R1                                                            
         CLI   FLDLEND(R2),0                                                    
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK FOR HELP EXIT                                                 *         
***********************************************************************         
                                                                                
ANYHELP  TM    FLDATBD(R2),FATBXHDR                                             
         BNZR  RE                                                               
         CLI   FLDHDRL(R2),C'?'                                                 
         JE    ANYHELP2                                                         
         SR    R1,R1                                                            
         IC    R1,FLDILEND(R2)                                                  
         BCTR  R1,0                                                             
         BASR  RF,0                                                             
         EX    R1,6(RF)                                                         
         BNER  RE                                                               
         CLC   FLDHDRL(0,R2),LP@HELPP                                           
                                                                                
ANYHELP2 MVI   ERROR,NOHELP        YES - IS HELP AVAILABLE                      
         CLI   PHHELP,0            NO - TOUGH                                   
         JE    ERRXIT                                                           
         MVC   FLDHDRL(#HELPPLQ,R2),LP@HELPP                                    
         OI    FLDOINDD(R2),FOUTTRN                                             
         MVC   OVERLAY,PHHELP                                                   
         L     R3,EFHTAG                                                        
         GOTOR LOADEM,DMCB,1       LOAD IN HELP SCREEN                          
         GOTOR XMTSCR                                                           
         MVC   TWASCR,PHHELP                                                    
         MVI   OKNO,1                                                           
         J     OKEX                                                             
                                                                                
SWTOCON  NTR1  LABEL=NO            SWITCH TO CONTROL SYSTEM                     
         MVI   USEIO,YESQ                                                       
         MVC   SYSDIR,CTFILE                                                    
         MVC   SYSFIL,CTFILE                                                    
         LHI   R0,CTIDATA-CTIREC                                                
         STCM  R0,3,DATADISP                                                    
         LHI   R0,L'CTIKEY                                                      
         STCM  R0,3,LKEY                                                        
         LHI   R0,CTLSYSQ                                                       
         SLL   R0,24                                                            
         ICM   R0,B'0111',EFFS                                                  
         GOTOR SWITCH,DMCB,(R0),0                                               
         CLI   4(R1),0                                                          
         J     XIT                                                              
                                                                                
READUP   TM    GENSTAT1,RDUPAPPL   IF APPLIC. CONTROLLING READ UPDATES          
         BZR   RE                                                               
         MVI   RDUPDATE,YESQ       INSURE IT'S ALWAYS SET HERE                  
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CONTROL PRINTING OF REPORT                                          *         
***********************************************************************         
                                                                                
DOPRNT   NTR1  LABEL=NO                                                         
         ICM   R3,15,TWADCONS                                                   
         JZ    DOPRNT02                                                         
         USING TWADCOND,R3                                                      
         L     R3,TISPRACT         IF A(PRINT ACTIVE SWITCH) RESOLVED           
         LTR   R3,R3                                                            
         JZ    DOPRNT02                                                         
         MVI   0(R3),NOQ           INITIALIZE IT                                
                                                                                
DOPRNT02 MVI   MODE,PRINTREP                                                    
         GOTOR GOOVER              PROCESS REPORT                               
         XC    LASTLIST,LASTLIST                                                
         XC    LASTSELK,LASTSELK                                                
         LTR   R3,R3               TEST A(PRINT ACTIVE SWITCH) RESOLVED         
         JZ    DOPRNTX                                                          
         CLI   0(R3),NOQ           IF NOTHING WAS PRINTED                       
         JNE   DOPRNTX                                                          
*&&US*&& CLI   TWAWHEN,TWW$UPSO    ALL SOONS MUST HAVE SOME OUTPUT              
*&&US*&& JE    DOPRNT04                                                         
*&&US*&& CLI   TWAWHEN,TWW$SOON                                                 
*&&US*&& JE    DOPRNT04                                                         
*&&US*&& GOTOR PRTDET              PRINT REQUEST DETAILS?                       
*&&US*&& JNE   DOPRNTX             NO                                           
                                                                                
DOPRNT04 L     RF,TWAMASTC         RF=A(MASTC)                                  
         TM    MCPRTIND-MASTD(RF),MCPRTXFI                                      
         JZ    DOPRNT06                                                         
         MVC   P,SPACES            SET ONE PAGE ONLY REQUEST DETAILS            
         GOTO1 TWAVPRNT,DUB,P,=C'BC01'                                          
         J     DOPRNTX                                                          
                                                                                
DOPRNT06 MVC   P,SPACES            SET TO DISPLAY NOTHING TO REPORT MSG         
         GOTOR TWAVPRNT,DUB,P,BL01                                              
         BASR  RE,RF                                                            
         MVCDD P+3(#DSK02LQ),GE#DSK02 JOB RAN TO NORMAL COMPLETION              
         BASR  RE,RF                                                            
         MVCDD P+3(#DSK01LQ),GE#DSK01 THERE WAS NOTHING ACTIVE TO REP           
         BASR  RE,RF                                                            
                                                                                
DOPRNTX  J     XIT                                                              
                                                                                
***********************************************************************         
* ROUTINE DETERMINES IF REQUEST DETAILS SHOULD PRINT                  *         
***********************************************************************         
                                                                                
PRTDET   L     R1,TWAMASTC         RETURN R1 = A(MASTER WORK AREA)              
         USING MASTD,R1                                                         
         CLI   MCREQREP,NOQ        TEST SUPPRESSED VIA CONTROL CARD             
         JE    PRTDETN                                                          
         CLI   MCREQREP,YESQ       IF NOT FORCED VIA CONTROL CARD               
         JE    *+12                                                             
         TM    GENSTAT2,NOREQDET   TEST PROGRAM SWITCH TO SUPPRESS THEM         
         JO    PRTDETN                                                          
PRTDETY  CR    RE,RE               DO PRINT - RETURN CC EQUAL                   
         BR    RE                                                               
PRTDETN  LTR   RE,RE               DON'T PRINT - RETURN CC NOT EQUAL            
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO CONTROL SAVE/RESTORE OF LIST SCREENS                    *         
***********************************************************************         
                                                                                
SAVLST   NTR1  LABEL=NO            SAVE A SCREEN                                
         ICM   R0,15,EFFS                                                       
         GOTOR SWITCH,DMCB,(R0),0                                               
         L     R1,0(R1)                                                         
         LA    RF,TWAD                                                          
         AHI   RF,LSTSCMAP-TWATASK                                              
         MVC   0(L'LSTSCMAP,RF),TSCRNE-UTLD(R1)                                 
         MVI   LSTONTWA,YESQ                                                    
         MVC   COMMAND,DMWRTL                                                   
         J     RESLST02                                                         
                                                                                
RESLST   NTR1  LABEL=NO            RESTORE A SCREEN                             
         CLI   LSTONTWA,YESQ       IS THERE ONE TO RESTORE                      
         JNE   XIT                                                              
*&&US*&& MVC   COMMAND,DMRDIR                                                   
*&&UK*&& MVC   COMMAND,DMREAD                                                   
         XC    DMCB+20(4),DMCB+20                                               
         TM    GENSTAT3,RESTXE00   RESTORE X'E00' TO TWA0                       
         JZ    RESLST02                                                         
         MVC   DMCB+20(2),LEQUAL   SET P6 OF DMGR CALL IN TEMPIO                
         MVC   DMCB+22(2),=X'0E00'                                              
         MVC   COMMAND,DMREAD                                                   
                                                                                
RESLST02 L     R2,ATWA                                                          
         LHI   R3,4                                                             
         GOTOR TEMPIO                                                           
         XC    DMCB+20(4),DMCB+20                                               
         ICM   R0,15,EFFS                                                       
         GOTOR SWITCH,DMCB,(R0),0                                               
         L     R1,0(R1)                                                         
         GOTOR PROTOFF                                                          
         LA    RF,TWAD                                                          
         AHI   RF,LSTSCMAP-TWATASK                                              
         MVC   TSCRNE-UTLD(L'LSTSCMAP,R1),0(RF)                                 
         GOTOR PROTON                                                           
         J     XITY                                                             
         EJECT                                                                  
***********************************************************************         
* SET UP TWA ADDRESSES                                                *         
***********************************************************************         
                                                                                
SETTWA   NTR1  LABEL=NO                                                         
         XC    AFRSTREC,AFRSTREC                                                
         XC    AFRSTKEY,AFRSTKEY                                                
         L     R2,EFHTAG                                                        
         SR    R1,R1                                                            
                                                                                
SETTWA02 GOTOR BUMP                                                             
         JE    SETTWAX                                                          
         TM    FLDATBD(R2),FATBPROT                                             
         JO    SETTWA02                                                         
         ST    R2,AFRSTKEY                                                      
         ST    R2,AFRSTREC                                                      
         ST    R2,AFRSTRCH                                                      
                                                                                
SETTWA04 GOTOR BUMP                                                             
         JE    SETTWAX                                                          
         TM    FLDATBD(R2),FATBPROT                                             
         JNO   SETTWA04                                                         
         CLI   FLDLEND(R2),FLDHDRL+1                                            
         JNE   SETTWA04                                                         
                                                                                
SETTWA06 GOTOR BUMP                                                             
         JE    SETTWAX                                                          
         TM    FLDATBD(R2),FATBPROT                                             
         JNO   *+12                                                             
         ST    R2,AFRSTRCH         NOTE HEADER BEFORE FIRST RECORD              
         J     SETTWA06                                                         
         ST    R2,AFRSTREC         AND FIRST RECORD INPUT                       
                                                                                
SETTWAX  J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD A PHASE                                                        *         
***********************************************************************         
                                                                                
LOADEM   NTR1  LABEL=NO                                                         
LOADEM$  BASE  ,                                                                
         MVC   BYTE,3(R1)          DON'T USE APPLICATION CALLS                  
         CLI   BYTE,1              FOR SCREENS (1)                              
         BE    LOADEM04                                                         
         L     R3,AOVERLAY                                                      
         SR    R0,R0               R0=0 IF REGULAR LOAD                         
         C     R3,SYSDUMMY                                                      
         BNE   *+8                                                              
         LHI   R0,1                R0=1 IF LOADING INTO SYSDUMMY                
         TM    GENSTAT1,APPLIC     USE APPLICATION CALLS?                       
         BO    LOADEM12                                                         
                                                                                
LOADEM04 MVC   DMCB+4(4),SYSPHASE  NORMAL CALL TO LOADEM                        
         ST    R3,DMCB                                                          
         CLI   BYTE,1              IF WE'RE LOADING A SCREEN                    
         BNE   LOADEM06                                                         
         LM    R4,R5,TWAD+X'E00'   SAVE 1ST 8 BYTES OF MY SAVED STORAGE         
         CLI   ALTPROG,0           IF ALTERNATE PROGRAM DEFINED                 
         BE    LOADEM06                                                         
         MVC   DMCB+6(1),ALTPROG   USE IT                                       
                                                                                
LOADEM06 GOTOR CALLOV,DMCB,,,0                                                  
         OC    DMCB+9(3),DMCB+9                                                 
         BZ    LOADEM14            PHASE NOT FOUND                              
         CLI   BYTE,1              IF THIS ISN'T A SCREEN                       
         BNE   LOADEM08            GO UPDATE AOVERLAY                           
         STM   R4,R5,TWAD+X'E00'   RESTORE 1ST 8 BYTES OF SAVED STORAGE         
         LA    RF,TWAD+X'E07'      RF=A(END OF USABLE TWA0 FOR SCREEN)          
         LA    R3,0(R3)                                                         
         C     R3,ATWA             IF A(LOAD) IS ON/AFTER A(TWA)                
         JL    XIT                                                              
         CR    R3,RF               AND BEFORE END OF USABLE AREA                
         JH    XIT                                                              
         AH    R3,DMCB+10          THEN INSURE NOT LOADED PAST END              
         CR    R3,RF                                                            
         JNH   XIT                                                              
         DC    H'0'                SCREEN EXCEEDS END OF USABLE AREA            
                                                                                
LOADEM08 L     R1,DMCB                                                          
         CLI   OFFLINE,YESQ        ALLOW LOAD ANYWHERE                          
         BNE   *+10                IF RUNNING OFFLINE                           
         LR    R3,R1                                                            
         B     LOADEM10                                                         
         LTR   R3,R3                                                            
         BNZ   *+6                                                              
         LR    R3,R1                                                            
         CR    R1,R3                                                            
         BE    *+6                                                              
         DC    H'0'             LOADS OUT OF PHASE???                           
         OC    DMCB+10(2),DMCB+10                                               
         BNZ   *+6                                                              
         DC    H'0'             ZERO LENGTH PHASE???                            
         AH    R1,DMCB+10                                                       
         AHI   R1,8                                                             
         SRL   R1,3                                                             
         SLL   R1,3                                                             
                                                                                
LOADEM10 ST    R1,AOVERLAY      GET A(NEXT DOUBLE WORD)                         
         CLI   OFFLINE,YESQ     IF RUNNING OFFLINE                              
         BNE   LOADEMX                                                          
         LTR   R0,R0            AND LOADING TO SYSDUMMY                         
         BZ    LOADEMX                                                          
         ST    R3,SYSDUMMY      SET SYSDUMMY TO ACTUAL LOAD ADDRESS             
         B     LOADEMX                                                          
                                                                                
LOADEM12 XC    DMCB(12),DMCB    APPLICATION CALLS                               
         MVC   DMCB(1),OVERLAY                                                  
         GOTOR CALLOV,DMCB                                                      
         CLI   DMCB+4,FF        CHECK ERROR,                                    
         BE    LOADEM14         PHASE NOT FOUND                                 
         L     R3,DMCB          RETURN A(PHASE)                                 
         B     LOADEMX                                                          
                                                                                
LOADEM14 LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMSGNO1,NOPHASE CANT FIND OVERLAY                               
         MVI   GTMSYS,FF        GENERAL SYSTEM MESSAGE                          
         MVI   GTLTXT,6         L'PHASE NAME                                    
         LA    R3,BLOCK         BUILD PHASE NAME TO APPEND                      
         STCM  R3,7,GTATXT                                                      
         DROP  R1                                                               
         MVC   WORK(4),SYSPHASE                                                 
         CLI   BYTE,1           IF WE'RE LOADING A SCREEN                       
         BNE   LOADEM16                                                         
         CLI   ALTPROG,0        AND ALTERNATE PROGRAM DEFINED                   
         BE    LOADEM16                                                         
         MVC   WORK+2(1),ALTPROG USE IT                                         
                                                                                
LOADEM16 GOTOR HEXOUT,DMCB,WORK+1,(R3),3,0                                      
         MVI   0(R3),C'T'                                                       
         OI    GENSTAT2,USGETTXT                                                
         J     ERRXIT                                                           
                                                                                
LOADEMX  XIT1  REGS=(R3)                                                        
         ORG   *-2                                                              
         BR    RE                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO HANDLE TEMPSTR MAINTENANCE                              *         
***********************************************************************         
                                                                                
SAVWRK   NTR1  LABEL=NO                                                         
SAVWRK$  MVC   TWANSAVE,NTWA       SAVE UP TO 3 AREAS                           
         GOTOR ANYTWA0             ANYTHING TO SAVE AT BOTTOM OF TWA0           
         JZ    *+6                 RETURNS REGS R0,R1,R2,R3                     
         MVCL  R0,R2               SAVE IT                                      
         CLI   TWANSAVE,0          ANY ADDITIONAL TEMPSTR PAGES TO SAVE         
         JE    SAVWRKX                                                          
         MVC   COMMAND,DMWRTL                                                   
         GOTOR SETDP6              SET P6 OF DMGR CALL IN TEMPIO                
         GOTOR SAVRST                                                           
SAVWRKX  J     XIT                                                              
                                                                                
RESWRK   NTR1  LABEL=NO                                                         
         GOTOR ANYTWA0             TEST ANYTHING TO RESTORE FROM TWA0           
         JZ    *+6                 RETURNS REGS R0,R1,R2,R3                     
         MVCL  R2,R0               RESTORE IT                                   
         CLI   TWANSAVE,0                                                       
         JE    RESWRKX                                                          
         MVC   NTWA,TWANSAVE       RESTORE UP TO 3 AREAS                        
         MVC   COMMAND,DMREAD                                                   
         GOTOR SETDP6              SET P6 OF DMGR CALL IN TEMPIO                
         GOTOR SAVRST                                                           
         ICM   RE,15,AENDSV        OPTIONAL END OF SAVE STORAGE                 
         JZ    RESWRKX                                                          
         LHI   RF,2304             CLEAR A BIT FROM HERE                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
RESWRKX  J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SUBSIDIARY ROUTINES FOR SAVED STORAGE MAINTENANCE                   *         
***********************************************************************         
                                                                                
ANYTWA0  OC    LSVTWA0,LSVTWA0     ANYTHING TO SAVE AT BOTTOM OF TWA0           
         BZR   RE                                                               
         LH    R1,LSVTWA0                                                       
         CHI   R1,TWA018K          MAX WE CAN KEEP THERE                        
         JNH   *+6                                                              
         DC    H'0'                YOU NEED TO ASK FOR LESS                     
         LHI   R0,(18-2)*ONEK      TWA SIZE IS 18K, SAVE 2K FOR CHKPT           
         SR    R0,R1               SET TO START AT BOTTOM OF TWA0               
         A     R0,ATWA                                                          
         L     R2,ASTARTSV         A(SAVED STORAGE)                             
         LTR   R2,R2                                                            
         JNZ   *+8                                                              
         L     R2,ASYSD                                                         
         LR    R3,R1               SET L'MOVE IN BOTH ODD REGISTERS             
         LTR   RE,RE                                                            
         BR    RE                  OK - RETURN CC NOT EQUAL                     
                                                                                
SETDP6   STM   RE,R1,12(RD)                                                     
         MVC   DMCB+20(2),LEQUAL   SET P6 OF DMGR CALL IN TEMPIO                
         LHI   R0,2304                                                          
         TM    NTWA,NTWA06K+NTWA14K+NTWA18K                                     
         JZ    SETDP6X                                                          
         TM    NTWA,NTWA06K        OPTION TO RESTORE 06K AREA                   
         JZ    SETDP602                                                         
         LHI   R0,6*ONEK                                                        
         LHI   RF,NTWA14K+NTWA18K  ENSURE 14/18 NOT ALSO SET                    
         J     SETDP606                                                         
                                                                                
SETDP602 TM    NTWA,NTWA14K        OPTION TO RESTORE 14K AREA                   
         JZ    SETDP604                                                         
         LHI   R0,4*ONEK                                                        
         LHI   RF,NTWA06K+NTWA18K  ENSURE 06/18 NOT ALSO SET                    
         J     SETDP606                                                         
                                                                                
SETDP604 LHI   R0,18*ONEK          OPTION TO RESTORE 18K AREA                   
         LHI   RF,NTWA06K+NTWA14K  ENSURE 06/14 NOT ALSO SET                    
                                                                                
SETDP606 BASR  RE,0                                                             
         EX    RF,10(RE)                                                        
         JZ    SETDP6X                                                          
         DC    H'0'                MIX OF 6/14/18 K REQUESTED                   
         TM    NTWA,0                                                           
                                                                                
SETDP6X  STCM  R0,3,DMCB+22                                                     
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE A REPORT                                                 *         
***********************************************************************         
                                                                                
INIREP$  CLI   PQSW,0              UNLESS USER OPTS TO OPEN PQ                  
         JNE   *+8                 DO IT NOW                                    
         GOTOR OPENPQ                                                           
         OC    VPRINT,VPRINT                                                    
         JZ    XIT                                                              
         GOTOR PRTDET              TEST PRINTING REQUEST DETAILS                
         JNE   INIREP02                                                         
         ICM   R0,B'1111',ABOX                                                  
         JZ    *+8                                                              
         ICM   R0,B'1000',PASSABOX                                              
         GOTOR REQTWA,DMCB,(3,TWAD),('FF',ACOMFACS),VPRINT,(R0)                 
                                                                                
INIREP02 GOTOR TWAPTCHR,DMCB,BASERB,AGO                                         
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CLOSE PRINT QUEUE REPORT                                            *         
***********************************************************************         
                                                                                
CLSREP   NTR1  BASE=*,LABEL=*                                                   
         CLI   PQSW,2              MUST HAVE BEEN OPENED                        
         BE    *+12                                                             
         MVI   OKNO,22                                                          
         B     CLSREPX                                                          
                                                                                
         MVI   SPMODE,FF                                                        
         GOTOR SPOOL,PARAS,SPOOLD  WRAP UP PRINT CONTROL INTERVAL               
         OC    SPOOLPAG,SPOOLPAG                                                
         BNZ   *+12                                                             
         MVI   OKNO,22                                                          
         B     CLSREPX                                                          
                                                                                
         MVI   OKNO,23             REPORT SPOOLED.ID=&1 PGS=&2 LNS=&3           
         MVC   BLOCK(21),SPACES    SET UP GETTXT SUBST PARAM BLOCK              
*                                  L'TXTA+1,TXTA,L'TXTB+1,TXTB ETC ,00          
         MVI   BLOCK,10            XXX,99999 - REP I.D.     PARAM &1            
         MVI   BLOCK+10,5          9999 - PAGE COUNT        PARAM &2            
         MVI   BLOCK+15,6          99999 - LINE COUNT       PARAM &3            
         MVI   BLOCK+21,0          EOT                                          
         MVC   BLOCK+1(3),SPOOLID                                               
         MVI   BLOCK+4,C','                                                     
         EDIT  (2,SPOOLRPN),(5,BLOCK+5),ALIGN=LEFT                              
         EDIT  (2,SPOOLPAG),(4,BLOCK+11),ALIGN=LEFT                             
         EDIT  (2,SPOOLLIN),(5,BLOCK+16),ALIGN=LEFT                             
         LA    R1,GETTXTCB         SET A(SUBST PARAM LIST)                      
         LA    RF,BLOCK                                                         
         STCM  RF,7,GTASUBST-GETTXTD(R1)                                        
                                                                                
         CLI   ONLYSPUL,YESQ       SPOOL ONLY SYSTEMS                           
         BNE   CLSREPX                                                          
         CLC   SPOOLRPN,=H'9999'   TEST 'XXX,9999' WILL FIT INTO                
         BH    CLSREPX             RECORD FIELD                                 
         L     R1,EFHREC                                                        
         MVC   FLDHDRL(8,R1),BLOCK+1                                            
         OI    FLDOINDD(R1),FOUTTRN                                             
                                                                                
CLSREPX  J     XIT                                                              
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* TEST IF SELECT FIELD IS PRESENT                                     *         
***********************************************************************         
                                                                                
TSTSEL   TM    GLSTSTAT,NOSELFLD                                                
         JO    TSTSELN                                                          
         TM    FLDATBD(R2),FATBPROT                                             
         JO    TSTSELN                                                          
         TM    FLDATBD(R2),FATBLOW                                              
         JO    TSTSELN                                                          
         CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
TSTSELN  LTR   RE,RE                                                            
         BR    RE                                                               
                                                                                
***********************************************************************         
* TRANSMIT THE ENTIRE SCREEN                                          *         
***********************************************************************         
                                                                                
XMTSCR   STM   RE,R1,12(RD)                                                     
                                                                                
         LA    R1,CONHEADH                                                      
         SR    R0,R0                                                            
XMTSCR02 OI    FLDOINDD(R1),FOUTTRN                                             
         ICM   R0,1,FLDLEND(R1)                                                 
         JZ    *+10                                                             
         AR    R1,R0                                                            
         J     XMTSCR02                                                         
         MVI   1(R1),1                                                          
         MVI   2(R1),1                                                          
                                                                                
XMTSCRX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ A SELECTED RECORD                                   *         
***********************************************************************         
                                                                                
SELGET   NTR1  LABEL=NO            R3=A(LISTDIR ENTRY)                          
         TM    GENSTAT3,MULTFILS   IF SYSTEM HAS MULTIPLE FILES                 
         JZ    *+12                                                             
         MVI   MODE,SETFILE        GIVE APPLIC. A CHANCE TO SWITCH              
         GOTOR GOOVER                                                           
         OI    DMINBTS,X'08'       RECORDS MAY BE DELETED                       
         CLI   USEIO,YESQ                                                       
         JE    SELGET02                                                         
         LA    R1,KEY              SET R4 = A(KEY)                              
         TM    GENSTAT4,USEBIGKY   FOR LARGE KEYS USE BIGKEY                    
         JZ    *+8                                                              
         LA    R1,BIGKEY                                                        
         AH    R1,LKEY                                                          
         AH    R1,LSTATUS                                                       
         MVC   0(4,R1),2(R3)                                                    
         MVC   LASTSEL,2(R3)       SAVE DA OF LAST SELECTION                    
         OC    LASTSEL,LASTSEL     TEST DSKAD = 0                               
         JZ    SELGETX             YES - NOT A REAL DISK ADDRESS                
         CLI   0(R3),CHASELQ       IF WE'RE ABOUT TO CHANGE                     
         JNE   *+8                                                              
         GOTOR READUP              THEN MAKE SURE WE READ FOR UPDATE            
         GOTOR GETREC              GET THE RECORD                               
         MVC   GLOBDA,DMDSKADD                                                  
         L     RE,AIO                                                           
         LA    R0,KEY                                                           
         TM    GENSTAT4,USEBIGKY   FOR LARGE KEYS USE BIGKEY                    
         JZ    *+8                                                              
         LA    R0,BIGKEY                                                        
         LH    R1,LKEY                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         CLC   0(1,R3),LP@DELS     IF WE'RE ABOUT TO DELETE                     
         JE    *+14                                                             
         CLC   0(1,R3),LP@RESS     OR WE'RE ABOUT TO RESTORE                    
         JNE   *+8                                                              
         GOTOR READUP              THEN MAKE SURE WE READ FOR UPDATE            
         GOTOR READ                READ KEY                                     
         J     SELGETX                                                          
                                                                                
SELGET02 SR    RE,RE                                                            
         IC    RE,SELLISTN                                                      
         MH    RE,LKEY                                                          
         LA    RE,LISTKEYS(RE)                                                  
         XC    KEY,KEY                                                          
         LA    R0,KEY                                                           
         LH    R1,LKEY                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   LASTSELK,KEY        SAVE LAST SELECTED FOR NEXT TIME             
         CLI   0(R3),CHASELQ       TEST FOR CHA,DEL,RESTORE                     
         JE    SELGET04                                                         
         CLC   0(1,R3),LP@DELS                                                  
         JE    SELGET04                                                         
         CLC   0(1,R3),LP@RESS                                                  
         JNE   SELGET06                                                         
                                                                                
SELGET04 GOTOR READUP              INSURE READ FOR UPDATE SET                   
                                                                                
SELGET06 GOTOR READ                                                             
                                                                                
SELGETX  NI    DMINBTS,X'F7'                                                    
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALIZE RFPIO CONTROL BLOCK AND CALL RFPIO TO INITIALIZE         *         
***********************************************************************         
                                                                                
         USING QRFPD,R3                                                         
         USING RFPBLK,R4                                                        
RFPINI   LR    R0,R4               CLEAR RFP INTERFACE BLOCK                    
         SR    R1,R1                                                            
         LHI   R1,RFPBLKLN                                                      
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R0,R2                                                            
                                                                                
         MVI   RFPINIT,0           INITIALIZE RFP BLOCK                         
         MVC   RFPACOMF,ACOMFACS   - A(COMFACS)                                 
         LR    R0,R4                                                            
         AHI   R0,8*ONEK                                                        
         ST    R0,RFPAMIN          - A(MINIO IO BUFFER)                         
         MVC   RFPFUID,TWAORIG     - USER ID                                    
         MVC   RFPFAGY,TWAAGY      - AGENCY                                     
         MVC   RFPFSYS,SYSTEM      - CONNECTED SYSTEM                           
         TM    GENSTAT5,USERFPXT   IF WE ARE TO USE EXTENSION                   
         JNO   *+8                                                              
         OI    RFPFLAGS,RFPXSYMS   SET FLAG                                     
         GOTOR ARFPIO,DMCB,RFPBLK                                               
         CLI   RFPERROR,RFPNOERR   ANY ERRORS?                                  
         JE    *+6                 NO                                           
         DC    H'0'                                                             
         OI    RFPFFLAG,RFPSPOOF   SET SPOOF-STYLE REQUEST                      
         J     XIT                                                              
                                                                                
***********************************************************************         
* VALIDATE RFP GROUP CODE                                             *         
***********************************************************************         
                                                                                
RFPVAL   MVC   RFPFGRP,QRFPWORK    GROUP NAME                                   
         OC    RFPFGRP,SPACES                                                   
         MVI   RFPMODE,RFPVALGP    VALIDATE GROUP                               
         OI    RFPFFLAG,RFPFSYMS   RETURN ALL SYMBOLIC EQUATES                  
         GOTOR ARFPIO,DMCB,RFPBLK                                               
         CLI   RFPERROR,RFPNOERR                                                
         JE    XITY                                                             
         CLI   RFPERROR,RFPNOGRP                                                
         JE    *+6                                                              
         DC    H'0'                UNKNOWN ERROR                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         MVI   GTMSYS,FF           GENERAL MSG SYSTEM                           
         OI    GENSTAT2,USGETTXT                                                
         MVI   GTMSGNO1,109        INVALID GROUP NAME                           
         J     XITN                                                             
         DROP  R1                                                               
                                                                                
***********************************************************************         
* VALIDATE SYMBOL & RETURN CORRESPONDING ESCAPE SEQUENCE              *         
***********************************************************************         
                                                                                
RFPSYM   SR    R0,R0                                                            
         IC    R0,RFPVNUMS         # OF SYMBOLS IN RFP TABLE                    
         LA    R1,RFPVSYMS         DEFAULT TO ORIGINAL SYMBOL TABLE             
         TM    GENSTAT5,USERFPXT   IF WE ARE TO USE EXTENSION                   
         JNO   *+12                                                             
         LHI   R1,RFPXTNSN-RFPBLK  RESET SYMBOL TABLE POINTER                   
         LA    R1,RFPBLK(R1)                                                    
         CLC   QRFPWORK,RFPVSYMB-RFPVSYMS(R1)                                   
         JE    RFPSYM02                                                         
         AHI   R1,RFPVSYML                                                      
         BRCT  R0,*-14                                                          
         XC    QRFPWORK,QRFPWORK   PASS BACK NULLS FOR INVALID SYMBOL           
         J     RFPSYMX                                                          
                                                                                
RFPSYM02 XC    QRFPWORK,QRFPWORK                                                
         MVC   QRFPWORK(L'RFPVSYME),RFPVSYME-RFPVSYMS(R5)                       
                                                                                
RFPSYMX  J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST SECURITY ACCESS BLOCK                               *         
*                                                                     *         
* NTRY - R1=A(RECACT ENTRY)                                           *         
* EXIT - CC=EQUAL IF AUTHORIZED, NOT EQUAL IF NOT AUTHORIZED          *         
***********************************************************************         
                                                                                
TSTRAS   NTR1  LABEL=NO                                                         
         CLI   OFFLINE,YESQ        SUPPRESS TESTING OFFLINE                     
         JE    XITY                                                             
         OC    ASECBLK,ASECBLK     TEST SECURITY BLOCK DEFINED                  
         JZ    XITY                                                             
         CLI   0(R1),LUPRACQ       TEST SECURITY ON RECORD/ACTION PAIR          
         JNE   TSTRAS02                                                         
         LA    RF,2(R1)                                                         
         ICM   RF,8,1(R1)          RF=(RECORD, A(ACTION))                       
         LA    R4,SECPRACT         R4=ACTION (TESTING RECORD/ACTION)            
         TM    GENSTAT6,GESECOVR   ARE WE USING A RECORD OVERRIDE?              
         JNO   TSTRAS04            (GESECOVR CONFLICTS WITH SECMASKS)           
         CLI   12(R1),0            ANY OVERRIDE PROVIDED?                       
         JE    TSTRAS04            NO OVERRIDE-GO AS IS                         
         ICM   RF,8,12(R1)         STICK IN OVERRIDE INSTEAD OF RECORD          
         J     TSTRAS04                                                         
*                                  ELSE MUST BE RECORD OR ACTION                
TSTRAS02 LA    RF,9(R1)            RF=A(RECORD OR ACTION)                       
         TM    GENSTAT6,GESECOVR   ARE WE USING A RECORD OVERRIDE?              
         JNO   *+16                (GESECOVR CONFLICTS WITH SECMASKS)           
         CLI   12(R1),0            ANY OVERRIDE PROVIDED?                       
         JE    *+8                 NO OVERRIDE-GO AS IS                         
         LA    RF,12(R1)           POINT TO OVERRIDE INSTEAD OF RECORD          
         LA    R4,SECPRCD          R4=ACTION (TESTING RECORD)                   
         CLI   0(R1),X'02'         IF TESTING ACTION                            
         JNE   TSTRAS04                                                         
         LA    R4,SECPACT          R4=ACTION (TESTING ACTION)                   
                                                                                
TSTRAS04 TM    GENSTAT7,GES7DDS    TEST FOR DDS ONLY RECORDS                    
         JZ    TSTRAS08                                                         
         CLI   0(R1),X'02'         DON'T BOTHER WITH ACTION                     
         JE    TSTRAS08                                                         
         CLI   LRECACT,12          MUST BE MORE THAN 12                         
         JNH   TSTRAS08                                                         
         LA    RE,12(R1)           DEFAULT IS +12                               
         CLI   LDDSDISP,0          TEST DISPLACEMENT TO DDS FLAG SET            
         JE    TSTRAS06                                                         
         SR    RE,RE                                                            
         IC    RE,LDDSDISP         YES - USE IT                                 
         LA    RE,0(RE,R1)                                                      
TSTRAS06 TM    0(RE),X'80'         TEST DDS ONLY ACTION                         
         JZ    TSTRAS08                                                         
         CLI   TWAOFFC,C'*'        YES - ALWAYS ALLOW IF DDS A TERMINAL         
         JE    XITY                                                             
                                                                                
TSTRAS08 GOTOR SECRET,DMCB,((R4),ASECBLK),(RF)                                  
         J     XIT                 SECRET RETURNS CC                            
         EJECT                                                                  
***********************************************************************         
* EXITS AND ERRORS                                                    *         
***********************************************************************         
                                                                                
XITR2    CR    R2,R2                                                            
         XIT1  REGS=(R2)                                                        
                                                                                
ERRNOTN  MVI   ERROR,NOTNUM                                                     
         J     ERRXIT                                                           
                                                                                
ERRINV   MVI   ERROR,INVALID                                                    
         J     ERRXIT                                                           
                                                                                
ERREDICT MVI   ERROR,NOEDICTR                                                   
         J     ERRXIT                                                           
                                                                                
ERRFAX   MVI   ERROR,INVFAX                                                     
         J     ERRXIT                                                           
                                                                                
ERRDEST  MVI   ERROR,INVDEST                                                    
         J     ERRXIT                                                           
                                                                                
ERRXIT   GOTOR ERREX                                                            
                                                                                
REPXIT   CLI   ACTNUM,ACTSEL       TEST WE CAME FROM SELECT                     
         JNE   REPXIT02            NO                                           
         L     R2,EFHACT                                                        
         MVC   FLDHDRL(#SELPLQ,R2),LP@SELP                                      
         MVI   FLDILEND(R2),#SELPLQ                                             
         J     XIT                 BACK TO LIST LOGIC                           
                                                                                
REPXIT02 LA    R1,GETTXTCB         DEFINE MSG NO AND TYPE                       
         USING GETTXTD,R1                                                       
         MVC   GTMSGNO1,OKNO                                                    
         MVI   GTMTYP,GTMINF       INFORMATION                                  
         MVI   GTMSYS,FF           GENERAL MSG SYSTEM                           
         TM    GENSTAT6,GES$LINK   TEST RUNNING DDLINK UPLOAD                   
         JZ    *+12                                                             
         NI    GETTXTCB+(GT1INDS-GETTXTD),FF-(GT1NOREF)                         
         OI    GETTXTCB+(GT1INDS-GETTXTD),GT1REF                                
         GOTOR GETTXT              R1=A(CONTROL BLOCK)                          
         J     EXIT                                                             
         DROP  R1                                                               
                                                                                
MODEX    L     R1,EFHACT                                                        
         TM    FLDATBD(R1),FATBPROT                                             
         JO    OKEX                DON'T MODIFY IF PROTECTED                    
         OI    FLDOINDD(R1),FOUTTRN+FOUTMOD                                     
                                                                                
OKEX     TM    GENSTAT2,USMYOK+USGETTXT TEST IF APPL SET GETTXTCB               
         JO    OKEX02                                                           
         TM    GENSTAT2,USMYOK          TEST IF APPL SET OWN MSG                
         JO    OKEX08                                                           
         XC    PARAS(L'GTBLOCK),PARAS   DEFINE GETTXT PARAMS                    
         LA    R1,PARAS                                                         
         USING GETTXTD,R1                                                       
         MVC   GTMSGNO1,OKNO       MESSAGE NUMBER (1 BYTE)                      
         MVI   GTMSYS,FF           SYSTEM ZERO (GENERAL) MESSAGES               
         J     OKEX04                                                           
                                                                                
OKEX02   LA    R1,GETTXTCB         USER HAS DEFINED CONTROL BLOCK               
         CLI   GTMTYP,0            CHECK THAT MESSAGE TYPE DEFINED              
         JNE   OKEX06                                                           
                                                                                
OKEX04   MVI   GTMTYP,GTMINF       SET INFORMATION MESSAGE                      
         DROP  R1                                                               
                                                                                
OKEX06   TM    GENSTAT6,GES$LINK   TEST RUNNING DDLINK UPLOAD                   
         JZ    *+12                                                             
         NI    GETTXTCB+(GT1INDS-GETTXTD),FF-(GT1NOREF)                         
         OI    GETTXTCB+(GT1INDS-GETTXTD),GT1REF                                
         GOTOR GETTXT              R1=A(PARAS)-GENNEW. A(GETTXTCB)-APPL         
         J     OKEX10                                                           
                                                                                
OKEX08   TM    GENSTAT2,STLCOK     APPL SET MSG - MAY WANT LOWER CASE           
         JZ    OKEX10                                                           
         TR    CONHEAD+1(59),UPRLWR                                             
                                                                                
OKEX10   NI    GENSTAT2,255-USMYOK-STLCOK-USGETTXT                              
         CLI   PQSW,2              IF PQ REPORT HAS BEEN OPENED                 
         JNE   EXIT                CLOSE IT                                     
         GOTOR CLSREP                                                           
         J     REPXIT                                                           
                                                                                
EXIT     CLI   GCMODE,GCMSLAVQ     EXIT HERE IF SLAVED MODE                     
         JE    XIT                                                              
         MVI   GENSTAT2,0                                                       
         ICM   R0,15,ACURFORC      FORCE CURSOR TO THIS ADDRESS?                
         JZ    *+6                                                              
         LR    R2,R0                                                            
         LTR   R2,R2                                                            
         JNZ   *+8                                                              
         L     R2,EFHREC                                                        
         OI    FLDOINDD(R2),FOUTCUR                                             
         LA    R0,TWAD                                                          
         SR    R2,R0               AND SAVE LOCATION                            
         STH   R2,TWADISP                                                       
         L     R2,AERRAREA                                                      
         MVC   TWALACT,ACTNUM                                                   
         MVC   TWALREC,RECNUM                                                   
         OI    FLDOINDD(R2),FOUTTRN                                             
                                                                                
         TM    GENSTAT6,GES$LINK   TEST DDLINK UPLOAD IN PROCESS                
         JZ    EXIT02                                                           
         GOTOR PUTMSG              SEND MESSAGE TEXT TO DDLINK                  
         L     R1,ALIOB            FORCE NEXT INPUT RECORD                      
         OI    LIOBINDS-LIOBD(R1),LIOBINXT                                      
         L     RD,BASERD                                                        
         LM    RE,RC,12(RD)                                                     
         J     MAIN                GET NEXT UPLOAD RECORD                       
                                                                                
EXIT02   TM    GENSTAT4,SVTWA0FF   SAVE TWA0 ON 'FF' SCREEN?                    
         JO    *+12                YES                                          
         CLI   TWASCR,FF           IF REQUEST DISPLAY NOT ACTIVE                
         JE    *+8                                                              
         GOTOR SAVWRK              SAVE SAVED STORAGE                           
         L     RD,SYSRD                                                         
         J     XITY                                                             
                                                                                
XITL     LHI   RE,0                SET CC=LOW ON EXIT                           
         J     XITCC                                                            
XITY     DS    0H                  SET CC-EQUAL ON EXIT                         
XITE     LHI   RE,1                                                             
         J     XITCC                                                            
XITN     DS    0H                  SET CC=HIGH ON EXIT                          
XITH     LHI   RE,2                                                             
XITCC    CHI   RE,1                                                             
                                                                                
XIT      XIT1  ,                                                                
         ORG   *-2                                                              
         BR    RE                                                               
                                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
CTLLETQ  EQU   C'C'                CONTROL SYSTEM LETTER                        
CTLSYSQ  EQU   X'0A'               CONTROL SYSTEM NUMBER                        
                                                                                
ONEK     EQU   1024                                                             
FF       EQU   X'FF'                                                            
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
                                                                                
GCMNONEQ EQU   C' '                NO MODE                                      
GCMSLAVQ EQU   C'S'                SLAVE MODE                                   
GCMPROGQ EQU   C'4'                PROGRAM RECORD MODE                          
GCMDPRPQ EQU   C'X'                DELETE PARTIAL REPORT                        
                                                                                
LUPRECQ  EQU   1                   LOOKUP RECORD ENTRY                          
LUPACTQ  EQU   2                   LOOKUP ACTION ENTRY                          
LUPRACQ  EQU   3                   LOOKUP RECORD/ACTION COMBO ENTRY             
LUPPRGQ  EQU   4                   LOOKUP PROGRAM RECORD ENTRY                  
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
                                                                                
         LTORG                                                                  
                                                                                
DSTLIT   DC    C'&&&&DST&&&&&&&&&&&&&&'                                         
OUTLIT   DC    C'&&&&OUT&&&&&&&&&&&&&&'                                         
                                                                                
UPLOAD   DC    C'UPLOAD'           UPLOADING ACTION                             
LKIOTOKN DC    C'LOSV'             WSSVR TOKEN FOR LINKIO SAVE DATA             
LIOBTLEN DC    AL2(18*ONEK)        TOTAL LENGTH OF LIOB AND BUFFERS             
FAXPFX   DC    C'FX='              FAX DESTINATION PREFIX                       
FAXOUT   DC    CL(L'TWAOUT)'FAX'   FAX OUTPUT TYPE                              
PASSABOX DC    C'B'                PASSING BOX ADDRESS TO REQTWA                
                                                                                
LEQUAL   DC    C'L='                                                            
EFFS     DC    X'FFFFFFFF'                                                      
EZEROES  DC    C'000000'                                                        
BL01     DC    C'BL01'                                                          
T00A     DC    X'D9000A'                                                        
                                                                                
DMADDR   DC    CL(L'COMMAND)'ADDREC'                                            
DMPUTR   DC    CL(L'COMMAND)'PUTREC'                                            
DMGETR   DC    CL(L'COMMAND)'GETREC'                                            
DMADD    DC    CL(L'COMMAND)'DMADD'                                             
DMWRTL   DC    CL(L'COMMAND)'DMWRT'                                             
DMREAD   DC    CL(L'COMMAND)'DMREAD'                                            
DMRDHI   DC    CL(L'COMMAND)'DMRDHI'                                            
DMRSEQ   DC    CL(L'COMMAND)'DMRSEQ'                                            
DMRDIR   DC    CL(L'COMMAND)'DMRDIR'                                            
                                                                                
CTFILE   DC    CL(L'DMFILE)'CTFILE'                                             
GENDIR   DC    CL(L'DMFILE)'GENDIR'                                             
TEMPSTR  DC    CL(L'DMFILE)'TEMPSTR'                                            
                                                                                
LINKMAP  DS    0XL(LIORL)          ** LINKIO RECORD MAP **                      
         DC    X'FEF5',X'FEF5',AL2(LINKHDR-LINKMAP)                             
         DC    X'FEF4',X'FEF4',AL2(LINKDTA-LINKMAP)                             
LINKMAPX DC    AL2(0)                                                           
                                                                                
LINKHDR  DS    0XL(LIODL)          ** HEADER RECORD FIELDS **                   
                                                                                
         DC    AL2(1)              RECORD NUMBER                                
         DC    AL1(LIOBSB1Q)                                                    
         DC    AL2(LINK$REC-GEND)                                               
         DC    AL1(L'LINK$REC)                                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL2(2)              ACTION NUMBER                                
         DC    AL1(LIOBSB1Q)                                                    
         DC    AL2(LINK$ACT-GEND)                                               
         DC    AL1(L'LINK$ACT)                                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
LINKHDRX DC    AL2(0)                                                           
                                                                                
LINKDTA  DS    0X                  ** DATA RECORD FIELDS **                     
                                                                                
         DC    AL2(1)              FIELD NUMBER (EXTENDED HEADER)               
         DC    AL1(LIOBSB1Q)                                                    
         DC    AL2(LINK$FLD-GEND)                                               
         DC    AL1(L'LINK$FLD)                                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL2(2)              RELATIVE FIELD NUMBER                        
         DC    AL1(LIOBSB1Q)                                                    
         DC    AL2(LINK$REL-GEND)                                               
         DC    AL1(L'LINK$REL)                                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
                                                                                
         DC    AL2(3)              FIELD DATA                                   
         DC    AL1(LIOBSB1Q)                                                    
         DC    AL2(LINK$DTA-GEND)                                               
         DC    AL1(0)                                                           
         DC    AL1(LIODINDX)                                                    
         DC    AL1(0)                                                           
                                                                                
LINKDTAX DC    AL2(0)                                                           
                                                                                
SEQTAB   DC    C'  ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890'                        
                                                                                
RFPTAB   DS    0XL(RFPTABL)        ** RFP MODE TABLE **                         
         DC    AL1(QRFPINIT),AL2(RFPINI-GENNEW)                                 
         DC    AL1(QRFPGVAL),AL2(RFPVAL-GENNEW)                                 
         DC    AL1(QRFPSYMB),AL2(RFPSYM-GENNEW)                                 
RFPTABN  EQU   (*-RFPTAB)/L'RFPTAB                                              
                                                                                
SELTAB   DS    0XL(SELTABL)        ** SELECT ACTION TABLE **                    
         DC    AL2(LP@CHAS-GEND,LP@CHA-GEND)                                    
         DC    AL2(LP@SELS-GEND,LP@DIS-GEND)                                    
         DC    AL2(LP@DELS-GEND,LP@DEL-GEND)                                    
         DC    AL2(LP@RESS-GEND,LP@RES-GEND)                                    
         DC    AL2(LP@REPS-GEND,LP@REP-GEND)                                    
SELTABN  EQU   (*-SELTAB)/SELTABL                                               
                                                                                
LISTLST  DS    0XL(LISTLL)         ** LIST SCROLL ACTIONS **                    
         DC    AL2(LP@FRSTP-GEND),AL1(LISTSFST)                                 
         DC    AL2(LP@LASTP-GEND),AL1(LISTSLST)                                 
         DC    AL2(LP@NEXTP-GEND),AL1(LISTSNXT)                                 
         DC    AL2(LP@THISP-GEND),AL1(LISTSCUR)                                 
         DC    AL2(LP@XP-GEND),AL1(LISTSCUR)                                    
         DC    AL2(LP@EXITP-GEND),AL1(LISTSCUR)                                 
LISTLSTN EQU   (*-LISTLST)/LISTLL                                               
                                                                                
PRGOPTS  DS    0XL(PRTOPTL)        ** OPTIONS FOR PROGRAM RECORDS **            
         DC    AL2(LP@NO-GEND),AL1(WOK$NO,TWW$NOW)                              
         DC    AL2(VALOUT-GENNEW,0)                                             
         DC    AL2(LP@NEXT-GEND),AL1(WOK$NO,TWW$NOW)                            
         DC    AL2(VALOUT-GENNEW,0)                                             
                                                                                
PRTOPTS  DS    0XL(PRTOPTL)        ** GENERAL PRINT OPTIONS **                  
         DC    AL2(LP@NOW-GEND),AL1(WOK$NOW,TWW$NOW)                            
         DC    AL2(VPREMU-GENNEW,0)                                             
*&&UK*&& DC    AL2(LP@SOON-GEND),AL1(WOK$SOON,TWW$SOON)                         
*&&UK*&& DC    AL2(VPREMU-GENNEW,0)                                             
*&&UK*&& DC    AL2(LP@ASAP-GEND),AL1(WOK$SOON,TWW$SOON)                         
*&&UK*&& DC    AL2(VPREMU-GENNEW,0)                                             
*&&US*&& DC    AL2(LP@SOON-GEND),AL1(WOK$SOON,TWW$SOON)                         
*&&US*&& DC    AL2(VPREMU-GENNEW,VALPOPX-GENNEW)                                
*&&US*&& DC    AL2(LP@ASAP-GEND),AL1(WOK$SOON,TWW$SOON)                         
*&&US*&& DC    AL2(VPREMU-GENNEW,VALPOPX-GENNEW)                                
         DC    AL2(LP@OV-GEND),AL1(WOK$OV,TWW$OV)                               
         DC    AL2(VPREMU-GENNEW,VALPOPX-GENNEW)                                
         DC    AL2(LP@ONT-GEND),AL1(WOK$OV,TWW$OV)                              
         DC    AL2(VPREMU-GENNEW,VALPOPX-GENNEW)                                
         DC    AL2(LP@DDS-GEND),AL1(WOK$DDS,TWW$OV)                             
         DC    AL2(VPREMU-GENNEW,VALPOPX-GENNEW)                                
PRTOPTSX DC    AL1(FF)                                                          
                                                                                
UPRLWR   DC    XL16'00404040404040404040404040404040' 00-0F                     
         DC    XL16'40404040404040404040404040404040' 10-1F                     
         DC    XL16'40404040404040404040404040404040' 20-2F                     
         DC    XL16'40404040404040404040404040404040' 30-3F                     
         DC    XL16'404040404040404040404A4B4C4D4E4F' 40-4F                     
         DC    XL16'504040404040404040405A5B5C5D5E5F' 50-5F                     
         DC    XL16'606140404040404040406A6B6C6D6E6F' 60-6F                     
         DC    XL16'404040404040404040407A7B7C7D7E7F' 70-7F                     
         DC    XL16'40818283848586878889404040404040' 80-8F                     
         DC    XL16'40919293949596979899404040404040' 90-9F                     
         DC    XL16'4040A2A3A4A5A6A7A8A9404040404040' A0-AF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040' B0-BF                     
         DC    XL16'40818283848586878889404040404040' C0-CF                     
         DC    XL16'40919293949596979899404040404040' D0-DF                     
         DC    XL16'4040A2A3A4A5A6A7A8A9404040404040' E0-EF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040' F0-FF                     
                                                                                
CRESLIST DS    0AL1                ** CORE RESIDENT PHASE LIST **               
*&&US*&& DC    AL1(QBOOKVAL)                                                    
*&&UK*&& DC    AL1(FF)                                                          
         DC    AL1(QCENTER)                                                     
         DC    AL1(QCHOPPER)                                                    
         DC    AL1(QDAYVAL)                                                     
CRESSPUL DC    AL1(QSPOOL)                                                      
         DC    AL1(QSQUASH)                                                     
         DC    AL1(QTIMVAL)                                                     
*&&US*&& DC    AL1(QUNDAY)                                                      
*&&UK*&& DC    AL1(FF)                                                          
         DC    AL1(QUNDRLIN)                                                    
         DC    AL1(QUNTIME)                                                     
         DC    AL1(QXSORT)                                                      
         DC    AL1(QGENPRG)                                                     
         DC    AL1(QEDITOR)                                                     
CRESLISL EQU   *-CRESLIST                                                       
                                                                                
DICMIX   DS    0X                  ** MIXED CASE LITERALS **                    
         DCDDL GE#BYID,(#BYIDLQ)   BY ID                                        
         DCDDL GE#CHGNO,(#CHGNOLQ) CHANGE NO.                                   
         DCDDL GE#CHGR,(#CHGRLQ)   CHANGE REASON                                
         DCDDL GE#DSKAD,(#DSKADLQ) DISK ADDRESS                                 
         DCDDL GE#PASSW,(#PASSWLQ) PASSWORD                                     
         DCDDL GE#RECAD,(#RECADLQ) RECORD ADDED                                 
         DCDDL GE#RECLN,(#RECLNLQ) RECORD LENGTH                                
         DCDDL GE#SEC,(#SECLQ)     SECURITY                                     
         DCDDL GE#SECLV,(#SECLVLQ) SECURITY LEVEL                               
DICMIXX  DC    AL1(0)                                                           
                                                                                
DICUPR   DS    0X                  ** UPPER CASE LITERALS **                    
         DCDDL GE#ACTV,(#ACTVLQ)   ACTIVE                                       
         DCDDL GE#ACTV,(#ACTLQ)    ACT                                          
         DCDDL GE#ASAP,WHENLQ      ASAP                                         
         DCDDL GE#CHA,(#CHASLQ)    CHANGE (SINGLE CHR)                          
         DCDDL GE#CHA,(#CHALQ)     CHANGE (3 CHR)                               
         DCDDL GE#DEL,(#DELSLQ)    DELETE (SINGLE CHR)                          
         DCDDL GE#DEL,(#DELLQ)     DELETE (3 CHR)                               
         DCDDL GE#DDS,WHENLQ       DDS (AT DDS)                                 
         DCDDL GE#DIS,(#DISSLQ)    DISPLAY (SINGLE CHR)                         
         DCDDL GE#DIS,(#DISLQ)     DISPLAY (3 CHR)                              
         DCDDL GE#DIS,(#DISLLQ)    DISPLAY (FULL WORD)                          
         DCDDL GE#EXIT,(#EXITPLQ)  EXIT (PADDED)                                
         DCDDL GE#FIRST,(#FRSTPLQ) FIRST (PADDED)                               
         DCDDL GE#HELP,(#HELPPLQ)  HELP (PADDED)                                
         DCDDL GE#ID,(#IDLQ)       ID                                           
         DCDDL GE#LAST,(#LASTPLQ)  LAST (PADDED)                                
         DCDDL GE#LIS,(#LISTPLQ)   LIST (PADDED)                                
         DCDDL GE#ME,(#MELQ)       ME                                           
         DCDDL GE#NEXT,(#NEXTLQ)   NEXT                                         
         DCDDL GE#NEXT,(#NEXTPLQ)  NEXT (PADDED)                                
         DCDDL GE#NO,WHENLQ        NO                                           
         DCDDL GE#NOW,WHENLQ       NOW                                          
         DCDDL GE#ON,(#OLQ)        O (OVERNIGHT)                                
         DCDDL GE#ONT,WHENLQ       ON (OVERNIGHT)                               
         DCDDL GE#OV,WHENLQ        OV (OVERNIGHT)                               
         DCDDL GE#PASSW,(#PASSLQ)  PASS                                         
         DCDDL GE#REP,(#REPSLQ)    REPORT (SINGLE CHR)                          
         DCDDL GE#REP,(#REPLQ)     REPORT (3 CHR)                               
         DCDDL GE#REQ,(#REQLQ)     REQ                                          
         DCDDL GE#RES,(#RESSLQ)    RESTORE (SINGLE CHR)                         
         DCDDL GE#RES,(#RESLQ)     RESTORE (3 CHR)                              
         DCDDL GE#SEL,(#SELSLQ)    SELECT (SINGLE CHR)                          
         DCDDL GE#SEL,(#SELPLQ)    SELECT (PADDED)                              
         DCDDL GE#SOON,WHENLQ      SOON                                         
         DCDDL GE#THIS,(#THISPLQ)  THIS (PADDED)                                
         DCDDL GE#TODAY,(#TODAYLQ) TODAY                                        
         DCDDL GE#X,(#XPLQ)        'X' (PADDED)                                 
DICUPRX  DC    AL1(0)                                                           
         EJECT                                                                  
RFPTABD  DSECT                     ** RFP MODE TABLE **                         
RFPTMODE DS    AL1                 CALLING MODE                                 
RFPTADDR DS    AL2                 ROUTINE ADDRESS                              
RFPTABL  EQU   *-RFPTABD                                                        
                                                                                
LISTLD   DSECT                     ** LIST OPTIONS TABLE **                     
LISTSNAM DS    AL2                 NAME                                         
LISTSET  DS    C                   ** LISTSW SETTING **                         
LISTSFST EQU   C'F'                FIRST                                        
LISTSLST EQU   C'L'                LAST                                         
LISTSNXT EQU   C'N'                NEXT                                         
LISTSCUR EQU   C'T'                RE-DISPLAY                                   
LISTLL   EQU   *-LISTLD                                                         
                                                                                
PRTOPTD  DSECT                     ** PRINT OPTIONS (WHEN) TABLE **             
PRTONAME DS    AL2                 NAME                                         
PRTOWHEN DS    X                   WHEN SETTING                                 
PRTOWVAL DS    X                   TWAWHEN SETTING                              
PRTONSPO DS    AL2                 BRANCH DISP. NON-SPOOL (0=INVALID)           
PRTOYSPO DS    AL2                 DITTO SPOOL-ONLY                             
PRTOPTL  EQU   *-PRTOPTD                                                        
                                                                                
SELTABD  DSECT                     ** SELECT ACTION TABLE **                    
SELTCHAR DS    AL2                 SINGLE CHARACTER                             
SELTNAME DS    AL2                 3 CHARACTER EXPANSION                        
SELTABL  EQU   *-SELTABD                                                        
                                                                                
ACDD     DSECT                     ** ACTIVITY DISPLAY SCREEN LINE **           
         DS    XL8                                                              
ACDDATA  DS    0CL70                                                            
ACDTXT1  DS    CL18                                                             
ACDDATE  DS    CL9                                                              
         DS    C                                                                
ACDTXT2  DS    CL10                                                             
ACDIDNO  DS    CL4                                                              
         DS    C                                                                
ACDID    DS    CL10                                                             
         DS    C                                                                
ACDPASS  DS    CL12                                                             
         ORG   ACDDATE                                                          
ACDDSKAD DS    CL8                                                              
         ORG   ACDDATE                                                          
ACDRECL  DS    CL4                                                              
         ORG   ACDDATE                                                          
ACDSECLV DS    CL3                                                              
         ORG   ACDDATE                                                          
ACDCHRSN DS    CL40                                                             
         ORG   ACDTXT1+14                                                       
ACDCHNO  DS    CL3                                                              
         ORG   ACDDATA+L'ACDDATA                                                
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
* OTHER INCLUDED BOOKS HERE                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE DDSECURED                                                      
       ++INCLUDE FAPQPL                                                         
       ++INCLUDE DDSPOOK                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAUTL                                                          
       ++INCLUDE FAGETTXTD                                                      
TWAD     DSECT                                                                  
         DS    CL64                                                             
CONHEADH DS    CL8                                                              
CONHEAD  DS    CL60                                                             
CONSERVH DS    CL8                                                              
CONSERV  DS    CL17                                                             
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDLANGEQUS                                                     
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDFLDHDR                                                       
FLDHDRL  EQU   FLDDATA-FLDHDRD                                                  
FLDLEND  EQU   FLDLEN-FLDHDRD                                                   
FLDATBD  EQU   FLDATB-FLDHDRD                                                   
FLDILEND EQU   FLDILEN-FLDHDRD                                                  
FLDIINDD EQU   FLDIIND-FLDHDRD                                                  
FLDOINDD EQU   FLDOIND-FLDHDRD                                                  
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTGENEDICT                                                     
       ++INCLUDE GEGENREF                                                       
       ++INCLUDE DMREQHDR                                                       
       ++INCLUDE DDFAXINFOD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE GERFPIOD                                                       
       ++INCLUDE GEGENRFPD                                                      
LIOBD    DSECT                                                                  
       ++INCLUDE DDLINKIOD                                                      
       ++INCLUDE DDLINKD                                                        
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004GEGENNEW  10/11/06'                                      
         END                                                                    
