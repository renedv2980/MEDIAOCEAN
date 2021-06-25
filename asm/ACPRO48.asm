*          DATA SET ACPRO48    AT LEVEL 008 AS OF 02/25/15                      
*PHASE T60B48A                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B48 - TEXT LIST'                                             
T60B48   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B48**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK AREA                    
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         ST    R2,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    KEYLOGIC                                                         
         CLI   MODE,VALREC                                                      
         BE    RECLOGIC                                                         
         B     XIT                                                              
         SPACE 3                                                                
***********************************************************************         
* VALKEY  LOGIC                                                       *         
***********************************************************************         
*                                                                               
KEYLOGIC LA    RE,LOCAL            CLEAR LOCAL STORAGE                          
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         BAS   RE,VALHED           VALIDATE SCREEN                              
*                                                                               
         MVI   INTMODE,EDTLIST     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES - SET FIRST TIME LIST                    
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES - SET FIRST TIME LIST                    
         B     XIT                                                              
         SPACE 3                                                                
***********************************************************************         
* VALREC  LOGIC - DISPLAY OR CHANGE                                   *         
***********************************************************************         
*                                                                               
RECLOGIC BAS   RE,SETSCR           SET SCREEN LINE ADDRESSES                    
         BAS   RE,GETTAB           GET STORAGE                                  
         CLI   INTMODE,FSTLIST     FIRST TIME THROUGH ?                         
         BE    DISLOGIC            YES                                          
         BAS   RE,EDT                                                           
         MVI   INTMODE,DISLIST     CONTINUE LIST                                
         B     DISLOGIC                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE HEADING FIELD(S)                                           *         
***********************************************************************         
*                                                                               
VALHED   NTR1                                                                   
         LA    R2,CONRECH                                                       
         MVI   KEYCHG,C'N'                                                      
         MVI   OPTION,0                                                         
         GOTO1 SETHEIR                                                          
*                                                                               
         LA    R2,TXLOFGH          OFFICE GROUP IS OPTIONAL                     
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALH020                                                          
         GOTO1 VALOG                                                            
*                                                                               
VALH020  LA    R2,TXLOFFH          OFFICE IS OPTIONAL                           
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALH040                                                          
         MVI   ERROR,NOTOFNOG                                                   
         CLI   TXLOFGH+5,0         NOT COMPATIBLE WITH OFFICE GROUP             
         BNE   ERREXIT                                                          
         GOTO1 VALOFF                                                           
*                                                                               
VALH040  LA    R2,TXLCLIH          CLIENT IS OPTIONAL                           
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALH060                                                          
         MVI   ERROR,NOTCLNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   TXLOFGH+5,0                                                      
         BNE   ERREXIT                                                          
         MVI   ERROR,NOTCLNOF      NOT COMPATIBLE WITH OFFICE                   
         CLI   TXLOFFH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALCLI                                                           
*                                                                               
VALH060  LA    R2,TXLPROH          PRODUCT IS OPTIONAL                          
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALH080                                                          
         MVI   ERROR,NEEDCLI       NEED CLIENT IF PRODUCT ENTERED               
         CLI   TXLCLIH+5,0                                                      
         BE    ERREXIT                                                          
         GOTO1 VALPROD                                                          
*                                                                               
VALH080  LA    R2,TXLJOBH          JOB IS OPTIONAL                              
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALH100                                                          
         MVI   ERROR,NEEDPRO       NEED PRODUCT IF JOB ENTERED                  
         CLI   TXLPROH+5,0                                                      
         BE    ERREXIT                                                          
         GOTO1 VALJOB                                                           
*                                                                               
VALH100  LA    R2,TXLMGRH          MEDIA GROUP IS OPTIONAL                      
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALH140                                                          
         MVI   ERROR,NOTJBNME                                                   
         CLI   TXLJOBH+5,0                                                      
         BNE   ERREXIT                                                          
*                                                                               
VALH120  GOTO1 VALMG                                                            
*                                                                               
VALH140  LA    R2,TXLMEDH          MEDIA IS OPTIONAL                            
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHEDX                                                          
         MVI   ERROR,NOTJBNME                                                   
         CLI   TXLJOBH+5,0                                                      
         BNE   ERREXIT                                                          
         MVI   ERROR,NOTMENMG                                                   
         CLI   TXLMGRH+5,0                                                      
         BNE   ERREXIT                                                          
*                                                                               
VALH160  GOTO1 VALMED                                                           
*                                                                               
VALHEDX  CLI   KEYCHG,C'Y'         DID SOMETHING CHANGE ?                       
         BNE   XIT                 NO, EXIT                                     
         BAS   RE,LOAD             YES, LOAD UP BUFFER TO WRITE TO              
         BAS   RE,PUTTAB           SAVE IT FOR LATER                            
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
TSTKEY   TM    4(R2),X'20'         HAS FIELD CHANGED ?                          
         BOR   RE                  NO, EXIT                                     
         OI    4(R2),X'20'         YES, INDICATE VALIDATED                      
         MVI   KEYCHG,C'Y'          AND SET SWITCH                              
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CLEAR SCREEN AND TABLE, BRANCH TO READ AND DISPLAY ROUTINES         *         
* AND DETERMINE APPROPRIATE MESSAGE                                   *         
***********************************************************************         
*                                                                               
DISLOGIC GOTO1 VCLEARF,DMCB,AFSTSEL,AENDSCR                                     
         GOTO1 (RF),(R1),(1,AFSTSEL),APFFLD                                     
         MVI   NUMLINES,0                                                       
*                                                                               
         L     RE,ASTORAGE         CLEAR SELTAB                                 
         LH    RF,=YL2(MAXLINE*SELTABL)                                         
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CLI   INTMODE,FSTLIST                                                  
         BNE   *+10                                                             
*                                                                               
         XC    LLASTTXT,LLASTTXT   CLEAR OUT LAST TEXT LISTED                   
         BAS   RE,READLST                                                       
         L     R2,AFSTSEL                                                       
         CLI   NUMLINES,MAXLINE    SEE IF SCREEN IS FULL                        
         BE    FULLMESS            YES                                          
         SR    R3,R3               SEE IF SCREEN IS EMPTY                       
         ICM   R3,1,NUMLINES       YES                                          
         BZ    NONEMESS                                                         
         LA    R2,TXLOFGH          POSITION CURSOR AT FIRST KEY FIELD           
         XC    LLASTTXT,LLASTTXT   CLEAR LAST TEXT LISTED                       
         MVC   CONHEAD(L'DONEMSG),DONEMSG                                       
DISLOGX  ST    R2,ACURFORC                                                      
         BAS   RE,PUTTAB           SAVE TABLE                                   
         B     XIT                                                              
*                                                                               
FULLMESS MVC   CONHEAD(L'FULLMSG),FULLMSG                                       
         B     DISLOGX                                                          
*                                                                               
NONEMESS MVC   CONHEAD(L'NONEMSG),NONEMSG                                       
         B     DISLOGX                                                          
         EJECT                                                                  
***********************************************************************         
* READ A RECORD, SET THE LINE AND DISPLAY THE DATA                    *         
***********************************************************************         
*                                                                               
READLST  NTR1                                                                   
         L     R2,AFSTSEL          FIND NEXT AVAILABLE LINE                     
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES                                                    
         BZ    READ020                                                          
         MH    R3,=Y(MAXFIELD)                                                  
         BAS   RE,BUMP                                                          
         BCT   R3,*-4                                                           
*                                                                               
READ020  ST    R2,ATHISLIN                                                      
         LA    R2,TXLOFGH                                                       
*                                                                               
         LA    R6,SRCHKEY                                                       
         USING ACTXKEY,R6                                                       
         XC    ACTXKEY,ACTXKEY                                                  
         MVI   ACTXRTYP,ACTXEQU                                                 
         MVI   ACTXSREC,ACTXSEQU                                                
         MVC   ACTXCUL,CUL                                                      
*                                                                               
         OC    LLASTTXT,LLASTTXT   IS THIS A CONTINUATION ?                     
         BNZ   READ900             YES                                          
*                                                                               
         OC    CLICODE,CLICODE     DO WE HAVE A CLIENT ?                        
         BZ    READ200             NO, LOOK FOR OFFICE/OFFICE GROUP             
         MVC   ACTXCLI,CLICODE     YES, MOVE IT TO KEY                          
         OC    PRODCODE,PRODCODE   DO WE HAVE A PRODUCT ?                       
         BZ    READ140             NO, READ CLIENT ONLY                         
         MVC   ACTXPROD,PRODCODE   YES, MOVE IT TO KEY                          
         OC    JOBNUM,JOBNUM       DO WE HAVE A JOB ?                           
         BZ    READ140             NO, READ CLIENT, PRODUCT ONLY                
         MVC   ACTXJOB,JOBNUM      YES, MOVE IT TO KEY                          
         MVC   SAVERT,=S(*+6)                                                   
         BAS   RE,PRNTALL                                                       
         BE    READX                                                            
         XC    ACTXJOB,ACTXJOB                                                  
*                                                                               
READ140  OC    MEDIA,MEDIA                                                      
         BZ    READ160                                                          
         MVC   ACTXMED,MEDIA                                                    
         MVC   SAVERT,=S(*+6)                                                   
         BAS   RE,PRNTALL                                                       
         BE    READX                                                            
         XC    ACTXMED,ACTXMED                                                  
*                                                                               
READ160  OC    MGROUP,MGROUP                                                    
         BZ    READ180                                                          
         MVC   ACTXMG,MGROUP                                                    
         MVC   SAVERT,=S(*+6)                                                   
         BAS   RE,PRNTALL                                                       
         BE    READX                                                            
         XC    ACTXMG,ACTXMG                                                    
*                                                                               
READ180  MVC   SAVERT,=S(*+6)                                                   
         BAS   RE,PRNTALL                                                       
         BE    READX                                                            
         OC    ACTXPROD,ACTXPROD                                                
         BZ    READ200                                                          
         XC    ACTXPROD,ACTXPROD                                                
         B     READ140                                                          
*                                                                               
*                                                                               
READ200  XC    ACTXOG(ACTXWHER-ACTXOG+2),ACTXOG                                 
         CLC   EFFOFFC,SPACES                                                   
         BNH   READ300                                                          
         MVC   ACTXOFC,EFFOFFC                                                  
         OC    MEDIA,MEDIA                                                      
         BZ    READ240                                                          
         MVC   ACTXMED,MEDIA                                                    
         MVC   SAVERT,=S(*+6)                                                   
         BAS   RE,PRNTALL                                                       
         BE    READX                                                            
         XC    ACTXMED,ACTXMED                                                  
*                                                                               
READ240  OC    MGROUP,MGROUP                                                    
         BZ    READ280                                                          
         MVC   ACTXMG,MGROUP                                                    
         MVC   SAVERT,=S(*+6)                                                   
         BAS   RE,PRNTALL                                                       
         BE    READX                                                            
         XC    ACTXMG,ACTXMG                                                    
*                                                                               
READ280  MVC   SAVERT,=S(*+6)                                                   
         BAS   RE,PRNTALL                                                       
         BE    READX                                                            
*                                                                               
READ300  XC    ACTXOG(ACTXWHER-ACTXOG+2),ACTXOG                                 
         OC    EFFOFG,EFFOFG                                                    
         BZ    READ400                                                          
         MVC   ACTXOG,EFFOFG                                                    
         OC    MEDIA,MEDIA                                                      
         BZ    READ340                                                          
         MVC   ACTXMED,MEDIA                                                    
         MVC   SAVERT,=S(*+6)                                                   
         BAS   RE,PRNTALL                                                       
         BE    READX                                                            
         XC    ACTXMED,ACTXMED                                                  
*                                                                               
READ340  OC    MGROUP,MGROUP                                                    
         BZ    READ380                                                          
         MVC   ACTXMG,MGROUP                                                    
         MVC   SAVERT,=S(*+6)                                                   
         BAS   RE,PRNTALL                                                       
         BE    READX                                                            
         XC    ACTXMG,ACTXMG                                                    
*                                                                               
READ380  MVC   SAVERT,=S(*+6)                                                   
         BAS   RE,PRNTALL                                                       
         BE    READX                                                            
*                                                                               
READ400  XC    ACTXOG(ACTXWHER-ACTXOG+2),ACTXOG                                 
         OC    MEDIA,MEDIA                                                      
         BZ    READ500                                                          
         MVC   ACTXMED,MEDIA                                                    
         MVC   SAVERT,=S(*+6)                                                   
         BAS   RE,PRNTALL                                                       
         BE    READX                                                            
         XC    ACTXMED,ACTXMED                                                  
*                                                                               
         MVC   SAVERT,=S(*+6)                                                   
*                                                                               
READ500  OC    MGROUP,MGROUP                                                    
         BZ    READ600                                                          
         MVC   ACTXMG,MGROUP                                                    
         MVC   SAVERT,=S(*+6)                                                   
         BAS   RE,PRNTALL                                                       
         BE    READX                                                            
         XC    ACTXMG,ACTXMG                                                    
*                                                                               
READ600  MVC   SAVERT,=S(*+6)                                                   
         BAS   RE,PRNTALL                                                       
*                                                                               
READX    B     XIT                                                              
*                                                                               
READ900  MVC   ACTXKEY(L'LLASTTXT),LLASTTXT                                     
         LA    R1,ACTXWHER+1       GET TO LAST BYTE OF RECORD                   
         SR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         LA    RF,1(RF)            INCREMENT IT                                 
         STC   RF,0(R1)                                                         
*                                                                               
         MVC   NEXTREAD+2(2),SAVERT                                             
NEXTREAD DS    0H                                                               
**** DYNAMICALLY-CONSTRUCTED UNCONDITIONAL BRANCH INSTRUCTION ****              
**** NON-REENTRANT ****                                                         
         DC    X'47F0',S(0)                                                     
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* READ AND PRINT ALL ENTRIES WITH MATCHING KEYS                       *         
***********************************************************************         
*                                                                               
PRNTALL  NTR1                                                                   
         L     R6,AIO                                                           
         USING ACTXKEY,R6                                                       
         MVC   KEY,SRCHKEY                                                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
PRNT020  CLC   KEY(ACTXCAT-ACTXKEY),KEYSAVE                                     
         BNE   PRNT040                                                          
*                                                                               
         L     R2,ATHISLIN         ADDRESS CURRENT LINE                         
         BAS   RE,SETLIN           SET ADDRESSES                                
         MVC   LLASTTXT,ACTXKEY    SAVE KEY OF RECORD                           
         MVC   ATHISLIN,ANEXTSEL   UPDATE LINE POINTER                          
*                                                                               
         SR    RE,RE                                                            
         IC    RE,NUMLINES                                                      
         LR    R5,RE                                                            
         LA    RE,1(RE)                                                         
         STC   RE,NUMLINES                                                      
*                                                                               
         MH    R5,=Y(SELTABL)                                                   
         L     RE,ASTORAGE                                                      
         LA    R5,0(RE,R5)         ADDRESS TABLE ENTRY                          
         USING SELTABD,R5                                                       
         MVC   SELTAB,ACTXKEY                                                   
*                                                                               
         BAS   RE,DISPLAY                                                       
*                                                                               
         CLI   NUMLINES,MAXLINE    IS SCREEN FULL ?                             
         BE    PRNTX               YES, EXIT WITH RETURN CODE EQUAL             
*                                                                               
         GOTO1 SEQ                                                              
         B     PRNT020                                                          
*                                                                               
PRNT040  LTR   R8,R8               FORCE RETURN CODE OF NOT EQUAL               
         MVC   KEY,KEYSAVE         RESTORE KEY WE WERE LOOKING FOR              
*                                                                               
PRNTX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY A LINE OF DATA                                              *         
***********************************************************************         
*                                                                               
DISPLAY  NTR1                                                                   
*                                                                               
         L     R2,ASEL             SELECT VALIDATED                             
         OI    4(R2),X'20'                                                      
*                                                                               
         L     R2,AOFG             OFFICE GROUP VALIDATED                       
         OI    4(R2),X'20'                                                      
         MVC   8(L'ACTXOG,R2),ACTXOG                                            
*                                                                               
         L     R2,AOFF             OFFICE VALIDATED                             
         OI    4(R2),X'20'                                                      
         MVC   8(L'ACTXOFC,R2),ACTXOFC                                          
*                                                                               
         L     R2,ACLI             CLIENT VALIDATED                             
         OI    4(R2),X'20'                                                      
         MVC   8(L'ACTXCLI,R2),ACTXCLI                                          
*                                                                               
         L     R2,APRO             PRODUCT VALIDATED                            
         OI    4(R2),X'20'                                                      
         MVC   8(L'ACTXPROD,R2),ACTXPROD                                        
*                                                                               
         L     R2,AJOB             JOB VALIDATED                                
         OI    4(R2),X'20'                                                      
         MVC   8(L'ACTXJOB,R2),ACTXJOB                                          
*                                                                               
         L     R2,AMGR             MEDIA GROUP VALIDATED                        
         OI    4(R2),X'20'                                                      
         MVC   8(L'ACTXMG,R2),ACTXMG                                            
*                                                                               
         L     R2,AMED             MEDIA VALIDATED                              
         OI    4(R2),X'20'                                                      
         MVC   8(L'ACTXMED,R2),ACTXMED                                          
*                                                                               
         L     R2,ACAT             CATEGORY VALIDATED                           
         OI    4(R2),X'20'                                                      
         MVC   8(L'ACTXCAT,R2),ACTXCAT                                          
*                                                                               
         L     R2,AWRK             WORKCODE VALIDATED                           
         OI    4(R2),X'20'                                                      
         MVC   8(L'ACTXWORK,R2),ACTXWORK                                        
         MVC   10(L'ACTXSUFF,R2),ACTXSUFF                                       
*                                                                               
         L     R2,AFRM             FORM VALIDATED                               
         OI    4(R2),X'20'                                                      
         MVC   8(L'ACTXFORM,R2),ACTXFORM                                        
*                                                                               
         L     R2,AWHR             WHERE VALIDATED                              
         OI    4(R2),X'20'                                                      
         MVC   8(L'ACTXWHER,R2),ACTXWHER                                        
         CLI   ACTXWHER+1,C' '     ONLY 1 CHARACTER ?                           
         BE    DISP020                                                          
         XC    8(1,R2),9(R2)                                                    
         XC    9(1,R2),8(R2)                                                    
         XC    8(1,R2),9(R2)                                                    
*                                                                               
DISP020  MVC   SELWHER,8(R2)                                                    
*                                                                               
         MVI   ELCODE,ACTHELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACTHD,R6                                                         
         L     R2,APAN             PANEL VALIDATED                              
         OI    4(R2),X'20'                                                      
         MVC   8(L'ACTHPAN,R2),ACTHPAN                                          
*                                                                               
         L     R2,ACOM             COMMENT VALIDATED                            
         OI    4(R2),X'20'                                                      
         MVC   8(L'ACTHCOM,R2),ACTHCOM                                          
*                                                                               
DISLINEX B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* EDIT THE SCREEN LOOKING FOR SELECT FIELD OF 'S'                     *         
***********************************************************************         
*                                                                               
EDT      NTR1                                                                   
         L     R2,AFSTSEL          R2=A(FIRST SELECT FIELD)                     
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES       NUMBER OF LINES ON SCREEN                    
         BZ    EDTX                                                             
         L     R5,ASTORAGE         ADDRESS OF SELECT TABLE                      
         USING SELTABD,R5                                                       
*                                                                               
EDT2     BAS   RE,SETLIN           GET FIELD ADDRESSES FOR THIS LINE            
         L     R2,ASEL                                                          
         CLI   5(R2),0             IF NOTHING IN SELECT TRY NEXT LINE           
         BE    EDT6                                                             
         CLI   5(R2),1             IF IN SELECT, MUST BE 1 BYTE                 
         BNE   INVEND                                                           
         CLI   8(R2),C'*'          IF ALREADY EDITED GET NEXT LINE              
         BE    EDT6                                                             
         CLI   8(R2),C'S'          IF OTHER THAN 'S' - ERROR                    
         BNE   INVEND                                                           
*                                                                               
EDT4     MVI   8(R2),C'*'                                                       
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         LA    R6,SELTAB                                                        
         DROP  R5                                                               
         USING SELTABD,R6                                                       
         GOTO1 VCALL,WORK,=C'TEXT',=C'MAINT',(1,SELOFG),(2,SELOFF),(6,SX        
               ELCLI),(6,SELPRO),(6,SELJOB),(1,SELMG),(1,SELMED),(2,SELX        
               CAT),(3,SELWORK),(1,SELFORM),(2,SELWHER),0                       
*                                                                               
EDT6     L     R2,ANEXTSEL                                                      
         LA    R5,SELTABL(R5)                                                   
         BCT   R3,EDT2                                                          
*                                                                               
EDTX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD THE BUFFER WHERE SELTAB WILL BE SAVED                          *         
***********************************************************************         
*                                                                               
LOAD     NTR1                                                                   
         L     RE,=V(DUMMY)                                                     
         A     RE,RELO                                                          
         ST    RE,DMCB             SET LOAD POINT FOR BUFFER                    
         MVC   DMCB+4(3),SYSPHASE                                               
         MVI   DMCB+7,X'50'        SET BUFFER OVERLAY NUMBER                    
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'        IS LOAD OK ?                                 
         BNE   *+6                 YES                                          
         DC    H'0'                NO, BLOW UP                                  
         MVC   ASTORAGE,DMCB       SAVE THE ADDRESS                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO PUT STORAGE DATA FROM BUFF TO TWA 2                  *         
***********************************************************************         
*                                                                               
PUTTAB   ST    RE,SAVERE                                                        
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,2            PAGE=TWA2                                    
         MVC   DMCB+10(2),TERM     TERMINAL NUMBER                              
         GOTO1 DATAMGR,DMCB,=CL8'DMWRT',=C'TEMPSTR',,ASTORAGE                   
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 3                                                                
***********************************************************************         
* SUB-ROUTINE TO GET STORAGE DATA FROM TWA2                           *         
***********************************************************************         
*                                                                               
GETTAB   ST    RE,SAVERE                                                        
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,2                                                         
         MVC   DMCB+10(2),TERM                                                  
         GOTO1 DATAMGR,DMCB,=CL8'DMREAD',=C'TEMPSTR',,BUFF                      
         LA    RE,BUFF                                                          
         ST    RE,ASTORAGE                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SET SCREEN LINE ADDRESSES                                           *         
***********************************************************************         
*                                                                               
SETSCR   ST    RE,SAVERE                                                        
         LA    R2,TXLSELH                                                       
         ST    R2,AFSTSEL                                                       
         LA    R0,MAXLINE                                                       
         LA    R1,MAXFIELD                                                      
         MR    R0,R0               COMPUTE N'FIELDS TO BUMP                     
         BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
         ST    R2,APFFLD                                                        
         BAS   RE,BUMP                                                          
         ST    R2,AENDSCR          NOTE END OF SCREEN                           
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SET DATA LINE ADDRESSES AND GET NEXT LINE                           *         
* AT ENTRY R2 =A(SELECT FIELD HEADER)                                 *         
***********************************************************************         
*                                                                               
SETLIN   ST    RE,SAVERE                                                        
         LA    R0,MAXFIELD         NUMBER OF FIELDS PER LINE                    
         LA    R1,ASEL             SAVE ADDRESS OF SELECT                       
         ST    R2,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BAS   RE,BUMP             REPEAT FOR PROTECTED FIELDS                  
         BCT   R0,*-12                                                          
         ST    R2,ANEXTSEL         LINE DONE, SAVE NEXT SELECT ADDRESS          
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 3                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 3                                                                
INDEX    DC    AL1(ACTXCLI-ACTXKEY)                                             
         DC    AL1(ACTXPROD-ACTXKEY)                                            
         DC    AL1(ACTXJOB-ACTXKEY)                                             
         SPACE 3                                                                
INVEND   MVI   ERROR,INVALID                                                    
ERREXIT  GOTO1 VERRCUR                                                          
         SPACE 3                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
FULLMSG  DC    CL50'LIST DISPLAYED - PRESS ENTER FOR NEXT PAGE'                 
DONEMSG  DC    CL50'END OF DISPLAY'                                             
NONEMSG  DC    CL50'NO DATA TO DISPLAY'                                         
         SPACE 3                                                                
RELO     DC    A(0)                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROB8D                                                       
         DS    0F                                                               
*                                                                               
NUMLINES DS    X                   N'LINES ON CURRENT SCREEN                    
LLASTTXT DS    CL(L'SELTAB)        LAST JOB ON SCREEN                           
*                                                                               
ASTORAGE DS    A                   ADDRESS OF STORAGE BUFFER                    
*                                                                               
SAVERT   DS    XL2                 SAVE BASE/DISPL FOR NEXT SCREEN              
*                                                                               
MAXLINE  EQU   13                  MAXIMUM N'LINES PER SCREEN                   
FSTLIST  EQU   1                   FIRST TIME SWITCH                            
DISLIST  EQU   2                   DISPLAY SWITCH                               
EDTLIST  EQU   3                   EDIT SWITCH                                  
         EJECT                                                                  
*                                                                               
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN               USE THE DRONEBLK AREA                        
LOCAL    DS    0C                                                               
KEYCHG   DS    C                                                                
INTMODE  DS    X                   INTERNAL MODE                                
*                                                                               
SAVERE   DS    A                   SAVE RE FOR SUB-ROUTINES                     
SRCHKEY  DS    CL42                KEY FOR TEXT RECORD SEARCH                   
*                                                                               
AFSTSEL  DS    A                   A(FIRST SELECT FIELD)                        
APFFLD   DS    A                   A(PF LINE)                                   
AENDSCR  DS    A                   A(LAST LINE)                                 
*                                                                               
ATHISLIN DS    A                   A(CURRENT LINE)                              
ANEXTSEL DS    A                   A(NEXT LINE)                                 
*                                                                               
ASEL     DS    A                   A(SELECT FIELD)                              
AOFG     DS    A                   A(OFFICE GROUP FIELD)                        
AOFF     DS    A                   A(OFFICE FIELD)                              
ACLI     DS    A                   A(CLIENT FIELD)                              
APRO     DS    A                   A(PRODUCT FIELD)                             
AJOB     DS    A                   A(JOB FIELD)                                 
AMGR     DS    A                   A(MEDIA GROUP FIELD)                         
AMED     DS    A                   A(MEDIA FIELD)                               
ACAT     DS    A                   A(CATEGORY FIELD)                            
AWRK     DS    A                   A(WORKCODE FIELD)                            
AFRM     DS    A                   A(FORM FIELD)                                
AWHR     DS    A                   A(WHERE FIELD)                               
ACOM     DS    A                   A(COMMENT FIELD)                             
APAN     DS    A                   A(PANEL FIELD)                               
MAXFIELD EQU   (*-ASEL)/L'ASEL     MAXIMUM N'FIELDS PER LINE                    
*                                                                               
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
*                                                                               
* DSECT TO COVER SELECT TABLE                                                   
*                                                                               
SELTABD  DSECT                                                                  
SELTAB   DS    0CL(SELTABL)                                                     
SELKEY   DS    CL5                                                              
SELOFG   DS    X                                                                
SELOFF   DS    XL2                                                              
SELCLI   DS    CL6                                                              
SELPRO   DS    CL6                                                              
SELJOB   DS    CL6                                                              
SELMG    DS    CL1                                                              
SELMED   DS    CL1                                                              
SELCAT   DS    CL2                                                              
SELWORK  DS    CL2                                                              
SELSUFF  DS    CL1                                                              
SELFORM  DS    C                                                                
SELWHER  DS    CL2                                                              
SELTABL  EQU   *-SELKEY                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACPRO48   02/25/15'                                      
         END                                                                    
