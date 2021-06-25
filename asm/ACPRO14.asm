*          DATA SET ACPRO14    AT LEVEL 039 AS OF 07/10/20                      
*PHASE T60B14A                                                                  
         TITLE 'T60B14 - OPTION LIST'                                           
*GHOA 38 14FEB18 DSRD-18081 NEW OPT/MAIN 'EMAIL IF PERSN ASSIGN 2 JOB'          
*ASAX 39 26MAY20 DSRD-25987 NEW OPT/MAIN 'DATE EST IN LAST FISCAL YR'           
                                                                                
T60B14   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B14**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK AREA                    
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BE    KEYLOGIC                                                         
         CLI   MODE,VALREC                                                      
         BE    RECLOGIC                                                         
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
* VALKEY  LOGIC                                                                 
*                                                                               
KEYLOGIC LA    RE,LOCAL            CLEAR LOCAL STORAGE                          
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         BAS   RE,VALHED           VALIDATE SCREEN                              
         MVI   INTMODE,EDTLIST     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES - SET FIRST TIME LIST                    
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES - SET FIRST TIME LIST                    
         MVC   LCONTROL,QCONTROL   SAVE CONTROL FIELD VALUES                    
         CLI   INTMODE,FSTLIST     TEST FIRST TIME LIST                         
         BNE   *+8                                                              
         BAS   RE,BLDTWA           YES - BUILD SCREEN                           
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
* VALREC  LOGIC - DISPLAY OR CHANGE                                             
*                                                                               
RECLOGIC BAS   RE,SETSCR                                                        
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME LIST                     
         BE    DISLOGIC                                                         
         BAS   RE,TSTEDT           TEST FOR ANYTHING TO EDIT                    
         BE    EDTLOGIC            YES                                          
         MVI   INTMODE,DISLIST     NO, CONTINUE LIST                            
         SPACE 3                                                                
*                                                                               
* DISPLAY LOGIC                                                                 
*                                                                               
DISLOGIC GOTO1 VCLEARF,DMCB,AFSTSEL,AENDSCR                                     
         GOTO1 (RF),(R1),(1,AFSTSEL),APFFLD                                     
         MVI   LNLISTS,0                                                        
         LA    RE,LSELTAB          CLEAR TABLE                                  
         LA    RF,L'LSELTAB                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         CLI   INTMODE,FSTLIST                                                  
         BNE   *+10                                                             
         SPACE 1                                                                
         XC    LLASTJOB,LLASTJOB   CLEAR OUT LAST JOB LISTED                    
         BAS   RE,LIST                                                          
         L     R2,AFSTSEL                                                       
         CLI   LNLISTS,NLINES      SEE IF SCREEN IS FULL                        
         BE    FULLMESS            YES                                          
         SR    R1,R1                                                            
         ICM   R1,1,LNLISTS                                                     
         BZ    NONEMESS                                                         
         LA    R2,PROOGRH          POSITION CURSOR AT FIRST KEY FIELD           
         XC    LLASTJOB,LLASTJOB   CLEAR LAST JOB LISTED                        
         MVC   CONHEAD(L'LISTMSG2),LISTMSG2                                     
DISLOGX  ST    R2,ACURFORC                                                      
         B     XIT                                                              
         SPACE 1                                                                
FULLMESS MVC   CONHEAD(L'LISTMSG),LISTMSG                                       
         B     DISLOGX                                                          
         SPACE 1                                                                
NONEMESS MVC   CONHEAD(L'LISTMSG3),LISTMSG3                                     
         B     DISLOGX                                                          
         SPACE 1                                                                
*                                                                               
* EDIT LOGIC                                                                    
*                                                                               
EDTLOGIC BAS   RE,EDT                                                           
         L     R2,AFSTSEL                                                       
         ST    R2,ACURFORC                                                      
         MVC   CONHEAD(L'EDTMSG),EDTMSG                                         
         B     XIT                                                              
         EJECT                                                                  
VALHED   NTR1                                                                   
         MVC   PROTIT(L'REGTIT),REGTIT                                          
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR                                                          
         MVI   KEYCHG,C'N'                                                      
         MVI   OPTION,0                                                         
         LA    R2,PROOGRH          OPTIONAL OFFICE GROUP FILTER                 
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED2                                                          
         GOTO1 VALOG                                                            
         MVC   QOGR,EFFOFG                                                      
         SPACE 1                                                                
VALHED2  LA    R2,PROOFFH          OPTIONAL OFFICE FILTER                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED4                                                          
         MVI   ERROR,NOTOFNOG                                                   
         CLI   PROOGRH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALOFF                                                           
         MVC   QOFF,EFFOFFC                                                     
         SPACE 1                                                                
VALHED4  LA    R2,PROCLIH          OPTIONAL CLIENT FILTER                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED6                                                          
         MVI   ERROR,NOTCLNOG                                                   
         CLI   PROOGRH+5,0                                                      
         BNE   ERREXIT                                                          
         MVI   ERROR,NOTCLNOF                                                   
         CLI   PROOFFH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALCLI                                                           
         MVC   QCLI,CLICODE                                                     
         SPACE 1                                                                
VALHED6  LA    R2,PROPROH          OPTIONAL PRODUCT FILTER                      
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED8                                                          
         MVI   ERROR,NEEDCLI                                                    
         CLI   PROCLIH+5,0                                                      
         BE    ERREXIT                                                          
         GOTO1 VALPROD                                                          
         MVC   QPROD,PRODCODE                                                   
         SPACE 1                                                                
VALHED8  LA    R2,PROJOBH          OPTIONAL JOB FILTER                          
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED10                                                         
         MVI   ERROR,NEEDPRO                                                    
         CLI   PROPROH+5,0                                                      
         BE    ERREXIT                                                          
         GOTO1 VALJOB                                                           
         MVC   QJOB,JOBNUM                                                      
         SPACE 1                                                                
VALHED10 LA    R2,PROMGRH          OPTIONAL MEDIA GROUP FILTER                  
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED12                                                         
         MVI   ERROR,NOTJBNME                                                   
         CLI   PROJOBH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALMG                                                            
         MVC   QMGR,MGROUP                                                      
         SPACE 1                                                                
VALHED12 LA    R2,PROMEDH          OPTIONAL MEDIA FILTER                        
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED14                                                         
         MVI   ERROR,NOTMENMG                                                   
         CLI   PROMGRH+5,0                                                      
         BNE   ERREXIT                                                          
         MVI   ERROR,NOTJBNME                                                   
         CLI   PROJOBH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALMED                                                           
         MVC   QMED,MEDIA                                                       
         SPACE 1                                                                
VALHED14 LA    R2,PROWGRH          OPTIONAL WORK GROUP FILTER                   
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED16                                                         
         GOTO1 VALWG                                                            
         MVC   QWGR,WGROUP                                                      
         SPACE 1                                                                
VALHED16 LA    R2,PROWRKH          OPTIONAL WORK FILTER                         
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED18                                                         
         MVI   ERROR,NOTWKNWG                                                   
         CLI   PROWGRH+5,0         TEST FOR WORK GROUP INPUT                    
         BNE   ERREXIT                                                          
         GOTO1 VALWORK                                                          
         MVC   QWRK,WORKCODE                                                    
         SPACE 1                                                                
VALHED18 LA    R2,PROOPTH          OPTIONAL FIELD SELECTION                     
         BAS   RE,TSTKEY                                                        
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)                                                       
         BZ    VALHED20                                                         
         MVC   QOPT,SPACES                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         SPACE 1                                                                
         MVC   QOPT(0),8(R2)                                                    
         BAS   RE,FINDTIT          FIND TITLE AND VALIDATE OPTION #             
         SPACE 1                                                                
VALHED20 OI    PROOGRH+4,X'20'                                                  
         OI    PROOFFH+4,X'20'                                                  
         OI    PROCLIH+4,X'20'                                                  
         OI    PROPROH+4,X'20'                                                  
         OI    PROJOBH+4,X'20'                                                  
         OI    PROMGRH+4,X'20'                                                  
         OI    PROMEDH+4,X'20'                                                  
         OI    PROWGRH+4,X'20'                                                  
         OI    PROWRKH+4,X'20'                                                  
         OI    PROOPTH+4,X'20'                                                  
         SPACE 1                                                                
VALHEDX  B     XIT                                                              
         SPACE 3                                                                
TSTKEY   TM    4(R2),X'20'                                                      
         BOR   RE                                                               
         MVI   KEYCHG,C'Y'                                                      
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO BUILD THE SCREEN DYNAMICALLY                                   
*                                                                               
BLDTWA   NTR1                                                                   
         LA    R2,PROTAGH          R2=START OF SCREEN                           
         LA    R3,REGELS           R3=A(SCREEN DEFINITION ELEMENTS)             
         OC    QOPT,QOPT           IF OPTION FIELD USER, CHANGE                 
         BZ    *+8                  ELEMENTS                                    
         LA    R3,OPTELS                                                        
         LA    R1,WORK                                                          
         USING TWAPARMD,R1                                                      
         XC    TWAPARMD(TWAPARML),TWAPARMD CLEAR PARAMETER LIST                 
         MVC   TWAPATWA,ATWA               MOVE IN A(TWA)                       
         ST    R3,TWAPAFST                         A(ELEMENTS)                  
         MVC   TWAPAMAX,=AL4(LSAVES-T60BFFD) LENGTH OF TWA                      
         ST    R2,TWAPAOUT                         A(START OF SCREEN)           
         LA    R0,NLINES                                                        
         SPACE 1                                                                
BLDTWA2  GOTO1 VTWABLD,TWAPARMD                                                 
         CLI   TWAPERRS,0          BLOW UP IF ANY ERRORS                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,TWAPANXT         STORE A(NEXT FIELD) AS                       
         ST    R2,TWAPAOUT          A(OUTPUT)                                   
         BCT   R0,BLDTWA2          REPEAT FOR EACH LINE                         
         SPACE 1                                                                
BLDTWA4  LA    R3,REGPFS           BUILD THE PF LINE NOW                        
         OC    QOPT,QOPT                                                        
         BZ    *+8                                                              
         LA    R3,OPTPFS                                                        
         ST    R3,TWAPAFST                                                      
         GOTO1 VTWABLD,TWAPARMD                                                 
         CLI   TWAPERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
BLDTWAX  B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO SET FIRST FIELD, PF FIELD AND LAST FIELD                       
*                                                                               
SETSCR   ST    RE,SAVERE                                                        
         LA    R2,PROTAGH          SAVE ADDRESS OF FIRST SELECT FIELD           
         ST    R2,AFSTSEL                                                       
         LA    R0,NLINES           NUMBER OF LINES PER SCREEN                   
         LA    R1,REGFLDS          NUMBER OF FIELDS PER LINE                    
         OC    QOPT,QOPT                                                        
         BZ    *+8                                                              
         LA    R1,OPTFLDS                                                       
         MR    R0,R0               LINES X FIELDS = NUMBER TO BUMP              
         BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
         ST    R2,APFFLD           AFTER ALL DATA LINES IS PF LINE              
         BAS   RE,BUMP                                                          
         ST    R2,AENDSCR          AFTER PF LINE IS END-OF-SCREEN               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO TEST FOR ANYTHING TO EDIT ON SCREEN                            
*                                                                               
* ON EXIT: CC=EQ IF EDIT NEEDED; CC=NEQ IF EDIT NOT NEEDED                      
*                                                                               
TSTEDT   NTR1                                                                   
         L     R2,AFSTSEL          R2=A(FIRST SELECT FIELD)                     
         SR    R3,R3                                                            
         ICM   R3,1,LNLISTS        EXIT IF NOTHING ON SCREEN                    
         BZ    TSTEDTN                                                          
         SPACE 1                                                                
TSTEDT2  CLI   5(R2),0             SELECT FIELD ENTERED ?                       
         BE    TSTEDT4             NO, TEST IF OPTION FIELD USED                
         CLI   8(R2),C'*'          WAS SELECT ALREADY PROCESSED ?               
         BE    TSTEDT4             YES, TEST OPTION FIELD ALSO                  
         B     TSTEDTY             NO, INDICATE EDIT NEEDED                     
         SPACE 1                                                                
TSTEDT4  LA    R4,REGFLDS          GET NUMBER OF FIELDS                         
         OC    QOPT,QOPT                                                        
         BZ    *+8                                                              
         LA    R4,OPTFLDS                                                       
         BCTR  R4,0                                                             
         SPACE 1                                                                
TSTEDT6  BAS   RE,BUMP                                                          
         TM    1(R2),X'20'         IS THIS A PROTECTED FIELD ?                  
         BO    *+12                YES, SKIP IT                                 
         TM    4(R2),X'20'         NO, WAS IT CHANGED ?                         
         BZ    TSTEDTY             YES, INDICATE EDIT NEEDED                    
         BCT   R4,TSTEDT6          NO, TRY NEXT FIELD                           
         BAS   RE,BUMP             BUMP TO NEXT LINE                            
         BCT   R3,TSTEDT2          AND LOOK THERE                               
         SPACE 1                                                                
TSTEDTN  LTR   R8,R8               SET CC=NEQ                                   
         B     TSTEDTX                                                          
         SPACE 1                                                                
TSTEDTY  CR    R8,R8               SET CC=EQ                                    
         SPACE 1                                                                
TSTEDTX  B     XIT                                                              
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO LIST OPTION RECORDS                                            
*                                                                               
LIST     NTR1                                                                   
         L     R2,AFSTSEL                                                       
         ST    R2,ATHISLIN                                                      
         LA    R4,KEY                                                           
         USING ACOPKEY,R4                                                       
         OC    LLASTJOB,LLASTJOB   TEST IF CONTINUING LIST                      
         BZ    LIST4                                                            
         XC    KEY,KEY             STORE KEY TO LAST RECORD READ                
         MVC   ACOPKEY(L'LLASTJOB),LLASTJOB                                     
         GOTO1 READ                REREAD IT                                    
LIST2    MVI   ACOPWORK+2,X'FF'    NOW GET NEXT ONE                             
         B     LIST6                                                            
         SPACE 1                                                                
*                                                                               
* FIRST TIME LOGIC - BUILD KEY                                                  
*                                                                               
LIST4    XC    ACOPKEY,ACOPKEY                                                  
         MVI   ACOPRTYP,ACOPEQU                                                 
         MVI   ACOPSREC,ACOPSEQU                                                
         MVC   ACOPCUL,CUL                                                      
         MVC   ACOPOG,QOGR                                                      
         MVC   ACOPOFC,QOFF                                                     
         CLI   PROCLIH+5,0                                                      
         BE    *+10                                                             
         MVC   ACOPCLI,QCLI                                                     
         CLI   PROPROH+5,0                                                      
         BE    *+10                                                             
         MVC   ACOPPRO,QPROD                                                    
         CLI   PROJOBH+5,0                                                      
         BE    *+10                                                             
         MVC   ACOPJOB,QJOB                                                     
         MVC   ACOPMG,QMGR                                                      
         MVC   ACOPMED,QMED                                                     
         CLI   ACOPMED,0           TEST MEDIA INPUT                             
         BE    *+8                 NO                                           
         MVI   ACOPMG,X'FF'        YES-SET MEDIA GROUP                          
         MVC   ACOPWG,QWGR                                                      
         MVC   ACOPWORK,QWRK                                                    
         OC    ACOPWORK,ACOPWORK                                                
         BZ    *+8                                                              
         MVI   ACOPWG,X'FF'        FORCE WORKCODE GROUP                         
         SPACE 1                                                                
*                                                                               
* FILE READING LOGIC                                                            
*                                                                               
LIST6    GOTO1 HIGH                                                             
         GOTO1 CATCHIOS                                                         
         B     LIST10                                                           
         SPACE 1                                                                
LIST8    GOTO1 SEQ                                                              
         GOTO1 CATCHIOS                                                         
         SPACE 1                                                                
LIST10   CLC   ACOPKEY(ACOPOG-ACOPKEY),KEYSAVE   CHECK C/B                      
         BNE   XIT                                                              
         CLI   QOGR,0              CHECK OFFICE GROUP                           
         BE    LIST12                                                           
         CLC   ACOPKEY(ACOPOFC-ACOPKEY),KEYSAVE                                 
         BNE   XIT                                                              
         CLC   ACOPOG,QOGR                                                      
         BNE   LIST8                                                            
         SPACE 1                                                                
LIST12   OC    QOFF,QOFF           CHECK OFFICE                                 
         BZ    LIST14                                                           
         CLC   ACOPKEY(ACOPCLI-ACOPKEY),KEYSAVE                                 
         BNE   XIT                                                              
         CLC   ACOPOFC,QOFF                                                     
         BNE   LIST8                                                            
         SPACE 1                                                                
LIST14   CLI   QCLI,0              CHECK CLIENT                                 
         BE    LIST16                                                           
         CLC   ACOPKEY(ACOPPRO-ACOPKEY),KEYSAVE                                 
         BNE   XIT                                                              
         CLC   ACOPCLI,QCLI                                                     
         BNE   LIST8                                                            
         SPACE 1                                                                
LIST16   CLI   QPROD,0             CHECK PRODUCT                                
         BE    LIST18                                                           
         CLC   ACOPKEY(ACOPJOB-ACOPKEY),KEYSAVE                                 
         BNE   XIT                                                              
         CLC   ACOPPRO,QPROD                                                    
         BNE   LIST8                                                            
         SPACE 1                                                                
LIST18   CLI   QJOB,0              CHECK JOB                                    
         BE    LIST20                                                           
         CLC   ACOPKEY(ACOPMG-ACOPKEY),KEYSAVE                                  
         BNE   XIT                                                              
         CLC   ACOPJOB,QJOB                                                     
         BNE   LIST8                                                            
         SPACE 1                                                                
LIST20   CLI   QMGR,0              CHECK MEDIA GROUP                            
         BE    LIST22                                                           
         CLC   ACOPKEY(ACOPMED-ACOPKEY),KEYSAVE                                 
         BNE   XIT                                                              
         CLC   ACOPMG,QMGR                                                      
         BNE   LIST8                                                            
         SPACE 1                                                                
LIST22   CLI   QMED,0              CHECK MEDIA                                  
         BE    LIST24                                                           
         CLC   ACOPKEY(ACOPWG-ACOPKEY),KEYSAVE                                  
         BNE   XIT                                                              
         CLC   ACOPMED,QMED                                                     
         BNE   LIST8                                                            
         SPACE 1                                                                
LIST24   CLI   QWGR,0              CHECK WORK GROUP                             
         BE    LIST26                                                           
         CLC   ACOPKEY(ACOPWORK-ACOPKEY),KEYSAVE                                
         BNE   XIT                                                              
         CLC   ACOPWG,QWGR                                                      
         BNE   LIST8                                                            
         SPACE 1                                                                
LIST26   OC    QWRK,QWRK           CHECK WORK CODE                              
         BZ    LIST28                                                           
         CLC   ACOPKEY(ACOPWORK+L'ACOPWORK-ACOPKEY),KEYSAVE                     
         BNE   XIT                                                              
         CLC   ACOPWORK,QWRK                                                    
         BNE   LIST8                                                            
         SPACE 1                                                                
LIST28   L     R2,ATHISLIN                                                      
         BAS   RE,SETLIN                                                        
         OC    QOPT,QOPT                                                        
         BZ    *+12                                                             
         BAS   RE,DOPTION                                                       
         BNE   LIST8                                                            
         BAS   RE,DISREG                                                        
         SPACE 1                                                                
LIST30   MVC   LLASTJOB,ACOPKEY    SAVE ACCOUNT KEY                             
         MVC   ATHISLIN,ANEXTSEL   UPDATE LINE POINTER                          
         SR    RE,RE               INCREMENT LIST RECORD COUNT                  
         IC    RE,LNLISTS                                                       
         LR    R1,RE                                                            
         LA    RE,1(RE)                                                         
         STC   RE,LNLISTS                                                       
         MH    R1,=Y(SELTABL)                                                   
         LA    R1,LSELTAB(R1)      ADDRESS TABLE ENTRY                          
         USING SELTABD,R1                                                       
         MVC   SELKEY,ACOPKEY                                                   
         MVI   SELACT,C' '                                                      
         CLI   LNLISTS,NLINES      IS SCREEN FULL ?                             
         BNE   LIST8               NO, GET NEXT                                 
         SPACE 1                                                                
LISTX    B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO DISPLAY REGULAR FIELDS                                         
*                                                                               
DISREG   NTR1  ,                                                                
         L     R2,ASEL                                                          
         OI    4(R2),X'20'                                                      
         L     R4,AIO                                                           
         USING ACOPKEY,R4                                                       
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR                                                        
         USING REGPROTD,R3                                                      
         MVC   REGOGR,ACOPOG                                                    
         MVC   REGOFF,ACOPOFC                                                   
         MVC   REGCLI,ACOPCLI                                                   
         MVC   REGPRD,ACOPPRO                                                   
         MVC   REGJOB,ACOPJOB                                                   
         CLI   ACOPMG,X'FF'                                                     
         BE    *+10                                                             
         MVC   REGMGR,ACOPMG                                                    
         MVC   REGMED,ACOPMED                                                   
         CLI   ACOPWG,X'FF'                                                     
         BE    *+10                                                             
         MVC   REGWGR,ACOPWG                                                    
         MVC   REGWRK,ACOPWORK                                                  
         OC    ACOPOG(ACOPWORK+2-ACOPOG),ACOPOG                                 
         BNZ   *+10                                                             
         MVC   0(3,R3),=C'ALL'                                                  
         L     R2,APROT                                                         
         OI    6(R2),X'80'                                                      
         SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         SH    R1,=H'9'            SUBTRACT LENGTH OF HEADER +1                 
         EX    R1,*+8                                                           
         B     DISREGX                                                          
         MVC   8(0,R2),LISTAR                                                   
         SPACE 1                                                                
DISREGX  B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
DOPTION  NTR1                                                                   
         GOTO1 HELLO,DMCB,(C'G',=C'ACCOUNT '),                         X        
               (X'A4',AIO),(1,OPTNUMB)                                          
         CLI   DMCB+12,0                                                        
         BNE   DOPTIONN                                                         
         L     R6,DMCB+12                                                       
         L     R2,AOPT                                                          
         GOTO1 VDISOPT,DMCB,(R6),(R2)                                           
         OI    4(R2),X'20'                                                      
         TM    FACFLAG,XIROSYS+XIROMODE+XIWRONGF                                
         BZ    DOPTIONY                                                         
         OI    1(R2),X'20'         READ ONLY, PROTECT THIS FIELD                
         NI    1(R2),X'FF'-X'08'                                                
DOPTIONY CR    R8,R8                                                            
         B     DOPTIONX                                                         
         SPACE 1                                                                
DOPTIONN LTR   R8,R8                                                            
         SPACE 1                                                                
DOPTIONX B     XIT                                                              
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO EDIT THE LIST SCREEN                                           
*                                                                               
EDT      NTR1                                                                   
         L     R2,AFSTSEL          R2=A(FIRST SELECT FIELD)                     
         SR    R3,R3                                                            
         IC    R3,LNLISTS          NUMBER OF LINES ON SCREEN                    
         LA    R5,LSELTAB          ADDRESS OF SELECT TABLE                      
         USING SELTABD,R5                                                       
         SPACE 1                                                                
EDT2     BAS   RE,SETLIN           GET FIELD ADDRESSES FOR THIS LINE            
         L     R2,ASEL                                                          
         CLI   5(R2),0             IF NOTHING IN SELECT, CHANGE MUST BE         
         BE    EDT4                 TO OPTION                                   
         CLI   5(R2),1                                                          
         BNE   INVEND                                                           
         CLI   8(R2),C'*'          GET OPTION ALSO IF SELECT ALREADY            
         BE    EDT4                 EDITED                                      
         CLI   8(R2),C'S'          OR IF DISPLAY OR                             
         BE    EDT4                                                             
         CLI   8(R2),C'C'          CHANGE SELECTED                              
         BNE   INVEND              ANYTHING ELSE IS ERROR                       
         SPACE 1                                                                
EDT4     LA    R4,KEY              READ THE OPTION RECORD                       
         USING ACKEYD,R4                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(L'SELKEY),SELKEY                                             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMREAD'),SYSFIL,KEY,AIO,0                 
         L     RE,AIO                                                           
         MVC   KEY,0(RE)           EXTRACT RETURNED KEY                         
         CLC   ACKEYD(ACLENGTH-ACKEYD),KEYSAVE                                  
         BE    *+6                                                              
         DC    H'0'                BLOW UP IF NOT FOUND                         
         OC    QOPT,QOPT                                                        
         BZ    *+8                                                              
         BAS   RE,EDTOPT                                                        
         CLI   5(R2),0                                                          
         BE    EDT6                                                             
         CLI   8(R2),C'*'                                                       
         BE    EDT6                                                             
         MVC   SELACT,8(R2)                                                     
         MVI   8(R2),C'*'                                                       
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         MVC   PASSKEY,SELKEY      MOVE SELECT KEY TO WORK AREA                 
         LA    R4,PASSKEY                                                       
         USING PASSKEYD,R4                                                      
         CLI   PMG,X'FF'           CHANGE KEY BEFORE PASSING IT                 
         BNE   *+8                                                              
         MVI   PMG,0                                                            
         CLI   PWG,X'FF'                                                        
         BNE   *+8                                                              
         MVI   PWG,0                                                            
         GOTO1 VCALL,WORK,=C'OPTION',=C'MAINT',(L'POFG,POFG),(L'POFF,POX        
               FF),(L'PCLI,PCLI),(L'PPRO,PPRO),(L'PJOB,PJOB),(L'PMG,PMGX        
               ),(L'PMED,PMED),(L'PWG,PWG),(L'PWK,PWK),0                        
         SPACE 1                                                                
EDT6     L     R2,ANEXTSEL                                                      
         LA    R5,SELTABL(R5)                                                   
         BCT   R3,EDT2                                                          
         SPACE 1                                                                
EDTX     B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO EDIT THE OPTION FIELD                                          
*                                                                               
EDTOPT   NTR1  WORK=(R2,POINTLN)                                                
         MVI   UPDATE,C'N'                                                      
         ST    R2,APOINT                                                        
         L     R2,AOPT                                                          
         TM    4(R2),X'20'         WAS FIELD MODIFIED ?                         
         BO    EDTOPTX             NO, EXIT                                     
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VSAVPTRS,DMCB,(X'80',AIO),APOINT                                 
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING ACOPD,R6                                                         
         MVC   ACOPNUM,OPTNUMB                                                  
         GOTO1 VVALOPT,DMCB,(R2),(R6)                                           
         BNE   INVEND                                                           
         GOTO1 HELLO,DMCB,(C'D',=C'ACCOUNT'),(X'A4',AIO),              X        
               (1,ACOPNUM)                                                      
         GOTO1 ADDELEM                                                          
         GOTO1 PERSIN                                                           
         GOTO1 WRITE                                                            
         GOTO1 VCHGPTRS,DMCB,(X'80',AIO),APOINT                                 
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
EDTOPTX  B     XIT                                                              
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO SET FIELD ADDRESSES ANMD GET NEXT LINE                         
*                                                                               
SETLIN   ST    RE,SAVERE                                                        
         LA    R0,REGFLDS          NUMBER OF FIELDS PER LINE                    
         OC    QOPT,QOPT                                                        
         BZ    *+8                                                              
         LA    R0,OPTFLDS                                                       
         LA    R1,ASEL             SAVE ADDRESS OF SELECT                       
         ST    R2,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BAS   RE,BUMP             REPEAT FOR PROTECTED AND OPTION,             
         BCT   R0,*-12               IF ANY                                     
         ST    R2,ANEXTSEL         LINE DONE, SAVE NEXT SELECT ADDRESS          
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO VALIDATE OPTION NUMBER AND GET TITLE                           
*                                                                               
         SPACE 3                                                                
FINDTIT  NTR1                                                                   
         CLI   5(R2),L'OPTBKEY     CANNOT BE LONGER THAN KEY LENGTH             
         BH    INVEND                                                           
         L     R1,AOPTTAB                                                       
         USING OPTBD,R1                                                         
FINDTIT2 CLI   OPTBOPN,0           OPTION CODE INVALID IF END                   
         BE    INVEND               OF TABLE HIT                                
         CLI   5(R2),L'OPTBSHRT    MATCH ON LONG OR SHORT KEY                   
         BH    FINDTIT4                                                         
         CLC   OPTBSHRT,QOPT        SHORT KEY                                   
         BE    FINDTIT8                                                         
         B     FINDTIT6                                                         
         SPACE 1                                                                
FINDTIT4 CLC   5(1,R2),OPTBMINL    LONG KEY                                     
         BL    FINDTIT6                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   QOPT(0),OPTBKEY                                                  
         BE    FINDTIT8                                                         
         SPACE 1                                                                
FINDTIT6 LA    R1,OPTBL(R1)                                                     
         B     FINDTIT2                                                         
         SPACE 1                                                                
FINDTIT8 MVC   OPTHED,OPTBDESC                                                  
         MVC   OPTNUMB,OPTBOPN                                                  
         MVC   PROTIT(L'OPTTIT),OPTTIT                                          
         OI    PROTITH+6,X'80'                                                  
         SPACE 1                                                                
VALOPTNX XIT1                                                                   
         EJECT                                                                  
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
INVEND   MVI   ERROR,INVALID                                                    
ERREXIT  GOTO1 VERRCUR                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
LISTMSG  DC    C'LIST DISPLAYED - PRESS ENTER FOR NEXT PAGE'                    
LISTMSG2 DC    C'LIST DISPLAYED'                                                
LISTMSG3 DC    C'NO OPTION RECORD FOUND'                                        
EDTMSG   DC    C'CHANGES COMPLETED'                                             
         SPACE 3                                                                
REGTIT   DC    CL(L'PROTIT)' '                                                  
         ORG   REGTIT                                                           
         DC    CL5'SEL'                                                         
         DC    CL29'OGR OFF CLIENT PRODCT JOB'                                  
         DC    CL15'MGR MED WGR WC^'                                            
         ORG                                                                    
         SPACE 3                                                                
OPTTIT   DC    CL(L'PROTIT)' '                                                  
         ORG   OPTTIT                                                           
         DC    CL5'SEL'                                                         
         DC    CL29'OGR OFF CLIENT PRODCT JOB'                                  
         DC    CL15'MGR MED WGR WC^'                                            
OPTHED   DS    CL27                                                             
         DC    X'5F'                                                            
         ORG                                                                    
         SPACE 3                                                                
REGELS   DC    X'01',AL1(TWAELLNQ),AL1(1),AL1(02),AL1(03),X'08',AL1(32)         
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(07),AL1(74),X'20',AL1(0)          
         DC    X'00'                                                            
         SPACE 3                                                                
OPTELS   DC    X'01',AL1(TWAELLNQ),AL1(1),AL1(02),AL1(03),X'08',AL1(32)         
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(07),AL1(43),X'20',AL1(0)          
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(51),AL1(29),X'08',AL1(0)          
         DC    X'00'                                                            
         SPACE 3                                                                
REGPFS   DC    X'01',AL1(7+70),AL1(2),AL1(2),AL1(70),X'28',AL1(0)               
         DC    CL70'PF12 = RETURN'                                              
         DC    X'00'                                                            
         SPACE 3                                                                
OPTPFS   DC    X'01',AL1(7+78),AL1(2),AL1(2),AL1(78),X'28',AL1(0)               
         DC    CL78'PF12 = RETURN'                                              
         DC    X'00'                                                            
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
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
*FAXTRAINF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAXTRAINF                                                      
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROE4D                                                       
         ORG   T60BFFD+2408                                                     
LSAVES   DS    0D                                                               
LCONTROL DS    CL(QCONTRLN)        LAST TIME CONTROLS                           
LNLISTS  DS    X                   M'LISTS ON SCREEN                            
LLASTJOB DS    CL(L'SELKEY)        LAST JOB ON SCREEN                           
LSELTAB  DS    CL(NLINES*SELTABL)                                               
OPTNAME  DS    CL3                                                              
OPTNUMB  DS    CL1                                                              
*                                                                               
* EQUATES                                                                       
*                                                                               
NLINES   EQU   13                  N'LINES PER SCREEN                           
REGFLDS  EQU   2                   N'FIELDS ON REGULAR LIST SCREEN              
OPTFLDS  EQU   3                   N'FIELDS ON OPTION LIST SCREEN               
FSTLIST  EQU   1                   FIRST TIME SWITCH                            
DISLIST  EQU   2                   DISPLAY SWITCH                               
EDTLIST  EQU   3                   EDIT SWITCH                                  
POINTLN  EQU   ((8*54)+1)                                                       
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
QCONTROL DS    0C                                                               
QOGR     DS    CL(L'EFFOFG)                                                     
QOFF     DS    CL(L'EFFOFFC)                                                    
QCLI     DS    CL(L'CLICODE)                                                    
QPROD    DS    CL(L'PRODCODE)                                                   
QJOB     DS    CL(L'JOBNUM)                                                     
QMGR     DS    CL(L'MGROUP)                                                     
QMED     DS    CL(L'MEDIA)                                                      
QWGR     DS    CL(L'WGROUP)                                                     
QWRK     DS    CL(L'WORKCODE)                                                   
QOPT     DS    CL8                                                              
QCONTRLN EQU   *-QCONTROL                                                       
*                                                                               
SAVERE   DS    A                   SAVE RE FOR SUB-ROUTINES                     
*                                                                               
AFSTSEL  DS    A                   A(SELECT FIELD)                              
APFFLD   DS    A                   A(PF FIELD)                                  
AENDSCR  DS    A                   A(END-OF-SCREEN)                             
*                                                                               
ATHISLIN DS    A                                                                
*                                                                               
ASEL     DS    A                   A(SELECT FIELD)                              
APROT    DS    A                   A(PROTECTED FIELD)                           
AOPT     DS    A                   A(OPTION FIELD)                              
*                                                                               
ANEXTSEL DS    A                                                                
*                                                                               
APOINT   DS    A                   A(PASSIVE POINTER BUFFER)                    
*                                                                               
EXLEN    DS    X                                                                
UPDATE   DS    C                                                                
PASSKEY  DS    CL(L'SELKEY)                                                     
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
*                                                                               
* DSECT TO COVER REGULAR LIST PROTECTED FIELDS                                  
*                                                                               
REGPROTD DSECT                                                                  
         DS    C                                                                
REGOGR   DS    C                                                                
         DS    CL3                                                              
REGOFF   DS    CL2                                                              
         DS    C                                                                
REGCLI   DS    CL6                                                              
         DS    C                                                                
REGPRD   DS    CL6                                                              
         DS    C                                                                
REGJOB   DS    CL6                                                              
         DS    CL2                                                              
REGMGR   DS    C                                                                
         DS    CL3                                                              
REGMED   DS    C                                                                
         DS    CL3                                                              
REGWGR   DS    C                                                                
         DS    CL2                                                              
REGWRK   DS    CL2                                                              
         DS    C                                                                
REGPROTL EQU   *-REGPROTD                                                       
         SPACE 3                                                                
*                                                                               
* DSECT TO COVER SELECT TABLE                                                   
*                                                                               
SELTABD  DSECT                                                                  
SELACT   DS    C                                                                
SELKEY   DS    CL(ACOPWORK-ACOPKEY+2)                                           
SELTABL  EQU   *-SELTABD                                                        
         EJECT                                                                  
*                                                                               
* DSECT  TO COVER PASSING OF KEY                                                
*                                                                               
PASSKEYD DSECT                                                                  
         DS    CL5                                                              
POFG     DS    CL1                                                              
POFF     DS    CL2                                                              
PCLI     DS    CL6                                                              
PPRO     DS    CL6                                                              
PJOB     DS    CL6                                                              
PMG      DS    CL1                                                              
PMED     DS    CL1                                                              
PWG      DS    CL1                                                              
PWK      DS    CL2                                                              
         EJECT                                                                  
       ++INCLUDE DDTWABLDD                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039ACPRO14   07/10/20'                                      
         END                                                                    
