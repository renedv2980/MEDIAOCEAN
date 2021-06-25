*          DATA SET ACPRO1A    AT LEVEL 005 AS OF 04/20/10                      
*PHASE T60B1AA                                                                  
         TITLE 'T60B1A - JOB NUMBER LIST (JNUM)'                                
T60B1A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B1A**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK AREA                    
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    KEYLOGIC                                                         
         CLI   MODE,VALREC                                                      
         BE    RECLOGIC                                                         
         B     XIT                                                              
         EJECT                                                                  
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
*                                                                               
*                                                                               
* VALREC  LOGIC - DISPLAY OR CHANGE                                             
*                                                                               
RECLOGIC BAS   RE,SETSCR                                                        
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME LIST                     
         BE    DISLOGIC                                                         
         BAS   RE,TSTEDT           TEST FOR ANYTHING TO EDIT                    
         BE    EDTLOGIC            YES                                          
         MVI   INTMODE,DISLIST     NO, CONTINUE LIST                            
*                                                                               
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
*                                                                               
         CLI   INTMODE,FSTLIST                                                  
         BNE   *+10                                                             
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
*                                                                               
DISLOGX  ST    R2,ACURFORC                                                      
         B     XIT                                                              
*                                                                               
FULLMESS MVC   CONHEAD(L'LISTMSG),LISTMSG                                       
         B     DISLOGX                                                          
*                                                                               
NONEMESS MVC   CONHEAD(L'LISTMSG3),LISTMSG3                                     
         B     DISLOGX                                                          
*                                                                               
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
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR                                                          
         MVI   KEYCHG,C'N'                                                      
         MVI   OPTION,0                                                         
         LA    R2,PROOGRH          OPTIONAL OFFICE GROUP FILTER                 
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED2                                                          
         CLC   =C'ALL',8(R2)       DID THEY ENTER ALL ?                         
         BE    VALHED2                                                          
         GOTO1 VALOG                                                            
         MVC   QOGR,EFFOFG                                                      
*                                                                               
VALHED2  LA    R2,PROOFFH          OPTIONAL OFFICE FILTER                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED4                                                          
         MVI   ERROR,NOTOFNOG                                                   
         CLI   PROOGRH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALOFF                                                           
         MVC   QOFF,EFFOFFC                                                     
*                                                                               
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
*                                                                               
VALHED6  LA    R2,PROPROH          OPTIONAL PRODUCT FILTER                      
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED8                                                          
         MVI   ERROR,NEEDCLI                                                    
         CLI   PROCLIH+5,0                                                      
         BE    ERREXIT                                                          
         GOTO1 VALPROD                                                          
         MVC   QPROD,PRODCODE                                                   
*                                                                               
VALHED8  LA    R2,PROMGRH          OPTIONAL MEDIA GROUP FILTER                  
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED10                                                         
         GOTO1 VALMG                                                            
         MVC   QMGR,MGROUP                                                      
*                                                                               
VALHED10 LA    R2,PROMEDH          OPTIONAL MEDIA FILTER                        
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED12                                                         
         MVI   ERROR,NOTMENMG                                                   
         CLI   PROMGRH+5,0                                                      
         BNE   ERREXIT                                                          
         GOTO1 VALMED                                                           
         MVC   QMED,MEDIA                                                       
*                                                                               
VALHED12 LA    R2,PROEFFH          OPTIONAL EFFECTIVE DATE                      
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED13                                                         
         GOTO1 ANY                                                              
         MVI   ERROR,INVDATE                                                    
         GOTO1 DATVAL,DMCB,WORK,DUB                                             
         OC    DMCB,DMCB                                                        
         BZ    ERREXIT                                                          
         GOTO1 DATCON,DMCB,DUB,(1,QDATE)                                        
         XC    QDATE,EFFS                                                       
*                                                                               
VALHED13 LA    R2,PROTYPH          OPTIONAL TYPE                                
         MVI   QTYP,X'FF'                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED14                                                         
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         MVI   QTYP,AJNKDRFQ                                                    
         CLI   PROTYP,C'D'                                                      
         BE    VALHED14                                                         
         MVI   QTYP,AJNKLIVQ                                                    
         CLI   PROTYP,C'L'                                                      
         BNE   ERREXIT                                                          
*                                                                               
VALHED14 LA    R2,PROSTAH          OPTIONAL STATUS                              
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED16                                                         
         CLI   8(R2),C'I'                                                       
         BE    *+12                                                             
         CLI   8(R2),C'A'                                                       
         BNE   INVEND                                                           
         MVC   QSTAT,8(R2)                                                      
*                                                                               
VALHED16 OI    PROOGRH+4,X'20'                                                  
         OI    PROOFFH+4,X'20'                                                  
         OI    PROCLIH+4,X'20'                                                  
         OI    PROPROH+4,X'20'                                                  
         OI    PROMGRH+4,X'20'                                                  
         OI    PROMEDH+4,X'20'                                                  
         OI    PROEFFH+4,X'20'                                                  
         OI    PROTYPH+4,X'20'                                                  
         OI    PROSTAH+4,X'20'                                                  
*                                                                               
VALHEDX  B     XIT                                                              
*                                                                               
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
         LA    R3,DELINE           R3=A(SCREEN DEFINITION ELEMENTS)             
         LA    R1,WORK                                                          
         USING TWAPARMD,R1                                                      
         XC    TWAPARMD(TWAPARML),TWAPARMD CLEAR PARAMETER LIST                 
         MVC   TWAPATWA,ATWA               MOVE IN A(TWA)                       
         ST    R3,TWAPAFST                         A(ELEMENTS)                  
         MVC   TWAPAMAX,=AL4(LSAVES-T60BFFD) LENGTH OF TWA                      
         ST    R2,TWAPAOUT                         A(START OF SCREEN)           
         LA    R0,NLINES                                                        
*                                                                               
BLDTWA2  GOTO1 VTWABLD,TWAPARMD                                                 
         CLI   TWAPERRS,0          BLOW UP IF ANY ERRORS                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,TWAPANXT         STORE A(NEXT FIELD) AS                       
         ST    R2,TWAPAOUT          A(OUTPUT)                                   
         BCT   R0,BLDTWA2          REPEAT FOR EACH LINE                         
*                                                                               
BLDTWA4  LA    R3,PFLINE           BUILD THE PF LINE NOW                        
         ST    R3,TWAPAFST                                                      
         GOTO1 VTWABLD,TWAPARMD                                                 
         CLI   TWAPERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
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
         LA    R1,NUMFLDS          NUMBER OF FIELDS PER LINE                    
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
*                                                                               
TSTEDT2  CLI   5(R2),0             SELECT FIELD ENTERED ?                       
         BE    TSTEDT4             NO, TEST IF OPTION FIELD USED                
         CLI   8(R2),C'*'          WAS SELECT ALREADY PROCESSED ?               
         BE    TSTEDT4             YES, TEST OPTION FIELD ALSO                  
         B     TSTEDTY             NO, INDICATE EDIT NEEDED                     
*                                                                               
TSTEDT4  LA    R4,NUMFLDS          GET NUMBER OF FIELDS                         
         BCTR  R4,0                                                             
*                                                                               
TSTEDT6  BAS   RE,BUMP                                                          
         TM    1(R2),X'20'         IS THIS A PROTECTED FIELD ?                  
         BO    *+12                YES, SKIP IT                                 
         TM    4(R2),X'20'         NO, WAS IT CHANGED ?                         
         BZ    TSTEDTY             YES, INDICATE EDIT NEEDED                    
         BCT   R4,TSTEDT6          NO, TRY NEXT FIELD                           
         BAS   RE,BUMP             BUMP TO NEXT LINE                            
         BCT   R3,TSTEDT2          AND LOOK THERE                               
*                                                                               
TSTEDTN  LTR   R8,R8               SET CC=NEQ                                   
         B     TSTEDTX                                                          
*                                                                               
TSTEDTY  CR    R8,R8               SET CC=EQ                                    
*                                                                               
TSTEDTX  B     XIT                                                              
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO LIST OPTION RECORDS                                            
*                                                                               
LIST     NTR1                                                                   
         L     R2,AFSTSEL                                                       
         ST    R2,ATHISLIN                                                      
         LA    R4,KEY                                                           
         USING AJNRECD,R4                                                       
         OC    LLASTJOB,LLASTJOB   TEST IF CONTINUING LIST                      
         BZ    LIST4                                                            
         XC    KEY,KEY             STORE KEY TO LAST RECORD READ                
         MVC   AJNKEY(L'LLASTJOB),LLASTJOB                                      
         GOTO1 READ                REREAD IT                                    
         B     LIST6                                                            
*                                                                               
* FIRST TIME LOGIC - BUILD KEY                                                  
*                                                                               
LIST4    XC    AJNKEY,AJNKEY                                                    
         MVI   AJNKTYP,AJNKTYPQ                                                 
         MVI   AJNKSUB,AJNKSUBQ                                                 
         MVC   AJNKCUL,CUL                                                      
         MVC   AJNKOG,QOGR                                                      
         MVC   AJNKOFC,QOFF                                                     
         MVC   AJNKCLI,QCLI                                                     
         MVC   AJNKPRO,QPROD                                                    
         MVC   AJNKMG,QMGR                                                      
         MVC   AJNKMED,QMED                                                     
*                                                                               
LIST6    GOTO1 HIGH                                                             
         B     LIST10                                                           
*                                                                               
LIST8    GOTO1 SEQ                                                              
*                                                                               
LIST10   CLC   AJNKEY(AJNKOG-AJNKEY),KEYSAVE   CHECK C/B                        
         BNE   LISTX                                                            
         CLI   QOGR,0              CHECK OFFICE GROUP                           
         BE    LIST12                                                           
         CLC   AJNKEY(AJNKOFC-AJNKEY),KEYSAVE                                   
         BNE   LISTX                                                            
         CLC   AJNKOG,QOGR                                                      
         BNE   LIST8                                                            
*                                                                               
LIST12   OC    QOFF,QOFF           CHECK OFFICE                                 
         BZ    LIST14                                                           
         CLC   AJNKEY(AJNKCLI-AJNKEY),KEYSAVE                                   
         BNE   LISTX                                                            
         CLC   AJNKOFC,QOFF                                                     
         BNE   LIST8                                                            
*                                                                               
LIST14   CLI   QCLI,0              CHECK CLIENT                                 
         BE    LIST16                                                           
         CLC   AJNKEY(AJNKPRO-AJNKEY),KEYSAVE                                   
         BNE   LISTX                                                            
         CLC   AJNKCLI,QCLI                                                     
         BNE   LIST8                                                            
*                                                                               
LIST16   CLI   QPROD,0             CHECK PRODUCT                                
         BE    LIST18                                                           
         CLC   AJNKEY(AJNKMG-AJNKEY),KEYSAVE                                    
         BNE   LISTX                                                            
         CLC   AJNKPRO,QPROD                                                    
         BNE   LIST8                                                            
*                                                                               
LIST18   CLI   QMGR,0              CHECK MEDIA GROUP                            
         BE    LIST20                                                           
         CLC   AJNKEY(AJNKMED-AJNKEY),KEYSAVE                                   
         BNE   LISTX                                                            
         CLC   AJNKMG,QMGR                                                      
         BNE   LIST8                                                            
*                                                                               
LIST20   CLI   QMED,0              CHECK MEDIA                                  
         BE    LIST22                                                           
         CLC   AJNKEY(AJNKEFF-AJNKEY),KEYSAVE                                   
         BNE   LISTX                                                            
         CLC   AJNKMED,QMED                                                     
         BNE   LIST8                                                            
*                                                                               
LIST22   CLI   QDATE,0             CHECK DATE                                   
         BE    LIST23                                                           
         CLC   QDATE,AJNKEFF                                                    
         BL    LIST8                                                            
*                                                                               
LIST23   CLI   QTYP,X'FF'          CHECK TYPE                                   
         BE    LIST24                                                           
         CLC   QTYP,AJNKTYPE                                                    
         BNE   LIST8                                                            
*                                                                               
LIST24   CLI   QSTAT,0             CHECK STATUS                                 
         BE    LIST28                                                           
         TM    AJNRSTAT,AJNNDACT                                                
         BO    LIST26                                                           
         CLI   QSTAT,C'A'                                                       
         BE    LIST28                                                           
         B     LIST8                                                            
*                                                                               
LIST26   CLI   QSTAT,C'I'                                                       
         BNE   LIST8                                                            
*                                                                               
LIST28   L     R2,ATHISLIN                                                      
         BAS   RE,SETLIN                                                        
         BAS   RE,DISPLAY                                                       
*                                                                               
         MVC   LLASTJOB,AJNKEY     SAVE ACCOUNT KEY                             
         MVC   ATHISLIN,ANEXTSEL   UPDATE LINE POINTER                          
         SR    RE,RE               INCREMENT LIST RECORD COUNT                  
         IC    RE,LNLISTS                                                       
         LR    R1,RE                                                            
         LA    RE,1(RE)                                                         
         STC   RE,LNLISTS                                                       
         MH    R1,=Y(SELTABL)                                                   
         LA    R1,LSELTAB(R1)      ADDRESS TABLE ENTRY                          
         USING SELTABD,R1                                                       
         MVC   SELKEY,AJNKEY                                                    
         MVI   SELACT,C' '                                                      
         CLI   LNLISTS,NLINES      IS SCREEN FULL ?                             
         BNE   LIST8               NO, GET NEXT                                 
         DROP  R1                                                               
*                                                                               
LISTX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO DISPLAY DATA FIELDS                                            
*                                                                               
DISPLAY  NTR1  ,                                                                
         L     R2,ASEL                                                          
         OI    4(R2),X'20'                                                      
         L     R4,AIO                                                           
         USING AJNRECD,R4                                                       
         USING JNAELD,R6                                                        
         MVI   ELCODE,JNAELQ                                                    
         BAS   RE,GETELIO                                                       
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR                                                        
         USING LSTDSECT,R3                                                      
         MVC   LSTOGR,AJNKOG                                                    
         MVC   LSTOFF,AJNKOFC                                                   
         MVC   LSTCLI,AJNKCLI                                                   
         MVC   LSTPRD,AJNKPRO                                                   
         MVC   LSTMGR,AJNKMG                                                    
         MVC   LSTMED,AJNKMED                                                   
         MVI   LSTTYP,C'L'                                                      
         CLI   AJNKTYPE,AJNKLIVQ                                                
         BE    *+8                                                              
         MVI   LSTTYP,C'D'                                                      
         MVI   LSTSTAT,C'A'                                                     
         TM    AJNRSTAT,AJNNDACT                                                
         BZ    *+8                                                              
         MVI   LSTSTAT,C'I'                                                     
         MVC   WORK,AJNKEFF                                                     
         XC    WORK(3),EFFS                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(8,LSTDATE)                                 
         MVC   LSTSTRT,JNASTRT                                                  
         MVC   LSTP2,JNAP2                                                      
         MVC   LSTP3,JNAP3                                                      
         MVC   LSTP4,JNAP4                                                      
         MVC   LSTP5,JNAP5                                                      
         MVC   LSTP6,JNAP6                                                      
         MVC   LSTLAST,JNALNUM                                                  
*                                                                               
         OC    AJNKOG(AJNKEFF-AJNKOG),AJNKOG                                    
         BNZ   *+10                                                             
         MVC   0(3,R3),=C'ALL'                                                  
         L     R2,APROT                                                         
         OI    6(R2),X'80'                                                      
         SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         SH    R1,=H'9'            SUBTRACT LENGTH OF HEADER +1                 
         EX    R1,*+8                                                           
         B     DISPLAYX                                                         
         MVC   8(0,R2),LISTAR                                                   
*                                                                               
DISPLAYX B     XIT                                                              
         DROP  R4,R6                                                            
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
*                                                                               
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
*                                                                               
EDT4     LA    R4,KEY              READ THE JOB NUMBER RECORD                   
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
*                                                                               
         CLI   5(R2),0                                                          
         BE    EDT8                                                             
         CLI   8(R2),C'*'                                                       
         BE    EDT8                                                             
         MVC   SELACT,8(R2)                                                     
         MVI   8(R2),C'*'                                                       
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         USING SELKEYD,R5                                                       
         MVC   WORK,SDATE                                                       
         XC    WORK(3),EFFS                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(8,OUTDATE)                                 
*                                                                               
         MVI   OUTTYPE,C'L'                                                     
         CLI   STYPE,X'00'         IS IT LIVE?                                  
         BE    *+8                 YES                                          
         MVI   OUTTYPE,C'D'        NO, MUST BE DRAFT                            
*                                                                               
         CLI   SACT,C'C'                                                        
         BE    EDT6                                                             
         GOTO1 VCALL,WORK,=C'JNUM',=C'DIS',(L'SOFG,SOFG),(L'SOFF,SOFF),X        
               (L'SCLI,SCLI),(L'SPRO,SPRO),(L'SMG,SMG),(L'SMED,SMED),(LX        
               'OUTDATE,OUTDATE),(L'OUTTYPE,OUTTYPE),0                          
*                                                                               
EDT6     GOTO1 VCALL,WORK,=C'JNUM',=C'CHA',(L'SOFG,SOFG),(L'SOFF,SOFF),X        
               (L'SCLI,SCLI),(L'SPRO,SPRO),(L'SMG,SMG),(L'SMED,SMED),(LX        
               'OUTDATE,OUTDATE),(L'OUTTYPE,OUTTYPE),0                          
*                                                                               
EDT8     L     R2,ANEXTSEL                                                      
         LA    R5,SELTABL(R5)                                                   
         BCT   R3,EDT2                                                          
*                                                                               
EDTX     B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO SET FIELD ADDRESSES AND GET NEXT LINE                          
*                                                                               
SETLIN   ST    RE,SAVERE                                                        
         LA    R0,NUMFLDS          NUMBER OF FIELDS PER LINE                    
         LA    R1,ASEL             SAVE ADDRESS OF SELECT                       
         ST    R2,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BAS   RE,BUMP             REPEAT FOR PROTECTED AND OPTION,             
         BCT   R0,*-12               IF ANY                                     
         ST    R2,ANEXTSEL         LINE DONE, SAVE NEXT SELECT ADDRESS          
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
*                                                                               
INVEND   MVI   ERROR,INVALID                                                    
ERREXIT  GOTO1 VERRCUR                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
EFFS     DC    3X'FF'                                                           
*                                                                               
LISTMSG  DC    C'LIST DISPLAYED - PRESS ENTER FOR NEXT PAGE'                    
LISTMSG2 DC    C'LIST DISPLAYED'                                                
LISTMSG3 DC    C'NO LIST RECORDS FOUND'                                         
EDTMSG   DC    C'CHANGES COMPLETED'                                             
*                                                                               
DELINE   DC    X'01',AL1(TWAELLNQ),AL1(1),AL1(02),AL1(03),X'08',AL1(32)         
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(07),AL1(74),X'20',AL1(0)          
         DC    X'00'                                                            
*                                                                               
PFLINE   DC    X'01',AL1(7+70),AL1(2),AL1(2),AL1(70),X'28',AL1(0)               
         DC    CL70'S=DISPLAY, C=CHANGE'                                        
         DC    X'00'                                                            
         EJECT                                                                  
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
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROEAD                                                       
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
NUMFLDS  EQU   2                   N'FIELDS ON SCREEN                           
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
QCONTROL DS    0C                                                               
QOGR     DS    CL(L'EFFOFG)                                                     
QOFF     DS    CL(L'EFFOFFC)                                                    
QCLI     DS    CL(L'CLICODE)                                                    
QPROD    DS    CL(L'PRODCODE)                                                   
QJOB     DS    CL(L'JOBNUM)                                                     
QMGR     DS    CL(L'MGROUP)                                                     
QMED     DS    CL(L'MEDIA)                                                      
QDATE    DS    XL(L'AJNKEFF)                                                    
QSTAT    DS    CL(L'AJNRSTAT)                                                   
QTYP     DS    XL1                                                              
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
OUTDATE  DS    CL8                                                              
OUTTYPE  DS    CL1                                                              
*                                                                               
EXLEN    DS    X                                                                
UPDATE   DS    C                                                                
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
*                                                                               
* DSECT TO COVER LIST PROTECTED FIELDS                                          
*                                                                               
LSTDSECT DSECT                                                                  
         DS    C                                                                
LSTOGR   DS    C                                                                
         DS    CL2                                                              
LSTOFF   DS    CL2                                                              
         DS    CL2                                                              
LSTCLI   DS    CL3                                                              
         DS    C                                                                
LSTPRD   DS    CL3                                                              
         DS    CL2                                                              
LSTMGR   DS    C                                                                
         DS    CL3                                                              
LSTMED   DS    C                                                                
         DS    CL2                                                              
LSTTYP   DS    CL1                                                              
         DS    CL2                                                              
LSTSTAT  DS    C                                                                
         DS    CL2                                                              
LSTDATE  DS    CL8                                                              
         DS    C                                                                
LSTSTRT  DS    CL5                                                              
         DS    C                                                                
LSTP2    DS    CL2                                                              
         DS    CL1                                                              
LSTP3    DS    CL2                                                              
         DS    CL1                                                              
LSTP4    DS    CL2                                                              
         DS    CL1                                                              
LSTP5    DS    CL2                                                              
         DS    CL1                                                              
LSTP6    DS    CL2                                                              
         DS    CL1                                                              
LSTLAST  DS    CL12                                                             
LSTPROTL EQU   *-LSTDSECT                                                       
*                                                                               
*                                                                               
* DSECT TO COVER OPTION LIST PROTECTED FIELDS                                   
*                                                                               
OPTPROTD DSECT                                                                  
         SPACE 3                                                                
*                                                                               
* DSECT TO COVER SELECT TABLE                                                   
*                                                                               
SELTABD  DSECT                                                                  
SELACT   DS    C                                                                
SELKEY   DS    CL(AJNKTYPE+L'AJNKTYPE-AJNKEY)                                   
SELTABL  EQU   *-SELTABD                                                        
         EJECT                                                                  
*                                                                               
* DSECT  TO COVER PASSING OF KEY                                                
*                                                                               
SELKEYD  DSECT                                                                  
SACT     DS    C                                                                
         DS    CL5                                                              
SOFG     DS    CL1                                                              
SOFF     DS    CL2                                                              
SCLI     DS    CL6                                                              
SPRO     DS    CL6                                                              
SMG      DS    CL1                                                              
SMED     DS    CL1                                                              
SDATE    DS    CL3                                                              
STYPE    DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE DDTWABLDD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACPRO1A   04/20/10'                                      
         END                                                                    
