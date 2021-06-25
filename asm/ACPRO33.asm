*          DATA SET ACPRO33    AT LEVEL 054 AS OF 04/11/07                      
*PHASE T60B33A                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B33 - JOB ESTIMATE LIST'                                     
T60B33   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B33**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BE    EST2                                                             
         CLI   MODE,VALREC                                                      
         BE    EST4                                                             
         B     XIT                                                              
*                                                                               
* VALKEY LOGIC                                                                  
*                                                                               
EST2     LA    RE,LOCAL                                                         
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         ST    R2,RELO                                                          
         BAS   RE,VALHED                                                        
*                                                                               
         MVI   CHASWA,C'Y'                                                      
         MVI   CHASWP,C'Y'                                                      
         CLI   AUTHOR,0                                                         
         BE    EST3                                                             
         TM    AUTHOR,CAT3Q                                                     
         BNZ   *+8                                                              
         MVI   CHASWA,C'N'                                                      
         TM    AUTHOR,CAT4Q                                                     
         BNZ   *+8                                                              
         MVI   CHASWP,C'N'                                                      
*                                                                               
EST3     MVI   INTMODE,EDTLIST     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES-SET FIRST TIME LIST                      
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES                                          
         BAS   RE,RDOPT            READ OPTIONS FOR JOB                         
         L     RE,ATIA             USE TIA FOR JOBBER BUFFER                    
         ST    RE,ACOLTAB                                                       
         L     RF,=A(LENTIA)                                                    
         SRL   RF,1                SPLIT TIA IN HALF                            
         ST    RF,LCOLTAB                                                       
         ST    RF,LOPVTAB                                                       
         LA    RE,0(RF,RE)                                                      
         ST    RE,AOPVTAB                                                       
         B     XIT                                                              
*                                                                               
* VALREC LOGIC-DISPLAY OR EDIT                                                  
*                                                                               
EST4     CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME LIST                     
         BE    EST6                                                             
         BAS   RE,PROCPF           PROCESS PF KEYS                              
         BAS   RE,TSTEDT           TEST ANYTHING TO EDIT                        
         BE    EST10               YES                                          
*                                                                               
         MVI   INTMODE,DISLIST     CONTINUE LIST                                
*                                                                               
         CLI   PFKEY,PF11          TEST PF11=ELIST REPORT                       
         BNE   EST6                                                             
*                                                                               
         BAS   RE,EREP             GENERATE REPORT                              
         LA    R2,PROCLIH                                                       
         CLI   LNLISTS,0           TEST ANYTHING ON SCREEN                      
         BE    *+8                                                              
         LA    R2,PROSEL1H                                                      
         B     EST9                                                             
*                                                                               
* DISPLAY LOGIC                                                                 
*                                                                               
EST6     GOTO1 VCLEARF,DMCB,PROSEL1H,PROLAST                                    
         GOTO1 (RF),(R1),(1,PROSEL1H),PROPFH                                    
         MVI   LNLISTS,0                                                        
         LA    RE,LSELTAB                                                       
         LA    RF,L'LSELTAB                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         CLI   INTMODE,FSTLIST                                                  
         BNE   *+10                                                             
         XC    LLASTEST,LLASTEST   CLEAR OUT LAST ESTIMATE LISTED               
         BAS   RE,DISJOB           DISPLAY JOB DATA                             
*                                                                               
EST7     BAS   RE,LIST                                                          
         LA    R2,PROSEL1H                                                      
         CLI   LNLISTS,NLINES      TEST IF SCREEN FILLED                        
         BE    EST8                YES                                          
         LA    R2,PROCLIH          PUT CURSOR AT FIRST KEY FIELD                
         XC    LLASTEST,LLASTEST   NO-MUST BE AT END-OF-LIST                    
         MVC   CONHEAD(L'LIST2MSG),LIST2MSG                                     
         B     EST9                                                             
*                                                                               
EST8     MVC   CONHEAD(L'LISTMSG),LISTMSG                                       
*                                                                               
EST9     ST    R2,ACURFORC                                                      
         B     XIT                                                              
*                                                                               
* EDIT LOGIC                                                                    
*                                                                               
EST10    BAS   RE,EDT                                                           
         LA    R2,PROSEL1H                                                      
         ST    R2,ACURFORC                                                      
         MVC   CONHEAD(L'EDTMSG),EDTMSG                                         
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE THE HEADLINE FIELDS                                   
*                                                                               
VALHED   NTR1                                                                   
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR                                                          
         MVI   KEYCHG,C'N'         INITIALIZE KEY FIELD CHANGE SWITCH           
         MVI   OPTION,0            NO NAME FIELDS TO BE SHOWN                   
*                                                                               
         LA    R2,PROCLIH                                                       
         BAS   RE,TSTKEY                                                        
         GOTO1 VALCLI                                                           
*                                                                               
VALHED2  LA    R2,PROPROH                                                       
         BAS   RE,TSTKEY                                                        
         GOTO1 VALPROD                                                          
*                                                                               
VALHED4  LA    R2,PROJOBH                                                       
         BAS   RE,TSTKEY                                                        
         GOTO1 VALJOB                                                           
*                                                                               
         L     R0,AIO3             SAVE THE JOB RECORD IN IO3                   
         ST    R0,AJOB                                                          
         L     RE,AIO              RE=A(JOB RECORD)                             
         LH    R1,ACLENGTH-ACKEYD(RE)                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   PROEXP,SPACES                                                    
         TM    JOBJSTAT,ACJBXJOB   IS THIS AN XJOB                              
         BZ    *+10                NO                                           
         MVC   PROEXP(L'JEXJOB),JEXJOB                                          
         OI    PROEXPH+6,X'80'     TRANSMIT FIELD BACK                          
*                                                                               
         MVI   ERROR,BOESTERR                                                   
         TM    JOBJSTAT,JOBSMCSE   TEST JOB USES MCS ESTIMATES                  
         BO    ERREND              YES, ERROR                                   
*                                                                               
         MVI   ERROR,OLDESERR                                                   
         TM    JOBJSTAT,ACJBNEWQ   TEST JOB USES NEW ESTIMATES                  
         BZ    ERREND              NO-STOP IT                                   
*                                                                               
VALHED6  OI    PROCLIH+4,X'20'     SET ON PREV VALID BITS                       
         OI    PROPROH+4,X'20'                                                  
         OI    PROJOBH+4,X'20'                                                  
*                                                                               
VALHEDX  B     XIT                                                              
         SPACE 2                                                                
TSTKEY   TM    4(R2),X'20'                                                      
         BOR   RE                                                               
         MVI   KEYCHG,C'Y'                                                      
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO PROCESS PF KEYS                                                
*                                                                               
PROCPF   NTR1                                                                   
         LA    R2,PROSEL1H                                                      
         SR    R3,R3                                                            
         ICM   R3,1,LNLISTS                                                     
         BZ    PROCPFX                                                          
         LA    R5,LSELTAB                                                       
         USING SELTABD,R5                                                       
*                                                                               
         CLI   PFKEY,PF4                                                        
         BNE   PROCPFX                                                          
         L     RE,ATWA                                                          
         AH    RE,MODLAST                                                       
         LA    R1,CONACTH                                                       
         CR    RE,R1               ANY FIELD MODIFIED AFTER ACTION              
         BH    PROCPFX             YES                                          
         L     RE,ATWA                                                          
         AH    RE,CURDISP                                                       
         ST    RE,ACURSOR                                                       
*                                                                               
PROCPF2  BAS   RE,SETLIN           GET ADDRESSES                                
         L     R2,ASEL                                                          
         C     R2,ACURSOR                                                       
         BNE   PROCPFNT            FIND OUT WHERE CURSOR IS                     
         LA    R6,SELACT                                                        
         DROP  R5                                                               
         USING SELTABD,R6                                                       
         MVI   PFKEY,0                                                          
         MVI   CALLSP,0                                                         
         MVC   FULL,SPACES                                                      
         MVC   FULL(1),SELKEY      CONVERT ESTIMATE TO PRINTABLE                
         ZIC   R0,SELKEY+1         FORMAT                                       
         EDIT  (R0),(3,FULL+1),ALIGN=LEFT                                       
*                                                                               
         GOTO1 VCALL,WORK,=C'JOB',=C'SUMMARY',(6,CLICODE),(6,PRODCODE),X        
               (6,JOBNUM),(4,FULL),0                                            
*                                                                               
PROCPFNT L     R2,ANEXTSEL         POINT TO NEXT SELECT FIELD                   
         LA    R5,SELTABL(R5)      NEXT SELECT FIELD ENTRY                      
         BCT   R3,PROCPF2                                                       
*                                                                               
PROCPFX  B     XIT                                                              
         DROP  R6                                                               
* SUB-ROUTINE TO TEST FOR ANYTHING TO EDIT ON SCREEN                            
*                                                                               
* ON EXIT, CC=EQ TO EDIT, CC=NEQ TO CONTINUE DISPLAY                            
*                                                                               
TSTEDT   NTR1                                                                   
         LA    R2,PROSEL1H         R2=A(FIRST SELECT FIELD)                     
         SR    R3,R3                                                            
         ICM   R3,1,LNLISTS                                                     
         BZ    TSTEDTN             NOTHING ON SCREEN                            
*                                                                               
TSTEDT2  CLI   5(R2),0             TEST INPUT IN SELECT FIELD                   
         BE    TSTEDT4             NO                                           
         CLI   8(R2),C'*'          TEST ALREADY PROCESSED                       
         BE    TSTEDT4             YES                                          
         B     TSTEDTY             INPUT IN A SELECT FIELD                      
*                                                                               
TSTEDT4  LA    R4,LISTFLDS-1       R4=FIELD COUNTER                             
*                                                                               
TSTEDT5  BAS   RE,BUMP                                                          
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    *+12                YES                                          
         TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BZ    TSTEDTY             YES                                          
*                                                                               
         BCT   R4,TSTEDT5                                                       
*                                                                               
         BAS   RE,BUMP             ADVANCE TO SELECT                            
         BCT   R3,TSTEDT2          ANOTHER LINE                                 
*                                                                               
TSTEDTN  LTR   RB,RB               SET CC=NEQ                                   
         B     TSTEDTX                                                          
*                                                                               
TSTEDTY  CR    RB,RB               SET CC=EQ                                    
*                                                                               
TSTEDTX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO READ THE JOB OPTIONS                                           
*                                                                               
RDOPT    ST    RE,SAVERE                                                        
         MVC   GOADM,DATAMGR                                                    
         MVC   GOSELCUL,CUL                                                     
         MVC   GOSELCLI,CLICODE                                                 
         MVC   GOSELPRO,PRODCODE                                                
         MVC   GOSELJOB,JOBNUM                                                  
         MVI   GOWHICH,C'N'        NEW OPTIONS ONLY                             
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO DISPLAY THE JOB DATA                                           
*                                                                               
DISJOB   NTR1  ,                                                                
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(7),=C'SCHEME='                                            
         MVC   LISTAR+7(L'GOSCHEME),GOSCHEME                                    
         MVC   LISTAR+16(4),=C'NAE='                                            
         MVC   LISTAR+20(1),GONEEDAE SHOW NEED APPROVED EST VALUE               
         OC    LISTAR,SPACES                                                    
         GOTO1 SQUASHER,DMCB,LISTAR,L'LISTAR                                    
         LA    R2,PROJOBDH                                                      
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'         TRANSMIT FIELD BACK                          
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO LIST ESTIMATE RECORDS                                          
*                                                                               
* AT ENTRY, LNLISTS CONTAINS N'LIST LINES ALREADY ON SCREEN                     
*           AND LLASTEST CONTAINS LAST KEY READ OR BINARY ZERO                  
*                                                                               
LIST     NTR1                                                                   
         LA    R2,PROSEL1H                                                      
         LA    R4,KEY                                                           
         USING ACEVKEY,R4                                                       
*                                                                               
LIST2    ST    R2,ATHISLIN         INITIALIZE LIST LINE POINTER                 
*                                                                               
         XC    ACEVKEY,ACEVKEY                                                  
         MVI   ACEVRTYP,ACEVEQU                                                 
         MVI   ACEVSREC,ACEVSEQU                                                
         MVC   ACEVCUL,CUL                                                      
         MVC   ACEVCLI,CLICODE                                                  
         MVC   ACEVPROD,PRODCODE                                                
         MVC   ACEVJOB,JOBNUM                                                   
*                                                                               
LIST4    OC    LLASTEST,LLASTEST   TEST RESUMING READ                           
         BZ    LIST10              NO-STARTING FROM BEGINNING                   
*                                                                               
         MVC   ACEVTYPE(2),LLASTEST                                             
         ZIC   R1,ACEVERS          BUMP TO NEXT ESTIMATE                        
         LA    R1,1(R1)                                                         
         STC   R1,ACEVERS                                                       
         B     LIST10                                                           
*                                                                               
* FILE READING LOGIC                                                            
*                                                                               
LIST10   OI    DMINBTS,X'08'       RETURN DELETES                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         GOTO1 CATCHIOS                                                         
         B     LIST12                                                           
*                                                                               
LIST11   OI    DMINBTS,X'08'       RETURN DELETES                               
         MVI   RDUPDATE,C'N'                                                    
         LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
         GOTO1 CATCHIOS                                                         
*                                                                               
LIST12   CLC   ACEVKEY(ACEVTYPE-ACEVKEY),KEYSAVE  TEST SAME JOB                 
         BNE   LISTX               ALL DONE                                     
         LA    RF,ACEVKEY+(EVEKWC-EVEKEY)                                       
         OC    0(L'EVEKWC,RF),0(RF)  IS IT A TIME EST RECORD?                   
         BNZ   LIST11                YES - SO READ NEXT RECORD                  
*                                                                               
* DISPLAY NEW LIST LINE ON SCREEN                                               
*                                                                               
LIST20   L     R2,ATHISLIN                                                      
         BAS   RE,SETLIN                                                        
         L     R4,AIO                                                           
         BAS   RE,DISEST                                                        
*                                                                               
         MVC   LLASTEST,ACEVTYPE                                                
         MVC   ATHISLIN,ANEXTSEL                                                
         ZIC   RE,LNLISTS                                                       
         LR    R1,RE                                                            
         LA    RE,1(RE)            INCREMENT LIST LINES COUNT                   
         STC   RE,LNLISTS                                                       
         MH    R1,=Y(SELTABL)                                                   
         LA    R1,LSELTAB(R1)                                                   
         USING SELTABD,R1                                                       
         MVC   SELKEY,ACEVTYPE     SAVE ESTIMATE TYPE/VERSION                   
         MVI   SELACT,C' '                                                      
*                                                                               
         CLI   LNLISTS,NLINES      TEST SCREEN FILLED                           
         BE    LISTX                                                            
*                                                                               
LIST30   B     LIST11              NEXT ESTIMATE                                
*                                                                               
LISTX    B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY LIST LINE DATA FOR AN ESTIMATE                         
*                                                                               
DISEST   NTR1  ,                                                                
         L     R4,AIO                                                           
         USING ACEVKEY,R4                                                       
*                                                                               
         BAS   RE,SETATTR          SET ATTRIBUTES                               
*                                                                               
         L     R2,ASEL                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
         L     R2,ACODE                                                         
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(1),ACEVTYPE                                               
         ZIC   R0,ACEVERS                                                       
         EDIT  (R0),(3,LISTAR+1),ALIGN=LEFT                                     
         BAS   RE,MOVEFLD                                                       
*                                                                               
DISEST2  L     R2,ANAME1                                                        
         OI    4(R2),X'20'                                                      
         MVI   BYTE,1                                                           
         BAS   RE,DISNAME                                                       
*                                                                               
         L     R2,ANAME2                                                        
         OI    4(R2),X'20'                                                      
         MVI   BYTE,2                                                           
         BAS   RE,DISNAME                                                       
*                                                                               
DISEST4  L     R2,AAPPROV                                                       
         OI    4(R2),X'20'                                                      
         NI    1(R2),X'FF'-X'20'   UNPROTECT FIELD                              
         CLI   CHASWA,C'Y'         IS A CHANGE ALLOWED ?                        
         BE    *+8                 YES                                          
         OI    1(R2),X'20'         NO, PROTECT IT AGAIN                         
         CLI   GOAPPMFP,C'M'       APPROVE IN MAINFRAME?                        
         BE    *+8                 YES                                          
         OI    1(R2),X'20'         NO, PROTECT IT AGAIN                         
*                                                                               
         MVI   ELCODE,ACEAELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   DISEST6             NOT APPROVED                                 
*                                                                               
         USING ACEAD,R6                                                         
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(L'ACEAPPBY),ACEAPPBY                                      
         LA    R3,LISTAR+L'ACEAPPBY+1                                           
         GOTO1 DATCON,DMCB,(1,ACEADATE),(8,(R3))                                
         GOTO1 SQUASHER,DMCB,LISTAR,L'LISTAR                                    
         BAS   RE,MOVEFLD                                                       
*                                                                               
DISEST6  L     R2,APREP                                                         
         OI    4(R2),X'20'                                                      
         NI    1(R2),X'FF'-X'20'   UNPROTECT FIELD                              
         CLI   CHASWP,C'Y'         IS A CHANGE ALLOWED ?                        
         BE    *+8                 YES                                          
         OI    1(R2),X'20'         NO, PROTECT IT AGAIN                         
         CLI   GOPRPMFP,C'M'       MAINT PREPARER IN MAINFRAME?                 
         BE    *+8                 YES                                          
         OI    1(R2),X'20'         NO, PROTECT IT                               
*                                                                               
         MVI   ELCODE,ACEPELQ      LOOK FOR PREPARER DATA                       
         BAS   RE,GETELIO                                                       
         BNE   DISEST8                                                          
*                                                                               
         USING ACEPD,R6                                                         
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(L'ACEPREP),ACEPREP                                        
         LA    R3,LISTAR+L'ACEPREP+1                                            
         GOTO1 DATCON,DMCB,(1,ACEPDATE),(8,(R3))                                
         GOTO1 SQUASHER,DMCB,LISTAR,L'LISTAR                                    
         BAS   RE,MOVEFLD                                                       
*                                                                               
DISEST8  L     R2,AACT                                                          
         MVC   LISTAR,SPACES                                                    
         TM    ACSTATUS,X'80'                                                   
         BZ    DISEST10                                                         
*                                                                               
         MVC   LISTAR(7),=C'DELETED'                                            
         BAS   RE,MOVEFLD                                                       
         B     DISESTX                                                          
*                                                                               
DISEST10 MVI   ELCODE,ACEUELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   DISEST12            DID NOT FIND ELEMENT                         
*                                                                               
         USING ACEUD,R6                                                         
         MVC   LISTAR(L'ACEUPERS),ACEUPERS                                      
         LA    R3,LISTAR+L'ACEUPERS+2                                           
         GOTO1 DATCON,DMCB,(1,ACEULAST),(8,(R3))                                
         GOTO1 SQUASHER,DMCB,LISTAR,L'LISTAR                                    
         BAS   RE,MOVEFLD                                                       
*                                                                               
DISEST12 BAS   RE,BLDCOLS          BUILD COLUMN LIST                            
         BAS   RE,LOOK             LOOKUP ESTIMATE NET/GROSS                    
         BAS   RE,DISTOT           DISPLAY TOTAL FIELD                          
*                                                                               
DISESTX  B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO GET THE ESTIMATE NAME. AT ENTRY, BYTE CONTAINS                 
* NAME NUMBER                                                                   
*                                                                               
DISNAME  ST    RE,FULL                                                          
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('ACENELQ',AIO),(1,BYTE)                
         CLI   12(R1),0                                                         
         BNE   DISNAMEX                                                         
*                                                                               
         L     R6,12(R1)                                                        
         USING ACEND,R6                                                         
         MVC   LISTAR,SPACES                                                    
         ZIC   R1,ACENLEN                                                       
         SH    R1,=Y(ACENAME-ACEND+1)                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTAR(0),ACENAME                                                
         BAS   RE,MOVEFLD                                                       
*                                                                               
DISNAMEX L     RE,FULL                                                          
         BR    RE                                                               
         DROP  R6                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO DISPLAY THE ESTIMATE TOTAL                                     
*                                                                               
DISTOT   NTR1  ,                                                                
         MVC   LISTAR,SPACES                                                    
         EDIT  NETEST,(14,LISTAR),2,ALIGN=LEFT,ZERO=NOBLANK,MINUS=YES           
         L     R2,ANET                                                          
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R3,LISTAR                                                        
         EDIT  GROSSEST,(14,(R3)),2,ALIGN=LEFT,ZERO=NOBLANK,MINUS=YES           
         L     R2,AGRS                                                          
         BAS   RE,MOVEFLD                                                       
*                                                                               
DISTOTX  B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*                                                                               
MOVEFLD  ST    RE,SAVERE                                                        
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),LISTAR                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R3,8(R2,R1)         GET LAST BYTE OF FIELD                       
         LA    RF,1(R1)            GET LENGTH OF FIELD                          
         CLI   0(R3),C' '          LOOK FOR SIGNIFICANT DATA                    
         BH    *+10                                                             
         BCTR  R3,0                                                             
         BCT   RF,*-10                                                          
         STC   RF,5(R2)                                                         
*                                                                               
MOVEFLDX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO BUILD THE JOBCOLS COLUMN LIST                                  
*                                                                               
BLDCOLS  NTR1  ,                                                                
         XC    LISTAR,LISTAR       DUMMY FIELD HEADER                           
         LA    R3,LISTAR+8         WITH FIELD DATA AFTERWARD                    
         L     R4,AIO                                                           
         USING ACEVKEY,R4                                                       
         MVC   0(1,R3),ACEVTYPE    BUILD NET ESTIMATE EXPRESSION                
         LA    R3,1(R3)                                                         
         ZIC   R0,ACEVERS                                                       
         EDIT  (R0),(3,(R3)),ALIGN=LEFT                                         
         AR    R3,R0                                                            
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)            ATTACH GROSS ESTIMATE EXPRESSION             
         MVC   0(1,R3),ACEVTYPE                                                 
         LA    R3,1(R3)                                                         
         ZIC   R0,ACEVERS                                                       
         EDIT  (R0),(3,(R3)),ALIGN=LEFT,TRAIL=C'G'                              
         AR    R3,R0                                                            
         LA    RE,LISTAR+8                                                      
         SR    R3,RE               COMPUTE DATA LENGTH                          
         STC   R3,LISTAR+5                                                      
         LA    R3,8(R3)                                                         
         STC   R3,LISTAR                                                        
*                                                                               
BLDCOLS2 GOTO1 VJOBCOL,DMCB,LISTAR,COLIST,ACOMFACS                              
         CLI   4(R1),0             TEST FOR ERROR                               
         BNE   BLDCOLSX            NO                                           
         DC    H'0'                                                             
*                                                                               
BLDCOLSX B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO LOOK UP THE NET AND GROSS FOR THE JOB                          
*                                                                               
* CALLED BY LIST FOR EACH ESTIMATE RECORD                                       
* ROUTINE USES BLOCK TO BUILD JOBBLOCK AREA                                     
*                                                                               
LOOK     NTR1  ,                                                                
         LA    R6,BLOCK                                                         
         USING JBLOCKD,R6                                                       
         LR    RE,R6                                                            
         LA    RF,JBLOCKL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   JBAJOB,AJOB                                                      
         LA    R1,COLIST                                                        
         ST    R1,JBACOLS                                                       
         MVC   JBACOM,ACOMFACS                                                  
         MVC   JBAIO,AIO1                                                       
         MVC   JBAKEY,AIO1                                                      
         LA    R1,GOBLOCK                                                       
         ST    R1,JBAGOBLK                                                      
*                                                                               
         MVC   JBGETOPT,GETOPT                                                  
         MVI   JBSELFUN,JBGETEST   PASSING ESTIMATE TO JOBBER                   
         MVC   JBACOLTB(16),ACOLTAB                                             
         GOTO1 JOBBER,DMCB,JBLOCK                                               
         CLI   JBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LOOK2    L     R5,JBACOLTB                                                      
         USING JBCOLD,R5                                                        
         ZAP   NETEST,JBCOLVAL                                                  
         ZAP   GROSSEST,JBCOLVAL+L'JBCOLVAL                                     
*                                                                               
LOOKX    B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
* SUB-ROUTINE TO EDIT THE LIST SCREEN                                           
*                                                                               
EDT      NTR1                                                                   
         LA    R2,PROSEL1H         R2=A(SELECT FIELD)                           
         ZIC   R3,LNLISTS                                                       
         LA    R5,LSELTAB                                                       
         USING SELTABD,R5                                                       
*                                                                               
EDT2     BAS   RE,SETLIN           SET FIELD HEADER ADCONS                      
         L     R2,ASEL                                                          
*                                                                               
EDT3     TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    *+12                YES                                          
         TM    4(R2),X'20'         TEST FOR CHANGED UNPROTECTED FIELD           
         BZ    EDT4                YES                                          
         BAS   RE,BUMP                                                          
         C     R2,ANEXTSEL         TEST IF PAST LINE                            
         BE    EDT15               YES-NOTHING HAS CHANGED                      
         B     EDT3                LOOK AT NEXT FIELD                           
*                                                                               
EDT4     L     R2,ASEL             RESTORE R2=A(SELECT FIELD)                   
         CLI   5(R2),0             TEST ANY SELECT INPUT                        
         BE    EDT6                                                             
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   ERREND                                                           
         CLI   8(R2),C'*'          TEST ALREADY EDITED                          
         BE    EDT6                YES                                          
         CLI   8(R2),C'S'          TEST 'S'=DISPLAY                             
         BE    EDT6                                                             
         CLI   8(R2),C'D'          TEST 'D'=DELETE                              
         BE    *+12                                                             
         CLI   8(R2),C'R'          TEST 'R'=RESTORE                             
         BNE   ERREND                                                           
*                                                                               
         TM    JOBSTAT,X'40'       TEST FOR CLOSED JOB                          
         BZ    EDT5                NO                                           
         MVI   ERROR,CLOSERR       NO DELETES OR RESTORES                       
         B     ERREND                                                           
*                                                                               
EDT5     CLI   8(R2),C'D'          TEST FOR DELETE                              
         BNE   EDT6                NO                                           
         CLC   SELKEY(2),ORIGEST   TEST IF DELETING R1                          
         BNE   EDT6                                                             
         MVI   ERROR,DELORERR                                                   
         B     ERREND                                                           
*                                                                               
EDT6     MVI   UPDATE,C'N'         INITIALIZE UPDATE SWITCH TO NO               
         LA    R4,KEY              READ THE ESTIMATE RECORD                     
         USING ACEVKEY,R4                                                       
         XC    ACEVKEY,ACEVKEY                                                  
         MVI   ACEVRTYP,ACEVEQU                                                 
         MVI   ACEVSREC,ACEVSEQU                                                
         MVC   ACEVCUL,CUL                                                      
         MVC   ACEVCLI,CLICODE                                                  
         MVC   ACEVPROD,PRODCODE                                                
         MVC   ACEVJOB,JOBNUM                                                   
         MVC   ACEVTYPE(2),SELKEY                                               
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   ACEVKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE IF NOT FOUND                     
*                                                                               
         L     R4,AIO                                                           
         MVI   DELSW,C'N'                                                       
         TM    ACSTATUS,X'80'      TEST FOR DELETED RECORD                      
         BZ    *+8                                                              
         MVI   DELSW,C'Y'          NOTE RECORD IS DELETED                       
         BAS   RE,EDTEST           EDIT REGULAR LIST LINE                       
*                                                                               
EDT8     MVC   SELACT,8(R2)        SAVE ACTION                                  
         TM    4(R2),X'20'         TEST FOR UNCHANGED SELECT FIELD              
         BO    EDT9                YES                                          
*                                                                               
         OI    4(R2),X'20'         NOTE AS PREVIOUSLY VALIDATED                 
         OI    6(R2),X'80'         XMIT FIELD BACK                              
         CLI   5(R2),0             TEST FOR SELECT INPUT                        
         BE    EDT9                NO                                           
         CLI   8(R2),C'*'          TEST ALREADY THERE                           
         BE    EDT9                                                             
*                                                                               
         MVI   8(R2),C'*'          MARK SELECT FIELD                            
         CLI   SELACT,C'S'         TEST FOR SELECT                              
         BE    EDT9                YES                                          
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         OI    ACSTATUS,X'80'      NOTE DELETED                                 
         CLI   SELACT,C'D'        TEST FOR DELETE                               
         BE    EDT9                                                             
         NI    ACSTATUS,X'FF'-X'80' TURN OFF DELETE BIT                         
*                                                                               
EDT9     CLI   UPDATE,C'Y'                                                      
         BNE   EDT10                                                            
*                                                                               
EDT9A    GOTO1 WRITE                                                            
EDT9AA   MVI   KEY+EVEKSEQ-EVEKEY+L'EVEKSEQ,X'FF'                               
         GOTO1 HIGH                LOOK FOR TIME ESTIMATING RECORDS             
         CLC   KEY(EVEKWC-EVEKEY),KEYSAVE TEST SAME ESTIMATE                    
         BNE   EDT9B               NO                                           
         CLI   SELACT,C'D'         TEST FOR DELETE                              
         BNE   *+12                NO                                           
         OI    ACSTATUS,X'80'      YES - DELETE THE RECORD                      
         B     EDT9A                                                            
         SR    RE,RE                                                            
         ICM   RE,3,KEY+ACCORLEN                                                
         CH    RE,=Y(ACCORFST+1)   TEST EMPTY RECORD                            
         BNH   EDT9AA              YES - DON'T RESTORE IT                       
         NI    ACSTATUS,X'FF'-X'80' TURN OFF DELETE BIT                         
         B     EDT9A                                                            
*                                                                               
EDT9B    MVC   KEY,KEYSAVE                                                      
         LA    RF,KEY+(EVEKWC-EVEKEY)                                           
         XC    0(L'EVEKWC+L'EVEKSWC+L'EVEKSEQ+1,RF),0(RF)                       
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 READ                REREAD THE ESTIMATE RECORD                   
*                                                                               
         GOTO1 VCLEARF,PARAS,ANAME1,ANEXTSEL      CLEAR UNPROTECTED             
         GOTO1 VCLEARF,PARAS,(1,ANAME1),ANEXTSEL  ... AND PROT'D FIELDS         
*                                                                               
         BAS   RE,DISEST           RE-DISPLAY ESTIMATE DATA                     
*                                                                               
         CLI   SELACT,C'D'        TEST FOR DELETE                               
         BNE   *+8                                                              
         BAS   RE,DELSES           DELETE ANY SESSION RECORDS                   
*                                                                               
         CLI   SELACT,C'R'         TEST FOR RESTORE                             
         BNE   *+8                                                              
         BAS   RE,RESTSES          RESTORE SESSION RECORDS                      
*                                                                               
EDT10    CLI   SELACT,C'S'         TEST FOR SELECT                              
         BNE   EDT15               NO                                           
*                                                                               
         MVC   FULL,SPACES                                                      
         MVC   FULL(1),ACEVTYPE    CONVERT ESTIMATE TO PRINTABLE                
         ZIC   R0,ACEVERS          FORMAT                                       
         EDIT  (R0),(3,FULL+1),ALIGN=LEFT                                       
*                                                                               
         MVI   PFKEY,0                                                          
         L     RF,VCALL                                                         
         SR    R1,R1                                                            
         ICM   R1,1,CALLSP                                                      
         BZ    EDT12                                                            
         LA    RE,CALLSTK                                                       
*                                                                               
         CLI   0(RE),X'32'         IS ESTIMATE IN STACK ALREADY ?               
         BE    *+16                YES                                          
         LA    RE,1(RE)            NO, KEEP LOOKING                             
         BCT   R1,*-12                                                          
         B     EDT12                                                            
*                                                                               
         MVI   CALLSP,0                                                         
         L     RF,VTRANSF                                                       
*                                                                               
EDT12    GOTO1 (RF),WORK,RECNJOB,ACTNEST,(6,CLICODE),(6,PRODCODE),(6,JOX        
               BNUM),(4,FULL),0                                                 
*                                                                               
EDT15    L     R2,ANEXTSEL                                                      
         LA    R5,SELTABL(R5)                                                   
         BCT   R3,EDT2                                                          
*                                                                               
EDTX     B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO EDIT A LIST LINE                                               
*                                                                               
EDTEST   NTR1                                                                   
         L     R2,ANAME1           R2=A(ESTIMATE NAME FIELD)                    
         TM    4(R2),X'20'         TEST IF ANY CHANGE                           
         BO    EDTEST2                                                          
*                                                                               
         MVI   BYTE,1              SET FIELD NUMBER                             
         BAS   RE,EDTNAME          EDIT THE NAME FIELD                          
*                                                                               
EDTEST2  L     R2,ANAME2           R2=A(NAME 2 FIELD)                           
         TM    4(R2),X'20'                                                      
         BO    EDTEST4                                                          
*                                                                               
         MVI   BYTE,2                                                           
         BAS   RE,EDTNAME                                                       
*                                                                               
EDTEST4  L     R2,AAPPROV                                                       
         TM    4(R2),X'20'                                                      
         BO    EDTEST6                                                          
*                                                                               
         BAS   RE,EDTAPP                                                        
*                                                                               
EDTEST6  L     R2,APREP                                                         
         TM    4(R2),X'20'                                                      
         BO    EDTEST8                                                          
*                                                                               
         BAS   RE,EDTPREP                                                       
*                                                                               
EDTEST8  CLI   UPDATE,C'Y'                                                      
         BNE   EDTESTX                                                          
*                                                                               
         TM    JOBSTAT,X'40'       TEST IF JOB IS CLOSED                        
         BZ    EDTEST10            NO-OK TO CHANGE                              
*                                                                               
         MVI   ERROR,CLOSERR       FIND CHANGED FIELD                           
         L     R2,ANAME1           AND TAKE AN ERROR EXIT                       
         SR    RE,RE                                                            
         L     RF,ANEXTSEL                                                      
         BCTR  RF,0                                                             
*                                                                               
EDTEST9  TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    *+12                YES                                          
         TM    4(R2),X'20'                                                      
         BZ    ERREND                                                           
         IC    RE,0(R2)                                                         
         BXLE  R2,RE,EDTEST9                                                    
*                                                                               
EDTEST10 L     R2,ANAME1                                                        
         OI    4(R2),X'20'                                                      
         L     R2,ANAME2                                                        
         OI    4(R2),X'20'                                                      
         L     R2,AAPPROV                                                       
         OI    4(R2),X'20'                                                      
         L     R2,APREP                                                         
         OI    4(R2),X'20'                                                      
*                                                                               
EDTESTX  B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO EDIT A NAME FIELD.  AT ENTRY, R2=A(FIELD HEADER)               
* AND BYTE CONTAINS FIELD NUMBER                                                
*                                                                               
EDTNAME  ST    RE,SAVERE                                                        
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),('ACENELQ',AIO),(1,BYTE)                
         CLI   5(R2),0             TEST FOR INPUT IN FIELD                      
         BE    EDTNAMEX            NO                                           
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ACEND,R6                                                         
         MVI   ACENEL,ACENELQ                                                   
         MVC   ACENNUM,BYTE                                                     
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACENAME(0),8(R2)                                                 
         LA    R1,ACENAME-ACEND+1(R1) RESTORE EL LENGTH                         
         STC   R1,ACENLEN                                                       
         GOTO1 ADDELEM                                                          
*                                                                               
EDTNAMEX MVI   UPDATE,C'Y'                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT THE APPROVAL FIELD                                        
*                                                                               
EDTAPP   NTR1  ,                                                                
         MVI   UPDATE,C'Y'         NOTE TO UPDATE RECORD                        
         MVI   ERROR,CANTAPPR                                                   
         BAS   RE,CHKFUND          SEE IF FUNDED & UPDATE AS NEEDED             
         BE    EDTAPP1             OK                                           
         MVI   5(R2),0             ERROR, CLEAR APPROVAL FIELD                  
         XC    8(L'PROAPP,R2),8(R2)                                             
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'                                                      
         L     R2,ASEL                                                          
         B     ERREND                                                           
*                                                                               
EDTAPP1  MVI   ELCODE,ACEAELQ                                                   
         GOTO1 REMELEM                                                          
         CLI   5(R2),0                                                          
         BE    EDTAPPX             FIELD WAS ERASED                             
*                                                                               
         MVI   ERROR,APPLNERR                                                   
         L     R4,AIO                                                           
         USING ACEVKEY,R4                                                       
         CLI   ACEVTYPE,ACEVPLN    TEST FOR PLANNING ESTIMATE                   
         BE    ERREND              YES-STOP IT                                  
*                                                                               
         BAS   RE,EDTPD            EDIT FOR PERSON/DATE                         
*                                                                               
EDTAPP2  XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ACEAD,R6                                                         
         MVI   ACEAEL,ACEAELQ                                                   
         MVI   ACEALEN,ACEALENQ                                                 
         MVC   ACEAINP,TODAYP                                                   
         MVC   ACEAPPBY,PERSON                                                  
         MVC   ACEADATE,DATE                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
EDTAPPX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO EDIT THE PREPARER FIELD                                        
*                                                                               
EDTPREP  NTR1  ,                                                                
         MVI   UPDATE,C'Y'         NOTE TO UPDATE RECORD                        
         MVI   ELCODE,ACEPELQ                                                   
         GOTO1 REMELEM                                                          
         CLI   5(R2),0                                                          
         BE    EDTPREPX            FIELD WAS ERASED                             
*                                                                               
         BAS   RE,EDTPD            EDIT FOR PERSON/DATE                         
*                                                                               
EDTPREP2 XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ACEPD,R6                                                         
         MVI   ACEPEL,ACEPELQ                                                   
         MVI   ACEPLEN,ACEPLENQ                                                 
         MVC   ACEPINP,TODAYP                                                   
         MVC   ACEPREP,PERSON                                                   
         MVC   ACEPDATE,DATE                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
EDTPREPX B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO EDIT A FIELD FOR PERSON/DATE INPUT                             
* ON EXIT, PERSON CONTAINS INPUT NAME AND DATE CONTAINS DATE VALUE              
*                                                                               
EDTPD    NTR1  ,                                                                
         LA    R3,8(R2)            INITIALIZE INPUT POINTER                     
         GOTO1 PARSNIP,DMCB,(R2),BLOCK,('PSNNONLQ',PDSEP)                       
         MVI   ERROR,INVALID                                                    
         CLI   8(R1),0             TEST FOR PARSNIP ERROR                       
         BE    EDTPD1              NO                                           
         SR    R3,R3                                                            
         ICM   R3,7,9(R1)                                                       
         B     EDTPDR                                                           
*                                                                               
EDTPD1   CLI   4(R1),2             TEST NO MORE THAN 2 COMPONENTS               
         BH    EDTPDR                                                           
*                                                                               
EDTPD2   LA    R4,BLOCK                                                         
         USING PSND,R4                                                          
         L     R3,PSNCOMP          R3=A(COMPONENT)                              
         CLI   PSNTAG,PSNFLDQ                                                   
         BNE   EDTPDR                                                           
         CLI   PSNLEN,8                                                         
         BH    EDTPDR                                                           
*                                                                               
         MVC   PERSON,SPACES       EXTRACT PERSON                               
         ZIC   R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PERSON(0),0(R3)                                                  
*                                                                               
EDTPD4   MVC   DATE,TODAYP         DEFAULT IS TODAY'S DATE                      
         ICM   R4,15,PSNFLD        IS THERE ANOTHER FIELD                       
         BZ    EDTPDX              NO                                           
*                                                                               
EDTPD6   L     R3,PSNCOMP                                                       
         MVI   ERROR,INVDATE                                                    
         MVC   BYTE,AGYLANG                                                     
         OI    BYTE,PVINSGLS       RETURN SINGLE DATE AS SINGLE                 
         OI    BYTE,PVINSGLO       SINGLE DATE ALLOWED ONLY                     
         GOTO1 PERVAL,DMCB,(PSNLEN,(R3)),(BYTE,WORK)                            
         CLI   4(R1),PVRCONE       TEST SINGLE DATE RETURNED                    
         BNE   EDTPDR                                                           
         LA    R1,WORK                                                          
         MVC   DATE,PVALPSTA-PERVALD(R1)                                        
*                                                                               
EDTPDX   B     XIT                                                              
*                                                                               
EDTPDR   LA    RE,8(R2)            POINT AT START OF FIELD                      
         SR    R3,RE               COMPUTE INDEX INTO FIELD                     
         STC   R3,ERRNDX                                                        
         B     ERREND                                                           
         SPACE 2                                                                
PDSEP    DC    AL1(1),C'='                                                      
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C' '                                                      
         DC    AL1(1),C' '                                                      
         EJECT                                                                  
*******************************************************************             
* SUB-ROUTINE TO DELETE ANY SESSION RECORDS A FOR THE ESTIMATE    *             
* IN AIO                                                          *             
*******************************************************************             
         SPACE 1                                                                
DELSES   NTR1                                                                   
         BAS   RE,BLDSESKY                                                      
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         USING SESRECD,R4                                                       
DELSES10 LA    R4,KEY                                                           
         CLC   SESKEY(SESKMED-SESKEY),KEYSAVE                                   
         BNE   DELSESX             IS THIS A SESSION REC FOR THIS EST           
*                                                                               
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         OI    ACSTATUS,X'80'      TURN ON DELETE BIT                           
         GOTO1 WRITE               WRITE SESSION EST OUT                        
*                                                                               
         GOTO1 SEQ                                                              
         B     DELSES10                                                         
DELSESX  B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* SUB-ROUTINE TO RESTORE ANY SESSION RECORDS FOR THE ESTIMATE     *             
* IN AIO                                                          *             
*******************************************************************             
         SPACE 1                                                                
RESTSES  NTR1                                                                   
         BAS   RE,BLDSESKY                                                      
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'       RETURN DELETES                               
         GOTO1 HIGH                                                             
*                                                                               
         USING SESRECD,R4                                                       
RESTSESA LA    R4,KEY                                                           
         CLC   SESKEY(SESKMED-SESKEY),KEYSAVE                                   
         BNE   RESTSESX            IS THIS A SESSION REC FOR THIS EST           
*                                                                               
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         NI    ACSTATUS,X'FF'-X'80' TURN OFF DELETE BIT                         
         GOTO1 WRITE               WRITE SESSION EST OUT                        
*                                                                               
         MVI   ELCODE,GDAELQ       LOOK FOR TRAILER EL                          
         BAS   RE,GETELIO                                                       
         BE    RESTSESX                                                         
*                                                                               
         GOTO1 SEQ                                                              
         B     RESTSESA                                                         
RESTSESX B     XIT                                                              
*                                                                               
*                                                                               
*        USIND THE ESTIMATE RECORD IN AIO, BUILD A SESSION KEY IN KEY           
*                                                                               
BLDSESKY L     R6,AIO                                                           
         LA    R4,KEY                                                           
         USING SESRECD,R4                                                       
         XC    SESKEY,SESKEY                                                    
         MVC   SESKEY(SESKMED-SESKEY),0(R6) CPJ, VERSION                        
         MVI   SESKSUB,SESKSUBQ   SESSION EST SUB TYPE                          
         BR    RE                                                               
         EJECT                                                                  
*******************************************************************             
* SUB-ROUTINE TO SEE IF JOB IS FUNDED AND TO MAKE APPROPRIATE     *             
* UPDATES                                                         *             
*******************************************************************             
*                                                                               
CHKFUND  NTR1                                                                   
         L     R4,AIO                                                           
         USING ACEVKEY,R4                                                       
         CLI   ACEVTYPE,ACEVREV    ONLY CONCERNED WITH REVISIONS                
         BNE   CHKFOK                                                           
         MVC   SVEST,ACEVTYPE                                                   
         DROP R4                                                                
*                                                                               
         MVC   AIO,AIO3                                                         
         USING JFNELD,R6                                                        
         MVI   ELCODE,JFNELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   CHKFOK              NOT FUNDED, NOTHING ELSE TO DO               
*                                                                               
         CLI   5(R2),0             ARE WE UNAPPROVING?                          
         BNE   CHKF00              NO                                           
*                                                                               
         CLC   SVEST,JFNEST        YES, IS ESTIMATE FUNDED?                     
         BNE   CHKFOK              NOPE, NOTHING ELSE TO DO                     
*                                                                               
CHKF00   ZAP   SVJAMT,JFNAMT       SAVE THE AMOUNT CURRENTLY FUNDED             
*                                                                               
         MVC   AIO,AIO2            USE IO2 FOR AUTHORIZATION RECORD             
         LA    R4,KEY                                                           
         USING AUTKEY,R4                                                        
         MVC   AUTKEY,SPACES                                                    
         MVI   AUTKTYP,AUTKTYPQ                                                 
         MVI   AUTKSUB,AUTKSUBQ                                                 
         MVC   AUTKCPY(3),CUL                                                   
         MVC   AUTKOGR(L'JFNKEY),JFNKEY                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   AUTKEY,KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE IF NOT FOUND                     
         DROP  R4,R6                                                            
*                                                                               
         USING AUTHELD,R6                                                       
         MVI   ELCODE,AUTHELQ      GET DATA ELEMENT                             
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                MUST HAVE THIS ONE                           
         ST    R6,SAVER6                                                        
*                                                                               
CHKF02   BAS   RE,NEXTEL           ANY MORE?                                    
         BNE   CHKF04              NO                                           
         ST    R6,SAVER6           YES, FIND THE LAST ONE                       
         B     CHKF02                                                           
*                                                                               
CHKF04   L     R6,SAVER6           GET ADDRESS OF LAST AUTHELQ                  
         ZAP   SVAAMT,AUTHAMT      SAVE THE AMOUNT                              
*                                                                               
         LA    R4,KEY              READ THE FUND RECORD NOW                     
         USING FUNKEY,R4                                                        
         MVC   FUNKEY,SPACES                                                    
         MVI   FUNKTYP,FUNKTYPQ                                                 
         MVI   FUNKSUB,FUNKSUBQ                                                 
         MVC   FUNKCPY(3),CUL      SAME KEY AS AUTRECD                          
         MVC   FUNKOGR(L'JFNKEY),KEYSAVE+5                                      
*                                                                               
         MVI   RDUPDATE,C'Y'       MAY NEED TO UPDATE THIS RECORD               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   FUNKEY,KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE IF NOT FOUND                     
         DROP  R4,R6                                                            
*                                                                               
         USING FUNELD,R6                                                        
         MVI   ELCODE,FUNELQ       GET DATA ELEMENT                             
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE IF NOT FOUND                     
         ZAP   SVFAMT,FUNAMT       SAVE THE FUNDED AMOUNT                       
*                                                                               
         CLI   5(R2),0             ARE WE UNAPPROVING?                          
         BNE   CHKF08              NO                                           
         SP    FUNAMT,SVJAMT       YES, DECREASE AMOUNT FUNDED                  
         GOTO1 WRITE               REWRITE FUND RECORD                          
*                                                                               
         USING FJNELD,R6                                                        
CHKF05   MVI   ELCODE,FJNELQ                                                    
         BAS   RE,GETELIO          FIND THE FUND JOB ELEMENT                    
         B     *+8                                                              
*                                                                               
CHKF06   BAS   RE,NEXTEL                                                        
         BE    CHKF07                                                           
         GOTO1 SEQ                 IF FUNDED, IT MUST BE HERE                   
         LA    R4,KEY              READ THE FUND RECORD NOW                     
         USING FUNKEY,R4                                                        
         CLC   FUNKEY(L'FUNKEY-1),KEYSAVE                                       
         BE    CHKF05                                                           
         DC    H'0'                                                             
*                                                                               
CHKF07   L     R4,AIO3                                                          
         CLC   FJNJOB,3(R4)        IS THIS THE JOB?                             
         BNE   CHKF06              NO, KEEP LOOKING                             
*                                                                               
         MVI   0(R6),X'FF'         YES, MARK IT FOR DELETION                    
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         L     R4,AIO                                                           
         CLI   FUNKSEQ,X'40'                                                    
         BE    CHKF07A             ORIGINAL FUND RECORD, OK                     
         MVI   ELCODE,FJNELQ       OTHERWISE, ANY JOB ELEMENTS:                 
         BAS   RE,GETELIO                                                       
         BE    CHKF07A             YES, OK                                      
         OI    FUNRSTA,X'80'       NO, DELETE IT                                
*                                                                               
CHKF07A  GOTO1 WRITE               REWRITE FUND RECORD                          
         DROP  R6                                                               
*                                                                               
         MVC   AIO,AIO3            GET JOB IO AREA                              
         MVI   ELCODE,JOBELQ       GET JOB ELEMENT                              
         BAS   RE,GETELIO                                                       
         BE    *+6                 WE ALWAYS HAVE THIS ELEMENT                  
         DC    H'0'                                                             
*                                                                               
         USING JOBELD,R6                                                        
         NI    JOBSTA2,X'FF'-JOBSFUN    INDICATE NOT FUNDED                     
*                                                                               
         MVI   ELCODE,JFNELQ                                                    
         GOTO1 REMELEM             REMOVE JOB FUNDING ELEMENT                   
         GOTO1 WRITE               WRITE BACK THE JOB RECORD                    
         B     CHKFOK                                                           
*                                                                               
         USING FUNELD,R6                                                        
CHKF08   ZAP   DUB,SVAAMT          APPROVALS PROCESSED HERE                     
         SP    DUB,SVFAMT          FIND AVAILABLE AMOUNT                        
         AP    DUB,SVJAMT          ADD BACK CURRENTLY FUNDED AMOUNT             
*                                                                               
         L     R2,ANET             GET THE ENTRY                                
         SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'80',8(R2)),(RF)                                  
         ZAP   SVNET,6(6,R1)       SAVE AMOUNT                                  
*                                                                               
         CP    DUB,SVNET           IS THERE ENOUGH MONEY LEFT?                  
         BL    CHKFNG              NO                                           
*                                                                               
         SP    FUNAMT,SVJAMT       YES, REMOVE OLD AMOUNT                       
         AP    FUNAMT,SVNET        AND ADD IN NEW AMOUNT                        
         GOTO1 WRITE               REWRITE FUNDED RECORD                        
         DROP  R6                                                               
*                                                                               
         USING JFNELD,R6                                                        
         MVC   AIO,AIO3            GET JOB IO AREA                              
         MVI   ELCODE,JFNELQ       GET JOB FUND ELEMENT                         
         BAS   RE,GETELIO                                                       
         BE    *+6                 WE HAVE TO HAVE THIS ELEMENT                 
         DC    H'0'                                                             
*                                                                               
         ZAP   JFNAMT,SVNET        CHANGE THE FUNDED AMOUNT                     
         MVC   JFNEST,SVEST          AND THE ESTIMATE NUMBER                    
         GOTO1 WRITE               WRITE BACK THE JOB RECORD                    
*                                                                               
CHKFOK   CR    RB,RB               CONDITION CODE EQUAL                         
         B     CHKFX                                                            
*                                                                               
CHKFNG   LTR   RB,RB               CONDITION CODE NOT EQUAL                     
*                                                                               
CHKFX    MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* SUB-ROUTINE TO CALL THE ELIST REPORT MODULE TO GENERATE AN      *             
* ON-LINE REPORT                                                  *             
*******************************************************************             
         SPACE 1                                                                
EREP     NTR1  WORK=(R6,ERWORKL)                                                
         USING ERWORKD,R6                                                       
         MVC   ERSYSRD,SYSRD       SAVE SYSRD                                   
         LA    RE,ERSAVE           RE=A(SAVE STORAGE)                           
         LA    RF,LOCALLN          RF=L'LOCAL WORKING STORAGE                   
         LR    R1,RF                                                            
         LA    R0,LOCAL                                                         
         MVCL  RE,R0               SAVE LOCAL STORAGE                           
*                                                                               
         MVI   PFKEY,0                                                          
         L     RF,=V(DUMMY)                                                     
         A     RF,RELO                                                          
         ST    RF,SYSDUMMY         LOAD IN ELIST AFTER THIS MODULE              
         BAS   RE,ERSETRD          SET RETURN POINT HERE                        
         GOTO1 VCALL,WORK,RECNEL,ACTNREP,=C',',=C',',(6,CLICODE),(6,PROX        
               DCODE),(6,JOBNUM),0                                              
*                                                                               
         MVC   DUB,SPACES          SET UP WHEN FIELD                            
         MVC   DUB(4),=C'NOW,'                                                  
         MVC   DUB+4(3),TWAALIAS                                                
         MVC   CONWHEN,DUB                                                      
         MVI   CONWHENH+5,5                                                     
         CLI   DUB+5,C' '                                                       
         BE    EREP10                                                           
         MVI   CONWHENH+5,6                                                     
         CLI   DUB+6,C' '                                                       
         BE    EREP10                                                           
         MVI   CONWHENH+5,7                                                     
*                                                                               
EREP10   GOTO1 GENCON,DMCB,(R8)                                                 
*                                                                               
EREP15   GOTO1 VRETURN             RESTORE SCREEN                               
*                                                                               
EREP20   MVI   GOAGAIN,C'N'        TURN OFF GENCON CALL FLAG                    
         MVC   SYSRD,ERSYSRD       RESTORE SYSRD                                
         LA    RE,LOCAL                                                         
         LA    RF,LOCALLN          RESTORE LOCAL STORAGE                        
         LR    R1,RF                                                            
         LA    R0,ERSAVE                                                        
         MVCL  RE,R0                                                            
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 2                                                                
ERSETRD  NTR1  ,                                                                
         ST    RD,SYSRD                                                         
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO SET ADCONS FOR A LIST FIELD LINE                               
* AT ENTRY, R2=A(SELECT FIELD HEADER)                                           
*                                                                               
SETLIN   ST    RE,SAVERE                                                        
         LA    R0,LISTFLDS                                                      
         LA    R1,ASEL                                                          
         ST    R2,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BAS   RE,BUMP                                                          
         BCT   R0,*-12                                                          
         ST    R2,ANEXTSEL                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO SET ATTRIBUTE BITS FOR THE LIST FIELDS                         
*        ASSUMES AIO HAS THE RECORD YOU WANT TO SET DISPLAY ATTRIBUTES          
*        FOR                                                                    
*                                                                               
SETATTR  NTR1                                                                   
*                                                                               
         L     R4,AIO                                                           
         USING ACEVKEY,R4                                                       
         LA    R3,NORMATTR         ASSUME USING NORMAL ATTRIBUTES               
         TM    ACSTATUS,X'80'      IS RECORD DELETED                            
         BNO   *+8                 NO                                           
         LA    R3,DELATTR          YES, USE "DELETED" ATTRIBUTES                
         L     R2,ASEL             START AT THIS SELECT FIELD                   
         LA    R0,LISTFLDS         NUMBER OF FIELDS TO SET                      
*                                                                               
SETA30   NI    1(R2),X'FF'-FATBPROT-FATBHIGH                                    
         OC    1(1,R2),0(R3)                                                    
         BAS   RE,BUMP                                                          
         LA    R3,1(R3)                                                         
         BCT   R0,SETA30                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
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
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
LISTMSG  DC    C'LIST DISPLAYED - PRESS ENTER FOR NEXT PAGE'                    
LIST2MSG DC    C'LIST DISPLAYED'                                                
EDTMSG   DC    C'CHANGES COMPLETED'                                             
JEXJOB   DC    C'(EXP)'                                                         
ORIGEST  DC    C'R',X'01'                                                       
*                                                                               
NORMATTR DC    AL1(FATBHIGH)                                                    
         DC    AL1(FATBPROT)                                                    
         DC    AL1(FATBHIGH)                                                    
         DC    AL1(FATBHIGH)                                                    
         DC    AL1(FATBHIGH)                                                    
         DC    AL1(FATBHIGH)                                                    
         DC    AL1(FATBPROT)                                                    
         DC    AL1(FATBPROT)                                                    
         DC    AL1(FATBPROT)                                                    
*                                                                               
DELATTR  DC    AL1(FATBHIGH)                                                    
         DC    AL1(FATBPROT)                                                    
         DC    AL1(FATBPROT)                                                    
         DC    AL1(FATBPROT)                                                    
         DC    AL1(FATBPROT)                                                    
         DC    AL1(FATBPROT)                                                    
         DC    AL1(FATBPROT)                                                    
         DC    AL1(FATBPROT)                                                    
         DC    AL1(FATBPROT)                                                    
         EJECT                                                                  
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
*ACJOBBERD                                                                      
*ACPROWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENBOTH                                                                      
*ACGENFILE                                                                      
*DDFLDHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN               USE THE DRONEBLK AREA                        
LOCAL    DS    0X                                                               
RELO     DS    A                                                                
INTMODE  DS    X                   INTERNAL MODE                                
KEYCHG   DS    C                                                                
CHASWA   DS    C                   AUTHORIZE FOR APPROVER CHANGE                
CHASWP   DS    C                   AUTHORIZE FOR PREPARER CHANGE                
*                                                                               
SAVERE   DS    A                                                                
SAVER6   DS    A                                                                
ACURSOR  DS    A                                                                
*                                                                               
ATHISLIN DS    A                                                                
ASEL     DS    A                                                                
ACODE    DS    A                                                                
ANAME1   DS    A                                                                
ANAME2   DS    A                                                                
AAPPROV  DS    A                                                                
APREP    DS    A                                                                
AACT     DS    A                                                                
ANET     DS    A                                                                
AGRS     DS    A                                                                
ANEXTSEL DS    A                                                                
*                                                                               
DELSW    DS    C                                                                
UPDATE   DS    C                                                                
PERSON   DS    CL8                                                              
DATE     DS    CL3                                                              
NETEST   DS    PL8                                                              
GROSSEST DS    PL8                                                              
*                                                                               
SVAAMT   DS    PL6                                                              
SVFAMT   DS    PL6                                                              
SVJAMT   DS    PL6                                                              
SVNET    DS    PL6                                                              
SVEST    DS    CL2                                                              
*                                                                               
AJOB     DS    A                                                                
ACOLTAB  DS    A                                                                
LCOLTAB  DS    F                                                                
AOPVTAB  DS    A                                                                
LOPVTAB  DS    F                                                                
*                                                                               
COLIST   DS    XL80                                                             
*                                                                               
       ++INCLUDE ACGOBLOCK                                                      
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
*                                                                               
*                                                                               
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROC3D                                                       
         SPACE 2                                                                
         ORG   T60BFFD+2618                                                     
LSAVES   DS    0D                                                               
LNLISTS  DS    X                   N'LISTS ON SCREEN                            
LLASTEST DS    CL2                 LAST ESTIMATE ON SCREEN                      
LSELTAB  DS    CL(NLINES*SELTABL)                                               
         DS    CL((SAVAREA-LSAVES)-(*-LSAVES))  SPARE                           
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
NLINES   EQU   7                   N'LIST SCREEN LINES                          
LISTFLDS EQU   (ANEXTSEL-ASEL)/4   N'FIELDS ON LIST SCREEN LINE                 
FSTLIST  EQU   1                                                                
DISLIST  EQU   2                                                                
EDTLIST  EQU   3                                                                
         SPACE 2                                                                
* DSECT TO COVER SELECT TABLE                                                   
*                                                                               
SELTABD  DSECT                                                                  
SELACT   DS    C                   SELECT ACTION                                
SELKEY   DS    CL2                                                              
SELTABL  EQU   *-SELTABD                                                        
         SPACE 2                                                                
* DSECT TO COVER EREP SUB-ROUTINE WORKING STORAGE                               
*                                                                               
ERWORKD  DSECT                                                                  
ERSYSRD  DS    A                                                                
ERSAVE   DS    XL(LOCALLN)         SAVED LOCAL WORKING STORAGE                  
ERWORKL  EQU   *-ERWORKD                                                        
         EJECT                                                                  
       ++INCLUDE DDPARSNIPD                                                     
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054ACPRO33   04/11/07'                                      
         END                                                                    
