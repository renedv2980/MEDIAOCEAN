*          DATA SET ACINT15    AT LEVEL 015 AS OF 05/01/02                      
*PHASE T61915A,*                                                                
         TITLE 'T61915 - CHECK LIST'                                            
T61915   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T61915**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         ST    R2,RELO                                                          
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BE    CHK2                                                             
         CLI   MODE,VALREC                                                      
         BE    CHK5                                                             
         B     XIT                                                              
*                                                                               
* VALKEY LOGIC                                                                  
*                                                                               
CHK2     CLI   CALLER,X'12'        TEST CALLED BY HEADER                        
         BE    CHK3                YES                                          
         CLI   RETURNED,X'15'      TEST RETURNED SAME TRANSACTN                 
         BNE   CHK2A               NO                                           
         CLI   TWALACT,ACTNDFT     TEST IF RETURNED FROM DRAFT                  
         BE    CHK4                YES                                          
         CLI   TWALACT,ACTNFILT    TEST IF RETURNED FROM FILTER                 
         BE    CHK4                                                             
*                                                                               
CHK2A    BAS   RE,VALHED                                                        
         MVI   INTMODE,EDTLIST     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES-SET FIRST TIME LIST                      
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES                                          
         B     XIT                                                              
*                                                                               
* CALLED FROM HEADER TO START A LIST                                            
*                                                                               
CHK3     BAS   RE,SETHED           SET HEADLINE FIELDS                          
         MVI   INTMODE,FSTLIST     SET TO BEGIN LIST                            
         B     XIT                                                              
*                                                                               
* RETURNED FROM DRAFT OR FILTER                                                 
*                                                                               
CHK4     LA    R2,CONHEADH         PUT OUT MESSAGE                              
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(L'SPOOLID),SPOOLID                                        
         LA    R4,LISTAR+L'SPOOLID                                              
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         SR    R0,R0                                                            
         ICM   R0,3,SPOOLRPN                                                    
         EDIT  (R0),(5,(R4)),ALIGN=LEFT                                         
         AR    R4,R0                                                            
         MVC   1(L'PQMSG,R4),PQMSG                                              
         BAS   RE,MOVEFLD                                                       
*                                                                               
         LA    R2,LISSEL1H         POSITION THE CURSOR                          
         CLI   LNLISTS,MAXRECS     TEST IF SCREEN WAS FILLED                    
         BE    *+8                 YES                                          
         LA    R2,LISPROH                                                       
         ST    R2,ACURFORC         MESSAGE SET BY REPORT OVERLAY                
         GOTO1 ERREX2              EXIT BACK TO USER NOW                        
*                                                                               
* VALREC LOGIC-DISPLAY OR CHANGE                                                
*                                                                               
CHK5     BAS   RE,SETSCR                                                        
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME LIST                     
         BE    CHK6                                                             
         BAS   RE,PROCPF           PROCESS ANY PF KEYS                          
         BAS   RE,TSTEDT           TEST ANYTHING TO EDIT                        
         BE    CHK20               YES                                          
*                                                                               
         MVI   INTMODE,DISLIST     CONTINUE LIST                                
*                                                                               
* DISPLAY LOGIC                                                                 
*                                                                               
CHK6     GOTO1 VCLEARF,DMCB,AFSTSEL,AENDSCR                                     
         GOTO1 (RF),(R1),(1,AFSTSEL),APFFLD                                     
*                                                                               
         XC    FSTKEY,FSTKEY       INITIALIZE LIST TO START AGAIN               
         MVC   FSTRNUM,=H'1'                                                    
*                                                                               
CHK7     CLI   RETURNED,X'12'      TEST RETURNED FROM HEADER                    
         BE    CHK8                YES-TRY TO RE-DISPLAY CALLING PAGE           
         CLI   INTMODE,DISLIST     TEST CONTINUING LIST                         
         BNE   CHK9                NO                                           
         CLI   RETURNED,X'13'      TEST RETURNED FROM CHECK ESTIMATE            
         BE    CHK8                YES                                          
         CLI   RETURNED,X'14'      TEST RETURNED FROM ESTIMATE ADJUST           
         BNE   CHK9                                                             
*                                                                               
CHK8     CLI   LNLISTS,0           YES-RE-CREATE LAST SCREEN                    
         BE    CHK10               IF ANYTHING WAS ON IT                        
         LA    R5,LSELTAB                                                       
         USING SELTABD,R5                                                       
         MVC   FSTKEY,SELKEY       SET TO FIRST KEY ON SCREEN                   
         MVC   FSTRNUM,SELRNUM                                                  
         B     CHK10                                                            
*                                                                               
CHK9     CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME                          
         BE    CHK10               YES                                          
         CLI   PFKEY,PF7           TEST PF7=BACKWARD SCROLL                     
         BNE   *+12                NO                                           
         BAS   RE,BACK                                                          
         B     CHK10                                                            
*                                                                               
         BAS   RE,FORWARD                                                       
*                                                                               
CHK10    MVI   LNLISTS,0                                                        
         MVI   LSWAP,0                                                          
         LA    RE,LSELTAB                                                       
         LA    RF,MAXRECS*SELTABL                                               
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
CHK11    BAS   RE,LIST                                                          
         BAS   RE,DISTOT                                                        
         L     R2,AFSTSEL                                                       
         CLI   LNLISTS,MAXRECS     TEST IF SCREEN FILLED                        
         BE    CHK12               YES                                          
         LA    R2,LISPROH          PUT CURSOR AT FIRST KEY FIELD                
         MVC   CONHEAD(L'LIST2MSG),LIST2MSG                                     
         B     CHK14                                                            
*                                                                               
CHK12    MVC   CONHEAD(L'LISTMSG),LISTMSG                                       
*                                                                               
CHK14    ST    R2,ACURFORC                                                      
         BAS   RE,SAVTSAR          SAVE THE TSAR BLOCK                          
         B     XIT                                                              
*                                                                               
* EDIT LOGIC                                                                    
*                                                                               
CHK20    CLI   RETURNED,X'13'      TEST RETURNED FROM CHECK ESTIMATE            
         BE    *+12                YES                                          
         CLI   RETURNED,X'14'      TEST BACK FROM ESTIMATE ADJUST               
         BNE   *+8                                                              
         BAS   RE,DISSWAP          RE-DISPLAY SWAPPED LINE                      
*                                                                               
         MVI   LSWAP,0                                                          
         BAS   RE,EDT                                                           
         BAS   RE,DISTOT                                                        
         L     R2,AFSTSEL                                                       
         ST    R2,ACURFORC                                                      
         MVC   CONHEAD(L'EDTMSG),EDTMSG                                         
         BAS   RE,SAVTSAR          SAVE THE TSAR BLOCK                          
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE THE HEADLINE FIELDS                                   
*                                                                               
VALHED   NTR1                                                                   
         MVI   KEYCHG,C'N'         INITIALIZE KEY FIELD CHANGE SWITCH           
         MVI   REVERSE,C'N'                                                     
         LA    R2,LISPROH                                                       
         BAS   RE,TSTKEY                                                        
         GOTO1 AVALPRD                                                          
*                                                                               
VALHED2  LA    R2,LISMEDH                                                       
         BAS   RE,TSTKEY                                                        
         GOTO1 AVALMED                                                          
*                                                                               
VALHED4  LA    R2,LISESTH                                                       
         BAS   RE,TSTKEY                                                        
         GOTO1 AVALEST                                                          
*                                                                               
         CLI   KEYCHG,C'Y'         TEST IF FILTER FIELDS CHANGED                
         BNE   *+8                                                              
         MVI   FILTSW,C'Y'         LET BASE WRITE BACK SAVEVALS                 
*                                                                               
VALHED6  MVI   SCROLL,0                                                         
         LA    R2,LISSCRH          EDIT SCROLL AMOUNT                           
         CLI   5(R2),0                                                          
         BE    VALHED8             NO INPUT                                     
         TM    4(R2),X'08'         TEST FOR NUMERIC FIELD                       
         BO    VALHED7             YES                                          
*                                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ANY                                                              
         MVI   SCROLL,MAXRECS                                                   
         CLC   =C'PAGE',WORK       TEST FOR 'PAGE' INPUT                        
         BE    VALHED8             YES                                          
         MVI   SCROLL,MAXRECS/2                                                 
         CLC   =C'HALF',WORK                                                    
         BE    VALHED8                                                          
         B     ERREND                                                           
*                                                                               
VALHED7  GOTO1 VALINUM                                                          
         MVI   ERROR,INVALID                                                    
         CLI   ACTUAL,MAXRECS      TEST INPUT OF MORE THAN 1 PAGE               
         BH    ERREND              STOP IT                                      
         MVC   SCROLL,ACTUAL                                                    
*                                                                               
VALHED8  LA    R2,LISOPTH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED10                                                         
*                                                                               
         MVI   ERROR,INVALID                                                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'REVERSE'                                              
         BNE   ERREND                                                           
         MVI   REVERSE,C'Y'                                                     
*                                                                               
VALHED10 OI    LISPROH+4,X'20'     SET ON PREV VALID BITS                       
         OI    LISMEDH+4,X'20'                                                  
         OI    LISESTH+4,X'20'                                                  
         OI    LISOPTH+4,X'20'                                                  
*                                                                               
VALHEDX  B     XIT                                                              
         SPACE 2                                                                
TSTKEY   TM    4(R2),X'20'                                                      
         BOR   RE                                                               
         MVI   KEYCHG,C'Y'                                                      
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO SET HEADLINE FIELDS WHEN CALLED FROM HEADER TO BEGIN           
* A LIST                                                                        
*                                                                               
SETHED   NTR1  ,                                                                
*                                                                               
         MVC   LISRCV,RECEIVE+3    DISPLAY RECEIVABLE A/C                       
         OI    LISRCVH+6,X'80'                                                  
*                                                                               
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(L'SVCLIFLD),SVCLIFLD    DISPLAY CLI FLD (HEADER)          
         LA    R2,LISCLIH                                                       
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
SETHED1  LA    R2,LISPERH          DISPLAY ADVERTISING PERIOD                   
         MVC   LISTAR,SPACES                                                    
         MVC   FULL(2),ADVST                                                    
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(1,FULL),(9,LISTAR)                                  
         CLC   ADVST,ADVEND        TEST SINGLE MONTH                            
         BE    SETHED2                                                          
*                                                                               
         MVI   LISTAR+6,C'-'                                                    
         MVC   FULL(2),ADVEND      DISPLAY PERIOD END                           
         GOTO1 DATCON,DMCB,(1,FULL),(9,LISTAR+7)                                
*                                                                               
SETHED2  BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
SETHED3  MVC   LISTAR,SPACES                                                    
         LA    R2,LISPROH          PRODUCTS FIRST                               
         MVC   LISTAR(L'PRODCHAR),PRODCHAR                                      
         OC    LISTAR,SPACES                                                    
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'         XMIT                                         
         OI    4(R2),X'20'         NOTE AS PREVIOUSLY VALID                     
*                                                                               
         LA    R2,LISMEDH                                                       
         MVC   LISTAR(L'MEDCHAR),MEDCHAR                                        
         OC    LISTAR,SPACES                                                    
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,LISESTH                                                       
         MVC   LISTAR(L'ESTCHAR),ESTCHAR                                        
         OC    LISTAR,SPACES                                                    
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'                                                      
*                                                                               
         OI    LISOPTH+4,X'20'     MARK OPTIONS AS PREV VALID                   
         OI    LISOPTH+6,X'80'                                                  
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO PROCESS PF KEYS                                                
*                                                                               
PROCPF   NTR1                                                                   
         L     R2,AFSTSEL          R2=A(SELECT FIELD)                           
         LA    R5,LSELTAB          R5=A(SELECT FIELD TABLE)                     
         USING SELTABD,R5                                                       
*                                                                               
PROCPF1  CLI   PFKEY,PF2           TEST PF2=DRAFT                               
         BE    PROCPF2                                                          
         CLI   PFKEY,PF3           TEST PF3=FILTER                              
         BE    PROCPF2                                                          
         CLI   PFKEY,PF4           TEST FOR PF4=CHECK REVIEW                    
         BE    PROCPF2             YES                                          
         CLI   PFKEY,PF6           TEST FOR PF6=UPDATE                          
         BE    PROCPF2                                                          
         CLI   PFKEY,PF10          TEST FOR PF10=QUIT                           
         BNE   PROCPFX                                                          
*                                                                               
PROCPF2  L     RE,ATWA                                                          
         AH    RE,MODLAST                                                       
         LA    R1,CONACTH                                                       
         CR    RE,R1               TEST FOR ANY FIELD MODIFIED                  
         BH    PROCPFX             AFTER ACTION                                 
*                                                                               
         CLI   PFKEY,PF2                                                        
         BE    PROCPF3                                                          
         CLI   PFKEY,PF3                                                        
         BE    PROCPF4                                                          
         CLI   PFKEY,PF10                                                       
         BE    PROCPF5                                                          
         CLI   PFKEY,PF6                                                        
         BE    PROCPF6                                                          
*                                                                               
         MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'CHECK',=C'REVIEW',=C',',0                          
*                                                                               
PROCPF3  MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'CHECK',=C'DRAFT',=C',',0                           
*                                                                               
PROCPF4  MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'CHECK',=C'FILTER',=C',',0                          
*                                                                               
PROCPF5  MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'CHECK',=C'QUIT',=C',',0                            
*                                                                               
PROCPF6  CP    TOTPAID,CHKAMT      TEST TOTAL PAID=CHECK AMOUNT                 
         BE    PROCPF7                                                          
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R4,LISTAR                                                        
         MVC   0(L'TOTPCON,R4),TOTPCON                                          
         LA    R4,L'TOTPCON+1(R4)                                               
         CURED TOTPAID,(12,(R4)),2,ALIGN=LEFT,MINUS=YES                         
         AR    R4,R0                                                            
         LA    R4,1(R4)                                                         
         MVC   0(L'NEQCON,R4),NEQCON                                            
         LA    R4,L'NEQCON+1(R4)                                                
         MVC   0(L'CHKCON,R4),CHKCON                                            
         LA    R4,L'CHKCON+1(R4)                                                
         CURED (P8,CHKAMT),(12,(R4)),2,ALIGN=LEFT,MINUS=YES                     
         LA    R2,CONHEADH                                                      
         BAS   RE,MOVEFLD                                                       
         MVI   ERROR,X'FE'         SPECIAL ERROR MESSAGE                        
         LA    R2,LISSEL1H                                                      
         B     ERREND                                                           
*                                                                               
PROCPF7  MVI   PFKEY,0                                                          
         GOTO1 VCALL,WORK,=C'CHECK',=C'UPDATE',=C',',0                          
*                                                                               
PROCPFX  B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* SUB-ROUTINE TO TEST FOR ANYTHING TO EDIT ON SCREEN                            
*                                                                               
* ON EXIT, CC=EQ TO EDIT, CC=NEQ TO CONTINUE DISPLAY                            
*                                                                               
TSTEDT   NTR1  ,                                                                
         L     R2,AFSTSEL          R2=A(FIRST SELECT FIELD)                     
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
* SUB-ROUTINE TO PERFORM FORWARD SCROLLING                                      
*                                                                               
* ON EXIT, FSTKEY AND FSTRNUM CONTAIN START POINT FOR LIST                      
*                                                                               
FORWARD  NTR1  ,                                                                
         SR    R3,R3               R3=N'ITEMS ON LAST SCREEN                    
         ICM   R3,1,LNLISTS                                                     
         BZ    FORWARD8                                                         
*                                                                               
FORWARD2 SR    R2,R2                                                            
         ICM   R2,1,SCROLL                                                      
         BNZ   *+8                                                              
         LA    R2,MAXRECS                                                       
         LA    R5,LSELTAB                                                       
         USING SELTABD,R5                                                       
         CR    R2,R3               TEST SCROLL AMT VS. N'LIST ITEMS             
         BE    FORWARD4                                                         
         BH    FORWARD6            SCROLL MORE THAN ITEMS ON SCREEN             
*                                                                               
         MH    R2,=Y(SELTABL)      USE SCROLL AS INDEX INTO TABLE               
         LA    R5,0(R2,R5)                                                      
         MVC   FSTKEY,SELKEY                                                    
         MVC   FSTRNUM,SELRNUM                                                  
         B     FORWARDX                                                         
*                                                                               
FORWARD4 BCTR  R3,0                GET LAST KEY IN TABLE                        
         MH    R3,=Y(SELTABL)                                                   
         LA    R5,0(R3,R5)                                                      
         MVC   FSTKEY,SELKEY                                                    
         MVC   FSTRNUM,SELRNUM                                                  
         CLI   LNLISTS,MAXRECS     TEST IF LAST SCREEN FILLED                   
         BL    FORWARDX            NO-AT END OF FILE                            
*                                                                               
FORWARD5 LH    R4,FSTRNUM          YES-BUMP AHEAD TO NEXT RECORD                
         LA    R4,1(R4)                                                         
         CH    R4,TSARNUM          TEST IF PAST EOF                             
         BH    FORWARDX            YES-ALL DONE                                 
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAGET                                                    
         STCM  R4,3,TSRNUM                                                      
         GOTO1 TSAR                                                             
         BNE   FORWARDX                                                         
         MVC   FSTKEY,TSARKEY      SET START KEY/NUMBER                         
         MVC   FSTRNUM,TSRNUM                                                   
         B     FORWARDX                                                         
*                                                                               
FORWARD6 CLI   SCROLL,0            TEST USER INPUT SCROLL AMT                   
         BE    FORWARD8            NO-START LIST AGAIN                          
*                                                                               
         BCTR  R3,0                GET LAST RECORD ON SCREEN                    
         MH    R3,=Y(SELTABL)      ASSUME USER OVERSHOT THE END                 
         LA    R5,0(R3,R5)                                                      
         MVC   FSTKEY,SELKEY                                                    
         MVC   FSTRNUM,SELRNUM                                                  
         B     FORWARDX                                                         
*                                                                               
FORWARD8 MVC   FSTRNUM,=H'1'       START LIST AT BEGINNING                      
         XC    FSTKEY,FSTKEY                                                    
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAGET                                                    
         MVC   TSRNUM,FSTRNUM                                                   
         GOTO1 TSAR                                                             
         BNE   *+10                                                             
         MVC   FSTKEY,TSARKEY                                                   
*                                                                               
FORWARDX B     XIT                                                              
         DROP  R1,R5                                                            
         EJECT                                                                  
* SUB-ROUTINE TO PERFORM BACKWARDS SCROLLING                                    
*                                                                               
* ON EXIT, FSTKEY AND FSTRNUM = FIRST RECORD'S KEY AND NUMBER                   
*                                                                               
BACK     NTR1  ,                                                                
         LA    R5,LSELTAB                                                       
         USING SELTABD,R5                                                       
         MVC   FSTKEY,SELKEY       EXTRACT FIRST KEY                            
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSARDH                                                    
         MVC   TSARKEY(TSARKEYL),SELKEY                                         
         GOTO1 TSAR                                                             
         BNE   BACKX               THERE IS NOTHING TO DISPLAY                  
*                                                                               
BACK2    SR    R2,R2                                                            
         ICM   R2,3,TSRNUM         R2=FIRST RECORD'S NUMBER                     
         STCM  R2,3,FSTRNUM                                                     
         SH    R2,=H'1'                                                         
         BZ    BACKX               ALREADY AT START OF FILE                     
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,1,SCROLL                                                      
         BNZ   *+8                 GET SCROLL AMOUNT                            
         LA    R3,MAXRECS          DEFAULT TO SCREEN SIZE                       
*                                                                               
BACK4    L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAGET                                                    
         STH   R2,TSRNUM                                                        
         GOTO1 TSAR                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AFILTER                                                          
         BNE   BACK8               REJECT THIS RECORD                           
*                                                                               
BACK6    MVC   FSTKEY,TSARKEY      EXTRACT KEY                                  
         MVC   FSTRNUM,TSRNUM      AND RECORD NUMBER                            
         BCT   R3,BACK8                                                         
         B     BACKX               SCROLL AMOUNT DONE                           
*                                                                               
BACK8    BCT   R2,BACK4                                                         
*                                                                               
BACKX    B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
* SUB-ROUTINE TO LIST ESTIMATES AND THEIR PAYABLE AMOUNTS                       
*                                                                               
* AT ENTRY, LNLISTS CONTAINS N'LIST LINES ALREADY ON SCREEN                     
*           AND LLASTCLI CONTAINS LAST KEY READ OR BINARY ZERO                  
*                                                                               
LIST     NTR1  ,                                                                
         L     R2,AFSTSEL          INITIALIZE SCREEN LINE POINTER               
         ST    R2,ATHISLIN                                                      
         LH    R3,FSTRNUM          R3=RECORD NUMBER TO READ                     
         LA    R5,LSELTAB                                                       
         USING SELTABD,R5                                                       
*                                                                               
LIST2    L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAGET                                                    
         STH   R3,TSRNUM                                                        
         GOTO1 TSAR                                                             
         BNE   LIST6                                                            
         GOTO1 AFILTER                                                          
         BNE   LIST6                                                            
*                                                                               
LIST4    MVI   SELACT,C' '                                                      
         MVC   SELKEY,TSARKEY                                                   
         MVC   SELRNUM,TSRNUM                                                   
         LA    R4,KEY              GET THE ESTIMATE RECORD                      
         USING ACINKEY,R4                                                       
         MVC   ACINKEY,SPACES                                                   
         MVI   ACINCOD,ACINEQU                                                  
         MVI   ACINSREC,ACINSEQU                                                
         MVC   ACINCUL(L'RECEIVE),RECEIVE                                       
         MVC   ACINCLT,TSARCLT     CLIENT CODE                                  
         MVC   ACINPRD,TSARPROD    PRODUCT  CODE                                
         MVC   ACINMED,TSARMED     MEDIA                                        
         MVC   ACINEST,TSAREST     ESTIMATE                                     
         GOTO1 READ                                                             
*                                                                               
LIST5    L     R2,ATHISLIN                                                      
         BAS   RE,SETLIN                                                        
         BAS   RE,DISEST                                                        
         MVC   ATHISLIN,ANEXTSEL   RESET LINE POINTER TO NEXT ONE               
         ZIC   R1,LNLISTS                                                       
         LA    R1,1(R1)            INCREMENT COUNT OF ITEMS ON SCREEN           
         STC   R1,LNLISTS                                                       
         LA    R5,SELTABL(R5)      NEXT TABLE ENTRY                             
         CLI   LNLISTS,MAXRECS     TEST SCREEN FILLED                           
         BE    LISTX               YES                                          
*                                                                               
LIST6    LA    R3,1(R3)            INCREMENT RECORD NUMBER                      
         CH    R3,TSARNUM          TEST PAST EOF                                
         BNH   LIST2                                                            
*                                                                               
LISTX    B     XIT                                                              
*                                                                               
         DROP  R1,R4,R5                                                         
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY LIST LINE DATA FOR AN ESTIMATE                         
*                                                                               
* ON ENTRY, TSARREC CONTAINS TSAR RECORD AND AIO=A(ESTIMATE)                    
*                                                                               
DISEST   NTR1  ,                                                                
         L     R4,AIO                                                           
         USING ACINKEY,R4                                                       
         L     R2,ASEL                                                          
         OI    4(R2),X'20'                                                      
         ZAP   GROSS,=P'0'                                                      
         ZAP   RECV,=P'0'                                                       
         ZAP   POSTED,=P'0'                                                     
         ZAP   PAID,=P'0'                                                       
         ZAP   ALLOC,=P'0'                                                      
*                                                                               
DISEST1  ZIC   R0,ADVNUM           R0=LOOP COUNTER                              
         LA    R1,TSARPAID         R1=A(PAID BUCKETS)                           
         AP    ALLOC,0(L'TSARPAID,R1) UPDATE ALLOCATED THIS TIME                
         LA    R1,L'TSARPAID(R1)                                                
         BCT   R0,*-10                                                          
*                                                                               
DISEST2  L     R2,ADATA                                                         
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR                                                        
         USING LSTPROTD,R3                                                      
         MVI   ELCODE,ACIESEQU     GET ESTIMATE ELEMENTS                        
         BAS   RE,GETELIO                                                       
*                                                                               
DISEST4  BNE   DISEST6                                                          
         USING ACINESTD,R6                                                      
         CLC   ACIESMTH,ADVST      TEST MONTH WITHIN PERIOD                     
         BL    DISEST5             NO                                           
         CLC   ACIESMTH,ADVEND                                                  
         BH    DISEST5                                                          
         AP    GROSS,ACIESGRS                                                   
         AP    RECV,ACIESREC                                                    
         TM    ACIESTAT,X'80'      TEST IF MONTH POSTED                         
         BZ    *+10                                                             
         AP    POSTED,ACIESREC     YES                                          
         AP    PAID,ACIESPD                                                     
*                                                                               
DISEST5  BAS   RE,NEXTEL                                                        
         B     DISEST4                                                          
*                                                                               
DISEST6  MVC   LSTCLT,ACINCLT      CLIENT CODE                                  
         MVC   LSTPRD,ACINPRD      PRODUCT CODE                                 
         MVC   LSTMED(2),ACINMED   MEDIA                                        
         MVC   LSTEST,ACINEST      ESTIMATE                                     
         USING ACKEYD,R4                                                        
         TM    ACSTATUS,X'02'      TEST FOR MI MEDIA                            
         BZ    *+16                                                             
         MVC   LSTMED+3(2),LSTMED                                               
         MVC   LSTMED(3),=C'MI='                                                
*                                                                               
         CURED GROSS,(L'LSTGROSS,LSTGROSS),2,ZERO=NOBLANK,FLOAT=-               
         CURED RECV,(L'LSTRECV,LSTRECV),2,ZERO=NOBLANK,FLOAT=-                  
         CURED POSTED,(L'LSTPOST,LSTPOST),2,ZERO=NOBLANK,FLOAT=-                
         AP    PAID,ALLOC          INCLUDE ALLOCATIONS IN PAID                  
         CURED PAID,(L'LSTPAID,LSTPAID),2,ZERO=NOBLANK,FLOAT=-                  
         L     R2,ADATA                                                         
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'         SHIP IT BACK                                 
*                                                                               
DISEST8  L     R2,AOPEN                                                         
         OI    6(R2),X'80'         XMIT BACK                                    
         ZAP   OPEN,RECV                                                        
         SP    OPEN,PAID                                                        
         MVC   LISTAR,SPACES                                                    
         MVI   LISTAR,C'*'                                                      
         CURED OPEN,(10,LISTAR+1),2,ZERO=NOBLANK,ALIGN=LEFT,FLOAT=-             
         OI    4(R2),X'20'         NOTE OPEN AS PREV VALID                      
         CLI   REVERSE,C'Y'        TEST REVERSE OPTION                          
         BNE   DISEST10                                                         
*                                                                               
DISEST9  CLI   INTMODE,EDTLIST     TEST RE-DISPLAYING ITEM                      
         BE    *+8                 YES                                          
         NI    4(R2),X'FF'-X'20'   TURN OFF PREV VALID                          
         CP    ALLOC,=P'0'         TEST IF ANYTHING PAID                        
         BNE   *+14                YES-DEFAULT IS NO DOUBLE PAY                 
         CP    OPEN,=P'0'          TEST FOR AN OPEN AMOUNT                      
         BNE   DISEST10            YES-WE CAN PAY IT AGAIN                      
         MVI   LISTAR,C'N'                                                      
         OI    4(R2),X'20'         SET VALID ON                                 
*                                                                               
DISEST10 BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'         XMIT BACK                                    
*                                                                               
DISESTX  B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
* SUB-ROUTINE TO RE-DISPLAY A SWAPPED LINE                                      
*                                                                               
DISSWAP  NTR1  ,                                                                
         ZIC   R3,LSWAP                                                         
         BCTR  R3,0                                                             
         MH    R3,=Y(SELTABL)                                                   
         LA    R5,LSELTAB(R3)      INDEX TO SWAP LINE IN TABLE                  
         USING SELTABD,R5                                                       
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVC   TSARKEY(TSARKEYL),SELKEY                                         
         MVI   TSACTN,TSARDH                                                    
         GOTO1 TSAR                                                             
         BE    *+6                                                              
         DC    H'0'                CAN'T FIND IT                                
*                                                                               
DISSWAP2 LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   ACINKEY,SPACES                                                   
         MVI   ACINCOD,ACINEQU                                                  
         MVI   ACINSREC,ACINSEQU                                                
         MVC   ACINCUL(L'RECEIVE),RECEIVE                                       
         MVC   ACINCLT,TSARCLT     CLIENT CODE                                  
         MVC   ACINPRD,TSARPROD    PRODUCT CODE                                 
         MVC   ACINMED,TSARMED     MEDIA CODE                                   
         MVC   ACINEST,TSAREST     ESTIMATE                                     
*                                                                               
         GOTO1 HIGH                                                             
         CLC   ACINKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE IF NOT FOUND                     
*                                                                               
DISSWAP4 ZIC   R3,LSWAP                                                         
         L     R2,AFSTSEL                                                       
         ST    R2,ATHISLIN                                                      
*                                                                               
DISSWAP6 L     R2,ATHISLIN                                                      
         BAS   RE,SETLIN                                                        
         MVC   ATHISLIN,ANEXTSEL   SET NEXT LINE POINTER                        
         BCT   R3,DISSWAP6                                                      
*                                                                               
         BAS   RE,DISEST           DISPLAY THIS ESTIMATE                        
         BAS   RE,DISTOT           RE-DO TOTALS LINE                            
*                                                                               
DISSWAPX B     XIT                                                              
         DROP  R1,R4,R5                                                         
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY TOTALS LINE                                            
*                                                                               
DISTOT   ST    RE,SAVERE                                                        
         MVC   LISTOT,SPACES                                                    
         OI    LISTOTH+6,X'80'                                                  
         MVC   LISTOT(L'CHKCON),CHKCON                                          
         LA    R4,LISTOT+L'CHKCON+1                                             
         CURED CHKAMT,(12,(R4)),2,ALIGN=LEFT,FLOAT=-                            
         AR    R4,R0                                                            
         LA    R4,2(R4)                                                         
         MVC   0(L'PAIDCON,R4),PAIDCON                                          
         LA    R4,L'PAIDCON+1(R4)                                               
         CURED TOTPAID,(12,(R4)),2,ALIGN=LEFT,FLOAT=-                           
         AR    R4,R0               UPDATE POINTER                               
         LA    R4,2(R4)                                                         
         MVC   0(L'BALCON,R4),BALCON                                            
         LA    R4,L'BALCON+1(R4)                                                
         ZAP   BALANCE,CHKAMT      BALANCE=CHECK AMOUNT-TOTAL PAID              
         SP    BALANCE,TOTPAID                                                  
         CURED BALANCE,(12,(R4)),2,ALIGN=LEFT,FLOAT=-                           
         L     RE,SAVERE                                                        
         BR    RE                                                               
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
*                                                                               
MOVEFLDX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT THE LIST SCREEN                                           
*                                                                               
* NOTE-GENCON DISABLES READ FOR UPDATE FOR ACTION LIST (ACTNUM=ACTLIST)         
*                                                                               
EDT      NTR1  ,                                                                
         L     R2,AFSTSEL          R2=A(SELECT FIELD)                           
         ZIC   R3,LNLISTS                                                       
         LA    R5,LSELTAB                                                       
         USING SELTABD,R5                                                       
*                                                                               
EDT2     BAS   RE,SETLIN                                                        
         L     R2,ASEL             TEST TO EDIT LINE                            
         CLI   5(R2),1                                                          
         BL    *+16                NOTHING TO EDIT IN SELECT                    
         BH    EDT3                AN ERROR IS WAITING                          
         CLI   8(R2),C'*'          TEST SELECT ALREADY EDITED                   
         BNE   EDT3                NO                                           
         L     R2,AOPEN                                                         
         TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BO    EDT8                NO-NEXT LINE                                 
*                                                                               
EDT3     L     R2,ASEL                                                          
         CLI   5(R2),0             TEST ANY SELECT INPUT                        
         BE    EDT4                                                             
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   ERREND                                                           
         CLI   8(R2),C'*'          TEST ALREADY EDITED                          
         BE    EDT4                YES                                          
*                                                                               
         CLI   8(R2),C'A'          TEST 'A'=EST ADJUST                          
         BE    EDT4                                                             
         CLI   8(R2),C'E'          TEST 'E'=CHECK ESTIMATE                      
         BNE   ERREND                                                           
*                                                                               
EDT4     L     R1,ATSARBLK         READ THE TSAR RECORD                         
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAGET       GET BY RECORD NUMBER                         
         MVC   TSRNUM,SELRNUM      SET RECORD NUMBER                            
         GOTO1 TSAR                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TSARKEY(TSARKEYL),SELKEY TEST SAME KEY                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EDT5     LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   ACINKEY,SPACES                                                   
         MVI   ACINCOD,ACINEQU                                                  
         MVI   ACINSREC,ACINSEQU                                                
         MVC   ACINCUL(L'RECEIVE),RECEIVE                                       
         MVC   ACINCLT,TSARCLT     CLIENT CODE                                  
         MVC   ACINPRD,TSARPROD    PRODUCT CODE                                 
         MVC   ACINMED,TSARMED     MEDIA CODE                                   
         MVC   ACINEST,TSAREST     ESTIMATE                                     
*                                                                               
         GOTO1 HIGH                                                             
         CLC   ACINKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE IF NOT FOUND                     
         BAS   RE,EDTOP            EDIT REGULAR LIST LINE                       
         BAS   RE,DISEST           RE-DISPLAY ESTIMATE                          
*                                                                               
EDT6     CLI   5(R2),0             TEST FOR SELECT INPUT                        
         BE    EDT8                NO                                           
         CLI   8(R2),C'*'          TEST ALREADY THERE                           
         BE    EDT8                                                             
         MVC   SELACT,8(R2)        SAVE ACTION                                  
         MVI   8(R2),C'*'          MARK SELECT FIELD                            
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'         XMIT FIELD BACK                              
*                                                                               
         ZIC   RE,LNLISTS                                                       
         SR    RE,R3                                                            
         LA    RE,1(RE)            COMPUTE INDEX TO SWAPPED LINE                
         STC   RE,LSWAP            NOTE LAST SWAP LINE                          
         BAS   RE,SAVTSAR          SAVE THE BLOCK BEFORE SWAPPING               
*                                                                               
         MVI   DUB,2                                                            
         MVC   DUB+1(2),ACINMED                                                 
         TM    ACSTATUS,X'02'                                                   
         BZ    EDT6A                                                            
         MVI   DUB,5                                                            
         MVC   DUB+1(3),=C'MI='                                                 
         MVC   DUB+4(2),ACINMED                                                 
*                                                                               
EDT6A    CLI   SELACT,C'E'         TEST 'C'=CHECK ESTIMATE                      
         BE    EDT7                                                             
*                                                                               
         GOTO1 VCALL,WORK,=C'EST',=C'ADJUST',(L'ACINACC,ACINACC),(L'ACIX        
               NCLT,ACINCLT),(L'ACINPRD,ACINPRD),(DUB,DUB+1),(L'ACINESTX        
               ,ACINEST),0                                                      
*                                                                               
EDT7     GOTO1 VCALL,WORK,=C'CHECK',=C'EST',(L'ACINCLT,ACINCLT),(L'ACINX        
               PRD,ACINPRD),(DUB,DUB+1),(L'ACINEST,ACINEST),0                   
*                                                                               
EDT8     L     R2,ANEXTSEL                                                      
         LA    R5,SELTABL(R5)                                                   
         BCT   R3,EDT2                                                          
*                                                                               
EDTX     B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO EDIT OPEN AMOUNT FIELD                                         
*                                                                               
EDTOP    NTR1                                                                   
         MVI   UPDATE,C'N'         SET UPDATE SWITCH TO NO                      
         L     R2,AOPEN                                                         
         TM    4(R2),X'20'                                                      
         BO    EDTOPX                                                           
         CLI   REVERSE,C'Y'        TEST FOR REVERSE PAY OPTION                  
         BE    *+16                YES                                          
         CLI   8(R2),C'*'          TEST FIELD STARTS WITH ASTER                 
         BE    EDTOPX                                                           
         B     EDTOP1                                                           
*                                                                               
         CLI   8(R2),C'N'          TEST 'N'=NO PAYMENT                          
         BE    EDTOPX                                                           
*                                                                               
EDTOP1   BAS   RE,GETMON                                                        
*                                                                               
EDTOP2   GOTO1 ANY                                                              
         ZAP   THISPAY,=P'0'                                                    
         CLI   8(R2),C'U'          TEST U=UNMARK                                
         BNE   EDTOP4              NO                                           
*                                                                               
         LA    R3,TSARPAID                                                      
         ZIC   R0,ADVNUM                                                        
*                                                                               
EDTOP3   ZAP   DUB,0(L'TSARPAID,R3)  GET ALLOCATED AMOUNT                       
         MP    DUB,=P'-1'          COMPLEMENT IT                                
         AP    THISPAY,DUB                                                      
         ZAP   0(L'TSARPAID,R3),=P'0'                                           
         LA    R3,L'TSARPAID(R3)   NEXT MONTH                                   
         BCT   R0,EDTOP3                                                        
         B     EDTOP20                                                          
*                                                                               
EDTOP4   MVI   ERROR,INVALID                                                    
         ZIC   R0,5(R2)            GET I/P LEN                                  
         CLI   8(R2),C'Y'          TEST FIELD STARTS WITH 'Y'                   
         BE    EDTOP5              YES                                          
         CLI   REVERSE,C'Y'        TEST FOR REVERSE OPTION                      
         BNE   EDTOP6              NO                                           
         CLI   8(R2),C'*'          TEST FIELD STARTS WITH '*'                   
         BNE   EDTOP6                                                           
*                                                                               
EDTOP5   SH    R0,=H'1'                                                         
         BZ    ERREND                                                           
         MVC   WORK(L'WORK-1),WORK+1 SHIFT OUT 'Y'                              
*                                                                               
EDTOP6   GOTO1 CASHVAL,DMCB,(X'82',WORK),(R0)                                   
         CLI   0(R1),0                                                          
         BNE   ERREND                                                           
         ZAP   THISPAY,4(8,R1)     GET PAY AMOUNT                               
         CP    THISPAY,=P'0'       TEST FOR ZERO                                
         BE    ERREND              YES                                          
*                                                                               
EDTOP8   CP    THISPAY,OPEN        TEST PAYMENT=OPEN AMT                        
         BE    EDTOP10             YES                                          
         CLI   ADVNUM,1            TEST PAYING ONE MONTH ONLY                   
         BE    EDTOP9              YES-CAN ALLOW PARTIAL OR OVERPAYMENT         
         MVI   ERROR,X'FE'                                                      
         MVC   CONHEAD(L'AMTERR),AMTERR                                         
         B     ERREND                                                           
*                                                                               
EDTOP9   AP    TSARPAID,THISPAY    UPDATE PAID AMOUNT                           
         B     EDTOP20                                                          
*                                                                               
EDTOP10  LA    R1,TSARPAID         UPDATE PAID THIS TIME BUCKETS                
         LA    R5,MONTAB                                                        
         USING MONTHD,R5                                                        
         ZIC   R3,ADVNUM                                                        
*                                                                               
EDTOP11  AP    0(L'TSARPAID,R1),MONOPEN                                         
         LA    R1,L'TSARPAID(R1)                                                
         LA    R5,L'MONTAB(R5)                                                  
         BCT   R3,EDTOP11                                                       
*                                                                               
EDTOP20  MVI   UPDATE,C'Y'                                                      
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 TSAR                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         AP    TOTPAID,THISPAY     UPDATE PAID SO FAR BUCKET                    
         OI    4(R2),X'20'                                                      
*                                                                               
EDTOPX   B     XIT                                                              
         DROP  R1,R5                                                            
         EJECT                                                                  
* SUB-ROUTINE TO GET THE MONTHLY BUCKET TABLE                                   
*                                                                               
* AT ENTRY, TSARREC CONTAINS TSAR RECORD AND AIO=A(ESTIMATE)                    
*                                                                               
GETMON   NTR1  ,                                                                
         ZAP   RECV,=P'0'                                                       
         ZAP   PAID,=P'0'                                                       
         ZAP   OPEN,=P'0'                                                       
         LA    R5,MONTAB           R5=A(MONTH TABLE)                            
         USING MONTHD,R5                                                        
         ZIC   R3,ADVNUM           R3=COUNTER OF MONTHS                         
         LA    R6,TSARPAID         R6=TSAR PAID THIS TIME BUCKETS               
*                                                                               
         MVC   FULL(2),ADVST       START OF ADVERTISING PERIOD                  
*                                                                               
* BUILD MONTH TABLE                                                             
*                                                                               
GETMON2  MVC   MONMTH,FULL                                                      
         ZAP   MONRECV,=P'0'                                                    
         ZAP   MONPAID,0(L'TSARPAID,R6)                                         
         ZAP   MONOPEN,=P'0'                                                    
*                                                                               
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(1,FULL),(0,WORK)                                    
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,1                                  
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,FULL)                                  
*                                                                               
GETMON4  LA    R6,L'TSARPAID(R6)   NEXT TSAR BUCKET                             
         LA    R5,L'MONTAB(R5)     NEXT MONTH TABLE ENTRY                       
         BCT   R3,GETMON2                                                       
*                                                                               
* FIND THE MONTH ELEMENTS WHICH BELONG IN THE PAY PERIOD AND                    
* UPDATE THEIR MONTH ENTRIES                                                    
*                                                                               
GETMON6  MVI   ELCODE,ACIESEQU                                                  
         BAS   RE,GETELIO                                                       
*                                                                               
GETMON8  BNE   GETMON12            EOR                                          
         USING ACINESTD,R6                                                      
         ZIC   R3,ADVNUM           R3=LOOP COUNTER                              
         LA    R5,MONTAB                                                        
         CLC   ACIESMTH,MONMTH     FIND THE MONTH                               
         BE    GETMON9             YES                                          
         LA    R5,L'MONTAB(R5)                                                  
         BCT   R3,*-14                                                          
         B     GETMON10            NOT IN PAY PERIOD                            
*                                                                               
GETMON9  AP    MONRECV,ACIESREC    UPDATE MONTH ENTRY                           
         AP    MONPAID,ACIESPD                                                  
         AP    RECV,ACIESREC       UPDATE BUCKETS FOR ESTIMATE                  
         AP    PAID,ACIESPD                                                     
*                                                                               
GETMON10 BAS   RE,NEXTEL                                                        
         B     GETMON8                                                          
*                                                                               
GETMON12 LA    R5,MONTAB           COMPUTE OPEN AMOUNTS FOR EACH MONTH          
         ZIC   R3,ADVNUM                                                        
GETMON13 ZAP   MONOPEN,MONRECV                                                  
         SP    MONOPEN,MONPAID     COMPUTE OPEN                                 
         AP    OPEN,MONOPEN                                                     
         LA    R5,L'MONTAB(R5)                                                  
         BCT   R3,GETMON13                                                      
*                                                                               
GETMONX  B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
* SUB-ROUTINES TO SET SCREEN ADDRESSES                                          
*                                                                               
SETSCR   ST    RE,SAVERE                                                        
         LA    R2,LISSEL1H                                                      
         ST    R2,AFSTSEL                                                       
         LA    R1,MAXRECS*LISTFLDS FIELD COUNTER                                
         BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
         ST    R2,ATOTAL                                                        
         BAS   RE,BUMP                                                          
         ST    R2,APFFLD                                                        
         BAS   RE,BUMP                                                          
         CLI   0(R2),0             TEST FOR EOS                                 
         BNE   *-8                                                              
         ST    R2,AENDSCR          NOTE END OF SCREEN                           
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
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
SAVTSAR  ST    RE,SAVERE                                                        
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSASAV                                                    
         GOTO1 TSAR                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R1                                                               
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   BAS   RE,SAVTSAR          SAVE THE TSAR BLOCK                          
         GOTO1 VERRCUR                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
LISTMSG  DC    C'List Displayed - Press ENTER For next page'                    
LIST2MSG DC    C'List Displayed'                                                
EDTMSG   DC    C'Changes Completed'                                             
AMTERR   DC    C'** ERROR * You Can Only Pay the Open Amount'                   
PQMSG    DC    C'now on the Print Queue'                                        
CHKCON   DC    C'Check Amount'                                                  
PAIDCON  DC    C'Paid So Far'                                                   
BALCON   DC    C'Balance'                                                       
NEQCON   DC    C'does not equal'                                                
TOTPCON  DC    C'Total paid'                                                    
         EJECT                                                                  
**********************************************************************          
* PATCH AREA                                                         *          
**********************************************************************          
         SPACE 1                                                                
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ++INCLUDE - DSECTS ARE HIDDEN IN HERE                              *          
**********************************************************************          
         SPACE 1                                                                
*ACINTWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACINTWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE ACINT12COM                                                     
         ORG   OVERWRK                                                          
INTMODE  DS    X                   INTERNAL MODE                                
KEYCHG   DS    C                                                                
REVERSE  DS    CL1                 Y=REVERSE PAYMENT OPTION                     
SCROLL   DS    XL1                                                              
*                                                                               
FSTRNUM  DS    H                                                                
FSTKEY   DS    CL(TSARKEYL)                                                     
*                                                                               
GROSS    DS    PL6                                                              
RECV     DS    PL6                                                              
POSTED   DS    PL6                                                              
PAID     DS    PL6                                                              
ALLOC    DS    PL6                                                              
OPEN     DS    PL6                                                              
THISPAY  DS    PL6                                                              
BALANCE  DS    PL6                                                              
*                                                                               
ACURSOR  DS    A                                                                
*                                                                               
AFSTSEL  DS    A                                                                
ATOTAL   DS    A                                                                
APFFLD   DS    A                                                                
AENDSCR  DS    A                                                                
*                                                                               
ATHISLIN DS    A                                                                
ASEL     DS    A                                                                
ADATA    DS    A                                                                
AOPEN    DS    A                                                                
ANEXTSEL DS    A                                                                
*                                                                               
UPDATE   DS    C                                                                
MONTAB   DS    12CL(MONLNQ)        TABLE OF MONTHS AND THEIR OPEN AMTS          
*                                                                               
         DS    CL(L'OVERWRK-(*-OVERWRK)) SPARE                                  
         EJECT                                                                  
T619FFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACINTF5D                                                       
         SPACE 2                                                                
LSAVES   DS    0D                                                               
LNLISTS  DS    X                   N'LISTS ON SCREEN                            
LSWAP    DS    X                   INDEX TO LAST SWAPPED LINE                   
LSELTAB  DS    CL(MAXRECS*SELTABL)                                              
         DS    CL((SAVAREA-LSAVES)-(*-LSAVES))  SPARE                           
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
MAXRECS  EQU   10                  N'LIST SCREEN LINES                          
LISTFLDS EQU   3                   N'FIELDS ON LIST SCREEN LINE                 
FSTLIST  EQU   1                                                                
DISLIST  EQU   2                                                                
EDTLIST  EQU   3                                                                
         SPACE 2                                                                
* DSECT TO COVER LIST LINE PROTECTED FIELD                                      
*                                                                               
LSTPROTD DSECT                                                                  
LSTCLT   DS    CL3                 CLIENT CODE                                  
         DS    CL1                                                              
LSTPRD   DS    CL3                 PRODUCT CODE                                 
         DS    CL1                                                              
LSTMED   DS    CL5                 MEDIA                                        
         DS    CL1                                                              
LSTEST   DS    CL6                 ESTIMATE                                     
         DS    CL1                                                              
LSTGROSS DS    CL11                GROSS AMOUNT                                 
         DS    CL1                                                              
LSTRECV  DS    CL11                RECEVIABLE AMOUNT                            
         DS    CL1                                                              
LSTPOST  DS    CL9                 POSTED AMOUNT                                
         DS    CL1                                                              
LSTPAID  DS    CL9                 PAID AMOUNT                                  
         SPACE 2                                                                
* DSECT TO COVER SELECT TABLE                                                   
*                                                                               
SELTABD  DSECT                                                                  
SELACT   DS    C                   SELECT ACTION                                
SELKEY   DS    CL(TSARKEYL)        TSAR RECORD KEY                              
SELRNUM  DS    CL(L'TSRNUM)        TSAR RECORD NUMBER                           
SELTABL  EQU   *-SELTABD                                                        
         SPACE 2                                                                
* DSECT TO COVER MONTH TABLE                                                    
*                                                                               
MONTHD   DSECT                                                                  
MONMTH   DS    PL2                 MONTH                                        
MONRECV  DS    PL6                                                              
MONPAID  DS    PL6                                                              
MONOPEN  DS    PL6                                                              
MONLNQ   EQU   *-MONTHD                                                         
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015ACINT15   05/01/02'                                      
         END                                                                    
