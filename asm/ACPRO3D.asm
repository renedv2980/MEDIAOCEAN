*          DATA SET ACPRO3D    AT LEVEL 070 AS OF 07/23/13                      
*PHASE T60B3DC                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B3D - CYCLE BILLING RECORD MAINT'                            
T60B3D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B3D**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         ST    R2,RELO                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         SPACE 1                                                                
*                                                                               
         L     R2,ATWA                                                          
         AH    R2,CURDISP                                                       
         ST    R2,ACURSOR                                                       
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   CYC10                                                            
         BAS   RE,VKEY                                                          
         B     XIT                                                              
CYC10    CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* VALKEY LOGIC                                                                  
*----------------------------------------------------------------------         
*                                                                               
VKEY     NTR1                                                                   
*                                                                               
         L     RE,ATIA             USE TIA FOR JOBBER BUFFER                    
         ST    RE,ACOLTAB                                                       
         L     RF,=A(LENTIA)                                                    
         SRL   RF,1                SPLIT TIA IN HALF                            
         ST    RF,LCOLTAB                                                       
         ST    RF,LOPVTAB                                                       
         LA    RE,0(RF,RE)                                                      
         ST    RE,AOPVTAB                                                       
*                                                                               
         BAS   RE,VALHED                                                        
         BAS   RE,RDOPT            READ OPTIONS FOR JOB                         
         BAS   RE,CHKJOB           CHECK JOB AS VALID FOR THIS                  
         BAS   RE,BLDCOLS                                                       
         BAS   RE,LOOK             LOOKUP ESTIMATE                              
         BAS   RE,CHKBILLS         CHECK JOB AS BILLED                          
*                                                                               
         MVI   INTMODE,EDTLIST     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES-SET FIRST TIME LIST                      
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES                                          
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* VALREC LOGIC-DISPLAY OR EDIT                                                  
*----------------------------------------------------------------------         
*                                                                               
VREC     NTR1                                                                   
*                                                                               
         LA    RE,TABLE                                                         
         ST    RE,ATABLE                                                        
*                                                                               
         BAS   RE,RELOOK           RESET ESTIMATE VALUES                        
*                                                                               
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME LIST                     
         BNE   VREC05                                                           
*                                                                               
         MVI   INTMODE,DISLIST     CONTINUE LIST                                
*                                                                               
         BAS   RE,INITTBL                                                       
         BAS   RE,INITSCHD                                                      
         BAS   RE,CLRSCR           CLEAR DATA LINES                             
         BAS   RE,READREC                                                       
         BNE   VREC02                                                           
*                                                                               
         BAS   RE,BLDTBL                                                        
         BAS   RE,DISREC           DISPLAY RECORD DATA                          
*                                                                               
VREC02   L     R5,ATABLE                                                        
         CLI   0(R5),X'FF'         ANY TABLE DATA                               
         BE    VREC03              NO                                           
         BAS   RE,DISTBL                                                        
         B     VRECX                                                            
*                                                                               
VREC03   BAS   RE,INITSCR          SEED TABLE FOR SCREEN                        
         BAS   RE,INITTAB                                                       
         BAS   RE,BLDREC           SEED AIO                                     
         B     VRECX                                                            
*                                                                               
VREC05   EQU   *                                                                
         BAS   RE,READREC                                                       
         BE    *+8                                                              
         BAS   RE,CLRAIO           CLEAR OUT THE RESULTS OF THE HIGH            
*                                                                               
         BAS   RE,EDTSCR           EDIT FIELDS ON SCREEN                        
         BAS   RE,BLDREC           BUILD RECORD FROM TABLE                      
         BAS   RE,RECOUT           ADD OR WRITE RECORD                          
*                                                                               
         BAS   RE,PROCPF           PROCESS PF KEYS                              
*                                                                               
VREC20   BAS   RE,SETSCR           CLEAR AND PROTECT DATA FIELDS                
         BAS   RE,DISREC           DISPLAY RECORD DATA                          
*                                                                               
         BAS   RE,DISTBL                                                        
*                                                                               
         L     R5,ATABLE           ANY DATA DISPLAYED                           
         CLI   0(R5),X'FF'                                                      
         BNE   *+8                                                              
         BAS   RE,INITSCR          RE INIT SCREEN                               
*                                                                               
VRECX    MVC   CONHEAD(L'DISMSG),DISMSG                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO VALIDATE THE HEADLINE FIELDS                                   
*----------------------------------------------------------------------         
*                                                                               
VALHED   NTR1                                                                   
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR                                                          
         MVI   KEYCHG,C'N'         INITIALIZE KEY FIELD CHANGE SWITCH           
         MVI   OPTION,0            NO NAME FIELDS TO BE SHOWN                   
*                                                                               
         LA    R2,CYCCLIH                                                       
         BAS   RE,TSTKEY                                                        
         GOTO1 VALCLI                                                           
*                                                                               
VALHED2  LA    R2,CYCPROH                                                       
         BAS   RE,TSTKEY                                                        
         GOTO1 VALPROD                                                          
*                                                                               
VALHED4  LA    R2,CYCJOBH                                                       
         BAS   RE,TSTKEY                                                        
         MVI   OPTION,C'Y'         DISPLAY NAME IN FOLLOWING FIELD              
         GOTO1 VALJOB                                                           
*                                                                               
         L     R0,AIO3             SAVE JOB IN AIO3                             
         ST    R0,AJOB                                                          
         L     RE,AIO                                                           
         LH    R1,ACCORLEN(RE)                                                  
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
VALHED6  OI    CYCCLIH+4,X'20'     SET ON PREV VALID BITS                       
         OI    CYCPROH+4,X'20'                                                  
         OI    CYCJOBH+4,X'20'                                                  
*                                                                               
VALHEDX  B     XIT                                                              
         SPACE 2                                                                
TSTKEY   TM    4(R2),X'20'                                                      
         BOR   RE                                                               
         MVI   KEYCHG,C'Y'                                                      
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO READ A RECORD AND BUILD A TABLE                                
*----------------------------------------------------------------------         
*                                                                               
BLDTBL   NTR1                                                                   
*                                                                               
         USING BCYELD,R6                                                        
         MVI   ELCODE,BCYELQ                                                    
         L     R5,ATABLE                                                        
         LA    R0,TBMAX                                                         
         BAS   RE,GETELIO                                                       
         BNE   BLDTB40                                                          
         USING TABLED,R5                                                        
*                                                                               
BLDTB30  BAS   RE,MOVETB           MOVE ELEMENT TO TABLE                        
*                                                                               
         ZIC   RF,TBCOUNT                                                       
         LA    RF,1(RF)                                                         
         STC   RF,TBCOUNT                                                       
*                                                                               
         LA    R5,TBNEXT                                                        
         BAS   RE,NEXTEL                                                        
         BNE   BLDTB40                                                          
         BCT   R0,BLDTB30                                                       
         DC    H'0'                RECORD HAS MORE ELS THAN TBMAX               
*                                                                               
BLDTB40  MVI   0(R5),X'FF'         END OF TABLE                                 
*                                                                               
BLDTBX   B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO READ A RECORD                                                  
* CC IS SET ON XIT                                                              
*----------------------------------------------------------------------         
*                                                                               
READREC  NTR1                                                                   
         MVC   AIO,AIO1                                                         
         BAS   RE,BLDKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(30),KEYSAVE     DOES CYCLE RECORD EXIST                      
         BE    XIT                 YES                                          
*                                                                               
         CLI   BILLING,YES         NO ANY BILLING                               
         BNE   XIT                 NO, RETURN W/NEQ CC                          
         MVC   CONHEAD(L'BADBIL),BADBIL                                         
         LA    R2,CYCJOBH                                                       
         B     MYERROR                                                          
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO READ A TABLE AND BUILD A RECORD                                
*----------------------------------------------------------------------         
*                                                                               
BLDREC   NTR1                                                                   
         MVC   AIO,AIO1                                                         
         BAS   RE,BLDKEY                                                        
         L     R6,AIO                                                           
         USING JCBRECD,R6                                                       
         BAS   RE,CLRAIO                                                        
*                                                                               
         MVC   JCBKEY,KEY                                                       
         MVC   ACCORLEN(2,R6),DATADISP   INIT REC LENGTH                        
*                                                                               
         BAS   RE,VALAIR           VALIDATE AND WRITE AIR DATE                  
*                                                                               
         GOTO1 PERSIN              WRITE PERSON ELEMENT                         
*                                                                               
         USING TABLED,R5                                                        
         L     R5,ATABLE                                                        
         LA    R0,TBMAX                                                         
*                                                                               
         XR    R6,R6                                                            
         LA    R6,1(R6)            PRIME R6 WITH TABLE ENTRY NUMBER             
*                                                                               
BLDRE40  CLI   0(R5),X'FF'         E-O-TABLE                                    
         BE    BLDREX                                                           
         BAS   RE,MOVEELEM         MOVE TABLE TO ELEMENT                        
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R5,TBNEXT                                                        
*                                                                               
         BCT   R0,BLDRE40                                                       
         CLI   0(R5),X'FF'         E-O-TABLE                                    
         BE    BLDREX              YES, JUST MADE IT                            
*                                                                               
         DC    H'0'                RECORD HAS MORE ELS THAN TBMAX               
*                                                                               
BLDREX   EQU   *                                                                
         B     XIT                                                              
*                                                                               
*                                                                               
CLRAIO   ST    RE,SAVERE                                                        
         L     RE,AIO                                                           
         LA    RF,1000                                                          
         XCEF                                                                   
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* ADD OR WRITE THE RECORD IN AIO                                                
*----------------------------------------------------------------------         
*                                                                               
RECOUT   NTR1                                                                   
         MVI   RDUPDATE,C'Y'                                                    
         BAS   RE,BLDKEY                                                        
         MVC   AIO,AIO2            DON'T OVERWRITE YOUR RECORD                  
*                                                                               
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         USING JCBRECD,R6                                                       
         LA    R6,KEY                                                           
         MVC   AIO,AIO1                                                         
         CLC   JCBKEY,KEYSAVE                                                   
         BNE   RECOADD                                                          
         GOTO1 WRITE                                                            
         B     RECOX                                                            
*                                                                               
RECOADD  EQU   *                                                                
         BAS   RE,BLDKEY                                                        
         GOTO1 ADD                                                              
RECOX    B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO DISPLAY THE RECORD DATA                                        
*        NEEDS JOBBER DATA AND THE RECORD IN AIO                                
*----------------------------------------------------------------------         
*                                                                               
DISREC   NTR1                                                                   
         MVI   ELCODE,GDAELQ                                                    
         MVC   LISTAR,SPACES                                                    
         BAS   RE,GETELIO                                                       
         BNE   DISR20                                                           
*                                                                               
         USING GDAELD,R6                                                        
         GOTO1 DATCON,DMCB,(1,GDADATE),(6,LISTAR)                               
DISR20   LA    R2,CYCAIRH                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
         USING JBLOCKD,R6                                                       
         LA    R6,BLOCK                                                         
         MVC   LISTAR,SPACES                                                    
         OC    JBHIREV,JBHIREV     ANY REVS?                                    
         BZ    DISR25                                                           
         MVI   LISTAR,C'R'                                                      
         LA    R4,LISTAR+1                                                      
         EDIT  (B1,JBHIREV),(2,LISTAR+1),ALIGN=LEFT                             
         AR    R4,R0                                                            
         MVI   0(R4),C','                                                       
*                                                                               
         EDIT  (P6,HIGHREV),(13,2(R4)),2,ALIGN=LEFT                             
DISR25   LA    R2,CYCHRVH                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(4),=C'CE/R'                                               
         EDIT  (B1,JBCURVER),(2,LISTAR+4),ALIGN=LEFT                            
         LA    R2,CYCCURH                                                       
         BAS   RE,MOVEFLD          PRINT CURRENT REVISION                       
*                                                                               
         LA    R5,0                                                             
         USING BCYELD,R6                                                        
         MVI   ELCODE,BCYELQ                                                    
         MVC   LISTAR,SPACES                                                    
         XC    NEXTCYC,NEXTCYC                                                  
         BAS   RE,GETELIO                                                       
DISR30   BNE   DISR45                                                           
*                                                                               
         LA    R5,1(R5)                                                         
         OC    BCYBILD,BCYBILD                                                  
         BZ    DISR40                                                           
         BAS   RE,NEXTEL                                                        
         B     DISR30                                                           
*                                                                               
DISR40   EDIT  ((R5)),(2,LISTAR)                                                
         STC   R5,NEXTCYC                                                       
DISR45   LA    R2,CYCNEXH                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
         GOTO1 PERSOUT             PERSON DATE TO WORK                          
         LA    R2,CYCPERH                                                       
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(20),WORK+20                                               
         BAS   RE,MOVEFLD                                                       
*                                                                               
DISRX    B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO DISPLAY THE TABLE ON THE SCREEN                                
*        ALSO BUMPS ACCUMULATORS                                                
*----------------------------------------------------------------------         
*                                                                               
DISTBL   NTR1                                                                   
*                                                                               
         XC    ACURFORC,ACURFORC   CLEAR CURSOR POSITION                        
         XC    CUMPCT,CUMPCT                                                    
         ZAP   ESTTOT,=P'0'                                                     
         ZAP   BILLTOT,=P'0'                                                    
*                                                                               
         USING TABLED,R5                                                        
         L     R5,ATABLE                                                        
         LA    R0,SCRMAX                                                        
         LA    R2,CYCSEQ1H                                                      
         BAS   RE,SETLIN                                                        
         L     R2,ASEQ                                                          
*                                                                               
DISTB30  CLI   0(R5),X'FF'         END OF TABLE                                 
         BE    DISTBX              YES                                          
*                                                                               
         BAS   RE,TBLTOSCR         MOVE TABLE ENTRY TO SCREEN LINE              
         BAS   RE,SETATTR          SET PROPER ATTRIBUTES FOR THIS ENTRY         
*                                                                               
         LA    R5,TBNEXT                                                        
         L     R2,ANEXTLIN                                                      
         BAS   RE,SETLIN                                                        
         L     R2,ASEQ                                                          
         BCT   R0,DISTB30                                                       
         DC    H'0'                RECORD HAS MORE ELS THAN TBMAX               
*                                                                               
DISTBX   EQU   *                                                                
         BAS   RE,DISTOTS          DISPLAY TOTALS                               
*                                                                               
         OC    ACURFORC,ACURFORC   WAS THIS SET WHILE DISPLAYING                
         BNZ   DISTBXX                                                          
         LA    R2,CYCSEQ1H         PUT CURSOR AT FIRST UNPRO                    
         BAS   RE,BUMPTOUN                                                      
         ST    R2,ACURFORC                                                      
*                                                                               
DISTBXX  B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO INITIALIZE THE TABLE                                           
*----------------------------------------------------------------------         
*                                                                               
INITTBL  NTR1                                                                   
         LA    R0,TBMAX                                                         
         USING TABLED,R5                                                        
         L     R5,ATABLE                                                        
         MVI   0(R5),X'FF'                                                      
         XC    TBCOUNT,TBCOUNT                                                  
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO BUILD A CYCLE BILLING KEY    USES R6                           
*----------------------------------------------------------------------         
*                                                                               
BLDKEY   EQU   *                                                                
         USING JCBRECD,R6                                                       
         LA    R6,KEY                                                           
         XC    JCBKEY,JCBKEY                                                    
         MVI   JCBKTYP,JCBKTYPQ                                                 
         MVI   JCBKSUB,JCBKSUBQ                                                 
         MVC   JCBKCUL,CUL                                                      
         MVC   JCBKCLI,CLICODE                                                  
         MVC   JCBKPRO,PRODCODE                                                 
         MVC   JCBKJOB,JOBNUM                                                   
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO MOVE THE TABLE ENTRY AT 0(R5) TO THE SCREEN LINE               
* STARTING AT ASEQ                                                              
* THIS ROUTINE ALSO MAINTAINS THE ESTTOT ANBD BILLTOT ACCUMULATORS              
*----------------------------------------------------------------------         
*                                                                               
         USING TABLED,R5                                                        
TBLTOSCR NTR1                                                                   
         L     R2,ASEQ                                                          
         OC    TBSEQ,TBSEQ         IS SEQ DEFINED?                              
         BNZ   TBT10               YES                                          
*                                                                               
         L     R2,ADES             NO, FORCE CURSOR HERE                        
         ST    R2,ACURFORC                                                      
         B     TBTX                                                             
*                                                                               
TBT10    MVC   LISTAR,SPACES                                                    
         EDIT  (B1,TBSEQ),(2,LISTAR)                                            
         BAS   RE,MOVEFLD                                                       
*                                                                               
         L     R2,ADES                                                          
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(L'TBDESC),TBDESC                                          
         BAS   RE,MOVEFLD                                                       
*                                                                               
         L     R2,ABLM             BILLING MONTH                                
         MVC   LISTAR,SPACES                                                    
         OC    TBMON,TBMON                                                      
         BZ    TBT30                                                            
         MVC   WORK(2),TBMON       MOVE THE YM                                  
         MVI   WORK+2,1            FUDGE THE DAY                                
         GOTO1 DATCON,DMCB,(1,WORK),(6,LISTAR)                                  
*                                                                               
*        NOTE "AND AFTER" HAS BEEN NOOPED, THIS IS FOR OLD RECORDS              
*                                                                               
         TM    TBSTAT,TBSMON       IS THIS DATE AND AFTER                       
         BNO   TBT30                                                            
         MVI   LISTAR+6,C'+'                                                    
*                                                                               
TBT30    BAS   RE,MOVEFLD                                                       
         OI    4(R2),X'20'         VALIDATE                                     
*                                                                               
         L     R2,ACUM                                                          
         MVC   LISTAR,SPACES                                                    
         XR    R3,R3               R3 PASSES CUM AMOUNT TO AINC EDIT            
*                                                                               
         OC    TBCUM,TBCUM                                                      
         BZ    TBT45                                                            
*                                                                               
         CLI   TBCUM,X'FF'         HI VALS                                      
         BNE   TBT40                                                            
*                                                                               
         MVC   LISTAR(5),=C'TOTAL'                                              
         B     TBT45                                                            
*                                                                               
TBT40    XR    R3,R3                                                            
         ICM   R3,3,TBCUM                                                       
         EDIT  ((R3)),(8,LISTAR),2,ALIGN=LEFT,MINUS=YES                         
TBT45    BAS   RE,MOVEFLD                                                       
         OI    4(R2),X'20'         VALIDATE                                     
*                                                                               
TBT50    L     R2,AINC                                                          
         MVC   LISTAR,SPACES                                                    
         CLI   TBCUM,X'FF'         TOTAL                                        
         BE    TBT60                                                            
*                                                                               
TBT55    XR    R1,R1                                                            
         ICM   R1,3,CUMPCT         SUBTRACT PREV CUM FROM THIS CUM              
         SR    R3,R1                                                            
         STH   R3,HALF                                                          
         CVD   R3,MYDUB            SAVE PACKED INCREMENT                        
         EDIT  (B2,HALF),(8,LISTAR),2,ALIGN=LEFT,MINUS=YES                      
         MVC   CUMPCT,TBCUM                                                     
TBT60    BAS   RE,MOVEFLD          WRITE INCREMENT TO SCREEN                    
         OI    4(R2),X'20'         VALIDATE                                     
*                                                                               
         L     R2,ABLA                                                          
         MVC   LISTAR,SPACES                                                    
         OC    TBBILD,TBBILD       IS THIS BILLED                               
         BZ    TBT70               NO                                           
         EDIT  (P6,TBBILA),(13,LISTAR),2,MINUS=YES,ALIGN=LEFT                   
         AP    BILLTOT,TBBILA                                                   
TBT70    BAS   RE,MOVEFLD                                                       
         OI    4(R2),X'20'         VALIDATE                                     
*                                                                               
         L     R2,AESA                                                          
         MVC   LISTAR,SPACES                                                    
         CLI   TBCUM,X'FF'                                                      
         BE    TBT80                                                            
         ZAP   PL16,CUREST                                                      
         MP    PL16,MYDUB                                                       
         SRP   PL16,64-4,5                                                      
         ZAP   MYDUB(6),PL16                                                    
         EDIT  (P6,MYDUB),(13,LISTAR),2,MINUS=YES,ALIGN=LEFT                    
         AP    ESTTOT,MYDUB(6)                                                  
TBT80    BAS   RE,MOVEFLD                                                       
         OI    4(R2),X'20'         VALIDATE                                     
*                                                                               
TBTX     B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* DISPLAY TOTALS                                                                
*----------------------------------------------------------------------         
DISTOTS  NTR1                                                                   
         MVC   LISTAR,SPACES                                                    
         EDIT  (P6,BILLTOT),(13,LISTAR),2,MINUS=YES,ALIGN=LEFT                  
         LA    R2,CYCBILH                                                       
         BAS   RE,MOVEFLD                                                       
         MVC   LISTAR,SPACES                                                    
         EDIT  (P6,ESTTOT),(13,LISTAR),2,MINUS=YES,ALIGN=LEFT                   
         LA    R2,CYCESTH                                                       
         BAS   RE,MOVEFLD                                                       
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO PROCESS PF KEYS                                                
*----------------------------------------------------------------------         
*                                                                               
PROCPF   NTR1                                                                   
         LA    R3,SCRMAX                                                        
*                                                                               
         CLI   PFKEY,0                                                          
         BE    PROCPFX                                                          
*                                                                               
         LA    R2,CYCSEQ1H                                                      
*                                                                               
         CLI   PFKEY,PFKINS                                                     
         BNE   PROCPF1                                                          
         L     R5,ATABLE           IS TABLE EMPTY                               
         CLI   0(R2),X'FF'                                                      
         BE    PROCPFX             YES LET SCRINIT DO THE INS                   
*                                                                               
PROCPF1  C     R2,ACURSOR                                                       
         BH    CURERR              BAD FIELD FOR PF KEY                         
*                                                                               
         LA    R6,1                PRIME R6 WITH 1                              
PROCPF2  BAS   RE,SETLIN           GET ADDRESSES                                
         L     R2,ACURSOR          IS THE CURSOR ...                            
         C     R2,ANEXTLIN         ... WITHIN THIS LINE                         
         BL    PROCPF3             YES                                          
*                                                                               
         L     R2,ANEXTLIN         POINT TO NEXT LINE                           
         LA    R6,1(R6)            NEXT TABLE ENTRY                             
         BCT   R3,PROCPF2                                                       
*                                                                               
PROCPF3  LR    R1,R6                                                            
         CLI   PFKEY,PFKDEL                                                     
         BNE   PROCPF4                                                          
         BAS   RE,CHKSCR           ANYTHING TO DELETE HERE                      
         BNE   PROCPFX             NO, LEAVE                                    
         BAS   RE,DELETE                                                        
         BAS   RE,BLDREC           BUILD RECORD FROM TABLE                      
         BAS   RE,RECOUT           WRITE IMMEDIATLY AFTER DELETE                
*                                                                               
PROCPF4  CLI   PFKEY,PFKINS                                                     
         BNE   *+8                                                              
         BAS   RE,INSERT                                                        
*                                                                               
PROCPFX  MVI   PFKEY,0                                                          
*                                                                               
         B     XIT                                                              
*                                                                               
CURERR   MVC   CONHEAD(L'BADPFK),BADPFK                                         
         LA    R2,ACURSOR                                                       
         B     MYERROR                                                          
         EJECT                                                                  
*----------------------------------------------------------------------         
* INSERT: BUMP TABLE                                                            
*        R1 IS THE ENTRY TO INSERT AFTER                                        
*----------------------------------------------------------------------         
*                                                                               
INSERT   NTR1                                                                   
         CLI   TBCOUNT,TBMAX       ROOM TO INSERT                               
         BNL   FULLERR             NO                                           
*                                                                               
         USING TABLED,R5                                                        
         L     R5,ATABLE                                                        
         LR    R3,R1                                                            
         MH    R3,=Y(TBLEN)        MAKE R3 AN OFFSET INTO TABLE                 
         LA    R3,0(R3,R5)         R3 IS A(NEW TABLE ENTRY)                     
         LA    R0,TBMAX            FIND END OF TABLE                            
*                                                                               
         CLI   0(R3),X'FF'         IS THERE A TABLE ENTRY?                      
         BE    INS20               NO                                           
*                                                                               
         OC    TBBILD-TBREC(L'TBBILD,R3),TBBILD-TBREC(R3) IS NEXT BILD          
         BNZ   INSERR                                  YES, CAN'T INS           
*                                                      BEFORE BILLED            
INS20    CLI   0(R5),X'FF'                                                      
         BE    INS40               GOT IT, PUSH IT DOWN                         
         LA    R5,TBNEXT                                                        
         BCT   R0,INS20                                                         
         DC    H'0'                END OF TABLE MISSING                         
*                                                                               
INS40    MVC   TBNEXT(TBLEN),0(R5)   SHIFT TABLE ENTRY                          
         SH    R5,=Y(TBLEN)                                                     
         CR    R5,R3               UNTIL I REACH THE START                      
         BNL   INS40                                                            
*                                                                               
         MVC   0(TBLEN,R3),BLNKTB                                               
*                                                                               
         IC    R6,TBCOUNT          BUMP TABLE COUNTER                           
         LA    R6,1(R6)                                                         
         STC   R6,TBCOUNT                                                       
*                                                                               
INSX     B     XIT                                                              
*                                                                               
INSERR   MVC   CONHEAD(L'BADINS),BADINS                                         
         LA    R2,CYCSEQ1H                                                      
         B     MYERROR                                                          
*                                                                               
FULLERR  MVC   CONHEAD(L'FULINS),FULINS                                         
         LA    R2,CYCSEQ1H                                                      
         B     MYERROR                                                          
         EJECT                                                                  
*----------------------------------------------------------------------         
* DELETE: DELETE A TABLE ENTRY                                                  
*        R1 IS THE ENTRY TO DELETE                                              
*----------------------------------------------------------------------         
*                                                                               
DELETE   NTR1                                                                   
         CLI   TBCOUNT,0           ANYTHING TO DELETE                           
         BE    DELERR              NO                                           
*                                                                               
         LTR   R1,R1                                                            
         BZ    DELERR                                                           
         BCTR  R1,0                MAKE R1 AN INDEX                             
*                                                                               
         USING TABLED,R5                                                        
         L     R5,ATABLE                                                        
         MH    R1,=Y(TBLEN)        MAKE R1 AN OFFSET INTO TABLE                 
         LA    R5,0(R1,R5)         R5 IS A(TABLE ENTRY TO DELETE)               
*                                                                               
         OC    TBBILD,TBBILD       IS THIS BILLED?                              
         BNZ   DELERR              YES,                                         
*                                                                               
         LA    R0,TBMAX                                                         
         XR    R6,R6                                                            
DEL20    LA    R3,TBNEXT                                                        
         MVC   TBREC(TBLEN),0(R3)  SUCK TABLE IN                                
*                                                                               
         CLI   0(R5),X'FF'         DID I MOVE END OF TABLE                      
         BE    DELX                GOT IT, I'M DONE                             
*                                                                               
         LA    R5,TBNEXT                                                        
*                                                                               
         BCT   R0,DEL20                                                         
         DC    H'0'                END OF TABLE MISSING                         
*                                                                               
DELX     ZIC   R6,TBCOUNT          DECREMENT COUNTER                            
         BCTR  R6,0                                                             
         STC   R6,TBCOUNT                                                       
*                                                                               
         B     XIT                                                              
*                                                                               
DELERR   MVC   CONHEAD(L'BADDEL),BADDEL                                         
         LA    R2,CYCSEQ1H                                                      
         B     MYERROR                                                          
         EJECT                                                                  
*----------------------------------------------------------------------         
* EDIT THE FIELDS ON THE SCREEN AND REBUILD THE TABLE DATA                      
*----------------------------------------------------------------------         
*                                                                               
EDTSCR   NTR1                                                                   
*                                                                               
         MVI   SEQSAVE,1                                                        
         XC    PREVMON,PREVMON                                                  
         XC    TOTSTAT,TOTSTAT                                                  
         XC    TBCOUNT,TBCOUNT                                                  
         XC    CUMPCT,CUMPCT                                                    
*                                                                               
         LA    R2,CYCSEQ1H                                                      
         L     R5,ATABLE                                                        
         LA    R4,SCRMAX                                                        
*                                                                               
EDTS20   BAS   RE,SETLIN           SET FIELD ADCONS                             
*                                                                               
         BAS   RE,CHKSCR           ANYTHING ON THIS SCREEN LINE                 
         BNE   EDTS70              NO, LEAVE TABLE BE                           
*                                                                               
         BAS   RE,FILLTBL          FILL TABLE DATA FROM RECORD                  
*                                                                               
         L     R2,ADES                                                          
         TM    1(R2),FATBPROT      PROTECTED FIELDS HERE                        
         BO    EDTS50              YES, DON'T EDIT                              
*                                                                               
         TM    TOTSTAT,TOTSPLUS    HAVE I EDITED A DATE PLUS                    
         BO    PLUSERR             NO, CONTINUE                                 
*                                                                               
         MVC   TBDESC,SPACES       GET SCREEN DATA                              
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    EDTS30                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TBDESC(0),8(R2)                                                  
*                                                                               
EDTS30   L     R2,ABLM                                                          
         MVI   ERROR,MISSING                                                    
         ICM   R1,1,5(R2)                                                       
         BZ    ERREND                                                           
         MVI   ERROR,INVDATE                                                    
*                                                                               
         NI    TBSTAT,X'FF'-TBSMON                                              
         LA    R3,8(R2)                                                         
*                                                                               
EDTS31   CLI   0(R3),C'+'        SCAN FOR A +                                   
         BNE   EDTS32                                                           
*        OI    TBSTAT,TBSMON                                                    
         MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
*                                                                               
EDTS32   LA    R3,1(R3)                                                         
         BCT   R1,EDTS31                                                        
*                                                                               
EDTS35   GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         OC    DMCB,DMCB                                                        
         BZ    ERREND                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+10)                                 
*                                                                               
         MVC   TBMON,WORK+10       SAVE THE YM                                  
         CLC   TBMON,PREVMON       THIS LESS THAN PREVOIUS                      
         BL    ORDERR              BAD DATE ORDER                               
*                                                                               
         L     R2,ACUM                                                          
         MVI   ERROR,MISSING                                                    
         XR    R3,R3                                                            
         ICM   R3,1,5(R2)                                                       
         BZ    ERREND                                                           
*                                                                               
         MVI   ERROR,INVALID                                                    
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'TOTAL'                                                
         BNE   EDTS40                                                           
         MVI   TBCUM,X'FF'                                                      
         B     EDTS50                                                           
*                                                                               
EDTS40   TM    TOTSTAT,TOTSTOT     HAVE I DONE TOTAL                            
         BO    TOTERR                                                           
*                                                                               
         TM    TBSTAT,TBSMON      IS THIS A +                                   
         BO    PLSERR              YES, ONLY FOR TOTAL                          
*                                                                               
         LA    R3,1(R3)            RESTORE FIELD LENGTH                         
         GOTO1 CASHVAL,DMCB,(X'02',8(R2)),(R3)                                  
         CLI   0(R1),X'FF'                                                      
         BE    ERREND                                                           
*                                                                               
         MVC   FULL,4(R1)          SAVE CUMULATIVE ENTERED                      
         ICM   R3,15,FULL                                                       
         BNP   ERREND                                                           
         C     R3,=F'10000'        100 PCT IS THE MAX                           
         BH    ERREND                                                           
*                                                                               
         STCM  R3,3,TBCUM          SAVE THE CUM PCT HERE                        
*                                  RESTORE PROTECTED FIELDS                     
EDTS50   ZIC   R1,SEQSAVE                                                       
         LA    R1,1(R1)                                                         
         STC   R1,SEQSAVE                                                       
*                                                                               
         IC    R1,TBCOUNT                                                       
         LA    R1,1(R1)                                                         
         STC   R1,TBCOUNT                                                       
*                                                                               
         CLI   TBCUM,X'FF'         IS THIS A TOTAL ?                            
         BNE   *+8                                                              
         OI    TOTSTAT,TOTSTOT     YES, SET TO FLAG SUBSEQUENT PCTS             
*                                                                               
         TM    TBSTAT,TBSMON       IS THIS A PLUS MONTH                         
         BNO   *+8                                                              
         OI    TOTSTAT,TOTSPLUS    YES                                          
*                                                                               
         MVC   PREVMON,TBMON       TO ASSURE MONTHS ARE ASCENDING               
*                                                                               
         LA    R5,TBNEXT                                                        
*                                                                               
EDTS70   L     R2,ANEXTLIN                                                      
         BCT   R4,EDTS20                                                        
*                                                                               
         MVI   0(R5),X'FF'         END OF TABLE                                 
EDTX     B     XIT                                                              
*                                                                               
TOTERR   MVC   CONHEAD(L'BADTOT),BADTOT                                         
         L     R2,ACUM                                                          
         B     MYERROR                                                          
*                                                                               
PLSERR   MVC   CONHEAD(L'BADPLS),BADPLS                                         
         L     R2,ABLM                                                          
         B     MYERROR                                                          
*                                                                               
PLUSERR  MVC   CONHEAD(L'NOMODATA),NOMODATA                                     
         L     R2,ABLM                                                          
         B     MYERROR                                                          
*                                                                               
ORDERR   MVC   CONHEAD(L'BADORD),BADORD                                         
         L     R2,ABLM                                                          
         B     MYERROR                                                          
         EJECT                                                                  
*----------------------------------------------------------------------         
*        SEE IF THERE IS DATA ON THE SCREEN LINE                                
*        RETURNS CC EQ IF I SHOULD WRITE THE LINE TO TABLE                      
*----------------------------------------------------------------------         
CHKSCR   NTR1                                                                   
         XC    BYTE,BYTE                                                        
         XR    R3,R3               FOR ICM IN CHKFLD                            
*                                                                               
         L     R2,ABLM                                                          
         BAS   RE,CHKFLD                                                        
*                                                                               
         L     R2,ADES                                                          
         BAS   RE,CHKFLD                                                        
*                                                                               
         L     R2,ACUM                                                          
         BAS   RE,CHKFLD                                                        
*                                                                               
CHKSX    CLI   BYTE,GOTDATA                                                     
         B     XIT                                                              
*                                                                               
CHKFLD   TM    1(R2),FATBPROT      IS FIELD PROTECTED?                          
         BO    CHKFX               YES, MUST BE FILLED                          
*                                                                               
         ICM   R3,1,5(R2)                                                       
         BZR   RE                                                               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),SPACES                                                   
         BNHR  RE                                                               
CHKFX    MVI   BYTE,GOTDATA                                                     
         BR    RE                                                               
GOTDATA  EQU   X'FF'                                                            
         EJECT                                                                  
*----------------------------------------------------------------------         
*        VALIDATE THE AIR DATE AND WRITE THE NEW ELEMENT TO AIO                 
*----------------------------------------------------------------------         
VALAIR   NTR1                                                                   
         MVI   ELCODE,GDAELQ                                                    
         GOTO1 REMELEM                                                          
         LA    R2,CYCAIRH                                                       
         XR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    VALAX                                                            
*                                                                               
         LA    R3,8(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),SPACES      ACCEPT SPACES AS I/P                         
         BNH   VALAX                                                            
*                                                                               
         MVI   ERROR,INVDATE                                                    
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         OC    DMCB,DMCB                                                        
         BZ    ERREND                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+10)                                 
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING GDAELD,R6                                                        
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVC   GDADATE,WORK+10                                                  
         GOTO1 ADDELEM                                                          
VALAX    B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*----------------------------------------------------------------------         
* FOR THE TABLE ENTRY AT 0(R5), FILL IN THE NON SCREEN DATA FROM                
*  THE RECORD IN AIO OR INITIALIZE, IF THE SEQ IS NOT IN AIO                    
*----------------------------------------------------------------------         
*                                                                               
FILLTBL  NTR1                                                                   
         L     R2,ASEQ             GET SEQUENCE NUMBER FROM SCREEN              
         XR    R1,R1                                                            
         ICM   R1,1,7(R2)                                                       
         BZ    FILLTB20            FIELD IS EMPTY                               
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STC   R1,BYTE                                                          
*                                                                               
         USING BCYELD,R6                                                        
         USING TABLED,R5                                                        
         MVI   ELCODE,BCYELQ                                                    
         BAS   RE,GETELIO                                                       
FILLTB05 BNE   FILLTB20            SEQ NOT FOUND, INITIALIZE                    
*                                                                               
         CLC   BCYCLE,BYTE                                                      
         BE    FILLTB10                                                         
         BAS   RE,NEXTEL                                                        
         B     FILLTB05                                                         
*                                                                               
FILLTB10 BAS   RE,MOVETB           MOVE ELEMENT TO TABLE                        
         B     FILLTBX                                                          
*                                                                               
FILLTB20 MVC   0(TBLEN,R5),BLNKTB                                               
*                                                                               
FILLTBX  MVC   TBSEQ,SEQSAVE                                                    
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO READ THE JOB OPTIONS                                           
*----------------------------------------------------------------------         
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
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO CHECK IF A JOB TO SET UP THESE RECORDS                         
*----------------------------------------------------------------------         
*                                                                               
CHKJOB   NTR1  ,                                                                
         CLI   GOBILTYP,C'A'                                                    
         BE    XIT                                                              
*                                                                               
         MVC   CONHEAD(L'INVJOB),INVJOB                                         
         LA    R2,CYCJOBH                                                       
         B     MYERROR                                                          
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO CHECK IF A JOB HAS ANY BILLING                                 
*----------------------------------------------------------------------         
*                                                                               
CHKBILLS NTR1  ,                                                                
         MVI   BILLING,NO                                                       
         MVC   KEY,SPACES                                                       
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(L'CLICODE),CLICODE                                         
*                                                                               
         ZIC   R1,LCLI                                                          
         LA    R1,KEY+3(R1)        POINT AT PRODUCT POSITION                    
         MVC   0(L'PRODCODE,R1),PRODCODE                                        
*                                                                               
         ZIC   R1,LCLIPRO                                                       
         LA    R1,KEY+3(R1)        POINT AT JOB                                 
         MVC   0(L'JOBNUM,R1),JOBNUM                                            
         LA    R4,KEY                                                           
         USING TRNRECD,R4                                                       
         MVC   TRNKWORK,=C'99'                                                  
         GOTO1 HIGH                                                             
         CLC   TRNKEY(TRNKWORK-TRNKEY+L'TRNKWORK),KEYSAVE ANY BILLING           
         BNE   CHKBX                                                            
         MVI   BILLING,YES                                                      
*                                                                               
CHKBX    B     XIT                                                              
         EJECT                                                                  
         CLI   GOBILTYP,C'A'                                                    
         BE    XIT                                                              
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO MOVE THE CYCLE ELEMENT AT 0(R6) TO                             
* THE TABLE ENTRY AT 0(R5)                                                      
*----------------------------------------------------------------------         
*                                                                               
MOVETB   EQU   *                                                                
         USING TABLED,R5                                                        
         USING BCYELD,R6                                                        
         MVC   TBSEQ,BCYCLE                                                     
         MVC   TBMON,BCYMON                                                     
         MVC   TBSTAT,BCYSTAT                                                   
         MVC   TBCUM,BCYPER                                                     
         MVC   TBDESC,BCYDESC                                                   
         MVC   TBREF,BCYREF                                                     
         MVC   TBBILD,BCYBILD                                                   
         MVC   TBUNBD,BCYUNBD                                                   
         ZAP   TBESTA,BCYESTA                                                   
         ZAP   TBBILA,BCYBILA                                                   
*                                                                               
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO CREATE AN ELEMENT IN "ELEMENT" FROM                            
* THE TABLE ENTRY AT 0(R5)                                                      
*----------------------------------------------------------------------         
*                                                                               
MOVEELEM EQU   *                                                                
         USING TABLED,R5                                                        
         USING BCYELD,R6                                                        
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         MVI   BCYEL,BCYELQ                                                     
         MVI   BCYLN,BCYLNQ                                                     
         MVC   BCYCLE,TBSEQ                                                     
         MVC   BCYMON,TBMON                                                     
         MVC   BCYSTAT,TBSTAT                                                   
         MVC   BCYPER,TBCUM                                                     
         MVC   BCYDESC,TBDESC                                                   
         MVC   BCYREF,TBREF                                                     
         MVC   BCYBILD,TBBILD                                                   
         MVC   BCYUNBD,TBUNBD                                                   
         MVC   BCYESTA,TBESTA                                                   
         MVC   BCYBILA,TBBILA                                                   
*                                                                               
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*----------------------------------------------------------------------         
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
MOVEFLDX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* SUB-ROUTINE TO BUILD THE JOBCOLS COLUMN LIST                                  
*----------------------------------------------------------------------         
*                                                                               
BLDCOLS  NTR1  ,                                                                
         LA    R3,COLIST                                                        
         GOTO1 VJOBCOL,DMCB,FLDH,(R3),ACOMFACS                                  
         CLI   4(R1),0             TEST FOR ERROR                               
         BNE   BLDCOLSX            NO                                           
         DC    H'0'                                                             
BLDCOLSX B     XIT                                                              
*                                                                               
FLDH     DC    AL1(8+L'FLD),4X'00',AL1(L'FLD),AL2(0)                            
FLD      DC    C'CE,HR'                                                         
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* CALL JOBBER                                                                   
*----------------------------------------------------------------------         
*                                                                               
LOOK     NTR1  ,                                                                
         USING JBLOCKD,R6                                                       
         LA    R6,BLOCK            CLEAR JOBLOCK                                
         LR    RE,R6                                                            
         LA    RF,JBLOCKL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   JBAJOB,AJOB         COMPLETE THE JOBBLOCK                        
         LA    R1,COLIST                                                        
         ST    R1,JBACOLS                                                       
         MVC   JBACOM,ACOMFACS                                                  
         LA    RE,GOBLOCK                                                       
         ST    RE,JBAGOBLK                                                      
         MVC   JBAIO,AIO1          USE IO AREA 1                                
         MVC   JBGETOPT,GETOPT                                                  
         MVC   JBSELSCH,GOSCHEME                                                
         MVC   JBACOLTB(16),ACOLTAB COLUMN/OPERAND VALUE TABLES                 
         LA    RE,FLDH                                                          
         ST    RE,JBORICLI                                                      
*                                                                               
LOOK2    GOTO1 JOBBER,DMCB,JBLOCK                                               
         CLI   JBERROR,0           TEST FOR ERROR                               
         BE    LOOKX                                                            
         CLI   JBERROR,JBERRSCH    TEST FOR SCHEME ERROR                        
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT FOR NOW                           
*                                                                               
         MVI   ERROR,BADSCH        INVALID SCHEME                               
         LA    R2,CYCJOBH                                                       
         B     ERREND                                                           
*                                                                               
LOOK4    L     R5,JBACOLTB                                                      
         CLI   JBNEWEST,JBMCSQ                                                  
         BNE   LOOK6                                                            
         USING MJETABD,R5                                                       
         ZAP   CUREST,MJETVAL(6)                                                
         ZAP   HIGHREV,MJETVAL+6(6)                                             
         B     LOOKX                                                            
*                                                                               
         USING JBCOLD,R5                                                        
LOOK6    ZAP   CUREST,JBCOLVAL(6)                                               
         ZAP   HIGHREV,JBCOLVAL+6(6)                                            
*                                                                               
LOOKX    B     XIT                                                              
         DROP  R5                                                               
*                                                                               
*        RESET FIELDS SET IN LOOK FOR VREC PROCESSING                           
*                                                                               
RELOOK   NTR1                                                                   
         USING JBLOCKD,R6                                                       
         LA    R6,BLOCK            CLEAR JOBLOCK                                
         L     R5,JBACOLTB                                                      
         CLI   JBNEWEST,JBMCSQ                                                  
         BNE   RELOOK2                                                          
*                                                                               
         USING MJETABD,R5                                                       
         ZAP   CUREST,MJETVAL(6)                                                
         ZAP   HIGHREV,MJETVAL+6(6)                                             
         B     RELOOKX                                                          
*                                                                               
         USING JBCOLD,R5                                                        
RELOOK2  ZAP   CUREST,JBCOLVAL(6)                                               
         ZAP   HIGHREV,JBCOLVAL+6(6)                                            
*                                                                               
RELOOKX  B     XIT                                                              
*                                                                               
         DROP  R5,R6                                                            
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO SET ADCONS FOR A LIST FIELD LINE                               
* AT ENTRY, R2=A(SELECT FIELD HEADER)                                           
*                                                                               
SETLIN   ST    RE,SAVERE                                                        
         LA    R0,NFIELDS                                                       
         LA    R1,ASEQ                                                          
         ST    R2,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BAS   RE,BUMP                                                          
         BCT   R0,*-12                                                          
         ST    R2,ANEXTLIN                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*     PROTECT ALL DATA FIELDS ON THE SCREEN                                     
*     CLEAR ALL DATA FIELDS ON THE SCREEN                                       
*---------------------------------------------------------------------          
*                                                                               
*                                                                               
SETSCR   NTR1                                                                   
*                                                                               
         LA    R5,SCRMAX                                                        
         LA    R2,CYCSEQ1H         R2=A(FIRST DATA FIELD)                       
         MVC   LISTAR,SPACES                                                    
*                                                                               
SETS20   LA    R3,UNBLATTR        ASSUME USING NORMAL ATTRIBUTES                
         LA    R4,NFIELDS          NUMBER OF FIELDS TO SET                      
         BAS   RE,SETLIN                                                        
         L     R2,ASEQ             SET R2 TO START OF LINE                      
*                                                                               
SETS30   NI    1(R2),X'FF'-FATBPROT-FATBHIGH                                    
         OC    1(1,R2),0(R3)                                                    
         BAS   RE,MOVEFLD                                                       
         BAS   RE,BUMP                                                          
         LA    R3,1(R3)                                                         
         BCT   R4,SETS30                                                        
*                                                                               
         L     R2,ANEXTLIN                                                      
*                                                                               
         BCT   R5,SETS20                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*     CLEAR FIELDS, CALL FROM READREC                                           
*---------------------------------------------------------------------          
*                                                                               
*                                                                               
INITSCHD NTR1                                                                   
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R2,CYCAIRH                                                       
         BAS   RE,MOVEFLD                                                       
         LA    R2,CYCHRVH                                                       
         BAS   RE,MOVEFLD                                                       
         LA    R2,CYCNEXH                                                       
         BAS   RE,MOVEFLD                                                       
         B     XIT                                                              
*                                                                               
*---------------------------------------------------------------------          
*     FORMAT A SCREEN FOR A NEW RECORD                                          
*     UNPROTECT  LINES AND FILL IN SEQUENCE NUMBERS                             
*---------------------------------------------------------------------          
*                                                                               
*                                                                               
INITSCR  NTR1                                                                   
*                                                                               
         LA    R5,SCRMAX                                                        
         LA    R2,CYCSEQ1H         R2=A(FIRST DATA FIELD)                       
         XR    R6,R6                                                            
         LA    R6,1                                                             
*                                                                               
INITS20  LA    R3,UNBLATTR         USING NEW RCCORD ATTRIB'S                    
         LA    R4,NFIELDS          NUMBER OF FIELDS TO SET                      
         BAS   RE,SETLIN                                                        
         L     R2,ASEQ                                                          
         MVC   LISTAR,SPACES                                                    
*                                                                               
INITS30  NI    1(R2),X'FF'-FATBPROT-FATBHIGH                                    
         OC    1(1,R2),0(R3)                                                    
         BAS   RE,MOVEFLD                                                       
         BAS   RE,BUMP                                                          
         LA    R3,1(R3)                                                         
         BCT   R4,INITS30                                                       
*                                                                               
         L     R2,ASEQ             SET SEQ NUMBER ON SCREEN                     
         EDIT  ((R6)),(2,LISTAR)                                                
         BAS   RE,MOVEFLD                                                       
         LA    R6,1(R6)                                                         
*                                                                               
         L     R2,ANEXTLIN                                                      
*                                                                               
         BCT   R5,INITS20                                                       
*                                                                               
         MVC   LISTAR,SPACES       CLEAR TOTAL FIELDS                           
         LA    R2,CYCBILH                                                       
         BAS   RE,MOVEFLD                                                       
         LA    R2,CYCESTH                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*     CLEAR THE DATA LINES ON THE SCREEN                                        
*---------------------------------------------------------------------          
*                                                                               
*                                                                               
CLRSCR   NTR1                                                                   
*                                                                               
         LA    R5,SCRMAX                                                        
         LA    R2,CYCSEQ1H         R2=A(FIRST DATA FIELD)                       
         XR    R6,R6                                                            
         LA    R6,1                                                             
*                                                                               
CLRS20   LA    R3,UNBLATTR         USING NEW RCCORD ATTRIB'S                    
         LA    R4,NFIELDS          NUMBER OF FIELDS TO SET                      
         BAS   RE,SETLIN                                                        
         L     R2,ASEQ                                                          
         MVC   LISTAR,SPACES                                                    
*                                                                               
CLRS30   NI    1(R2),X'FF'-FATBPROT-FATBHIGH                                    
         OC    1(1,R2),0(R3)                                                    
         BAS   RE,MOVEFLD                                                       
         BAS   RE,BUMP                                                          
         LA    R3,1(R3)                                                         
         BCT   R4,CLRS30                                                        
*                                                                               
         L     R2,ANEXTLIN                                                      
*                                                                               
         BCT   R5,CLRS20                                                        
*                                                                               
         LA    R2,CYCBILH                                                       
         BAS   RE,MOVEFLD                                                       
         LA    R2,CYCESTH                                                       
         BAS   RE,MOVEFLD                                                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*     SET TABLE WITH  BLANK TABLE ENTRIES                                       
*---------------------------------------------------------------------          
*                                                                               
*                                                                               
INITTAB  NTR1                                                                   
*                                                                               
         L     R5,ATABLE                                                        
         XR    R6,R6                                                            
         LA    R6,1                                                             
         LA    R4,TBMAX                                                         
*                                                                               
         USING TABLED,R5                                                        
INITT20  MVC   0(TBLEN,R5),BLNKTB                                               
         STC   R6,TBSEQ                                                         
         LA    R6,1(R6)                                                         
         LA    R5,TBNEXT                                                        
         BCT   R4,INITT20                                                       
*                                                                               
         MVI   0(R5),X'FF'                                                      
         MVI   TBCOUNT,TBMAX                                                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*        DELETE UNUSED ENTRIES FROM THE TABLE                                   
*---------------------------------------------------------------------          
DELTAB   NTR1                                                                   
         USING TABLED,R5                                                        
         LA    R1,1                                                             
         LA    R0,TBMAX                                                         
         L     R5,ATABLE                                                        
DELT10   CLI   0(R5),X'FF'         EOT                                          
         BE    DELTX               YES                                          
         CLC   TBDESC,SPACES       ANY DESCRIPTION ?                            
         BH    DELT20              YES                                          
         OC    TBMON,TBMON         ANY MONTH?                                   
         BNZ   DELT20              YES                                          
         OC    TBCUM,TBCUM         AND PCT?                                     
         BNZ   DELT20              YES                                          
         BAS   RE,DELETE                                                        
         B     DELT30                                                           
*                                                                               
DELT20   LA    R5,TBNEXT                                                        
         LA    R1,1(R1)                                                         
DELT30   BCT   R0,DELT10                                                        
*                                                                               
DELTX    B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*        AFTER A SCREEN ROW HAS BEEN DISPLAYED, SET ATTRIBUTES OF               
*        SCREEN.                                                                
*        ASSUMES SETLIN HAS SET ADCONS FOR THE ROW AND R5 IS A(TABLE            
*        ENTRY DISPLAYED)                                                       
*---------------------------------------------------------------------          
*                                                                               
SETATTR  NTR1                                                                   
*                                                                               
         L     R2,ASEQ                                                          
*                                                                               
         USING TABLED,R5                                                        
         LA    R3,BLDATTR          ATTRIBS FOR A BILLED CYCLE                   
         LA    R0,NFIELDS          NUMBER OF FIELDS TO SET                      
         OC    TBBILD,TBBILD       IS ENTRY BILLED                              
         BNZ   SETA30              YES                                          
         LA    R3,UNBLATTR                                                      
*                                                                               
         CLC   TBSEQ,NEXTCYC       IS THIS THE NEXT BILLABLE CYCLE              
         BNE   SETA30              NO                                           
         LA    R3,NEXTATTR                                                      
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
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
MYERROR  ST    R2,ACURFORC                                                      
         MVI   ERROR,X'FE'                                                      
         OI    GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
*                                                                               
XIT      XIT1                                                                   
         SPACE 1                                                                
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
         SPACE 1                                                                
DISMSG   DC    C'DATA DISPLAYED - ENTER CHANGES'                                
INVJOB   DC    C'ERROR - JOB MUST HAVE BILL TYPE ''AUTO''.'                     
FULINS   DC    C'ERROR - SCREEN IS FULL'                                        
BADINS   DC    C'ERROR -CAN''T INSERT BETWEEN BILLED ITEMS'                     
BADDEL   DC    C'ERROR - CANNOT DELETE'                                         
BADPFK   DC    C'ERROR - CURSOR MUST BE ON A FIELD TO DEL/INS AFTER'            
BADTOT   DC    C'ERROR - RATE CANNOT FOLLOW ''TOTAL''.'                         
BADORD   DC    C'ERROR - MONTHS MUST BE ASCENDING'                              
BADPLS   DC    C'ERROR - ''AND AFTER'' ONLY VALID FOR TOTAL BILLS'              
BADBIL   DC    C'ERROR - JOB HAS BEEN BILLED BY NON-AUTO BILLING'               
NOMODATA DC    C'ERROR - CANNOT ADD CYCLE IF PREVIOUS BILL MONTH IS MMMX        
               /YY+'                                                            
*                                                                               
BLDATTR  DC    AL1(FATBPROT)       BILLED CYCLE, FULLY PROTECTED                
         DC    AL1(FATBPROT)                                                    
         DC    AL1(FATBPROT)                                                    
         DC    AL1(FATBPROT)                                                    
         DC    AL1(FATBPROT)                                                    
         DC    AL1(FATBPROT)                                                    
         DC    AL1(FATBPROT)                                                    
*                                                                               
NEXTATTR EQU   *                   ATTRIBUTES FOR NEXT CYCLE TO BILL            
         DC    AL1(FATBHIGH+FATBPROT)                                           
         DC    AL1(FATBHIGH)                                                    
         DC    AL1(FATBHIGH)                                                    
         DC    AL1(FATBHIGH)                                                    
         DC    AL1(FATBHIGH+FATBPROT)                                           
         DC    AL1(FATBHIGH+FATBPROT)                                           
         DC    AL1(FATBHIGH+FATBPROT)                                           
*                                                                               
UNBLATTR EQU   *                   ATTRIBUTES FOR UNBILLED CYCLES               
         DC    AL1(FATBPROT)                                                    
         DC    AL1(0)              DESC                                         
         DC    AL1(0)              BILL MONTH                                   
         DC    AL1(0)              CUM                                          
         DC    AL1(FATBPROT)                                                    
         DC    AL1(FATBPROT)                                                    
         DC    AL1(FATBPROT)                                                    
*                                                                               
*        A BLANK CYCLE BILLING ELEMENT TEMPLATE                                 
*                                                                               
BLNKTB   DS    0C                                                               
         DC    XL(L'TBSEQ)'00'                                                  
         DC    XL(L'TBMON)'00'                                                  
         DC    XL(L'TBCUM)'00'                                                  
         DC    CL(L'TBDESC)' '                                                  
         DC    CL(L'TBREF)' '                                                   
         DC    XL(L'TBBILD)'00'                                                 
         DC    XL(L'TBUNBD)'00'                                                 
         DC    PL(L'TBBILA)'0'                                                  
         DC    PL(L'TBESTA)'0'                                                  
         DC    XL(L'TBSTAT)'00'                                                 
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
*ACGENFILE                                                                      
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
       ++INCLUDE ACPROWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN                                                            
LOCAL    DS    0X                                                               
MYDUB    DS    D                                                                
RELO     DS    A                                                                
SAVERE   DS    A                                                                
ACURSOR  DS    A                                                                
*                                                                               
ATHISLIN DS    A                                                                
ASEQ     DS    A                                                                
ADES     DS    A                                                                
ABLM     DS    A                                                                
ACUM     DS    A                                                                
AINC     DS    A                                                                
ABLA     DS    A                                                                
AESA     DS    A                                                                
NFIELDS  EQU   (*-ASEQ)/4                                                       
ANEXTLIN DS    A                                                                
*                                                                               
ATABLE   DS    A                                                                
AJOB     DS    A                                                                
ACOLTAB  DS    F                                                                
LCOLTAB  DS    F                                                                
AOPVTAB  DS    A                                                                
LOPVTAB  DS    F                                                                
PL16     DS    PL16                                                             
*                                                                               
COLIST   DS    XL80                                                             
*                                                                               
INTMODE  DS    X                   INTERNAL MODE                                
KEYCHG   DS    C                                                                
*                                                                               
DELSW    DS    C                                                                
UPDATE   DS    C                                                                
SEQSAVE  DS    X                   SEQ WHEN BUILDING TABLE FROM SCREEN          
NEXTCYC  DS    X                   NEXT CYCLE TO BILL, SET IN DISREC            
PREVMON  DS    CL(L'TBMON)         BILL MONTH OF PREV TABLE ENTRY               
TOTSTAT  DS    X                                                                
TOTSTOT  EQU   X'01'               I HAVE HAD A TOTAL                           
TOTSPLUS EQU   X'02'               I HAVE HAD A TOTAL+                          
CUMPCT   DS    XL2                 CUMULATIVE PERCENTAGE                        
PERSON   DS    CL8                                                              
DATE     DS    CL3                                                              
CUREST   DS    PL6                                                              
HIGHREV  DS    PL6                                                              
BILLTOT  DS    PL6                                                              
ESTTOT   DS    PL6                                                              
*                                                                               
       ++INCLUDE ACGOBLOCK                                                      
TABLE    DS    ((TBLEN*TBMAX)+1)C                                               
         EJECT                                                                  
*                                                                               
*                                                                               
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROCDD                                                       
         SPACE 2                                                                
         ORG   T60BFFD+2504                                                     
SVSAVE   DS    0D                                                               
TBCOUNT  DS    CL1                                                              
BILLING  DS    CL1                                                              
         DS    CL((SAVAREA-SVSAVE)-(*-SVSAVE))  SPARE                           
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
TBMAX    EQU   10                  N'TABLE ENTRIES                              
SCRMAX   EQU   TBMAX               MAX LINES ON SCREEN                          
FSTLIST  EQU   1                                                                
DISLIST  EQU   2                                                                
EDTLIST  EQU   3                                                                
         SPACE 2                                                                
PFKINS   EQU   2                                                                
PFKDEL   EQU   3                                                                
         SPACE 2                                                                
* DSECT TO COVER TABLE                                                          
*        TBSEQ=X'FF' IS END OF TABLE                                            
*                                                                               
TABLED   DSECT                                                                  
TBREC    DS    0C                                                               
TBSEQ    DS    CL1                 BILLING SEQUENCE                             
TBMON    DS    XL2                 YM OF BILLING                                
TBCUM    DS    XL2                 CUMULATIVE PCT TO HAVE BILLED                
*                                  ZERO=TOTAL                                   
TBDESC   DS    CL20                DESCRIPTION OF THIS CYCLE                    
TBREF    DS    CL6                 REF NUMBER OF BILL                           
TBBILD   DS    XL2                 BILL DATE (NAR+33 IN 99 REC)                 
TBUNBD   DS    XL2                 REV DATE (NAR+35 IN 99 REC)                  
TBBILA   DS    PL6                 BILLED AMOUNT                                
TBESTA   DS    PL6                 ESTIMATE AMOUNT                              
TBSTAT   DS    X                   STATUS BYTE                                  
TBSMON   EQU   BCYSMON             BILLING MONTH IS "AND AFTER"                 
TBNEXT   EQU   *                                                                
TBLEN    EQU   *-TABLED                                                         
         SPACE 2                                                                
         EJECT                                                                  
       ++INCLUDE DDPARSNIPD                                                     
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'070ACPRO3D   07/23/13'                                      
         END                                                                    
