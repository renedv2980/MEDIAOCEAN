*          DATA SET ACPRO59    AT LEVEL 003 AS OF 09/12/02                      
*PHASE T60B59A                                                                  
         TITLE 'ADVERTISER LIST'                                                
T60B59   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B59**,R7,RR=R2                                              
         ST    R2,RELO                                                          
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
         BE    JOB2                                                             
         CLI   MODE,VALREC                                                      
         BE    JOB4                                                             
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
*              VALKEY LOGIC                                       *             
*******************************************************************             
         SPACE                                                                  
JOB2     LA    RE,LOCAL            CLEAR LOCAL STORAGE                          
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         BAS   RE,VHED                                                          
*                                                                               
         GOTO1 DICTATE,DMCB,C'LL  ',DDIN,DDOUT                                  
*                                                                               
         MVI   CHASW,C'Y'                                                       
         CLI   TWAAUTH,0                                                        
         BE    JOB3                                                             
*        TM    TWAAUTH,CAT1Q+CAT3Q+CAT5Q TEST FOR CAT 1,3 OR 5                  
*        BNZ   *+8                                                              
*        MVI   CHASW,C'N'                                                       
*                                                                               
JOB3     MVI   INTMODE,EDTLIST     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST     YES-SET FIRST TIME LIST                      
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTLIST                                                  
         CLI   INTMODE,FSTLIST     TEST FIRST TIME LIST                         
         BNE   *+8                                                              
         BAS   RE,BLDTWA           YES-BUILD A SCREEN                           
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
*              VALREC LOGIC - DISPLAY OR CHANGE                   *             
*******************************************************************             
         SPACE                                                                  
JOB4     BAS   RE,SETSCR                                                        
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME LIST                     
         BE    JOB6                                                             
         BAS   RE,TSTEDT           TEST ANYTHING TO EDIT                        
         BE    JOB10               YES                                          
*                                                                               
         MVI   INTMODE,DISLIST     CONTINUE LIST                                
*                                                                               
* DISPLAY LOGIC                                                                 
*                                                                               
JOB6     GOTO1 VCLEARF,DMCB,AFSTSEL,AENDSCR                                     
         GOTO1 (RF),(R1),(1,AFSTSEL),AHELPFLD                                   
         MVI   LNLISTS,0                                                        
         LA    RE,LSELTAB                                                       
         LA    RF,L'LSELTAB                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         CLI   INTMODE,FSTLIST                                                  
         BNE   *+10                                                             
         XC    LASTADV,LASTADV     CLEAR OUT LAST ADVERTISER LISTED             
*                                                                               
JOB7     BAS   RE,LIST                                                          
         L     R2,AFSTSEL                                                       
         CLC   LNLISTS,NLINES      TEST IF SCREEN FILLED                        
         BE    JOB8                YES                                          
         XC    LASTADV,LASTADV     NO-MUST BE AT END-OF-LIST                    
         LA    R0,AP$LDISP                                                      
         CLI   LNLISTS,0           TEST ANYTHING ON SCREEN                      
         BNE   JOBIMSG             YES                                          
*                                                                               
         LA    R2,ADVLSTAH         POSITION CURSOR AT FIRST KEY FIELD           
         LA    R0,AP$NMREC                                                      
         CLI   INTMODE,FSTLIST     TEST FOR FIRST TIME                          
         BNE   JOBIMSG             NO                                           
         LA    R0,AP$NOREC                                                      
         B     JOBIMSG                                                          
*                                                                               
JOB8     LA    R0,AP$LDMOR                                                      
*                                                                               
JOBIMSG  ST    R2,ACURFORC                                                      
         SR    RF,RF                                                            
         ICM   RF,4,GETMSYS                                                     
         GOTO1 GETTXT,DMCB,(R0),(0,CONHEADH),(C'I',0),0,0,(RF)                  
         B     XIT                                                              
*                                                                               
* EDIT LOGIC                                                                    
*                                                                               
JOB10    BAS   RE,EDT                                                           
         L     R2,AFSTSEL                                                       
         ST    R2,ACURFORC                                                      
         LA    R0,AP$CHDUN                                                      
         B     JOBIMSG                                                          
         EJECT                                                                  
*******************************************************************             
*              VALIDATE THE HEADLINE FIELDS                       *             
*******************************************************************             
         SPACE 1                                                                
VHED     NTR1                                                                   
         GOTO1 SETHEIR                                                          
         MVI   KEYCHG,C'N'                                                      
         LA    R2,ADVLSTAH         START CODE (OPTIONAL)                        
         BAS   RE,TSTKEY                                                        
         CLI   ADVLSTAH+5,0                                                     
         BE    VHED01                                                           
         GOTO1 ANY                                                              
         MVI   ERROR,TOOLNG                                                     
         CLC   ADVLSTAH+5(1),LCLI                                               
         BH    ERREND                                                           
         MVC   QADV,WORK                                                        
         OI    ADVLSTAH+6,X'80'                                                 
VHED01   OI    ADVLSTAH+4,X'20'                                                 
*                                                                               
VHED02   MVC   NLINES(L'SCRNTAB),SCRNTAB                                        
         LA    R0,2                                                             
         LA    RE,ALISTELS         RE=A(ADCONS)                                 
         L     R1,RELO                                                          
VHED4    L     RF,0(RE)                                                         
         LTR   RF,RF               TEST FOR ZERO ADCON                          
         BZ    *+6                                                              
         AR    RF,R1               RELOCATE                                     
         ST    RF,0(RE)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,VHED4                                                         
*                                                                               
VHEDX    B     XIT                                                              
*                                                                               
TSTKEY   TM    4(R2),X'20'                                                      
         BOR   RE                                                               
         MVI   KEYCHG,C'Y'                                                      
         BR    RE                                                               
         EJECT                                                                  
*******************************************************************             
*              BUILD SCREEN DYNAMICALLY                           *             
*******************************************************************             
         SPACE                                                                  
BLDTWA   NTR1  ,                                                                
         LA    R2,ADVLTAGH         START SCREEN AT SAME POINT                   
         L     R3,ALISTELS                                                      
         USING TWAELEMD,R3                                                      
         SR    R0,R0                                                            
*                                                                               
BLDTWA3  CLI   TWAELCD,0           TEST FOR EOT                                 
         BE    BLDTWA4                                                          
         CLI   TWAELCD,X'03'       TEST FOR POTENTIAL UNP FIELD                 
         BNE   *+8                                                              
         MVI   TWAEATB,X'20'       MAKE IT PROTECTED                            
         IC    R0,TWAELLN                                                       
         AR    R3,R0                                                            
         B     BLDTWA3                                                          
*                                                                               
BLDTWA4  XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING TWAPARMD,R1                                                      
         XC    TWAPARMD(TWAPARML),TWAPARMD                                      
         MVC   TWAPATWA,ATWA                                                    
         MVC   TWAPAFST,ALISTELS                                                
         MVC   TWAPAMAX,=AL4(LSAVES-T60BFFD)                                    
         ST    R2,TWAPAOUT                                                      
         ZIC   R0,NLINES                                                        
*                                                                               
BLDTWA6  GOTO1 VTWABLD,TWAPARMD                                                 
         CLI   TWAPERRS,0          TEST FOR ERROR                               
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,TWAPANXT                                                      
         ST    R2,TWAPAOUT         SET A(OUTPUT)=A(NEXT FIELD)                  
         BCT   R0,BLDTWA6                                                       
*                                                                               
BLDTWA8  L     R3,AHELPELS                                                      
         MVC   TWAPAFST,AHELPELS   BUILD PF HELP LINE                           
         BAS   RE,PFTEXT                                                        
         GOTO1 VTWABLD,TWAPARMD                                                 
         CLI   TWAPERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDTWAX  B     XIT                                                              
         DROP  R1                                                               
*                                                                               
PFTEXT   NTR1                                                                   
         LA    R2,WORK+24                                                       
         LA    R4,WORK+48                                                       
         LA    R5,AS$PFADL                                                      
         GOTO1 GETTXT,(R2),(R5),('PFLMAX',(R4)),(C'S',0),,(X'08',0)             
         MVC   TWAEDTA(PFLMAX),0(R4)                                            
         MVI   TWAEDTA+PFLMAX,X'00'                                             
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*******************************************************************             
*              SET SCREEN ADDRESSES                               *             
*******************************************************************             
         SPACE                                                                  
SETSCR   ST    RE,SAVERE                                                        
         LA    R2,ADVLTAGH                                                      
         ST    R2,AFSTSEL                                                       
         ZIC   R0,NLINES                                                        
         ZIC   R1,NFIELDS                                                       
         MR    R0,R0               COMPUTE N'FIELDS TO BUMP                     
         BAS   RE,BUMP                                                          
         BCT   R1,*-4                                                           
         ST    R2,AHELPFLD                                                      
         BAS   RE,BUMP                                                          
         ST    R2,AENDSCR          NOTE END OF SCREEN                           
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 3                                                                
SETLIN   ST    RE,SAVERE           R2=A(SELECT FIELD HEADER) AT ENTRY           
         ZIC   R0,NFIELDS                                                       
         LA    R1,ASEL                                                          
         ST    R2,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BAS   RE,BUMP                                                          
         BCT   R0,*-12                                                          
         ST    R2,ANEXTSEL                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*******************************************************************             
*              TEST FOR ANYTHING TO EDIT ON SCREEN                *             
*******************************************************************             
         SPACE                                                                  
*              ON EXIT : CC=EQ TO EDIT                                          
*                        CC=NEQ TO CONTINUE DISPLAY                             
         SPACE                                                                  
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
TSTEDT4  ZIC   R4,NFIELDS          R4=FIELD COUNTER                             
         BCTR  R4,0                LESS ONE FOR SELECT                          
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
*******************************************************************             
*              LIST ADVERTISERS FOR AGENCY                        *             
*******************************************************************             
         SPACE 1                                                                
LIST     NTR1                                                                   
         L     R2,AFSTSEL                                                       
         ST    R2,ATHISLIN         SET POINTER TO LIST LINE                     
         GOTO1 SETHEIR                                                          
         LA    R4,KEY                                                           
         USING ADVRECD,R4                                                       
         XC    ADVKEY,ADVKEY                                                    
         OC    LASTADV,LASTADV     TEST IF CONTINUING LIST                      
         BZ    LIST2                                                            
         MVC   ADVKEY,LASTADV      USE SAVED KEY                                
         MVC   SKEY,ADVKEY                                                      
         B     LISTFX                                                           
*                                                                               
LIST2    MVI   ADVKTYP,ADVKTYPQ    BUILD ADV RECORD KEY IF NEW LIST             
         MVC   ADVKCPY(1),CUL                                                   
         MVC   SKEY,ADVKEY         SAVE KEY                                     
         OC    QADV,QADV           TEST FOR START INPUT                         
         BZ    LIST20              NO                                           
         MVC   ADVKADV,QADV                                                     
*                                                                               
LIST20   GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR',ADVKEY,ADVKEY                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ADVKEY(2),SKEY      IS THIS AN ADVERTISER RECORD?                
         BNE   LISTX               NO  - ALL DONE                               
         OC    ADVKNULL,ADVKNULL   YES - SKIP IF A PASSIVE POINTER              
         BNZ   LISTFX                                                           
         MVC   IODA,ADVKEY+(ACCKDA-ACCRECD)                                     
         GOTO1 (RF),(R1),=C'GETREC',=C'ACCMST',IODA,AIO,IOWORK                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,AIO                                                           
         MVC   ADVKEY,0(RE)                                                     
*                                                                               
         MVC   QADV,ADVKADV                                                     
         GOTO1 SETLLINE            MOVE IN ADVERTISER DETAILS                   
         MVC   TSUB(L'AC@SUBAC),AC@SUBAC  MOVE IN TITLES                        
         MVC   TCODE(L'AC@CODE),AC@CODE                                         
         MVC   TNAME(L'AC@NAME),AC@NAME                                         
         MVC   ADVLTIT(L'TITLES),TITLES                                         
         L     R2,ATHISLIN                                                      
         BAS   RE,SETLIN           SET LIST LINE ADDRESSES                      
         L     R2,ASEL                                                          
         OI    4(R2),X'20'         SET ON PREV VALIDATED BIT                    
         NI    1(R2),X'FF'-X'20'                                                
         L     R2,APROT                                                         
         OI    6(R2),X'80'         SET ON TX BIT                                
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+4                                                           
         MVC   8(0,R2),LISTAR                                                   
         SPACE                                                                  
*                                                                               
LIST30   MVC   LASTADV,ADVKEY      SAVE ADVERTISER RECORD KEY                   
         MVC   ATHISLIN,ANEXTSEL   UPDATE LINE POINTER                          
         ZIC   RE,LNLISTS                                                       
         LR    R1,RE                                                            
         LA    RE,1(RE)            INCREMENT LIST RECORD COUNT                  
         STC   RE,LNLISTS                                                       
*                                                                               
         MH    R1,=Y(SELTABL)                                                   
         LA    R1,LSELTAB(R1)                                                   
         USING SELTABD,R1                                                       
         MVC   SELACT,SPACES                                                    
         LA    RF,ADVKEY                                                        
         MVC   SELKEY(L'SELKEY),0(RF)  SAVE ADVERTISER RECORD KEY               
*                                                                               
LIST31   CLC   LNLISTS,NLINES      TEST SCREEN FILLED                           
         BE    LISTX                                                            
*                                                                               
LISTFX   LA    RF,ADVKNULL-ADVRECD(R4)                                          
         MVI   0(RF),X'FF'        FORCE NEXT ADVERTISER RECORD READ             
         B     LIST20                                                           
*                                                                               
LISTX    B     XIT                                                              
         DROP  R1                                                               
         DROP  R4                                                               
         EJECT                                                                  
*******************************************************************             
*              SET ADVERTISER DETAILS ON LIST LINE                *             
*******************************************************************             
         SPACE                                                                  
SETLLINE NTR1                                                                   
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         LA    R4,LISTAR                                                        
         USING LLPROTD,R4                                                       
         MVC   LLCODE,QADV                                                      
*                                                                               
         L     R3,AIO                                                           
         USING ADVRECD,R3                                                       
         LA    R3,ADVRFST                                                       
         USING NAMELD,R3                                                        
SETL02   CLI   NAMEL,NAMELQ                                                     
         BNE   SETL04                                                           
         XR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+4                                                           
         MVC   LLNAME(0),NAMEREC                                                
         B     SETL06                                                           
*                                                                               
         USING RSTELD,R3                                                        
SETL04   CLI   RSTEL,RSTELQ                                                     
         BNE   SETL06                                                           
         MVC   LLFIL1,RSTFILT1                                                  
         MVC   LLFIL2,RSTFILT2                                                  
         MVC   LLFIL3,RSTFILT3                                                  
         MVC   LLFIL4,RSTFILT4                                                  
         CLI   RSTLN,RSTLN2Q                                                    
         BNE   *+10                                                             
         MVC   LLFIL5,RSTFILT5                                                  
         B     SETL06                                                           
*                                                                               
SETL06   XR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BNE   SETL02                                                           
         B     XIT                                                              
         DROP  R3                                                               
         DROP  R4                                                               
         EJECT                                                                  
*******************************************************************             
*              EDIT LIST SCREEN                                   *             
*******************************************************************             
         SPACE                                                                  
* NOTE-GENCON DISABLES READ FOR UPDATE FOR ACTION LIST                          
         SPACE                                                                  
EDT      NTR1  ,                                                                
         L     R2,AFSTSEL          R2=A(SELECT FIELD)                           
         ZIC   R3,LNLISTS                                                       
         LA    R5,LSELTAB                                                       
         USING SELTABD,R5                                                       
*                                                                               
EDT2     BAS   RE,SETLIN                                                        
         L     R2,ASEL                                                          
         CLI   5(R2),0             TEST ANY SELECT INPUT                        
         BE    EDT8                                                             
         CLI   8(R2),C'*'          TEST ALREADY EDITED                          
         BE    EDT8                YES                                          
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLC   8(1,R2),AC@SEL      TEST 'S' = SELECT TO MAINTAIN                
         BNE   ERREND                                                           
*                                                                               
EDT8     LA    R4,KEY                                                           
         USING ADVRECD,R4                                                       
         XC    ADVKEY,ADVKEY                                                    
         MVC   ADVKEY(L'SELKEY),SELKEY                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCDIR',ADVKEY,ADVKEY                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODA,ADVKEY+(ACCKDA-ACCRECD)                                     
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST',IODA,AIO,IOWORK               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,AIO                                                           
         MVC   ADVKEY,0(RE)                                                     
*                                                                               
EDT10    CLI   5(R2),0             TEST FOR SELECT INPUT                        
         BE    EDT20               NO                                           
         CLI   8(R2),C'*'          TEST ALREADY THERE                           
         BE    EDT20                                                            
         MVC   SELACT,8(R2)        SAVE ACTION                                  
         MVC   8(3,R2),SPACES      CLEAR SELECT FIELD                           
         MVI   8(R2),C'*'          MARK SELECT FIELD                            
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'         XMIT FIELD BACK                              
         MVI   PFKEY,0             CLEAR BEFORE CALLING OVERLAY                 
*                                                                               
         CLC   SELACT(1),AC@SEL                                                 
         BNE   ERREND                                                           
         MVI   TWALREC,0                                                        
         GOTO1 VCALL,WORK,RECNADV,ACTNMNT,(5,ADVKADV),0                         
*                                                                               
EDT20    L     R2,ANEXTSEL                                                      
         LA    R5,SELTABL(R5)                                                   
         BCT   R3,EDT2                                                          
*                                                                               
EDTX     B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
*              SUPPORTING SUBROUTINES AND CONSTANTS               *             
*******************************************************************             
         SPACE                                                                  
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 2                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 2                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
RELO     DC    A(0)                                                             
OPTBUFFL DC    F'3000'                                                          
         SPACE 2                                                                
         DS    0F                                                               
SCRNTAB  DS    0CL12                                                            
         DC    AL1(SELLINES,2),X'0000'                                          
         DC    A(SCRNELS),A(SCRNHELP)                                           
         SPACE 2                                                                
TITLES   DC    CL(L'ADVLTIT)' '                                                 
         ORG   TITLES                                                           
TSUB     DC    CL(L'AC@SUBAC)' '                                                
         DC    CL5' '                                                           
TCODE    DC    CL(L'AC@CODE)' '                                                 
         DC    CL3' '                                                           
TNAME    DC    CL(L'AC@NAME)' '                                                 
         DC    CL33' '                                                          
         DC    CL10'F1 2 3 4 5'                                                 
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
* ELEMENT TABLES FOR SCREEN GENERATION                                          
*                                                                               
SCRNELS  DC    X'01',AL1(TWAELLNQ),AL1(1),AL1(02),AL1(03),X'08',AL1(0)          
         DC    X'01',AL1(TWAELLNQ),AL1(0),AL1(06),AL1(70),X'20',AL1(0)          
         DC    X'00'                                                            
SCRNELSL EQU   *-SCRNELS                                                        
         SPACE 2                                                                
SCRNHELP DC    X'01',AL1(TWAELLNQ+PFLMAX),AL1(1),AL1(02),AL1(PFLMAX)            
         DC    X'28',AL1(0)                                                     
         DC    X'00'                                                            
         EJECT                                                                  
DDIN     DS    0C                                                               
         DCDDL AC#SEL,6                                                         
         DCDDL AC#SUBAC,3                                                       
         DCDDL AC#CODE,4                                                        
         DCDDL AC#NAME,4                                                        
         DC    X'00'                                                            
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
* ACPROWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN               USE THE DRONEBLK AREA                        
LOCAL    DS    0C                                                               
KEYCHG   DS    C                                                                
INTMODE  DS    X                   INTERNAL MODE                                
CHASW    DS    C                                                                
*                                                                               
QCONTROL DS    0C                                                               
QADV     DS    CL5                                                              
QCONTRLN EQU   *-QCONTROL                                                       
*                                                                               
SAVERE   DS    A                                                                
ACURSOR  DS    A                                                                
*                                                                               
NLINES   DS    X                                                                
NFIELDS  DS    X                                                                
         DS    XL2                                                              
ALISTELS DS    A                                                                
AHELPELS DS    A                                                                
*                                                                               
AFSTSEL  DS    A                                                                
AHELPFLD DS    A                                                                
AENDSCR  DS    A                                                                
*                                                                               
ATHISLIN DS    A                                                                
ASEL     DS    A                                                                
*                                                                               
APROT    DS    A                                                                
ANEXTSEL DS    A                                                                
*                                                                               
SKEY     DS    CL48                                                             
*                                                                               
IODA     DS    XL4                                                              
IOWORK   DS    XL96                                                             
*                                                                               
DDOUT    DS    0C                                                               
         DSDDL PRINT=YES                                                        
         PRINT GEN                                                              
         EJECT                                                                  
* ACPANBLOCK                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACPANBLOCK                                                     
         PRINT ON                                                               
* ACMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROADD                                                       
         SPACE 2                                                                
         ORG   T60BFFD+2408                                                     
LSAVES   DS    0D                                                               
LNLISTS  DS    X                   N'LISTS ON SCREEN                            
LASTADV  DS    CL42                LAST ADVERTISER ON SCREEN                    
LSELTAB  DS    CL(SELLINES*SELTABL)                                             
*        DS    CL((SAVAREA-LSAVES)-(*-LSAVES))  SPARE                           
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
FSTLIST  EQU   1                                                                
DISLIST  EQU   2                                                                
EDTLIST  EQU   3                                                                
         SPACE 2                                                                
* DSECT TO COVER LIST LINE PROTECTED FIELD                                      
*                                                                               
LLPROTD DSECT                                                                   
         DS    CL4                                                              
LLCODE   DS    CL5                                                              
         DS    CL2                                                              
LLNAME   DS    CL36                                                             
         DS    CL2                                                              
LLFIL1   DS    CL1                                                              
         DS    CL1                                                              
LLFIL2   DS    CL1                                                              
         DS    CL1                                                              
LLFIL3   DS    CL1                                                              
         DS    CL1                                                              
LLFIL4   DS    CL1                                                              
         DS    CL1                                                              
LLFIL5   DS    CL1                                                              
         SPACE 2                                                                
* DSECT TO COVER SELECT TABLE                                                   
*                                                                               
SELTABD  DSECT                                                                  
SELLINES EQU   16                  MAX SCREEN LINES                             
SELACT   DS    CL3                 SELECT ACTION                                
SELKEY   DS    CL7                 ADVERTISER KEY                               
SELTABL  EQU   *-SELTABD                                                        
         EJECT                                                                  
       ++INCLUDE DDTWABLDD                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACPRO59   09/12/02'                                      
         END                                                                    
