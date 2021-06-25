*          DATA SET ACREPC602  AT LEVEL 050 AS OF 05/01/02                      
*PHASE ACC602A,+0                                                               
*INCLUDE SQUASHER                                                               
*INCLUDE ACSLRY                                                                 
*INCLUDE UNDERLIN                                                               
         TITLE 'ACC602 - DEPARTMENTAL COST STATEMENT'                           
ACC602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*ACC6*,R9                                                      
         L     RA,0(,R1)                                                        
*                                                                               
         USING ACWORKD,RA                                                       
*                                                                               
         LA    RC,SPACEND                                                       
*                                                                               
         USING ACC6D,RC                                                         
*                                                                               
         EJECT ,                                                                
*              FIRST FOR RUN                                                    
         SPACE 1                                                                
DCS1     CLI   MODE,RUNFRST                                                     
         BNE   DCS10                                                            
         RELOC RELO                                                             
         LA    RE,RELOTAB          RELOCATE A-TYPES                             
         LA    R1,ATYPES                                                        
*                                                                               
RELOOP   L     RF,0(,RE)                                                        
         A     RF,RELO                                                          
         ST    RF,0(,R1)                                                        
         LA    R1,4(,R1)                                                        
         LA    RE,4(,RE)                                                        
         CLI   0(RE),X'FF'                                                      
         BNE   RELOOP                                                           
         SPACE 1                                                                
DCSXIT   XIT1                                                                   
         EJECT ,                                                                
*              FIRST FOR REQUEST                                                
         SPACE 1                                                                
DCS10    CLI   MODE,REQFRST                                                     
         BNE   DCS20                                                            
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK(4),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(1,START)                                   
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(1,END)                                     
         GOTO1 DATCON,DMCB,(1,END),(0,WORK+6)                                   
         MVC   QEND(2),WORK+6      MAKE SURE IN DDS FORMAT FOR YR2000           
         MVC   STEND(2),START                                                   
         MVC   STEND+2(2),END                                                   
         SPACE 1                                                                
************* BUILDING QUARTER DATES INTO TABLE**************                   
         SPACE 1                                                                
         MVC   QTRTAB(2),START                  START DATE INTO TABL            
         MVC   DATE1(4),QSTART                  YYMM OF START DATE              
         MVC   DATE1+4(2),=C'01'                 DAY IS 01                      
         ZAP   QTRCOUNT,=P'0'                   CLEAR QUARTER COUNTER           
*                                                                               
         SR    R2,R2                            CLEAR R2 FOR COUNTER            
         LA    R8,QTRTAB                        DATE TABLE INTO R8              
*                                                                               
DCS12    GOTO1 ADDAY,DMCB,(C'M',DATE1),DATE2,F'2'   ADD 2 MONTHS                
         GOTO1 DATCON,DMCB,(0,DATE2),(1,QTREND) PACK INTO QTREND                
         MVC   2(2,R8),QTREND                   MOVE YYMM INTO TABLE            
         CLC   DATE2(4),QEND                    IS QTR END OUT OF PER           
         BNL   DCS14                                                            
         LA    R8,QTRLNQ(,R8)                   RESET TAB FOR NEXT QTR          
         MVI   0(R8),X'FF'                      END OF TABLE MARKER             
         LA    R2,1(,R2)                        ADD 1 TO QUARTER COUNT          
         GOTO1 ADDAY,DMCB,(C'M',DATE2),DATE1,F'1'   ADD 1 MONTH                 
         GOTO1 DATCON,DMCB,(0,DATE1),(1,QTRSTART) START NEXT QTR                
         MVC   0(2,R8),QTRSTART                 MOVE YYMM INTO TABLE            
         B     DCS12                                                            
*                                                                               
DCS14    CLC   DATE2(4),QEND                    IS QTR END = TO REQ END         
         BE    DCS16                                                            
         MVC   P+1(L'ERRMSG1),ERRMSG1                                           
         GOTO1 ACREPORT                                                         
         MVI   FCRDACC,C'N'                                                     
         MVI   FCRDTRNS,C'N'                                                    
         MVI   FCRDHIST,C'N'                                                    
         L     RF,AMONACC                                                       
*                                                                               
         USING ACMD,RF                                                          
*                                                                               
         MVI   ACMMODE,REQLAST                                                  
         B     DCSXIT                                                           
*                                                                               
         DROP  RF                                                               
*                                                                               
DCS16    LA    R8,QTRLNQ(,R8)                   R8 TO END OF TABLE              
         MVI   0(R8),X'FF'                      END OF TABLE MARKER             
         LA    R2,1(,R2)                        ADD 1 TO QUARTER COUNT          
         STC   R2,QTRCOUNT                                                      
         EJECT ,                                                                
         MVC   PERIOD,SPACES                                                    
         MVC   PERIOD(14),=C'FOR THE PERIOD'                                    
         GOTO1 DATCON,DMCB,(1,START),(9,WORK)                                   
         MVC   WORK+3(2),WORK+4    JAN/82 TO JAN82                              
         MVC   PERIOD+15(5),WORK                                                
         GOTO1 DATCON,DMCB,(1,END),(9,WORK)                                     
         MVC   WORK+3(2),WORK+4                                                 
         CLC   START,END                                                        
         BE    *+14                                                             
         MVC   PERIOD+21(5),WORK                                                
         MVI   PERIOD+20,C'-'                                                   
*                                                                               
         L     R5,DEPTC            CLEAR DEPT AND REQUEST TABLES                
         GOTO1 CLEAR,DMCB,(R5)                                                  
         L     R5,REQC                                                          
         GOTO1 CLEAR,DMCB,(R5)                                                  
         L     R6,ALLOC            CLEAR ALLOCATED AND DIRECT AREAS             
         USING CODED,R6                                                         
         XC    CODE(CODELEN),CODE                                               
         L     R6,DIRECT                                                        
         XC    CODE(CODELEN),CODE                                               
*                                                                               
         MVC   COMP,SPACES                                                      
         MVC   COMP(7),=C'COMPANY'                                              
         L     R4,ADCMPNAM         COMPANY FOR HEAD LINES                       
         LA    R6,COMP+10                                                       
         BAS   RE,NAMOUT                                                        
         B     DCSXIT                                                           
         EJECT ,                                                                
*              FIRST FOR DEPARTMENT                                             
         SPACE 1                                                                
DCS20    CLI   MODE,LEVBFRST                                                    
         BNE   DCS30                                                            
         L     R5,DEPTC            CLEAR DEPT TABLES                            
         GOTO1 CLEAR,DMCB,(R5)                                                  
         ZAP   DEPSAL,=P'0'                                                     
         ZAP   QACCUM1,=P'0'                                                    
         ZAP   QACCUM2,=P'0'                                                    
         ZAP   QACCUM3,=P'0'                                                    
         ZAP   QACCUM4,=P'0'                                                    
         ZAP   TOTSAL,=P'0'                                                     
*                                                                               
         USING PERCENTD,R8                                                      
*                                                                               
         ZIC   R4,QTRCOUNT         NUMBER OF QTRS INTO R4                       
         LA    R8,QTRTAB           ADDR OF TABLE IN R8                          
*                                                                               
DCS20A   ZAP   BENPCT,=P'0'        CLEAR BENEFIT PERCENT                        
         ZAP   ADMNPCT,=P'0'       CLEAR ADMN PERCENT                           
         LA    R8,QTRLNQ(,R8)      NEXT TABLE ENTRY                             
         BCT   R4,DCS20A                                                        
         SPACE 1                                                                
         L     R2,ADHEIRA        * GET BENEFIT AND ADMIN PCT   *                
         BAS   RE,EMPBEN         * FROM FILE                   *                
         L     R2,ADHEIRB        *                             *                
         BAS   RE,EMPBEN         *******************************                
         SPACE 1                                                                
         MVI   WANT,C'Y'                                                        
*                                                                               
         USING ACSTATD,R4                                                       
*                                                                               
         L     R4,ADLVBSTA                                                      
         TM    ACSTSTX,X'08'                                                    
         B     DCS22                                                            
         BZ    DCS22                                                            
         MVI   WANT,C'N'           SKIP INDIRECT DEPTS.                         
         B     DCSXIT                                                           
*                                                                               
DCS22    MVC   DEPT,SPACES                                                      
         MVC   DEPT(10),=C'DEPARTMENT'                                          
         L     R4,ADHEIRB                                                       
         MVC   DEPT+11(2),4(R4)    CODE                                         
         L     R4,ADLVBNAM                                                      
         LA    R6,DEPT+14                                                       
         BAS   RE,NAMOUT                                                        
*                                                                               
         L     R5,DEPTC                                                         
         GOTO1 CLEAR,DMCB,(R5)     CLEAR DEPT. BUCKETS                          
         B     DCSXIT                                                           
         EJECT ,                                                                
*              PROCESS ACCOUNTS - PERSON                                        
         SPACE 1                                                                
DCS30    CLI   MODE,PROCACC                                                     
         BNE   DCS50                                                            
         CLI   WANT,C'N'                                                        
         BE    DCSXIT              SKIP INDIRECT DEPTS.                         
         L     R4,ADACC                                                         
         CLI   3(R4),C'9'                                                       
         BE    DCSXIT              SKIP CORP. OVERHEAD                          
         CLC   8(3,R4),=C'999'     OVERHEAD ARE                                 
         BE    DCS35               ALLOCATED EXPENSES                           
*                                                                               
         LA    R7,ACCUMS           ADDRS OF QTRLY ACCUMULATORS                  
         LA    R8,QTRTAB           QTRS IN REQUEST PERIOD                       
         CLI   0(R8),X'FF'         END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                TABLE EMPTY --DIE                            
*                                                                               
DCS31    MVC   STED(4),0(R8)        MOVE IN QTR END DATE                        
*MN      GOTO1 ACSLRY,DMCB,(R4),STED,SALAREA                                    
         GOTO1 ACSLRY,DMCB,(X'80',(R4)),STED,SALAREA,ADCOMFAC                   
*                                                                               
         LA    R4,SALAREA                                                       
         L     R5,DEPTC                                                         
*                                                                               
         USING SLRD,R4                                                          
         USING COSTD,R5                                                         
*                                                                               
         LA    R4,SLRSAL                                                        
         LA    R5,REGULAR                                                       
         LA    R3,SALCNT                                                        
*                                                                               
DCS32    CLI   6(R4),X'00'         SKIP IF NOT DOLLARS (SKIP PCTS)              
         BNE   DCS33                                                            
         AP    0(6,R5),0(6,R4)     ADD SALARY,OVT,TMP,BONUS TO DEPT.            
         AP    TOTSAL,0(6,R4)      ADD TO DEPT SALARY                           
         AP    0(6,R7),0(6,R4)     ADD TO CORRECT QTR SAL ACCUM                 
*                                                                               
DCS33    LA    R4,7(,R4)           ADDR OF NEXT SAL CATEGORY                    
         LA    R5,6(,R5)           MATCHING ADDR IN COSTD DSECT                 
         BCT   R3,DCS32                                                         
*                                                                               
         LA    R7,6(,R7)           POINT R7 TO NEXT QTR ACCUM                   
         LA    R8,QTRLNQ(,R8)      NEXT TABLE QTR END DATE                      
         L     R4,ADACC            POINT R4 BACK TO ACCT                        
         CLI   0(R8),X'FF'         END OF TABLE?                                
         BNE   DCS31               GO GET THE NEXT QTRS SALARY                  
         B     DCSXIT                                                           
         EJECT ,                                                                
*              PROCESS ACCOUNT - OVERHEAD                                       
         SPACE 1                                                                
         USING CODED,R6            FIND A MATCH IN ALLOCATED CODE LIST          
         SPACE 1                                                                
DCS35    BAS   RE,ACCFIX           FIX ACCOUNT CODES                            
         L     R4,ADACC                                                         
         L     R6,ALLOC                                                         
*                                                                               
         SR    R0,R0                                                            
*                                                                               
DCS36    CLI   0(R6),0             NO MATCH FOUND A SLOT                        
         BE    DCS38                                                            
         CLC   ACCODE,CODE                                                      
         BE    DCS39               FOUND A MATCH                                
         LA    R6,CODELEN(,R6)                                                  
         AH    R0,=H'1'                                                         
         B     DCS36                                                            
*                                                                               
DCS38    MVC   CODE(7),ACCODE       MOVE NEW CODE                               
         MVC   CODENME,ACCNAME                                                  
         LA    R6,CODELEN(R6)                                                   
         XC    CODE(CODELEN),CODE  CLEAR NEXT ENRTY                             
*                                                                               
         USING COSTD,R5                                                         
*                                                                               
DCS39    L     R5,DEPTC                                                         
         STC   R0,BYTE                                                          
         LA    R5,EXPENSES                                                      
*                                                                               
DCS40    CLI   0(R5),0             FIND A SLOT IN DEPT EXPENSE LIST             
         BE    DCS41                                                            
         CLI   0(R5),C'A'                                                       
         BNE   DCS40A                                                           
         CLC   1(1,R5),BYTE                                                     
         BE    DCS42                                                            
*                                                                               
DCS40A   LA    R5,8(,R5)                                                        
         B     DCS40                                                            
*                                                                               
DCS41    MVI   0(R5),C'A'          ALLOCATED                                    
         STC   R0,1(,R5)           DISP. IN CODE NAME TABLE                     
         MVI   BYTE,0                                                           
*                                                                               
DCS42    L     R4,ADACC                                                         
*MN      GOTO1 ACSLRY,DMCB,(R4),STEND,SALAREA                                   
         GOTO1 ACSLRY,DMCB,(X'80',(R4)),STEND,SALAREA,ADCOMFAC                  
         LA    R4,SALAREA                                                       
*                                                                               
         USING SLRD,R4                                                          
*                                                                               
         CLI   BYTE,0                                                           
         BNE   DCS43                                                            
         ZAP   2(6,R5),SLRTOT      ALLOCATED FOR PERIOD                         
         B     DCSXIT                                                           
*                                                                               
DCS43    AP    2(6,R5),SLRTOT                                                   
         B     DCSXIT                                                           
         EJECT ,                                                                
*              LAST FOR DEPARTMENT                                              
         SPACE 1                                                                
DCS50    CLI   MODE,LEVBLAST       AT END OF DEPT. READ 1P                      
         BNE   DCS70                                                            
         CLI   WANT,C'N'                                                        
         BE    DCSXIT              SKIP INDIRECT DEPTS.                         
         L     R4,ADHEIRA                                                       
         CLI   3(R4),C'9'          SKIP OFFICE 9                                
         BE    DCSXIT                                                           
         BAS   RE,BENIN            ADD BENEFIT                                  
         BAS   RE,ADMIN            AND ADMINISTRATIVE                           
         MVC   SAVEKEY,KEY         FOR DIRECT EXPENSES                          
         LA    R4,KEY                                                           
*                                                                               
         USING ACKEYD,R4                                                        
*                                                                               
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),QCOMPANY                                             
         MVC   ACKEYACC+1(2),=C'1P'                                             
         L     R3,ADHEIRB                                                       
         MVC   SAVEDEPT,4(R3)                                                   
         MVC   ACKEYACC+3(2),4(R3)   DEPT. CODE                                 
         MVC   SAVEACCN,SPACES                                                  
         ZAP   SAVECOST,=P'0'                                                   
         L     R4,RECORD                                                        
         MVC   ACKEYACC,KEY                                                     
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',(R4),(R4)                        
         CLC   ACKEYACC,KEY                                                     
         BNE   DCS54               NO DEPT. IN 1P                               
*                                                                               
DCS52    MVI   ELCODE,X'32'                                                     
         BAS   RE,GETEL                                                         
         BNE   DCS55                                                            
         BAS   RE,POSTLAST         NEW ACCOUNT-POST LAST                        
         L     R4,RECORD                                                        
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO NAME                                      
*                                                                               
         LA    R6,SAVEACCN                                                      
         BAS   RE,NAMOUT           NEW ACCOUNT NAME                             
         ZAP   SAVECOST,=P'0'                                                   
         L     R4,RECORD                                                        
         MVC   SAVEACC,ACKEYACC+5  ACCOUNT CODE WITHIN DEPT.                    
*                                                                               
DCS53    L     R4,RECORD                                                        
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCOUNT',(R4),(R4)                        
         CLC   ACKEYACC(5),KEY     SAME DEPT.                                   
         BE    DCS52                                                            
         BAS   RE,POSTLAST         NOT SAME-POST LAST                           
*                                                                               
DCS54    MVC   KEY,SAVEKEY         RESTORE LAST KEY FOR MONACC                  
         L     R4,RECORD                                                        
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',KEY,(R4)                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                CAN'T READ SAVED KEY                         
*                                                                               
         L     R5,DEPTC                                                         
         GOTO1 PRNTIT,DMCB,(R5)    PRINT DEPT. PAGE                             
         BAS   RE,ADDUP            ADD DEPT TO REQUEST                          
         GOTO1 CLEAR,DMCB,(R5)     CLEAR DEPT TOTALS                            
         B     DCSXIT                                                           
*                                                                               
DCS55    L     R4,RECORD                                                        
         MVI   ELCODE,X'45'                                                     
         BAS   RE,GETEL                                                         
         BNE   DCS53               NO HISTORY                                   
*                                                                               
         USING TRHISTD,R4                                                       
*                                                                               
DCS56    CLC   TRHSYEAR(2),START                                                
         BL    DCS57                                                            
         CLC   TRHSYEAR(2),END                                                  
         BH    DCS57                                                            
         AP    SAVECOST,TRHSDR                                                  
*                                                                               
DCS57    BAS   RE,NEXTEL                                                        
         BE    DCS56                                                            
         B     DCS53                                                            
         EJECT ,                                                                
*              LAST FOR REQUEST                                                 
         SPACE 1                                                                
DCS70    CLI   MODE,REQLAST                                                     
         BNE   DCSXIT                                                           
         CLI   FCRDACC,C'N'                                                     
         BE    DCSXIT                                                           
         L     R5,REQC                                                          
         GOTO1 PRNTIT,DMCB,(R5)                                                 
         B     DCSXIT                                                           
         EJECT ,                                                                
*              ROUTINE TO GET EMPLOYEE BENEFIT AND ADMIN. PERCENT               
         SPACE 1                                                                
         USING PERCENTD,R8                                                      
         USING ACSALRYD,R4         SALARY DSECT                                 
         SPACE 1                                                                
EMPBEN   NTR1                                                                   
         LR    R4,R2                                                            
         MVI   ELCODE,X'52'        GO GET SALARY ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   DCSXIT              NO 52 ELEMENT GET OUT                        
*                                                                               
EMPBEN01 CLI   ACSALSTA,X'00'      IS IT A DOLLAR AMOUNT                        
         BE    EMPBEN03            YES - DONT WANT IT.GET NEXT EL               
         LA    R8,QTRTAB           TABLE IN R8                                  
*                                                                               
EMPBEN02 CLC   QTRST(4),ACSALBEG   IS IT THE CORRECT QUARTER                    
         BE    EMPBEN04            YES GO FILL IN TABLE SLOT                    
         LA    R8,QTRLNQ(,R8)      NEXT QTR DATE IN TABLE                       
         CLI   0(R8),X'FF'         END OF TABLE                                 
         BNE   EMPBEN02            NO  - CHECK THE DATE                         
*                                                                               
EMPBEN03 BAS   RE,NEXTEL           GO GET NEXT 52 ELEMENT                       
         BE    EMPBEN01                                                         
         B     DCSXIT                                                           
*                                                                               
EMPBEN04 TM    ACSALTYP,X'10'      IS IT A BENEFIT PERCENTAGE                   
         BZ    ADMINP              NO --ITS AN ADMIN PERCENTAGE                 
*                                                                               
         MVC   BENSTAT,ACSALSTA    HOW MANY DECIMAL PLACES                      
         ZAP   BENPCT,ACSALARY     BENEFIT PERCENTAGE INTO TABLE                
         B     EMPBEN03            GO LOOK FOR NEXT 52 ELEMENT                  
*                                                                               
ADMINP   TM    ACSALTYP,X'09'      MAKE SURE ITS A ADMIN PCT                    
         BZ    EMPBEN03            NO - ITS NOT GO GET NEXT 52 EL               
         MVC   ADMNSTAT,ACSALSTA   HOW MANY DECIMAL PLACES                      
         ZAP   ADMNPCT,ACSALARY    ADMIN  PERCENTAGE INTO TABLE                 
         B     EMPBEN03            GO LOOK FOR NEXT 52 ELEMENT                  
         EJECT ,                                                                
*              ROUTINE TO ADD BENEFIT  TABLE                                    
         SPACE 1                                                                
         USING CODED,R6                                                         
         SPACE 1                                                                
BENIN    NTR1                                                                   
         L     R6,ALLOC            FIND A MATCH IN ALLOCATED CODE LIST          
         SR    R0,R0                                                            
*                                                                               
BEN6     CLI   0(R6),0             NO MATCH FOUND A SLOT                        
         BE    BEN8                                                             
         CLC   CODE(7),=CL7'AAAA'                                               
         BE    BEN9                FOUND A MATCH                                
         LA    R6,CODELEN(R6)                                                   
         AH    R0,=H'1'                                                         
         B     BEN6                                                             
*                                                                               
BEN8     MVC   CODE(7),=CL7'AAAA'  MOVE NEW CODE                                
         MVC   CODENME,=CL36'EMPLOYEE BENEFITS'                                 
         LA    R6,CODELEN(R6)                                                   
         XC    CODE(CODELEN),CODE  CLEAR NEXT ENRTY                             
*                                                                               
         USING COSTD,R5                                                         
*                                                                               
BEN9     L     R5,DEPTC                                                         
         LA    R5,EXPENSES                                                      
*                                                                               
BEN10    CLI   0(R5),0             FIND A SLOT IN DEPT EXPENSE LIST             
         BE    BEN11                                                            
         LA    R5,8(,R5)                                                        
         B     BEN10                                                            
*                                                                               
BEN11    MVI   0(R5),C'A'          ALLOCATED                                    
         STC   R0,1(,R5)           DISP. IN CODE NAME TABLE                     
         LA    R7,ACCUMS           ADDR OF DEPT SAL ACCUMS IN R7                
         LA    R8,QTRTAB           TABLE INTO R8                                
         ZIC   R4,QTRCOUNT         NUMBER OF QTRS IN REQ INTO R4                
*                                                                               
BEN12    CP    BENPCT,=P'0'        IS THERE A BENEFIT PERCENTAGE                
         BE    BEN14               NO - RESET FOR NEXT QTR CALC                 
*                                                                               
BEN13    ZAP   DIVWRK,5(6,R8)      BENEFIT PCT                                  
         MP    DIVWRK,0(6,R7)      X DEPT SALARY FOR THAT QTR                   
         TM    BENSTAT,X'10'       IS IT 5 DECIMAL PLACES                       
         BZ    BEN13A              NO - ASSUME ITS 2                            
         SRP   DIVWRK,64-7,5       PERCENTAGE IS 5 DECIMAL PLACES               
         B     BEN13B                                                           
*                                                                               
BEN13A   SRP   DIVWRK,64-4,5       PERCENTAGE IS 2 DECIMAL PLACES               
*                                                                               
BEN13B   AP    DEPSAL,DIVWRK+10(6)                                              
*                                                                               
BEN14    LA    R8,QTRLNQ(,R8)      NEXT QTR ST/END DATES                        
         LA    R7,6(,R7)           NEXT QTR ACCUM                               
         ZAP   DIVWRK,=P'0'        CLEAR DIVWRK                                 
         BCT   R4,BEN12            THE NUMBER OF QTRS IN REQ PERIOD             
*                                                                               
         ZAP   2(6,R5),DEPSAL      MOVE DEPT BENEFIT TOTAL INTO EXP             
*                                  LIST                                         
         ZAP   DEPSAL,=P'0'        CLEAR DEPT SAL                               
         B     DCSXIT                                                           
         EJECT ,                                                                
*              ROUTINE TO ADD ADMINISTRATIVE TO TABLE                           
         SPACE 1                                                                
         USING CODED,R6                                                         
         SPACE 1                                                                
ADMIN    NTR1                                                                   
         L     R6,ALLOC            FIND A MATCH IN ALLOCATED CODE LIST          
         SR    R0,R0                                                            
*                                                                               
ADM6     CLI   0(R6),0             NO MATCH FOUND A SLOT                        
         BE    ADM8                                                             
         CLC   CODE(7),=CL7'AAAAA'                                              
         BE    ADM9                FOUND A MATCH                                
         LA    R6,CODELEN(,R6)                                                  
         AH    R0,=H'1'                                                         
         B     ADM6                                                             
*                                                                               
ADM8     MVC   CODE(7),=CL7'AAAAA'  MOVE NEW CODE                               
         MVC   CODENME,=CL36'ADMINISTRATIVE'                                    
         LA    R6,CODELEN(R6)                                                   
         XC    CODE(CODELEN),CODE  CLEAR NEXT ENRTY                             
*                                                                               
         USING COSTD,R5                                                         
*                                                                               
ADM9     L     R5,DEPTC                                                         
         LA    R5,EXPENSES                                                      
*                                                                               
ADM10    CLI   0(R5),0             FIND A SLOT IN DEPT EXPENSE LIST             
         BE    ADM11                                                            
         LA    R5,8(,R5)                                                        
         B     ADM10                                                            
*                                                                               
ADM11    MVI   0(R5),C'A'          ALLOCATED                                    
         STC   R0,1(,R5)           DISP. IN CODE NAME TABLE                     
         LA    R7,ACCUMS           ADDR OF DEPT SAL ACCUMS IN R7                
         LA    R8,QTRTAB           TABLE INTO R8                                
         ZIC   R4,QTRCOUNT         NUMBER OF QTRS IN REQ                        
*                                                                               
ADM12    CP    ADMNPCT,=P'0'       IS THERE A ADMIN PERCENTAGE                  
         BE    ADM14               NO - RESET FOR NEXT QTR CALC                 
*                                                                               
ADM13    ZAP   DIVWRK,12(6,R8)     ADMIN PCT                                    
         MP    DIVWRK,0(6,R7)      X DEPT SALARY FOR THAT QTR                   
         TM    ADMNSTAT,X'10'      IS IT 5 DECIMAL PLACES                       
         BZ    ADM13A              NO - ASSUME ITS 2                            
         SRP   DIVWRK,64-7,5       PERCENTAGE IS 5 DECIMAL PLACES               
         B     ADM13B                                                           
*                                                                               
ADM13A   SRP   DIVWRK,64-4,5       PERCENTAGE IS 2 DECIMAL PLACES               
*                                                                               
ADM13B   AP    DEPSAL,DIVWRK+10(6)                                              
*                                                                               
ADM14    LA    R8,QTRLNQ(,R8)      NEXT QTR ST/END DATES                        
         LA    R7,6(,R7)           NEXT QTR ACCUM                               
         ZAP   DIVWRK,=P'0'        CLEAR DIVWRK                                 
         BCT   R4,ADM12            THE NUMBER OF QTRS IN REQ PERIOD             
*                                                                               
         ZAP   2(6,R5),DEPSAL      MOVE DEPT BENEFIT TOTAL INTO EXP             
*                                  LIST                                         
         ZAP   DEPSAL,=P'0'        CLEAR DEPT SAL                               
         B     DCSXIT                                                           
         EJECT ,                                                                
*              ROUTINE TO POST DIRECT EXPENSES                                  
         SPACE 1                                                                
         USING CODED,R6                                                         
         SPACE 1                                                                
POSTLAST NTR1                                                                   
         CP    SAVECOST,=P'0'                                                   
         BE    DCSXIT              NOTHING TO POST                              
         L     R6,DIRECT                                                        
         SR    R0,R0               FIND A SLOT IN DIRECT CODE LIST              
*                                                                               
POSTL2   CLI   0(R6),0                                                          
         BE    POSTL5                                                           
         CLC   SAVEACC,CODE                                                     
         BE    POSTL7                                                           
         LA    R6,CODELEN(,R6)                                                  
         AH    R0,=H'1'                                                         
         B     POSTL2                                                           
*                                                                               
POSTL5   MVC   CODE(7),SAVEACC                                                  
         MVC   CODENME,SAVEACCN                                                 
         LA    R6,CODELEN(,R6)                                                  
         XC    CODE(CODELEN),CODE  CLEAR NEXT ENTRY                             
*                                                                               
         USING COSTD,R5                                                         
*                                                                               
POSTL7   L     R5,DEPTC                                                         
         LA    R5,EXPENSES         FIND A SLOT IN DEPT EXPENSE                  
*                                                                               
POSTL10  CLI   0(R5),0                                                          
         BE    POSTL15                                                          
         LA    R5,8(,R5)                                                        
         B     POSTL10                                                          
*                                                                               
POSTL15  MVI   0(R5),C'D'          DIRECT                                       
         STC   R0,1(,R5)           DISP. IN CODE TABLE                          
         AP    2(6,R5),SAVECOST                                                 
         B     DCSXIT                                                           
         EJECT ,                                                                
*              ROUTINE TO ADD DEPT TO REQUEST TOTALS                            
         SPACE 1                                                                
         USING COSTD,R5                                                         
         SPACE 1                                                                
ADDUP    NTR1                                                                   
         L     R5,DEPTC                                                         
         L     R6,REQC          ADD DEPT SALARIES TO REQUEST SALARIES           
         LA    R2,REGULAR                                                       
         LA    R4,REGULAR-COSTD(,R6)                                            
         LA    R3,SALCNT                                                        
*                                                                               
ADDUP1   AP    0(6,R4),0(6,R2)                                                  
         LA    R4,6(,R4)                                                        
         LA    R2,6(,R2)                                                        
         BCT   R3,ADDUP1                                                        
*                                                                               
         LA    R2,EXPENSES      ADD DEPT EXPENSES TO REQUEST EXPENSES           
         LA    R3,EXPCNT                                                        
*                                                                               
ADDUP2   LA    R4,EXPENSES-COSTD(R6)                                            
         CLI   0(R2),0             END OF DEPT EXPENSES                         
         BE    DCSXIT                                                           
*                                                                               
ADDUP3   CLC   0(2,R2),0(R4)       MATCH TYPE DEPT VS REQ                       
         BNE   ADDUP5                                                           
         AP    2(6,R4),2(6,R2)     ADD DEPT EXP. TO REQUEST                     
*                                                                               
ADDUP4   LA    R2,8(,R2)                                                        
         BCT   R3,ADDUP2           NEXT DEPT EXPENSE                            
         B     DCSXIT                                                           
*                                                                               
ADDUP5   LA    R4,8(,R4)           NEXT REQUEST ENRTY                           
         CLI   0(R4),0                                                          
         BNE   ADDUP3                                                           
         MVC   0(8,R4),0(R2)       NEW ENTRY IN REQ TABLE                       
         B     ADDUP4                                                           
         EJECT ,                                                                
*              ROUTINE TO PRINT A PAGE                                          
         SPACE 1                                                                
         USING COSTD,R5                                                         
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         L     R5,0(,R1)                                                        
         LA    R2,REGULAR          SEE IF ANYTHING TO PRINT                     
         LA    R3,SALCNT                                                        
*                                                                               
PRNT05   CP    0(6,R2),=P'0'                                                    
         BNE   PRNT10                                                           
         LA    R2,6(,R2)                                                        
         BCT   R3,PRNT05                                                        
*                                                                               
         LA    R2,EXPENSES                                                      
         LA    R3,EXPCNT                                                        
*                                                                               
PRNT07   CP    2(6,R2),=P'0'                                                    
         BNE   PRNT10                                                           
         LA    R2,8(,R2)                                                        
         BCT   R3,PRNT07                                                        
         B     DCSXIT              NOTHING TO PRINT                             
*                                                                               
PRNT10   MVI   FORCEHED,C'Y'                                                    
         MVC   HEAD8+1(18),=C'DEPARTMENTAL COSTS'                               
         ZAP   TOTSAL,=P'0'                                                     
         ZAP   TOTALC,=P'0'                                                     
         ZAP   TOTDIR,=P'0'                                                     
         ZAP   TOTALL,=P'0'                                                     
*                                                                               
         LA    R2,REGULAR          ADD UP SALARIES                              
         LA    R3,SALCNT                                                        
*                                                                               
PRNT10A  AP    TOTSAL,0(6,R2)                                                   
         LA    R2,6(,R2)                                                        
         BCT   R3,PRNT10A                                                       
*                                                                               
         CP    TOTSAL,=P'0'                                                     
         BE    PRNT20              NO SALARIES                                  
         MVC   P+3(8),=C'SALARIES'                                              
         GOTO1 UNDERLIN,DMCB,(8,P+3),PSECOND+3                                  
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
         LA    R2,REGULAR                                                       
         LA    R4,SALTYPE                                                       
         LA    R3,SALCNT                                                        
*                                                                               
PRNT11   CP    0(6,R2),=P'0'       PRINT SALARIES                               
         BE    PRNT13                                                           
         MVC   P+6(10),0(R4)                                                    
         BAS   RE,FRMT1                                                         
         BAS   RE,REPORT                                                        
*                                                                               
PRNT13   LA    R2,6(,R2)                                                        
         LA    R4,10(,R4)                                                       
         BCT   R3,PRNT11                                                        
         MVC   P+15(14),=C'TOTAL SALARIES'                                      
         LA    R2,TOTSAL                                                        
         BAS   RE,FRMT2                                                         
         MVI   SPACING,3                                                        
         BAS   RE,REPORT           TOTAL SALARIES                               
         AP    TOTALL,TOTSAL                                                    
*                                                                               
PRNT20   LA    R2,EXPENSES                                                      
         LA    R3,EXPCNT                                                        
*                                                                               
PRNT22   LA    R4,TOTALC           ADD UP EXPENSES BY TYPE                      
         CLI   0(R2),C'A'                                                       
         BE    *+8                                                              
         LA    R4,TOTDIR                                                        
         AP    0(6,R4),2(6,R2)                                                  
         AP    TOTALL,2(6,R2)                                                   
         LA    R2,8(,R2)                                                        
         BCT   R3,PRNT22                                                        
         CP    TOTALC,=P'0'                                                     
         BNE   PRNT25                                                           
         CP    TOTDIR,=P'0'                                                     
         BE    PRNT40              NO EXPENSES                                  
*                                                                               
PRNT25   MVC   P+3(8),=C'EXPENSES'                                              
         GOTO1 UNDERLIN,DMCB,(8,P+3),PSECOND+3                                  
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
         CP    TOTALC,=P'0'                                                     
         BE    PRNT30              NO ALLOCATED                                 
         MVC   P+6(18),=C'ALLOCATED EXPENSES'                                   
         GOTO1 UNDERLIN,DMCB,(18,P+6),PSECOND+6                                 
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
         LA    R2,EXPENSES                                                      
         LA    R3,EXPCNT                                                        
*                                                                               
PRNT26   CLI   0(R2),C'A'          FIRST ALLOCATED                              
         BNE   PRNT28                                                           
         CP    2(6,R2),=P'0'                                                    
         BE    PRNT28                                                           
         ZIC   R6,1(,R2)           ENTRY IN TABLE                               
         LA    R7,CODELEN                                                       
         MR    R6,R6                                                            
         L     R6,ALLOC            TABLE OF ALLOCATED CODES                     
         AR    R6,R7                                                            
*                                                                               
         USING CODED,R6                                                         
*                                                                               
         MVC   P+10(36),CODENME                                                 
         LA    R2,2(,R2)                                                        
         BAS   RE,FRMT1                                                         
         BAS   RE,REPORT                                                        
         SH    R2,=H'2'                                                         
*                                                                               
PRNT28   LA    R2,8(,R2)                                                        
         BCT   R3,PRNT26                                                        
         MVC   P+15(24),=C'TOTAL ALLOCATED EXPENSES'                            
         LA    R2,TOTALC                                                        
         BAS   RE,FRMT2                                                         
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
PRNT30   CP    TOTDIR,=P'0'                                                     
         BE    PRNT40                                                           
         MVC   P+6(15),=C'DIRECT EXPENSES'                                      
         GOTO1 UNDERLIN,DMCB,(15,P+6),PSECOND+6                                 
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
         LA    R2,EXPENSES                                                      
         LA    R3,EXPCNT                                                        
*                                                                               
PRNT32   CLI   0(R2),C'D'                                                       
         BNE   PRNT35              NOW DIRECT                                   
         CP    2(6,R2),=P'0'                                                    
         BE    PRNT35                                                           
         ZIC   R6,1(,R2)                                                        
         LA    R7,CODELEN                                                       
         MR    R6,R6                                                            
         L     R6,DIRECT                                                        
         AR    R6,R7                                                            
*                                                                               
         USING CODED,R6                                                         
*                                                                               
         MVC   P+10(36),CODENME                                                 
         LA    R2,2(,R2)                                                        
         BAS   RE,FRMT1                                                         
         BAS   RE,REPORT                                                        
         SH    R2,=H'2'                                                         
*                                                                               
PRNT35   LA    R2,8(,R2)                                                        
         BCT   R3,PRNT32                                                        
         MVC   P+15(21),=C'TOTAL DIRECT EXPENSES'                               
         LA    R2,TOTDIR                                                        
         BAS   RE,FRMT2                                                         
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
PRNT40   MVC   P+15(23),=C'TOTAL DEPARTMENTAL COST'                             
         LA    R2,TOTALL                                                        
         BAS   RE,FRMT2                                                         
         BAS   RE,REPORT                                                        
         B     DCSXIT                                                           
         EJECT ,                                                                
*              SET UP HEADLINES - FORMAT DATA                                   
         SPACE 1                                                                
REPORT   NTR1                                                                   
         MVC   HEAD4+82(L'PERIOD),PERIOD                                        
         MVC   HEAD4+1(L'COMP),COMP                                             
         CLI   MODE,REQLAST                                                     
         BE    *+10                                                             
         MVC   HEAD5+1(L'DEPT),DEPT                                             
         GOTO1 ACREPORT                                                         
         B     DCSXIT                                                           
*                                                                               
FRMT1    EDIT  (P6,0(R2)),(14,P+56),2,MINUS=YES,COMMAS=YES                      
         BR    RE                                                               
*                                                                               
FRMT2    EDIT  (P6,0(R2)),(14,P+56),2,MINUS=YES,COMMAS=YES                      
         BR    RE                                                               
         EJECT ,                                                                
*              ROUTINE TO CHANGE ACCOUNT CODES AND NAMES                        
         SPACE 1                                                                
ACCFIX   NTR1                                                                   
         MVC   ACCODE,SPACES                                                    
         L     R4,ADACC                                                         
         MVC   ACCODE(7),8(R4)                                                  
         L     R4,ADACCNAM                                                      
         LA    R6,ACCNAME                                                       
         BAS   RE,NAMOUT                                                        
         LA    R5,ACCTAB                                                        
         L     R4,ADACC                                                         
*                                                                               
ACCFIX1  CLI   0(R5),X'FF'         END OF TABLE                                 
         BE    DCSXIT                                                           
         CLC   8(7,R4),0(R5)                                                    
         BE    ACCFIX3                                                          
         LA    R5,26(,R5)                                                       
         B     ACCFIX1                                                          
*                                                                               
ACCFIX3  MVC   ACCODE,SPACES                                                    
         MVC   ACCNAME,SPACES                                                   
         MVC   ACCODE,7(R5)                                                     
         MVC   ACCNAME(12),14(R5)                                               
         B     DCSXIT                                                           
         EJECT ,                                                                
*              SUBROUTINES                                                      
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
         USING ACNAMED,R4                                                       
*                                                                               
NAMOUT   LTR   R4,R4                                                            
         BZR   RE                                                               
         MVC   0(36,R6),SPACES                                                  
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         EXMVC R3,0(R6),ACNMNAME                                                
         BR    RE                                                               
*                                                                               
         USING COSTD,R5                                                         
*                                                                               
CLEAR    NTR1                                                                   
         L     R5,0(,R1)           CLEAR A COST TABLE                           
         LA    R2,REGULAR                                                       
         LA    R3,SALCNT                                                        
*                                                                               
CLEAR10  ZAP   0(6,R2),=P'0'                                                    
         LA    R2,6(,R2)                                                        
         BCT   R3,CLEAR10                                                       
*                                                                               
         LA    R2,EXPENSES                                                      
         LA    R3,EXPCNT                                                        
*                                                                               
CLEAR20  XC    0(2,R2),0(R2)                                                    
         ZAP   2(6,R2),=P'0'                                                    
         LA    R2,8(,R2)                                                        
         BCT   R3,CLEAR20                                                       
         B     DCSXIT                                                           
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
*              CONSTANTS                                                        
         SPACE 1                                                                
RELOTAB  DS    0A                                                               
         DC    V(SQUASHER)                                                      
         DC    A(ARECORD)                                                       
         DC    A(ADEPTC)                                                        
         DC    A(AREQC)                                                         
         DC    V(ACSLRY)                                                        
         DC    A(AALLOC)                                                        
         DC    A(ADIRECT)                                                       
         DC    V(UNDERLIN)                                                      
         DC    X'FF'                                                            
         SPACE 1                                                                
SALTYPE  DC    CL10'REGULAR'                                                    
         DC    CL10'OVERTIME'                                                   
         DC    CL10'TEMPORARY'                                                  
         DC    CL10'BONUS'                                                      
         SPACE 1                                                                
ERRMSG1  DC    CL45'** ERROR ** - ENDDATE MUST FALL ON END OF QTR'              
         SPACE 1                                                                
ACCTAB   DC    C'99987  ',C'99987  ',C'DEPARTMENTAL'                            
         DC    C'99988  ',C'99987  ',C'DEPARTMENTAL'                            
         DC    C'99989  ',C'99987  ',C'DEPARTMENTAL'                            
         DC    C'99991  ',C'99987  ',C'DEPARTMENTAL'                            
         DC    C'99992  ',C'99987  ',C'DEPARTMENTAL'                            
         DC    C'99993  ',C'99987  ',C'DEPARTMENTAL'                            
         DC    C'99994  ',C'99987  ',C'DEPARTMENTAL'                            
         DC    C'99995  ',C'99987  ',C'DEPARTMENTAL'                            
         DC    C'99996  ',C'99987  ',C'DEPARTMENTAL'                            
         DC    C'99997  ',C'99987  ',C'DEPARTMENTAL'                            
         DC    C'99998  ',C'99987  ',C'DEPARTMENTAL'                            
         DC    X'FF'                                                            
         EJECT ,                                                                
*              DSECT FOR WORKING STORAGE                                        
ACC6D    DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
SQUASHER DS    V                                                                
RECORD   DS    A                                                                
DEPTC    DS    A                                                                
REQC     DS    A                                                                
ACSLRY   DS    A                                                                
ALLOC    DS    A                                                                
DIRECT   DS    A                                                                
UNDERLIN DS    A                                                                
         SPACE 1                                                                
START    DS    CL3                                                              
END      DS    CL3                                                              
STEND    DS    CL4                                                              
STED     DS    CL4                                                              
PERIOD   DS    CL26                                                             
         SPACE 1                                                                
DATE1    DS    CL6                                                              
DATE2    DS    CL6                                                              
QTRSTART DS    CL3                                                              
QTREND   DS    CL3                                                              
         SPACE 1                                                                
ELCODE   DS    CL1                                                              
SALAREA  DS    CL200                                                            
         SPACE 1                                                                
TOTSAL   DS    PL6                                                              
TOTALC   DS    PL6                                                              
TOTDIR   DS    PL6                                                              
TOTALL   DS    PL6                                                              
DEPSAL   DS    PL6                                                              
         SPACE 1                                                                
COMP     DS    CL60                                                             
DEPT     DS    CL60                                                             
         SPACE 1                                                                
SAVEKEY  DS    CL32                                                             
SAVEDEPT DS    CL2                                                              
SAVEACC  DS    CL7                                                              
SAVEACCN DS    CL36                                                             
SAVECOST DS    PL6                                                              
WANT     DS    CL1                                                              
         SPACE 1                                                                
ACCODE   DS    CL7                                                              
ACCNAME  DS    CL36                                                             
         SPACE 1                                                                
PCTBENEF DS    PL3                                                              
PCTADMIN DS    PL3                                                              
DIVWRK   DS    PL16                                                             
         SPACE 1                                                                
QTRCOUNT DS    PL1                                                              
ACCUMS   DS    0C                                                               
QACCUM1  DS    PL6                                                              
QACCUM2  DS    PL6                                                              
QACCUM3  DS    PL6                                                              
QACCUM4  DS    PL6                                                              
         SPACE 1                                                                
QTRLNQ   EQU   18                                                               
QTRTAB   DS    CL(QTRLNQ*4+1)                                                   
         EJECT ,                                                                
*              DSECT FOR COST RECORD                                            
         SPACE 1                                                                
COSTD    DSECT                                                                  
REGULAR  DS    PL6                 REGULAR SALARIES                             
OVERTIME DS    PL6                 OVERTIME                                     
TEMPORY  DS    PL6                 TEMPORARY                                    
BONUS    DS    PL6                 BONUS                                        
SALCNT   EQU   (*-COSTD)/6                                                      
EXPENSES DS    25CL8                                                            
         ORG   EXPENSES                                                         
EXPTYPE  DS    CL1                 EXP. TYPE  A=ALLOCATED, D=DIRECT             
EXPENTRY DS    XL1                 NUMBER OF ENTRY IN ALLOC. OR DIRECT          
EXPAMNT  DS    PL6                 AMOUNT                                       
         DS    24CL8                                                            
EXPCNT   EQU   (*-EXPENSES)/8                                                   
COSTLEN  EQU   *-COSTD                                                          
         SPACE 1                                                                
*              DSECT FOR ALLOCATED AND DIRECT CODES                             
         SPACE 1                                                                
CODED    DSECT                                                                  
CODE     DS    CL10                                                             
CODENME  DS    CL36                                                             
CODELEN  EQU   *-CODED                                                          
         EJECT ,                                                                
ACC602   CSECT                                                                  
         ENTRY ARECORD                                                          
ARECORD  DS    0D                                                               
         DS    CL42                                                             
         DS    CL1000                                                           
         SPACE 1                                                                
         ENTRY ADEPTC                                                           
ADEPTC   DS    0D                                                               
         DS    (COSTLEN)C                                                       
         SPACE 1                                                                
         ENTRY AREQC                                                            
AREQC    DS    0D                                                               
         DS    (COSTLEN)C                                                       
         SPACE 1                                                                
         ENTRY AALLOC                                                           
AALLOC   DS    0D                                                               
         DS    (25*CODELEN)C                                                    
         SPACE 1                                                                
         ENTRY ADIRECT                                                          
ADIRECT  DS    0D                                                               
         DS    (25*CODELEN)C                                                    
         EJECT ,                                                                
PERCENTD DSECT                     ***TABLE OF QTR PERCENTAGES***               
QTRST    DS    CL2                 START YYMM OF QUARTER                        
QTRED    DS    CL2                 END   YYMM OF QUARTER                        
BENSTAT  DS    CL1                 X'20'=BEN PCT 2 DP,  X'10'=5 DP              
BENPCT   DS    PL6                 BENEFIT PERCENTAGE                           
ADMNSTAT DS    CL1                 X'20'=ADMN PCT 2 DP, X'10'=5 DP              
ADMNPCT  DS    PL6                 ADMINISTRATIVE PERCENTAGE                    
         EJECT ,                                                                
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*DDSLRD                                                                         
         PRINT OFF                                                              
       ++INCLUDE DDSLRD                                                         
         PRINT ON                                                               
*ACMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050ACREPC602 05/01/02'                                      
         END                                                                    
