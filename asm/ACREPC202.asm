*          DATA SET ACREPC202  AT LEVEL 004 AS OF 05/01/02                      
*PHASE ACC202A,+0                                                               
*INCLUDE SORTER                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE CENTER                                                                 
*INCLUDE SQUASHER                                                               
*INCLUDE UNDERLIN                                                               
*INCLUDE ACSLRY                                                                 
         TITLE 'ACC202 - TIME SHEET EDIT'                                       
ACC202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACC2**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GENERAL W/S)                            
         LA    RC,SPACEND                                                       
         USING PROGD,RC            RC=A(LOCAL W/S)                              
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BNE   CA10                                                             
         B     XIT                                                              
         SPACE 2                                                                
XIT      XMOD1 1                                                                
         EJECT                                                                  
CA10     CLI   MODE,REQFRST                                                     
         BNE   CA80                                                             
         LA    R1,SRTKYLNQ              LENGTH OF SORT KEY                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
         LA    R1,SRTRCLNQ              LENGTH OF SORT RECORD                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(3),DUB+6(2)                                           
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         SPACE 1                                                                
         MVC   WORK(6),QSTART                                                   
         MVC   WORK+4(2),=C'28'                                                 
         LA    R4,1                                                             
         CLI   QOPT6,C'N'                                                       
         BNE   CA10A                                                            
         CLI   QOPT1,C' '                                                       
         BNE   CA10A                                                            
         MVI   QOPT1,C'Y'                                                       
         SPACE 1                                                                
CA10A    EQU   *                                                                
         BAS   RE,DTADD            FIND END DATE OF MONTH                       
         CLC   WORK(4),WORK+6      SAME MONTH                                   
         BNE   CA10B               FOUND LAST DAT OF THE MONTH                  
         MVC   WORK(6),WORK+6                                                   
         B     CA10A                                                            
CA10B    EQU   *                                                                
         GOTO1 DATCON,DMCB,(0,WORK),(1,RQDATE)                                  
         MVI   RCSUBPRG,0                                                       
         CLI   QOPT1,C'P'                                                       
         BNE   *+8                                                              
         MVI   RCSUBPRG,1                                                       
         CLI   QOPT6,C'N'                                                       
         BNE   *+8                                                              
         MVI   RCSUBPRG,2                                                       
         CLI   QOPT4,C'W'                                                       
         BNE   CA11                                                             
         LA    R2,DEPTOT                                                        
         LA    R3,7                                                             
CLRTOT   ZAP   0(6,R2),=P'0'                                                    
         LA    R2,6(R2)                                                         
         BCT   R3,CLRTOT                                                        
CA11     XC    DATAB,DATAB                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   BASIS,C'B'          BROADCAST MONTH                              
*                                  SET UP DEFAULT PARAMETERS                    
         MVI   PTYPE,C'W'          WEEKLY                                       
         MVI   TYPE,C'P'           PERCENT-BASED                                
         MVI   UNITS,100           UNITS PER PERIOD = 100                       
         OC    PROGPROF(4),PROGPROF                                             
         BZ    CA14                                                             
         LA    RE,4                                                             
         LA    RF,PROGPROF                                                      
         LA    R1,OPTIONS                                                       
CA12     CLI   0(RF),0                                                          
         BE    *+10                                                             
         MVC   0(1,R1),0(RF)                                                    
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   RE,CA12                                                          
         SPACE 1                                                                
CA14     CLI   QOPT4,C'D'          SPECIAL DAILY                                
         BNE   CA15                                                             
         MVI   PTYPE,C'D'                                                       
         MVI   BASIS,C'M'                                                       
         MVI   TYPE,C'H'                                                        
         MVI   PERNO,5                                                          
         PACK  DUB,QSELECT(2)      UNITS                                        
         CVB   R1,DUB                                                           
         STC   R1,UNITS                                                         
         B     CA79                                                             
         SPACE 1                                                                
CA15     MVI   PERNO,4             4-PERIODS IS DEFAULT                         
         CLI   QOPT2,C' '                                                       
         BE    CA20                                                             
         PACK  DUB,QOPT2(1)        OPTION 2 CAN OVERRIDE                        
         CVB   R1,DUB                                                           
         STC   R1,PERNO                                                         
         SPACE 1                                                                
CA20     CLC   QSTART,SPACES                                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   QOPT4,C'W'                                                       
         BNE   *+10                                                             
         MVC   SAVEQSTR,QSTART                                                  
         MVC   QSTART+4(2),=C'15'  15TH IS IN MIDDLE OF MONTH                   
         GOTO1 DATCON,DMCB,(0,QSTART),(1,DATE3)                                 
         SPACE 1                                                                
         CLI   QOPT2,C' '                                                       
         BNE   CA21                                                             
         CLI   BASIS,C'B'          BROADCAST MONTH BASIS                        
         BNE   CA21                                                             
         CLI   QOPT4,C'W'                                                       
         BE    CA21                                                             
         GOTO1 GETBROAD,DMCB,(1,QSTART),WORK,GETDAY,ADDAY                       
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R2,DMCB             4 OR 5 WEEKS IN MONTH                        
         STC   R2,PERNO                                                         
         SPACE 1                                                                
CA21     BAS   RE,COLHEAD          CHECK FOR HOURS ELEMENT                      
         CLI   QOPT4,C'W'                                                       
         BE    CA36                                                             
         CLI   BASIS,C'B'                                                       
         BNE   CA30                                                             
         GOTO1 GETBROAD,DMCB,(1,QSTART),WORK,GETDAY,ADDAY                       
         LA    R3,BROADST                                                       
         BAS   RE,DTCONV                                                        
         MVC   WORK+12(6),WORK                                                  
         MVC   WORK(6),WORK+6                                                   
         LA    R3,BROADND                                                       
         BAS   RE,DTCONV                                                        
         MVC   WORK(6),WORK+12                                                  
         SPACE 1                                                                
         BAS   RE,DATEUP           WORK OUT COLUMN DATES                        
         B     CA40                                                             
         SPACE 1                                                                
CA30     DS    0H                                                               
         CLI   BASIS,C'M'          MONTHLY BASIS                                
         BNE   XIT                                                              
         MVC   QSTART+4(2),=C'01'                                               
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'28'                                                 
         SPACE 1                                                                
         LA    R4,1                                                             
CA32     BAS   RE,DTADD            FIND END DATE OF MONTH                       
         CLC   WORK(4),WORK+6      SAME MONTH                                   
         BNE   CA34                                                             
         MVC   WORK(6),WORK+6      YES-BUMP ANOTHER DAY                         
         B     CA32                                                             
CA34     LA    R3,BROADND          NO-TUCK AWAY END DATE                        
         BAS   RE,DTCONV                                                        
         MVC   WORK(6),QSTART                                                   
         BAS   RE,DATEUP           WORK OUT COLUMN DATES                        
         MVC   DATAB+21(3),BROADND FORCE END OF MONTH                           
         B     CA40                                                             
         SPACE 2                                                                
CA36     DS    0H                                                               
         MVC   WORK(6),SAVEQSTR                                                 
         LA    R3,6                           NUMBER OF DAYS BACK               
         LNR   R4,R3                          MAKE NEGATIVE                     
         BAS   RE,DTADD                                                         
         MVC   WORK(6),WORK+6                 BEGINNING OF MONTH                
         MVC   SAVEFRST,WORK                                                    
         LA    R3,BROADST                                                       
         BAS   RE,DTCONV                                                        
         LA    R3,DATAB                                                         
         MVC   0(3,R3),BROADST                FIRST BEGINNING DATE              
         MVC   WORK(6),SAVEFRST                                                 
         LA    R4,6                                                             
         BAS   RE,DTADD                                                         
         MVC   WORK(6),WORK+6                                                   
         LA    R3,3(R3)                                                         
         BAS   RE,DTCONV                                                        
         ZIC   R5,PERNO                                                         
CA39     LA    R4,1                                                             
         BAS   RE,DTADD                                                         
         MVC   WORK(6),WORK+6                                                   
         LA    R3,3(R3)                                                         
         BAS   RE,DTCONV                                                        
         LA    R4,6                                                             
         BAS   RE,DTADD                                                         
         MVC   WORK(6),WORK+6                                                   
         LA    R3,3(R3)                                                         
         BAS   RE,DTCONV                                                        
         BCT   R5,CA39                                                          
         SPACE 2                                                                
CA40     DS    0H                                                               
         CLI   QOPT7,C'T'          TRANSACTION MONTH REQUESTED ?                
         BE    CA60                YES                                          
         CLI   QOPT7,C'B'          NO, BATCH MONTH ?                            
         BE    CA70                YES                                          
*                                                                               
CA50     CLI   PROGPROF+6,C'T'     NO, PROFILE FOR TRANSACTION MONTH ?          
         BNE   CA70                NO                                           
*                                                                               
CA60     GOTO1 DATCON,DMCB,(0,QSTART),(1,MOS)                                   
         B     CA71                                                             
*                                                                               
CA70     MVC   MOS(1),QSTART+1     YEAR                                         
         MVC   MOS+1(1),QSTART+3   MONTH                                        
         CLI   QSTART+2,C'1'                                                    
         BNE   CA71                                                             
         MVI   MOS+1,C'A'          BUILD MOS FOR UK OPTION                      
         CLI   QSTART+3,C'0'                                                    
         BE    CA71                                                             
         MVI   MOS+1,C'B'                                                       
         CLI   QSTART+3,C'1'                                                    
         BE    CA71                                                             
         MVI   MOS+1,C'C'                                                       
         EJECT                                                                  
CA71     DS    0H                                                               
         CLI   PTYPE,C'D'                                                       
         BE    CA79                                                             
         CLI   PTYPE,C'W'          ADJUST DATE TABLE - IF NECESSARY             
         BE    CA78                                                             
         CLI   PTYPE,C'H'                                                       
         BE    CA72                                                             
         LA    RF,DATAB+21                                                      
         CLI   PERNO,4                                                          
         BNH   *+8                                                              
         LA    RF,BROADND                                                       
         MVC   DATAB+3(3),0(RF)    FOR MONTHLY,TABLE IS 1-WIDE                  
         XC    DATAB+6(6),DATAB+6                                               
         B     CA78                                                             
         SPACE 1                                                                
CA72     MVC   DATAB+3(3),DATAB+9  FOR HALF MONTH,TABLE IS 2-WIDE               
         MVC   DATAB+9(3),BROADND                                               
         MVI   DATAB+08,X'16'                                                   
         MVI   DATAB+05,X'15'                                                   
         XC    DATAB+12(18),DATAB+12                                            
         B     CA78                                                             
CA78     BAS   RE,HEADBLD                                                       
         B     XIT                                                              
         SPACE 1                                                                
CA79     MVC   WORK(6),QSTART                                                   
         BAS   RE,DATEUP           WORK OUT COLUMN DATES                        
         BAS   RE,COLHEAD                                                       
         SPACE 1                                                                
CA79A    BAS   RE,HEADBLD                                                       
         MVC   WEEKSTRT(3),DATAB+3    SAVE START DATE OF REQUEST WEEK           
         MVC   WEEKEND(3),DATAB+39    SAVE WEEK ENDING DATE                     
         B     XIT                                                              
         EJECT                                                                  
CA80     CLI   MODE,LEDGFRST       FIRST FOR LEDGER                             
         BNE   CA90                                                             
         MVI   ELCODE,X'16'                                                     
         L     R4,ADLEDGER                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACHEIRD,R4                                                       
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVC   LVCDSP,ACHRLEVB     DISPLACEMENT TO LEVEL C                      
         MVC   LVDDSP,ACHRLEVC     DISPLACEMENT TO LEVEL D                      
         MVC   LVALN,ACHRLEVA      LEVEL A LENGTH                               
         IC    RF,ACHRLEVB                                                      
         IC    RE,ACHRLEVA                                                      
         SR    RF,RE                                                            
         STC   RF,LVBLN            LEVEL B LENGTH                               
         IC    RF,ACHRLEVC                                                      
         IC    RE,ACHRLEVB                                                      
         SR    RF,RE                                                            
         STC   RF,LVCLN            LEVEL C LENGTH                               
         IC    RF,ACHRLEVD                                                      
         IC    RE,ACHRLEVC                                                      
         SR    RF,RE                                                            
         STC   RF,LVDLN            LEVEL D LENGTH                               
         B     XIT                                                              
         EJECT                                                                  
CA90     DS    0H                                                               
         CLI   MODE,LEVBFRST       FIRST FOR DEPT                               
         BNE   CA100                                                            
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
CA100    CLI   MODE,PROCACC                                                     
         BNE   CA150                                                            
         L     R2,ADACC                                                         
         MVI   FCRDTRNS,C'N'                                                    
         CLI   3(R2),C'9'          IGNORE OVERHEAD ACCOUNTS                     
         BE    XIT                                                              
         CLC   4(2,R2),=C'999'                                                  
         BE    XIT                                                              
         CLC   8(3,R2),=C'999'                                                  
         BE    XIT                                                              
         BAS   RE,PARSE            GET ACCOUNT LEVELS                           
         CLI   QOPT3,C' '          LOCKED OPTION                                
         BE    CA101E                                                           
         L     RF,ADACCSTA                                                      
         USING ACSTATD,RF                                                       
         CLI   QOPT3,C'S'                                                       
         BE    CA101C                                                           
         TM    ACSTSTAT,X'20'      LOCKED ONLY                                  
         BZ    XIT                                                              
         B     CA101E                                                           
CA101C   CLI   PROGPROF+4,C'Y'                                                  
         BE    CA101E          DON'T SUPPRESS LOCKED IF ACTIVITY                
         TM    ACSTSTAT,X'20'      SUPPRESS LOCKED                              
         BO    XIT                                                              
CA101E   DS    0H                                                               
         MVI   FCRDTRNS,C'Y'                                                    
         LA    R3,7                                                             
         LA    R6,PERLINE                                                       
CA102    ZAP   0(6,R6),=P'0'                                                    
         LA    R6,6(R6)                                                         
         BCT   R3,CA102                                                         
         MVI   DATNO,0                                                          
         L     R5,ATABLE                                                        
         USING BIND,R5                                                          
         XC    BININ,BININ         CLEAR TABLE                                  
         BAS   RE,GETSAL           GET SALARY                                   
         B     XIT                                                              
         EJECT                                                                  
CA150    CLI   MODE,PROCTRNS                                                    
         BNE   CA300                                                            
         LA    R6,PACCS            CLEAR CONTRA ACCOUNT LINE                    
         LA    R3,7                                                             
CA152    ZAP   0(L'PACC1,R6),=P'0'                                              
         LA    R6,L'PACC1(R6)                                                   
         BCT   R3,CA152                                                         
         MVC   PNAME,SPACES                                                     
         L     RF,ADSUBAC                                                       
         USING TRSUBHD,RF                                                       
         MVC   PKEY,TRSBACNT                                                    
         ZIC   RE,TRSBLEN                                                       
         SH    RE,=H'18'                                                        
         BM    CA201                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PNAME(0),TRSBNAME                                                
         SPACE 1                                                                
CA201    L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         CLI   TRNSEL,X'44'                                                     
         BNE   XIT                                                              
         CLI   TRNSTYPE,X'1B'      TIME SHEETS                                  
         BE    CA202                                                            
         CLI   TRNSTYPE,49         TIME SHEETS                                  
         BE    CA202                                                            
         CLI   TRNSTYPE,X'29'                                                   
         BNE   XIT                                                              
         SPACE 1                                                                
CA202    CLI   PTYPE,C'D'          IS IT A DAILY                                
         BNE   CA204               NO - CHECK MOS                               
         CLC   TRNSDATE,WEEKSTRT   IS TRANS DATE BEFORE START OF WEEK           
         BL    XIT                 YES - WE DON'T WANT IT                       
         CLC   TRNSDATE,WEEKEND    IS TRANS DATE AFTER OUR 7DAY WEEK            
         BH    XIT                 YES - WE DON'T WANT IT                       
         B     CA208               DON'T CHECK MOS FOR DAILY                    
         SPACE 1                                                                
CA204    CLI   QOPT7,C'B'          BATCH MONTH REQUESTED ?                      
         BE    CA206               YES                                          
         CLI   QOPT7,C'T'          NO, TRANSACTION MONTH REQUESTED ?            
         BE    CA205               YES                                          
         CLI   PROGPROF+6,C'T'     NO, PROFILE FOR TRANSACTION MONTH            
         BNE   CA206               NO                                           
*                                                                               
CA205    CLC   TRNSDATE(2),MOS     YES, USE TRANSACTION MONTH                   
         BNE   XIT                                                              
         B     CA208                                                            
         SPACE 1                                                                
CA206    CLC   TRNSBTCH(2),MOS     USE BATCH MONTH                              
         BNE   XIT                                                              
         SPACE 1                                                                
CA208    CLI   QOPT1,C'P'          IS IT PROJECT REPORT                         
         BNE   CA210               BRANCH IF NOT                                
         MVI   ELCODE,X'51'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   CA208A              NO PROJECT ELEMENT                           
         LR    R3,R4               R3 TO 51 ELEMENT                             
         SPACE 1                                                                
         USING ACPCD,R3                                                         
         CLI   ACPCLEN,X'22'                                                    
         BL    CA208A              NO PROJECT CODE                              
         MVC   PKEY,ACPCPRJT       PROJECT CODE TO TABLE                        
         MVC   PNAME,SPACES                                                     
CA208A   L     R4,ADTRANS          RESET R4 TO TRANSACTION                      
         EJECT                                                                  
CA210    MVI   ELCODE,X'50'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   XIT                 NO HOURS                                     
         LR    R3,R4                                                            
         L     R4,ADTRANS                                                       
         USING TRCASHD,R3                                                       
         SPACE 1                                                                
         LA    RE,DATAB            FIND CORRECT BUCKET FOR HOURS                
         LA    RF,PACCS                                                         
         ZIC   RF,PERNO                                                         
         MH    RF,=H'6'                                                         
         LA    RF,PACCS(RF)        GET TO OTHER COLUMN                          
         SPACE 1                                                                
         CLI   PTYPE,C'D'                                                       
         BE    CA216               FOR DAILY                                    
         SPACE 1                                                                
         CLC   TRNSDATE,0(RE)                                                   
         BL    CA214                                                            
         ZIC   R1,PERNO                                                         
         LA    RF,PACCS                                                         
CA212    CLC   TRNSDATE,3(RE)                                                   
         BNH   CA214                                                            
         LA    RE,6(RE)                                                         
         LA    RF,L'PACC1(RF)                                                   
         BCT   R1,CA212            DEFAULT IS OTHER                             
         SPACE 1                                                                
CA214    AP    0(L'PACC1,RF),TRCSAMNT                                           
         ZIC   RF,PERNO                                                         
         AH    RF,=H'1'                                                         
         MH    RF,=H'6'                                                         
         LA    RF,PACCS(RF)                                                     
         AP    0(6,RF),TRCSAMNT      TOTAL COLUMN                               
         B     CA250                                                            
         SPACE 1                                                                
CA216    ZIC   R1,PERNO                                                         
         LA    RF,PACCS                                                         
CA218    CLC   TRNSDATE,0(RE)      FOR DAILY DROP                               
         BL    XIT                 IF NOT IN REQUEST PERIOD                     
         CLC   TRNSDATE,3(RE)                                                   
         BNH   CA214                                                            
         LA    RE,6(RE)                                                         
         LA    RF,L'PACC1(RF)                                                   
         BCT   R1,CA218            DEFAULT IS OTHER                             
         B     CA214               IF ITS NOT DAYS 1-5 (MON-FRI)                
*                                  IT MUST BE DAYS 6-7 (SAT-SUN)                
         EJECT                                                                  
CA250    CLC   PACCS,=7PL6'0'                                                   
         BE    XIT                 IF THERE IS ANY ACTIVITY                     
         GOTO1 BINADD,DMCB,PKEY,ATABLE   ADD THIS ITEM TO TABLE                 
         SPACE 1                                                                
         LA    RE,PERLINE                                                       
         LA    RF,PACCS            ADD TO PERSON TOTAL LINE                     
         LA    R1,7                                                             
CA254    AP    0(L'PERLINE,RE),0(L'PACC1,RF)                                    
         LA    RE,L'PERLINE(RE)                                                 
         LA    RF,L'PACC1(RF)                                                   
         BCT   R1,CA254                                                         
         B     XIT                                                              
         EJECT                                                                  
CA300    CLI   MODE,ACCLAST                                                     
         BNE   CA400                                                            
         CLI   FCRDTRNS,C'N'                                                    
         BE    XIT                                                              
         MVI   ACTFLAG,C'N'        RESET ACTIVITY FLAG TO NO                    
         L     R5,ATABLE                                                        
         USING BIND,R5                                                          
         LA    R2,BINTABLE                                                      
         MVC   PLINE,0(R2)                                                      
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BNZ   CA301               BRANCH IF SOME ACTIVITY                      
         CLI   QOPT3,C'S'                                                       
         BNE   CA300A                                                           
         L     RF,ADACCSTA                                                      
         USING ACSTATD,RF                                                       
         TM    ACSTSTAT,X'20'      SUPPRESS LOCKED IF NO ACTIVITY               
         BO    CA350                                                            
CA300A   CLI   QOPT1,C'M'          NO ACTIVITY IS OK FOR OPT 1 =M               
         BE    CA350                                                            
         B     *+8                                                              
CA301    EQU   *                                                                
         MVI   ACTFLAG,C'Y'        ACTIVITY FLAG IS YES                         
         CLI   QOPT1,C'Y'          PRINT EVERYONE                               
         BE    CA320                                                            
         CLI   QOPT1,C'T'          OPTION TO SHOW ONLY THOSE WITH NO            
*                                  TIMESHEET FOR ANY OR ALL REQUESTED           
*                                  PERIODS                                      
         BE    CA302                                                            
         ZIC   RF,PERNO                                                         
         MH    RF,=H'6'                                                         
         LA    RF,PERLINE(RF)      GET TO OTHER COLUMN                          
         CP    0(6,RF),=P'0'                                                    
         BNE   CA320               PRINT IF ANY 'OTHER'                         
         CLI   QOPT1,C'M'          IF MOS OPTION ONLY PRINT                     
         BE    CA350               IF SOMETHING IN OTHER                        
CA302    DS    0H                                                               
         LA    R3,7                                                             
         LA    R6,PERLINE                                                       
         LA    R7,PERUNITS                                                      
CA303    CP    0(6,R7),=P'0'       DON'T CHECK IF NO UNITS                      
         BE    CA311                                                            
CA304    CLI   TYPE,C'P'                                                        
         BNE   CA312                                                            
CA310    CP    0(6,R7),0(6,R6)                                                  
         BNE   CA320               MUST BE EQUAL FOR PCT BASIS                  
CA311    LA    R6,6(R6)                                                         
         LA    R7,6(R7)                                                         
         BCT   R3,CA303                                                         
         B     CA350                                                            
         SPACE 1                                                                
CA312    CLI   TYPE,C'H'           HOURLY TYPE                                  
         BNE   CA350                                                            
         CLI   QOPT1,C'T'          OPTION TO SHOW ONLY THOSE WITH NO            
*                                  TIMESHEET FOR ANY OR ALL REQESTED            
*                                  PERIODS                                      
         BNE   CA314                                                            
         CP    0(6,R6),=P'0'       NO TIME FOR THIS PERIOD COL?                 
         BE    CA320               THEN YOU WANT TO PRINT THIS GUY              
         B     CA311               IF TIME, THEN CHECK NEXT COL                 
CA314    CP    0(6,R7),0(6,R6)                                                  
         BH    CA320               PRINT IF LOW                                 
         B     CA311                                                            
         SPACE 1                                                                
CA320    DS    0H                                                               
         CLI   QOPT6,C'N'                                                       
         BNE   CA320A                                                           
         CP    SALARY,=P'0'                                                     
         BNE   CA350                                                            
         SPACE 1                                                                
*                                                                               
CA320A   L     R2,ADACC                                                         
         LA    R3,P+1                                                           
         SR    R1,R1                                                            
         IC    R1,LVCLN            LENGTH OF SUB-DEPT                           
         BCTR  R1,0                                                             
         SR    RF,RF                                                            
         IC    RF,LVCDSP           DISPLACEMENT TO SUB-DEPT                     
         LA    RE,3(RF,R2)         RE TO SUB-DEPT IN ACCOUNT                    
         EX    R1,*+4                                                           
         MVC   0(0,R3),0(RE)       SUB-DEPT TO P                                
         LA    R3,2(R1,R3)                                                      
         IC    R1,LVDLN            LENGTH OF PERSON CODE                        
         BCTR  R1,0                                                             
         IC    RF,LVDDSP           DISPLACEMENT TO PERSON                       
         LA    RE,3(RF,R2)         RE TO PERSON IN ACCOUNT                      
         EX    R1,*+4                                                           
         MVC   0(0,R3),0(RE)       PERSON CODE TO P                             
         MVC   SBDPER,P+1          SAVE SUB DEPARTMENT/PERSON CODE              
         L     R2,ADACCNAM                                                      
         ZIC   R3,1(R2)                                                         
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+16(0),2(R2)                                                    
         SPACE 1                                                                
         USING ACEMPD,R4              HIRE AND TERM DATE                        
         LR    R4,R2                                                            
         MVI   ELCODE,X'56'           GET HIRE AND FIRE DATE                    
         BAS   RE,NEXTEL                                                        
         BNE   CA320C                                                           
         CLC   ACEMPHIR,RQDATE                                                  
         BNH   CA320B1                PUT IN TABLE IN NOT LOW                   
         BAS   RE,HNTTABLE                                                      
         CLI   ACTFLAG,C'Y'                                                     
         BNE   CA350                                                            
CA320B1  EQU   *                                                                
         CLI   PROGPROF+5,C'B'        PRINT HIRE AND TERM DATES ?               
         BE    *+8                         (DEFAULT IS N)                       
         CLI   PROGPROF+5,C'H'        PRINT JUST HIRE DATES?                    
         BE    *+8                                                              
         CLI   PROGPROF+5,C'T'        PRINT JUST TERM. DATES?                   
         BNE   *+8                                                              
         BAS   RE,PRTHNT              PRINT                                     
         SPACE 1                                                                
CA320C   EQU   *                                                                
         CLI   QOPT5,C'Y'                                                       
         BNE   CA321                                                            
         BAS   RE,ONLYTOTS            ONLY TOTALS                               
         B     CA330                                                            
         SPACE 1                                                                
CA321    GOTO1 SQUASHER,DMCB,P+1,(0,55)                                         
         GOTO1 UNDERLIN,DMCB,(55,P+1),(0,PSECOND+1)                             
         BAS   RE,REPORT                                                        
         BAS   RE,REPORT                                                        
         L     R5,ATABLE                                                        
         USING BIND,R5                                                          
         LA    R2,BINTABLE                                                      
         MVC   PLINE,0(R2)                                                      
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BNZ   CA322                                                            
         MVC   P+42(20),=C'** NO TIME SHEETS **'                                
         CP    SALARY,=P'0'                                                     
         BE    *+10                                                             
         MVC   P+42(40),=CL40'** NO TIME SHEETS - SALARY PRESENT **'            
         BAS   RE,REPORT                                                        
         B     CA350                                                            
         SPACE 1                                                                
CA322    MVC   P+2(12),PKEY+3                                                   
         LA    R4,P+13                                                          
         LA    R6,25                                                            
CA323    CLI   0(R4),C' '          SQUASH IT TO LEFT                            
         BNE   CA324                                                            
         BCTR  R4,0                                                             
         AH    R6,=H'1'                                                         
         B     CA323                                                            
         SPACE 1                                                                
CA324    CLC   PNAME,SPACES                                                     
         BNE   *+8                                                              
         BAS   RE,GETNME           GET NAME FOR PROJECTS                        
         LA    RF,PNAME                                                         
         GOTO1 CHOPPER,DMCB,(36,0(RF)),((R6),2(R4)),(C'P',2)                    
         LA    R4,PACCS                                                         
         MVI   FTYPE,C'D'                                                       
         BAS   RE,FORMAT                                                        
         CLC   P+42(70),SPACES                                                  
         BNE   CA325                                                            
         MVC   P,SPACES                                                         
         B     CA326                                                            
         SPACE 1                                                                
CA325    BAS   RE,REPORT                                                        
CA326    LA    R2,L'PLINE(R2)                                                   
         MVC   PLINE,0(R2)                                                      
         BCT   R3,CA322                                                         
         SPACE 1                                                                
         BAS   RE,REPORT                                                        
         MVC   P+1(9),=C'TOTAL FOR'                                             
         MVC   P+11(L'SBDPER),SBDPER   SUB-DEPT / PERSON CODES                  
         LA    R4,PERLINE                                                       
         MVI   FTYPE,C'T'                                                       
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         BAS   RE,REPORT                                                        
         CP    SALARY,=P'0'                                                     
         BNE   *+8                                                              
         CLI   QOPT6,C'Y'                                                       
         BNE   *+10                                                             
         MVC   P+42(24),=CL24'** SALARY NOT PRESENT **'                         
         BAS   RE,REPORT                                                        
         BAS   RE,REPORT                                                        
         SPACE 1                                                                
CA330    CLI   QOPT4,C'W'                                                       
         BNE   CA350                                                            
         LA    R5,DEPTOT                                                        
         LA    R1,7                                                             
         LA    R2,PERLINE                                                       
ADDDEPT  AP    0(6,R5),0(6,R2)                                                  
         LA    R5,6(R5)                                                         
         LA    R2,6(R2)                                                         
         BCT   R1,ADDDEPT                                                       
CA350    B     XIT                                                              
         EJECT                                                                  
*              ONLY PRINT TOTAL LINES                                           
ONLYTOTS NTR1                                                                   
         GOTO1 SQUASHER,DMCB,P+1,(0,55)                                         
         L     R5,ATABLE                                                        
         USING BIND,R5                                                          
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BNZ   ONLYT4                                                           
         MVC   P+42(20),=C'** NO TIME SHEETS **'                                
         CP    SALARY,=P'0'                                                     
         BE    *+10                                                             
         MVC   P+42(40),=CL40'** NO TIME SHEETS - SALARY PRESENT **'            
         BAS   RE,REPORT                                                        
         B     XIT                                                              
         SPACE 1                                                                
ONLYT4   LA    R4,PERLINE                                                       
         MVI   FTYPE,C'T'                                                       
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         BAS   RE,REPORT                                                        
         CP    SALARY,=P'0'                                                     
         BNE   *+8                                                              
         CLI   QOPT6,C'Y'                                                       
         BNE   *+10                                                             
         MVC   P+42(24),=CL24'** SALARY NOT PRESENT **'                         
         BAS   RE,REPORT                                                        
         BAS   RE,REPORT                                                        
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
CA400    CLI   MODE,LEVBLAST                                                    
         BNE   CA500                                                            
         CLI   QOPT4,C'W'                                                       
         BNE   XIT                                                              
         CLC   DEPTOT(42),=7PL6'0'                                              
         BE    XIT                                                              
         GOTO1 ACREPORT            SKIP A LINE BEFORE TOTALS                    
         LA    R4,DEPTOT           PRINT DEPT TOTALS                            
         LA    R6,P+41                                                          
         ZIC   R3,PERNO                                                         
         AH    R3,=H'2'                                                         
CA401    CP    0(6,R4),=P'0'                                                    
         BNE   CA402                                                            
         MVI   8(R6),C'*'                                                       
         B     CA403                                                            
CA402    EDIT  (P6,(R4)),(8,0(R6)),2,FLOAT=-,ZERO=BLANK                         
CA403    LA    R4,6(R4)                                                         
         LA    R6,10(R6)                                                        
         BCT   R3,CA401                                                         
         MVC   P+1(11),=C'DEPT TOTALS'                                          
         BAS   RE,REPORT                                                        
         LA    R4,DEPTOT                                                        
         LA    R1,7                                                             
ZAPTOTS  ZAP   0(6,R4),=P'0'                                                    
         LA    R4,6(R4)                                                         
         BCT   R1,ZAPTOTS                                                       
         B     XIT                                                              
         SPACE 1                                                                
CA500    CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         MVI   RCSUBPRG,3                                                       
         MVI   FORCEHED,C'Y'                                                    
CA510    EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    CA520                                                            
         MVC   SRTREC(SRTRCLNQ),0(R5)                                           
         MVC   HEAD4+87(L'HPER),HPER  PERIOD DATE                               
         MVC   P+3(L'SRTOFF),SRTOFF   OFFICE                                    
         MVC   P+9(L'SRTDEP),SRTDEP   DEPT.                                     
         MVC   P+17(L'SRTSUB),SRTSUB  SUB DEPT.                                 
         MVC   P+25(L'SRTPER),SRTPER  STAFF NO.                                 
         MVC   P+35(36),SRTNAME       NAME                                      
         MVC   P+73(8),SRTHRDT        HIRE DATE                                 
         GOTO1 ACREPORT                                                         
         B     CA510                                                            
         SPACE 1                                                                
CA520    EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
         EJECT                                                                  
*              BUILD HEADLINES AND PRINT DATA                                   
         SPACE 2                                                                
REPORT   NTR1                                                                   
         MVC   HEAD5+12(L'OFF),OFF OFFICE                                       
         L     R1,ADLVANAM                                                      
         LA    RF,HEAD5+17                                                      
         BAS   RE,NAMOUT                                                        
         MVC   HEAD6+12(L'DEP),DEP DEPARTMENT                                   
         L     R1,ADLVBNAM                                                      
         LA    RF,HEAD6+17                                                      
         BAS   RE,NAMOUT                                                        
         CLI   QOPT1,C'T'          OPTION TO SHOW ONLY MISSING                  
         BNE   *+10                                                             
         MVC   HEAD6+80(31),=C'*EMPLOYEES MISSING TIME SHEETS*'                 
         MVC   HEAD4+87(L'HPER),HPER                                            
         MVC   HEAD9+41(70),HD09                                                
         MVC   HEAD10+41(70),HD10                                               
         MVC   HEAD11+41(70),HD11                                               
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         SPACE 2                                                                
NAMOUT   NTR1                                                                   
         ZIC   RE,1(R1)                                                         
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     XIT                                                              
         MVC   0(0,RF),2(R1)                                                    
         SPACE 2                                                                
FORMAT   NTR1                      FORMAT 6 PACKED FIELDS                       
         LA    R5,PERUNITS                                                      
         LA    R6,P+41                                                          
         ZIC   R3,PERNO                                                         
         AH    R3,=H'2'            OTHERS AND TOTAL                             
FMT2     CP    0(6,R4),=P'0'                                                    
         BE    FMT3                                                             
         EDIT  (P6,0(R4)),(8,0(R6)),2,FLOAT=-,ZERO=BLANK                        
FMT3     CLI   FTYPE,C'T'          TOTAL LINE ONLY                              
         BNE   FMT10                                                            
         CH    R3,=H'2'            AM I DOING OTHERS                            
         BNE   FMT4                                                             
         CP    0(6,R4),=P'0'                                                    
         BNE   FMT5                ANYTHING IN OTHERS IS ERROR                  
FMT4     CLI   TYPE,C'P'                                                        
         BNE   FMT9                                                             
         CP    0(6,R5),0(6,R4)                                                  
         BE    FMT10                                                            
FMT5     MVI   8(R6),C'*'          BAD COLUMNS GET AN ASTERISK                  
         B     FMT10                                                            
         SPACE 1                                                                
FMT9     CP    0(6,R4),0(6,R5)                                                  
         BNL   FMT10               NOT ENOUGH HOURS                             
         MVI   8(R6),C'*'                                                       
FMT10    LA    R4,6(R4)            PERSONS HOURS                                
         LA    R5,6(R5)            PERUNITS                                     
         LA    R6,10(R6)           PRINT AREA                                   
         BCT   R3,FMT2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO BUILD UNITS COLUMNS                                   
         SPACE 1                                                                
COLHEAD  NTR1                                                                   
         ZIC   RE,UNITS                                                         
         MH    RE,=H'100'          ADD DECIMAL TO NUMBER OF UNITS               
         CVD   RE,DUB                                                           
         MVC   PERUNITS(42),=7PL6'0'                                            
         LA    R2,PERUNITS                                                      
         ZIC   R3,PERNO            NUMBER OF PERIODS                            
COLH2    ZAP   0(6,R2),DUB         NUMBER TO EACH PERIOD                        
         LA    R2,6(R2)                                                         
         BCT   R3,COLH2                                                         
         CLI   PTYPE,C'D'                                                       
         BE    COLH10              SKIP THE 1R STUFF IF DAILY                   
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
COLH3    L     R4,AIOAREA                                                       
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'1R'                                             
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',(R4),(R4)                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                CAN'T READ 1R LEDGER                         
         MVI   ELCODE,X'17'                                                     
         BAS   RE,GETEL                                                         
         BNE   COLH10              NO STANDARD HOURS                            
         SPACE 1                                                                
         USING ACHOD,R4                                                         
COLH5    CLC   ACHODATE,DATE3      MATCH REQUESTED MONTH                        
         BE    COLH6                                                            
         BAS   RE,NEXTEL                                                        
         BE    COLH5                                                            
         B     COLH10                                                           
         SPACE 1                                                                
COLH6    MVC   PERUNITS(42),=7PL6'0'                                            
         LA    R3,5                                                             
         CLI   QOPT2,C' '                                                       
         BE    COLH6A                                                           
         PACK  DUB,QOPT2(1)        OPTION 2 CAN OVERRIDE                        
         CVB   R3,DUB                                                           
         SPACE 1                                                                
COLH6A   LA    R5,PERUNITS                                                      
         LA    R6,ACHOURS                                                       
         SPACE 1                                                                
COLH7    ZAP   0(6,R5),0(4,R6)     NEW UNITS TO PERUNITS AND UNITS              
         LA    R5,6(R5)                                                         
         LA    R6,4(R6)                                                         
         BCT   R3,COLH7                                                         
         LA    R3,5                                                             
         LA    R5,PERUNITS+24                                                   
COLH8    CP    0(6,R5),=P'0'                                                    
         BNE   COLH9               STOP AT LAST SIGNICANT INPUT                 
         SH    R5,=H'6'                                                         
         BCT   R3,COLH8                                                         
COLH9    STC   R3,PERNO            NEW NUMBER OF PERIODS                        
         SPACE 1                                                                
COLH10   LA    R5,PERUNITS                                                      
         ZIC   R3,PERNO                                                         
         LTR   R3,R3                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LR    RF,R3                                                            
         AH    RF,=H'1'                                                         
         MH    RF,=H'6'                                                         
         LA    RF,PERUNITS(RF)     RF TO TOTAL COLUMN                           
         AP    0(6,RF),0(6,R5)                                                  
         LA    R5,6(R5)                                                         
         BCT   R3,*-10                                                          
         B     XIT                                                              
         EJECT                                                                  
*              BUILD PERIOD/ UNIT FIELDS FOR HEADLINES                          
         SPACE 2                                                                
HEADBLD  NTR1                                                                   
         MVC   HPER,SPACES                                                      
         GOTO1 DATCON,DMCB,(0,QSTART),(9,HPER)                                  
         MVC   HPER+7(3),=C'WKS'                                                
         CLI   PTYPE,C'W'                                                       
         BE    HBLD2                                                            
         MVC   HPER+7(4),=C'HALF'                                               
         CLI   PTYPE,C'H'                                                       
         BE    HBLD2                                                            
         MVC   HPER+7(5),=C'MONTH'                                              
         CLI   PTYPE,C'D'                                                       
         BNE   HBLD2                                                            
         GOTO1 DATCON,DMCB,(0,QSTART),(5,HPER)                                  
         MVI   HPER+8,C'-'                                                      
         GOTO1 DATCON,DMCB,(1,DATAB+39),(5,HPER+9)                              
         SPACE 1                                                                
HBLD2    MVC   HD09,SPACES                                                      
         MVC   HD10,SPACES                                                      
         MVC   HD11,SPACES                                                      
         MVC   STEND(2),DATAB+3       USE CALENDAR MONTHS FOR START             
         MVC   STEND+2(2),DATAB+3      AND END DATES FOR SALARY                 
         LA    R2,HD09                                                          
         LA    R5,PERUNITS                                                      
         LA    R4,DATAB                                                         
         ZIC   R3,PERNO                                                         
         SPACE 1                                                                
HBLD4    GOTO1 DATCON,DMCB,(1,3(R4)),(8,0(R2))  END DATE TO HEAD                
         LA    R6,L'HD09(R2)                                                    
         EDIT  (P6,0(R5)),(7,1(R6)),2  UNITS TO HEAD 10                         
         LA    R6,L'HD09(R6)                                                    
         MVC   0(8,R6),=8C'-'      UNDERLINE HEAD11                             
         LA    R5,6(R5)                                                         
         LA    R4,6(R4)                                                         
         LA    R2,10(R2)                                                        
         BCT   R3,HBLD4                                                         
         SPACE 1                                                                
         LA    R6,L'HD09(R2)                                                    
         MVC   2(5,R6),=C'OTHER'   TO LINE 10                                   
         LA    R6,L'HD09(R6)                                                    
         MVC   0(8,R6),=8C'-'                                                   
         LA    R2,10(R2)                                                        
         SPACE 1                                                                
         MVC   3(5,R2),=C'TOTAL'                                                
         LA    R6,L'HD09(R2)                                                    
         ZIC   R3,PERNO                                                         
         AH    R3,=H'1'                                                         
         MH    R3,=H'6'                                                         
         LA    R3,PERUNITS(R3)     R3 TO TOTAL UNITS                            
         EDIT  (P6,0(R3)),(7,1(R6)),2                                           
         LA    R6,L'HD09(R6)                                                    
         MVC   0(8,R6),=8C'-'                                                   
         B     XIT                                                              
         EJECT                                                                  
         USING ACEMPD,R4           HIRE AND TERM DATE                           
HNTTABLE NTR1                                                                   
         LA    R5,SRTREC                                                        
         MVC   0(SRTRCLNQ,R5),SPACES                                            
         L     R2,ADACCNAM                                                      
         ZIC   R3,1(R2)                                                         
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SRTNAME(0),2(R2)                      PUT NAME IN                
         GOTO1 DATCON,DMCB,(1,ACEMPHIR),(5,SRTHRDT)                             
         MVC   SRTOFF,OFF          OFFICE                                       
         MVC   SRTDEP,DEP          DEPT.                                        
         MVC   SRTSUB,SUB          SUB-DEPT                                     
         MVC   SRTPER,PER          PERSON                                       
         GOTO1 SORTER,DMCB,=C'PUT',(R5)                                         
         B     XIT                                                              
         EJECT                                                                  
* GET LEVELS OF ACCOUNT                                                         
*                                                                               
PARSE    NTR1  ,                                                                
         MVC   ACLVS,SPACES                                                     
         L     R2,ADACC                                                         
         LA    R2,3(R2)                                                         
         SR    R1,R1                                                            
         IC    R1,LVALN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   OFF(0),0(R2)        OFFICE CODE                                  
         LA    R2,1(R1,R2)                                                      
         IC    R1,LVBLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   DEP(0),0(R2)        DEPT CODE                                    
         LA    R2,1(R1,R2)                                                      
         IC    R1,LVCLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SUB(0),0(R2)        SUB-DEPT CODE                                
         LA    R2,1(R1,R2)                                                      
         IC    R1,LVDLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   PER(0),0(R2)        PERSON CODE                                  
         B     XIT                                                              
         EJECT                                                                  
*        PRINT EMPLOYEE HIRE AND/OR TERM DATES                                  
         SPACE 1                                                                
PRTHNT   NTR1                                                                   
         USING ACEMPD,R4           HIRE AND TERM DATE                           
         LA    R3,PTHIRD+4                                                      
         OC    ACEMPHIR,ACEMPHIR       IS THERE A HIRE DATE                     
         BZ    PRTHNT1                                                          
         CLI   PROGPROF+5,C'T'     DO WE JUST WANT TERM DATE                    
         BE    PRTHNT1                                                          
         MVI   0(R3),C'*'                                                       
         LA    R3,1(R3)                                                         
         MVC   0(2,R3),=C'H-'                                                   
         LA    R3,2(R3)                                                         
         GOTO1 DATCON,DMCB,(1,ACEMPHIR),(5,(R3))                                
         LA    R3,8(R3)                                                         
         CLI   PROGPROF+5,C'H'     DO WE JUST WANT HIRE DATE                    
         BE    PRTHNT8                                                          
PRTHNT1 EQU    *                                                                
         OC    ACEMPTRM,ACEMPTRM       IS THERE A TERM DATE                     
         BZ    PRTHNT8                                                          
         CLI   PROGPROF+5,C'T'     DO WE WANT BOTH HIRE N TERM                  
         BE    PRTHNT3             IF PROFILE = B OR ANYTHING BUT H,T,N         
         SPACE 1                                                                
         OC    ACEMPHIR,ACEMPHIR       IS THERE A HIRE DATE                     
         BZ    PRTHNT3                                                          
         MVI   0(R3),C','          THEN PRINT COMMA                             
         LA    R3,1(R3)                                                         
         B     PRTHNT4                                                          
         SPACE 1                                                                
PRTHNT3 EQU    *                                                                
         MVI   0(R3),C'*'                                                       
         LA    R3,1(R3)                                                         
PRTHNT4 EQU    *                                                                
         MVC   0(2,R3),=C'T-'                                                   
         LA    R3,2(R3)                                                         
         GOTO1 DATCON,DMCB,(1,ACEMPTRM),(5,(R3))                                
         LA    R3,8(R3)                                                         
PRTHNT8 EQU    *                                                                
         CLI   PTHIRD+4,C'*'                                                    
         BNE   PRTHNT9                                                          
         MVI   0(R3),C'*'                                                       
         GOTO1 UNDERLIN,DMCB,(55,PTHIRD+1),(0,PFOURTH+1)                        
PRTHNT9 EQU    *                                                                
         B     XIT                                                              
         EJECT                                                                  
*               SUBROUTINES                                                     
         SPACE 1                                                                
*              ADD START END DATES TO DATAB                                     
DATEUP   NTR1                                                                   
         ZIC   R2,PERNO                                                         
         CLI   QOPT4,C'D'          FOR DAILY ITS SEVEN DAYS                     
         BNE   *+8                 SAT AND SUN ARE INCLUDE IN FRI               
         LA    R2,7                                                             
         CH    R2,=H'2'                                                         
         BNL   *+8                                                              
         LA    R2,2                                                             
         LA    R3,DATAB                                                         
DATEUP2  BAS   RE,DTCONV                                                        
         LA    R4,6                LAST DAY OF THIS WEEK                        
         LA    R3,3(R3)                                                         
         CLI   PTYPE,C'D'                                                       
         BE    DATEUP3                                                          
         BAS   RE,DTADD                                                         
         MVC   WORK(6),WORK+6                                                   
DATEUP3  BAS   RE,DTCONV                                                        
         LA    R3,3(R3)                                                         
         LA    R4,1                FIRST DAY OF NEXT WEEK                       
         BAS   RE,DTADD                                                         
         MVC   WORK(6),WORK+6                                                   
         BCT   R2,DATEUP2                                                       
         B     XIT                                                              
         SPACE 2                                                                
DTCONV   NTR1                                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(1,(R3))                                    
         B     XIT                                                              
         SPACE 2                                                                
DTADD    NTR1                                                                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R4)                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD TO A BINSRCH TABLE                                
*              PARAM1              A(RECORD TO BE ADDED)                        
*              PARAM2              A(BINSRCH PARAMS)                            
BINADD   NTR1                                                                   
         USING BIND,R5                                                          
         L     R5,4(R1)            BINSRCH PARAMETERS                           
         L     R3,0(R1)            A(RECORD)                                    
         MVC   DMCB+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,(R3)),(R2)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1                                                           
         BE    BINXIT              NOT FOUND - ADDED                            
         L     R4,DMCB             A(RECORD FOUND)                              
         ZIC   R6,BINFRST          DISP. TO FIRST BUCKET                        
         AR    R4,R6               RECORD FOUND                                 
         AR    R3,R6               NEW RECORD                                   
         ZIC   R7,BINNUMB          NUMBER OF BUCKETS                            
         TM    BINSTAT,X'80'                                                    
         BO    BINBIN              DATA IS BINARY                               
         AP    0(6,R4),0(6,R3)     ADD NEW TO OLD                               
         LA    R4,6(R4)                                                         
         LA    R3,6(R3)                                                         
         BCT   R7,*-14                                                          
         B     BINXIT                                                           
         SPACE 1                                                                
BINBIN   L     RE,0(R3)                                                         
         L     RF,0(R4)                                                         
         AR    RF,RE                                                            
         ST    RF,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R7,BINBIN                                                        
BINXIT   XMOD1 1                                                                
         EJECT                                                                  
*              GET SALARY                                                       
         SPACE 1                                                                
GETSAL   NTR1                                                                   
         L     R2,ADACC                                                         
*MN      GOTO1 ACSLRY,DMCB,(R2),STEND,SALAREA                                   
         GOTO1 ACSLRY,DMCB,(X'80',(R2)),STEND,SALAREA,ADCOMFAC                  
         LA    R2,SALAREA                                                       
         USING SLRD,R2                                                          
         ZAP   SALARY,SLRTOT                                                    
         B     XIT                                                              
         SPACE 1                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              GET PROJECT/CLIENT NAME                                          
         SPACE 1                                                                
GETNME   NTR1                                                                   
         L     R2,ADACC                                                         
         MVC   SVKEY,0(R2)                                                      
         USING ACKEYD,R4                                                        
         L     R4,AIOAREA                                                       
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC,PKEY       CLIENT PROJECT KEY                           
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',(R4),(R4)                        
         CLI   DMCB+8,0                                                         
         BNE   GETNMEX                                                          
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   GETNMEX                                                          
         USING ACNAMED,R4                                                       
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PNAME(0),ACNMNAME                                                
         SPACE 1                                                                
GETNMEX  L     R4,AIOAREA          RE-READ ACCOUNT                              
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC,SVKEY                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',(R4),(R4)                        
         CLI   DMCB+8,0                                                         
         BE    XIT                                                              
         DC    H'0'                CAN'T READ ACCOUNT                           
         EJECT                                                                  
*              CONSTANTS                                                        
ATABLE   DC    A(TABLE)                                                         
AIOAREA  DC    A(IOAREA)                                                        
GETBROAD DC    V(GETBROAD)                                                      
CENTER   DC    V(CENTER)                                                        
SORTER   DC    V(SORTER)                                                        
UNDERLIN DC    V(UNDERLIN)                                                      
SQUASHER DC    V(SQUASHER)                                                      
ACSLRY   DC    V(ACSLRY)                                                        
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)'                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
TABLE    DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(PLEN)           RECORD LENGTH                                
         DC    AL4(L'PKEY)         DISP. TO KEY/KEY LENGTH                      
         DC    AL4(PMAX)           MAX IN TABLE                                 
         DC    AL1(PCNT)           NUMBER OF BUCKETS                            
         DC    AL1(PACCS-PKEY)     DISP TO BUCKETS                              
         DC    AL2(0)                                                           
         DS    (PMAX*PLEN)C        THE TABLE                                    
PMAX     EQU   400                                                              
         SPACE 1                                                                
IOAREA   DS    D                                                                
         DS    1000C                                                            
         EJECT                                                                  
*              DSECT FOR PROGRAM                                                
         SPACE 2                                                                
PROGD    DSECT                                                                  
OPTIONS  DS    0CL4                                                             
BASIS    DS    CL1                                                              
TYPE     DS    CL1                                                              
PTYPE    DS    CL1                                                              
UNITS    DS    CL1                                                              
DATAB    DS    CL42                START/END DATES X 5                          
SVKEY    DS    CL15                                                             
PERLINE  DS    7PL6                                                             
PERNO    DS    CL1                                                              
DATE3    DS    CL2                                                              
         DS    CL1                                                              
FTYPE    DS    CL1                                                              
BROADST  DS    CL3                                                              
BROADND  DS    CL3                                                              
WEEKSTRT DS    CL3                                                              
WEEKEND  DS    CL3                                                              
SAVEWORK DS    CL6                                                              
SAVEQSTR DS    CL6                                                              
SAVEFRST DS    CL6                                                              
PLINE    DS    0CL93                                                            
PKEY     DS    CL15                                                             
PNAME    DS    CL36                                                             
PACCS    DS    0CL42                                                            
PACC1    DS    PL6                                                              
PACC2    DS    PL6                                                              
PACC3    DS    PL6                                                              
PACC4    DS    PL6                                                              
PACC5    DS    PL6                                                              
PACC6    DS    PL6                                                              
PACC7    DS    PL6                                                              
PEND     EQU   *                                                                
PCNT     EQU   (PEND-PACCS)/6                                                   
PLEN     EQU   *-PKEY                                                           
PERUNITS DS    7PL6                                                             
DEPTOT   DS    0CL42                                                            
DEPT1    DS    PL6                                                              
DEPT2    DS    PL6                                                              
DEPT3    DS    PL6                                                              
DEPT4    DS    PL6                                                              
DEPT5    DS    PL6                                                              
DEPT6    DS    PL6                                                              
DEPT7    DS    PL6                                                              
HPER     DS    CL20                                                             
DATNO    DS    CL1                                                              
DATYPE   DS    CL1                                                              
MOS      DS    CL2                 MON/YR OF TRNSBTCH OR TRNSDATE               
         DS    CL1                 KEEP WITH MOS- NEED FOR DATCON               
ELCODE   DS    CL1                                                              
DUB1     DS    D                                                                
HD09     DS    CL70                                                             
HD10     DS    CL70                                                             
HD11     DS    CL70                                                             
         SPACE 1                                                                
SRTREC   DS    0F                                                               
SRTOFF   DS    CL(L'OFF)           OFFICE                                       
SRTDEP   DS    CL(L'DEP)           DEPARTMENT                                   
SRTSUB   DS    CL(L'SUB)           SUB-DEPT.                                    
SRTPER   DS    CL(L'PER)           PERSON                                       
SRTNAME  DS    CL36                                                             
SRTKYLNQ EQU   *-SRTREC                                                         
SRTHRDT  DS    CL8                                                              
SRTRCLNQ EQU   *-SRTREC                                                         
         SPACE 1                                                                
STEND    DS    CL4                 START END FOR SALARY                         
SALAREA  DS    CL100                                                            
SALARY   DS    PL6                                                              
ACTFLAG  DS    CL1                 ACTIVITY FLAG                                
RQDATE   DS    PL3                 PACKED REQUEST START DATE                    
*                                                                               
ACLVS    DS    0CL(ACLNQ)                                                       
OFF      DS    CL2                 OFFICE                                       
DEP      DS    CL3                 DEPARTMENT                                   
SUB      DS    CL3                 SUB-DEPARTMENT                               
PER      DS    CL8                 PERSON                                       
ACLNQ    EQU   *-OFF                                                            
*                                                                               
LVALN    DS    XL1                 LEVEL A LENGTH                               
LVBLN    DS    XL1                 LEVEL B LENGTH                               
LVCLN    DS    XL1                 LEVEL C LENGTH                               
LVDLN    DS    XL1                 LEVEL D LENGTH                               
*                                                                               
LVCDSP   DS    XL1                 DISPLACEMENT TO LEVEL C                      
LVDDSP   DS    XL1                 DISPLACEMENT TO LEVEL D                      
*                                                                               
SBDPER   DS    CL11                SUB-DEPT / PERSON CODES                      
         EJECT                                                                  
*              DSECT FOR THE BINSRCH LIST                                       
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISP. TO FIRST BUCKET                        
BINSTAT  DS    CL1                 X'80' BINARY DATA                            
         DS    CL1                 SPARE                                        
BINTABLE DS    0CL1                                                             
         EJECT                                                                  
*        ACGENBOTH                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        DDSLRD                                                                 
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDSLRD                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPC202 05/01/02'                                      
         END                                                                    
