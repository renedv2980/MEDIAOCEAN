*          DATA SET ACREP9302  AT LEVEL 007 AS OF 05/01/02                      
*PHASE AC9302A,+0                                                               
*INCLUDE SQUASHER                                                               
*INCLUDE UNDERLIN                                                               
*INCLUDE ACSLRY                                                                 
         TITLE 'AC9302 - PAYROLL RATE LISTING'                                  
AC9302A  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC93**,R9       MODIFY FOR BASE REGS & WRKD                  
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC9302D,RC                                                       
         EJECT                                                                  
* *********************************************************************         
* MODES -                                                                       
* *********************************************************************         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUN                                                              
         CLI   MODE,REQFRST                                                     
         BE    REQ                                                              
         CLI   MODE,LEDGFRST                                                    
         BE    LDG                                                              
         CLI   MODE,LEVAFRST                                                    
         BE    LVA                                                              
         CLI   MODE,LEVBFRST                                                    
         BE    LVB                                                              
         CLI   MODE,LEVCFRST                                                    
         BE    LVC                                                              
         CLI   MODE,PROCACC                                                     
         BE    PAC                                                              
         CLI   MODE,LEVCLAST                                                    
         BE    LVCL                                                             
         CLI   MODE,LEVBLAST                                                    
         BE    LVBL                                                             
         CLI   MODE,LEVALAST                                                    
         BE    LVAL                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* *********************************************************************         
* RUNFRST MODE - GET RELOCATE ADDRESS TYPES                                     
* *********************************************************************         
*                                                                               
RUN      LA    R1,ATYPES                                                        
         RELOC RELO                                                             
         LA    RE,RELOTAB          RELOCATE A-TYPES                             
RUNLP    L     RF,0(RE)                                                         
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RUNLP                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAY)                                 
         B     XIT                                                              
         EJECT                                                                  
* *********************************************************************         
* REQUEST MODE - CREATE HEADER AND TITLES FOR REPORT FROM REQST SCREEN.         
* *********************************************************************         
*                                                                               
REQ      LA    R5,REQTOT                                                        
         USING LEVD,R5                                                          
         MVC   LEVCST(LEVCSTQ),PKZEROS   CLEAR COST FIELDS                      
         MVC   LEVNAM,SPACES                                                    
         MVC   LEVNAM(7),=C'REQUEST'                                            
         MVI   RCSUBPRG,0                                                       
         XC    START,START                                                      
         MVC   END,=2X'FF'                                                      
         CLI   QOPT2,C' '          DEFAULT=TODAY'S DATE & RCSUBPRG=0            
         BE    REQ0D                                                            
         CLI   QOPT2,C'C'          IF COST OPTION- RCSUBPRG1,TODAYS DT          
         BE    REQ0A                                                            
         B     REQ0D                                                            
*                                                                               
REQ0A    MVI   RCSUBPRG,1          COST OPTION                                  
         MVC   START,TODAY         DATE IS TODAY'S DATE                         
         MVC   END,TODAY           THRU DATE IS TODAY                           
         B     REQ0D                                                            
*                                                                               
REQ0D    MVC   PERIOD,SPACES       IF DIRECT BR TO HERE, RSUBPRG=0              
         LA    R2,PERIOD           PT TO PERIOD LINE                            
         USING PERD,R2             PLACE PERIOD DSECT ATOP                      
         CLC   QSTART,SPACES       ANY START DATE SPECIFIED?                    
         BE    REQ1                NO, DON'T DO DATE CONVERSION                 
         MVC   WORK(4),QSTART      PUT DATE INTO USABLE FORMAT                  
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   START,WORK+6        SAVE START DATE                              
*                                                                               
REQ1     CLC   QEND,SPACES         WAS AN END DATE SPECIFIED                    
         BE    REQ2                NO, SKIP DATE CONVERSION                     
         MVC   WORK(4),QEND        CONVERT END DATE TO NICE FORMAT              
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   END,WORK+6                                                       
         CLC   START,END           MAKE SURE END DATE > START DATE              
         BNH   REQ2                IT IS, THEN DATES ARE OKAY                   
         MVC   START,END           IT'S NOT,USE END DATE FOR BOTH               
*                                                                               
REQ2     CLC   START(4),=X'0000FFFF'  WAS A DATE SPECIFIED?                     
         BE    REQ3                   NO START DATE GIVEN                       
         MVC   PERLIT1,=C'FOR THE PERIOD'   REPORT IS FOR A PERIOD              
         OC    START,START         IS THERE A START DATE?                       
         BZ    REQ2A               NO DATE, GET END DATE                        
         MVC   WORK(2),START       CONVERT TO DISPLAYABLE FORMAT                
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(9,PERSTRT)                                 
*                                                                               
REQ2A    CLC   END,=X'FFFF'        IS THERE AN END DATE                         
         BE    REQ3                NO, QUIT DATE PART                           
         CLC   START,END           IS END DATE SAME AS START DATE?              
         BE    REQ3                YES, DON'T PRINT AGAIN                       
         MVC   WORK(2),END         CONVERT END DATE TO DISPLY FORMAT            
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(9,WORK+3)                                  
         OC    START,START                                                      
         BNZ   REQ2B                                                            
         MVC   PERLIT2,=C'THRU'                                                 
         MVC   PEREND1,WORK+3                                                   
         B     REQ3                                                             
*                                                                               
REQ2B    MVI   PERDASH,C'-'                                                     
         MVC   PEREND2,WORK+3      DISPLAY END DATE                             
*                                                                               
REQ3     L     R4,ADCMPNAM         GET COMPANY NAME                             
         LA    R6,COMPNAM          SAVE DESTINATION                             
         DROP  R2                                                               
         BAS   RE,NAMOUT           USES NAMELD DSECT TO GET NAME                
*                                                                               
         MVC   PAGE,=H'1'          SET PAGE COUNTER TO 1                        
         MVI   FORCEHED,C'Y'       NEW PAGE                                     
*                                                                               
         MVC   MYHEAD,SPACES       CLEAR HEADER                                 
         LA    R8,PROGPROF         PT TO HEADLNE CATEGORIES RQSTD LIST          
         USING PROGD,R8            USE PROGRAM PROFILE DSECT                    
         LA    RF,PROGNPG          GET ADDR OF NEWPG OPTION (MAYBE=Y)           
         LA    R2,PROFLIST         PT TO COLUMN HEADINGS LITERALS               
         USING LABELD,R2           USE HEADING DSECT TO ACCESS LITERALS         
         LA    R4,MYHEAD           PRINTLINE WHERE HEADINGS GO                  
         USING COLUMND,R4          USE HEADER DSECT                             
         LA    R3,1                R3=  # COLUMN HEADINGS ON REPORT             
         STC   R3,COLCNT           SAVE COL COUNTER                             
         MVI   OTHOPT,C'N'         OTHER OPTION NOT DISPLAYABLE-DEFAULT         
*                                                                               
REQ5     CR    R8,RF               IS THIS THE NEW PAGE OPTION?                 
         BE    REQ6                YES, SKIP IT                                 
         CLI   0(R8),C'Y'          WAS THIS DETAIL COLUMN REQUESTED?            
         BNE   REQ6                NO, BUMP COUNTERS. LOOK @ NEXT ONE           
         CLI   COLCNT,MAXCOLQ      IS MAX # COLS DISPLAYED?                     
         BE    REQ9                YES, EXIT                                    
         MVC   COLUMN,LABEL        YES, PUT LABEL HEADING ON PRT LINE           
         CLC   LABEL,OTHERL        IS THIS THE 'OTHER' OPTION?                  
         BNE   *+8                 NO- SO DON'T SET THE FLAG                    
         MVI   OTHOPT,C'Y'         YES, OTHER OPTION IS DISPLAYABLE             
         LA    R3,1(R3)            BUMP COUNTER                                 
         STC   R3,COLCNT           SAVE COUNTER                                 
         LA    R4,COLMQ(R4)        BUMP TO NEXT COLUMN POSITION                 
         MVC   COLUMN,TOTALC       MOVE IN THE 'TOTAL' LITERAL                  
*                                                                               
REQ6     LA    R8,1(R8)            BUMP TO NEXT CATAGORY RQST BYTE              
         LA    R2,LABELQ(R2)       BUMP TO NEXT HEADING IN TABLE                
         CLI   LABEL,X'FF'         END OF TABLE?                                
         BNE   REQ5                NO, THERE ARE MORE HEADINGS                  
*                                                                               
REQ9     B     XIT                                                              
         DROP  R2                                                               
         DROP  R4                                                               
         DROP  R5                                                               
         DROP  R8                                                               
         EJECT                                                                  
* *******************************************************************           
* LEDGFRST MODE: GET LENGTHS OF FIELDS AND THEIR DESCRIPTIONS                   
*          FOR A GIVEN COMPANY CODE, UNIT CODE, AND LEDGER CODE.                
* *******************************************************************           
*                                                                               
LDG      L     RF,ADLDGHIR         GET ADDRESS OF ELEMENT                       
         USING ACLELD,RF           USE DSECT FOR ELEMNT LENGTHS                 
         LA    R1,ACLVALS          PT TO LENGTH OF LEVS IN ELEMENT              
         LA    R3,LLEVA            PT TO BEGIN OF SAVE AREA                     
         USING LEVD,R3             USE DSECT FOR SAVE INFO                      
         LA    R0,LEVMAX           MAX NUMBER OF LEVELS                         
         SR    R2,R2               NUMBER OF ACTUAL LEVELS                      
         SR    R4,R4               OLD COMBINED LENGTH                          
*                                                                               
LDG02    ZIC   R5,0(R1)            NEW COMBINED LN OF LEVELS                    
         STC   R5,LEVRUNLN         SAVE  RUNNING LENGTH                         
         SR    R5,R4               GET ACTUAL LENGTH OF LEVEL                   
         STC   R5,LEVACTLN         SAVE ACTUAL LENGTH                           
         ZIC   R4,LEVRUNLN         NEW COMB LN->SAVE AS OLD LN                  
         MVC   LEVDES,ACLVDESC-ACLVLEN(R1)     SAVE DESCRIPTION                 
         LA    R2,1(R2)            ADD TO LEVEL COUNT                           
         CLI   LEVRUNLN,MAXLEN     LAST LEV HAS MAX FOR ACCT                    
         BE    LDG04               YES, WE'RE DONE DOING LENGTHS                
         LA    R3,LLEVQ(R3)        BUMP TO NEXT LEVEL SAVE AREA                 
         LA    R1,L'ACLVALS(R1)     BUMP TO NEXT LEVEL LN&DESCR IN ELMN         
         BCT   R0,LDG02                                                         
         DC    H'0'                                                             
*                                                                               
LDG04    STC   R2,NUMLEV           ACTUAL NUMBER OF LEVELS                      
         DROP  RF                  DROP LENGTHS DSECT                           
         DROP  R3                  DROP LENVD DSECT                             
*                                                                               
         MVI   LONGDES,7           LONGEST DESCRIPTION=7                        
         LA    R5,LLEVA            PT TO FIRST LEVEL                            
         USING LEVD,R5             USE DSECT                                    
LDG05    GOTO1 SQUASHER,DMCB,LEVDES,15                                          
         CLC   LONGDES,DMCB+7      IS PREV DESCR LARGER?                        
         BH    *+10                YES,                                         
         MVC   LONGDES,DMCB+7       SAVE LARGER LENGTH                          
         LA    R5,LLEVQ(R5)        BUMP TO NEXT LEVEL STORE AREA                
         BCT   R2,LDG05                                                         
         DROP  R5                  DROP LEVEL DSECT                             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* *******************************************************************           
* LEVAFRST MODE: GET SOME OF THE SALARY INFO                                    
* *******************************************************************           
*                                                                               
LVA      LA    R5,LLEVA            PT TO LEVA SAVE STORAGE INFO                 
         USING LEVD,R5             PLACE LEVEL INFO DSECT                       
         MVC   LEVCST(LEVCSTQ),PKZEROS   CLEAR COST FIELDS                      
*                                                                               
         L     R2,ADHEIRA          GET ADDRESS OF SALARY INFO                   
         CLI   RCSUBPRG,1          IS THIS A COST REPORT                        
         BNE   LVA10               ONLY GO TO SALARY IF COST LIST               
         LA    R6,SALAREA          GET ADDRESS OF SALARY AREA                   
*        GOTO1 ACSLRY,DMCB,(R2),START,(R6)                                      
         GOTO1 ACSLRY,DMCB,(X'80',(R2)),START,(R6),ADCOMFAC                     
         BAS   RE,SALINFO          PROCESS BENEFIT/BOUS PERCENT                 
*                                                                               
LVA10    MVC   LEVACC,3(R2)                                                     
         L     R4,ADLVANAM                                                      
         LA    R6,LEVNAM                                                        
         BAS   RE,NAMOUT                                                        
         L     R4,ADLVASTA                                                      
         LA    R6,LEVSTAT                                                       
         BAS   RE,STATOUT                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
LVAX     B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* ********************************************************************          
* LEVBFRST - LEVEL B PROCESSING                                                 
* ********************************************************************          
*                                                                               
LVB      L     R2,ADHEIRB          ADDRESS OF LEV B INFO                        
         LA    R5,LLEVB            PT TO SAVE STORAGE AREA                      
         USING LEVD,R5             USE DSECT                                    
         MVC   LEVCST(LEVCSTQ),PKZEROS   CLEAR COST FIELDS                      
         CLI   RCSUBPRG,1          IS THIS A COST LISTING REPORT?               
         BNE   LVB10               ONLY GO TO SALARY IF COST LIST               
         LA    R6,SALAREA          GET ADDRESS OF SAVE SALARY AREA              
*        GOTO1 ACSLRY,DMCB,(R2),START,(R6)                                      
         GOTO1 ACSLRY,DMCB,(X'80',(R2)),START,(R6),ADCOMFAC                     
         BAS   RE,SALINFO          PROCESS BENEFIT/BOUS PERCENT                 
*                                                                               
LVB10    MVC   LEVACC,SPACES                                                    
         LA    R4,LLEVA               PT TO LEV-A STORAGE INFO                  
         ZIC   R8,LEVRUNLN-LEVD(R4)   LEVEL A RUNNING LENGTH IN R8              
         LA    R2,3(R8,R2)            ACCOUNT AT LEVEL B                        
         ZIC   R3,LEVRUNLN                                                      
         SR    R3,R8                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   LEVACC(0),0(R2)                                                  
*                                                                               
         L     R4,ADLVBNAM                                                      
         LA    R6,LEVNAM                                                        
         BAS   RE,NAMOUT                                                        
         LA    R6,LEVSTAT                                                       
         L     R4,ADLVBSTA                                                      
         BAS   RE,STATOUT                                                       
*                                                                               
         MVI   FRSTSUB,C'Y'                                                     
         CLI   NUMLEV,LEVMAX                                                    
         BNE   LVBX                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVI   FRSTSUB,C'N'                                                     
*                                                                               
LVBX     B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* ********************************************************************          
* LEVCFRST                                                                      
* ********************************************************************          
*                                                                               
LVC      LA    R5,LLEVC            PT TO SAVED STORAGE AREA                     
         USING LEVD,R5             USE DSECT FOR LEVEL STORE INFO               
         MVC   LEVCST(LEVCSTQ),PKZEROS   CLEAR COST FIELDS                      
         ZAP   PCTBENEF,=P'0'      CLEAR BENEFIT PERCENTS                       
         ZAP   PCTADMIN,=P'0'      CLEAR ADMIN PERCENTS                         
*                                                                               
         L     R2,ADHEIRA                                                       
         BAS   RE,EMPBEN           GET PERCENT FOR OFFICE                       
         L     R2,ADHEIRB                                                       
         BAS   RE,EMPBEN           GET PERCENT FOR DEPT                         
         L     R2,ADHEIRC                                                       
         BAS   RE,EMPBEN           GET PERCENT FOR SUB DEPT                     
         ZAP   PCTBENSV,PCTBENEF   SAVE THESE PCT'S AS DEFAULT                  
         ZAP   PCTADMSV,PCTADMIN                                                
         L     R2,ADHEIRC                                                       
         CLI   RCSUBPRG,1                                                       
         BNE   PA55                ONLY GO TO SALARY IF COST LIST               
*                                                                               
         LA    R6,SALAREA                                                       
*        GOTO1 ACSLRY,DMCB,(R2),START,(R6)                                      
         GOTO1 ACSLRY,DMCB,(X'80',(R2)),START,(R6),ADCOMFAC                     
         BAS   RE,SALINFO          PROCESS BENEFIT/BOUS PERCENT                 
*                                                                               
PA55     MVC   LEVACC,SPACES                                                    
         LA    R4,LLEVB                                                         
         ZIC   R8,LEVRUNLN-LEVD(R4)   LEVEL B LENGTH IN R8                      
         LA    R2,3(R8,R2)         ACCOUNT AT LEVEL C                           
         ZIC   R3,LEVRUNLN                                                      
         SR    R3,R8                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   LEVACC(0),0(R2)                                                  
*                                                                               
         L     R4,ADLVCNAM                                                      
         LA    R6,LEVNAM                                                        
         BAS   RE,NAMOUT                                                        
         LA    R6,LEVSTAT                                                       
         L     R4,ADLVCSTA                                                      
         BAS   RE,STATOUT                                                       
         MVI   FRSTSUB,C'Y'                                                     
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* ********************************************************************          
* PROCACC - PROCESS AN ACCOUNT                                                  
* ********************************************************************          
*                                                                               
PAC      LA    R5,ACCTOT                                                        
         CLI   NUMLEV,LEVMAX                                                    
         BE    *+8                                                              
         LA    R5,LLEVC                                                         
         USING LEVD,R5             USE STORAGE SAVE DSECT                       
         MVC   LEVCST(LEVCSTQ),PKZEROS   CLEAR COST FIELDS                      
         L     R2,ADACCSTA                                                      
         USING RSTELD,R2                                                        
         CLI   QOPT1,C' '                                                       
         BE    PA62                                                             
         CLI   QOPT1,C'L'                                                       
         BE    PA61                                                             
         CLI   QOPT1,C'S'                                                       
         BNE   XIT                                                              
         TM    RSTSTAT1,RSTSACIL      SUPPRESS LOCKED                           
         BO    XIT                                                              
         B     PA62                                                             
PA61     TM    RSTSTAT1,RSTSACIL    LOCKED ONLY                                 
         BZ    XIT                                                              
*                                                                               
PA62     ZAP   PCTBENEF,PCTBENSV   RESTORE DEFAULT PCTS                         
         ZAP   PCTADMIN,PCTADMSV                                                
         L     R2,ADACC                                                         
         BAS   RE,EMPBEN           GET PERCENT FOR EMPLOYEE                     
         MVI   OHDACC,C'N'                                                      
*                                                                               
         LA    RE,3(R2)            PT TO RECORD                                 
         LA    R5,LLEVA            PT TO LEVEL STORAGE INFO                     
         USING LEVD,R5                                                          
*                                                                               
PA62A    ZIC   R1,LEVACTLN         GET ACTUAL LN OF LEVEL                       
         BCTR  R1,0                DECR FOR EXCLC                               
         CLI   LEVRUNLN,MAXLEN     IS THIS THE LAST LEVEL?                      
         BNE   PA62B               NO                                           
         CLC   0(MIN9Q,RE),MASK9   ARE THE MIN # 9'S HERE FOR OV.ACT?           
         BE    PA63                YES, THIS IS AN OVERHEAD ACCT.               
         BNE   PA63A               NO MATCH, PROCESS NORMALLY                   
*                                                                               
PA62B    EXCLC R1,0(RE),MASK9      DOES ENTIRE FIELD HAVE 9'S?                  
         BE    PA63                YES                                          
         LA    RE,1(R1,RE)         BUMP TO CORRECT FIELD IN RECD                
         LA    R5,LLEVQ(R5)        BUMP TO NEXT LEVEL STORE INFO                
         B     PA62A                                                            
*                                                                               
PA63     MVI   OHDACC,C'Y'         OVERHEAD ACCOUNT                             
*                                                                               
PA63A    LA    R5,ACCTOT                                                        
         USING LEVD,R5                                                          
         CLI   NUMLEV,LEVMAX                                                    
         BE    *+8                                                              
         LA    R5,LLEVC                                                         
         L     R2,ADACC                                                         
         CLI   RCSUBPRG,1                                                       
         BNE   PA64                                                             
         LA    R6,SALAREA                                                       
*        GOTO1 ACSLRY,DMCB,(R2),START,(R6)                                      
         GOTO1 ACSLRY,DMCB,(X'80',(R2)),START,(R6),ADCOMFAC                     
         BAS   RE,SALINFO          PROCESS BENEFIT/BOUS PERCENT                 
*                                                                               
PA64     MVC   LEVACC,SPACES                                                    
         LA    R4,LLEVC            GET LENGTH OF LAST LEVEL                     
         CLI   NUMLEV,LEVMAX                                                    
         BE    *+8                                                              
         LA    R4,LLEVB                                                         
         ZIC   R8,LEVRUNLN-LEVD(R4)   LENGTH OF LAST IN R8                      
         LA    R2,3(R8,R2)                                                      
         ZIC   R3,LEVRUNLN                                                      
         SR    R3,R8                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   LEVACC(0),0(R2)                                                  
*                                                                               
         L     R4,ADACCNAM                                                      
         LA    R6,LEVNAM                                                        
         BAS   RE,NAMOUT                                                        
         LA    R6,LEVSTAT                                                       
         L     R4,ADACCSTA                                                      
         BAS   RE,STATOUT                                                       
*                                                                               
         MVC   P,SPACES            CLEAR PRINT LINE                             
         LA    R2,P                PT TO PRINT LINE                             
         USING PRT1D,R2            USE PRINT LINE DSECT                         
         MVC   WORK01,SPACES                                                    
         MVC   PRTACC(7),LEVACC                                                 
         MVC   WORK01(36),LEVNAM                                                
         MVC   WORK01+37(10),LEVSTAT                                            
*                                  **HIRE AND TERM DATES**                      
         CLI   PROGPROF+(PROGHTD-PROGD),C'B'     (DEFAULT IS N)                 
         BE    *+8                                                              
         CLI   PROGPROF+(PROGHTD-PROGD),C'T'                                    
         BE    *+8                                                              
         CLI   PROGPROF+(PROGHTD-PROGD),C'H'                                    
         BNE   *+8                                                              
         BAS   RE,HNTDATE                                                       
*                                                                               
         GOTO1 SQUASHER,DMCB,WORK01,76                                          
         GOTO1 CHOPPER,DMCB,(76,WORK01),(30,PRTNAM),(C'P',3)                    
         ZAP   ITEM,=P'0'                                                       
         L     R4,ADACC                                                         
         MVI   ELCODE,MSAELQ                                                    
         BAS   RE,GETEL                                                         
         DROP  R2                                                               
*                                                                               
PA65     BNE   PA88                                                             
         CLI   RCSUBPRG,1                                                       
         BE    PA70                                                             
         USING MSAELD,R4                                                        
         CLC   MSABEG,END                                                       
         BH    PA68                STARTS AFTER END                             
         OC    MSAEND,MSAEND                                                    
         BZ    PA67                                                             
         CLC   MSAEND,START                                                     
         BL    PA68                ENDS BEFORE START                            
PA67     AP    ITEM,=P'1'                                                       
         BAS   RE,LIST                                                          
         BAS   RE,PRNTIT                                                        
PA68     BAS   RE,NEXTEL                                                        
         B     PA65                                                             
*                                                                               
         EJECT                                                                  
* *******************************************************************           
* ACCLAST FOR COST LISTING                                                      
* *******************************************************************           
*                                                                               
PA70     BAS   RE,CALC             GET AMOUNTS FOR ACCOUNT                      
         LA    R5,ACCTOT           ACCOUNT TOTALS                               
         USING LEVD,R5             USE STORAGE DSECT                            
         CLI   NUMLEV,LEVMAX       DO WE HAVE 4 LEVELS?                         
         BE    *+8                 YES                                          
         LA    R5,LLEVC            NO, LLEVC WILL HOLD TOTALS                   
         LA    R2,LEVCST           PT TO LIST OF COSTS SAVED                    
         BAS   RE,FORMAT           OUTPUT THEM ON PRINT LINE                    
         CP    ITEM,=P'0'          WAS ONLY SALARY OUPUT?                       
         BE    PA88                YES, DON'T OUTPUT ANY TOTAL COLUMN           
         BAS   RE,PRNTIT           NO, PRINT OUT TOTAL FOR LINE                 
*                                                                               
         LA    R6,LLEVC            ADD ACCOUNT TOTALS TO SUB-DEPT               
         CLI   NUMLEV,LEVMAX       DO WE HAVE 4 LEVELS?                         
         BE    *+8                 YES,                                         
         LA    R6,LLEVB            IF 3 LEVEL LEDGER, USE LEVB                  
         BAS   RE,ADDUP            ADD UP TOTALS FOR LEVELS-1                   
         BAS   RE,CLRBK            CLEAR THE LEDGER AFTER TOTS GOTTEN           
         B     XIT                                                              
*                                                                               
PA88     CP    ITEM,=P'0'          IF ONLY SALARY OUTPUT, DO BLANK LINE         
         BNE   XIT                 IF MORE STUFF OUTPUT, NO BLANK LINE          
         MVC   P,SPACES            BLANK LINE                                   
         MVC   PSECOND,SPACES      CLEAR NEXT LINE AS WELL                      
         B     XIT                                                              
         EJECT                                                                  
* ********************************************************************          
* LVCL - LEVEL C LAST                                                           
* ********************************************************************          
*                                                                               
LVCL     CLI   RCSUBPRG,1          IGNORE IF NOT COST LISTING                   
         BNE   XIT                                                              
         LA    R5,LLEVC                                                         
         BAS   RE,TOTALL           SUB-DEPT TOTALS                              
         LA    R6,LLEVB                                                         
         BAS   RE,ADDUP            ADD SUB DEPT (R5) TO DEPT (R6)               
         BAS   RE,CLRBK                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* ********************************************************************          
* LVBL - LEVEL B LAST                                                           
* ********************************************************************          
         SPACE 1                                                                
LVBL     CLI   RCSUBPRG,1                                                       
         BNE   XIT                                                              
         LA    R5,LLEVB                                                         
         BAS   RE,TOTALL            DEPT TOTALS                                 
         LA    R6,LLEVA                                                         
         BAS   RE,ADDUP            ADD DEPT(R5) TO OFFICE(R6) TOTALS            
         BAS   RE,CLRBK                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* ********************************************************************          
* LVAL - LEVEL A LAST                                                           
* ********************************************************************          
         SPACE 1                                                                
LVAL     CLI   RCSUBPRG,1                                                       
         BNE   XIT                                                              
         LA    R5,LLEVA                                                         
         BAS   RE,TOTALL           OFFICE TOTALS                                
         LA    R6,REQTOT                                                        
         BAS   RE,ADDUP            ADD OFFICE(R5) TO REQUEST(R6)                
         BAS   RE,CLRBK                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* ********************************************************************          
* REQL - REQUEST LAST                                                           
* ********************************************************************          
         SPACE 1                                                                
REQL     MVI   FRSTSUB,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
         CLI   RCSUBPRG,1                                                       
         BNE   XIT                                                              
         LA    R5,REQTOT                                                        
         BAS   RE,TOTALL           REQUEST TOTALS                               
         BAS   RE,CLRBK                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* ********************************************************************          
* LIST - ROUTINE TO PRING SALARY ELEMENTS                                       
* ********************************************************************          
         SPACE 1                                                                
LIST     NTR1                                                                   
         USING MSAELD,R4                                                        
         LA    R5,TYPETBL                                                       
         USING SALTYPD,R5                                                       
*                                                                               
LIST1    CLI   SALTYPE,X'FF'       END OF TABLE?                                
         BNE   *+6                 NO, KEEP COMPARING                           
         DC    H'0'                YES, UNKNOWN SALARY TYPE                     
         CLC   MSATYPE,SALTYPE     COMPARE RECORDS TYPE TO TABLE                
         BE    LIST2               MATCH, GET ABBREVIATION                      
         LA    R5,SALTYQ(R5)       NO MATCH,.BUMP TO NEXT SAL TYPE              
         B     LIST1               LOOK AT NEXT TABLE ENTRY                     
*                                                                               
LIST2    LA    R2,P                PT TO PRINT LINE                             
         USING PRT3D,R2            USE SALARY PRT LN DSECT                      
         MVC   PRTTYP(L'SALABRV),SALABRV    SALARY ABRV ON PRT LN               
         MVC   WORK(2),MSABEG                                                   
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(9,PRTBEG)                                  
         OC    MSAEND,MSAEND                                                    
         BZ    LIST4                                                            
         MVC   WORK(2),MSAEND                                                   
         GOTO1 DATCON,DMCB,(1,WORK),(9,PRTEND)                                  
         MVI   PRTSPC,C','                                                      
*                                                                               
LIST4    LA    R5,BASETBL                                                       
LIST5    CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   MSABASIS,0(R5)                                                   
         BE    LIST6                                                            
         LA    R5,3(R5)                                                         
         B     LIST5                                                            
*                                                                               
LIST6    MVC   PRTBASE,0(R5)                                                    
         TM    MSASTAT,MSAS2DP                                                  
         BO    LIST7                                                            
         TM    MSASTAT,MSAS5DP                                                  
         BO    LIST8                                                            
         EDIT  (P6,MSALARY),(12,PRTSLR1),2,MINUS=YES                            
         B     LISTX                                                            
*                                                                               
LIST7    EDIT  (P6,MSALARY),(12,PRTSLR2),2,MINUS=YES,TRAIL=C'%'                 
         B     LISTX                                                            
*                                                                               
LIST8    EDIT  (P6,MSALARY),(14,PRTSLR2),5,MINUS=YES,TRAIL=C'%'                 
         B     LISTX                                                            
*                                                                               
LISTX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
* ********************************************************************          
* CALC - ROUTINE TO CALCULATE COST FOR ACCOUNT                                  
* ********************************************************************          
         SPACE 1                                                                
CALC     NTR1                                                                   
         USING LEVD,R5                                                          
         USING SLRD,R6                                                          
         ZAP   BONUS,=P'0'         INITIALIZE BONUS AMT TO 0                    
         ZAP   PENSION,=P'0'       INITIALIZE PENSION AMT TO 0                  
         ZAP   BENEFIT,PCTBENEF    DEFAULT BENEFIT & ADMIN FROM                 
         ZAP   ADMIN,PCTADMIN      NUMBER ELEMENTS--3 DEC PLACES                
         SRP   BENEFIT,2,0         FORCE TO  5 DECIMAL PLACES                   
         SRP   ADMIN,2,0           FORCE TO  5 DECIMAL PLACES                   
*                                                                               
         LA    R5,LLEVA            LOOP THRU LEVS GETTING PERCENTS              
         LA    R3,LEVMAX           SET COUNTER TO MAX # LEVELS                  
         CLI   NUMLEV,LEVMAX       ARE ALL (4) LEVELS PRESENT?                  
         BE    CALC1               YES, DON'T ADJUST COUNTER                    
         LA    R3,LEVMAX-1         NO, WE HAVE 1 LESS LEVEL                     
*                                                                               
CALC1    CP    LEVBONPC,=P'0'      MERGE BONUS PERCENT FROM HIGHER LEVS         
         BE    *+10                NO BONUS PERCENT                             
         ZAP   BONUS,LEVBONPC      SAVE BONUS PERCENT                           
         CP    LEVBENPC,=P'0'      GET BENEFIT PERCENT                          
         BE    *+10                NO BENEFIT PERCENT                           
         ZAP   BENEFIT,LEVBENPC    SAVE BENEFIT PERCENT                         
         CP    LEVADMPC,=P'0'      GET ADMINISTRATIVE PERCENT                   
         BE    *+10                NO ADMIN PERCENT                             
         ZAP   ADMIN,LEVADMPC      SAVE ADMIN PERCENT                           
         CP    LEVPENPC,=P'0'      GET PENSION PERCENT                          
         BE    *+10                NO PENSION PERCENT                           
         ZAP   PENSION,LEVPENPC    SAVE PENSION PERCENT                         
         LA    R5,LLEVQ(R5)        NEXT LEVEL SAVE AREA                         
         BCT   R3,CALC1            PROCESS ALL LEVELS UNTILL NO MORE            
*                                                                               
         SH    R5,=AL2(LLEVQ)      ADDRESS OF LAST LEVEL PARSED                 
         L     R4,ADACC            PT TO ACCOUNT DATA                           
         LA    R6,SALAREA          PT TO SALARY SAVE AREA                       
*        GOTO1 ACSLRY,DMCB,(R4),START,(R6)   GET SALARY INFO                    
         GOTO1 ACSLRY,DMCB,(X'80',(R4)),START,(R6),ADCOMFAC                     
*                                                                               
         LA    R6,SALAREA          PT TO SALARY INFO EXTRACTED                  
         LA    R7,SLRSAL           START SUMMING FROM SALARY                    
         CLI   OHDACC,C'Y'         IS THIS AN OVERHEAD ACCT                     
         BNE   CALC2               NO, SKIP SPECIAL PROCESSING                  
         AP    LEVOVH,SLRSAL       IF OVERHEAD JUST ADD IT                      
         AP    LEVSLR,SLRSAL                                                    
         B     CALC8                                                            
*                                                                               
CALC2    LA    R8,LEVCST           PT TO SUM FIELDS                             
         LA    R3,SALLNQ           LENGTH OF SALARY LIST TO PROCESS             
         ZAP   DUB,=P'0'                                                        
*                                                                               
CALC3    TM    SLRBKLN(R7),SLR5DP  IS THIS A PERCENT?                           
         BO    CALC4               YES, DON'T ADD PERCENTS                      
         TM    SLRBKLN(R7),SLR2DP  IS THIS A PERCENT?                           
         BO    CALC4               YES, DON'T ADD PERCENTS                      
         ZAP   0(LEVSZQ,R8),0(LEVSZQ,R7) AMOUNT TO ACCUMULATES (LEVD)           
         AP    DUB,0(SLRBKLN,R7)     ADD TO TOTAL                               
*                                                                               
CALC4    LA    R8,LEVSZQ(R8)       BUMP TO NEXT COST BUCKET                     
         LA    R7,SLRBKLN+1(R7)    NEXT SALARY ITEM +1 BYTE FOR STATUS          
         BCT   R3,CALC3            GET ALL ITEMS IN SAL LIST TO SUM             
*                                                                               
         TM    SLRPENST,SLR5DP     IS THIS A PERCENT?                           
         BO    CALC5               YES, DON'T ADD PERCENTS                      
         TM    SLRPENST,SLR2DP     IS THIS A PERCENT?                           
         BO    CALC5               YES, DON'T ADD PERCENTS                      
         ZAP   LEVPEN,SLRPEN       AMOUNT TO ACCUMULATES (LEVD)                 
         AP    DUB,SLRPEN          ADD TO TOTAL                                 
*                                                                               
CALC5    CP    LEVBON,=P'0'        DID WE GET A BONUS AMT FROM A LEVEL          
         BNE   CALC6               YES, WE HAVE A BONUS AMT                     
         CP    BONUS,=P'0'         DO WE HAVE A DEFAULT BONUS AMT?              
         BE    CALC6               NO BONUS PERCENT                             
         ZAP   WRK,DUB             HAVE DEFAULT. COPY TOTAL SALARIES            
         MP    WRK,BONUS           SALARY X DEFAULT PERCENT (5-DP)              
         SRP   WRK,64-7,5          DIVIDE TO GET 2 DEC PLACES                   
         ZAP   LEVBON,WRK+10(LEVSZQ) BONUS AMOUNT -                             
*                                                                               
CALC6    CP    LEVBEN,=P'0'        BENEFIT AMT                                  
         BNE   CALC7               WE HAVE BENEFIT % FROM A LEVEL               
         CP    BENEFIT,=P'0'       IS THERE A DEFAULT BENEFIT %                 
         BE    CALC7               NO BENEFIT PERCENT                           
         ZAP   WRK,DUB             HAVE A DEFAULT, USE IT. COPY TOTALS          
         MP    WRK,BENEFIT         SALARY X DEFAULT BENEFIT PERCENT             
         SRP   WRK,64-7,5          DIVIDE TO GET 2 DEC PLACES                   
         ZAP   LEVBEN,WRK+10(LEVSZQ) SAVE BENEFIT AMT                           
*                                                                               
CALC7    CP    LEVADM,=P'0'        ADMIN AMT                                    
         BNE   CALC7A              WE HAVE BENEFIT % FROM A LEVEL               
         CP    ADMIN,=P'0'         IS THERE A DEFAULT ADMIN                     
         BE    CALC7A              NO ADMIN PERCENT                             
         ZAP   WRK,DUB             HAVE A DEFAULT, USE IT. COPY TOTALS          
         MP    WRK,ADMIN           SALARY X DEFAULT BENEFIT PERCENT             
         SRP   WRK,64-7,5          DIVIDE TO GET 2 DEC PLACES                   
         ZAP   LEVADM,WRK+10(LEVSZQ) SAVE ADMIN AMT                             
*                                                                               
CALC7A   CP    LEVPEN,=P'0'        ANY PEN AMT ALREADY?                         
         BNE   CALC8               WE HAVE BENEFIT % FROM A LEVEL               
         CP    PENSION,=P'0'       IS THERE A PENSION PERCENT?                  
         BE    CALC8               NO                                           
         ZAP   WRK,DUB             GET TOTAL SALARY                             
         MP    WRK,PENSION         SALARY X PENSION PERCENT                     
         SRP   WRK,64-7,5          DIVIDE TO GET 2 DEC PLACES                   
         ZAP   LEVPEN,WRK+10(LEVSZQ) SAVE PENSION AMOUNT                        
*                                                                               
CALC8    LA    R3,LEVSALQ          ADD COSTS TO TOTAL COLUMN                    
         ZAP   DUB,=P'0'           CLEAR COUNTER                                
         LA    R8,LEVCST           PT TO COST FIELD                             
*                                                                               
CALC9    AP    DUB,0(LEVSZQ,R8)    CALULATE THE TOTAL COST                      
         LA    R8,LEVSZQ(R8)       BUMP TO NEXT COST FIELD                      
         BCT   R3,CALC9            LOOP THRU ALL COST FIELDS                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* ********************************************************************          
* EMPBEN -  ROUTINE TO FIND BENEFIT/ ADMIN PCT. IN NUMBER ELEMENTS              
* ********************************************************************          
         SPACE 1                                                                
EMPBEN   NTR1                       FIND EMPLOYEE BENEFIT PERCENTAGE            
         LR    R4,R2                                                            
         MVI   ELCODE,OTHELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   ADMINS                                                           
         USING OTHELD,R4                                                        
         MVC   WORK(5),=5X'F0'                                                  
         MVZ   WORK(5),OTHNUM                                                   
         CLC   WORK(5),=5X'F0'                                                  
         BNE   ADMINS                                                           
         PACK  PCTBENEF,OTHNUM(5)                                               
         SPACE 1                                                                
ADMINS   LR    R4,R2                                                            
         MVI   ELCODE,FFNELQ       GET ADMINISTRATIVE PERCENT                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING FFNELD,R4                                                        
         MVC   WORK(5),=5X'F0'                                                  
         MVZ   WORK(5),FFNUMBER                                                 
         CLC   WORK(5),=5X'F0'                                                  
         BNE   XIT                                                              
         PACK  PCTADMIN,FFNUMBER(5)                                             
         B     XIT                                                              
         EJECT                                                                  
* ********************************************************************          
* ADDUP- ADD TO HIGHER LEVEL                                                    
* ********************************************************************          
         SPACE 1                                                                
ADDUP    NTR1                                                                   
         LA    R3,LEVSALQ+1        # OF COST FIELDS+ LEVOVH FIELD               
         AP    0(LEVSZQ,R6),0(LEVSZQ,R5)                                        
         LA    R6,LEVSZQ(R6)                                                    
         LA    R5,LEVSZQ(R5)                                                    
         BCT   R3,*-14                                                          
         B     XIT                                                              
         EJECT                                                                  
* ********************************************************************          
* FORMAT - PLACE COST FIELDS ON THE PRINT LINE                                  
*          (R2) - PTS TO THE TOP OF THE LEVCST LIST                             
* ********************************************************************          
         SPACE 1                                                                
FORMAT   NTR1                                                                   
         ST    R2,SAVEADR          SAVE ADDRESS OF COST LIST                    
         LA    R7,P+PRT4CSTQ       EDIT SALARY                                  
         MVI   TOTSW,C'N'                                                       
         EDIT  (P6,0(R2)),(12,0(R7)),2,MINUS=YES,ZERO=BLANK                     
         ZAP   TOTAL,0(LEVSZQ,R2)       ADD TO TOTAL                            
         CP    0(LEVSZQ,R2),=P'0'                                               
         BE    *+10                                                             
         AP    ITEM,=P'1'                                                       
         LA    R7,COLMQ(R7)        BUMP TO NEXT OUTPUT COLUMN ON REPT           
         LA    R2,LEVSZQ(R2)       BUMP TO NEXT FIELD IN LEVCST LIST            
         LA    R3,LEVSALQ-1        NUMBER OF COST FIELDS-1(FOR SALARY)          
         LA    R6,PROGPROF         PT TO THE PROFILES FILTER LIST               
         LA    RF,PROGPROF+PROGNPG-PROGD  ADDRESS OF NON-SALARY FIELD           
         ST    RF,IGNOREA          IGNORE DATA AT THIS ADDRESS                  
         LA    R4,1                COLUMN COUNTER                               
         STC   R4,COLCNT           1 COL=SALARY ON REPORT ALREADY               
*                                                                               
FORMAT1  CLI   0(R6),C'Y'                                                       
         BNE   FORMAT3             DON'T PRINT THIS SALARY TYPE                 
         C     R6,IGNOREA          IS THIS THE ADDRESS TO BYPASS?               
         BE    FORMAT3             YES,IT'S NOT AN SAL TYPE OPT FIELD           
         CLI   COLCNT,MAXCOLQ      HAVE THE MAX # COLS BEEN DISLYD?             
         BE    FORMAT4             YES, DO THE TOTAL                            
         EDIT  (P6,0(R2)),(12,0(R7)),2,MINUS=YES,ZERO=BLANK                     
         CP    0(LEVSZQ,R2),=P'0'  WAS THIS COST ZERO?                          
         BE    *+10                YES, DON'T INCR CNTR                         
         AP    ITEM,=P'1'          INCR TOTAL NUMBER NON ZERO ITEMS             
         AP    TOTAL,0(LEVSZQ,R2)  ADD COST FIELD AMT TO TOTAL                  
         MVI   TOTSW,C'Y'          PRINT A TOTAL                                
         LA    R7,COLMQ(R7)        NEXT COLUMN POSITION ON PAGE                 
         LA    R4,1(R4)            BUMP COLUMN COUNTER                          
         STC   R4,COLCNT           SAVE AWAY                                    
*                                                                               
FORMAT3  LA    R2,LEVSZQ(R2)                                                    
         LA    R6,1(R6)                                                         
         BCT   R3,FORMAT1                                                       
*                                                                               
         CLI   OTHOPT,C'Y'         WAS 'OTHER' OPTION REQSTD & DISPLY           
         BNE   FORMAT4             NO.DON'T OUTPUT 'OTHER'.DO TOTAL             
         BAS   RE,OTHCALC          CALCULATE THE 'OTHER' AMOUNT                 
         EDIT  (P6,OTHER),(12,0(R7)),2,MINUS=YES,ZERO=BLANK                     
         LA    R7,COLMQ(R7)        BUMP TO TOTAL COLUMN                         
         MVI   TOTSW,C'Y'          PRINT A TOTAL                                
*                                                                               
FORMAT4  CLI   TOTSW,C'Y'                                                       
         BNE   XIT                 DON'T PRINT TOTAL                            
         EDIT  (P6,TOTAL),(12,0(R7)),2,MINUS=YES,ZERO=BLANK                     
         B     XIT                                                              
         EJECT                                                                  
* ********************************************************************          
* OTHCALC-  ROUTINE TO CALCULATE THE  'OTHER' AMOUNT.                           
*           'OTHER' = THE TOTAL COST - THE COST FIELDS DISPLYD                  
* ********************************************************************          
*                                                                               
OTHCALC  NTR1                      SAVE AWAY REGISTERS                          
         LA    R3,LEVSALQ          # COST BUCKETS                               
         L     R4,SAVEADR          PT TO FIRST BUCKET IN LIST                   
         ZAP   OTHER,=P'0'         INNITIALIZE TOTAL TO ZERO                    
         AP    OTHER,0(LEVSZQ,R4)         ADD SALARY TO TOTAL                   
         LA    R4,LEVSZQ(R4)       BUMP TO NEXT BUCKET                          
         BCT   R3,*-10             KEEP SUMMING UNTIL END OF LIST               
*                                                                               
         SP    OTHER,TOTAL         ACTUAL TOTAL-DISPLAYED COLS TOTAL            
         AP    TOTAL,OTHER         TOTAL NOW INCLUDES 'OTHER'                   
         B     XIT                 RETURN TO CALLER                             
* ********************************************************************          
* CLRBK -   ROUTINE TO CLEAR BUCKETS                                            
* ********************************************************************          
*                                                                               
         USING LEVD,R5                                                          
CLRBK    MVC   LEVCST(LEVCSTQ),PKZEROS                                          
         BR    RE                                                               
         EJECT                                                                  
* ********************************************************************          
* SALINFO- SAVE SALARY AND BENEFIT INFO IF X'20' SET. R5 PTS TO LEVD.           
* ********************************************************************          
* MAKE ALL PERCENTS TO 5 DECIMAL PLACES                                         
SALINFO  LA    R6,SALAREA          PT TO SALARY INFO                            
         USING SLRD,R6             USE ITS DSECT                                
         TM    SLRBONST,SLR5DP     X'10' PERCENT 5 DEC PLACES                   
         BNZ   SALINF1             SAVE PERCENT AT THIS LEVEL                   
         TM    SLRBONST,SLR2DP     X'20' PERCENT 2 DEC PLACES                   
         BZ    SALINF2             NO, WE DON'T HAVE A PERCENT                  
         SRP   SLRBON,3,0          NOW PERCENT IS 5 DEC PLACES                  
         MVI   SLRBONST,SLR5DP     SET FLAG TO 5 DEC PLACES                     
*                                                                               
SALINF1  ZAP   LEVBONPC,SLRBON     SAVE BONUS PERCENT AT THIS LEVEL             
*                                                                               
SALINF2  TM    SLRBENST,SLR5DP     X'10' PERCENT 5 DEC PLACES                   
         BNZ   SALINF3             5 DECIMAL PT PERCENT, SAVE                   
         TM    SLRBENST,SLR2DP     2 DEC PLACES- MAKE 5 DP                      
         BZ    SALINF4             NOT A PERCENT, DONE                          
         SRP   SLRBEN,3,0          NOW PERCENT IS 5 DEC PLACES                  
         MVI   SLRBENST,SLR5DP     SET FLAG TO 5 DEC PLACES                     
*                                                                               
SALINF3  ZAP   LEVBENPC,SLRBEN     SAVE BENEFIT PERCENT AT THIS LEVEL           
*                                                                               
SALINF4  TM    SLRPENST,SLR5DP     X'10' PERCENT 5 DEC PLACES - PENSION         
         BNZ   SALINF5             5 DECIMAL PT PERCENT, SAVE                   
         TM    SLRPENST,SLR2DP     2 DEC PLACES- MAKE 5 DP - PENSION            
         BZ    SALINF6             NOT A PERCENT, DONE                          
         SRP   SLRPEN,3,0          NOW PERCENT IS 5 DEC PLACES                  
         MVI   SLRPENST,SLR5DP     SET FLAG TO 5 DEC PLACES                     
*                                                                               
SALINF5  ZAP   LEVPENPC,SLRPEN     SAVE PENSION PERCENT AT THIS LEVEL           
*                                                                               
SALINF6  TM    SLRADMST,SLR5DP     X'10' PERCENT 5 DEC PLACES - PENSION         
         BNZ   SALINF7             5 DECIMAL PT PERCENT, SAVE                   
         TM    SLRADMST,SLR2DP     2 DEC PLACES- MAKE 5 DP - PENSION            
         BZ    SALINFX             NOT A PERCENT, DONE                          
         SRP   SLRADM,3,0          NOW PERCENT IS 5 DEC PLACES                  
         MVI   SLRADMST,SLR5DP     SET FLAG TO 5 DEC PLACES                     
*                                                                               
SALINF7  ZAP   LEVADMPC,SLRADM     SAVE ADMIN PERCENT AT THIS LEVEL             
*                                                                               
SALINFX  BR    RE                                                               
*                                                                               
         EJECT                                                                  
* ********************************************************************          
* PRNTIT -  ROUNTINE TO HANDLE PRINTING                                         
* ********************************************************************          
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         BAS   RE,HEADPG           HEADUP THE PAGE                              
         CLI   MODE,LEVAFRST                                                    
         BE    PRNT4                                                            
         CLI   PROGPROF+PROGNPG-PROGD,C'Y'     NEW PAGE FOR SUB-DEPT            
         BE    PRNT3                                                            
         CLI   FRSTSUB,C'Y'                                                     
         BNE   PRNT3               NOT FIRST FOR THIS SUB-DEPT                  
         MVC   SVP1,P                                                           
         MVC   SVP2,PSECOND                                                     
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         GOTO1 ACREPORT            DO A BLANK LINE                              
         BAS   RE,HEADPG                                                        
         MVC   P,SVP1                                                           
         MVC   PSECOND,SVP2                                                     
         MVI   FRSTSUB,C'N'                                                     
         MVI   FORCEMID,C'Y'                                                    
         LA    R2,MID1+1                                                        
         LA    R5,LLEVC                                                         
         CLI   NUMLEV,LEVMAX                                                    
         BE    *+8                                                              
         LA    R5,LLEVB                                                         
         MVC   0(58,R2),LEVACC                                                  
         GOTO1 SQUASHER,DMCB,(R2),58                                            
         GOTO1 UNDERLIN,DMCB,(58,(R2)),MID2+1                                   
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         SPACE 1                                                                
PRNT3    CLI   P+1,C' '                                                         
         BE    PRNT4                                                            
         MVC   SVP1,P                                                           
         MVC   SVP2,PSECOND                                                     
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         GOTO1 ACREPORT            DO A BLANK LINE                              
         BAS   RE,HEADPG           IF FIRST FOR ACCOUNT                         
         MVC   P,SVP1                                                           
         MVC   PSECOND,SVP2                                                     
PRNT4    GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
* ********************************************************************          
* TOTALL -  PRINT OVERALL TOTALS                                                
* ********************************************************************          
         SPACE 1                                                                
TOTALL   NTR1                                                                   
         LA    R2,LEVCST                                                        
         BAS   RE,FORMAT                                                        
         LA    R7,P                PT TO PRINT LINE                             
         USING PRT4D,R7            USE COST LISTING DSECT                       
         CLC   PRT4CSTS,SPACES     ARE ALL COST FIELDS BLANK?                   
         BE    TOTALLX             YES, NOT TOTALS TO PRINT                     
         MVC   PRT4LIT1,=C'TOTAL FOR '                                          
         GOTO1 CHOPPER,DMCB,(46,LEVNAM),(25,PRT4NAM),(C'P',2)                   
         MVC   SVP1,P                                                           
         MVC   SVP2,PSECOND                                                     
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         BAS   RE,HEADPG                                                        
         GOTO1 ACREPORT                                                         
         MVC   P,SVP1                                                           
         MVC   PSECOND,SVP2                                                     
         BAS   RE,HEADPG                                                        
         GOTO1 ACREPORT                                                         
         SPACE 1                                                                
         BAS   RE,TOTSAL           PRINT SALARY TOTALS                          
         BAS   RE,TOTOVH           AND OVERHEAD TOTALS                          
TOTALLX  B     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
* ********************************************************************          
* TOTSAL -  PRINT TOTAL SALARIES LINE                                           
* ********************************************************************          
TOTSAL   NTR1                                                                   
         CP    LEVOVH,=P'0'        IF NO OVERHEAD                               
         BE    XIT                 DON'T PRINT SUB TOTALS                       
         SP    LEVSLR,LEVOVH       TAKE OVERHEAD OUT OF TOTALS                  
         LA    R2,LEVCST           TO GET SALARIES                              
         BAS   RE,FORMAT           EDIT SALARY DATA                             
         AP    LEVSLR,LEVOVH       ADD OVERHEAD BACK TO TOTAL                   
         LA    R7,P                PT TO PRINT LINE                             
         USING PRT4D,R7            USE COST LISTING DSECT                       
         CLC   PRT4CSTS,SPACES                                                  
         BE    XIT                 NOTHING GOOD TO PRINT                        
         MVC   PRT4TYP,=C'*SALARIES*'                                           
         BAS   RE,HEADPG                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
               EJECT                                                            
* ********************************************************************          
* TOTOVH -  PRINT TOTAL OVERHEAD                                                
* ********************************************************************          
         SPACE 1                                                                
TOTOVH   NTR1                                                                   
         LR    R4,R5               SAVE ADDRESS OF BUCKETS                      
         LA    R5,WRKTOT                                                        
         MVC   LEVCST(LEVCSTQ),PKZEROS                                          
         LR    R5,R4               RESTORE ADDRESS OF BUCKETS                   
         LA    R4,WRKTOT                                                        
         ZAP   LEVCST-LEVD(LEVSZQ,R4),LEVOVH  O/H TO WORK AREA TO PRT           
         LA    R2,LEVCST-LEVD(R4)                                               
         BAS   RE,FORMAT                                                        
         LA    R7,P                PT TO PRINT LINE                             
         USING PRT4D,R7            USE COST LISTING DSECT                       
         CLC   PRT4CSTS,SPACES                                                  
         BE    TOTOVHX             NOTHING TO PRINT                             
         MVC   PRT4TYP,=C'*OVERHEAD*'                                           
         BAS   RE,HEADPG                                                        
         GOTO1 ACREPORT                                                         
TOTOVHX  B     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
* ********************************************************************          
* HEADPG -  ROUTINE TO SET UP THE PAGE HEADER                                   
* ********************************************************************          
         SPACE 1                                                                
HEADPG   NTR1                                                                   
         LA    R2,HEAD3            PT TO 3RD LINE OF HEADER                     
         USING HDRD,R2             USE HEADER DSECT                             
         MVC   HDRCMPL,=C'COMPANY' MOVE IN COMPANY LITERAL                      
         ZIC   R3,LONGDES          WHAT IS LENGTH OF LONGEST DESCPTN            
         AR    R2,R3               ALIGN DATA                                   
         LA    R2,1(R2)            LEAVE A  SPACES BETWN LABEL & DATA           
         MVC   HDRCMP,COMPNAM      MOVE IN COMPANY NAME                         
*                                                                               
         CLI   MODE,REQLAST        IS THIS THE LAST REQUEST?                    
         BE    HEADPG7             YES, NO LEVEL NAMES TO MOVE IN               
         LA    R2,HEAD4            NO. GO TO NEXT LINE DOWN                     
         LA    R5,LLEVA            PT TO LEV A INFO                             
         BAS   RE,DESACC           PUT DESCRPTN & ACCT NAME ON PRT LN           
*                                                                               
         CLI   NUMLEV,LEVMAX-1     DO WE HAVE LESS THAN MAX LEVS?               
         BE    HEADPG3             YES, DON'T CONTINUE                          
         CLI   MODE,LEVAFRST       ARE WE DOING LEVA PROCESSING?                
         BE    HEADPG3             YES, DON'T CONTINUE                          
         LA    R2,HEAD5            NO. GO TO NEXT LINE DOWN                     
         LA    R5,LLEVB            PT TO LEV B INFO                             
         BAS   RE,DESACC           PUT DESCRPTN & ACCT NAME ON PRT LN           
*                                                                               
         CLI   PROGPROF+PROGNPG-PROGD,C'Y'    NEW PAGE FOR SUB DEPT?            
         BNE   HEADPG7             NO, DON'T PAGE BREAK                         
         CLI   FRSTSUB,C'Y'        IS A BREAK NEEDED                            
         BNE   *+12                NO, SO DON'T RESET                           
         MVI   FRSTSUB,C'N'        RESET PAGE BREAK STATUS                      
         MVI   FORCEHED,C'Y'       SET PAGE BREAK                               
         LA    R2,HEAD6            NEXT LINE DOWN                               
         LA    R5,LLEVC            PT TO LEV C INFO                             
         BAS   RE,DESACC           PUT DESCRPTN & ACCT NAME ON PRT LN           
         B     HEADPG7             SKIP SPECIAL LEVEL PROCESSING                
*                                                                               
HEADPG3  CLI   PROGPROF+PROGNPG-PROGD,C'Y'   NEW PAGE FOR SUB-DEPT?             
         BNE   HEADPG7                                                          
         CLI   FRSTSUB,C'Y'                                                     
         BNE   *+12                                                             
         MVI   FRSTSUB,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,HEAD5                                                         
         LA    R5,LLEVB                                                         
         BAS   RE,DESACC           PUT DESCRPTN & ACCT NAME ON PRT LN           
*                                                                               
HEADPG7  LA    R2,HEAD4                                                         
         MVC   HDRPER,PERIOD       MOVE IN PERIOD LINE ON RT SIDE OF PG         
         CLI   RCSUBPRG,1                                                       
         BNE   HEADPGX                                                          
         LA    R2,HEAD8                                                         
         MVC   HDRMYHD,MYHEAD                                                   
*                                                                               
HEADPGX  B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
* ********************************************************************          
* DESACC -  ROUTINE TO PLACE DESCRIPTION AND ACCOUNT NAME ON PRT LINE           
*           R2- ADDRESS OF BEGINING OF HEAD LINE                                
*           R5- PTS TO LLEVA/B/C                                                
* ********************************************************************          
*                                                                               
         USING HDRD,R2             USE HEAD LINE DSECT                          
         USING LEVD,R5             USE LEVEL STORAGE DSECT                      
DESACC   NTR1                                                                   
         ZIC   R3,LONGDES          WHAT IS LENGTH OF LONGEST DESCPTN            
         LR    R4,R2               PT TO BEGINING OF COLUMN                     
         AR    R4,R3               BUMP TO END OF 1ST FIELD                     
         LA    R4,2(R4)            PT TO BEGINING OF NEXT FIELD                 
         BCTR  R3,0                DECR FOR EXMVC                               
         EXMVC R3,HDRDES,LEVDES    MOVE IN DESCRIPTION LABEL                    
         LR    R2,R4               BUMP TO NEXT FIELD                           
         MVC   HDRACC,LEVACC       MOVE IN ACCOUNT NAME                         
         GOTO1 SQUASHER,DMCB,(R2),L'HDRACC   REMOVE EXTRA BLANKS                
         B     XIT                                                              
         DROP  R2                                                               
         DROP  R5                                                               
*                                                                               
* ********************************************************************          
* HEADUP -  ROUTINE TO HEADUP A PAGE                                            
* ********************************************************************          
         SPACE 1                                                                
         USING LEVD,R5             USE LEVEL STORAGE DSECT                      
HEADUP   NTR1                                                                   
         MVC   HEAD3+1(7),=C'COMPANY'                                           
         ZIC   R3,LONGDES                                                       
         LA    R2,HEAD3+1                                                       
         LA    R2,2(R3,R2)                                                      
         MVC   0(36,R2),COMPNAM    COMPANY NAME                                 
         SPACE 1                                                                
         CLI   MODE,REQLAST                                                     
         BE    HEADUP7                                                          
         LA    R2,132(R2)                                                       
         LA    R5,LLEVA            LEVEL A  ACCOUNT AND NAME                    
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   HEAD4+1(0),LEVDES                                                
         MVC   0(58,R2),LEVACC                                                  
         GOTO1 SQUASHER,DMCB,(R2),L'HDRACC                                      
         SPACE 1                                                                
         CLI   NUMLEV,LEVMAX-1                                                  
         BE    HEADUP3                                                          
         CLI   MODE,LEVAFRST                                                    
         BE    HEADUP3                                                          
         LA    R2,132(R2)          LEVEL B ACCOUNT AND NAME                     
         LA    R5,LLEVB                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   HEAD5+1(0),LEVDES                                                
         MVC   0(58,R2),LEVACC                                                  
         GOTO1 SQUASHER,DMCB,(R2),58                                            
         SPACE 1                                                                
         CLI   PROGPROF+PROGNPG-PROGD,C'Y'    NEW PAGE FOR SUB-DEPT             
         BNE   HEADUP7                                                          
         CLI   FRSTSUB,C'Y'                                                     
         BNE   *+12                                                             
         MVI   FRSTSUB,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,132(R2)                                                       
         LA    R5,LLEVC                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   HEAD6+1(0),LEVDES                                                
         MVC   0(58,R2),LEVACC                                                  
         GOTO1 SQUASHER,DMCB,(R2),58                                            
         B     HEADUP7                                                          
         SPACE 1                                                                
HEADUP3  CLI   PROGPROF+PROGNPG-PROGD,C'Y'     NEW PAGE FOR SUB-DEPT            
         BNE   HEADUP7                                                          
         CLI   FRSTSUB,C'Y'                                                     
         BNE   *+12                                                             
         MVI   FRSTSUB,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,132(R2)                                                       
         LA    R5,LLEVB                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   HEAD5+1(0),LEVDES                                                
         MVC   0(58,R2),LEVACC                                                  
         GOTO1 SQUASHER,DMCB,(R2),58                                            
         SPACE 1                                                                
HEADUP7  MVC   HEAD4+100(L'PERIOD),PERIOD                                       
         CLI   RCSUBPRG,1                                                       
         BNE   *+10                                                             
         MVC   HEAD8+50(L'MYHEAD),MYHEAD                                        
         B     XIT                                                              
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 1                                                                
         USING NAMELD,R4                                                        
NAMOUT   LTR   R4,R4                                                            
         BZR   RE                                                               
         MVC   0(36,R6),SPACES                                                  
         ZIC   R3,NAMLN                                                         
         SH    R3,=H'3'                                                         
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R6),NAMEREC                                                  
         SPACE 1                                                                
         USING RSTELD,R4                                                        
STATOUT  MVC   0(10,R6),SPACES                                                  
         LTR   R4,R4                                                            
         BZR   RE                                                               
         TM    RSTSTAT3,RSTSIDPT                                                
         BZ    *+10                                                             
         MVC   0(5,R6),=C'(IND)'                                                
         TM    RSTSTAT1,RSTSPIAE                                                
         BZ    *+10                                                             
         MVC   0(6,R6),=C'(EXEC)'                                               
         BR    RE                                                               
         EJECT                                                                  
         USING EMPELD,R4                                                        
HNTDATE  NTR1                                                                   
         L     R4,ADACC                                                         
         MVI   ELCODE,EMPELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   HNT9                                                             
         LA    R3,WORK01+48                                                     
         LR    R6,R3                                                            
         OC    EMPHIR,EMPHIR                                                    
         BZ    HNT1                                                             
         CLI   PROGPROF+PROGHTD-PROGD,C'T'                                      
         BE    HNT1                                                             
         MVI   0(R3),C'*'                                                       
         LA    R3,1(R3)                                                         
         MVC   0(2,R3),=C'H='                                                   
         LA    R3,2(R3)                                                         
         GOTO1 DATCON,DMCB,(1,EMPHIR),(5,(R3))                                  
         LA    R3,8(R3)                                                         
         CLI   PROGPROF+PROGHTD-PROGD,C'H'                                      
         BE    HNT8                                                             
HNT1     EQU   *                                                                
         OC    EMPTRM,EMPTRM                                                    
         BZ    HNT8                                                             
         CLI   PROGPROF+PROGHTD-PROGD,C'T'                                      
         BE    HNT4                                                             
         SPACE 1                                                                
         OC    EMPHIR,EMPHIR                                                    
         BZ    HNT3                                                             
         MVI   0(R3),X'5E'                SEMI-COLON                            
         LA    R3,1(R3)                                                         
         B     HNT4                                                             
         SPACE 1                                                                
HNT3     EQU   *                                                                
         MVI   0(R3),C'*'                                                       
         LA    R3,1(R3)                                                         
HNT4     EQU   *                                                                
         MVC   0(2,R3),=C'T='                                                   
         LA    R3,2(R3)                                                         
         GOTO1 DATCON,DMCB,(1,EMPTRM),(5,(R3))                                  
         LA    R3,8(R3)                                                         
HNT8     EQU   *                                                                
         CLI   0(R6),C'*'                                                       
         BNE   HNT9                                                             
         MVI   0(R3),C'*'                                                       
HNT9     EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
* ********************************************************************          
* LTORG                                                                         
* ********************************************************************          
         LTORG                                                                  
         EJECT                                                                  
* ********************************************************************          
* CONSTANTS                                                                     
* ********************************************************************          
         SPACE 1                                                                
RELOTAB  DS    0A                                                               
         DC    V(SQUASHER)                                                      
         DC    V(ACSLRY)                                                        
         DC    V(UNDERLIN)                                                      
         DC    X'FF'                                                            
         SPACE 1                                                                
TYPETBL  DC    AL1(MSATSLRY),C'SAL '                                            
         DC    AL1(MSATOVER),C'OT  '                                            
         DC    AL1(MSATTEMP),C'TEMP'                                            
         DC    AL1(MSATBONU),C'BON '                                            
         DC    AL1(MSATPENS),C'PEN '                                            
         DC    AL1(MSATBENE),C'BEN '                                            
         DC    AL1(MSATADMN),C'ADM '                                            
         DC    AL1(MSATBUDG),C'BUD '                                            
         DC    AL1(MSATRATE),C'RTE '                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
BASETBL  DC    C'MON'                                                           
         DC    C'QTR'                                                           
         DC    C'YTD'                                                           
         DC    C'AN '                                                           
         DC    C'HR '                                                           
         DC    X'FF'                                                            
         SPACE 1                                                                
PROFLIST DC    CL12'   OVERTIME '                                               
         DC    CL12'       TEMP '                                               
         DC    CL12'      BONUS '                                               
         DC    CL12'    BENEFIT '                                               
         DC    CL12'            '   NEW PAGE FOR SUB-DEPT FILLER                
         DC    CL12'            '   PRINT HIRE FILLER                           
         DC    CL12'      ADMIN '                                               
         DC    CL12'    PENSION '                                               
OTHERL   DC    CL12'      OTHER '                                               
         DC    X'FF'                                                            
         SPACE 1                                                                
*                                                                               
*                                                                               
TOTALC   DC    CL12'      TOTAL '                                               
MASK9    DC    (MAXLEN)C'9'        MASK FOR DETECTING 9'S                       
PKZEROS  DC    (LEVCSTQ/6)PL6'0'   PACKED ZEROS TO CLEAR COST FIELD             
         EJECT                                                                  
*                                                                               
* ********************************************************************          
* EQUATES                                                                       
* ********************************************************************          
MAXCOLQ  EQU   6                   MAXIMUM COLS ON SCREEN                       
LEVMAX   EQU   4                   MAXIMUM # LEVELS ALLOWED                     
MAXLEN   EQU   12                  MAXIMUM LENGTH OF A LEVEL                    
MIN9Q    EQU   3                   MIN # 9'S PRESENT FOR OVERHEAD ACCT          
*                                                                               
         EJECT                                                                  
* ********************************************************************          
*              DSECT FOR WORKING STORAGE                                        
AC9302D  DSECT                                                                  
*                                                                               
* TABLE OF ADDRESSES TO BE RELOCATED.                                           
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
SQUASHER DS    V                                                                
ACSLRY   DS    V                                                                
UNDERLIN DS    V                                                                
*                                                                               
COLCNT   DS    X                   NUMBER COLUMNS DISPLAYED ON REPORT           
OTHOPT   DS    X                   IF OTHER OPTION REQSTD AND IT FITS           
*                                  ON REPT THEN =C'Y' ELSE =C'N'                
IGNOREA  DS    A                   ADDRESS TO IGNORE                            
SAVEADR  DS    A                   ADDRESS OF CST LIST-'OTHER' NEEDS IT         
*                                                                               
ELCODE   DS    CL1                                                              
FRSTSUB  DS    CL1                                                              
LONGDES  DS    CL1                 LENGTH OF LONGEST DESCRIPTION                
NUMLEV   DS    CL1                 NUMBER OF LEVELS                             
TOTSW    DS    CL1                                                              
OHDACC   DS    CL1                                                              
START    DS    CL2                                                              
END      DS    CL2                                                              
TODAY    DS    CL3                                                              
*                                                                               
WRK      DS    CL16                                                             
PERIOD   DS    CL28                                                             
COMPNAM  DS    CL36                                                             
WORK01   DS    CL80                                                             
MYHEAD   DS    CL81                                                             
SVP1     DS    CL132                                                            
SVP2     DS    CL132                                                            
*                                                                               
ITEM     DS    PL2                                                              
OTHER    DS    PL(LEVSZQ)                                                       
TOTAL    DS    PL(LEVSZQ)                                                       
BONUS    DS    PL(LEVSZQ)                                                       
BENEFIT  DS    PL(LEVSZQ)                                                       
ADMIN    DS    PL(LEVSZQ)                                                       
PENSION  DS    PL(LEVSZQ)                                                       
PCTBENEF DS    PL(LEVSZQ)                                                       
PCTADMIN DS    PL(LEVSZQ)                                                       
PCTBENSV DS    PL(LEVSZQ)          SAVE A DEFAULT PCT                           
PCTADMSV DS    PL(LEVSZQ)                                                       
*                                                                               
         EJECT                                                                  
* *****************************************************************             
* LEVEL INFORMATION SAVED HERE. LEVD DSECT BELOW DEFINES THE FIELDS             
* *****************************************************************             
*                                                                               
         DS    0D                  ALLIGN ON A DOUBLE WORD BOUNDARY             
REQTOT   DS    (LEVLEN)C                                                        
         DS    0D                                                               
LLEVA    DS    (LEVLEN)C                                                        
         DS    0D                                                               
LLEVQ    EQU   *-LLEVA             OFFSET FROM LEVA TO LEVB STORAGE             
*                                                                               
LLEVB    DS    (LEVLEN)C                                                        
         DS    0D                                                               
*                                                                               
LLEVC    DS    (LEVLEN)C                                                        
         DS    0D                                                               
*                                                                               
ACCTOT   DS    (LEVLEN)C                                                        
         DS    0D                                                               
*                                                                               
WRKTOT   DS    (LEVLEN)C                                                        
         DS    0D                                                               
SALAREA  DS    (SLRLEN)C                                                        
         EJECT                                                                  
*                                                                               
* ******************************************************************            
* LEVEL STORAGE DSECT INFORMATION- LENGSTH, COST, ETC                           
* ******************************************************************            
LEVD     DSECT                                                                  
LEVSZQ   EQU   6                   SIZE OF COST BUCKETS                         
LEVCST   DS    0PL(LEVSZQ)         START OF COST BUCKETS                        
LEVSLR   DS    PL(LEVSZQ)          SALARY AMOUNT                                
LEVOVT   DS    PL(LEVSZQ)          OVERTIME AMOUNT                              
LEVTMP   DS    PL(LEVSZQ)          TEMP AMOUNT                                  
LEVBON   DS    PL(LEVSZQ)          BONUS AMOUNT                                 
LEVBEN   DS    PL(LEVSZQ)          BENEFIT AMOUNT                               
LEVFILL1 DS    PL(LEVSZQ)          FILLER FOR NEW PAGE OPTION                   
LEVFILL2 DS    PL(LEVSZQ)          FILLER FOR HIRE/TERM OPTION                  
LEVADM   DS    PL(LEVSZQ)          ADMIN AMOUNT                                 
LEVPEN   DS    PL(LEVSZQ)          PENSION AMOUNT                               
*                                                                               
LEVSALQ  EQU   (*-LEVCST)/LEVSZQ   NUMBER OF COST BUCKETS                       
*                                                                               
LEVOVH   DS    PL(LEVSZQ)          OVERHEAD                                     
LEVBONPC DS    PL(LEVSZQ)          BONUS   PERCENT                              
LEVBENPC DS    PL(LEVSZQ)          BENEFIT PERCENT                              
LEVADMPC DS    PL(LEVSZQ)          ADMIN   PERCENT                              
LEVPENPC DS    PL(LEVSZQ)          PENSION PERCENT                              
*                                                                               
LEVCSTQ  EQU   *-LEVD              LENGTH OF AMT FIELDS                         
*                                                                               
LEVRUNLN DS    CL1                 RUNNING LENGTH UP TO (INCL) THIS LEV         
LEVACTLN DS    CL1                 ACTUAL LENGTH OF THIS LEVEL                  
LEVDES   DS    CL15                DESCRIPTION                                  
LEVACC   DS    CL12                ACCOUNT CODE                                 
LEVNAM   DS    CL36                ACCOUNT NAME                                 
LEVSTAT  DS    CL10                STATUS                                       
LEVLEN   EQU   *-LEVD                                                           
         EJECT                                                                  
* ******************************************************************            
* PERIOD DSECT                                                                  
* ******************************************************************            
PERD     DSECT                     DSECT FOR PERIOD                             
PERLIT1  DS    CL14                LITERAL: 'FOR THE PERIOD'   (P+0)            
         DS    CL1                 BLANK SPACE                                  
PERSTRT  DS    CL6                 START DATE                  (P+15)           
         ORG   PERSTRT             BACK UP TO (P+15)                            
PERLIT2  DS    CL4                 LITERAL: 'THRU'             (P+15)           
         DS    CL1                 BLANK SPACE                                  
PEREND1  DS    CL6                 END DATE1                   (P+20)           
         ORG   PEREND1+1           BACK UP TO (P+21)                            
PERDASH  DS    CL1                 DASH                        (P+21)           
PEREND2  DS    CL6                 END DATE2                   (P+22)           
*                                                                               
         SPACE 1                                                                
* ******************************************************************            
* PRINT LINE DSECT FOR ACCT LEVEL AND NAME                                      
* ******************************************************************            
PRT1D    DSECT                     PRINT LINE- LEVEL NAME & ACCT                
PRTLIN   DS    0CL132              LENGTH OF PRINT LINE                         
         DS    CL1                 BLANK SPACE                                  
PRTACC   DS    CL7                 ACCOUNT LEVEL                                
         DS    CL1                 BLANK SPACE                                  
PRTNAM   DS    CL30                LEVEL NAME                                   
         SPACE 2                                                                
* ******************************************************************            
* PRINT LINE DSECT FOR SALARY ELEMENT INFORMATION                               
* ******************************************************************            
PRT3D    DSECT                     PRINT LINE FOR SALARY ELEMENTS               
         DS    CL47                                                             
PRTTYP   DS    CL4                 SALARY TYPE           (P+47)                 
         DS    CL3                 BLANK SPACE                                  
PRTBEG   DS    CL6                 BEG DATE              (P+53)                 
PRTSPC   DS    CL1                 BLANK SPACE, OR COMMA                        
PRTEND   DS    CL6                 END DATE              (P+60)                 
         DS    CL6                 BLANK SPACE                                  
PRTBASE  DS    CL3                 BASE TABLE LITERAL    (P+72)                 
         DS    CL11                BLANK                                        
PRTSLR1  DS    CL12                SALARY1               (P+86)                 
         ORG   PRTSLR1+1                                                        
PRTSLR2  DS    CL12                SALARY2               (P+87)                 
         DS    CL3                                                              
         SPACE 2                                                                
* ******************************************************************            
* PRT4D  -  PRINT LINE DSECT FOR COST REPORT: ACCT NAME, SALARIES               
*           OTHER, AND TOTAL APPEAR ON A LINE                                   
* ******************************************************************            
PRT4D    DSECT                     COST PRINT LINE                              
PRT4SPC  DS    C                   1 SPACE INDENT      (P+ 0)                   
PRT4LIT1 DS    CL10                'TOTAL FOR' LITERAL (P+ 1)                   
PRT4NAM  DS    CL25                ACCOUNT NAME        (P+11)                   
         DS    CL2                                                              
         ORG   PRT4SPC                                                          
PRT4SPC2 DS    CL8                 INDENT FOR OVERHEAD LABEL                    
PRT4TYP  DS    CL10                OVERHEAD TYPE       (P+ 8)                   
         DS    CL20                                                             
PRT4CSTS DS    CL(COLMQ*MAXCOLQ)   COSTS COLUMNS                                
PRT4CSTQ EQU   PRT4CSTS-PRT4SPC2   DISPLACEMENT TO COST 1ST COST FLD            
         EJECT                                                                  
* ******************************************************************            
* PROGD  -  USED ON TOP OF PROGPROF LIST. THE BYTES IN THE LIST                 
*           WHERE SET BY THE REQUESTOR DETERMINING WHICH OF THE                 
*           COST COLUMNS SHOULD APPEAR IN THE REPORT.                           
* ******************************************************************            
PROGD    DSECT                     PROGPROF PRINT LINE                          
PROGOVT  DS    X                   OVERTIME                                     
PROGTMP  DS    X                   TEMP                                         
PROGBNS  DS    X                   BONUS                                        
PROGBEN  DS    X                   BENEFIT                                      
PROGNPG  DS    X                   NEW PAGE FOR SUB-DEPT                        
PROGHTD  DS    X                   HIRE AND/OR TERM DATES                       
PROGADM  DS    X                   ADMIN                                        
PROGPEN  DS    X                   PENSION                                      
PROGOTH  DS    X                   OTHER                                        
*                                                                               
* ******************************************************************            
* HDRD   -  DSECT FOR THE HEAD 1-6 LINES FOR THE PAGE HEADINGS                  
* ******************************************************************            
HDRD     DSECT                                                                  
         DS    CL1                 INDENT 1 SPACE                               
HDRCMPL  DS    CL7                 LITERAL 'COMPANY'                            
         ORG   HDRCMPL                                                          
HDRCMP   DS    CL36                COMPANY NAME                                 
         ORG   HDRCMPL                                                          
HDRDES   DS    CL1                 LEVEL DESCRIPTION (EX-MOVE)                  
         ORG   HDRCMPL                                                          
HDRACC   DS    CL58                LEVEL ACCOUNT                                
         ORG   HDRCMPL                                                          
         DS    CL92                TAB FOR RIGHT-JUSTIFIED PERIOD               
HDRPER   DS    CL(L'PERIOD)        PERIOD                                       
         ORG   HDRCMPL                                                          
         DS    CL49                TAB FOR COLUMN HEADINGS LINE                 
HDRMYHD  DS    CL(L'MYHEAD)        MYHEAD-COLUMN HEADINGS                       
*                                                                               
* ******************************************************************            
* COLUMND-  BUMPS THROUGH MYHEAD PUTTING IN COLUMN HEADINGS                     
* ******************************************************************            
COLUMND  DSECT                                                                  
COLUMN   DS    CL12                COLUMN HEADING                               
COLMQ    EQU   *-COLUMND           LENGTH OF COLUMN                             
*                                                                               
* ******************************************************************            
* PROFD-    BUMP THROUGH TABLE OF HEADINGS LITERALS.                            
* ******************************************************************            
LABELD   DSECT                                                                  
LABEL    DS    CL12                COLUMN HEADING                               
LABELQ   EQU   *-LABELD            LENGTH OF COLUMN                             
*                                                                               
* ******************************************************************            
* SALARY-TYPE-TABLE DSECT                                                       
* ******************************************************************            
SALTYPD  DSECT                     DSECT FOR TYPE-TABLE-SALARY ABRV             
SALTYPE  DS    X                   TYPE OF SALARY                               
SALABRV  DS    CL4                 SALARY ABREVIATION                           
SALTYQ   EQU   *-SALTYPD           DISPLACEMENT TO NEXT NEXT ENTRY              
*                                                                               
* ******************************************************************            
         EJECT                                                                  
       ++INCLUDE DDSLRD                                                         
SALLNQ   EQU   (SLRADMST-SLRSAL)/(SLROVT-SLRSAL)+1                              
         EJECT                                                                  
*ACGENFILE                                                                      
*ACREPWORKD                                                                     
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREP9302 05/01/02'                                      
         END                                                                    
