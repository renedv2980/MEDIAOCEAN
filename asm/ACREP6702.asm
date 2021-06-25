*          DATA SET ACREP6702  AT LEVEL 024 AS OF 09/12/14                      
*PHASE AC6702A                                                                  
*INCLUDE ACCDIV                                                                 
*INCLUDE UNDERLIN                                                               
*INCLUDE ACJBEXC                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE VATICAN                                                                
         TITLE 'AC6702 - PRODUCTION JOB STATUS REPORT'                          
AC6702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC67**,R9,R8,R7,R6                                           
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC67D,RC                                                         
         CLI   MODE,RUNFRST                                                     
         BE    RUN1ST                                                           
         CLI   MODE,REQFRST                                                     
         BE    REQ1ST                                                           
         CLI   MODE,LEDGFRST                                                    
         BE    LDG1ST                                                           
         CLI   MODE,LEVAFRST                                                    
         BE    LVA1ST                                                           
         CLI   MODE,PROCACC                                                     
         BE    JOBRTN                                                           
         CLI   MODE,ANALFRST                                                    
         BE    WCRTN                                                            
         CLI   MODE,PROCTRNS                                                    
         BE    TRNRTN                                                           
         CLI   MODE,ACCLAST                                                     
         BE    LASTACC                                                          
         CLI   MODE,REQLAST                                                     
         BE    REQEND                                                           
         CLI   MODE,RUNLAST                                                     
         BE    RUNEND                                                           
EXIT     XIT1                                                                   
         EJECT ,                                                                
RUN1ST   DS    0H                  M O D E = R U N F R S T   R T N              
         MVI   MYMODE,RUNFRST                                                   
         LH    RF,=Y(AC67DLEN)                                                  
         LR    RE,RC                                                            
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0                                                            
         ZAP   BLDHRS,=P'0'        NEEDED ONE TIME TO CLEAR PZEROS              
         MVC   PZEROS,BLDHRS                                                    
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     RF,ADMASTD                                                       
         USING MASTD,RF                                                         
         L     RF,MCBXAREA                                                      
         ST    RF,ADBOX                                                         
         L     RF,=A(HOOK)                                                      
         ST    RF,HEADHOOK                                                      
         L     RF,=A(SUMDATA)                                                   
         ST    RF,ASUMTAB                                                       
         L     RF,=A(MYREPORT)                                                  
         ST    RF,AMYREPT                                                       
         L     RF,=A(OPTTAB)                                                    
         ST    RF,AOPTTAB                                                       
         L     RF,=A(TIMESUM)                                                   
         ST    RF,ATIMESUM                                                      
         L     RF,=A(TIMETAB)                                                   
         ST    RF,ATIMETAB                                                      
         L     RF,=A(MENUDATA)                                                  
         ST    RF,AMENUTAB                                                      
         L     RF,=A(BXTOP)                                                     
         ST    RF,ABXTOP                                                        
         L     RF,=A(BUFCSECT)                                                  
         ST    RF,ADBUFC                                                        
         L     RF,=A(SAVERC)                                                    
         ST    RC,0(RF)            SAVE REG C                                   
         L     RF,=A(BOXRC)                                                     
         ST    RC,0(RF)            SAVE REG C                                   
         L     R3,AMONACC                                                       
*                                                                               
         GOTO1 =A(TSARINIT)                                                     
         GOTO1 =A(GETBUFF),DMCB,(RC)                                            
         GOTO1 =A(SETBUFF),DMCB,(RC) SET BUFFALO                                
*                                                                               
         USING ACMD,R3                                                          
         L     R2,=A(FLDH)                                                      
         GOTO1 ACMAJOBL,DMCB,(R2),ACMACOLL,ADCOMFAC                             
         CLI   DMCB+4,X'00'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =A(PSTINIT),DMCB,(RC)                                            
         B     EXIT                                                             
         DROP  R3                                                               
*----------------------------------------------------------------------         
*        RELEASE GETMAINED CORE                                                 
*----------------------------------------------------------------------         
RUNEND   DS    0H                                                               
         GOTO1 =A(RELBUFF),DMCB,(RC)                                            
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        INIT REQUEST LEVEL ACCUMULATORS                                        
*        START UP SORT - VALIDATE REQUEST                                       
*----------------------------------------------------------------------         
*                                                                               
REQ1ST   DS    0H                  M O D E = R E Q F R S T   R T N              
         MVI   MYMODE,REQFRST                                                   
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   LINE,1                                                           
         MVI   SORTANY,0                                                        
         USING ACCUMSD,R2          DSECT FOR ALL ACCUMULATORS                   
         L     R2,ACUMAREA                                                      
         MVC   REQTOTS,PZEROS                                                   
         MVC   TOTCHREQ,PZEROS                                                  
         MVC   REQXJOBS,PZEROS                                                  
         DROP  R2                                                               
         MVI   BALANCE,C'N'                                                     
         L     R2,ASUMTAB                                                       
         USING SUMTABD,R2                                                       
REQ20    MVI   SMTBRQSW,C'N'       CLEAR PRINT SWITCHES                         
         LA    R2,L'SUMTAB(R2)                                                  
         CLI   0(R2),0                                                          
         BNE   REQ20                                                            
         CLC   QEND,SPACES                                                      
         BE    REQ40                                                            
         CLC   QEND+4(2),SPACES                                                 
         BNE   REQ30                                                            
         MVC   QEND+4(2),=C'31'   FOR PEEL,CLOSED OPTION                        
REQ30    GOTO1 DATCON,DMCB,(0,QEND),(1,PEND)                                    
         B     REQ50                                                            
REQ40    MVC   PEND,=X'FFFFFF'                                                  
REQ50    XC    PSTART,PSTART                                                    
         CLC   QSTART,SPACES                                                    
         BE    REQ70                                                            
         CLC   QSTART+4(2),SPACES                                               
         BNE   REQ60                                                            
         MVC   QSTART+4(2),=C'01'                                               
REQ60    GOTO1 DATCON,DMCB,(0,QSTART),(1,PSTART)                                
REQ70    GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAY2)                                
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         MVC   MYMEND,ACMMEND                                                   
         MVC   MYMSTR,ACMMSTR       SAVE IN CASE THEY WANT A FULL SUM           
         DROP  R2                                                               
*                                                                               
         MVI   FCREVOVR,FCREVTRY+FCREVTRN  DISABLE MASTERS REV'SAL FILT         
         GOTO1 ADSORTER,DMCB,A(SORTCARD),A(RECCARD),0                           
         GOTO1 =A(GET1RHIR)                                                     
         L     R0,COMMAREA                                                      
         LH    R1,=Y(CIOLEN)                                                    
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                       CLEAR COMMAREA                       
*                                                                               
         L     RF,BILLTAB                                                       
         XC    0(L'FULL,RF),0(RF)  FIRST FULL OF BILLTAB IS COUNT               
         MVC   L'FULL(L'FULL,RF),=F'300'  SECOND FULL IS MAX                    
         XC    RETSTAT,RETSTAT                                                  
         MVI   FILTERSW,C'Y'                                                    
         CLI   QOPT2,C'T'          FILTERING TRANS BY TDATE                     
         BE    REQ200              YES, SET FLAG                                
         CLI   QOPT2,C'H'          FILTERING HELD                               
         BE    REQ200              YES, SET FLAG                                
         CLI   QOPT2,C'U'          FILTERING UNHELD                             
         BE    REQ200              YES, SET FLAG                                
         CLI   QOPT5,C' '          FILTERING TRANS BY BILLED STATUS             
         BNE   REQ200              YES, SET FLAG                                
         CLC   QMOSPERD,SPACES     FILTERING TRANS BY MOS                       
         BE    REQFX               NO, DONE                                     
*                                                                               
REQ200   MVI   FILTERSW,C'Y'                                                    
*                                                                               
REQFX    B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        CALC REQUEST LEVEL, SAVE LEDGER PROFILES FROM MONACC                   
*----------------------------------------------------------------------         
*                                                                               
LDG1ST   DS    0H                  M O D E = L E D G F R S T   R T N            
         MVI   MYMODE,LEDGFRST                                                  
         GOTO1 =V(ACCDIV),DMCB,ADLDGHIR,QCOMPANY,AREA                           
         LA    RF,4                                                             
         LA    R1,AREA+1                                                        
LDG20    CLC   0(6,R1),SPACES      FIND LEVEL OF REQUEST                        
         BE    LDG30                                                            
         BCTR  RF,0                                                             
         LA    R1,13(,R1)          4=LEDGER,3=CLI ETC.                          
         B     LDG20                                                            
*                                                                               
LDG30    STC   RF,RTYPE                                                         
         L     R2,APROFILE                                                      
         USING ACPROFSD,R2                                                      
         MVC   LEDGPROF,ACPPFLDG       SAVE LEDGER LEVEL PROFILE                
         MVC   SRTBYOFF(1),ACPPFLDG+18  SAVE THE SORT BY OFFICE OPT             
         MVC   SAVCUL(1),RCCOMPFL  SAVE COMP, UNIT, LEDG                        
         MVC   SAVCUL+1(2),QUNIT                                                
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        SAVE CLIENT LEVEL PROFILES                                             
*----------------------------------------------------------------------         
*                                                                               
LVA1ST   DS    0H                                                               
         MVI   MYMODE,LEVAFRST                                                  
         XC    SVOFFICE,SVOFFICE                                                
         L     R2,ADLVASUP         24 EL ON CLIENT RECORD                       
         USING ACPROFD,R2                                                       
         CLI   ACPROFFC,C' '                                                    
         BNH   *+10                                                             
         MVC   SVOFFICE,ACPROFFC                                                
         L     R3,ADACC                                                         
         LA    R3,3(R3)            POINT R3 TO THE CLIENT                       
         GOTO1 =A(BUILDPRO),DMCB,(RC)                                           
         GOTO1 =A(REPTINIT),DMCB,(RC)    SET REPORT TYPE (NEED PROF'S)          
*                                                                               
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         MVC   ACMMEND,MYMEND         RESTORE MOS FILTERS                       
         MVC   ACMMSTR,MYMSTR                                                   
         CLI   PROF18,C'Y'         DOES THIS CLI GET A FULL WC SUMMARY          
         BE    LVA150              YES                                          
         CLI   PROF33,C'Y'         SEPARATE TIME AND OOPS                       
         BE    LVA150              YES                                          
         CLI   PROF33,C'S'         SEPARATE TIME AND OOPS                       
         BNE   LVAX                NO,                                          
LVA150   XC    ACMMSTR,ACMMSTR       YES, TURN OFF MOS FILTERING                
         MVC   ACMMEND,=X'FFFF'                                                 
LVAX     CLI   PROF33,C'S'         SEPARATING STAFF AND OUT OF POCKET           
         BNE   *+8                                                              
         MVI   PROF33,C'Y'         MAKE IT TIME AND OOP FOR TRANSACT            
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        CALLED FROM FIRST ACCOUNT FROM MONACC                                  
*        DOES GETELS ON X'26', X'30' , X'32' FILTERS FOR OPTION 2,3,6N          
*        CALLS EXCEPTION ROUTINE WITH REASON FROM QSELECT                       
*----------------------------------------------------------------------         
*                                                                               
JOBRTN   DS    0H                    FILTER A JOB FROM SORT, IF POSS            
         MVI   MYMODE,PROCACC      PROCESS AN ACCOUNT                           
         XC    SUBSTAT,SUBSTAT     CLEAR FOR JOB TOTAL                          
*                                                                               
         MVC   SAVEACLI,CLIBUFF                                                 
         MVC   SAVEAPRO,PROBUFF                                                 
         MVC   CLIBUFF(4),ADHEIRA  PREPS FOR POSSIBLE JOBBER CALL               
         MVC   PROBUFF(4),ADHEIRB                                               
*                                                                               
         GOTO1 =A(VFRSTACC),DMCB,(RC)                                           
*                                                                               
         CLI   BYTE,C'N'           DID I REJECT THIS JOB ALREADY                
         BE    JOBX                YES, LEAVE WITH FCRDTRNS SET TO N            
         GOTO1 =A(LOOKUP),DMCB,(RC)                                             
*                                                                               
         USING JXCEPTD,R5                                                       
         LA    R5,JXBLOCK                                                       
         XC    JXBLOCK(JXLEN),JXBLOCK                                           
         MVI   JXMODE,JXMPRACC     INITIALIZE JXBLOCK                           
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
         DROP  R5                                                               
*                                                                               
*                                                                               
JOBYES   MVI   FCRDTRNS,C'Y'                                                    
*                                                                               
JOBX     MVC   CLIBUFF,SAVEACLI                                                 
         MVC   PROBUFF,SAVEAPRO                                                 
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        CHECK  IF THEY WAN'T ALL OF THIS W/C CONSIDERED TIME                   
*----------------------------------------------------------------------         
*                                                                               
         USING ACKEYD,R3                                                        
*                                                                               
WCRTN    DS    0H                                                               
         L     R3,ADSUBAC                                                       
         SH    R3,DATADISP                                                      
         MVC   SVANAL,ACKEYWRK                                                  
         GOTO1 =A(WCNAME),DMCB,(RC) WORKCODE NAME/TYPE, ETC.                    
*                                                                               
         L     R2,ADGOBLOC                                                      
         USING GOBLOCKD,R2                                                      
         ZAP   SVCOMRTE,GOAGYCOM   COMMISION AT WORKCODE                        
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PUT TRANSACTION DATA TO SORT                                           
*----------------------------------------------------------------------         
*                                                                               
TRNRTN   DS    0H                  M O D E = P R O C T R N S   R T N            
         MVI   MYMODE,PROCTRNS                                                  
         GOTO1 =A(TRANSACT),DMCB,(RC)                                           
*                                                                               
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        LASTACC - IF A JOB HAS NO TRANS, PUT A SORT RECORD OUT                 
*        SO THE ESTIMATES WILL BE READ                                          
*----------------------------------------------------------------------         
*                                                                               
LASTACC  DS    0H                                                               
         MVI   MYMODE,ACCLAST                                                   
         CLI   FCRDTRNS,C'N'       DID I TRY TO READ TRANSACTIONS?              
         BE    EXIT                NO, FORGET ABOUT THIS JOB                    
*                                                                               
         GOTO1 =A(VLASTACC),DMCB,(RC)                                           
*                                                                               
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        GET RECORDS FROM SORT AND PROCESS                                      
*----------------------------------------------------------------------         
*                                                                               
REQEND   DS    0H                  M O D E = R E Q L A S T   R T N              
         MVI   MYMODE,REQLAST                                                   
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,(X'80',1)                          
         MVC   BUFACCS,PZEROS                                                   
*                                                                               
         LA    R1,SWT              INITIALIZE ACTIVITY SWITCHES                 
         LA    R0,SWTNUM                                                        
REQE000  MVI   0(R1),C'N'                                                       
         LA    R1,1(,R1)                                                        
         BCT   R0,REQE000                                                       
*                                                                               
         L     R2,COMMAREA                                                      
         MVI   0(R2),0                                                          
         XC    PREVSRT(SRTDATAL),PREVSRT                                        
         XC    BADJOB(SRTJBLN),BADJOB                                           
         USING TIMACCD,R2                                                       
         L     R2,TIMEAREA                                                      
         LA    R2,REQTIME                                                       
         LA    R3,PZEROS                                                        
         LA    R1,NTIMEACC                                                      
         BAS   RE,ZAPEM                                                         
         B     REQGET                                                           
         DROP R2                                                                
*                                                                               
         USING SORTD,R3                                                         
REQX     L     R3,SORTAREA                                                      
         CLI   SRTOFF,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SRTWC,=C'99'        BILLING?                                     
         BNE   REQGET                                                           
*                                                                               
         CLI   SRTTYPE,SRTTTIME  FAKE BILLING, THAT IS.                         
         BE    LT35                YES, GET NEXT RECORD FROM A TABLE            
         CLI   SRTTYPE,SRTTOOP     NOT SORTER                                   
         BE    LT35                                                             
*                                                                               
REQGET   GOTO1 ADSORTER,DMCB,=C'GET'                                            
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BNZ   REQ1                                                             
*                                                                               
         L     R3,SORTAREA         LAST PASS THRU, FORCE LAST PROCESSES         
         MVI   SRTOFF,X'FF'                                                     
         CLI   SORTANY,0           DID WE SORT ANYTHING?                        
         BE    LSTCA               NO, SKIP THE NEW INSTRUCTION                 
         CLI   JOBACTV,C'Y'                                                     
         BNE   LSTCA                                                            
         MVI   CONACTV,C'Y'                                                     
         B     LSTCA                                                            
*                                                                               
REQ1     L     R3,SORTAREA         FIRST CLEAR SORTAREA                         
         XC    SRTKEY(SRTDATAL),SRTKEY                                          
         XC    SRTDATA(255),SRTDATA                                             
         XC    SRTDATA+255(145),SRTDATA+255                                     
         LH    R3,0(R2)           FIRST HALF OF SORT REC IS LENGTH              
         L     R4,SORTAREA                                                      
         LR    R5,R3              LENGTH FOR R4                                 
         MVCL  R4,R2                                                            
*                                                                               
         CLC   QUESTOR(4),=C'HELP'                                              
         BNE   REQ2                                                             
*                                                                               
         USING SORTD,R3                                                         
         L     R3,SORTAREA                                                      
         LA    R1,=C'PUT'                                                       
         LA    R2,SRTKEYLN                                                      
         GOTO1 =V(PRNTBL),DMCB,(3,(R1)),(R3),C'DUMP',(R2),=C'2D',(C'P',X        
               PRINT)                                                           
         LA    R3,SRTDATA                                                       
         LA    R2,200                                                           
         GOTO1 =V(PRNTBL),DMCB,(3,(R1)),(R3),C'DUMP',(R2),=C'2D',(C'P',X        
               PRINT)                                                           
*                                                                               
         USING SORTD,R3                                                         
REQ2     L     R3,SORTAREA                                                      
         OC    SRTWC,SRTWC         REJECT THIS JOB?                             
         BNZ   REQ3                NO                                           
         MVC   BADJOB(SRTJBLN),SRTKEY                                           
         B     REQGET                                                           
REQ3     CLC   SRTKEY(SRTJBLN),BADJOB                                           
         BE    REQGET                                                           
*                                                                               
         CLI   SRTWC,X'01'         IS THIS A LIST OF EXCEPTION REASONS          
         BNE   REQE40              NO, PROCESS TRAN                             
         USING JXCEPTD,R5                                                       
         LA    R5,JXBLOCK                                                       
         MVI   SVNCODES,0                                                       
         XC    SVCODES,SVCODES                                                  
         XR    R1,R1                                                            
         ICM   R1,1,SRTWC+2                                                     
         BZ    REQGET                                                           
         SLA   R1,1                                                             
         BCTR  R1,0                                                             
         USING JXCEPTD,R5                                                       
         LA    R5,JXBLOCK                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVCODES(0),SRTWC+3                                               
         MVC   SVNCODES,SRTWC+2                                                 
         CLC   QUESTOR(4),=C'HELP'                                              
         BNE   REQGET                                                           
         MVC   P+1(100),SRTKEY                                                  
         GOTO1 ACREPORT                                                         
         B     REQGET                                                           
*                                                                               
REQE40   OC    PREVSRT(SRTDATAL),PREVSRT                                        
         BZ    FRSTOFF                 START IT UP                              
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        LAST FOR CONTRA ACCOUNT, PUT VENDOR TOTALS                   *         
*                                                                     *         
*                                                                     *         
*---------------------------------------------------------------------*         
*                                                                               
LSTCA    L     R3,SORTAREA                                                      
         CLC   SRTKEY(SRTCALN),PREVSRT SAME CONTRA ACOUNT                       
         BE    PROCTRAN              JUST PROCESS THIS TRANSACTION              
*                                                                               
         CLI   CONACTV,C'Y'                                                     
         BNE   LSTWC                                                            
         MVI   ANLACTV,C'Y'                                                     
         CLI   PROF21,C'Y'         DOUBLE SPACE AFTER VENDORS                   
         BNE   LSTWC                                                            
         BAS   RE,PRINTEM          PRINT A BLANK                                
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*  CHECK FOR LAST WORK CODE, PRINT W/C TOTALS,                        *         
*        BUILD JOBTOTS ACCUM, AND TOTCHJOB ACCUM                      *         
*---------------------------------------------------------------------*         
*                                                                               
LSTWC    L     R3,SORTAREA                                                      
         CLC   SRTKEY(SRTWCLN),PREVSRT  SAME WORK CODE                          
         BE    FRSTCA                YES, JUST NEED TO PRINT NEW CONTRA         
*                                                                               
         MVI   MYMODE,ANALLAST                                                  
         CLI   ANLACTV,C'Y'        ANYTHING TO PRINT                            
         BNE   LSTTYPE                                                          
         MVI   JOBACTV,C'Y'                                                     
*                                                                               
         CLC   SVANAL,=C'99'       DON'T CALCULATE FOR BILLING                  
         BE    LWC50                                                            
*                                                                               
         ZAP   BUFACCS9,=P'0'                                                   
         TM    XJOBSTAT,XJOB       IS THIS AN XJOB                              
         BNZ   LWC50               YES, DON'T CALCULATE BILLABLE                
*                                                                               
         ZAP   BUFACCS9,BUFACCS6   UNBILLED CHARGES = GROSS                     
*                                                                               
         CLI   PROF13,C'Y'         SHOW NET AS LESS CD ?                        
         BE    *+10                YES, CD IS GONE                              
         SP    BUFACCS9,BUFACCS7   LESS CD                                      
*                                                                               
         SP    BUFACCS9,BUFACCS8   LESS BILLED                                  
*                                  ADD WC TOTALS TO EITHER JOBTOTS OR           
LWC50    LA    RE,BUFACCS          TYPETOTS                                     
         USING ACCUMSD,R2                                                       
         L     R2,ACUMAREA                                                      
         LA    RF,JOBTOTS                                                       
         OC    PROCSTAT,PROCSTAT   ARE WE SEPARATING TIME AND OOPS              
         BZ    LWC55               NO, JUST ACCUM JOB TOTS                      
         LA    RF,TYPTOTS                                                       
         MVI   TYPACTV,C'Y'                                                     
LWC55    LA    R1,10                                                            
LWC60    CLC   SVANAL,=C'99'                                                    
         BNE   LWC70                                                            
         SP    0(8,RF),0(8,RE)     SUBTRACT BILLING AMOUNT                      
         B     LWC80                                                            
LWC70    AP    0(8,RF),0(8,RE)     BUILD JOB TOTAL LINE                         
LWC80    LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R1,LWC60                                                         
*                                                                               
*        UPDATE TOTAL CHARGES ACCUMULATORS                                      
         CLC   SVANAL,=C'99'       BILLING?                                     
         BE    LWC88               YES                                          
*                                                                               
         L     R2,ACUMAREA         UPDATE TOTAL CHARGES                         
         LA    R2,TOTCHJOB                                                      
         OC    PROCSTAT,PROCSTAT                                                
         BZ    *+12                                                             
         L     R2,ACUMAREA                                                      
         LA    R2,TOTCHTYP                                                      
*                                                                               
         LA    R3,BUFACCS                                                       
         LA    R1,ACCUMNUM         NUMBER OF ACCUMULATORS                       
         BAS   RE,ADDEM            ADD R3 TO R2                                 
         B     LWC88                                                            
*                                                                               
LWC88    CLC   SVANAL,=C'99'       BILLING                                      
         BNE   LWC89               NO                                           
         L     R2,ACUMAREA                                                      
         LA    R5,TOTCHJOB         FIX THE BILLING TOTALS IF NEEDED             
         LA    R2,JOBSTAT                                                       
         BAS   RE,FIXBAL                                                        
*                                                                               
LWC89    CP    WCSW,=P'1'          TOTALS NECESSARY?                            
         BH    LWC90               YES                                          
         CLI   PROF28,C'Y'         BOX OFF WC TOTS?                             
         BE    *+8                 YES, JUST UNDERLINE                          
         BAS   RE,PRINTEM          NO - SKIP A LINE AND BUMP TOTALS             
*                                                                               
         LA    R2,PREVSRT                                                       
         CLI   SRTTYPE-SRTKEY(R2),SRTTTIME  TIME WC?                            
         BNE   LWC92                                                            
         LA    R1,4                BUMP TOTS IN LIU OF SUBLINE                  
         L     R3,TIMESUB          A(WC ACCUMULATOR)                            
         LA    R2,TIMSUBLN(R3)     A(TYPE ACCUMULATO)R                          
         BAS   RE,ADDEM                                                         
         B     LWC92                                                            
*                                                                               
LWC90    LA    R3,BUFACCS          PRINT WORK CODE TOTALS                       
         MVC   P+4(11),=C'*TOTALS FOR'                                          
         MVC   P+17(2),SVANAL                                                   
*                                                                               
         MVI   SUBLGST,C'Y'        ATTEMPT TO PRINT GST                         
         BAS   RE,SUBLINE                                                       
         MVI   SUBLGST,C'N'        DON'T ATTEMPT TO PRINT GST                   
*                                                                               
LWC92    CLI   PROF28,C'Y'         BOX OFF WC TOTS?                             
         BNE   LWC95               NO                                           
*                                                                               
         ZIC   RF,LINE             I'LL NEED 4 LINES TO PRINT NXT WC            
         ZIC   RE,MAXLINES         PLUS 1 FOR THE MID ITSELF                    
         SH    RE,=H'5'                                                         
         CR    RF,RE               WILL THERE BE ROOM FOR THE NEXT W/C          
         BNH   LWC94               YES                                          
         MVI   FORCEHED,C'Y'                                                    
         B     LWC95                                                            
*                                                                               
LWC94    DS    0H                                                               
         GOTO1 =A(BOXMID)                                                       
         CLI   FORCEHED,C'Y'       BOXMID RETURNED A TOF                        
         BE    LWC95                                                            
         BAS   RE,PRINTEM                                                       
*                                                                               
LWC95    MVI   BUFTYPE,C'1'        PUT WC TOTAL TO BUFFALO FOR SUMMARY          
         BAS   RE,PUTWCSUM                                                      
*                                                                               
*                                                                               
         TM    JOBSTAT,NEEDESTS    DO WE NEED ESTIMATE VALUES NOW?              
         BNO   LWC99               NO                                           
         CLI   PROF25,C'Y'         PRINT ESTIMATES W/O ACTUALS?                 
         BNE   LWC99               NO                                           
*                                  IF THIS IS LAST WC FOR JOB, CALLPEST         
         L     R3,SORTAREA                                                      
         CLI   SRTTYPE,X'00'       ARE WE SEPARTATING TIME AND OOPS             
         BNE   LWC99               YES, LSTTYPE WILL CALLPEST                   
*                                                                               
         CLC   SRTKEY(SRTJBLN),PREVSRT NEW JOB COMING                           
         BNE   LWC97                   NO                                       
*                                                                               
         CLC   SRTWC-SRTKEY(2,R2),=C'99' WAS THIS BILLING?                      
         BE    LWC99                     YES, I CAALLED CALLPEST                
*                                                                               
LWC97    CLC   SRTWC,=C'99'        OLD JOB, IS BILLING COMING UP...             
         BNE   LWC99               NO                                           
*                                                                               
         LA    R2,PREVSRT                                                       
         MVC   CPESTSTR,SRTWC-SRTKEY(R2)                                        
         ZIC   RF,CPESTSTR+1                                                    
         LA    RF,1(RF)                                                         
         STC   RF,CPESTSTR+1                                                    
         MVI   CPESTSTP,C' '                                                    
         MVI   CPESTETP,C' '                                                    
         MVI   CPESTEND,X'FF'                                                   
*                                                                               
*        MVC   P+1(3),=C'LWC'                                                   
         BAS   RE,CALLPEST                                                      
*                                                                               
*        PRINT TOTAL CHARGES IF ...                                             
*              1) NEXT TRANSACTION IS BILLING FOR THE CURRENT JOB               
*              2) NEXT TRANSACTION IS A NEW JOB AND THERE WERE NONE             
*                                                                               
         USING ACCUMSD,R2                                                       
LWC99    MVC   BUFACCS,PZEROS                                                   
         LA    R2,PREVSRT                                                       
         L     R3,SORTAREA                                                      
         CLI   SRTTYPE-SRTKEY(R2),X'00'   TIME VS OOP                           
         BNE   LSTTYPE                  LSTTYPE WILL WORRY ABOUT BILLS          
*                                                                               
         CLC   SRTKEY(SRTJBLN),PREVSRT  SAME JOB                                
         BNE   LWC100                   NO                                      
*                                                                               
         CLC   SRTWC,=C'99'       IS BILLING COMEING                            
         BE    LWC130              YES, PRINT TOTAL CHARGES                     
         B     LSTTYPE             NO, CONTINUE                                 
*                                                                               
LWC100   TM    JOBSTAT,CHARGES     ARE THERE CHARGES ON THIS JOB                
         BO    LSTTYPE             YES                                          
*                                                                               
         LA    R2,PREVSRT                                                       
         CLC   SRTWC-SRTKEY(2,R2),=C'99' WAS THIS BILLING?                      
         BNE   LSTTYPE                   NO,                                    
*                                                                               
         USING ACCUMSD,R2                                                       
LWC130   L     R2,ACUMAREA                                                      
         USING LITTABD,R5                                                       
         L     R5,=A(LITTAB)                                                    
         MVC   P+2(17),TOTCHAWC                                                 
         LA    R3,TOTCHJOB                                                      
         MVI   MYMODE,ACCLAST                                                   
         BAS   RE,SUBLINE                                                       
*                                                                               
         MVI   CHRGPEND,C'N'                                                    
*                                                                               
         DROP  R2,R3,R5                                                         
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*     CHECK FOR LAST TRANSACTION TYPE, PRODUCE TOTALS FOR MANPOWER    *         
*        DETAILS OR OOPS DETAILS                                      *         
*        IF TIME HAS BEEN SEGREGATED FROM OOPS, PRINT BILLING FOR THE *         
*        TRANSACTION TYPE NOW                                         *         
*---------------------------------------------------------------------*         
*                                                                               
         USING SORTD,R3                                                         
*                                                                               
LSTTYPE  L     R3,SORTAREA                                                      
         CLC   SRTKEY(SRTTYLN),PREVSRT  SAME TRNSACTION TYPE                    
         BE    FRSTWC                JUST DO NEW WCODE STUFF                    
         CLI   TYPACTV,C'Y'                                                     
         BNE   LSTJOB                                                           
         LA    R2,PREVSRT                                                       
         CLI   SRTTYPE-SRTKEY(R2),0  SEPARATING TRANSTRACTION TYPE              
         BE    LSTJOB              NO, CHECK LSTJOB                             
*                                                                               
         CLI   SRTTYPE-SRTKEY(R2),SRTTBILL WAS PREVIOUS A SAVED BILL            
         BE    FRSTTYPE            YES, I'LL PRINT IT LATER                     
*                                                                               
         MVI   MYMODE,0            NOT LAST WC. NOT LAST JOB.                   
*                                                                               
         TM    JOBSTAT,NEEDESTS    DO WE NEED ESTIMATE VALUES NOW?              
         BNO   LT20                NO                                           
         CLI   PROF25,C'Y'         PRINT ESTIMATES W/O ACTUALS?                 
         BNE   LT20                NO                                           
*                                                                               
*                                                                               
         LA    R2,PREVSRT                                                       
         MVC   CPESTSTR,SRTWC-SRTKEY(R2)                                        
*                                                                               
         ZIC   RF,CPESTSTR+1                                                    
         LA    RF,1(RF)                                                         
         STC   RF,CPESTSTR+1                                                    
         MVI   CPESTSTP,O_O_P_WC   SET TO GET OOP WC'S                          
         CLI   SRTTYPE-SRTKEY(R2),SRTTTIME TIME?                                
         BNE   *+8                                                              
         MVI   CPESTSTP,TIMEWC     SET TO GET TIME WC'S                         
*                                                                               
         MVI   CPESTEND,X'FF'      TILL THE END                                 
         MVC   CPESTETP,CPESTSTP                                                
         CLC   SRTKEY(SRTJBLN),PREVSRT SAME JOB COMMING UP                      
         BE    LT10                                                             
         MVI   CPESTETP,X'FF'      TILL THE END                                 
*                                                                               
LT10     BAS   RE,CALLPEST                                                      
*                                                                               
         USING ACCUMSD,R2                                                       
LT20     L     R2,ACUMAREA         PRINT TOTAL CHARGES                          
         LA    R3,TOTCHTYP                                                      
*                                                                               
         USING LITTABD,R5                                                       
         L     R5,=A(LITTAB)                                                    
         MVC   P+1(16),TIMCHA                                                   
         TM    PROCSTAT,TIMETRAN                                                
         BO    *+10                                                             
         MVC   P+1(16),OOPCHA                                                   
*                                                                               
         MVC   GRSSW,=C'YY'        IN CASE NOT TURNED ON                        
         BAS   RE,SUBLINE                                                       
*                                                                               
         L     R2,ACUMAREA         ADD TYPE TOTALS TO JOB TOTALS                
         LA    R3,TOTCHTYP                                                      
         LA    R2,TOTCHJOB                                                      
         LA    R1,ACCUMNUM         NUMBER OF ACCUMULATORS                       
         BAS   RE,ADDEM                                                         
         L     R2,ACUMAREA                                                      
         LA    R3,TYPTOTS                                                       
         LA    R2,JOBTOTS                                                       
         BAS   RE,ADDEM                                                         
*                                                                               
         USING BILLTABD,R1                                                      
         L     R1,BILLTAB          BUILD TIME BILLING RECORD                    
         L     R4,0(R1)            NUMBER IN TABLE                              
         LTR   R4,R4                                                            
         BNZ   LT30A               PROCESS BILLING                              
*                                                                               
         L     R3,SORTAREA                                                      
         CLC   SRTKEY(SRTJBLN),PREVSRT  SAME JOB                                
         BNE   LSTJOB              NO, DO JOB TOTS                              
*                                                                               
         BAS   RE,BXBOT            SAME JOB, CLOSE BOXES                        
*                                                                               
         B     FRSTTYPE            RESET HEADERS                                
*                                                                               
LT30A    CLI   PROF28,C'Y'         BOX OFF WC TOTS?                             
         BNE   LT30B               NO                                           
         GOTO1 =A(BOXMID)                                                       
         CLI   FORCEHED,C'Y'       BOXMID RETURNED A TOF                        
         BE    LT30B                                                            
         BAS   RE,PRINTEM                                                       
*                                                                               
LT30B    L     R2,SORTAREA         PROCESS SAVED  BILLING                       
         LH    R3,0(R2)           SAVE CURRENT SORT DATA                        
         L     R4,SORTSAVE                                                      
         LR    R5,R3              LENGTH FOR R4                                 
         MVCL  R4,R2                                                            
         MVC   PREVSAVE,PREVSRT    SAVE PREVIOUS SORT KEY ALSO                  
*                                                                               
LSTTA    L     R1,BILLTAB          BUILD TIME BILLING RECORD                    
         L     R4,0(R1)            NUMBER IN TABLE                              
         LA    R1,8(R1)            TABLE DATA                                   
         LA    R4,1(R4)            FOR THE BCT ON TOP OF THE LOOP               
         MVC   BUFACCS,PZEROS      PREP WC ACCUMULATOR                          
         MVC   SVANAL,=C'99'                                                    
         MVI   WCP_TIME,C'N'                                                    
         MVI   WTYPE,O_O_P_WC                                                   
         DROP  R5                                                               
*                                                                               
         USING LITTABD,R5                                                       
         L     R5,=A(LITTAB)                                                    
         TM    PROCSTAT,FINLBILL                                                
         BNO   LSTTB                                                            
         MVC   P+1(15),BILDET               PRINT WC 99 HEADER                  
         B     LSTTC                                                            
LSTTB    MVC   P+1(16),TIMBIL                                                   
         TM    PROCSTAT,TIMETRAN                                                
         BO    *+10                                                             
         MVC   P+1(16),OOPBIL                                                   
LSTTC    MVI   PSECOND+1,X'BF'     UNDERSCORE                                   
         MVC   PSECOND+2(14),PSECOND+1                                          
         BAS   RE,PRINTEM                                                       
         MVI   ANALPEND,1                                                       
         B     LT40                                                             
*                                                                               
LT35     L     R4,SAVER4           THIS IS THE ENTRY POINT WHEN CALLED          
         L     R1,SAVER1           FROM REQX (GET A NEW FAKE BILLING)           
*                                                                               
LT40     BCT   R4,*+8                                                           
         B     LT60                                                             
*                                                                               
         NI    BILLSTAT,X'FF'-BADBILL     ASSUME BILL IS GOOD                   
         L     R3,SORTAREA                                                      
         MVC   SRTKEY(SRTJBLN),PREVSRT FOR THE PREVIOUS JOB...                  
         TM    PROCSTAT,FINLBILL                                                
         BO    LT50                                                             
         TM    PROCSTAT,TIMETRAN          AM I DOING TIME?                      
         BNO   LT45                NO                                           
         CP    BILTNET,=P'0'       IS THERE TIME ON THIS BILL                   
         BNE   LT50                YES                                          
         CP    BILTCOM,=P'0'       IS THERE TIME COMMISSION                     
         BNE   LT50                YES                                          
         OI    BILLSTAT,BADBILL    NO TIME, REJECT BILL HERE                    
         B     LT54                AND GET NEXT FROM TABLE                      
*                                                                               
LT45     CP    BILLNET,BILTNET     IS THIS BILL ALL TIME                        
         BNE   LT50                NO, PROCESS AS OOPS                          
         CP    BILLNET,=P'0'       IS NET 0                                     
         BE    LT50                YES, PROCESS ZERO BILLS AS OOPS              
*                                                                               
         OI    BILLSTAT,BADBILL    NO TIME, REJECT BILL HERE                    
         B     LT54                AND GET NEXT FROM TABLE                      
*                                                                               
LT50     MVC   SRTCA,BILLCA             ... RECREATE ANY BILLING IT             
         MVC   SRTWC,=C'99'                MIGHT HAVE HAD                       
         MVC   SRTNUM,BILLNO                                                    
         MVC   SRTDATE,BILLDATE                                                 
         MVI   SRTTYPE,SRTTTIME    FAKE TIME BILLING KEY                        
         LA    R3,SRTDATA          BUMP R3                                      
         USING SR44D,R3            FAKE TIME BILLING DATA                       
         MVI   SR44ID,X'44'                                                     
         MVC   SR44LEN,=AL1(SR44LN1+36)  SAVE A 36 CHARACTER NARR               
         MVC   SR44DATE,BILLDATE                                                
         MVC   SR44USED,BILLUS                                                  
         MVC   SR44MOS,BILLMOS                                                  
         MVC   SR44BTCH,BILLBTCH                                                
         MVC   SR44NARR(15),BILLTYPE                                            
*                                                                               
         TM    PROCSTAT,TIMETRAN   IS THIS SUPOSED TO BE FAKE TIME              
         BNO   LT53                NO                                           
*                                                                               
         USING SR44D,R3            FAKE TIME BILLING DATA                       
         MVC   SR44AMNT,BILTNET                                                 
         ZAP   SR44NARR+15(6),BILTCOM    TIME COMMISION                         
         ZAP   SR44NARR+21(6),=P'0'      CASH D ON TIME !!                      
         ZAP   SR44NARR+27(6),BILTNET    PAYABLE                                
         AP    SR44NARR+27(6),BILTCOM    PAYABLE                                
         B     LT54                                                             
*                                                                               
LT53     L     R3,SORTAREA                                                      
         USING SORTD,R3                                                         
         MVI   SRTTYPE,SRTTOOP     FAKE OOPS BILLING KEY                        
*                                                                               
         LA    R3,SRTDATA          FAKE OOPS BILLING DATA                       
         USING SR44D,R3                                                         
         ZAP   SR44AMNT,BILLNET          NET AMOUNT                             
         ZAP   SR44NARR+15(6),BILLCOM    TOTAL COMISSION ON BILL                
         ZAP   SR44NARR+21(6),BILLCD                                            
         ZAP   SR44NARR+27(6),BILLNET    PAYABLE (NET+COMM)                     
         AP    SR44NARR+27(6),BILLCOM                                           
*                                                                               
         TM    PROCSTAT,FINLBILL         IS THIS FINAL BILL PRINTING?           
         BO    LT54                      YES                                    
*                                                                               
         SP    SR44NARR+15(6),BILTCOM    LESS TIME COM IS OOPS COM              
         SP    SR44AMNT,BILTNET          LESS NET TIME                          
         SP    SR44NARR+27(6),BILTNET    OOPS PAYABLE                           
         SP    SR44NARR+27(6),BILTCOM                                           
*                                                                               
LT54     ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         TM    PROCSTAT,FINLBILL   IS THIS FINAL BILL PRINTING?                 
         BNO   LT55                NO                                           
*                                                                               
         OC    BILLGST,BILLGST     ANY GST                                      
         BZ    LT55                NO                                           
         BAS   RE,GETGST           YES, RESTORE GST (IF ANY)                    
*                                                                               
LT55     LA    R1,LBILLTAB(R1)                                                  
         ST    R1,SAVER1                                                        
         ST    R4,SAVER4                                                        
         TM    BILLSTAT,BADBILL    IS THIS BILL A DIFFERNT TYPE                 
         BO    LT40                YES, TRY NEXT IN TABLE                       
*                                                                               
         MVI   0(R3),0             END OF RECORD                                
*                                                                               
         USING SORTD,R3                                                         
         L     R3,SORTAREA                                                      
         MVC   SVANAL,SRTWC                                                     
         CLC   SRTKEY(SRTCALN),PREVSRT  SAME CONTRA                             
         BE    PROCTRAN                                                         
         B     FRSTCA                   PRINT THE NEW CONTRA AC                 
         EJECT ,                                                                
* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---           
*        END OF FAKE BILLING PROCESSING CALCULATE JOB TOTALS                    
*        PRINT BILLING TOTALS (A LSTWC FOR FAKE 99'S)                           
* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---           
*                                                                               
LT60     DS    0H                                                               
         L     R2,ACUMAREA         ACCUMULATE BILLING TOTAL                     
         TM    PROCSTAT,FINLBILL    DID I JUST PRINT BILLING TOTALS             
         BO    LT70                                                             
*                                                                               
         LA    R5,TOTCHTYP         FIX THE BILLING TOTALS IF NEEDED             
         LA    R2,TYPESTAT                                                      
         BAS   RE,FIXBAL                                                        
         L     R2,ACUMAREA         ACCUMULATE BILLING TOTAL                     
         LA    R2,BILLTOTS                                                      
         LA    R3,BUFACCS                                                       
         LA    R1,ACCUMNUM         NUMBER OF ACCUMULATORS                       
         BAS   RE,ADDEM            ADD R3 TO R2                                 
*                                                                               
         MVC   P+4(11),=C'*TOTALS FOR'                                          
         MVC   P+17(2),=C'99'                                                   
         BAS   RE,SUBLINE                                                       
         CLI   PROF28,C'Y'         BOX OFF WC TOTS?                             
         BNE   LT60A               NO                                           
*                                                                               
         GOTO1 =A(BOXMID)                                                       
         CLI   FORCEHED,C'Y'       BOXMID RETURNED A TOF                        
         BE    LT60A                                                            
         BAS   RE,PRINTEM                                                       
*                                                                               
LT60A    L     R2,ACUMAREA         CALCULATE BALANCE                            
         LA    R2,TYPTOTS                                                       
         LA    R3,BUFACCS                                                       
         LA    R1,ACCUMNUM         NUMBER OF ACCUMULATORS                       
         BAS   RE,SUBEM            SUBTRACT R3 FROM R2 FOR TYPE BALANCE         
*                                                                               
         MVC   P+7(11),=C'**BALANCE**'                                          
         LR    R3,R2                                                            
         BAS   RE,SUBLINE                                                       
*                                                                               
* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---           
*        RESTORE SAVED SORT RECORD                                              
* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---           
LT60B    L     R4,SORTSAVE         IS THE JOB I JUST PROCESSED THE SAME         
         L     R2,SORTAREA         AS THE ONE SAVED                             
         CLC   4(SRTJBLN,R2),4(R4) SAME ?                                       
         BNE   LSTT04              NO, DO BILLING TOTALS                        
*                                                                               
         BAS   RE,BXBOT            CLOSE THESE BOXES                            
         BAS   RE,RESTSORT         RESTORE SORT DATA                            
         B     FRSTTYPE            AND RESET HEADERS                            
*                                                                               
* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---           
*        BEFORE I PROCESS NEXT JOB MAKE SURE THERE IS NO BILLING LEFT           
* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---           
LSTT04   TM    PROCSTAT,TIMETRAN   WAS THERE ONLY TIME ON THIS TRAN             
         BNO   LSTT04A             NO                                           
         MVI   PROCSTAT,OOPSTRAN                                                
         B     LSTTA                                                            
*                                                                               
LSTT04A  L     R2,ACUMAREA                                                      
         USING LITTABD,R5                                                       
         L     R5,=A(LITTAB)                                                    
         MVC   P+2(17),TOTCHA                                                   
         MVC   SVANAL,SPACES       CLEAR WORK CODE                              
         LA    R3,TOTCHJOB                                                      
         BAS   RE,SUBLINE                                                       
         MVI   CHRGPEND,C'N'                                                    
*                                                                               
         TM    BILLSTAT,GOTTBILL+GOTOBILL                                       
         BNO   LT80                                                             
         OI    PROCSTAT,FINLBILL                                                
         B     LSTTA                                                            
*                                                                               
*        DO FINAL BILL TOTALS                                                   
*                                                                               
LT70     L     R5,=A(LITTAB)                                                    
         MVC   P+2(17),TOTBIL                                                   
         LA    R3,BILLTOTS                                                      
*                                                                               
         MVI   SUBLGST,C'Y'        ATTEMPT TO PRINT TAX FOR SUBLINE             
         BAS   RE,SUBLINE                                                       
*                                                                               
         MVI   SUBLGST,C'N'                                                     
*                                                                               
         DROP  R5                                                               
*                                                                               
LT80     MVC   BUFACCS,BILLTOTS    PUT WC 99 TOTS TO WC SUMMARY                 
         USING LITTABD,RF                                                       
         L     RF,=A(LITTAB)                                                    
         MVC   WNAME,LITBILL                                                    
         DROP  RF                                                               
         MVI   BUFTYPE,C'1'        PUT WC TOTAL TO BUFFALO FOR SUMMARY          
         MVC   SVANAL,=C'99'                                                    
         BAS   RE,PUTWCSUM                                                      
*                                                                               
         L     R2,ACUMAREA         CALCULATE THE OVERALL JOB BAL                
         LA    R3,BILLTOTS                                                      
         LA    R2,JOBTOTS                                                       
         LA    R1,ACCUMNUM         NUMBER OF ACCUMULATORS                       
         BAS   RE,SUBEM                                                         
         BAS   RE,RESTSORT                                                      
         B     LSTJOB                                                           
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
*                                                                               
*        LAST TYPE UTILITIES                                                    
*                                                                               
RESTSORT L     R2,SORTAREA        RESTORE PREVIOUS SORT RECORD                  
         L     R4,SORTSAVE        SAVE AREA                                     
         LH    R3,0(,R4)                                                        
         LR    R5,R3              LENGTH FOR R4                                 
         MVCL  R2,R4                                                            
         MVC   PREVSRT,PREVSAVE                                                 
         BR    RE                                                               
*                                                                               
*       RESTORE GST ELEMENTS TO A SORT RECORD                                   
*       R3=A(WHERE IN SORTDATA TO PUT THE RECORED                               
*       R1=A(SAVED BILL RECORD)                                                 
*                                                                               
         USING BILLTABD,R1                                                      
GETGST   ST    RE,SAVERE                                                        
         L     RE,COMMAREA                                                      
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         ICM   R0,3,BILLGST        OFFSET ONTO COMMAREA                         
         AR    RE,R0                                                            
*                                                                               
GETGST30 CLI   0(RE),0             EOR?                                         
         BE    GETGSTX             YES                                          
*                                                                               
         CLI   0(RE),VBIELQ        GST ELEMENT                                  
         BE    GETGST50                                                         
         CLI   0(RE),PBIELQ        PST ELEMENT                                  
         BE    GETGST50                                                         
         DC    H'0'                SEE WHAT IS IS                               
*                                                                               
GETGST50 IC    RF,1(,RE)                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(RE)                                                    
*                                                                               
         AR    RE,RF                                                            
         AR    R3,RF                                                            
         B     GETGST30                                                         
*                                                                               
GETGSTX  L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         DROP  R1                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*     CHECK FOR LAST JOB:                                             *         
*        LOOP THROURG BUFFALO TAB                                               
*        PRINT WC SUMMARY                                                       
*        PRINT PROFILES, COMMENTS, ESTIMATES, ETC                               
*        PRINT JOB TOTALS, UPDATE HIGHER LEVEL TOTALS                           
*---------------------------------------------------------------------*         
*                                                                               
         USING SORTD,R3                                                         
*                                                                               
LSTJOB   L     R3,SORTAREA                                                      
         CLC   SRTKEY(SRTJBLN),PREVSRT  SAME JOB                                
         BE    FRSTTYPE              JUST RESET HEADERS                         
*                                                                               
         MVI   MYMODE,ACCLAST                                                   
         MVC   SVANAL,SPACES       CLEAR WORK CODE                              
         CLI   PROFILES+14,C'Y'    PROFILE TO PRINT ALL JOBS                    
         BE    *+12                ACTIVE OR NOT                                
         CLI   JOBACTV,C'Y'                                                     
         BNE   LSTPROD                                                          
*                                                                               
         MVI   PROACTV,C'Y'                                                     
*                                                                               
         USING ACCUMSD,R2                                                       
         L     R2,ACUMAREA         PRINT JOB TOTALS                             
         LA    R3,JOBTOTS                                                       
         USING LITTABD,R5                                                       
         L     R5,=A(LITTAB)                                                    
         MVC   P+2(17),JOBBAL                                                   
         CLI   CHRGPEND,C'Y'       TOTAL CHARGES STILL PENDING                  
         BNE   LSTJ24                                                           
         LA    R3,TOTCHJOB                                                      
         MVC   P+2(17),TOTCHA                                                   
LSTJ24   BAS   RE,SUBLINE                                                       
         DROP  R2                                                               
         BAS   RE,BXBOT            PRINT A BOX BOTTOM                           
         DROP  R5                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        PREP FOR THE WORKCODE SUMMARY                                          
*---------------------------------------------------------------------*         
*                                                                               
LSTJ26   MVI   PRTFLAG,PRTNGWC                                                  
         XC    BUFREC,BUFREC       PREP FOR WORK CODE SUMMARY                   
         MVI   BUFTYPE,C'1'        GET LEVEL ONE RECORDS                        
         MVI   RCSUBPRG,1                                                       
         MVI   MYBOXSW,2           SUMMARY BOXES                                
         MVI   CHRGPEND,C'Y'                                                    
         MVC   ATABLE,ASUMTAB                                                   
         L     R2,ASUMTAB                                                       
         USING SUMTABD,R2                                                       
LSTJ27   MVI   SMTBJBSW,C'N'       CLEAR PRINT SWITCHES                         
         LA    R2,L'SUMTAB(R2)                                                  
         CLI   0(R2),0                                                          
         BNE   LSTJ27                                                           
*                                                                               
         CLI   PROF12,C'Y'         SUMMARIES ON NEW PAGE                        
         BE    LSTJ28              YES                                          
*                                                                               
         CLI   LINE,46             ROOM FOR HEADERS, AT LEAST                   
         BL    LSTJ30              YES, SET MIDS                                
*                                                                               
LSTJ28   MVI   FORCEHED,C'Y'       IF NOT SKIP TO NEW PAGE                      
         B     LSTJ32                                                           
*                                                                               
LSTJ30   MVI   MIDSW,1                                                          
*                                                                               
LSTJ32   GOTO1 ABXTOP              BOX TOP AND COLUMNS FOR SUMMARY              
*                                                                               
LSTJ34   CLI   FORCEHED,C'Y'       NEW PAGE?                                    
         BE    LSTJ40              YES                                          
*                                                                               
         BAS   RE,PRINTEM          PRINT COL HEADS BUILD                        
*                                                                               
         MVI   FORCEMID,C'Y'                                                    
         MVC   MID1+4(9),=C'WORK-CODE'                                          
         MVC   MID1+19(112),SAVMID1   W/C SUMMARY COLS                          
         MVC   MID2+19(112),SAVMID2                                             
         BAS   RE,PRINTEM                                                       
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        IF THEY WISH A FULL WORK CODE SUMMARY, GET TRANSACTIONS      *         
*        YOU REJECTED (BUFTYPE-3) AND RESTORE THEM (BUFTYPE=1).       *         
*---------------------------------------------------------------------*         
*                                                                               
LSTJ40   CLI   PROF18,C'Y'         FULL WC SUM?                                 
         BNE   LSTJ41              NO                                           
*                                                                               
         MVC   BUFKEY,SPACES       YES, GET SAVED REJECTED TRANSACTIONS         
         MVI   BUFTYPE,C'3'                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',(0,ADBUFC),BUFREC,1                        
LSTJ41A  TM    DMCB+8,X'80'                                                     
         BO    LSTJ41                                                           
         MVI   BUFTYPE,C'1'        RESTORE SAVED REJECTED TRANSACTIONS          
         CLC   BUFANAL,=C'99'      BILLING??                                    
         BE    LSTJ41B             YES                                          
*                                                                               
         ZAP   BUFACCS9,=P'0'                                                   
         TM    XJOBSTAT,XJOB                                                    
         BNZ   LSTJ41B                                                          
         ZAP   BUFACCS9,BUFACCS6   UNBILLED CHARGES = GROSS                     
*                                                                               
         CLI   PROF13,C'Y'         SHOW NET AS LESS CD ?                        
         BE    *+10                YES, CD IS GONE                              
         SP    BUFACCS9,BUFACCS7   LESS CD                                      
*                                                                               
         SP    BUFACCS9,BUFACCS8   LESS BILLED                                  
*                                                                               
LSTJ41B  LA    R3,BUFACCS                                                       
         LA    R1,ACCUMNUM                                                      
         USING ACCUMSD,R2                                                       
         L     R2,ACUMAREA         RESTORE JOB TOTALS                           
         LA    R2,JOBTOTS                                                       
         CLC   BUFANAL,=C'99'      BILLING??                                    
         BNE   LSTJ41C             NO                                           
*                                                                               
         USING LITTABD,RF                                                       
         L     RF,=A(LITTAB)                                                    
         MVC   BUFNAME,LITBILL     IN CASE BILLS WERE SUPRESSED                 
         DROP  RF                                                               
         BAS   RE,SUBEM            SUB FROM JOB TOTS                            
         B     LSTJ41D             AND DON'T BOTHER WITH CHARGES                
*                                                                               
LSTJ41C  BAS   RE,ADDEM            ADD CHARGES TO JOB TOTS                      
         L     R2,ACUMAREA                                                      
         LA    R2,TOTCHJOB         AND TOTAL CHARGES                            
         BAS   RE,ADDEM                                                         
*                                                                               
LSTJ41D  GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC  PUT NEW TOTS TO BUFF         
*                                                                               
         MVI   BUFTYPE,C'3'        GET NEXT BUCKET                              
         GOTO1 BUFFALO,DMCB,=C'HIGH',(0,ADBUFC),BUFREC,1                        
         GOTO1 BUFFALO,DMCB,=C'SEQ',(C'3',ADBUFC),BUFREC,1                      
         B     LSTJ41A                                                          
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        PRODUCE THE WORK CODE SUMMARY                                *         
*---------------------------------------------------------------------*         
*                                                                               
LSTJ41   DS    0H                                                               
         MVC   BUFKEY,SPACES                                                    
         MVI   BUFTYPE,C'1'        GET SAVED WORK CODE BUFFS                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',(0,ADBUFC),BUFREC,1                        
         MVC   WCTPPREV,BUFWTYPE                                                
         USING ACCUMSD,R2                                                       
         L     R2,ACUMAREA                                                      
         MVC   SAVETOTS,PZEROS                                                  
         XC    WCTPCNT,WCTPCNT                                                  
*                                                                               
         TM    XJOBSTAT,XJOB       IS THIS AN X JOB                             
         BNO   LSTJ42              NO                                           
         MVC   JOBXJOBS,TOTCHJOB   KEEP TOTALS AS XJOB TOTALS                   
*                                                                               
LSTJ42   TM    DMCB+8,X'80'                                                     
         BO    LSTJ82                                                           
         CLI   BUFTYPE,C'1'                                                     
         BNE   LSTJ82                                                           
         CLC   BUFANAL,=C'99'      BILLING                                      
         BNE   LSTJ44              NO, PROCESS WORKCODE                         
*                                                                               
*                                  BEFORE PROCESSING BILLING, PRINT             
*                                  TOTAL CHARGES                                
         CLI   PROF33,C'Y'                                                      
         BNE   *+12                                                             
         MVI   BUFWTYPE,X'FF'      FORCE LASTS                                  
         BAS   RE,WCTPLAST         PRINT W/C TYPE TOTAL                         
*                                                                               
         BAS   RE,PRINTEM                                                       
         USING ACCUMSD,R2                                                       
         L     R2,ACUMAREA                                                      
         LA    R3,TOTCHJOB                                                      
         USING LITTABD,R5                                                       
         L     R5,=A(LITTAB)                                                    
         MVC   P+2(17),TOTCHAWC                                                 
         MVI   TOTXJOB,C'N'                                                     
         DROP  R5                                                               
*                                                                               
         TM    XJOBSTAT,XJOB       IS THIS AN X JOB                             
         BNO   LSTJ43                                                           
         LA    R3,JOBXJOBS         USE/BUMP XJOB TOTALS                         
         MVI   TOTXJOB,C'Y'        TO SUPRESS BILLABLE CALCULATION              
*                                                                               
LSTJ43   MVI   SPACING,2                                                        
         L     R4,ASUMTAB          JOB SUMMARY PRINT SWITCHES                   
         LA    R4,23(R4)                                                        
         MVI   BALANCE,C'N'                                                     
         GOTO1 =A(TOTALS),DMCB,(RC)                                             
         MVI   CHRGPEND,C'N'                                                    
*                                                                               
LSTJ44   CLI   PROF18,C'Y'         FULL W/C SUMMARY?                            
         BE    LSTJ48              INCLUDE EVERYTHING                           
         CLI   QOPT2,C'T'          IF LIMIT TRANSACTIONS                        
         BE    LSTJ46                                                           
         CLI   QOPT2,C'H'          OR HELD ITEMS ONLY                           
         BE    LSTJ46                                                           
         CLI   QOPT2,C'U'          OR SUPPRESS HELD ITEMS                       
         BE    LSTJ46                                                           
         CLI   MYMEND,X'FF'        OR MONTH OF ACTIVITY                         
         BNE   LSTJ46                                                           
         CLI   QOPT5,C'S'          OR SUPPRESS BILLED ITEMS                     
         BE    LSTJ46                                                           
         CLI   QOPT5,C'A'          OR ALLOCATED ONLY                            
         BE    LSTJ46                                                           
         CLI   QOPT5,C'U'          OR UNALLOCATED ONLY                          
         BNE   LSTJ48                                                           
LSTJ46   CLC   BUFACCS3(64),PZEROS AND IF NO BALANCES ON WORKCODE               
         BNE   LSTJ48                                                           
         L     R2,ACUMAREA                                                      
         LA    R3,JOBTOTS                                                       
         BAS   RE,SPESTS                                                        
         LA    R3,TOTCHJOB                                                      
         BAS   RE,SPESTS                                                        
         B     LSTJ80                                                           
*                                                                               
         USING ACCUMD,R3                                                        
SPESTS   SP    ORESTAC,BUFACCS1                                                 
         SP    REVESTAC,BUFACCSC                                                
         SP    CURESTAC,BUFACCS2                                                
         SP    ESTGRSAC,BUFACCSD                                                
         SP    ESTCOMAC,BUFACCSE                                                
         SP    RHIESTAC,BUFACCSH                                                
*                                                                               
         BR    RE                                                               
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
LSTJ48   CLI   PROF33,C'Y'         SEPARATING TIME AND OOP                      
         BNE   LSTJ49              NO                                           
*                                                                               
         BAS   RE,WCTPLAST         LAST-OF-WC-TYPE PROCESSING                   
         BAS   RE,WCTPBUMP         BUMP BUFFALO ACCUMS                          
*                                                                               
LSTJ49   MVC   P+1(2),BUFANAL      WORK CODE TO PRINT LINE                      
         MVC   P+4(15),BUFNAME     WORK CODE NAME                               
*                                                                               
         ZAP   COMAMT,=P'0'        NEEDED IF FIELD=11 OR 12                     
         USING SUMTABD,R2                                                       
         L     R2,ASUMTAB                                                       
         LA    R3,BUFACCS                                                       
LSTJ50   CLI   SMTBWANT,C'Y'       DO I WANT THIS COLUMN                        
         BNE   LSTJ76                                                           
*                                                                               
         ST    R3,FULL             SAVE CURRENT ACCUM.                          
*                                                                               
         TM    SMTBSTAT,NOBUF      IS THIS NOT A SAVED ACCUM                    
         BZ    LSTJ64                                                           
         TM    SMTBSTAT,NOBILL                                                  
         BZ    LSTJ52                                                           
         CLC   BUFANAL,=C'99'      DON'T DO FOR BILLING                         
         BE    LSTJ76                                                           
*                                                                               
LSTJ52   CLI   SMTBFNUM,4          OVER/UNDER                                   
         BNE   LSTJ54                                                           
         ZAP   NETAMT,BUFACCS4     TOTAL NET                                    
         SP    NETAMT,BUFACCS2     LESS PRESENT ESTIMATE TOTAL                  
         LA    R3,NETAMT                                                        
         B     LSTJ64                                                           
*                                                                               
LSTJ54   CLI   SMTBFNUM,13         OVER/UNDER PCT.                              
         BNE   LSTJ56                                                           
*                                  OVER/UNDER IN NETAMT FROM LAST COL.          
         ZAP   COMAMT,BUFACCS2     PRESENT ESTIMATE IN COMAMT                   
         GOTO1 =A(GENPCT),DMCB,(RC) ETURNS PCT IN NETAMT                        
         CP    NETAMT,=P'0'                                                     
         BE    LSTJ74                                                           
         MVC   AREA(20),SPACES                                                  
         EDIT  (P8,NETAMT),(13,AREA),1,FLOAT=-                                  
         B     LSTJ66                                                           
*                                                                               
LSTJ56   CLI   SMTBFNUM,11         ESTIMATE COMMISSION                          
         BNE   LSTJ58                                                           
         LA    R3,BUFACCSE                                                      
         B     LSTJ64                                                           
*                                                                               
LSTJ58   CLI   SMTBFNUM,12         ESTIMATE GROSS (ASSUME COMMISSION            
         BNE   LSTJ62                              ALREADY DONE.)               
         LA    R3,BUFACCSD                                                      
         B     LSTJ64                                                           
*                                                                               
LSTJ62   CLI   SMTBFNUM,14         GROSS OVER/UNDER                             
         BNE   LSTJ63                                                           
         ZAP   NETAMT,BUFACCS6     GROSS                                        
         SP    NETAMT,BUFACCSD     LESS GROSS ESTIMATE                          
         LA    R3,NETAMT                                                        
         B     LSTJ64                                                           
*                                                                               
LSTJ63   CLI   SMTBFNUM,17         HIGHEST REVISION                             
         BNE   LSTJ64                                                           
         LA    R3,BUFACCSH                                                      
*                                                                               
LSTJ64   CP    0(8,R3),=P'0'                                                    
         BE    LSTJ74                                                           
*                                                                               
         MVC   AREA(20),SPACES                                                  
         EDIT  (P8,0(R3)),(13,AREA),2,FLOAT=-                                   
*                                                                               
LSTJ66   MVI   SMTBJBSW,C'Y'      TURN ON PRINT SWITCH                          
         LA    R1,12               MAXIMUM FIELD WIDTH - 1                      
         ZIC   RE,SMTBLEN          FIELD WIDTH                                  
         SR    R1,RE                                                            
         LA    RF,AREA                                                          
         LA    RF,0(R1,RF)         SHIFT TO RIGHT                               
         ZIC   R1,SMTBPOSI         RELATIVE ADDRESS                             
         LA    R1,P(R1)                                                         
         STC   RE,LSTJ68+1                                                      
LSTJ68   MVC   0(0,R1),0(RF)                                                    
*                                                                               
*                                                                               
         CLI   SMTBFNUM,4          SWITCH SIGNS FOR OVER/UNDER                  
         BE    LSTJ72                                                           
*                                                                               
         CLI   SMTBFNUM,14         GROSS OVER/UNDER                             
         BE    LSTJ72                                                           
         CLI   SMTBFNUM,13         AND FOR OVER/UNDER PERCENT                   
         BNE   LSTJ74                                                           
*                                                                               
LSTJ72   LR    RF,RE                                                            
         BAS   RE,INVSIGN                                                       
LSTJ74   L     R3,FULL             RESTORE A(LAST ACCUM.)                       
LSTJ76   TM    SMTBSTAT,NOBUF      IF NOT A SAVED ACCUM                         
         BO    LSTJ78              THEN DON'T INCREMENT                         
         LA    R3,8(R3)                                                         
LSTJ78   LA    R2,L'SUMTAB(R2)                                                  
         CLI   SMTBWANT,0          END OF TABLE                                 
         BNE   LSTJ50                                                           
*                                                                               
         BAS   RE,PRINTEM                                                       
*                                                                               
         CLC   BUFANAL,=C'99'                                                   
         BNE   LSTJ80                                                           
         GOTO1 =A(PRTAXTOT),DMCB,(RC)    PRINT GST PST  Y                       
*                                                                               
LSTJ80   CLC   BUFANAL,=C'99'                                                   
         BE    *+8                                                              
         MVI   CHRGPEND,C'Y'                                                    
         GOTO1 BUFFALO,DMCB,=C'SEQ',(C'1',ADBUFC),BUFREC,1                      
         B     LSTJ42                                                           
*----------------------------------------------------------------------         
*        PRODUCE SUMMARY TOTALS                                                 
*----------------------------------------------------------------------         
LSTJ82   BAS   RE,PRINTEM                                                       
*        MVI   PRTFLAG,PRTNGWCT  LEAVE THIS PRTNGWC DON'T ALLOW WCTOTS          
         USING ACCUMSD,R2                                                       
         CLI   CHRGPEND,C'Y'                                                    
         BNE   LSTJ84                                                           
*                                                                               
         CLI   PROF33,C'Y'                                                      
         BNE   *+12                                                             
         MVI   BUFWTYPE,X'FF'      FORCE LASTS                                  
         BAS   RE,WCTPLAST                                                      
         L     R2,ACUMAREA                                                      
*                                                                               
         USING LITTABD,R5                                                       
         L     R5,=A(LITTAB)                                                    
         MVC   P+2(17),TOTCHAWC                                                 
         MVI   BALANCE,C'N'                                                     
         LA    R3,TOTCHJOB                                                      
         MVI   TOTXJOB,C'N'                                                     
*                                                                               
         TM    XJOBSTAT,XJOB       IS THIS AN X JOB                             
         BNO   LSTJ83                                                           
         LA    R3,JOBXJOBS         USE/BUMP XJOB TOTALS                         
         MVI   TOTXJOB,C'Y'                                                     
*                                                                               
LSTJ83   MVI   SPACING,2                                                        
         L     R4,ASUMTAB          JOB SUMMARY PRINT SWITCHES                   
         USING SUMTABD,R4                                                       
         LA    R4,SMTBJBSW                                                      
         DROP  R4                                                               
         GOTO1 =A(TOTALS),DMCB,(RC)                                             
         MVI   TOTXJOB,C'N'                                                     
*                                                                               
         USING LITTABD,R5                                                       
LSTJ84   L     R5,=A(LITTAB)                                                    
         MVC   P+2(17),JOBBAL                                                   
         MVI   BALANCE,C'Y'                                                     
         L     R2,ACUMAREA                                                      
         LA    R3,JOBTOTS                                                       
         L     R4,ASUMTAB          JOB SUMMARY PRINT SWITCHES                   
         LA    R4,SMTBJBSW-SUMTABD(R4)                                          
         GOTO1 =A(TOTALS),DMCB,(RC)                                             
*                                                                               
         MVI   BALANCE,C'N'                                                     
         BAS   RE,BXBOT                                                         
         B     LSTJ90                                                           
         DROP  R2,R5                                                            
*                                                                               
*---------------------------------------------------------------------*         
*        END OF WORK CODE SUMMARY                                     *         
*---------------------------------------------------------------------*         
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        SUBROUTINES TO SUPPORT BREAKING W/C SUMMARY BY WORK CODE TYPE*         
*---------------------------------------------------------------------*         
*                                                                               
         USING ACCUMSD,R2                                                       
*                                                                               
WCTPBUMP ST    RE,SAVERE           BUMP BUFF ACCUMS INTO SAVETOTS               
         L     R2,ACUMAREA                                                      
         LA    R2,SAVETOTS                                                      
         LA    R3,BUFACCS                                                       
         LA    R1,ACCUMNUM                                                      
         BAS   RE,ADDEM                                                         
*                                                                               
         ZIC   RE,WCTPCNT                                                       
         LA    RE,1(,RE)                                                        
         STC   RE,WCTPCNT                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
WCTPLAST ST    RE,SAVERE           LAST-OF-WC-TYPE PROCESSING                   
         CLI   WCTPSTAT,BOTHWC     BOTH TYPES OF WORK CODES IN BUFFALO          
         BNE   WCTPLX              NO, DONT DO LASTS                            
*                                                                               
         CLC   WCTPPREV,BUFWTYPE   IS THERE A NEW W/C TYPE IN BUFFALO           
         BE    WCTPLX              NO                                           
*                                                                               
         CLI   WCTPCNT,1           MORE THAT 1 BUFFALO ACCUM HERE               
         BH    WCTPL50             YES                                          
         BE    WCTPL55             ONLY 1 BUFFALO ACCUM?                        
         XC    WCTPCNT,WCTPCNT                                                  
         B     WCTPL60                                                          
*                                                                               
         USING LITTABD,R5                                                       
WCTPL50  L     R5,=A(LITTAB)                                                    
         LA    R4,TIMCHA                                                        
         MVI   SPACING,2           SPACE AFTER TIME TOTAL                       
         CLI   WCTPPREV,TIMEWC                                                  
         BE    *+12                                                             
         LA    R4,OOPCHA                                                        
         MVI   SPACING,1                                                        
         MVC   P+1(L'TIMCHA),0(R4)                                              
*                                                                               
WCTPL55  L     R2,ACUMAREA                                                      
         LA    R3,SAVETOTS                                                      
         L     R4,ASUMTAB                                                       
         LA    R4,SMTBJBSW-SUMTABD(R4)                                          
*                                                                               
         CLI   WCTPCNT,1           ONLY 1 BUFFALO ACCUM HERE                    
         BE    WCTPL58             YES, DO NOT NEED TO ADD UP TOTALS            
*                                                                               
         GOTO1 =A(TOTALS),DMCB,(RC)                                             
WCTPL58  MVC   SAVETOTS,PZEROS                                                  
         XC    WCTPCNT,WCTPCNT                                                  
*                                                                               
WCTPL60  CLI   PROF28,C'Y'         DO THEY WANT BOXES                           
         BNE   WCTPLX                                                           
         GOTO1 =A(BOXMID)                                                       
         CLI   FORCEHED,C'Y'       BOXMID RETURNED A TOF                        
         BE    *+8                                                              
         BAS   RE,PRINTEM                                                       
*                                                                               
WCTPLX   MVC   WCTPPREV,BUFWTYPE                                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        FINISH UP LAST OF ACCOUNT PROCESSING                         *         
*---------------------------------------------------------------------*         
*                                                                               
         USING ACCUMSD,R2                                                       
*                                                                               
LSTJ90   MVI   PRTFLAG,PRTNGDAT    CONSIDER THIS STUFF DETAIL                   
*                                                                               
         L     R2,AMONACC                                                       
         TM    ACMINDS-ACMBGN(R2),ACMIEMUD                                      
         BNO   LSTJ90A                                                          
         GOTO1 =A(LISTLINK),DMCB,(RC)                                           
*                                                                               
LSTJ90A  CLI   PROF22,C'Y'         LIST EXCEPTION REASONS                       
         BNE   LSTJ91                                                           
         L     R5,=A(LITTAB)                                                    
         USING LITTABD,R5                                                       
         MVC   P+1(17),EXCREA                                                   
         DROP  R5                                                               
         GOTO1 =V(UNDERLIN),DMCB,(18,P+1),PSECOND+1                             
         BAS   RE,PRINTEM                                                       
*                                                                               
         GOTO1 =A(PRTEXCP)                                                      
         MVI   SPACING,1                                                        
         BAS   RE,PRINTEM                                                       
*                                                                               
LSTJ91   CLI   SVNEWEST,C'Y'       NEW ESTIMATE JOB?                            
         BNE   LSTJ95                                                           
         BAS   RE,PRTEST           PRINT THEIR P&R'S                            
*                                                                               
         USING GOBLOCKD,R3                                                      
LSTJ95   CLI   PROF20,C'Y'         PRINT RETAIL RECORD AFTER JOB                
         BNE   LSTJ115             NO                                           
         L     R3,ADGOBLOC                                                      
         CLC   GODIST,SPACES       DOES THIS JOB HAVE A DIST SCHEME             
         BNH   LSTJ115             NO                                           
*                                                                               
         MVC   SVDIST,GODIST                                                    
         DROP  R3                                                               
         GOTO1 =A(GETRET),DMCB,(RC)                                             
         TM    RETSTAT,TYPEQ100    UNITS ARE %'S                                
         BNO   LSTJ110                                                          
         ZAP   SVDISTOT,=P'10000'  THEN THE TOTAL IS 100.00%                    
*                                                                               
         USING RBILLD,R5                                                        
LSTJ110  MVC   PREVDIST,SVDIST     SAVE DISTRIBUTION SCHEME                     
         MVC   PREVSTAT,RETSTAT    SAVE PROCESS STATUS                          
         L     R5,RBILLTAB                                                      
         CLI   0(R5),X'FF'                                                      
         BE    LSTJ115                                                          
         USING LITTABD,RF                                                       
         L     RF,=A(LITTAB)                                                    
         MVC   P+1(15),PARTIN                                                   
         DROP  RF                                                               
         MVC   P+17(3),SVDIST                                                   
         GOTO1 =V(UNDERLIN),DMCB,(19,P+1),PSECOND+1                             
         BAS   RE,PRINTEM                                                       
         LA    R4,P+1                                                           
*                                                                               
LSTJ110A MVC   0(12,R4),RBNUM                                                   
         LA    R4,12(R4)           POINT TO END OF PRINT FIELD                  
LSTJ111  CLI   0(R4),C' '          BACK UP TO FIRST NON SPACE                   
         BNE   LSTJ112                                                          
         BCTR  R4,0                                                             
         B     LSTJ111                                                          
LSTJ112  LA    R4,1(R4)                                                         
         MVI   0(R4),C'-'          DASH                                         
         LA    R4,1(R4)                                                         
         EDIT  (P6,RBUNITS),(11,0(R4)),2,ALIGN=LEFT                             
         AR    R4,R0                                                            
         MVI   0(R4),C'/'          SLASH                                        
         LA    R4,1(R4)            SPACE                                        
         ZAP   PL16,RBUNITS        CALC %                                       
         MP    PL16,=P'10000000'                                                
         DP    PL16,SVDISTOT                                                    
         SRP   PL16(10),64-1,5                                                  
         EDIT  (P10,PL16),(12,0(R4)),4,ALIGN=LEFT,DROP=2,TRAIL=C'%'             
         AR    R4,R0                                                            
         MVI   0(R4),C','          COMMA                                        
         LA    R4,2(R4)            SPACE                                        
*                                                                               
         LA    R5,RBLEN(R5)                                                     
         CLI   0(R5),X'FF'         GET NEXT IN TABLE                            
         BE    LSTJ115                                                          
*                                                                               
         LA    R1,P                                                             
         LA    R1,90(R1)          LAST PRINTABLE ADDRESS                        
         CR    R4,R1               DO I HAVE ROOM                               
         BL    LSTJ110A            YES                                          
         BAS   RE,PRINTEM                                                       
         LA    R4,P+1                                                           
         B     LSTJ110A                                                         
*                                                                               
LSTJ115  BAS   RE,COMCLR           CLEAR TRAILING COMMA IF AROUND               
         CLI   PROF10,C'U'         PRINT USER FIELDS?                           
         BE    LSTJ116             NO                                           
         CLI   PROF10,C'A'         PRINT USER FIELDS?                           
         BNE   *+8                                                              
LSTJ116  BAS   RE,USRPRT                                                        
*                                                                               
         GOTO1 =A(COMMPRT),DMCB,(RC) PRINT COMMENTS, PROFILES, ETC.             
*                                  PUT JOB TIME TOTALS IN JOBTOTS               
         USING ACCUMSD,R2                                                       
         L     R2,ACUMAREA                                                      
         MVC   TOTCHRGS,TOTCHJOB                                                
         BAS   RE,GETTIME          PRINT TIME SUMMARY (MANPOWER REPORT)         
*                                  PUT JOB TIME TOTALS IN JOBTOTS               
         DROP  R2                                                               
LSTJ117  L     R2,TIMEAREA         FIRST BUCKET OF TIMEAREA IS PROD TOT         
         USING TIMACCD,R2                                                       
         L     R3,ACUMAREA                                                      
         USING ACCUMSD,R3                                                       
         USING TIMACCD,R2                                                       
         LA    R3,JOBTOTS          JOB TIME TOTALS ARE IN JOBTOTS               
         LA    R1,NTIMEACC         NUMBER OF TIME ACCUMULATORS                  
         BAS   RE,ADDEM                                                         
         NI    XJOBSTAT,X'FF'-XJOB                                              
         MVI   TOTXJOB,C'N'                                                     
         B     LSTPROD                                                          
         DROP  R2,R3,R5                                                         
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*     CHECK FOR LAST OF PRODUCT                                       *         
*                                                                     *         
*---------------------------------------------------------------------*         
*                                                                               
         USING SORTD,R3                                                         
*                                                                               
LSTPROD  L     R3,SORTAREA                                                      
         CLC   SRTKEY(SRTPRLN),PREVSRT  SAME PRODUCT                            
         BE    FRSTACCT                                                         
*                                  M O D E = L E V B L A S T   R T N            
         MVI   MYMODE,LEVBLAST                                                  
         XC    SVH9,SVH9           CLEAR POB, BUT ASSURE H9 PRINTS              
         L     R3,SORTAREA                                                      
         CLI   PROACTV,C'Y'        PRINT IF PRODUCT ACTIVITY                    
         BNE   LSTCLI                                                           
         CLI   RTYPE,1             AND IF REQUEST FOR PRODUCT LEVEL             
         BE    LSTCLI              OR HIGHER                                    
         CLI   QOPT4,C'S'                                                       
         BE    LSTCLI              BUT NOT IF SUPPRESSING HIGH LEVELS           
         MVI   PROACTV,C'N'                                                     
         MVI   CLIACTV,C'Y'                                                     
         MVI   RCSUBPRG,3                                                       
         MVI   MYBOXSW,2                                                        
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   HEAD5+59(15),=15X'BF'                                            
         MVC   HEAD6+59(15),=C'PRODUCT SUMMARY'                                 
         MVC   HEAD7+59(15),=15X'BF'                                            
         B     BLV40                                                            
*                                                                               
BLV30    CLI   PROF01,C'Y'         NEW PAGE FOR PRODUCT TOTALS                  
         BE    BLV40                                                            
         CLI   PROF10,C'N'         OR IF PRINTING COMMENTS                      
         BNE   BLV40                                                            
         CLI   LINE,46             OR IF NOT ENOUGH ROOM                        
         BH    BLV40                                                            
*                                                                               
         MVI   FORCEHED,C'N'                                                    
         MVI   MIDSW,1                                                          
         MVC   ATABLE,ASUMTAB                                                   
         GOTO1 ABXTOP                                                           
*                                                                               
         MVC   PRTFSAVE,PRTFLAG    FORCE PRINT FOR ALIGNMENT                    
         MVI   PRTFLAG,PRTALL                                                   
         GOTO1 AMYREPT,DMCB,(RC)                                                
*                                                                               
         MVI   FORCEMID,C'Y'                                                    
         MVC   MID1+19(112),SAVMID1   W/C SUMMARY COLS                          
         MVC   MID2+19(112),SAVMID2                                             
         GOTO1 AMYREPT,DMCB,(RC)                                                
         MVC   PRTFLAG,PRTFSAVE                                                 
*                                                                               
         USING LITTABD,R5                                                       
BLV40    L     R5,=A(LITTAB)                                                    
         MVC   P+1(15),PROCHA                                                   
         MVI   SPACING,2                                                        
         USING ACCUMSD,R2                                                       
         L     R2,ACUMAREA                                                      
         LA    R3,TOTCHPRD                                                      
         L     R4,ASUMTAB          PRODUCT PRINT SWITCHES                       
         LA    R4,SMTBPRSW-SUMTABD(R4)                                          
         MVI   TOTXJOB,C'N'                                                     
         GOTO1 =A(TOTALS),DMCB,(RC)                                             
*                                                                               
         TM    XJOBSTAT,XJINPRO    IS THERE XJOB DATA IN THIS PRODUCT           
         BNO   BLV50                                                            
         LA    R3,PRDXJOBS         USE/BUMP XJOB TOTALS                         
         MVC   P+1(15),LITPRXJ                                                  
         MVI   SPACING,2                                                        
         MVI   BALANCE,C'N'                                                     
         MVI   TOTXJOB,C'Y'                                                     
         GOTO1 =A(TOTALS),DMCB,(RC)                                             
         NI    XJOBSTAT,X'FF'-XJINPRO  TURN THIS OFF                            
         MVI   TOTXJOB,C'N'                                                     
*                                                                               
BLV50    MVC   P+1(15),PROBAL                                                   
         MVI   BALANCE,C'Y'                                                     
         LA    R3,PRDTOTS                                                       
         L     R4,ASUMTAB          SET SUMMARY PRINT SWITCHES '                 
         LA    R4,SMTBPRSW-SUMTABD(R4)                                          
         GOTO1 =A(TOTALS),DMCB,(RC)                                             
         MVI   BALANCE,C'N'                                                     
         DROP  R2                                                               
         CLI   PROF50,C'Y'         FORCE TOTALS                                 
         BE    *+12                YES                                          
         CLI   PROF24,C'Y'         SUPRESS MP SUMMARIES?                        
         BE    LSTCLI                                                           
*                                                                               
         CLI   PROF50,C'Y'         FORCE TOTALS                                 
         BE    *+12                                                             
         CLI   PROMP,C'Y'          MP ON THIS PRODUCT?                          
         BNE   LSTCLI              NOPE                                         
*                                                                               
         MVI   PROMP,C'N'                                                       
         MVI   CLIMP,C'Y'                                                       
         USING ACCUMSD,R4                                                       
         L     R4,ACUMAREA                                                      
         LA    R4,TOTCHPRD                                                      
         DROP  R4                                                               
         USING TIMACCD,R2                                                       
         L     R2,TIMEAREA                                                      
         LA    R3,PROTIME                                                       
         LA    R2,CLITIME                                                       
         BAS   RE,HIGHTIME                                                      
         DROP  R2                                                               
         B     LSTCLI                                                           
         DROP  R3,R5                                                            
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*     CHECK FOR LAST OF CLIENT                                        *         
*---------------------------------------------------------------------*         
*                                                                               
         USING SORTD,R3                                                         
*                                                                               
LSTCLI   L     R3,SORTAREA                                                      
         CLC   SRTKEY(SRTCLLN),PREVSRT  SAME CLIENT                             
         BE    FRSTPROD                                                         
         MVI   MYMODE,LEVALAST                                                  
         L     R3,SORTAREA                                                      
         CLI   CLIACTV,C'Y'        ONLY IF CLIENT ACTIVITY                      
         BNE   LSTOFF                                                           
         CLI   RTYPE,3             AND REQUEST FOR CLIENT OR HIGHER             
         BL    LSTOFF                                                           
         CLI   QOPT4,C'S'                                                       
         BE    LSTOFF              BUT NOT IF SUPPRESSING HIGH LEVELS           
         MVI   CLIACTV,C'N'                                                     
         MVI   REQACTV,C'Y'                                                     
         MVI   OFFACTV,C'Y'                                                     
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,4                                                       
         MVI   MYBOXSW,2                                                        
*                                                                               
         MVC   HEAD5+60(14),=15X'BF'                                            
         MVC   HEAD6+60(14),=C'CLIENT SUMMARY'                                  
         MVC   HEAD7+60(14),=15X'BF'                                            
*                                                                               
LSTC20   BAS   RE,PRINTEM                                                       
         USING LITTABD,R5                                                       
         L     R5,=A(LITTAB)                                                    
         MVC   P+1(14),CLICHA                                                   
         MVI   SPACING,2                                                        
         USING ACCUMSD,R2                                                       
         L     R2,ACUMAREA                                                      
         LA    R3,TOTCHCLI                                                      
         CLI   SRTBYOFF,C'Y'           OFFICE TOTALS?                           
         BNE   LSTC50                                                           
         LA    R2,TOTCHOFF             BUMP OFFICE TOTALS HERE                  
         LA    R1,ACCUMNUM             TOTALS BUMPS CLI INTO REQEST             
         BAS   RE,ADDEM                                                         
         L     R2,ACUMAREA                                                      
*                                                                               
LSTC50   L     R4,ASUMTAB                                                       
         LA    R4,SMTBCLSW-SUMTABD(R4)                                          
         MVI   BALANCE,C'N'                                                     
         MVI   TOTXJOB,C'N'                                                     
         GOTO1 =A(TOTALS),DMCB,(RC)                                             
         TM    XJOBSTAT,XJINCLI    IS THERE XJOB DATA IN THIS CLIENT            
         BNO   LSTC55                                                           
         LA    R3,CLIXJOBS         USE/BUMP XJOB TOTALS                         
         MVC   P+1(15),LITCLXJ                                                  
         MVI   SPACING,2                                                        
         MVI   BALANCE,C'N'                                                     
         MVI   TOTXJOB,C'Y'                                                     
         GOTO1 =A(TOTALS),DMCB,(RC)                                             
         NI    XJOBSTAT,X'FF'-XJINCLI  TURN THIS OFF                            
         MVI   TOTXJOB,C'N'                                                     
         CLI   SRTBYOFF,C'Y'           OFFICE TOTALS?                           
         BNE   LSTC55                                                           
         LA    R2,OFFXJOBS             BUMP OFFICE TOTALS HERE                  
         LA    R1,ACCUMNUM             TOTALS BUMPS CLI INTO REQEST             
         BAS   RE,ADDEM                                                         
*                                                                               
LSTC55   MVC   P+1(14),CLIBAL                                                   
         MVI   BALANCE,C'Y'                                                     
         L     R2,ACUMAREA                                                      
         LA    R3,CLITOTS                                                       
         CLI   SRTBYOFF,C'Y'           OFFICE TOTALS?                           
         BNE   LSTC60                                                           
         LA    R2,OFFTOTS              BUMP OFFICE TOTALS HERE                  
         LA    R1,ACCUMNUM             TOTALS BUMPS CLI INTO REQEST             
         BAS   RE,ADDEM                                                         
         DROP  R2,R5                                                            
*                                                                               
LSTC60   L     R4,ASUMTAB                                                       
         LA    R4,SMTBCLSW-SUMTABD(R4)                                          
         GOTO1 =A(TOTALS),DMCB,(RC)                                             
         MVI   BALANCE,C'N'                                                     
*                                                                               
         CLI   PROF50,C'Y'         FORCE TOTALS                                 
         BE    *+12                                                             
         CLI   PROF24,C'Y'         SUPRESS MP SUMMARIES?                        
         BE    LSTOFF                                                           
*                                                                               
         CLI   PROF50,C'Y'         FORCE TOTALS                                 
         BE    *+12                                                             
         CLI   CLIMP,C'Y'          MP ON THIS CLI                               
         BNE   LSTOFF              NO                                           
*                                                                               
         MVI   OFFMP,C'Y'                                                       
         MVI   REQMP,C'Y'                                                       
         MVI   CLIMP,C'N'                                                       
         USING ACCUMSD,R4                                                       
         L     R4,ACUMAREA                                                      
         LA    R4,TOTCHCLI                                                      
         DROP  R4                                                               
         USING TIMACCD,R2                                                       
         L     R2,TIMEAREA                                                      
         LA    R3,CLITIME                                                       
         LA    R2,OFFTIME                                                       
         BAS   RE,HIGHTIME                                                      
         LA    R1,NTIMEACC         NUMBER OF TIME ACCUMULATORS                  
         L     R2,TIMEAREA                                                      
         LA    R2,REQTIME          UPDATE REQ TOTS FROM HERE                    
         BAS   RE,ADDEM                                                         
         B     LSTOFF                                                           
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*     CHECK FOR LAST OF OFFICE                                        *         
*---------------------------------------------------------------------*         
*                                                                               
         USING SORTD,R3                                                         
*                                                                               
LSTOFF   L     R3,SORTAREA                                                      
         CLC   SRTKEY(SRTOFLN),PREVSRT  SAME OFFICE                             
         BE    FRSTCLI                                                          
         CLI   OFFACTV,C'Y'                                                     
         BNE   FRSTOFF                                                          
         CLI   SRTBYOFF,C'Y'       DO THEY WANT OFFICE LEVEL TOTALS             
         BNE   FRSTOFF             NO                                           
         CLI   QOPT4,C'S'          SUPRESS HIGH LEVELS                          
         BE    FRSTOFF             YES                                          
         CLI   RTYPE,4             LEDGER LEVEL REQ                             
         BNE   FRSTOFF             NO , NO OFFICE TOTALS                        
         MVI   OFFACTV,C'N'                                                     
         MVI   MYMODE,OFFLAST                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,5                                                       
         MVC   HEAD5+1(6),=C'OFFICE'                                            
         MVC   HEAD5+10(2),SVOFFICE                                             
         MVI   MYBOXSW,2                                                        
*                                                                               
         MVC   HEAD5+60(14),=15X'BF'                                            
         MVC   HEAD6+60(14),=C'OFFICE SUMMARY'                                  
         MVC   HEAD7+60(14),=15X'BF'                                            
*                                                                               
LSTOF30  BAS   RE,PRINTEM                                                       
         MVI   MYMODE,OFFLAST                                                   
         USING LITTABD,R5                                                       
         L     R5,=A(LITTAB)                                                    
         MVC   P+1(14),OFFCHA                                                   
         MVI   SPACING,2                                                        
         USING ACCUMSD,R2                                                       
         L     R2,ACUMAREA                                                      
         LA    R3,TOTCHOFF                                                      
         L     R4,ASUMTAB                                                       
         LA    R4,SMTBOFSW-SUMTABD(R4)                                          
*                                                                               
         MVI   BALANCE,C'N'                                                     
         MVI   TOTXJOB,C'N'                                                     
         GOTO1 =A(TOTALS),DMCB,(RC)                                             
*                                                                               
         TM    XJOBSTAT,XJINOFF                                                 
         BNO   LSTOF50                                                          
         LA    R3,OFFXJOBS         USE/BUMP XJOB TOTALS                         
         MVC   P+1(15),LITOFXJ                                                  
         MVI   SPACING,2                                                        
         MVI   BALANCE,C'N'                                                     
         MVI   TOTXJOB,C'Y'                                                     
         GOTO1 =A(TOTALS),DMCB,(RC)                                             
         NI    XJOBSTAT,X'FF'-XJINOFF  TURN THIS OFF                            
         MVI   TOTXJOB,C'N'                                                     
*                                                                               
LSTOF50  MVC   P+1(14),OFFBAL                                                   
         MVI   BALANCE,C'Y'                                                     
         LA    R3,OFFTOTS                                                       
         GOTO1 =A(TOTALS),DMCB,(RC)                                             
         MVI   BALANCE,C'N'                                                     
         DROP  R2,R5                                                            
*                                                                               
         CLI   PROF50,C'Y'         FORCE TOTALS                                 
         BE    *+12                                                             
         CLI   PROF24,C'Y'         SUPRESS MP SUMMARIES?                        
         BE    FRSTOFF                                                          
*                                                                               
         CLI   PROF50,C'Y'         FORCE TOTALS                                 
         BE    *+12                                                             
         CLI   OFFMP,C'Y'          MP ON THIS OFFICE                            
         BNE   FRSTOFF             NO                                           
*                                                                               
         MVI   OFFMP,C'N'                                                       
         USING ACCUMSD,R4                                                       
         L     R4,ACUMAREA                                                      
         LA    R4,TOTCHOFF                                                      
         DROP  R4                                                               
         USING TIMACCD,R2                                                       
         L     R2,TIMEAREA                                                      
         LA    R3,OFFTIME                                                       
         LA    R2,CLITIME          DUMMY, ZAPED IN FRSTCLI                      
         BAS   RE,HIGHTIME         PRINT OFFICE TIME                            
         B     FRSTOFF                                                          
         DROP  R2,R3                                                            
*                                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        DO FIRST FOR OFFICE STUFF, TOP OF FORM, MOVE OFFICE INTO     *         
*        HEADER                                                       *         
*---------------------------------------------------------------------*         
*                                                                               
         USING SORTD,R3                                                         
         USING ACKEYD,R2                                                        
*                                                                               
FRSTOFF  DS    0H                                                               
         L     R3,SORTAREA                                                      
         CLI   SRTOFF,X'FF'        'LAST OFFICE' OFFICE                         
         BE    LSTOFFX             THEN I'M DONE                                
         USING ACCUMSD,R2          DSECT FOR ALL ACCUMULATORS                   
         L     R2,ACUMAREA                                                      
         MVC   OFFTOTS,PZEROS                                                   
         MVC   TOTCHOFF,PZEROS                                                  
         MVC   OFFXJOBS,PZEROS                                                  
         MVI   CLIACTV,C'N'                                                     
*                                                                               
         L     R2,ASUMTAB                                                       
         USING SUMTABD,R2                                                       
FOFF10   MVI   SMTBOFSW,C'N'       CLEAR PRINT SWITCHES                         
         LA    R2,L'SUMTAB(R2)                                                  
         CLI   0(R2),0                                                          
         BNE   FOFF10                                                           
*                                                                               
         USING TIMACCD,R2                                                       
         L     R2,TIMEAREA         ZERO OFFICE LEVEL TIME TOTALS                
         LA    R2,OFFTIME                                                       
         LA    R3,PZEROS                                                        
         LA    R1,NTIMEACC                                                      
         BAS   RE,ZAPEM                                                         
         B     FRSTCLI                                                          
         DROP  R2                                                               
LSTOFFX  CLI   RTYPE,4             ONLY IF REQUEST FOR WHOLE LEDGER             
         BNE   EXIT                                                             
         CLI   REQACTV,C'Y'                                                     
         BNE   EXIT                                                             
         MVI   MYMODE,REQLAST                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,5                                                       
         MVI   MYBOXSW,2                                                        
*                                                                               
         MVC   HEAD5+60(15),=15X'BF'                                            
         MVC   HEAD6+60(15),=C'REQUEST SUMMARY'                                 
         MVC   HEAD7+60(15),=15X'BF'                                            
*                                                                               
LSTRQ30  BAS   RE,PRINTEM                                                       
         USING LITTABD,R5                                                       
         L     R5,=A(LITTAB)                                                    
         MVC   P+1(15),REQCHA                                                   
         MVI   SPACING,2                                                        
         USING ACCUMSD,R2                                                       
         L     R2,ACUMAREA                                                      
         LA    R3,TOTCHREQ                                                      
         L     R4,ASUMTAB          PRINT SWITCHES                               
         LA    R4,SMTBRQSW-SUMTABD(R4)                                          
         MVI   BALANCE,C'N'                                                     
         GOTO1 =A(TOTALS),DMCB,(RC)                                             
*                                                                               
         TM    XJOBSTAT,XJINREQ                                                 
         BNO   LSTOFX50                                                         
         LA    R3,OFFXJOBS         USE/BUMP XJOB TOTALS                         
         MVC   P+1(15),LITRQXJ                                                  
         MVI   SPACING,2                                                        
         MVI   TOTXJOB,C'Y'                                                     
         GOTO1 =A(TOTALS),DMCB,(RC)                                             
         MVI   TOTXJOB,C'N'                                                     
         NI    XJOBSTAT,X'FF'-XJINREQ  TURN THIS OFF                            
*                                                                               
LSTOFX50 MVC   P+1(15),REQBAL                                                   
         MVI   BALANCE,C'Y'                                                     
         LA    R3,REQTOTS                                                       
         L     R4,ASUMTAB          PRINT SWITCHES                               
         LA    R4,SMTBRQSW-SUMTABD(R4)                                          
         GOTO1 =A(TOTALS),DMCB,(RC)                                             
         MVI   BALANCE,C'N'                                                     
*                                                                               
         CLI   PROF50,C'Y'         FORCE TOTALS                                 
         BE    *+12                YES, FORCE EM                                
         CLI   PROF24,C'Y'         SUPRESS MP SUMMARIES?                        
         BE    EXIT                                                             
*                                                                               
         CLI   PROF50,C'Y'         FORCE TOTALS                                 
         BE    *+12                YES, FORCE EM                                
         CLI   REQMP,C'Y'                                                       
         BNE   EXIT                                                             
*                                                                               
         L     R2,ACUMAREA                                                      
         LA    R4,TOTCHREQ                                                      
         USING TIMACCD,R2                                                       
         L     R2,TIMEAREA                                                      
         LA    R3,REQTIME                                                       
         LA    R2,CLITIME          DUMMY FOR REQLAWST                           
         BAS   RE,HIGHTIME                                                      
         B     EXIT                                                             
         DROP  R2,R3,R5                                                         
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        GET CLIENT RECORD FROM DATAMGR, SAVE CLIENT LEVEL PROFILES   *         
*        IF THEY ARE FOUND, ELSE USE COMPOSITE LEDGER PROFILES        *         
*        SET UP REPORT AND SUMMARY HEADERS AS PERCLIENT LEVEL PROFILE *         
*---------------------------------------------------------------------*         
*                                                                               
         USING ACKEYD,R2                                                        
*                                                                               
FRSTCLI  DS    0H                                                               
         GOTO1 =A(AFRSTCLI),DMCB,(RC)                                           
         B     FRSTPROD                                                         
         DROP  R2                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        GET PRODUCT RECORD                                           *         
*---------------------------------------------------------------------*         
*                                                                               
         USING ACKEYD,R2                                                        
         USING SORTD,R3                                                         
*                                                                               
FRSTPROD DS    0H                                                               
         LA    R2,MYKEY                                                         
         L     R3,SORTAREA                                                      
         MVC   SAVPRO,SRTPRO           SAVE PROD FOR PRINTEM                    
         MVC   MYKEY,SPACES                                                     
         MVC   ACKEYACC(3),SAVCUL                                               
         MVC   ACKEYACC+3(3),SRTCLI                                             
         MVC   ACKEYACC+6(3),SRTPRO                                             
         BAS   RE,READPRO                                                       
         L     R2,PROBUFF                                                       
         LA    R3,PRONAME                                                       
         GOTO1 =A(GETNAME),DMCB,(RC)                                            
         MVI   ELCODE,X'24'                                                     
         BAS   RE,GETEL                                                         
         BE    FPRO10                                                           
         SR    R2,R2                                                            
FPRO10   MVI   MYMODE,LEVBFRST                                                  
         GOTO1 =A(BUILD24),DMCB,(RC),(R2)  PUT POB IN SVH9                      
*                                                                               
         USING ACCUMSD,R2                                                       
         L     R2,ACUMAREA                                                      
         MVC   PRDTOTS,PZEROS                                                   
         MVC   TOTCHPRD,PZEROS                                                  
         MVC   PRDXJOBS,PZEROS                                                  
         MVI   PROACTV,C'N'                                                     
*                                                                               
         USING TIMACCD,R2                                                       
         L     R2,TIMEAREA         ZERO PRODUCT TIME TOTALS                     
         LA    R2,PROTIME                                                       
         LA    R3,PZEROS                                                        
         LA    R1,NTIMEACC                                                      
         BAS   RE,ZAPEM                                                         
         DROP  R2                                                               
         L     R2,ASUMTAB                                                       
         USING SUMTABD,R2                                                       
BLV2     MVI   SMTBPRSW,C'N'       CLEAR PRINT SWITCHES                         
         LA    R2,L'SUMTAB(R2)                                                  
         CLI   0(R2),0                                                          
         BNE   BLV2                                                             
         DROP  R2,R3                                                            
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        GET AN ACCOUNT RECORD                                        *         
*---------------------------------------------------------------------*         
*                                                                               
         USING ACKEYD,R2                                                        
         USING SORTD,R3                                                         
*                                                                               
FRSTACCT DS    0H                                                               
         XC    PROCSTAT,PROCSTAT      INIT TRANSACTION STATUS BYTE              
         XC    BILLSTAT,BILLSTAT                                                
         XC    JOBSTAT,JOBSTAT        INIT JOB STATUS BYTE                      
         XC    WCTPSTAT,WCTPSTAT      WORKCODE TYPE TOTAL'S CONTROL             
         MVI   PRTFLAG,PRTNGDET                                                 
         MVI   JOBACTV,C'N'                                                     
         MVI   TYPACTV,C'N'                                                     
         MVI   ANLACTV,C'N'                                                     
         MVI   CONACTV,C'N'                                                     
         MVC   NEXTGST,=H'1'          PRIME OFFSET AT 1                         
         ZAP   GSTTOTAL,=P'0'         ZAP GST ACCUM                             
         GOTO1 =A(PSTCLR),DMCB,(RC),PSTTTJBQ                                    
         MVI   SUBLGST,C'N'        FLAG TO HAVE SUBLINE PRINT GST               
         MVI   GSTFLAG,C'N'        Y, JOB HAS GST                               
         MVI   PSTFLAG,C'N'        DITTO FOR PST                                
         MVI   RCSUBPRG,0                                                       
         OI    JOBSTAT,NEEDOPT        NEED A GETOPT CALL FROM HERE              
         MVI   MYMODE,PROCACC         FOR ACCOUNT TYPE HEADERS                  
*                                                                               
         LA    R2,MYKEY                                                         
         L     R3,SORTAREA                                                      
         MVC   SAVJOB,SRTJOB           SAVE JOB FOR PRINTEM                     
         MVC   ACKEYWRK(ACRECORD-ACKEYWRK),SPACES HGHER THAN JOB LVL            
         MVC   ACKEYACC(3),SAVCUL                                               
         MVC   ACKEYACC+3(3),SRTCLI                                             
         MVC   ACKEYACC+6(3),SRTPRO                                             
         MVC   ACKEYACC+9(6),SRTJOB                                             
         BAS   RE,READREAD                                                      
         L     R2,DMGRBUFF                                                      
         ST    R2,ADACC                                                         
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    R2,ADACCSTA                                                      
         USING ACSTATD,R2                                                       
         MVC   SAVELAST,ACSTLAST   SAVE LAST ACT DATE                           
         MVC   SVSTAT,ACSTSTAT   SAVE LAST ACT DATE                             
*                                                                               
         MVI   ELCODE,X'32'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R2,ADACCBAL                                                      
*                                                                               
         L     R2,DMGRBUFF                                                      
         LA    R3,JOBNAME                                                       
         GOTO1 =A(GETNAME),DMCB,(RC)   SAVE ACCOUNT NAME IN 0(R3)               
*                                                                               
         L     R2,ADCOMP               SAVE MEDIA NAME                          
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   FJOBG                                                            
         USING ACMEDIAD,R2                                                      
FJOBA    CLC   ACMDCODE,SAVJOB                                                  
         BNE   FJOBD                                                            
         MVC   MEDIANM(12),ACMDDESC+3                                           
         B     FJOBG                                                            
*                                                                               
FJOBD    BAS   RE,NEXTEL                                                        
         BE    FJOBA                                                            
*                                                                               
FJOBG    MVI   ELCODE,X'24'                                                     
         L     R2,DMGRBUFF                                                      
         BAS   RE,GETEL                                                         
         BE    FJOBJ                                                            
         SR    R2,R2               SET R2 FOR BUILD24                           
FJOBJ    MVI   MYMODE,PROCACC                                                   
         GOTO1 =A(BUILD24),DMCB,(RC),(R2)  PUT POB IN SVH9                      
*                                                                               
         NI    XJOBSTAT,X'FF'-XJOB SEE IF WE GOT AN X JOB                       
         MVI   APPSW,C'Y'          OLD ESTIMATE APPROVAL SWITCH                 
         MVI   ELCODE,JOBELQ       GET JOB ELEMENT                              
         L     R2,DMGRBUFF                                                      
         BAS   RE,GETEL                                                         
         USING JOBELD,R2                                                        
         CLI   JOBLN,JOBLN2Q       DOES THIS JOB HAVE STA1                      
         BNH   FJOBL               NO                                           
         TM    JOBSTA1,JOBSXJOB                                                 
         BNO   *+8                                                              
         OI    XJOBSTAT,XJOB+XJINPRO+XJINCLI+XJINOFF+XJINREQ                    
         TM    JOBSTA1,JOBSUEST    IS THERE AN UNAPPROVED ESTIMATE              
         BNO   *+8                 NO                                           
         MVI   APPSW,C'N'                                                       
*                                                                               
         USING ACCUMSD,R2                                                       
FJOBL    L     R2,ACUMAREA                                                      
         MVC   JOBTOTS,PZEROS                                                   
         MVC   TOTCHJOB,PZEROS                                                  
         MVC   TOTCHRGS,PZEROS                                                  
         MVC   BILLTOTS,PZEROS                                                  
         L     R2,BILLTAB         TABLE OF BILLS FOR EXCLUDING TIME             
         XC    0(4,R2),0(R2)       CLEAR  COUNTER                               
         DROP  R2                                                               
         MVI   CHRGPEND,C'Y'                                                    
         LA    R2,PRTSWS+1         CLEAR PRINT SWITCHES                         
         LA    RF,NPRTSWS                                                       
FAC200   MVI   0(R2),C'N'                                                       
         LA    R2,2(R2)                                                         
         BCT   RF,FAC200                                                        
*                                                                               
         MVC   ATABLE,AOPTTAB      DEFAULT TABLE SETTING                        
         MVC   RATE,OOPRATE        DEFAULT PRINT RATE IN NARR SETTING           
         MVC   PRINTEST,PROF17     USE OOPS PRINT EST VAL PROFILE               
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,(X'80',1)                          
         MVC   BUFACCS,PZEROS                                                   
*                                                                               
         GOTO1 =A(ESTRTN),DMCB,(RC) GETOPT, JOBBER CALLS, BUILD A TABLE         
*                                  OF ESTIMATES FOR THIS JOB                    
*                                                                               
         L     R5,ADGOBLOC                                                      
         USING GOBLOCKD,R5                                                      
         MVC   SVOFFICE,GOEFFOFC                                                
         DROP  R5                                                               
         MVC   SVANAL,SPACES       CLEAR WORK CODE                              
*                                                                               
         USING JXCEPTD,R5                                                       
         LA    R5,JXBLOCK                                                       
         MVC   JXCODES,SVCODES     RESTORE EXCEPTION REASONS                    
         MVC   JXNCODES,SVNCODES                                                
*                                                                               
         L     R2,AUNITS                                                        
         LA    R2,16(R2)                                                        
         ZAP   0(8,R2),=P'0'                                                    
*                                                                               
         CLI   PROF33,C'Y'         SEPERATING TIME                              
         BNE   FAC210                                                           
         L     R2,TIMESUB          CLEAR TIME JOB TOTALS ACCUMS                 
         LA    R2,2*TIMSUBLN(R2)   JOB  IS THE THIRD                            
         LA    R3,PZEROS                                                        
         LA    R1,4                                                             
         BAS   RE,ZAPEM                                                         
*                                                                               
FAC210   MVC   BUFANAL,SPACES                                                   
         MVI   ANLACTV,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   PROF17,C'1'                                                      
         BNL   FAC220                                                           
         CLI   PROF47,C'1'         TIME PRINT ESTIMATE VAL                      
         BNL   FAC220                                                           
         B     *+8                                                              
FAC220   OI    JOBSTAT,NEEDESTS    WILL NEED ESTIMATE VALUES IN THE             
*                                  DETAIL SECTION OF THIS REPORT                
*                                                                               
         MVI   RCSUBPRG,0          RESET PRINT FIELDS                           
         MVI   MYBOXSW,1                                                        
         MVI   FORCEMID,C'N'                                                    
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         DROP  R3,R5                                                            
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        PROCESS A NEW TRANSACTION TYPE, PRINT THE RIGHT HEADERS      *         
*---------------------------------------------------------------------*         
*                                                                               
         USING SORTD,R3                                                         
*                                                                               
FRSTTYPE DS    0H                                                               
         L     R3,SORTAREA                                                      
         L     R4,AOPTTAB          OPTIONS (OOPS TABLE IS THE DEFAULT)          
         ST    R4,ATABLE                                                        
         XC    PROCSTAT,PROCSTAT                                                
         XC    TYPESTAT,TYPESTAT                                                
         MVI   TYPACTV,C'N'                                                     
*                                                                               
         L     R2,AUNITS                                                        
         LA    R2,8(R2)                                                         
         ZAP   0(8,R2),=P'0'                                                    
*                                                                               
         CLI   SRTTYPE,X'00'       AM I SORTING BY TRANS TYPE                   
         BE    FRSTWC              NO, HEADERS ARE SET IN  RSTACC               
*                                                                               
         USING ACCUMSD,R2                                                       
FTY1     L     R2,ACUMAREA                                                      
         MVC   TYPTOTS,PZEROS                                                   
         MVC   TOTCHTYP,PZEROS                                                  
*                                                                               
         L     R2,TIMESUB          CLEAR TIME TYPE TOTALS ACCUMS                
         LA    R2,TIMSUBLN(R2)     TYPE IS THE SECOND ACCUM                     
         LA    R3,PZEROS                                                        
         LA    R1,4                                                             
         BAS   RE,ZAPEM                                                         
         DROP  R2                                                               
         USING SORTD,R3                                                         
         L     R3,SORTAREA                                                      
*                                                                               
         CLI   SRTTYPE,SRTTOOP     AM I PRINTING OUT OF POCKET TRANS            
         BE    FTY2                YES, SET UP HEADERS                          
*                                                                               
         CLI   SRTTYPE,SRTTTIME    AM I PRINTING TIME ONLY TRANSACTIONS         
         BNE   FRSTWC              NO, SKIP                                     
*                                                                               
         OI    PROCSTAT,TIMETRAN   TIME TRANSACTIONS                            
         MVC   RATE,STAFRATE      PRINT HRS/RATE IN NARR SETTING                
         MVC   PRINTEST,PROF47     USE TIME PRINT EST VAL PROFILE               
         L     R4,ATIMETAB                                                      
         ST    R4,ATABLE           USE TIME COL SETTINGS                        
         MVI   MIDSW,1                                                          
         MVI   MYBOXSW,4                                                        
         MVI   RCSUBPRG,7                                                       
         B     FRSTWC                                                           
*                                                                               
FTY2     DS    0H                                                               
         OI    PROCSTAT,OOPSTRAN                                                
         MVC   RATE,OOPRATE        PRINT HRS/RATE IN NARR SETTING               
         MVC   PRINTEST,PROF17     USE OOPS PRINT EST VAL PROFILE               
         L     R4,AOPTTAB                                                       
         ST    R4,ATABLE                                                        
         MVI   MIDSW,1             OPPS TRANSACTION                             
         MVI   MYBOXSW,1                                                        
         MVI   RCSUBPRG,0                                                       
         CLI   FORCEHED,C'Y'                                                    
         BE    FRSTWC                                                           
*                                                                               
         ZIC   RE,LINE             CHECK FOR ENOUGH ROOM                        
         LA    RE,11(RE)           NEED 4 FOR HEADS 2 FOR WC                    
         ZIC   RF,MAXLINES                                                      
         CR    RE,RF                                                            
         BL    FTY3                ENOUGH ROOM, PRINT MID HEADERS               
         MVI   FORCEHED,C'Y'       NO, ROOM, FORCE TOP-O-FORM                   
         B     FRSTWC                                                           
*                                                                               
FTY3     DS    0H                                                               
         GOTO1 ABXTOP                                                           
         BAS   RE,PRINTEM                                                       
         MVI   FORCEMID,C'Y'                                                    
         MVC   MID1,SVH11                                                       
         MVC   MID2,SVH12                                                       
         BAS   RE,PRINTEM                                                       
         DROP  R3                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        GET WC NAME, GET RATE                                        *         
*        PROCESS ESTIMATES, IF WANTED AT THIS LEVEL                   *         
*---------------------------------------------------------------------*         
*                                                                               
         USING SORTD,R3                                                         
*                                                                               
FRSTWC   DS    0H                                                               
         LA    R2,PRTSWS           CLEAR PRINT SWITCHES                         
         LA    RF,NPRTSWS                                                       
FWC20    MVI   0(R2),C'N'                                                       
         LA    R2,2(,R2)                                                        
         BCT   RF,FWC20                                                         
*                                                                               
         L     R3,SORTAREA                                                      
         ZAP   WCSW,=P'0'                                                       
         MVI   ANALPEND,0                                                       
         MVI   ANLACTV,C'N'                                                     
*                                                                               
         L     R2,AUNITS           CLEAR UNITS ACCUM                            
         ZAP   0(8,R2),=P'0'                                                    
*                                                                               
         MVC   BUFACCS,PZEROS      CLEAR BUFFALO ACCUMS                         
         ZAP   BUFPCT,=P'0'                                                     
         MVC   COMMABLE,PZEROS     COMMISSIONABLE AMOUNT OF A W/C               
         CLI   SRTTYPE,SRTTBILL   SHOULD THIS BE IN THE TABLE                   
         BE    PROCTRAN            YES                                          
         CLC   SRTWC,=C'99'        BILLING                                      
         BE    FWC30               YES                                          
*                                                                               
         TM    JOBSTAT,NEEDESTS    DO WE NEED ESTIMATE VALUES NOW?              
         BNO   FWC23A              NO                                           
         CLI   PROF25,C'Y'         PRINT ESTIMATES W/O ACTUALS?                 
         BNE   FWC23A              NO                                           
*                                                                               
         MVC   CPESTSTR,SVANAL     PRINT ESTIMATES FROM PREV WC TO              
         ZIC   RF,CPESTSTR+1       CURRENT                                      
         LA    RF,1(RF)                                                         
         STC   RF,CPESTSTR+1                                                    
         MVI   CPESTSTP,C' '                                                    
         MVI   CPESTETP,C' '                                                    
*                                                                               
         MVC   CPESTEND,SRTWC                                                   
         CLI   PROF33,C'Y'         SEPERATE TIME AND OOP                        
         BNE   FWC23               NO, DON'T NEED TYPE                          
*                                                                               
         MVI   CPESTSTP,TIMEWC     START WITH TIME                              
*                                                                               
         CLC   SRTKEY(SRTJBLN),PREVSRT   SAME JOB                               
         BNE   FWC22                     NO START WITH TIME                     
*                                                                               
         LA    R2,PREVSRT                                                       
         CLI   SRTTYPE-SRTKEY(R2),SRTTOOP PRECV                                 
         BNE   *+8                                                              
         MVI   CPESTSTP,O_O_P_WC                                                
*                                                                               
FWC22    MVI   CPESTETP,TIMEWC     SET END STATUS                               
         CLI   SRTTYPE,SRTTTIME          TIME WORK CODE                         
         BE    FWC23                     YES,                                   
*                                        NO, MUST BE OOPS TRANS                 
         MVI   CPESTETP,O_O_P_WC                                                
*                                                                               
FWC23    BAS   RE,CALLPEST         PRINT ESTIMATE VALUES UP TO THIS             
         CLC   BUFANAL,SVANAL      SVANAL SET IF ESTIMATE FOUND                 
         BNE   FWC23A                                                           
         CLC   BUFANAL,SRTWC       DID I GET THIS WC BACK                       
         BE    FWC30                                                            
*                                                                               
FWC23A   MVC   BUFKEY,SPACES                                                    
         MVI   BUFTYPE,C'1'                                                     
         MVC   BUFANAL,SRTWC                                                    
         CLI   SRTTYPE,0                                                        
         BE    FWC23C                                                           
*                                                                               
         MVI   BUFWTYPE,TIMEWC                                                  
         CLI   SRTTYPE,SRTTTIME                                                 
         BE    *+8                                                              
         MVI   BUFWTYPE,O_O_P_WC                                                
*                                                                               
FWC23C   GOTO1 BUFFALO,DMCB,=C'HIGH',(0,ADBUFC),BUFREC,1                        
         TM    DMCB+8,X'80'                                                     
         BO    FWC25                                                            
*                                                                               
         CLI   BUFTYPE,C'1'        DID I GET AN EST RECORD BACK                 
         BNE   FWC25               NO, NO ESTIMATES                             
*                                                                               
         CLC   BUFANAL,SRTWC       IS THIS THE WORK CODE I'M LOOKIN FOR         
         BE    FWC30               EQUAL, GET TRANSACTION DATA                  
*                                  NOT FOUND, SET UP BUFREC                     
FWC25    MVC   BUFACCS,PZEROS                                                   
         CLI   SRTWC,X'FF'         CALLED FROM A JOB WITH ESTIMATES             
         BNE   FWC25A              BUT NO TRANS, SO FINISH UP                   
         MVC   PREVSRT(SRTDATAL),SRTKEY                                         
         B     REQX                                                             
*                                                                               
FWC25A   MVC   SVANAL,SRTWC                                                     
*                                                                               
         L     R2,ADGOBLOC                                                      
         USING GOBLOCKD,R2                                                      
         MVC   GOSELWC,SVANAL      CALL GETOPT FOR COMISSION, OTHER             
         GOTO1 GETOPT,DMCB,GOBLOCKD GETOPT PARMS ARE SET AT 'PROCACC'           
         XC    GOSELWC,GOSELWC                                                  
         ZAP   BUFPCT,GOAGYCOM   COMMISION AT THIS LEVEL                        
*                                                                               
         GOTO1 =A(WCNAME),DMCB,(RC)                                             
         MVC   BUFANAL,SVANAL                                                   
         MVI   BUFWTYPE,O_O_P_WC                                                
         TM    PROCSTAT,TIMETRAN                                                
         BNO   *+8                                                              
         MVI   BUFWTYPE,TIMEWC                                                  
         MVC   BUFNAME,WNAME                                                    
         MVI   BUFTYPE,C'1'                                                     
         B     FWC35                                                            
*                                                                               
FWC30    MVC   SVANAL,SRTWC                                                     
         BAS   RE,PESTVAL          PRINT WC HEAD AND ESTIMATE .                 
*                                                                               
FWC35    L     R2,TIMESUB          CLEAR TIME WC TOTALS ACCUMS                  
         LA    R3,PZEROS                                                        
         LA    R1,4                                                             
         BAS   RE,ZAPEM                                                         
*                                                                               
FWC40    MVC   BUFACCS,PZEROS      CLEAR BUFFALO ACCUMS                         
*                                                                               
         ZAP   SVCOMRTE,BUFPCT     SAVE COMMISION RATE                          
*                                                                               
         DROP  R2,R3               THEY ARE ALREADY IN BUFFALO TABLE            
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        GET CA NAME                                                  *         
*---------------------------------------------------------------------*         
*                                                                               
         USING SORTD,R3                                                         
*                                                                               
FRSTCA   DS    0H                                                               
         MVI   SUPPSW,C'Y'         SET FIRST TIME SWITCH                        
         L     R3,SORTAREA                                                      
         MVI   CONACTV,C'N'                                                     
         MVC   VENDNAME,SPACES                                                  
         LA    R2,SRTDATA                                                       
         USING SR44D,R2                                                         
         CLI   SR44TYPE,9                                                       
         BE    PROCTRAN                                                         
         DROP  R2                                                               
*                                                                               
         USING ACKEYD,R2                                                        
         LA    R2,MYKEY                                                         
         MVC   MYKEY,SPACES                SPACE OUT KEY                        
         MVC   ACKEYACC(3),SAVCUL                                               
         MVC   ACKEYACC+3(3),SRTCLI                                             
         MVC   ACKEYACC+6(3),SRTPRO                                             
         MVC   ACKEYACC+9(6),SRTJOB                                             
         MVC   ACKEYWRK,SRTWC                                                   
         CLI   SRTKEYLN(R3),X'68'  IS THIS A P/O                                
         BNE   *+10                                                             
         MVC   ACKEYWRK(2),=C'**'  THEN USE ORIGINAL PO WORK CODE               
         MVC   ACKEYCON(1),RCCOMPFL                                             
         MVC   ACKEYCON+1(14),SRTCA                                             
         BAS   RE,READIO                                                        
         L     R2,IOSPACE                                                       
         CLC   MYKEY(32),0(R2)                                                  
         BNE   FRSTCA10                                                         
*                                                                               
         MVI   ELCODE,X'43'                                                     
         BAS   RE,GETEL                DID I GET THE SUB ACC?                   
         BNE   FRSTCA10                NO                                       
*                                      NO                                       
         USING TRSUBHD,R2                                                       
         ZIC   R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         BM    FRSTCA10                                                         
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   VENDNAME(0),TRSBNAME                                             
*                                                                               
FRSTCA10 MVC   PREV4C,SPACES                                                    
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        PROCESS A TRANSACTION                                        *         
*---------------------------------------------------------------------*         
*                                                                               
         USING SORTD,R3                                                         
*                                                                               
PROCTRAN DS    0H                                                               
         MVI   MYMODE,PROCTRNS                                                  
*                                                                               
         BAS   RE,CLRPRTBF                                                      
*                                                                               
         L     R3,SORTAREA                                                      
         MVC   PREVSRT(SRTDATAL),SRTKEY                                         
*                                                                               
         CLC   SRTWC,=C'99'        BILLING                                      
         BE    *+8                 YES                                          
         OI    JOBSTAT,CHARGES                                                  
*                                                                               
         CLI   SRTTYPE,SRTTBILL    IS THIS A BILLING AND THEY                   
         BNE   TRN002              ARE SEGREGATING TIME                         
         MVI   ANLACTV,C'N'        DONT PRINT IT ...                            
         MVI   CONACTV,C'N'                                                     
         MVI   TYPACTV,C'N'                                                     
         GOTO1 =A(BILLPUT),DMCB,(RC)   SAVE THE BILLED INFO                     
         B     REQGET              GET NEXT RECORD                              
*                                                                               
TRN002   CLI   SRTTYPE,SRTTTIME    IS THIS A TIME CHARGE                        
         BNE   TRN005              NO                                           
         BAS   RE,PUTTIME          YES SAVE TIME CHARGES FOR BILLING            
*                                                                               
TRN005   XC    TRANSTAT,TRANSTAT   CLEAR TRANSACTION STATUS                     
         LA    R2,SRTDATA                                                       
         USING SR44D,R2                                                         
         CLI   0(R2),X'44'                                                      
         BNE   TRN050                                                           
         MVI   WROFFED,C'N'        INIT WRITE OFF BYTE                          
*                                                                               
         CLC   SRTWC,=C'99'        BILLING                                      
         BNE   TRN007              NO                                           
         CLI   SR44NARR,C'C'       CLIENT BILLING                               
         BE    TRN006              YES                                          
         CLI   SR44NARR,C'A'       OR ALLOCATED BILLING                         
         BNE   TRN007              NO                                           
TRN006   MVI   CLIBILL,C'Y'        FLAG JOB FOR EXCEPTION REASON 9              
*                                                                               
TRN007   CLI   SR44TYPE,57                                                      
         BE    TRN008                                                           
         CLI   SR44TYPE,58                                                      
         BNE   TRN009                                                           
TRN008   OI    TRANSTAT,WRITEOFF                                                
         MVI   WROFFED,C'Y'                                                     
TRN009   CLI   QOPT2,C'H'          HELD ITEMS ONLY                              
         BNE   TRN010                                                           
         TM    SR44STAT,X'04'                                                   
         BZ    TRN045              REJECT                                       
TRN010   CLI   QOPT2,C'U'          SUPPRESS HELD ITEMS                          
         BNE   TRN020                                                           
         TM    SR44STAT,X'04'                                                   
         BO    TRN045              REJECT                                       
*                                                                               
TRN020   CLI   QOPT5,C'S'          SUPPRESS BILLED ITEMS                        
         BNE   TRN030                                                           
         OC    SR44USED,SR44USED   Q, FULLY BILLED                              
         BNZ   TRN045               Y, REJECT                                   
*                                                                               
TRN030   CLI   QOPT2,C'T'          TRANSACTION DATE FILTERING                   
         BE    TRN040                                                           
         CLI   QOPT2,C'H'                                                       
         BE    TRN040                                                           
         CLI   QOPT2,C'U'                                                       
         BNE   TRN041                                                           
TRN040   CLC   SR44DATE,PSTART                                                  
         BL    TRN045              REJECT                                       
         CLC   SR44DATE,PEND                                                    
         BH    TRN045                                                           
*                                                                               
TRN041   CLC   SR44MOS,MYMSTR      MOS FILTERING?                               
         BL    TRN045                                                           
         CLC   SR44MOS,MYMEND                                                   
         BH    TRN045                                                           
*                                                                               
         CLI   QREVERSE,C' '       REVERSAL STATUS ?                            
         BE    TRN050              NOT IMPORTANT                                
*                                                                               
*----------------------------------------------------------------------         
* NOOPED, 10/24/94 AS PER LRES/BURSON                                           
*----------------------------------------------------------------------         
*        CLI   QOPT2,C' '          FILTERING TRANS WITH OPTION 2                
*        BNE   TRN050              YES, INCLUDE ALL REVERSALS                   
*----------------------------------------------------------------------         
*                                                                               
         MVI   TRNISREV,C'N'       ASSUME TRN IS NOT REVERSED                   
         TM    SR44STAT,X'20'                                                   
         BNO   TRN041C             BIT NOT ON, NOT REVERSED                     
*                                                                               
         BAS   RE,CHKREVDT         CHECK REVERSAL DATE                          
         BNE   *+8                 NOT IN RANGE, NOT REVERSED                   
         MVI   TRNISREV,C'Y'                                                    
*                                                                               
TRN041C  CLI   QREVERSE,QREVNONE   FILTER OUT REVERSALS                         
         BNE   TRN050              NO, ACCEPT ALL                               
         CLI   TRNISREV,C'Y'       IS TRANSACTION REVERSED                      
         BNE   TRN050              NO, ACCEPT                                   
*                                                                               
TRN045   CLI   PROF18,C'Y'         WANT FULL W/C SUMM?                          
         BE    *+8                 YES, JUST SET STATUS                         
         B     REQX                NO, REJECT THIS TRAN                         
*                                                                               
         OI    TRANSTAT,REJECT                                                  
TRN050   MVC   SVANAL,SRTWC                                                     
*                                                                               
         USING TABLED,R4                                                        
         L     R4,ATABLE           A(TABLE) YOU ARE READING                     
         CLI   SUPPSW,C'Y'         THEN ONLY PRINT CODE 1ST TIME                
         BNE   TRN055                                                           
         LA    RF,1                                                             
         BAS   RE,SETAPRT                                                       
         MVC   0(14,RF),SRTCA      SUPPLIER                                     
TRN055   CLC   SVANAL,=C'99'                                                    
         BNE   TRN060                                                           
         CLI   SRTCA,C'3'          RETAIL BILLING?                              
         BE    TRN060              PRINT CONTRA ACCOUNT NOT ...                 
         LA    RF,1                                                             
         BAS   RE,SETAPRT                                                       
         MVC   0(15,RF),SR44NARR   BILLING TYPE                                 
         B     TRN080                                                           
*                                                                               
TRN060   CLI   SUPPNAME,C'N'       DO WE WANT SUPPLIER NAME                     
         BE    TRN91               NU                                           
TRN080   CLC   SVANAL,=C'99'       NO SUPPNAME FOR BILLING                      
         BE    TRN91                                                            
         CLI   SUPPSW,C'Y'         IS THIS THE FIRST TIME                       
         BNE   TRN91                                                            
         ZIC   RF,SUPPNAME+2                                                    
         BAS   RE,SETAPRT                                                       
         LA    R0,18                                                            
         CLI   SUPPNAME,C'U'       PRINT IT UNDER CODE                          
         BNE   TRN090              NO                                           
         LA    RF,L'P(RF)          YES, USE PSECOND                             
         LA    R0,15                                                            
         ST    RF,PRTADDR                                                       
TRN090   LR    R2,RF                                                            
         GOTO1 CHOPPER,DMCB,(36,VENDNAME),((R0),(R2)),(C'P',3)                  
*                                                                               
         USING SR44D,R2                                                         
TRN91    LA    R2,SRTDATA          PRINT 4C ACCOUNT                             
         CLI   SR44TYPE,49         IF ITS A TYPE 49                             
         BNE   TRN93                                                            
*                                                                               
         USING SR4CD,R2                                                         
         MVI   ELCODE,X'4C'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   TRN93                                                            
*                                                                               
         CLC   SR4CACCT,PREV4C     SAME AS PREV TRANSACTION                     
         BE    TRN93               YES, DON'T BOTHER PRINTING                   
*                                                                               
         LA    R0,10               NUM OF PRT LINES TO CHECK                    
         LA    RF,1               PRINT UNDER ACCOUNT                           
         BAS   RE,SETAPRT                                                       
TRN91A   CLC   0(18,RF),SPACES                                                  
         BNE   TRN92                                                            
         MVC   4(14,RF),SR4CACCT                                                
         MVC   PREV4C,SR4CACCT                                                  
         B     TRN93                                                            
*                                                                               
TRN92    LA    RF,L'P(RF)          CHECK NEXT 'P'                               
         BCT   R0,TRN91A           FALL THRU IF NO ROOM TO PRINT                
*                                                                               
TRN93    ZIC   RF,INVNUM+2         PRINT INVOICE NUMBER                         
         BAS   RE,SETAPRT                                                       
         MVC   0(6,RF),SRTNUM                                                   
*                                                                               
         ZIC   RF,INVDATE+2        PRINT INVOICE DATE                           
         BAS   RE,SETAPRT                                                       
         CLI   INVDATE,C'S'        STACK DATE UNDER NUMBER                      
         BNE   TRN095              NO                                           
         LA    RF,L'P(RF)          Y, USE PSECOND                               
TRN095   LR    R5,RF                                                            
         GOTO1 DATCON,DMCB,(1,SRTDATE),(8,(R5))                                 
*                                                                               
         MVI   ELCODE,X'23'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   TRN100                                                           
*                                                                               
         LR    RF,R5               RESTORE RF                                   
         USING SR23D,R2                                                         
         LA    RF,L'P(RF)          PRINT UNDER THE DATE                         
         MVC   0(6,RF),SR23NUM                                                  
         DROP  R2                                                               
*                                                                               
         USING SR44D,R2                                                         
TRN100   LA    R2,SRTDATA                                                       
         CLI   0(R2),X'68'       ORDERS                                         
         BNE   TRN110                                                           
         GOTO1 =A(ORDIT),DMCB,(RC)                                              
         B     TRN330                                                           
*                                                                               
TRN110   GOTO1 =A(SETTRN),DMCB,(RC)    SET TRANSACTION BUCKETS                  
*                                                                               
         CLI   HELD,C'Y'           FLAG HELD ITEMS                              
         BNE   TRN135              NO                                           
*                                                                               
         TM    SR44STAT,X'04'      IS THIS HELD?                                
         BNO   TRN135              NO                                           
         ZIC   RF,HELD+2                                                        
         BAS   RE,SETAPRT                                                       
         MVI   0(RF),C'*'                                                       
*                                                                               
TRN135   LA    R2,SRTDATA                                                       
         CLI   BTCH,C'Y'                                                        
         BNE   TRN140                                                           
         ZIC   RF,BTCH+2                                                        
         BAS   RE,SETAPRT                                                       
         MVC   0(6,RF),SR44BTCH                                                 
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PRINT HOURS/RATE, IF THEY HAVE A COL                                   
*----------------------------------------------------------------------         
*                                                                               
TRN140   LA    R2,SRTDATA                                                       
*                                                                               
         MVI   ELCODE,X'40'        LOOK FOR EL-40                               
         BAS   RE,NEXTEL           GET EL-40                                    
         BNE   TRN150              NONE                                         
*                                                                               
         USING TABLED,R4                                                        
         L     R4,ATABLE                                                        
         CLI   THOURS,C'Y'                                                      
         BNE   TRN140A                                                          
         ZIC   RF,THOURS+2                                                      
         BAS   RE,SETAPRT                                                       
         ZAP   DOUBLE,SVHOUR                                                    
         BAS   RE,PRTHRS                                                        
*                                                                               
TRN140A  CLI   TRATE,C'Y'                                                       
         BNE   TRN140B                                                          
         ZIC   RF,TRATE+2                                                       
         BAS   RE,SETAPRT                                                       
         B     TRN140C                                                          
*                                                                               
TRN140B  CLI   TRATE,C'S'                                                       
         BNE   TRN150                                                           
         ZIC   RF,THOURS+2                                                      
         BAS   RE,SETAPRT                                                       
         BAS   RE,BUMPAPRT                                                      
TRN140C  ZAP   DOUBLE,SVRATE                                                    
         BAS   RE,PR_RATE                                                       
*                                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*   UNIT/PRICE                                                                  
*---------------------------------------------------------------------*         
*                                                                               
TRN150   DS    0H                                                               
         CLI   UNITPRCE,C'Y'       ARE WE PRINTING THIS                         
         BNE   TRN160                                                           
*                                                                               
         LA    R2,SRTDATA                                                       
         MVI   ELCODE,X'7C'        IS THERE UNIT/PRICE DATA HERE                
         BAS   RE,NEXTEL           GET EL-40                                    
         BNE   TRN160              NONE                                         
*                                                                               
         ZIC   RF,UNITPRCE+2                                                    
         BAS   RE,SETAPRT                                                       
         ZAP   DOUBLE,SV7CUNIT                                                  
         BAS   RE,PRTUNIT          PRINT IT BASED ON STATUS                     
*                                                                               
         TM    SV7CSTAT,UNPSQTRH   QUARTER HOURS?                               
         BO    *+10                YES                                          
         MP    DOUBLE,=P'100'      NO, ADJUST FOR DECIMALS                      
         ZAP   SV7CUNIT,DOUBLE                                                  
*                                                                               
         BAS   RE,BUMPAPRT                                                      
         ZAP   DOUBLE,SV7CPRCE                                                  
         BAS   RE,PR_RATE                                                       
*                                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*   CHECK 'SUPRESS BILLED' OR 'BILLED ONLY' OPTIONS                             
*---------------------------------------------------------------------*         
*                                                                               
TRN160   DS    0H                                                               
         CLI   QOPT5,C' '                                                       
         BE    TRN170                                                           
         GOTO1 =A(PROCOP5)                                                      
         TM    TRANSTAT,REJECT     TRAN WAS REJECTED                            
         BNO   TRN170              NO                                           
*                                                                               
         CLI   PROF18,C'Y'         TRAN IS NOT WANTED, DO THEY WANT IT          
         BE    *+8                 IN THE SUMMARY                               
         B     TRN380              NO, QUICK EXIT                               
         EJECT ,                                                                
TRN170   CLI   PROF13,0            SHOW NET/GROSS AS LESS CD                    
         BE    FIG90                                                            
         CLI   PROF13,C'N'                                                      
         BE    FIG90                                                            
         SP    NETAMT,CDAMT                                                     
         CLI   PROF13,C'1'         SHOW NET ONLY AS LESS CD                     
         BE    FIG90                                                            
         SP    GRSAMT,CDAMT                                                     
         DROP  R2,R3,R4                                                         
         EJECT  ,                                                               
*---------------------------------------------------------------------*         
*        PRINT NET, CD, AND/OR GROSS, IF THEY ARE WANTED                        
*---------------------------------------------------------------------*         
*                                                                               
         USING TABLED,R4                                                        
*                                                                               
FIG90    L     R4,ATABLE           ADDRESS OF TABLE FOR THIS TRANS              
         CLI   NET,C'Y'            PRINT NET AMOUNT ?                           
         BNE   FIG100                                                           
         LA    R2,NET                                                           
         LA    R1,NETAMT                                                        
         LA    R5,NETSW                                                         
         BAS   RE,FIGOUT                                                        
*                                                                               
FIG100   CLI   COMM,C'Y'           PRINT COMMISSION ?                           
         BNE   FIG110                                                           
         LA    R2,COMM                                                          
         LA    R1,COMAMT                                                        
         LA    R5,COMSW                                                         
         BAS   RE,FIGOUT                                                        
*                                                                               
FIG110   CLI   GROSS,C'Y'          PRINT GROSS ?                                
         BNE   FIG130                                                           
         CLI   PROF44,C'S'         OPTION TO SUPRESS DOLLAR DETAIL              
         BNE   FIG120              NO                                           
         TM    PROCSTAT,TIMETRAN   ON TIME DETAIL ONLY                          
         BO    FIG130                                                           
*                                                                               
FIG120   LA    R2,GROSS                                                         
         LA    R1,GRSAMT                                                        
         LA    R5,GRSSW                                                         
         BAS   RE,FIGOUT                                                        
*                                                                               
         CLC   SVANAL,=C'99'       BILLING                                      
         BNE   FIG130                                                           
         BAS   RE,PRTTAX           PRINT ANY GST/PST ON BILL                    
*                                                                               
FIG130   CLI   DISCOUNT,C'Y'       PRINT CASH DISCOUNT ?                        
         BE    FIG140                                                           
*                                                                               
         CLI   DISCDEF,C'Y'        DEFAULT COLUMN                               
         BNE   BILL10                                                           
         CP    CDAMT,=P'0'                                                      
         BE    BILL10                                                           
         ZIC   RF,DISCDEF+2        PUT 'D' IN DEFAULT COLUMN                    
         BAS   RE,SETAPRT                                                       
         MVI   0(RF),C'D'                                                       
         B     BILL10                                                           
*                                                                               
FIG140   LA    R2,DISCOUNT                                                      
         LA    R1,CDAMT                                                         
         LA    R5,DISCSW                                                        
         BAS   RE,FIGOUT                                                        
         DROP  R4                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        PRINT BILLING INFO                                                     
*---------------------------------------------------------------------*         
*                                                                               
         USING SORTD,R3                                                         
*                                                                               
BILL10   TM    TRANSTAT,WRITEOFF   TYPE 57,58                                   
         BO    BILL85A             YES, NO BILLED INFO                          
         CLI   BILLED,C'Y'         Q, FULLY BILLED                              
         BE    BILL20                                                           
*                                                                               
         OI    JOBSTAT,NOTFULL     SET STATUS THAT THERE IS NO NEED             
         OI    TYPESTAT,NOTFULL    TO ADJUST THE BAL OF THIS JOB                
*                                                                               
         CLC   SVANAL,=C'99'       BILLING                                      
         BE    BILL20              YES, BILLABLE IS MOOT                        
*                                                                               
*                                  *------------------------------*             
*                                  * BILLABLE AMOUNT              *             
*                                  *------------------------------*             
         USING TABLED,R4                                                        
         L     R4,ATABLE                                                        
         CLI   BILLABLE,C'Y'       BILLABLE COLUMN                              
         BNE   BILL12                                                           
         ZIC   RF,BILLABLE+2       BILLABLE AMOUNT                              
         BAS   RE,SETAPRT                                                       
         SH    RF,=H'2'            ADJUST R5 FOR THE EDIT                       
         ZAP   DOUBLE,DUBABLE      PRINT BILLABLE                               
         BZ    BILL20                                                           
         ST    RF,PRTADDR                                                       
         BAS   RE,DOUBEDIT                                                      
         MVC   BLLABLSW(2),=C'YY'  TURN ON PRINT SWITCHES                       
*                                                                               
*                                  *------------------------------*             
*                                  * BILLABLE HOURS               *             
*                                  *------------------------------*             
*                                                                               
BILL12   CLI   TBLABLHR,C'Y'                                                    
         BNE   BILL20                                                           
         ZAP   DOUBLE,SVHOUR                                                    
         SP    DOUBLE,BLDHRS                                                    
         SP    DOUBLE,WOHRS                                                     
         ZIC   RF,TBLABLHR+2                                                    
         BAS   RE,SETAPRT                                                       
         BAS   RE,PRTHRS                                                        
*                                                                               
BILL20   ZAP   WOAMNT,=P'0'        DONT NEED THIS ONCE I PRINTED B'ABLE         
         ZAP   WOHRS,=P'0'         SINCE IT IS CARRIED ON THE TYPE57'S          
*                                                                               
         CLI   BILLED,C'N'         Q, NOT BILLED                                
         BNE   BILL20A              Y, LEAVE THIS ROUTINE                       
         CP    EL4BCNT,=P'1'       ARE THERE 4B'S THOUGH                        
         BH    BILL30              YES, PRINTEM                                 
         B     BILL85A                                                          
* -------------------------------------------------------------------           
*        NOTE 4/93, DEPEND ON 4B DATA FOR ALL BILLING INFO                      
* -------------------------------------------------------------------           
*                                                                               
BILL20A  DS    0H                                                               
*                                                                               
*        CLI   BILLED,C'P'         IF PARTIALLY BILLED, KEEP BLDAMT             
*        BE    BILL30              AS SUM OF 4B'S                               
*        ZAP   BLDAMT,GRSAMT       ELSE TAKE GROSS AMOUNT FROM FIGIT            
*        CLI   PROF13,C'Y'         DON'T IF GROSS ALREADY LESS CD               
*        BE    BILL30                                                           
*        SP    BLDAMT,CDAMT        GET BILLED AMT  (GROSS-CD)                   
*                                                                               
         USING SR44D,R2                                                         
BILL30   LA    R2,SRTDATA                                                       
         MVC   BILLSW(2),=C'YY'    TURN ON PRINT SWITCHES                       
         CLC   SVANAL,=C'99'                                                    
         BNE   BILL40                                                           
         ZAP   BLDAMT,SR44NARR+27(6) SAVE PAYABLE AMOUNT FOR W/C 99             
*                                                                               
BILL40   MVC   BILNO,=C'  **    '  PREP BILL DATE, BILL NUMBER                  
         MVC   BILDTE,=C'   **   '                                              
         CLC   SVANAL,=C'99'       NOT FOR ACTUAL BILLING                       
         BE    BILL45                                                           
         CP    EL4BCNT,=P'1'       MULTIPLE BILLING 4B'S?                       
         BH    BILL45              YES, PRINT DETAILS LATER                     
*                                                                               
         USING SR4BD,R2                                                         
         MVI   ELCODE,X'4B'                                                     
         LA    R2,SRTDATA                                                       
BILL42   BAS   RE,NEXTEL           GET 4B ELEMENT                               
         BNE   BILL45                                                           
         CLI   SR4BNO,C'W'         WRITE OFF 4B                                 
         BE    BILL42              YES, GET NEXT                                
         CLC   SR4BNO,SPACES                                                    
         BNH   BILL42              GET NEXT                                     
*                                                                               
         MVC   BILNO,SR4BNO                                                     
         MVC   DOUBLE,SR4BDTE                                                   
         LA    RF,BILDTE                                                        
         ST    RF,PRTADDR                                                       
         BAS   RE,PRT2DTE                                                       
*                                                                               
BILL45   DS    0H                                                               
         USING TABLED,R4                                                        
BILL50   L     R4,ATABLE                                                        
*                                                                               
         CLI   QOPT5,C'S'          SUPRESS BILLED                               
         BE    NAR000              YESA, DON'T PRINT BILLED/W/O INFO            
         CLI   QOPT5,C'U'          UNALLOCATED ONLY                             
         BE    NAR000                                                           
         CLI   QOPT5,C'A'          ALLOCEATE ONLY                               
         BE    NAR000                                                           
*                                                                               
*                                  *------------------------------*             
*                                  * BILLED AMOUNT                *             
*                                  *------------------------------*             
*                                                                               
         CLI   BILLAMT,C'Y'                                                     
         BNE   BILL60                                                           
         CP    BLDAMT,=P'0'                                                     
         BE    BILL60                                                           
         ZIC   RF,BILLAMT+2                                                     
         BAS   RE,SETAPRT                                                       
         ZAP   DOUBLE,BLDAMT                                                    
         ST    RF,PRTADDR                                                       
         BAS   RE,DOUBEDIT                                                      
*                                                                               
*                                  *------------------------------*             
*                                  * BILL NUMBER                  *             
*                                  *------------------------------*             
*                                                                               
BILL60   CLI   BILLNUM,C'Y'                                                     
         BNE   BILL70                                                           
         ZIC   RF,BILLNUM+2                                                     
         BAS   RE,SETAPRT                                                       
         MVC   0(6,RF),BILNO                                                    
*                                                                               
*                                  *------------------------------*             
*                                  * BILL DATE                    *             
*                                  *------------------------------*             
*                                                                               
BILL70   CLI   BILLDTE,C'Y'                                                     
         BNE   BILL80                                                           
         ZIC   RF,BILLDTE+2                                                     
         BAS   RE,SETAPRT                                                       
         MVC   0(8,RF),BILDTE                                                   
*                                                                               
*                                  *------------------------------*             
*                                  * PRINT "B" IF BILLED          *             
*                                  *------------------------------*             
*                                                                               
BILL80   CLI   BILLDEF,C'Y'                                                     
         BNE   BILL85                                                           
         CLI   BILLED,C'Y'                                                      
         BNE   BILL85                                                           
         ZIC   RF,BILLDEF+2                                                     
         BAS   RE,SETAPRT                                                       
         MVI   0(RF),C'B'                                                       
*                                                                               
*                                  *------------------------------*             
*                                  * BILLED HOURS                 *             
*                                  *------------------------------*             
*                                                                               
BILL85   CLI   TBLDHRS,C'Y'                                                     
         BNE   BILL85A                                                          
         ZIC   RF,TBLDHRS+2                                                     
         BAS   RE,SETAPRT                                                       
         ZAP   DOUBLE,BLDHRS                                                    
         BAS   RE,PRTHRS                                                        
*                                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        PRINT WRITE OFF INFO, IF WANTED                                        
*              TYPE 57 TRANSACTIONS CARRY ALL THIS INFO                         
*              THE TRAN WRITTEN OFF WILL ONLY HAVE THE WRITE OFF NUMB           
*---------------------------------------------------------------------*         
*                                                                               
BILL85A  TM    PROCSTAT,TIMETRAN   THIS IS A TIME TRANSACTION?                  
         BNO   NAR000              NO                                           
         CP    WOCNT,=P'0'                                                      
         BE    NAR000              NONE AT ALL,                                 
*                                                                               
         CLI   TWONUM,C'N'         PRINT WRITE OFF NUMBER                       
         BE    BILL90              NO                                           
         ZIC   RF,TWONUM+2         PRINT DISPLACEMENT                           
         BAS   RE,SETAPRT                                                       
         CLI   TWONUM,C'Y'         PRINT WRITE OFF NUMBER IN A COL              
         BE    BILL88              YES, ELSE I'M STACKING W/O NUM               
*                                                                               
BILL86   CLC   0(6,RF),SPACES      ANYTHING IN BILL NUMBER FIELD                
         BE    BILL88              NO                                           
         LA    RF,L'P(RF)          GET FREE BILL NUMBER CELL                    
         B     BILL86                                                           
*                                                                               
BILL88   MVC   0(6,RF),WONO                                                     
*                                                                               
BILL90   CLI   TWODATE,C'Y'        PRINT WRITE OFF DATE                         
         BNE   BILL95              NO                                           
         ZIC   RF,TWODATE+2        PRINT DISPLACEMENT                           
         BAS   RE,SETAPRT         CALC PRINT ADRESS                             
         MVC   DOUBLE(L'WODATE),WODATE                                          
         BAS   RE,PRT2DTE          PRINT THE COMPRESSED DATE                    
*                                                                               
BILL95   CLI   TWOHOURS,C'Y'       PRINT WRITE OFF HOURS                        
         BNE   BILL100             NO                                           
         ZIC   RF,TWOHOURS+2       PRINT DISPLACEMENT                           
         BAS   RE,SETAPRT          CALC PRINT ADRESS                            
         ZAP   DOUBLE,WOHRS                                                     
         BAS   RE,PRTHRS                                                        
*                                                                               
BILL100  CLI   TWOAMNT,C'Y'                                                     
         BNE   NAR000                                                           
         ZAP   DOUBLE,WOAMNT                                                    
         ZIC   RF,TWOAMNT+2                                                     
         BAS   RE,SETAPRT                                                       
         ST    RF,PRTADDR                                                       
         BAS   RE,DOUBEDIT                                                      
*                                                                               
         DROP  R2,R3,R4                                                         
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*   PRINT NARRATIVE OR WHAT THE USER WANTS TO SEE IN THE NARRATIVE    *         
*---------------------------------------------------------------------*         
*                                                                               
         USING TABLED,R1                                                        
         USING SORTD,R3                                                         
*                                                                               
NAR000   L     R1,ATABLE                                                        
         LA    R4,AREA             R4=A(OUTPUT AREA)                            
         MVI   AREA,C' '                                                        
         MVC   AREA+1(L'AREA-1),AREA                                            
         ZAP   ALOCHRS,=P'0'                                                    
         ZAP   ALOCAMNT,=P'0'                                                   
         CLI   NARR,C'Y'           DONT PRINT NARR?                             
         BNE   TRN187              JUST PRINT IN COLS                           
*                                                                               
         CLC   SVANAL,=C'99'       BILLING ?                                    
         BNE   NAR009                                                           
*                                                                               
         USING SRGSD,R2                                                         
         LA    R2,SRTDATA          SEE IF THERE IS GST ON THE BILL              
         MVI   ELCODE,VBIELQ                                                    
         BAS   RE,NEXTEL                                                        
         BNE   NAR005                                                           
         MVC   0(4,R4),=C'GST:'                                                 
         LA    R4,4(R4)                                                         
*                                                                               
NAR001   MVC   0(1,R4),SRGSTYPE                                                 
         MVI   1(R4),C'='                                                       
*                                                                               
         TM    SRGSINDS,VBIIDC3                                                 
         BZ    NAR003                                                           
         EDIT  (2,SRGSRATE),(6,2(R4)),3,FILL=0,TRAIL=C'%'                       
         B     NAR004                                                           
*                                                                               
NAR003   EDIT  (2,SRGSRATE),(6,2(R4)),2,FILL=0,TRAIL=C'%'                       
*                                                                               
NAR004   MVI   8(R4),C'/'                                                       
         EDIT  (P6,SRGSAMNT),(12,9(R4)),2,MINUS=YES,ALIGN=LEFT                  
         AR    R4,R0                                                            
         LA    R4,8(R4)                                                         
         CLI   0(R4),C'-'          TRAILING MINUS AROUND                        
         BNE   *+8                 NO                                           
         LA    R4,1(R4)            YES BUMP PAST IT                             
*                                                                               
         MVI   0(R4),C','                                                       
         LA    R4,2(R4)                                                         
         BAS   RE,NEXTEL                                                        
         BE    NAR001                                                           
*                                                                               
         USING SRPSD,R2                                                         
NAR005   LA    R2,SRTDATA          SEE IF THERE IS PST ON THE BILL              
         MVI   ELCODE,PBIELQ                                                    
         BAS   RE,NEXTEL                                                        
         BNE   NAR200                                                           
*                                                                               
NAR006   MVC   0(6,R4),SRPSNAME                                                 
         MVI   6(R4),C':'                                                       
         LA    R4,7(R4)                                                         
         MVC   0(1,R4),SRPSTYPE                                                 
         MVI   1(R4),C'='                                                       
*                                                                               
         TM    SRPSINDS,PBIIDC3                                                 
         BZ    NAR007                                                           
         EDIT  (2,SRPSRATE),(6,2(R4)),3,FILL=0,TRAIL=C'%'                       
         B     NAR008                                                           
*                                                                               
NAR007   EDIT  (2,SRPSRATE),(6,2(R4)),2,FILL=0,TRAIL=C'%'                       
*                                                                               
NAR008   MVI   8(R4),C'/'                                                       
         EDIT  (P6,SRPSAMNT),(12,9(R4)),2,MINUS=YES,ALIGN=LEFT                  
         AR    R4,R0                                                            
         LA    R4,8(R4)                                                         
         CLI   0(R4),C'-'          TRAILING MINUS AROUND                        
         BNE   *+8                 NO                                           
         LA    R4,1(R4)            YES BUMP PAST IT                             
*                                                                               
         MVI   0(R4),C','                                                       
         LA    R4,2(R4)                                                         
         BAS   RE,NEXTEL                                                        
         BE    NAR006                                                           
         B     NAR200              CLEAR TRAILING COMMA                         
*                                                                               
NAR009   LA    R2,SRTDATA                                                       
         MVI   ELCODE,X'4B'        ALLOCATED AMOUNT                             
         USING SR4BD,R2                                                         
NAR010   BAS   RE,NEXTEL                                                        
         BNE   NAR060                                                           
         CLC   SR4BNO,SPACES       IN ALLOC. EL. W/ BILL NUM=SPACES             
         BNE   NAR010                                                           
         CP    SR4BAMNT,=P'0'      ONLY IF SOMETHING THERE                      
         BNE   NAR020                                                           
*                                                                               
         CP    SVRATE,=P'0'        ALLOCATED AMOUNT ZERO, IF RATE ALSO          
         BNE   NAR060               ZERO, PRINT ALLOCATED HOURS                 
         CP    SR4BHRS,=P'0'                                                    
         BE    NAR060                                                           
         CLI   THOURS,C'Y'         SPECIAL HOURS COL?                           
         BE    NAR020                                                           
*                                                                               
         MVC   0(2,R4),=C'H='                                                   
         EDIT  (P5,SR4BHRS),(12,2(R4)),2,MINUS=YES,ALIGN=LEFT                   
         ZAP   ALOCHRS,SR4BHRS                                                  
         MVC   HRSSW,=C'YY'                                                     
         B     NAR030                                                           
*                                                                               
NAR020   MVC   0(2,R4),=C'A='                                                   
         EDIT  (P5,SR4BAMNT),(12,2(R4)),2,MINUS=YES,ALIGN=LEFT                  
         ZAP   ALOCAMNT,SR4BAMNT                                                
NAR030   MVC   NARRSW,=C'YY'                                                    
         AR    R4,R0                                                            
         BCTR  R4,0                                                             
         CLC   0(3,R4),=C'.00'     DON'T PRINT ZERO FRACTIONS                   
         BNE   NAR040                                                           
         MVC   0(3,R4),SPACES                                                   
         B     NAR050                                                           
*                                                                               
NAR040   LA    R4,3(R4)                                                         
NAR050   MVI   0(R4),C','                                                       
         LA    R4,2(R4)                                                         
NAR060   BAS   RE,TIMEOUT          PRINT TOTAL HOURS AND RATE IN NARR           
         LA    R2,SRTDATA                                                       
         MVI   ELCODE,X'4E'        TRANSFERS                                    
         BAS   RE,NEXTEL                                                        
         BNE   NAR070                                                           
         USING SR4ED,R2                                                         
         MVC   0(2,R4),=C'T/'                                                   
         MVC   2(1,R4),SR4ETYPE    TO OR FROM                                   
         MVC   4(12,R4),SR4EACC+2                                               
         GOTO1 DATCON,DMCB,(1,SR4EDATE),(8,17(R4))                              
         MVI   25(R4),C','                                                      
         LA    R4,26(R4)                                                        
NAR070   LA    R2,SRTDATA                                                       
         MVI   ELCODE,X'25'                                                     
         USING SR25D,R2                                                         
NAR080   BAS   RE,NEXTEL                                                        
         BNE   NAR090                                                           
         MVC   0(4,R4),=C'ORD='                                                 
         MVC   4(6,R4),SR25NO                                                   
         MVI   10(R4),C','                                                      
         LA    R4,12(0,R4)                                                      
         B     NAR080                                                           
*                                                                               
NAR090   TM    XJOBSTAT,XJOB                                                    
         BNO   NAR120                                                           
         USING APEELD,R2                                                        
         LA    R2,SRTDATA                                                       
         MVI   ELCODE,APEELQ                                                    
         BAS   RE,NEXTEL                                                        
         BNE   NAR120                                                           
*                                                                               
         ZIC   R0,APENUM           NUMBER OF MINI ELEMENTS                      
         MVC   0(4,R4),=C'A/P='                                                 
         XR    R1,R1                                                            
         LA    R4,4(R4)                                                         
         LA    R2,APENTRY                                                       
*                                                                               
NAR100   IC    R1,APENLEN-APENTRY(R2)                                           
         SH    R1,=H'3'            GET ACCOUNT LENGTH -1                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),2(R2)                                                    
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         LA    R2,3(R1,R2)         BUMP R2 L'AP+1+APLEN+APSTAT                  
         BCT   R0,NAR100                                                        
*                                                                               
NAR120   L     R1,ATABLE                                                        
         CLI   RATE,C'Y'           DID I ALREADY PRINT HOURS/RATE               
         BE    NAR150              YES                                          
         CLI   THOURS,C'A'         PRINT HOUR IN NARR                           
         BNE   NAR140              NO                                           
         BAS   RE,HOUREDIT                                                      
         AR    R4,R0                                                            
         MVC   1(3,R4),=C'HRS'                                                  
         MVI   4(R4),C','                                                       
         LA    R4,6(R4)                                                         
*                                                                               
NAR140   CLI   TRATE,C'A'                                                       
         BNE   NAR150                                                           
         SH    R4,=H'7'                                                         
         BAS   RE,RATEEDIT                                                      
         AR    R4,R0                                                            
         LA    R4,8(R4)                                                         
         MVC   0(3,R4),=C'/HR'                                                  
         LA    R4,4(R4)                                                         
*                                                                               
NAR150   LA    R2,SRTDATA          RESTORE TO 44 ELEMENT                        
         USING SR44D,R2                                                         
         ZIC   RE,SR44LEN          DEDUCE LENGTH OF NARRATIVE-1                 
         SH    RE,=Y(SR44LN1+1)                                                 
         BNP   NAR200                                                           
         STC   RE,NAR190+1                                                      
*                                                                               
         LA    RF,AREA+L'AREA      SEE IF I HAVE ENOUGH ROOM LEFT               
         SR    RF,R4               IN AREA                                      
         CR    RE,RF                                                            
         BL    NAR190                                                           
         BCTR  RF,0                                                             
         STC   RF,NAR190+1                                                      
*                                                                               
NAR190   MVC   0(0,R4),SR44NARR                                                 
         AR    R4,RE                                                            
NAR200   BAS   RE,COMCLR           CLEAR TRAILING COMMA IF AROUND               
*                                                                               
         L     R1,ATABLE                                                        
         ZIC   RF,NARR+2           CHOP NARRATIVE TO P-LINES                    
         BAS   RE,SETAPRT                                                       
         ZIC   R4,NARR+1           WIDTH OF NARR COL                            
         LR    R2,RF                                                            
         GOTO1 CHOPPER,DMCB,(240,AREA),((R4),(R2)),(C'P',20)                    
NARX     DS    0H                                                               
         DROP  R1,R2,R3                                                         
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*   ADD TO TIME SUMMARY                                                         
*---------------------------------------------------------------------*         
*                                                                               
         USING TIMED,R1                                                         
         USING SR44D,R2                                                         
         USING SORTD,R3                                                         
*                                                                               
TRN187   DS    0H                                                               
         LA    R2,SRTDATA                                                       
*                                                                               
         CLC   SRTCA(2),=C'1R'  TIME TRANSACTION                                
         BE    TRN187A                                                          
         CLI   WCP_TIME,C'Y'       CONSIDER THIS WC TIME ON MP SUM?             
         BE    TRN187A             YES                                          
         CLI   PROF23,C'Y'                                                      
         BNE   TRNSUBT             NO                                           
         CLC   SRTCA(2),=C'SK' IS THIS A TIME CHARGE?                           
         BNE   TRNSUBT                                                          
*                                                                               
TRN187A  LA    R1,BILDAREA                                                      
         XC    BILDAREA,BILDAREA   INIT. RECORD                                 
         MVC   TIMEACCS,PZEROS                                                  
         MVI   TIMETYPE,C'2'                                                    
         MVC   TIMEACCT(14),SRTCA  ACCOUNT                                      
         MVC   TIMEREF,SRTNUM      INVOICE NUMBER                               
         MVC   TIMENAME,SPACES     ACCOUNT NAME                                 
         MVC   TIMENAME(36),VENDNAME                                            
         MVC   TIMEDATE,SR44DATE   INVOICE DATE                                 
*                                                                               
         CLI   SR44TYPE,48         TYPE 48 (AUTO REVERSE)                       
         BNE   *+8                                                              
         MVI   TIMETY48,X'FF'                                                   
*                                                                               
         CLI   PROF18,C'Y'                                                      
         BNE   TRN187E                                                          
         TM    TRANSTAT,PART       IS THIS A PARTIAL TRAN                       
         BZ    TRN187E             NO                                           
         USING TRNSAVED,R3                                                      
         L     R3,TRANSAVE         YES, USE FULL AMOUNTS HERE                   
         ZAP   TIMEGRS,TRSVGRS                                                  
         ZAP   TIMEBILL,TRSVBLD    BILLED FROM                                  
         ZAP   TIMEUNBL,TRSVGRS    UNBILLED CHARGES = GROSS                     
         SP    TIMEUNBL,TIMEBILL   LESS BILLED                                  
         ZAP   TIMEHOUR,TRSVHRS                                                 
         ZAP   TIMERATE,SVRATE                                                  
         B     TRN187F                                                          
*                                                                               
TRN187E  ZAP   TIMEGRS,GRSAMT      GROSS FROM FIGIT                             
         ZAP   TIMEBILL,BLDAMT     BILLED FROM BILLIT2                          
         ZAP   TIMEUNBL,TIMEGRS    UNBILLED CHARGES = GROSS                     
         SP    TIMEUNBL,TIMEBILL   LESS BILLED                                  
         ZAP   TIMEHOUR,SVHOUR                                                  
         ZAP   TIMERATE,SVRATE                                                  
         ZAP   TIMEWOAM,WOAMNT                                                  
         ZAP   TIMEWOHR,WOHRS                                                   
         DROP  R1                                                               
*                                                                               
TRN187F  GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BILDAREA                             
*                                                                               
         USING SORTD,R3                                                         
         L     R3,SORTAREA                                                      
         CLC   SRTCA(2),=C'1R'  CA OF 1R                                        
         BNE   TRNSUBT          NO                                              
         USING TIMED,R2                                                         
         LA    R2,BILDAREA                                                      
         XC    TIMETY48(TIMENAME-TIMETY48),TIMETY48 CLEAR DATA                  
         MVC   TIMENAME,SPACES                                                  
*                                                                               
         GOTO1 =A(SETMPTOT),DMCB,TIMEACCT,EMPLEVB,(RC) DEPT TOTALS              
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BILDAREA                             
*                                                                               
         GOTO1 =A(SETMPTOT),DMCB,TIMEACCT,EMPLEVA,(RC) OFFICE TOTALS            
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BILDAREA                             
         DROP  R2,R3                                                            
*                                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*   ADD TO SUBTOTAL ACCUMULATORS                                                
*        BUILD W/C SUB TOTALS                                                   
*        IF FULL SUMMARIES, SAVE REJECTED/ADJUSTED AMOUNTS                      
*---------------------------------------------------------------------*         
*                                                                               
TRNSUBT  TM    PROCSTAT,FINLBILL   IS THIS BILLING DETAIL RECORD                
         BO    TRN272              ALLREADY IN TABLE THEN                       
         TM    TRANSTAT,REJECT     REJECT                                       
         BO    TRNSUB5             SAVE IT AS SUCH                              
         LA    R2,BUFACCS4         ADD,NET,COM,GROSS,CD TO BUFFACCS4-7          
         LA    R1,5                ADD TO FIVE BUFFALO ACCUMULATORS             
         LA    R3,NETAMT           BUFACCS8 GETS BILLED AMOUNT                  
         BAS   RE,ADDEM                                                         
*                                                                               
         AP    BUFACCSB,ALOCHRS                                                 
         AP    BUFACCSA,ALOCAMNT                                                
         USING SORTD,R3                                                         
         L     R3,SORTAREA                                                      
         CLI   SRTTYPE,SRTTTIME    IS THE CLIENT SEPERATING TIME CHARGE         
         BNE   TRNSUB5                                                          
         USING TIMSUBD,R1                                                       
         L     R1,TIMESUB         TIME SUBTOTALS BUCKETS                        
         AP    TSHRS,SVHOUR       TOTAL HOURS                                   
         AP    TSBLDHRS,BLDHRS    BILLED HOURS                                  
         AP    TSWOHRS,WOHRS                                                    
         AP    TSWOAMT,WOAMNT                                                   
         DROP R1                                                                
TRNSUB5  L     R1,AUNITS           BUMP UNIT ACCUM                              
         LA    R0,3                                                             
         AP    0(8,R1),SV7CUNIT                                                 
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         CLI   PROF18,C'Y'         FULL WC SUMMARY                              
         BNE   TRN272              NO, IF YOU MADE IT HERE,YOUR OK              
         TM    TRANSTAT,REJECT+PART   TRAN REJECTED OR ADJUSTED                 
         BZ    TRN272              NO,                                          
*                                                                               
         LA    R1,BILDAREA         YES SAVE IT IN BUFFALO                       
         MVC   0(L'BUFKEY,R1),SPACES                                            
         MVI   BUFTYPE-BUFKEY(R1),C'3'   REJECTS ARE TYPE THREE                 
         MVC   BUFANAL-BUFKEY(L'BUFANAL,R1),SVANAL     WORKCODE                 
         MVC   BUFOTH-BUFREC(L'BUFOTH,R1),SPACES                                
         MVC   BUFNAME-BUFKEY(L'BUFNAME,R1),WNAME  NAME                         
*                                                                               
         CLI   PROF33,C'Y'        SEPERATE TIME AND OOP                         
         BNE   TRNSUB6                                                          
*                                                                               
         MVC   BUFWTYPE-BUFKEY(L'BUFWTYPE,R1),WTYPE    WORKCODE TYPE            
*                                                                               
         CLC   BUFANAL,=C'99'      IS THIS A BILLING RECORD                     
         BE    TRNSUB6             YES, DON'T INCLUDE IN STATUS                 
*                                                                               
         OC    WCTPSTAT,WTYPE      SAVE W/C TYPES ON JOB                        
*                                                                               
TRNSUB6  MVC   BUFACCS-BUFREC(L'BUFACCS,R1),PZEROS                              
         TM    TRANSTAT,PART       IS THIS A PARTIAL                            
         BNO   TRNSUB7                                                          
*                                                                               
         MVI   BYTE,C'S'           SAVE ONLY AMOUNT SUPRESSED                   
         GOTO1 =A(SAVEAMNT)                                                     
         LA    R1,BILDAREA         RESTORE R1                                   
         L     R3,TRANSAVE         REMAINDER OF PARTIALS                        
         B     TRNSUB10                                                         
*                                                                               
TRNSUB7  AP    BUFACCSB-BUFREC(8,R1),ALOCHRS                                    
         AP    BUFACCSA-BUFREC(8,R1),ALOCAMNT                                   
         LA    R3,NETAMT           BUFACCS8 GETS BILLED AMOUNT                  
TRNSUB10 LA    R2,BUFACCS4-BUFREC(R1)                                           
         LA    R1,5                ADD TO FIVE BUFFALO ACCUMULATORS             
         BAS   RE,ADDEM                                                         
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BILDAREA                             
         MVI   JOBACTV,C'Y'        CONSIDER THIS JOB ACTIVITY                   
         TM    TRANSTAT,REJECT     IS THIS A REJECT                             
         BO    TRN380              QUICK EXIT                                   
         DROP  R3                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PRINT BILLING AND/OR WRITE OFF DETAILS (4B'S OR '4F'S')                
*----------------------------------------------------------------------         
*                                                                               
TRN272   CLC   SVANAL,=C'99'       BILLING ?                                    
         BE    TRN330                                                           
         CLI   QOPT5,C'S'          Q, SUPPRESSING BILLED INFO                   
         BE    TRN330               Y, SUPRESS BILLING DETAILS                  
         CLI   QOPT5,C'A'                                                       
         BE    TRN330                                                           
         CLI   QOPT5,C'U'                                                       
         BE    TRN330                                                           
*                                                                               
         USING TABLED,R1                                                        
         L     R1,ATABLE           SET R5 TO THE FIRST AVAILABLE LINE           
*                                                                               
         L     R5,PRTBUFF                                                       
         CP    EL4BCNT,=P'1'       MULTIPLE BILLS                               
         BNH   *+8                 NO                                           
         LA    R5,L'P(R5)          SKIP THE BILL NO **                          
*                                                                               
         CLI   TWONUM,C'S'         STACKING W/O NUMBERS                         
         BNE   TRN275                                                           
*                                                                               
         CP    WOCNT,=P'0'         ANY WO'S                                     
         BE    *+8                 NO                                           
         LA    R5,L'P(R5)          SKIP THE WONO ALSO (** OR WONNNN)            
*                                                                               
         USING TABLED,R1                                                        
TRN275   DS    0H                                                               
         USING SORTD,R3                                                         
         L     R3,SORTAREA                                                      
         LA    R2,SRTDATA                                                       
         MVI   ELCODE,X'4B'                                                     
*                                                                               
         USING SR4BD,R2            (LINE1 BILLNO IS **)                         
TRN280   BAS   RE,NEXTEL           PROCESS 2ND,3RD,ETC---IF ANY                 
         BNE   PARTXA                                                           
*                                                                               
TRN295   CLC   SR4BNO,SPACES                                                    
         BNH   TRN280                                                           
         CLI   SR4BNO,C'W'         IS THIS A WRITE OFF 4B                       
         BNE   TRN300              NO, PRINT THE BILLING DATA                   
*                                                                               
         CLI   TWONUM,C'S'         STACKING W/O NUMBERS                         
         BNE   TRN280              NO, PRINT WONO'S LATER                       
*                                                                               
         CP    WOCNT,=P'1'         DID I ALREAY PRINT THIS                      
         BNH   TRN280              YES                                          
*                                                                               
         ZIC   RF,TWONUM+2                                                      
         BAS   RE,SETAP_R5                                                      
*                                                                               
TRN295C  BAS   RE,PRTNO                                                         
         B     PARTX                                                            
*                                  PRINT BILLING DATA                           
*                                                                               
TRN300   CLI   NARR,C'Y'           ANY NARRATIVE?                               
         BNE   TRN320                                                           
         CLI   RATE,C'N'           PRINT THE RATE?                              
         BE    TRN320              NO                                           
         CP    SR4BHRS,=P'0'       HOURS ON THIS 4B                             
         BE    TRN320              NO                                           
         CP    SV7CPRCE,=P'0'                                                   
         BE    TRN320              NO                                           
*                                                                               
         ZAP   SVHOUR,SR4BHRS                                                   
*        ZAP   SVRATE,SR4BRATE                                                  
         ZAP   SVRATE,SV7CPRCE                                                  
         BZ    TRN320                                                           
         ZIC   R4,NARR+2                                                        
         LA    R4,0(R4,R5)                                                      
         CP    EL4BCNT,=P'1'       ONLY ONE BILL HERE                           
         BH    TRN312              NO                                           
*                                                                               
         ZIC   RF,NARR+2           IF ONE BILL, USE TOP NAR ROW                 
         BAS   RE,SETAPRT                                                       
         LR    R4,RF                                                            
*                                                                               
TRN312   CLC   0(10,R4),SPACES     FIND SPACES IN NARRATIVE COL                 
         BE    TRN315                                                           
         LA    R4,L'P(R4)                                                       
         B     TRN312                                                           
*                                                                               
TRN315   BAS   RE,TIMEOUT                                                       
*                                                                               
TRN320   DS    0H                  PRINT PARTIAL BILLINGS                       
         CP    EL4BCNT,=P'1'       DID I ALREAY PRINT THIS                      
         BNH   TRN280              YES                                          
*                                                                               
         CLI   BILLAMT,C'Y'                                                     
         BNE   PART3                                                            
         ZAP   DOUBLE,SR4BAMNT                                                  
         ZAP   PL16,SR4BAMNT       CALCULATE COMMISSION                         
         AP    PL16,SR4BCSD        ADD CD TO NET                                
         MP    PL16,SR4BRATE                                                    
         SRP   PL16,64-6,5                                                      
         ZAP   DUB,PL16           SAVE COMMISION IN DUB                         
         CLI   PROF13,C'Y'         UNLESS ALREADY PRINTING '-CD'                
         BE    PART2                                                            
         AP    DOUBLE,SR4BCSD                                                   
PART2    AP    DOUBLE,DUB          ADD COMMISSION AMOUNT                        
         ZIC   RF,BILLAMT+2        BILLED AMOUNT                                
         BAS   RE,SETAP_R5                                                      
         BAS   RE,PRTAMNT                                                       
*                                                                               
PART3    CLI   BILLNUM,C'Y'                                                     
         BNE   PART4                                                            
         ZIC   RF,BILLNUM+2        BILL NUMBER                                  
         BAS   RE,SETAP_R5                                                      
         BAS   RE,PRTNO                                                         
*                                                                               
PART4    CLI   BILLDTE,C'Y'                                                     
         BNE   PART4A                                                           
         ZIC   RF,BILLDTE+2        BILL DATE                                    
         BAS   RE,SETAP_R5                                                      
         MVC   DOUBLE(L'SR4BDTE),SR4BDTE                                        
         BAS   RE,PRT2DTE                                                       
*                                                                               
PART4A   CLI   TBLDHRS,C'Y'                                                     
         BNE   PARTX                                                            
         ZIC   RF,TBLDHRS+2                                                     
         BAS   RE,SETAP_R5                                                      
         ZAP   DOUBLE,SR4BHRS                                                   
         BAS   RE,PRTHRS                                                        
         B     PARTX                                                            
*                                                                               
PARTX    LA    R5,L'P(R5)                                                       
         CP    EL4BCNT,=P'0'       GET NXT 4B ?                                 
         B     TRN280              YES                                          
*                                                                               
PARTXA   CLI   TWONUM,C'Y'         WRITE OFF NUMBER, SEPERATE COL               
         BNE   TRN330              NOT WANTED                                   
*                                                                               
         CP    WOCNT,=P'1'         MULTIPLE WO'S                                
         BNH   TRN330              ALREADY PRINTED ONE WONO                     
*                                                                               
         L     R3,SORTAREA                                                      
         LA    R2,SRTDATA                                                       
         MVI   ELCODE,X'4B'                                                     
*                                                                               
         ZIC   RF,TWONUM+2                                                      
         BAS   RE,SETAP_R5                                                      
         ST    RF,PRTADDR                                                       
*                                                                               
         USING SR4BD,R2                                                         
PARTXB   BAS   RE,NEXTEL                                                        
         BNE   TRN330                                                           
*                                                                               
         CLI   SR4BNO,C'W'                                                      
         BNE   PARTXB              ALREADY PRINTED BILL NUMBERS                 
*                                                                               
         L     RF,PRTADDR          RESTORE RF (TRASHED BY GETEL)                
         BAS   RE,PRTNO                                                         
*                                                                               
         BAS   RE,BUMPAPRT         BUMP PRTADDR                                 
         B     PARTXB              PRINT NEXT                                   
*                                                                               
TRN330   MVI   CONACTV,C'Y'                                                     
*                                                                               
TRN350   CLC   SVANAL,=C'99'                                                    
         BNE   TRN370                                                           
         USING SORTD,R3                                                         
         L     R3,SORTAREA                                                      
         CLI   SRTTYPE,SRTTTIME    IS THIS TIME BILLING?                        
         BNE   TRN360                                                           
         OI    BILLSTAT,GOTTBILL                                                
*                                                                               
TRN360   CLI   SRTTYPE,SRTTOOP     IS THIS OOPS BILLING?                        
         BNE   TRN370                                                           
         OI    BILLSTAT,GOTOBILL                                                
*                                                                               
TRN370   BAS   RE,PRTRAN           PRINT THIS TRANSACTION NOW                   
         B     REQX                                                             
*                                                                               
TRN380   BAS   RE,BLNKPRT          DON'T PRINT THIS TRAN                        
         B     REQX                                                             
         DROP  R1,R2,R3                                                         
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PRINT AN AMOUNT                                                        
*        R2 IS A(TABLE ENTRY TO PRINT)                                          
*        R4 IS A(PACKED AMOUNT TO PRINT)                                        
*        R5 IS A(SWITCH TO SET)                                                 
*----------------------------------------------------------------------         
*                                                                               
FIGOUT   NTR1                                                                   
         CP    0(8,R1),=P'0'                                                    
         BE    EXIT                                                             
         MVC   0(2,R5),=C'YY'      TURN ON PRINT SWITCHES                       
         ZIC   RF,2(R2)            DISPLACEMENT INTO 'P' TO PRINT               
         BAS   RE,SETAPRT                                                       
         CLI   1(R2),X'0B'         IF CD OR COMM USE DIFF EDIT                  
         BE    FIG300                                                           
FIG200   EDIT  (P8,(R1)),(13,(RF)),2,MINUS=YES                                  
         B     EXIT                                                             
FIG300   EDIT  (P8,(R1)),(11,(RF)),2,MINUS=YES                                  
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PASSED A TRANSACTION WITH REVERSAL BIT ON                              
*        RETURN EQ IF THE REVERSAL DATE IN THE STATUS ELEMENT FALLS             
*        WITHIN MOS PARAMETERS                                                  
*----------------------------------------------------------------------         
*                                                                               
         USING SORTD,R3                                                         
*                                                                               
CHKREVDT NTR1                                                                   
         L     R3,SORTAREA                                                      
         LA    R2,SRTDATA                                                       
         MVI   ELCODE,X'60'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   CHKREQ                                                           
         USING SR60D,R2                                                         
         OC    SR60RMOS,SR60RMOS   IS THERE A MOS OF REV'ING TRAN DATE          
         BZ    CHKREQ              NO, RETURN OK                                
*                                                                               
         CLC   SR60RMOS,MYMSTR                                                  
         BL    CHKRNEQ                                                          
         CLC   SR60RMOS,MYMEND                                                  
         BH    CHKRNEQ                                                          
*                                                                               
CHKREQ   CR    RB,RB                                                            
         B     CHKRX                                                            
*                                                                               
CHKRNEQ  LTR   RB,RB                                                            
CHKRX    B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        EDITS, DATCON CALLS AND THE LIKE                                       
*----------------------------------------------------------------------         
*                                                                               
         USING SR4BD,R2            (LINE1 BILLNO IS **)                         
*                                                                               
PRTNO    DS    0H                                                               
         MVC   0(6,RF),SR4BNO                                                   
         BR    RE                                                               
         DROP  R2                                                               
*                                                                               
PRT2DTE  NTR1                      PRINT THE COMPRESSED DATE IN DOUBLE          
*                                  AT PRTADDR                                   
         L     R2,PRTADDR                                                       
         GOTO1 DATCON,DMCB,(2,DOUBLE),(8,(R2))                                  
         B     EXIT                                                             
*                                                                               
DOUBEDIT CP    DOUBLE,=P'0'                                                     
         BER   RE                                                               
*                                                                               
PRTAMNT  DS    0H                  PRINT SR4BAMNT FIELD                         
         ST    RE,SAVERE                                                        
         EDIT  (P8,DOUBLE),(13,(RF)),2,MINUS=YES                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRTHRS   DS    0H                                                               
         CP    DOUBLE,=P'0'                                                     
         BER   RE                                                               
*                                                                               
PR_RATE  ST    RE,SAVERE                                                        
         EDIT  (P8,DOUBLE),(8,(RF)),2,MINUS=YES                                 
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRTUNIT  DS    0H                                                               
         CP    DOUBLE,=P'0'                                                     
         BER   RE                                                               
*                                                                               
         ST    RE,SAVERE                                                        
         CURED (P8,DOUBLE),(8,(RF)),0,MINUS=YES                                 
         ORG   *-2                                                              
         TM    SV7CSTAT,UNPSQTRH   QUARTER HOURS?                               
         BZ    *+8                 NO                                           
         MVI   11(R1),2            YES, SET DECIMALS                            
         BASR  RE,RF                                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         USING SR44D,R2                                                         
         USING SORTD,R3                                                         
CALCCOM  DS    0H                  CALC COMMISION INTO COMAMT                   
         LA    R2,SRTDATA                                                       
         ZAP   COMAMT,=P'0'                                                     
         TM    SR44STAT,X'01'      NON COMMISSIONABLE                           
         BOR   RE                  COM IS ZERP                                  
         ZAP   PL16,NETAMT                                                      
         MP    PL16,BUFPCT         CALCULATE COMM. AND GROSS                    
         SRP   PL16,64-6,5         RATE IS FOUR DECIMAL PLACES                  
         ZAP   COMAMT,PL16         COMMISSION                                   
         BR    RE                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        THE FOLLOWING SUBROUTINES MAINTAIN PRTADDR                             
*----------------------------------------------------------------------         
*                                                                               
SETAPRT  DS    0H                  I/P-OFFSET IN RF                             
         ST    RE,SAVERE           O/P PRINT ADDRESS IN PRTADDR AND RF          
         L     RE,PRTBUFF                                                       
         LA    RF,0(RF,RE)                                                      
         ST    RF,PRTADDR                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
BUMPAPRT DS    0H                  I/P PRTADDR                                  
*                                  O/P PRTADDR+L'P                              
         L     RF,PRTADDR                                                       
         LA    RF,L'P(,RF)                                                      
         ST    RF,PRTADDR                                                       
         BR    RE                                                               
SETAP_R5 DS    0H                  R5 IS A POINTER TO A LINE IN PRTBUFF         
*                                  RF IS AN OFFSET                              
         LA    RF,0(RF,R5)                                                      
         ST    RF,PRTADDR                                                       
         BR    RE                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        INVERT SIGNS AFTER THE NUMBER HAS BEEN EDITED (PUT TO 'P')             
*----------------------------------------------------------------------         
*                                                                               
INVSIGN  DS    0H                  INVERT SIGNS                                 
         STC   RF,INV20+1          RF=LENGTH OF FIELD-1                         
INV20    CLC   0(0,R1),SPACES      R1=A(PRINT POSITION)                         
         BER   RE                                                               
INV30    CLI   0(R1),C' '          BUMP FORWARD TO NON-BLANK                    
         BNE   INV40                                                            
         LA    R1,1(,R1)                                                        
         B     INV30                                                            
INV40    CLI   0(R1),C'-'          MINUS BECOMES A BLANK                        
         BE    INV50                                                            
         BCTR  R1,0                                                             
         MVI   0(R1),C'+'          BLANK BECOMES A PLUS                         
         BR    RE                                                               
INV50    MVI   0(R1),C' '                                                       
         BR    RE                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        ADD DUBS AT R3 TO R2,R1 NUMBER OF TIMES                                
*        USED TO UPDATE ACCUMS TOTAL                                            
*        SUBEM IS CALLED WHEN I HAVE A BILLING ACCUM                            
*        ZAPEM ZAPS                                                             
*----------------------------------------------------------------------         
*                                                                               
ADDEM    NTR1                                                                   
ADDEM01  AP    0(8,R2),0(8,R3)                                                  
         LA    R2,8(,R2)                                                        
         LA    R3,8(,R3)                                                        
         BCT   R1,ADDEM01                                                       
         MVI   ADDEM01,X'FA'       IN CASE I WAS CALLED FROM SUBEM              
         B     EXIT                                                             
*                                                                               
SUBEM    MVI   ADDEM01,X'FB'       MAKE THE AP AN SP                            
         B     ADDEM                                                            
*                                                                               
ZAPEM    MVI   ADDEM01,X'F8'       MAKE THE AP AN ZAP                           
         B     ADDEM                                                            
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        CLEAR THE TRAILING COMMA IN R4 MINUS 2 (CLEAR COMMA SPACE)             
*----------------------------------------------------------------------         
*                                                                               
COMCLR   SH    R4,=H'2'            CLEAR TRAILING COMMA IF AROUND               
         CLI   0(R4),C','                                                       
         BE    *+12                                                             
*                                                                               
         CLI   0(R4),C';'                                                       
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
         BR    RE                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        IF THERE IS A CLIENT BILL TYPE AND THERE ARE NO UNBILLED               
*        ON THE JOB ADJUST BILLING COMMISSION TO MATCH TOTAL COMMISSION         
*        OF THE CHARGES                                                         
*        R5 IS A(CHARGES BUCKET), R2 IS STATUS BYTE                             
*        BUFACCS CONTAINS BILLING TOTALS                                        
*----------------------------------------------------------------------         
*                                                                               
FIXBAL   NTR1                                                                   
         TM    0(R2),NOTFULL       ARE THERE UNBILLED CHARGES HERE              
         BO    EXIT                YES                                          
         L     R2,ADGOBLOC                                                      
         USING GOBLOCKD,R2                                                      
         CLI   GOBILTYP,C'C'       CLIENT BILL TYPE?                            
         BNE   EXIT                NO                                           
         DROP  R2                                                               
         LA    R4,BUFACCS                                                       
         USING ACCUMD,R5                                                        
         ZAP   DUB,NETAC                                                        
         MP    DUB,=P'-1'                                                       
         CP    NETAC-ACCUMD(8,R4),DUB    NET BILLING = NET CHARGES              
         BNE   EXIT                      NO                                     
         LA    R2,NETAC-ACCUMD(R4)       MAKE THE BALENCE OF THIS               
         LA    R3,PZEROS                 JOB ZERO                               
         LA    R1,6                                                             
         BAS   RE,ZAPEM                                                         
         LA    R3,NETAC                                                         
         BAS   RE,SUBEM                                                         
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PRINT HRS/RATE IN NARRATIVE                                            
*----------------------------------------------------------------------         
*                                                                               
TIMEOUT  NTR1  ,                   R4=A(OUTPUT AREA)                            
         CLI   RATE,C'N'           Q, NO HRS/RATE IN NARRATIVE                  
         BE    EXIT                 Y, EXIT                                     
         CP    SVHOUR,=P'0'                                                     
         BE    EXIT                                                             
         BAS   RE,HOUREDIT                                                      
         AR    R4,R0                                                            
         SH    R4,=H'3'                                                         
         CLC   0(3,R4),=C'.00'     DON'T PRINT ZERO FRACTIONS                   
         BNE   TIM20                                                            
         MVI   0(R4),C' '                                                       
         B     TIM30                                                            
TIM20    LA    R4,3(,R4)                                                        
TIM30    MVC   1(3,R4),=C'HRS'                                                  
         MVI   5(R4),C'@'                                                       
         CP    SVRATE,=P'0'                                                     
         BNE   TIM35                                                            
         MVC   7(4,R4),=C'0/HR'                                                 
         LA    R4,7(R4)                                                         
         B     TIM50                                                            
TIM35    BAS   RE,RATEEDIT                                                      
         AR    R4,R0                                                            
         LA    R4,3(R4)                                                         
         CLC   1(3,R4),=C'.00'     DON'T PRINT ZERO PENNIES                     
         BE    TIM40                                                            
         LA    R4,3(R4)                                                         
TIM40    MVC   1(3,R4),=C'/HR'                                                  
TIM50    LA    R4,5(R4)                                                         
         XIT1  REGS=(R4)           RETURN A(NEXT AVAILABLE SLOT)                
*                                                                               
*                                                                               
RATEEDIT EDIT  (P8,SVRATE),(8,7(R4)),2,ALIGN=LEFT                               
         BR    RE                                                               
*                                                                               
HOUREDIT EDIT  (P8,SVHOUR),(8,0(R4)),2,ALIGN=LEFT,FLOAT=-                       
         BR    RE                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*   IF SEGREGATING TIME, SAVE BILLED TIME IN BILLTABLE, BY '99' REFNO           
*---------------------------------------------------------------------*         
*                                                                               
         USING SR44D,R2                                                         
         USING SORTD,R3                                                         
*                                                                               
PUTTIME  NTR1                                                                   
         LA    R2,SRTDATA                                                       
         MVI   BYTE,C' '                                                        
         OC    SR44USED,SR44USED   FULLY BILLED TRAN?                           
         BZ    PUTT10              NO                                           
         MVI   BYTE,C'U'           SET FLAG TO USE TRNSAMNT                     
         ZAP   DUB,SR44AMNT        SAVE TRNSAMNT                                
*                                                                               
PUTT10   MVI   ELCODE,X'4B'                                                     
PUTT20   BAS   RE,NEXTEL                                                        
         BNE   PUTT50              NO (MORE) BILLING ON THIS TIME TRANS         
         USING BILLTABD,R5                                                      
         USING SR4BD,R2                                                         
         L     R5,BILLTAB                                                       
         L     R0,0(R5)            NUMBER IN TABLE                              
         LTR   R0,R0               ANY BILLING SAVED                            
         BZ    EXIT                NO                                           
         LA    R5,8(R5)            TABLE DATA                                   
PUTT30   CLC   SR4BNO,BILLNO                                                    
         BNE   PUTT40                                                           
         CLI   BILLUS,0            WAS THERE A NARR+33 DATE ON THIS             
         BE    PUTT32              NO, INV NUM IS GOOD ENOUGH                   
         CLC   SR4BDTE,BILLUS                                                   
         BNE   PUTT40                                                           
*                                                                               
PUTT32   CLI   BILLTYPE,C'A'       ALLOCATED BILL                               
         BNE   PUTT34                                                           
         GOTO1 DATCON,DMCB,(2,SR4BDTE),(1,WORK)  CONVERT TO YMD                 
         CLC   WORK(3),BILLDATE                                                 
         BNE   PUTT40                                                           
*                                                                               
PUTT34   CLI   BYTE,C'U'           FULLY BILLED?                                
         BNE   PUTT35              NO                                           
         CP    SR4BAMNT,=P'0'      IS THERE AN AMOUNT HERE?                     
         BNE   PUTT35              YES, USE IT                                  
         ZAP   SR4BAMNT,DUB        NO, USE SAVED TRNSAMNT                       
         MVI   BYTE,C'D'           AND LEAVE                                    
*                                                                               
PUTT35   ZAP   PL16,SR4BAMNT       PREP FOR COMMISSION AMOUNT                   
         AP    PL16,SR4BCSD        INCASE THERE WAS CASHD                       
         MP    PL16,SR4BRATE                                                    
         SRP   PL16,64-6,5                                                      
         AP    BILTCOM,PL16                                                     
         AP    BILTNET,SR4BAMNT                                                 
         AP    BILTHRS,SR4BHRS                                                  
         CLI   BYTE,C'D'           DID I USE TRNSAMNT                           
         BE    PUTT50                                                           
         B     PUTT20              FOUND A BILL, I'M DONE W/ THIS TRANS         
*                                                                               
PUTT40   LA    R5,LBILLTAB(R5)                                                  
         BCT   R0,PUTT30           NOTE: IF YOU DONT FIND A 99 INVOICE          
         B     PUTT20              NUM FOR A TRBDNO, YU IGNORE                  
*                                                                               
PUTT50   MVI   ELCODE,X'49'        BILLED THEN REVERSED                         
         LA    R2,SRTDATA                                                       
         LA    R4,SRTDATA          GET SRT44AMNT OFF OF R4                      
         BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         USING REVBD,R2                                                         
         L     R5,BILLTAB                                                       
         LA    R5,8(R5)            TABLE DATA                                   
*                                                                               
PUTT55   CLC   REVBDAT,BILLUS      IS THIS THE ORIGINAL BILLING                 
         BNE   PUTT60              NO                                           
         AP    BILTNET(6),SR44AMNT-SR44D(6,R4)                                  
PUTT60   CLC   REVBUND,BILLUS      IS THIS THE REVERSAL BILLING                 
         BNE   PUTT70              NO                                           
         SP    BILTNET(6),SR44AMNT-SR44D(6,R4)                                  
PUTT70   LA    R5,LBILLTAB(R5)                                                  
         BCT   R0,PUTT55           NOTE: IF YOU DONT FIND A 99 INVOICE          
         B     EXIT                NUM FOR A TRBDNO, YU IGNORE                  
         DROP  R2,R3,R5                                                         
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PRINT A TRAN, CLOSE BOXES IF NECESARY                                  
*        IF THE USER HAS ASKED FOR A FULL WC SUMM, DO DATE FILTERING            
*        HERE                                                                   
*                                                                               
*        DO NOT USE SAVERE IN HERE, RE IS SAVED IN FULL WHEN CALLED             
*----------------------------------------------------------------------         
*                                                                               
         USING BOXD,R4                                                          
*                                                                               
PRTRAN   NTR1                                                                   
         XR    RF,RF               ANYTHING TO PRINT                            
         BAS   RE,SETAPRT                                                       
         CLC   0(L'P,RF),SPACES                                                 
         BE    EXIT                                                             
         AP    WCSW,=P'1'                                                       
         MVI   SUPPSW,C'N'         TURN OFF FIRST TIME SWITCH                   
         CLI   ANALPEND,0          DO WE NEED WORK CODE HEADING                 
         BNE   PRT65                                                            
*                                                                               
         MVI   ANALPEND,1                                                       
         MVC   P+1(2),SVANAL       MOVE OUT WORK CODE                           
         MVC   P+4(15),WNAME       AND NAME                                     
         GOTO1 =V(UNDERLIN),DMCB,(19,P),(X'BF',PSECOND)                         
*                                                                               
*                                                                               
PRT30    ZIC   R3,LINE             CHECK FOR ENOUGH ROOM                        
         LA    R3,4(R3)                                                         
         ZIC   R4,MAXLINES                                                      
         CR    R3,R4                                                            
         BNH   PRT50                                                            
         MVI   FORCEHED,C'Y'                                                    
PRT50    L     R4,ADBOX                                                         
         MVC   MYBOXCTL,BOXDDCTL                                                
         MVI   BOXDDCTL,0          TURN OFF DATA DICT, PRINTING A TRAN          
*                                                                               
*####################################################                           
*        EDIT  LINE,(4,P+29),ALIGN=LEFT     #########                           
*####################################################                           
*                                                                               
         BAS   RE,PRINTEM          PRINT WORK CODE HEADING                      
*                                                                               
*                                                                               
PRT65    BAS   RE,PRINTBUF         PRINT PRTBUFF                                
*                                                                               
         GOTO1 =A(PRTSUB),DMCB,(RC)    PRINT ADDITIONAL CONTRA ACCOUNTS         
         L     R4,ADBOX                                                         
         MVC   BOXDDCTL,MYBOXCTL   RESTORE DATA DICT BOX SETTING                
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
PRINTBUF NTR1                                                                   
         L     R2,PRTBUFF                                                       
         LA    R0,PBMAX                                                         
PBUFF50  MVC   P,0(R2)                                                          
         CLC   P,SPACES                                                         
         BE    *+8                                                              
         BAS   RE,PRINTEM          PRINT THE TRANSACTION                        
         LA    R2,L'P(R2)                                                       
         BCT   R0,PBUFF50                                                       
         B     EXIT                                                             
*                                                                               
CLRPRTBF NTR1                                                                   
         LH    RF,=Y(PRTBLN)       CLEAR PRINT BUFFER                           
         L     RE,PRTBUFF                                                       
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0                                                            
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PRINT TAX ON BILL                                                      
*             IF SEPARATING TIME AND OOP, PRINT ONLY BILLING TOTALS             
*        ON ENTRY, R2 IS A(GROSS TABLE ENTRY), THUS 2(R2) IS PRINT              
*        OFFSET                                                                 
*----------------------------------------------------------------------         
PRTTAX   NTR1                                                                   
*                                                                               
         OC    PROCSTAT,PROCSTAT   TIME VS OOP                                  
         BZ    PTX10               NO                                           
         TM    PROCSTAT,FINLBILL   ONLY PRINT FOR BILLING DETAILS               
         BNO   PTXX                                                             
*                                                                               
         USING SRGSD,R2                                                         
PTX10    LR    R3,R2               SAVE R2 IN R3                                
         L     R2,SORTAREA                                                      
         LA    R2,SRTKEYLN(R2)                                                  
         MVI   ELCODE,VBIELQ                                                    
         BAS   RE,NEXTEL                                                        
         BNE   PTX50               LOOK FOR PBI ELS                             
         ZAP   DOUBLE,=P'0'                                                     
         MVI   GSTFLAG,C'Y'        FLAG JOB AS HAVING GST                       
*                                                                               
PTX30    AP    DOUBLE,SRGSAMNT                                                  
         BAS   RE,NEXTEL                                                        
         BE    PTX30                                                            
         ZIC   RF,2(R3)            GET OFFSET                                   
         BAS   RE,SETAPRT                                                       
         BAS   RE,BUMPAPRT                                                      
         CP    DOUBLE,=P'9999999'  WILL GST FIT                                 
         BNH   *+8                 YES                                          
         LA    RF,L'P(RF)          NO, NEXT LINE                                
         BAS   RE,PRTAMNT                                                       
*                                                                               
         L     R5,PRTADDR          ADDRESS FROM BUMPAPRT                        
         MVC   0(4,R5),=C'GST='                                                 
         ST    RF,PRTADDR          SET PRTADDR TO LAST LINE USED                
         AP    GSTTOTAL,DOUBLE     ACCUMULATE GST TOTAL FOR JOB                 
*                                                                               
         USING SRPSD,R2                                                         
PTX50    GOTO1 =A(PSTCLR),DMCB,(RC),PSTTTBIQ                                    
         L     R2,SORTAREA         LOOK FOR PST                                 
         LA    R2,SRTKEYLN(R2)                                                  
         MVI   ELCODE,PBIELQ                                                    
         BAS   RE,NEXTEL                                                        
         BNE   PTXX                NO PST                                       
         MVI   PSTFLAG,C'Y'        FLAG JOB AS HAVING GST                       
*                                                                               
PTX80    DS    0H                                                               
         GOTO1 =A(PSTPUT),DMCB,(RC),(R2)                                        
         BAS   RE,NEXTEL                                                        
         BE    PTX80                                                            
*                                                                               
         USING PSTTABD,R3                                                       
         L     R3,APSTTAB                                                       
         LA    R0,MAXPRV                                                        
*                                                                               
PTX90    BAS   RE,BUMPAPRT         NEXT P LINE                                  
         OC    PSTTNAME,PSTTNAME   IS THERE A PROVINCE DEFINED HERE             
         BZ    PTXX                NO, DONE                                     
         CP    PSTTAMNT,=P'9999'   WILL AMOUNT FIT NEXT TO NAME                 
         BH    PTX95                                                            
         CP    PSTTAMNT,=P'-9999'                                               
         BNL   *+8                 YES                                          
*                                                                               
PTX95    LA    RF,L'P(RF)          NO PRINT IN NEXT                             
         ZAP   DOUBLE,PSTTAMNT                                                  
         BAS   RE,PRTAMNT                                                       
*                                                                               
         L     R5,PRTADDR                                                       
         MVC   0(6,R5),PSTTNAME                                                 
         MVI   6(R5),C'='                                                       
         ST    RF,PRTADDR                                                       
         LA    R3,PSTTABLN(R3)                                                  
         BCT   R0,PTX90                                                         
*                                                                               
PTXX     B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        DEAL WITH POSSIBILITY OF NEEDING TO PRINT ESTIMATE VALUES              
*        FOR TIME WORKCODES, IF JOB HAS NO TIME TRANSACTIONS                    
*----------------------------------------------------------------------         
*                                                                               
*----------------------------------------------------------------------         
*        PRINT A RANGE OF ESTIMATE VALUES                                       
*        RETURNS BUFREC SET TO WORKCODE VALUE PASSED IN CPESTEND                
*                OR HIGHER, IF NOT FOUND                                        
*        NOTE: EST VALUE NOT PRINTED FOR CPESTEND                               
*----------------------------------------------------------------------         
*                                                                               
CALLPEST NTR1                                                                   
*                                                                               
*        MVC   P+11(10),=C'CALLPEST  '                                          
*        MVC   P+21(1),CPESTSTP                                                 
*        MVI   P+22,C'/'                                                        
*        MVC   P+23(2),CPESTSTR                                                 
*        MVI   P+25,C'/'                                                        
*        MVC   P+26(1),CPESTETP                                                 
*        MVI   P+27,C'/'                                                        
*        MVC   P+28(2),CPESTEND                                                 
*        GOTO1 AMYREPT,DMCB,(RC)                                                
*                                                                               
         MVC   BUFKEY,SPACES                                                    
         MVI   BUFTYPE,C'1'                                                     
         MVC   BUFWTYPE,CPESTSTP   START TYPE                                   
         MVC   BUFANAL,CPESTSTR    START WC                                     
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'HIGH',(0,ADBUFC),BUFREC,1                        
CPEST20  TM    DMCB+8,X'80'        NO EST RECORDS IN BUFFALO                    
         BO    CPESTXX                                                          
         CLI   BUFTYPE,C'1'                                                     
         BNE   CPESTX                                                           
         CLC   BUFWTYPE,CPESTETP   DID I GET ENDING TYPE?                       
         BH    CPESTX              PAST IT                                      
         CLC   BUFANAL,CPESTEND    GOT  ENDING WORKCODE                         
         BNL   CPESTX              YES, ALL DONE                                
*                                                                               
CPEST30  MVC   SVANAL,BUFANAL      SAVE WC FROM BUFFALO                         
         MVI   ANALPEND,0                                                       
         CLI   PROF25,C'Y'         PRINT ESTIMATES W/O ACTUALS                  
         BNE   *+8                 NO                                           
         BAS   RE,PESTVAL          PRINT ESTIMATE                               
         MVI   ANALPEND,0          SO NEXT EST VAL WILL PRINT                   
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',(C'1',ADBUFC),BUFREC,1                      
         B     CPEST20                                                          
*                                                                               
CPESTX   CLI   BUFTYPE,C'2'        THIS IS NOT AN ESTIMATE                      
         BE    CPESTXX                                                          
         MVC   SVANAL,BUFANAL      SAVE WC FROM BUFFALO                         
*                                                                               
CPESTXX  B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PRINT THE ESTIMATE VALUE IN EITHER THE W/C NAME FIELD OR               
*        IN A SPECIAL ESTIMATE VALUE COL                                        
*----------------------------------------------------------------------         
*                                                                               
PESTVAL  NTR1                                                                   
         GOTO1 =A(WCNAME),DMCB,(RC)                                             
         CLI   QOPT1,C'S'          SUMMARY ONLY                                 
         BE    EXIT                BYE BYE                                      
         CLC   SVANAL,=C'**'       PURCHACE ORDER                               
         BE    EXIT                DONT WORRY ABOUT ESTIMATE VAL                
         CLC   SVANAL,=C'99'       SAME FOR BILLING                             
         BE    EXIT                                                             
         TM    JOBSTAT,NEEDESTS    DO I NEED ESTIMATES?                         
         BNO   EXIT                NO                                           
*                                                                               
*        OC    PROCSTAT,PROCSTAT   SEPARATING TIME AND OOPS                     
*        BZ    PEST10              NO                                           
*        MVI   BYTE,O_O_P_WC       SET TO ACCEPT OOP WC'S                       
*        TM    PROCSTAT,TIMETRAN   IS THIS TIME                                 
*        BNO   *+8                 NO                                           
*        MVI   BYTE,TIMEWC         SET TO ACCEPT TIME WC'S                      
*        CLC   BUFWTYPE,BYTE       IS THIS WC THE RIGHT TYPE                    
*        BNE   EXIT                NO                                           
PEST10   ZAP   DOUBLE,BUFACCS2     LOAD DOUBLE                                  
         BZ    EXIT                NOTHING THERE                                
         MVI   ANLACTV,C'Y'                                                     
*                                                                               
         MVC   P+1(2),SVANAL       MOVE OUT WORK CODE                           
         MVC   P+4(15),WNAME       AND NAME                                     
         CLI   PRINTEST,C'1'       NET ESTIMATE COL                             
         BE    PEST36                                                           
         CLI   PRINTEST,C'3'       PRT  NET UNDER W/C                           
         BE    PEST36                                                           
         CLI   PRINTEST,C'2'       GROSS ESTIMATE COL                           
         BE    PEST35                                                           
         CLI   PRINTEST,C'4'       PRT GRS UNDER W/C                            
         BE    PEST35                                                           
         BAS   RE,BLNKPRT                                                       
         B     EXIT                                                             
*                                                                               
PEST35   ZAP   DOUBLE,BUFACCSD     USE GROSS                                    
*                                                                               
PEST36   CLI   PRINTEST,C'2'       WHERE AM I PRINTING                          
         BH    PEST38              UNDER W/C                                    
         USING TABLED,R2                                                        
         L     R2,ATABLE                                                        
         CLI   ESTVALUE,C'Y'       MAKE SHUR I HAVE A COL                       
         BNE   PESTX               NOPE                                         
         ZIC   RF,ESTVALUE+2                                                    
         LA    RF,P(RF)                                                         
         ST    RF,PRTADDR                                                       
         BAS   RE,DOUBEDIT         EDIT DOUBLE INTO 0(RF)                       
         GOTO1 =V(UNDERLIN),DMCB,(19,P),(X'BF',PSECOND)                         
         B     PESTX                                                            
*                                                                               
PEST38   MVC   PSECOND+1(3),=C'EN='  ESTIMATE NET                               
         CLI   PRINTEST,C'4'       DO THE WANT GROSS                            
         BNE   *+8                 NO                                           
         MVI   PSECOND+2,C'G'      YES, MAKE EN= EG=                            
         LA    R5,PSECOND+4                                                     
         EDIT  (P8,DOUBLE),(13,(R5)),2,MINUS=YES,ALIGN=LEFT                     
         GOTO1 =V(UNDERLIN),DMCB,(19,P),(X'BF',PTHIRD)                          
*                                                                               
PESTX    MVI   ANALPEND,1                                                       
         BAS   RE,PRINTEM                                                       
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        FIND OUT WHAT FIELDS THE UER IS PRINTING AND PRINT                     
*        SUBTOTALS FOR THOSE COLUMNS WHICH GET THEM                             
*        CALLED FROM LAST WC, LAST JOB, LAST TYPE                               
*        R3 IS LINE OF ACUMAREA YOU WISH TO PRINT, COVERED BY ACCUMD            
*        ACUMAREA IS BUILT AND MAINTAINED BY 'TOTALS'                           
*----------------------------------------------------------------------         
*                                                                               
         USING ACCUMD,R2                                                        
         USING TABLED,R1                                                        
*                                                                               
SUBLINE  NTR1                                                                   
         BAS   RE,CLRPRTBF                                                      
         XR    RF,RF                                                            
         BAS   RE,SETAPRT                                                       
         MVC   1(18,RF),P+1        RETREIVE *TOTALS FOR FROM P                  
*                                                                               
         L     R1,ATABLE           A(TABLE) YOU ARE READING                     
         LR    R2,R3                                                            
         XC    STACK,STACK         BUILD TABLE OF AMOUNTS TO PRINT              
         LA    R4,STACK            AND WHERE TO PRINT THEM                      
         CLI   ORDAMT,C'Y'                                                      
         BNE   SUB20                                                            
*                                                                               
         ZAP   0(8,R4),ORDAC                                                    
         ZIC   RF,ORDAMT+2                                                      
         BAS   RE,SETAPRT                                                       
         STCM  RF,15,8(R4)                                                      
*                                                                               
SUB20    LA    R4,12(R4)                                                        
         CLI   BILLAMT,C'Y'        IF BILLED AMOUNT                             
         BNE   SUB30                                                            
         CLI   MYMODE,ACCLAST      AND DOING JOB BALANCE                        
         BNE   SUB25                                                            
         MVI   BILLSW+1,C'N'       THEN DON'T PRINT                             
         B     SUB30                                                            
*                                                                               
SUB25    ZAP   0(8,R4),BILLAC                                                   
         BZ    SUB30                                                            
         ZIC   RF,BILLAMT+2                                                     
         BAS   RE,SETAPRT                                                       
         STCM  RF,15,8(R4)                                                      
SUB30    LA    R4,12(R4)                                                        
*                                                                               
         CLI   NET,C'Y'                                                         
         BNE   SUB40                                                            
         ZAP   0(8,R4),NETAC                                                    
         CLI   MYMODE,ANALLAST     IF AT END OF ORDERS                          
         BNE   SUB35               MOVE ORDER TOTAL TO NET COL                  
         CLC   SVANAL,=C'**'                                                    
         BNE   SUB35                                                            
         ZAP   0(8,R4),ORDAC                                                    
SUB35    ZIC   RF,NET+2                                                         
         BAS   RE,SETAPRT                                                       
         STCM  RF,15,8(R4)                                                      
*                                                                               
SUB40    LA    R4,12(R4)                                                        
         CLI   COMM,C'Y'                                                        
         BNE   SUB45                                                            
         ZAP   0(8,R4),COMMAC                                                   
         ZIC   RF,COMM+2                                                        
         BAS   RE,SETAPRT                                                       
         STCM  RF,15,8(R4)                                                      
*                                                                               
SUB45    LA    R4,12(R4)                                                        
         CLI   GROSS,C'Y'                                                       
         BNE   SUB50                                                            
         ZAP   0(8,R4),GRSAC                                                    
         ZIC   RF,GROSS+2                                                       
         BAS   RE,SETAPRT                                                       
         STCM  RF,15,8(R4)                                                      
*                                                                               
SUB50    LA    R4,12(R4)                                                        
         CLI   DISCOUNT,C'Y'                                                    
         BNE   SUB55                                                            
         ZAP   0(8,R4),CDAC                                                     
         ZIC   RF,DISCOUNT+2                                                    
         BAS   RE,SETAPRT                                                       
         STCM  RF,15,8(R4)                                                      
*                                                                               
SUB55    LA    R4,12(R4)                                                        
         CLI   BILLABLE,C'Y'       IF BILLABLE COL                              
         BNE   SUB65                                                            
         CLI   QOPT5,C'B'          BILLED ONLY,                                 
         BNE   SUB55A              NO                                           
         ZAP   0(8,R4),=P'0'       ZAP BILLABLE                                 
         B     SUB60A              PRINT ZERO                                   
SUB55A   CLI   MYMODE,ANALLAST                                                  
         BNE   SUB60                                                            
         CLC   SVANAL,=C'99'       DON'T FOR W/C 99 TOTALS                      
         BE    SUB65                                                            
SUB60    ZAP   0(8,R4),GRSAC       GROSS---USE TO USE NET. IRENE W.             
         SP    0(8,R4),BILLAC      LESS BILLED                                  
SUB60A   ZIC   RF,BILLABLE+2                                                    
         BAS   RE,SETAPRT                                                       
         STCM  RF,15,8(R4)                                                      
*                                                                               
SUB65    LA    R4,12(R4)                                                        
         CLI   NARR,C'Y'                                                        
         BNE   SUB70                                                            
         ZAP   0(8,R4),ALLOCAC                                                  
         ZIC   RF,NARR+2                                                        
         BAS   RE,SETAPRT                                                       
         STCM  RF,15,8(R4)                                                      
         LA    R4,12(R4)                                                        
         LA    RF,15(RF)                                                        
         STCM  RF,15,8(R4)                                                      
         ZAP   0(8,R4),ALLOCHRS                                                 
*                                                                               
SUB70    LA    R1,STACK                                                         
         LA    R3,NPRTSWS                                                       
         LA    R5,PRTSWS                                                        
         CLI   MYMODE,ANALLAST                                                  
         BE    SUB75                                                            
         LA    R5,1(R5)            USE SECOND BYTE FOR JOB TOTALS               
SUB75    OC    8(4,R1),8(R1)       PRINT LOCATION                               
         BZ    SUB90                                                            
         CLI   0(R5),C'Y'          ANYTHING TO PRINT                            
         BNE   SUB90                                                            
         ICM   RF,15,8(R1)                                                      
         CH    R3,=AL2(NPRTSWS-((COMSW-PRTSWS)/2)) DIFF EDIT IF COMM            
         BE    SUB85                                                            
         CH    R3,=AL2(NPRTSWS-((DISCSW-PRTSWS)/2))   CD,                       
         BE    SUB85                                                            
         CH    R3,=AL2(NPRTSWS-((BLLABLSW-PRTSWS)/2))  BILLABLE,                
         BE    SUB85                                                            
         CH    R3,=AL2(NPRTSWS-((NARRSW-PRTSWS)/2))    OR ALLOCATED             
         BE    SUB78                                                            
         CH    R3,=AL2(NPRTSWS-((HRSSW-PRTSWS)/2))     OR ALLOCATED HRS         
         BNE   SUB80                                                            
         CP    0(8,R1),=P'0'                                                    
         BE    SUB90                                                            
         MVC   0(2,RF),=C'H='                                                   
         B     SUB79                                                            
*                                                                               
SUB78    CP    0(8,R1),=P'0'       SKIP PRINTING IF ZERO                        
         BE    SUB90                                                            
         MVC   0(2,RF),=C'A='                                                   
SUB79    LA    RF,2(RF)                                                         
         EDIT  (P8,(R1)),(11,(RF)),2,MINUS=YES,ALIGN=LEFT                       
         B      SUB90                                                           
SUB80    EDIT  (P8,(R1)),(13,(RF)),2,MINUS=YES                                  
         B     SUB90                                                            
*                                                                               
SUB85    EDIT  (P8,(R1)),(11,(RF)),2,MINUS=YES                                  
SUB90    LA    R1,12(R1)                                                        
         LA    R5,2(R5)                                                         
         BCT   R3,SUB75                                                         
*                                                                               
         L     R1,ATABLE                                                        
         CLI   UNITPRCE,C'Y'       UNIT/PRICE COL                               
         BNE   SUB94               NO                                           
*                                                                               
         CLC   SVANAL,=C'99'       NO FOR BILLING                               
         BE    SUB94                                                            
*                                                                               
         L     R3,AUNITS           ADDRESS OF UNIT ACCUMULATORS                 
         CLI   MYMODE,ANALLAST     LAST WC, USE FIRST ACCUM                     
         BE    SUB92                                                            
*                                                                               
         LA    R3,8(R3)                                                         
         CLI   MYMODE,ACCLAST      LAST TYPE USES SECOND                        
         BNE   SUB92                                                            
         LA    R3,8(R3)            LAST ACCOUNT USES THIRD                      
*                                                                               
SUB92    ZIC   RF,UNITPRCE+2                                                    
         BAS   RE,SETAPRT                                                       
*                                                                               
         ZAP   DOUBLE,0(8,R3)                                                   
         MVC   SV7CSTAT,SUBSTAT    MOVE IN STATUS FOR PRTUNIT                   
         TM    SV7CSTAT,UNPSQTRH   ANYTHING HOURLY?                             
         BO    SUB93               YES, PRINT IT                                
         ZAP   DUB,DOUBLE          NO,CHANGE BACK TO UNITS                      
         DP    DUB,=P'100'                                                      
         ZAP   DOUBLE,DUB(6)                                                    
*                                                                               
SUB93    BAS   RE,PRTUNIT                                                       
*                                                                               
SUB94    CLI   PROF33,C'Y'         TIME SPECIFIC COLS                           
         BNE   SUB220                                                           
*                                                                               
         CLC   SVANAL,=C'99'       PRINT ADDITIONAL TOTALS                      
         BE    SUB220                                                           
*                                                                               
         L     R3,TIMESUB          ADDRESS OF ADDITIONAL TIME ACCUMS            
         CLI   MYMODE,ANALLAST     LAST WC, USE FIRST ACCUM                     
         BE    SUB95                                                            
         LA    R3,TIMSUBLN(R3)                                                  
         CLI   MYMODE,ACCLAST      LAST TYPE USES SECOND                        
         BNE   SUB95                                                            
         LA    R3,TIMSUBLN(R3)     LAST ACCOUNT USES THIRD                      
*                                                                               
         USING TIMSUBD,R3                                                       
SUB95    L     R1,ATABLE           A(TABLE) YOU ARE READING                     
         CLI   TBLDHRS,C'Y'                                                     
         BNE   SUB100                                                           
         ZAP   DOUBLE,TSBLDHRS                                                  
         ZIC   RF,TBLDHRS+2                                                     
         BAS   RE,SETAPRT                                                       
         BAS   RE,PRTHRS                                                        
*                                                                               
SUB100   CLI   TBLABLHR,C'Y'                                                    
         BNE   SUB110                                                           
         ZAP   DOUBLE,TSHRS                                                     
         SP    DOUBLE,TSBLDHRS                                                  
         ZIC   RF,TBLABLHR+2                                                    
         BAS   RE,SETAPRT                                                       
         BAS   RE,PRTHRS                                                        
*                                                                               
SUB110   CLI   THOURS,C'Y'                                                      
         BNE   SUB120                                                           
         ZAP   DOUBLE,TSHRS                                                     
         ZIC   RF,THOURS+2                                                      
         BAS   RE,SETAPRT                                                       
         BAS   RE,PRTHRS                                                        
*                                                                               
SUB120   CLI   TWOHOURS,C'Y'                                                    
         BNE   SUB130                                                           
         ZAP   DOUBLE,TSWOHRS                                                   
         ZIC   RF,TWOHOURS+2                                                    
         BAS   RE,SETAPRT                                                       
         BAS   RE,PRTHRS                                                        
         MVI   BYTE,C'H'                                                        
         BAS   RE,PUTMEMO                                                       
*                                                                               
SUB130   CLI   TWOAMNT,C'Y'                                                     
         BNE   SUB135                                                           
         ZAP   DOUBLE,TSWOAMT                                                   
         ZIC   RF,TWOAMNT+2                                                     
         BAS   RE,SETAPRT                                                       
         BAS   RE,DOUBEDIT                                                      
         MVI   BYTE,C'A'                                                        
         BAS   RE,PUTMEMO                                                       
*                                                                               
SUB135   CLI   MYMODE,ACCLAST      DONT CARRY UP TOTALS AT ACCLAST              
         BE    SUB220                                                           
         CLI   MYMODE,0            DONT CARRY UP TOTALS AT LSTTYPE CALL         
         BE    SUB220                                                           
         LA    R2,TIMSUBLN(R3)     POINT R2 TO THE HIGHER ACCUM                 
         LA    R1,4                                                             
         BAS   RE,ADDEM            BUMP UP TOTALS                               
*                                                                               
SUB220   CLI   SUBLGST,C'Y'        SHOULD I PRINT TAX (BILLING TOTS)            
         BNE   *+8                                                              
         BAS   RE,SUBLTAX                                                       
*                                                                               
         BAS   RE,PRINTBUF         PRINT DATA IN PRTBUF                         
         CLC   LINE,MAXLINES       ROOM TO SKIP                                 
         BNL   *+8                 NO, JUST TOF                                 
         BAS   RE,PRINTEM          AND SKIP A LINE                              
         B     EXIT                                                             
         DROP  R1,R2,R3                                                         
*                                                                               
PUTMEMO  DS    0H                                                               
         CLI   MYMODE,0            LASTTYPE CALL                                
         BNER  RE                  NO                                           
         CP    DOUBLE,=P'0'        ANYTHING PRINTED                             
         BER   RE                  NO                                           
         CLI   BYTE,C'A'           AMOUNT                                       
         BNE   PUTMEM20            NO                                           
         LA    RF,3(RF)            CENTER FOR LARGER AMOUNT FIELD               
PUTMEM20 LA    RF,L'P(RF)          ON THE NEXT PRINT LINE                       
         MVC   0(6,RF),=C'*MEMO*'                                               
         BR    RE                                                               
         EJECT ,                                                                
* --------------------------------------------------------------------          
*        PRINT GST/PST TAX AMOUNTS BENEATH GROSS COL OF SUBLINE                 
* --------------------------------------------------------------------          
*                                                                               
SUBLTAX  NTR1                                                                   
         CLC   SVANAL,=C'99'       BILLING TOTAL?                               
         BNE   SUBLTXX             NO                                           
*                                                                               
         USING TABLED,R1                                                        
         L     R1,ATABLE                                                        
         CLI   GROSS,C'Y'          PRINTING GROSS AMOUNT                        
         BNE   SUBLTXX             NO                                           
*                                                                               
         ZIC   RF,GROSS+2          YES, PRINT GST NOW                           
         BAS   RE,SETAPRT                                                       
         BAS   RE,BUMPAPRT                                                      
         LR    R2,RF                                                            
*                                                                               
         CLI   GSTFLAG,C'Y'        ANY GST ON JOB                               
         BNE   SUBLTX70            NO                                           
*                                                                               
         CP    GSTTOTAL,=P'9999999'                                             
         BL    *+8                 YES                                          
         BAS   RE,BUMPAPRT         NO PRINT IN NEXT P                           
         LR    R2,RF               SAVE PRINT ADDRESS                           
         ZAP   DOUBLE,GSTTOTAL                                                  
         BAS   RE,PRTAMNT                                                       
*                                                                               
         L     R1,ATABLE                                                        
         ZIC   RF,GROSS+2                                                       
         BAS   RE,SETAPRT                                                       
         BAS   RE,BUMPAPRT                                                      
         MVC   0(4,RF),=C'GST='                                                 
*                                                                               
SUBLTX70 ST    R2,PRTADDR          RESTORE PRINT ADDRESS                        
         BAS   RE,BUMPAPRT                                                      
*                                                                               
         USING PSTTABD,R3                                                       
         L     R3,APSTTAB                                                       
         LA    R0,MAXPRV*2                                                      
SUBLTX80 CLI   PSTTTYPE,PSTTTJBQ                                                
         BNE   SUBLTX90                                                         
*                                                                               
         OC    PSTTNAME,PSTTNAME   ANY PROVINCE DEFINED HERE                    
         BZ    SUBLTX90                                                         
*                                                                               
         LR    R4,RF               SAVE STARTING PRT ADDRESS                    
         CP    PSTTAMNT,=P'9999'                                                
         BH    SUBLTX85                                                         
         CP    PSTTAMNT,=P'-9999'                                               
         BNL   *+8                 YES                                          
*                                                                               
SUBLTX85 BAS   RE,BUMPAPRT         NO PRINT IN NEXT P                           
         LR    R2,RF               SAVE PRINT ADDRESS                           
         ZAP   DOUBLE,PSTTAMNT                                                  
         BAS   RE,PRTAMNT                                                       
*                                                                               
         LR    RF,R4                                                            
         MVC   0(6,RF),PSTTNAME                                                 
         MVI   6(RF),C'='                                                       
         ST    R2,PRTADDR          RESTORE LAST P USED                          
         BAS   RE,BUMPAPRT                                                      
*                                                                               
SUBLTX90 LA    R3,PSTTABLN(R3)                                                  
         BCT   R0,SUBLTX80                                                      
*                                                                               
SUBLTXX  B     EXIT                                                             
         DROP  R1,R3                                                            
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PUT A LINE OUT OF THE WORK CODE SUMMARY                                
*----------------------------------------------------------------------         
*                                                                               
PUTWCSUM NTR1                                                                   
         MVC   BUFKEY,SPACES                                                    
         MVI   BUFTYPE,C'1'                                                     
         MVC   BUFANAL,SVANAL      PUT A WC SUMMARY LINE TO BUFFALO             
         MVC   BUFNAME,WNAME                                                    
         CLI   PROF33,C'Y'        SEPERATE TIME AND OOP                         
         BNE   PWC40                                                            
*                                                                               
         MVC   BUFWTYPE,WTYPE                                                   
*                                                                               
         CLC   BUFANAL,=C'99'      IS THIS A BILLING RECORD                     
         BE    PWC40               YES, DON'T INCLUDE IN STATUS                 
*                                                                               
         OC    WCTPSTAT,WTYPE      SAVE W/C TYPES ON JOB                        
*                                                                               
PWC40    GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        SET HEADLINES, CALL AMYREPT                                            
*----------------------------------------------------------------------         
*                                                                               
PRINTEM  NTR1                                                                   
*                                                                               
PRTM9    GOTO1 AMYREPT,DMCB,(RC)                                                
         B     EXIT                                                             
*                                                                               
*                                                                               
BLNKPRT  DS    0H                  BLANKS-OUT P THRU PFOURTH                    
         LA    R3,4*L'P                                                         
         LA    R2,P                                                             
         XR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  R2,R0                                                            
         BR    RE                                                               
         EJECT ,                                                                
*---------------------------------------------+                                 
*        PRINT USER RECORDS                   |                                 
*---------------------------------------------+                                 
*                                                                               
         USING ACUFD,R2                                                         
*                                                                               
USRPRT   NTR1                                                                   
         L     R2,DMGRBUFF                                                      
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
USR1     BNE   EXIT                                                             
         BAS   RE,PRINTEM                                                       
         MVC   P+1(12),=C'USER FIELDS:'                                         
         GOTO1 =V(UNDERLIN),DMCB,(12,P+1),PSECOND+1                             
         BAS   RE,PRINTEM                                                       
*                                                                               
USR1A    MVC   P+1(12),ACUFDESC                                                 
         ZIC   R1,ACUFLEN                                                       
         SH    R1,=H'33'                                                        
         BM    USR2                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    ACUFDATA(0),ACUFDATA                                             
         BZ    USR2                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+14(0),ACUFDATA                                                 
         B     USR3                                                             
USR2     TM    ACUFSTAT,X'40'      NEED THIS FIELD FOR BILLS?                   
         BNO   USR3                                                             
         USING LITTABD,R5                                                       
         L     R5,=A(LITTAB)                                                    
         MVC   P+43(14),NEED2BL                                                 
USR3     BAS   RE,PRINTEM                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,NEXTEL                                                        
         BE    USR1A                                                            
         BAS   RE,PRINTEM                                                       
         B     EXIT                                                             
*                                                                               
         DROP  R2,R5                                                            
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        PRINT THE MANPOWER SUMMARY                                   *         
*---------------------------------------------------------------------*         
*                                                                               
         USING TIMED,R4                                                         
*                                                                               
GETTIME  NTR1                                                                   
         MVI   PRTFLAG,PRTNGMP                                                  
         L     R2,ACUMAREA                                                      
         USING ACCUMSD,R2                                                       
         MVC   JOBTOTS,PZEROS                                                   
*                                                                               
         LA    R4,BILDAREA                                                      
         XC    BILDAREA,BILDAREA                                                
         XC    LEVBTOT,LEVBTOT                                                  
         XC    LEVATOT,LEVATOT                                                  
         MVI   TIMETYPE,C'2'                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',(0,ADBUFC),(R4),1                          
         TM    DMCB+8,X'80'                                                     
         BO    GETT05              NO DATA IN BUFFALO                           
         CLI   TIMETYPE,C'2'       IS THIS A MP RECORD                          
         BE    GETT06              YES                                          
*                                                                               
GETT05   CLI   PROF50,C'Y'         FORCE TOTALS                                 
         BNE   EXIT                NO, DONE                                     
         MVI   SPACING,1                                                        
         BAS   RE,BLNKPRT          BLANK-OUT P THRU PFOURTH                     
         BAS   RE,SETTHEAD         GENERATE TIME HEADINGS                       
         LA    R3,P                PRIME R3 FOR JTOT                            
         L     R2,ACUMAREA         RESET R2 (CREAMED BY BLNKPRT)                
         B     GETT100             AND PRODUCE ZERO TOTALS !!!                  
*                                                                               
*                                                                               
GETT06   MVI   SPACING,1                                                        
         BAS   RE,BLNKPRT          BLANK-OUT P THRU PFOURTH                     
         BAS   RE,SETTHEAD         GENERATE TIME HEADINGS                       
*                                                                               
         LA    R4,BILDAREA         BUFFALO WORK AREA                            
         MVC   STACK,SPACES                                                     
*                                                                               
         USING PRINTD,R3                                                        
GETT10   DS    0H                                                               
         MVI   BYTE,C'N'           INIT TOTALS INDICATOR                        
         MVI   TIMETOT,C'N'        INIT TOTALS INDICATOR                        
         GOTO1 =A(ISMPTOT),DMCB,TIMEACCT,EMPLEVB,(RC)                           
         BNE   GETT12              NO                                           
*                                                                               
         ST    R1,FULL             SAVE LEN OF ACCNT                            
*                                                                               
         GOTO1 =A(BUMPCNT),DMCB,(RC),EMPLEVA                                    
         BNE   GETT55                                                           
         GOTO1 =A(NEEDBTOT),DMCB,(RC)                                           
         BNE   GETT55                                                           
*                                                                               
         L     R1,FULL             RESTORE LEN OF ACCOUNT                       
*                                                                               
         LA    R2,LEVBTOT                                                       
         B     GETT15                                                           
*                                                                               
GETT12   DS    0H                                                               
         GOTO1 =A(ISMPTOT),DMCB,TIMEACCT,EMPLEVA,(RC)                           
         BNE   GETT20                                                           
         CP    TIMECNT1,=P'1'      FORCED OFFICE TOTALS?                        
         BH    GETT14              YES                                          
         CP    TIMECNT,=P'1'                                                    
         BE    GETT55                                                           
*                                                                               
GETT14   LA    R2,LEVATOT                                                       
GETT15   MVI   TIMETOT,C'Y'        UNDERLINE BEFORE WRITEING TOTS               
         BAS   RE,TIMELINE                                                      
         MVC   P+1(10),=C'TOTALS FOR'                                           
         BCTR  R1,0                R1 RETURNED BY ISMPTOT                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+12(0),TIMEACCT                                                 
         XC    0(2,R2),0(R2)                                                    
*                                                                               
         CLI   PROF49,C'Y'         PRINT 1R ACCOUNTS ON MP SUM                  
         BNE   GETT18                                                           
         GOTO1 =A(GETMPNAM)        SET TIMENAME                                 
         LA    R3,PSECOND                                                       
         GOTO1 CHOPPER,DMCB,(36,TIMENAME),(17,PACCT),(C'P',3)                   
*                                                                               
GETT18   LA    R3,P                                                             
         ZAP   TIMEBAHR,TIMEHOUR   SET BILLABLE HOURS                           
         SP    TIMEHOUR,TIMEWOHR   CALC TOTAL HOURS                             
         ZAP   TIMEBGRS,TIMEGRS    SET BILLABLE GROSS                           
         SP    TIMEGRS,TIMEWOAM    CALC TOTAL GROSS                             
         L     R2,ACUMAREA         RESET R2                                     
         MVC   STACK(14),TIMEACCT  SET PREVIOUS                                 
         B     GETT50A             PRINT BUCKETS                                
*                                                                               
GETT20   L     R2,ACUMAREA         RELOAD R2                                    
         MVC   TOTCHJOB,PZEROS     FIRST TIME FOR ACCOUNT                       
         CLI   PROF24,C'Y'         SUMPRESS MANPOWER SUMMARY                    
         BE    GETT40              SKIP PRINT                                   
         MVI   PROMP,C'Y'          PRODUCE MP SUM AT E-O-PRODUCT                
         LA    R3,P                                                             
         MVC   PACCT(14),TIMEACCT       ACCOUNT CODE                            
         MVC   STACK(14),TIMEACCT                                               
         MVC   STACK+15(36),TIMENAME                                            
         LA    R3,PSECOND                                                       
         GOTO1 CHOPPER,DMCB,(36,STACK+15),(17,PACCT),(C'P',3)                   
         GOTO1 =A(BUMPCNT),DMCB,(RC),EMPLEVB                                    
         LA    R3,P                                                             
*                                                                               
GETT30   MVC   PREFDATE(6),TIMEREF                                              
         LA    R3,PSECOND                                                       
         GOTO1 DATCON,DMCB,(1,TIMEDATE),(8,PREFDATE)                            
         LA    R3,P                                                             
         CLI   PROF44,C'S'         SUPPRESS AMOUNTS                             
         BE    GETT40              Y, SUPPRESS RATE ALSO                        
         EDIT  (P4,TIMERATE),(6,PRATE),2   HOURLY RATE                          
*                                                                               
GETT40   LA    R3,P                                                             
         ZAP   TIMEBAHR,TIMEHOUR   SET BILLABLE HOURS                           
         SP    TIMEHOUR,TIMEWOHR   CALC TOTAL HOURS                             
         ZAP   TIMEBGRS,TIMEGRS    SET BILLABLE GROSS                           
         SP    TIMEGRS,TIMEWOAM    CALC TOTAL GROSS                             
*                                                                               
         LA    RE,TIMEACCS                                                      
         LA    RF,TOTCHJOB         ACCOUNT TOTALS                               
         LA    R1,JOBTOTS          TIME TOTALS                                  
         LA    R0,NTIMEACC                                                      
*                                                                               
GETT50   AP    0(8,RF),0(8,RE)     ADD TO HIGHER LEVELS                         
         AP    0(8,R1),0(8,RE)                                                  
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         LA    R1,8(R1)                                                         
         BCT   R0,GETT50                                                        
*                                                                               
GETT50A  CLI   PROF24,C'Y'         SUPPRESS MANPOWER SUMMARY                    
         BE    GETT55              SKIP PRINT                                   
*                                                                               
         BAS   RE,DTOT             PRINT DETAIL LINE                            
*                                                                               
GETT55   L     R2,ACUMAREA                                                      
         GOTO1 BUFFALO,DMCB,=C'SEQ',(C'2',ADBUFC),(R4),1                        
         TM    DMCB+8,X'80'                                                     
         BO    GETT70                                                           
         CLI   PROF24,C'Y'         SUPPRESS MANPOWER SUMMARY                    
         BE    GETT10              SKIP PRINT                                   
*                                                                               
         CLC   STACK(14),TIMEACCT  HAS ACCOUNT CHANGED                          
         BNE   GETT60              YES                                          
         MVI   BYTE,C'Y'           FLAG NEED EMPLOYEE TOTALS                    
         B     GETT30              AND PRINT THIS TIMESHEET                     
*                                                                               
GETT60   CLI   BYTE,C'Y'           MULTIPLE TIMESHEETS FOR EMPLOYEE             
         BNE   *+8                 MO, NO *TOTALS* LINE                         
         BAS   RE,ATOT                                                          
*                                                                               
         LA    R1,TIMEACCT         IS A 1R TOTAL RECORD COMING                  
         LA    R0,L'TIMEACCT                                                    
         CLI   0(R1),X'FF'                                                      
         BE    GETT65              YES, POSTPONE UNDERLINING                    
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
*                                                                               
         MVI   TIMETOT,C'Y'        UNDERLINE BEFORE A NEW ACCOUNT               
         BAS   RE,TIMELINE                                                      
*                                                                               
GETT65   MVI   BYTE,C'N'                                                        
         B     GETT10              AND PRINT NEXT                               
         EJECT ,                                                                
*                                                                               
*        FINISH UP MANPOWER SUMMARY                                             
*                                                                               
GETT70   CLI   BYTE,C'Y'           ARE ACCOUNT TOTALS NEEDED                    
         BNE   GETT80                                                           
         MVI   TIMETOT,C'N'                                                     
         BAS   RE,ATOT             PRINT THEM                                   
         B     GETT90                                                           
GETT80   BAS   RE,PRINTEM                                                       
*                                                                               
GETT90   MVI   TIMETOT,C'Y'                                                     
         BAS   RE,TIMELINE         DO LINE BEFORE TOTAL                         
         BAS   RE,JTOT             PRINT JOB TOTALS                             
*                                                                               
GETT100  MVI   FORCEHED,C'Y'                                                    
         MVI   PRTFLAG,PRTNGMPT                                                 
         MVC   TIME11(35),SPACES                                                
         MVC   TIME12(35),SPACES                                                
         BAS   RE,JTOT                                                          
         BAS   RE,OTOT             PRINT OUT-OF-POCKET TOTALS                   
         BAS   RE,TTOT                                                          
         BAS   RE,BXBOT                                                         
         B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT ,                                                                
*              ROUTINES TO PRINT A LINE OF ACCUMS FOR TIME SUMMARY              
*                                                                               
*----------------------------------------------------------------------         
*        PRINT MANPOWER TOTALS                                                  
*----------------------------------------------------------------------         
*                                                                               
         USING ACCUMSD,R2                                                       
         USING PRINTD,R3                                                        
         USING LITTABD,RF                                                       
*                                                                               
JTOT     L     RF,=A(LITTAB)                                                    
         MVC   PACCT(17),MPTOTS                                                 
         DROP  RF                                                               
         LA    RF,JOBTOTS                                                       
         MVI   SPACING,2                                                        
         B     ALLTOT                                                           
         DROP  R2,R3                                                            
         SPACE 2                                                                
*----------------------------------------------------------------------         
*        PRINT OUT-OF POCKET TOTALS                                             
*        REFORMAT THE REGULAR ACCUMULATORS TO LOOK LIKE TIME ACCUMS             
*        TOTCHRGS(24) GET GROSS, BILLED, AND BILLABLE                           
*----------------------------------------------------------------------         
*                                                                               
         USING ACCUMSD,R2                                                       
         USING PRINTD,R3                                                        
*                                                                               
OTOT     NTR1                                                                   
         L     R2,ACUMAREA                                                      
         LA    R3,JOBTOTS          TIME CHARGES                                 
         LA    R2,TOTCHRGS         WHERE OOPS CHARGES ARE                       
         USING ACCUMD,R2                                                        
         ZAP   0(8,R2),GRSAC                    GROSS TO ACCUM 1                
         ZAP   8(8,R2),BILLAC                   BILLED TO ACCUM 2               
         MVC   24(16,R2),0(R2)                  GROSS-BILLED TO 4 AND 5         
         SP    0(8,R2),40(8,R3)                 GROSS LESS TIME B-GROSS         
         SP    8(8,R2),48(8,R3)                 BILLED LESS TIME BILLED         
         ZAP   16(8,R2),0(8,R2)                 OOPS GROSS TO ACCUM 3           
         SP    16(8,R2),8(8,R2)                 LESS OOPS BILLED-B'ABLE         
         LA    R3,P                                                             
         USING LITTABD,RF                                                       
         L     RF,=A(LITTAB)                                                    
         MVC   PACCT(17),OOPTOTS                                                
         LR    RF,R2                                                            
         B     OOPPRT                                                           
         DROP  R2,R3,RF                                                         
         SPACE  2                                                               
*----------------------------------------------------------------------         
*        PRINT SUM OF MANPOWER AND OUT-OF-POCKET                                
*----------------------------------------------------------------------         
*                                                                               
         USING ACCUMSD,R2                                                       
         USING PRINTD,R3                                                        
*                                                                               
TTOT     NTR1                                                                   
         L     R2,ACUMAREA                                                      
         ZAP   TOTCHRGS+40(8),TOTCHRGS+24(8)     GROSS                          
         SP    TOTCHRGS+40(8),TOTCHRGS+32(8)     LESS BILLED                    
*                                                                               
         USING LITTABD,RF                                                       
         L     RF,=A(LITTAB)                                                    
         MVC   PACCT(17),TOTCHA                                                 
         DROP  RF                                                               
*                                                                               
         LA    RF,TOTCHRGS+24                    GROSS, BILLED, B'ABLE          
OOPPRT   MVI   SPACING,2                                                        
         LA    R5,PBABGRS                                                       
         LA    R2,3                                                             
         BRAS  RE,OUTTOT                                                        
         BAS   RE,PRINTEM                                                       
         B     EXIT                                                             
*                                                                               
ATOT     LA    RF,TOTCHJOB                                                      
         MVC   PACCT+L'PACCT-8(8),=C'*TOTALS*'                                  
         MVI   SPACING,2                                                        
         B     DTOT2                                                            
*                                                                               
         USING TIMED,R4                                                         
*                                                                               
DTOT     LA    RF,TIMEACCS         DETAIL LINE                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
DTOT2    CLI   PROF44,C'S'         SUPRESS AMOUNTS                              
         BNE   ALLTOT               Y, PRINT DOLLARS                            
         MVC   TIMEGRS-TIMEACCS(5*8,RF),PZEROS NO, NO DOLLAR DETAILS            
*                                                                               
* -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -         
*        PRINT A TIME ACCUM, HOURS, GROSS,B'ED, B'ABLE,WO HRS,WO AMNT           
* -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -         
ALLTOT   NTR1                                                                   
         LA    R5,PHOUR                                                         
         LA    R2,3                THREE HOUR TYPE PRINTS                       
         BAS   RE,HOURED                                                        
ALLT2    LA    R2,5                FIVE DOLLAR TYPE BUCKETS                     
         LA    R5,PGRS                                                          
         BRAS  RE,OUTTOT                                                        
         BAS   RE,PRINTEM                                                       
         BAS   RE,TIMELINE                                                      
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
TIMELINE NTR1                                                                   
         CLI   TIMETOT,C'Y'        DO THEY NEED BOXES                           
         BNE   TIMEL3                                                           
         CLI   PROF28,C'Y'         DO THEY WANT BOXES                           
         BNE   TIMEL3                                                           
         GOTO1 =A(BOXMID)                                                       
         CLI   FORCEHED,C'Y'       BOXMID RETURNED A TOF                        
         BE    TIMEL3                                                           
         BAS   RE,PRINTEM                                                       
TIMEL3   MVI   TIMETOT,C'N'                                                     
         B     EXIT                                                             
         SPACE 2                                                                
* -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -         
*        GENERATE HEADINGS FOR THE TIME SUMMARY                                 
* -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -         
SETTHEAD NTR1                                                                   
         MVC   TIME11,SPACES                                                    
         MVC   TIME12,SPACES                                                    
         L     R2,ATIMESUM         GENERATE HEADINGS                            
         ST    R2,ATABLE           SET ATABLE FOR BXTOP                         
         LA    R3,TIME11                                                        
         LA    R4,TIME12                                                        
         LA    R5,1                                                             
         LA    R2,132                                                           
         GOTO1 =A(GENHEADS),DMCB,(RC),(R2)                                      
         MVI   MYBOXSW,3                                                        
         MVI   RCSUBPRG,6                                                       
*                                                                               
         CLI   MYMODE,ACCLAST      ARE WE DOING THIS FOR LAST ACCOUNT           
         BNE   SETTH01                                                          
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
*                                                                               
SETTH01  MVC   TIME11(35),SPACES                                                
         MVC   TIME12(35),SPACES                                                
         CLI   LINE,46              ENOUGH ROOM ?                               
         BNH   SETTH02              YES                                         
         MVI   FORCEHED,C'Y'        NO                                          
*                                                                               
SETTH02  CLI   FORCEHED,C'Y'                                                    
         BE    EXIT                                                             
*                                                                               
         BAS   RE,BXBOT                                                         
         MVI   MIDSW,1                                                          
         MVI   MYBOXSW,3                                                        
         MVI   RCSUBPRG,6                                                       
         GOTO1 ABXTOP                                                           
         BAS   RE,PRINTEM                                                       
         MVI   FORCEMID,C'Y'                                                    
         MVC   MID1,TIME11                                                      
         MVC   MID2,TIME12                                                      
         BAS   RE,PRINTEM                                                       
         B     EXIT                                                             
         SPACE  2                                                               
HOURED   DS    0H                                                               
         ST    RE,SAVERE                                                        
HRED1    CP    0(8,RF),=P'0'                                                    
         BE    HRED2                                                            
         EDIT  (P8,0(RF)),(8,0(R5)),2,FLOAT=-  HOURS                            
HRED2    LA    RF,8(0,RF)                                                       
         LA    R5,9(0,R5)                                                       
         BCT   R2,HRED1                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT ,                                                                
* -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -         
*        PRINT HIGH LEVEL MP SUMMARIES     R2=A(NEXT HIGHER TIME ACCUM)         
*        KEEP HIGH LEVEL TOTALS            R3=A(THIS TIME BUCKET)               
*                                          R4=A(THIS OOPS BUCKET)               
* -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -         
*                                                                               
         USING ACCUMSD,R2                                                       
*                                                                               
HIGHTIME NTR1                                                                   
         MVI   PRTFLAG,PRTNGMPT                                                 
*                                                                               
         MVC   BYTE,PRTFLAG        IS WHAT WE ARE PRINTING NOW ...              
         NC    BYTE,PRTTYPE        WHAT WE WANT?                                
         BZ    HIGHTX              NOPE                                         
*                                                                               
         LA    R1,NTIMEACC         NUMBER OF TIME ACCUMULATORS                  
         BAS   RE,ADDEM            ADD R3 TO R2                                 
         L     R2,ACUMAREA                                                      
         MVC   JOBTOTS(8*NTIMEACC),0(R3)                                        
         MVC   TOTCHRGS(8*ACCUMNUM),0(R4)                                       
         BAS   RE,SETTHEAD         SET MANPOWER SUMMARY HEADERS                 
         MVC   TIME11(35),SPACES                                                
         MVC   TIME12(35),SPACES                                                
         LA    R3,P                                                             
         BAS   RE,JTOT             PRINT MP TOTS                                
         BAS   RE,OTOT             PRINT OOP TOTS                               
         BAS   RE,TTOT             PRINT SUM OF ABOVE                           
         BAS   RE,BXBOT                                                         
HIGHTX   B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
* -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -         
*        POP IN A BOTTOM AT END OF JOB                                          
* -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -         
*                                                                               
BXBOT    NTR1                                                                   
         MVC   MYROW,SPACES                                                     
         MVI   MYROW+9,C'T'                                                     
         MVI   MYROW+12,C'M'                                                    
         MVI   MYBOXSW,X'FF'       STOP BOXES FROM STARTING ON NXT PG           
         MVI   RCSUBPRG,2          AS WELL AS HEADS                             
         ZIC   RF,LINE                                                          
         LA    RE,MYROW(RF)                                                     
         BCTR  RE,0                                                             
         MVI   0(RE),C'B'                                                       
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXROWS,MYROW                                                    
         MVI   BOXINIT,0                                                        
         BAS   RE,PRINTEM                                                       
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT ,                                                                
* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---           
*        FOR JOBS ON NEW ESTIMATES LIST THEIR PLANNING AND REVISED              
*        ESTIMATES                                                              
* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---           
*                                                                               
PRTEST   NTR1                                                                   
         CLI   SVNEWEST,C'Y'       IS JOB ON NEW ESTIMATES                      
         BNE   EXIT                NO                                           
*                                                                               
         USING LITTABD,RF                                                       
         L     RF,=A(LITTAB)                                                    
         MVC   P+1(10),LITEST                                                   
         DROP  RF                                                               
         GOTO1 =V(UNDERLIN),DMCB,(10,P+1),PSECOND+1                             
         BAS   RE,PRINTEM                                                       
         LA    R4,P+1                                                           
         L     R2,PLANESTS         LIST OF THE PLANNING ESTS ON JOB             
         MVI   BYTE,C'P'                                                        
         BAS   RE,PRTE40                                                        
         L     R2,REVESTS         LIST OF THE REVISION ESTS ON JOB              
         MVI   BYTE,C'R'                                                        
         BAS   RE,PRTE40                                                        
         BAS   RE,COMCLR           CLEAR TRAILING COMMA IF AROUND               
         BAS   RE,PRINTEM                                                       
         B     EXIT                                                             
*                                                                               
PRTE40   NTR1                                                                   
PRTE45   CLI   0(R2),0             LAST OR NONE                                 
         BNE   PRTE46                                                           
         XIT1  REGS=(R4)           SAVE LAST PRINTABLE ADDRESS                  
PRTE46   MVC   0(1,R4),BYTE                                                     
         LA    R4,1(R4)                                                         
         EDIT  (B1,0(R2)),(3,0(R4)),ALIGN=LEFT                                  
         AR    R4,R0               ADD LENGTH OF OUTPUT FIELD                   
         CLI   BYTE,C'R'                                                        
         BNE   PRTE50                                                           
         CLC   0(1,R2),SVHIAPP     IS THIS THE HIGHEST APPROVED EST             
         BNE   PRTE50              NO                                           
         MVI   0(R4),C'*'          YES, STAR IT                                 
         LA    R4,1(R4)                                                         
PRTE50   MVI   0(R4),C','          COMMA AFTER                                  
         LA    R4,2(R4)            SPACE AFTER COMMA                            
*                                                                               
         LA    R2,1(R2)            NEXT EST NUMBER                              
         LA    R1,P                                                             
         LA    R1,105(R1)          LAST PRINTABLE ADDRESS                       
         CR    R4,R1               DO I HAVE ROOM                               
         BL    PRTE45              YES                                          
         BAS   RE,PRINTEM                                                       
         LA    R4,P+1                                                           
         B     PRTE45                                                           
         EJECT ,                                                                
READIO   L     R2,IOSPACE                                                       
         B     CALLDMGR                                                         
READREAD L     R2,DMGRBUFF                                                      
         B     CALLDMGR                                                         
READPRO  L     R2,PROBUFF                                                       
*                                                                               
CALLDMGR NTR1                                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',MYKEY,(R2)                       
         B     EXIT                                                             
         EJECT ,                                                                
*                                                                               
         GETEL R2,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
*        THESE EQUATES ARE TESTED FOR IN THE SMTBSTAT FIELD                     
NOBUF    EQU   X'01'                                                            
NOBAL    EQU   X'02'                                                            
NOBILL   EQU   X'04'                                                            
NOACCUM  EQU   X'08'                                                            
*                                                                               
         EJECT ,                                                                
* OPTION TABLES  (OPTTAB AND SUMTAB LENGTHS MUST BE THE SAME)                   
*                                                                               
* BYTE 1 PRINT THIS OR NOT , Y OR N                                             
* BYTE 2 LENGTH OF DATA OR HEADLINE INFO (GREATER OF THESE TWO)                 
* BYTE 3 POSITION IN RELATION TO P+1                                            
* BYTES 4-13 MIDLINE1 VALUE                                                     
* BYTES 14-23 MIDLINE2 VALUE                                                    
* BYTES 24-27 PRINT SWITCHES (JOB,PROD,CLI,REQ)                                 
* BYTE 28 FIELD NUMBER                                                          
* BYTE 29 FIELD STATUS                                                          
*                                                                               
*                                                                               
* THE DATA IN SUMTAB IS FOUND AS SUMDATA BELOW                                  
*                                                                               
*********************************************************************           
*        IF CHANGING THE ORDER OF THE ABOVE COLUMNS YOU MUST        *           
*        CHANGE MENUTAB AND THE WIDTH TABLES ON THE NEXT PAGE.      *           
*********************************************************************           
         EJECT ,                                                                
* SUMMARY MENU TABLE                                                            
*                                                                               
* BYTE 1 MENU OPTION (FROM PROFILE NUMBER 16)                                   
* BYTES 1-12 PRINT THIS COLUMN IN SUMTAB OR NOT (Y OR N)                        
*                                                                               
** CAREFUL - THIS TABLE IS VERY DEPENDENT ON ORDER OF COLS. IN SUMTAB.          
*                                                                               
MENUTAB  DS    0CL(NCOLS+1)                                                     
*                                                                               
*        THE ACTUAL TABLE HAS BEEN MOVED BELOW                                  
*                                                                               
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*        PRINT THE BUCKETS AT RF, AT PRINT LOCATION R5, R2 # OF TIMES *         
***********************************************************************         
OUTTOT   NTR1  BASE=*,LABEL=*                                                   
OUT1     CP    0(8,RF),=P'0'                                                    
         BE    OUT2                                                             
         EDIT  (P8,0(RF)),(13,0(R5)),2,FLOAT=-                                  
OUT2     LA    RF,8(0,RF)                                                       
         LA    R5,14(0,R5)                                                      
         BCT   R2,OUT1                                                          
         XIT1                                                                   
         EJECT                                                                  
*        SUMTAB COVERED AS SUMTABD X SUMMARYD                                   
SUMTAB   DS    0CL30                                                            
SUMDATA  DS    0C                                                               
         DC    C'N',AL1(0),AL1(0),CL20'  ORIGINAL  ESTIMATE',C'NNNNN'           
         DC    AL1(1),AL1(NOBAL+NOBILL)                                         
         DC    C'N',AL1(0),AL1(0),CL20'  CURRENT   REVISION',C'NNNNN'           
         DC    AL1(15),AL1(NOBAL+NOBILL)                                        
         DC    C'N',AL1(0),AL1(0),CL20'   HIGHEST   GRS REV',C'NNNNN'           
         DC    AL1(16),AL1(NOBAL+NOBILL)                                        
         DC    C'N',AL1(0),AL1(0),CL20'  PRESENT   ESTIMATE',C'NNNNN'           
         DC    AL1(2),AL1(NOBAL+NOBILL)                                         
         DC    C'N',AL1(0),AL1(0),CL20' ESTIMATE COMMISSION',C'NNNNN'           
         DC    AL1(11),AL1(NOBAL+NOBILL+NOBUF)                                  
         DC    C'N',AL1(0),AL1(0),CL20'  ESTIMATE    GROSS ',C'NNNNN'           
         DC    AL1(12),AL1(NOBAL+NOBILL+NOBUF)                                  
         DC    C'N',AL1(0),AL1(0),CL20'   ORDER     AMOUNT ',C'NNNNN'           
         DC    AL1(3),X'00'                                                     
         DC    C'N',AL1(0),AL1(0),CL20'     NET            ',C'NNNNN'           
         DC    AL1(5),X'00'                                                     
         DC    C'N',AL1(0),AL1(0),CL20' OVER(+)/  UNDER AMT',C'NNNNN'           
         DC    AL1(4),AL1(NOBAL+NOBILL+NOBUF+NOACCUM)                           
         DC    C'N',AL1(0),AL1(0),CL20' OVER(+)/  UNDER PCT',C'NNNNN'           
         DC    AL1(13),AL1(NOBAL+NOBILL+NOBUF+NOACCUM)                          
         DC    C'N',AL1(0),AL1(0),CL20'COMMISSION          ',C'NNNNN'           
         DC    AL1(6),X'00'                                                     
         DC    C'N',AL1(0),AL1(0),CL20'    GROSS           ',C'NNNNN'           
         DC    AL1(7),X'00'                                                     
         DC    C'N',AL1(0),AL1(0),CL20'OVR(+)/UNDGROSS AMT ',C'NNNNN'           
         DC    AL1(14),AL1(NOBAL+NOBILL+NOBUF+NOACCUM)                          
         DC    C'N',AL1(0),AL1(0),CL20'   CASH    DISCOUNT ',C'NNNNN'           
         DC    AL1(8),X'00'                                                     
         DC    C'N',AL1(0),AL1(0),CL20'    BILLED    AMOUNT',C'NNNNN'           
         DC    AL1(9),AL1(NOBAL)                                                
         DC    C'N',AL1(0),AL1(0),CL20'  UNBILLED  CHARGES ',C'NNNNN'           
         DC    AL1(10),AL1(NOBAL+NOBILL)                                        
         DC    C'N',AL1(0),AL1(0),CL20' HIGHEST   REVISION ',C'NNNNN'           
         DC    AL1(17),AL1(NOBAL+NOBILL+NOBUF)                                  
         DC    AL1(0)                                                           
         EJECT ,                                                                
*----------------------------------------------------------------------         
* PROCESS OPTION 5                                                              
* 1) REJECT TRANSACTIONS IF THEY ARE NOT WANTED ON THE REPORT                   
* 2) SET PARTIAL AMOUNTS IF ONLY PART OF THE TRANSACTION IS WANTED              
* RETURNS: TRANSTAT SET TO REJECT TO REJECT TRANSACTION                         
*          TRANSACTION BUCKETS ADJUSTED FOR THE PARTIAL AMOUNTS                 
*---------------------------------------------------------------------          
*                                                                               
         DS    0H                                                               
PROCOP5  LR    R1,RC                                                            
         NMOD1 0,**OPT5**                                                       
         LR    RC,R1                                                            
         CLI   QOPT5,C'S'          SUPPRESS BILLED ITEMS                        
         BNE   PO30                                                             
*                                                                               
         CLI   BILLED,C'Y'         Q, FULLY BILLED                              
         BE    PO60                 Y, REJECT                                   
         CLI   WROFFED,C'Y'        Q, FULLY WRITTEN OFF?                        
         BE    PO60                 Y, REJECT                                   
         TM    TRANSTAT,WRITEOFF   Q, TYPE 57 TRANSACTION                       
         BO    PO60                 Y, REJECT                                   
         B     PO100                N, TRAN OK FOR NOW (MAY BE PARTIAL)         
*                                                                               
PO30     CLI   QOPT5,C'B'          BILLED ITEMS ONLY                            
         BNE   PO40                                                             
         CLI   WROFFED,C'P'        PARTIALLY MARKED                             
         BE    PO100               YES, PASS ALONG                              
         CLI   BILLED,C'P'                                                      
         BE    PO100                                                            
*                                                                               
         CLI   WROFFED,C'Y'        FULLY WRITTEN OFF                            
         BE    PO100                Y, CONSIDER BILLED                          
         CLI   BILLED,C'N'         Q, NOT BILLED AT ALL                         
         BE    PO60                 Y, REJECT                                   
         B     PO100                N, TRAN IS  OK FOR NOW                      
*                                                                               
PO40     CLI   QOPT5,C'A'          ALLOCATED ONLY                               
         BNE   PO50                NO                                           
*                                                                               
         USING SORTD,R3                                                         
         L     R3,SORTAREA                                                      
         CLC   SRTWC,=C'99'        IS THIS A BILL                               
         BE    PO60                YES, REJECT                                  
*                                                                               
         MVI   ELCODE,X'FF'        GET PRORATA AMOUNTS ELEMENT                  
         BAS   RE,GETSRA                                                        
         USING SRAMTSD,R2                                                       
         TM    SRASTAT3,PG$FULUT   TRAN IS FULLY UTILIZED                       
         BO    PO60                YES, REJECT                                  
         CP    SRAALLOC,=P'0'      ANY ALLOCATED                                
         BNE   PO100               YES, ACCEPT TRANSACTION                      
         B     PO60                                                             
*                                                                               
PO50     CLI   QOPT5,C'U'          UNALLOCATED ONLY                             
         BNE   POX                 NO, ACCEPT TRANSACTION                       
*                                                                               
         USING SORTD,R3                                                         
         L     R3,SORTAREA                                                      
         CLC   SRTWC,=C'99'        IS THIS A BILL                               
         BE    PO60                YES, REJECT                                  
*                                                                               
         MVI   ELCODE,X'FF'        GET PRORATA AMOUNTS ELEMENT                  
         BAS   RE,GETSRA                                                        
         USING SRAMTSD,R2                                                       
         TM    SRASTAT3,PG$FULUT   TRAN IS FULLY UTILIZED                       
         BO    PO60                YES, REJECT, CANT HAVE ANY UNALL'ED          
         LA    R1,SRANET                                                        
         BAS   RE,CALCUNAL                                                      
         CP    DUB,=P'0'                                                        
         BNE   PO100               UNALLOCATED AMOUNT                           
         B     PO60                                                             
*                                                                               
*                                                                               
PO60     DS    0H                  REJECT THE TRANSACTION                       
         OI    TRANSTAT,REJECT                                                  
         B     POX                                                              
*                                                                               
*---------------------------------------------------------------------*         
*        ADJUST DOLLAR AMOUNTS ON PARTIALY ITEMS                      *         
*---------------------------------------------------------------------*         
*                                                                               
*                                                                               
PO100    CLI   QOPT5,C'B'          Q, BILLED ITEMS ONLY                         
         BNE   PO140                NO                                          
*                                  USE BILLED NUMBERS IN TRAN                   
         CLI   BILLED,C'P'         IS THIS PARTIAL BILLED                       
         BE    *+12                YES, ADJUST                                  
         CLI   WROFFED,C'P'        IS THIS PARTIAL WRITTEN OFF                  
         BNE   POX                 NO, DONT ADJUST                              
*                                  USE BILLED NUMBERS IN TRAN                   
         MVI   BYTE,C'Z'                                                        
         GOTO1 =A(SAVEAMNT)                                                     
*                                                                               
         ZAP   NETAMT,NETBLAMT     NET BECOMES NET BILLED                       
         BAS   RE,POCOM            PUT COMMISSION INTO COMAMT                   
         ZAP   GRSAMT,NETAMT       GROSS IS GROSS BILLED                        
         AP    GRSAMT,COMAMT       NEW GROSS                                    
         AP    NETAMT,WOAMNT                                                    
         AP    GRSAMT,WOAMNT                                                    
         ZAP   CDAMT,BLDCDAMT      CD IS BILLED CD                              
         ZAP   SVHOUR,BLDHRS       HOURS ARE NOW BILLED HOURS                   
         AP    SVHOUR,WOHRS                                                     
         ZAP   DUBABLE,=P'0'       BILLABLE AMOUNT IS ZERO                      
         ZAP   WOAMNT,=P'0'        W/O AMT IS ZERO                              
         ZAP   WOHRS,=P'0'         W/O HOURS                                    
         B     POX                                                              
*                                                                               
PO140    DS    0H                                                               
         CLI   QOPT5,C'S'          Q, SUPPRESSING BILLED ITEMS                  
         BNE   PO160                N,                                          
*                                                                               
         CLI   BILLED,C'P'         IS THIS PARTIAL BILLED                       
         BE    *+12                YES, ADJUST                                  
         CLI   WROFFED,C'P'        IS THIS PARTIAL WRITTEN OFF                  
         BNE   POX                 NO, DONT ADJUST                              
*                                  USE BILLED NUMBERS IN TRAN                   
         MVI   BYTE,C'Z'                                                        
         GOTO1 =A(SAVEAMNT)                                                     
*                                                                               
         ZAP   BLDAMT,=P'0'        SUPPRESSING BILLED AMOUNT                    
         SP    NETAMT,NETBLAMT     TRAN NET LESS BILLED NET--EL4B               
         SP    NETAMT,WOAMNT       LESS WHAT WAS WRITTEN OFF                    
         BAS   RE,POCOM                                                         
         ZAP   GRSAMT,NETAMT                                                    
         AP    GRSAMT,COMAMT       GROSS                                        
         SP    CDAMT,BLDCDAMT      TRAN CD-AMT LESS BILLED CD-AMT-EL4B          
         SP    SVHOUR,BLDHRS       EL40 HOURS LESS EL4B HOURS                   
         SP    SVHOUR,WOHRS        LESS HOURS WRITTEN OFF                       
         B     POX                                                              
*                                                                               
PO160    DS    0H                                                               
         CLI   QOPT5,C'A'          Q, ALLOCATED ONLY                            
         BNE   PO180                N,                                          
*                                                                               
         BAS   RE,GETSRA                                                        
         USING SRAMTSD,R2                                                       
         ZAP   BLDAMT,=P'0'        SUPPRESSING BILLED AMOUNT                    
         ZAP   BLDHRS,=P'0'        AND HOURS                                    
         ZAP   WOHRS,=P'0'                                                      
         ZAP   WOAMNT,=P'0'                                                     
*                                                                               
         ZAP   NETAMT,SRAALLOC     NET BECOMES ALLOCATED                        
         BAS   RE,POCOM                                                         
         ZAP   GRSAMT,NETAMT                                                    
         AP    GRSAMT,COMAMT       GROSS IS ALLOCATED GROSS                     
         ZAP   DUBABLE,GRSAMT                                                   
         ZAP   CDAMT,SRAA_CD       ALLOCATED CD                                 
         ZAP   SVHOUR,SRAA_HR      ALLOCATED HOURS                              
         B     POX                                                              
*                                                                               
PO180    DS    0H                                                               
         CLI   QOPT5,C'U'          Q, UNALLOCATED ONLY                          
         BNE   POX                  N,                                          
         MVI   BYTE,C'Z'                                                        
         GOTO1 =A(SAVEAMNT)                                                     
*                                                                               
         MVI   ELCODE,X'FF'        GET PRORATA AMOUNTS ELEMENT                  
         BAS   RE,GETSRA                                                        
         USING SRAMTSD,R2                                                       
         ZAP   BLDAMT,=P'0'                                                     
         ZAP   BLDHRS,=P'0'                                                     
         ZAP   WOHRS,=P'0'                                                      
         ZAP   WOAMNT,=P'0'                                                     
*                                                                               
         LA    R1,SRANET                                                        
         BAS   RE,CALCUNAL                                                      
         ZAP   NETAMT,DUB          NET AND BILLABLE BECOME UNALLOC'ED           
*                                                                               
         LA    R1,SRAN_CD                                                       
         BAS   RE,CALCUNAL                                                      
         ZAP   CDAMT,DUB           UNALLOCATED CD                               
*                                                                               
         BAS   RE,POCOM                                                         
         ZAP   GRSAMT,NETAMT                                                    
         AP    GRSAMT,COMAMT       GROSS IS ALLOCATED GROSS                     
         ZAP   DUBABLE,GRSAMT                                                   
*                                                                               
         LA    R1,SRAN_HR                                                       
         BAS   RE,CALCUNAL                                                      
         ZAP   SVHOUR,DUB          UNALLOCATED HOURS                            
*                                                                               
P0200    DS    0H                                                               
POX      XIT1                                                                   
         DROP   R2,R3                                                           
         SPACE  2                                                               
*----------------------------------------------------------------------         
*SET COMAMT, BASED ON NETAMT, COMMISSIONABLE STATUS AND BUFPCT                  
*----------------------------------------------------------------------         
*                                                                               
         USING SORTD,R3                                                         
         USING SR44D,R2                                                         
*                                                                               
POCOM    DS    0H                  CALC COMMISION INTO COMAMT                   
         L     R3,SORTAREA                                                      
         ST    R2,SAVER1                                                        
         LA    R2,SRTDATA                                                       
         ZAP   COMAMT,=P'0'                                                     
         TM    SR44STAT,X'01'      NON COMMISSIONABLE                           
         BO    POCX                COM IS ZERP                                  
         ZAP   PL16,NETAMT                                                      
         MP    PL16,BUFPCT         CALCULATE COMM. AND GROSS                    
         SRP   PL16,64-6,5         RATE IS FOUR DECIMAL PLACES                  
         ZAP   COMAMT,PL16         COMMISSION                                   
*                                                                               
POCX     L     R2,SAVER1                                                        
         BR    RE                                                               
         DROP  R2,R3                                                            
         SPACE  2                                                               
*----------------------------------------------------------------------         
* SET R2 TO THE A(SRAMTD ELEMENT ON SRTDATA)                                    
* ASSUMES R3 IS A(SORTAREA)                                                     
*----------------------------------------------------------------------         
*                                                                               
         USING SORTD,R3                                                         
*                                                                               
GETSRA   DS    0H                                                               
         LA    R2,SRTDATA                                                       
GETSRA10 CLI   0(R2),X'FF'         GOT IT                                       
         BER   RE                  YES, EXIT                                    
         CLI   0(R2),0             EOR                                          
         BNE   *+6                 YES, DIE                                     
         DC    H'0'                                                             
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     GETSRA10                                                         
         DROP  R3                                                               
         SPACE  2                                                               
*----------------------------------------------------------------------         
* BASED ON SRAMTD, CALC UNALLOCATED BASED ON THE BUCKETS AT 0(R1)               
*----------------------------------------------------------------------         
*                                                                               
CALCUNAL ZAP   DUB,0(6,R1)         FIRST BUCKET IS NET                          
         SP    DUB,6(6,R1)         LESS ALLOCATED                               
         SP    DUB,12(6,R1)        LESS BILLED                                  
         SP    DUB,18(6,R1)        LESS XFERED                                  
         SP    DUB,24(6,R1)        LESS WRITTEM OFF                             
         BR    RE                                                               
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        SAVE/RESTORE A PARTIALY BILLED TRASACTION                              
*         SAVE - ZAPS NETAMT-BLDHRS, DUBABLE,WOAMNT,WOHRS AND SVHOUR            
*              INTO TRANSAVE. THE BILLED OR BILLABLE PORTION OF THE             
*              TRANSACTION IS THEN ADJUSTED AND REPORTED ON.                    
*      RESTORE - SUBTRACTS THE ADJUSTED AMOUNTS FROM THE SAVED AMOUNTS          
*              THE DIFFRENCE IS ADDED TO THE WC SUM SO THAT FULL                
*              SUMMARY AMOUNTS ARE REPORTED.                                    
*----------------------------------------------------------------------         
*                                                                               
SAVEAMNT LR    R1,RC                                                            
         NMOD1 0,**SVAM**                                                       
         LR    RC,R1                                                            
         CLI   PROF18,C'Y'         FULL WC SUMM?                                
         BNE   SAVEAX              NO, DONT BOTHER WITH THIS                    
         OI    TRANSTAT,PART                                                    
         LA    R1,5                                                             
         LA    R3,NETAMT           SAVE NETAMT THRU BILLED AMOUNT               
         L     R2,TRANSAVE                                                      
         LA    RF,ZAPSA                                                         
         MVI   SAVEAEX,X'F8'       MAKE IT ZAP                                  
         CLI   BYTE,C'Z'           CALLED TO ZAP                                
         BE    SAVEA40             YES                                          
         LA    RF,SUBSA            NO, CALLED TO SUTRACT                        
         MVI   SAVEAEX,X'FB'       MAKE IT A SP                                 
SAVEA40  BASR  RE,RF                                                            
         LA    R2,40(R2)                                                        
         LA    R3,BLDCDAMT         BILLED CD                                    
         EX    0,SAVEAEX                                                        
*                                                                               
         LA    R2,8(R2)                                                         
         LA    R3,BLDHRS           BILLED HOURS                                 
         EX    0,SAVEAEX                                                        
*                                                                               
         LA    R2,8(R2)                                                         
         LA    R3,DUBABLE          BILLABLE                                     
         EX    0,SAVEAEX                                                        
*                                                                               
         LA    R2,8(R2)                                                         
         LA    R3,WOAMNT           WRITE OFF AMOUNT                             
         EX    0,SAVEAEX                                                        
*                                                                               
         LA    R2,8(R2)                                                         
         LA    R3,WOHRS            WRITE OFF HOURS                              
         EX    0,SAVEAEX                                                        
*                                                                               
         LA    R2,8(R2)                                                         
         LA    R3,SVHOUR           TOTAL HOURS                                  
         EX    0,SAVEAEX                                                        
SAVEAX   XIT1                                                                   
*                                                                               
SAVEAEX  ZAP   0(8,R2),0(8,R3)     NOTE ZAP MODIFIED DURING EXECUTION           
*                                                                               
*----------------------------------------------------------------------         
*        ADD DUBS AT R3 TO R2,R1 NUMBER OF TIMES                                
*----------------------------------------------------------------------         
ADDSA    NTR1                                                                   
ADDSA01  AP    0(8,R2),0(8,R3)                                                  
         LA    R2,8(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R1,ADDSA01                                                       
         MVI   ADDSA01,X'FA'     IN CASE I WAS CALLED FROM SUBEM                
         XIT1                                                                   
*                                                                               
SUBSA    MVI   ADDSA01,X'FB'     MAKE THE AP AN SP                              
         B     ADDSA                                                            
*                                                                               
ZAPSA    MVI   ADDSA01,X'F8'       MAKE THE AP AN ZAP                           
         B     ADDSA                                                            
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
* R4 ADDRESSES AN EMPLOYEE ACCOUNT TIMEREC                                      
* GET THE  TOTAL RECORD FOR THIS AND BUMP TIMECNT, THEN RESTORE                 
* THE BUFFALO READ SEQUENCE, P2 IS THE LEVEL                                    
*---------------------------------------------------------------------          
*                                                                               
BUMPCNT  NMOD1 BUFRECL,**BMPC**                                                 
         LR    R7,RC               SET R7 TO WORK AREA                          
         USING TIMED,R4                                                         
         CLC   TIMEACCT(2),=C'1R'                                               
         BNE   BUMPCNOX                                                         
         DROP  R4                                                               
         L     RC,0(,R1)                                                        
         L     R5,4(,R1)           LEVEL IN R5                                  
         USING TIMED,R7                                                         
         MVC   0(BUFRECL,R7),0(R4)                                              
         GOTO1 =A(SETMPTOT),DMCB,TIMEACCT,(R5),(RC) SET TOTAL KEY               
         XC    TIMETY48(TIMENAME-TIMETY48),TIMETY48 CLEAR DATA                  
         MVC   TIMENAME,SPACES                                                  
*                                                                               
*        GET THE TOTAL RECORD FROM BUFFALO                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',(0,ADBUFC),(R7),1                          
         TM    DMCB+8,X'80'        ANY TOTAL RECORD                             
         BO    BUMPCNO             NO, MAY HAVE WASHED OUT OF BUFFALO           
         CLI   TIMETYPE,C'2'                                                    
         BNE   BUMPCNO                                                          
         MVC   TIMEACCS,PZEROS                                                  
         ZAP   TIMECNT,=P'1'       BUMP COUNT FOR TOTALS                        
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,(R7)                                 
         B     BUMPCYES                                                         
*                                                                               
BUMPCNO  GOTO1 =A(RESTTIME)        RESTORE BUFF FROM R4                         
BUMPCNOX CR    R1,RB                                                            
         B     BUMPCX                                                           
*                                                                               
BUMPCYES GOTO1 =A(RESTTIME)        RESTORE BUFF FROM R4                         
         CR    RB,RB                                                            
BUMPCX   XIT1                                                                   
         DROP  R7                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
* R4 ADDRESSES A LEVELB TOTAL FOR THE MP SUMMARY                                
* SUPPRESS PRINTING (RETURN NEQ) OF THE TIME REC IF:                            
* TIMECNT=1 OR                                                                  
* THE SUBSEQUENT BUFFALO RECORD IS A OFFICE TOTAL WITH A TIMECNT                
* THE SAME AS THE CURRENT TIMEREC                                               
* RESTORE READ SEQUENCE ON XITING                                               
*---------------------------------------------------------------------          
*                                                                               
         USING TIMED,R4                                                         
*                                                                               
NEEDBTOT NMOD1 BUFRECL,**NBTO**                                                 
         LR    R7,RC               SET R7 TO WORK AREA                          
         L     RC,0(,R1)                                                        
         CP    TIMECNT,=P'1'       ARE THERE MULTIPLE RECORDS IN THIS           
         BE    NEEDBNO             NO                                           
*                                                                               
         MVC   0(BUFRECL,R7),TIMEKEY       GET NEXT RECORD                      
         MVI   TIMETY48-TIMEKEY(R7),X'FF'                                       
         GOTO1 BUFFALO,DMCB,=C'HIGH',(0,ADBUFC),(R7),1                          
         TM    DMCB+8,X'80'        ANY MORE RECORDS                             
         BO    NEEDBERR            HAS TO BE                                    
         CLI   TIMETYPE-TIMEKEY(R7),C'2'                                        
         BNE   NEEDBERR                                                         
*                                  IS THIS AN OFFICE TOTAL                      
         LA    R2,TIMEACCT-TIMEKEY(R7)                                          
         GOTO1 =A(ISMPTOT),DMCB,(R2),EMPLEVA,(RC)                               
         BNE   NEEDBYES            NO, PRINT THIS DEPT TOT                      
*                                                                               
* IS THERE MORE THAN 1 LEVEL B IN THIS LEVELA                                   
*                                                                               
         CP    TIMECNT-TIMEKEY(L'TIMECNT,R7),=P'1'                              
         BH    NEEDBYES            YES,PRINT THIS DEPT TOTAL                    
*                                                                               
*SINCE WE ARE SUPPRESSING THIS DEPT TOT, SET TIMECNT1 IN THE OFFICE             
* TOTAL INCASE THERE IS ONLY 1 DEP HERE IF TIMECNT GT 1, OFF TOT WILL           
* PRINT                                                                         
*                                                                               
         MVC   TIMEACCS-TIMEKEY(L'TIMEACCS,R7),PZEROS                           
         ZAP   TIMECNT1-TIMEKEY(L'TIMECNT1,R7),TIMECNT                          
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,(R7)                                 
*                                                                               
         GOTO1 =A(RESTTIME)        RESTORE BUFFALO'S READSEQ                    
NEEDBNO  CR    R1,RB                                                            
         B     NEEDBX                                                           
*                                                                               
NEEDBYES GOTO1 =A(RESTTIME)        RESTORE BUFFALO'S READSEQ                    
         CR    RB,RB                                                            
NEEDBX   XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
*------------------------------------------------------------------*            
* RESTORE BUFFALOS READ SEQ TO THE RECORD IN 0(R4)                              
*------------------------------------------------------------------*            
*                                                                               
         USING TIMED,R4                                                         
*                                                                               
RESTTIME LR    R1,RC                                                            
         NMOD1 0,**RETI**                                                       
         LR    RC,R1                                                            
         GOTO1 BUFFALO,DMCB,=C'HIGH',(0,ADBUFC),(R4),1                          
         TM    DMCB+8,X'80'        ANY MORE RECORDS                             
         BO    NEEDBERR            HAS TO BE                                    
         CLI   TIMETYPE,C'2'                                                    
         BNE   NEEDBERR            HAS TO BE                                    
         XIT1                                                                   
         DROP  R4                                                               
*                                                                               
NEEDBERR MVC   P+1(17),=C'ERROR IN NEEDBTOT'                                    
         GOTO1 ACREPORT                                                         
         DC    H'0'                                                             
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
* SET THE 14 BYTE ACCOUNT IN P1 SO THAT THE NON SIGNIFICANT LEVELS WILL         
* BE SET TO X'FF'S, LEVEL IS IN P2, P3 IS RC                                    
*---------------------------------------------------------------------          
*                                                                               
SETMPTOT NMOD1 4,**MPTO**          4 DOUBLES WORKING STORAGE                    
         LR    R7,RC               SET R7 TO WORK AREA                          
         L     RC,8(,R1)                                                        
         L     R3,0(,R1)           A(ACCOUNT TO SET)                            
         LA    R3,2(,R3)           ADDRESS LEVEL A IN ACCOUNT                   
         L     R4,4(,R1)           LEVEL YOU WISH TO PRODUCE TOTALS FOR         
*                                                                               
         MVI   0(R7),X'FF'         INIT WORK AREA                               
         MVC   1(L'TIMEACCT-1,R7),0(R7)                                         
*                                                                               
         LA    R5,EMPLEVD          START WITH LEVEL D                           
SMPT20   CR    R5,R4                                                            
         BNH   SMPTX               WHATS IN R4?? LEVEL E??                      
         STC   R5,EMPLEVEL                                                      
         BAS   RE,SET1ROFF         SET R2 TO OFFSET, R1 TO LENGTH               
*                                                                               
         LA    R6,0(R2,R3)         ADDRESS LEVEL DATA                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R7)       MOVE IN THE X'FF''S                          
         BCT   R5,SMPT20                                                        
*                                                                               
SMPTX    XIT1                                                                   
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
* PASSED THE TIMED RECORD IN 0(R4), SET TIMENAME TO THE NAME OF THE             
* ACCOUNT IN TIMEACC                                                            
* RETURNS THE LENGTH OF THE ACCOUNT IN R1                                       
*---------------------------------------------------------------------          
*                                                                               
         USING TIMED,R4                                                         
         USING MPNAWKD,R9                                                       
*                                                                               
GETMPNAM LR    R1,RC                                                            
         NMOD1 MPNAWKLN,**MPNA**                                                
         LR    R9,RC               SET R7 TO WORK AREA                          
         LR    RC,R1               AND RESTORE RC                               
         BAS   RE,MPSETKEY         SET TSAR KEY FROM TIMEACCT                   
         BAS   RE,LOOKTSAR         SEE IF NAME IS IN TSAR                       
         BE    MP50                YES, SET TIMENAME                            
         BAS   RE,MPSETKEY         RESET TSAR KEY FROM TIMEACCT                 
         BAS   RE,MPDMREAD         READ 1R ACCOUNT                              
         BAS   RE,PUTTSAR          WRITE RECORD TO TSAR                         
*                                                                               
MP50     MVC   TIMENAME,MPTSNAME                                                
MPX      XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
*                                                                               
         USING TIMED,R4                                                         
*                                                                               
MPSETKEY NTR1                                                                   
         MVC   MPTSKEY,TIMEACCT                                                 
         LA    R2,MPTSKEY                                                       
         LA    R0,L'MPTSKEY                                                     
MPSK20   CLI   0(R2),X'FF'                                                      
         BNE   *+8                                                              
         MVI   0(R2),C' '                                                       
         LA    R2,1(R2)                                                         
         BCT   R0,MPSK20                                                        
         B     MPX                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*                                                                               
         USING TSARD,R5                                                         
*                                                                               
LOOKTSAR NTR1                                                                   
         LA    R5,TSAREA                                                        
         MVI   TSOFFACT,TSARDH                                                  
         LA    RE,MPTSREC                                                       
         ST    RE,TSAREC                                                        
         GOTO1 ATSAROFF,(R5)                                                    
         TM    TSERRS,TSERNF                                                    
         BO    LTSN                                                             
         CR    R5,R5                                                            
         B     LTSX                                                             
LTSN     LTR   R5,R5                                                            
LTSX     B     MPX                                                              
         DROP  R5                                                               
         SPACE 2                                                                
*                                                                               
         USING TSARD,R5                                                         
*                                                                               
PUTTSAR  NTR1                                                                   
         LA    R5,TSAREA                                                        
         MVI   TSOFFACT,TSAADD                                                  
         LA    RE,MPTSREC                                                       
         ST    RE,TSAREC                                                        
         GOTO1 ATSAROFF,(R5)                                                    
         CLI   TSERRS,0                                                         
         BE    MPX                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
         SPACE 2                                                                
MPDMREAD NTR1  WORK=(R2,12)        READ 1R ACCOUNT                              
         XC    MPKEY,MPKEY                                                      
         USING ACTRECD,R6                                                       
         LA    R6,MPKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL                                                 
         MVC   ACTKULA,MPTSKEY                                                  
         GOTO1 DATAMGR,DMCB,(X'08',=CL8'DMREAD'),=CL8'ACCDIR',(R6),(R6)         
         TM    DMCB+8,X'10'        WAS RECORD NOT FOUND                         
         BO    MPDM50              YES                                          
*                                                                               
         LA    R3,ACTKDA           ADDRESS DISK ADDRESS FOR GETREC              
         GOTO1 DATAMGR,DMCB,=C'GETREC  ',=C'ACCMST ',(R3),MPIO,(R2)             
         LA    R6,MPIO                                                          
         LA    R6,ACTRFST                                                       
*                                                                               
MPDM20   CLI   0(R6),0             EOR                                          
         BNE   *+6                                                              
         DC    H'0'                YES, NO NAMEL                                
*                                                                               
         CLI   0(R6),X'20'                                                      
         BE    MPDM30                                                           
         ZIC   R1,1(R6)                                                         
         LA    R6,0(R1,R6)                                                      
         B     MPDM20                                                           
*                                                                               
         USING NAMELD,R6                                                        
MPDM30   MVC   MPTSNAME,SPACES                                                  
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MPTSNAME(0),NAMEREC                                              
         B     MPDMX                                                            
*                                                                               
MPDM50   MVC   MPTSNAME,SPACES                                                  
         MVC   MPTSNAME(22),=C'** RECORD NOT FOUND **'                          
*                                                                               
MPDMX    B     MPX                                                              
         DROP  R6,R9                                                            
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*                                                                               
         USING AC67D,RC                                                         
*                                                                               
TSARINIT LR    R1,RC                                                            
         NMOD1 0,**TSIN**                                                       
         LR    RC,R1                                                            
         MVC   DUB,=CL8'T00A7D'    LOAD IN TSAROFF                              
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    DMCB+4(4),DMCB+4    TEST IF LOADED                               
         BNZ   *+6                                                              
         DC    H'0'                NOT LOADED DIE                               
         MVC   ATSAROFF,4(R1)      SAVE A(TSAROFF)                              
         L     R0,=A(MPBFSZ)      ACQUIRE TABLE ABOVE 16M LINE                  
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF               DID WE GET STORAGE?                          
         BZ    *+6                                                              
         DC    H'0'                NO, SO DIE                                   
*                                                                               
         USING TSARD,R5                                                         
         XC    TSAREA,TSAREA                                                    
         LA    R5,TSAREA                                                        
         MVI   TSOFFACT,TSAINI                                                  
         ST    R1,TSABUF                                                        
         MVC   TSAREC,=A(MPBFSZ)                                                
         MVI   TSKEYL,L'MPTSKEY                                                 
         MVC   TSRECL,=Y(MPTSRECL)                                              
         MVC   TSACOM,ADCOMFAC                                                  
         GOTO1 ATSAROFF,(R5)                                                    
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
* IS THE 14 BYTE ACCOUNT IN P1 A TOTAL FOR THE LEVEL IN P2                      
* RC IS PASSED IN P3                                                            
* RETURNS THE LENGTH OF THE ACCOUNT IN R1                                       
*---------------------------------------------------------------------          
*                                                                               
ISMPTOT  NMOD1 2,**MPTO**          2 DUBS WORK                                  
         LR    R7,RC               SET R7 TO WORK AREA                          
         L     RC,8(,R1)                                                        
         L     R3,0(,R1)           A(ACCOUNT TO CHECK)                          
         CLC   0(2,R3),=C'1R'                                                   
         BNE   IMPXX               RETURN NOT EQ                                
*                                                                               
         LA    R3,2(,R3)           ADDRESS LEVEL A IN ACCOUNT                   
         MVC   1(1,R7),7(R1)       LEVEL YOU WISH TO CHECK FOR                  
*                                                                               
         MVI   0(R7),EMPLEVA       ASSUME OFFICE TOTAL                          
         MVI   EMPLEVEL,EMPLEVB    GET LEVEL B OFFSETS                          
         BAS   RE,SET1ROFF         SET R2 TO OFFSET, R1 TO LENGTH               
         LA    R6,0(R2,R3)         ADDRESS LEVEL DATA                           
         CLI   0(R6),X'FF'         IS DEPT SET TO FF'S                          
         BE    IMPX                SEE IF THATS WHAT THEY WANT                  
*                                                                               
         MVI   0(R7),EMPLEVB                                                    
         MVI   EMPLEVEL,EMPLEVC                                                 
         BAS   RE,SET1ROFF                                                      
         LA    R6,0(R2,R3)                                                      
         CLI   0(R6),X'FF'                                                      
         BE    IMPX                                                             
*                                                                               
         MVI   0(R7),EMPLEVC                                                    
         MVI   EMPLEVEL,EMPLEVD                                                 
         BAS   RE,SET1ROFF                                                      
         LA    R6,0(R2,R3)                                                      
         CLI   0(R6),X'FF'                                                      
         BE    IMPX                                                             
         B     IMPXX               RETURN NEQ, NOT A TOTAL                      
*                                                                               
IMPX     CLC   0(1,R7),1(R7)                                                    
         LA    R1,2(R2)            SET OFFSET INTO R1 (LEN OF TOT LEV)          
*                                                                               
IMPXX    XIT1  REGS=(R1)                                                        
         SPACE 2                                                                
*----------------------------------------------------------------------         
*       SET R2 TO THE OFFSET OF EMPLEVEL INTO A 12 BYTE ACCOUNT FIELD           
*       AND R1 TO THE LENGTH OF THAT FIELD                                      
*---------------------------------------------------------------------          
SET1ROFF DS    0H                                                               
         XR    R2,R2                                                            
         XR    R1,R1                                                            
         XR    R0,R0                                                            
         LA    RF,SV1RHEIR                                                      
         LA    RF,2(,RF)           BUMP PAST ELCODE/LEN                         
         IC    R1,0(,RF)                                                        
         CLI   EMPLEVEL,1                                                       
         BER   RE                                                               
*                                                                               
         IC    R0,0(,RF)           SAVE LENGTH OF PREV                          
         IC    R2,0(,RF)           SET OFFSET TO LEVEL 2                        
         LA    RF,16(,RF)                                                       
         IC    R1,0(,RF)                                                        
         SR    R1,R0               SET LEN OF LEVEL 2                           
         CLI   EMPLEVEL,2                                                       
         BER   RE                                                               
*                                                                               
         IC    R0,0(RF)                                                         
         IC    R2,0(RF)                                                         
         LA    RF,16(RF)                                                        
         IC    R1,0(RF)                                                         
         SR    R1,R0               SET LEN OF LEVEL 3                           
         CLI   EMPLEVEL,3                                                       
         BER   RE                                                               
*                                                                               
         IC    R0,0(RF)                                                         
         IC    R2,0(RF)                                                         
         LA    RF,16(RF)                                                        
         IC    R1,0(RF)                                                         
         SR    R1,R0               SET LEN OF LEVEL 3                           
         CLI   EMPLEVEL,4                                                       
         BER   RE                                                               
         DC    H'0'                                                             
         SPACE  2                                                               
*                                                                               
GET1RHIR LR    R1,RC                                                            
         NMOD1 0,**GT1R**                                                       
         LR    RC,R1                                                            
         USING ACKEYD,R7                                                        
         L     R7,IOSPACE                                                       
         XC    SV1RHEIR,SV1RHEIR   CLEAR IT IN CASE THERE ISN'T ONE             
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES                               
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'1R'                                             
         GOTO1 DATAMGR,DMCB,=CL8'DMREAD',=C'ACCOUNT',(R7),(R7)                  
         CLI   DMCB+8,X'10'        DID WE FIND RECORD?                          
         BE    G1RXIT              NO                                           
*                                                                               
         AH    R7,DATADISP                                                      
G1R10    CLI   0(R7),0                                                          
         BE    G1RXIT                                                           
         CLI   0(R7),X'16'                                                      
         BE    G1R50                                                            
         ZIC   R1,1(R7)                                                         
         LA    R7,0(R1,R7)                                                      
         B     G1R10                                                            
G1R50    MVC   SV1RHEIR,0(R7)                                                   
*                                                                               
G1RXIT   XIT1                                                                   
         DROP  R7                                                               
         LTORG                                                                  
         EJECT ,                                                                
         DS    0D                                                               
HOOK     NMOD1 0,*HOOK*                                                         
         L     RC,SAVERC           RESTORE REG C                                
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   MYCOL,SPACES                                                     
         MVC   MYROW,SPACES                                                     
*                                                                               
         CLI   MYBOXSW,1                                                        
         BNE   HOOK1A                                                           
         MVI   MYCOL,C'L'          FIX UP BOX COLUMNS                           
         LA    RF,MYCOL+1          START POINT FOR MIDDLES                      
         L     RE,AOPTTAB          BUILD CENTERS BASED ON OPTION TABLE          
         B     HOOK2                                                            
*                                                                               
HOOK1A   CLI   MYBOXSW,2                                                        
         BNE   HOOK1B                                                           
         MVI   MYCOL,C'L'          BOXES FOR JOB SUMMARY                        
         MVI   MYCOL+19,C'C'                                                    
         L     RE,ASUMTAB                                                       
         LA    RF,MYCOL+20                                                      
         MVC   HEAD11+19(112),SAVMID1                                           
         MVC   HEAD12+19(112),SAVMID2                                           
         CLI   PRTFLAG,PRTNGWC     PRINTING W/C SUMMARY?                        
         BNE   *+10                NO                                           
         MVC   HEAD11+4(9),=C'WORK-CODE'                                        
         B     HOOK2                                                            
*                                                                               
HOOK1B   CLI   MYBOXSW,3                                                        
         BNE   HOOK1C                                                           
         MVI   MYCOL,C'L'          BOXES FOR TIME SUMMARY                       
         L     RE,ATIMESUM                                                      
         LA    RF,MYCOL+1                                                       
         B     HOOK2                                                            
*                                                                               
HOOK1C   CLI   MYBOXSW,4                                                        
         BNE   HOOKX                                                            
*                                                                               
         MVI   MYCOL,C'L'          BOXES FOR TIME DETAIL                        
         L     RE,ATIMETAB                                                      
         LA    RF,MYCOL+1                                                       
         B     HOOK2                                                            
*                                                                               
HOOK2    CLI   0(RE),0                                                          
         BE    HOOK6                                                            
         CLI   0(RE),C'Y'                                                       
         BNE   HOOK4                                                            
*                                                                               
         ZIC   R1,1(RE)                                                         
         AR    RF,R1               POINT TO NEXT GAP                            
         MVI   0(RF),C'C'          AND POP IN A CENTRE                          
         LA    RF,1(RF)                                                         
HOOK4    LA    RE,L'OPTTAB(RE)                                                  
         B     HOOK2                                                            
HOOK6    BCTR  RF,0                                                             
         MVI   0(RF),C'R'          MAKE LAST CENTRE A RIGHT                     
         MVI   MYROW+9,C'T'                                                     
         MVI   MYROW+12,C'M'                                                    
         MVI   MYROW+56,C'B'                                                    
         B     HOOKX                                                            
*                                                                               
HOOKX    MVC   BOXCOLS,MYCOL                                                    
         MVC   BOXROWS,MYROW                                                    
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
*                                                                               
         DROP  R4                                                               
*                                                                               
*--------------------------------------------------------------------           
*        SET HEADER INFO                                                        
*--------------------------------------------------------------------           
*                                                                               
         CLI   MYMODE,OFFLAST                                                   
         BE    SETH50                                                           
         CLI   RCSUBPRG,5          REQLAST OF LAST OFFICE CALL                  
         BE    SETH50                                                           
*        USING SORTD,R3                                                         
*        L     R3,SORTAREA         CLIENT                                       
         MVC   HEAD5+12(3),SAVCLI                                               
         MVC   HEAD5+19(36),CLINAME                                             
         CLI   MYMODE,LEVALAST                                                  
         BE    SETH50                                                           
*                                                                               
         MVC   HEAD6+12(3),SAVPRO                                               
         MVC   HEAD6+19(36),PRONAME                                             
         CLI   MYMODE,LEVBLAST                                                  
         BE    SETH50                                                           
*                                                                               
         MVC   HEAD7+12(1),SAVJOB  (MEDIA)                                      
         MVC   HEAD7+19(12),MEDIANM                                             
         MVC   HEAD8+12(6),SAVJOB                                               
         MVC   HEAD8+19(36),JOBNAME                                             
         GOTO1 =A(GETNAMES),DMCB,(RC)     JOB LEVEL HEADER INFO                 
*                                                                               
SETH50   DS    0H                                                               
         GOTO1 =A(REPTHEAD),DMCB,(RC)                                           
         MVI   CLEARFUT,C'N'                                                    
         CLI   RCSUBPRG,5                                                       
         BE    SETH90                                                           
*                                                                               
SETH60   DS    0H                                                               
         MVC   HEAD9+17(L'SVH9),SVH9  PRINT ON BILL                             
*                                                                               
         CLI   RCSUBPRG,1          PRINTING MP SUMMARY                          
         BL    SETH80              COL HEADINGS ARE SET IN MID                  
*                                                                               
         CLI   RCSUBPRG,5          1,2,3,4,5 GET                                
         BNH   SETH90              NO COL HEADINGS (USE WC SET IN MID)          
*                                                                               
         CLI   RCSUBPRG,6                                                       
         BNE   SETH70                                                           
         MVC   HEAD11,TIME11                                                    
         MVC   HEAD12,TIME12                                                    
*                                                                               
         LA    R1,=C'MANPOWER SUMMARY'                                          
         TM    PRTFLAG,PRTNGMPT    AM I DOING TOTALS                            
         BNO   *+8                                                              
         LA    R1,=C'  JOB SUMMARY   '                                          
*                                                                               
         MVC   HEAD5+59(16),=16X'BF'                                            
         MVC   HEAD6+59(16),0(R1)                                               
         MVC   HEAD7+59(16),=16X'BF'                                            
         B     SETH90                                                           
*                                                                               
SETH70   CLI   RCSUBPRG,7                                                       
         BNE   SETH80                                                           
         MVC   HEAD11,TDET11                                                    
         MVC   HEAD12,TDET12                                                    
         B     SETH90                                                           
*                                                                               
SETH80   MVC   HEAD11,SVH11                                                     
         MVC   HEAD12,SVH12                                                     
SETH90   DS    0H                                                               
         XMOD1 1                                                                
SAVERC   DC    A(0)                                                             
         EJECT ,                                                                
         DS    0D                                                               
BXTOP    NMOD1 0,**BXTP**          POP IN A TOP FOR START OF SUMMARY            
         L     RC,BOXRC                                                         
         MVC   MYROW,SPACES                                                     
         MVC   MYCOL,SPACES                                                     
         MVC   P,SPACES                                                         
         ZIC   RF,LINE                                                          
         LA    RE,MYROW(RF)                                                     
         ZIC   RF,MIDSW                                                         
         SR    RE,RF                                                            
         MVI   MIDSW,0                                                          
         MVI   0(RE),C'T'                                                       
         MVI   3(RE),C'M'                                                       
         MVI   MYROW+56,C'B'                                                    
         MVI   MYCOL,C'L'                                                       
         LA    RF,MYCOL+1          START POINT FOR OOPS DETAIL                  
         CLI   RCSUBPRG,6          MANPOWER SUMMARY                             
         BE    BXTOP1                                                           
         CLI   RCSUBPRG,0                                                       
         BE    BXTOP1                                                           
         MVI   MYCOL+19,C'C'                                                    
         LA    RF,MYCOL+20         START POINT FOR WC SUMMARY                   
         MVI   MYCOL+19,C'C'                                                    
BXTOP1   L     RE,ATABLE           BUILD CENTERS BASED ON OPTION TABLE          
BXTOP2   CLI   0(RE),0                                                          
         BE    BXTOP6                                                           
         CLI   0(RE),C'Y'                                                       
         BNE   BXTOP4                                                           
         ZIC   R1,1(RE)                                                         
         AR    RF,R1               POINT TO NEXT GAP                            
         MVI   0(RF),C'C'          AND POP IN A CENTRE                          
         LA    RF,1(RF)                                                         
BXTOP4   LA    RE,L'OPTTAB(RE)                                                  
         B     BXTOP2                                                           
BXTOP6   BCTR  RF,0                                                             
         MVI   0(RF),C'R'          MAKE LAST CENTRE A RIGHT                     
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXROWS,MYROW                                                    
         MVC   BOXCOLS,MYCOL                                                    
         MVI   BOXINIT,0                                                        
         XIT1                                                                   
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
**************************************************************                  
*        PRINT A MIDLINE, WHEREVER YOU ARE                                      
**************************************************************                  
*                                                                               
BOXMID   NMOD1 0,**BXMD**                                                       
         L     RC,BOXRC                                                         
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         ZIC   R2,LINE                                                          
         ZIC   RE,MAXLINES         MAX LINES ALLOWED                            
         BCTR  RE,0                IS THERE ROOM FOR A MID AND A LINE           
         CR    R2,RE                                                            
         BL    BOXM20                                                           
         MVI   FORCEHED,C'Y'                                                    
         B     BOXM50                                                           
BOXM20   ZIC   R2,LINE                                                          
         LA    R3,BOXROWS(R2)                                                   
         BCTR  R3,0                                                             
         MVI   0(R3),C'M'                                                       
         MVI   BOXINIT,0                                                        
BOXM50   XIT1                                                                   
         LTORG                                                                  
*                                                                               
BOXRC    DC    A(0)                                                             
         DROP  R4                                                               
*                                                                               
         EJECT ,                                                                
**************************************************************                  
*        CONSTANTS                                                              
**************************************************************                  
*                                                                               
BUFCSECT DS    0D                  SPACE TO BUILD THE BUFFALO CSECT             
         DS    CL(L'BUFFCNTL)                                                   
*                                                                               
*        BUFF  LINES=300,ROWS=1,COLUMNS=16,FLAVOR=PACKED,KEYLIST=(29,A)         
*              ,COMMENT=36                                                      
         EJECT  ,                                                               
*------------------------------------------------------------------*            
*        SET BUFFALO -- BUILD THE BUFFALO CSECT IN THE AREA GIVEN  *            
*                       BY ADBUFC                                  *            
*------------------------------------------------------------------*            
*                                                                               
         USING BUFFALOD,R3                                                      
*                                                                               
SETBUFF  DS    0D                                                               
         NMOD1 0,**SETB**                                                       
         LA    RC,SPACEND                                                       
*                                                                               
         L     R3,ADBUFC                                                        
         XC    BUFFCNTL,BUFFCNTL                                                
         MVI   BUFFLCOM+3,36       COMMENT=36                                   
         MVI   BUFFROWS+3,1        ROWS=1                                       
*                                                                               
         SR    R1,R1                                                            
         LA    R1,BUFACCSQ         NUMBER OF ACCUMULATED COLUMNS                
         ST    R1,BUFFCOLS         NUMBER OF ACCUMULATORS                       
         MH    R1,=Y(L'BUFACCS1)                                                
         ST    R1,BUFFLDTA         SAVE LENGTH OF ACCUMULATORS                  
         ST    R1,BUFFWROW         WIDTH OF ROWS                                
         MVI   BUFFWCOL+3,8        WIDTH OF COLUMNS                             
         MVI   BUFFFLIP,C'A'       FILE INDICATOR                               
*                                                                               
         LH    R2,=H'29'           KEYLIST=(29,A)                               
         ST    R2,BUFFLKEY         SAVE KEY LENGTH                              
*                                                                               
         LA    R6,BUFFLIST                                                      
         MVI   0(R6),29                                                         
         MVI   1(R6),C'A'                                                       
         MVI   2(R6),X'FF'         MARK END WITH X'FF'                          
*                                                                               
         AR    R2,R1               LENGTH OF FULL RECORD                        
         ST    R2,BUFFLALL                                                      
*                                                                               
         MVI   BUFFFLVR,C'P'       PACKED FLAVOR                                
         L     R1,=AL4(300)        LINES=300                                    
         ST    R1,BUFFCRMX                                                      
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC                                      
         MVC   BUFFADDR,ABUFCORE   A(CORE BUFFER)                               
*                                                                               
         XIT1                                                                   
         DROP  R3                                                               
         EJECT ,                                                                
SORTCARD DC    CL80'SORT FIELDS=(5,41,A),FORMAT=BI'                             
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=(1000)'                                
*                                                                               
LITTAB   DC    CL17'**STAFF CHARGES*'       LITERAL TABLE                       
         DC    CL17'**OOPS CHARGES**'                                           
         DC    CL17'**TOTAL CHARGES**'  OR 'TOTAL STAFF AND OOP'                
         DC    CL17'**TOTAL CHARGES**'  ALWAYS CHARGES                          
         DC    CL17'BILLING DETAILS  '                                          
         DC    CL17'99 STAFF BILLING '                                          
         DC    CL17'99 OOPS BILLING  '                                          
         DC    CL17'**TOTAL BILLING**'                                          
         DC    CL17'***JOB BALANCE***'                                          
         DC    CL17'EXCEPTION REASONS'                                          
         DC    CL17'PARTICIPANTS IN  '                                          
         DC    CL17'PRODUCT CHARGES  '                                          
         DC    CL17'PRODUCT BALANCE  '                                          
         DC    CL17'CLIENT CHARGES  '                                           
         DC    CL17'CLIENT BALANCE  '                                           
         DC    CL17'OFFICE CHARGES  '                                           
         DC    CL17'OFFICE BALANCE  '                                           
         DC    CL17'REQUEST CHARGES'                                            
         DC    CL17'REQUEST BALANCE '                                           
         DC    CL17'NEEDED TO BILL  '                                           
         DC    CL17'UNBILLABLE CODES'                                           
         DC    CL17'OTHER INFORMATION'                                          
         DC    CL19'**MANPOWER TOTALS**'                                        
         DC    CL19'OUT-OF-POCKET TOTAL'                                        
         DC    CL10'ESTIMATES:'                                                 
         DC    CL15'BILLING'                                                    
         DC    CL15'ORDERS'                                                     
         DC    CL15'EXPENSE CHARGES'                                            
         DC    CL15'EXPENSE CHARGES'                                            
         DC    CL15'EXPENSE CHARGES'                                            
         DC    CL15'EXPENSE CHARGES'                                            
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        SET TRANSACTION BUCKETS                                                
*----------------------------------------------------------------------         
SETTRN   NMOD1 0,**SETTR*                                                       
         L     RC,0(,R1)                                                        
         ZAP   SVHOUR,=P'0'                                                     
         ZAP   SVRATE,=P'0'                                                     
         ZAP   SV7CUNIT,=P'0'                                                   
         ZAP   SV7CPRCE,=P'0'                                                   
         ZAP   NETBLAMT,=P'0'      BILLED AMOUNT                                
         ZAP   BLDCDAMT,=P'0'      CASH DISCOUNT ON BILLED AMOUNT               
         ZAP   BLDHRS,=P'0'        BILLED HOURS                                 
         ZAP   BLDAMT,=P'0'        BILLED AMOUNT. EL4B                          
         ZAP   DUBABLE,=P'0'       GROSS                                        
         ZAP   EL4BCNT,=P'0'       COUNT OF BILLINGS                            
         ZAP   WOAMNT,=P'0'                                                     
         ZAP   WOHRS,=P'0'                                                      
         ZAP   WOCNT,=P'0'                                                      
         MVC   WONO,SPACES                                                      
         XC    WODATE,WODATE                                                    
         XC    SV7CSTAT,SV7CSTAT                                                
         MVI   BILLED,C'N'                                                      
         MVI   WROFFED,C'N'                                                     
*                                                                               
*----------------------------------------------------------------------         
*        EXTRACT HOURS AND RATE FROM 40 EL                                      
*----------------------------------------------------------------------         
         USING SORTD,R3                                                         
         L     R3,SORTAREA                                                      
         LA    R2,SRTDATA                                                       
*                                                                               
         USING SR44D,R2                                                         
         CLC   SVANAL,=C'99'                                                    
         BE    STR10                                                            
*                                                                               
         TM    SR44STAT,X'80'                                                   
         BO    STR10                                                            
         MP    SR44AMNT,=P'-1'     CREDIT NON-BILL POSTINGS                     
*                                                                               
STR10    MVI   ELCODE,X'40'        LOOK FOR EL-40                               
         BAS   RE,STNEXTEL         GET EL-40                                    
         BNE   STR20                                                            
         USING SR40D,R2                                                         
         ZAP   SVHOUR,SR40HOUR                                                  
         OC    SR40RATE,SR40RATE   IS A RATE DEFINED?                           
         BZ    *+10                NO                                           
         ZAP   SVRATE,SR40RATE                                                  
*                                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*   TOTAL UP ALL FIELDS IN THE 4B'S                                   *         
*---------------------------------------------------------------------*         
*                                                                               
         USING SR44D,R2                                                         
*                                                                               
STR20    LA    R2,SRTDATA                                                       
         CLC   SVANAL,=C'99'       BILLING                                      
         BNE   STR50               NO                                           
         MVI   BILLED,C'Y'                                                      
         B     STR900              SKIP ALL THIS 4B STUFF                       
*                                                                               
STR50    TM    TRANSTAT,WRITEOFF   WRITE OFF?                                   
         BO    STR890              YES                                          
*                                                                               
         USING SR4BD,R2                                                         
         MVI   ELCODE,X'4B'                                                     
STR100   BAS   RE,STNEXTEL                                                      
         BNE   STR300              NONE OR NO MORE ALLOCATION                   
         CLC   SR4BNO,SPACES                                                    
         BNH   STR100              NO BILL NUMBER MEANS NOT BILLED              
         CLI   SR4BNO,C'W'         WRITE OFF 4B?                                
         BE    STR650              SAVE IN W/O ACCUMS                           
*                                                                               
         CP    SR4BAMNT,=P'0'      ZERO BILLED AMOUNT                           
         BNE   *+8                                                              
         BAS   RE,FILLBILL         FILL IN IF FULLY BILLED AND 1 4B             
*                                                                               
         AP    EL4BCNT,=P'1'       EL-4B COUNT                                  
         AP    NETBLAMT,SR4BAMNT   ADD BILLED AMT---NET                         
         ZAP   PL16,SR4BAMNT       SAVE BILLED AMT---NET                        
         AP    BLDCDAMT,SR4BCSD    BILLED CASH DISCOUNT                         
         AP    PL16,SR4BCSD        ADD CD---NET                                 
         MP    PL16,SR4BRATE                                                    
         SRP   PL16,64-6,5                                                      
         AP    BLDAMT,PL16        SAVE COMMISION IN DUB                         
         AP    BLDHRS,SR4BHRS                                                   
         B     STR100                                                           
*                                                                               
         USING SR44D,R2                                                         
STR300   LA    R2,SRTDATA                                                       
         OC    SR44USED,SR44USED   AUTO BILLED                                  
         BNZ   STR600              YES                                          
         AP    BLDAMT,NETBLAMT     ADD BILLED AMOUNT TO COMMISSION AMT          
*                                                                               
         CLI   PROF13,C'Y'         SHOW NET AS LESS CD ?                        
         BE    STR400              YES, DON'T ADD IT THEN                       
         AP    BLDAMT,BLDCDAMT     NO, ADD CD TO NET PLUS COMMISSION            
*                                                                               
STR400   CP    EL4BCNT,=P'0'       IF NO 4B'S AND NO US DATE                    
         BE    STR700             ITS NOT BILLED                                
*                                                                               
         CP    NETBLAMT,=P'0'                                                   
         BE    STR700              ZERO AMOUNT, NOT BILLED                      
*                                                                               
         MVI   BILLED,C'P'         ASSUMES PARTIALLY  BILLED                    
         CP    NETBLAMT,SR44AMNT                                                
         BNE   STR700               IF TOTAL DOES NOT EQUAL NET                 
STR500   MVI   BILLED,C'Y'                                                      
         B     STR700                                                           
*                                                                               
STR600   ZAP   NETBLAMT,SR44AMNT   FULLY BILD, BILLED AMNT=TRNSAMNT             
         SP    NETBLAMT,WOAMNT     LESS W/O AMOUNT                              
         AP    BLDAMT,NETBLAMT     ADD BILLED AMOUNT TO COMMISSION AMT          
         ZAP   BLDHRS,SVHOUR       IF FULLY BILLED BILD HRD=TOTAL HRS           
         SP    BLDHRS,WOHRS        LESS W/O HOURS                               
         B     STR400              CHECK IF ITS FULLY BILLED                    
*                                                                               
         USING SR4BD,R2                                                         
STR650   AP    WOAMNT,SR4BAMNT                                                  
         AP    WOHRS,SR4BHRS                                                    
         MVC   WONO,SR4BNO                                                      
         AP    WOCNT,=P'1'                                                      
         B     STR100                                                           
*                                                                               
         USING SR44D,R2                                                         
STR700   LA    R2,SRTDATA                                                       
         ZAP   DOUBLE,WOAMNT       NET W/OED                                    
         BZ    STR800                                                           
         MVI   WROFFED,C'P'                                                     
         AP    DOUBLE,NETBLAMT     PLUS NET BILLED                              
         CP    SR44AMNT,DOUBLE     FULLY WRITTEN OFF/BILLED?                    
         BNE   *+8                                                              
         MVI   WROFFED,C'Y'                                                     
*                                                                               
STR800   CP    WOCNT,=P'1'         ONLY ONE WRITE OFF ON THIS TRAN              
         BE    STR890                                                           
         MVC   WONO,=C'  **  '     PRINT DETAILS FOLLOWING THE **               
         EJECT ,                                                                
*---------------------------------------------------------------------+         
*   GET WRITEOFF AMOUNTS ON TYPE 57 TRANSACTIONS                                
*---------------------------------------------------------------------+         
*                                                                               
         USING SR44D,R2                                                         
*                                                                               
STR890   LA    R2,SRTDATA                                                       
         TM    TRANSTAT,WRITEOFF                                                
         BNO   STR900                                                           
*                                                                               
         MVI   ELCODE,X'4F'        WRITE OFF ELEMENT ID,                        
         USING SR4BD,R2                                                         
         BAS   RE,STNEXTEL        ANYTHING (ELSE) WRITTEN OFF THIS TRAN         
         BNE   STR900              NO                                           
*                                                                               
         AP    WOCNT,=P'1'                                                      
         AP    WOAMNT,SR4BAMNT     STORE THESE AS NEGATIVE                      
         AP    WOHRS,SR4BHRS                                                    
         MVC   WONO,SR4BNO                                                      
         MVC   WODATE,SR4BDTE                                                   
         B     STR900              ONLY ONE 4F EL PER W/O TRAN                  
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*   CALCULATE NET COMM GROSS CD IN NETAMT,GRTSAMT,ETC.                *         
*---------------------------------------------------------------------*         
*                                                                               
         USING SR44D,R2                                                         
STR900   LA    R2,SRTDATA                                                       
         ZAP   NETAMT,SR44AMNT     MOVES TRAN NET AMOUNT                        
*                                                                               
         TM    XJOBSTAT,XJOB       XJOB                                         
         BZ    STR910              NO                                           
*                                                                               
         USING SR50D,R2                                                         
         LA    R2,SRTDATA                                                       
         MVI   ELCODE,X'51'        GET XTRAN AMOUNT                             
         BAS   RE,STNEXTEL                                                      
         BNE   *+10                                                             
         ZAP   NETAMT,SR50AMNT                                                  
         USING SR44D,R2                                                         
         LA    R2,SRTDATA          RESET R2                                     
*                                                                               
STR910   CLC   SVANAL,=C'99'       PREVIOUS BILLING                             
         BNE   STR920                                                           
*                                                                               
         ZAP   COMAMT,SR44NARR+15(6) COMMISSION                                 
         ZAP   CDAMT,SR44NARR+21(6) CASH DISCOUNT                               
         AP    NETAMT,CDAMT        PUT CD BACK INTO NET FOR GROSS               
         B     STR950              CALC GROSS                                   
*                                                                               
STR920   ZAP   CDAMT,=P'0'                                                      
         MVI   ELCODE,X'50'        CD ELEMENT                                   
         BAS   RE,STNEXTEL                                                      
         BNE   STR930                                                           
         USING SR50D,R2                                                         
         ZAP   CDAMT,SR50AMNT                                                   
         USING SR44D,R2                                                         
STR930   LA    R2,SRTDATA                                                       
*                                                                               
         AP    NETAMT,CDAMT        ADD ANY CD BACK TO NET TO CALC COMIS         
         TM    SR44STAT,X'01'      NON COMMISSIONABLE                           
         BO    STR940              COM IS ZERO                                  
         AP    COMMABLE,NETAMT     KEEP TOTAL OF COMMISSIONABLE AMOUNT          
*                                  IN CASE YOU HAVE TO RECALCULATE              
STR940   BAS   RE,STRCC            PUT COMMISSION INTO COMAMT                   
*                                                                               
STR950   ZAP   GRSAMT,NETAMT                                                    
         AP    GRSAMT,COMAMT       GROSS                                        
*                                                                               
         TM    XJOBSTAT,XJOB       XJOB                                         
         BNZ   STR1000             YES, NO BILLABLE                             
*                                                                               
*                                  CALCULATE BILLABLE                           
         ZAP   DUBABLE,GRSAMT      GROSS                                        
         SP    DUBABLE,BLDAMT      GROSS LESS BILLED                            
         SP    DUBABLE,WOAMNT      LESS WRITTEN OFF                             
* --------------------------------------------------------------------          
*        EXTRACT UNIT/PRICE FROM SR7C                                           
* --------------------------------------------------------------------          
STR1000  L     R3,SORTAREA                                                      
         LA    R2,SRTDATA                                                       
*                                                                               
         USING SR7CD,R2                                                         
         MVI   ELCODE,X'7C'        LOOK FOR EL-7C                               
         BAS   RE,STNEXTEL                                                      
         BNE   STRX                                                             
*                                                                               
         ZAP   SV7CUNIT,SR7CUNIT                                                
         ZAP   SV7CPRCE,SR7CPRCE                                                
         MVC   SV7CSTAT,SR7CSTAT                                                
         OC    SUBSTAT,SR7CSTAT                                                 
*                                                                               
STRX     XMOD1                                                                  
         DROP  R2,R3                                                            
         EJECT ,                                                                
*        R2 IS A(ZERO AMOUNT 4B), IF FULLY BILLED (USED) AND 1 4B               
*              SET SR4BAMNT TO TRNSAMNT                                         
*                                                                               
         USING SR4BD,R2                                                         
         USING SORTD,R3                                                         
         USING SR44D,R4                                                         
*                                                                               
FILLBILL NTR1                                                                   
         LA    R4,SRTDATA                                                       
         OC    SR44USED,SR44USED   AUTO BILLED                                  
         BZ    FBX                 NO                                           
*                                                                               
         CP    SR4BAMNT,SR44AMNT   ZERO CHARGE                                  
         BE    FBX                                                              
         LR    R5,R2               SAVE A(FIRST 4B)                             
         LA    R2,SRTDATA          HOW MANY 4B'S ON TRANSACTION                 
         MVI   ELCODE,X'4B'                                                     
         BAS   RE,STNEXTEL         MULTIPLE 4B'S                                
         BAS   RE,STNEXTEL                                                      
         BE    FBX                 YES                                          
         LR    R2,R5               ADDRESS 4B ELEMENT                           
         ZAP   SR4BAMNT,SR44AMNT   USE TRNSAMNT AS BILLED AMOUNT                
*                                                                               
FBX      XIT1                                                                   
         DROP  R2,R3,R4                                                         
         SPACE 2                                                                
STFIRST  CLI   0(R2),0                                                          
         BNE   *+10                                                             
         CLI   0(R2),1           SET NEQ CC                                     
         BR    RE                                                               
         CLC   ELCODE,0(R2)                                                     
         BER   RE                BE                                             
         SPACE  2                                                               
STNEXTEL SR    RF,RF                                                            
         IC    RF,1(R2)                                                         
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         CLI   1(R2),1             SET NOT EQUAL CC                             
         BR    RE                                                               
         AR    R2,RF                                                            
         B     STFIRST                                                          
         SPACE  2                                                               
*                                                                               
         USING SR44D,R2                                                         
         USING SORTD,R3                                                         
*                                                                               
STRCC    DS    0H                  CALC COMMISION INTO COMAMT                   
         LA    R2,SRTDATA                                                       
         ZAP   COMAMT,=P'0'                                                     
         TM    SR44STAT,X'01'      NON COMMISSIONABLE                           
         BOR   RE                  COM IS ZERP                                  
         ZAP   PL16,NETAMT                                                      
         MP    PL16,BUFPCT         CALCULATE COMM. AND GROSS                    
         SRP   PL16,64-6,5         RATE IS FOUR DECIMAL PLACES                  
         ZAP   COMAMT,PL16         COMMISSION                                   
         BR    RE                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PRINT ADDITIONAL ACCOUNTS FOR OGILVY                                   
*----------------------------------------------------------------------         
*                                                                               
         USING SORTD,R3                                                         
*                                                                               
PRTSUB   NMOD1 0,PRTSUB                                                         
         L     RC,0(,R1)                                                        
*                                                                               
         L     R3,SORTAREA                                                      
         LA    R2,SRTDATA                                                       
         SR    R1,R1                                                            
*                                                                               
         USING SR44D,R2                                                         
         CLI   SR44TYPE,49         TYPE 49                                      
         BE    PRTSUBX             YES, PREV PRINTED                            
*                                                                               
         CLC   SRTCA(2),=C'SE'     SE CONTRA                                    
         BNE   PRTSUBX             NO                                           
*                                                                               
         TM    XJOBSTAT,XJOB       X JOB                                        
         BNO   PRTSUBX             NO, DON'T PRINT THIS DATA                    
*                                                                               
         USING SR4CD,R2                                                         
PRTSUB10 CLI   0(R2),0             EOR                                          
         BE    PRTSUBX                                                          
*                                                                               
         CLI   0(R2),X'4C'         4C ELEMENT                                   
         BE    PRTSUB20            YES                                          
*                                                                               
         IC    R1,1(R2)                                                         
         LA    R2,0(R1,R2)                                                      
         B     PRTSUB10                                                         
*                                                                               
         USING TABLED,R4                                                        
PRTSUB20 L     R4,ATABLE                                                        
         CLC   SR4CACCT,PREV4C     SAME AS PREV 4C ACCOUNT                      
         BE    PRTSUBX             YES, DON'T BOTHER                            
         MVC   PREV4C,SR4CACCT                                                  
*                                                                               
         MVC   P+5(L'SR4CACCT),SR4CACCT                                         
*                                                                               
         CLI   SUPPNAME,C'N'       WANT THE NAME TOO                            
         BE    PRTSUB70            NO, ACCOUNT IS GOOD                          
*                                                                               
         USING ACKEYD,R5                                                        
         LA    R5,MYKEY                                                         
         MVC   MYKEY,SPACES                                                     
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(14),SR4CACCT                                          
         MVC   COMMAND,=CL8'DMREAD'                                             
         L     R5,IOSPACE                                                       
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',MYKEY,(R5)                      
*                                                                               
         MVC   WORK,SPACES                                                      
         L     R5,IOSPACE                                                       
         AH    R5,DATADISP                                                      
         SR    R1,R1                                                            
PRTSUB30 CLI   0(R5),0                                                          
         BE    PRTSUB70            NO NAME ON RECORD                            
*                                                                               
         CLI   0(R5),X'20'                                                      
         BE    PRTSUB40                                                         
         IC    R1,1(R5)                                                         
         LA    R5,0(R1,R5)                                                      
         B     PRTSUB30                                                         
*                                                                               
         USING ACNAMED,R5          SAVE NAME IN WORK                            
PRTSUB40 ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         BM    PRTSUB50                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ACNMNAME                                                 
*                                                                               
PRTSUB50 ZIC   RF,SUPPNAME+2       OFFSET TO PRINT                              
         LA    RF,P(RF)                                                         
         LA    R0,18                                                            
         CLI   SUPPNAME,C'U'       PRINT UNDER?                                 
         BNE   PRTSUB60            NO                                           
         LA    RF,L'P(RF)                                                       
         LA    RF,4(RF)            OFFSET 4 INTO WC/SUPPLIER COL                
         LA    R0,14                                                            
PRTSUB60 LR    R2,RF                                                            
         GOTO1 CHOPPER,DMCB,(36,WORK),((R0),(R2)),(C'P',3)                      
PRTSUB70 GOTO1 AMYREPT,DMCB,(RC)                                                
*                                                                               
PRTSUBX  XIT1                                                                   
         DROP  R2,R3,R4,R5                                                      
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        SAVE BILLING INFO FROM THE W/C '99''S, WHICH HAVE BEEN SORTED          
*        TO THE BEGINNING OF THE JOB                                            
*----------------------------------------------------------------------         
*                                                                               
         USING BILLTABD,R2                                                      
         USING SORTD,R3                                                         
         USING SR44D,R4                                                         
*                                                                               
BILLPUT  NMOD1 0,*BPUT                                                          
*                                                                               
         L     RC,0(,R1)                                                        
         XC    BILDAREA,BILDAREA                                                
         LA    R2,BILDAREA                                                      
         L     R3,SORTAREA                                                      
         LA    R4,SRTDATA                                                       
         MVC   BILLCA(14),SRTCA                                                 
         MVC   BILLNO(6),SRTNUM                                                 
         MVC   BILLDATE(3),SRTDATE                                              
         MVC   BILLUS,SR44USED     MATCH WITH DATE IN TRANSACTION 4B EL         
         MVC   BILLBTCH,SR44BTCH                                                
         MVC   BILLMOS,SR44MOS                                                  
         MVC   BILLTYPE(15),SR44NARR                                            
         MVC   BILLDATE,SR44DATE                                                
         ZAP   BILLCOM,SR44NARR+15(6)                                           
         ZAP   BILLNET,SR44AMNT                                                 
         ZAP   BILLCD,SR44NARR+21(6)                                            
         ZAP   BILTNET,=P'0'       WILL BE FILLED IN FROM THE 4B'S              
         ZAP   BILTCOM,=P'0'                                                    
         ZAP   BILTHRS,=P'0'                                                    
*                                                                               
         BAS   RE,BILLPG           SAVE GST ON BILL                             
*                                                                               
         L     R3,BILLTAB          A(TABLE HEADER                               
         L     R4,0(R3)            NUMBER IN THE TABLE                          
         L     R1,4(R3)            MAX ALLOWED                                  
         LA    R5,8(R3)            A(TABLE DATA)                                
         CR    R1,R4               IS MAX STILL HIGHER THAN # IN TAB            
         BH    *+6                 YES                                          
         DC    H'0'                NO, NO MORE ROOM                             
         LTR   R4,R4               ANYTHING IN TABLE                            
         BZ    BILLP02             NO, JUMP RIGHT IN THEN                       
BILLP01  LA    R5,LBILLTAB(R5)     BUMP UP TABLE                                
         BCT   R4,BILLP01          TILL AT END                                  
BILLP02  MVC   0(LBILLTAB,R5),0(R2)  SAVE DATA IN TABLE                         
         L     R4,0(R3)            UPDATE TABLE COUNT                           
         LA    R4,1(R4)                                                         
         ST    R4,0(R3)                                                         
         XIT1                                                                   
         SPACE 2                                                                
*                                                                               
*                                  SAVE GST ON A BILL IN COMMAREA               
*                                  R2 IS A(WHERE I AM BUILDING THE BILL         
*                                  TAB RECORD)                                  
BILLPG   NTR1                                                                   
         XC    BILLGST,BILLGST     ASSUME THERE IS NONE                         
         L     R3,SORTAREA                                                      
         LA    R4,SRTDATA                                                       
         XR    R1,R1                                                            
*                                                                               
         USING SRGSD,R4                                                         
BILLPG10 CLI   0(R4),0                                                          
         BE    BILLPGX             NO GST ON BILL                               
*                                                                               
         CLI   0(R4),VBIELQ                                                     
         BE    BILLPG40                                                         
         CLI   0(R4),PBIELQ                                                     
         BE    BILLPG40                                                         
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     BILLPG10                                                         
*                                                                               
BILLPG40 MVC   BILLGST,NEXTGST     SAVE OFFSET INTO COMMAREA I WILL             
*                                  SAVE THIS BILLS GST ELEMENTS                 
         L     R3,COMMAREA                                                      
         ICM   R1,3,NEXTGST                                                     
         AR    R3,R1               R3 IS WHERE TO SAVE ELEMENTS                 
         XR    R1,R1               R1 WILL GET ELEMENT LENGTHS                  
         XR    R5,R5               R5 WILL SUM LENGTHS OF ELEMENTS              
*                                                                               
BILLPG50 IC    R1,1(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)                                                    
*                                                                               
         LA    R1,1(R1)                                                         
         AR    R3,R1               BUMP O/P                                     
         AR    R4,R1               BUMP I/P                                     
         AR    R5,R1               BUMP TOTAL LENGTH SAVED                      
*                                                                               
BILLPG60 CLI   0(R4),0             EOR                                          
         BE    BILLPG70            YES, FINISH UP                               
         CLI   0(R4),VBIELQ        VAT ELEMENT                                  
         BE    BILLPG50            YES, SAVE IT                                 
         CLI   0(R4),PBIELQ        PST ELEMENT                                  
         BE    BILLPG50            YES, SAVE IT                                 
*                                                                               
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     BILLPG60                                                         
*                                                                               
BILLPG70 MVI   0(R3),0             MARK END OF ELLIST                           
         LA    R5,1(R5)            BUMP LENGTH SAVED                            
         ICM   R1,3,NEXTGST        BUMP NEXT OFFSET                             
         AR    R1,R5                                                            
         STCM  R1,3,NEXTGST        SAVE NEXT OFFSET                             
         CH    R1,=Y(CIOLEN)                                                    
*                                                                               
         BL    *+6                                                              
         DC    H'0'                NOT ENOUGH ROOM                              
*                                                                               
BILLPGX  XIT1                                                                   
*                                                                               
         DROP  R2,R3,R4                                                         
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        GOT A NEW CLIENT FROM SORT, SET UP HEADERS, ETC.                       
*---------------------------------------------------------------------          
*                                                                               
         USING ACKEYD,R2                                                        
         USING SORTD,R3                                                         
*                                                                               
AFRSTCLI NMOD1 0,*FCLI                                                          
         L     RC,0(,R1)                                                        
         L     R3,SORTAREA                                                      
         MVC   SAVCLI,SRTCLI           SAVE CLIENT FOR PRINTEM                  
         LA    R2,MYKEY                                                         
         MVC   MYKEY,SPACES                                                     
         MVC   ACKEYACC(3),SAVCUL                                               
         MVC   ACKEYACC+3(3),SRTCLI                                             
         MVC   COMMAND,=CL8'DMREAD'                                             
         L     R2,CLIBUFF                                                       
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',MYKEY,(R2)                      
         LA    R3,CLINAME                                                       
         MVC   0(36,R3),SPACES                                                  
*                                                                               
         L     R2,CLIBUFF         GET CLIENT NAME                               
         AH    R2,DATADISP                                                      
FCLI00   CLI   0(R2),0                                                          
         BE    FCLI05                                                           
*                                                                               
         CLI   0(R2),X'20'                                                      
         BNE   FCLI00A                                                          
         USING ACNAMED,R2                                                       
         IC    R4,ACNMLEN                                                       
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),ACNMNAME                                                 
*                                                                               
FCLI00A  ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     FCLI00                                                           
*                                                                               
FCLI05   L     R2,CLIBUFF          GET 24 ELEMENT                               
         AH    R2,DATADISP                                                      
FCLI06   CLI   0(R2),0                                                          
         BE    FCLI09                                                           
         CLI   0(R2),X'24'                                                      
         BE    FCLI10                                                           
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     FCLI06                                                           
FCLI09   SR    R2,R2                                                            
*                                                                               
FCLI10   MVI   MYMODE,LEVAFRST                                                  
         XC    SVH9,SVH9           FILL W/X'00'S SO HEAD9 PRINTS                
         GOTO1 =A(BUILD24),DMCB,(RC),(R2)  PUT POB IN SVH9                      
         MVC   COMMLVL,SPACES                                                   
         LA    R3,SAVCLI                                                        
         GOTO1 =A(BUILDPRO),DMCB,(RC)                                           
*                                                                               
         CLI   QOPT7,C' '          REQUEST TO PRINT COMMENTS, ETC.              
         BE    *+10                NO, USE PROFILE VALUE                        
         MVC   PROF10(1),QOPT7                                                  
*                                                                               
         CLI   QOPT1,C'M'          M/P SUMMARY REQUEST                          
         BE    FCLI10_A                                                         
         CLI   QOPT1,C'P'          M/P +DETAIL REQUEST                          
         BNE   *+8                                                              
FCLI10_A MVI   PROF24,C'N'         MAKE SURE THEY GET ONE                       
*                                                                               
         USING ACCUMSD,R2                                                       
         L     R2,ACUMAREA                                                      
         MVC   CLITOTS,PZEROS                                                   
         MVC   TOTCHCLI,PZEROS                                                  
         MVC   CLIXJOBS,PZEROS                                                  
*                                                                               
         USING LITTABD,RF                                                       
         L     RF,=A(LITTAB)                                                    
         MVC   TOTCHA,=C'**TOTAL CHARGES**'                                     
         MVC   TIMCHA,=C'**TIME CHARGES** '                                     
         MVC   TIMBIL,=C'99 TIME BILLING  '                                     
         CLI   PROF33,C'S'         SEPARATING STAFF AND OOP                     
         BNE   FCLI10A                                                          
         MVC   TOTCHA,=C'TOTAL STAFF && OOP'                                    
         MVC   TIMCHA,=C'**STAFF CHARGES**'                                     
         MVC   TIMBIL,=C'99 STAFF BILLING '                                     
         MVI   PROF33,C'Y'                                                      
         DROP  RF                                                               
FCLI10A  MVI   CLIACTV,C'N'                                                     
*                                                                               
         USING TIMACCD,R2                                                       
         L     R2,TIMEAREA         ZERO CLIENT LEVEL TIME TOTALS                
         LA    R2,CLITIME                                                       
         LA    R3,PZEROS                                                        
         LA    R1,NTIMEACC                                                      
FCLI15   ZAP   0(8,R2),0(8,R3)                                                  
         LA    R2,8(,R2)                                                        
         LA    R3,8(,R3)                                                        
         BCT   R1,FCLI15                                                        
         DROP R2,R3                                                             
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* FIRST CLIENT -   FIX UP SUMMARY TABLE                               *         
*---------------------------------------------------------------------*         
*                                                                               
         USING SUMTABD,R2                                                       
*                                                                               
         L     R2,ASUMTAB          FIX UP SUMMARY TABLE                         
ALV20    MVI   SMTBCLSW,C'N'       CLEAR PRINT SWITCHES                         
         LA    R2,L'SUMTAB(,R2)                                                 
         CLI   0(R2),0                                                          
         BNE   ALV20                                                            
*                                                                               
         L     R2,ASUMTAB          GENERATE SUMMARY HEADINGS                    
         ST    R2,ATABLE                                                        
         GOTO1 =A(SETSUM),DMCB,(RC),(R2)                                        
         ZIC   R2,PAGEWDTH                                                      
         SH    R2,=H'19'                                                        
         MVC   SAVMID1,SPACES                                                   
         MVC   SAVMID2,SPACES                                                   
         LA    R3,SAVMID1                                                       
         LA    R4,SAVMID2                                                       
         LA    R5,1+18                                                          
         GOTO1 =A(GENHEADS),DMCB,(RC),(R2)                                      
*                                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* FIRST CLIENT -   SET UP COL HEADINGS FOR THE REGULAR REPORT                   
*---------------------------------------------------------------------*         
*                                                                               
         L     R2,AOPTTAB          REGULAR REPORT COLUMNS                       
         ST    R2,ATABLE           GENERAL A(TABLE YOU ARE PROCESSING           
         LA    R0,3                                                             
ALV35    MVI   0(R2),C'Y'          FIRST THREE OPTIONS DEFAULT TO Y             
         LA    R2,L'OPTTAB(,R2)                                                 
         BCT   R0,ALV35                                                         
*                                                                               
ALV36    MVI   0(R2),C'N'          RESET FOR EACH CLIENT                        
         LA    R2,L'OPTTAB(,R2)                                                 
         CLI   0(R2),0                                                          
         BNE   ALV36                                                            
*                                                                               
         USING TABLED,R2                                                        
         L     R2,AOPTTAB          SET UP OPTION TABLE                          
         GOTO1 =A(SETOPT),DMCB,(RC),(R2)                                        
*                                                                               
ALV80    CLI   NARR,C'Y'           IF PRINTING NARRATIVE                        
         BNE   ALV86                                                            
*                                                                               
         GOTO1 =A(SETNARR),DMCB,(RC)  E EXTRA SPACE TO THE NARRATIVE            
*                                                                               
ALV86    LA    R3,SVH11                                                         
         LA    R4,SVH12                                                         
         MVC   SVH11,SPACES                                                     
         MVC   SVH12,SPACES                                                     
         LA    R5,1                                                             
         L     R2,AOPTTAB                                                       
         ST    R2,ATABLE                                                        
         ZIC   R2,PAGEWDTH                                                      
         GOTO1 =A(GENHEADS),DMCB,(RC),(R2)                                      
         L     R2,AOPTTAB                                                       
         CLI   INVDATE,C'S'                                                     
         BNE   ALV90                                                            
         LA    R1,SVH12                                                         
         BAS   RE,STKDATE                                                       
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* FIRST CLIENT -                                                      *         
*     IF THE USER WANTS TO SEPARATE TIME AND OUT-OF-POCKET CHARGES    *         
*        SET UP TIMETAB AS PER THEIR PROFILE OPTIONS                  *         
*---------------------------------------------------------------------*         
*                                                                     *         
ALV90    CLI   PROF33,C'Y'             SEPERATE TIME TRANSACTOONS               
         BNE   ALVX                                                             
         L     R2,ATIMETAB         TABLE OF TIME COLS                           
         ST    R2,ATABLE           GENERAL A(TABLE YOU ARE PROCESSING           
         LA    R0,3                                                             
ALV100   MVI   0(R2),C'Y'          FIRST THREE OPTIONS DEFAULT TO Y             
         LA    R2,L'OPTTAB(,R2)                                                 
         BCT   R0,ALV100                                                        
*                                                                               
ALV101   MVI   0(R2),C'N'          RESET FOR EACH CLIENT                        
         LA    R2,L'OPTTAB(,R2)                                                 
         CLI   0(R2),0                                                          
         BNE   ALV101                                                           
         L     R2,ATIMETAB        TIME DETAIL REPORT HEADINGS                   
         GOTO1 =A(SETTIME),DMCB,(RC),(R2)                                       
*                                                                               
         CLI   PROF35,C'Y'         PRINT NARRATIVE ON TIME DETAIL R             
         BE    ALV220                                                           
         CLI   PROF35,C'P'         PRINT NARRATIVE ON TIME DETAIL R             
         BNE   ALV250                                                           
ALV220   MVI   NARR,C'Y'                                                        
         GOTO1 =A(SETNARR),DMCB,(RC)   EXTRA SPACE TO THE NARRATIVE             
*                                                                               
ALV250   LA    R3,TDET11           GENERATE HEADINGS (R2 IS POINTING            
         LA    R4,TDET12           THE TABLE)                                   
         MVC   TDET11,SPACES                                                    
         MVC   TDET12,SPACES                                                    
         L     R2,ATIMETAB        TIME DETAIL REPORT HEADINGS                   
         ST    R2,ATABLE                                                        
         LA    R5,1                                                             
         ZIC   R2,PAGEWDTH                                                      
         GOTO1 =A(GENHEADS),DMCB,(RC),(R2)                                      
         L     R2,ATIMETAB        TIME DETAIL REPORT HEADINGS                   
         CLI   TWONUM,C'S'         STACKING WO NUM                              
         BNE   ALV260                                                           
         MVC   TWONUM+2(1),BILLNUM+2 YES, USE BILLNUM DISPLACE                  
         LA    R1,TDET11                                                        
         ZIC   R4,BILLNUM+2                                                     
         AR    R4,R1                                                            
         MVC   0(6,R4),=C'BIL/WO'                                               
*                                                                               
ALV260   CLI   INVDATE,C'S'        STACKING INVNO/DATE                          
         BNE   ALV270              NO                                           
         LA    R1,TDET12                                                        
         BAS   RE,STKDATE                                                       
ALV270   CLI   TRATE,C'S'          STACKING HOURS/RATE                          
         BNE   ALVX                                                             
         LA    R1,TDET12                                                        
         ZIC   R4,THOURS+2                                                      
         AR    R4,R1                                                            
         MVC   1(4,R4),=C'RATE'                                                 
*                                                                               
ALVX     XIT1                                                                   
         SPACE  2                                                               
*---------------------------------------------------------------------*         
*                                  RESET HEADERS WHEN STACK NUM/DATE            
*---------------------------------------------------------------------*         
*                                                                               
STKDATE  DS    0H                   SET DISPLACEMENT FOR INV DATE               
         MVC   INVDATE+2(1),INVNUM+2                                            
         ZIC   R4,INVNUM+2          CHANGE HEADERS                              
         AR    R4,R1               R1 IS A(SECOND HEADER LINE)                  
         MVC   0(8,R4),=C'NUM/DATE'  (NOTE- WIDTH OF INVNUM FIELD IS            
         BR    RE                    INCREASED TO 8 WHEN STACKING               
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        GIVE ANY EXTRA SPACE TO THE NARRATIVE                                  
*        R2 IS A(TABLE YOU ARE WORKING ON)                                      
*----------------------------------------------------------------------         
*                                                                               
SETNARR  NMOD1 0,*SETN                                                          
         L     RC,0(,R1)                                                        
         SR    RF,RF               PREP FOR IC'S                                
         SR    RE,RE                                                            
SETN1    CLI   0(R2),0             RUN DOWN TABLE AND ADD UP                    
         BE    SETN3               HOW MUCH ALLOCATED                           
         CLI   0(R2),C'Y'                                                       
         BNE   SETN2                                                            
         IC    RE,1(,R2)           FIELD LENGTH                                 
         AR    RF,RE                                                            
         LA    RF,1(,RF)           ADD ONE FOR EACH GAP                         
SETN2    LA    R2,L'OPTTAB(,R2)                                                 
         B     SETN1                                                            
*                                                                               
SETN3    IC    RE,PAGEWDTH         WIDTH OF THE PAGE                            
         USING TABLED,R2                                                        
         L     R2,ATABLE                                                        
         BCTR  RE,0                ADD ONE FOR FIRST COL                        
         CR    RF,RE               WILL ALL FIT ANYWAY                          
         BH    SETN4                                                            
         SR    RE,RF               IF SO - ADD ANY BALANCE TO NARR              
         IC    RF,NARR+1                                                        
         AR    RE,RF                                                            
         STC   RE,NARR+1                                                        
         B     SETNX                                                            
SETN4    MVI   NARR,C'N'           TURN OFF NARRATIVE (IT WONT FIT)             
SETNX    XIT1                                                                   
         DROP  R2                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*        BUILD COMPOSITE 24 ELEMENT DATA                              *         
*---------------------------------------------------------------------*         
*                                                                               
BUILD24  NMOD1 0,*BLD24                                                         
         L     RC,0(,R1)                                                        
         L     R2,4(,R1)                                                        
         LTR   R2,R2                                                            
         BZ    B2405                                                            
         USING ACPROFD,R2                                                       
         XC    SVOFFICE,SVOFFICE                                                
         CLC   ACPROFFC,SPACES                                                  
         BNH   *+10                                                             
         MVC   SVOFFICE,ACPROFFC   SAVE OFFICE                                  
*                                                                               
B2405    LA    R3,EL24TAB                                                       
         CLI   MYMODE,LEVAFRST     SAVE 24 EL FOR CLI, PROD, AND JOB            
         BE    B2410                                                            
         LA    R3,255(,R3)                                                      
         CLI   MYMODE,LEVBFRST                                                  
         BE    B2410                                                            
         LA    R3,255(R3)                                                       
B2410    LTR   R2,R2               WAS A 24 EL FOUND THIS LEVEL?                
         BNZ   B2415               YES, SAVE IT                                 
         XC    0(255,R3),0(R3)     NO, CLEAR THIS LEVEL                         
         B     *+10                                                             
*                                                                               
B2415    MVC   0(255,R3),0(R2)                                                  
*                                                                               
         CLI   MYMODE,PROCACC      SET EFFECTIVE 24 EL STUFF FOR JOB            
         BNE   B24X                                                             
*                                                                               
         MVI   PROFNARR,C' '                                                    
         MVC   PROFNARR+1(L'PROFNARR-1),PROFNARR                                
         XC    SVH9,SVH9                                                        
         XC    B24STAT,B24STAT                                                  
*                                                                               
         LA    R0,3                                                             
         LA    R2,EL24TAB                                                       
         LA    R2,2*255(R2)        POINT TO JOB LEVEL 24 EL                     
*                                                                               
B2440    TM    B24STAT,GOTPOB      DID I ALREADY GET A POB                      
         BO    B2450               YES                                          
         CLC   ACPRBLPR,SPACES     LOOK FOR IT AT THIS LEVEL                    
         BNH   B2450                                                            
         MVC   SVH9,ACPRBLPR                                                    
         OI    B24STAT,GOTPOB                                                   
*                                                                               
B2450    TM    B24STAT,GOTPNARR    DID I GET OTHER CHAT YET?                    
         BO    B24100                                                           
         CLI   ACPRLEN,105         OTHER CHAT                                   
         BE    B24100              NOT AT THIS LEVEL                            
         OI    B24STAT,GOTPNARR                                                 
         MVC   PROFNARR(50),ACPRNARR                                            
         CLI   ACPRLEN,155         HOW MUCH CHAT                                
         BE    B24100                                                           
         MVC   PROFNARR+50(50),ACPRNARR+50                                      
         CLI   ACPRLEN,205                                                      
         BE    B24100                                                           
         MVC   PROFNARR+100(50),ACPRNARR+100                                    
*                                                                               
B24100   TM    B24STAT,GOTPOB+GOTPNARR                                          
         BO    B24X                                                             
         SH    R2,=Y(255)                                                       
         BCT   R0,B2440                                                         
B24X     XIT1                                                                   
         DROP  R2                                                               
*                                                                               
B24STAT  DS    CL1                                                              
GOTPOB   EQU   1                                                                
GOTPNARR EQU   2                                                                
EL24TAB  DS    (3*255)C            SPACE FOR 3 24 ELEMENTS                      
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        GO TO GETPROF WITH A COMPOSITE KEY SO IT RETURNS YOU A                 
*        COMPOSITE PROFILE.                                                     
*        0(R3) IS CLIENT - SVOFFICE IS SET OR X'00'                             
*----------------------------------------------------------------------         
*                                                                               
         USING PROFKD,R2                                                        
*                                                                               
BUILDPRO NMOD1 0,*BPROF                                                         
         L     RC,0(,R1)                                                        
         LA    R2,WORK                                                          
         XC    PROFKEY,PROFKEY                                                  
         MVC   PROFILES,LEDGPROF   RESTORE LEDGER LEVEL PROFILE                 
         XC    PROGPROF,PROGPROF   16 BYTES TO SEE IF THEY HAVE A CLI           
         MVI   PROFKSYS,C'A'       PROFILE                                      
         MVC   PROFKPGM,=C'067'                                                 
         MVC   PROFKAGY,ALPHAID                                                 
         MVC   PROFKUNL,=C'SJ'                                                  
         MVC   PROFKACC,0(R3)                                                   
         OC    SVOFFICE,SVOFFICE   IS THERE AN OFFICE?                          
         BZ    BPRO50              NO                                           
*                                                                               
         USING ACCOMPD,R4                                                       
         L     R4,ADCMPEL                                                       
         TM    ACMPSTA4,X'01'                                                   
         BO    BPRO10                                                           
         MVC   PROFKOFF,SVOFFICE                                                
         MVI   PROFKAST,C'*'                                                    
         B     BPRO50                                                           
*                                                                               
BPRO10   MVI   PROFKAST,C'+'                                                    
         MVC   PROFKOFC,SVOFFICE                                                
*                                                                               
BPRO50   XC    DMCB,DMCB                                                        
         GOTO1 GETPROF,DMCB,PROFKEY,PROGPROF,(0,DATAMGR),0                      
         MVC   PROFILES(16),PROGPROF   SAVE THE PROFILE                         
*                                                                               
         XC    PROGPROF,PROGPROF                                                
         MVC   PROFKPGM(2),PROFKPGM+1                                           
         MVI   PROFKPGM+2,C'A'              677 TO 67A                          
         XI    PROFKSYS,X'40'          MAKE SYS LOWERCASE (3 CHAR PGM)          
         GOTO1 (RF),(R1)                                                        
         MVC   PROF17(16),PROGPROF        SAVE THE 67A PROFILE                  
*                                                                               
         MVI   PROFKPGM+2,C'B'              67A TO 67B                          
         XC    PROGPROF,PROGPROF                                                
         GOTO1 (RF),(R1)                                                        
         MVC   PROF33(16),PROGPROF        SAVE THE CLIENT 67B PROFILE           
*                                                                               
         MVI   PROFKPGM+2,C'C'              67B TO 67C                          
         XC    PROGPROF,PROGPROF                                                
         GOTO1 (RF),(R1)                                                        
         MVC   PROF49(16),PROGPROF        SAVE THE CLIENT 67C PROFILE           
*                                                                               
         MVC   PROF18(1),LEDGPROF+17 FULL W/C SUM FROM LEDGER LEV               
         XIT1                                                                   
         DROP  R2,R4                                                            
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
* -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -         
*        SAVE ESTIMATE WORK CODE VALUES TO A TABLE AT PROCACC                   
*        SAVE ESTIMATE COLS                                                     
*        SAVE HIGHEST REVISION DATA                                             
* -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -         
*                                                                               
ESTRTN   NMOD1 0,*ESTR*                                                         
         L     RC,0(,R1)                                                        
         GOTO1 =A(LOOKUP),DMCB,(RC)                                             
*                                                                               
         USING JBLOCKD,R5                                                       
         MVC   SVNEWEST,JBNEWEST                                                
         CLI   JBNEWEST,C'Y'       IS JOB ON NEW ESTIMATES                      
         BNE   EST50               NO                                           
         XC    MYKEY,MYKEY         YES, PREP READING NEW EST RECORDS            
         USING ACKEYD,R2                                                        
         LA    R2,MYKEY                                                         
         MVI   ACEVRTYP,ACEVEQU                                                 
         MVI   ACEVSREC,ACEVSEQU                                                
         MVC   ACEVCUL(3),SAVCUL                                                
         MVC   ACEVCLI,SPACES                                                   
         MVC   ACEVCLI(L'SAVCLI),SAVCLI                                         
         MVC   ACEVPROD,SPACES                                                  
         MVC   ACEVPROD(L'SAVPRO),SAVPRO                                        
         MVC   ACEVJOB(L'SAVJOB),SAVJOB                                         
*                                                                               
EST50    LH    R0,JBNROWS                                                       
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BE    EST60               YES                                          
*                                                                               
         USING JBCOLD,R3                                                        
EST52    CLI   JBCOLTYP,JBCOLTWC                                                
         BNE   EST62                                                            
*                                                                               
EST53    CLI   PROF18,C'Y'         DON'T FILTER ON SUMMARIES                    
         BE    EST54                                                            
         CLI   QOPT2,C'T'          IF LIMIT TRANSACTIONS                        
         BE    EST56                                                            
         CLI   QOPT2,C'H'          OR HELD ITEMS ONLY                           
         BE    EST56                                                            
         CLI   QOPT2,C'U'          OR SUPPRESS HELD ITEMS                       
         BE    EST56                                                            
         L     RF,AMONACC          OR MONTH OF ACTIVITY                         
         USING ACMD,RF                                                          
         CLI   ACMMEND,X'FF'                                                    
         DROP  RF                                                               
         BNE   EST56                                                            
         CLI   QOPT5,C'S'          OR SUPPRESS BILLED ITEMS                     
         BE    EST56                                                            
         CLI   QOPT5,C'B'          OR BILLED ITEMS ONLY                         
         BE    EST56               THEN SKIP                                    
         CLI   QOPT2,C'P'          OR PEELABLE JOBS ONLY                        
         BE    EST56               THEN SKIP                                    
*                                                                               
EST54    MVI   JOBACTV,C'Y'                                                     
*                                                                               
         USING MJETABD,R3                                                       
EST56    CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BNE   EST561              YES                                          
         MVC   SVANAL,MJETWCD                                                   
         B     EST562                                                           
         DROP  R3                                                               
*                                                                               
         USING JBCOLD,R3                                                        
EST561   MVC   SVANAL,JBCOLWC      ADD TO BUFFALO TABLE                         
*                                                                               
EST562   GOTO1 =A(WCNAME),DMCB,(RC)                                             
         MVC   BUFKEY,SPACES                                                    
         MVI   BUFTYPE,C'1'                                                     
         MVC   BUFANAL,SVANAL                                                   
         MVC   BUFNAME,WNAME                                                    
         CLI   PROF33,C'Y'        SEPERATE TIME AND OOP                         
         BNE   *+10                                                             
         MVC   BUFWTYPE,WTYPE      SAVE WORKCODE TYPE                           
         MVC   BUFACCS,PZEROS                                                   
         USING ACCUMD,R4                                                        
         LA    R4,BUFACCS          LOAD BUFFALO ACCUMS W/EST VALUES             
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BE    EST56A              YES                                          
         ZAP   ORESTAC,JBCOLVAL(6)                                              
         ZAP   CURESTAC,JBCOLVAL+6(6)                                           
         ZAP   ESTGRSAC,JBCOLVAL+12(6)                                          
         ZAP   ESTCOMAC,JBCOLVAL+18(6)                                          
         ZAP   REVESTAC,JBCOLVAL+24(6)                                          
         ZAP   GRVESTAC,JBCOLVAL+30(6)                                          
         ZAP   RHIESTAC,JBCOLVAL+24(6)                                          
         ZAP   BUFPCT,JBCOLCOM                                                  
         B     EST57                                                            
         DROP  R3                                                               
*                                                                               
         USING MJETABD,R3                                                       
EST56A   ZAP   ORESTAC,MJETVAL(6)                                               
         ZAP   CURESTAC,MJETVAL+6(6)                                            
         ZAP   ESTGRSAC,MJETVAL+12(6)                                           
         ZAP   ESTCOMAC,MJETVAL+18(6)                                           
         ZAP   REVESTAC,MJETVAL+24(6)                                           
         ZAP   GRVESTAC,MJETVAL+30(6)                                           
         ZAP   RHIESTAC,MJETVAL+24(6)                                           
         ZAP   BUFPCT,MJETVAL+42(6)                                             
*                                                                               
EST57    GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         USING ACCUMSD,R2                                                       
         L     R2,ACUMAREA         SAVE JOB ESTIMATE TOTALS                     
         LA    R2,JOBTOTS                                                       
         AP    ORESTAC-ACCUMD(8,R2),ORESTAC                                     
         AP    CURESTAC-ACCUMD(8,R2),CURESTAC                                   
         AP    ESTGRSAC-ACCUMD(8,R2),ESTGRSAC                                   
         AP    ESTCOMAC-ACCUMD(8,R2),ESTCOMAC                                   
         AP    REVESTAC-ACCUMD(8,R2),REVESTAC                                   
         AP    GRVESTAC-ACCUMD(8,R2),GRVESTAC                                   
         AP    RHIESTAC-ACCUMD(8,R2),RHIESTAC                                   
*                                                                               
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BNE   EST62               NO                                           
         B     EST61               YES                                          
*                                                                               
EST60    CLI   MJETTYP,MJETTEQ     ARE WE AT THE END?                           
         BE    EST370              YES                                          
         CLI   MJETTYP,MJETTWQ     NO, AT A WORKCODE?                           
         BNE   EST61               YES                                          
         OC    MJET1RA,MJET1RA     BUT NOT 1R-LEVEL DETAILS                     
         BZ    EST53                                                            
*                                                                               
EST61    XR    R0,R0                                                            
         IC    R0,MJETLEN                                                       
         AR    R3,R0                                                            
         B     EST60                                                            
         DROP  R3                                                               
*                                                                               
         USING JBCOLD,R3                                                        
EST62    AH    R3,JBLCOL                                                        
         BCT   R0,EST52                                                         
         DROP  R2,R4                                                            
*                                                                               
         USING ACCUMSD,R2                                                       
EST370   L     R2,ACUMAREA         SAVE JOB ESTIMATE TOTALS                     
         MVC   TOTCHJOB,JOBTOTS    AS TOTAL CHARGES                             
         DROP  R2                                                               
*                                                                               
         CLI   JBNEWEST,C'Y'       IS JOB ON NEW ESTS                           
         BNE   ESTX                NO                                           
*                                                                               
EST380   MVC   SVHIAPP,JBHIAPP     YES, BUILD TABLES OF PLANNING AND            
         L     R4,PLANESTS         REVISION ESTIMATE NUMBERS                    
         L     R5,REVESTS                                                       
         XC    0(255,R4),0(R4)     CLEAR TABLES                                 
         XC    0(255,R5),0(R5)                                                  
         USING ACKEYD,R2                                                        
         LA    R2,MYKEY                                                         
         XC    ACEVTYPE(L'ACEVKEY-(ACEVTYPE-ACEVKEY)),ACEVTYPE                  
         BAS   RE,ESTHIGH          POINT R2 TO THE RECORD                       
*                                                                               
EST403   CLC   MYKEY(ACEVTYPE-ACEVKEY),0(R2) READING SAME JOB                   
         BNE   ESTX                NO                                           
         LA    RF,ACEVKEY+(EVEKWC-EVEKEY)                                       
         OC    0(L'EVEKWC,RF),0(RF)  IS IT A TIME EST RECORD?                   
         BNZ   EST430                YES - SO READ NEXT RECORD                  
*                                                                               
EST405   CLI   ACEVTYPE,ACEVREV    REVISION TYPE                                
         BNE   EST410                                                           
         MVC   0(1,R5),ACEVERS     SAVE THE REVISION NUMBER                     
         LA    R5,1(R5)                                                         
         B     EST420                                                           
*                                                                               
EST410   CLI   ACEVTYPE,ACEVPLN    PLANNING TYPE                                
         BNE   EST420                                                           
         MVC   0(1,R4),ACEVERS     SAVE THE PLANNING NUMBER                     
         LA    R4,1(R4)                                                         
EST420   AH    R2,DATADISP                                                      
EST423   CLI   0(R2),0             END OF RECORD                                
         BE    EST430                                                           
         CLI   0(R2),X'A9'                                                      
         BE    EST425                                                           
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     EST423                                                           
*                                                                               
         USING ACEUD,R2            GET LAST CHANGE DATE                         
EST425   CLC   ACEULAST,SAVELAST   GET LASTEST EST ACTIVITY                     
         BNH   *+10                                                             
         MVC   SAVELAST,ACEULAST                                                
*                                                                               
EST430   BAS   RE,ESTSEQ           READ NEXT RECORD                             
         B     EST403                                                           
ESTX     XIT1                                                                   
*                                                                               
ESTREAD  MVC   COMMAND,=CL8'DMREAD'                                             
         L     R2,IOSPACE                                                       
         B     ESTDMGR                                                          
ESTHIGH  MVC   COMMAND,=CL8'DMRDHI'                                             
         L     R2,IOSPACE                                                       
         B     ESTDMGR                                                          
ESTSEQ   MVC   COMMAND,DMRSEQ                                                   
         L     R2,IOSPACE                                                       
         B     ESTDMGR                                                          
*                                                                               
ESTDMGR  NTR1                                                                   
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',MYKEY,(R2)                      
         B     ESTX                                                             
         DROP  R2,R3,R5                                                         
         EJECT ,                                                                
* -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -         
*        CALL GETOPT, THEN JOBBER TO BUILD A TABLE OF ESTIMATE VALUES           
*        FLD IS SET TO RETURN A TABLE:                                          
*        ORIG EST, CUR EST, CUR EST GROSS, RATE FOR EACH W/C                    
* -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -         
*                                                                               
LOOKUP   NMOD1 0,*LKUP*                                                         
         L     RC,0(,R1)                                                        
         L     R2,ADGOBLOC                                                      
         USING GOBLOCKD,R2                                                      
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELCLI(L'SAVCLI),SAVCLI                                        
         MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELPRO(L'SAVPRO),SAVPRO                                        
         MVC   GOSELJOB(L'SAVJOB),SAVJOB                                        
         TM    JOBSTAT,NEEDOPT     DO I NEED A GETOPT CALL HERE?                
         BNO   LOOK01              NO (MONACC MAY HAVE CALLED ALREADY)          
         MVC   GOACLI,CLIBUFF                                                   
         MVC   GOAPRO,PROBUFF                                                   
         MVC   GOAJOB,ADACC                                                     
         GOTO1 GETOPT,DMCB,GOBLOCKD                                             
LOOK01   L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
*                                                                               
         L     R5,ACMAJOBB                                                      
         USING JBLOCKD,R5                                                       
         MVC   JBAJOB,ADACC                                                     
         MVC   JBACOLS,ACMACOLL                                                 
         MVC   JBACOM,ADCOMFAC                                                  
         MVC   JBAGOBLK,ADGOBLOC                                                
         MVC   JBAIO,ACMAJOBI                                                   
         MVC   JBAKEY,LASTIO       REREAD LAST RECORD                           
         TM    JOBSTAT,NEEDOPT     IF CALLED BEFORE SORT                        
         BNO   *+10                                                             
         XC    JBAKEY,JBAKEY       DONT REREAD                                  
         MVC   JBGETOPT,GETOPT                                                  
*                                                                               
         MVC   JBACOLTB,ACMACOL                                                 
         MVC   JBLCOLTB,ACMLCOL                                                 
         MVC   JBAOPVTB,ACMAOPV                                                 
         MVC   JBLOPVTB,ACMLOPV                                                 
*                                                                               
         MVI   JBSELFUN,JBGETDE    GET BO DETAILS                               
         LA    RE,FLDH                                                          
         ST    RE,JBORICLI                                                      
*                                                                               
         GOTO1 ACMAJOBR,DMCB,ACMAJOBB                                           
         CLI   JBERROR,X'00'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,ACMACOL                                                       
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BNE   LOOK02              NO                                           
         USING MJETABD,R3                                                       
         ZAP   ESTIMATE,MJETVAL+6(6)                                            
         ZAP   ESTGROSS,MJETVAL+12(6)                                           
         ZAP   HRESTMAT,MJETVAL+24(6) HIGHEST REVISION EST VALUE                
         ZAP   ORDSVAMT,MJETVAL+36(6)                                           
         B     LOOK04                                                           
         DROP  R3                                                               
*                                                                               
         USING JBCOLD,R3                                                        
LOOK02   ZAP   ESTIMATE,JBCOLVAL+6(6)                                           
         ZAP   ESTGROSS,JBCOLVAL+12(6)                                          
         ZAP   HRESTMAT,JBCOLVAL+24(6) HIGHEST REVISION EST VALUE               
         ZAP   ORDSVAMT,JBCOLVAL+36(6)                                          
*                                                                               
LOOK04   OI    JOBSTAT,JOBBERED                                                 
         XIT1  REGS=(R3,R5)                                                     
*                                                                               
FLDH     DC    AL1(8+L'FLD),4X'00',AL1(L'FLD),AL2(0)                            
FLD      DC    C'OE,CE,CEG,CEC,HR,HRG,PO,RATE'                                  
*                                                                               
         DROP  R2,R3,R5                                                         
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        CALCULATE A PRECENTAGE    NETAMT=DIVIDEND                              
*                                  COMAMT=DIVISOR                               
*----------------------------------------------------------------------         
*                                                                               
         USING SUMTABD,R2                                                       
*                                                                               
GENPCT   NMOD1 0,*GPCT*                                                         
         L     RC,0(,R1)                                                        
         CP    COMAMT,=P'0'        CAN'T DIVIDE BY ZERO                         
         BNE   GENP20                                                           
         ZAP   NETAMT,=P'0'                                                     
         B     GPCTX                                                            
GENP20   MVI   BYTE,0                                                           
         CLI   SMTBFNUM,13         IF CALCULATING OVER/UNDER PCT.               
         BNE   GENP30                                                           
         CP    NETAMT,=P'0'        INSURE AMOUNT IS POSITIVE                    
         BNL   GENP30                                                           
         MP    NETAMT,=P'-1'                                                    
         MVI   BYTE,1                                                           
GENP30   ZAP   PL16,NETAMT         ACCUM.                                       
         MP    PL16,=P'2000'       SHIFT DECIMAL AND MULTIPLY BY 2              
         DP    PL16,COMAMT+2(6)    DIVIDED BY TOTAL                             
         CP    PL16(10),=P'0'      IF POSITIVE                                  
         BL    GENP40                                                           
         AP    PL16(10),=P'1'      ADD ONE FOR ROUNDING                         
GENP40   ZAP   NETAMT,PL16(10)                                                  
         ZAP   PL16,NETAMT                                                      
         DP    PL16,=PL6'2'        NOW DIVIDE BY TWO                            
         CLI   BYTE,1              DID I REVERSE SIGN                           
         BNE   GENP50                                                           
         MP    PL16(10),=P'-1'     YES, SO CHANGE IT BACK                       
GENP50   ZAP   NETAMT,PL16(10)     ROUNDED ANSWER RETURNED IN NETAMT            
GPCTX    XIT1                                                                   
         DROP R2                                                                
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        GENERATE REPORT HEADINGS                                               
*----------------------------------------------------------------------         
*                                                                               
REPTHEAD NMOD1 0,*RHED*                                                         
         L     RC,0(,R1)                                                        
         CLI   PAGEWDTH,132                                                     
         BNE   RHEAD10                                                          
         MVC   HEAD1+52(28),=C'PRODUCTION JOB STATUS REPORT'                    
         MVC   HEAD2+52(28),=28C'-'                                             
         TM    XJOBSTAT,XJOB                                                    
         BNO   RHEAD1                                                           
*                                                                               
         MVC   HEAD3+52(28),=C'------- EXPENSE JOB --------'                    
         MVC   HEAD4+52(28),=C'- AMOUNTS ARE MEMOS ONLY   -'                    
*                                                                               
RHEAD1   MVC   HEAD1+92(9),=C'REPORT AC'                                        
         MVC   HEAD1+101(2),QPROG                                               
         MVC   HEAD1+121(4),=C'PAGE'                                            
         LA    R2,HEAD1+126                                                     
         EDIT  PAGE,(4,0(R2)),ALIGN=LEFT                                        
         MVC   HEAD2+92(9),=C'REQUESTOR'                                        
         MVC   HEAD2+102(L'QUESTOR),QUESTOR                                     
*                                                                               
         CLI   RCSUBPRG,5          REQUEST/OFFICE SUMMARY                       
         BE    RHEAD2              YES                                          
         CLI   RCSUBPRG,4          CLIENT SUMMARY                               
         BE    RHEAD2              YES                                          
         CLI   RCSUBPRG,5          PRODUCT SUMMARY                              
         BE    RHEAD2              YES                                          
*                                                                               
         MVC   HEAD4+92(12),=C'BILLING TYPE'                                    
         MVC   HEAD6+92(10),=C'JOB OPENED'                                      
         MVC   HEAD7+92(10),=C'CLOSE DATE'                                      
         MVC   HEAD8+92(13),=C'LAST ACTIVITY'                                   
*                                                                               
RHEAD2   L     R2,AMONACC          PRINT MOS, IF NEEDED                         
         USING ACMD,R2                                                          
         CLC   ACMCMSTR,SPACES                                                  
         BNE   PMOS05                                                           
         CLC   ACMCMEND,SPACES                                                  
         BE    PMOSX                                                            
PMOS05   MVC   HEAD9+92(8),=C'POSTINGS'                                         
         CLC   ACMCMEND,ACMCMSTR                                                
         BNE   PMOS10                                                           
         MVC   HEAD9+101(3),=C'FOR'                                             
         MVC   HEAD9+105(6),ACMCMEND                                            
         B     PMOSX                                                            
*                                                                               
PMOS10   CLC   ACMCMSTR,SPACES                                                  
         BE    PMOS20                                                           
         MVC   HEAD9+101(4),=C'FROM'                                            
         MVC   HEAD9+106(6),ACMCMSTR                                            
         CLC   ACMCMEND,SPACES                                                  
         BE    PMOSX                                                            
         MVC   HEAD9+113(4),=C'THRU'                                            
         MVC   HEAD9+118(6),ACMCMEND                                            
         B     PMOSX                                                            
PMOS20   MVC   HEAD9+101(4),=C'THRU'                                            
         MVC   HEAD9+106(6),ACMCMEND                                            
         DROP  R2                                                               
*                                                                               
PMOSX    CLI   QOPT5,C'S'                                                       
         BNE   *+10                                                             
         MVC   HEAD3+92(19),=C'BILLABLE ITEMS ONLY'                             
         CLI   QOPT5,C'B'                                                       
         BNE   *+10                                                             
         MVC   HEAD3+92(19),=C'BILLED ITEMS ONLY  '                             
         CLI   QOPT5,C'A'                                                       
         BNE   *+10                                                             
         MVC   HEAD3+92(20),=C'ALLOCATED ITEMS ONLY'                            
         CLI   QOPT5,C'U'                                                       
         BNE   *+10                                                             
         MVC   HEAD3+92(22),=C'UNALLOCATED ITEMS ONLY'                          
*                                                                               
         CLI   PROF18,C'Y'         FULL SUMMARIES                               
         BNE   RHEADX              NO                                           
*                                                                               
         CLI   FILTERSW,C'Y'       AM I FILTERING TRANSACTIONS                  
         BNE   RHEADX                                                           
*                                                                               
         CLI   RCSUBPRG,6          MAN POWER SUMMARY                            
         BE    RHEAD5                                                           
         CLI   RCSUBPRG,3          PROD TOTAL?                                  
         BE    RHEAD5                                                           
         CLI   RCSUBPRG,4          CLI TOT?                                     
         BE    RHEAD5                                                           
         CLI   RCSUBPRG,5          REQ TOT?                                     
         BE    RHEAD5                                                           
         CLI   RCSUBPRG,1          WORKCODE SUM                                 
         BNE   RHEADX                                                           
RHEAD5   MVC   HEAD3+92(23),=C'** FULL SUMMARY **     '                         
         MVC   HEAD8+50(35),SPACES GET RID OF MOSFILT                           
         B     RHEADX                                                           
*                                                                               
RHEAD10  DS    0H                                                               
         MVC   HEAD1+37(28),=C'PRODUCTION JOB STATUS REPORT'                    
         MVC   HEAD2+37(28),=28C'-'                                             
*                                                                               
         TM    XJOBSTAT,XJOB                                                    
         BNO   RHEAD20                                                          
         MVC   HEAD3+37(28),=C'------- EXPENSE JOB --------'                    
         MVC   HEAD4+37(28),=C'- AMOUNTS ARE MEMO''S ONLY  -'                   
RHEAD20  MVC   HEAD1+77(9),=C'REPORT AC'                                        
         MVC   HEAD1+86(2),QPROG                                                
         MVC   HEAD1+99(4),=C'PAGE'                                             
         LA    R2,HEAD1+104                                                     
         EDIT  PAGE,(4,0(R2)),ALIGN=LEFT                                        
         MVC   HEAD2+77(9),=C'REQUESTOR'                                        
         MVC   HEAD2+87(L'QUESTOR),QUESTOR                                      
         MVC   HEAD4+77(12),=C'BILLING TYPE'                                    
         MVC   HEAD6+77(10),=C'JOB OPENED'                                      
         MVC   HEAD7+77(10),=C'CLOSE DATE'                                      
         MVC   HEAD8+77(13),=C'LAST ACTIVITY'                                   
         CLI   QOPT5,C'S'                                                       
         BNE   *+10                                                             
         MVC   HEAD3+77(19),=C'BILLABLE ITEMS ONLY'                             
         CLI   QOPT5,C'B'                                                       
         BNE   *+10                                                             
         MVC   HEAD3+77(19),=C'BILLED ITEMS ONLY  '                             
RHEADX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        JOB, CLIENT, PROD         R3=A(ACCUMS)                                 
*        ETC TOTALS                R4=A(PRINT SWITCHES)                         
*        BUMPS TOTALS INTO HIGHER LEVEL ACCUM                                   
*----------------------------------------------------------------------         
*                                                                               
         USING SUMTABD,R2                                                       
         USING ACCUMD,R5                                                        
*                                                                               
TOTALS   NMOD1 0,*TOTS*                                                         
         L     RC,0(,R1)                                                        
         CLC   0(L'ACCUMS,R3),PZEROS                                            
         BE    TOT90                                                            
         LR    R5,R3               SAVE START OF ACCUMS                         
*                                                                               
         ZAP   UNBILLAC,=P'0'                                                   
         CLI   TOTXJOB,C'Y'        DOING AN XJOB STAT                           
         BE    TOT10               YES, SUPRESS BILLABLE                        
*                                                                               
         ZAP   UNBILLAC,GRSAC      RECACLULATE BILLABLE                         
         CLI   PROF13,C'Y'         SHOW NET AS LESS CD ?                        
         BE    *+10                YES, CD IS GONE                              
         SP    UNBILLAC,CDAC       LESS CD                                      
*                                                                               
         SP    UNBILLAC,BILLAC     IN CASE ITS NEEDED                           
*                                                                               
TOT10    L     R2,ASUMTAB                                                       
TOT20    ST    R3,FULL             SAVE CURRENT ACCUM.                          
         CLI   SMTBWANT,C'Y'       DO WE WANT THIS COLUMN                       
         BNE   TOT80                                                            
         CLI   0(R4),C'Y'          ANYTHING TO PRINT                            
         BNE   TOT75                                                            
         TM    SMTBSTAT,NOBAL      DON'T WANT BALANCES                          
         BZ    TOT25                                                            
         CLI   BALANCE,C'Y'                                                     
         BE    TOT75                                                            
*                                                                               
TOT25    CLI   MYMODE,REQLAST                                                   
         BE    TOT30                                                            
         MVI   1(R4),C'Y'          TURN ON NEXT HIGHEST LEVEL                   
         CLI   MYMODE,LEVALAST     IS THIS A CLI TOTAL                          
         BNE   *+8                 NO                                           
         MVI   2(R4),C'Y'          TURN ON OFFICE LEVEL ALSO                    
*                                                                               
TOT30    TM    SMTBSTAT,NOBUF      IS THIS A NEW FIELD                          
         BZ    TOT55                                                            
         CLI   SMTBFNUM,4          OVER/UNDER                                   
         BNE   TOT35                                                            
         ZAP   NETAMT,NETAC        NET                                          
         SP    NETAMT,CURESTAC     LESS PRESENT ESTIMATE                        
         LA    R3,NETAMT                                                        
         B     TOT55                                                            
*                                  OVER/UNDER IN NETAMT FROM LAST COL.          
TOT35    CLI   SMTBFNUM,13         OVER/UNDER PERCENT                           
         BNE   TOT40                                                            
*                                  OVER/UNDER IN NETAMT FROM LAST COL.          
         ZAP   COMAMT,CURESTAC     PRESENT ESTIMATE IN COMAMT                   
         GOTO1 =A(GENPCT),DMCB,(RC) ETURNS PCT IN NETAMT                        
         MVC   AREA(20),SPACES                                                  
         EDIT  (P8,NETAMT),(13,AREA),1,FLOAT=-                                  
         B     TOT60                                                            
*                                                                               
TOT40    CLI   SMTBFNUM,11         ESTIMATE COMMISSION                          
         BNE   TOT45                                                            
         LA    R3,ESTCOMAC                                                      
         B     TOT55                                                            
*                                                                               
TOT45    CLI   SMTBFNUM,12         ESTIMATE GROSS                               
         BNE   TOT50                                                            
         LA    R3,ESTGRSAC                                                      
         B     TOT55                                                            
*                                                                               
TOT50    CLI   SMTBFNUM,14         GROSS OVER/UNDER                             
         BNE   TOT52                                                            
         ZAP   NETAMT,GRSAC        GROSS                                        
         SP    NETAMT,ESTGRSAC     LESS GROSS ESTIMATE                          
         LA    R3,NETAMT                                                        
         B     TOT55                                                            
*                                                                               
TOT52    CLI   SMTBFNUM,17         HIGHEST REVISION                             
         BNE   TOT55                                                            
         LA    R3,RHIESTAC                                                      
*                                                                               
TOT55    MVC   AREA(20),SPACES                                                  
         EDIT  (P8,(R3)),(13,AREA),2,FLOAT=-,ZERO=BLANK                         
TOT60    LA    R1,12               MAXIMUM FIELD WIDTH - 1                      
         ZIC   RE,SMTBLEN          FIELD WIDTH                                  
         SR    R1,RE                                                            
         LA    RF,AREA                                                          
         LA    RF,0(R1,RF)         SHIFT TO RIGHT                               
         ZIC   R1,SMTBPOSI         RELATIVE ADDRESS                             
         LA    R1,P(R1)                                                         
         STC   RE,TOT65+1                                                       
TOT65    MVC   0(0,R1),0(RF)                                                    
         CLI   SMTBFNUM,4          SWITCH SIGNS FOR OVER/UNDER                  
         BE    TOT70                                                            
         CLI   SMTBFNUM,14         AND FOR GROSS OVER/UNDER                     
         BE    TOT70                                                            
         CLI   SMTBFNUM,13         AND FOR OVER/UNDER PCT.                      
         BNE   TOT75                                                            
TOT70    LR    RF,RE                                                            
         BAS   RE,TOTINV                                                        
TOT75    TM    SMTBSTAT,NOACCUM                                                 
         BO    TOT80                                                            
         CLI   MYMODE,REQLAST      DON'T BUMP TOTALS AT REQLAST                 
         BE    TOT80                                                            
         CLI   MYMODE,OFFLAST      OFFICE TOTALS HANDLED IN LSTCLI              
         BE    TOT80                                                            
         TM    SMTBSTAT,NOBUF                                                   
         BZ    TOT78                                                            
         CLI   BALANCE,C'Y'                                                     
         BE    TOT80                                                            
         CLI   0(R4),C'Y'                                                       
         BNE   TOT80                                                            
TOT78    AP    L'ACCUMS(8,R3),0(8,R3) ADD TO NEXT LEVEL UP                      
TOT80    L     R3,FULL             RESTORE A(LAST ACCUM.)                       
         TM    SMTBSTAT,NOBUF      IF A NEW FIELD                               
         BO    TOT85               DON'T INCREMENT ACCUM.                       
         LA    R3,8(R3)                                                         
TOT85    LA    R2,L'SUMTAB(R2)                                                  
         LA    R4,L'SUMTAB(R4)                                                  
         CLI   SMTBWANT,0          END OF TABLE                                 
         BNE   TOT20                                                            
TOT90    BAS   RE,TOTPRNT                                                       
         B     TOTXIT                                                           
         DROP  R2,R5                                                            
*                                                                               
TOTPRNT  NTR1                                                                   
*                                                                               
         MVC   PRTFSAVE,PRTFLAG                                                 
         CLI   MYMODE,ACCLAST                                                   
         BE    *+8                                                              
         MVI   PRTFLAG,PRTALL                                                   
         GOTO1 AMYREPT,DMCB,(RC)                                                
         MVC   PRTFLAG,PRTFSAVE                                                 
         B     TOTXIT                                                           
*                                                                               
TOTINV   DS    0H                  INVERT SIGNS                                 
         STC   RF,TOTI20+1         RF=LENGTH OF FIELD-1                         
TOTI20   CLC   0(0,R1),SPACES      R1=A(PRINT POSITION)                         
         BER   RE                                                               
TOTI30   CLI   0(R1),C' '          BUMP FORWARD TO NON-BLANK                    
         BNE   TOTI40                                                           
         LA    R1,1(R1)                                                         
         B     TOTI30                                                           
TOTI40   CLI   0(R1),C'-'          MINUS BECOMES A BLANK                        
         BE    TOTI50                                                           
         BCTR  R1,0                                                             
         MVI   0(R1),C'+'          BLANK BECOMES A PLUS                         
         BR    RE                                                               
TOTI50   MVI   0(R1),C' '                                                       
         BR    RE                                                               
*                                                                               
TOTXIT   XIT1                                                                   
*                                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PROCESS A PURCHASE ORDER                                               
*        R2 POINTS TO THE 68 EL FROM SORT                                       
*----------------------------------------------------------------------         
*                                                                               
         USING SORTD,R3                                                         
         USING SR68D,R2                                                         
         USING TABLED,R4                                                        
*                                                                               
ORDIT    NMOD1 0,*ORDR*                                                         
         L     RC,0(,R1)                                                        
         L     R4,ATABLE                                                        
         ZAP   NETAMT,=P'0'                                                     
         ZIC   RF,INVNUM+2         PRINT MAJOR W/C IN INVOICE COL               
         BAS   RE,SETAPRTO                                                      
         MVC   0(4,RF),=C'W/C='                                                 
         MVC   4(2,RF),SR68WC      SHOW MAJOR WORK CODE                         
ORD20    AP    NETAMT,SR68AMNT     RUNNING ORDER TOTAL                          
         SP    NETAMT,SR68AVIL     INVOICED TOTAL                               
ORD25    ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),X'68'        DID I GET THE NEXT P/O                        
         BE    ORD20              YES                                           
         CLI   0(R2),0            END OF REC                                    
         BNE   ORD25                                                            
*                                                                               
         AP    BUFACCS3,NETAMT     SAVE ORD TOTAL                               
*                                                                               
         CLI   ORDNO,C'Y'          SHOW ORDER NO AMD ORIG AMOUNT                
         BNE   ORD30                                                            
*                                                                               
         MVC   ORDSW(2),=C'YY'     TURN ON PRINT SWITCHES                       
         ZIC   RF,ORDNO+2                                                       
         BAS   RE,SETAPRTO                                                      
         MVC   0(6,RF),SRTNUM                                                   
         ZIC   RF,ORDAMT+2         IF ORDNO THEN ORDAMT                         
         BAS   RE,SETAPRTO                                                      
         EDIT  (P8,NETAMT),(13,0(RF)),2,MINUS=YES                               
*                                                                               
         CLI   NARR,C'Y'           IS THERE A NARR                              
         BNE   ORDX                                                             
         ZIC   RF,NARR+2           PUT AUTH THERE                               
         LA    RF,P-L'P(RF)        ORD35 WILL BUMP TO NEXT P                    
         B     ORD35                                                            
*                                                                               
ORD30    CLI   NARR,C'Y'           IF NO ORDER NUMBER COL BUT                   
         BNE   ORD40               WE HAVE A NARR COL                           
         MVC   NARRSW,=C'YY'                                                    
         ZIC   RF,NARR+2           THEN POP ORDER NUMBER INTO NARR              
         BAS   RE,SETAPRTO                                                      
         MVC   0(4,RF),=C'ORD='                                                 
         MVC   4(6,RF),SRTNUM                                                   
*                                                                               
ORD35    LA    RF,L'P(RF)          NEXT LINE OF NARR                            
         ST    RF,FULL             SAVE RF                                      
         MVC   WORK(L'MYKEY),MYKEY                                              
         USING ACKEYD,R5                                                        
         LA    R5,MYKEY                                                         
         XC    MYKEY,MYKEY                                                      
         MVI   ACOKCODE,X'1A'                                                   
         MVC   ACOKCOMP(1),RCCOMPFL                                             
         MVC   ACOKNUM(6),SRTNUM   ORDER NUMBER                                 
         MVC   COMMAND,=CL8'DMREAD'                                             
         L     R2,IOSPACE                                                       
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',MYKEY,(R2)                      
         L     R2,IOSPACE                                                       
         AH    R2,DATADISP                                                      
ORD35A   CLI   0(R2),X'67'        DID I GET THE P/O                             
         BE    ORD36              YES                                           
         CLI   0(R2),0            END OF REC                                    
         BNE   *+6                SOMTHINGS WRONG                               
         DC    H'0'                                                             
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     ORD35A                                                           
*                                                                               
ORD36    MVC   MYKEY,WORK                                                       
         L     RF,FULL                                                          
         MVC   0(5,RF),=C'AUTH='                                                
         USING ACORDRD,R2                                                       
         ZIC   RE,NARR+1           WIDTH OF NARRATIVE COL                       
         LA    R1,20               WHAT I NEED TO FIT AUTH=AUTHOR               
         CR    RE,R1                                                            
         BNL   ORD37               ENOUGH ROOM                                  
         LA    RF,L'P(RF)          NEXT P                                       
         B     *+8                                                              
ORD37    LA    RF,5(RF)                                                         
         MVC   0(15,RF),ACORAUTH   PRINT P/O AUTHORIZER IN NARRATIVE            
*                                                                               
ORD40    CLI   ORDNO,C'Y'          IS THERE AN ORDER COL                        
         BE    ORDX                YES, I ALREADY DID THIS                      
         CLI   NET,C'Y'                                                         
         BNE   ORDX                                                             
         ZIC   RF,NET+2                                                         
         BAS   RE,SETAPRTO                                                      
         EDIT  (P8,NETAMT),(13,0(RF)),2,MINUS=YES,BRACKET=YES                   
         MVC   NETSW(2),=C'YY'     TURN ON NET PRINT SWITCHES                   
ORDX     XIT1                                                                   
         DROP  R2,R3,R4,R5                                                      
*                                                                               
SETAPRTO DS    0H                  I/P OFFSET INTO PRTBUFF                      
         ST    RE,SAVERE           O/P PRINT ADDRESS                            
         L     RE,PRTBUFF                                                       
         LA    RF,0(RF,RE)                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         LTORG                                                                  
         EJECT ,                                                                
*-------------------------------+  RF=AVAILABLE COLUMNS                         
* LOOP THRU TABLE PASSED        |  R1=STARTING COLUMN                           
* BUILDING HEADERS IN 0(R3)     |  R2=A(TABLE)                                  
* AND 0(R4)                     |  R3=A(LINE 1)                                 
*-------------------------------+  R4=A(LINE 2)                                 
*                                                                               
         USING SUMTABD,R2                                                       
*                                                                               
GENHEADS NMOD1 0,*GHEA*                                                         
         L     RC,0(,R1)                                                        
         L     RF,4(,R1)                                                        
         L     R2,ATABLE                                                        
         MVC   0(L'MYHEADS,R3),SPACES                                           
         MVC   0(L'MYHEADS,R4),SPACES                                           
         BCTR  RF,0                ALLOW ONE FOR LEFT SIDE OF BOX               
GENH2    CLI   SMTBWANT,C'Y'       DO I WANT THIS COLUMN                        
         BNE   GENH6                                                            
         ZIC   RE,SMTBLEN          COLUMN WIDTH                                 
         BCTR  RF,0                ALLOW ONE FOR GAP                            
         SR    RF,RE               ANY ROOM                                     
         BNM   GENH4               YES CARRY ON                                 
         AR    RF,RE               NO RESET AND TRY NEXT                        
         MVI   SMTBWANT,C'N'       BUT FIRST - SET OFF PRINT INDICATOR          
         B     GENH6                                                            
GENH4    STC   R5,SMTBPOSI         STARTING POSITION                            
         LA    R1,9                MAX MVC LENGTH                               
         CR    RE,R1               IS FIELD WIDTH GT 10                         
         BH    GENH4A              YES, USE 10                                  
         LR    R1,RE               NO, USE FIELD WIDTH FOR MOVE                 
GENH4A   EX    R1,GENH5                                                         
         EX    R1,GENH5A                                                        
         B     GENH5B                                                           
GENH5    MVC   1(0,R3),SMTBNAM1    HEADLINE                                     
GENH5A   MVC   1(0,R4),SMTBNAM2    HEADLINE2                                    
*                                                                               
GENH5B   LA    RE,1(RE)                                                         
         AR    R3,RE               POINT TO NEXT                                
         AR    R4,RE                                                            
         AR    R5,RE                                                            
GENH6    LA    R2,L'SUMTAB(R2)                                                  
         CLI   SMTBWANT,0          END                                          
         BNE   GENH2                                                            
         XIT1                                                                   
         DROP  R2                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*- RETURN, IN WNAME, THE NAME OF THE WORK CODE PASSED IN SVANAL      -*         
*- NOTE SPECIALS FOR W/C ** (P/O'S) AND W/C 99 (BILLING)             -*         
*---------------------------------------------------------------------*         
*                                                                               
WCNAME   NMOD1 0,*WCNA*                                                         
         L     RC,0(,R1)                                                        
         L     R2,ADGOBLOC                                                      
         USING GOBLOCKD,R2                                                      
         MVC   GOSELWC,SVANAL                                                   
         GOTO1 GETOPT,DMCB,GOBLOCKD                                             
         L     RF,ADLEDGER                                                      
         AH    RF,DATADISP                                                      
         SR    RE,RE                                                            
         MVI   WTYPE,O_O_P_WC                                                   
         MVI   WCP_TIME,C'N'                                                    
         MVC   WNAME,=CL15'BILLING'                                             
*                                                                               
         CLC   SVANAL,=C'**'                                                    
         BNE   WCN2                                                             
         MVC   WNAME,=CL15'ORDERS'                                              
WCN2     CLI   0(RF),0                                                          
         BE    WCNX                                                             
         CLI   0(RF),X'12'                                                      
         BE    WCN6                                                             
WCN4     IC    RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     WCN2                                                             
         USING ACANALD,RF                                                       
WCN6     CLC   SVANAL,ACANCODE                                                  
         BNE   WCN4                                                             
         MVC   WNAME,ACANDESC                                                   
*                                                                               
         MVI   WCP_TIME,C'N'       A CHARGES AS TIME FLAG FOR P                 
*                                                                               
         MVC   BYTE,ACANTYPE       ESTABLISH WORKCODE TYPE                      
         CLC   GOWRKTY,SPACES                                                   
         BNH   *+10                                                             
         MVC   BYTE,GOWRKTY                                                     
*                                                                               
         CLI   BYTE,C'T'           T IS TIME                                    
         BE    WCN8                                                             
*                                                                               
         CLI   BYTE,C'R'           R IS TIME                                    
         BE    WCN8                                                             
*                                                                               
         CLI   BYTE,C'P'           P MIGHT BE TIME                              
         BNE   WCN9                                                             
         CLI   PROF27,C'Y'         P WC'S AS TIME                               
         BNE   WCN9                NO                                           
         MVI   WCP_TIME,C'Y'                                                    
*                                                                               
WCN8     MVI   WTYPE,TIMEWC        SET AS TIME WC                               
         B     WCNX                                                             
*                                                                               
WCN9     MVI   WCP_TIME,C'N'                                                    
         MVI   WTYPE,O_O_P_WC                                                   
         B     WCNX                                                             
*                                                                               
WCNX     XIT1                                                                   
         DROP  R2,RF                                                            
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------+                                 
*        PRINT COMMENTS AND PROFILES          |                                 
*---------------------------------------------+                                 
*                                                                               
COMMPRT  NMOD1 0,*CMPR*                                                         
         L     RC,0(,R1)                                                        
         L     R3,ADGOBLOC                                                      
         USING GOBLOCKD,R3                                                      
         MVI   RCSUBPRG,2          TURN OFF COLUMN HEADINGS                     
         L     R2,ADPROFIL                                                      
         USING ACPROFD,R2                                                       
         CLC   GOUWLIST,SPACES                                                  
         BE    CM01                                                             
         OC    GOUWLIST,GOUWLIST                                                
         BZ    CM01                                                             
         BAS   RE,COMPRT                                                        
         LA    R3,GOUWLIST                                                      
         USING LITTABD,R5                                                       
         L     R5,=A(LITTAB)                                                    
         MVC   P+1(17),UNBCODES                                                 
         LA    R4,P+18                                                          
         LA    R5,6                                                             
CM01A    MVC   0(2,R4),0(R3)                                                    
         CLI   2(R3),C' '          ANYTHING IN NEXT WC SPOT                     
         BNH   CM01B               NO                                           
         MVI   2(R4),C','                                                       
         LA    R4,4(R4)                                                         
         LA    R3,2(R3)                                                         
         BCT   R5,CM01A                                                         
         BAS   RE,COMMACL                                                       
*                                                                               
CM01B    BAS   RE,COMPRT                                                        
*                                                                               
CM01     CLI   PROF10,C'Y'         COMMENTS ONLY                                
         BE    CM02                                                             
         CLI   PROF10,C'P'         PROFILES ONLY                                
         BE    CM01D                                                            
         CLI   PROF10,C'A'         ALL                                          
         BE    CM01D                                                            
         CLI   PROF10,C'B'         BOTH                                         
         BNE   COMMX                                                            
CM01D    GOTO1 =A(PROFILE),DMCB,(RC)                                            
         BAS   RE,COMPRT                                                        
         MVC   P+1(8),=C'PROFILES'                                              
         MVC   PSECOND+1(8),=19C'-'                                             
         MVC   PTHIRD+1(131),PSAVE                                              
         MVI   SPACING,2                                                        
         BAS   RE,COMPRT                                                        
*                                                                               
CM02     CLI   PROF10,C'P'         PROFILES ONLY                                
         BE    COMMX                                                            
         CLI   PROFNARR,C' '                                                    
         BNE   CM03                                                             
         CLC   PROFNARR+1(L'PROFNARR-1),PROFNARR                                
         BE    CM06                                                             
         USING LITTABD,R5                                                       
CM03     L     R5,=A(LITTAB)                                                    
         MVC   P+1(17),OTHINFO                                                  
         MVC   PSECOND+1(17),=19C'-'                                            
         ZIC   RF,LINE             CHECK FOR ENOUGH ROOM                        
         LA    RF,6(RF)                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,COMPRT                                                        
         LA    R2,P+1                                                           
         LA    R0,50                   NARRATIVE COMES IN BLOCKS OF 50          
         GOTO1 CHOPPER,DMCB,(150,PROFNARR),((R0),(R2)),(C'P',3)                 
         BAS   RE,COMPRT                                                        
         EJECT ,                                                                
* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---           
*        PRINT COMMENTS                                                         
* --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---           
*                                                                               
CM06     CLI   PROF10,C'Y'                                                      
         BE    CMS                                                              
         CLI   PROF10,C'B'                                                      
         BE    CMS                                                              
         CLI   PROF10,C'A'                                                      
         BE    CMS                                                              
         B     COMMX                                                            
*                                                                               
*---------------------------------------------------------------------*         
*        LOOK FOR COMMENT DATA AT JOB THEN PROD THEN CLI LEVELS                 
*---------------------------------------------------------------------*         
CMS      LA    R0,3                READ FOR THREE LEVELS OF ACCOUNTS            
         MVI   BYTE,C'N'                                                        
         MVI   CPSW,C'N'           COMMENT HEADING SWITCH                       
         L     R4,COMMAREA                                                      
*                                                                               
CMS00    LR    R1,R0               GET LOCATION TO LOAD ADDRESS FROM            
         SHI   R1,3                REVERSE THE COUNT TO COUNT 0-2               
         LPR   R1,R1               NEED TO START AT DMGRBUFF THEN               
         MHI   R1,L'DMGRBUFF       NEXT CLIBUFF AND LAST PROBUFF                
         L     R2,DMGRBUFF(R1)     R2 = A(NEXT ACCOUNT LEVEL RECORD)            
         MVI   ELCODE,X'3E'                                                     
         BAS   RE,COMGET           R2 = POINTS AT X'3E'                         
*                                                                               
         USING ACOMMD,R2                                                        
CMSA     BNE   CMS10                                                            
*                                                                               
CMS1     MVI   BYTE,C'Y'           SAVE COMMENT DATA                            
         ZIC   R3,ACOMLEN                                                       
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R2)                                                    
*                                                                               
         AR    R4,R3               BUMP R4                                      
         MVI   0(R4),0             SET EOR                                      
         BAS   RE,COMNEXT                                                       
         B     CMSA                                                             
*                                                                               
CMS10    CLI   BYTE,C'Y'           DID I GET A COMMENT AT THIS LEVEL            
         BNE   CMS11               YES, PRINTIT                                 
         BAS   RE,CM7                                                           
         BAS   RE,COMPRT           SPACE AFTER THE COMMENT                      
         L     R4,COMMAREA                                                      
         MVI   BYTE,C'N'                                                        
*                                                                               
CMS11    BCT   R0,CMS00                                                         
         B     COMMX               NO COMMENTS FOUNT AT ANY LEVEL               
         DROP  R2,R3,R5                                                         
*                                                                               
CM7      NTR1                      PRINT COMMENT DATA                           
         L     R2,COMMAREA                                                      
         CLI   0(R2),0                                                          
         BE    COMMX                                                            
         USING ACOMMD,R2                                                        
         CLI   0(R2),X'3E'         MAKE SURE THERE IS A COMMENT THERE           
         BNE   COMMX                                                            
         CLI   1(R2),0             CHECK FOR A VALID LENGTH                     
         BE    COMMX                                                            
*                                                                               
CM8      CLI   ACOMTYPE,0          IS THIS A DATA ELEMENT                       
         BNE   CM10                NO, IT CONTAINS A KEY                        
CM9      LR    R4,R2               YES, PRINT THE DATA                          
         MVI   COMMSW,C'C'         WE ARE READING FROM COMMAREA                 
         B     CM14                                                             
*                                                                               
CM10     XC    COMMKEY,COMMKEY     READ A STANDARD COMMENT RECORD               
         MVI   COMMSW,C'D'         WE ARE READING FROM DATAMGR                  
         MVI   COMMKEY,X'0C'                                                    
         MVC   COMMKEY+1(1),QCOMPANY                                            
         MVC   COMMKEY+2(6),ACOMMENT                                            
         L     R4,IOSPACE                                                       
         MVC   KEYSAVE,COMMKEY                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',COMMKEY,(R4)                     
         CLC   KEYSAVE,0(R4)                                                    
         BNE   CM44                DIDN'T READ IT                               
         AH    R4,DATADISP                                                      
*                                                                               
CM14     SR    RF,RF               NOW COUNT THE NUMER OF LINES                 
         LR    RE,R4                                                            
CM15     CLI   0(RE),0             IN THE COMMENT                               
         BE    CM16                                                             
         CLI   0(RE),X'3E'                                                      
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
         ZIC   R3,1(RE)                                                         
         AR    RE,R3                                                            
         B     CM15                                                             
*                                                                               
CM16     ZIC   R3,MAXLINES         SEE IF ALL THE COMMENT WILL FIT              
         ZIC   RE,LINE                                                          
         SR    R3,RE                                                            
         CR    RF,R3               NEEDED VS WHAT IS LEFT                       
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
CM22     SR    R3,R3                                                            
         CLI   0(R4),0             END OF RECORD                                
         BE    CM44                GET NEXT RECORD                              
         CLI   0(R4),X'3E'                                                      
         BE    CM26                                                             
CM24     ZIC   R3,1(R4)            GET NEXT ELEMENT ON THIS RECORD              
         AR    R4,R3                                                            
         B     CM22                                                             
         DROP  R2                                                               
*                                                                               
         USING ACOMMD,R4                                                        
CM26     CLI   ACOMTYPE,0          IS THE NEXT 3E EL DATA?                      
         BE    CM26B               YES,                                         
         LR    R2,R4               WILL NEED R2 FOR GETEL                       
         B     CM10                                                             
CM26B    CLI   CPSW,C'Y'           HAVE I PRINTED HEADING                       
         BE    CM27                                                             
         CLI   FORCEHED,C'Y'                                                    
         BE    *+12                                                             
         MVI   SPACING,2                                                        
         BAS   RE,COMPRT                                                        
*                                                                               
         MVC   P+1(8),=C'COMMENTS'                                              
         MVC   PSECOND+1(8),=19C'-'                                             
         BAS   RE,COMPRT                                                        
         MVI   CPSW,C'Y'                                                        
*                                                                               
CM27     ZIC   R3,ACOMLEN                                                       
         SH    R3,=H'5'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+1(0),ACOMMENT                                                  
         BAS   RE,COMPRT                                                        
         B     CM24                GET NEXT ELEMENT                             
*                                                                               
CM44     CLI   COMMSW,C'C'         END OF COMMAREA?                             
         BE    COMMX               YES, I'M DONE                                
         MVI   ELCODE,X'3E'        GET NEXT COMMENT FROM COMMAREA               
         BAS   RE,COMNEXT                                                       
         BNE   COMMX               NONE LEFT, I'M DONE                          
         B     CM8                 READ THE COMMENT RECORD                      
*                                                                               
COMMX    XIT1                                                                   
         DROP  R4                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        COMMENT PRINTING UTILITIES                                             
*----------------------------------------------------------------------         
*                                                                               
COMPRT   DS    0H                                                               
         ST    RE,SAVERE                                                        
         GOTO1 AMYREPT,DMCB,(RC)                                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
*----------------------------------------------------------------------         
*        CLEAR THE TRAILING COMMA IN R4 MINUS 2 (CLEAR COMMA SPACE)             
*----------------------------------------------------------------------         
*                                                                               
COMMACL  SH    R4,=H'2'            CLEAR TRAILING COMMA IF AROUND               
         CLI   0(R4),C','                                                       
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
         BR    RE                                                               
*                                                                               
*----------------------------------------------------------------------         
*        GETEL FOR THIS NMOD                                                    
*----------------------------------------------------------------------         
*                                                                               
COMGET   AH    R2,DATADISP                                                      
COMFIRST CLI   0(R2),0                                                          
         BNE   *+10                                                             
         CLI   0(R2),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                BE INSTRUCTION                               
         CLC   ELCODE,0(R2)                                                     
         BCR   8,RE                BE                                           
COMNEXT  SR    RF,RF                                                            
         IC    RF,1(R2)                                                         
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         CLI   1(R2),1             SET NOT EQUAL CC                             
         BR    RE                                                               
         AR    R2,RF                                                            
         B     COMFIRST                                                         
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        LIST LINKED ACCOUNTS BY READING PASSIVE POINTERS                       
*----------------------------------------------------------------------         
*                                                                               
         USING LLWORKD,R9                                                       
*                                                                               
LISTLINK NMOD1 LLWORKLN,LSTLNK                                                  
         LR    R9,RC               ADDRESS NMOD SPACE W/R9                      
         L     RC,0(,R1)                                                        
         L     R2,DMGRBUFF         LOOK FOR LINK ELEMENT                        
         AH    R2,DATADISP                                                      
         XR    R1,R1                                                            
*                                                                               
LL10     CLI   0(R2),0             EOR                                          
         BE    LL50                YES                                          
*                                                                               
         CLI   0(R2),X'BA'         LINK EL                                      
         BE    LL20                                                             
         IC    R1,1(R2)                                                         
         LA    R2,0(R1,R2)                                                      
         B     LL10                                                             
*                                                                               
         USING LNKELD,R2                                                        
LL20     MVC   P,SPACES                                                         
         MVC   P+1(17),=C'LINK INFORMATION:'                                    
         GOTO1 =V(UNDERLIN),DMCB,(17,P+1),PSECOND+1                             
         OC    LNKAGJB,LNKAGJB                                                  
         BZ    LL30                                                             
         MVC   P+19(13),=C'AGENCY JOB IS'                                       
         MVC   P+33(12),LNKAGJB                                                 
         B     LLX                                                              
*                                                                               
LL30     MVC   P+19(30),=C'THIS STUDIO JOB IS NOT LINKED.'                      
         B     LLX                                                              
*                                                                               
LL50     DS    0H                  SEE IF ITS A LINKED AGENCY JOB               
         USING SACRECD,R6                                                       
         USING ACTRECD,R2          A(JOB RECORD)                                
         LA    R6,LLKEY                                                         
         L     R2,DMGRBUFF         A(JOB RECORD)                                
         XC    LLKEY,LLKEY                                                      
         MVI   SACKTYP,SACKTYPQ                                                 
         MVI   SACKSUB,SACKSUBQ                                                 
         MVC   SACKCPY,ACTKCPY                                                  
         MVC   SACKAJB,ACTKACT                                                  
         GOTO1 =A(NAHIGH),DMCB,LLKEY,LLKEYSV                                    
         CLC   LLKEY(SACKAJB-SACKEY+L'SACKAJB),LLKEYSV                          
         BNE   LLXX                NO LINKAGE ON JOB                            
*                                                                               
         MVC   P+1(17),=C'LINK INFORMATION:'                                    
         MVC   P+19(17),=C'(STUDIO TYPE/JOB)'                                   
         GOTO1 =V(UNDERLIN),DMCB,(35,P+1),PSECOND+1                             
         LA    R5,P+38                                                          
*                                                                               
*                                                                               
LL100    LA    RF,P                                                             
         LA    RF,L'P-20(RF)                                                    
         CR    RF,R5               ANY ROOM                                     
         BH    LL110               YES                                          
*                                                                               
         GOTO1 AMYREPT,DMCB,(RC)                                                
         LA    R5,P+38                                                          
*                                                                               
LL110    MVC   0(L'SACKSTY,R5),SACKSTY                                          
         MVI   4(R5),C'/'                                                       
         MVC   5(L'SACKSJB,R5),SACKSJB                                          
         MVI   L'SACKAJB+L'SACKSTY+1(R5),C','                                   
*                                                                               
         LA    R5,19(R5)                                                        
*                                                                               
         MVI   SACKPO,X'FF'        GET NEXT LINK                                
         GOTO1 =A(NAHIGH),DMCB,LLKEY,LLKEYSV                                    
         CLC   LLKEY(SACKAJB-SACKEY+L'SACKAJB),LLKEYSV                          
         BE    LL100               PRINT NEXT LINK                              
*                                                                               
         SH    R5,=H'2'                                                         
         MVI   0(R5),C' '          CLEAR TRAILING COMMA                         
*                                                                               
LLX      MVI   SPACING,2           PRINT AND XIT                                
         GOTO1 AMYREPT,DMCB,(RC)                                                
*                                                                               
LLXX     XMOD1                                                                  
         DROP  R2,R6,R9                                                         
*                                                                               
         EJECT ,                                                                
*                                                                               
         USING NAWORKD,R9                                                       
*                                                                               
NAHIGH   NMOD1 NAWORKLN,**NAHI**                                                
         LR    R9,RC                                                            
         MVC   NAAKEY,0(R1)                                                     
         MVC   NAAKEYSV,4(R1)                                                   
         MVC   NAAIO,8(R1)                                                      
         MVC   NACOM,=CL8'DMRDHI'                                               
         B     NADMDIR                                                          
*                                                                               
NADMDIR  L     RE,NAAKEY                                                        
         L     RF,NAAKEYSV                                                      
         MVC   0(ACCKLEN,RF),0(RE)                                              
*                                                                               
         GOTO1 DATAMGR,DMCB,NACOM,=C'ACCDIR',NAAKEY,NAAKEY                      
         XMOD1                                                                  
         DROP  R9                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
* PRINT THE EXCEPTION REASONS IN JXCODES                                        
*----------------------------------------------------------------------         
*                                                                               
         USING JXCEPTD,R5                                                       
*                                                                               
PRTEXCP  LR    R1,RC                                                            
         NMOD1 0,*PREX*                                                         
         LR    RC,R1                                                            
         LA    R5,JXBLOCK                                                       
         CLI   JXNCODES,0                                                       
         BE    PEXNONE                                                          
         MVI   JXMODE,JXMREM       REMOVE REASON 7                              
         MVI   JXCODE,C'7'                                                      
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
         CLI   JXNCODES,0                                                       
         BE    PEXNONE                                                          
*                                                                               
         ZIC   R2,JXNCODES                                                      
         LA    R3,JXCODES                                                       
         MVI   JXMODE,JXMLIT       SET JXALIT TO STRING TO PRINT                
*                                                                               
PEX50    MVC   JXCODE,0(R3)                                                     
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
         USING JXMSGD,R4                                                        
         ICM   R4,15,JXALIT                                                     
         MVC   P+1(L'JXMLONG),JXMLONG                                           
         GOTO1 ACREPORT                                                         
         LA    R3,2(R3)                                                         
         BCT   R2,PEX50                                                         
         B     PEXX                                                             
*                                                                               
PEXNONE  MVC   P+1(4),=C'NONE'                                                  
         GOTO1 ACREPORT                                                         
PEXX     XIT1                                                                   
         DROP  R4,R5                                                            
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        AMYREPT GLOBAL SUBROUTINE TO CALL ACREPORT                             
*----------------------------------------------------------------------         
*                                                                               
MYREPORT NMOD1 0,MYREPT                                                         
         L     RC,0(,R1)                                                        
         CLI   PRTTYPE,PRTALL      PRINTING ALL                                 
         BE    MYREP90             YES                                          
*                                                                               
         MVC   BYTE,PRTFLAG        IS WHAT WE ARE PRINTING NOW ...              
         NC    BYTE,PRTTYPE        WHAT WE WANT?                                
         BZ    MYREPX              NOPE                                         
MYREP90  GOTO1 ACREPORT                                                         
         B     MYREPXX                                                          
*                                                                               
MYREPX   LA    R3,4*L'P            DISCARD PRINT LINE                           
         LA    R2,P                                                             
         XR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  R2,R0                                                            
         MVI   SPACING,1                                                        
MYREPXX  XIT1                                                                   
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        INITITLIZE PRTTYPE WITH THE TYPE(S) OF REPORT YOU WANT                 
*        NOTE - TOTALS ROUTINE REPLACES PRTFLAG WITH PRTALL BEFORE CALL         
*        ING AMYREPT, SO THE WC SUM TOTA ALWAYS PRINT                           
*----------------------------------------------------------------------         
*                                                                               
REPTINIT NMOD1 0,REPINT                                                         
         L     RC,0(,R1)                                                        
         MVI   PRTTYPE,PRTALL                                                   
         CLI   QOPT1,C' '          ANY REPORT FILTERING                         
         BE    REPIX               NOPE                                         
*                                                                               
         CLI   QOPT1,C'W'          WC+DETAIL+DATA                               
         BNE   *+12                                                             
         NI    PRTTYPE,PRTALL-(PRTMP+PRTMPT)   TURN OFF MP SUM                  
         B     REPIX                                                            
*                                                                               
         CLI   QOPT1,C'P'          MP+DETAIL+DATA                               
         BNE   *+12                                                             
         NI    PRTTYPE,PRTALL-(PRTWC+PRTWCT)     NO  WC SUM                     
         B     REPIX                                                            
*                                                                               
         CLI   QOPT1,C'D'          DETAIL ONLY                                  
         BNE   *+12                                                             
         NI    PRTTYPE,PRTALL-(PRTWC+PRTMP+PRTMPT+PRTWCT)                       
         B     REPIX                                                            
*                                                                               
         CLI   QOPT1,C'S'          W/C ONLY                                     
         BNE   *+12                                                             
         NI    PRTTYPE,PRTALL-(PRTDET+PRTMP+PRTMPT)                             
         B     REPIX                                                            
*                                                                               
         CLI   QOPT1,C'M'          M/P ONLY                                     
         BNE   *+8                                                              
         NI    PRTTYPE,PRTALL-(PRTDET+PRTWC+PRTDATA)                            
*                                                                               
REPIX    CLI   PROF24,C'Y'         NO MANPOWER SUMMARY                          
         BNE   *+8                                                              
         NI    PRTTYPE,PRTALL-(PRTMP+PRTMPT)                                    
         CLI   PROF50,C'Y'         FORCE TOTALS                                 
         BNE   *+8                                                              
         OI    PRTTYPE,PRTMPT+PRTWCT                                            
         XIT1                                                                   
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        LIST PROFILES AS DEFINED IN GETOPT                                     
*----------------------------------------------------------------------         
*                                                                               
         USING GOBLOCKD,R3                                                      
*                                                                               
PROFILE  NMOD1 0,*PFIL*                                                         
         L     RC,0(,R1)                                                        
         L     R3,ADGOBLOC                                                      
         MVC   PSAVE(132),SPACES                                                
         LA    R5,PSAVE                                                         
         SR    RF,RF                                                            
         IC    RF,GODUEDAY                                                      
         CH    RF,=H'10'                                                        
         BE    PRO6                                                             
         CVD   RF,DUB                                                           
         MVC   0(7,R5),=C'DUE=NN,'                                              
         UNPK  4(2,R5),DUB+6(2)                                                 
         OI    5(R5),X'F0'                                                      
         LA    R5,7(R5)                                                         
PRO6     CP    GOOVRPER,=P'10000'                                               
         BE    PRO10                                                            
         MVC   0(12,R5),=C'OVER=NNN.NN,'                                        
         EDIT  (P4,GOOVRPER),(6,5(R5)),2,FILL=0                                 
         CLI   5(R5),C'0'                                                       
         BNE   PRO8                                                             
         MVC   5(7,R5),6(R5)                                                    
         BCTR  R5,0                                                             
PRO8     LA    R5,12(R5)                                                        
PRO10    CP    GOMINBIL,=P'5000'                                                
         BE    PRO14                                                            
         MVC   0(12,R5),=C'LOW=NNNN.NN,'                                        
         EDIT  (P4,GOMINBIL),(7,4(R5)),2,FILL=0                                 
         CLI   4(R5),C'0'                                                       
         BNE   PRO12                                                            
         MVC   4(8,R5),5(R5)                                                    
         MVI   11(R5),C' '                                                      
         BCTR  R5,0                                                             
PRO12    LA    R5,12(R5)                                                        
PRO14    CLI   GOCLISUM,C'Y'                                                    
         BE    PRO16                                                            
         MVC   0(11,R5),=C'SUMMARY=NO,'                                         
         LA    R5,11(R5)                                                        
PRO16    CLI   GOPAYNET,C'Y'                                                    
         BNE   PRO17                                                            
         MVC   0(8,R5),=C'PAY=NET,'                                             
         LA    R5,8(R5)                                                         
PRO17    CLI   GOBILDET,C'N'                                                    
         BNE   PRO18                                                            
         MVC   0(10,R5),=C'DETAIL=NO,'                                          
         LA    R5,10(R5)                                                        
PRO18    CLI   GOWCONES,C'Y'                                                    
         BNE   PRO18A                                                           
         MVC   0(14,R5),=C'ESTDETAIL=YES,'                                      
         LA    R5,14(R5)                                                        
PRO18A   CLI   GOCLICD,C'N'                                                     
         BNE   PRO18B                                                           
         MVC   0(8,R5),=C'DISC=NO,'                                             
         LA    R5,8(R5)                                                         
PRO18B   CLI   GONEEDES,C'N'                                                    
         BNE   PRO18C                                                           
         MVC   0(12,R5),=C'ESTIMATE=NO,'                                        
         LA    R5,12(R5)                                                        
PRO18C   CLI   GOAUTOTA,C'Y'                                                    
         BE    PRO18D                                                           
         MVC   0(7,R5),=C'ETA=NO,'                                              
         LA    R5,7(R5)                                                         
PRO18D   CLI   GOESTCOM,C'Y'                                                    
         BE    PRO18E                                                           
         MVC   0(9,R5),=C'ECOMM=NO,'                                            
         LA    R5,9(R5)                                                         
PRO18E   CLI   GOTRANS,C'Y'                                                     
         BE    PRO18G                                                           
         MVC   0(12,R5),=C'TRANSFER=NO,'                                        
         LA    R5,12(R5)                                                        
PRO18G   CLI   GOGRPLCD,C'Y'                                                    
         BNE   PRO18H                                                           
         MVC   0(12,R5),=C'POST=PLUSCD,'                                        
         LA    R5,12(R5)                                                        
PRO18H   DS    0H                                                               
         L     R2,AMONACC                                                       
         USING ACMD,R2                                                          
*                                                                               
         L     R2,ACMAJOBB                                                      
         USING JBLOCKD,R2                                                       
         CLI   JBNEWEST,C'Y'       IS JOB ON NEW ESTIMATES                      
         BE    PRO18H1                                                          
         CLI   JBNEWEST,JBMCSQ     OR MCS ESTIMATES                             
         BNE   PRO18H5                                                          
*                                                                               
PRO18H1  CLI   GONEEDAE,C'Y'       NEED APPROVAL TO BILL ?                      
         BNE   PRO18J              NO                                           
         CLI   JBLOWREV,X'00'      YES, ANY ESTIMATES ?                         
         BE    PRO18J              NO                                           
         OC    JBHIAPP,JBHIAPP     YES, ANY APPROVED ?                          
         BNZ   PRO18J              YES                                          
         B     PRO18H9                                                          
*                                                                               
PRO18H5  DS    0H                  OLD ESTIMATES                                
         SR    R1,R1                                                            
         L     R2,DMGRBUFF                                                      
         AH    R2,DATADISP                                                      
PRO18H6  CLI   0(R2),0                                                          
         BE    PRO18J                                                           
         CLI   0(R2),X'26'                                                      
         BE    PRO18H7                                                          
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     PRO18H6                                                          
*                                                                               
         USING ACJOBD,R2                                                        
PRO18H7  TM    ACJBSTAT,ACJBUNAQ                                                
         BZ    PRO18J                                                           
PRO18H9  MVC   0(10,R5),=C'EST=UNAPP,'                                          
         LA    R5,10(R5)                                                        
PRO18J   CLI   GONEWJUN,C'Y'                                                    
         BNE   PRO19                                                            
         MVC   0(11,R5),=C'JOBS=UNAPP,'                                         
         LA    R5,11(R5)                                                        
PRO19    CLC   PSAVE(132),SPACES                                                
         BE    PROX                                                             
         BCTR  R5,0                                                             
         CLI   0(R5),C','                                                       
         BNE   *+8                                                              
         MVI   0(R5),C' '                                                       
PROX     XIT1                                                                   
         DROP  R2,R3                                                            
*                                                                               
*                                        LITTERALS                              
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        SET UP HEADLINES WITH JOB LEVEL STUFF (DATES, STATUS, ETC)             
*----------------------------------------------------------------------         
*                                                                               
         USING GOBLOCKD,R5                                                      
*                                                                               
GETNAMES NMOD1 0,*GETN*                                                         
         L     RC,0(,R1)                                                        
         L     R5,ADGOBLOC                                                      
         L     R2,DMGRBUFF             MUST HAVE JOB RECORD                     
         CLI   GOEFFOFC,0                                                       
         BE    GET45                                                            
         CLI   PAGEWDTH,132                                                     
         BNE   GET40                                                            
         MVC   HEAD2+119(6),=C'OFFICE'                                          
         MVC   HEAD2+126(2),GOEFFOFC                                            
         B     GET45                                                            
GET40    MVC   HEAD2+99(6),=C'OFFICE'                                           
         MVC   HEAD2+106(2),GOEFFOFC                                            
GET45    MVC   AREA(100),SPACES                                                 
         MVC   AREA(05),=C'TOTAL'                                               
         CLI   GOBILTYP,0                                                       
         BE    GET70                                                            
         CLI   GOBILTYP,C'T'                                                    
         BE    GET60                                                            
         MVC   AREA(8),=C'ONE-LINE'                                             
         CLI   GOBILTYP,C'1'                                                    
         BE    GET60                                                            
         MVC   AREA(10),=C'UNBILLABLE'                                          
         CLI   GOBILTYP,C'U'                                                    
         BE    GET60                                                            
         MVC   AREA(11),=C'PROGRESSIVE'                                         
         CLI   GOBILTYP,C'P'                                                    
         BE    GET60                                                            
         MVC   AREA(11),=CL11'CLIENT'                                           
         CLI   GOBILTYP,C'C'                                                    
         BE    GET60                                                            
         MVC   AREA(100),SPACES                                                 
         EDIT  (4,GOBILAM1),(12,AREA),2,ALIGN=LEFT                              
         LA    R4,AREA+1                                                        
GET50    CLI   0(R4),C' '                                                       
         BE    GET55                                                            
         LA    R4,1(R4)                                                         
         B     GET50                                                            
GET55    MVC   1(3,R4),=C'FEE'                                                  
         CLI   GOBILTYP,C'F'                                                    
         BE    GET60                                                            
         MVC   1(14,R4),=C'SPECIAL AMOUNT'                                      
         CLI   GOBILTYP,C'S'                                                    
         BE    GET60                                                            
         SH    R4,=H'3'                                                         
         MVC   0(20,R4),=C' PERCENT OF ESTIMATE'                                
         CLI   PAGEWDTH,132                                                     
         BE    GET60                                                            
         MVC   0(20,R4),=CL20' PC OF EST'                                       
GET60    CLI   PAGEWDTH,132                                                     
         BNE   GET65                                                            
         MVC   HEAD4+105(27),AREA                                               
         B     GET70                                                            
GET65    MVC   HEAD4+90(27),AREA                                                
GET70    MVI   ELCODEB,X'26'        JOB DATES                                   
         BAS   RE,GETELB                                                        
         BNE   GET80                                                            
         USING ACJOBD,R2                                                        
         LA    R4,HEAD7+109                                                     
         CLI   PAGEWDTH,132                                                     
         BE    GET75                                                            
         LA    R4,HEAD7+95                                                      
GET75    GOTO1 DATCON,DMCB,(1,ACJBCLOS),(8,0(R4))                               
         TM    SVSTAT,X'40'        IF JOB ACTUALLY CLOSED                       
         BZ    GET80                                                            
         MVC   9(3,R4),=C'(C)'     THEN MARK DATE                               
         B     GET80                                                            
GET80    MVI   ELCODEB,X'30'                                                    
         L     R2,DMGRBUFF                                                      
         BAS   RE,GETELB                                                        
         BNE   GET100                                                           
*                                                                               
         USING RSTELD,R2                                                        
         LA    R4,HEAD6+109                                                     
         CLI   PAGEWDTH,132                                                     
         BE    GET85                                                            
         LA    R4,HEAD6+95                                                      
GET85    GOTO1 DATCON,DMCB,(1,RSTBDATE),(8,0(R4))                               
         LA    R4,HEAD8+109                                                     
         CLI   PAGEWDTH,132                                                     
         BE    GET90                                                            
         LA    R4,HEAD8+95                                                      
GET90    GOTO1 DATCON,DMCB,(1,SAVELAST),(8,0(R4))                               
*                                                                               
GET100   DS    0H                                                               
         L     R2,DMGRBUFF                                                      
         MVI   ELCODEB,X'30'                                                    
         BAS   RE,GETELB                                                        
         USING RSTELD,R2                                                        
         LA    R3,HEAD5+92                                                      
         CLI   PAGEWDTH,132                                                     
         BE    GET110                                                           
         LA    R3,HEAD5+77                                                      
*                                                                               
GET110   CLI   RSTFILT1,C' '                                                    
         BE    GET115                                                           
         MVC   0(5,R3),=C'FLT1='                                                
         MVC   5(1,R3),RSTFILT1                                                 
         LA    R3,5(R3)                                                         
*                                                                               
GET115   CLI   RSTFILT2,C' '                                                    
         BE    GET120                                                           
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         MVI   1(R3),C','                                                       
         LA    R3,3(R3)                                                         
         MVC   0(5,R3),=C'FLT2='                                                
         MVC   5(1,R3),RSTFILT2                                                 
         LA    R3,5(R3)                                                         
*                                                                               
GET120   CLI   RSTFILT3,C' '                                                    
         BE    GET125                                                           
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         MVI   1(R3),C','                                                       
         LA    R3,3(R3)                                                         
         MVC   0(5,R3),=C'FLT3='                                                
         MVC   5(1,R3),RSTFILT3                                                 
         LA    R3,5(R3)                                                         
*                                                                               
GET125   CLI   RSTFILT4,C' '                                                    
         BE    GET130                                                           
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         MVI   1(R3),C','                                                       
         LA    R3,3(R3)                                                         
         MVC   0(5,R3),=C'FLT4='                                                
         MVC   5(1,R3),RSTFILT4                                                 
         LA    R3,5(R3)                                                         
*                                                                               
GET130   CLI   RSTFILT5,C' '                                                    
         BE    GETX                                                             
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         MVI   1(R3),C','                                                       
         LA    R3,3(R3)                                                         
         MVC   0(5,R3),=C'FLT5='                                                
         MVC   5(1,R3),RSTFILT5                                                 
*                                                                               
GETX     XIT1                                                                   
         DROP  R2,R5                                                            
*                                                                               
GETELB   AH    R2,DATADISP                                                      
FIRSTELB CLI   0(R2),0                                                          
         BNE   *+10                                                             
         CLI   0(R2),1                                                          
         BR    RE                                                               
         CLI   ELCODEB,0                                                        
         BCR   8,RE                BE INSTRUCTION                               
         CLC   ELCODEB,0(R2)                                                    
         BCR   8,RE                BE                                           
*                                                                               
NEXTELB  SR    RF,RF                                                            
         IC    RF,1(R2)                                                         
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         CLI   1(R2),1             SET NOT EQUAL CC                             
         BR    RE                                                               
         AR    R2,RF                                                            
         B     FIRSTELB                                                         
*                                                                               
ELCODEB  DS    CL1                                                              
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*                                                                               
         USING TABLED,R2                                                        
*                                                                               
SETOPT   NMOD1 0,*SOPT*                                                         
         L     RC,0(,R1)           P1 IS RC                                     
         L     R2,4(,R1)           R2 IS A (OPTION TABLE)                       
         MVI   NARR+1,15           IN CASE NARR CHANGED BY EARLIER REQ          
*                                                                               
         LA    R3,PROF08           SET BILLING COLUMNS                          
         GOTO1 =A(SETBILL),DMCB,(RC),(R2),(R3)                                  
*                                                                               
         MVC   INVNUM+13(8),=C' NUMBER '                                        
         CLI   PROF04,C'Y'       STACK INVNO/DATE                               
         BNE   SOP37                                                            
         MVI   INVDATE,C'S'                                                     
         MVI   INVNUM+1,8          INCREASE LENGTH TO ACCOMODATE DATE           
*                                                                               
SOP37    CLI   PROF11,C'S'         SUPPRESS BOTH CD DEFAULT AND COMM.           
         BE    SOP38                                                            
         MVI   COMM,C'Y'                                                        
         CLI   PROF11,C'E'         WANT COMMISSION, BUT NO CD COLUMN            
         BE    SOP38                                                            
         MVI   DISCDEF,C'Y'                                                     
         CLI   PROF11,0            MAKE COMM A NO PROFILE DEFAULT               
         BE    SOP38                                                            
         CLI   PROF11,C'C'         COMMISSION OPTION (+CD DEFAULT)              
         BE    SOP38                                                            
         MVI   DISCDEF,C'N'                                                     
         MVI   DISCOUNT,C'Y'                                                    
         CLI   PROF11,C'B'         COMMISSION AND CD AMOUNT COLUMNS             
         BE    SOP38                                                            
         MVI   COMM,C'N'                                                        
         CLI   PROF11,C'D'         CD AMOUNT COLUMN                             
         BE    SOP38                                                            
         MVI   DISCOUNT,C'N'       MUST BE JUST CD DEFAULT                      
         MVI   DISCDEF,C'Y'                                                     
         CLI   PROF11,C'F'         FLAG COMISSIONABLE ITEMS                     
         BNE   SOP38                                                            
         MVI   TCOMDEF,C'Y'                                                     
*                                                                               
SOP38    CLI   PROF02,C'Y'         PRINT BATCH REF NO.                          
         BNE   SOP40                                                            
         MVI   BTCH,C'Y'                                                        
*                                                                               
SOP40    CLI   PROF05,C'Y'       PRINT ORDER INFO                               
         BNE   SOP42                                                            
         MVI   ORDNO,C'Y'                                                       
         MVI   ORDAMT,C'Y'                                                      
*                                                                               
SOP42    MVI   OOPRATE,C'Y'        PRINT HOURLY RATE IN NARRATIVE               
         CLI   PROF06,C'Y'         PRINT NARRATIVE?                             
         BE    SOP44               YES.                                         
         CLI   PROF06,C'P'         PRINT RATE IN NARR                           
         BNE   SOP45               NO                                           
         MVI   OOPRATE,C'N'        DONT PRINT HOURLY RATE IN NARRATIVE          
SOP44    MVI   NARR,C'Y'                                                        
*                                                                               
SOP45    CLI   PROF17,C'1'         PRINT ESTIMATE VALUE                         
         BNE   SOP50                                                            
         MVI   ESTVALUE,C'Y'                                                    
         MVC   ESTVALUE+4(24),=C'   NET     ESTIMATE '                          
SOP50    CLI   PROF17,C'2'         PRINT ESTIMATE VALUE (GROSS)                 
         BNE   SOP55                                                            
         MVI   ESTVALUE,C'Y'                                                    
         MVC   ESTVALUE+4(24),=C' GROSS     ESTIMATE'                           
*                                                                               
SOP55    CLI   PROF07,0            CASH COLUMNS                                 
         BE    SOP64                                                            
         CLI   PROF07,C'B'                                                      
         BNE   SOP66                                                            
SOP64    MVI   NET,C'Y'            MAKE BOTH A NO-PROFILE DEFAULT               
         MVI   GROSS,C'Y'                                                       
         B     SOP70                                                            
SOP66    CLI   PROF07,C'N'         INDIVIDUAL COLUMNS ONLY                      
         BNE   SOP68                                                            
         MVI   NET,C'Y'                                                         
         B     SOP70                                                            
SOP68    CLI   PROF07,C'G'                                                      
         BNE   SOP72                                                            
         MVI   GROSS,C'Y'                                                       
         B     SOP72                                                            
*                                                                               
SOP70    MVC   NET+16(7),SPACES                                                 
         MVC   GROSS+16(7),SPACES                                               
         CLI   PROF13,0            SHOW NET/GROSS AS LESS CD                    
         BE    SOP72               DEFAULT IS NET/GROSS                         
         CLI   PROF13,C'N'                                                      
         BE    SOP72                                                            
         MVC   NET+16(7),=C'LESS CD'                                            
         CLI   PROF13,C'1'         SHOW NET ONLY AS LESS CD                     
         BE    SOP72                                                            
         MVC   GROSS+16(7),=C'LESS CD'                                          
*                                                                               
SOP72    CLI   PROF14,C'Y'         SHOW SUPPLIER NAME                           
         BNE   SOP74                                                            
         MVI   SUPPNAME,C'Y'                                                    
         B     SOP80                                                            
SOP74    CLI   PROF14,C'U'         UNDER CODE (NO COLUMN)                       
         BNE   *+8                                                              
         MVI   SUPPNAME,C'U'                                                    
         MVI   SUPPNAME+2,1        OFFSET TO PRINT                              
*                                                                               
SOP80    CLI   PROF26,C'Y'         FLAG HELD                                    
         BNE   *+8                                                              
         MVI   HELD,C'Y'                                                        
*                                                                               
         CLI   PROF30,C'Y'         PRINT UNIT/PRICE COL                         
         BNE   *+8                                                              
         MVI   UNITPRCE,C'Y'                                                    
*                                                                               
SETX     XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT ,                                                                
*                                                                               
         USING TABLED,R2                                                        
*                                                                               
SETTIME  NMOD1 0,*STIM*                                                         
         L     RC,0(,R1)           P1 IS RC                                     
         L     R2,4(,R1)           P2 IS A (OPTION TABLE)                       
         MVI   NARR+1,15           IN CASE NARR CHANGED BY EARLIER REQ          
*                                                                               
         MVI   STAFRATE,C'Y'      PRINT HRS/RATE IN NARRATIVE                   
         CLI   PROF35,C'P'         UNLESS THEY DONT WANT TO                     
         BNE   *+8                                                              
         MVI   STAFRATE,C'N'                                                    
*                                                                               
         LA    R3,PROF42                                                        
         GOTO1 =A(SETBILL),DMCB,(RC),(R2),(R3)                                  
*                                                                               
STI050   CLI   PROF47,C'1'         PRINT ESTIMATE VALUE                         
         BNE   STI060                                                           
         MVI   ESTVALUE,C'Y'                                                    
         MVC   ESTVALUE+4(24),=C'   NET     ESTIMATE '                          
STI060   CLI   PROF47,C'2'         PRINT ESTIMATE VALUE (GROSS)                 
         BNE   STI070                                                           
         MVI   ESTVALUE,C'Y'                                                    
         MVC   ESTVALUE+4(24),=C' GROSS     ESTIMATE'                           
*                                                                               
STI070   CLI   PROF37,C'Y'             PRINT HOUR AND RATE COLS                 
         BNE   STI105                  NO                                       
         MVI   THOURS,C'Y'                                                      
         MVI   TRATE,C'Y'                                                       
         B     STI120                                                           
STI105   CLI   PROF37,C'H'             JUSTR PRINT HOURS                        
         BNE   STI110                                                           
         MVI   THOURS,C'Y'                                                      
         MVI   TRATE,C'A'             RATE IN NARRATIVE                         
         B     STI120                                                           
STI110   CLI   PROF37,C'R'             JUST PRINT RATE                          
         BNE   STI115                                                           
         MVI   TRATE,C'Y'                                                       
         MVI   THOURS,C'A'         HOURS IN THE NARRATIVE                       
         B     STI120                                                           
STI115   CLI   PROF37,C'T'             PRINT HOURS BUT SUPRESS DOLLARS          
         BNE   STI117                                                           
         MVI   THOURS,C'Y'                                                      
         B     STI120                                                           
*                                                                               
STI117   CLI   PROF37,C'S'             STACK HOURS AND RATE                     
         BNE   STI120                                                           
         MVI   THOURS,C'Y'                                                      
         MVI   TRATE,C'S'                                                       
         MVC   THOURS+1(1),TRATE+1     USE LARGER RATE WIDTH                    
         B     STI120                                                           
*                                                                               
STI120   CLI   PROF38,C'Y'             BILLABLE HOURS                           
         BNE   STI125                                                           
         MVI   TBLABLHR,C'Y'                                                    
STI125   CLI   PROF39,C'Y'             BILLED HOURS                             
         BNE   STI180                                                           
         MVI   TBLDHRS,C'Y'                                                     
*                                                                               
STI180   MVI   GROSS,C'Y'              PRINT GROSS IS THE DEFAULT               
         CLI   PROF44,C'N'             DON'T PRINT GROSS                        
         BNE   *+8                                                              
         MVI   GROSS,C'N'                                                       
*                                                                               
STI185   CLI   PROF40,C'1'                                                      
         BNE   STI190                                                           
         MVI   TWOHOURS,C'Y'                                                    
         MVI   TWOAMNT,C'Y'                                                     
         B     STI205                                                           
STI190   CLI   PROF40,C'2'                                                      
         BNE   STI195                                                           
         MVI   TWOAMNT,C'Y'                                                     
         B     STI205                                                           
STI195   CLI   PROF40,C'3'                                                      
         BNE   STI200                                                           
         MVI   TWOAMNT,C'Y'                                                     
         MVI   TWOHOURS,C'Y'                                                    
         MVI   TWONUM,C'Y'                                                      
STI200   CLI   PROF40,C'4'                                                      
         BNE   STI205                                                           
         MVI   TWOAMNT,C'Y'                                                     
         MVI   TWOHOURS,C'Y'                                                    
         MVI   TWONUM,C'Y'                                                      
         MVI   TWODATE,C'Y'                                                     
*                                                                               
STI205   CLI   PROF41,C'Y'        STACK BILL NUM W/O NUM?                       
         BNE   STI210                                                           
         CLI   TWONUM,C'Y'         PRINTING THE WO NUMBER?                      
         BNE   STI210              NO                                           
         CLI   BILLNUM,C'Y'        PRINTING THE BILL NUMBER?                    
         BNE   STI210              NO, FORGET ABOUT WO NUMBER                   
         MVI   TWONUM,C'S'                                                      
*                                                                               
STI210   CLI   PROF45,C'Y'         PRINT COMMISSION?                            
         BNE   STI215                                                           
         MVI   COMM,C'Y'                                                        
*                                                                               
STI215   CLI   PROF45,C'F'         FLAG COMISSIONABLE WITH A 'C'                
         BNE   STI217                                                           
         MVI   TCOMDEF,C'Y'                                                     
*                                                                               
STI217   CLI   PROF46,C'Y'         PRINT BATCH REF                              
         BNE   STI220                                                           
         MVI   BTCH,C'Y'           TURN THIS ON FOR TIME DETAILS                
*                                                                               
STI220   CLI   PROF43,C'Y'        STACK INVNO/DATE                              
         BNE   STI225                                                           
         MVI   INVNUM,C'Y'                                                      
         MVI   INVDATE,C'S'                                                     
         MVI   INVNUM+1,8          INCREASE LENGTH TO ACCOMODATE DATE           
*                                                                               
STI225   CLI   PROF34,C'Y'         PRINT ORDER DETAILS                          
         BNE   STI230                                                           
         MVI   ORDNO,C'Y'                                                       
         MVI   ORDAMT,C'Y'                                                      
*                                                                               
STI230   CLI   PROF36,C'Y'         SHOW SUPPLIER NAME                           
         BNE   STI235                                                           
         MVI   SUPPNAME,C'Y'                                                    
         B     STI240                                                           
STI235   CLI   PROF36,C'U'         UNDER CODE (NO COLUMN)                       
         BNE   *+8                                                              
         MVI   SUPPNAME,C'U'                                                    
         MVI   SUPPNAME+2,1        OFFSET TO PRINT                              
*                                                                               
STI240   CLI   PROF26,C'Y'         FLAG HELD                                    
         BNE   *+8                                                              
         MVI   HELD,C'Y'                                                        
STIX     XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        SET COLUMNS FOR BILLED AMOUNTS FOR TIME AND OOP DETAIL                 
*        R1 IS THE PROFILE VALUE, R2 POINTS TO THE CORECT TABLE                 
*----------------------------------------------------------------------         
*                                                                               
         USING TABLED,R2                                                        
*                                                                               
SETBILL  NMOD1 0,*SETB*                                                         
         L     RC,0(,R1)           P1 IS RC                                     
         L     R2,4(,R1)           P2 IS A(TABLE)                               
         L     R1,8(,R1)           P3 IS PROFILE OPTION                         
         CLI   0(R1),C'D'          BILL DATE ONLY                               
         BNE   SETB5                                                            
         MVI   BILLDTE,C'Y'                                                     
         B     SETBX                                                            
SETB5    CLI   0(R1),C'A'          BILL AMOUNT ONLY                             
         BNE   SETB10                                                           
         MVI   BILLAMT,C'Y'                                                     
         B     SETBX                                                            
SETB10   CLI   0(R1),C'N'          BILL NUMBER ONLY                             
         BNE   SETB12                                                           
         MVI   BILLNUM,C'Y'                                                     
         B     SETBX                                                            
SETB12   CLI   0(R1),C'B'          BILLABLE ONLY                                
         BNE   SETB15                                                           
         MVI   BILLABLE,C'Y'                                                    
         B     SETBX                                                            
SETB15   CLI   0(R1),C'1'          BILL NUMBER/AMOUNT                           
         BNE   SETB20                                                           
         MVI   BILLNUM,C'Y'                                                     
         MVI   BILLAMT,C'Y'                                                     
         B     SETBX                                                            
SETB20   CLI   0(R1),C'2'          BILL NUMBER/DATE                             
         BNE   SETB25                                                           
         MVI   BILLNUM,C'Y'                                                     
         MVI   BILLDTE,C'Y'                                                     
         B     SETBX                                                            
SETB25   CLI   0(R1),C'3'          BILL AMOUNT/DATE                             
         BNE   SETB30                                                           
         MVI   BILLAMT,C'Y'                                                     
         MVI   BILLDTE,C'Y'                                                     
         B     SETBX                                                            
SETB30   CLI   0(R1),C'4'          BILL AMOUNT/DATE/NUMBER                      
         BNE   SETB35                                                           
         MVI   BILLNUM,C'Y'                                                     
         MVI   BILLAMT,C'Y'                                                     
         MVI   BILLDTE,C'Y'                                                     
         B     SETBX                                                            
SETB35   CLI   0(R1),C'5'          BILL AMOUNT/NUMBER/BILLABLE                  
         BNE   SETB40                                                           
         MVI   BILLNUM,C'Y'                                                     
         MVI   BILLAMT,C'Y'                                                     
         MVI   BILLABLE,C'Y'                                                    
         B     SETBX                                                            
SETB40   MVI   BILLDEF,C'Y'           JUST * BILLED TRANSACTIONS                
SETBX    XIT1                                                                   
         DROP  R2                                                               
         EJECT ,                                                                
*                                                                               
         USING SUMMARYD,R2                                                      
*                                                                               
SETSUM   NMOD1 0,*SSUM*                                                         
         L     RC,0(,R1)           P1 IS RC                                     
         L     R2,4(,R1)           P2 IS A (OPTION TABLE)                       
         MVC   SNET+16(7),SPACES                                                
         MVC   SGROSS+16(7),SPACES                                              
         CLI   PROF13,0            SHOW NET/GROSS AS LESS CD                    
         BE    SSU22                                                            
         CLI   PROF13,C'N'                                                      
         BE    SSU22                                                            
         MVC   SNET+16(7),=C'LESS CD'                                           
         CLI   PROF13,C'1'         SHOW NET ONLY AS LESS CD                     
         BE    SSU22                                                            
         MVC   SGROSS+16(7),=C'LESS CD'                                         
SSU22    MVI   PAGEWDTH,132                                                     
         LA    R1,WIDTH132         COLUMN WIDTHS                                
         CLC   RCOUNTRY,=C'UK'                                                  
         BE    SSU24                                                            
         CLI   PROF03,C'Y'         WANT 110 COLUMNS                             
         BNE   SSU26                                                            
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     RF,ADMASTD                                                       
         USING MASTD,RF                                                         
         L     RF,MCVREMOT                                                      
         LTR   RF,RF                                                            
         BZ    SSU26                                                            
         USING REMOTED,RF                                                       
         OC    REMOTKEY(10),REMOTKEY                                            
         BZ    SSU26                                                            
SSU24    LA    R1,WIDTH110                                                      
         MVI   PAGEWDTH,110                                                     
SSU26    L     R2,ASUMTAB          SUMMARY TABLE                                
         USING SUMTABD,R2                                                       
SSU28    MVC   SMTBLEN,0(R1)       FILL IN COLUMN WIDTHS                        
         LA    R2,L'SUMTAB(R2)                                                  
         LA    R1,1(R1)                                                         
         CLI   0(R2),0                                                          
         BNE   SSU28                                                            
         L     R1,AMENUTAB         DECIDE WHICH COLUMNS TO USE                  
SSU30    CLI   0(R1),C'#'          END OF TABLE - DEFAULT                       
         BE    SSU32                                                            
         CLC   0(1,R1),PROF16                                                   
         BE    SSU32                                                            
         LA    R1,L'MENUTAB(R1)                                                 
         B     SSU30                                                            
SSU32    LA    R1,1(R1)            BUMP TO FIRST CHARACTER                      
         L     R2,ASUMTAB                                                       
         USING SUMTABD,R2                                                       
SSU34    MVC   SMTBWANT,0(R1)      MOVE IN Y'S AND N'S                          
         LA    R1,1(R1)                                                         
         LA    R2,L'SUMTAB(R2)                                                  
         CLI   0(R2),0             END OF TABLE                                 
         BNE   SSU34                                                            
         XIT1                                                                   
         DROP  R2,RF                                                            
* COLUMN WIDTHS BASED ON WIDTH OF REPORT                                        
*                                                                               
WIDTH132 DC    AL1(12,12,12,12,10,12,10,12,12,10,10,12,12,10,12,11,10)          
WIDTH110 DC    AL1(10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10)          
NCOLS    EQU   *-WIDTH110          NUMBER OF POSSIBLE COLUMNS                   
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        BUILD TABLE OF PARTICIPANTS FOR A JOB                                  
*----------------------------------------------------------------------         
*                                                                               
GETRET   NMOD1 0,*GRET*                                                         
         L     RC,0(,R1)           P1 IS RC                                     
         L     R5,RBILLTAB         TABLE TO SAVE PARTICIPANTS                   
         ZAP   SVDISTOT,=P'0'      TOTAL UNITS FOR % CALCING                    
         MVC   MYKEY,SPACES        READ RETAIL RECORD                           
         LA    R2,MYKEY                                                         
         USING ACKEYD,R2                                                        
         MVC   ACKEYACC(1),RCCOMPFL     BUILD KEY FOR RETAIL LEDGER             
         MVI   ACKEYACC+1,C'3'                                                  
         MVC   ACKEYACC+2(1),SVDIST     FIRST BYTE IS LEDGER                    
         BAS   R3,RETHIGH                                                       
*                                                                               
GETR96   CLC   MYKEY(3),0(R2)      SAME UNIT/LEDGER?                            
         BNE   GETRX                                                            
         AH    R2,DATADISP                                                      
         XC    RETSTAT,RETSTAT                                                  
GETR10   CLI   0(R2),0                                                          
         BE    GETR50                                                           
         CLI   0(R2),X'14'         GET LEDGER EL                                
         BE    GETR30                                                           
         CLI   0(R2),X'16'         GET HEIRCHY EL                               
         BE    GETR40                                                           
GETR20   ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     GETR10                                                           
*                                                                               
         USING ACLEDGD,R2                                                       
GETR30   MVC   RETSTAT,ACLTSTAT                                                 
         NI    RETSTAT,X'FE'       TURN OFF ALL BUT TYPE=100% BIT               
         B     GETR20                                                           
*                                                                               
         USING ACHEIRD,R2                                                       
GETR40   LA    R1,ACHRLEVD                                                      
         B     GETR20                                                           
*                                                                               
         USING ACKEYD,R2                                                        
GETR50   LA    R2,MYKEY            GET ASTERIC ACCOUNT                          
         MVC   ACKEYACC+3(12),=CL12'************'                               
         BAS   R3,RETHIGH                                                       
         CLC   MYKEY(42),0(R2)                                                  
         BNE   GETRX                                                            
*                                                                               
         USING ACDISTD,R2                                                       
         AH    R2,DATADISP         SEE IF THIS RETAILER IS PART OF THIS         
GETR60   CLI   0(R2),0                                                          
         BE    GETRX               NO, IT IS NOT                                
         CLI   0(R2),X'62'                                                      
         BNE   GETR70                                                           
         CLC   ACDICODE,SVDIST+1                                                
         BNE   GETR70                                                           
         ZAP   SVDISTOT,ACDIVAL    SAVE TOTAL UNITS FOR THIS SCHEME             
         B     GETR80                                                           
GETR70   ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     GETR60                                                           
*                                                                               
GETR80   LA    R2,MYKEY            LOOK FOR JOB LEVEL SCHEME                    
         USING ACKEYD,R2                                                        
         MVC   ACKEYCON,SAVKEY     KEY OF THE JOB I'M WORKING ON                
         BAS   R3,RETHIGH                                                       
         CLC   MYKEY(42),0(R2)                                                  
         BE    GETR85              GET JOB LEVEL SCHEME                         
*                                                                               
*                                  IF THERE IS NO JOB LEVEL SCHEME              
*                                  SEE IF I CAN USE THE SCHEME ALREADY          
*                                  SAVED                                        
         CLC   SVDIST,PREVDIST     IS THE SCHEME THE SAME?                      
         BNE   GETR100             NO, GET NEW SCHEME                           
         TM    PREVSTAT,JOBLVL     IS SAVED SCHEME FROM THE JOB LEVEL           
         BO    GETR100             YES, GET A NEW SCHEME                        
         CLI   0(R5),X'FF'         IS ANYTHING THERE?                           
         BE    GETR100             NO, GET A NEW SCHEME                         
         B     GETRXX              USE THE SAVED SCHEME                         
*                                                                               
GETR85   AH    R2,DATADISP         SEE IF THIS SCHEME AND JOB MATCH             
GETR90   CLI   0(R2),0                                                          
         BE    GETR100             NO JOB LEVEL SCHEME                          
         CLI   0(R2),X'62'                                                      
         BNE   GETR95                                                           
         USING ACDISTD,R2                                                       
         CLC   ACDICODE,SVDIST+1                                                
         BNE   GETR95                                                           
         ZAP   SVDISTOT,ACDIVAL    SAVE TOTAL UNITS FOR THIS SCHEME             
         OI    RETSTAT,JOBLVL      SET FLAG TO GET AT JOB LEVEL                 
         B     GETR100                                                          
GETR95   ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     GETR90                                                           
*                                                                               
GETR100  BAS   R3,RETSEQ           READ LEDGER,                                 
         CLC   MYKEY(3),0(R2)      SAME LEDGER                                  
         BNE   GETRX               NO, I'M DONE                                 
         AH    R2,DATADISP                                                      
GETR110  CLI   0(R2),0                                                          
         BE    GETR130                                                          
         CLI   0(R2),X'32'                                                      
         BE    GETR120                                                          
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     GETR110                                                          
*                                                                               
         USING RBILLD,R5                                                        
         USING ACKEYD,R2                                                        
GETR120  L     R2,IOSPACE       SAVE ACCOUNT                                    
         MVC   RBNUM,ACKEYACC+3                                                 
         OI    RETSTAT,PEND32                                                   
         AH    R2,DATADISP                                                      
*                                                                               
GETR130  TM    RETSTAT,JOBLVL      DO I NEED A JOB LEVEL                        
         BNO   GETR140             NO, THIS RECORDS FINE                        
         L     R2,IOSPACE                                                       
         CLC   ACKEYCON,SAVKEY     DO I HAVE A JOB LEVEL                        
         BNE   GETR100             NO, TRY NEXT RECORD                          
         CLC   ACKEYACC+3(12),RBNUM ARE THESE LOWEST LEVEL HIST RECS            
         BNE   GETR100             NO (IE IS THERE A POSTING ACCOUNT            
*                                  FOR THE KEY OF THIS RECORD                   
*                                                                               
         AH    R2,DATADISP         POINT TO DATA                                
GETR140  TM    RETSTAT,PEND32      HAVE I FOUND THE POSTING RECORD YET          
         BNO   GETR100             NO                                           
         CLI   0(R2),0                                                          
         BE    GETR100             NO JOB LEVEL SCHEME                          
         CLI   0(R2),X'62'                                                      
         BNE   GETR150                                                          
         USING ACDISTD,R2                                                       
         CLC   ACDICODE,SVDIST+1                                                
         BNE   GETR150                                                          
         USING RBILLD,R5                                                        
         ZAP   RBUNITS,ACDIVAL     SAVE THE UNITS                               
         USING ACKEYD,R2                                                        
         LA    R5,RBLEN(R5)                                                     
         MVI   0(R5),X'FF'         E-O-D MARKER                                 
         NI    RETSTAT,X'FF'-PEND32                                             
         B     GETR100                                                          
*                                                                               
GETR150  ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     GETR140                                                          
*                                                                               
GETRX    MVI   0(R5),X'FF'         E-O-D MARKER                                 
GETRXX   XIT1                                                                   
*                                                                               
RETHIGH  MVC   COMMAND,=CL8'DMRDHI'                                             
         B     *+10                                                             
RETSEQ   MVC   COMMAND,DMRSEQ                                                   
         L     R2,IOSPACE                                                       
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',MYKEY,(R2)                      
         BR    R3                                                               
         DROP  R2,R5                                                            
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PUT A TRANSACTION RECORD TO SORT                                       
*----------------------------------------------------------------------         
*                                                                               
         USING JXCEPTD,R5                                                       
*                                                                               
TRANSACT NMOD1 0,*TRAN*                                                         
         L     RC,0(,R1)           P1 IS RC                                     
         LA    R5,JXBLOCK                                                       
         MVI   JXMODE,JXMPRTRN     INITIALIZE JXBLOCK                           
         L     R1,ADTRANS                                                       
         SH    R1,DATADISP                                                      
         ST    R1,JXAREC                                                        
*                                                                               
         USING ACMD,R1                                                          
         L     R1,AMONACC                                                       
         MVC   JXAJBBLK,ACMAJOBB   A(JOBBER BLOCK)                              
         MVC   JXAGOBLK,ADGOBLOC   A(GETOPT BLOCK)                              
         ZAP   JXEST,ESTIMATE      NET ESTIMATE VALUE                           
         ZAP   JXGEST,ESTGROSS     GROSS ESTIMATE VALUE                         
         ZAP   JXHREST,HRESTMAT    HIGHEST REVISION ESTIMATE VALUE              
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
         DROP  R1,R5                                                            
*                                                                               
         USING ACMD,RE                                                          
         L     RE,AMONACC          SET ADTRANS TO A(HISTORY TRNS)               
         TM    ACMHIST,ACMHPRC     PROCESSING A HISTORY RECORD                  
         BZ    TRAN01              NO                                           
*                                                                               
         L     RF,ACMHIO           TRAN IS SOMEWHERE ELSE                       
         AH    RF,DATADISP                                                      
         ST    RF,ADTRANS                                                       
         DROP  RE                                                               
*                                                                               
TRAN01   L     R2,ADTRANS                                                       
         L     R3,SORTAREA                                                      
         SH    R2,DATADISP                                                      
         USING SORTD,R3                                                         
         USING ACKEYD,R2                                                        
*                                                                               
         CLI   QHIST,C'Y'          READING THE ARCHIEVE                         
         BE    TRAN05              YES, ACCEPT PEELED JOBS                      
         OC    ACDTPEEL,ACDTPEEL   HAS THIS JOB BEEN PEELED?                    
         BZ    TRAN05              NO                                           
         CLC   ACDTPEEL,TODAY2     GIVE THEM 1 LAST SHOT AT A REPORT            
         BNL   TRAN05                                                           
*        MVI   FCRDTRNS,C'N'       REJECT PEELED JOBS                           
         B     TRANXXXX            AS PER LRES, ONLY REJECT PEELED TRNS         
*                                                                               
TRAN05   XC    SRTLRECL,SRTLRECL                                                
         XC    SRTKEY(SRTDATAL),SRTKEY                                          
         XC    SRTDATA(255),SRTDATA                                             
         XC    SRTDATA+255(145),SRTDATA+255                                     
         MVC   SRTCLI,3(R2)                                                     
         MVC   SRTPRO,6(R2)                                                     
         MVC   SRTJOB,9(R2)                                                     
         MVC   SRTCA,ACKEYCON+1                                                 
         MVC   SRTWC,ACKEYWRK                                                   
         MVC   SRTDATE,ACKEYDTE                                                 
         MVC   SRTNUM,ACKEYREF                                                  
         MVC   SRTSBR,ACKEYSBR                                                  
*                                                                               
         CLC   ACKEYWRK(2),=C'99'   IS THIS A BILLING                           
         BE    *+8                 YES,                                         
         OI    JOBSTAT,CHARGES     NO, SET 'THERE ARE CHARGES' FLAG             
*                                                                               
         CLI   PROF33,C'Y'         SEPARATE TIME AND OOPS TRANSACTIONS          
         BNE   TRNB                NO                                           
         MVI   SRTTYPE,SRTTOOP     OOPS ARE SORTED LAST                         
         CLC   ACKEYWRK(2),=C'99'   IS THIS A BILLING                           
         BNE   TRNA1               NO                                           
*                                                                               
         OI    JOBSTAT,BILLING     JOB HAS BILLING                              
         TM    JOBSTAT,CHARGES     ARE THEIR CHARGES ON THE JOB                 
         BO    *+12                YES, WILL PROCESS BILLING SEPARTLY           
         MVI   SRTTYPE,0                                                        
         B     TRNB                                                             
         MVI   SRTTYPE,SRTTBILL                                                 
         B     TRNB                                                             
* -----------------------------------------------------------------             
* SET TYPE OF TIME                                                              
* -----------------------------------------------------------------             
TRNA1    CLI   WCP_TIME,C'Y'        IS THIS A WC P AS TIME WORKCODE             
         BE    TRNA                YES                                          
*                                                                               
         CLI   PROF48,C'Y'         TIME V OOP BASED ON WORKCODE TYPE            
         BNE   TRNAA               NO, CHECK CONTRA                             
         CLI   WTYPE,TIMEWC       IS THIS A TIME WORKCODE                       
         BE    TRNA                YES, SET AS TIME CHARGE                      
         B     TRNB                ELSE OOP WORKCODE                            
*                                                                               
TRNAA    CLI   PROF23,C'Y'         CONSIDER SK TIME                             
         BNE   TRNAAA              NO                                           
         CLC   ACKEYCON+1(2),=C'SK' IS THIS A TIME CHARGE?                      
         BE    TRNA                YES                                          
*                                                                               
TRNAAA   CLC   ACKEYCON+1(2),=C'1R' IS THIS A TIME CHARGE?                      
         BNE   TRNB                NO                                           
*                                                                               
TRNA     MVI   SRTTYPE,SRTTTIME    FLAG TRANSACTION AS TIME TRANSACTION         
*                                                                               
TRNB     DS    0H                                                               
         CLI   SRTBYOFF,C'Y'                                                    
         BNE   TRNC                                                             
         L     R5,ADGOBLOC                                                      
         USING GOBLOCKD,R5                                                      
         MVC   SRTOFF,GOEFFOFC                                                  
         DROP  R5                                                               
TRNC     LA    R3,SRTDATA         BUMP R3 UP                                    
         USING SR44D,R3                                                         
         MVC   SR44USED,ACDTUSED                                                
*                                                                               
         USING TRANSD,R2                                                        
         AH    R2,DATADISP                                                      
         CLC   TRNSANAL,=C'**'         PURCHACE ORDER?                          
         BNE   TRNS44                  NO                                       
*                                                                               
         USING ACOAMTD,R2                                                       
         MVI   ELCODE,X'68'                                                     
         BAS   RE,NEXTELA                                                       
         BNE   TRNSO3                                                           
*                                                                               
TRNSO1   CLI   PROF09,C'Y'             MERGE INTO MAIN W/C                      
         BNE   TRNSO2                                                           
*                                                                               
         L     R3,SORTAREA             POINT  TO THE KEY                        
         USING SORTD,R3                                                         
         MVC   SRTWC,ACOAWC                                                     
*                                                                               
TRNSO2   L     R3,SORTAREA             POINT TO THE DATA                        
         USING SORTD,R3                                                         
         LA    R3,SRTDATA                                                       
         USING SR68D,R3                                                         
         MVI   SR68ID,X'68'                                                     
         LA    R0,SR68LN1                                                       
         STC   R0,SR68LEN                                                       
         MVC   SR68WC,ACOAWC                                                    
         MVC   SR68AMNT,ACOAMT                                                  
         MVC   SR68AVIL,ACOAIVAL                                                
*                                                                               
         LA    R3,SR68LN1(R3)                                                   
         L     R4,SORTAREA                                                      
         MVI   0(R3),0             END OF RECORD                                
         LA    R3,1(R3)                                                         
         SR    R3,R4               ONE PAST LAST MINUS FIRST IS LENGTH          
         STH   R3,0(R4)                                                         
*                                                                               
         GOTO1 ADSORTER,DMCB,=C'PUT',(R4)                                       
         MVI   SORTANY,1           INDICATE WE SORTED SOMETHING                 
*                                                                               
         BAS   RE,NEXTELA                                                       
         BE    TRNSO1                                                           
*                                                                               
TRNSO3   B     TRANXXXX                                                         
*                                                                               
         USING SR44D,R3                                                         
         USING TRANSD,R2                                                        
TRNS44   TM    TRNSSTAT,X'80'                                                   
         BNO   TRNS44A                                                          
         AP    ADJBAL,TRNSAMNT                                                  
         B     TRNS44B                                                          
TRNS44A  SP    ADJBAL,TRNSAMNT                                                  
TRNS44B  MVI   SR44ID,X'44'                                                     
         ZAP   SR44AMNT,TRNSAMNT                                                
         MVC   SR44TYPE,TRNSTYPE                                                
         MVC   SR44STAT,TRNSSTAT                                                
         MVC   SR44DATE,TRNSDATE                                                
         MVC   SR44BTCH,TRNSBTCH                                                
         USING ACMD,R4                                                          
         L     R4,AMONACC                                                       
         MVC   SR44MOS,ACMMDTE                                                  
         DROP  R4                                                               
         LA    R4,SR44NARR                                                      
         CLC   TRNSANAL,=C'99'     IS THIS BILLING                              
         BNE   TRNS441                                                          
*                                                                               
* PATCH FOR BAD WORKCODE 99'S ON ICCNJ                                          
         CLI   TRNSTYPE,1                                                       
         BE    TRANXXXX                                                         
*                                                                               
*                                                                               
         ZIC   R1,=X'35'           ONLY MOVE 36 OF THE NARRTIVE                 
         B     TRNS442                                                          
TRNS441  ZIC   R1,TRNSLEN                                                       
         SH    R1,=H'29'                                                        
         BM TRNS4420                                                            
TRNS442  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),TRNSNARR   SAVE NARRATIVE                                
*                                                                               
TRNS4420 CLC   TRNSANAL,=C'99'     IS THIS BILLING                              
         BNE   TRNS4422                                                         
         CLC   TRNSNARR(7),=C'BILL NO'   UK FUCKUP BILL?                        
         BNE   TRNS4422                  NO                                     
*                                                                               
         ZAP   SR44NARR+15(6),=P'0'      PRIME BUCKETS                          
         ZAP   SR44NARR+21(6),=P'0'                                             
         ZAP   SR44NARR+27(6),SR44AMNT                                          
*                                                                               
TRNS4422 LA    R1,1(R1)           CORRECT THE LENGTH OF THE NARRATIVE           
         LA    R1,SR44LN1(R1)     ADD LENGTH OF EL TO L'NARRATIVE               
         STC   R1,SR44LEN         STORE IT                                      
         AR    R3,R1              BUMP R3 WITH R1                               
*                                                                               
         XC    BYTE,BYTE                                                        
         TM    TRNSSTAT,1          IS THIS A NON COMM TRANSACTION               
         BNO   *+8                                                              
         OI    BYTE,NONCOM         SET FLAG TO GET CORRECT RATE                 
         CLI   TRNSTYPE,57         IS THIS A WRITE OFF TRANSACTION              
         BE    TRNS442A                                                         
         CLI   TRNSTYPE,58         IS THIS A WRITE OFF TRANSACTION              
         BNE   TRNS443                                                          
TRNS442A OI    BYTE,TYPE57         SET FLAG TO GET CORRECT HOURS                
*                                                                               
TRNS443  MVI   ELCODE,X'40'                                                     
         BAS   RE,NEXTELA                                                       
         BNE   TRNX1                                                            
         USING SR40D,R3                                                         
         USING ACPERSD,R2                                                       
         MVI   SR40ID,X'40'                                                     
         LA    R0,SR40LN1                                                       
         STC   R0,SR40LEN                                                       
         MVC   SR40RATE,ACPSRATE                                                
         ZAP   SR40HOUR,=P'0'                                                   
         TM    BYTE,TYPE57         IS THIS A TYPE 57                            
         BO    *+10                GET HOURS FROM THE 4B                        
         MVC   SR40HOUR,ACPSHOUR                                                
         LA    R3,SR40LN1(R3)                                                   
*                                                                               
         USING ACMD,R4                                                          
TRNX1    L     R4,AMONACC                                                       
         L     R2,ACMAPRO2                                                      
         CLI   0(R2),PTAELQ        IS THERE A PTA ELEMENT IN THE BUFFER         
         BE    TRNX21              YUP                                          
         B     TRNX4                                                            
         DROP  R4                                                               
*                                                                               
         USING PTAELD,R2                                                        
TRNX2    MVI   ELCODE,PTAELQ                                                    
         BAS   RE,NEXTELA                                                       
         BNE   TRNX4                                                            
*                                                                               
TRNX21   TM    PTASTAT1,PTASPEND   IS THIS PENDING?                             
         BNO   TRNX21A             NO                                           
         CLI   PTATYPE,PTATRAL     BILLING                                      
         BNE   TRNX2               NO, PENDING NOT NEEDED FOR NON BILLS         
*                                                                               
         USING SR4BD,R3                                                         
TRNX21A  MVI   SR4BID,X'4B'                                                     
         CLI   PTATYPE,PTATWOF     WRITE OFF ELEMENT                            
         BE    TRNX22                                                           
         CLI   PTATYPE,PTATWOFR    W/O RECOVERY ELEMENT                         
         BNE   TRNX25                                                           
*                                                                               
TRNX22   TM    BYTE,TYPE57         IS THIS A W/O TRAN                           
         BNO   *+8                 NO,                                          
         MVI   SR4BID,X'4F'        FLAG AS WRITE OFF 4B                         
*                                                                               
         MVC   SR4BNO,PTAWREF                                                   
*&&DO                                                                           
         MVI   SR4BNO,C'W'         SAVE WRITEOFF NUMBER                         
         MVI   SR4BNO+1,C'O'       ASSUME WRITE OFF                             
         CLI   PTATYPE,PTATWOFR    IS IT A RECOVERY                             
         BNE   *+8                 NO                                           
         MVI   SR4BNO+1,C'R'       START WITH WR                                
         MVC   DUB(2),PTASEQN      NOMBER IS STORED AS ITS COMPLEMENT           
*        XC    DUB(2),=X'FFFF'                                                  
         LH    RF,DUB                                                           
         LCR   RF,RF                                                            
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SR4BNO+2(4),DUB                                                  
*&&                                                                             
         ZAP   SR4BAMNT,PTANET                                                  
         BP    *+8                                                              
         OI    BYTE,NEGAMNT      IF NEG AMOUNT MAKE SURE OF NEG HRS             
         GOTO1 DATCON,DMCB,(1,PTAWDAT),(2,SR4BDTE) YMD TO COMP.                 
         LH    RE,PTAHOURS                                                      
         LTR   RE,RE                                                            
         BNM   *+8                                                              
         OI    BYTE,NEGHRS                                                      
         CVD   RE,DUB                                                           
         ZAP   SR4BHRS,DUB                                                      
         ZAP   SR4BRATE,=P'0'                                                   
         ZAP   SR4BCSD,=P'0'                                                    
         TM    BYTE,TYPE57         WAS THIS A TYPE57                            
         BNO   TRNX3A              NO                                           
*                                                                               
         CP    SR4BAMNT,=P'0'      ZERO AMOUNT                                  
         BNE   *+8                 NO                                           
         NI    BYTE,X'FF'-NEGAMNT-NEGHRS  YES, SIGN IS IRRELEVENT               
*                                                                               
         TM    BYTE,NEGAMNT+NEGHRS HOURS AND AMOUNT MUST HAVE SAME SIGN         
         BNM   *+10                IE. NOT MIXED SIGN                           
         MP    SR4BHRS,=P'-1'      REVERSE SIGN OF HOURS                        
         DROP  R3                                                               
         USING SR40D,R2            READJUST HOURS IN TYPE 57'S                  
         ST    R2,FULL             SAVE ADDRESS OF PTA ELEMENT                  
         L     R2,SORTAREA                                                      
         USING SORTD,R2                                                         
         LA    R2,SRTDATA                                                       
         USING SR40D,R2                                                         
         MVI   ELCODE,X'40'        FOR WRITE OFFS WE NEED 4B HOURS              
         BAS   RE,NEXTELA          POINT TO THE SR40 ELEMENT                    
         BNE   TRNX24                                                           
         TM    BYTE,NEGAMNT+NEGHRS HOURS AND AMOUNT MUST HAVE SAME SIGN         
         BNM   *+10                IE. NOT MIXED SIGN                           
         MP    DUB,=P'-1'          REVERSE SIGN OF HOURS                        
         AP    SR40HOUR,DUB        AP IN 4B HOURS                               
TRNX24   L     R2,FULL             RESTORE R2 FOR GETEL                         
         USING SR4BD,R3                                                         
         B     TRNX3A              SET LENGTHS, BUMP                            
*                                                                               
         USING PTAELD,R2                                                        
TRNX25   CLI   PTATYPE,PTATRAL     BILLING OR ALLOCATION?                       
         BNE   TRNX2               NO, NEXTEL                                   
         MVC   SR4BNO,SPACES       ASSUME ONLY ALLOCATED                        
         TM    PTASTAT1,PTASPEND   IS THIS PENDING?                             
         BO    *+10                YES, ALLOCATED TO US                         
         MVC   SR4BNO,PTARBLNO                                                  
         ZAP   SR4BAMNT,PTANET                                                  
         MVC   SR4BDTE,PTARBLDT                                                 
*                                                                               
         ZAP   SR4BRATE,=P'0'                                                   
         ZAP   SR4BCSD,=P'0'                                                    
*                                                                               
         TM    BYTE,NONCOM                                                      
         BO    *+10                                                             
         ZAP   SR4BRATE,PTARCORT  ZAP IN COMMISION RATE                         
*                                                                               
         ZAP   SR4BCSD,PTACDSC                                                  
         LH    RE,PTAHOURS        IF THERE ARE HOURS PRESENT                    
         CVD   RE,DUB                                                           
         ZAP   SR4BHRS,DUB                                                      
*                                                                               
TRNX3A   LA    R0,SR4BLEN2                                                      
         STC   R0,SR4BLEN                                                       
         LA    R3,SR4BLEN2(R3)                                                  
         B     TRNX2                                                            
         DROP  R3,R2                                                            
*                                                                               
TRNX4    L     R2,ADTRANS                                                       
         MVI   ELCODE,X'50'                                                     
TRNX4A   BAS   RE,NEXTELA                                                       
         BNE   TRNX5                                                            
         USING TRCASHD,R2                                                       
         CLI   TRCSTYPE,C'D'       CASH D                                       
         BNE   TRNX4D             NO                                            
         USING SR50D,R3                                                         
         LA    R0,SR50LN1                                                       
         STC   R0,SR50LEN                                                       
         MVI   SR50ID,X'50'                                                     
         ZAP   SR50AMNT,TRCSAMNT                                                
         BAS   RE,CHKBLCD                                                       
         LA    R3,SR50LN1(R3)                                                   
         B     TRNX4A                                                           
*                                                                               
TRNX4D   CLI   TRCSTYPE,C'S'       XJOB POSTING                                 
         BNE   TRNX4A             NO                                            
         USING SR50D,R3                                                         
         LA    R0,SR50LN1                                                       
         STC   R0,SR50LEN                                                       
         MVI   SR50ID,X'51'        SAVE XJOB POSTINGS W/EL ID 51                
         ZAP   SR50AMNT,TRCSAMNT                                                
         LA    R3,SR50LN1(R3)                                                   
         B     TRNX4A                                                           
*                                                                               
TRNX5    L     R2,ADTRANS                                                       
         USING TRTRANSD,R2                                                      
         USING SR4ED,R3                                                         
         MVI   ELCODE,X'4E'                                                     
         BAS   RE,NEXTELA                                                       
         BNE   TRNX6                                                            
         MVI   SR4EID,X'4E'                                                     
         LA    R0,SR4ELN1                                                       
         STC   R0,SR4ELEN                                                       
         MVC   SR4EACC,TRTRACC+1                                                
         MVC   SR4ETYPE,TRTRTYPE                                                
         MVC   SR4EDATE,TRTRDATE                                                
         LA    R3,SR4ELN1(R3)                                                   
*                                                                               
TRNX6    L     R2,ADTRANS                                                       
         MVI   ELCODE,X'23'                                                     
         BAS   RE,NEXTELA                                                       
         BNE   TRNX8                                                            
         USING ACOTHERD,R2                                                      
         USING SR23D,R3                                                         
         MVC   SR23NUM,ACOTNUM                                                  
         MVI   SR23ID,X'23'                                                     
         LA    R0,SR23LN1                                                       
         STC   R0,SR23LEN                                                       
         LA    R3,SR23LN1(R3)                                                   
*                                                                               
TRNX8    L     R2,ADTRANS                                                       
         MVI   ELCODE,X'25'                                                     
         BAS   RE,NEXTELA                                                       
         BNE   TRNX9                                                            
         USING SR25D,R3                                                         
         USING ACNOD,R2                                                         
         MVI   SR25ID,X'25'                                                     
         MVC   SR25NO,ACNO                                                      
         LA    R0,SR25LN1                                                       
         STC   R0,SR25LEN                                                       
         LA    R3,SR25LN1(R3)                                                   
*                                                                               
         USING TRANSD,R2                                                        
TRNX9    L     R2,ADTRANS          GET 4C ACCOUNT                               
*                                                                               
         TM    BYTE,TYPE57         SUBSID ACCOUNT INFO ON W/O'S IS IN           
         BO    TRNX12              2C ELEMENT                                   
*                                                                               
         USING SR4CD,R3                                                         
         USING TRSDESCD,R2                                                      
TRNX10   MVI   ELCODE,X'4C'                                                     
         BAS   RE,NEXTELA                                                       
         BNE   TRNX13                                                           
         MVC   SR4CACCT,SPACES                                                  
         ZIC   R1,TRSDLEN                                                       
         CH    R1,=H'16'           MAX ACCOUNT CODE LEN IS 14                   
         BNH   *+8                                                              
         LA    R1,16                                                            
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SR4CACCT(0),TRSDACCS                                             
         MVI   SR4CID,X'4C'                                                     
         LA    R1,SR4CLN1                                                       
         STC   R1,SR4CLEN                                                       
         LA    R3,SR4CLN1(R3)                                                   
         B     TRNX13                                                           
*                                                                               
TRNX12   MVI   ELCODE,X'2C'        LOOK FOR SUBSIDARY POSTING ON W/O'S          
         MVC   SR4CACCT,SPACES     SORT IT IN THE 4/C INFO                      
         MVC   SR4CACCT(4),=C'SIWO'  DEFAULT W/O SUBSIDARY ACCOUNT              
         BAS   RE,NEXTELA                                                       
         BNE   TRNX12A             FINISH UP BUILDING THE ELEMENT               
         USING ACSPECD,R2                                                       
         TM    ACSPTYP,ACSPOWO     IS THE A W/O ACCOUNT HERE?                   
         BNO   TRNX12A                                                          
         MVC   SR4CACCT,ACSPACCT                                                
TRNX12A  MVI   SR4CID,X'4C'                                                     
         LA    R1,SR4CLN1                                                       
         STC   R1,SR4CLEN                                                       
         LA    R3,SR4CLN1(R3)                                                   
*                                                                               
         USING TRBUETD,R2                                                       
         USING SR4BD,R3                                                         
TRNX13   B     TRNX15              NOTE PRORATA CONVERTS 5C'S TO OFFSET         
*                                  PTAS                                         
         L     R2,ADTRANS                                                       
         MVI   ELCODE,X'5C'        STORE CLIENT BILL UNBILL DATA                
*                                                                               
TRNX13A  BAS   RE,NEXTELA          AS OFSETTING 4B'S                            
         BNE   TRNX15              NONE FOUND                                   
*                                                                               
         MVI   SR4BID,X'4B'                                                     
         MVC   SR4BNO,TRBUNO       ORIGINAL BILL NUMBER                         
*                                                                               
         ICM   RF,15,TRBUAMNT                                                   
         CVD   RF,DUB                                                           
         ZAP   SR4BAMNT,DUB        ORIG BILL AMOUNT                             
         MVC   SR4BDTE,TRBUDTE     BILLED DATE                                  
         ZAP   SR4BRATE,=P'0'     ZAP COMMISION                                 
         TM    BYTE,NONCOM                                                      
         BO    TRNX13C                                                          
         TM    TRBUCMST,X'80'     PACKED COMMISION                              
         BNO   TRNX13B            NO                                            
         ZAP   SR4BRATE,TRBUCMP   ZAP IN COMMISION                              
         B     TRNX13C                                                          
*                                                                               
TRNX13B  ICM   RF,15,TRBUCMR      BINARY COMISSION                              
         CVD   RF,DUB                                                           
         ZAP   SR4BRATE,DUB                                                     
*                                                                               
TRNX13C  ICM   RF,15,TRBUCSD       CASH DISC?                                   
         CVD   RF,DUB                                                           
         ZAP   SR4BCSD,DUB                                                      
*                                                                               
         ICM   RF,15,TRBUHRS       IF THERE ARE HOURS PRESENT                   
         CVD   RF,DUB                                                           
         ZAP   SR4BHRS,DUB                                                      
*                                                                               
         LR    R4,R3               SAVE A(ORIGINAL BILL 4B)                     
         LA    R0,SR4BLEN2                                                      
         STC   R0,SR4BLEN                                                       
         LA    R3,SR4BLEN2(R3)     BUMP R3 TO FREE SPACE                        
*                                                                               
         MVC   0(SR4BLEN2,R3),0(R4) CREATE REVERSAL 4B                          
         MVC   SR4BNO,TRBUUNNO     REVERSAL BILL NUM                            
         MVC   SR4BDTE,TRBUUND     DATE REVERSED                                
*                                  REVERSE AMOUNTS                              
         ZAP   DUB,SR4BAMNT                                                     
         MP    DUB,=P'-1'                                                       
         ZAP   SR4BAMNT,DUB                                                     
         ZAP   DUB,SR4BHRS                                                      
         MP    DUB,=P'-1'                                                       
         ZAP   SR4BHRS,DUB                                                      
         ZAP   DUB,SR4BCSD                                                      
         MP    DUB,=P'-1'                                                       
         ZAP   SR4BCSD,DUB                                                      
         LA    R0,SR4BLEN2                                                      
         STC   R0,SR4BLEN                                                       
         LA    R3,SR4BLEN2(R3)     BUMP R3 AGAIN                                
         B     TRNX13A             GET NEXT 5C                                  
         DROP  R3,R2                                                            
*                                                                               
TRNX15   L     R2,ADTRANS                                                       
*                                                                               
         USING TRANSD,R2                                                        
         CLC   TRNSANAL,=C'99'     IS THIS BILLING                              
         BNE   TRNX16              NO                                           
*                                                                               
         MVI   ELCODE,VBIELQ                                                    
TRNX15A  BAS   RE,NEXTELA          LOOK FOR GST                                 
         BNE   TRNX15D             NONE                                         
*                                                                               
         USING SRGSD,R3                                                         
         USING VBIELD,R2                                                        
         MVI   SRGSID,VBIELQ                                                    
         MVI   SRGSLEN,SRGSLN1                                                  
         MVC   SRGSTYPE,VBITYPE                                                 
         MVC   SRGSRATE,VBIRATE                                                 
         MVC   SRGSINDS,VBIINDS                                                 
         ZAP   SRGSAMNT,VBIVAT                                                  
         LA    R3,SRGSLN1(R3)                                                   
         B     TRNX15A                                                          
*                                                                               
TRNX15D  MVI   ELCODE,PBIELQ                                                    
         L     R2,ADTRANS                                                       
*                                                                               
TRNX15G  BAS   RE,NEXTELA          LOOK FOR PST                                 
         BNE   TRNX16              NONE                                         
*                                                                               
         USING SRPSD,R3                                                         
         USING PBIELD,R2                                                        
         MVI   SRPSID,PBIELQ                                                    
         MVI   SRPSLEN,SRPSLN1                                                  
         MVC   SRPSTYPE,PBITYPE                                                 
         MVC   SRPSRATE,PBIRATE                                                 
         MVC   SRPSINDS,PBIINDS                                                 
         ZAP   SRPSAMNT,PBIPST                                                  
         MVC   SRPSPRV,PBIPRV                                                   
*                                                                               
         USING VTCD,R5             GET PROVINCE NAME                            
         XC    AREA,AREA                                                        
         LA    R5,AREA                                                          
         MVI   VTCACTN,VTCANAME                                                 
         MVC   VTCCOMF,ADCOMFAC                                                 
         MVC   VTCPRV,PBIPRV       SET PROVINCE CODE                            
         MVC   SRPSNAME,SPACES     CLEAR IN CASE NOT FOUND                      
         GOTO1 =V(VATICAN),(R5)                                                 
         MVC   SRPSNAME,VTCPRVD    GET THE DESCRIPTION                          
         LA    R3,SRPSLN1(R3)                                                   
         B     TRNX15G                                                          
         DROP  R5                                                               
*                                                                               
         USING APEELD,R2                                                        
TRNX16   L     R2,ADTRANS          SAVE ANALYSIS POINTERS                       
         MVI   ELCODE,APEELQ                                                    
         BAS   RE,NEXTELA                                                       
         BNE   TRNX17                                                           
         ZIC   R1,APELN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)       SAVE ENTIRE APEEL                            
         LA    R1,1(R1)            RESTORE R3 TO ACTUAL LENGTH                  
         LA    R3,0(R1,R3)         BUMP R3                                      
*                                                                               
TRNX17   L     R2,ADTRANS                                                       
         MVI   ELCODE,X'60'                                                     
         BAS   RE,NEXTELA                                                       
         BNE   TRNX18                                                           
         USING SR60D,R3                                                         
         USING TRSTATD,R2                                                       
         MVI   SR60ID,X'60'                                                     
         MVC   SR60RMOS,TRSTRMOS                                                
         LA    R0,SR60LN1                                                       
         STC   R0,SR60LEN                                                       
         LA    R3,SR60LN1(R3)                                                   
*                                                                               
TRNX18   L     R2,ADTRANS                                                       
         MVI   ELCODE,X'7C'                                                     
         BAS   RE,NEXTELA                                                       
         BNE   TRNX19                                                           
         USING SR7CD,R3                                                         
         USING UNPELD,R2                                                        
         MVI   SR7CID,X'7C'                                                     
         MVC   SR7CPRCE,UNPRICE                                                 
         MVC   SR7CSTAT,UNPSTAT                                                 
         MVC   SR7CUNIT,UNPUNIT                                                 
         LA    R0,SR7CLN1                                                       
         STC   R0,SR7CLEN                                                       
         LA    R3,SR7CLN1(R3)                                                   
*                                                                               
         USING ACMD,R4                                                          
TRNX19   L     R4,AMONACC                                                       
         L     R2,ACMAPROB                                                      
         USING PRORATAD,R2                                                      
         USING SRAMTSD,R3                                                       
         MVI   SRAID,SRAIDQ                                                     
*                                                                               
         MVC   SRASTAT3,PG$STAT3   SAVE PG$FULUT                                
*                                                                               
         ZAP   SRANET,PA$NET                                                    
         ZAP   SRABILL,PA$NETBL                                                 
         ZAP   SRAALLOC,PP$AALLO                                                
         ZAP   SRATRANS,PA$XFRAM                                                
         ZAP   SRAWRIOF,PA$WOFAM                                                
*                                                                               
         ZAP   SRAN_CD,PA$DSC                                                   
         ZAP   SRAB_CD,PA$DSCB                                                  
         ZAP   SRAA_CD,PP$ADSCB                                                 
         ZAP   SRAT_CD,PA$DSCX                                                  
         ZAP   SRAW_CD,PA$DSCW                                                  
*                                                                               
         ZAP   SRAN_HR,PA$HOURS                                                 
         ZAP   SRAB_HR,PA$HRSB                                                  
         ZAP   SRAA_HR,PP$HRSB                                                  
         ZAP   SRAT_HR,PA$HRSX                                                  
         ZAP   SRAW_HR,PA$HRSW                                                  
         MVI   SRALEN,SRAMLN1                                                   
         LA    R3,SRAMLN1(R3)                                                   
*                                                                               
TRNX20   L     R2,SORTAREA                                                      
         MVI   0(R3),0             END OF RECORD                                
         LA    R3,1(R3)                                                         
         SR    R3,R2               ONE PAST LAST MINUS FIRST IS LENGTH          
         STH   R3,0(R2)                                                         
*                                                                               
         CLC   QUESTOR(4),=C'HELP'                                              
         BNE   PUTS01                                                           
*                                                                               
         L     R3,ADTRANS                                                       
         SH    R3,DATADISP                                                      
         LA    R1,=C'TRN'                                                       
         LA    R2,2000                                                          
         GOTO1 =V(PRNTBL),DMCB,(3,(R1)),(R3),C'DUMP',(R2),=C'2D',(C'P',X        
               PRINT)                                                           
*                                                                               
         USING ACMD,R4                                                          
         L     R4,AMONACC                                                       
         L     R3,ACMAPRO2                                                      
         LA    R5,=C'PTA'                                                       
         LA    R2,2000                                                          
         GOTO1 =V(PRNTBL),DMCB,(3,(R5)),(R3),C'DUMP',(R2),=C'2D',(C'P',X        
               PRINT)                                                           
*                                                                               
         L     R3,ACMAPROB                                                      
         LA    R5,=C'PROBLOCK'                                                  
         LA    R2,PR$LNQ                                                        
         GOTO1 =V(PRNTBL),DMCB,(8,(R5)),(R3),C'DUMP',(R2),=C'2D',(C'P',X        
               PRINT)                                                           
*                                                                               
         USING SORTD,R3                                                         
         L     R3,SORTAREA                                                      
         LA    R5,=C'PUT'                                                       
         LA    R2,SRTKEYLN                                                      
         GOTO1 =V(PRNTBL),DMCB,(3,(R5)),(R3),C'DUMP',(R2),=C'2D',(C'P',X        
               PRINT)                                                           
         LH    R2,0(R3)            LENGTH                                       
         LA    R3,SRTDATA                                                       
         GOTO1 =V(PRNTBL),DMCB,(3,(R5)),(R3),C'DUMP',(R2),=C'2D',(C'P',X        
               PRINT)                                                           
PUTS01   L     R3,SORTAREA                                                      
         GOTO1 ADSORTER,DMCB,=C'PUT',(R3)                                       
         MVI   SORTANY,1           INDICATE WE SORTED SOMETHING                 
TRANXXXX XIT1                                                                   
         DROP  R2,R3,R4                                                         
*                                                                               
FIRSTELA CLI   0(R2),0                                                          
         BNE   *+10                                                             
         CLI   0(R2),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                BE INSTRUCTION                               
         CLC   ELCODE,0(R2)                                                     
         BCR   8,RE                BE                                           
*                                                                               
NEXTELA  SR    RF,RF                                                            
         IC    RF,1(R2)                                                         
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         CLI   1(R2),1             SET NOT EQUAL CC                             
         BR    RE                                                               
         AR    R2,RF                                                            
         B     FIRSTELA                                                         
*---------------------------------------------------------------------          
* IF A TRANSACTION IS FULLY BILLED AND THERE IS A 50 ELEMENT AND THERE          
* IS ONLY 1 4B ELEMENT ON THE TRANSACTION, SET SR4BCSD TO TRCSAMNT              
* EXPECTS R2 TO ADDRESS A CASH DISC 50 ELEMENT                                  
*---------------------------------------------------------------------          
*                                                                               
         USING TRCASHD,R2                                                       
*                                                                               
CHKBLCD  NTR1                                                                   
         ZAP   EL4BCNT,=P'0'                                                    
         USING ACKEYD,R3                                                        
         L     R3,ADTRANS                                                       
         SH    R3,DATADISP                                                      
         OC    ACDTUSED,ACDTUSED   FULLY BILLED                                 
         BZ    CHKBX               NO                                           
         L     R3,SORTAREA                                                      
         USING SORTD,R3                                                         
         LA    R3,SRTDATA                                                       
*                                                                               
CHKB30   CLI   0(R3),0                                                          
         BE    CHKB50                                                           
         CLI   0(R3),X'4B'                                                      
         BNE   CHKB40                                                           
         AP    EL4BCNT,=P'1'                                                    
         ST    R3,EL4BR2           SAVE ADDRESS                                 
CHKB40   ZIC   R1,1(R3)                                                         
         LA    R3,0(R1,R3)                                                      
         B     CHKB30                                                           
*                                                                               
CHKB50   CP    EL4BCNT,=P'1'       IS THERE 1 AND ONLY 1 4B                     
         BNE   CHKBX               NO                                           
         L     R3,EL4BR2                                                        
         USING SR4BD,R3                                                         
         ZAP   SR4BCSD,TRCSAMNT                                                 
*                                                                               
CHKBX    XIT1                                                                   
         DROP  R2,R3                                                            
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        FIRST ACCOUNT, FILTER A JOB FROM MONACC                                
*        PASSES BACK BYTE=N FOR MONACC TO IGNORE JOB (FRDRTRNS=N)               
*----------------------------------------------------------------------         
*                                                                               
VFRSTACC NMOD1 0,*FRSA*                                                         
         L     RC,0(,R1)           P1 IS RC                                     
         L     R2,ADACC                                                         
         MVC   SAVCLI(3),3(R2)                                                  
         MVC   SAVPRO(3),6(R2)                                                  
         MVC   SAVJOB(6),9(R2)                                                  
         L     R3,ADACCSTA                                                      
         MVI   FCRDTRNS,C'N'                                                    
         MVI   BYTE,C'N'           ASSUME I WILL REJECT THIS JOB                
         XC    JOBSTAT,JOBSTAT                                                  
         ZAP   ADJBAL,=P'0'        BAL OF TRANSACTIONS PASSED                   
         USING ACSTATD,R3                                                       
         MVC   SVSTAT,ACSTSTAT         CLOSED, LOCKED STAT                      
         MVI   APPSW,C'Y'          SET TO APPROVED                              
         XC    ELADDR,ELADDR                                                    
         AH    R2,DATADISP                                                      
FLT00    CLI   0(R2),0                                                          
         BE    FLT1                                                             
         CLI   0(R2),X'26'                                                      
         BE    FLT0                                                             
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     FLT00                                                            
*                                                                               
FLT0     ST    R2,ELADDR                                                        
         USING JOBELD,R2                                                        
         CLI   JOBLN,JOBLN2Q       CHECK XJOBS                                  
         BNH   FLT0A                                                            
         TM    JOBSTA1,JOBSXJOB                                                 
         BZ    FLT0A                                                            
         CLI   QOPT5,C' '          BILLED/BILLABLE                              
         BNE   VFRSTX              YES, NO XJOBS                                
*                                                                               
         USING ACJOBD,R2                                                        
FLT0A    TM    ACJBSTAT,X'80'                                                   
         BZ    *+8                                                              
         MVI   APPSW,C'N'                                                       
FLT1     CLI   QOPT2,C' '                                                       
         BE    FLT10                                                            
         CLI   QOPT2,C'S'          SUPPRESS CLOSED                              
         BNE   FLT2                                                             
         TM    SVSTAT,X'40'                                                     
         BO    VFRSTX                                                           
         B     FLT10                                                            
*                                                                               
FLT2     CLI   QOPT2,C'C'          CLOSED ONLY                                  
         BNE   FLT4                                                             
         TM    SVSTAT,X'40'                                                     
         BZ    VFRSTX                                                           
         L     R2,ELADDR                                                        
         LTR   R2,R2                                                            
         BZ    FLT10                                                            
         CLC   ACJBCLOS,PSTART     MUST BE BETWEEN REQ. DATES                   
         BL    VFRSTX                                                           
         CLC   ACJBCLOS,PEND                                                    
         BH    VFRSTX                                                           
         B     FLT10                                                            
FLT4     CLI   QOPT2,C'P'          PEELABLE                                     
         BNE   FLT10                                                            
         TM    SVSTAT,X'40'        MUST BE CLOSED                               
         BZ    VFRSTX                                                           
         CLC   ACSTLAST(2),PEND    AND HAVE NO ACTIVITY                         
         BH    VFRSTX              BEYOND END MONTH OF REQUEST                  
         DROP  R3                                                               
         L     R2,ADACCBAL                                                      
         USING ACBALD,R2                                                        
         CP    ACBLDR,ACBLCR       AND HAVE ZERO BALANCE                        
         BNE   VFRSTX                                                           
FLT10    CLI   QOPT3,C' '          LOCKED OPTION                                
         BE    FLT20                                                            
         CLI   QOPT3,C'S'          SUPPRESS LOCKED                              
         BNE   FLT12                                                            
         TM    SVSTAT,X'20'                                                     
         BO    VFRSTX                                                           
         B     FLT20                                                            
FLT12    CLI   QOPT3,C'L'          LOCKED ONLY                                  
         BNE   FLT20                                                            
         TM    SVSTAT,X'20'                                                     
         BZ    VFRSTX                                                           
FLT20    CLI   QOPT6,C' '          ZERO BALANCE OPTION                          
         BE    FLTOK                                                            
         USING ACMD,R3                                                          
         L     R3,AMONACC                                                       
         CLC   ACMMEND,=X'FFFF'       MOS END SPECIFIED                         
         BNE   FLTOK               USE EFFECTIVE BAL THEN                       
         OC    ACMMSTR,ACMMSTR                                                  
         BNZ   FLTOK                                                            
         L     R2,ADACCBAL                                                      
         CLI   QOPT6,C'Z'          ZERO BALANCE ONLY                            
         BNE   FLT42                                                            
         CP    ACBLDR,ACBLCR                                                    
         BNE   VFRSTX                                                           
         B     FLTOK                                                            
FLT42    CP    ACBLDR,ACBLCR       SUPPRESS ZERO BALANCE                        
         BE    VFRSTX                                                           
FLTOK    MVI   BYTE,C'Y'                                                        
VFRSTX   XIT1                                                                   
         DROP  R2,R3                                                            
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        IF THIS JOB HAS ESTIMATES, PUT A RECORD OUT TO SORT                    
*        REJECT JOB FOR ZERO BAL                                                
*----------------------------------------------------------------------         
*                                                                               
         USING JXCEPTD,R6                                                       
*                                                                               
VLASTACC NMOD1 0,*LSTA*                                                         
         L     RC,0(,R1)           P1 IS RC                                     
         LA    R6,JXBLOCK                                                       
         MVI   JXMODE,JXMLSACC                                                  
         MVC   JXAREC,ADACC                                                     
*                                                                               
         USING ACMD,R1                                                          
         L     R1,AMONACC                                                       
         MVC   JXAJBBLK,ACMAJOBB   A(JOBBER BLOCK)                              
         MVC   JXAGOBLK,ADGOBLOC   A(GETOPT BLOCK)                              
         ZAP   JXEST,ESTIMATE      NET ESTIMATE VALUE                           
         ZAP   JXGEST,ESTGROSS     GROSS ESTIMATE VALUE                         
         ZAP   JXHREST,HRESTMAT    HIGHEST REVISION ESTIMATE VALUE              
         ZAP   JXBIGBAL,=P'0'      FOR EXCEPTION REASON B                       
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
*                                                                               
         CLC   QUESTOR(4),=C'HELP'                                              
         BNE   VLA20                                                            
         L     R1,ADACC                                                         
         MVC   P+1(8),=C'EXCEPTNS'                                              
         MVC   P+10(12),3(R1)                                                   
         MVC   P+25(L'JXCODES),JXCODES                                          
         GOTO1 ACREPORT                                                         
*                                                                               
VLA20    BAS   RE,VLBLDKEY                                                      
         USING SORTD,R3                                                         
         L     R3,SORTAREA                                                      
         MVI   SRTWC,X'01'         FLAG AS LIST OF EXCEPTOPN REASONS            
         MVC   SRTWC+2(1),JXNCODES                                              
         XR    R1,R1                                                            
         ICM   R1,1,JXNCODES                                                    
         BZ    VLA50                                                            
         SLA   R1,1                FIELDS ARE 2 BYTES                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRTWC+3(0),JXCODES                                               
*                                                                               
VLA50    MVI   SRTLRECL+1,100      INSERT LENGTH                                
         GOTO1 ADSORTER,DMCB,=C'PUT',(R3)                                       
*        MVC   P+1(100),SRTLRECL+2                                              
*        GOTO1 ACREPORT                                                         
         DROP  R1,R6                                                            
*                                                                               
         MVI   BYTE,C' '                                                        
*                                                                               
         CLI   QSELECT,C' '                                                     
         BE    VLAST02C                                                         
*                                                                               
         LA    R3,QSELECT                                                       
         LA    R4,6                                                             
VLAST02  CLI   0(R3),C' '          NO MORE IN LIST                              
         BE    VLAST03R            NO MATCHES, REJECT                           
         USING JXCEPTD,R5                                                       
         LA    R5,JXBLOCK                                                       
         MVI   JXMODE,JXMCHK                                                    
         MVC   JXCODE,0(R3)                                                     
*                                                                               
         GOTO1 =V(ACEXCP),DMCB,JXBLOCK                                          
*                                                                               
         BE    VLAST02C            WANT, CONTINUE                               
*                                                                               
VLAST02B LA    R3,1(R3)                                                         
         BCT   R4,VLAST02                                                       
         B     VLAST03R            NO MATCHES, REJECT                           
*                                                                               
VLAST02C CLI   QOPT6,C' '          ZERO BALANCE OPTION                          
         BE    VLAST04                                                          
         USING ACMD,R3                                                          
         L     R3,AMONACC                                                       
         CLC   ACMMEND,=X'FFFF'       MOS END SPECIFIED                         
         BNE   VLAST03             USE EFFECTIVE BAL THEN                       
         OC    ACMMSTR,ACMMSTR                                                  
         BZ    VLAST04             NO MOS FILTER NOW                            
*                                                                               
VLAST03  CP    ADJBAL,=P'0'        ZERO EFFECTIVE BAL?                          
         BE    VLAST03Z            YES                                          
*                                                                               
         CLI   QOPT6,C'Z'          NO, WANT ZERO ONLY                           
         BNE   VLAST04             YES                                          
         B     VLAST03R                                                         
*                                                                               
VLAST03Z CLI   QOPT6,C'S'          SUPRESS ZERO BAL                             
         BNE   VLAST04                                                          
*                                                                               
VLAST03R MVI   BYTE,C'R'           REJECT A JOB                                 
         B     VLAST20             PUT A RECORD TO SORT                         
*                                                                               
VLAST04  TM    JOBSTAT,CHARGES+BILLING  WERE THERE RECORDS ON THE JOB           
         BNZ   VLASTX                   YES                                     
*                                                                               
         L     R2,ADACC            IF EST RECORDS, PUT OUT A DUMMY              
         AH    R2,DATADISP                                                      
VLAST05  CLI   0(R2),0                                                          
         BE    VLAST10                                                          
         CLI   0(R2),X'35'                                                      
         BE    VLAST20             JOB IS ON OLD ESTIMATES                      
         ZIC   R1,1(R2)            GET NEXT EL                                  
         AR    R2,R1                                                            
         B     VLAST05                                                          
         DROP  R3                                                               
*                                                                               
VLAST10  XC    MYKEY,MYKEY         PREP READING NEW EST RECORDS                 
         USING ACKEYD,R2                                                        
         LA    R2,MYKEY                                                         
         L     R3,ADACC                                                         
         USING ACMD,R5                                                          
         L     R5,AMONACC                                                       
         L     R5,ACMAJOBB                                                      
         USING JBLOCKD,R5                                                       
         CLI   JBNEWEST,JBMCSQ                                                  
         BE    VLAST15                                                          
         MVI   ACEVRTYP,ACEVEQU                                                 
         MVI   ACEVSREC,ACEVSEQU                                                
         MVC   ACEVCUL(1),RCCOMPFL                                              
         MVC   ACEVCUL+1(2),=C'SJ'                                              
         MVC   ACEVCLI(L'ACEVCLI+L'ACEVPROD),SPACES                             
         MVC   ACEVCLI(3),3(R3)                                                 
         MVC   ACEVPROD(3),6(R3)                                                
         MVC   ACEVJOB(6),9(R3)                                                 
         L     R2,IOSPACE                                                       
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',MYKEY,(R2)                       
         CLC   MYKEY(23),0(R2)     SAME JOB RETURNED                            
         BNE   VLASTX              NO, NO ESTIMATES ON THIS JOB                 
         B     VLAST20                                                          
*                                                                               
         USING ESTRECD,R2                                                       
VLAST15  MVI   ESTKTYP,ESTKTYPQ                                                 
         MVI   ESTKSUB,ESTKSUBQ                                                 
         MVC   ESTKCPY,RCCOMPFL                                                 
         MVC   ESTKCLI,3(R3)                                                    
         MVC   ESTKPRO,6(R3)                                                    
         MVC   ESTKJOB,9(R3)                                                    
         L     R2,IOSPACE                                                       
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',MYKEY,(R2)                       
         CLC   MYKEY(39),0(R2)     SAME JOB RETURNED                            
         BNE   VLASTX              NO, NO ESTIMATES ON THIS JOB                 
*                                                                               
VLAST20  L     R2,ADACC                                                         
         L     R3,SORTAREA                                                      
         USING SORTD,R3                                                         
         USING ACKEYD,R2                                                        
         BAS   RE,VLBLDKEY         BUILD A JOB SORT KEY                         
*                                                                               
VLAST30  CLI   BYTE,C'R'           PUT OUT A REJECT RECORD                      
         BE    VLAST40             (ONE WITH NO WORKCODE)                       
         MVC   SRTWC,=X'FFFF'                                                   
*                                                                               
VLAST40  MVI   SRTLRECL+1,50       INSERT LENGTH                                
         GOTO1 ADSORTER,DMCB,=C'PUT',(R3)                                       
VLASTX   XIT1                                                                   
         DROP  R5,R2,R3                                                         
*                                                                               
         USING SORTD,R3                                                         
         USING ACKEYD,R2                                                        
*                                                                               
VLBLDKEY NTR1                                                                   
         L     R2,ADACC                                                         
         L     R3,SORTAREA                                                      
         XC    SRTLRECL,SRTLRECL                                                
         XC    SRTKEY(SRTDATAL),SRTKEY                                          
         XC    SRTDATA(255),SRTDATA                                             
         XC    SRTDATA+255(145),SRTDATA+255                                     
         MVC   SRTCLI,3(R2)                                                     
         MVC   SRTPRO,6(R2)                                                     
         MVC   SRTJOB,9(R2)                                                     
*                                                                               
         CLI   SRTBYOFF,C'Y'                                                    
         BNE   VLASTX                                                           
         L     R5,ADGOBLOC                                                      
         USING GOBLOCKD,R5                                                      
         MVC   SRTOFF,GOEFFOFC                                                  
         B     VLASTX                                                           
         DROP  R2,R3,R5                                                         
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        CLEAR PST TABLE, P2 IS TYPE OF ENTRIES TO CLEAR                        
*----------------------------------------------------------------------         
*                                                                               
         USING PSTTABD,R3                                                       
*                                                                               
PSTCLR   NMOD1 0,*PSTC*                                                         
         L     RC,0(,R1)                                                        
         L     R3,APSTTAB                                                       
         LA    R0,MAXPRV*2                                                      
PSTC10   MVC   BYTE,PSTTTYPE                                                    
         NC    BYTE,DMCB+7         DO I WANT TO CLEAR THIS TYPE                 
         BZ    PSTC30              NO                                           
         XC    PSTTNAME,PSTTNAME                                                
         ZAP   PSTTAMNT,=P'0'                                                   
PSTC30   LA    R3,PSTTABLN(R3)                                                  
         BCT   R0,PSTC10                                                        
*                                                                               
         XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         SPACE  2                                                               
*----------------------------------------------------------------------         
*        INITIALIZE PSTTABLE WITH PSTTYPES                                      
*----------------------------------------------------------------------         
*                                                                               
         USING PSTTABD,R3                                                       
*                                                                               
PSTINIT  NMOD1 0,*PSTI*                                                         
         L     RC,0(R1)                                                         
         L     R3,APSTTAB                                                       
         LA    R0,MAXPRV                                                        
PSTI10   MVI   PSTTTYPE,PSTTTBIQ                                                
         XC    PSTTNAME,PSTTNAME                                                
         ZAP   PSTTAMNT,=P'0'                                                   
         LA    R3,PSTTABLN(R3)                                                  
         BCT   R0,PSTI10                                                        
*                                                                               
         LA    R0,MAXPRV                                                        
PSTI20   MVI   PSTTTYPE,PSTTTJBQ                                                
         XC    PSTTNAME,PSTTNAME                                                
         ZAP   PSTTAMNT,=P'0'                                                   
         LA    R3,PSTTABLN(R3)                                                  
         BCT   R0,PSTI20                                                        
*                                                                               
         XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         SPACE  2                                                               
*----------------------------------------------------------------------         
*        ADD A SRPSD ENTRY TO PSTTAB                                            
*        P2 IS A(SRPSEL)                                                        
*----------------------------------------------------------------------         
*                                                                               
         USING SRPSD,R2                                                         
         USING PSTTABD,R3                                                       
*                                                                               
PSTPUT   NMOD1 0,*PSTP*                                                         
         L     RC,0(,R1)                                                        
         L     R2,4(,R1)                                                        
         L     R3,APSTTAB          PUT THE INVOICE ENTRY FIRST                  
         LA    R0,MAXPRV                                                        
PSTP10   OC    PSTTNAME,PSTTNAME   ANYTHING HERE                                
         BZ    PSTP30              NO, USE THIS SPACE                           
         CLC   PSTTNAME,SRPSNAME                                                
         BE    PSTP30                                                           
         LA    R3,PSTTABLN(R3)                                                  
         BCT   R0,PSTP10                                                        
         DC    H'0'                                                             
*                                                                               
PSTP30   CLI   PSTTTYPE,PSTTTBIQ   IS THIS AN INVOICE ENTRY                     
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         MVC   PSTTNAME,SRPSNAME                                                
         AP    PSTTAMNT,SRPSAMNT                                                
*                                                                               
         L     R3,APSTTAB          PUT JOB LEVEL PST                            
         LA    R3,MAXPRV*PSTTABLN(R3)                                           
PSTP50   OC    PSTTNAME,PSTTNAME   ANYTHING HERE                                
         BZ    PSTP70              NO, USE THIS SPACE                           
         CLC   PSTTNAME,SRPSNAME                                                
         BE    PSTP70                                                           
         LA    R3,PSTTABLN(R3)                                                  
         BCT   R0,PSTP50                                                        
         DC    H'0'                                                             
*                                                                               
PSTP70   CLI   PSTTTYPE,PSTTTJBQ   IS THIS AN JOB ENTRY                         
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         MVC   PSTTNAME,SRPSNAME                                                
         AP    PSTTAMNT,SRPSAMNT                                                
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
*                                                                               
         USING SUMTABD,R2                                                       
*                                                                               
PRTAXTOT NMOD1 0,PTTAXTO           PRINT ANY GST TOTAL BENEATH BILLING          
         L     RC,0(,R1)                                                        
         CLC   BUFANAL,=C'99'                                                   
         BNE   PTAXXX                                                           
*                                                                               
         USING SUMTABD,R2                                                       
         L     R2,ASUMTAB                                                       
PTAX05   CLI   SMTBWANT,0          END OF TABLE                                 
         BE    PTAXXX                                                           
         CLI   SMTBFNUM,9          BILLED AMOUNT?                               
         BE    PTAX10                                                           
         LA    R2,L'SUMTAB(,R2)                                                 
         B     PTAX05                                                           
*                                                                               
PTAX10   LH    RF,=Y(PRTBLN)       CLEAR PRINT BUFFER                           
         L     RE,PRTBUFF                                                       
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0                                                            
*                                                                               
         L     RF,PRTBUFF                                                       
         ZIC   R3,SMTBPOSI         RELATIVE ADDRESS                             
         AR    R3,RF             SET R3 AS PRINT ADRESS                         
*                                                                               
         CLI   GSTFLAG,C'Y'                                                     
         BNE   PTAX60                                                           
*                                                                               
         MVC   1(4,R3),=C'GST='                                                 
         MVC   AREA+20(13),SPACES                                               
         EDIT  (P6,GSTTOTAL),(13,AREA+20),2,FLOAT=-                             
*                                                                               
         CP    GSTTOTAL,=P'9999999'                                             
         BNH   *+8                 YES                                          
         LA    R3,L'P(R3)          NO PRINT IN NEXT P                           
         LA    R1,12               MAXIMUM FIELD WIDTH - 1                      
         ZIC   RE,SMTBLEN          FIELD WIDTH                                  
         SR    R1,RE                                                            
         LA    RF,AREA+20                                                       
         LA    RF,0(R1,RF)         SHIFT TO RIGHT                               
         SH    RE,=H'4'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R3),4(RF)                                                    
         LA    R3,L'P(R3)          NEXT P                                       
*                                                                               
PTAX60   CLI   PSTFLAG,C'Y'        ANY PST ON THIS JOB                          
         BNE   PTAXX                                                            
*                                                                               
         USING PSTTABD,R4                                                       
         L     R4,APSTTAB                                                       
         LA    R4,MAXPRV*PSTTABLN(R4)  BUMP TO JOB TOTALS                       
         LA    R5,MAXPRV                                                        
*                                                                               
PTAX70   OC    PSTTNAME,PSTTNAME                                                
         BZ    PTAXX                                                            
         MVC   1(6,R3),PSTTNAME                                                 
         MVI   7(R3),C'='                                                       
         MVC   AREA+20(13),SPACES                                               
         EDIT  (P6,PSTTAMNT),(13,AREA+20),2,FLOAT=-                             
*                                                                               
         CP    PSTTAMNT,=P'9999999'                                             
         BH    PTAX75                                                           
         CP    PSTTAMNT,=P'-9999999'                                            
         BNL   *+8                 YES                                          
*                                                                               
PTAX75   LA    R3,L'P(R3)          NO PRINT IN NEXT P                           
         LA    R1,12               MAXIMUM FIELD WIDTH - 1                      
         ZIC   RE,SMTBLEN          FIELD WIDTH                                  
         SR    R1,RE                                                            
         LA    RF,AREA+20                                                       
         LA    RF,0(R1,RF)         SHIFT TO RIGHT                               
         SH    RE,=H'4'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R3),4(RF)                                                    
*                                                                               
         LA    R3,L'P(R3)          BUMP PRINT ADDRESS                           
         LA    R4,PSTTABLN(R4)     BUMP PSTTAB                                  
         BCT   R5,PTAX70                                                        
*                                                                               
PTAXX    L     R2,PRTBUFF                                                       
         LA    R0,PBMAX                                                         
PTAXX10  MVC   P,0(R2)                                                          
         CLC   P,SPACES                                                         
         BE    PTAXX20                                                          
         GOTO1 ACREPORT                                                         
PTAXX20  LA    R2,L'P(R2)                                                       
         BCT   R0,PTAXX10                                                       
*                                                                               
PTAXXX   XIT1                                                                   
         DROP  R2,R4                                                            
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        GET SPACE FOR THE NAME TABLES                                          
*----------------------------------------------------------------------         
*                                                                               
GETBUFF  NMOD1 0,*GETB*                                                         
         L     RC,0(,R1)                                                        
         L     R0,=A(BUFSIZE)     SUM OF ALL TABLE SIZES                        
         GETMAIN R,LV=(0)                                                       
         LA    R0,MAINNUM                                                       
         LR    R5,R1               R5 IS BUFFER POINTER                         
         ST    R1,ABUFF            SAVE BUFF START                              
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD          PRINT THE BUFFER IN DUMP                     
         USING MASTD,R2                                                         
         STCM  R5,15,MCUSRDMP                                                   
         LR    RF,R5                                                            
         A     RF,=A(BUFSIZE)                                                   
         STCM  RF,15,MCUSRDMP+4                                                 
*                                                                               
         L     R2,=A(MAINTAB)                                                   
         USING MAIND,R2                                                         
*                                                                               
GETB10   MVC   *+8(2),MAINAST     SCON OF WHERE TO STORE BUFF LOCATION          
         ST    R5,FULL             FULL IS A DUMMY FOR THE ASSEMBLER            
         AH    R5,MAINSIZE                                                      
*                                                                               
         LA    R2,MAINLEN(R2)                                                   
         BCT   R0,GETB10                                                        
*                                                                               
         B     BUFFX                                                            
         DROP  R2                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        RELEASE GETMAINED SPACE                                                
*----------------------------------------------------------------------         
*                                                                               
RELBUFF  NMOD1 0,*RELB*                                                         
         L     RC,0(,R1)                                                        
         L     R1,ABUFF                                                         
         L     R0,=A(BUFSIZE)     SUM OF ALL TABLE SIZES                        
         FREEMAIN R,A=(1),LV=(0)                                                
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD          PRINT THE BUFFER IN DUMP                     
         USING MASTD,R2                                                         
         XC    MCUSRDMP,MCUSRDMP   CLEAR XTRA DUMP ADDRESS                      
*                                                                               
         B     BUFFX                                                            
BUFFX    XIT1                                                                   
         LTORG                                                                  
         DROP  R2                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        MOVE THE NAME OF AN ACCOUNT INTO 0(R3)                                 
*----------------------------------------------------------------------         
*                                                                               
GETNAME  NMOD1 0,*GETN*                                                         
         L     RC,0(,R1)                                                        
         MVC   0(36,R3),SPACES                                                  
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL2                                                        
         BNE   GETNMX                                                           
*                                                                               
         USING ACNAMED,R2                                                       
         IC    R4,ACNMLEN                                                       
         SH    R4,=H'3'                                                         
         STC   R4,GETNM+1          MODIFY LENGTH OF MVC                         
GETNM    MVC   0(0,R3),ACNMNAME                                                 
*                                                                               
GETNMX   XIT1                                                                   
*                                                                               
         GETELN R2,DATADISP,ELCODE,2                                            
         DROP R2                                                                
         EJECT ,                                                                
* OPTION TABLES  (OPTTAB AND SUMTAB LENGTHS MUST BE THE SAME)                   
*                                                                               
* BYTE 1 PRINT THIS OR NOT , Y OR N                                             
* BYTE 2 LENGTH OF DATA OR HEADLINE INFO (GREATER OF THESE TWO)                 
* BYTE 3 POSITION IN RELATION TO P+1                                            
* BYTES 4-13 HEADLINE1 VALUE                                                    
* BYTES 14-23 HEADLINE2 VALUE (US ONLY)                                         
* BYTES 24-29 SPARE (US ONLY)                                                   
*                                                                               
OPTTAB   DS    0CL30                                                            
         DC    C'Y',AL1(18),AL1(0),CL26'WC/CONTRA    ACCOUNT',AL1(0)            
         DC    C'Y',AL1(7),AL1(0),CL26'INVOICE   NUMBER',AL1(0)                 
         DC    C'Y',AL1(8),AL1(0),CL26'INVOICE    DATE ',AL1(0)                 
         DC    C'N',AL1(18),AL1(0),CL26'  SUPPLIER    NAME',AL1(0)              
         DC    C'N',AL1(13),AL1(0),CL26'  GROSS EST   VALUE',AL1(0)             
         DC    C'N',AL1(6),AL1(0),CL26'ORDER     NUMBER',AL1(0)                 
         DC    C'N',AL1(13),AL1(0),CL26'    ORDER     AMOUNT',AL1(0)            
         DC    C'N',AL1(8),AL1(0),CL26'  RATE',AL1(0)                           
         DC    C'N',AL1(8),AL1(0),CL26' HOURS',AL1(0)                           
         DC    C'N',AL1(8),AL1(0),CL26' UNITS     PRICE',AL1(0)                 
         DC    C'N',AL1(13),AL1(0),CL26'     NET',AL1(0)                        
         DC    C'N',AL1(11),AL1(0),CL26'COMMISSION',AL1(0)                      
         DC    C'N',AL1(13),AL1(0),CL26'    GROSS',AL1(0)                       
         DC    C'N',AL1(1),AL1(0),CL26'C',AL1(0) COMMISSION DEFAULT             
         DC    C'N',AL1(11),AL1(0),CL26'   CASH    DISCOUNT',AL1(0)             
         DC    C'N',AL1(1),AL1(0),CL26'D',AL1(0)                                
         DC    C'N',AL1(8),AL1(0),CL26'  W/O       DATE ',AL1(0)                
         DC    C'Y',AL1(8),AL1(0),CL26'  W/O      HOURS',AL1(0)                 
         DC    C'Y',AL1(13),AL1(0),CL26'     W/O      AMOUNT'                   
         DC    AL1(0)                                                           
         DC    C'Y',AL1(6),AL1(0),CL26'  W/O      NUMBER'                       
         DC    AL1(0)                                                           
         DC    C'N',AL1(6),AL1(0),CL26' BILL     NUMBER'                        
         DC    AL1(0)                                                           
         DC    C'N',AL1(8),AL1(0),CL26'BILLED    HOURS'                         
         DC    AL1(0)                                                           
         DC    C'N',AL1(13),AL1(0),CL26'    BILLED    AMOUNT'                   
         DC    AL1(0)                                                           
         DC    C'N',AL1(8),AL1(0),CL26'  BILL      DATE'                        
         DC    AL1(0)                                                           
         DC    C'N',AL1(8),AL1(0),CL26'BILLABLE    HOURS'                       
         DC    AL1(0)                                                           
         DC    C'N',AL1(11),AL1(0),CL26'  BILLABLE   AMOUNT'                    
         DC    AL1(0)                                                           
         DC    C'N',AL1(1),AL1(0),CL26'B'                                       
         DC    AL1(0)                                                           
         DC    C'N',AL1(1),AL1(0),CL26'H'                                       
         DC    AL1(0)                                                           
         DC    C'N',AL1(15),AL1(0),CL26'NARRATIVE'                              
         DC    AL1(0)                                                           
         DC    C'N',AL1(6),AL1(0),CL26'BATCH'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         EJECT ,                                                                
***********************************************************************         
*   TIME TAB  - IF THE USER HAS ASKED THAT TIME TRANSACTION BE        *         
*               REPORTED ON SEPARATLY THESE VALUES WILL BE SET FROM   *         
*               THE PROFILE                                           *         
*               THIS TABLE DEFINES WHAT TO PRINT                      *         
***********************************************************************         
TIMETAB  DS    0CL30                                                            
         DC    C'Y',AL1(18),AL1(0),CL26'WC/       EMPLOYEE'                     
         DC    AL1(0)                                                           
         DC    C'Y',AL1(7),AL1(0),CL26'INVOICE   NUMBER'                        
         DC    AL1(0)                                                           
         DC    C'Y',AL1(8),AL1(0),CL26'INVOICE    DATE '                        
         DC    AL1(0)                                                           
         DC    C'U',AL1(18),AL1(0),CL26'  EMPLOYEE    NAME'                     
         DC    AL1(0)                                                           
         DC    C'N',AL1(13),AL1(0),CL26'  GROSS EST   VALUE'                    
         DC    AL1(0)                                                           
         DC    C'N',AL1(6),AL1(0),CL26'ORDER     NUMBER'                        
         DC    AL1(0)                                                           
         DC    C'N',AL1(13),AL1(0),CL26'    ORDER     AMOUNT'                   
         DC    AL1(0)                                                           
         DC    C'N',AL1(8),AL1(0),CL26'  RATE'                                  
         DC    AL1(0)                                                           
         DC    C'N',AL1(8),AL1(0),CL26' HOURS'                                  
         DC    AL1(0)                                                           
         DC    C'N',AL1(8),AL1(0),CL26' UNITS     PRICE'                        
         DC    AL1(0)                                                           
         DC    C'N',AL1(13),AL1(0),CL26'     NET'                               
         DC    AL1(0)                                                           
         DC    C'N',AL1(11),AL1(0),CL26'COMMISSION'                             
         DC    AL1(0)                                                           
         DC    C'Y',AL1(13),AL1(0),CL26'    GROSS'                              
         DC    AL1(0)                                                           
         DC    C'N',AL1(1),AL1(0),CL26'C'        COMMISSION DEFAULT             
         DC    AL1(0)                                                           
         DC    C'N',AL1(11),AL1(0),CL26'   CASH    DISCOUNT'                    
         DC    AL1(0)                                                           
         DC    C'N',AL1(1),AL1(0),CL26'D'                                       
         DC    AL1(0)                                                           
         DC    C'N',AL1(8),AL1(0),CL26'  W/O       DATE '                       
         DC    AL1(0)                                                           
         DC    C'Y',AL1(8),AL1(0),CL26'  W/O      HOURS'                        
         DC    AL1(0)                                                           
         DC    C'Y',AL1(13),AL1(0),CL26'     W/O      AMOUNT'                   
         DC    AL1(0)                                                           
         DC    C'N',AL1(6),AL1(0),CL26'  W/O     NUMBER'                        
         DC    AL1(0)                                                           
         DC    C'N',AL1(6),AL1(0),CL26' BILL     NUMBER'                        
         DC    AL1(0)                                                           
         DC    C'N',AL1(8),AL1(0),CL26'BILLED    HOURS'                         
         DC    AL1(0)                                                           
         DC    C'N',AL1(13),AL1(0),CL26'    BILLED    AMOUNT'                   
         DC    AL1(0)                                                           
         DC    C'N',AL1(8),AL1(0),CL26'  BILL      DATE'                        
         DC    AL1(0)                                                           
         DC    C'N',AL1(8),AL1(0),CL26'BILLABLE    HOURS'                       
         DC    AL1(0)                                                           
         DC    C'N',AL1(11),AL1(0),CL26'  BILLABLE   AMOUNT'                    
         DC    AL1(0)                                                           
         DC    C'N',AL1(1),AL1(0),CL26'B'                                       
         DC    AL1(0)                                                           
         DC    C'N',AL1(1),AL1(0),CL26'H'                                       
         DC    AL1(0)                                                           
         DC    C'N',AL1(15),AL1(0),CL26'NARRATIVE'                              
         DC    AL1(0)                                                           
         DC    C'N',AL1(6),AL1(0),CL26'BATCH'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
TIMESUM  DS    0CL30                                                            
         DC    C'Y',AL1(17),AL1(0),CL26'  EMPLOYEE        '                     
         DC    AL1(0)                                                           
         DC    C'Y',AL1(8),AL1(0),CL26'INV NUM   INV DATE'                      
         DC    AL1(0)                                                           
         DC    C'Y',AL1(6),AL1(0),CL26' RATE'                                   
         DC    AL1(0)                                                           
         DC    C'Y',AL1(8),AL1(0),CL26' TOTAL     HOURS'                        
         DC    AL1(0)                                                           
         DC    C'Y',AL1(8),AL1(0),CL26'  W/O      HOURS'                        
         DC    AL1(0)                                                           
         DC    C'Y',AL1(8),AL1(0),CL26'TOT HRS   LESS W/O'                      
         DC    AL1(0)                                                           
         DC    C'Y',AL1(13),AL1(0),CL26'    TOTAL     GROSS'                    
         DC    AL1(0)                                                           
         DC    C'Y',AL1(13),AL1(0),CL26'     W/O      AMOUNT'                   
         DC    AL1(0)                                                           
         DC    C'Y',AL1(13),AL1(0),CL26' TOT GROSS LESS W/O'                    
         DC    AL1(0)                                                           
         DC    C'Y',AL1(13),AL1(0),CL26'    BILLED    AMOUNT'                   
         DC    AL1(0)                                                           
         DC    C'Y',AL1(13),AL1(0),CL26'  BILLABLE   AMOUNT '                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
MENUDATA DC    C'AYNNYNNNYNNYYNYYYN' NOT ESTCOM,ESTGRS,ORD,OVUN                 
         DC    C'BYNNYNNNYYNYYNYYYN' NOT ESTCOM,ESTGRS,ORD,OPCT                 
         DC    C'CYNNYNNNYYYYYNNYYN' NOT ESTCOM,ESTGRS,ORD,CD                   
         DC    C'DYNNYNNNYYYNNNYYYN' NOT ESTCOM,ESTGRS,ORD,COMM,GRS             
         DC    C'ENNNYNNYYYNYYNYYYN' NOT ESTCOM,ESTGRS,ORIG,OPCT                
         DC    C'FNNNYNNNYYYYYNYYYN' NOT ESTCOM,ESTGRS,ORIG,ORD                 
         DC    C'GNNNNNNYYNNYYNYYYN' NOT ESTCOM,ESTGRS,ORIG,CURR,OVUN           
         DC    C'HNNNNNNNYNNYYNYYYN' NOT ESTCOM,ESTGRS,ORIG,CURR,OVUN,O         
         DC    C'IYNNYYYYYYYNNNNNNN' NOT COMM,GRS,CD,BILL,UNBILL                
         DC    C'JYNNYYYNYYYNNNNNNN' NOT COMM,GRS,CD,BILL,UNBILL,ORD            
         DC    C'KYNNYYYNYYYNNNNYYN' NOT ORD,COMM,GRS,CD                        
         DC    C'LYNNYYYNYNNYYNNYYN' NOT ORD,OVUN,CD                            
         DC    C'MYNNYYYNYNNNNNNYYN' NOT ORD,OVUN,NET,COMM,GRS,CD               
         DC    C'OYNNYYYNNNNNYYNYYN' NOT ORD,OVUN,NET,COMM,CD  HAS GRS          
         DC    C'PNNNYNYYYYNYYNNYYN' NOT ESTCOM,ORIG,ORD                        
         DC    C'QNYNYNNYYNNYYNYYYN'                                            
         DC    C'RYYNYNNNYNNYYNYYYN' NEW ESTIMATES                              
         DC    C'SNNYNNYYYNNYYNYYYN'                                            
         DC    C'TYNNYYYNNNNNYYNYYY' TYPE O + HIGHEST REVISION                  
         DC    C'#YNNYNNYYNNYYNYYYN' NOT ESTCOM,ESTGRS,OVUN (DEFAULT)           
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        MAINTAB IS TABLE OF HOW GETMAIN CORE SHOULD BE SPLIT UP                
*--------------------------------------------------------------------*          
MAINTAB  DS    0F                                                               
         DC    S(BILLTAB)          NOTE BILLTAB NEEDS FULL ALIGNMENT            
         DC    Y(BLTBLN)                                                        
*                                                                               
         DC    S(SORTAREA)         SORTAREA NEEDS FULL                          
         DC    Y(SRTRECLN)                                                      
*                                                                               
         DC    S(IOSPACE)                                                       
         DC    Y(IOLEN)                                                         
*                                                                               
         DC    S(DMGRBUFF)                                                      
         DC    Y(IOLEN)                                                         
*                                                                               
         DC    S(CLIBUFF)                                                       
         DC    Y(IOLEN)                                                         
*                                                                               
         DC    S(PROBUFF)                                                       
         DC    Y(IOLEN)                                                         
*                                                                               
         DC    S(COMMAREA)                                                      
         DC    Y(CIOLEN)                                                        
*                                                                               
         DC    S(SORTSAVE)                                                      
         DC    Y(SRTRECLN)                                                      
*                                                                               
         DC    S(ACUMAREA)                                                      
         DC    Y(ACCUMLEN)                                                      
*                                                                               
         DC    S(TIMEAREA)                                                      
         DC    Y(TIMACCLN)                                                      
*                                                                               
         DC    S(TIMESUB)                                                       
         DC    Y(LTSTB)                                                         
*                                                                               
         DC    S(RBILLTAB)                                                      
         DC    Y(RBTBLN)                                                        
*                                                                               
         DC    S(WOTAB)                                                         
         DC    Y(WOTBLN)                                                        
*                                                                               
         DC    S(PLANESTS)                                                      
         DC    Y(MAXESTS)                                                       
*                                                                               
         DC    S(REVESTS)                                                       
         DC    Y(MAXESTS)                                                       
*                                                                               
         DC    S(TRANSAVE)                                                      
         DC    Y(TRANSLN)                                                       
*                                                                               
         DC    S(PRTBUFF)                                                       
         DC    Y(PRTBLN)                                                        
*                                                                               
         DC    S(AUNITS)           UNIT TOTAL ACCUMULATORS                      
         DC    Y(UNTBFLN)                                                       
*                                                                               
         DC    S(APSTTAB)          PST SUMMARIZED BY PROVINCE                   
         DC    Y(MAXPRV*PSTTABLN*2)                                             
*                                                                               
         DC    S(ABUFCORE)                                                      
         DC    Y(00)               NOTE SIZE IS GT 16K SO MAKE SURE             
*                                  THIS IS THE LAST IN MAINTAB                  
MAINNUM  EQU   (*-MAINTAB)/MAINLEN                                              
*                                                                               
MAXESTS  EQU   255                                                              
IOLEN    EQU   2000                                                             
CIOLEN   EQU   3100                                                             
TRANSLN  EQU   (11*8)                                                           
WOTBLN   EQU   (20*6)                                                           
LTSTB    EQU   (3*TIMSUBLN)                                                     
PRTBLN   EQU   (PBMAX*L'P)                                                      
PBMAX    EQU   30                                                               
RBTBLN   EQU   (300*RBLEN)                                                      
BLTBLN   EQU   (300*LBILLTAB)+(2*L'FULL)                                        
UNTBFLN  EQU   (3*8)               3 PL8 ACCUMULATORS FOR SUBLINE               
LBUFCORE EQU   (300*BUFRECL)                                                    
MAXPRV   EQU   15                  MAXIMUM NUMBER OF PROVINCES BILLED           
PRVTBSZ  EQU   MAXPRV*PSTTABLN*2   SIZE OF PSTTABLE                             
*                                                                               
BUFSIZE  EQU   (5*IOLEN)+(2*SRTRECLN)+ACCUMLEN+TIMACCLN+LTSTB+WOTBLN+(2X        
               *MAXESTS)+TRANSLN+RBTBLN+BLTBLN+PRTBLN+UNTBFLN+LBUFCORE+X        
               PRVTBSZ                                                          
         EJECT ,                                                                
*---------------------------------------------------------------------          
*                                                                               
*        D E S C T S                                                            
*                                                                               
*---------------------------------------------------------------------          
AC67D    DSECT                                                                  
NETAMT   DS    D                   NET AT TRAN LEVEL                            
COMAMT   DS    D                   COM                                          
GRSAMT   DS    D                   GROSS                                        
CDAMT    DS    D                   CD                                           
BLDAMT   DS    D                   BILLED AMOUNT                                
NETBLAMT DS    D                   BILLNO OR NET AMOUNT                         
BLDCDAMT DS    D                   BILLED CD                                    
*                                                                               
BLDHRS   DS    D                   BLDHRS USED IN RUN1ST TO CLR PZEROS          
PZEROS   DS    CL128                                                            
*                                                                               
DUBABLE  DS    D                   BILLABLE AMOUNT                              
WOAMNT   DS    D                   WRITTEN OFF AMOUNT IN A TRAN                 
WODATE   DS    D                   DATE                                         
WOHRS    DS    PL8                 WRTITTEN OFF HOURS IN A TRAN                 
WOCNT    DS    PL4                 A COUNT OF '4F' ELEMENTS IN THE TRAN         
WONO     DS    CL6                 WRITE OFF NUMBER                             
ALOCHRS  DS    PL5                                                              
ALOCAMNT DS    PL5                                                              
COMMABLE DS    PL8                 COMMISSIONABLE AMOUNT OF A W/C               
ADJBAL   DS    PL8                 ADJUSTED BAL                                 
BILDTE   DS    CL8                                                              
BILNO    DS    CL6                                                              
SV7CUNIT DS    PL8                 UNITS ON A TRANSACTION                       
SV7CPRCE DS    PL8                                                              
SV7CSTAT DS    XL1                                                              
*                                                                               
SUBSTAT  DS    X                   PRINTING STATUS FOR UNITS/HOURS              
*                                                                               
SVHOUR   DS    D                                                                
SVRATE   DS    D                                                                
ESTIMATE DS    D                                                                
ESTGROSS DS    D                                                                
HRESTMAT DS    D                                                                
ORDSVAMT DS    PL8                                                              
ATSAROFF DS    A                                                                
ATSARBUF DS    A                                                                
AOPTTAB  DS    A                                                                
ASUMTAB  DS    A                                                                
ATIMETAB DS    A                                                                
ATIMESUM DS    A                                                                
AMENUTAB DS    A                                                                
ADBOX    DS    A                                                                
ABXTOP   DS    A                                                                
ELADDR   DS    A                                                                
ATABLE   DS    A                                                                
SAVER1   DS    A                   SAVE TABLE LOCATIONS WHEN PROCESSING         
SAVER4   DS    A                   FAKE BILLING                                 
EL4BR2   DS    A                   R2 WHEN GETELING 4B ELS                      
EL4FR2   DS    A                   R2 WHEN GETELING 4F ELS                      
SAVERE   DS    A                   SAVE RE                                      
AMYREPT  DS    A                                                                
ADBUFC   DS    A                                                                
AUNITS   DS    A                   A(UNITS ACCUMULATORS)                        
*                                                                               
ABUFF    DS    A                   ADDRESSES OF GETMAINED CORE                  
IOSPACE  DS    A                                                                
* * * * * * * * * * *              KEEP THESE TOGETHER                          
DMGRBUFF DS    A                                                                
CLIBUFF  DS    A                                                                
PROBUFF  DS    A                                                                
* * * * * * * * * * *                                                           
COMMAREA DS    A                                                                
SORTAREA DS    A                                                                
SORTSAVE DS    A                                                                
ACUMAREA DS    A                                                                
TIMEAREA DS    A                                                                
TIMESUB  DS    A                                                                
RBILLTAB DS    A                                                                
BILLTAB  DS    A                                                                
WOTAB    DS    A                                                                
PLANESTS DS    A                                                                
REVESTS  DS    A                                                                
TRANSAVE DS    A                                                                
PRTBUFF  DS    A                                                                
APSTTAB  DS    A                                                                
ABUFCORE DS    A                                                                
SAVEACLI DS    A                                                                
SAVEAPRO DS    A                                                                
*                                                                               
PRTADDR  DS    F                   POINTER FOR PRTBUFF                          
PEREST   DS    F                   PC EST BILLING PCT                           
APPSW    DS    CL1                 OLD EST UNAPPROVED EST SWITCH                
*                                  USED BY EXCEPTION LOGIC                      
TRNISREV DS    CL1                 Y, TRANSACTION IS A REVERSAL FOR             
*                                     THIS REQUEST                              
TRANSTAT DS    CL1                 TRANSACTION STATUS BYTE                      
REJECT   EQU   1                   REJCT TRAN BASED ON REQUEST OPTIONS          
WRITEOFF EQU   2                   TRAN IS TYPE 57 OR 58                        
PART     EQU   4                   TRAN AMOUNTS HAVE BEEN ADJUSTED              
PROCSTAT DS    CL1                 WHAT KIND OF RECORD AM I PROCESSING          
TIMETRAN EQU   1                   FAKE TIME BILLING?                           
OOPSTRAN EQU   4                                                                
FINLBILL EQU   8                                                                
BILLSTAT DS    CL1                 BILLING STATUS BYTE                          
GOTTBILL EQU   1                   GOT TIME BILLING:                            
GOTOBILL EQU   2                   GOT OOPS BILLING:                            
BADBILL  EQU   4                   BILL IS A DIFFRENT TYPE THAN CURRENT         
SRTBYOFF DS    CL1                 PROF19 SAVED AT THE LEDGER LEV               
FULLSUM  DS    CL1                 FUL SUMMARY PROFILE, LEDGER LEV              
JOBSTAT  DS    CL1                                                              
TYPESTAT DS    CL1                 STATUS OF A TRANSACTION TYPE                 
CHARGES  EQU   1                   JOB HAS CHARGES                              
NEEDESTS EQU   2                   NEED ESTIMATE VALUES                         
JOBBERED EQU   4                   I HAVE SET JOBBER FOR THIS JOB               
NEEDOPT  EQU   8                   DO I NEED A GETOPT CALL                      
NOTFULL  EQU   16                  JOB/WC IS NOT FULLY BILLED                   
GOTTIME  EQU   32                  THERE WERE TIME TRANS ON THIIS JOB           
BILLING  EQU   64                                                               
PREVSTAT DS    CL1                 STATUS OF PREVIOUS RETAIL BILL PROC          
RETSTAT  DS    CL1                 STATUS OF RETAIL BILL PROCESS                
TYPEQ100 EQU   X'01'                                                            
JOBLVL   EQU   X'02'                                                            
PEND32   EQU   X'04'                                                            
*                                                                               
XJOBSTAT DS    CL1                 FLAG TO PRODUCE X JOB TOTALS                 
XJOB     EQU   1                   THIS IS AN XJOB                              
XJINPRO  EQU   2                                                                
XJINCLI  EQU   4                                                                
XJINOFF  EQU   8                                                                
XJINREQ  EQU   16                                                               
*                                                                               
PRTFLAG  DS    CL1                 TYPE OF REPORT PRINTING NOW                  
PRTNGDET EQU   1                   PRINTING DETAIL REPORT                       
PRTNGWC  EQU   2                   PRINTING WORKCODE SUMMARY                    
PRTNGMP  EQU   4                   PRINTING MAN POWER SUM                       
PRTNGDAT EQU   8                   PRINTING COMMENTS, PROFILES, ETC             
PRTNGMPT EQU   16                  PRINTING MANPOWER TOTALS                     
PRTNGWCT EQU   32                  WORKCODE STYLE TOTALS                        
*                                                                               
PRTTYPE  DS    CL1                 TYPE OF REPORT I WANT TO PRINT               
PRTDET   EQU   PRTNGDET                                                         
PRTWC    EQU   PRTNGWC                                                          
PRTMP    EQU   PRTNGMP                                                          
PRTDATA  EQU   PRTNGDAT                                                         
PRTMPT   EQU   PRTNGMPT                                                         
PRTWCT   EQU   PRTNGWCT                                                         
PRTALL   EQU   PRTDET+PRTMP+PRTWC+PRTDATA+PRTMPT+PRTWCT                         
PRTFSAVE DS    CL1                                                              
*                                                                               
PL16     DS    PL16                                                             
TODAY2   DS    CL2                                                              
TODAYP   DS    PL3                                                              
ELCODE   DS    CL1                                                              
RTYPE    DS    CL1                                                              
BILLED   DS    CL1                                                              
WROFFED  DS    CL1                 Y, TRAN IS FULLY WRITTEN OFF                 
CLIBILL  DS    CL1                 Y, JOB HAS CLIENT BILLING                    
HELDFLAG DS    CL1                 Y, JOB HAS HELD INVOICES                     
RETFLAG  DS    CL1                 STATUS OF RETAIL BILLING                     
HASBILLS EQU   80                                                               
RETSCHEM EQU   1                                                                
RETBILLS EQU   2                                                                
EXGROSS  DS    PL8                 TOTAL GROSS ON JOB FOR EXCEPTION             
BILFLTSW DS    CL1                                                              
OVERAMT  DS    PL8                                                              
*                                                                               
SUBLGST  DS    CL1                 Y, HAVE SUBLINE ATTEMPT GST PRINT            
*                                                                               
FILTERSW DS    CL1                 Y, FILTERING TRANSACTIONS                    
*                                                                               
GSTFLAG  DS    CL1                 Y, JOB HAS GST BILLED                        
PSTFLAG  DS    CL1                 Y, JOB HAS PST BILLED                        
*                                                                               
SWT      DS    0C                                                               
REQACTV  DS    CL1                 INITED TO 'N'                                
OFFACTV  DS    CL1                                                              
CLIACTV  DS    CL1                                                              
PROACTV  DS    CL1                                                              
JOBACTV  DS    CL1                                                              
TYPACTV  DS    CL1                                                              
ANLACTV  DS    CL1                                                              
CONACTV  DS    CL1                                                              
REQMP    DS    CL1                                                              
OFFMP    DS    CL1                                                              
CLIMP    DS    CL1                                                              
PROMP    DS    CL1                                                              
JOBMP    DS    CL1                                                              
SWTNUM   EQU   *-SWT                                                            
*                                                                               
SAVEKEY  DS    CL49                                                             
COMMSW   DS    CL1                                                              
COMMLVL  DS    CL12                                                             
PREV4C   DS    CL14                PREVIOUS 4C ACCOUNT                          
COMMKEY  DS    CL49                                                             
MYKEY    DS    CL49                                                             
COMMAND  DS    CL8                                                              
VENDNAME DS    CL36                                                             
CLINAME  DS    CL36                                                             
PRONAME  DS    CL36                                                             
JOBNAME  DS    CL36                                                             
MEDIANM  DS    CL12                MEDIA NAME                                   
PEND     DS    PL3                                                              
ORD      DS    CL6                                                              
MYMEND   DS    CL2                 SAVED MOS FILTERS INCASE THE USER            
MYMSTR   DS    CL2                 WANTS ALL TRANS ON W/C SUMM                  
PSTART   DS    CL3                                                              
SAVELAST DS    CL3                 DATE OF LAST ACTIV (TRANS OR EST)            
*        SAVED GETOPT VARIBLES                                                  
SVHIAPP  DS    CL1                 NUMDBER OF HIGHEST APPROVED ESTIMATE         
SVNEWEST DS    CL1                 IS JOB ON NEW ESTS                           
SVDIST   DS    CL3                 DISTRIBUTION SCHEME                          
PREVDIST DS    CL3                 DISTRIBUTION SCHEME WHICH IS                 
*                                  CURRENTLY SAVED IN THE TABLE                 
SVCOMRTE DS    PL4                 COMM RATE A W/C LEVEL                        
*                                                                               
SVDISTOT DS    PL6                 DISTRIBUTION SCHEME                          
MYBOXCTL DS    CL1                 SAVE BOX DICTIONARY SETTING                  
MYBOXSW  DS    CL1                                                              
MYMODE   DS    CL1                                                              
MYCOL    DS    CL132                                                            
MYROW    DS    CL100                                                            
MIDBAL   DS    CL1                                                              
MYHEADS  DS    0CL132                                                           
SVH11    DS    CL132                                                            
SVH12    DS    CL132                                                            
SAVMID1  DS    CL132                                                            
SAVMID2  DS    CL132                                                            
TIME11   DS    CL132                                                            
TIME12   DS    CL132                                                            
TDET11   DS    CL132              HEADERS FOR THE TIME DETAIL REPORT            
TDET12   DS    CL132                                                            
SVH9     DS    CL50                                                             
PROFNARR DS    CL150              24 ELEMENT COMPOSITE NARRATIVES               
SVANAL   DS    CL2                                                              
SAVKEY   DS    0CL15               SAVED KEY OF THE JOB YOU ARE WORKING         
SAVCUL   DS    CL3                 ON                                           
SAVCLI   DS    CL3                                                              
SAVPRO   DS    CL3                                                              
SAVJOB   DS    CL6                                                              
SVOFFICE DS    CL2                 SAVED ACPROFFC (OFFICE)                      
*                                                                               
WNAME    DS    CL15                WORK CODE NAME                               
WTYPE    DS    CL1                 WORK CODE TYPE                               
O_O_P_WC EQU   X'42'               OUT OF POCKET WORKCODE                       
TIMEWC   EQU   X'41'               TIME WC                                      
WCP_TIME DS    CL1                 CONSIDER THIS WORKCODE TIME                  
WCTPPREV DS    CL1                 PREVIOUS W/C TYPE                            
WCTPCNT  DS    XL1                 COUNT OF W/C'S BY TYPE                       
WCTPSTAT DS    XL1                 TYPES OF W/C'S PUT TO BUFFALO                
BOTHWC   EQU   X'43'               TIME AND OOPS OR'ED TOGETHER                 
*                                                                               
*                                  CALLPEST PARMS                               
CPESTSTP DS    CL1                 STARTING WC TYPE                             
CPESTSTR DS    CL2                 START WC                                     
CPESTETP DS    CL1                 ENDING WC TYPE                               
CPESTEND DS    CL2                 END WC                                       
*                                                                               
LEVBTOT  DS    0CL2                                                             
LEVBGOT  DS    CL1                 THESE FIELDS CONTROL LEVEL                   
LEVBNEED DS    CL1                 SUB TOTALS ON THE M/P SUMMARY                
LEVATOT  DS    0CL2                                                             
LEVAGOT  DS    CL1                                                              
LEVANEED DS    CL1                                                              
TIMETOT  DS    CL1                 UNDERLINE IF Y                               
SV1RHEIR DS    CL(ACHRLENQ)        SAVED 1R HEIRARCHY ELEMENT                   
EMPLEVEL DS    CL1                 CURRENT EMPLOYEE LEVEL\                      
EMPLEVA  EQU   1                                                                
EMPLEVB  EQU   2                                                                
EMPLEVC  EQU   3                                                                
EMPLEVD  EQU   4                                                                
*                                                                               
ANALPEND DS    CL1                                                              
APENDSW  DS    CL1                                                              
TOTSW    DS    CL1                                                              
ANALSV   DS    CL2                                                              
SVSTAT   DS    CL1                                                              
NEXTGST  DS    AL2                 OFFSET INTO COMMAREA TO PUT GST              
GSTTOTAL DS    PL6                 TOTAL GST ON A JOB                           
BILDAREA DS    CL193                                                            
BUFREC   DS    0CL193                                                           
BUFKEY   DS    0CL29                                                            
BUFTYPE  DS    CL1                                                              
BUFWTYPE DS    CL1                 WORK CODE TYPE (TIME OR OOP)                 
BUFANAL  DS    CL2                                                              
         DS    CL25                N/D IN KEY                                   
BUFOTH   DS    0CL36                                                            
BUFNAME  DS    CL15                                                             
BUFPCT   DS    PL4                                                              
         DS    CL17                SPARE                                        
BUFACCS  DS    0CL128                                                           
BUFACCS1 DS    PL8                 ORIGINAL ESTIMATE                            
BUFACCSC DS    PL8                 LATEST REVISED ESTIMATE                      
BUFACCSF DS    PL8                 LATEST REVISED ESTIMATE GROSS                
BUFACCS2 DS    PL8                 PRESENT ESTIMATE                             
BUFACCS3 DS    PL8                 ORDERED                                      
BUFACCS4 DS    PL8                 NET                                          
BUFACCS5 DS    PL8                 COMM                                         
BUFACCS6 DS    PL8                 GROSS                                        
BUFACCS7 DS    PL8                 CD                                           
BUFACCS8 DS    PL8                 BILLED                                       
BUFACCS9 DS    PL8                 UNBILLED CHARGES                             
BUFACCSA DS    PL8                 ALLOCATED CHARGES                            
BUFACCSB DS    PL8                 ALLOCATED HOURS                              
BUFACCSD DS    PL8                 GROS EST                                     
BUFACCSE DS    PL8                 GROSS EST COM                                
BUFACCSH DS    PL8                 HIGHEST REVISION                             
BUFACCSQ EQU   ((*-BUFACCS)/8)                                                  
BUFRECL  EQU   *-BUFREC                                                         
STACK    DS    CL(NPRTSWS*12)                                                   
WCSW     DS    PL2                                                              
MIDSW    DS    CL1                                                              
PRTSWS   DS    0CL2                PRINT SWITCHES (W/C,JOB)                     
ORDSW    DS    CL2                                                              
BILLSW   DS    CL2                                                              
NETSW    DS    CL2                                                              
COMSW    DS    CL2                                                              
GRSSW    DS    CL2                                                              
DISCSW   DS    CL2                                                              
BLLABLSW DS    CL2                                                              
NARRSW   DS    CL2                                                              
HRSSW    DS    CL2                                                              
NPRTSWS  EQU   (*-PRTSWS)/2                                                     
CPSW     DS    CL1                                                              
CHRGPEND DS    CL1                                                              
BALANCE  DS    CL1                 INDICATE PRINTING BALANCES                   
TOTXJOB  DS    CL1                 INDICATE PRINTING AN XJOB TOTAL              
SUPPSW   DS    CL1                                                              
PAGEWDTH DS    CL1                                                              
EL4BCNT  DS    PL2                                                              
RATE     DS    CL1                                                              
         DS    CL1                                                              
STAFRATE DS    CL1                                                              
OOPRATE  DS    CL1                                                              
*                                                                               
JXBLOCK  DS    CL(JXLEN)                                                        
SVNCODES DS    CL1                                                              
SVCODES  DS    CL(L'JXCODES)                                                    
TSAREA   DS    CL(TSARDL)                                                       
*                                                                               
PROFILES DS    0CL64               ROOM FOR 4 16 BYTE PROFILE SCREENS           
PROF01   DS    CL1                 NEW PAGE FOR PRODUCT TOTALS                  
PROF02   DS    CL1                 PRINT BATCH REFERENCE                        
PROF03   DS    CL1                 PRINT REDUCED FORMAT                         
PROF04   DS    CL1                 STACK INVOICE NUMBER/DATE                    
PROF05   DS    CL1                 PRINT ORDER DETAILS                          
PROF06   DS    CL1                 PRINT NARRATIVE                              
PROF07   DS    CL1                 PRINT NET AND/OR GROSS                       
PROF08   DS    CL1                 PRINT BILL NUMBER/AMOUNT/DATE                
PROF09   DS    CL1                 MERGE ORDERS INTO WORKCODE                   
PROF10   DS    CL1                 PRINT COMENTS AND/OR PROFILES                
PROF11   DS    CL1                 PRINT COMMISSION AND/OR CD                   
PROF12   DS    CL1                 PRINT W/C SUMMARY ON NEW PAGE                
PROF13   DS    CL1                 PRINT NET/GROSS AS 'LESS CD'                 
PROF14   DS    CL1                 PRINT SUPPLIER NAME                          
PROF15   DS    CL1                 UNUSED                                       
PROF16   DS    CL1                 WORKCODE SUMMARY FORMAT                      
*                                                                               
PROF17   DS    CL1                 PRINT GROSS/NET ESTIMATE VALUE               
PROF18   DS    CL1                 INCLUDE ALL CHARGES IN SUMMARY               
PROF19   DS    CL1                 SORT REPORT BY OFFICE/CLIENT                 
PROF20   DS    CL1                 PRINT RETAIL RECORD AFTER JOB                
PROF21   DS    CL1                 DOUBLE SPACE VENDORS                         
PROF22   DS    CL1                 SHOW EXCEPTION REASONS                       
PROF23   DS    CL1                 TREAT SK CONTRA AS TIME                      
PROF24   DS    CL1                 SUPPRESS MANPOWER SUMMARIES                  
PROF25   DS    CL1                 PRINT ESTIMATES WITHOUT ACTUALS              
PROF26   DS    CL1                 FLAG HELD ITEMS                              
PROF27   DS    CL1                 TREAT WORKCODE TYPE 'P' AS TIME              
PROF28   DS    CL1                 UNDERLINE WORKCODE TOTALS                    
PROF29   DS    CL1                 UNUSED                                       
PROF30   DS    CL1                 PRINT UNITS/PRICES                           
PROF31   DS    CL1                 UNUSED                                       
PROF32   DS    CL1                 UNUSED                                       
*                                                                               
PROF33   DS    CL1                 PRINT SUBTOTALS BY TIME/OOP                  
PROF34   DS    CL1                 PRINT ORDER DETAILS                          
PROF35   DS    CL1                 PRINT NARRATIVE                              
PROF36   DS    CL1                 PRINT EMPLOYEE NAME                          
PROF37   DS    CL1                 PRINT HOURS/RATE                             
PROF38   DS    CL1                 PRINT BILLABLE HOURS                         
PROF39   DS    CL1                 PRINT BILLED HOURS                           
PROF40   DS    CL1                 PRINT WRITE-OFF INFORMATION                  
PROF41   DS    CL1                 STACK WRITE-OFF NUMBER/BILL NUMBER           
PROF42   DS    CL1                 PRINT BILLED/BILLABLE                        
PROF43   DS    CL1                 STACK INVOICE NUMBER/DATE                    
PROF44   DS    CL1                 PRINT AMOUNTS                                
PROF45   DS    CL1                 PRINT COMMISSION                             
PROF46   DS    CL1                 PRINT BATCH REFERENCE                        
PROF47   DS    CL1                 PRINT GROSS/NET ESTIMATE VALUE               
PROF48   DS    CL1                 SEPARATE TIME/OOP BY WORKCODE TYPE           
*                                                                               
PROF49   DS    CL1                 PRINT ACCOUNT NAMES IN MP SUMMARY            
PROF50   DS    CL1                 FORCE TOTALS                                 
         DS    CL14                UNUSED PROFILES                              
*                                                                               
PRINTEST DS    CL1                                                              
LEDGPROF DS    CL48                LEDGER LEV COMPOSITE PROFILE                 
AREA     DS    CL240                                                            
PREVSRT  DS    (SRTDATAL)C        PREVIOUS SORT KEY                             
PREVSAVE DS    (SRTDATAL)C        PREVIOUS SORT KEY SAVE                        
         DS    0F                 FULL ALLIGN SORTAREA                          
BADJOB   DS    (SRTJBLN)C                                                       
PSAVE    DS    CL133                                                            
SORTANY  DS    X                   1=WE ADDED RECORDS TO SORTER                 
AC67DLEN EQU   *-AC67D                                                          
         EJECT .                                                                
*        COVERS SUMMARY TABLE                                                   
SUMTABD  DSECT                                                                  
SMTBWANT DS    CL1                                                              
SMTBLEN  DS    CL1                                                              
SMTBPOSI DS    CL1                                                              
SMTBNAM1 DS    CL10                                                             
SMTBNAM2 DS    CL10                                                             
SMTBJBSW DS    CL1                 FIELD IS ACTIVE FOR JOB                      
SMTBPRSW DS    CL1                                                              
SMTBCLSW DS    CL1                                                              
SMTBRQSW DS    CL1                                                              
SMTBOFSW DS    CL1                 FIELD IS ACTIVE FOR OFFICE                   
SMTBFNUM DS    CL1                                                              
SMTBSTAT DS    CL1                 X'01' 1 = NOT A BUFFALO ACCUM.               
*                                  X'02' 1 = DO NOT PRINT BALANCE               
*                                  X'04' 1 = DO NOT PRINT FOR WC=99             
*                                  X'08' 1 = NO ACCUMULATOR                     
*                                                                               
*                                                                               
TABLED   DSECT                                                                  
CONTRAAC DS    CL30                                                             
INVNUM   DS    CL30                                                             
INVDATE  DS    CL30                                                             
SUPPNAME DS    CL30                                                             
ESTVALUE DS    CL30                                                             
ORDNO    DS    CL30                                                             
ORDAMT   DS    CL30                                                             
TRATE    DS    CL30                                                             
THOURS   DS    CL30                                                             
UNITPRCE DS    CL30                                                             
NET      DS    CL30                                                             
COMM     DS    CL30                                                             
GROSS    DS    CL30                                                             
TCOMDEF  DS    CL30                                                             
DISCOUNT DS    CL30                                                             
DISCDEF  DS    CL30                                                             
TWODATE  DS    CL30                                                             
TWOHOURS DS    CL30                                                             
TWOAMNT  DS    CL30                                                             
TWONUM   DS    CL30                                                             
BILLNUM  DS    CL30                                                             
TBLDHRS  DS    CL30                                                             
BILLAMT  DS    CL30                                                             
BILLDTE  DS    CL30                                                             
TBLABLHR DS    CL30                                                             
BILLABLE DS    CL30                                                             
BILLDEF  DS    CL30                                                             
HELD     DS    CL30                                                             
NARR     DS    CL30                                                             
BTCH     DS    CL30                                                             
         EJECT ,                                                                
TRNSAVED DSECT                     DSECT TO COVER TRANSAVE AREA                 
TRSVNET  DS    PL8                                                              
TRSVCOM  DS    PL8                                                              
TRSVGRS  DS    PL8                                                              
TRSVCD   DS    PL8                                                              
TRSVBLD  DS    PL8                                                              
TRSVBCD  DS    PL8                 BILLED CD                                    
TRSVBHR  DS    PL8                 BILLED HOURS                                 
TRSVBAB  DS    PL8                 BILLABLE                                     
TRSVWOA  DS    PL8                 W/O AMNOUNT                                  
TRSVWOH  DS    PL8                 W/O HOURS                                    
TRSVHRS  DS    PL8                 TOTAL HOURS                                  
*                                                                               
*              DSECT FOR ACCUMULATORS                                           
*                                                                               
ACCUMD   DSECT                                                                  
ORESTAC  DS    PL8                                                              
REVESTAC DS    PL8                                                              
GRVESTAC DS    PL8                                                              
CURESTAC DS    PL8                                                              
ORDAC    DS    PL8                                                              
NETAC    DS    PL8                                                              
COMMAC   DS    PL8                                                              
GRSAC    DS    PL8                                                              
CDAC     DS    PL8                                                              
BILLAC   DS    PL8                                                              
UNBILLAC DS    PL8                                                              
ALLOCAC  DS    PL8                                                              
ALLOCHRS DS    PL8                                                              
ESTGRSAC DS    PL8                 BUFACCSD                                     
ESTCOMAC DS    PL8                 BUFACCSE                                     
RHIESTAC DS    PL8                 BUFACCSH                                     
ACCUMNUM EQU   ((*-ACCUMD)/8)     NUMBER OF PL8 ACCUMS ABOVE                    
         EJECT ,                                                                
SUMMARYD DSECT                     COVERS SUMMARY TABLE                         
ORIGEST  DS    CL30                                                             
CURRREV  DS    CL30                                                             
GRSREV   DS    CL30                                                             
CURREST  DS    CL30                                                             
COMMEST  DS    CL30                                                             
GROSSEST DS    CL30                                                             
SORDAMT  DS    CL30                                                             
SNET     DS    CL30                                                             
OVUNEST  DS    CL30                                                             
OVUNPCT  DS    CL30                                                             
SCOMM    DS    CL30                                                             
SGROSS   DS    CL30                                                             
GOVUNEST DS    CL30                                                             
SDISC    DS    CL30                                                             
SBILLAMT DS    CL30                                                             
UNBILLED DS    CL30                                                             
HIGHREV  DS    CL30                                                             
         EJECT ,                                                                
TIMED    DSECT                     DSECT TO COVER MP SUMMARY BUFF RECS          
TIMEKEY  DS    0CL29                                                            
TIMETYPE DS    CL1                 C'2'                                         
TIMEACCT DS    0CL15               ACCOUNT + TY48                               
         DS    CL2                                                              
         DS    CL1                                                              
         DS    CL2                                                              
         DS    CL2                                                              
         DS    CL7                                                              
TIMETY48 DS    CL1                 KEEP TYPE 47'S AND 48'S SEPARATE             
TIMEDATE DS    PL3                 INV DATE                                     
TIMEREF  DS    CL6                 INV NO.                                      
TIMERATE DS    PL4                 RATE                                         
TIMEOTH  DS    0CL36                                                            
TIMENAME DS    CL36                NAME                                         
TIMEACCS DS    0CL128              LEN IS 16 PL8 ACCUMS                         
TIMEHOUR DS    PL8                 HOURS                                        
TIMEWOHR DS    PL8                 HOURS WRITTEN OFF                            
TIMEBAHR DS    PL8                 BILLABLE HOURS                               
TIMEGRS  DS    PL8                 GROSS                                        
TIMEWOAM DS    PL8                 AMOUNT WRITTEN OFF                           
TIMEBGRS DS    PL8                 BILLABLE GRS (GROSS - WRITTEN OFF)           
TIMEBILL DS    PL8                 BILLED                                       
TIMEUNBL DS    PL8                 UNBILLED CHARGES                             
TIMECNT  DS    PL8                 COUNT RECORDS IN THIS TOTAL                  
TIMECNT1 DS    PL8                 NUMBER OF DETAIL IN OFFICE TOT               
*                                  WHEN DEP TOT IS SUPPRESSED                   
*                                  USED IN MP SUM                               
NTIMEACC EQU   (*-TIMEACCS)/8      NUMBER OF ACCUMS USED                        
         DS    PL8                 N/D                                          
         DS    PL8                 N/D                                          
         DS    PL8                 N/D                                          
         DS    PL8                 N/D                                          
         DS    PL8                 N/D                                          
         DS    PL8                 N/D                                          
*                                                                               
*                                                                               
*              DSECT TO COVER THE LITERAL TABLE                                 
*                                                                               
LITTABD  DSECT                                                                  
TIMCHA   DS    CL17                                                             
OOPCHA   DS    CL17                                                             
TOTCHA   DS    CL17                                                             
TOTCHAWC DS    CL17                                                             
BILDET   DS    CL17                                                             
TIMBIL   DS    CL17                                                             
OOPBIL   DS    CL17                                                             
TOTBIL   DS    CL17                                                             
JOBBAL   DS    CL17                                                             
EXCREA   DS    CL17                                                             
PARTIN   DS    CL17                                                             
PROCHA   DS    CL17                                                             
PROBAL   DS    CL17                                                             
CLICHA   DS    CL17                                                             
CLIBAL   DS    CL17                                                             
OFFCHA   DS    CL17                                                             
OFFBAL   DS    CL17                                                             
REQCHA   DS    CL17                                                             
REQBAL   DS    CL17                                                             
NEED2BL  DS    CL17                                                             
UNBCODES DS    CL17                                                             
OTHINFO  DS    CL17                                                             
MPTOTS   DS    CL19                                                             
OOPTOTS  DS    CL19                                                             
LITEST   DS    CL10                                                             
LITBILL  DS    CL15                                                             
LITORD   DS    CL15                                                             
LITPRXJ  DS    CL15                                                             
LITCLXJ  DS    CL15                                                             
LITOFXJ  DS    CL15                                                             
LITRQXJ  DS    CL15                                                             
*                                                                               
*              DSECT TO COVER TIME SUMMARY PRINT LINE                           
*                                                                               
PRINTD   DSECT                                                                  
         DS    CL1                                                              
PACCT    DS    CL17                ACCOUNT CODE/NAME                            
         DS    CL1                                                              
PREFDATE DS    CL8                 INV NO./INVOICE DATE                         
         DS    CL1                                                              
PRATE    DS    CL6                 RATE                                         
         DS    CL1                                                              
PHOUR    DS    CL8                 HOURS                                        
         DS    CL1                                                              
PWOHOUR  DS    CL8                 HOURS WRITTEN OFF THIS CHARGE                
         DS    CL1                                                              
PBABLEHR DS    CL8                 BILLABLE HOURS                               
         DS    CL1                                                              
PGRS     DS    CL13                GROSS                                        
         DS    CL1                                                              
PWOGRS   DS    CL13                GROSS WRITTEN OFF THIS CHARGE                
         DS    CL1                                                              
PBABGRS  DS    CL13                BILLABLE GROSS (GROSS - WRITTEN OFF)         
         DS    CL1                                                              
PBILL    DS    CL13                BILLED                                       
         DS    CL1                                                              
PUNBL    DS    CL13                UNBILLED CHARGES                             
         EJECT ,                                                                
*        STORAGE OBTAINED BY LINKLIST ROUTINE                                   
*                                                                               
LLWORKD  DSECT                                                                  
LLKEY    DS    CL(ACCKLEN)                                                      
LLKEYSV  DS    CL(ACCKLEN)                                                      
LLWORKLN EQU   *-LLWORKD                                                        
*                                                                               
*        STORAGE OBTAINED BY NEW ACC READ ROUTINES                              
NAWORKD  DSECT                                                                  
NAAKEY   DS    A                                                                
NAAKEYSV DS    A                                                                
NAAIO    DS    A                                                                
NACOM    DS    CL8                                                              
NAWORKLN EQU   *-NAWORKD                                                        
         EJECT ,                                                                
*                                                                               
*        TABLE OF PST SUMMARIZED BY PROVINCE NAME                               
*                                                                               
PSTTABD  DSECT                                                                  
PSTTTYPE DS    CL1                                                              
PSTTTBIQ EQU   1                   THIS TABLE ENTRY IS FOR A BILL               
PSTTTJBQ EQU   2                   THIS TABLE ENTRY IS FOR THE JOB              
PSTTNAME DS    CL6                                                              
PSTTAMNT DS    PL6                                                              
PSTTABLN EQU   *-PSTTABD                                                        
         EJECT ,                                                                
SORTD    DSECT                     DSECT FOR SORT KEY                           
SRTLRECL DS    F                  FULL RESERVED FOR LRECL                       
SRTKEY   DS    0C                                                               
SRTOFF   DS    CL2                                                              
SRTOFLN  EQU   *-SRTKEY                                                         
SRTCLI   DS    CL3                                                              
SRTCLLN  EQU   *-SRTKEY                                                         
SRTPRO   DS    CL3                                                              
SRTPRLN  EQU   *-SRTKEY                                                         
SRTJOB   DS    CL6                                                              
SRTJBLN  EQU   *-SRTKEY                                                         
SRTTYPE  DS    CL1                TIME OR OOP OR BILLING                        
SRTTBILL EQU   C'1'                SORT RECORD IS BILLING                       
SRTTTIME EQU   C'2'                SORT RECORD IS TIME                          
SRTTOOP  EQU   C'3'                SORT RECORD IS AN O_O_P TRANSACTION          
SRTTYLN  EQU   *-SRTKEY                                                         
SRTWC    DS    CL2                WORKCODE                                      
SRTWCLN  EQU   *-SRTKEY                                                         
SRTCA    DS    CL14               CONTRA ACCOUNT                                
SRTCALN  EQU   *-SRTKEY                                                         
SRTDATE  DS    CL3                DATE ADDED                                    
SRTNUM   DS    CL6                REF NUM                                       
SRTSBR   DS    CL6                SUB REF NUM (FOR SORT)                        
SRTUSED  DS    CL2                                                              
SRTKEYLN EQU   *-SORTD                                                          
SRTDATAL EQU   *-SRTKEY                                                         
SRTDATA  DS    CL(900-SRTKEYLN)                                                 
SRTRECLN EQU   *-SORTD                                                          
*                                                                               
SR44D    DSECT                                                                  
SR44ID   DS    CL1                                                              
SR44LEN  DS    CL1                                                              
SR44AMNT DS    PL6                                                              
SR44DATE DS    CL3                                                              
SR44BTCH DS    CL6                                                              
SR44MOS  DS    CL2                 X'YYMM' MONTH OF SERV                        
SR44STAT DS    CL1                                                              
SR44TYPE DS    CL1                                                              
SR44USED DS    CL2                                                              
SR44NARR DS    0C                 VARIBLE LENGTH NARRATIVE                      
SR44LN1  EQU   *-SR44D                                                          
*                                                                               
SR40D    DSECT                                                                  
SR40ID   DS    CL1                                                              
SR40LEN  DS    CL1                                                              
SR40RATE DS    PL4                                                              
SR40HOUR DS    CL3                                                              
SR40LN1  EQU   *-SR40D                                                          
*                                                                               
SR4BD    DSECT                                                                  
SR4BID   DS    CL1                                                              
SR4BLEN  DS    CL1                THIS EL IS VARIBLE LEN                        
SR4BNO   DS    CL6                                                              
SR4BAMNT DS    PL5                                                              
SR4BDTE  DS    CL2                                                              
SR4BLN1  EQU   *-SR4BD            LENGTH OF AN A21 TYPE 4B                      
SR4BRATE DS    PL4                                                              
SR4BCSD  DS    PL5                                                              
SR4BHRS  DS    PL5                                                              
SR4BLEN2 EQU   *-SR4BD            LEN OF A $BILL 4B                             
TYPE57   EQU   1                  4B ELEMENTS ARE PROCESSED DIFFERNTLY          
NONCOM   EQU   2                  FOR WRITE OFFS AND NON COM ITEMS              
NEGAMNT  EQU   4                  IF NEG 4B AMOUNT, MAKE SURE OF NEG HR         
NEGHRS   EQU   8                  IF NEG 4B HRS, MAKE SURE OF NEG AMNT          
*                                                                               
SR4ED    DSECT                                                                  
SR4EID   DS    CL1                                                              
SR4ELEN  DS    CL1                                                              
SR4EACC  DS    CL14                                                             
SR4ETYPE DS    CL1                                                              
SR4EDATE DS    CL3                                                              
SR4ELN1  EQU   *-SR4ED                                                          
*                                                                               
SR4CD    DSECT                                                                  
SR4CID   DS    CL1                                                              
SR4CLEN  DS    CL1                                                              
SR4CACCT DS    CL14                                                             
SR4CLN1  EQU   *-SR4CD                                                          
*                                                                               
SR50D    DSECT                                                                  
SR50ID   DS    CL1                                                              
SR50LEN  DS    CL1                                                              
SR50AMNT DS    PL6                                                              
SR50LN1  EQU   *-SR50D                                                          
*                                                                               
SR23D    DSECT                                                                  
SR23ID   DS    CL1                                                              
SR23LEN  DS    CL1                                                              
SR23NUM  DS    CL6                                                              
SR23LN1  EQU   *-SR23D                                                          
*                                                                               
SR25D    DSECT                                                                  
SR25ID   DS    CL1                                                              
SR25LEN  DS    CL1                                                              
SR25NO   DS    CL6                                                              
SR25LN1  EQU   *-SR25D                                                          
*                                                                               
SR60D    DSECT                                                                  
SR60ID   DS    CL1                                                              
SR60LEN  DS    CL1                                                              
SR60RMOS DS    CL2                                                              
SR60LN1  EQU   *-SR60D                                                          
*                                                                               
SR68D    DSECT                                                                  
SR68ID   DS    CL1                                                              
SR68LEN  DS    CL1                                                              
SR68WC   DS    CL2                                                              
SR68AMNT DS    CL6                                                              
SR68AVIL DS    CL6                                                              
SR68LN1  EQU   *-SR68D                                                          
*                                                                               
SRGSD    DSECT                                                                  
SRGSID   DS    CL1                 SAVE A BILLS GSAT AMOUNTS                    
SRGSLEN  DS    CL1                                                              
SRGSTYPE DS    CL1                                                              
SRGSRATE DS    XL2                                                              
SRGSINDS DS    X                                                                
SRGSAMNT DS    CL6                                                              
SRGSLN1  EQU   *-SRGSD                                                          
*                                                                               
SRPSD    DSECT                                                                  
SRPSID   DS    CL1                 SAVE A BILLS GSAT AMOUNTS                    
SRPSLEN  DS    CL1                                                              
SRPSTYPE DS    CL1                                                              
SRPSRATE DS    XL2                                                              
SRPSINDS DS    X                                                                
SRPSAMNT DS    CL6                                                              
SRPSPRV  DS    CL2                                                              
SRPSNAME DS    CL6                                                              
SRPSLN1  EQU   *-SRPSD                                                          
*                                                                               
SR7CD    DSECT                                                                  
SR7CID   DS    CL1                 SAVE A BILLS GSAT AMOUNTS                    
SR7CLEN  DS    CL1                                                              
SR7CPRCE DS    CL4                                                              
SR7CUNIT DS    XL3                                                              
SR7CSTAT DS    XL1                                                              
SR7CLN1  EQU   *-SR7CD                                                          
*                                                                               
SRAMTSD  DSECT                     TRANSACTION AMOUNTS FROM PRORATA             
SRAID    DS    CL1                                                              
SRAIDQ   EQU   X'FF'                                                            
SRALEN   DS    CL1                                                              
SRASTAT3 DS    CL1                                                              
*                                                                               
SRANET   DS    PL6                 NET                                          
SRABILL  DS    PL6                 BILLED                                       
SRAALLOC DS    PL6                 ALLOCATED                                    
SRATRANS DS    PL6                 TRANSFERED                                   
SRAWRIOF DS    PL6                 WRITTEN OFF                                  
*                                                                               
SRAN_CD  DS    PL6                 NET CD                                       
SRAB_CD  DS    PL6                 BILLED CD                                    
SRAA_CD  DS    PL6                 ALLOCATED CD                                 
SRAT_CD  DS    PL6                 TRANSFERED CD                                
SRAW_CD  DS    PL6                 WRITTEN OFF CD                               
*                                                                               
SRAN_HR  DS    PL6                 NET HOURS                                    
SRAB_HR  DS    PL6                 BILLED HOURS                                 
SRAA_HR  DS    PL6                 ALLOCATED HOURS                              
SRAT_HR  DS    PL6                 TRANSFERED HOURS                             
SRAW_HR  DS    PL6                 WRITTEN OFF HOURS                            
SRAMLN1  EQU   *-SRAMTSD                                                        
*                                                                               
         EJECT ,                                                                
RBILLD   DSECT                     TO COVER TABLE OF SAVED PARTICIPANTS         
RBNUM    DS    CL12                ACCOUNT NUMBER                               
RBUNITS  DS    PL6                 UNITS                                        
RBLEN    EQU   *-RBILLD                                                         
*                                                                               
PROFKD   DSECT                         TO COVER GETPROFS KEY                    
PROFKEY  DS    0CL16                                                            
PROFKSYS DS    CL1                                                              
PROFKPGM DS    CL3                                                              
         DS    CL1                                                              
PROFKUNL DS    CL2                                                              
PROFKACC DS    CL3                                                              
PROFKAST DS    CL1                                                              
PROFKOFF DS    CL1                                                              
PROFKAGY DS    CL2                                                              
PROFKOFC DS    CL2                 NEW OFFICE                                   
*                                                                               
*                                                                               
BILLTABD DSECT                     DSECT TO COVER  TABLE                        
BILLKEY  DS    0C                  SAVED SORT KEY OF THIS RECORD                
BILLCA   DS    CL14                                                             
BILLNO   DS    CL6                                                              
BILLDATE DS    CL3                                                              
BILLKYLN EQU   *-BILLTABD                                                       
BILLUS   DS    CL2                                                              
BILLBTCH DS    CL6                                                              
BILLMOS  DS    CL2                                                              
BILLTYPE DS    CL15                                                             
BILLNET  DS    PL6                                                              
BILLCOM  DS    PL6                                                              
BILLCD   DS    PL6                                                              
BILTNET  DS    PL6                                                              
BILTCOM  DS    PL6                                                              
BILTHRS  DS    PL4                                                              
BILLGST  DS    AL2                 ADDRESS OF GST ELEMENTS FOR THIS EL          
LBILLTAB EQU   *-BILLTABD                                                       
         SPACE 2                                                                
ACCUMSD  DSECT                                                                  
ACCUMS   DS    0CL(8*ACCUMNUM)                                                  
BILLTOTS DS    0CL(8*ACCUMNUM)     TOTAL BILLING, IF BILLS ARE SEPAR'ED         
SAVCHTOT DS    CL(8*ACCUMNUM)      NUMBER OF ACCUMS BY PL8                      
TOTCHTYP DS    CL(8*ACCUMNUM)                                                   
TOTCHJOB DS    CL(8*ACCUMNUM)      ACCUMNUM IS DEFINED IN ACCUMD                
TOTCHPRD DS    CL(8*ACCUMNUM)                                                   
TOTCHCLI DS    CL(8*ACCUMNUM)                                                   
TOTCHREQ DS    CL(8*ACCUMNUM)                                                   
TOTCHOFF DS    CL(8*ACCUMNUM)                                                   
TOTCHRGS DS    CL(8*ACCUMNUM)                                                   
SAVETOTS DS    CL(8*ACCUMNUM)                                                   
TYPTOTS  DS    CL(8*ACCUMNUM)                                                   
JOBTOTS  DS    CL(8*ACCUMNUM)                                                   
PRDTOTS  DS    CL(8*ACCUMNUM)                                                   
CLITOTS  DS    CL(8*ACCUMNUM)                                                   
REQTOTS  DS    CL(8*ACCUMNUM)                                                   
OFFTOTS  DS    CL(8*ACCUMNUM)                                                   
JOBXJOBS DS    CL(8*ACCUMNUM)      NEEDED SO TOTALS BUMPS INTO PRDXJOB          
PRDXJOBS DS    CL(8*ACCUMNUM)                                                   
CLIXJOBS DS    CL(8*ACCUMNUM)                                                   
REQXJOBS DS    CL(8*ACCUMNUM)                                                   
OFFXJOBS DS    CL(8*ACCUMNUM)                                                   
ACCUMLEN EQU   *-ACCUMSD                                                        
*                                                                               
TIMACCD  DSECT                     FOR HIGHER LEVEL MANPOWER TOTALS             
PROTIME  DS    CL(8*NTIMEACC)                                                   
CLITIME  DS    CL(8*NTIMEACC)                                                   
OFFTIME  DS    CL(8*NTIMEACC)                                                   
REQTIME  DS    CL(8*NTIMEACC)                                                   
TIMACCLN EQU   *-TIMACCD                                                        
*                                                                               
TIMSUBD  DSECT                     WC, TYPE AND JOB MANPOWER TOTALS             
TSHRS    DS    PL8                                                              
TSBLDHRS DS    PL8                                                              
TSWOHRS  DS    PL8                                                              
TSWOAMT  DS    PL8                                                              
TIMSUBLN EQU   *-TIMSUBD                                                        
*                                                                               
*-------------------------------------------------------------------*           
* DSECT FOR MAIN TAB, A TABLE WHICH LOOPS THRU THE STORAGE GETMAIN              
*        GETS                                                                   
*-------------------------------------------------------------------*           
MAIND    DSECT                                                                  
MAINAST  DS    S                   ADDRESS TO STORE A(TABLE)                    
MAINSIZE DS    Y                                                                
MAINLEN  EQU   *-MAIND                                                          
         EJECT ,                                                                
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         EJECT ,                                                                
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* AVATICAND                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACVATICAND                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDBUFFALOD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACJOBBERD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
* ACREPPROFD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPPROFD                                                     
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         EJECT ,                                                                
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
         EJECT ,                                                                
PRORATAD DSECT                                                                  
       ++INCLUDE ACPRORATAD                                                     
*                                                                               
       ++INCLUDE ACJOBEXCPD                                                     
*                                                                               
       ++INCLUDE DDTSARD                                                        
MPNAWKD  DSECT                                                                  
MPTSREC  DS    0C                                                               
MPTSKEY  DS    CL14                                                             
MPTSNAME DS    CL36                                                             
MPTSRECL EQU   *-MPTSREC                                                        
MPKEY    DS    CL(ACCKLEN)                                                      
MPIO     DS    CL(ACCKLEN+2000)                                                 
MPNAWKLN EQU   *-MPNAWKD                                                        
MPBFSZ   EQU   1000*MPTSRECL                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024ACREP6702 09/12/14'                                      
         END                                                                    
