*          DATA SET SPNWS05WZ  AT LEVEL 069 AS OF 08/13/97                      
*PHASE T20705A,*                                                                
         TITLE 'NWS05 - BUYERS WORK SHEET - WORK LIST/SELECT OVERLAY'           
T20705   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20705**,RA,RR=RE                                              
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APNTRYA                                                       
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
*                                                                               
         LA    R0,ACOMMN           SET COMMON ROUTINES                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         L     R1,=A(COMMON)                                                    
         A     R1,APRELO                                                        
         ST    R1,ACOMM(RE)                                                     
         STC   RF,ACOMM(RE)                                                     
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
         MVI   COMCHG,0            INIT RECORD CHANGE INDICATOR                 
*                                                                               
         MVI   APBYTE,8                                                         
         CLI   INREC,RECPKG        TEST RECORD=PACKAGE                          
         BE    BWS4                YES-CALL BWS08                               
         CLI   INREC,RECORB        TEST RECORD=ORBIT                            
         BE    BWS4                YES-CALL BWS08                               
         MVI   APBYTE,X'0B'                                                     
         CLI   INREC,RECGOL        TEST RECORD=GOAL                             
         BE    BWS4                YES-CALL BWS0B                               
*                                                                               
         MVI   APBYTE,9                                                         
         CLI   APACTN,ACTSPL       TEST ACTION=SPILL                            
         BE    BWS4                YES-CALL BWS09                               
         MVI   APBYTE,X'0A'                                                     
         CLI   APACTN,ACTSEN       TEST ACTION=SEND                             
         BE    BWS4                YES-CALL BWS0A                               
         MVI   APBYTE,X'96'                                                     
         CLI   APACTN,ACTCAL       TEST ACTION=CALL (CALL LETTER CHA)           
         BE    BWS4                                                             
         MVI   APBYTE,X'98'                                                     
         CLI   APACTN,ACTFIX       TEST FOR ACTION FIX                          
         BE    BWS4                                                             
         MVI   APBYTE,6                                                         
         CLI   APACTN,ACTDEM       TEST FOR ACTION DEMO                         
         BE    BWS4                                                             
         CLI   INOSID,0            OR SID OPTION SPECIFIED                      
         BE    BWS2                   AND NOT WORK TRANSFER                     
         CLI   APACTN,ACTXFR                                                    
         BNE   BWS4                                                             
         CLI   INREC,RECWRK                                                     
         BNE   BWS4                YES-CALL BWS06                               
*                                                                               
BWS2     MVI   APBYTE,7                                                         
         CLI   APACTN,ACTXFR       TEST FOR WORK TRANSFER                       
         BNE   BWS6                YES-CALL BWS07                               
*                                                                               
BWS4     MVC   COMPARM,APPARM     (SAVE APPARM)                                 
         XC    APPARM,APPARM                                                    
         MVC   APPARM(1),APBYTE       SET OVERLAY NUMBER                        
         GOTO1 VCOLY,APPARM,,0,0      CALL OVERLAY                              
         L     RF,0(R1)                                                         
         BALR  RE,RF                                                            
         B     EXIT                                                             
*                                                                               
BWS6     LA    R2,IOKEY                                                         
         USING BWHRECD,R2                                                       
         LA    R3,APRECKEY                                                      
         USING BWDRECD,R3                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         DC    AL4(0)  VALKEY                                                   
         DC    AL4(0)  VALREC                                                   
         DC    AL4(0)  DISKEY                                                   
         DC    AL4(0)  DISREC                                                   
         DC    AL4(0)  DELREC                                                   
         DC    AL4(0)  RESREC                                                   
         B     VALPAR                                                           
         B     GETSEL                                                           
         B     DISSEL                                                           
         B     VALSEL                                                           
         B     EXIT    FSTLST                                                   
         B     ERASE   APMPROC                                                  
         B     FSTSCR                                                           
         B     LASSCR                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     PUTKEY                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     DISSEL                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT PARAMETERS                                          *         
* OUTPUT : FVMSGNO NE FVFOK IF KEY IS INVALID                         *         
*          APRECKEY                                                   *         
*          APPARM FOR ROOT                                            *         
***********************************************************************         
         SPACE 1                                                                
VALPAR   GOTO1 =A(VALPRM),RR=APRELO  VALIDATE SELECT PARAMETERS                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIRST LIST SCREEN HOOK                                              *         
***********************************************************************         
         SPACE 1                                                                
FSTSCR   MVI   SVACTIV,0           NO AVTIVITY YET                              
         CLI   APACTN,ACTSSK       TEST SUPER-SKED ACTION                       
         BE    FSCR2                                                            
         CLI   APACTN,ACTSKD       TEST SCHEDULE ACTION                         
         BNE   FSCRX                                                            
         NI    TWAFLAG,FF-TWAFMTPD                                              
         GOTO1 ATWABLD,TWABELS     BUILD SCHEDULE HALF OF SCREEN                
*                                                                               
FSCR2    LA    R2,CMPDATSD         FORMAT THE SCHEDULE CALENDAR                 
         LA    R8,WRKL8H+CAL1DSPL                                               
         LA    R9,WRKL8H+CAL2DSPL                                               
         CLI   APACTN,ACTSSK                                                    
         BE    *+12                                                             
         OI    7(R8),X'80'                                                      
         B     *+12                                                             
         LA    R8,SSKCL1H                                                       
         LA    R9,SSKCL2H                                                       
         MVC   8(55,R8),SPACES                                                  
         MVC   8(55,R9),SPACES                                                  
         OI    6(R8),FVOXMT                                                     
         OI    6(R9),FVOXMT                                                     
         LA    R4,L'FVIHDR(R8)                                                  
         LA    R8,L'FVIHDR-1(R8)                                                
         LA    R9,L'FVIHDR-1(R9)                                                
*                                                                               
FSCR6    MVC   2(2,R9),4(R2)       DAY NUMBER                                   
         LA    R8,4(R8)                                                         
         LA    R9,4(R9)                                                         
         CLI   6(R2),FF            TEST END OF CAMPAIGN PERIOD                  
         BE    FSCR8                                                            
         CLC   2(2,R2),8(R2)       TEST CHANGE OF MONTH                         
         BNE   FSCR8                                                            
         LA    R2,6(R2)                                                         
         B     FSCR6                                                            
*                                                                               
FSCR8    LR    RE,R8               FORMAT MONTH NAME TO LINE ABOVE              
         SR    RE,R4                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),DASHES                                                   
         LA    RE,2(RE)                                                         
         SRL   RE,1                                                             
         AR    R4,RE                                                            
         BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         GOTO1 VDATCON,APPARM,(0,(R2)),(4,APWORK)                               
         MVC   0(3,R4),APWORK                                                   
         CLI   6(R2),FF            TEST END OF CAMPAIGN PERIOD                  
         BE    FSCR10              YES                                          
         LA    R2,6(R2)            NO - CONTINUE                                
         LA    R4,1(R8)                                                         
         B     FSCR6                                                            
*                                                                               
FSCR10   B     FSCRX                                                            
*                                                                               
FSCRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR LIST SCREEN HOOK                                      *         
***********************************************************************         
         SPACE 1                                                                
LASSCR   CLI   SCPFKEY,255     TEST FOR ANY LFM ACTION FROM THIS SCREEN         
         BE    LSCRX           YES-WE'LL STAY ON CURRENT SCREEN ANYWAY          
         MVC   SCPFKEY,APPFKEY     NO-SET PF KEY                                
         TM    SVACTIV,SVVALSEL    TEST FOR ANY VALIDATE LIST/SELECT            
         BZ    LSCR2                                                            
         CLI   APPFKEY,0           YES - TEST FOR NO PFKEY                      
         BNE   LSCR2                                                            
         MVI   SCPFKEY,PFK05       FORCE STAY ON CURRENT SCREEN                 
*                                                                               
LSCR2    MVI   APMODE,APMPFKS      PF KEY SET                                   
*                                                                               
LSCRX    NI    SVACTIV,FF-SVVALSEL                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
* INPUT  : APINDS EQ APILFLST FIRST TIME / FIRST SCREEN               *         
*                    APILNLST FIRST TIME / NOT FIRST SCREEN           *         
*                    OTHER    NEXT TIME(S)                            *         
*          APRECKEY                                                   *         
* OUTPUT : APRECKEY                                                   *         
*          APRECDA = STATION                                          *         
*          APRECID+0(1) = RECORD IDENTIFICATION BYTE                  *         
*          APRECID+1(2) = DAYPART/LENGTH                              *         
*          APMODE  = APMEOFS FOR END-OF-LIST                          *         
*          AND, FOR PACKAGE/ORBIT RECORDS :                           *         
*          APRECNUM = RECPKG OR RECORB                                *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   MVC   LTUNUM,SVTUNUM      RESTORE SAVED TSAR RECORD NUMBERS            
         MVC   LTUMAX,SVTUMAX      AND PAGE/LINE NUMBERS                        
         MVC   LPAGE,SVPAGE                                                     
         MVC   LPAGDSP,SVPAGDSP                                                 
         MVC   LLINE,SVLINE                                                     
         TM    TWAINDS,TWAICCSW                                                 
         BZ    GETS0                                                            
         MVC   LTUNUM,SVTUNUM2                                                  
         MVC   LTUMAX,SVTUMAX2                                                  
         MVC   LPAGE,SVPAGE2                                                    
         MVC   LPAGDSP,SVPAGDS2                                                 
         MVC   LLINE,SVLINE2                                                    
*                                                                               
GETS0    CLI   APINDS,APILFLST     TEST FOR VERY FIRST TIME                     
         BNE   GETS1                                                            
         MVI   LLINE,1             YES - INITIALIZE PAGE NUMBER                 
         MVI   LPAGE,C'A'                                                       
         MVI   LPAGDSP,0                                                        
         MVC   LTUNUM,=H'1'        START AT FIRST RECORD                        
         TM    TWAINDS,TWAICCSW    EXCEPT COMPANION CAMPAIGN,                   
         BZ    GETS1A                                                           
         LH    R1,SVTUMAX          START WHERE MASTER CAMPAIGN LEFT OFF         
         LA    R1,1(R1)                                                         
         STH   R1,LTUNUM                                                        
         B     GETS1A                                                           
*                                                                               
GETS1    CLI   APINDS,APILNLST     TEST FOR FIRST LINE OF SCREEN                
         BNE   GETS33                                                           
*                                                                               
GETS1A   MVI   SVACTIV,0           NO ACTIVITY FOR THIS SCREEN YET              
         MVI   FVMAXL,2                                                         
         GOTO1 AFVAL,WRKSCRH       VALIDATE SCROLL                              
         BH    GETSX                                                            
         BL    GETS4                                                            
         XC    WRKSCR,WRKSCR       CLEAR SCROLL                                 
         OI    WRKSCRH+6,FVOXMT                                                 
         OI    FVIFLD,C' '                                                      
         MVI   APBYTE,1                                                         
         CLI   FVILEN,2                                                         
         BNE   GETS2                                                            
         MVC   APBYTE,FVIFLD+1                                                  
         CLI   APBYTE,C'1'                                                      
         BL    ENOTV                                                            
         NI    APBYTE,X'0F'                                                     
         CLI   APBYTE,NSKDLINS                                                  
         BH    ENOTV                                                            
*                                                                               
GETS2    CLC   LPAGE,FVIFLD        TEST CHANGE OF PAGE AND LINE                 
         BNE   *+14                                                             
         CLC   LLINE,APBYTE                                                     
         BE    GETS4               NO                                           
         MVC   LLINE,APBYTE                                                     
         SR    R1,R1               YES - VALIDATE PAGE                          
         LA    RE,ALPHATAB                                                      
*                                                                               
GETS3    CLI   0(RE),FF                                                         
         BE    ENOTV                                                            
         CLC   FVIFLD(1),0(RE)                                                  
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,GETS3                                                         
         LPR   R1,R1                                                            
         MVC   LPAGE,FVIFLD                                                     
         STC   R1,LPAGDSP                                                       
         SR    R0,R0               FIND TSAR RECORD NUMBER OF FIRST             
         LA    RE,NSKDLINS         RECORD FOR THIS SCREEN                       
         MR    R0,RE                                                            
         ZIC   RE,LLINE                                                         
         AR    R1,RE                                                            
         TM    TWAINDS,TWAICCSW                                                 
         BZ    *+8                                                              
         AH    R1,SVTUMAX                                                       
         STH   R1,LTUNUM                                                        
*                                                                               
GETS4    MVC   LFSTLINE,LLINE      SAVE FIRST LINE NUMBER                       
         CLI   APINDS,APILFLST     TEST FOR FIRST LINE / FIRST PAGE             
         BNE   GETS36              NO                                           
*                                                                               
         LA    R4,TSARREC          YES-DELETE EXISTING TSAR RECORDS             
         USING TRECD,R4                                                         
         XC    TSARREC(TUPDRECL),TSARREC                                        
         MVI   TRECTYP,TRECUPD     RECORD TYPE = UPDATE                         
         MVI   APBYTE,1            SET CAMPAIGN SEQ NUM = 1                     
         TM    TWAINDS,TWAICCSW    OR 2 FOR COMPANION CAMPAIGN                  
         BZ    *+8                                                              
         MVI   APBYTE,2                                                         
         MVC   TUCAMP,APBYTE                                                    
*                                                                               
GETS5    MVI   TSARACT,TSARDH                                                   
         GOTO1 ATSAR,TREC                                                       
         BE    GETS6                                                            
         CLC   FVMSGNO,=AL2(FVFERNF)  TEST EXACT RECORD NOT FOUND               
         BNE   GETS7                  NO-MUST BE EOF                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
GETS6    CLI   TRECTYP,TRECUPD     TEST UPDATE RECORD                           
         BH    GETS7               NO-DONE                                      
         CLC   TUCAMP,APBYTE       TEST CORRECT CAMPAIGN SEQ NUM                
         BNE   GETS7               NO-DONE                                      
         MVI   TSARACT,TSADEL      YES-DELETE IT                                
         GOTO1 ATSAR,TREC                                                       
         BE    GETS5               NEXT RECORD                                  
         DC    H'0'                                                             
*                                                                               
GETS7    MVC   FVMSGNO,=AL2(FVFOK)                                              
         XC    SVACPTS(4*NMAXWKS),SVACPTS   CLEAR ACTUAL POINTS ACCUMS          
         XC    SVACXPTS,SVACXPTS                                                
         XC    SVACDOL,SVACDOL     CLEAR ACTUAL DOLLARS                         
         XC    SVGLDOL,SVGLDOL     CLEAR GOALS                                  
         XC    SVGLPTST,SVGLPTST                                                
         XC    SVGLPTS(56),SVGLPTS                                              
*                                                                               
         LA    RE,BWDKELST-BWDKEY  SET KEY COMPARE LENGTH                       
         MVI   LFLAG,0                                                          
         OC    QSTA,QSTA                                                        
         BZ    *+8                                                              
         LA    RE,L'BWDKELST(RE)                                                
         BCTR  RE,0                                                             
         STC   RE,LKEYCOMP                                                      
         XC    COMDPLST,COMDPLST                                                
         XC    COMSLLST,COMSLLST                                                
         SR    R8,R8               R8 = RECORD COUNT                            
         MVC   IOKEY(13),APRECKEY                                               
*                                                                               
GETS7A   LA    R1,MINHI2           START READING DETAIL RECORDS                 
         B     GETS8+4                                                          
*                                                                               
GETS8    LA    R1,MINSEQ2          READ NEXT DETAIL RECORD                      
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)   TEST REACHED END                           
         B     GETS30                                                           
         ZIC   RE,LKEYCOMP                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   GETS30                                                           
         L     R3,AIOAREA2         ADDRESS THE RECORD                           
*                                                                               
         OC    QSTA,QSTA           TEST STATION FILTER                          
         BZ    *+16                                                             
         CLC   QSTA,BWDSTA         YES-MATCH THE STATION                        
         BE    *+6                                                              
         DC    H'0'                UNMATCH-FATAL ERROR                          
         OC    QCABLE,QCABLE       TEST CABLE SYSTEM FILTER                     
         BZ    *+14                                                             
         CLC   QCABLE,BWDSTA       YES-MATCH CABLE SYSTEM                       
         BNE   GETS8                                                            
         CLI   BDPT,0              TEST DAYPART FILTER                          
         BE    GETS8B                                                           
         CLC   BWDDPT,BDPT         NO-MATCH THE DAYPART                         
         BE    GETS8B                                                           
         CLI   INODPT,C'M'         NO-TEST DPT=MAS AND DAYPART IS               
         BNE   GETS8                  A MASTER                                  
         CLI   DPTTYPE,C'M'                                                     
         BNE   GETS8                                                            
         LA    R1,DPTSUBS          YES-ACCEPT ANY OF THE SUB-DPTS               
         LA    R0,L'DPTSUBS                                                     
*                                                                               
GETS8A   CLI   0(R1),0                                                          
         BE    GETS8                                                            
         CLC   BWDDPT,0(R1)                                                     
         BNE   *+12                                                             
         OI    LFLAG2,LDPTMAS      INDICATE WE HAVE A DPT OTHER                 
         B     GETS8B              THAN THE MASTER                              
         LA    R1,1(R1)                                                         
         BCT   R0,GETS8A                                                        
         B     GETS8                                                            
*                                                                               
GETS8B   CLI   BSLN,0              TEST SPOT LENGTH FILTER                      
         BE    *+14                                                             
         CLC   BWDSLN,BSLN         YES-MATCH THE LENGTH                         
         BNE   GETS8                                                            
*                                                                               
         NI    LFLAG2,255-LEFDTSUP                                              
******** TM    LFLAG2,LCAMP2       ONLY SCHEDULED LINES FOR COMPANION           
******** BO    GETS9               CAMPAIGN                                     
********                  -------> MIGHT NEED TO PICK UNSCHEDULED LINES         
********                           FOR GOALS                                    
         CLI   APACTN,ACTSSK       TEST SCHEDULE ACTION                         
         BE    *+12                                                             
         CLI   APACTN,ACTSKD                                                    
         BNE   GETS9E                                                           
         TM    INOIND,INOISKD      YES-TEST SCHEDULED SPOTS ONLY                
         BZ    GETS9E                                                           
*                                                                               
GETS9    LA    R9,BWDEL            YES-LOOK FOR SPOTS PER WEEK ELEMENT          
         SR    R0,R0                                                            
*                                                                               
GETS9A   CLI   0(R9),0                                                          
         BE    GETS8               NO SCHEDULE - GET NEXT RECORD                
         CLI   0(R9),SPWELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R9)                                                         
         AR    R9,R0                                                            
         B     GETS9A                                                           
         OC    BWDEFDT2,BWDEFDT2   TEST EFFECTIVE DATES                         
         BZ    GETS9E                                                           
         GOTO1 VDATCON,APPARM,(3,BWDEFDT2),(2,APFULL)                           
         OC    BWDEFDT3,BWDEFDT3                                                
         BZ    GETS9B                                                           
         GOTO1 (RF),(R1),(3,BWDEFDT3),(2,APFULL+2)                              
*                                                                               
GETS9B   ZIC   RF,1(R9)            YES-SEE IF THERE NO SPOTS FOR ANY            
         AR    RF,R9                   EFFECTIVE DATE RANGES                    
         LA    R9,SPWPERWK-SPWEL(R9)                                            
         LA    RE,CMPDATSP                                                      
         OI    LFLAG2,LEFDTSUP     START WITH ALL SUPPRESSED                    
*                                                                               
GETS9C   CR    R9,RF                                                            
         BNL   GETS9E                                                           
         CLI   0(R9),0             TEST SPOTS THIS WEEK                         
         BE    GETS9D                                                           
         CLC   APFULL(2),2(RE)     YES-IF EFF DATE 1 RANGE,                     
         BNH   *+12                                                             
         NI    LFLAG2,255-LEF1SUPP     DON'T SUPPRES EFF DATE 1 LINE            
         B     GETS9D                                                           
         OC    BWDEFDT3,BWDEFDT3   IF EFF DATE 2 RANGE,                         
         BZ    *+14                                                             
         CLC   APFULL+2(2),2(RE)                                                
         BNH   *+12                                                             
         NI    LFLAG2,255-LEF2SUPP     DON'T SUPPRES EFF DATE 2 LINE            
         B     GETS9D                                                           
         NI    LFLAG2,255-LEF3SUPP     DON'T SUPPRES EFF DATE 3 LINE            
*                                                                               
GETS9D   LA    R9,1(R9)                                                         
         LA    RE,4(RE)                                                         
         B     GETS9C                                                           
*                                                                               
GETS9E   NI    LFLAG,FF-LPKGMAS-LPKGSLV-LORBMAS                                 
         TM    BWDINDS,BWDIORB+BWDIPKG  TEST PACKAGE OR ORBIT                   
         BNZ   *+14                                                             
         CLI   BWDKELPO,0          NO-CHECK THAT PKG/ORB SEQ NUM EQ 0           
         BE    GETS11                                                           
         DC    H'0'                                                             
         CLI   BWDKELPO,0          YES-IF PKG/ORB SEQ NUM EQ 0,                 
         BE    GETS8                   THIS IS A CRAP RECORD - IGNORE           
         CLI   BWDKELSQ,0          TEST SLAVE                                   
         BE    GETS10                                                           
         TM    BWDINDS,BWDIORB     YES-TEST ORBIT SLAVE                         
         BO    GETS8               YES-NEXT RECORD                              
         OI    LFLAG,LPKGSLV       ELSE, INDICATE PACKAGE SLAVE                 
         B     GETS13              MAYBE NEW SPOT LENGTH                        
*                                                                               
GETS10   TM    BWDINDS,BWDIPKG     TEST PACKAGE MASTER                          
         BZ    *+12                                                             
         OI    LFLAG,LPKGMAS       YES-INDICATE PACKAGE MASTER                  
         B     GETS11                                                           
         OI    LFLAG,LORBMAS       ELSE INDICATE ORBIT MASTER                   
*                                                                               
GETS11   TM    LFLAG2,LDPTMAS      TEST MASTER AND SUB DAYPARTS                 
         BO    *+12                                                             
         CLI   BDPT,0              OR NO DAYPART FILTER                         
         BNE   GETS13                                                           
         LA    R1,COMDPLST         YES-ADD THIS DAYPART TO LIST                 
         LA    R0,L'COMDPLST                                                    
*                                                                               
GETS12   CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   0(1,R1),BWDDPT                                                   
         B     GETS13                                                           
         CLC   BWDDPT,0(R1)                                                     
         BE    GETS13                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,GETS12                                                        
         DC    H'0'                                                             
*                                                                               
GETS13   CLI   BSLN,0              TEST SPOT LENGTH FILTER                      
         BNE   GETS15                                                           
         LA    R1,COMSLLST         NO-ADD THIS LENGTH TO LIST                   
         LA    R0,L'COMSLLST                                                    
*                                                                               
GETS14   CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   0(1,R1),BWDSLN                                                   
         B     GETS15                                                           
         CLC   BWDSLN,0(R1)                                                     
         BE    GETS15                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,GETS14                                                        
         DC    H'0'                                                             
*                                                                               
GETS15   TM    LFLAG,LPKGSLV       TEST PACKAGE SLAVE                           
         BO    GETS16              YES-GET THE RATING                           
         TM    LFLAG2,LCAMP2       NO TSAR RECORDS FOR COMPANION CAMPN          
         BO    GETS16                                                           
         LA    R4,TSARREC          BUILD TSAR RECORD                            
         USING TRECD,R4                                                         
         XC    TSARREC(TUPDRECL),TSARREC                                        
         MVC   TRECL,=Y(TUPDRECL)                                               
         MVI   TRECTYP,TRECUPD                                                  
*                                  SET PARTS OF TSAR RECORD COMMON              
*                                  TO ALL SORT SEQUENCES.                       
         MVI   TUCAMP,1            SET CAMPAIGN SEQ NUM = 1                     
         TM    TWAINDS,TWAICCSW    OR 2 FOR COMPANION CAMPAIGN                  
         BZ    *+8                                                              
         MVI   TUCAMP,2                                                         
         MVC   TUSEQ,BWDKELSQ      RECORD SEQUENCE NUMBER                       
         TM    BWDINDS,BWDIPKG     PACKAGE/ORBIT INDICATORS                     
         BZ    *+8                                                              
         OI    TUINDS,TUIPKG                                                    
         TM    BWDINDS,BWDIORB                                                  
         BZ    *+8                                                              
         OI    TUINDS,TUIORB                                                    
         MVC   TUELKEY,BWDKELST    ELEM KEY STA/PKG-ORB/DAYS/TIMES/SEQ          
*                                                                               
*                                  SET TSAR RECORD KEY                          
         CLI   INORNK,INORNKT                                                   
         BE    GETS15D                                                          
         CLI   INORNK,INORNKP                                                   
         BNE   GETS15A                                                          
         MVC   TUDPT3,BWDDPT       CPP/DPTLEN SEQ                               
         MVC   TUSLN3,BWDSLN                                                    
         B     GETS15B                                                          
*                                                                               
GETS15A  MVC   TUDPT,BWDDPT                                                     
         MVC   TUSLN,BWDSLN                                                     
*                                                                               
GETS15B  MVI   TUSTA,0                                                          
         MVC   TUSTA+1(5),BWDSTA                                                
         CLI   BWDSTA,C'0'         TEST CABLE                                   
         BL    GETS15C                                                          
         PACK  APDUB,BWDSTA(4)     YES-BINARY STATION (2)                       
         CVB   RE,APDUB                AND CHARACTER NETWORK (3)                
         STCM  RE,3,TUSTA+1                                                     
         MVC   TUSTA+3(3),BWDSTA+5                                              
         MVI   TUSTA,1             SEQUENCE AFTER NON-CABLE                     
*                                                                               
GETS15C  MVC   TUDAYS,BWDDAYS                                                   
         MVC   TUTIMCOD,BWDKELTM                                                
         MVI   TUDAYCOD,X'FF'      SET SPECIAL DAY CODE FOR SORTING             
         CLI   BWDDAYS,0           TEST PACKAGE/ORBIT MASTER                    
         BNE   *+14                                                             
         MVC   TUSEQ,BWDKELPO      YES-SORT IN PGK/ORB SEQ NUM ORDER            
         BE    GETS16              AFTER ALL OTHER LINES                        
         MVI   TUDAYCOD,0                                                       
         CLI   BWDDAYS,X'7C'       M-F                                          
         BE    GETS16                                                           
         MVI   TUDAYCOD,1                                                       
         CLI   BWDDAYS,X'7F'       M-SU                                         
         BE    GETS16                                                           
         SR    RF,RF                                                            
         ZIC   R0,BWDDAYS                                                       
         SRDL  R0,9                                                             
         SLDL  R0,1                                                             
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         BCT   RF,*-10                                                          
         LPR   RF,RF                                                            
         STC   RF,TUDAYCOD                                                      
         B     GETS16                                                           
*                                                                               
GETS15D  BAL   RE,DTSEQ            DAY/TIME/STATION/DPTLEN SEQ                  
*                                                                               
GETS16   XC    LRATING,LRATING     GET THE RATING                               
         XC    APFULL,APFULL                                                    
         LA    R1,BWDEL                                                         
         SR    R0,R0                                                            
*                                                                               
GETS17   CLI   0(R1),0                                                          
         BE    GETS20                                                           
         CLI   0(R1),DMOELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GETS17                                                           
         MVC   APWORK(2),INORTG+1                                               
         OC    INORTG,INORTG       TEST FOR OVERRIDE TARGET DEMO                
         BNZ   GETS18              YES-FIND OVERRIDE DEMO VALUE                 
********                                                                        
         B     *+12                1/24/91 - FIND PRIMARY DEMO ANYWAY           
********                                                                        
         TM    LFLAG,LPKGMAS+LORBMAS   NO-TEST PKG/ORBIT MASTER                 
         BZ    *+14                                                             
         MVC   APWORK(2),ESTDEMS+1     YES-LOOK FOR PRIMARY DEMO                
         B     GETS18                                                           
         LA    R1,DMODEMO-DMOEL(R1)  NO-EXTRACT RATING                          
         MVC   LRATING,5(R1)                                                    
         ST    R1,APFULL                                                        
         B     GETS20                                                           
*                                                                               
GETS18   MVC   APWORK+2(2),APWORK                                               
         OC    INORTG,INORTG                                                    
         BZ    *+10                                                             
         MVC   APWORK+2(1),ESTDEMS+1                                            
         LA    RE,L'DMODEMO                                                     
         ZIC   RF,1(R1)                                                         
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    R1,DMODEMO-DMOEL(R1)                                             
*                                                                               
GETS19   CLC   1(2,R1),APWORK                                                   
         BNE   *+8                                                              
         ST    R1,APFULL                                                        
         CLC   1(2,R1),APWORK+2                                                 
         BNE   *+10                                                             
         MVC   LRATING,5(R1)                                                    
         BXLE  R1,RE,GETS19                                                     
*                                                                               
GETS20   TM    LFLAG,LPKGSLV       TEST PACKAGE SLAVE                           
         BO    GETS29              YES-SKIP TO GET POINTS/DOLLARS               
         TM    LFLAG2,LCAMP2       TEST COMPANION CAMPAIGN                      
         BO    GETS29              YES-SKIP TO GET POINTS/DOLLARS               
         LA    R9,BWDCOST1         R9=A(COST)                                   
         TM    LFLAG2,LEF1SUPP     TEST SUPPRESS EFFECTIVE DATE 1               
         BO    GETS27              YES                                          
*                                                                               
GETS22   CLI   INORNK,INORNKS      SORT IN DPTLEN/STA/DAY/TIME SEQ              
         BE    GETS26                                                           
         CLI   INORNK,INORNKT      SORT IN DAY/TIME/STA/DPTLEN SEQ              
         BE    GETS26                                                           
         SR    RE,RE               SORT IN CPP OR DEMO SEQ                      
         ICM   R1,15,APFULL                                                     
         BZ    *+8                                                              
         ICM   RE,7,5(R1)          RE=RATING                                    
         CLI   INORNK,INORNKD      TEST RANK IN DEMO VALUE SEQ                  
         BE    GETS24              YES-USE INVERSE RATING                       
         LTR   RE,RE               NO-RANK IN CPP SEQ                           
         BZ    GETS26              ZERO RATING-CPP=ZERO                         
         XC    TUCPP,TUCPP                                                      
         XC    TUDEMO,TUDEMO                                                    
         ICM   R1,15,0(R9)         R1=COST                                      
         BNZ   *+16                TEST ZERO COST                               
         CLI   CLTBWPRO+10,C'Y'    YES-TEST RANK BONUSES FIRST                  
         BE    GETS26                  YES-                                     
         B     GETS24                  NO-RANK IN DEMO SEQUENCE                 
         SR    R0,R0               CALCULATE CPP                                
         M     R0,=F'20'                                                        
         DR    R0,RE                                                            
         LA    R1,1(R1)                                                         
         SRL   R1,1                                                             
         STCM  R1,7,TUCPP          SAVE CPP IN RECORD TABLE                     
         B     GETS26                                                           
*                                                                               
GETS24   LNR   RE,RE               INVERSE RATING                               
         BCTR  RE,0                MINUS ONE FOR ZERO                           
         STCM  RE,7,TUDEMO         SAVE IN RECORD TABLE                         
*                                                                               
GETS26   MVI   TSARACT,TSAADD       ADD TSAR RECORD                             
         GOTO1 ATSAR,TREC                                                       
         BNE   GETS99                                                           
         LA    R8,1(R8)            INCREMENT RECORD COUNT                       
*                                                                               
GETS27   OC    BWDEFDT2,BWDEFDT2   TEST EFFECTIVE DATES                         
         BZ    GETS29              NO                                           
         TM    TUINDS,TUIEFDT3     YES-TEST PROCESSED EFFECTIVE COST 3          
         BO    GETS29              YES-DONE WITH THIS RECORD                    
         TM    TUINDS,TUIEFDT2     TEST JUST PROCESSED EFFECTIVE COST 2         
         BO    GETS28                                                           
         OI    TUINDS,TUIEFDT2     NO-PROCESS EFFECTIVE COST 2                  
         TM    LFLAG2,LEF2SUPP     EXCEPT IF IT'S SUPPRESSED                    
         BO    GETS28                                                           
         LA    R9,BWDCOST2                                                      
         B     GETS22                                                           
*                                                                               
GETS28   OC    BWDEFDT3,BWDEFDT3   YES-TEST EFFECTIVE DATE 3                    
         BZ    GETS29                                                           
         NI    TUINDS,FF-TUIEFDT2  YES-PROCESS EFFETCIVE COST 3                 
         TM    LFLAG2,LEF3SUPP     EXCEPT IF IT'S SUPPRESSED                    
         BO    GETS29                                                           
         OI    TUINDS,TUIEFDT3                                                  
         LA    R9,BWDCOST3                                                      
         B     GETS22                                                           
*                                                                               
GETS29   TM    LFLAG,LPKGMAS       TEST PACKAGE MASTER                          
         BO    *+8                                                              
         BAL   RE,ACPTSDOL         NO-ACCUMULATE POINTS/DOLLARS                 
         B     GETS8               GET NEXT RECORD                              
*                                                                               
GETS30   LTR   R8,R8               TEST ANY RECORDS                             
         BZ    GETS90              NO-TELL USER                                 
*                                                                               
         CLI   APACTN,ACTSKD       TEST SCHEDULE ACTION                         
         BE    *+12                                                             
         CLI   APACTN,ACTSSK                                                    
         BNE   GETS31                                                           
         OC    CMPCCAM,CMPCCAM     AND THERE'S A COMPANION CAMPAIGN             
         BZ    GETS31                                                           
         TM    LFLAG2,LCAMP2       AND COMPANION NOT PROCESSED YET              
         BO    GETS30D                                                          
         OI    LFLAG2,LCAMP2       YES-GET RECORDS FOR COMPANION CPMN           
         MVC   SVBCAM,BCAM                                                      
         MVC   SVBVALS(BVALSX-BCLT),BCLT                                        
         MVC   SVQVALS(QVALSX-QCLT),QCLT                                        
         MVC   APHALF,CMPCCAM                                                   
         XC    APHALF,XFF                                                       
         GOTO1 AGETCAM,APHALF      GET COMPANION                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BCLT(BVALSX-BCLT),SVBVALS                                        
         MVC   QCLT(QVALSX-QCLT),SVQVALS                                        
*                                                                               
         XC    IOKEY,IOKEY         READ CPMN/MKT HEADER                         
         MVC   IOKEY(BWHKSEQ-BWHKEY),HDRKEY                                     
         LA    R2,IOKEY                                                         
         MVC   BWHKCAM,APHALF                                                   
         GOTO1 AIO,DIRHI+IO2                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                   
         BNE   GETS30D             NO RECORDS FOR MARKET                        
         MVC   APHALF,BWHKSEQ      EXTRACT CAMP/MKT SEQ NUM                     
         SR    R9,R9                                                            
         OC    QSTA,QSTA           TEST STATION FILTER                          
         BZ    GETS30C                                                          
         GOTO1 AIO,FILGET2         YES-GET HEADER RECORD                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA2         AND FIND STATION CODE                        
         LA    R1,BWHFSTEL                                                      
         SR    R0,R0                                                            
*                                                                               
GETS30B  CLI   0(R1),0                                                          
         BE    GETS30D             NO RECORDS FOR STATION                       
         CLI   0(R1),BWHELCDQ                                                   
         BNE   *+18                                                             
         USING BWHEL,R1                                                         
         IC    R9,BWHSEQ                                                        
         CLC   BWHSTA,QSTA                                                      
         BE    GETS30C                                                          
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GETS30B                                                          
*                                                                               
GETS30C  XC    IOKEY,IOKEY         BUILD DETAIL KEY                             
         MVC   IOKEY(BWDKSEQ-BWDKEY),APRECKEY                                   
         LA    R3,IOKEY                                                         
         MVC   BWDKSEQ,APHALF                                                   
         MVI   BWDKELCD,BWDELCDQ                                                
         STC   R9,BWDKELST                                                      
         B     GETS7A              GO BACK AND READ RECORDS                     
*                                                                               
GETS30D  BAL   RE,GOALS            GET GOALS FOR COMPANION                      
         BNE   GETS80                                                           
         XC    BCLT(G1WPROF-BCLT),BCLT   TO GET PROFILES... RESTORED            
         GOTO1 AGETCAM,SVBCAM      GET BACK ORIGINAL CAMPAIGN                   
         BE    *+6                 AND CONTINUE                                 
         DC    H'0'                                                             
         MVC   BCLT(BVALSX-BCLT),SVBVALS                                        
         MVC   QCLT(QVALSX-QCLT),SVQVALS                                        
         NI    LFLAG2,255-LCAMP2                                                
*                                                                               
GETS31   TM    TWAINDS,TWAICCSW    SAVE N'RECORDS                               
         BZ    *+8                                                              
         AH    R8,SVTUMAX                                                       
         STH   R8,LTUMAX                                                        
         CLI   APACTN,ACTSKD       FOR SCHEDULE ACTIONS --                      
         BNE   *+16                                                             
         LA    R8,WRKL8H+GLDSPL    FORMAT GOAL DOLLARS/POINTS                   
         LA    R9,WRKL8H+CMTDSPL   DISPLAY COMMENT LINE IF POINTS ARE           
         B     GETS32              ROUNDED                                      
         CLI   APACTN,ACTSSK                                                    
         BNE   GETS36                                                           
         LA    R8,SSKGOLH                                                       
         LA    R9,SSKCMTH                                                       
*                                                                               
GETS32   BAL   RE,GOALS            GET THE GOALS                                
         BNE   GETS80                                                           
         GOTO1 AFMTGOAL,APPARM,(R8),(R9),SVGVA   AND FORMAT                     
         LA    R8,WRKL8H+ACDSPL    FORMAT ACTUAL POINTS AND DOLLARS             
         CLI   APACTN,ACTSSK                                                    
         BNE   *+8                                                              
         LA    R8,SSKACTH                                                       
         GOTO1 AFMACPTS,APPARM,(R8),SVGVA                                       
         GOTO1 AFMACDOL,APPARM,(R8),SVGVA                                       
         B     GETS36                                                           
*                                                                               
*                                                                               
GETS33   LH    RF,LTUNUM           NOT FIRST LINE - GET NEXT RECORD             
         LA    RF,1(RF)                                                         
         STH   RF,LTUNUM                                                        
         CLI   LLINE,NSKDLINS      TEST LINE NUMBER REACHED LIMIT               
         BL    GETS34                                                           
         MVI   LLINE,1             YES - RESET LINE NUMBER                      
         ZIC   RE,LPAGDSP                                                       
         LA    RF,1(RE)                  AUGMENT PAGE LETTER                    
         LA    RE,ALPHATAB(RF)                                                  
         CLI   0(RE),FF                                                         
         BE    *+14                                                             
         STC   RF,LPAGDSP                                                       
         MVC   LPAGE,0(RE)                                                      
         B     GETS36                                                           
*                                                                               
GETS34   ZIC   RE,LLINE            NOT FIRST LINE -                             
         LA    RE,1(RE)            AUGMENT LINE NUMBER                          
         STC   RE,LLINE                                                         
*                                                                               
GETS36   CLC   LTUNUM,LTUMAX       TEST REACHED END OF RECORDS                  
         BH    GETS90                                                           
         LA    R4,TSARREC          NO-GET TSAR RECORD                           
         USING TRECD,R4                                                         
         MVC   TSARNUM,LTUNUM                                                   
         MVI   TSARACT,TSAGET                                                   
         GOTO1 ATSAR,TREC                                                       
         BNE   GETSX                                                            
         CLI   TRECTYP,TRECUPD                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,APRECKEY         SET VALUES FOR ROOT                          
         MVI   BWDKEL,BWDELCDQ                                                  
         MVC   BWDKEL+1(6),TUELKEY                                              
         XC    APRECID,APRECID                                                  
         CLI   CUDMED,C'C'         CANADIAN INDICATOR                           
         BNE   *+8                                                              
         OI    APRECID,RICANAD                                                  
         OC    CMPCCAM,CMPCCAM     TEST COMPANION CAMPAIGN                      
         BZ    *+8                                                              
         OI    APRECID,RICCAMP     YES-SET INDICATOR                            
         TM    TUINDS,TUIEFDT2     SET EFFECTIVE DATE INDICATORS                
         BZ    *+8                                                              
         OI    APRECID,RIEFFDT2                                                 
         TM    TUINDS,TUIEFDT3                                                  
         BZ    *+8                                                              
         OI    APRECID,RIEFFDT3                                                 
         MVI   APRECNUM,0                                                       
         TM    TUINDS,TUIPKG       TEST PACKAGE/ORBIT RECORD                    
         BZ    *+8                                                              
         MVI   APRECNUM,RECPKG     YES-SET APRECNUM                             
         TM    TUINDS,TUIORB                                                    
         BZ    *+8                                                              
         MVI   APRECNUM,RECORB                                                  
*                                                                               
         MVC   IOKEY(13),APRECKEY   READ THE RECORD                             
         GOTO1 AMIN,MINHI2                                                      
         BNE   EPF4                                                             
         CLC   IOKEY(13),IOKEYSAV                                               
         BNE   EPF4                                                             
         L     R3,AIOAREA2         SET MORE VALUES FOR ROOT                     
         MVC   APRECDA,BWDSTA      APRECDA=STATION                              
         CLI   BWDSTA,C'0'         TEST CABLE                                   
         BL    *+10                                                             
         MVC   APRECKEY+40(3),BWDSTA+5  YES-STORE CABLE NET IN KEY+40           
         MVC   APRECID+1(1),BWDDPT APRECID+1(2)=DAYPART/LENGTH                  
         MVC   APRECID+2(1),BWDSLN                                              
*                                                                               
         CLI   APACTN,ACTSKD       TEST FOR SCHEDULE OR SUPER-SKED              
         BE    *+12                                                             
         CLI   APACTN,ACTSSK                                                    
         BNE   GETS80              NO-EXIT                                      
*                                                                               
         LA    RE,NMAXWKS          YES-BUILD SCHEDULE LINE IN APRECKEY          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    APRECKEY+20(0),APRECKEY+20   CLEAR SCHEDULE LINE                 
         LA    R8,BWDEL            FIND SPOTS PER WEEK ELEMENT                  
         SR    R0,R0                                                            
*                                                                               
GETS38   CLI   0(R8),0                                                          
         BE    GETS42                                                           
         CLI   0(R8),SPWELCDQ                                                   
         BE    GETS40                                                           
         IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     GETS38                                                           
*                                                                               
         USING SPWEL,R8                                                         
GETS40   ZIC   RE,SPWELLN                                                       
         BCTR  RE,0                                                             
         SH    RE,=Y(SPWPERWK-SPWEL)                                            
         EX    RE,*+8              MOVE SPOTS PER WEEK LINE TO KEY              
         B     *+10                                                             
         MVC   APRECKEY+20(0),SPWPERWK                                          
         DROP  R8                                                               
*                                                                               
GETS42   TM    BWDINDS,BWDIPKG     TEST PACKAGE MASTER                          
         BZ    GETS44                                                           
         ZIC   RE,CMPNWKS          YES - MARK ALL WEEKS INVALID                 
         LA    RF,APRECKEY+20                                                   
         OI    0(RF),X'40'                                                      
         LA    RF,1(RF)                                                         
         BCT   RE,*-8                                                           
         B     GETS80                                                           
*                                                                               
******** TM    CMPOPTS,CAMODLY     TEST DAILY SKED                              
******** BO    GETS50                                                           
GETS44   TM    INOIND,INOINOG      NO-TEST NOGOALS OPTION                       
         BO    GETS50                                                           
         TM    CLTIND,CLTIGOL      NO-TEST GOALS REQUIRED FOR BUY               
         BZ    GETS50                                                           
         CLC   TWADPT,BWDDPT       YES-MARK AS INVALID THOSE WEEKS              
         BNE   *+14                    WITHOUT GOALS                            
         CLC   TWASLN,BWDSLN                                                    
         BE    GETS46                                                           
         MVC   APHALF,BDPT                                                      
         MVC   BDPT,BWDDPT                                                      
         CLI   BWDSUBDP,0                                                       
         BE    *+10                                                             
         MVC   BDPT,BWDSUBDP                                                    
         XC    DPTSUBS,DPTSUBS                                                  
         MVC   BSLN,BWDSLN                                                      
         GOTO1 AGETGOAL                                                         
         MVC   BDPT(2),APHALF                                                   
         CLI   BDPT,0                                                           
         BE    GETS45                                                           
         GOTO1 AGETDPT,APHALF                                                   
         BE    GETS45                                                           
         DC    H'0'                                                             
*                                                                               
GETS45   L     RE,AIOAREA3                                                      
         MVC   SVGLPTS(56),64(RE)                                               
*&&DO                                                                           
GETS46   ZIC   R0,CMPNWKS                                                       
         LA    RE,SVGLPTS                                                       
         LA    RF,APRECKEY+20                                                   
*                                                                               
GETS48   OC    0(4,RE),0(RE)                                                    
         BNZ   *+8                                                              
         OI    0(RF),X'20'       <======  NO GOALS                              
         LA    RE,4(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,GETS48                                                        
*&&                                                                             
GETS46   DS    0H                                                               
GETS50   OC    BWDEFDT2,BWDEFDT2   TEST EFFECTIVE DATES                         
         BZ    GETS62                                                           
         GOTO1 VDATCON,APPARM,(3,BWDEFDT2),(2,APDUB)                            
         OC    BWDEFDT3,BWDEFDT3                                                
         BZ    GETS52                                                           
         GOTO1 VDATCON,APPARM,(3,BWDEFDT3),(2,APDUB+2)                          
*                                                                               
GETS52   LA    RE,CMPDATSP         MARK WEEKS THAT ARE OUT OF DATE              
         LA    RF,APRECKEY+20      BOUNDS                                       
         LA    R0,NMAXWKS                                                       
*                                                                               
GETS54   TM    TUINDS,TUIEFDT2+TUIEFDT3  TEST EFFECTIVE COST 1                  
         BNZ   GETS56                                                           
         CLC   APDUB(2),2(RE)                                                   
         BNH   GETS60                                                           
         B     GETS61                                                           
*                                                                               
GETS56   TM    TUINDS,TUIEFDT2                                                  
         BZ    GETS58                                                           
         CLC   APDUB(2),2(RE)                                                   
         BH    GETS60                                                           
         OC    BWDEFDT3,BWDEFDT3                                                
         BZ    GETS61                                                           
         CLC   APDUB+2(2),2(RE)                                                 
         BNH   GETS60                                                           
         B     GETS61                                                           
*                                                                               
GETS58   CLC   APDUB+2(2),2(RE)                                                 
         BNH   GETS61                                                           
*                                                                               
GETS60   OI    0(RF),X'80'         X'80' = THIS WEEK NOT ALLOWED                
*                                                                               
GETS61   LA    RE,4(RE)            NEXT WEEK                                    
         LA    RF,1(RF)                                                         
         BCT   R0,GETS54                                                        
*                                                                               
GETS62   TM    BWDINDS,BWDIORB     TEST ORBIT                                   
         BO    GETS80              YES - SKIP DAY CHECK                         
         TM    CMPOPTS,CAMODLY     TEST DAILY SKED                              
         BZ    GETS65                                                           
         LA    R8,CMPDATSD         YES-MARK INVALID DAYS                        
         LA    R9,APRECKEY+20                                                   
         LA    R0,NMAXWKS                                                       
*                                                                               
GETS63   CLI   0(R8),X'FF'                                                      
         BE    GETS64                                                           
         GOTO1 VGETDAY,APPARM,(R8),APFULL                                       
         CLC   APFULL,SPACES                                                    
         BH    *+6                                                              
         DC    H'0'                                                             
         LA    R8,6(R8)                                                         
         ZIC   RF,0(R1)                                                         
         LA    RE,X'80'                                                         
         SRL   RE,1                                                             
         BCT   RF,*-4                                                           
         STC   RE,APBYTE                                                        
         NC    APBYTE,BWDDAYS                                                   
         BNZ   GETS64+4                                                         
*                                                                               
GETS64   OI    0(R9),X'80'         DAY IS INVALID                               
         LA    R9,1(R9)                                                         
         BCT   R0,GETS63                                                        
         B     GETS80                                                           
*                                                                               
GETS65   GOTO1 VGETDAY,APPARM,CMPDATSD,APWORK GET DAY OF WEEK OF FIRST          
         CLC   APWORK(3),SPACES               DAY OF CAMPAIGN                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APBYTE,0(R1)                                                     
         SR    R8,R8                                                            
         ICM   R8,1,ESTOWSDY       TEST OUT-OF-WEEK ROTATORS                    
         BZ    GETS66                                                           
         BCTR  R8,0                YES-GET RELATIVE DAY                         
         ZIC   RE,0(R1)                                                         
         SR    RE,R8                                                            
         BP    *+8                                                              
         LA    RE,7(RE)                                                         
         STC   RE,APBYTE                                                        
*                                                                               
GETS66   ZIC   RE,BWDDAYS          GET DAY OF WEEK OF LAST DAY OF DAYS          
         SR    RF,RF                                                            
         LA    R1,7                                                             
         LTR   R8,R8               TEST OUT-OF-WEEK ROTATORS                    
         BZ    *+6                                                              
         SR    R1,R8                                                            
         SRDL  RE,1                                                             
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         BCT   R1,*-10                                                          
         LTR   R1,R1                                                            
         BP    *+8                                                              
         LA    R1,7(R1)                                                         
         CLM   R1,1,APBYTE         TEST LAST DAY BEFORE CAMPAIGN START          
         BNL   *+8                                                              
         OI    APRECKEY+20,X'40'   YES - MARK FIRST WEEK AS INVALID             
*                                                                               
         MVC   APWORK(6),CMPFLND   GET DAY OF WEEK OF LAST DAY OF               
         TM    CMPOPTS,CAMOWKS     CAMPAIGN                                     
         BO    GETS68                                                           
         GOTO1 VDATCON,APPARM,(3,CMPND),(0,APWORK)                              
*                                                                               
GETS68   GOTO1 VGETDAY,APPARM,APWORK,APWORK+6                                   
         CLC   APWORK+6(3),SPACES                                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APBYTE,0(R1)                                                     
         LTR   R8,R8               TEST OUT-OF-WEEK ROTATORS                    
         BZ    GETS70                                                           
         ZIC   RE,0(R1)                                                         
         SR    RE,R8                                                            
         BP    *+8                                                              
         LA    RE,7(RE)                                                         
         STC   RE,APBYTE                                                        
*                                                                               
GETS70   SR    RE,RE               GET DAY OF WEEK OF FIRST DAY OF DAYS         
         ZIC   RF,BWDDAYS                                                       
         SLL   RF,25                                                            
         SR    R1,R1                                                            
         LTR   R8,R8               TEST OUT-OF-WEEK ROTATORS                    
         BZ    *+10                                                             
         SR    R1,R8                                                            
         LA    R1,7(R1)                                                         
         LA    R1,1(R1)                                                         
         SLDL  RE,1                                                             
         LTR   RE,RE                                                            
         BZ    *-10                                                             
         CH    R1,=H'7'                                                         
         BNH   *+8                                                              
         SH    R1,=H'7'                                                         
         CLM   R1,1,APBYTE         TEST FIRST DAY AFTER CAMPAIGN END            
         BNH   GETS72                                                           
         ZIC   RF,CMPNWKS          YES - MARK LAST WEEK AS INVALID              
         LA    RF,APRECKEY+20(RF)                                               
         BCTR  RF,0                                                             
         OI    0(RF),X'40'                                                      
*                                                                               
GETS72   STC   R1,APHALF           APHALF(1) = FIRST DAY OF DAYS                
         GOTO1 VGETDAY,APPARM,ESTST,APWORK    GET DAY OF WEEK OF FIRST          
         CLC   APWORK(3),SPACES               DAY OF ESTIMATE                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APHALF+1(1),0(R1)              = APHALF+1(1)                     
         LTR   R8,R8               TEST OUT-OF-WEEK ROTATORS                    
         BZ    GETS74                                                           
         ZIC   RE,0(R1)                                                         
         SR    RE,R8                                                            
         BP    *+8                                                              
         LA    RE,7(RE)                                                         
         STC   RE,APHALF+1                                                      
*                                                                               
GETS74   MVC   APWORK(6),ESTST                                                  
         CLI   0(R1),1             GET MONDAY OF EST START WEEK                 
         BE    GETS76                                                           
         ZIC   RE,0(R1)                                                         
         BCTR  RE,0                                                             
         LNR   RE,RE                                                            
         ST    RE,APPARM+8                                                      
         GOTO1 VADDAY,APPARM,ESTST,APWORK                                       
*                                                                               
GETS76   MVC   APWORK+6(6),CMPSTMON    CAMPAIGN START MONDAY                    
         TM    CMPOPTS,CAMOWKS         TEST NON-CONTIGUOUS FLIGHT WEEKS         
         BZ    *+10                                                             
         MVC   APWORK+6(6),CMPFLSTM    YES-USE FLIGHT START MONDAY              
         CLC   APWORK+6(6),APWORK  TEST CAMPAIGN START WK=EST START WK          
         BNE   GETS80                                                           
         CLC   APHALF(1),APHALF+1  TEST FIRST DAY BEFORE EST START              
         BNL   GETS80                                                           
         OI    APRECKEY+20,X'40'   YES - FIRST WEEK IS INVALID                  
*                                                                               
GETS80   TM    TWAINDS,TWAICCSW    SAVE TSAR NUMBERS                            
         BO    GETS82              AND PAGE/LINE NUMBERS                        
         MVC   SVTUNUM,LTUNUM                                                   
         MVC   SVTUMAX,LTUMAX                                                   
         MVC   SVPAGE,LPAGE                                                     
         MVC   SVPAGDSP,LPAGDSP                                                 
         MVC   SVLINE,LLINE                                                     
         B     GETSX                                                            
*                                                                               
GETS82   MVC   SVTUNUM2,LTUNUM                                                  
         MVC   SVTUMAX2,LTUMAX                                                  
         MVC   SVPAGE2,LPAGE                                                    
         MVC   SVPAGDS2,LPAGDSP                                                 
         MVC   SVLINE2,LLINE                                                    
         B     GETSX                                                            
*                                                                               
GETS90   MVI   APMODE,APMEOFS      END OF LIST                                  
         B     GETSX                                                            
*                                                                               
GETS99   MVC   FVMSGNO,=AL2(FVEREC)   RECORD ERROR                              
*                                                                               
GETSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SET TSAR KEY FOR PURE DAY/TIME SEQUENCE                             *         
***********************************************************************         
         SPACE 1                                                                
DTSEQ    NTR1  ,                                                                
         MVI   TUDTCAMP,1          SET CAMPAIGN SEQ NUM = 1                     
         TM    TWAINDS,TWAICCSW    OR 2 FOR COMPANION CAMPAIGN                  
         BZ    *+8                                                              
         MVI   TUDTCAMP,2                                                       
         MVC   TUDTSEQ,BWDKELSQ    RECORD SEQUENCE                              
         LA    R9,DTTAB            SET SEQUENCE CODES                           
         CLI   BWDKELPO,0          TEST PACKAGE/ORBIT                           
         BE    DT2                                                              
         MVC   TUDTSEQ,BWDKELPO    YES-SET RECORD SEQ TO ORB/PKG NUM            
         MVI   TUDTSEQ1,5          ORBITS 2ND TO LAST                           
         TM    BWDINDS,BWDIORB                                                  
         BO    DT10                                                             
         MVI   TUDTSEQ1,6          PACKAGES LAST                                
         B     DT10                                                             
*                                                                               
DT2      CLI   0(R9),FF            GET DAY/TIME SEQUENCING CODE                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R9),0                                                          
         BE    *+14                                                             
         CLC   BWDDAYS,0(R9)                                                    
         BNE   DT4                                                              
         CLC   BWDTIMES(2),1(R9)                                                
         BL    DT4                                                              
         CLC   BWDTIMES(2),3(R9)                                                
         BNL   DT4                                                              
         MVC   TUDTSEQ1,5(R9)                                                   
         B     DT6                                                              
*                                                                               
DT4      LA    R9,6(R9)                                                         
         B     DT2                                                              
*                                                                               
DT6      MVI   TUDTSEQ2,1          DAY CODE                                     
         CLI   BWDDAYS,X'7C'       1=MO-FR                                      
         BE    DT8                                                              
         MVI   TUDTSEQ2,7          7=MO-SU                                      
         CLI   BWDDAYS,X'7F'                                                    
         BE    DT8                                                              
         MVI   TUDTSEQ2,8          8=SA-SU                                      
         CLI   BWDDAYS,X'03'                                                    
         BE    DT8                                                              
         SR    R1,R1               2-6=MIXED DAYS                               
         SR    RF,RF               9-15=SINGLE DAY                              
         ZIC   R0,BWDDAYS                                                       
         SRDL  R0,9                                                             
         SLDL  R0,1                                                             
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         BCT   RF,*-10                                                          
         LPR   RF,RF                                                            
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    RF,7(RF)                                                         
         STC   RF,TUDTSEQ2                                                      
*                                                                               
DT8      SR    RE,RE               TIMES                                        
         ICM   RE,3,BWDTIMES                                                    
         SH    RE,=H'600'          6A-MIDNIGHT,MIDNIGHT-6A                      
         BNM   *+8                                                              
         AH    RE,=H'2400'                                                      
         STCM  RE,3,TUDTTIME                                                    
         ICM   RE,3,BWDTIMES+2                                                  
         SH    RE,=H'600'                                                       
         BNM   *+8                                                              
         AH    RE,=H'2400'                                                      
         STCM  RE,3,TUDTTIME+2                                                  
*                                                                               
DT10     MVC   TUDTDAYS,BWDDAYS    DAYS                                         
         MVC   TUDTSTA,BWDSTA      STATION                                      
         MVC   TUDTDPT,BWDDPT      DAYPART                                      
         MVC   TUDTSLN,BWDSLN      LENGTH                                       
*                                                                               
DTX      B     EXIT                                                             
         DROP  R4                                                               
         SPACE 2                                                                
DTTAB    DC    X'02',AL2(0600),AL2(2000),AL1(4)  SA    6A-8P                    
         DC    X'01',AL2(0600),AL2(1900),AL1(4)  SU    6A-7P                    
         DC    X'03',AL2(0600),AL2(2000),AL1(4)  SA-SU 6A-8P                    
         DC    X'01',AL2(1900),AL2(2000),AL1(2)  SU    7P-8P                    
         DC    X'00',AL2(0600),AL2(2000),AL1(1)  ALL   6A-8P                    
         DC    X'00',AL2(2000),AL2(2300),AL1(2)  ALL   8P-11P                   
         DC    X'00',AL2(2300),AL2(2401),AL1(3)  ALL   11P-MIDNIGHT             
         DC    X'00',AL2(0000),AL2(0600),AL1(3)  ALL   MIDNIGHT-6A              
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET GOAL DOLLARS AND POINTS                              *         
***********************************************************************         
         SPACE 1                                                                
GOALS    NTR1  ,                                                                
         MVC   APBYTE,BSLN         SAVE THE SPOT LENGTH                         
         MVC   APFLAG,BDPT         SAVE DAYPART                                 
         MVC   APHALF(1),CMPDPOPT  SAVE CAMPAIGN DAYPART OPTION                 
         XC    APWORK,APWORK                                                    
         TM    LFLAG2,LDPTMAS      TEST MASTER AND SUB DAYPARTS                 
         BO    *+12                                                             
         CLI   BDPT,0              OR NO DAYPART FILTER                         
         BNE   GOAL1               NO-ONLY ONE DAYPART                          
********                                                                        
         B     GOAL3               YES-ALWAYS INCLUDE ALL MASTER AND            
*                                  AND SUB DAYPARTS                             
********                                                                        
         CLI   COMDPLST+1,0        NO-TEST ONLY ONE DAYPART ANYWAY              
         BNE   GOAL2                                                            
         GOTO1 AGETDPT,COMDPLST    YES-GET IT                                   
         BE    GOAL1                                                            
         XC    FVXTRA,FVXTRA       INVALID DAYPART - EXIT NOW                   
         MVC   FVXTRA(1),COMDPLST                                               
         B     GOALX                                                            
*                                                                               
GOAL1    MVC   APWORK(1),BDPT      SINGLE DAYPART                               
         B     GOAL30                                                           
*                                                                               
GOAL2    CLI   CMPDPOPT,C'M'       MULTIPLE DPTS - TEST SUBDPT=MAS              
         BE    *+14                                                             
         MVC   APWORK(L'COMDPLST),COMDPLST NO                                   
         B     GOAL30                                                           
*                                                                               
GOAL3    LA    R0,L'COMDPLST       YES-BUILD LIST OF ALL DAYPARTS               
         LA    R4,COMDPLST             (MASTER AND SUBDPTS) IN APWORK           
         LA    R8,APWORK                                                        
         XC    APELEM,APELEM                                                    
*                                                                               
GOAL4    CLI   0(R4),0                                                          
         BE    GOAL20                                                           
         MVC   APDUB(1),0(R4)                                                   
         GOTO1 AGETDPT,APDUB                                                    
         BE    GOAL5                                                            
         XC    FVXTRA,FVXTRA       INVALID DAYPART - EXIT NOW                   
         MVC   FVXTRA(1),0(R4)                                                  
         B     GOALX                                                            
*                                                                               
GOAL5    CLI   DPTTYPE,C'S'        TEST DPT IS A SUBDPT                         
         BNE   GOAL7                                                            
         MVC   DPTSUBS(1),BDPT     YES-ADD TO LIST IF NOT ALREADY THERE         
         MVI   DPTSUBS+1,0                                                      
         LA    R1,APELEM           ADD MASTER DPT TO MASTER DPT LIST            
*                                                                               
GOAL6    CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   0(1,R1),DPTMAS                                                   
         B     GOAL8                                                            
         CLC   DPTMAS,0(R1)                                                     
         BE    GOAL8                                                            
         LA    R1,1(R1)                                                         
         B     GOAL6                                                            
*                                                                               
GOAL7    MVC   0(1,R8),0(R4)                                                    
         LA    R8,1(R8)                                                         
         CLI   DPTTYPE,C'M'                                                     
         BNE   GOAL18                                                           
*                                                                               
GOAL8    LA    RF,L'DPTSUBS                                                     
         LA    R1,DPTSUBS                                                       
         XC    APFULL,APFULL                                                    
*                                                                               
GOAL10   CLI   0(R1),0                                                          
         BE    GOAL16                                                           
         LA    RE,APWORK                                                        
*                                                                               
GOAL12   CLI   0(RE),0                                                          
         BNE   *+18                                                             
         MVC   0(1,RE),0(R1)                                                    
         ST    RE,APFULL                                                        
         B     GOAL14                                                           
         CLC   0(1,RE),0(R1)                                                    
         BE    GOAL14                                                           
         LA    RE,1(RE)                                                         
         B     GOAL12                                                           
*                                                                               
GOAL14   LA    R1,1(R1)                                                         
         BCT   RF,GOAL10                                                        
*                                                                               
GOAL16   ICM   R1,15,APFULL                                                     
         BZ    GOAL18                                                           
         LR    R8,R1                                                            
         LA    R8,1(R8)                                                         
*                                                                               
GOAL18   LA    R4,1(R4)                                                         
         BCT   R0,GOAL4                                                         
*                                                                               
GOAL20   LA    R1,APELEM           ADD ANY MISSING MASTER DAYPARTS              
*                                  TO THE DAYPART LIST                          
GOAL22   CLI   0(R1),0                                                          
         BE    GOAL28                                                           
         LA    R8,APWORK                                                        
*                                                                               
GOAL24   CLI   0(R8),0                                                          
         BNE   *+14                                                             
         MVC   0(1,R8),0(R1)                                                    
         B     GOAL26                                                           
         CLC   0(1,R8),0(R1)                                                    
         BE    GOAL26                                                           
         LA    R8,1(R8)                                                         
         B     GOAL24                                                           
*                                                                               
GOAL26   LA    R1,1(R1)                                                         
         B     GOAL22                                                           
*                                                                               
GOAL28   MVI   CMPDPOPT,C'S'       FAKE CAMP DPT OPT TO SUBDPT=SEP              
*                                                                               
GOAL30   LA    R4,APWORK           GET GOALS FOR ALL DAYPARTS                   
         CLI   APBYTE,0            TEST SINGLE SPOT LENGTH REQUEST              
         BE    GOAL32                                                           
         MVC   COMSLLST(1),APBYTE  YES                                          
         MVI   COMSLLST+1,0                                                     
*                                                                               
GOAL32   CLI   0(R4),0                                                          
         BE    GOAL40                                                           
         MVC   BDPT,0(R4)                                                       
         LA    R8,COMSLLST                                                      
         LA    R2,L'COMSLLST                                                    
*                                                                               
GOAL34   CLI   0(R8),0                                                          
         BE    GOAL38                                                           
         MVC   BSLN,0(R8)                                                       
         GOTO1 AGETGOAL                                                         
         BAL   RE,ADDGOAL                                                       
         LA    R8,1(R8)            NEXT SPOT LENGTH                             
         BCT   R2,GOAL34                                                        
*                                                                               
GOAL38   LA    R4,1(R4)            NEXT DAYPART                                 
         B     GOAL32                                                           
*                                                                               
GOAL40   MVI   BDPT,C'$'           TRY $GOALS ALSO                              
         MVI   BSLN,1                                                           
         GOTO1 AGETGOAL                                                         
         BAL   RE,ADDGOAL                                                       
*                                                                               
         MVC   BSLN,APBYTE         RESTORE VALUES                               
         MVC   BDPT,APFLAG                                                      
         MVC   CMPDPOPT,APHALF                                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
GOALX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         SPACE 2                                                                
ADDGOAL  LR    R0,RE                                                            
         L     R1,AIOAREA3                                                      
         L     RE,SVGLDOL                                                       
         A     RE,0(R1)                                                         
         ST    RE,SVGLDOL                                                       
         L     RE,SVGLPTST                                                      
         A     RE,4(R1)                                                         
         ST    RE,SVGLPTST                                                      
         TM    LFLAG2,LCAMP2       TEST COMPANION CAMPAIGN                      
         BO    ADDGOALX            YES-SKIP WEEKLY GOALS                        
         LA    R1,64(R1)                                                        
         LA    RF,SVGLPTS                                                       
         LA    R3,14                                                            
*                                                                               
ADDGOAL2 L     RE,0(RF)                                                         
         A     RE,0(R1)                                                         
         ST    RE,0(RF)                                                         
         LA    R1,4(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R3,ADDGOAL2                                                      
*                                                                               
ADDGOALX LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ACCUMULATE POINTS AND DOLLARS                            *         
* INPUT  : IOAREA2 CONTAINS DETAIL RECORD                             *         
*          LRATING=DEMO VALUE                                         *         
* SVACPTS = VECTOR OF TOTAL POINTS BY WEEK                            *         
* SVACDOL = TOTAL DOLLARS                                             *         
***********************************************************************         
         SPACE 1                                                                
ACPTSDOL NTR1                                                                   
         L     R3,AIOAREA2         ADDRESS THE DETAIL RECORD                    
         LA    R4,BWDEL                                                         
         SR    R0,R0               FIND SPOTS PER WEEK ELEMENT                  
*                                                                               
APD2     CLI   0(R4),0                                                          
         BE    APDX                NOT FOUND - EXIT                             
         CLI   0(R4),SPWELCDQ                                                   
         BE    APD4                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     APD2                                                             
*                                                                               
         USING SPWEL,R4                                                         
APD4     ZIC   RE,SPWELLN          FOUND                                        
         AR    RE,R4                                                            
         ST    RE,APFULL           APFULL = A(END OF SPOTS ELEM)                
         SR    R1,R1                                                            
         SR    R2,R2                                                            
         ICM   R2,7,LRATING        TEST RATING=0                                
         BZ    APD10               YES                                          
         LA    R0,NMAXWKS          NO-ACCUMULATE TOTAL RATINGS BY WEEK          
         LA    R8,SPWPERWK                                                      
         LA    R9,SVACPTS                                                       
*                                                                               
APD6     SR    RF,RF                                                            
         ICM   RF,1,0(R8)                                                       
         BZ    APD8                                                             
         MR    RE,R2               SPOTS X RATING                               
         TM    LFLAG2,LCAMP2       TEST COMPANION CAMPAIGN                      
         BZ    *+10                                                             
         AR    R1,RF               YES-ACCUMULATE EXTRA POINTS                  
         B     APD8                                                             
         L     RE,0(R9)                                                         
         AR    RE,RF                                                            
         ST    RE,0(R9)                                                         
*                                                                               
APD8     LA    R8,1(R8)                                                         
         CLM   R8,7,APFULL+1                                                    
         BNL   APD10                                                            
         LA    R9,4(R9)                                                         
         BCT   R0,APD6                                                          
*                                                                               
APD10    TM    LFLAG2,LCAMP2       ADD EXTRA POINTS FOR COMPANION CMPN          
         BZ    *+12                                                             
         A     R1,SVACXPTS                                                      
         ST    R1,SVACXPTS                                                      
*                                  ACCUMULATE TOTAL COST                        
         OC    BWDEFDT2,BWDEFDT2   TEST EFFECTIVE DATES                         
         BZ    APD11                                                            
         GOTO1 VDATCON,APPARM,(3,BWDEFDT2),(2,APDUB)  YES-APDUB(2)=             
         OC    BWDEFDT3,BWDEFDT3                          EFF DATE 2            
         BZ    APD11                                      APDUB+2(2)=           
         GOTO1 VDATCON,APPARM,(3,BWDEFDT3),(2,APDUB+2)    EFF DATE 3            
*                                                                               
APD11    LA    R0,NMAXWKS                                                       
         LA    R8,SPWPERWK                                                      
         LA    R9,CMPDATSP                                                      
         L     R1,SVACDOL          R1=COST ACCUMULATOR                          
*                                                                               
APD12    SR    RF,RF                                                            
         ICM   RF,1,0(R8)                                                       
         BZ    APD15                                                            
         ICM   R2,15,BWDCOST1                                                   
         OC    BWDEFDT2,BWDEFDT2   TEST EFF DATE 2                              
         BZ    APD14                                                            
         CLC   2(2,R9),APDUB       YES-TEST DATE IN THIS WEEK                   
         BL    APD14                                                            
         ICM   R2,15,BWDCOST2      YES-SWITCH TO EFF COST 2                     
         OC    BWDEFDT3,BWDEFDT3   TEST EFF DATE 3                              
         BZ    APD14                                                            
         CLC   2(2,R9),APDUB+2     YES-TEST DATE IN THIS WEEK                   
         BL    APD14                                                            
         ICM   R2,15,BWDCOST3      YES-SWITCH TO EFF COST 3                     
*                                                                               
APD14    LTR   R2,R2                                                            
         BZ    APD15                                                            
         MR    RE,R2               SPOTS X COST                                 
         AR    R1,RF                                                            
*                                                                               
APD15    LA    R8,1(R8)            NEXT WEEK                                    
         CLM   R8,7,APFULL+1                                                    
         BNL   APD16                                                            
         LA    R9,4(R9)                                                         
         BCT   R0,APD12                                                         
*                                                                               
APD16    ST    R1,SVACDOL          SAVE TOTAL COST SO FAR                       
*                                                                               
APDX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT RECORD                                          *         
* INPUT : APPARM+0(4) = A(TWA DISPLAY LINE)                           *         
*         APRECKEY = RECORD KEY                                       *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   L     R8,APPARM           A(TWA LINE)                                  
         ST    R8,COMATWAL                                                      
         CLI   APMODE,APMDISS2     TEST POST RECORD CHANGE DISPLAY MODE         
         BNE   DISS0                                                            
         LA    R3,APRECKEY                                                      
         CLI   BWDKTYP,BWDKTYPQ    YES-ONLY FOR BWS DETAIL RECORD               
         BNE   DISSX                                                            
         CLI   BWDKSUB,BWDKSUBQ                                                 
         BNE   DISSX                                                            
         MVC   IOKEY(13),APRECKEY  READ THE RECORD                              
         GOTO1 AMIN,MINRD2                                                      
         BE    DISS0                                                            
         MVC   FVMSGNO,=AL2(FVFOK)  NOT FOUND-MUST BE FOLLOWING DELETE          
         B     DISSX                                                            
*                                                                               
DISS0    L     R3,AIOAREA2         R3=A(RECORD)                                 
         CLI   APACTN,ACTSSK       TEST ACTION=SUPER-SKED                       
         BNE   DISS2                                                            
         USING SSKL1H,R8                                                        
         MVC   SSKLSP+2(L'SSKLSP-2),SPACES  YES-                                
         MVC   SSKLSP+3(5),BWDSTA  STATION                                      
         CLI   SSKLSP+7,C'T'                                                    
         BNE   *+8                                                              
         MVI   SSKLSP+7,C' '                                                    
         MVC   SSKLSP+9(10),BWDPROG    PROGRAMMING                              
         CLI   BWDSTA,C'0'         TEST CABLE STATION                           
         BL    DISS1                                                            
         MVC   SSKLSP+9(3),BWDSTA+5    YES-FIRST 3 CHARS ARE NETWORK            
         LA    R1,SSKLSP+12                                                     
         LA    RE,5                                                             
         CLI   BWDSTA+7,C' '                                                    
         BH    *+10                                                             
         BCTR  R1,0                                                             
         LA    RE,6                                                             
         MVI   0(R1),C':'                                                       
         EX    RE,*+4                                                           
         MVC   1(0,R1),BWDPROG                                                  
*                                                                               
DISS1    OI    SSKLSPH+6,FVOXMT                                                 
         MVC   SSKSKD,SPACES       INIT SCHEDULE TO SPACES                      
         LA    R4,SSKLSP                                                        
         B     DISS3                                                            
         DROP  R8                                                               
*                                                                               
         USING WRKL1H,R8                                                        
DISS2    GOTO1 ADISPSEL            DISPLAY THE RECORD                           
         BNE   DISSX                                                            
         LA    R4,WRKLST                                                        
         DROP  R8                                                               
*                                                                               
         USING LIST1D,R4                                                        
DISS3    CLI   APMODE,APMDISS2     TEST POST RECORD CHANGE DISPLAY MODE         
         BE    DISS8               YES-SKIP THE PART THAT WON'T CHANGE          
         MVC   LINEDISP(1),LPAGE   LINE NUMBER                                  
         MVC   LINEDISP+1(1),LLINE                                              
         OI    LINEDISP+1,X'F0'                                                 
         MVC   LISTLINE,LINEDISP                                                
         MVI   LIND,C' '                                                        
         TM    BWDINDS,BWDITRLK    TEST FOR TRANSFER LOCK-OUT                   
         BZ    *+12                                                             
         MVI   LIND,C':'           YES-SHOW WITH A COLON                        
         B     DISS7                                                            
         SR    R0,R0               NO-LOOK FOR BUY TRANSFER ELEMENT             
         LA    R1,BWDEL                                                         
         CLI   CLTBWPRO+9,C'Y'     TEST EFFECTIVE COSTS TO SEP LINES            
         BNE   DISS4                                                            
         MVI   APBYTE,0            YES-                                         
         TM    APRECID,RIEFFDT2+RIEFFDT3                                        
         BZ    DISS4                                                            
         MVI   APBYTE,BTRIEC2                                                   
         TM    APRECID,RIEFFDT3                                                 
         BZ    DISS4                                                            
         MVI   APBYTE,BTRIEC3                                                   
*                                                                               
DISS4    CLI   0(R1),0                                                          
         BE    DISS7                                                            
         CLI   0(R1),DTRELCDQ                                                   
         BE    DISS5                                                            
         CLI   0(R1),BTRELCDQ                                                   
         BNE   DISS6                                                            
         CLI   CLTBWPRO+9,C'Y'                                                  
         BNE   DISS5                                                            
         MVC   APFLAG,BTRIND-BTREL(R1)                                          
         NI    APFLAG,BTRIEC2+BTRIEC3                                           
         CLC   APFLAG,APBYTE                                                    
         BNE   DISS6                                                            
*                                                                               
DISS5    MVI   LIND,C'*'           BUY TRANSFER - SHOW WITH A STAR              
         B     DISS7                                                            
*                                                                               
DISS6    IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     DISS4                                                            
*                                                                               
DISS7    MVC   LISTIND,LIND        LINE INDICATOR                               
         DROP  R4                                                               
*                                                                               
DISS8    CLI   APACTN,ACTSSK       TEST SUPER-SKED ACTION                       
         BE    DISS16                                                           
         CLI   APACTN,ACTSKD       OR SCHEDULE ACTION                           
         BNE   DISSX                                                            
*                                                                               
         CLI   APMODE,APMDISS2     TEST POST RECORD CHANE DISPLAY MODE          
         BNE   DISS9                                                            
         L     RF,COMATWAL         YES-GET RELATIVE LINE NUMBER                 
         LA    R1,WRKL1H                                                        
         SR    RF,R1                                                            
         SR    RE,RE                                                            
         L     R1,=A(WRKL2H-WRKL1H)                                             
         DR    RE,R1                                                            
         B     DISS12                                                           
*                                                                               
DISS9    ZIC   RF,LLINE            FORMAT CORRESPONDING SCHEDULE LINE           
         ZIC   RE,LFSTLINE                                                      
         SR    RF,RE                                                            
         BNM   *+10                                                             
         LA    RE,NSKDLINS                                                      
         AR    RF,RE                                                            
         LTR   RF,RF                                                            
         BNZ   DISS12                                                           
         LA    R0,NSKDLINS         FOR FIRST LINE,                              
         LA    R4,WRKL8H+SKEDDSPL  CLEAR ALL THE SCHEDULE LINES                 
         USING SKDLINED,R4                                                      
*                                                                               
DISS10   XC    SKDLSP,SKDLSP                                                    
         OI    SKDLSPH+6,FVOXMT                                                 
         NI    SKDLSPH+FVATRB-FVIHDR,FF-FVAHIGH                                 
         XC    SKDSKD,SKDSKD                                                    
         OI    SKDSKDH+6,FVOXMT                                                 
         OI    SKDSKDH+FVIIND-FVIHDR,FVIVAL   PREVIOUSLY VALIDATED              
         NI    SKDSKDH+FVATRB-FVIHDR,FF-FVAHIGH                                 
         LA    R4,SKDLINEL(R4)                                                  
         BCT   R0,DISS10                                                        
*                                                                               
         LA    R4,WRKL8H+SKEDDSPL                                               
         B     DISS14                                                           
*                                                                               
DISS12   SR    RE,RE                                                            
         LA    R4,WRKL8H+SKEDDSPL                                               
         LA    R1,SKDLINEL                                                      
         MR    RE,R1                                                            
         AR    R4,RF                                                            
*                                                                               
         USING SKDLINED,R4                                                      
DISS14   CLI   APMODE,APMDISS2     TEST POST RECORD CHANGE DISPLAY MODE         
         BE    DISS15              YES-SKIP PART THAT WON'T CHANGE              
         MVC   SKDLIN,LINEDISP     LINE NUMBER                                  
         MVC   SKDIND,LIND         LINE INDICATOR                               
         MVC   SKDSTA,BWDSTA       STATION                                      
         CLI   SKDSTA+4,C'T'                                                    
         BNE   DISS15                                                           
         MVI   SKDSTA+4,C' '                                                    
*                                                                               
DISS15   MVC   SKDPRG,BWDPROG      PROGRAMMING                                  
         MVC   SKDSKD,SPACES       INIT SCHEDULE TO SPACES                      
         LA    RF,SKDSKD+2                                                      
         B     DISS16+4                                                         
*                                                                               
         USING SSKL1H,R8                                                        
DISS16   LA    RF,SSKSKD+2                                                      
         LA    RE,APRECKEY+20      PLACE X IN OFFLIMIT WEEKS AND                
         ZIC   R2,CMPNWKS          PLACE . IN OK WEEKS                          
         SR    R1,R1                                                            
         ICM   R1,12,BWDWKS                                                     
         LA    R9,SVGLPTS          GOAL POINTS                                  
*                                                                               
DISS18   SR    R0,R0                                                            
         SLDL  R0,1                                                             
         MVI   0(RF),C'.'                                                       
*                                                                               
         TM    INOIND,INOINOG      TEST NOGOALS OPTION                          
         BO    DISS18A                                                          
         TM    CLTIND,CLTIGOL      TEST GOALS REQUIRED FOR BUY                  
         BZ    DISS18A                                                          
         OC    0(4,R9),0(R9)                                                    
         BNZ   DISS18A                                                          
         MVI   0(RF),C'*'                                                       
         B     DISS19                                                           
*                                                                               
DISS18A  TM    0(RE),X'C0'                                                      
         BNZ   *+10                                                             
         LTR   R0,R0               TEST INACTIVE WEEK                           
         BZ    *+8                                                              
         MVI   0(RF),C'X'                                                       
*                                                                               
DISS19   LA    RE,1(RE)                                                         
         LA    RF,4(RF)                                                         
         LA    R9,4(R9)                                                         
         BCT   R2,DISS18                                                        
*                                                                               
         LA    R9,BWDEL            LOOK FOR SPOTS PER WEEK ELEMENT              
         SR    R0,R0                                                            
         MVI   APFLAG,0                                                         
*                                                                               
DISS20   CLI   0(R9),0                                                          
         BE    DISS30                                                           
         CLI   0(R9),SPWELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R9)                                                         
         AR    R9,R0                                                            
         B     DISS20                                                           
         USING SPWEL,R9            FOUND-FORMAT SPOTS PER WEEK TO               
         ZIC   RF,SPWELLN                SCREEN                                 
         AR    RF,R9                                                            
         BCTR  RF,0                                                             
         ZIC   RE,CMPNWKS                                                       
         LA    RE,SPWPERWK-SPWEL-1(RE,R9)                                       
         CR    RE,RF                                                            
         BNL   *+6                                                              
         LR    RF,RE                                                            
         LA    RE,1                                                             
         LA    R1,SPWPERWK                                                      
         LA    R2,SKDSKD+1                                                      
         CLI   APACTN,ACTSSK                                                    
         BNE   *+8                                                              
         LA    R2,SSKSKD+1                                                      
         SR    R0,R0                                                            
*                                                                               
DISS22   CLI   1(R2),C'X'                                                       
         BNE   DISS23                                                           
         CLI   0(R1),0             ALREADY NOTHING IN THIS WEEK?                
         BE    DISS24              YES, NOTHING TO CHANGE                       
         CLI   APMODE,APMDISS2     TEST POST RECORD CHANGE DISPLAY MODE         
         BE    DISS24              THEN DON'T TOUCH UNTIL LATER                 
*                                                                               
         ST    R1,TMPREGR1                                                      
         ST    RE,TMPREGRE                                                      
         ST    RF,TMPREGRF                                                      
         BAS   RE,RTEFFDAT         ARE WE IN THE CORRECT EFF DATE               
         L     R1,TMPREGR1                                                      
         L     RE,TMPREGRE                                                      
         L     RF,TMPREGRF                                                      
         BNE   DISS24              NO, CAN'T CLEAR SPOTS FOR THIS WEEK          
*                                                                               
         MVI   0(R1),0             CHANGE X-OUT SPTS TO 0                       
         OI    APFLAG,X'80'        NEED TO WRITE OUT THIS RECORD                
         B     DISS24                                                           
*                                                                               
DISS23   CLI   1(R2),C'*'                                                       
         BE    DISS24                                                           
         CLI   0(R1),0                                                          
         BE    DISS24                                                           
         IC    R0,0(R1)                                                         
         CVD   R0,APDUB                                                         
         UNPK  0(2,R2),APDUB                                                    
         OI    1(R2),X'F0'                                                      
*                                                                               
DISS24   LA    R2,4(R2)                                                         
         BXLE  R1,RE,DISS22                                                     
         DROP  R9                                                               
*                                                                               
DISS30   TM    APFLAG,X'80'        DO WE HAVE TO WRITE THIS RECORD OUT?         
         BZ    DISSX                                                            
         GOTO1 AMIN,MINWRT2                                                     
*                                                                               
DISSX    B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK THAT THE WEEK WE'RE ON FOR THE SCHEDULE LINE IS FOR THE                 
* CORRECT EFFECTIVE DATE.  IF NOT, THEN WE DO NOT PERFORM ANY ACTIONS.          
*                                                                               
* ON ENTRY:    TMPREGR1            A(WEEK IN SPWEL ELEMENT)                     
*              (R9)                A(SPWEL ELEMENT)                             
*              (R3)                A(BWD RECORD)                                
***********************************************************************         
RTEFFDAT NTR1                                                                   
         USING SPWEL,R9                                                         
         L     R1,TMPREGR1         R1 = D(FROM 1ST WEEK)                        
         LA    RF,SPWPERWK                                                      
         SR    R1,RF                                                            
*                                                                               
         SLL   R1,2                APFULL(3) = START DATE OF WEEK               
         LA    RE,CMPDATSP(R1)                                                  
         GOTO1 VDATCON,APPARM,(2,0(RE)),(3,APFULL)                              
         DROP  R9                                                               
*                                                                               
         LA    R4,TSARREC                                                       
         USING TRECD,R4                                                         
         TM    TUINDS,TUIEFDT2+TUIEFDT3   ARE WE ON AN EFF COST LINE?           
         BNZ   RTDAT10                    YES                                   
***************                                                                 
* PROCESSING THE LINE THAT USES THE ORIGINAL COST                               
***************                                                                 
         OC    BWDEFDT2,BWDEFDT2   ANY EFFECTIVE DATES?                         
         BZ    RTDATYES            NO, OKAY TO CLEAR ANY SPOTS                  
*                                                                               
         CLC   APFULL(3),BWDEFDT2  IS WEEK ON/AFTER 1ST EFF DATE?               
         BL    RTDATYES            NO, OKAY TO CLEAR ANY SPOTS                  
         B     RTDATNO             YES, LEAVE SPOTS ALONE                       
***************                                                                 
* PROCESSING A LINE THAT DOESN'T USE THE ORIGINAL COST                          
***************                                                                 
RTDAT10  TM    TUINDS,TUIEFDT3     ARE WE ON 2ND EFF COST LINE?                 
         BNZ   RTDAT20             YES                                          
*********                                                                       
*** LINE USING 1ST EFFECTIVE COST                                               
*********                                                                       
         CLC   APFULL(3),BWDEFDT2  DATE ON/AFTER 1ST EFFECTIVE DATE?            
         BL    RTDATNO             NO, DON'T TOUCH THIS WEEK                    
*                                                                               
         OC    BWDEFDT3,BWDEFDT3   ANY 2ND EFFECTIVE DATE?                      
         BZ    RTDATYES            NO, OKAY TO CLEAR ANY SPOTS                  
*                                                                               
         CLC   APFULL(3),BWDEFDT3  NO, MAKE SURE DATE IS ON/AFTER 1ST           
         BL    RTDATYES                EFFECTIVE DATE                           
         B     RTDATNO                                                          
*********                                                                       
*** LINE USING 2ND EFFECTIVE COST                                               
*********                                                                       
RTDAT20  CLC   APFULL(3),BWDEFDT3  DATE ON/AFTER 2ND EFFECTIVE DATE?            
         BL    RTDATNO             NO, DON'T TOUCH THIS WEEK                    
RTDATYES SR    RC,RC                                                            
RTDATNO  LTR   RC,RC                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE LIST/SELECT DATA                                           *         
* INPUT : APPARM+0(4) = A(TWA DISPLAY LINE)                           *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   MVC   IOKEY(13),APRECKEY  READ THE DETAIL RECORD                       
         GOTO1 AMIN,MINRD2                                                      
         BE    *+14                                                             
         MVC   FVADDR,APPARM                                                    
         B     VALSX                                                            
         MVC   COMATWAL,APPARM                                                  
         CLI   APACTN,ACTSSK       TEST ACTION = SUPER-SKED                     
         BNE   VALS4                                                            
         L     RF,APPARM           YES-                                         
         CLI   APRECNUM,RECPKG     TES PACKAGE RECORD                           
         BNE   VALS2                                                            
         MVI   FVIFLD-FVIHDR(RF),C'S'   YES-SET SELECT FIELD TO 'S'             
         MVI   FVILEN-FVIHDR(RF),1          AND EXIT                            
         OI    6(RF),FVOXMT                                                     
         B     VALSX                                                            
*                                                                               
VALS2    LA    R4,SSKSKDH-SSKL1H(RF)   R4=A(SCHEDULE)                           
         GOTO1 AVALSKD,APPARM,(R4),SSKACTH   VALIDATE SKED AND BUILD            
         BNE   VALSX                         SPOTS PER WEEK ELEMENT             
         B     VALS8                                                            
*                                  NOT SUPER-SKED --                            
VALS4    GOTO1 AVALSEL1            VALIDATE PART 1                              
         BNE   VALSX                                                            
*                                                                               
         L     R3,AIOAREA2                                                      
         CLI   APACTN,ACTSKD       FOR SCHEDULE ACTION --                       
         BNE   VALS8                                                            
         L     RF,COMATWAL                                                      
         LA    RE,WRKL1H           SEE IF ANY CHANGES ON CORRESPONDING          
         SR    RF,RE               SCHEDULE LINE                                
         SR    RE,RE                                                            
         LA    R1,WRKL2H-WRKL1H                                                 
         DR    RE,R1                                                            
         LTR   RE,RE                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R4,WRKL8H+SKEDDSPL                                               
         LA    R1,SKDLINEL                                                      
         MR    RE,R1                                                            
         AR    R4,RF                                                            
         USING SKDLINED,R4                                                      
         TM    SKDSKDH+FVIIND-FVIHDR,FVIVAL TEST FOR NOT PREV VALIDATED         
         BO    VALS8                                                            
         CLI   APRECNUM,RECPKG     TEST PACKAGE RECORD                          
         BNE   VALS6                                                            
         L     R1,COMATWAL         YES - SET SELECT FIELD TO 'S'                
         MVI   FVIFLD-FVIHDR(R1),C'S'    AND EXIT                               
         MVI   FVILEN-FVIHDR(R1),1                                              
         OI    6(R1),FVOXMT                                                     
         B     VALSX                                                            
*                                 VALIDATE SKED AND BUILD SPOTS/WK ELEM         
VALS6    GOTO1 AVALSKD,APPARM,SKDSKDH,WRKL8H+ACDSPL                             
         BNE   VALSX                                                            
         OI    SKDLSPH+FVATRB-FVIHDR,FVAHIGH  HIGHLIGHT THE SKED LINE           
         OI    SKDSKDH+FVATRB-FVIHDR,FVAHIGH                                    
         OI    SKDSKDH+FVIIND-FVIHDR,FVIVAL   VALIDATED                         
*                                                                               
VALS8    L     R8,COMATWAL         VALIDATE PART 2                              
         GOTO1 AVALSEL2                                                         
         BNE   VALSX                                                            
         CLI   APACTN,ACTSSK       FOR ACTION NOT SUPER-SKED,                   
         BE    VALS10                                                           
         GOTO1 ADISPSEL            REDISPLAY THE RECORD                         
*                                                                               
VALS10   TM    COMCHG,LCHANGE      TEST FOR ANY CHANGE                          
         BZ    VALSX                                                            
         OI    SVACTIV,SVVALSEL    YES                                          
         B     VALSX                                                            
*                                                                               
VALSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ERASE A RECORD                                                      *         
***********************************************************************         
         SPACE 1                                                                
ERASE    MVC   IOKEY,APRECKEY                                                   
         GOTO1 AMIN,MINRD2                                                      
         BNE   ERASEX                                                           
         GOTO1 AMIN,MINDEL                                                      
*                                                                               
         CLI   BWDKELPO,0          TEST PACKAGE/ORBIT RECORD                    
         BE    ERASEX                                                           
         CLI   BWDKELSQ,0          YES-TEST PACKAGE/ORBIT SLAVE                 
         BNE   ERASEX              YES-EXIT                                     
*                                  NO-THEN IT'S A PACKAGE/ORBIT MASTER          
         MVC   IOKEY,APRECKEY      DELETE ASSOCIATED SLAVES                     
         LA    R1,MINHI2                                                        
         B     ERASE2+4                                                         
*                                                                               
ERASE2   LA    R1,MINSEQ2                                                       
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     ERASEX                                                           
         CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                  
         BNE   ERASEX                                                           
*                                                                               
ERASE4   GOTO1 AMIN,MINDEL                                                      
         B     ERASE2              READ ALL PACKAGE RECORDS                     
*                                                                               
ERASEX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT KEY FOR THE COMPANION CAMPAIGN TO SAVED KEY TABLE    *         
***********************************************************************         
         SPACE 1                                                                
PUTKEY   LA    R4,APELEM                                                        
         SR    R0,R0                                                            
         MVI   0(R4),KEYMED        MEDIA                                        
         MVI   1(R4),3                                                          
         MVC   2(1,R4),QMED                                                     
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         MVI   0(R4),KEYBYR        BUYER                                        
         MVI   1(R4),5                                                          
         MVC   2(3,R4),QBYR                                                     
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         MVI   0(R4),KEYCAM        COMPANION CAMPAIGN                           
         MVI   1(R4),7                                                          
         SR    RE,RE                                                            
         ICM   RE,3,CMPCCAM                                                     
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  2(5,R4),APDUB                                                    
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         MVI   0(R4),KEYMKT        MARKET/STATION                               
         MVI   1(R4),6                                                          
         OC    QSTA,QSTA                                                        
         BNZ   *+14                                                             
         MVC   2(4,R4),QMKT                                                     
         B     PUTK0                                                            
         MVC   2(4,R4),QSTA                                                     
         CLI   QSTA,C'0'                                                        
         BL    PUTK0                                                            
         MVI   6(R4),C'/'                                                       
         MVC   7(3,R4),QSTA+5                                                   
         MVI   1(R4),10                                                         
*                                                                               
PUTK0    IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         MVI   0(R4),KEYDPL        DAYPART/LENGTH (OPTIONAL)                    
         SR    R1,R1                                                            
         LA    RF,2(R4)                                                         
         OC    QDPT,QDPT                                                        
         BZ    PUTK1                                                            
         MVC   0(1,RF),BDPT                                                     
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
PUTK1    SR    RE,RE                                                            
         ICM   RE,1,BSLN                                                        
         BZ    PUTK2                                                            
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  0(3,RF),APDUB                                                    
         LA    R1,3(R1)                                                         
         CLI   0(RF),C'0'                                                       
         BNE   PUTK2                                                            
         MVC   0(2,RF),1(RF)                                                    
         MVI   2(RF),C' '                                                       
         BCTR  R1,0                                                             
PUTK2    LTR   R1,R1                                                            
         BZ    PUTK3                                                            
         LA    R1,2(R1)                                                         
         STC   R1,1(R4)                                                         
         AR    R4,R1                                                            
PUTK3    MVI   0(R4),0                                                          
         GOTO1 APUTKEY                                                          
PUTKX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
         SPACE 1                                                                
ENOTV    MVC   FVMSGNO,=AL2(FVFNOTV)   INVALID FIELD                            
         B     EXIT                                                             
EPF4     MVC   FVMSGNO,=AL2(FVPF4)     PRESS PF4 TO CONTINUE                    
         B     EXIT                                                             
ETMR     MVC   FVMSGNO,=AL2(FVTMR)     TOO MANY RECORDS TO LIST                 
         B     EXIT                                                             
EFLTDPT  MVC   FVMSGNO,=AL2(FVFLTDPT)  TOO MANY RECORDS - FILTER BY DPT         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
XFF      DC    16X'FF'                                                          
DMREAD   DC    CL7'DMREAD'                                                      
DMWRITE  DC    CL7'DMWRT '                                                      
TEMPSTR  DC    CL7'TEMPSTR'                                                     
SPACES   DC    64C' '                                                           
DASHES   DC    64C'-'                                                           
ALPHATAB DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789*',X'FF'                   
*                                                                               
CAL1DSPL EQU   0                                                                
CAL2DSPL EQU   CAL1DSPL+55+8+22+8                                               
SKEDDSPL EQU   CAL2DSPL+55+8                                                    
CMTDSPL  EQU   SKEDDSPL+NSKDLINS*(22+8+55+8)                                    
GLDSPL   EQU   CMTDSPL+70+8+1+8                                                 
ACDSPL   EQU   GLDSPL+76+8+1+8                                                  
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
TWABELS  DC    X'013E0119372800',55X'00'    CALENDAR LINE 1                     
         DC    X'011D0102162800',CL22'Ln -Sta- ---Program---'                   
         DC    X'013E0019372800',55X'00'    CALENDAR LINE 2                     
         DC    X'011D0102162000',22X'00'    SCHEDULE LINE 1 PROT                
         DC    X'013E0019370000',55X'00'                    UNPROT              
         DC    X'011D0102162000',22X'00'    SCHEDULE LINE 2                     
         DC    X'013E0019370000',55X'00'                                        
         DC    X'011D0102162000',22X'00'                                        
         DC    X'013E0019370000',55X'00'                                        
         DC    X'011D0102162000',22X'00'                                        
         DC    X'013E0019370000',55X'00'                                        
         DC    X'011D0102162000',22X'00'                                        
         DC    X'013E0019370000',55X'00'                                        
         DC    X'011D0102162000',22X'00'                                        
         DC    X'013E0019370000',55X'00'                                        
         DC    X'011D0102162000',22X'00'                                        
         DC    X'013E0019370000',55X'00'                                        
         DC    X'014D0102462000',70X'00'    COMMENT                             
         DC    X'01080102012800',CL1'G'     GOAL TOTALS                         
         DC    X'015300044C2000',76X'00'                                        
         DC    X'01080102012800',CL1'A'     ACTUAL TOTALS                       
         DC    X'015300044C2000',76X'00'                                        
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT PARAMETERS                                          *         
* OUTPUT : FVMSGNO NE FVFOK IF KEY IS INVALID                         *         
*          APRECKEY                                                   *         
*          APPARM FOR ROOT                                            *         
***********************************************************************         
VALPRM   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'**VPRM**'                                                    
*                                                                               
         GOTO1 AVALPARM,BWSKY2H    VALIDATE SELECT PARAMETERS                   
         BNE   VALPX                                                            
         OI    APINDS2,APIMDIS2                                                 
*                                                                               
******** CLI   SCPFKEY,255         TEST LFM ACTION JUST COMPLETED               
******** BNE   VALP2                                                            
******** CLI   APPFKEY,PFK05       YES-TEST FOR PFKEY5 (STAY ON                 
******** BNE   VALP2                                    CURRENT SCREEN)         
******** MVI   SCPFKEY,PFK05       YES-HONOR IT                                 
*                                                                               
VALP2    CLI   APACTN,ACTSSK       FOR SCHEDULE ACTIONS -                       
         BE    *+12                                                             
         CLI   APACTN,ACTSKD                                                    
         BNE   VALPX                                                            
         CLI   APPFKEY,PFK01       TEST PF1 - PF5                               
         BL    VALP3                                                            
         CLI   APPFKEY,PFK05                                                    
         BH    VALP3                                                            
         GOTO1 VDMGR,APPARM,DMWRITE,TEMPSTR,(4,0),TWAD                          
         MVI   APMODE,APMSWP       YES-SWAP TO RECAP SCREEN                     
         MVI   APPARM,RECWRK                                                    
         MVI   APPARM+1,ACTDRP                                                  
         L     RE,APALOCAL         CLEAR WORKING STORAGE                        
         LH    RF,=Y(APLOCALX-APLOCAL)                                          
         XCEF  ,                                                                
*                                                                               
         CLI   APACTN,ACTSKD       TURN ON TWA INDICATOR                        
         BNE   *+12                                                             
         OI    TWAINDS,TWAIRCP1                                                 
         B     *+8                                                              
         OI    TWAINDS,TWAIRCP2                                                 
*                                                                               
         XC    INOFRM,INOFRM       CLEAR FORMAT OPTION                          
         B     VALPX               AND EXIT TO RECAP                            
*                                                                               
VALP3    CLI   APPFKEY,PFK10       TEST PF10 KEY                                
         BNE   VALP5                                                            
         MVI   APPFKEY,0                                                        
         OC    CMPCCAM,CMPCCAM     AND THERE'S A COMPANION CAMPAIGN             
         BZ    VALP5                                                            
         TM    TWAINDS,TWAICCSW    YES-TEST ALREADY IN COMPANION                
         BZ    VALP4                                                            
         MVI   APMODE,APMRET       YES-RETURN TO ORIGINAL CAMPAIGN              
         MVI   APLSMSEL,C' '       WITH INVISIBLE COMPLETION CHARACTER          
         NI    APINDS2,255-APIMDIS2                                             
         NI    TWAINDS,255-TWAICCSW-TWAICCS1                                    
         NI    TWALSCTL,255-TWALSHSL                                            
         MVC   SVGVA(SVGVAL),SVCCGVA  RESTORE GOAL VS ACTUAL AREA               
         B     VALPX                                                            
*                                                                               
VALP4    OI    TWAINDS,TWAICCSW    NO-SWAP TO COMPANION CAMPAIGN                
         OI    TWAINDS,TWAICCS1                                                 
         OI    TWALSCTL,TWALSHSL   HOLD COMPANION UNTIL PF10 AGAIN              
         LA    R1,WRKL1H           PLACE SELECT CODE IN FIRST LINE              
         CLI   APACTN,ACTSKD                                                    
         BE    *+8                                                              
         LA    R1,SSKL1H                                                        
         MVI   L'FVIHDR(R1),C'W'                                                
         OI    FVILEN-FVIHDR(R1),1                                              
         MVC   SVCCGVA,SVGVA       SAVE GOAL VS ACTUAL AREA                     
*                                                                               
VALP5    CLI   APACTN,ACTSKD       FOR SKEDULE ACTION -                         
         BNE   VALP12                                                           
         MVI   APPARM+4,NLSTLINS   SET NUMBER OF LIST LINES                     
         L     R1,ALSM                                                          
         USING LSMD,R1                                                          
         TM    LSMINDS,LSMISEL     IF USER NOW INVITED TO ENTER                 
         BZ    VALP12              SELECTIONS,                                  
         CLC   APRECKEY,LSMUKEY    AND THE KEY HAS NOT CHANGED --               
         BNE   VALP12                                                           
         LA    R0,NSKDLINS                                                      
         SR    R1,R1                                                            
         LA    RE,WRKL8H+SKEDDSPL                                               
         LA    R4,WRKL1H                                                        
         USING WRKL1H,R4                                                        
*                                                                               
VALP6    LA    RF,SKDLINEL(RE)     FOR ALL SCHEDULE LINES,                      
*                                                                               
VALP7    CR    RE,RF               SCAN THE FIELDS                              
         BE    VALP10                                                           
         BL    *+6                                                              
         DC    H'0'                                                             
         TM    FVATRB-FVIHDR(RE),FVAPROT    TEST FOR UNPROTECTED                
         BO    VALP8                                                            
         TM    FVIIND-FVIHDR(RE),FVIVAL     TEST FOR NOT PREV VALIDATED         
         BO    VALP8                                                            
         NI    WRKPRGH+FVIIND-FVIHDR,FF-FVIVAL TURN OFF PREV VALIDATED          
         B     VALP10                          BIT IN CORRESP LIST LINE         
*                                                                               
VALP8    ICM   R1,1,0(RE)          NEXT FIELD                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    RE,R1                                                            
         B     VALP7                                                            
*                                                                               
VALP10   LR    RE,RF               NEXT SCHEDULE LINE                           
         LA    R4,WRKL2H-WRKL1H(R4)                                             
         BCT   R0,VALP6                                                         
         DROP  R4                                                               
*                                                                               
VALP12   TM    TWAFLAG,TWAFMTPD    TEST SHOULD FORMAT ACTUAL PTS/DOL            
         BZ    VALPX                                                            
         LA    R4,WRKL8H+ACDSPL                                                 
         CLI   APACTN,ACTSKD                                                    
         BE    *+8                                                              
         LA    R4,SSKACTH                                                       
         LM    R8,R9,APPARM        PRESERVE APPARM(8)                           
         GOTO1 AFMACPTS,APPARM,(R4),SVGVA                                       
         GOTO1 AFMACDOL,APPARM,(R4),SVGVA                                       
         STM   R8,R9,APPARM                                                     
         NI    TWAFLAG,255-TWAFMTPD                                             
*                                                                               
VALPX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMMON ROUTINES                                                     *         
***********************************************************************         
         SPACE 1                                                                
COMMON   NMOD1 0,**BW5C**,RA,R9                                                 
         L     RC,APALOCAL                                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         L     R5,ATWA             R5=A(TWA)                                    
         USING TWAD,R5                                                          
         LH    R6,=Y(SAVAREA-TWAD)                                              
         LA    R6,TWAD(R6)         R6=A(SAVE AREA IN TWA)                       
         USING SAVAREA,R6                                                       
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     VALPARM                                                          
         B     DISPSEL                                                          
         B     VALSEL1                                                          
         B     VALSEL2                                                          
         B     FMACPTS                                                          
         B     FMACDOL                                                          
         B     TWABLD                                                           
         B     VALSKD                                                           
         B     FMTGOAL                                                          
*                                                                               
COMMONX  CLC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
COMXIT   XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT PARAMETERS -- PART 1                                *         
*                                                                     *         
* INPUT  : R1=A(SECOND KEY FIELD FOR MKT AND DPT/LEN) OR 0            *         
* OUTPUT : IF ERROR, FVMSGNO SET AND FVINDX SET                       *         
***********************************************************************         
         SPACE 1                                                                
VALPARM  ST    R1,COMSVRG1         SAVE A(SECOND KEY FIELD)                     
         LA    R2,IOKEY                                                         
         USING BWHKEY,R2                                                        
         LA    R3,APRECKEY                                                      
         USING BWDKEY,R3                                                        
         NI    TWAFLAG,FF-TWANODET   TURN OFF NO DETAIL RECORDS SWITCH          
         XC    BWHKEY,BWHKEY                                                    
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         L     RF,AKEYHDR          SET ACTUAL LENGTH OF KEY FIELD               
         LR    R8,RF                                                            
         ZIC   R4,FVILEN-FVIHDR(RF)                                             
         LA    R1,L'FVIHDR-1(R4,RF)                                             
         SR    RE,RE                                                            
         BCTR  RE,0                                                             
         LA    RF,L'FVIHDR(RF)                                                  
         CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  R4,0                                                             
         BXH   R1,RE,*-10                                                       
         STC   R4,FVILEN-FVIHDR(R8)                                             
*                                                                               
         GOTO1 VSCANNER,APPARM,AKEYHDR,AIOAREA1,C',=,='                         
         NI    SVINDS,255-SVIKEY2                                               
         LA    R8,1                                                             
*                                                                               
VPAR1    SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BZ    VPAR98                                                           
         MVI   BDPT,0                                                           
         MVI   BSLN,0                                                           
         L     R4,AIOAREA1                                                      
*                                                                               
VPAR2    STC   R8,FVINDX                                                        
         SR    RF,RF                                                            
         ICM   RF,1,0(R4)                                                       
         BZ    VPAR98                                                           
         CLI   1(R4),0                                                          
         BNE   VPAR94                                                           
         BAL   RE,VPARSETF                                                      
*                                                                               
         CLI   FVINDX,1                                                         
         BNE   VPAR4                                                            
         GOTO1 AVALMED,APWORK      VALIDATE MEDIA                               
         BNE   VPAR99                                                           
         MVC   BWHKAGMD,BAGYMD                                                  
         B     VPAR32                                                           
*                                                                               
VPAR4    CLI   FVINDX,2                                                         
         BNE   VPAR6                                                            
         GOTO1 AVALBYR,APWORK      VALIDATE BUYER                               
         BNE   VPAR99                                                           
         MVC   BWHKBYR,BBYR                                                     
         OC    BYRPW,BYRPW         CHECK BUYER'S PASSWORD                       
         BZ    VPAR32                                                           
         GOTO1 AVALPWD                                                          
         BNE   VPARX                                                            
         B     VPAR32                                                           
*                                                                               
VPAR6    CLI   FVINDX,3                                                         
         BNE   VPAR20                                                           
         GOTO1 AVALCAM,APWORK      VALIDATE CAMPAIGN NUMBER                     
         BNE   VPAR99                                                           
         MVC   BWHKCAM,BCAM                                                     
         GOTO1 AGETCLT,CMPCLTC     (GET CLIENT)                                 
         BNE   VPARX                                                            
         XC    PRDNM1,PRDNM1                                                    
         XC    PRDNM2,PRDNM2                                                    
         CLI   CMPPRD1,0           TEST FOR PIGGYBACKS                          
         BE    VPAR8                                                            
         GOTO1 AGETPRD,CMPPRD1     (GET PIGGYBACK PRD 1)                        
         MVC   PRDNM1,PRDNM                                                     
         MVC   COMPRD1,QPRD                                                     
         CLI   CMPPRD2,0                                                        
         BE    VPAR8                                                            
         GOTO1 AGETPRD,CMPPRD2     (GET PIGGYBACK PRD 2)                        
         MVC   PRDNM2,PRDNM                                                     
         MVC   COMPRD2,QPRD                                                     
*                                                                               
VPAR8    GOTO1 AGETPRD,CMPPRDN     (GET PRODUCT)                                
         BNE   VPARX                                                            
*                                                                               
         CLI   APACTN,ACTXFR       IF TRANSFERRING THEN FORCE THE READ          
         BNE   *+8                     OF THE ESTIMATE FOR LOCKOUT              
         MVI   BEST,0                                                           
*                                                                               
         GOTO1 AGETEST,CMPESTN     GET CAMPAIGN ESTIMATE DETAILS                
         BNE   VPARX                                                            
         SR    R1,R1                                                            
         OC    INORTG,INORTG       TEST FOR RATING OPTION                       
         BZ    VPAR12                                                           
         LA    RE,ESTDEMS                                                       
*                                                                               
VPAR10   OC    0(3,RE),0(RE)       YES - VALIDATE IT                            
         BZ    VPARERTG                                                         
         CLC   1(2,RE),INORTG+1                                                 
         BE    VPAR12                                                           
         LA    RE,3(RE)                                                         
         BCT   R1,VPAR10                                                        
*                                                                               
VPAR12   LPR   R1,R1                                                            
         XC    DBLOCK,DBLOCK       GET DEMO NAMES                               
         MVC   DBCOMFCS,ACOM                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
         LA    RE,ESTDEMS                                                       
         MVI   APBYTE,LNDEMOS                                                   
         ICM   RE,8,APBYTE                                                      
         ST    RE,APPARM                                                        
***      LA    RE,ESTUSRNM                                                      
***      ST    RE,APPARM+12                                                     
         XC    COMDNAMS,COMDNAMS                                                
         ST    R1,APFULL                                                        
         GOTO1 VDEMOCON,APPARM,,(6,COMDNAMS),(C'S',DBLOCK)                      
***************                                                                 
* DO USER DEFINED ONES BY HAND BECAUSE DEMOCON DOESN'T SEEM TO LIKE             
* THE TYPE '6' IN THE ABOVE CALL                                                
* TRIED THE TYPE '2' WHICH RETURNS THE USER DEFINED LABEL W/O A PROBLEM         
* BUT ALL THE DEMO NAMES BECAME 7 CHARS LONG  (RAD1849 NOT RA1849)              
***************                                                                 
         LA    RE,ESTDEMS                                                       
         LA    RF,ESTUSRNM                                                      
         LA    R1,COMDNAMS                                                      
*                                                                               
VPAR13   OC    0(3,RE),0(RE)       ANY MORE DEMOS?                              
         BZ    VPAR15              NO MORE                                      
         CLI   1(RE),X'21'         USER DEFINED?  (SEE DEDEMOCON)               
         BNE   VPAR14                                                           
         MVC   0(6,R1),0(RF)       COMDNAMS IS A LIST OF 6-BYTE NAMES           
         LA    RF,7(RF)              WHILE ESTUSRNM IS 7-BYTE NAMES             
VPAR14   LA    RE,3(RE)                                                         
         LA    R1,6(R1)                                                         
         B     VPAR13              LOOP UNTIL ALL DEMOS CHECKED                 
*                                                                               
VPAR15   L     R1,APFULL                                                        
         MH    R1,=H'6'                                                         
         LA    R1,COMDNAMS(R1)     POINT TO TARGET DEMO NAME                    
         ST    R1,APFULL           AND SAVE ITS ADDRESS IN APFULL               
*                                                                               
         MVC   SVDEMO1,ESTDEMS    SET TARGET DEMO (SVDEMO1)                     
         OC    INORTG,INORTG                                                    
         BZ    VPAR16                                                           
         CLC   SVDEMO1,INORTG                                                   
         BNE   *+14                                                             
         XC    INORTG,INORTG                                                    
         B     VPAR16                                                           
         MVC   SVDEMO1,INORTG                                                   
*                                                                               
VPAR16   XC    SVDEMO2,SVDEMO2     SET TARGET IMPRESSION/RTG (SVDEMO2)          
         MVC   APWORK(1),ESTDEMS+1                                              
         CLI   0(R1),C'R'                                                       
         BE    *+12                                                             
         CLI   0(R1),C'E'                                                       
         BNE   *+8                                                              
         MVI   APWORK,C'I'                                                      
         MVC   APWORK+1(1),SVDEMO1+2                                            
         LA    RE,ESTDEMS                                                       
*                                                                               
VPAR18   OC    0(3,RE),0(RE)                                                    
         BE    VPAR32                                                           
         CLC   1(2,RE),APWORK                                                   
         BE    *+12                                                             
         LA    RE,3(RE)                                                         
         B     VPAR18                                                           
         MVC   SVDEMO2,0(RE)                                                    
         B     VPAR32                                                           
*                                                                               
VPAR20   CLI   FVINDX,4                                                         
         BNE   VPAR30                                                           
         TM    2(R4),X'80'         TEST NUMERIC FIELD                           
         BZ    VPAR24                                                           
         CLI   APACTN,ACTADD       YES - TEST ACTION=ADD                        
         BNE   VPAR22                                                           
         CLI   INREC,RECPKG              AND RECORD=PACKAGE OR ORBIT            
         BE    VPAR95                    YES-MUST BE STATION                    
         CLI   INREC,RECORB                                                     
         BE    VPAR95                                                           
*                                                                               
VPAR22   GOTO1 AVALMKT,APWORK      VALIDATE MARKET                              
         BNE   VPAR99                                                           
         B     VPAR28                                                           
*                                                                               
VPAR24   CLI   INREC,RECGOL        TEST RECORD=GOAL                             
         BE    VPAR92              YES-INVALID                                  
*                                                                               
         GOTO1 AVALSTA,APWORK      VALIDATE STATION                             
         BNE   VPAR99                                                           
*                                                                               
VPAR28   MVC   BWHKMKT,BMKT                                                     
         B     VPAR32                                                           
*                                                                               
VPAR30   CLI   FVINDX,5                                                         
         BNE   VPAR31                                                           
         CLI   INREC,RECGOL        DPT/LEN INVALID FOR GOAL RECORD              
         BE    VPAR91                                                           
         GOTO1 AVALDPL,APWORK      VALIDATE DAYPART/LENGTH                      
         BNE   VPAR99                                                           
         CLI   CMPDPOPT,C'M'       TEST SUBDPTS SCHEDULED UNDER MASTER          
         BNE   VPAR32                                                           
         CLI   DPTTYPE,C'S'        YES-TEST SUBDAYPART                          
         BE    VPAR96                  YES-ERROR                                
         B     VPAR32                                                           
*                                                                               
VPAR31   B     VPAR94                                                           
*                                                                               
VPAR32   LA    R4,32(R4)           NEXT KEY VALUE                               
         LA    R8,1(R8)                                                         
         BCT   R0,VPAR2                                                         
*                                                                               
         LA    R8,1                CHECK FOR ALL KEY FIELDS PRESENT             
         OC    BWHKAGMD,BWHKAGMD                                                
         BZ    VPAR98                                                           
         LA    R8,1(R8)                                                         
         OC    BWHKBYR,BWHKBYR                                                  
         BZ    VPAR98                                                           
         LA    R8,1(R8)                                                         
         OC    BWHKCAM,BWHKCAM                                                  
         BZ    VPAR98                                                           
         LA    R8,1(R8)                                                         
         OC    BWHKMKT,BWHKMKT                                                  
         BNZ   VPAR34                                                           
         TM    SVINDS,SVIKEY2      TEST VALIDATING 2ND KEY FIELD                
         BO    VPAR98              YES                                          
         ICM   R1,15,COMSVRG1      NO-TEST A(2ND KEY FIELD) PASSED              
         BZ    VPAR98              NO                                           
         OI    SVINDS,SVIKEY2      YES-VALIDATE 2ND KEY FIELD                   
         MVI   FVMINL,1                FOR MARKET AND DPT/LEN                   
         GOTO1 AFVAL                                                            
         BNE   VPARX                                                            
         GOTO1 VSCANNER,APPARM,(C'C',FVIFLD),(2,AIOAREA1)                       
         B     VPAR1                                                            
*                                                                               
VPAR34   MVI   FVINDX,0                                                         
         MVC   HDRKEY,BWHKEY       SAVE HEADER KEY                              
         CLI   INODPT,C'M'         TEST DPT=MAS OPTION                          
         BNE   *+12                                                             
         CLI   BDPT,0              YES-TEST DAYPART IN KEY                      
         BE    VPAR93              NO-MISSING                                   
         CLI   INOSID,0            TEST SID OPTION SET                          
         BE    VPAR36                                                           
         OC    QSTA,QSTA           YES-TEST STATION OR DPT MUST BE SET          
         BNZ   VPAR36                                                           
         MVC   MKTNOCLT,BMKT       (SET MARKET FOR RANSID)                      
         OC    QCABLE,QCABLE       ALLOW CABLE FILTER                           
         BNZ   VPAR36                                                           
         CLI   BDPT,0                                                           
         BE    VPAR97                                                           
*                                                                               
VPAR36   CLI   BDPT,0              TEST VALIDATED DAYPART                       
         BNE   VPAR38              YES-THEN SPOT LENGTH SET                     
         CLI   BSLN,0              NO-IF LENGTH=0,                              
         BNE   VPAR38                                                           
         MVC   BSLN,CMPSLN         THEN USE CAMPAIGN LENGTH (IF ANY)            
*                                                                               
VPAR38   CLI   APACTN,ACTSSK       IF ACTION ISN'T SUPER-SKED                   
         BE    VPAR42                                                           
         CLI   APACTN,ACTXFR       OR TRANSFER                                  
         BE    VPAR42                                                           
         CLI   APACTN,ACTSPL       OR SPILL                                     
         BE    VPAR42                                                           
         CLI   APACTN,ACTFIX       OR FIX                                       
         BE    VPAR42                                                           
         CLI   APACTN,ACTCAL       OR CALL LETTER CHANGE                        
         BE    VPAR42                                                           
         CLI   INREC,RECPKG        OR RECORD ISN'T PACKAGE                      
         BE    VPAR42                                                           
         CLI   INREC,RECORB        OR RECORD ISN'T ORBIT                        
         BE    VPAR42                                                           
         CLI   INREC,RECGOL        OR RECORD ISN'T GOAL                         
         BE    VPAR42                                                           
         NI    SVINDS,255-SVIDPLN                                               
         TM    INOIND,INOICPM      TEST ALWAYS CPM IN HEADLINE                  
         BO    VPAR41              YES                                          
         CLI   BDPT,0              NO-TEST EITHER ALL DAYPARTS                  
         BE    VPAR40                 OR ALL SPOT LENGTHS                       
         CLI   BSLN,0                                                           
         BE    VPAR40                                                           
         CLI   INODPT,C'M'         OR DPT=MAS (MAYBE MULTIPLE DAYPARTS)         
         BNE   VPAR41                                                           
         CLI   DPTTYPE,C'M'           AND DAYPART IS A MASTER                   
         BNE   VPAR41                                                           
*                                                                               
VPAR40   OI    SVINDS,SVIDPLN         YES-DPT/LN IN COLUMN HEADING              
*                                                                               
VPAR41   L     R1,APFULL                                                        
         MVC   WRKDCP(6),0(R1)     FORMAT DEMO NAME TO HEADING                  
         MVI   WRKDCP+11,C'p'                                                   
         CLI   0(R1),C'R'                                                       
         BE    *+16                                                             
         CLI   0(R1),C'E'                                                       
         BE    *+8                                                              
         MVI   WRKDCP+11,C'm'      FORMAT M IF IMPRESSION (CPM)                 
         OI    WRKDCPH+6,FVOXMT                                                 
         OI    WRKCPMH+6,FVOXMT                                                 
         MVC   WRKCPM(6),=C'--Cpm-'     CPM IN COLUMN HEADING                   
         CLI   0(R1),C'R'                                                       
         BE    *+16                                                             
         CLI   0(R1),C'E'                                                       
         BE    *+8                                                              
         MVI   WRKCPM+4,C'p'       CHANGE CPM TITLE TO CPP                      
         TM    SVINDS,SVIDPLN      TEST DPT/LEN IN HEADLINE                     
         BZ    VPAR42                                                           
         MVC   WRKCPM(6),=C'Dpt/ln'                                             
*                                                                               
VPAR42   GOTO1 AIO,DIRHI+IO1       READ HEADER POINTER                          
         BNE   VPARX                                                            
         CLC   IOKEY(BWHKSEQ-BWHKEY),IOKEYSAV  TEST RECORD FOUND                
         BE    VPAR43                                                           
         OI    TWAFLAG,TWANOHDR    NO                                           
         LA    R1,BWSKEYH          SET CURSOR TO KEY IN CASE THIS IS            
         ST    R1,FVADDR           AN ERROR                                     
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         XC    BCMSEQ,BCMSEQ       NO CAMPAIGN/MARKET SEQ NO                    
         XC    APRECKEY,APRECKEY                                                
         MVC   APRECKEY(13),IOKEYSAV   SET KEY TO HEADER KEY                    
         B     VPAR50                                                           
*                                                                               
VPAR43   MVC   HDRDA,IODA          SAVE HEADER D/A                              
         GOTO1 AIO,FILGETU1        GET HEADER RECORD                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         MVC   BCMSEQ,BWHKSEQ      SAVE CAMPAIGN/MARKET SEQ NO                  
         SR    RE,RE                                                            
         MVI   BSTACD,0                                                         
******** OC    BSTA,BSTA           TEST STATION REQUESTED                       
         OC    QSTA,QSTA           TEST STATION REQUESTED                       
         BZ    VPAR48                                                           
         LA    R4,BWHFSTEL         YES-LOOK FOR IN HEADER                       
         SR    R0,R0                                                            
         SR    RE,RE                                                            
*                                                                               
VPAR44   CLI   0(R4),0                                                          
         BE    VPAR47                                                           
         CLI   0(R4),BWHELCDQ                                                   
         BNE   *+14                                                             
         USING BWHEL,R4                                                         
         CLC   BWHSTA,QSTA                                                      
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VPAR44                                                           
         IC    RE,BWHSEQ                                                        
         STC   RE,BSTACD           SET STATION SEQ NO                           
         B     VPAR48                                                           
*                                                                               
VPAR47   OI    TWAFLAG,TWANOSTA    STATION NOT FOUND-SET NO STATION             
         MVC   FVMSGNO,=AL2(FVFERNF)  AND ERROR CODE                            
         LA    R1,BWSKEYH                                                       
         ST    R1,FVADDR                                                        
         SR    RE,RE                                                            
*                                                                               
VPAR48   XC    APRECKEY,APRECKEY                                                
         MVI   BWDKTYP,BWDKTYPQ    SET UP FIRST PART OF DETAIL KEY              
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,BAGYMD                                                  
         MVC   BWDKBYR,BBYR                                                     
         MVC   BWDKSEQ,BWHKSEQ     CAMPAIGN/MKT SEQ NO FROM HEADER KEY          
         MVI   BWDKELCD,BWDELCDQ                                                
         STC   RE,BWDKELST         STATION CODE                                 
*                                                                               
         XC    DTLKEY,DTLKEY       SAVE THE DETAIL RECORD KEY                   
         MVC   DTLKEY(BWDKELST-BWDKEY),BWDKEY                                   
*                                                                               
VPAR50   MVC   APRECKEY+20(8),QSTA SET STATION/DPT/LEN IN KEY SO THAT           
         MVC   APRECKEY+28(1),BDPT ROOT CAN DETECT KEY CHANGE                   
         MVC   APRECKEY+29(1),BSLN                                              
         MVC   APRECKEY+30(5),QCABLE                                            
*                                                                               
         L     R1,ALSM                                                          
         CLC   APRECKEY,LSMUKEY-LSMD(R1) TEST CHANGE OF KEY                     
         BE    VPAR56                                                           
         CLI   APACTN,ACTXFR       AND ACTION IS NOT TRANSFER                   
         BE    VPAR56                                                           
         CLI   APACTN,ACTSPL       OR SPILL                                     
         BE    VPAR56                                                           
         CLI   APACTN,ACTFIX       OR FIX                                       
         BE    VPAR56                                                           
         CLI   APACTN,ACTSEN       OR SEND                                      
         BE    VPAR56                                                           
         CLI   INREC,RECGOL        OR RECORD'S NOT GOAL                         
         BE    VPAR56                                                           
         MVC   APELEM(86),BLANKS   YES - FORMAT CAMPAIGN DETAILS                
         MVC   APELEM(20),CLTNM                                                 
         MVI   APELEM+21,C'/'                                                   
         MVC   APELEM+23(20),PRDNM                                              
         CLC   PRDNM1,BLANKS                                                    
         BNH   VPAR52                                                           
         MVC   APELEM+23(10),PRDNM1                                             
         MVI   APELEM+33,C'-'                                                   
         MVC   APELEM+34(9),PRDNM2                                              
*                                                                               
VPAR52   MVI   APELEM+44,C'/'                                                   
         MVC   APELEM+46(3),QEST                                                
         MVC   APELEM+50(20),ESTNM                                              
         SR    RE,RE                                                            
         ICM   RE,1,BSLN                                                        
         BZ    VPAR54                                                           
         MVI   APELEM+71,C'/'                                                   
         MVC   APELEM+73(4),=C'SLN='                                            
         CVD   RE,APDUB                                                         
         UNPK  APELEM+77(3),APDUB                                               
         OI    APELEM+79,X'F0'                                                  
         CLI   APELEM+77,C'0'                                                   
         BNE   VPAR54                                                           
         MVC   APELEM+77(2),APELEM+78                                           
         MVI   APELEM+79,C' '                                                   
*                                                                               
VPAR54   MVC   APELEM+81(5),=C'/ ARB'     RATING SERVICE                        
         CLI   CUDMED,C'C'                                                      
         BNE   *+10                                                             
         MVC   APELEM+83(3),=C'BBM'                                             
         CLI   CLTSRC,C'A'                                                      
         BE    VPAR55                                                           
         MVC   APELEM+83(3),=C'NSI'                                             
         CLI   CUDMED,C'C'                                                      
         BNE   *+10                                                             
         MVC   APELEM+83(3),=C'CSI'                                             
         CLI   CLTSRC,C'N'                                                      
         BE    VPAR55                                                           
         MVC   APELEM+81(5),BLANKS                                              
*                                                                               
VPAR55   GOTO1 VSQUASH,APPARM,APELEM,86                                         
         MVC   WRKCPE,APELEM                                                    
         MVI   WRKCPE+L'WRKCPE-1,C'?'                                           
         OI    WRKCPEH+6,FVOXMT                                                 
         LA    R1,WRKCPE+L'WRKCPE                                               
         OI    6(R1),X'80'         RETRANSMIT THE SCROLL FIELD TOO              
*                                                                               
VPAR56   TM    SVINDS,SVIKEY2      TEST KEY2 USED                               
         BZ    VPAR58                                                           
         MVC   APHALF,FVMSGNO                                                   
         L     R1,COMSVRG1         YES-UPDATE KEY COMPONENT TABLE               
         GOTO1 AFVAL                                                            
         ZIC   R4,FVXLEN                                                        
         EX    R4,*+4                                                           
         MVC   APWORK(0),FVIFLD                                                 
         L     R1,AKEYHDR                                                       
         GOTO1 AFVAL                                                            
         MVC   FVMSGNO,APHALF                                                   
         ZIC   RE,FVILEN                                                        
         LA    R1,FVIFLD(RE)                                                    
         MVI   0(R1),C','                                                       
         EX    R4,*+4                                                           
         MVC   1(0,R1),APWORK                                                   
         LA    R4,2(R4,RE)                                                      
         STC   R4,FVILEN                                                        
         MVC   APFULL,AKEYHDR                                                   
         LA    R1,FVIHDR                                                        
         ST    R1,AKEYHDR                                                       
         MVC   APDUB(4),ACIOADD                                                 
         MVC   ACIOADD,AIOAREA4                                                 
         MVC   SVINKEYN,INKEYN                                                  
         MVC   SVINKEYT,INKEYT                                                  
         MVI   INKEYN,0                                                         
         MVC   INKEYT,LINKEYT                                                   
         GOTO1 ASETKEY                                                          
         MVC   AKEYHDR,APFULL                                                   
         MVC   ACIOADD,APDUB                                                    
         MVC   INKEYN,SVINKEYN                                                  
         MVC   INKEYT,SVINKEYT                                                  
*                                                                               
VPAR58   CLI   BDPT,0              TEST DAYPART SPECIFIED                       
         BNE   VPAR60                                                           
*                                  NO-REMOVE FROM SAVED KEYS                    
         GOTO1 VHELLO,APPARM,(C'D',SCTABNAM),('KEYDPL',SAVKEYL),0               
*                                                                               
VPAR60   CLI   INREC,RECPKG        FOR RECORD NOT PACKAGE                       
         BE    VPARX                                                            
         CLI   INREC,RECORB        OR RECORD NOT ORBIT                          
         BE    VPARX                                                            
         CLI   INREC,RECGOL        OR RECORD NOT GOAL                           
         BE    VPARX                                                            
         CLI   APACTN,ACTXFR       OR TRANSFER                                  
         BE    VPARX                                                            
         CLI   APACTN,ACTSPL       OR SPILL                                     
         BE    VPARX                                                            
         CLI   APACTN,ACTFIX       OR FIX --                                    
         BE    VPARX                                                            
         CLI   APACTN,ACTSSK       SET VALUES FOR ROOT                          
         BNE   VPAR62                                                           
         LA    R0,SSKL1H           SUPER-SKED SCREEN                            
         ST    R0,APPARM+0                                                      
         MVI   APPARM+4,NSSKLINS                                                
         LH    R0,=Y(SSKL2H-SSKL1H)                                             
         STH   R0,APPARM+6                                                      
         B     VPARX                                                            
*                                                                               
VPAR62   LA    R0,WRKL1H           UPDATE/SKED/DEMO SCREENS                     
         ST    R0,APPARM+0                                                      
         LA    R1,WRKL2H                                                        
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         LA    RF,WRKFINH                                                       
         SR    RF,R0                                                            
         SR    RE,RE                                                            
         DR    RE,R1                                                            
         LA    RF,1(RF)                                                         
         STC   RF,APPARM+4                                                      
         B     VPARX                                                            
*                                                                               
VPAR91   MVC   FVMSGNO,=AL2(FVIDPT2)    DAYPART NOT VALID                       
         B     VPAR99                                                           
*                                                                               
VPAR92   MVC   FVMSGNO,=AL2(FVIMKT)     INVALID MARKET                          
         B     VPAR99                                                           
*                                                                               
VPAR93   MVC   FVMSGNO,=AL2(FVNODPT)    DAYPART MISSING                         
         LA    R8,1(R8)                                                         
         B     VPAR99                                                           
*                                                                               
VPAR94   MVC   FVMSGNO,=AL2(FVFNOTV)    INVALID INPUT FIELD                     
         B     VPAR99                                                           
*                                                                               
VPAR95   MVC   FVMSGNO,=AL2(FVMKTSTA)   MKT INVALID-MUST ENTER STATION          
         B     VPAR99                                                           
*                                                                               
VPAR96   MVC   FVMSGNO,=AL2(FVISDPT)    INVALID SUBDAYPART                      
         B     VPAR99                                                           
*                                                                               
VPAR97   MVC   FVMSGNO,=AL2(FVSTADPT)                                           
         B     VPAR99                                                           
*                                                                               
VPAR98   MVC   FVMSGNO,=AL2(FVFNONE)   KEY FIELD MISSING                        
*                                                                               
VPAR99   LA    R1,BWSKEYH          KEY ERROR EXIT                               
         TM    SVINDS,SVIKEY2      TEST VALIDATING SECOND KEY FIELD             
         BZ    *+12                                                             
         L     R1,COMSVRG1                                                      
         SH    R8,=H'3'                                                         
         ST    R1,FVADDR                                                        
         STC   R8,FVINDX                                                        
*                                                                               
VPARX    B     COMMONX                                                          
         SPACE 2                                                                
VPARERTG MVC   FVMSGNO,=AL2(FVIRTGOP)   INVALID RATING OPTION                   
         LA    R1,BWSOPTH                                                       
         ST    R1,FVADDR                                                        
         B     VPARX                                                            
         SPACE 2                                                                
VPARSETF XC    APWORK,APWORK                                                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   APWORK+L'FVIHDR(0),12(R4)                                        
         LA    RF,1(RF)                                                         
         STC   RF,FVILEN-FVIHDR+APWORK                                          
         LA    RF,L'FVIHDR(RF)                                                  
         STC   RF,APWORK                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT RECORD -- PART 1                                *         
*                                                                     *         
* INPUT  : R8 = A(TWA DISPLAY LINE)                                   *         
*          IOAREA2 CONTAINS DETAIL RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
DISPSEL  L     R3,AIOAREA2                                                      
         USING BWDRECD,R3                                                       
         USING WRKL1H,R8                                                        
         XC    WRKDLM,WRKDLM                                                    
         OI    WRKDLMH+6,FVOXMT                                                 
         LA    R4,WRKLST                                                        
         USING LIST1D,R4                                                        
         MVC   LISTSTA,BWDSTA      STATION                                      
         OI    WRKLSTH+6,FVOXMT                                                 
         OI    WRKDAYH+6,FVOXMT    DAYS/TIMES                                   
         OI    WRKTIMH+6,FVOXMT                                                 
         CLI   BWDDAYS,0           TEST DAYS PRESENT                            
         BNE   DSEL4                                                            
         SR    RE,RE               NO-CHECK FOR PACKAGE/ORBIT                   
         ICM   RE,1,BWDKELPO                                                    
         BZ    DSEL4                                                            
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         LA    R4,WRKDLM                                                        
         USING LIST2D,R4                                                        
         UNPK  LISTMISC+3(2),APDUB                                              
         TM    BWDINDS,BWDIORB                                                  
         BO    DSEL2                                                            
         MVC   WRKDAY,=C'--P A C'       PACKAGE                                 
         OI    WRKDAYH+6,FVOXMT                                                 
         MVC   WRKTIM,=C'K A G E--  '                                           
         MVC   LISTMISC(3),=C'PKG'                                              
         B     DSEL6                                                            
*                                                                               
DSEL2    MVC   WRKDAY,=C'--- O R'       ORBIT                                   
         MVC   WRKTIM,=C'B I T ---  '                                           
         MVC   LISTMISC(3),=C'ORB'                                              
         B     DSEL6                                                            
*                                                                               
DSEL4    MVC   APHALF,BDPT                                                      
         GOTO1 AGETDAY,BWDDAYS     DAYS                                         
         MVC   WRKDAY,QDAYS                                                     
*                                                                               
         GOTO1 AGETTIM,BWDTIMES    TIMES                                        
         MVC   WRKTIM,QTIMES                                                    
         MVC   BDPT(2),APHALF                                                   
*                                                                               
DSEL6    XC    WRKCST,WRKCST       COST                                         
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         MVI   EBFLOAT,C'$'                                                     
         MVI   EBDECS,2                                                         
         LA    RF,WRKCST                                                        
         ST    RF,EBAOUT                                                        
         MVI   EBLOUT,L'WRKCST                                                  
         TM    APRECID,RIEFFDT2                                                 
         BZ    *+12                                                             
         LA    RF,BWDCOST2                                                      
         B     DSEL8                                                            
         TM    APRECID,RIEFFDT3                                                 
         BZ    *+12                                                             
         LA    RF,BWDCOST3                                                      
         B     DSEL8                                                            
         LA    RF,BWDCOST1                                                      
*                                                                               
DSEL8    ST    RF,EBAIN                                                         
         MVC   COMDETCS,0(RF)      SAVE DETAIL COST                             
         SR    RE,RE                                                            
         ICM   RF,15,0(RF)                                                      
         BNZ   *+14                                                             
         MVC   WRKCST+4(2),=C'$0'                                               
         B     DSEL16                                                           
         D     RE,=F'100'                                                       
         LTR   RE,RE                                                            
         BZ    DSEL12                                                           
*                                                                               
DSEL10   LA    RE,L'WRKCST-4                                                    
         LTR   RE,RE                                                            
         BNP   DSEL12                                                           
         LA    R1,1                                                             
         MH    R1,=H'10'                                                        
         BCT   RE,*-4                                                           
         CR    RF,R1                                                            
         BL    DSEL14                                                           
*                                                                               
DSEL12   MVI   EBSCIN,X'82'        SCALE PENNIES TO DOLLARS                     
         MVI   EBDECS,0                                                         
*                                                                               
DSEL14   GOTO1 VEDITOR,APPARM,EBLOCK   FORMAT THE COST                          
*                                                                               
DSEL16   OI    WRKCSTH+6,FVOXMT                                                 
*                                                                               
         MVC   WRKPRG,BWDPROG      PROGRAMMING                                  
         OI    WRKPRGH+6,FVOXMT                                                 
*                                                                               
         TM    APRECID,RIEFFDT2    EFFECTIVE DATE                               
         BZ    *+14                                                             
         MVC   APFULL(3),BWDEFDT2                                               
         B     *+18                                                             
         TM    APRECID,RIEFFDT3                                                 
         BZ    DSEL18                                                           
         MVC   APFULL(3),BWDEFDT3                                               
         LA    R4,WRKDLM                                                        
         USING LIST2D,R4                                                        
         GOTO1 VDATCON,APPARM,(3,APFULL),(4,LISTMISC)                           
*                                                                               
DSEL18   LA    R4,BWDEL                                                         
         SR    R0,R0                                                            
*                                                                               
DSEL20   CLI   0(R4),0             RATING                                       
         BE    DSEL28                                                           
         CLI   0(R4),DMOELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DSEL20                                                           
         USING DMOEL,R4                                                         
         LA    R1,DMODEMO          FIND TARGET RATING AND IMPRESSION            
         LA    RE,L'DMODEMO                                                     
         ZIC   RF,1(R4)                                                         
         AR    RF,R4                                                            
         BCTR  RF,0                                                             
         XC    APDUB,APDUB                                                      
         XC    APFULL,APFULL                                                    
         MVI   EBFLOAT,0                                                        
*                                                                               
DSEL22   CLC   1(2,R1),SVDEMO1+1                                                
         BNE   *+10                                                             
         MVC   APFULL,4(R1)        APFULL = TARGET DEMO                         
         CLC   1(2,R1),SVDEMO2+1                                                
         BNE   *+10                                                             
         MVC   APDUB(4),4(R1)      APDUB(4) = TARGET IMPRESSION/RATING          
         BXLE  R1,RE,DSEL22                                                     
*                                                                               
         TM    APFULL,DMODEMOV                                                  
         BZ    *+12                                                             
         MVI   EBFLOAT,C'*'                                                     
         NI    APFULL,FF-DMODEMOV                                               
         LA    RF,APFULL                                                        
         ST    RF,EBAIN                                                         
         MVI   EBLIN,4                                                          
         LA    RF,WRKRAT                                                        
         ST    RF,EBAOUT                                                        
         MVI   EBLOUT,L'WRKRAT                                                  
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         CLI   SVDEMO1+1,C'I'      TEST FOR IMPRESSION                          
         BNE   *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         OI    WRKRATH+6,FVOXMT                                                 
*                                                                               
         OC    COMDETCS,COMDETCS   TEST COST=0                                  
         BZ    DSEL24              YES                                          
         OC    APFULL,APFULL       NO-TEST RATING=0                             
         BZ    DSEL24              YES                                          
         L     R1,COMDETCS         NO-CALCULATE CPP                             
         M     R0,=F'20'                                                        
         D     R0,APFULL                                                        
         LA    R1,1(R1)                                                         
         SRL   R1,1                                                             
         STCM  R1,7,APRECID+1                                                   
         MVI   APFULL,0                                                         
         MVC   APFULL+1(3),APRECID+1                                            
         MVI   EBDECS,2                                                         
         MVI   EBSCIN,0                                                         
         LA    RF,WRKCPP                                                        
         ST    RF,EBAOUT                                                        
         MVI   EBLOUT,L'WRKCPP                                                  
         MVI   EBFLOAT,C'$'                                                     
         GOTO1 VEDITOR,APPARM,EBLOCK     DISPLAY CPP                            
         OI    WRKCPPH+6,FVOXMT                                                 
*                                                                               
DSEL24   TM    SVINDS,SVIDPLN      TEST DPTLEN IN HEADING, NOT CPM              
         BO    DSEL26                                                           
         OC    COMDETCS,COMDETCS                                                
         BZ    DSEL28                                                           
         NI    APDUB,FF-DMODEMOV   NO-DISPLAY CPM                               
         OC    APDUB(4),APDUB                                                   
         BZ    DSEL28                                                           
         L     R1,COMDETCS                                                      
         M     R0,=F'20'                                                        
         D     R0,APDUB                                                         
         LA    R1,1(R1)                                                         
         SRL   R1,1                                                             
         ST    R1,APFULL                                                        
         LA    R4,WRKDLM                                                        
         USING LIST2D,R4                                                        
         LA    RE,LISTCPM                                                       
         ST    RE,EBAOUT                                                        
         MVI   EBLOUT,L'LISTCPM                                                 
         MVI   EBFLOAT,C'$'                                                     
         MVI   EBDECS,2                                                         
         MVI   EBSCIN,0                                                         
         CLC   APFULL,=F'10000'                                                 
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'82'                                                     
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         B     DSEL28                                                           
*                                                                               
DSEL26   LA    R4,WRKDLM           DISPLAY DPTLEN INSTEAD OF CPM                
         USING LIST2D,R4                                                        
         MVC   LISTCPM(1),BWDDPT                                                
         ZIC   RE,BWDSLN                                                        
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  LISTCPM+1(3),APDUB                                               
         CLI   LISTCPM+1,C'0'                                                   
         BNE   DSEL28                                                           
         MVC   LISTCPM+1(2),LISTCPM+2                                           
         MVI   LISTCPM+3,C' '                                                   
*                                                                               
DSEL28   LA    R4,WRKDLM                                                        
         USING LIST2D,R4                                                        
         CLI   BWDSUBDP,0          DISPLAY SUB-DAYPART IF POSS                  
         BE    DSEL30                                                           
         CLC   LISTMISC,BLANKS                                                  
         BH    DSEL30                                                           
         MVC   LISTMISC(4),=C'DPT='                                             
         MVC   LISTMISC+4(1),BWDSUBDP                                           
*                                                                               
DSEL30   CLI   BWDADJ,0            TEST PROGRAM ADJACENCY CODE                  
         BE    DSEL32                                                           
         MVC   LISTMISC(3),=C'AJ=' YES-DISPLAY IT IN MISC FIELD                 
         MVC   LISTMISC+3(1),BWDADJ                                             
         MVI   LISTMISC+4,C' '                                                  
         CLI   BWDADJ,C'A'         TEST ALPHA                                   
         BNL   DSEL32                                                           
         UNPK  APFULL(3),BWDADJ(2) NO-THEN NUMERIC                              
         MVC   LISTMISC+3(2),APFULL                                             
*                                                                               
DSEL32   CLI   BWDSTA,C'0'         TEST CABLE STATION                           
         BL    DSELX                                                            
         MVC   LISTMISC,BLANKS     YES-DISPLAY NETWORK IN MISC FIELD            
         MVC   LISTMISC(3),BWDSTA+5                                             
*                                                                               
DSELX    B     COMMONX                                                          
         DROP  R4,R8                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE LIST/SELECT DATA -- PART 1                                 *         
* INPUT  : COMATWAL   = A(TWA DISPLAY LINE)                           *         
*          APRECID(1) = RECORD IDENTIFICATION BYTE                    *         
*          IOAREA2 CONTAINS DETAIL RECORD                             *         
* OUTPUT : COMCHG = RECORD CHANGE INDICATORS                          *         
***********************************************************************         
         SPACE 1                                                                
VALSEL1  L     R2,COMATWAL                                                      
         USING WRKL1H,R2                                                        
         L     R3,AIOAREA2                                                      
         USING BWDRECD,R3                                                       
         CLI   APRECNUM,RECSID     TEST NSID RECORD                             
         BNE   VSEL1                                                            
         TM    INOIND,INOINOT      AND NOTRANS OPTION SET                       
         BZ    VSEL1                                                            
         LA    RF,WRKL2H           YES-FORCE ALL UNPROTECTED FIELDS             
         LR    R1,R2                   TO NOT PREVIOUSLY VALIDATED, SO          
         SR    RE,RE                   THAT ALL CHANGES TO THIS LINE            
*                                      WILL BE RECOGNIZED                       
VSEL0    IC    RE,0(R1)                                                         
         AR    R1,RE                                                            
         CR    R1,RF                                                            
         BE    VSEL1                                                            
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BO    *+8                                                              
         NI    FVIIND-FVIHDR(R1),255-FVIVAL                                     
         B     VSEL0                                                            
*                                                                               
VSEL1    MVI   COMCHG,0                                                         
         XC    COMCPPC,COMCPPC     CLEAR COST FOR CPP CALCULATION               
         XC    COMCPPD,COMCPPD           DEMO                                   
         XC    COMSLNS,COMSLNS     CLEAR MULTIPLE SPOT LENGTHS                  
*                                                                               
         TM    WRKPRGH+FVIIND-FVIHDR,FVIVAL   PROGRAMMING                       
         BO    VSEL6                                                            
         GOTO1 AFVAL,WRKPRGH                                                    
         BH    VSELX                                                            
         BL    VSEL6               PROGRAM BLANK - IGNORE IT                    
         CLI   FVIFLD,C'='         TEST PROGRAM ADJACENCY CODE                  
         BNE   VSEL2                                                            
         GOTO1 AVALADJ,FVIFLD+1    VALIDATE PROGRAM ADJACENCY CODE              
         BNE   VSELX                                                            
         CLC   BWDADJ,ADJCODE      VALID - TEST CHANGE                          
         BE    VSEL6                                                            
         MVC   BWDADJ,ADJCODE      YES - SET ADJACENCY CODE                     
         OI    COMCHG,LPROG              INDICATE CHANGE (LIKE PROGRAM)         
         B     VSEL6                                                            
*                                                                               
VSEL2    CLC   FVIFLD(4),=C'SLN='    TEST SPOT LENGTHS FOR SID TRANSFER         
         BNE   VSEL4                                                            
         BAL   RE,VALSLNS            YES-VALIDATE                               
         BNE   VSELX                                                            
         OI    COMCHG,LPROG          INDICATE CHANGE (LIKE PROGRAM)             
         TM    TWAACTIV,TWAACPFK     TEST PF KEY SLN TRANSFER                   
         BZ    VSEL6                                                            
         OC    SVPROG,SVPROG         YES-TEST PROGRAM NAME OVERRIDE             
         BZ    VSEL6                                                            
         MVC   FVIFLD(L'SVPROG),SVPROG   YES-PUT PROGRAM NAME BACK              
         MVC   WRKPRG,SVPROG                                                    
         OI    WRKPRGH+6,FVOXMT                                                 
         XC    SVPROG,SVPROG                                                    
         B     VSEL5                                                            
*                                                                               
VSEL4    BAL   RE,VALCAMPS         VALIDATE FOR CAMPAIGNS=                      
         BNE   VSELX                                                            
         OC    COMCOLNS,COMCOLNS                                                
         BZ    VSEL5                                                            
         OI    COMCHG,LCOPY        YES-INDICATE COPY TO OTHER CAMPAIGNS         
         B     VSEL6                                                            
*                                                                               
VSEL5    CLC   BWDPROG(L'WRKPRG),FVIFLD      TEST CHANGE IN PROGRAM             
         BE    VSEL6                                                            
         MVC   BWDPROG,FVIFLD      YES - CHANGE THE PROGRAM                     
         OI    COMCHG,LPROG              INDICATE PROGRAM CHANGE                
         OI    BWDINDS,BWDIPRG           PROGRAM OVERRIDE                       
*                                                                               
VSEL6    TM    WRKRATH+FVIIND-FVIHDR,FVIVAL   RATING                            
         BO    VSEL40                                                           
         NI    LFLAG,FF-LDEMUP                                                  
         GOTO1 AFVAL,WRKRATH       VALIDATE RATING FIELD                        
         BH    VSELX                                                            
         BL    *+12                TEST MISSING                                 
         CLI   FVIFLD,C'X'         OR 'X' IN FIRST POSITION                     
         BNE   *+12                                                             
         OI    LFLAG,LDEMUP        YES-REQUIRE UPGRADE                          
         B     VSEL9                                                            
         ZIC   RE,FVILEN           SEARCH FIELD FOR *                           
         LR    R0,RE                                                            
         LA    R1,FVIFLD                                                        
*                                                                               
VSEL7    CLI   0(R1),C'*'                                                       
         BNE   *+12                                                             
         MVI   0(R1),C' '          FOUND-BLANK IT OUT                           
         B     VSEL8                                                            
         CLI   0(R1),C' '                                                       
         BH    VSEL8                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,VSEL7                                                         
*                                                                               
VSEL8    ST    RE,APPARM+4                                                      
         GOTO1 VCASHVAL,APPARM,(1,FVIFLD)                                       
         CLI   APPARM,FF                                                        
         BE    VSEL99                                                           
         MVC   LNEWRTG,APPARM+4    SAVE NEW RATING                              
         DROP  R2                                                               
*                                                                               
VSEL9    LA    R4,BWDEL            LOOK FOR DEMO ELEMENT                        
         USING DMOEL,R4                                                         
         SR    R0,R0                                                            
*                                                                               
VSEL10   CLI   0(R4),0                                                          
         BNE   VSEL11                                                           
         XC    APELEM,APELEM       NOT FOUND --                                 
         LA    R4,APELEM           ADD NEW DEMO ELEMENT                         
         MVI   DMOELCD,DMOELCDQ                                                 
         MVI   DMOELLN,L'DMODEMO+2                                              
         LA    R4,DMODEMO                                                       
         MVC   0(3,R4),ESTDEMS                                                  
         XC    APFULL,APFULL                                                    
         B     VSEL30                                                           
*                                                                               
VSEL11   CLI   0(R4),DMOELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VSEL10                                                           
         ST    R4,LADEMEL          SAVE A(DEMO ELEMENT)                         
         LR    R1,R4                                                            
         LA    R4,DMODEMO                                                       
******** OC    INORTG,INORTG       TEST FOR OVERRIDE TARGET DEMO                
******** BZ    VSEL12                                                           
*                                  ** ALWAYS FIND CORRECT DEMO NOW **           
         LA    RE,L'DMODEMO        YES - GET OVERRIDE DEMO VALUE                
         ZIC   RF,1(R1)                                                         
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
******** CLC   1(2,R4),INORTG+1                                                 
         CLC   1(2,R4),SVDEMO1+1                                                
         BE    VSEL12                                                           
         BXLE  R4,RE,*-10                                                       
*                                                                               
         XC    APELEM,APELEM       DEMO NOT FOUND IN ELEMENT                    
         ZIC   RF,1(R1)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(R1)     SAVE CURRENT ELEMENT                         
         GOTO1 ADELELS,BWDRECD     DELETE IT FROM RECORD                        
         ZIC   RF,APELEM+1                                                      
         LA    RF,L'DMODEMO(RF)    LENGTHEN THE ELEMENT BY ONE DEMO             
         STC   RF,APELEM+1                                                      
         GOTO1 AADDELS,BWDRECD     ADD BACK THE ELEMENT                         
******** MVC   0(3,R4),INORTG      MOVE NEW DEMO INTO THE ELEMENT               
         MVC   0(3,R4),SVDEMO1     MOVE NEW DEMO INTO THE ELEMENT               
*                                                                               
VSEL12   MVC   APFULL,4(R4)        SAVE OLD DEMO VALUE IN APFULL                
         NI    APFULL,FF-DMODEMOV                                               
         TM    LFLAG,LDEMUP        TEST DEMO UPGRADE REQUIRED                   
         BZ    VSEL28                                                           
         MVC   LDEMS(3),0(R4)      YES - DO THE UPGRADE                         
         MVI   LDEMS+3,FF                                                       
         BAL   RE,DEMUP                                                         
         BNE   VSELX                                                            
         MVC   LNEWRTG,LDEMVALS    SAVE NEW DEMO VALUE                          
         TM    CMPOPTS,CAMOAIMP+CAMOAALL+CAMOATGT  TEST AUTO DEMO               
         BZ    VSEL30                              ADJUSTMENTS                  
         CLI   LDEMS+1,C'R'        YES - TEST RATING WAS CHANGED                
         BE    *+12                                                             
         CLI   LDEMS+1,C'E'                                                     
         BNE   VSEL30                                                           
         NI    LFLAG,FF-LADJALL    YES -                                        
         TM    CMPOPTS,CAMOAALL+CAMOATGT   TEST ALL OR TGT                      
         BZ    VSEL13                                                           
         MVI   APBYTE,1            YES-TEST WHETHER THE ADJUSTMENT DEMO         
         TM    CMPOPTS,CAMOAALL        WAS CHANGED                              
         BO    *+10                                                             
         MVC   APBYTE,ESTDEMS+2                                                 
         CLC   APBYTE,LDEMS+2                                                   
         BNE   VSEL13                                                           
         OI    LFLAG,LADJALL       YES - INDICATE TO ADJUST ALL                 
*                                                                               
VSEL13   L     R8,LADEMEL                                                       
         LA    R0,L'DMODEMO                                                     
         ZIC   R1,1(R8)                                                         
         AR    R1,R8                                                            
         BCTR  R1,0                                                             
         LA    R8,DMODEMO-DMOEL(R8)                                             
         LA    RE,LDEMFRZ          BUILD LIST OF IMPS TO BE FROZEN              
*                                                                               
VSEL14   TM    4(R8),DMODEMOV                                                   
         BZ    VSEL16                                                           
         CLC   1(2,R8),1(R4)                                                    
         BE    VSEL16                                                           
         CLI   1(R8),C'R'                                                       
         BE    *+12                                                             
         CLI   1(R8),C'E'                                                       
         BNE   VSEL16                                                           
         MVI   0(RE),C'I'                                                       
         MVC   1(1,RE),2(R8)                                                    
         LA    RE,2(RE)                                                         
*                                                                               
VSEL16   BXLE  R8,R0,VSEL14                                                     
         MVI   0(RE),FF                                                         
*                                                                               
         L     R8,LADEMEL                                                       
         LA    R8,DMODEMO-DMOEL(R8)                                             
         LA    RF,LDEMS                                                         
         MVI   0(RF),FF            BUILD LIST OF DEMOS FOR UPGRADE              
*                                                                               
VSEL18   CLC   1(2,R8),1(R4)       TEST THIS IS TARGET DEMO                     
         BE    VSEL24                                                           
         TM    4(R8),DMODEMOV      NO - TEST DEMO HAS MANUAL OVERRIDE           
         BO    VSEL24                                                           
         TM    LFLAG,LADJALL       NO - TEST TO ADJUST ALL                      
         BZ    VSEL22                                                           
         LA    RE,LDEMFRZ          YES - TEST THIS DEMO TO BE FROZEN            
*                                                                               
VSEL20   CLI   0(RE),FF                                                         
         BE    VSEL23              NO                                           
         CLC   1(2,R8),0(RE)                                                    
         BE    VSEL24              YES                                          
         LA    RE,2(RE)                                                         
         B     VSEL20                                                           
*                                                                               
VSEL22   CLI   1(R8),C'I'          AUTOADJ=IMP - TEST THIS IS IMP               
         BNE   VSEL24                            WE WANT                        
         CLC   2(1,R8),2(R4)                                                    
         BNE   VSEL24                                                           
*                                                                               
VSEL23   MVC   0(3,RF),0(R8)       ADD DEMO TO LIST                             
         LA    RF,3(RF)                                                         
         MVI   0(RF),FF                                                         
*                                                                               
VSEL24   BXLE  R8,R0,VSEL18                                                     
*                                                                               
         CLI   LDEMS,FF            TEST UPGRADE NEEDED                          
         BE    VSEL30                                                           
         BAL   RE,DEMUP            YES - DO THE UPGRADE                         
         BNE   VSELX                                                            
         L     R8,LADEMEL                                                       
         LA    R8,DMODEMO-DMOEL(R8)                                             
         LA    RE,LDEMVALS                                                      
         LA    RF,LDEMS            MOVE DEMO VALS TO DEMO ELEMENT               
*                                                                               
VSEL25   CLC   1(2,R8),1(RF)                                                    
         BNE   VSEL26                                                           
         MVC   4(4,R8),0(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    RF,3(RF)                                                         
         CLI   0(RF),FF                                                         
         BE    VSEL30                                                           
*                                                                               
VSEL26   BXLE  R8,R0,VSEL25                                                     
         B     VSEL30                                                           
*                                                                               
VSEL28   CLC   APFULL,LNEWRTG      TEST FOR DEMO VALUE CHANGE                   
         BE    VSEL40                                                           
         TM    INFIND,INFINOAD     YES-TEST OPTION TO NOT AUTOADJ               
         BO    VSEL30                                                           
         TM    CMPOPTS,CAMOAIMP+CAMOAALL+CAMOATGT  NO-TEST FOR AUTO             
         BZ    VSEL30                                 DEMO ADJUST               
         OC    APFULL,APFULL              YES - TEST OLD RATING = 0             
         BZ    VSEL97                           YES - ERROR                     
         BAL   RE,DEMADJ                        NO  - AUTO ADJUST               
*                                                                               
VSEL30   CLI   APACTN,ACTSKD       TEST FOR SCHEDULE                            
         BNE   VSEL36                                                           
         L     RE,LNEWRTG          YES - CALCULATE NEW ACTUAL POINTS            
         S     RE,APFULL                                                        
         LA    R8,BWDEL                                                         
         SR    R0,R0                                                            
*                                                                               
VSEL32   CLI   0(R8),0                                                          
         BE    VSEL36                                                           
         CLI   0(R8),SPWELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     VSEL32                                                           
         LA    R2,SVACPTS                                                       
         ZIC   RF,CMPNWKS                                                       
         ZIC   R1,1(R8)                                                         
         SH    R1,=Y(SPWPERWK-SPWEL)                                            
         CR    R1,RF                                                            
         BNL   *+6                                                              
         LR    RF,R1                                                            
         LA    R8,SPWPERWK-SPWEL(R8)                                            
*                                                                               
VSEL34   ZIC   R1,0(R8)                                                         
         SR    R0,R0                                                            
         MR    R0,RE               SPOTS X CHANGE IN RATING                     
         L     R0,0(R2)                                                         
         AR    R0,R1                                                            
         ST    R0,0(R2)                                                         
         LA    R8,1(R8)                                                         
         LA    R2,4(R2)                                                         
         BCT   RF,VSEL34                                                        
*                                                                               
         GOTO1 CFMACPTS,APPARM,WRKL8H+ACDSPL,SVGVA   FMT NEW ACTUAL PTS         
*                                                                               
VSEL36   OI    COMCHG,LDEMO        INDICATE RATING CHANGE                       
         MVC   COMCPPD,LNEWRTG     SAVE RATING                                  
         MVC   4(4,R4),LNEWRTG     CHANGE RATING IN RECORD                      
         TM    LFLAG,LDEMUP        IF UPGRADE, NOT OVERRIDE                     
         BO    *+8                                                              
         OI    4(R4),DMODEMOV      INDICATE OVERRIDE                            
         LA    R1,APELEM+DMODEMO-DMOEL                                          
         CR    R1,R4               TEST ADDING NEW DEMO ELEMENT                 
         BNE   VSEL40                                                           
         GOTO1 AADDELS,BWDRECD                                                  
*                                                                               
VSEL40   NI    LFLAG,FF-LDEMUP                                                  
         L     R2,COMATWAL                                                      
         USING WRKL1H,R2                                                        
         TM    WRKDAYH+FVIIND-FVIHDR,FVIVAL   DAYS                              
         BO    VSEL42                                                           
         CLI   BWDKELPO,0          DAYS NOT VALID FOR PACKAGE/ORBIT             
         BNE   VSEL42                                                           
         MVC   APHALF,BDPT         SAVE DPT/SLN                                 
         GOTO1 AVALDAY,WRKDAYH                                                  
         BNE   VSELX                                                            
         MVC   BDPT(2),APHALF      RESTORE DPT/SLN                              
         CLC   BWDDAYS,BDAYS       TEST CHANGE OF DAYS                          
         BE    VSEL42                                                           
         OI    LFLAG,LDEMUP        YES - UPGRADE REQUIRED                       
         MVC   BWDDAYS,BDAYS                                                    
         OI    COMCHG,LDAYTIM+LDEMO      ASSUME DEMO CHANGE                     
*                                                                               
VSEL42   TM    WRKTIMH+FVIIND-FVIHDR,FVIVAL   TIMES                             
         BO    VSEL44                                                           
         CLI   BWDKELPO,0          TIMES NOT VALID FOR PACKAGE/ORBIT            
         BNE   VSEL44                                                           
         MVC   APHALF,BDPT         SAVE DPT/SLN                                 
         GOTO1 AVALTIM,WRKTIMH                                                  
         BNE   VSELX                                                            
         MVC   BDPT(2),APHALF      RESTORE DPT/SLN                              
         CLC   BWDTIMES,BTIMES     TEST FOR TIMES CHANGE                        
         BE    VSEL44                                                           
         OI    LFLAG,LDEMUP        YES - UPGRADE REQUIRED                       
         MVC   BWDTIMES,BTIMES                                                  
         MVC   BWDTIMCD,PTIMES                                                  
         OI    COMCHG,LDAYTIM+LDEMO      ASSUME DEMO CHANGE                     
*                                                                               
VSEL44   TM    LFLAG,LDEMUP        TEST UPGRADE REQUIRED                        
         BZ    VSEL70                                                           
         TM    BWDINDS,BWDIPRG     YES-TEST FOR PROGRAM OVERRIDE                
         BO    *+8                                                              
         BAL   RE,GETPROG          NO-GET PROGRAM NAME                          
         LA    R4,BWDEL                                                         
         SR    R0,R0               FIND THE DEMO ELEMENT                        
*                                                                               
VSEL46   CLI   0(R4),0                                                          
         BE    VSEL70                                                           
         CLI   0(R4),DMOELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VSEL46                                                           
         USING DMOEL,R4                                                         
         LA    R0,L'DMODEMO        GET READY FOR BXLE                           
         ZIC   R1,1(R4)                                                         
         AR    R1,R4                                                            
         BCTR  R1,0                                                             
         LA    RE,LDEMFRZ                                                       
         XC    LDEMFRZ,LDEMFRZ                                                  
         TM    CMPOPTS,CAMOAIMP+CAMOAALL+CAMOATGT TEST AUTO-ADJUSTMENTS         
         BZ    VSEL52                                                           
         LA    R8,DMODEMO          YES - DETERMINE WHICH DEMOS SHOULD           
*                                        BE FROZEN                              
VSEL48   TM    4(R8),DMODEMOV      TEST DEMO OVERRIDE                           
         BZ    VSEL50                                                           
         CLI   1(R8),C'R'          YES - TEST FOR RATING                        
         BE    *+12                                                             
         CLI   1(R8),C'E'                                                       
         BNE   VSEL50                                                           
         TM    CMPOPTS,CAMOAALL+CAMOATGT  TEST AUTOADJ=ALL/TGT                  
         BZ    VSEL49                                                           
         MVI   APBYTE,1            YES-                                         
         TM    CMPOPTS,CAMOAALL                                                 
         BO    *+10                                                             
         MVC   APBYTE,ESTDEMS+2                                                 
         CLC   APBYTE,2(R8)        TEST ADJUSTMENT DEMO                         
         BE    VSEL70              YES - DON'T CHANGE ANY DEMOS                 
*                                                                               
VSEL49   MVI   0(RE),C'I'          FREEZE THE IMPRESSION                        
         MVC   1(1,RE),2(R8)                                                    
         LA    RE,2(RE)                                                         
*                                                                               
VSEL50   BXLE  R8,R0,VSEL48        DO FOR ALL DEMOS                             
*                                                                               
VSEL52   MVI   0(RE),FF                                                         
         XC    LDEMS,LDEMS         BUILD DEMO LIST FOR UPGRADE                  
         LA    RF,LDEMS                                                         
         LA    R8,DMODEMO                                                       
*                                                                               
VSEL54   TM    4(R8),DMODEMOV      TEST OVERRIDE                                
         BO    VSEL60              YES - NO UPGRADE FOR IT                      
         LA    RE,LDEMFRZ          NO - SEE IF DEMO SHOULD BE FROZEN            
*                                                                               
VSEL56   CLI   0(RE),FF                                                         
         BE    VSEL58                                                           
         CLC   0(2,RE),1(R8)                                                    
         BE    VSEL60              YES                                          
         LA    RE,2(RE)                                                         
         B     VSEL56                                                           
*                                                                               
VSEL58   MVC   1(2,RF),1(R8)       NO - ADD TO LIST                             
         LA    RF,3(RF)                                                         
*                                                                               
VSEL60   BXLE  R8,R0,VSEL54        DO FOR ALL DEMOS                             
*                                                                               
         MVI   0(RF),FF                                                         
         CLI   LDEMS,FF            TEST UPGRADE NEEDED                          
         BE    VSEL70                                                           
         BAL   RE,DEMUP            YES - DO THE UPGRADES                        
         BNE   VSELX                                                            
         LA    R8,DMODEMO          MOVE DEMO VALUES INTO DEMO ELEMENT           
         LA    RE,LDEMS                                                         
         LA    RF,LDEMVALS                                                      
*                                                                               
VSEL62   CLI   0(RE),FF                                                         
         BE    VSEL70                                                           
         CLC   1(2,R8),1(RE)                                                    
         BNE   VSEL64                                                           
         MVC   4(4,R8),0(RF)                                                    
         LA    RE,3(RE)                                                         
         LA    RF,4(RF)                                                         
*                                                                               
VSEL64   BXLE  R8,R0,VSEL62        DO FOR ALL DEMOS                             
*                                                                               
VSEL70   TM    WRKCSTH+FVIIND-FVIHDR,FVIVAL   COST                              
         BO    VSEL85                                                           
         XC    LNEWCOST,LNEWCOST                                                
         GOTO1 AFVAL,WRKCSTH                                                    
         BH    VSELX                                                            
         BL    VSEL72                                                           
         DROP  R2                                                               
*                                                                               
         ZIC   RE,FVILEN           REMOVE THE * IF ANY                          
         LA    R1,FVIFLD                                                        
         CLI   0(R1),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         BCT   RE,*-16                                                          
*                                                                               
         ZIC   RE,FVILEN                                                        
         ST    RE,APPARM+4                                                      
         GOTO1 VCASHVAL,APPARM,FVIFLD                                           
         CLI   APPARM,FF                                                        
         BE    VSEL98                                                           
         MVC   LNEWCOST,APPARM+4                                                
         GOTO1 ANETCOST,LNEWCOST   NET DOWN IF NECESSARY                        
*                                                                               
VSEL72   TM    APRECID,RIEFFDT2                                                 
         BZ    *+12                                                             
         LA    RF,BWDCOST2                                                      
         B     VSEL73                                                           
         TM    APRECID,RIEFFDT3                                                 
         BZ    *+12                                                             
         LA    RF,BWDCOST3                                                      
         B     VSEL73                                                           
         LA    RF,BWDCOST1                                                      
*                                                                               
VSEL73   CLC   0(4,RF),LNEWCOST    TEST CHANGE IN COST                          
         BNE   *+16                                                             
         CLI   APRECNUM,RECSID     NO-TEST NSID RECORD                          
         BE    VSEL82              YES-FORCE COST CHANGE FOR TRANSFER           
         B     VSEL85                                                           
         ST    RF,LFULL                                                         
         CLI   APACTN,ACTSKD       YES - TEST FOR SCHEDULE                      
         BNE   VSEL80                                                           
         L     R1,LNEWCOST               YES - CALCULATE NEW COST               
         ICM   R0,15,0(RF)                                                      
         SR    R1,R0                                                            
         ST    R1,APFULL           APFULL = DIFFERENCE IN COST                  
         LA    R8,BWDEL                                                         
         SR    R0,R0                                                            
*                                                                               
VSEL74   CLI   0(R8),0                                                          
         BE    VSEL80                                                           
         CLI   0(R8),SPWELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     VSEL74                                                           
         ZIC   R4,1(R8)                                                         
         AR    R4,R8                                                            
         ZIC   RF,CMPNWKS                                                       
         LA    RF,SPWPERWK-SPWEL(RF,R8)                                         
         CR    RF,R4                                                            
         BNL   *+6                                                              
         LR    R4,RF                                                            
         LA    R8,SPWPERWK-SPWEL(R8)                                            
         LA    R1,APRECKEY+20                                                   
         L     R0,SVACDOL                                                       
*                                                                               
VSEL76   CLI   0(R1),FF                                                         
         BE    VSEL78                                                           
         TM    0(R1),X'80'         TEST OUT OF DATE BOUNDS                      
         BO    VSEL77                                                           
         ZIC   RF,0(R8)                                                         
         M     RE,APFULL           SPOTS X DIFFERENCE IN COST                   
         AR    R0,RF                                                            
*                                                                               
VSEL77   LA    R8,1(R8)                                                         
         CR    R8,R4                                                            
         BNL   VSEL78                                                           
         LA    R1,1(R1)                                                         
         B     VSEL76                                                           
*                                                                               
VSEL78   ST    R0,SVACDOL                                                       
         GOTO1 CFMACDOL,APPARM,WRKL8H+ACDSPL,SVGVA  FMT NEW TOTAL COST          
*                                                                               
VSEL80   L     RF,LFULL                                                         
         MVC   0(4,RF),LNEWCOST                                                 
*                                                                               
VSEL82   OI    COMCHG,LCOST                                                     
         MVC   COMCPPC,LNEWCOST                                                 
*                                                                               
VSEL85   B     VSELX                                                            
*                                                                               
VSEL97   MVC   FVMSGNO,=AL2(FVIDADJ) AUT DEMO ADJUST MUST BE TURNED OFF         
         B     VSELX                                                            
*                                                                               
VSEL98   MVC   FVMSGNO,=AL2(FVICST)   INVALID COST                              
         B     VSELX                                                            
*                                                                               
VSEL99   MVC   FVMSGNO,=AL2(FVFNOTV)  INVALID FIELD                             
*                                                                               
VSELX    B     COMMONX                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE SPOTLENGTHS FOR MULTIPLE LENGTH NSID TRANSFER              *         
***********************************************************************         
         SPACE 1                                                                
VALSLNS  NTR1                                                                   
         CLI   APRECNUM,RECSID     CHECK FOR NSID RECORD                        
         BNE   VSLN8                                                            
         CLI   BSLN,0              AND LIST IS FOR ALL SPOT LENGTHS             
         BNE   VSLN8                                                            
         CLI   INOSLN,0            AND SLN OPTION NOT SET                       
         BNE   VSLN8                                                            
         CLI   FVILEN,5            AT LEAST ONE LENGTH                          
         BL    VSLN8                                                            
         GOTO1 VSCANNER,APPARM,FVIHDR,(4,AIOAREA3),C',=/='                      
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BZ    VSLN8                                                            
         LA    R4,COMSLNS                                                       
         L     R8,AIOAREA3                                                      
         TM    3(R8),X'80'         TEST NUMERIC                                 
         BZ    VSLN8                                                            
         LA    RE,8(R8)                                                         
         LA    R2,1                                                             
         B     VSLN4                                                            
*                                                                               
VSLN2    CLI   1(R8),0                                                          
         BNE   VSLN8                                                            
         TM    2(R8),X'80'                                                      
         BZ    VSLN8                                                            
         LA    RE,4(R8)                                                         
*                                                                               
VSLN4    OC    0(3,RE),0(RE)                                                    
         BNZ   VSLN9                                                            
         LA    R1,SLNTAB                                                        
*                                                                               
VSLN6    CLI   0(R1),0                                                          
         BE    VSLN9                                                            
         CLC   0(1,R1),3(RE)                                                    
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     VSLN6                                                            
         MVC   0(1,R4),3(RE)                                                    
         LA    R4,1(R4)                                                         
         LA    R8,32(R8)                                                        
         LA    R2,1(R2)                                                         
         BCT   R0,VSLN2                                                         
*                                                                               
         TM    INOIND,INOINOT      TEST NO TRANSFER                             
         BZ    VSLNX                                                            
         CLI   COMSLNS+1,0         YES-TEST MORE THAN ONE LENGTH                
         BE    VSLNX                                                            
         MVC   FVMSGNO,=AL2(FVONESLN)  YES-ERROR                                
         B     VSLNX                                                            
*                                                                               
VSLN8    MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VSLNX                                                            
*                                                                               
VSLN9    MVC   FVMSGNO,=AL2(FVISLN)                                             
         STC   R2,FVINDX                                                        
*                                                                               
VSLNX    B     COMMONX                                                          
         SPACE 2                                                                
SLNTAB   DS    0XL1                                                             
         DC    AL1(5,10,15,20,30,40,45,50,60,75,90,120),AL1(0)                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE PROGRAM FIELD FOR 'CAMPAIGNS='                             *         
* OUTPUT : COMCOLNS=TABLE OF 3-BYTE ENTRIES: CAMP-MKT SEQ NO (2)      *         
*                                            STATION CODE (1)         *         
***********************************************************************         
         SPACE 1                                                                
VALCAMPS NTR1  ,                                                                
         MVC   APWORK(8),BWDSTA    SAVE THE STATION IN APWORK                   
         LA    R3,COMCOLNS         CLEAR TABLE OF CAMPAIGN COPY LINES           
         XC    0(MAXCOPYL*L'COMCOLNS,R3),0(R3)                                  
         GOTO1 VSCANNER,APPARM,FVIHDR,AIOAREA4,C',=,='                          
         SR    R8,R8                                                            
         ICM   R8,1,4(R1)                                                       
         BZ    VCAMX                                                            
         L     R4,AIOAREA4                                                      
         ZIC   RE,0(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,VCAMINP1                                                      
         BNE   VCAMX                                                            
         SR    RE,RE                                                            
         ICM   RE,1,1(R4)                                                       
         BZ    VCAMX                                                            
         CLI   APRECNUM,RECSID     INVALID FOR SID RECORD                       
         BE    VCAM96                                                           
         L     R1,AIOAREA2         INVALID FOR PACKAGE/ORBIT                    
         CLI   BWDKELPO-BWDKEY(R1),0                                            
         BNE   VCAM96                                                           
         LA    R0,MAXCOPYL                                                      
         LA    R1,22(R4)                                                        
         LA    R2,1                                                             
         LA    R5,3(R4)                                                         
         MVC   APFULL,8(R4)                                                     
*                                                                               
VCAM2    XC    APHALF,APHALF                                                    
         LA    R6,CAMKCAM-CAMKEY-1                                              
         BCTR  RE,0                                                             
         EX    RE,VCAMINP2         TEST LATEST CAMPAIGN                         
         BE    VCAM4                                                            
         EX    RE,VCAMINP3                                                      
         BE    VCAM4                                                            
         TM    0(R5),X'80'         TEST CAMPAIGN IS NUMERIC                     
         BZ    VCAM94                                                           
         OC    APFULL(2),APFULL                                                 
         BNZ   VCAM94                                                           
         MVC   APHALF,APFULL+2                                                  
         XC    APHALF,EFFS                                                      
         LA    R6,L'CAMKCAM(R6)                                                 
*                                                                               
VCAM4    LA    R5,IOKEY            VALIDATE CAMPAIGN                            
         USING CAMRECD,R5                                                       
         XC    CAMKEY,CAMKEY                                                    
         MVI   CAMKTYP,CAMKTYPQ                                                 
         MVI   CAMKSUB,CAMKSUBQ                                                 
         MVC   CAMKAGMD,BAGYMD                                                  
         MVC   CAMKBYR,BBYR                                                     
         MVC   CAMKCAM,APHALF                                                   
         GOTO1 AIO,DIRHI                                                        
         BNE   VCAM94                                                           
         EX    R6,VCAMKCOM                                                      
         BNE   VCAM94                                                           
         MVC   APHALF,CAMKCAM                                                   
         LA    R5,IOKEY            READ CAMPAIGN/MARKET HEADER                  
         USING BWHRECD,R5                                                       
         XC    BWHKEY,BWHKEY                                                    
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVC   BWHKAGMD,BAGYMD                                                  
         MVC   BWHKBYR,BBYR                                                     
         MVC   BWHKCAM,APHALF                                                   
         MVC   BWHKMKT,BMKT                                                     
         GOTO1 AIO,DIRHI+IO1                                                    
         BNE   VCAM92                                                           
         CLC   BWHKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                  
         BNE   VCAM92                                                           
         GOTO1 AIO,FILGET1                                                      
         BNE   VCAM92                                                           
         L     R5,AIOAREA1                                                      
         MVC   0(2,R3),BWHKSEQ     SAVE CAMPAIGN/MARKET SEQ NO                  
         LA    R1,BWHFSTEL                                                      
         SR    RF,RF               GET STATION CODE                             
*                                                                               
VCAM6    CLI   0(R1),0                                                          
         BE    VCAM92                                                           
         CLI   0(R1),BWHELCDQ                                                   
         BNE   *+14                                                             
         USING BWHEL,R1                                                         
         CLC   BWHSTA,APWORK                                                    
         BE    VCAM8                                                            
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     VCAM6                                                            
*                                                                               
VCAM8    MVC   2(1,R3),BWHSEQ      SAVE STATION CODE                            
         LA    R2,1(R2)                                                         
         LA    R4,32(R4)                                                        
         BCT   R8,*+8              NEXT COPY CAMPAIGN                           
         B     VCAMX                                                            
         LA    R3,3(R3)                                                         
         BCT   R0,*+8                                                           
         B     VCAM90                                                           
         CLI   1(R4),0                                                          
         BNE   VCAM94                                                           
         ZIC   RE,0(R4)                                                         
         LA    R1,12(R4)                                                        
         LA    R5,2(R4)                                                         
         MVC   APFULL,4(R4)                                                     
         B     VCAM2                                                            
*                                                                               
VCAM90   MVC   FVMSGNO,=AL2(FVTMCC)                                             
         B     VCAM99                                                           
*                                                                               
VCAM92   MVC   FVMSGNO,=AL2(FVWRKNF)   WORK RECORD NOT FOUND                    
         B     VCAM99                                                           
*                                                                               
VCAM94   MVC   FVMSGNO,=AL2(FVICAM)    INVALID CAMPAIGN                         
         B     VCAM99                                                           
*                                                                               
VCAM96   MVC   FVMSGNO,=AL2(FVFNOTV)   INVALID INPUT FIELD                      
         B     VCAMX                                                            
*                                                                               
VCAM99   STC   R2,FVINDX                                                        
*                                                                               
VCAMX    B     COMMONX                                                          
         DROP  R5                                                               
         SPACE 2                                                                
VCAMINP1 CLC   12(0,R4),=C'CAMPAIGNS'                                           
VCAMINP2 CLC   0(0,R1),=C'LAST  '                                               
VCAMINP3 CLC   0(0,R1),=C'LATEST'                                               
VCAMKCOM CLC   IOKEY(0),IOKEYSAV                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE LIST/SELECT DATA -- PART 2                                 *         
* INPUT  : COMCHG = RECORD CHANGE INDICATORS                          *         
***********************************************************************         
         SPACE 1                                                                
VALSEL2  L     R3,AIOAREA2                                                      
         USING BWDRECD,R3                                                       
         TM    COMCHG,LHDR         TEST FOR CHANGE TO HEADER                    
         BZ    VASL2                                                            
         MVC   IODA,HDRDA         YES-GET HEADER RECORD                         
         MVC   IOADDR,AIOAREA3                                                  
         GOTO1 AIO,FILGETU                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOADDR,AIOAREA1                                                  
         GOTO1 AIO,FILPUT          PUT HEADER RECORD                            
         BE    VASL2                                                            
         DC    H'0'                                                             
*                                                                               
VASL2    TM    COMCHG,LDET         TEST FOR DETAIL CHANGE                       
         BZ    VASL10                                                           
         TM    COMCHG,LDAYTIM      TEST DAYS/TIMES CHANGE                       
         BZ    VASL8                                                            
         MVC   BWDKELDY,BWDDAYS    YES-SET DAYS/TIMES IN ELEMENT KEY            
         MVC   BWDKELTM,BWDTIMCD                                                
         MVI   BWDKELSQ,0                                                       
         MVC   IOKEY(13),BWDKEY                                                 
         SR    R4,R4                                                            
         LA    R1,MINHI3           READ RECORDS FOR NEW DAYS/TIMES              
         B     VASL4+4                                                          
*                                                                               
VASL4    LA    R1,MINSEQ3                                                       
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VASL6                                                            
         LA    R3,IOKEY                                                         
         CLC   BWDKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                 
         BNE   VASL6                                                            
         IC    R4,BWDKELSQ         SAVE LATEST SEQ NUM                          
         B     VASL4                                                            
*                                                                               
VASL6    LA    R4,1(R4)            NEXT SEQUENCE NUMBER                         
         L     R3,AIOAREA2                                                      
         STC   R4,BWDSEQ           SAVE IN RECORD                               
         STC   R4,BWDKELSQ                                                      
*                                                                               
VASL8    MVC   IOKEY(13),APRECKEY  READ RECORD WITH OLD KEY                     
         GOTO1 AMIN,MINRD3                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AMIN,MINWRT2        WRITE NEW RECORD                             
         MVC   APRECKEY(13),BWDKEY   SET POSSIBLE KEY CHANGE FOR                
*                                    GENERAL                                    
*                                                                               
VASL10   TM    COMCHG,LCOPY        TEST COPY TO OTHER CAMPAIGNS                 
         BZ    VASLX                                                            
         GOTO1 AMIN,MINCLS         CLOSE THE CURRENT MASTER KEY                 
         LA    R8,BWDCOST1-BWDRECD                                              
         TM    APRECID,RIEFFDT2+RIEFFDT3                                        
         BZ    VASL11                                                           
         LA    R8,BWDCOST2-BWDRECD                                              
         TM    APRECID,RIEFFDT2                                                 
         BO    VASL11                                                           
         LA    R8,BWDCOST3-BWDRECD                                              
*                                                                               
VASL11   LA    R1,BWDRECD(R8)                                                   
         MVC   APFULL,0(R1)        SAVE THE COST                                
         L     R2,AIOAREA2                                                      
         LA    R4,COMCOLNS                                                      
         LA    R0,MAXCOPYL                                                      
*                                                                               
VASL12   OC    0(3,R4),0(R4)                                                    
         BZ    VASLX                                                            
         XC    IOKEY,IOKEY                                                      
         L     R3,AIOAREA2                                                      
         MVC   IOKEY(13),BWDKEY                                                 
         LA    R3,IOKEY                                                         
         MVC   BWDKSEQ,0(R4)       SET CAMPAIGN/MARKET SEQ NO                   
         MVC   BWDKELST,2(R4)      SET STATION CODE                             
         MVI   BWDKELSQ,0                                                       
         MVI   APBYTE,0                                                         
         LA    R1,MINHI3                                                        
         B     VASL14+4            READ THE DETAIL RECORD                       
*                                                                               
VASL14   LA    R1,MINSEQ3                                                       
         GOTO1 AMIN                                                             
         BNE   *+14                                                             
         CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                  
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VASL16                                                           
         L     R3,AIOAREA3                                                      
         CLC   BWDTIMES,BWDTIMES-BWDRECD(R2)    CHECK THE TIMES                 
         BNE   VASL14                                                           
         CLC   BWDSLN,BWDSLN-BWDRECD(R2)  CHECK SPOT LENGTH                     
         BNE   VASL14                                                           
         LA    R1,BWDRECD(R8)                                                   
         CLC   0(4,R1),APFULL      TEST COST CHANGE                             
         BE    *+14                                                             
         MVC   0(4,R1),APFULL      YES-CHANGE THE COST                          
         MVI   APBYTE,2                                                         
         CLC   BWDPROG,BWDPROG-BWDRECD(R2)    TEST PROGRAM CHANGE               
         BE    VASL15                                                           
         OI    BWDINDS,BWDIPRG     YES                                          
         MVC   BWDPROG,BWDPROG-BWDRECD(R2)                                      
         MVI   APBYTE,2                                                         
*                                                                               
VASL15   CLI   APBYTE,2            TEST CHANGE TO RECORD                        
         BNE   VASL14                                                           
         GOTO1 AMIN,MINWRT3        YES-WRITE THE RECORD BACK                    
         MVI   APBYTE,1                                                         
         GOTO1 (RF),MINHI3                                                      
         B     VASL14              NEXT DAY/TIME DUPLICATE                      
*                                                                               
VASL16   CLI   APBYTE,0            TEST ANY ACTIVITY FOR MASTER KEY             
         BE    VASL18                                                           
         GOTO1 AMIN,MINCLS         YES-CLOSE FOR MASTER KEY                     
*                                                                               
VASL18   LA    R4,3(R4)            NEXT CAMPAIGN                                
         BCT   R0,VASL12                                                        
*                                                                               
VASLX    B     COMMONX                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SCHEDULE AND BUILD SPOTS PER WEEK ELEMENT       *         
* INPUT  : P1 A(TWA SCHEDULE LINE)                                    *         
*          P2 A(TWA ACTUAL POINTS/DOLLARS LINE)                       *         
*          IOAREA2 CONTAINS DETAIL RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
VALSKD   L     R3,AIOAREA2                                                      
         USING BWDRECD,R3                                                       
         MVC   LAACLINE,4(R1)                                                   
         MVI   LFLAG,0                                                          
         L     R1,0(R1)                                                         
         ST    R1,LASKDLN                                                       
         GOTO1 AFVAL               VALIDATE SCHEDULE                            
         BH    VSKDX                                                            
         BL    VSKD90              NOTHING - EXIT                               
         XC    LNOSKED,LNOSKED                                                  
         OC    BWDWKS,BWDWKS       TEST ANY INACTIVE WEEKS                      
         BZ    VSKD1                                                            
         SR    R0,R0               YES-SET INACTIVE WEEK INDICATORS             
         SR    R1,R1                                                            
         ICM   R1,12,BWDWKS                                                     
         LA    RE,LNOSKED                                                       
         LA    RF,NMAXWKS                                                       
*                                                                               
VSKD0    SLDL  R0,1                                                             
         LTR   R0,R0                                                            
         BZ    *+10                                                             
         MVI   0(RE),1                                                          
         SR    R0,R0                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,VSKD0                                                         
*                                                                               
VSKD1    LA    RE,APRECKEY+20                                                   
         MVC   LSPW,EFFS        BUILD SPOTS PER WEEK                            
         LA    R0,NMAXWKS       X'FF' = NO SPOTS THIS WEEK                      
         LA    RF,LSPW          X'7F' = NO SPOTS & OUTSIDE OF EFFECTIVE         
         LA    R1,LNOSKED               DATE RANGE                              
*                                                                               
VSKD2    TM    0(RE),X'C0'                                                      
         BNZ   *+12                                                             
         CLI   0(R1),0             TEST INACTIVE WEEK                           
         BE    *+8                                                              
         NI    0(RF),FF-X'80'                                                   
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,VSKD2                                                         
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    RF,C'/'             BUILD LIST FOR TRT                           
         STC   RF,APELEM(RF)                                                    
         LA    RF,C'+'             (EVERY WEEK)                                 
         STC   RF,APELEM(RF)                                                    
         LA    RF,C'A'             (ALTERNATE WEEKS)                            
         STC   RF,APELEM(RF)                                                    
         LA    RF,C'T'             (EVERY THIRD WEEK)                           
         STC   RF,APELEM(RF)                                                    
         LA    RF,C'F'             (EVERY FOURTH WEEK)                          
         STC   RF,APELEM(RF)                                                    
         TRT   FVIFLD(55),APELEM                                                
         BNZ   *+16                                                             
         CLI   FVIFLD,C' '         NO SPECIAL CHARS --                          
         BH    VSKD99                                                           
         B     VSKD30              VALIDATE ALREADY EXPANDED SCHEDULE           
*                                                                               
         OI    LFLAG,LSHRTSKD      VALIDATE SHORTHAND SCHEDULE                  
         SR    RE,RE                                                            
         BCTR  RE,0                                                             
         LA    RF,FVIFLD-1         RE AND RF SET UP FOR BXH                     
         LA    R0,2                ALLOW 2 DIGITS FOR SPOTS PER WEEK            
         STC   R2,APBYTE           APBYTE = SPECIAL CHARACTER                   
         CLI   APBYTE,C'/'                                                      
         BNE   VSKD15                                                           
*                                                                               
VSKD4    BXH   R1,RE,*+8           FIRST SPECIAL CHARACTER IS /                 
         B     VSKD6                                                            
         CLI   0(R1),C' '          TEST FOR VALID CHARS PRECEDING /             
         BNH   VSKD6                                                            
         CLI   0(R1),C'.'                                                       
         BE    VSKD6                                                            
         CLI   0(R1),C'X'                                                       
         BE    VSKD6                                                            
         LTR   R0,R0                                                            
         BZ    VSKD99                                                           
         CLI   0(R1),C'0'                                                       
         BL    VSKD99                                                           
         CLI   0(R1),C'9'                                                       
         BH    VSKD99                                                           
         BCTR  R0,0                                                             
         B     VSKD4                                                            
*                                                                               
VSKD6    LA    R1,1(R1)            R1 = A(BEGINNING OF SHORTHAND SCHED)         
         LR    R4,R1               R4 = A(SAME)                                 
         BAL   RE,VSKDPOS          INITIALIZE SPOTS PER WEEK PRIOR              
*                                                                               
VSKD8    CLI   0(R4),C' '          TEST END OF SCHEDULE                         
         BNH   VSKD10                                                           
         CLI   0(R4),C'.'                                                       
         BE    VSKD10                                                           
         CLI   0(R4),C'X'                                                       
         BNE   VSKD12                                                           
*                                                                               
VSKD10   MVI   0(RF),X'FE'         YES - LEAVE REST OF WEEKS ALONE              
*                                                                               
VSKD11   LA    RF,1(RF)                                                         
         BCT   R0,VSKD10                                                        
         B     VSKD20                                                           
*                                                                               
VSKD12   CLI   0(R4),C'/'          TEST SKIP THIS WEEK                          
         BE    VSKD14                                                           
         CLI   0(R4),C'0'          TEST VALID NUMERIC                           
         BL    VSKD99                                                           
         CLI   0(R4),C'9'                                                       
         BH    VSKD99                                                           
         SR    RE,RE                                                            
         CLI   1(R4),C'0'                                                       
         BL    *+16                                                             
         CLI   1(R4),C'9'                                                       
         BH    VSKD99                                                           
         LA    RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  APDUB,0(0,R4)                                                    
         CVB   R1,APDUB                                                         
         STC   R1,0(RF)            STORE SPOTS/WEEK IN SCHEDULE FIELD           
         LA    R4,1(RE,R4)         NEXT BYTE                                    
         CLI   0(R4),C' '          TEST FOR END                                 
         BNH   VSKD11              YES                                          
         ST    R1,APFULL                                                        
         TRT   0(1,R4),APELEM      TEST FOR SPECIAL CHARACTER                   
         BZ    VSKD99              NO - ERROR                                   
         CLI   0(R4),C'/'          YES - TEST FOR WEEK SEPARATOR                
         BE    VSKD14                    YES                                    
         L     R1,APFULL                 NO - R1 = SPOTS PER WEEK               
         STC   R2,APBYTE                                                        
         LA    RE,=C'+ATF'                                                      
         SR    R2,R2                                                            
         BCTR  R2,0                                                             
         CLC   APBYTE,0(RE)                                                     
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R2,*-14                                                          
         LPR   R2,R2               R2 = FREQUENCY OF SPOTS                      
         LA    R3,LSPW+NMAXWKS-1   GET READY FOR BXLE                           
         LR    RE,RF                                                            
         LA    R0,LSPW                                                          
         SR    RE,R0                                                            
         SLL   RE,2                                                             
         LA    RE,SVGLPTS(RE)      POINT TO GOAL POINTS                         
         B     VSKD19                                                           
*                                                                               
VSKD14   LA    R4,1(R4)            NEXT WEEK                                    
         LA    RF,1(RF)                                                         
         BCT   R0,VSKD8                                                         
*                                                                               
         CLI   0(R4),C' '          MUST BE END OF SCHEDULE                      
         BH    VSKD99                                                           
         B     VSKD20                                                           
*                                                                               
VSKD15   CLI   1(R1),C' '          SPECIAL CHARACTER IS +,A,T,F                 
         BH    VSKD99                                                           
         LA    R4,=C'+ATF'                                                      
         SR    R2,R2                                                            
         BCTR  R2,0                                                             
         CLC   APBYTE,0(R4)                                                     
         BE    *+12                                                             
         LA    R4,1(R4)                                                         
         BCT   R2,*-14                                                          
         LPR   R2,R2               R2 = FREQUENCY OF SPOTS                      
         SR    R4,R4               R4 IS FOR EXECUTED PACK                      
         BXH   R1,RE,*+8           LOOK FOR VALID DIGITS BEFORE CHAR            
         B     VSKD99                                                           
         CLI   APBYTE,C'+'         TEST X+                                      
         BNE   VSKD15B                                                          
         CLI   0(R1),C'X'                                                       
         BNE   VSKD15B                                                          
         BXH   R1,RE,*+8           YES-CHECK NO INVALID CHARACTER               
         B     VSKD15A                 BEFORE IT                                
         CLI   0(R1),C' '                                                       
         BNH   VSKD15A                                                          
         CLI   0(R1),C'.'                                                       
         BNE   VSKD99                                                           
*                                                                               
VSKD15A  LA    R1,1(R1)            MAKE ALL SUBSEQUENT WEEKS INVALID            
         BAL   RE,VSKDPOS                                                       
         MVI   0(RF),X'7F'                                                      
         LA    RF,1(RF)                                                         
         BCT   R0,*-8                                                           
         B     VSKD20                                                           
*                                                                               
VSKD15B  CLI   0(R1),C'0'                                                       
         BL    VSKD99                                                           
         CLI   0(R1),C'9'                                                       
         BH    VSKD99                                                           
*                                                                               
VSKD16   BXH   R1,RE,*+8                                                        
         B     VSKD17                                                           
         CLI   0(R1),C' '                                                       
         BNH   VSKD17                                                           
         CLI   0(R1),C'.'                                                       
         BE    VSKD17                                                           
         CLI   0(R1),C'X'                                                       
         BE    VSKD17                                                           
         BCT   R0,*+8                                                           
         B     VSKD99                                                           
         CLI   0(R1),C'0'                                                       
         BL    VSKD99                                                           
         CLI   0(R1),C'9'                                                       
         BH    VSKD99                                                           
         LA    R4,1                                                             
         B     VSKD16                                                           
*                                                                               
VSKD17   LA    R1,1(R1)            R1 = A(START OF SHORTHAND SCHEDULE)          
         EX    R4,*+8              PACK SPOTS PER WEEK                          
         B     *+10                                                             
         PACK  APDUB,0(0,R1)                                                    
         BAL   RE,VSKDPOS          INITIALIZE SPOTS PER WEEK PRIOR              
         CVB   R1,APDUB            R1 = SPOTS PER WEEK                          
         CH    R1,=H'64'           CHECK NOT TOO MANY                           
         BNL   VSKD99                                                           
         TM    CMPOPTS,CAMODLY     DAILY LIMIT IS 9                             
         BZ    *+12                                                             
         CH    R1,=H'9'                                                         
         BH    VSKD99                                                           
         LA    R3,LSPW+NMAXWKS-1   GET READY FOR BXLE                           
         LR    RE,RF                                                            
         LA    R0,LSPW                                                          
         SR    RE,R0                                                            
         SLL   RE,2                                                             
         LA    RE,SVGLPTS(RE)                                                   
*                                                                               
******** TM    CMPOPTS,CAMODLY     TEST DAILY SKED                              
******** BO    VSKD18A                                                          
VSKD18   OC    SVGLPTST,SVGLPTST   NO-TEST FOR ANY GOAL POINTS                  
         BZ    VSKD18A                                                          
         OC    0(4,RE),0(RE)       YES - TEST GOAL FOR THIS WEEK                
         BZ    VSKD19                    NO - SKIP THIS WEEK                    
*                                                                               
VSKD18A  TM    0(RF),X'80'         TEST FOR INVALID WEEK                        
         BZ    VSKD19              YES - SKIP THIS WEEK                         
         STC   R1,0(RF)            SPOTS THIS WEEK                              
*                                                                               
VSKD19   LR    R0,R2                                                            
         LA    RE,4(RE)                                                         
         BCT   R0,*-4                                                           
         BXLE  RF,R2,VSKD18        DO UNTIL END OF PERIOD                       
*                                                                               
         B     VSKD20                                                           
         EJECT                                                                  
***********************************************************************         
* INPUT  : R1 = A(SHORTHAND SCHDULE)                                  *         
* OUTPUT : R0 = MAX NO OF WEEKS COVERED BY SHORTHAND SCHEDULE         *         
*          RF = A(FIRST POSITION IN SPOTS PER WEEK FIELD COVERED)     *         
***********************************************************************         
         SPACE 1                                                                
VSKDPOS  LA    RF,FVIFLD           CALCULATE NO OF WEEKS PRIOR TO               
         SR    R1,RF               SHORTHAND SCHEDULE                           
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    R1,1(R1)                                                         
         SRL   R1,2                                                             
         LA    RF,LSPW                                                          
         LTR   R0,R1               R0 = R1 = NO OF WEEKS PRIOR                  
         BZ    VSKDPOS2                                                         
         MVI   0(RF),X'FE'         X'FE' = THIS WEEK PRIOR TO SHORTHAND         
         LA    RF,1(RF)                    SCHEDULE                             
         BCT   R0,*-8                                                           
*                                                                               
VSKDPOS2 ZIC   R0,CMPNWKS                                                       
         SR    R0,R1               R0 = NO OF WEEKS COVERED BY SCHED            
         BNP   VSKD99                                                           
*                                                                               
VSKDPOSX BR    RE                                                               
         EJECT                                                                  
VSKD20   L     R3,AIOAREA2                                                      
         ZIC   R0,CMPNWKS          DISPLAY SCHEDULE IN FVIFLD                   
         SR    R1,R1                                                            
         LA    RE,FVIFLD-1                                                      
         LA    RF,LSPW                                                          
         CLI   0(RF),X'FE'         X'FE' = LEAVE THIS WEEK ALONE                
         BE    VSKD26                                                           
         MVC   1(3,RE),BLANKS                                                   
         B     VSKD23                                                           
*                                                                               
VSKD22   CLI   0(RF),X'FE'                                                      
         BE    VSKD26                                                           
         MVC   0(4,RE),BLANKS                                                   
*                                                                               
VSKD23   TM    0(RF),FF-X'80'                                                   
         BO    VSKD24                                                           
         ICM   R1,1,0(RF)                                                       
         BNZ   *+12                                                             
         MVI   3(RE),C'.'                                                       
         B     VSKD26                                                           
         CVD   R1,APDUB                                                         
         UNPK  2(2,RE),APDUB                                                    
         OI    3(RE),X'F0'                                                      
         B     VSKD26                                                           
*                                                                               
VSKD24   MVI   3(RE),C'.'                                                       
         TM    0(RF),X'80'                                                      
         BO    VSKD26                                                           
         MVI   3(RE),C'X'                                                       
*                                                                               
VSKD26   LA    RE,4(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,VSKD22                                                        
         EJECT                                                                  
VSKD30   XC    APELEM,APELEM       VALIDATE EXPANDED SCHEDULE                   
         SR    R0,R0                                                            
         LA    R4,BWDEL                                                         
*                                                                               
VSKD32   CLI   0(R4),0                                                          
         BE    VSKD34                                                           
         CLI   0(R4),SPWELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VSKD32                                                           
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(R4)                                                  
*                                                                               
VSKD34   LA    R4,APELEM                                                        
         USING SPWEL,R4                                                         
         MVI   SPWELCD,SPWELCDQ                                                 
         LA    R2,APRECKEY+20      R2 = A(CURRENT SPOTS LINE)                   
         IC    R0,CMPNWKS          LOOK FOR CHANGES                             
         LA    R8,SPWPERWK                                                      
         LA    RE,FVIFLD-1         BEGIN WITH FIRST WEEK                        
         NI    LFLAG,255-LINWKCHG                                               
         LA    R5,LNOSKED                                                       
*                                                                               
VSKD35   CLI   0(R2),FF            TEST END OF PERIOD                           
         BE    VSKD44                                                           
         LA    R1,1                                                             
         LA    RF,2(RE)                                                         
         CLC   0(2,RF),BLANKS      TEST ANYTHING FOR THIS WEEK                  
         BNH   VSKD42              NO - NEXT WEEK                               
         CLI   0(RF),C' '          TEST FOR TWO CHARACTERS                      
         BH    VSKD39              YES                                          
***  1 CHARACTER                                                                
         CLI   1(RF),C'*'          NO GOALS?                                    
         BE    VSKD42              YES, CHECK THE NEXT WEEK                     
         CLI   1(RF),C'X'          NO-TEST FOR X                                
         BNE   VSKD36                                                           
         SR    R1,R1               YES-MAKE SURE NO SPOTS THIS WEEK             
         TM    0(R2),X'C0'         TEST OUTSIDE DATE BOUNDS                     
         BNZ   VSKD42                                                           
         CLI   0(R5),0             NO-TEST WEEK ALREADY INACTIVE                
         BNE   VSKD40                                                           
         MVI   0(R5),1                NO-MAKE INACTIVE                          
         OI    LFLAG,LINWKCHG            AND INDICATE CHANGE                    
         B     VSKD40                                                           
*                                                                               
VSKD36   CLI   1(RF),C'.'          TEST FOR .                                   
         BNE   VSKD38                                                           
         SR    R1,R1               YES-MAKE SURE NO SPOTS THIS WEEK             
         CLI   0(R5),0             TEST INACTIVE WEEK                           
         BE    VSKD37              NO                                           
         OC    BWDDATES,BWDDATES   YES-TEST RECORD DATES SET                    
         BNZ   VSKD99              YES-CANNOT MAKE ACTIVE HERE                  
         MVI   0(R5),0             NO-MAKE WEEK ACTIVE AGAIN                    
         OI    LFLAG,LINWKCHG                                                   
         B     VSKD40                                                           
*                                                                               
VSKD37   TM    0(R2),X'C0'         TEST OUT OF DATE BOUNDS                      
         BNZ   VSKD99              YES-INVALID                                  
         B     VSKD40                                                           
*                                                                               
VSKD38   LA    RF,1(RF)                                                         
         BCTR  R1,0                                                             
*                                                                               
VSKD39   TM    0(R2),X'C0'        TEST FOR THIS WEEK OUT OF DATE BOUNDS         
         BNZ   VSKD99             YES-INVALID                                   
         CLI   0(R5),0            TEST THIS WEEK IS INACTIVE                    
         BNE   VSKD99             YES-INVALID                                   
         XC    APHALF,APHALF                                                    
         EX    R1,VSKDEX1                                                       
         EX    R1,VSKDEX2         TEST FOR NUMERIC                              
         BL    VSKD99             NO - INVALID                                  
         EX    R1,VSKDEX3                                                       
         CVB   R1,APDUB                                                         
         CH    R1,=H'64'          YES-TEST N'SPOTS NOT TOO HIGH                 
         BNL   VSKD99                                                           
         TM    CMPOPTS,CAMODLY     DAILY LIMIT IS 9                             
         BZ    VSKD40                                                           
         CH    R1,=H'9'                                                         
         BH    VSKD99                                                           
*                                                                               
VSKD40   CLM   R1,1,0(R8)         COMPARE TO OLD VALUE                          
         BE    VSKD42                                                           
         STC   R1,0(R8)           NOT EQUAL - MOVE NEW VALUE TO ELEMENT         
         OI    COMCHG,LSKED                   INDICATE ELEMENT CHANGE           
*                                                                               
VSKD42   LA    R8,1(R8)            NEXT WEEK                                    
         LA    R2,1(R2)                                                         
         LA    RE,4(RE)                                                         
         LA    R5,1(R5)                                                         
         CLC   0(2,RE),BLANKS      TEST FOR MORE THAN TWO CHARACTERS            
         BH    VSKD99              YES - INVALID                                
         BCT   R0,VSKD35           DO FOR ALL WEEKS                             
*                                                                               
VSKD44   L     R5,ATWA             RESTORE R5                                   
         TM    LFLAG,LINWKCHG      TEST CHANGE IN INACTIVE WEEKS                
         BZ    VSKD46                                                           
         OI    COMCHG,LSKED        YES-INDICATE RECORD CHANGE                   
         LA    R8,1                SET INACTIVE WEEK MASK IN RECORD             
         SLL   R8,31                                                            
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         LA    RE,LNOSKED                                                       
         LA    RF,NMAXWKS                                                       
*                                                                               
VSKD45   CLI   0(RE),0                                                          
         BE    *+6                                                              
         LR    R1,R8                                                            
         SLDL  R0,1                                                             
         LA    RE,1(RE)                                                         
         BCT   RF,VSKD45                                                        
         LA    RF,16-NMAXWKS                                                    
         LTR   RF,RF                                                            
         BZ    *+18                                                             
         BP    *+6                                                              
         DC    H'0'                                                             
         SLDL  R0,1                                                             
         BCT   RF,*-4                                                           
         STCM  R0,3,BWDWKS                                                      
*                                                                               
VSKD46   TM    LFLAG,LSHRTSKD      TEST FOR SHORTHAND SCHEDULE                  
         BZ    VSKD47                                                           
         L     R1,LASKDLN          YES - TRANSMIT SCHEDULE IN                   
         OI    6(R1),FVOXMT              EXPANDED FORM                          
         MVC   L'FVIHDR(L'SKDSKD,R1),FVIFLD                                     
*                                                                               
VSKD47   TM    COMCHG,LSKED        TEST FOR NEW SCHEDULE INPUT                  
         BZ    VSKD90                                                           
         ZIC   RE,CMPNWKS          YES - RE = NUMBER OF CAMPAIGN WEEKS          
         LA    RF,SPWPERWK-SPWEL(RE)                                            
         STC   RF,SPWELLN          SET ELEMENT LENGTH                           
         ST    R4,LREGSV           SAVE ADDRESS OF ELEMENT                      
         SR    R0,R0               YES --                                       
         LA    R1,BWDEL            LOOK FOR DEMO ELEMENT                        
*                                                                               
VSKD48   CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),DMOELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VSKD48                                                           
         MVI   APFULL,0                                                         
         MVC   APFULL+1(3),DMODEMO+5-DMOEL(R1)    APFULL = DEMO VALUE           
         OC    INORTG,INORTG       TEST FOR OVERRIDE TARGET DEMO                
         BZ    VSKD50                                                           
         MVI   APWORK,C'R'         YES - GET THE RATING                         
         CLI   INORTG+1,C'E'                                                    
         BNE   *+8                                                              
         MVI   APWORK,C'E'                                                      
         MVC   APWORK+1(1),INORTG+2                                             
         LA    RE,L'DMODEMO                                                     
         ZIC   RF,1(R1)                                                         
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    R1,DMODEMO-DMOEL(R1)                                             
         CLC   1(2,R1),APWORK                                                   
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     VSKD50                                                           
         MVC   APFULL+1(3),5(R1)                                                
*                                                                               
VSKD50   TM    APRECID,RIEFFDT2    FIND THE COST (LFULL)                        
         BZ    *+14                                                             
         MVC   LFULL,BWDCOST2                                                   
         B     VSKD52                                                           
         TM    APRECID,RIEFFDT3                                                 
         BZ    *+14                                                             
         MVC   LFULL,BWDCOST3                                                   
         B     VSKD52                                                           
         MVC   LFULL,BWDCOST1                                                   
*                                                                               
VSKD52   LA    R4,SPWPERWK         R4 = A(NEW SPOTS PER WEEK)                   
         LA    R8,APRECKEY+20      R8 = A(OLD SPOTS PER WEEK)                   
         LA    R2,SVACPTS          R2 = A(ACTUAL POINTS LINE)                   
         LA    R3,SVPKAPTS         R3 = (ACTUAL PTS FOR PACKAGE)                
         ZIC   RE,CMPNWKS          RE = N'WEEKS                                 
*                                                                               
VSKD54   TM    0(R8),X'C0'         TEST INVALID WEEK                            
         BNZ   VSKD56                                                           
         CLC   0(1,R8),0(R4)       TEST FOR CHANGE THIS WEEK                    
         BE    VSKD56                                                           
         ZIC   R0,0(R4)            YES - FIND THE DIFFERENCE                    
         ZIC   RF,0(R8)                                                         
         SR    R0,RF                                                            
         SRDL  R0,32                                                            
         STM   R0,R1,APDUB                                                      
         M     R0,APFULL           DIFFERENCE X RATING                          
         L     RF,0(R2)                                                         
         AR    RF,R1              ADD THE DIFFERENCE IN POINTS TO TOTAL         
         ST    RF,0(R2)                                                         
         CLI   INREC,RECPKG       AND FOR PACKAGE                               
         BNE   *+14                                                             
         L     RF,0(R3)                                                         
         AR    RF,R1                                                            
         ST    RF,0(R3)                                                         
         LM    R0,R1,APDUB                                                      
         M     R0,LFULL            DIFFERENCE X COST                            
         L     R0,SVACDOL                                                       
         AR    R0,R1               ADD TO TOTAL COST                            
         ST    R0,SVACDOL                                                       
         CLI   INREC,RECPKG       AND FOR PACKAGE                               
         BNE   *+14                                                             
         L     R0,SVPKADOL                                                      
         AR    R0,R1                                                            
         ST    R0,SVPKADOL                                                      
         MVC   0(1,R8),0(R4)                                                    
*                                                                               
VSKD56   LA    R4,1(R4)            NEXT WEEK                                    
         LA    R8,1(R8)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   RE,VSKD54           DO FOR ALL CAMPAIGN WEEKS                    
*                                                                               
         L     R3,AIOAREA2                                                      
         OC    LAACLINE,LAACLINE   TEST FOR ACTUAL POINTS/DOLLARS LINE          
         BZ    VSKD58              ON THIS SCREEN                               
         GOTO1 AFMACPTS,APPARM,LAACLINE,SVGVA  YES-FORMAT ACTUAL POINTS         
         GOTO1 AFMACDOL,APPARM,LAACLINE,SVGVA      FORMAT ACTUAL DOLLS          
*                                                                               
VSKD58   GOTO1 ADELELS,BWDRECD     DELETE OLD ELEMENT IF ANY                    
         L     R4,LREGSV                                                        
         OC    SPWPERWK(NMAXWKS),SPWPERWK  TEST FOR ANY SPOTS AT ALL            
         BZ    VSKD90                                                           
         GOTO1 AADDELS,BWDRECD     YES - ADD NEW ELEMENT                        
         TM    CMPIND,CMPISKD      TEST SPOTS SCHEDULED FOR CAMPAIGN            
         BO    VSKD90              PREVIOUSLY                                   
         LA    R3,IOKEY            NO-INDICATE SCHEDULING IN CAMPAIGN           
         USING CAMRECD,R3             RECORD                                    
         XC    IOKEY,IOKEY                                                      
         MVI   CAMKTYP,CAMKTYPQ                                                 
         MVI   CAMKSUB,CAMKSUBQ                                                 
         MVC   CAMKAGMD,BAGYMD                                                  
         MVC   CAMKBYR,BBYR                                                     
         MVC   CAMKCAM,BCAM                                                     
         GOTO1 AIO,DIRRD                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),FILGETU3                                                    
         L     R3,AIOAREA3                                                      
         OI    CAMINDS,CAMISKD                                                  
         GOTO1 (RF),FILPUT3                                                     
         OI    CMPIND,CMPISKD                                                   
         B     VSKD90                                                           
*                                                                               
VSKD90   MVC   FVMSGNO,=AL2(FVFOK) OK EXIT                                      
         B     VSKDX                                                            
*                                                                               
VSKD99   MVC   FVMSGNO,=AL2(FVFNOTV)   ERROR EXIT                               
*                                                                               
VSKDX    B     COMMONX                                                          
         SPACE 2                                                                
VSKDEX1  MVZ   APHALF(0),0(RF)     * EXECUTED INSTRUCTIONS                      
VSKDEX2  CLC   APHALF(0),=C'00'                                                 
VSKDEX3  PACK  APDUB,0(0,RF)                                                    
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET PROGRAM NAME BY DEMO LOOK-UP                         *         
***********************************************************************         
         SPACE 1                                                                
GETPROG  NTR1                                                                   
         USING BWDRECD,R3                                                       
         XC    SPDEMLK,SPDEMLK                                                  
         MVC   SPLKAREC,AIOAREA3                                                
         MVC   SPLKAFAC,ACOM                                                    
         MVC   SPLKAGY,CUAALF                                                   
         MVC   SPLKMED,CUDMED                                                   
         MVC   SPLKCLI,QCLT                                                     
         MVC   SPLKSRC,CLTSRC                                                   
         MVI   SPLKFIL,C'T'                                                     
         MVC   SPLKSTA,BWDSTA                                                   
         MVC   SPLKUMK,MKTLKUP                                                  
         MVC   SPLKDAY,BWDDAYS                                                  
         MVC   SPLKTIM,BWDTIMES                                                 
         MVC   SPLKBTYP,STABKTYP                                                
         MVC   APFULL(3),ESTDEMS                                                
         MVI   APFULL+3,FF                                                      
         LA    R1,APFULL                                                        
         ST    R1,SPLKALST                                                      
         LA    R1,APWORK                                                        
         ST    R1,SPLKAVAL                                                      
         MVI   SPLKSVI,X'FF'                                                    
         GOTO1 VSPDEMLK,APPARM,(X'FF',SPDEMLK)   CALL SPGETDEM                  
         MVC   BWDPROG,BLANKS                                                   
         MVC   BWDPROG(L'SPLKPRG),SPLKPRG        PROGRAM NAME RETURNED          
         B     COMMONX                                                          
         EJECT                                                                  
***********************************************************************         
* BIULD TWA                                                           *         
* INPUT : R1 = A(FIRST BUILD ELEMENT)                                 *         
***********************************************************************         
         SPACE 1                                                                
TWABLD   LA    R4,APPARM                                                        
         USING TWAPARMD,R4                                                      
         XC    TWAPARMD(TWAPARML),TWAPARMD                                      
         ST    R5,TWAPATWA                                                      
         USING TWAD,R5                                                          
         ST    R1,TWAPAFST                                                      
         LA    RF,WRKL8H                                                        
         ST    RF,TWAPAOUT                                                      
         GOTO1 VTWABLD,APPARM                                                   
         CLI   TWAPERRS,TWAPEOK                                                 
         BE    COMMONX                                                          
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT TOTAL ACTUAL POINTS TO SCREEN                     *         
* ENTRY : CFMACPTS FROM COMMON ROUTINE                                *         
*         FMACPTS  FROM MAIN                                          *         
* INPUT : P1 = A(TWA ACTUAL DOLLARS AND POINTS LINE)                  *         
*         P2 = A(GOAL VS ACTUAL INFO BLOCK)                           *         
***********************************************************************         
         SPACE 1                                                                
CFMACPTS NTR1                                                                   
*                                                                               
FMACPTS  MVC   LAACLINE,0(R1)                                                   
         L     R6,4(R1)            R6=A(GVA INFO BLOCK)                         
         USING GVAD,R6                                                          
         ZIC   R0,CMPNWKS                                                       
         LA    R2,GACPTS                                                        
         SR    R3,R3                                                            
         L     R4,LAACLINE                                                      
         LA    R4,L'FVIHDR(R4)                                                  
         LA    RE,TOTPAREA-TOTD(R4)                                             
         MVC   0(L'TOTPAREA,RE),BLANKS                                          
         LA    R4,TOTPTSWK-TOTD-1(R4)                                           
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBLIN,4                                                          
         MVI   EBLOUT,4                                                         
         MVI   EBTIN,C'B'                                                       
         MVC   EBSCIN,GSCALE                                                    
*                                                                               
FACP2    OC    0(4,R2),0(R2)                                                    
         BZ    FACP4                                                            
         A     R3,0(R2)            ACCUMULATE TOTAL OVER ALL WEEKS              
         ST    R2,EBAIN                                                         
         ST    R4,EBAOUT                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK     WEEK TOTAL                             
*                                                                               
FACP4    LA    R2,4(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,FACP2                                                         
*                                                                               
         L     R2,LAACLINE                                                      
         LA    R2,L'FVIHDR(R2)                                                  
         USING TOTD,R2                                                          
         A     R3,GACXPTS          ADD IN POSSIBLE EXTRA POINTS                 
         ST    R3,APFULL                                                        
         LA    RE,APFULL                                                        
         ST    RE,EBAIN                                                         
         MVI   EBLIN,4                                                          
         MVI   EBLOUT,4                                                         
         LA    RE,TOTPTS                                                        
         ST    RE,EBAOUT                                                        
         MVI   EBSCIN,X'81'                                                     
         CLC   APFULL,=F'99995'                                                 
         BL    FACP6                                                            
         MVI   EBSCIN,X'82'                                                     
         CLC   APFULL,=F'999995'                                                
         BL    FACP6                                                            
         MVI   EBSCIN,X'83'                                                     
*                                                                               
FACP6    GOTO1 VEDITOR,APPARM,EBLOCK     OVERALL TOTAL                          
*                                                                               
         MVC   APDUB(4),GGLPTST    CALCULATE ACTUAL/GOAL PERCENT                
         LA    R3,TOTPTSPC         AND FORMAT                                   
         BAL   RE,PERCENT                                                       
*                                                                               
FACPX    L     R1,LAACLINE                                                      
         OI    6(R1),FVOXMT                                                     
         B     COMXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT TOTAL COST TO SCREEN                              *         
* ENTRY : CFMACDOL FROM COMMON ROUTINE                                *         
*         FMACDOL  FROM MAIN                                          *         
* INPUT : P1 = A(TWA ACTUAL DOLLARS AND POINTS LINE)                  *         
*         P2 = A(GOAL VS ACTUAL INFO BLOCK)                           *         
***********************************************************************         
         SPACE 1                                                                
CFMACDOL NTR1                                                                   
*                                                                               
FMACDOL  MVC   LAACLINE,0(R1)                                                   
         L     R6,4(R1)            R6=A(GVA INFO BLOCK)                         
         USING GVAD,R6                                                          
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBLIN,4                                                          
         MVI   EBLOUT,7                                                         
         MVI   EBTIN,C'B'                                                       
         MVI   EBSCIN,X'82'                                                     
         MVI   EBFLOAT,C'$'                                                     
         L     R2,LAACLINE                                                      
         LA    R2,L'FVIHDR(R2)                                                  
         USING TOTD,R2                                                          
         MVC   TOTDOL,BLANKS                                                    
         LA    RE,TOTDOL                                                        
         ST    RE,EBAOUT                                                        
         LA    RE,GACDOL                                                        
         ST    RE,EBAIN                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
         MVC   APFULL,GACDOL       CALCULATE AND FORMAT                         
         MVC   APDUB(4),GGLDOL     ACTUAL/GOAL DOLLARS                          
         LA    R3,TOTDOLPC                                                      
         BAL   RE,PERCENT                                                       
*                                                                               
FACDX    L     R1,LAACLINE                                                      
         OI    6(R1),FVOXMT                                                     
         B     COMXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* CALCULATE AND FORMAT A PERCENT                                      *         
* APFULL(4) = DIVIDEND                                                *         
* APDUB(4)  = DIVISOR                                                 *         
* R3        = A(OUTPUT FIELD)                                         *         
***********************************************************************         
         SPACE 1                                                                
PERCENT  LR    R0,RE                                                            
         MVC   0(3,R3),BLANKS                                                   
         SR    RE,RE               CALCULATE PERCENT                            
         ICM   RF,15,APFULL                                                     
         BZ    PERCX                                                            
         OC    APDUB(4),APDUB                                                   
         BZ    PERCX                                                            
         M     RE,=F'100'                                                       
         D     RE,APDUB                                                         
         L     R1,APDUB                                                         
         SRA   R1,1                                                             
         CR    RE,R1               ROUND                                        
         BL    *+8                                                              
         LA    RF,1(RF)                                                         
         MVI   EBLOUT,3            FORMAT PERCENT                               
         CH    RF,=H'1000'                                                      
         BNL   PERCX                                                            
         CH    RF,=H'100'                                                       
         BNL   *+12                                                             
         MVI   EBLOUT,2                                                         
         MVI   2(R3),C'%'                                                       
         ST    R3,EBAOUT                                                        
         STH   RF,APHALF                                                        
         LA    RE,APHALF                                                        
         ST    RE,EBAIN                                                         
         MVI   EBLIN,2                                                          
         MVI   EBSCIN,0                                                         
         MVI   EBFLOAT,0                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
PERCX    LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* FORMAT THE GOALS                                                    *         
* INPUT  : PARM1=A(TWA GOAL DOLLARS AND POINTS FIELD)                 *         
*          PARM2=A(COMMENT LINE IF POINTS ARE ROUNDED)                *         
*          PARM3=A(GOAL VS POINTS INFO BLOCK)                         *         
***********************************************************************         
         SPACE 1                                                                
FMTGOAL  LR    R2,R1               R2=A(PARAMETER ADDRESS LIST)                 
         L     R6,8(R2)            R6=A(GVA INFO BLOCK)                         
         USING GVAD,R6                                                          
         XC    EBLOCK,EBLOCK       FORMAT GOAL DOLLARS & POINTS                 
         MVI   EBLIN,4                                                          
         MVI   EBLOUT,7                                                         
         MVI   EBTIN,C'B'                                                       
         MVI   EBSCIN,X'82'                                                     
         MVI   EBFLOAT,C'$'                                                     
         L     R1,0(R2)                                                         
         OI    6(R1),FVOXMT        TRANSMIT                                     
         LA    R8,L'FVIHDR(R1)                                                  
         USING TOTD,R8                                                          
         LA    RE,TOTDOL                                                        
         ST    RE,EBAOUT                                                        
         LA    RE,GGLDOL                                                        
         ST    RE,EBAIN                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         TM    GGLDOL,X'80'        TEST NEGATIVE                                
         BZ    FMTG1                                                            
         LA    R0,6                YES-PRINT MINUS SIGN                         
         LA    R1,TOTDOL                                                        
         CLI   0(R1),C'$'                                                       
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         B     FMTG1                                                            
         BCTR  R1,0                                                             
         MVI   0(R1),C'-'                                                       
*                                                                               
FMTG1    MVI   EBFLOAT,C'-'        SHOW NEGATIVES                               
         LA    RE,GGLPTST                                                       
         ST    RE,EBAIN                                                         
         LA    RE,TOTPTS-4                                                      
         ST    RE,EBAOUT                                                        
         MVI   EBLOUT,8                                                         
         MVI   EBSCIN,X'81'                                                     
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
         ZIC   R0,CMPNWKS          WEEKLY POINTS                                
         LA    R3,GGLPTS                                                        
         MVI   GSCALE,X'81'                                                     
*                                                                               
FMTG2    L     R1,0(R3)                                                         
         LPR   R1,R1                                                            
         C     R1,=F'9995'                                                      
         BL    FMTG4                                                            
         MVI   GSCALE,X'82'                                                     
         C     R1,=F'99995'                                                     
         BL    FMTG4                                                            
         MVI   GSCALE,X'83'                                                     
         B     FMTG6                                                            
*                                                                               
FMTG4    LA    R3,4(R3)                                                         
         BCT   R0,FMTG2                                                         
*                                                                               
FMTG6    MVI   EBLIN,4                                                          
         MVI   EBLOUT,4                                                         
         MVC   EBSCIN,GSCALE                                                    
         LA    R0,NMAXWKS                                                       
         LA    R4,TOTPTSWK-1                                                    
         LA    R3,GGLPTS                                                        
         MVC   TOTPTSWK,BLANKS                                                  
*                                                                               
FMTG8    OC    0(4,R3),0(R3)                                                    
         BZ    FMTG10                                                           
         ST    R3,EBAIN                                                         
         ST    R4,EBAOUT                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
FMTG10   LA    R4,4(R4)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,FMTG8                                                         
*                                                                               
         L     R1,4(R2)            DISPLAY COMMENT LINE IF POINTS ARE           
         OI    6(R1),FVOXMT        ROUNDED                                      
         LA    R1,L'FVIHDR(R1)                                                  
         MVC   0(70,R1),BLANKS                                                  
         CLI   GSCALE,X'81'                                                     
         BNH   FMTG12                                                           
         MVC   0(L'COMMENT,R1),COMMENT                                          
         LA    R1,L'COMMENT(R1)                                                 
         CLI   GSCALE,X'82'                                                     
         BNH   *+12                                                             
         MVI   0(R1),C'0'                                                       
         LA    R1,1(R1)                                                         
         LA    R1,1(R1)                                                         
*                                                                               
FMTG12   CLI   CUDMED,C'C'         TEST CANADA                                  
         BNE   FMTGX                                                            
         TM    NETIND,NETIDATA     AND THERE IS NETWORK/SPILL DATA              
         BZ    FMTGX                                                            
         MVC   0(L'CANCMT,R1),CANCMT   YES-DISPLAY COMMENT                      
*                                                                               
FMTGX    B     COMMONX                                                          
         DROP  R6,R8                                                            
         EJECT                                                                  
***********************************************************************         
* AUTO DEMO ADJUSTMENT ROUTINE                                        *         
* INPUT  : R4 = A(TARGET DEMO CODE)                                   *         
*          LADEMEL = A(DEMO ELEMENT)                                  *         
*          APFULL = OLD TARGET RATING                                 *         
*          LNEWRTG = NEW TARGET RATING                                (         
***********************************************************************         
         SPACE 1                                                                
DEMADJ   NTR1                                                                   
         NI    LFLAG,FF-LADJALL                                                 
         OI    LFLAG,LADJIMP       ADJUST TARGET IMPRESSION                     
         TM    CMPOPTS,CAMOAALL+CAMOATGT   TEST ADJUST ALL/TGT                  
         BZ    DA2                                                              
         MVC   APHALF,=X'D901'                                                  
         TM    CMPOPTS,CAMOAALL                                                 
         BO    *+10                                                             
         MVC   APHALF,ESTDEMS+1                                                 
         CLC   APHALF,1(R4)        YES-TEST TARGET IS ADJUSTMENT DEMO           
         BNE   DA2                                                              
         OI    LFLAG,LADJALL       YES                                          
         B     DA4                                                              
*                                                                               
DA2      CLI   1(R4),C'R'          TEST TARGET IS A RATING                      
         BE    DA4                 NO - NO ADJUSTMENTS                          
         CLI   1(R4),C'E'                                                       
         BNE   DAX                                                              
*                                                                               
DA4      L     R1,APFULL           CALCULATE PCT ADJUSTMENT                     
         SR    RE,RE                                                            
         L     RF,LNEWRTG                                                       
         M     RE,=F'2000'                                                      
         DR    RE,R1                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,LDEMADJ          LDEMADJ = PCT ADJUSTMENT                     
*                                                                               
         L     R1,LADEMEL          SCAN ALL DEMOS IN DEMO ALEMENT               
         USING DMOEL,R1                                                         
         ZIC   RF,1(R1)                                                         
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    R1,DMODEMO                                                       
         LA    RE,L'DMODEMO                                                     
         DROP  R1                                                               
*                                                                               
DA6      CLC   1(2,R1),1(R4)       TEST TARGET DEMO                             
         BE    DA12                YES - NO ADJ                                 
         TM    4(R1),DMODEMOV      TEST ALREADY MANUALLY OVERRIDDEN             
         BO    DA12                YES - NO ADJ                                 
         TM    LFLAG,LADJIMP       TEST TARGET IMPRESSION ADJUST                
         BZ    DA8                                                              
         CLI   1(R1),C'I'          YES - TEST ITS THE TARGET IMP                
         BNE   DA8                                                              
         CLC   2(1,R1),2(R4)                                                    
         BE    DA10                      YES - ADJUST                           
*                                                                               
DA8      TM    LFLAG,LADJALL       TEST ADJUST ALL                              
         BZ    DA12                NO                                           
         CLI   1(R1),C'I'          YES - TEST ITS AN IMPRESSION                 
         BNE   DA10                      NO - GO AND ADJUST                     
         L     R2,LADEMEL                YES - CHECK THAT ITS RATING            
         LA    R2,DMODEMO-DMOEL(R2)            HAS NOT BEEN MANUALLY            
*                                              OVERRIDDEN                       
DA9      CLI   1(R2),C'R'                                                       
         BE    *+12                                                             
         CLI   1(R2),C'E'                                                       
         BNE   *+14                                                             
         CLC   2(1,R2),2(R1)                                                    
         BE    *+12                                                             
         BXLE  R2,RE,DA9                                                        
         B     DA10                                                             
         TM    4(R2),DMODEMOV                                                   
         BO    DA12                                                             
*                                                                               
DA10     SR    R2,R2               DO THE ADJUSTMENT                            
         ICM   R2,7,5(R1)                                                       
         SR    R3,R3                                                            
         SRDA  R2,31                                                            
         M     R2,LDEMADJ                                                       
         D     R2,=F'1000'                                                      
         AH    R3,=H'1'                                                         
         SRA   R3,1                                                             
         ST    R3,4(R1)            STORE ADJUSTED DEMO IN ELEMENT               
*                                                                               
DA12     BXLE  R1,RE,DA6           NEXT DEMO                                    
*                                                                               
DAX      B     COMXIT                                                           
         EJECT                                                                  
***********************************************************************         
* DEMO UPGRADE ROUTINE                                                          
* INPUT  : LDEMS = DEMO LIST                                                    
* OUTPUT : LDEMVALS = DEMO VALUES                                               
***********************************************************************         
         SPACE 1                                                                
DEMUP    NTR1                                                                   
         USING BWDRECD,R3                                                       
         XC    LUPVALS,LUPVALS                                                  
         OC    CMPUP,CMPUP                                                      
         BZ    DM2                                                              
         MVC   LUPFIL,CMPUF                                                     
         MVC   LUPGRD,CMPUP                                                     
         MVC   LUPFRBK,CMPFB                                                    
         MVC   LUPFRBKL,CMPFBLST                                                
         MVC   LUPPUT,CMPUPUT                                                   
         MVC   LUPSHR,CMPUSHR                                                   
*                                                                               
DM2      SR    R0,R0                                                            
         LA    R8,BWDEL                                                         
*                                                                               
DM4      CLI   0(R8),0                                                          
         BE    DM10                                                             
         CLI   0(R8),UPGELCDQ                                                   
         BNE   DM6                                                              
         USING UPGEL,R8                                                         
         MVC   LUPFIL,UPGFILE                                                   
         MVC   LUPGRD,UPGRADE                                                   
         MVC   LUPFRBK,UPGFRBK                                                  
         CLI   UPGELLN,51                                                       
         BL    *+10                                                             
         MVC   LUPFRBKL,UPGFRBKL                                                
         MVC   LUPPUT,BWDUPUT                                                   
         MVC   LUPSHR,BWDUSHR                                                   
         B     DM8                                                              
         DROP  R8                                                               
*                                                                               
DM6      CLI   0(R8),ODTELCDQ                                                   
         BNE   DM8                                                              
         USING ODTEL,R8                                                         
         MVC   LOVDAY,ODTDAY                                                    
         MVC   LOVTIME,ODTTIME                                                  
         DROP  R8                                                               
*                                                                               
DM8      IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     DM4                                                              
*                                                                               
DM10     OC    LUPGRD,LUPGRD       TEST UPGRADE EXPRESSION FOUND                
         BNZ   DM12                                                             
         MVC   FVMSGNO,=AL2(FVNOCNUP)   NO-ERROR EXIT                           
         B     DMX                                                              
*                                                                               
DM12     LA    R8,LDMUPBLK                                                      
         USING SPDEMUPD,R8                                                      
         XC    SPDEMUPD(SPDEMUPL),SPDEMUPD                                      
         MVC   SPUPAREC,AIOAREA3                                                
         MVC   SPUPAFAC,ACOM                                                    
         MVC   SPUPAGY,CUAALF                                                   
         MVC   SPUPMED,CUDMED                                                   
         MVC   SPUPCLI,QCLT                                                     
         MVC   SPUPMKT,MKTLKUP                                                  
         MVC   SPUPSTA,BWDSTA                                                   
         MVC   SPUPDAY,BWDDAYS                                                  
         MVC   SPUPTIM,BWDTIMES                                                 
         MVC   SPUPFIL,LUPFIL                                                   
         MVC   SPUPSRC,CLTSRC                                                   
         MVC   SPUPFBK,LUPFRBK                                                  
         MVC   SPUPFBKL,LUPFRBKL                                                
         OC    SPUPFBK,SPUPFBK                                                  
         BNZ   *+16                                                             
         MVC   SPUPFBK,ESTBOOK                                                  
         XC    SPUPFBKL,SPUPFBKL                                                
         MVC   SPUPUDAY,LOVDAY                                                  
         MVC   SPUPUTIM,LOVTIME                                                 
         MVC   SPUPTYPE(L'LUPGRD),LUPGRD                                        
         MVC   SPUPBTYP,STABKTYP                                                
         CLI   CUDMED,C'C'         TEST CANADA                                  
         BNE   *+16                                                             
         TM    CLTIND2,CLTIANFR    AND 1W ANGLO/FRANCO OPTION ON                
         BZ    *+8                                                              
         OI    SPUPOPTS,SPOANGFR   YES                                          
*                                                                               
         CLI   G1WPROF+5,C'I'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPDMAI                                                
*                                                                               
         CLI   G1WPROF+7,C'Y'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM                                                
*                                                                               
         CLI   LUPPUT,C'1'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'N'                                                    
         CLI   LUPPUT,C'2'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'Y'                                                    
         CLI   LUPSHR,C'1'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'N'                                                    
         CLI   LUPSHR,C'2'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'Y'                                                    
         GOTO1 ASPDEMUP,APPARM,LDMUPBLK,LDEMS,LDEMVALS  CALL SPDEMUP            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
DMX      B     COMMONX                                                          
         EJECT                                                                  
EFFS     DC    16X'FF'                                                          
*                                                                               
FF       EQU   X'FF'                                                            
OVERELEM EQU   X'DE'                                                            
NLSTLINS EQU   (WRKL8H-WRKL1H)/(WRKL2H-WRKL1H)                                  
NSKDLINS EQU   7                                                                
NSSKLINS EQU   15                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
BLANKS   DC    100C' '                                                          
COMMENT  DC    CL20'Weekly Pts = Imps/10'                                       
CANCMT   DC    CL47'Goals are combined reduced by network and spill'            
LINKEYT  DC    AL1(KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                    
         EJECT                                                                  
TOTD     DSECT                                                                  
         DS    0CL76                                                            
*                                                                               
TOTDOL   DS    CL7                                                              
         DS    CL1                                                              
TOTDOLPC DS    CL3                                                              
*                                                                               
TOTPAREA DS    0CL65                                                            
         DS    CL1                                                              
TOTPTS   DS    CL4                                                              
         DS    CL1                                                              
TOTPTSPC DS    CL3                                                              
         DS    CL1                                                              
TOTPTSWK DS    CL55                                                             
         EJECT                                                                  
SKDLINED DSECT                                                                  
*                                                                               
SKDLSPH  DS    CL8                                                              
SKDLSP   DS    0CL22                                                            
SKDLIN   DS    CL2                                                              
SKDIND   DS    CL1                                                              
SKDSTA   DS    CL5                                                              
         DS    CL1                                                              
SKDPRG   DS    CL13                                                             
*                                                                               
SKDSKDH  DS    CL8                                                              
SKDSKD   DS    CL55                                                             
*                                                                               
SKDLINEL EQU   *-SKDLINED                                                       
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
LOCALD   DSECT                                                                  
*                                                                               
COMWRK   DS    0C                  COMMON BETWEEN BWS05/BWS06                   
*                                                                               
       ++INCLUDE SPNWS05WRK                                                     
         EJECT                                                                  
LOCALD   DSECT                                                                  
TMPREGR1 DS    A                   TEMPORARY STACK  (BLOWN AWAY)                
TMPREGRE DS    A                                                                
TMPREGRF DS    A                                                                
*                                                                               
         ORG   LOCALD+2048     *** BWS05 WORK AREA ***                          
LASKDLN  DS    A                                                                
LAACLINE DS    A                                                                
LADEMEL  DS    A                                                                
LASPWEL  DS    A                                                                
LREGSV   DS    F                                                                
LFULL    DS    F                                                                
LNEWCOST DS    F                                                                
LNEWRTG  DS    F                                                                
LDEMADJ  DS    F                                                                
LTUNUM   DS    H                                                                
LTUMAX   DS    H                                                                
LPAGE    DS    C                                                                
LPAGDSP  DS    X                                                                
LLINE    DS    X                                                                
LINEDISP DS    CL2                                                              
LRATING  DS    XL3                                                              
LNOSKED  DS    XL(NMAXWKS)                                                      
*                                                                               
LFLAG    DS    X                                                                
LSHRTSKD EQU   X'80'                                                            
LDEMUP   EQU   X'40'                                                            
LADJIMP  EQU   X'20'                                                            
LADJALL  EQU   X'10'                                                            
LPKGSLV  EQU   X'08'                                                            
LPKGMAS  EQU   X'04'                                                            
LINWKCHG EQU   X'02'                                                            
LORBMAS  EQU   X'01'                                                            
*                                                                               
LFLAG2   DS    X                                                                
LDPTMAS  EQU   X'80'                                                            
LEF1SUPP EQU   X'40'               SUPPRESS EFFECTIVE COST 1                    
LEF2SUPP EQU   X'20'               SUPPRESS EFFECTIVE COST 2                    
LEF3SUPP EQU   X'10'               SUPPRESS EFFECTIVE COST 3                    
LEFDTSUP EQU   LEF1SUPP+LEF2SUPP+LEF3SUPP                                       
LCAMP2   EQU   X'08'               READING COMPANION CAMPAIGN                   
*                                                                               
LIND     DS    X                                                                
LPKGNUM  DS    X                                                                
LFSTLINE DS    X                                                                
         DS    XL20                SPARE                                        
LSPW     DS    XL(NMAXWKS)                                                      
LDMUPBLK DS    (SPDEMUPL)X                                                      
LKEYCOMP DS    XL1                                                              
*                                                                               
LUPVALS  DS    0CL24                                                            
LUPFIL   DS    C                                                                
LUPGRD   DS    XL8                                                              
LUPFRBK  DS    XL2                                                              
LUPFRBKL DS    XL6                                                              
LUPPUT   DS    CL1                                                              
LUPSHR   DS    CL1                                                              
LOVDAY   DS    X                                                                
LOVTIME  DS    XL4                                                              
*                                                                               
LDEMFRZ  DS    XL(2*LNDEMOS)                                                    
         DS    X                                                                
LDEMS    DS    XL(3*LNDEMOS)                                                    
         DS    X                                                                
LDEMVALS DS    XL(4*LNDEMOS)                                                    
*                                                                               
SVINKEYN DS    XL(L'INKEYN)                                                     
SVINKEYT DS    XL(L'INKEYT)                                                     
*                                                                               
SVBCAM   DS    XL(L'BCAM)                                                       
SVBVALS  DS    XL(BVALSX-BVALS)                                                 
SVQVALS  DS    XL(QVALSX-QVALS)                                                 
*                                                                               
TSARREC  DS    (TUPDRECL)X         TSAR RECORD                                  
         EJECT                                                                  
       ++INCLUDE SPDEMLK                                                        
         SPACE 2                                                                
         ORG   LOCALD+3072     *** RESERVED FOR BWS06 ***                       
         DS    0H                                                               
         ORG   LOCALD+4096                                                      
         SPACE 1                                                                
LOCALX   EQU   *                                                                
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSFBD                                                       
         EJECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSFAD                                                       
         EJECT                                                                  
* SPNWSHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSHDR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDTWABLDD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDTWABLDD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENEST                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPDEMUPD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDEMUPD                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPNWSCAM                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSCAM                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'069SPNWS05WZ 08/13/97'                                      
         END                                                                    
