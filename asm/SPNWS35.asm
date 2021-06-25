*          DATA SET SPNWS35    AT LEVEL 242 AS OF 09/24/08                      
*PHASE T20735A,*                                                                
         TITLE 'NWS35 - BUYERS WORK SHEET - BUY LIST/SELECT OVERLAY'            
T20735   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20735**,RA,RR=RE                                              
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
*&&DO                                                                           
         CLI   INOSID,0            OR SID OPTION SPECIFIED                      
         BE    BWS2                   AND NOT WORK TRANSFER                     
         CLI   APACTN,ACTXFR                                                    
         BNE   BWS4                                                             
         CLI   INREC,RECBUY                                                     
         BNE   BWS4                YES-CALL BWS06                               
*&&                                                                             
BWS2     MVI   APBYTE,X'37'                                                     
         CLI   APACTN,ACTXFR       TEST FOR BUYS/TRANSFER                       
         BNE   BWS6                YES-CALL BWS37                               
*                                                                               
BWS4     MVC   COMPARM,APPARM     (SAVE APPARM)                                 
         XC    APPARM,APPARM                                                    
         MVC   APPARM(1),APBYTE       SET OVERLAY NUMBER                        
         GOTO1 VCOLY,APPARM,,0,0      CALL OVERLAY                              
         L     RF,0(R1)                                                         
         BASR  RE,RF                                                            
         B     EXIT                                                             
*                                                                               
BWS6     TM    APROFBTS,A00CANAD   TEST CANADIAN AGENCY                         
         BZ    BWS8                 - NOPE, CONTINUE NORMALLY                   
         MVC   FVMSGNO,=AL2(1269)    NO NBR FOR CANADIANS                       
         CLC   FVMSGNO,=AL2(FVFOK)   SETS CONDITION JUST IN CASE                
         B     EXIT                                                             
*                                                                               
BWS8     LA    R2,IOKEY                                                         
         USING BWHRECD,R2                                                       
         LA    R3,APRECKEY                                                      
         USING BWDRECD,R3                                                       
*                                                                               
         XR    RF,RF                                                            
         IC    RF,APMODE                                                        
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
         B     DISACHG                                                          
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
         DROP  R3                                                               
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
FSTSCR   BRAS  RE,FRSTLSCR                                                      
*                                                                               
FSCRX    B     EXIT                                                             
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
         GOTO1 =A(REMVGLOB),RR=APRELO   WE NEED TO REMOVE GLOBBER?              
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
*&&DO                                                                           
         TM    TWAINDS,TWAICCSW    SWITCHED INTO COMPANION CAMPAIGN?            
         BZ    GETS0                                                            
         MVC   LTUNUM,SVTUNUM2                                                  
         MVC   LTUMAX,SVTUMAX2                                                  
         MVC   LPAGE,SVPAGE2                                                    
         MVC   LPAGDSP,SVPAGDS2                                                 
         MVC   LLINE,SVLINE2                                                    
*&&                                                                             
GETS0    CLI   APINDS,APILFLST     TEST FOR VERY FIRST TIME                     
         BNE   GETS1                                                            
         MVI   LLINE,1             YES - INITIALIZE PAGE NUMBER                 
         MVI   LPAGE,C'A'                                                       
         MVI   LPAGDSP,0                                                        
         MVC   LTUNUM,=H'1'        START AT FIRST RECORD                        
         B     GETS1A                                                           
*                                                                               
GETS1    CLI   APINDS,APILNLST     TEST FOR FIRST LINE OF SCREEN                
         BNE   GETS73                                                           
*                                                                               
GETS1A   MVI   SVACTIV,0           NO ACTIVITY FOR THIS SCREEN YET              
         MVI   FVMAXL,2                                                         
         GOTO1 AFVAL,WRKSCRH       VALIDATE SCROLL                              
         BH    GETSX                                                            
         BL    GETS4                                                            
****     MVC   SCRLINPT,WRKSCR     SAVE OFF NEW PAGE LETTER                     
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
         XR    RE,RE                                                            
         IC    RE,LLINE                                                         
         AR    R1,RE                                                            
         TM    TWAINDS,TWAICCSW                                                 
         BZ    *+8                                                              
         AH    R1,SVTUMAX                                                       
         STH   R1,LTUNUM                                                        
*                                                                               
GETS4    MVC   LFSTLINE,LLINE      SAVE FIRST LINE NUMBER                       
         CLI   APINDS,APILFLST     TEST FOR FIRST LINE / FIRST PAGE             
         BNE   GETS76              NO                                           
*                                                                               
         LA    R4,TSARREC          YES-DELETE EXISTING TSAR RECORDS             
         USING TRECD,R4                                                         
         XC    TSARREC(TBUYRECL),TSARREC                                        
**       MVI   APBYTE,1            SET CAMPAIGN SEQ NUM = 1                     
**       TM    TWAINDS,TWAICCSW    OR 2 FOR COMPANION CAMPAIGN                  
**       BZ    *+8                                                              
**       MVI   APBYTE,2                                                         
**       MVC   TUCAMP,APBYTE                                                    
*                                                                               
GETS5    MVI   TSARACT,TSARDH                                                   
         GOTO1 ATSAR,TREC                                                       
         BE    GETS6                                                            
         CLC   FVMSGNO,=AL2(FVFERNF)  TEST EXACT RECORD NOT FOUND               
         BNE   GETS7                  NO-MUST BE EOF                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
GETS6    MVI   TSARACT,TSADEL      YES-DELETE IT                                
         GOTO1 ATSAR,TREC                                                       
         BE    GETS5               NEXT RECORD                                  
         DC    H'0'                                                             
*                                                                               
GETS7    MVC   FVMSGNO,=AL2(FVFOK)                                              
         XC    SVACPTS(4*53),SVACPTS   CLEAR ACTUAL POINTS ACCUMS               
         XC    SVACXPTS,SVACXPTS                                                
         XC    SVACDOL,SVACDOL     CLEAR ACTUAL DOLLARS                         
         XC    SVGLDOL,SVGLDOL     CLEAR GOALS                                  
         XC    SVGLPTST,SVGLPTST                                                
         XC    SVGLPTS(4*53),SVGLPTS                                            
*                                                                               
         MVI   LFLAG,0                                                          
         LA    RE,BUYMSTA+2-BUYKEY  SET KEY COMPARE LENGTH UP TO STA            
         OC    QSTA,QSTA            ANY STATION FILTER?                         
         BZ    *+8                                                              
         LA    RE,BUYKBUY-BUYKEY   YES, COMPARE UPTO ESTIMATE                   
         BCTR  RE,0                                                             
         STC   RE,LKEYCOMP                                                      
         XC    COMDPLST,COMDPLST                                                
         XC    COMSLLST,COMSLLST                                                
         SR    R8,R8               R8 = RECORD COUNT                            
         MVC   IOKEY(13),APRECKEY                                               
*                                                                               
GETSRDHI LA    R1,DIRHI+IO2        START READING BUY KEYS                       
         B     GETSIO                                                           
GETSRDSQ LA    R1,DIRSQ+IO2        READ NEXT BUY KEY                            
*                                                                               
GETSIO   LA    R2,IOKEY                                                         
         USING BUYKEY,R2                                                        
         GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)   TEST REACHED END                           
         B     GETS30                                                           
*                                                                               
         XR    RE,RE                                                            
         IC    RE,LKEYCOMP                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   GETS30                                                           
*                                                                               
         OC    QSTA,QSTA           DO WE HAVE A STATION FILTER?                 
         BNZ   GETS8                 YES, EST SHOULD HAVE BEEN CHECKED          
         OC    QCABLE,QCABLE       IF CABLE, QCABLE IS SET, NOT QSTA            
         BZ    GETS7A                 ....  WHY TIM, WHY?                       
*                                                                               
         GOTO1 VMSUNPK,APPARM,(X'80',BUYMSTA),APWORK,APWORK+L'QMKT              
         CLC   QCABLE(4),APWORK+L'QMKT                                          
         BE    GETS7A              MATCHED CABLE, STILL CHECK EST               
         BL    GETS30                                                           
         B     GETS7A00                                                         
*                                                                               
GETS7A   CLC   BUYKEST,CMPESTN     DOES THE ESTIMATE MATCH CAMP EST?            
         BE    GETS8               YES                                          
         BL    GETS7B              LESS THAN, SET TO CAMP EST AND TRY           
         SR    RE,RE               GREATER, BUMP STA AND SET EST                
GETS7A00 ICM   RE,7,BUYMSTA+2                                                   
         LA    RE,1(RE)                                                         
         STCM  RE,7,BUYMSTA+2                                                   
*                                                                               
GETS7B   MVC   BUYKEST,CMPESTN                                                  
         XC    BUYKBUY,BUYKBUY                                                  
         B     GETSRDHI                                                         
*                                                                               
GETS8    GOTO1 AIO,FILGET2                                                      
         BE    *+14                                                             
         CLI   IOERR,X'02'         RECORD IS DELETED THOUGH                     
         BE    GETSRDSQ                                                         
         DC    H'0'                                                             
*                                                                               
         L     RE,AIOAREA3         CLEAR OUT ANY OLD NBR RECORD                 
         LA    RF,2000                ON A NEW BUY RECORD                       
         XCEFL                                                                  
*                                                                               
         L     R2,AIOAREA2         ADDRESS THE RECORD                           
***************                                                                 
         CLC   BMKT,BUYMSTA        SPILL BUY?                                   
         BNE   GETSRDSQ            YES                                          
***************                                                                 
* DON'T NEED TO ADD TSAR REC YET, BUY COULD BE PART OF THIS CAMP                
*                             AND BUY IS NOT A MAKEGOOD                         
         NI    LFLAG2,X'FF'-LF2ATSRR-LF2NPOCM-LF2MKGDB                          
         NI    MISCFLG1,X'FF'-MF1MANUL-MF1COSOV-MF1ECSTS-MF1DSNBR               
         MVI   CURRCOVR,0          WE HAVEN'T DONE ANY COST OVERRIDES           
         MVI   COSTNUMB,0                                                       
         XC    COSTABLE,COSTABLE                                                
*                                                                               
*&&DO                                                                           
         CLI   BSLN,0              TEST SPOT LENGTH FILTER                      
         BE    *+14                                                             
         CLC   BDSEC,BSLN          YES-MATCH THE LENGTH                         
         BNE   GETSRDSQ                                                         
*                                                                               
         CLI   BDPT,0              ANY DAYPART FILTER?                          
         BE    GETS8B                                                           
         CLC   BDDAYPT,BDPT        MATCH ON DAYPART?                            
         BNE   GETS8A00                                                         
         CLI   DPTTYPE,C'S'        IS DAYPART FILTER A SUBDAYPART?              
         BNE   GETS8B              NO, NOTHING TO WORRY ABOUT                   
         TM    CLTIND3,CLTIMSDP    ARE WE SHOWING MASTER AND SUBDPT?            
         BNZ   GETSRDSQ            YES, ONLY MASTER DPT LIKE WORK/SKED          
         B     GETS8B                                                           
********                                                                        
*   BUYLINE ONLY HAVE ONE DAYPART CODE  (NOT MASTER AND SUB IN RECORD)          
********                                                                        
GETS8A00 LA    R1,DPTSUBS          YES-ACCEPT ANY OF THE SUB-DPTS               
         LA    R0,L'DPTSUBS                                                     
GETS8A10 CLI   0(R1),0                                                          
         BE    GETSRDSQ            DOESN'T EVEN MATCH A SUB                     
         CLC   BDDAYPT,0(R1)                                                    
         BNE   *+12                                                             
         OI    LFLAG2,LDPTMAS      INDICATE WE HAVE A DPT OTHER                 
         B     GETS8A20            THAN THE MASTER                              
         LA    R1,1(R1)                                                         
         BCT   R0,GETS8A10                                                      
         B     GETSRDSQ                                                         
*                                                                               
GETS8A20 CLI   INODPT,C'M'         TEST DPT=MAS                                 
         BNE   GETS8B                                                           
         CLI   DPTTYPE,C'M'                                                     
         BNE   GETSRDSQ                                                         
*                                                                               
*&&                                                                             
GETS8B   CLC   BDSTART,CMPST       HAVE AN INTERSECTION HERE?                   
         BH    GETS8B10                                                         
         CLC   BDEND,CMPST         BUY DATE NOT IN CAMPAIGN RANGE               
         BL    GETSRDSQ                                                         
         B     GETS8C                                                           
*                                                                               
GETS8B10 CLC   BDSTART,CMPND       BUY DATE NOT IN CAMPAGIN RANGE               
         BH    GETSRDSQ                                                         
*                                                                               
GETS8C   OC    BDMGDATE,BDMGDATE   MAKEGOOD BUY?                                
         BZ    *+8                                                              
         OI    LFLAG2,LF2MKGDB                                                  
*                                                                               
         LA    R9,BDELEM                                                        
         SR    R0,R0                                                            
GETS9    CLI   0(R9),0                                                          
         BNE   *+12                                                             
         OI    MISCFLG1,MF1MANUL   NO BWS XFR ELEMENT, MANUALLY ADDED           
         B     GETS10                                                           
*                                                                               
         CLI   0(R9),BWSCODEQ                                                   
         BE    *+14                                                             
         IC    R0,1(R9)                                                         
         AR    R9,R0                                                            
         B     GETS9                                                            
*                                                                               
         MVC   APDUB(2),BCAM                                                    
         XC    APDUB(2),XFF                                                     
*                                                                               
         USING BWSELEM,R9                                                       
         CLC   BWSBYR,QBYR                                                      
         BNE   *+14                                                             
         CLC   BWSCAM,APDUB                                                     
         BE    GETS10                                                           
         OI    LFLAG2,LF2NPOCM     THIS BUY IS NOT PART OF THIS CAMP            
         L     RE,AIOAREA3         CLEAR ANY OLD BUY REVISION RECORD            
         LA    RF,2000                                                          
         XCEFL                                                                  
****                                                                            
         DROP  R9                                                               
*&&DO                                                                           
GETS8C   NI    LFLAG2,255-LEFDTSUP                                              
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
         BE    GETSRDSQ            NO SCHEDULE - GET NEXT RECORD                
         CLI   0(R9),SPWELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R9)                                                         
         AR    R9,R0                                                            
         B     GETS9A                                                           
*                                                                               
GETS9B   XR    RF,RF               YES-SEE IF THERE NO SPOTS FOR ANY            
         IC    RF,1(R9)                                                         
         AR    RF,R9                   EFFECTIVE DATE RANGES                    
         LA    R9,SPWPERWK-SPWEL(R9)                                            
*                                                                               
         LR    RE,R5                                                            
         AHI   RE,CMPDATSP-TWAD                                                 
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
         BE    GETSRDSQ                THIS IS A CRAP RECORD - IGNORE           
         CLI   BWDKELSQ,0          TEST SLAVE                                   
         BE    GETS10                                                           
         TM    BWDINDS,BWDIORB     YES-TEST ORBIT SLAVE                         
         BO    GETSRDSQ            YES-NEXT RECORD                              
         OI    LFLAG,LPKGSLV       ELSE, INDICATE PACKAGE SLAVE                 
         B     GETS13              MAYBE NEW SPOT LENGTH                        
*                                                                               
GETS10   TM    BWDINDS,BWDIPKG     TEST PACKAGE MASTER                          
         BZ    *+12                                                             
         OI    LFLAG,LPKGMAS       YES-INDICATE PACKAGE MASTER                  
         B     GETS11                                                           
         OI    LFLAG,LORBMAS       ELSE INDICATE ORBIT MASTER                   
*&&                                                                             
GETS10   L     R2,AIOAREA2                                                      
         USING BUYKEY,R2                                                        
******  WE NEED THE NBR RECORD IN AIOAREA3 IF IT EXISTS!!!  *******             
         MVC   APWORK(L'IOKEY),IOKEY   SAVE COPY OF BUY KEY WE'RE ON            
         XC    IOKEY,IOKEY                                                      
         LA    RE,IOKEY                                                         
         USING NBRKEY,RE           LOOK FOR NWS BUY REVISION RECORD             
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ      BINARY CAMPAIGN/MARKET SEQ #                 
         MVC   NBRKSTA,BUYMSTA+L'BMKT                                           
         MVC   NBRKKBUY,APWORK+BUYKBUY-BUYKEY                                   
*                                                                               
         GOTO1 AIO,DIRHI+IO3                                                    
         BNE   GETS10A                                                          
*                                                                               
         CLC   IOKEY(L'NBRKEY),IOKEYSAV   FIND OUR BUY REVISION RECORD?         
         BNE   GETS10A                                                          
*                                                                               
         GOTO1 AIO,FILGET3                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    MISCFLG1,MF1DSNBR    - WE GOT AN NBR RECORD!                     
*                                                                               
GETS10A  MVC   IOKEY,APWORK        NO, RE-ESTABLISH OUR BUY KEY                 
         GOTO1 AIO,DIRRD+IO2                                                    
*                                                                               
         TM    MISCFLG1,MF1DSNBR                                                
         BNO   GETS11              WE DON'T HAVE NBR                            
*                                                                               
         L     RE,AIOAREA3                                                      
         USING NBRRECD,RE                                                       
         LA    RE,NBRFSTEL                                                      
         USING NBRSELD,RE          WE'RE NOW READY TO USE NBR INFO              
*                                                                               
**********  DAYPT AND SLN FILTERS FOR NO NBR RECORD BELOW  ************         
GETS11   CLI   BSLN,0              TEST SPOT LENGTH FILTER                      
         BE    GETS11C                                                          
         TM    MISCFLG1,MF1DSNBR                                                
         BNO   GETS11A                                                          
*                                                                               
         CLC   NBRSSLN,BSLN        YES-MATCH THE LENGTH  NBR                    
         BNE   GETSRDSQ                                                         
         B     GETS11C                                                          
GETS11A  CLC   BDSEC,BSLN          YES-MATCH THE LENGTH                         
         BNE   GETSRDSQ                                                         
*                                                                               
GETS11C  CLI   BDPT,0              ANY DAYPART FILTER?                          
         BE    GETS11T                                                          
         TM    MISCFLG1,MF1DSNBR                                                
         BNO   GETS11D                                                          
*                                                                               
         CLC   NBRSDYPT,BDPT       MATCH ON DAYPART?  NBR                       
         BNE   GETS11F                                                          
         B     GETS11E                                                          
GETS11D  CLC   BDDAYPT,BDPT        MATCH ON DAYPART?                            
         BNE   GETS11F                                                          
*                                                                               
GETS11E  CLI   DPTTYPE,C'S'        IS DAYPART FILTER A SUBDAYPART?              
         BNE   GETS11T             NO, NOTHING TO WORRY ABOUT                   
         TM    CLTIND3,CLTIMSDP    ARE WE SHOWING MASTER AND SUBDPT?            
         BNZ   GETSRDSQ            YES, ONLY MASTER DPT LIKE WORK/SKED          
         B     GETS11T                                                          
********                                                                        
*   BUYLINE ONLY HAVE ONE DAYPART CODE  (NOT MASTER AND SUB IN RECORD)          
********                                                                        
GETS11F  LA    R1,DPTSUBS          YES-ACCEPT ANY OF THE SUB-DPTS               
         LA    R0,L'DPTSUBS                                                     
*                                                                               
GETS11H  CLI   0(R1),0                                                          
         BE    GETSRDSQ            DOESN'T EVEN MATCH A SUB                     
         TM    MISCFLG1,MF1DSNBR                                                
         BNO   GETS11I                                                          
*                                                                               
         CLC   NBRSDYPT,0(R1)                                                   
         BNE   GETS11J                                                          
         OI    LFLAG2,LDPTMAS      INDICATE WE HAVE A DPT OTHER                 
         B     GETS11M             THAN THE MASTER                              
*                                                                               
GETS11I  CLC   BDDAYPT,0(R1)                                                    
         BNE   GETS11J                                                          
         OI    LFLAG2,LDPTMAS      INDICATE WE HAVE A DPT OTHER                 
         B     GETS11M             THAN THE MASTER                              
*                                                                               
GETS11J  LA    R1,1(R1)                                                         
         BCT   R0,GETS11H                                                       
         B     GETSRDSQ                                                         
*                                                                               
GETS11M  CLI   INODPT,C'M'         TEST DPT=MAS                                 
         BNE   GETS11T                                                          
         CLI   DPTTYPE,C'M'                                                     
         BNE   GETSRDSQ                                                         
*                                                                               
****************************************  MHC  02/06/04  ************           
GETS11T  TM    LFLAG2,LDPTMAS      TEST MASTER AND SUB DAYPARTS                 
         BO    *+12                                                             
         CLI   BDPT,0              OR NO DAYPART FILTER                         
         BNE   GETS13                                                           
         LA    R1,COMDPLST         YES-ADD THIS DAYPART TO LIST                 
         LA    R0,L'COMDPLST                                                    
*                                                                               
GETS12   CLI   0(R1),0                                                          
         BNE   GETS12A                                                          
         MVC   0(1,R1),BDDAYPT                                                  
         TM    MISCFLG1,MF1DSNBR   WE HAVE AN NBR RECORD?                       
         BNO   GETS13               - NOPE, CONTINUE                            
         MVC   0(1,R1),NBRSDYPT                                                 
         B     GETS13                                                           
*                                                                               
GETS12A  TM    MISCFLG1,MF1DSNBR   NBR OR NO?  (COMPARE WITH NBR OR NO)         
         BO    GETS12B                                                          
         CLC   BDDAYPT,0(R1)                                                    
         BE    GETS13                                                           
         B     GETS12E                                                          
GETS12B  CLC   NBRSDYPT,0(R1)                                                   
         BE    GETS13                                                           
*                                                                               
GETS12E  LA    R1,1(R1)                                                         
         BCT   R0,GETS12                                                        
         DC    H'0'                                                             
*                                                                               
GETS13   CLI   BSLN,0              TEST SPOT LENGTH FILTER                      
         BNE   GETS15                                                           
         LA    R1,COMSLLST         NO-ADD THIS LENGTH TO LIST                   
         LA    R0,L'COMSLLST                                                    
*                                                                               
GETS14   CLI   0(R1),0                                                          
         BNE   GETS14A                                                          
         MVC   0(1,R1),BDSEC                                                    
         TM    MISCFLG1,MF1DSNBR   WE HAVE AN NBR RECORD?                       
         BNO   GETS15               - NOPE, CONTINUE                            
         MVC   0(1,R1),NBRSSLN                                                  
         B     GETS15                                                           
*                                                                               
GETS14A  TM    MISCFLG1,MF1DSNBR   NBR OR NO?                                   
         BO    GETS14B                                                          
         CLC   BDSEC,0(R1)                                                      
         BE    GETS15                                                           
         B     GETS14E                                                          
GETS14B  CLC   NBRSSLN,0(R1)                                                    
         BE    GETS15                                                           
*                                                                               
GETS14E  LA    R1,1(R1)                                                         
         BCT   R0,GETS14                                                        
         DC    H'0'                                                             
         DROP  RE                  ** WE'RE DONE WITH NBR RECORD **             
*                                                                               
GETS15   TM    LFLAG,LPKGSLV       TEST PACKAGE SLAVE                           
         BO    GETS16              YES-GET THE RATING                           
         TM    LFLAG2,LCAMP2       NO TSAR RECORDS FOR COMPANION CAMPN          
         BO    GETS16                                                           
         LA    R4,TSARREC          BUILD TSAR RECORD                            
         USING TRECD,R4                                                         
         XC    TSARREC(TBUYRECL),TSARREC                                        
         MVC   TRECL,=Y(TBUYRECL)                                               
         MVI   TRECTYP,TRECBUY          WE GOT A REGULAR BUYLINE                
*                                                                               
GETS15A  TM    LFLAG2,LF2NPOCM     THIS BUY NOT PART OF THIS CAMP?              
         BZ    *+8                                                              
         OI    TBUYFLG1,TBF1NTCM   YES, NEED TO PROTECT LINE LATER              
         TM    LFLAG2,LF2MKGDB     THIS BUY A MAKEGOOD?                         
         BZ    *+8                                                              
         OI    TBUYFLG1,TBF1MKGD   YES, NEED TO SHOW A LOWER CASE 'M'           
         TM    MISCFLG1,MF1MANUL   MANUALLY ADDED BUY (NOT XFR BUY)?            
         BZ    *+8                                                              
         OI    TBUYFLG1,TBF1MANL   YES                                          
*                                                                               
         MVC   TBUYSTA,BUYMSTA+L'BMKT   BINARY STATION CODE                     
         MVC   TBUYBUYS,IOKEY+BUYKBUY-BUYKEY     BUY DETAILS IN KEY             
         MVC   TBUYLINE,BUYKBUY    BUY LINE #                                   
         MVC   TBUYDAYS,BDDAY                    DAYS                           
         MVC   TBUYTIMS(L'TBUYTIMS*2),BDTIMST    TIMES                          
         MVC   TBUYPROG,BDPROGRM                 PROGRAM                        
         MVC   TBUYADJC,BDPROGT                  ADJACENCY CODE                 
*                                                                               
         MVC   TBUYDYPT,BDDAYPT                  DAYPART                        
*                                                                               
         CLI   BDPT,0              ANY DAYPART FILTER?                          
         BE    GETSB16             NO, WE DON'T KNOW WHAT THE MASTER IS         
         CLC   BDDAYPT,BDPT        YES, SAME DAYPART?                           
         BE    GETSB17                                                          
         TM    CLTIND3,CLTIMSDP    MASTER/SUBDPT COMBINED?                      
         BZ    GETSB17             NO, DAYPART IS WHAT IS ON BUYLINE            
         MVC   TBUYDYPT,BDPT       NO, THEN DAYPART HAS TO BE A SUBDPT          
         B     GETSB16A                                                         
*                                                                               
GETSB16  TM    CLTIND3,CLTIMSDP    MASTER/SUBDPT COMBINED?                      
         BZ    GETSB17             NO, DAYPART IS WHAT IS ON BUYLINE            
         MVC   APHALF,BDPT         SAVE SO FILTERS DON'T GET CHANGED            
*                                                                               
         MVC   APWORK(L'IOKEY),IOKEY   SAVE OUR KEY FOR READ SEQ                
         GOTO1 AGETDPT,BDDAYPT                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,APWORK            RE-ESTABLISH OUR READ SEQ                
         GOTO1 AIO,DIRRD+IO2                                                    
*                                                                               
         MVC   BDPT(2),APHALF      RESTORE FILTERS IF ANY                       
         CLI   DPTTYPE,C'S'        TEST THIS IS A SUB DAYPART                   
         BNE   GETSB17             NO                                           
         MVC   TBUYDYPT,DPTMAS     YES-SET DAYPART TO THE MASTER                
*                                                                               
GETSB16A MVC   TBUYSDPT,BDDAYPT        AND SET SUB-DAYPART                      
*                                                                               
GETSB17  MVC   TBUYSLN,BDSEC                     SPOT LENGTH                    
         TM    MISCFLG1,MF1COSOV   ARE WE UPTO COST OVERRIDES                   
         BZ    GETSB20                                                          
         TM    MISCFLG1,MF1ECSTS   DO WE HAVE EFFECTIVE COSTS?                  
         BZ    GETSB20                                                          
         CLI   CURRCOVR,3          YES, FINISHED WITH EFFECTIVE COSTS?          
         BNL   GETSB20                                                          
         L     RE,AIOAREA3         NO                                           
         USING NBRKEY,RE                                                        
         LA    RE,NBRFSTEL                                                      
         USING NBRSELD,RE                                                       
         LA    RF,NBRSEDT2                                                      
         CLI   CURRCOVR,1                                                       
         BE    *+8                                                              
         LA    RF,NBRSEDT3                                                      
         OC    0(2,RF),0(RF)       IF EFFECTIVE DATE IS 0                       
         BZ    GETS29A             THEN NEXT RECORD                             
         DROP  RE                                                               
         GOTO1 VDATCON,APPARM,(3,(RF)),(2,APHALF)                               
*                                                                               
GETSB20  LA    R2,BDELEM                                                        
         USING REGELEM,R2                                                       
         XC    OTOBITS,OTOBITS                                                  
*                                                                               
GETSB30  CLI   0(R2),0             END OF RECORD?                               
         BE    GETS16                                                           
*                                                                               
GETSB32  CLI   0(R2),X'06'         NON-POL SPOT ELEMENT?                        
         BL    GETSB70                                                          
         CLI   0(R2),X'07'                                                      
         BNH   GETSB40                                                          
*                                                                               
         CLI   0(R2),X'0B'         POL SPOT ELEMENT?                            
         BL    GETSB70                                                          
         CLI   0(R2),X'0C'                                                      
         BH    GETSB70                                                          
*                                                                               
         CLI   BPRD,X'FF'          CAMPAIGN SET UP FOR POL?                     
         BNE   GETSB33                                                          
         OC    INOPRD(2),INOPRD    YES, ANY SPECIFIC PRODUCT FILTER?            
         BNZ   GETSB32A              YES, LIST ONLY SPOTS W/ THOSE PRDS         
         OC    CMPPRD1(2),CMPPRD1    DID CAMPAIGN HAVE PIGGYBACK?               
         BZ    GETSB40               NONE, LIST ALL SPOTS                       
         MVC   INOPRD(2),CMPPRD1                                                
*                                                                               
GETSB32A CLI   INOPRD,X'FF'        FILTER ONLY UNALLOCATED SPOTS?               
         BNE   GETSB32G                                                         
         CLI   RLEN,RPALLOC-REGELEM     YES, ANY ALLOCATIONS?                   
         BH    GETSB70                       YES, NEXT SPOT                     
         B     GETSB40                       NONE, LIST THIS                    
*                                                                               
GETSB32G CLI   RLEN,RPALLOC-REGELEM     YES, ANY ALLOCATIONS?                   
         BNH   GETSB70                       NONE, NEXT SPOT                    
         CLI   INOPRD+1,0          LOOKING FOR A 2ND PRODUCT ALSO?              
         BE    GETSB32M            NO, JUST ONE PRODUCT WE NEED                 
         CLI   RLEN,RPALLOC-REGELEM+L'RPALLOC*2                                 
         BL    GETSB70                                                          
         CLC   INOPRD(1),RPPRD                                                  
         BNE   GETSB70                                                          
         CLC   INOPRD+1(1),RPPRD+L'RPALLOC                                      
         BNE   GETSB70                                                          
         B     GETSB40                     USE THIS ONE                         
*                                                                               
*  ONLY ONE PRODUCT WE'RE LOOKING FOR IN THIS POL BUY                           
GETSB32M CLI   RLEN,RPALLOC-REGELEM+L'RPALLOC                                   
         BH    GETSB70                                                          
         CLC   INOPRD(1),RPPRD                                                  
         BNE   GETSB70                                                          
         B     GETSB40                                                          
*                                                                               
GETSB33  CLC   RPPRD,BPRD          MATCHES ON THE CAMPAIGN PRODUCT?             
         BE    GETSB34             YES                                          
         CLI   CMPPRD2,0           NO, MAKE SURE NOT PR2-PR1                    
         BE    GETSB70                 CAN'T BE                                 
         CLC   RPPRD,CMPPRD2                                                    
         BNE   GETSB70                 CAN'T BE                                 
         CLI   RLEN,RPSTAT2+L'RPSTAT2-REGELEM   PIGGYBACK IN ELEMENT?           
         BNH   GETSB70                          NO                              
         CLC   RPPRD+L'RPALLOC,BPRD                                             
         BNE   GETSB70                                                          
         B     GETSB40                                                          
*                                                                               
GETSB34  CLI   CMPPRD2,0           ANY CAMPAIGN PIGGYBACK PRODUCT?              
         BNE   GETSB34A            YES, MAKE SURE THERE IS A PB IN ELEM         
         CLI   RLEN,RPSTAT2+L'RPSTAT2-REGELEM   PIGGYBACK IN ELEMENT?           
         BH    GETSB70                          YES, SHOULDN'T BE               
         B     GETSB40                                                          
*                                                                               
GETSB34A CLI   RLEN,RPSTAT2+L'RPSTAT2-REGELEM   PIGGYBACK IN ELEMENT?           
         BNH   GETSB70                          NO                              
         CLC   RPPRD+L'RPALLOC,CMPPRD2          MATCH CAMP PIGGYBACK?           
         BNE   GETSB70                          NOPE                            
*                                                                               
GETSB40  LR    R1,R5               LIST OF FLIGHT DATES                         
         AHI   R1,CMPDATSP-TWAD                                                 
GETSB42  OC    0(2,R1),0(R1)                                                    
         BZ    GETSB70             NO MATCH IN FLIGHT DATES, NEXT SPOT          
*                                                                               
         CLC   RDATE,0(R1)         SPOT DATE IS IN THIS FLIGHT WEEK?            
         BL    *+14                                                             
         CLC   RDATE,2(R1)                                                      
         BNH   GETSB44                                                          
         LA    R1,4(R1)            CHECK NEXT FLIGHT WEEK                       
         B     GETSB42                                                          
*                                                                               
GETSB44  LR    RE,R1               RE = NTH DISPLACED WEEK IN SCHEDULE          
         LR    R0,R5                                                            
         AHI   R0,CMPDATSP-TWAD                                                 
         SR    RE,R0                                                            
         SRL   RE,2                DIVIDE BY 4                                  
         LA    RF,TBUYSKED(RE)                                                  
*                                                                               
         LA    R0,1                                                             
         CLI   RCODE,X'0B'         POOL SPOT?                                   
         BNL   *+8                                                              
         IC    R0,RNUM                                                          
*****                                                                           
*****    CLI   RCODE,X'07'         DO WE HAVE AN OTO?                           
*****    BE    *+12                                                             
         CLI   RCODE,X'0C'                                                      
         BNE   GETSB45             NO                                           
*                                                                               
         LR    R5,RE               DIVIDE BY 8 TO FIND OUT WHICH BYTE           
         SRL   R5,3                  OF OTOBITS TO MODIFY                       
         LA    R5,OTOBITS(R5)                                                   
         LR    R3,RE               R3 = REMAINDER AFTER DIVIDING BY 8           
         SLL   R3,32-3                                                          
         SRL   R3,32-3                                                          
         ZIC   R4,0(R5)            ALL WE HAVE TO DO IS MODIFY A BYTE           
         SLL   R4,0(R3)                                                         
         STC   R4,APBYTE                                                        
         OI    APBYTE,X'80'                                                     
         ICM   R4,1,APBYTE                                                      
         SRL   R4,0(R3)                                                         
         STC   R4,0(R5)           THAT SHOULD DO IT                             
*                                                                               
         LA    R4,TSARREC                                                       
         L     R5,ATWA                                                          
*****                                                                           
GETSB45  TM    RSTATUS,X'80'       MINUS SPOT?                                  
         BZ    *+6                                                              
         LNR   R0,R0               YES, NEGATE TO SUBTRACT                      
*                                                                               
GETSB46  OI    LFLAG2,LF2ATSRR     NEED TO ADD TSAR REC FOR THIS BUY            
*                                                                               
         CLI   REGELEM,X'0B'       POL SPOTS?                                   
         BL    GETSB69             NO, COST OVERRIDES AREN'T APPL HERE          
*                                                                               
         TM    RSTATUS,X'20'       YES, ELEMENT HAS RATE OVERRIDE?              
         BZ    GETSB50                  NO, REGULAR DEFAULT COST                
***************                                                                 
* RATE OVERRIDE POL SPOT ELEMENT                                                
***************                                                                 
         XR    R1,R1               THIS IS THE COST IN QUESTION                 
         ICM   R1,7,RPCOST                                                      
         L     R3,AIOAREA2                                                      
         TM    BDCIND2-BUYKEY(R3),X'20'       CANADIAN AGENCY BUY?              
         BNZ   GETSB46A                       YES                               
         TM    BDCIND2-BUYKEY(R3),X'10'       COST IN US DOLLARS                
         BZ    GETSB46A                                                         
         XR    R0,R0                                                            
         M     R0,=F'100'                                                       
GETSB46A LR    R0,R1               R0 = ACTUAL COST IN QUESTION                 
*                                                                               
         TM    MISCFLG1,MF1COSOV   UPTO COST OVERRIDES ALREADY?                 
         BNZ   GETSB47             YES                                          
*                                                                               
         LA    RE,COSTABLE                                                      
         CLI   COSTNUMB,0          DO WE HAVE ANY OVERRIDES YET?                
         BE    GETSB46C            NONE, OUR FIRST!                             
         XR    R1,R1                                                            
         IC    R1,COSTNUMB                                                      
GETSB46B CLM   R0,15,0(RE)         OVERRIDE COST IN TABLE ALRDY?                
         BE    GETSB70             YES, NEXT ELEM                               
         LA    RE,L'NBRSCST1(RE)                                                
         BCT   R1,GETSB46B                                                      
*                                                                               
GETSB46C XR    R1,R1               RE = A(NEXT OVERRIDE COST)                   
         IC    R1,COSTNUMB         R1 = NTH COST OVERRIDE                       
         LA    R1,1(R1)                                                         
         CHI   R1,MAXCOSTS                                                      
         BH    GETS998             ERROR!  MORE THAN 8 OVERRIDES                
***                                ...USED TO DIE, BUT ERROR NOW                
***      DC    H'0'                DIE IF MORE THAN 8 COST OVERRIDES            
*                                                                               
         STC   R1,COSTNUMB                                                      
         STCM  R0,15,0(RE)         STORE THE OVERRIDE IN THE TABLE              
         B     GETSB70                                                          
*********                                                                       
* PROCESSING COST OVERRIDES ALREADY                                             
*********                                                                       
GETSB47  CLM   R0,15,CURRCOST      MATCHES OUR OVERRIDE COST?                   
         BNE   GETSB70                                                          
         LA    R0,1                                                             
         TM    RSTATUS,X'80'       MINUS SPOT?                                  
         BZ    *+6                                                              
         LNR   R0,R0               YES, NEGATE TO SUBTRACT                      
         B     GETSB69             YES, BUMP THE COUNTS                         
***************                                                                 
* BUY COST POL SPOT ELEMENT                                                     
***************                                                                 
GETSB50  TM    MISCFLG1,MF1COSOV   UPTO COST OVERRIDES ALREADY?                 
         BZ    GETSB69             NO                                           
         TM    MISCFLG1,MF1ECSTS   DO WE HAVE EFFECTIVE COSTS?                  
         BZ    GETSB70                                                          
         CLI   CURRCOVR,3          YES, FINISHED WITH EFFECTIVE COSTS?          
         BNL   GETSB70                  YES                                     
         CLC   RDATE,APHALF        COMPARE AGAINST THE EFFECTIVE DATE           
         BL    GETSB70             BEFORE EFFECTIVE DATE                        
         B     GETSB69                                                          
*                                                                               
GETSB69  XR    RE,RE               BUMP UP THE SPOT COUNT                       
         IC    RE,0(RF)                                                         
         AR    RE,R0                                                            
         CHI   RE,256                                                           
         BL    *+6                                                              
         DC    H'0'                DIE, MORE THAN 255 SPOTS IN WEEK             
         CHI   RE,0                                                             
         BNL   *+6                                                              
         XR    RE,RE               CAN'T HANDLE NEGATIVE SPOTS YET              
         STC   RE,0(RF)                                                         
*                                                                               
GETSB70  XR    R0,R0               NEXT SPOT ELEMENT                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     GETSB30                                                          
*                                                                               
GETS16   MVC   TBUYOTOM,OTOBITS    SAVE THE OTO BIT MASKS                       
         XC    LRATING,LRATING     GET THE RATING                               
         XC    APFULL,APFULL                                                    
         L     R2,AIOAREA2                                                      
         USING BUYKEY,R2                                                        
         LA    R1,BDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
GETS17   CLI   0(R1),0                                                          
         BE    GETS20                                                           
         CLI   0(R1),X'02'         ORIGINAL DEMO ELEMENT                        
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GETS17                                                           
*                                                                               
         XC    APWORK,APWORK                                                    
         LA    RE,ESTDEMS+3                                                     
         LA    R0,ESTDEMS+L'ESTDEMS                                             
         LA    RF,APWORK+3                                                      
         MVC   APWORK(3),ESTDEMS                                                
         OC    INORTG,INORTG                                                    
         BZ    GETS17A                                                          
         MVC   APWORK(3),INORTG                                                 
         LA    RE,ESTDEMS                                                       
*                                                                               
GETS17A  OC    INORTG,INORTG                                                    
         BZ    *+14                                                             
         CLC   APWORK(3),0(RE)     REACHED DEMO THE USER PUT IN OPTION?         
         BE    GETS17B             YES                                          
         MVC   0(3,RF),0(RE)       NO                                           
         LA    RF,3(RF)            BUMP TO NEXT ENTRY IN OUR LIST               
GETS17B  LA    RE,3(RE)            BUMP TO NEXT ENTRY IN EST LIST               
         OC    0(3,RE),0(RE)                                                    
         BZ    GETS17X                                                          
         CR    RE,R0                                                            
         BL    GETS17A                                                          
*                                                                               
GETS17X  DS    0H                                                               
         LA    R9,TBUYDEMS                                                      
         LA    R2,APWORK           OUR DEMO LIST                                
         LR    R0,R1               SAVE A(DEMO ELEMENT)                         
*                                                                               
GETS18   LR    R1,R0               R1 = A(DEMO ELEMENT)                         
***  2 DECIMAL  ***                                                             
         ST    R1,APPARM                                                        
         GOTO1 AADJPREC,APPARM,,   SEE IF WE NEED TO CONVERT                    
***  2 DECIMAL  ***                                                             
         LR    R1,R0               RESET R1                                     
         LA    RE,8                RE = L'DEMO                                  
         XR    RF,RF               RF = A(LAST BYTE IN DEMO ELEMENT)            
         IC    RF,1(R1)                                                         
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    R1,NDEMNO-NDELEM(R1)  R1 = A(1ST DEMO ENTRY)                     
         MVC   0(3,R9),0(R2)       COPY DEMO CATEGORY                           
*                                                                               
GETS19   CLC   0(3,R1),0(R2)                                                    
         BNE   GETS19E                                                          
         MVC   3(5,R9),3(R1)       COPY THE RATING                              
***   IT DOESN'T LOOK LIKE LADEMEL, LASPWEL, OR LREGSV IS USED PRIOR            
         ST    R1,LADEMEL          SAVE OFF R1 TEMPORARILY                      
         ST    RE,LASPWEL          SAVE OFF RE                                  
         ST    RF,LREGSV           SAVE OFF RF                                  
         GOTO1 AADJPREC,APPARM,(R9)   CONVERT TBUYDEMS TO 2 DECIMALS            
         L     R1,LADEMEL                                                       
         L     RE,LASPWEL                                                       
         L     RF,LREGSV                                                        
***   IT DOESN'T LOOK LIKE LADEMEL, LASPWEL, OR LREGSV IS USED PRIOR            
GETS19E  BXLE  R1,RE,GETS19                                                     
*                                                                               
         LA    R9,L'TBUYDEMO(R9)                                                
         LA    R2,3(R2)                                                         
         LA    RE,TBUYDEMS+L'TBUYDEMS                                           
         CR    R9,RE               PAST OUR 8 DEMOS WE CAN STORE?               
         BNL   GETS19X             YES                                          
         OC    0(3,R2),0(R2)                                                    
         BNZ   GETS18                                                           
*                                                                               
GETS19X  LA    R1,TBUYDEMS                                                      
***  2 DECIMAL                                                                  
*        LR    R0,RE               SAVE OFF RE                                  
*        LR    R2,RF               SAVE OFF RF                                  
*        ST    R1,APPARM           SAVE IT OFF SO WE CAN ADJUST                 
*        GOTO1 AADJPREC,APPARM                                                  
*        L     R1,APPARM                                                        
*        LR    RE,R0               RESTORE RE                                   
*        LR    RF,R2               RESTORE RF                                   
***  2 DECIMAL                                                                  
         ST    R1,APFULL                                                        
         MVC   LRATING,5(R1)       LRATING IS DEFINED AS  XL3                   
*                                                                               
GETS20   DS    0H                                                               
         L     R2,AIOAREA2                                                      
         USING BUYKEY,R2                                                        
****     TM    LFLAG,LPKGSLV       TEST PACKAGE SLAVE                           
****     BO    GETS29              YES-SKIP TO GET POINTS/DOLLARS               
****     TM    LFLAG2,LCAMP2       TEST COMPANION CAMPAIGN                      
****     BO    GETS29              YES-SKIP TO GET POINTS/DOLLARS               
*                                                                               
         TM    MISCFLG1,MF1COSOV   UPTO COST OVERRIDES?                         
         BZ    GETS21                                                           
         ICM   RF,15,CURRCOST      YES, USE THE CURRENT COST OVERRIDE           
         B     GETS21A                                                          
*                                                                               
GETS21   LA    R9,BDCOST           R9=A(COST)                                   
         XR    RF,RF                                                            
         ICM   RF,7,0(R9)                                                       
         TM    BDCIND2,X'20'       CANADIAN AGENCY BUY?                         
         BNZ   GETS21A             YES                                          
         TM    BDCIND2,X'10'       COST IN US DOLLARS                           
         BZ    GETS21A                                                          
         SR    RE,RE                                                            
         M     RE,=F'100'                                                       
GETS21A  STCM  RF,15,TBUYCOST                                                   
         TM    BDCIND,X'01'        MINUS SPOT (NEG AMT)?                        
         BZ    GETS22                                                           
         LNR   RF,RF               YES                                          
         STCM  RF,15,TBUYCOST                                                   
*                                                                               
GETS22   DS    0H                                                               
***************                                                                 
* HOW DO WE HANDLE 0 SPOTS (WEEKS THAT HAVE BEEN PRE-EMPTED) AND WE             
*  WANT TO SEE THE BUYLINE?  COULD USE X'FF' FOR SPOTS???                       
***************                                                                 
GETS25   DS    0H                                                               
         TM    MISCFLG1,MF1COSOV                                                
         BNZ   GETS25A                                                          
*                                                                               
         MVC   APWORK(L'IOKEY),IOKEY   SAVE COPY OF BUY KEY WE'RE ON            
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING NBRKEY,R2           LOOK FOR NWS BUY REVISION RECORD             
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ      BINARY CAMPAIGN/MARKET SEQ #                 
         MVC   NBRKSTA,TBUYSTA                                                  
         MVC   NBRKKBUY,TBUYBUYS                                                
*                                                                               
         GOTO1 AIO,DIRHI+IO3                                                    
         BNE   GETS26                                                           
*                                                                               
         CLC   NBRKEY,IOKEYSAV      FIND OUR BUY REVISION RECORD?               
         BNE   GETS26                                                           
*                                                                               
         GOTO1 AIO,FILGET3                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETS25A  L     R2,AIOAREA3         R3=A(RECORD)                                 
         GOTO1 =A(NBR2TSAR),RR=APRELO                                           
         BE    GETS26              VALID?                                       
         MVC   IOKEY,APWORK        NO, RE-ESTABLISH OUR BUY KEY                 
         GOTO1 AIO,DIRRD+IO2       THIS HAPPENS WHEN NO MORE EFFECTIVE          
         B     GETS29                                                           
*                                                                               
GETS26   GOTO1 =A(SETTSRKY),RR=APRELO                                           
*                                                                               
         TM    MISCFLG1,MF1COSOV                                                
         BNZ   GETS26A                                                          
         MVC   IOKEY,APWORK        RE-ESTABLISH OUR BUY RECORD                  
         GOTO1 AIO,DIRRD+IO2                                                    
         GOTO1 (RF),FILGET2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETS26A  L     R2,AIOAREA2                                                      
         USING BUYKEY,R2                                                        
*                                                                               
****     OC    TBUYSKED,TBUYSKED   ANY SPOTS THAT ARE IN OUR FLIGHT?            
         TM    MISCFLG1,MF1DSNBR   DO WE HAVE AN NBR RECORD?                    
         BNZ   GETS26E              - YUP, WE NEED TO ADD TO TSAR!              
**                                                                              
         XC    APWORK,APWORK                                                    
         LA    R1,APWORK                                                        
         USING SPBUYVLD,R1                                                      
         MVC   SPBYAREC,AIOAREA2   A(BUY RECORD)                                
         MVC   SPBYAFAC,ACOM       A(COMFACS)                                   
         GOTOR VSPBYVAL,APPARM,APWORK                                           
         LA    R1,APWORK                                                        
         CLI   SPBYERR,0           DO WE HAVE AN ERROR?                         
         BE    GETS26C              - NOPE, NO ERRORS                           
*&&DO                                                                           
         TM    MISCFLG1,MF1ERROR   DO WE HAVE ONE ALREADY?                      
         BNZ   GETS26C                                                          
         OI    MISCFLG1,MF1ERROR   WE HAVE ONE NOW!                             
         MVC   FVMSGNO,=AL2(FVFSET)   WE HAVE OUR OWN MESSAGE                   
         XC    APDUB,APDUB                                                      
         MVC   APDUB+2(3),BUYKSTAC                                              
         LA    RF,BWSMSGH                                                       
         XC    8(L'BWSMSG,RF),8(RF)                                             
         MVC   8(L'BADREC,RF),BADREC                                            
         GOTOR VMSUNPK,APPARM,(X'80',APDUB),APWORK,APWORK+4                     
         LA    RF,BWSMSGH                                                       
         MVC   8(5,RF),APWORK+4                                                 
         CLI   BUYKBUY,X'FF'                                                    
         BNL   GETS26C                                                          
         LLC   R1,BUYKBUY                                                       
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  8+14(3,RF),APDUB+6(2)                                            
         OI    6(RF),X'80'                                                      
*&&                                                                             
         OI    TBUYFLG1,X'08'      BUY HAS ERRORS                               
         B     GETS26C                                                          
BADREC   DC    C'      BUYLINE     HAS ERRORS!'                                 
         DROP  R1                                                               
**                                                                              
GETS26C  DS    0H                                                               
         TM    LFLAG2,LF2ATSRR     NEED TO ADD TSAR REC FOR THIS BUY?           
         BZ    GETSRDSQ            NO, NEXT RECORD                              
GETS26E  MVI   TSARACT,TSAADD      YES, ADD TSAR RECORD                         
         GOTO1 ATSAR,TREC                                                       
         BE    GETS27                                                           
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         TM    TSERRS,TSEEOF                                                    
         DROP  R1                                                               
         BZ    GETS999                                                          
         B     ETMR                TOO MANY RECORDS TO LIST                     
*                                                                               
GETS27   LA    R8,1(R8)            INCREMENT RECORD COUNT                       
*                                                                               
GETS29   TM    LFLAG,LPKGMAS       TEST PACKAGE MASTER                          
         BO    *+8                                                              
         BRAS  RE,ACPTSDOL         NO-ACCUMULATE POINTS/DOLLARS                 
*                                                                               
         CLI   COSTNUMB,0          ANY COST OVERRIDES?                          
         BNE   *+12                YES                                          
         TM    MISCFLG1,MF1ECSTS   ANY EFFECTIVE COSTS?                         
         BZ    GETSRDSQ            NONE, GET THE NEXT BUY                       
*                                                                               
         TM    MISCFLG1,MF1COSOV                                                
         BNZ   *+8                                                              
         MVI   CURRCOVR,0          WE HAVEN'T DONE ANY COST OVERRIDES           
*                                                                               
GETS29A  XR    RE,RE                                                            
         IC    RE,CURRCOVR                                                      
         LR    RF,RE                                                            
         LA    RE,1(RE)                                                         
         STC   RE,CURRCOVR                                                      
*                                                                               
         TM    MISCFLG1,MF1ECSTS                                                
         BZ    GETS29B                                                          
         CLI   CURRCOVR,3                                                       
         BL    GETS29C                                                          
         AHI   RF,-2                                                            
GETS29B  CLM   RF,1,COSTNUMB                                                    
         BNL   GETSRDSQ                                                         
         MHI   RF,L'CURRCOST                                                    
         LA    RF,COSTABLE(RF)                                                  
         MVC   CURRCOST,0(RF)                                                   
GETS29C  OI    MISCFLG1,MF1COSOV   NOW WE'RE UPTO COST OVERRIDES                
         B     GETS10                                                           
***********************************                                             
* NOW FOR THE MANUALLY ADDED BUY REVISIONS RECORDS                              
***********************************                                             
GETS30   XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING NBRKEY,R2                                                        
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ                                                   
*                                                                               
         LA    R1,NBRKSTA-NBRKEY                                                
         OC    QSTA,QSTA                                                        
         BNZ   GETS33                                                           
         OC    QCABLE,QCABLE                                                    
         BZ    GETS34                                                           
         XC    APDUB,APDUB                                                      
         MVC   APDUB(L'QCABLE),QCABLE                                           
         MVI   APDUB+L'QCABLE-1,C'/'                                            
         GOTO1 VMSPACK,APPARM,=C'0000',APDUB,APWORK                             
         MVC   NBRKSTA,APWORK+2                                                 
         B     *+10                                                             
GETS33   MVC   NBRKSTA,BSTA                                                     
         LA    R1,NBRKKBUY-NBRKEY                                               
GETS34   BCTR  R1,0                                                             
         STC   R1,LKEYCOMP                                                      
*                                                                               
GSELRDHI LA    R1,DIRHI+IO2                                                     
         B     *+8                                                              
GSELRDSQ LA    R1,DIRSQ+IO2                                                     
*                                                                               
GSELIO   LA    R2,IOKEY                                                         
         GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)   TEST REACHED END                           
         B     GETS70                                                           
*                                                                               
         XR    RE,RE                                                            
         IC    RE,LKEYCOMP                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BE    GETS34Z                                                          
         OC    QCABLE,QCABLE                                                    
         BZ    GETS70                                                           
         CLC   IOKEY(NBRKSTA-NBRKEY+2),IOKEYSAV                                 
         BNE   GETS70              MUST MATCH UP TO LAST BYTE OF STA            
         MVC   APWORK(1),IOKEY+NBRKSTA-NBRKEY+2                                 
         MVC   APWORK+1(1),IOKEYSAV+NBRKSTA-NBRKEY+2                            
         NI    APWORK,X'80'                                                     
         NI    APWORK+1,X'80'                                                   
         CLC   APWORK(1),APWORK+1                                               
         BNE   GETS70                                                           
*                                                                               
GETS34Z  OC    NBRKKBUY,NBRKKBUY   LOOKING FOR MANUALLY ADDED REVISIONS         
         BNZ   GSELRDSQ                                                         
*                                                                               
         GOTO1 AIO,FILGET2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA2                                                      
         MVI   CURRCOVR,0          USE THIS AS EFFECTIVE COST COUNTER           
         LA    R2,NBRFSTEL                                                      
         USING NBRSELD,R2                                                       
*                                                                               
         CLI   BSLN,0              ANY SPOT LENGTH FILTER?                      
         BE    *+14                                                             
         CLC   NBRSSLN,BSLN                                                     
         BNE   GSELRDSQ                                                         
*                                                                               
         CLI   BDPT,0              ANY DAYPART FILTER?                          
         BE    GETS40                                                           
         CLC   NBRSDYPT,BDPT       MATCH ON DAYPART?                            
         BE    GETS40                                                           
         CLI   INODPT,C'M'         NO-TEST DPT=MAS AND DAYPART IS               
         BNE   GSELRDSQ               A MASTER                                  
         CLI   DPTTYPE,C'M'                                                     
         BNE   GSELRDSQ                                                         
         LA    R1,DPTSUBS          YES-ACCEPT ANY OF THE SUB-DPTS               
         LA    R0,L'DPTSUBS                                                     
*                                                                               
GETS35   CLI   0(R1),0                                                          
         BE    GSELRDSQ                                                         
         CLC   NBRSDYPT,0(R1)                                                   
         BNE   *+12                                                             
         OI    LFLAG2,LDPTMAS      INDICATE WE HAVE A DPT OTHER                 
         B     GETS40              THAN THE MASTER                              
         LA    R1,1(R1)                                                         
         BCT   R0,GETS35                                                        
         B     GETSRDSQ                                                         
*                                                                               
GETS40   CLI   BDPT,0              ANY DAYPART FILTER?                          
         BNE   GETS43                                                           
         LA    R1,COMDPLST         NONE, SEE IF WE NEED TO ADD TO LIST          
         LA    R0,L'COMDPLST                                                    
GETS41   CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   0(1,R1),NBRSDYPT                                                 
         B     GETS43                                                           
         CLC   NBRSDYPT,0(R1)                                                   
         BE    GETS43                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,GETS41                                                        
         DC    H'0'                                                             
*                                                                               
GETS43   CLI   BSLN,0              ANY SPOT LENGTH FILTER?                      
         BNE   GETS45                                                           
         LA    R1,COMSLLST         NONE, SEE IF WE NEED TO ADD TO LIST          
         LA    R0,L'COMSLLST                                                    
GETS44   CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   0(1,R1),NBRSSLN                                                  
         B     GETS45                                                           
         CLC   NBRSSLN,0(R1)                                                    
         BE    GETS45                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,GETS44                                                        
         DC    H'0'                                                             
*                                                                               
GETS45   LA    R4,TSARREC                                                       
         USING TRECD,R4                                                         
         L     R2,AIOAREA2                                                      
         USING NBRKEY,R2                                                        
         XC    TSARREC(TBUYRECL),TSARREC                                        
         MVC   TRECL,=Y(TBUYRECL)                                               
         MVI   TRECTYP,TRECBUY                                                  
         MVC   TBUYSTA,NBRKSTA                                                  
         MVC   TBUYSEQ,NBRKNBSQ                                                 
*                                  PUT NBR INFO INTO TSAR REC                   
         GOTO1 =A(NBR2TSAR),RR=APRELO                                           
*                                  SET UP THE TSAR KEY FOR THE RECORD           
         GOTO1 =A(SETTSRKY),RR=APRELO                                           
*                                                                               
         MVI   TSARACT,TSAADD      YES, ADD TSAR RECORD                         
         GOTO1 ATSAR,TREC                                                       
         BNE   GETS999                                                          
         LA    R8,1(R8)            INCREMENT RECORD COUNT                       
*                                                                               
         TM    LFLAG,LPKGMAS       TEST PACKAGE MASTER                          
         BO    *+8                                                              
         BRAS  RE,ACPTSDOL         NO-ACCUMULATE POINTS/DOLLARS                 
*                                                                               
         LA    R2,NBRFSTEL                                                      
         USING NBRSELD,R2                                                       
         CLI   CURRCOVR,2          PROCESSED EFFECTIVE COST 2?                  
         BE    GETS60              YES                                          
*                                                                               
         OC    NBRSEDT2,NBRSEDT2   ANY DATES FOR EFFECTIVE COST 1?              
         BZ    GETS60                                                           
         CLI   CURRCOVR,1          YES, DID WE DO EFFECTIVE COST 1 YET?         
         BNL   *+12                     YES, CHECK FOR EFF COST 2               
         MVI   CURRCOVR,1                                                       
         B     GETS40              LOOP BACK FOR EFFECTIVE COST 1               
*                                                                               
         OC    NBRSEDT3,NBRSEDT3   ANY DATES FOR EFFECTIVE COST 2?              
         BZ    GETS60                                                           
         MVI   CURRCOVR,2          YES                                          
         B     GETS40              LOOP BACK FOR EFFECTIVE COST 2               
*                                                                               
GETS60   B     GSELRDSQ            GET NEXT RECORD                              
*~                                                                              
GETS70   LTR   R8,R8               TEST ANY RECORDS                             
         BZ    GETS900             NO-TELL USER                                 
*&&DO                                                                           
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
         B     GETS7B              GO BACK AND READ RECORDS                     
*                                                                               
GETS30D  GOTO1 =A(GOALS),RR=APRELO    GET GOALS FOR COMPANION                   
         BNE   GETS800                                                          
         XC    BCLT(G1WPROF-BCLT),BCLT   TO GET PROFILES... RESTORED            
         GOTO1 AGETCAM,SVBCAM      GET BACK ORIGINAL CAMPAIGN                   
         BE    *+6                 AND CONTINUE                                 
         DC    H'0'                                                             
         MVC   BCLT(BVALSX-BCLT),SVBVALS                                        
         MVC   QCLT(QVALSX-QCLT),SVQVALS                                        
         NI    LFLAG2,255-LCAMP2                                                
*&&                                                                             
GETS71   TM    TWAINDS,TWAICCSW    SAVE N'RECORDS                               
         BZ    *+8                                                              
         AH    R8,SVTUMAX                                                       
         STH   R8,LTUMAX                                                        
*                                                                               
         CLI   APACTN,ACTSKD       FOR SCHEDULE ACTIONS --                      
         BNE   *+16                                                             
         LA    R8,WRKL8H+GLDSPL    FORMAT GOAL DOLLARS/POINTS                   
         LA    R9,WRKL8H+CMTDSPL   DISPLAY COMMENT LINE IF POINTS ARE           
         B     GETS72              ROUNDED                                      
         CLI   APACTN,ACTSSK                                                    
         BNE   GETS76                                                           
         LA    R8,SSKGOLH                                                       
         LA    R9,SSKCMTH                                                       
*                                                                               
GETS72   GOTO1 =A(GOALS),RR=APRELO   GET THE GOALS                              
         BNE   GETS800                                                          
         GOTO1 AFMTGOAL,APPARM,(R8),(R9),SVGVA   AND FORMAT                     
         LA    R8,WRKL8H+ACDSPL    FORMAT ACTUAL POINTS AND DOLLARS             
         CLI   APACTN,ACTSSK                                                    
         BNE   *+8                                                              
         LA    R8,SSKACTH                                                       
         GOTO1 AFMACPTS,APPARM,(R8),SVGVA                                       
         GOTO1 AFMACDOL,APPARM,(R8),SVGVA                                       
         B     GETS76                                                           
*                                                                               
GETS73   LH    RF,LTUNUM           NOT FIRST LINE - GET NEXT RECORD             
         LA    RF,1(RF)                                                         
         STH   RF,LTUNUM                                                        
         CLI   LLINE,NSKDLINS      TEST LINE NUMBER REACHED LIMIT               
         BL    GETS74                                                           
         MVI   LLINE,1             YES - RESET LINE NUMBER                      
         XR    RE,RE                                                            
         IC    RE,LPAGDSP                                                       
         LA    RF,1(RE)                  AUGMENT PAGE LETTER                    
         LA    RE,ALPHATAB(RF)                                                  
         CLI   0(RE),FF                                                         
         BE    *+14                                                             
         STC   RF,LPAGDSP                                                       
         MVC   LPAGE,0(RE)                                                      
         B     GETS76                                                           
*                                                                               
GETS74   ZIC   RE,LLINE            NOT FIRST LINE -                             
         LA    RE,1(RE)            AUGMENT LINE NUMBER                          
         STC   RE,LLINE                                                         
*                                                                               
GETS76   CLC   LTUNUM,LTUMAX       TEST REACHED END OF RECORDS                  
         BH    GETS900                                                          
         OC    LTUMAX,LTUMAX       NO RECORDS ANYWAY?                           
         BZ    GETS900                                                          
*                                                                               
         LA    R4,TSARREC          NO-GET TSAR RECORD                           
         USING TRECD,R4                                                         
         MVC   TSARNUM,LTUNUM                                                   
         MVI   TSARACT,TSAGET                                                   
         GOTO1 ATSAR,TREC                                                       
         BNE   GETSX                                                            
         CLI   TRECTYP,TRECBUY     AGENCY BUY RECORDS?                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    TBUYBUYS,TBUYBUYS   MANUAL BUY REVISION RECORD?                  
         BNZ   GETS76A             NO, BUY RECORD                               
*                                                                               
         LA    R2,APRECKEY                                                      
         XC    APRECKEY,APRECKEY                                                
         USING NBRKEY,R2                                                        
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ                                                   
         MVC   NBRKSTA,TBUYSTA                                                  
         MVC   NBRKNBSQ,TBUYSEQ                                                 
         B     GETS76B                                                          
*                                                                               
GETS76A  LA    R2,APRECKEY         SET VALUES FOR ROOT                          
         XC    APRECKEY,APRECKEY                                                
         USING BUYKEY,R2                                                        
         MVC   BUYKAM,BAGYMD                                                    
         MVC   BUYKCLT,BCLT                                                     
         MVC   BUYKPRD,BPRD                                                     
         MVC   BUYMSTA(L'BMKT),BMKT                                             
         MVC   BUYMSTA+L'BMKT(L'BSTA),TBUYSTA   STATION   FROM TSAR REC         
         MVC   BUYKEST,CMPESTN                                                  
         MVC   BUYKBUY,TBUYBUYS                                                 
         DROP  R2                                                               
GETS76B  MVC   IOKEY(13),APRECKEY                                               
         LA    R1,DIRRD+IO2                                                     
         GOTO1 AIO                                                              
         BNE   EPF4                                                             
         GOTO1 AIO,FILGET2                                                      
         BE    *+14                                                             
         CLI   IOERR,X'02'         RECORD IS DELETED THOUGH                     
         BE    GETSRDSQ                                                         
         DC    H'0'                                                             
*                                                                               
         L     R3,AIOAREA2         SET MORE VALUES FOR ROOT                     
         XC    APWORK,APWORK                                                    
         MVC   APWORK(L'BMKT),BMKT                                              
         MVC   APWORK+L'BMKT(L'TBUYSTA),TBUYSTA                                 
         GOTO1 VMSUNPK,APPARM,(X'80',APWORK),APWORK+5,APWORK+9                  
         MVC   APRECDA,APWORK+9         APRECDA=STATION                         
         CLI   APRECDA,C'0'             CABLE SYSTEM?                           
         BL    *+10                                                             
         MVC   APRECKEY+40(3),APWORK+14   STORE NTWK IN KEY+40                  
*                                                                               
         MVC   APRECID+1(1),TBUYDYPT   APRECID+1(2)=DAYPART/LENGTH              
         MVC   APRECID+2(1),TBUYSLN                                             
         MVI   APRECKEY+19,0                                                    
         OC    TBUYBUYS,TBUYBUYS                                                
         BZ    *+14                                                             
         MVC   APRECKEY+19(1),TBUYKCOV                                          
         B     *+10                                                             
         MVC   APRECKEY+19(1),TBUYKIND                                          
*                                                                               
GETS77   CLI   APACTN,ACTSKD       TEST FOR SCHEDULE OR SUPER-SKED              
         BE    *+12                                                             
         CLI   APACTN,ACTSSK                                                    
         BNE   GETS800             NO-EXIT                                      
*                                                                               
******   MVC   APRECKEY+38(2),TBUYOTOM  COPY THE OTO BITS                       
         ZIC   R1,INOSTDTD                                                      
         ICM   RE,15,TBUYOTOM                                                   
         XR    RF,RF                                                            
         ICM   RF,14,TBUYOTOM+4                                                 
         SLDL  RE,0(R1)                                                         
         STCM  RE,12,APRECKEY+38                                                
*                                  YES-BUILD SCHEDULE LINE IN APRECKEY          
         XR    RE,RE                                                            
         ICM   RE,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    RE,CMPDSTDT                                                      
         LA    RE,TBUYSKED(RE)                                                  
         MVC   APRECKEY+20(14),0(RE)                                            
*                                                                               
GETS440  OC    TBUYBUYS,TBUYBUYS   MANUALLY ADDED BUYS?                         
         BZ    GETS445             YES                                          
         TM    LFLAG2,LF2NPOCM     NOT PART OF THIS CAMPAIGN?                   
         BNZ   GETS440Y            NOPE, DON'T LOOK FOR REVISION                
*                                                                               
         MVC   APWORK(L'IOKEY),IOKEY   SAVE COPY OF BUY KEY WE'RE ON            
         XC    IOKEY,IOKEY                                                      
         LA    R3,IOKEY                                                         
         USING NBRKEY,R3           LOOK FOR NWS BUY REVISION RECORD             
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ      BINARY CAMPAIGN/MARKET SEQ #                 
         MVC   NBRKSTA,TBUYSTA                                                  
         MVC   NBRKKBUY,TBUYBUYS                                                
*                                                                               
         GOTO1 AIO,DIRHI+IO3                                                    
         BNE   GETS440Y                                                         
*                                                                               
         CLC   NBRKEY,IOKEYSAV      FIND OUR BUY REVISION RECORD?               
         BNE   GETS440Y                                                         
*                                                                               
         GOTO1 AIO,FILGET3                                                      
         BE    GETS440Z                                                         
*                                                                               
GETS440Y GOTO1 ASTUPNBR,APPARM,AIOAREA2,AIOAREA3,DBLOCK                         
GETS440Z L     R0,AIOAREA2                                                      
         LA    R1,2000                                                          
         L     RE,AIOAREA3                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R3,AIOAREA2                                                      
*                                                                               
         USING NBRKEY,R3                                                        
GETS445  LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
         TM    INOIND,INOINOG      NO-TEST NOGOALS OPTION                       
         BO    GETS500                                                          
         TM    CLTIND,CLTIGOL      NO-TEST GOALS REQUIRED FOR BUY               
         BZ    GETS500                                                          
         CLC   TWADPT,NBRSDYPT     YES-MARK AS INVALID THOSE WEEKS              
         BNE   *+14                    WITHOUT GOALS                            
         CLC   TWASLN,NBRSSLN                                                   
         BE    GETS460                                                          
         MVC   APHALF,BDPT                                                      
         MVC   BDPT,NBRSDYPT                                                    
         CLI   NBRSSBDP,0                                                       
         BE    *+10                                                             
         MVC   BDPT,NBRSSBDP                                                    
         XC    DPTSUBS,DPTSUBS                                                  
         MVC   BSLN,NBRSSLN                                                     
         GOTO1 AGETGOAL                                                         
         MVC   BDPT(2),APHALF                                                   
         CLI   BDPT,0                                                           
         BE    GETS450                                                          
         GOTO1 AGETDPT,APHALF                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BDPT,TWADPT         IS IT THE SAME DAYPART?                      
         BNE   GETS460             NO, SUBDAYPART, LEAVE MASTER GOALS           
*                                                                               
GETS450  L     RE,AIOAREA3                                                      
         MVC   SVGLPTS(53*4),220(RE)                                            
*                                                                               
GETS460  ZIC   R0,CMPNWKS                                                       
         CHI   R0,NMAXWKS                                                       
         BNH   *+8                                                              
         LA    R0,NMAXWKS                                                       
*                                                                               
         LA    RE,SVGLPTS                                                       
****                                                                            
         XR    RF,RF               ADJUST RE WITH DISPLACEMENT FROM             
         ICM   RF,1,INOSTDTD         THE CAMPAIGN START DATE                    
         BNZ   *+8                                                              
         IC    RF,CMPDSTDT                                                      
         MHI   RF,4                                                             
         AR    RE,RF                                                            
****                                                                            
         LA    RF,APRECKEY+20                                                   
*                                                                               
GETS480  OC    0(4,RE),0(RE)                                                    
         BNZ   *+8                                                              
         OI    0(RF),X'40'         MARK WEEKS THAT DON'T HAVE GOALS             
         LA    RE,4(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,GETS480                                                       
*                                                                               
GETS500  OC    NBRSEDT2,NBRSEDT2   TEST EFFECTIVE DATES                         
         BZ    GETS620             NONE, ACCEPT ALL WEEKS IN CAMP               
         GOTO1 VDATCON,APPARM,(3,NBRSEDT2),(2,APDUB)                            
         OC    NBRSEDT3,NBRSEDT3                                                
         BZ    GETS520                                                          
         GOTO1 VDATCON,APPARM,(3,NBRSEDT3),(2,APDUB+2)                          
*                                                                               
GETS520  LR    RE,R5               MARK WEEKS THAT ARE OUT OF DATE              
         AHI   RE,CMPDATSP-TWAD                                                 
*                                                                               
         XR    RF,RF               ADJUST RE WITH DISPLACEMENT FROM             
         ICM   RF,1,INOSTDTD         THE CAMPAIGN START DATE                    
         BNZ   *+8                                                              
         IC    RF,CMPDSTDT                                                      
         MHI   RF,4                                                             
         AR    RE,RF                                                            
*                                                                               
GETS530  LA    RF,APRECKEY+20      BOUNDS                                       
         LA    R0,NMAXWKS                                                       
*                                                                               
GETS540  TM    TBUYKIND,TBYUICS2+TBYUICS3   ON AN EFFECTIVE COST LINE?          
         BNZ   GETS560                      YES, WE HAVE                        
         CLI   TBUYKCOV,0          NOT EFFECTIVE, BUT A OVERRIDE                
         BNE   GETS610             YES, VALID WEEK                              
         CLC   2(2,RE),APDUB          NO, LESS THAN 1ST EFF DATE?               
         BL    GETS610                    YES, THIS WEEK IS OKAY                
         B     GETS600                    NO,  THIS WEEK IS INVALID             
*                                                                               
GETS560  TM    TBUYKIND,TBYUICS2   ON 1ST EFF DATE?                             
         BZ    GETS580             NO,  MUST BE 2ND EFF DATE                    
         CLC   2(2,RE),APDUB       YES, MAKE SURE WITHIN 1ST EFF COST           
         BL    GETS600                                                          
         OC    NBRSEDT3,NBRSEDT3        ANY 2ND EFFECTIVE DATE?                 
         BZ    GETS610                  NONE, OKAY THEN                         
         CLC   2(2,RE),APDUB+2          YES, LESS THAN 2ND EFF DATE?            
         BL    GETS610                       YES, THIS WEEK IS OKAY             
         B     GETS600                       NO,  THIS WEEK IS INVALID          
*                                                                               
GETS580  CLC   2(2,RE),APDUB+2                                                  
         BNL   GETS610                                                          
*                                                                               
GETS600  OI    0(RF),X'80'         X'80' = THIS WEEK NOT ALLOWED                
*                                                                               
GETS610  LA    RE,4(RE)            NEXT WEEK                                    
         LA    RF,1(RF)                                                         
         BCT   R0,GETS540                                                       
*                                                                               
GETS620  TM    NBRSINDS,NBRSIPRG   PROGRAMMING OVERRIDE?                        
         BZ    GETS625              - NOPE                                      
         OI    TBUYFLG1,TBF1PRG     - YUP                                       
*                                                                               
GETS625  TM    NBRSINDS,NBRSIORB   TEST ORBIT                                   
         BO    GETS800             YES - SKIP DAY CHECK                         
*                                                                               
         TM    CMPOPTS,CAMODLY     TEST DAILY SKED                              
         BZ    GETS650                                                          
         LR    R8,R5               YES-MARK INVALID DAYS                        
         AHI   R8,CMPDATSD-TWAD                                                 
*                                                                               
         XR    RF,RF               ADJUST R8 WITH DISPLACEMENT FROM             
         ICM   RF,1,INOSTDTD         THE CAMPAIGN START DATE                    
         BNZ   *+8                                                              
         IC    RF,CMPDSTDT                                                      
         MHI   RF,6                                                             
         AR    R8,RF                                                            
*                                                                               
         LA    R9,APRECKEY+20                                                   
         LA    R0,NMAXWKS                                                       
*                                                                               
GETS630  CLI   0(R8),X'FF'                                                      
         BE    GETS640                                                          
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
         NC    APBYTE,NBRSDAYS                                                  
         BNZ   GETS642                                                          
*                                                                               
GETS640  OI    0(R9),X'80'         DAY IS INVALID                               
GETS642  LA    R9,1(R9)                                                         
         BCT   R0,GETS630                                                       
         B     GETS800                                                          
*                                                                               
GETS650  LR    R1,R5                                                            
         AHI   R1,CMPDATSD-TWAD                                                 
         XR    RF,RF               ANY START DATE OPTION?                       
         ICM   RF,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    RF,CMPDSTDT                                                      
         MHI   RF,6                                                             
         AR    R1,RF                                                            
*                                                                               
         ST    R1,APPARM                                                        
         GOTO1 VGETDAY,APPARM,,APWORK     GET DAY OF WEEK OF FIRST              
         CLC   APWORK(3),SPACES               DAY OF CAMPAIGN                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APBYTE,0(R1)                                                     
         SR    R8,R8                                                            
         ICM   R8,1,ESTOWSDY       TEST OUT-OF-WEEK ROTATORS                    
         BZ    GETS660                                                          
         BCTR  R8,0                YES-GET RELATIVE DAY                         
         ZIC   RE,0(R1)                                                         
         SR    RE,R8                                                            
         BP    *+8                                                              
         LA    RE,7(RE)                                                         
         STC   RE,APBYTE                                                        
*                                                                               
GETS660  ZIC   RE,NBRSDAYS         GET DAY OF WEEK OF LAST DAY OF DAYS          
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
         BO    GETS680                                                          
         GOTO1 VDATCON,APPARM,(3,CMPND),(0,APWORK)                              
*                                                                               
GETS680  GOTO1 VGETDAY,APPARM,APWORK,APWORK+6                                   
         CLC   APWORK+6(3),SPACES                                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APBYTE,0(R1)                                                     
         LTR   R8,R8               TEST OUT-OF-WEEK ROTATORS                    
         BZ    GETS700                                                          
         ZIC   RE,0(R1)                                                         
         SR    RE,R8                                                            
         BP    *+8                                                              
         LA    RE,7(RE)                                                         
         STC   RE,APBYTE                                                        
*                                                                               
GETS700  SR    RE,RE               GET DAY OF WEEK OF FIRST DAY OF DAYS         
         ZIC   RF,NBRSDAYS                                                      
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
         CHI   R1,7                                                             
         BNH   *+8                                                              
         AHI   R1,-7                                                            
         CLM   R1,1,APBYTE         TEST FIRST DAY AFTER CAMPAIGN END            
         BNH   GETS720                                                          
*                                                                               
         ZIC   RF,CMPNWKS          YES - MARK LAST WEEK AS INVALID              
         SR    RE,RE                                                            
         ICM   RE,1,INOSTDTD                                                    
         BNZ   GETS700A                                                         
         CLI   CMPDSTDT,0                                                       
         BE    GETS710                                                          
         IC    RE,CMPDSTDT                                                      
GETS700A SR    RF,RE                                                            
GETS710  CHI   RF,NMAXWKS          WILL LAST WEEK BE SHOWN?                     
         BH    GETS720             NO, CAN'T FIT IT ON THE SCREEN               
         LA    RF,APRECKEY+20(RF)                                               
         BCTR  RF,0                                                             
         OI    0(RF),X'40'                                                      
*                                                                               
GETS720  STC   R1,APHALF           APHALF(1) = FIRST DAY OF DAYS                
         GOTO1 VGETDAY,APPARM,ESTST,APWORK    GET DAY OF WEEK OF FIRST          
         CLC   APWORK(3),SPACES               DAY OF ESTIMATE                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APHALF+1(1),0(R1)              = APHALF+1(1)                     
         LTR   R8,R8               TEST OUT-OF-WEEK ROTATORS                    
         BZ    GETS740                                                          
         ZIC   RE,0(R1)                                                         
         SR    RE,R8                                                            
         BP    *+8                                                              
         LA    RE,7(RE)                                                         
         STC   RE,APHALF+1                                                      
*                                                                               
GETS740  MVC   APWORK(6),ESTST                                                  
         CLI   0(R1),1             GET MONDAY OF EST START WEEK                 
         BE    GETS760                                                          
         ZIC   RE,0(R1)                                                         
         BCTR  RE,0                                                             
         LNR   RE,RE                                                            
         ST    RE,APPARM+8                                                      
         GOTO1 VADDAY,APPARM,ESTST,APWORK                                       
*                                                                               
GETS760  DS    0H                                                               
         XR    R1,R1                                                            
         ICM   R1,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    R1,CMPDSTDT                                                      
         CLM   R1,1,=X'1'          ARE WE ON THE FIRST PAGE?                    
         BH    GETS800              - NOPE, NO NEED FOR CHECK                   
*                                                                               
         MVC   APWORK+6(6),CMPSTMON    CAMPAIGN START MONDAY                    
         TM    CMPOPTS,CAMOWKS         TEST NON-CONTIGUOUS FLIGHT WEEKS         
         BZ    *+10                                                             
         MVC   APWORK+6(6),CMPFLSTM    YES-USE FLIGHT START MONDAY              
         CLC   APWORK+6(6),APWORK  TEST CAMPAIGN START WK=EST START WK          
         BNE   GETS800                                                          
         CLC   APHALF(1),APHALF+1  TEST FIRST DAY BEFORE EST START              
         BNL   GETS800                                                          
         OI    APRECKEY+20,X'40'   YES - FIRST WEEK IS INVALID                  
         DROP  R3                                                               
*                                                                               
GETS800  TM    TWAINDS,TWAICCSW    SAVE TSAR NUMBERS                            
         BO    GETS820             AND PAGE/LINE NUMBERS                        
         MVC   SVTUNUM,LTUNUM                                                   
         MVC   SVTUMAX,LTUMAX                                                   
         MVC   SVPAGE,LPAGE                                                     
         MVC   SVPAGDSP,LPAGDSP                                                 
         MVC   SVLINE,LLINE                                                     
         B     GETSX                                                            
*                                                                               
GETS820  MVC   SVTUNUM2,LTUNUM                                                  
         MVC   SVTUMAX2,LTUMAX                                                  
         MVC   SVPAGE2,LPAGE                                                    
         MVC   SVPAGDS2,LPAGDSP                                                 
         MVC   SVLINE2,LLINE                                                    
         B     GETSX                                                            
*                                                                               
GETS900  MVI   APMODE,APMEOFS      END OF LIST                                  
         B     GETSX                                                            
*                                                                               
GETS998  MVC   FVMSGNO,=AL2(FV8CSOV)   ONLY 8 OR LESS COST OVERRIDES            
         B     GETSX                                                            
FV8CSOV  EQU   1258                                                             
*                                                                               
GETS999  MVC   FVMSGNO,=AL2(FVEREC)   RECORD ERROR                              
*                                                                               
GETSX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT RECORD AFTER A CHANGE                                     
* INPUT : APPARM+0(4) = A(TWA DISPLAY LINE)                                     
*         APRECKEY = RECORD KEY                                                 
***********************************************************************         
DISACHG  L     R8,APPARM                                                        
         ST    R8,COMATWAL                                                      
*                                                                               
         LA    R3,APRECKEY                                                      
         USING BUYKEY,R3                                                        
*                                                                               
         MVC   IOKEY(13),APRECKEY  READ THE RECORD                              
         GOTO1 AIO,DIRRD+IO2                                                    
         BE    DISAC00                                                          
         MVC   FVMSGNO,=AL2(FVFOK)  NOT FOUND-MUST BE FOLLOWING DELETE          
         B     DISACX                                                           
*                                                                               
DISAC00  GOTO1 AIO,FILGET2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIOAREA2                                                      
         CLI   0(R3),NBRKTYPQ                                                   
         BE    DISAC50                                                          
         NI    LFLAG2,X'FF'-LF2MKGDB                                            
         OI    LFLAG2,LF2NPOCM     SO DISPLAYING WON'T PROTECT                  
         NI    MISCFLG1,X'FF'-MF1MANUL-MF1COSOV                                 
         MVI   CURRCOVR,0          WE HAVEN'T DONE ANY COST OVERRIDES           
         MVI   COSTNUMB,0                                                       
         XC    COSTABLE,COSTABLE                                                
*                                                                               
         OC    BDMGDATE,BDMGDATE                                                
         BZ    *+8                                                              
         OI    LFLAG2,LF2MKGDB                                                  
*                                                                               
         LA    RE,BDELEM                                                        
         XR    R0,R0                                                            
DISAC10  CLI   0(RE),0                                                          
         BNE   *+12                                                             
         OI    MISCFLG1,MF1MANUL   NO BWS XFR ELEMENT, MANUALLY ADDED           
         B     DISAC20                                                          
*                                                                               
         CLI   0(RE),BWSCODEQ                                                   
         BE    *+14                                                             
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     DISAC10                                                          
*                                                                               
         MVC   APDUB(2),BCAM                                                    
         XC    APDUB(2),XFF                                                     
*                                                                               
         USING BWSELEM,RE                                                       
         CLC   BWSBYR,QBYR                                                      
         BNE   *+14                                                             
         CLC   BWSCAM,APDUB                                                     
         BE    DISAC20                                                          
         OI    LFLAG2,LF2NPOCM     THIS BUY IS NOT PART OF THIS CAMP            
         B     DISACX              NO CHANGE POSSIBLE                           
         DROP  RE                                                               
*&&DO                                                                           
DISAC20  GOTO1 =A(SETUPNBR),RR=APRELO                                           
*&&                                                                             
DISAC20  GOTO1 ASTUPNBR,APPARM,AIOAREA2,AIOAREA3,DBLOCK                         
         L     R0,AIOAREA2                                                      
****     LA    R1,4000                                                          
         LHI   R1,6000                                                          
         L     RE,AIOAREA3                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
DISAC30  MVC   APWORK(L'IOKEY),IOKEY   SAVE COPY OF BUY KEY WE'RE ON            
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING NBRKEY,R2           LOOK FOR NWS BUY REVISION RECORD             
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ      BINARY CAMPAIGN/MARKET SEQ #                 
         MVC   NBRKSTA,BUYMSTA+L'BMKT                                           
         MVC   NBRKKBUY,BUYKBUY-BUYKEY+APRECKEY                                 
*                                                                               
         GOTO1 AIO,DIRHI+IO3                                                    
         BNE   DISAC50                                                          
*                                                                               
         CLC   NBRKEY,IOKEYSAV      FIND OUR BUY REVISION RECORD?               
         BNE   DISAC50                                                          
         DROP  R2                                                               
*                                                                               
         GOTO1 AIO,FILGET3                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R0,AIOAREA2                                                      
****     LA    R1,4000                                                          
         LHI   R1,6000                                                          
         L     RE,AIOAREA3                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
DISAC50  DS    0H                                                               
         L     R3,AIOAREA2                                                      
         USING NBRKEY,R3                                                        
         LA    R4,TSARREC          BUILD TSAR RECORD                            
         USING TRECD,R4                                                         
         XC    TSARREC(TBUYRECL),TSARREC                                        
         MVC   TRECL,=Y(TBUYRECL)                                               
         MVI   TRECTYP,TRECBUY                                                  
         MVC   TBUYSTA,NBRKSTA                                                  
         MVC   TBUYSEQ,NBRKNBSQ                                                 
         DROP  R4                                                               
*                                                                               
         L     R2,AIOAREA2                                                      
         GOTO1 =A(NBR2TSAR),RR=APRELO                                           
         GOTO1 =A(SETTSRKY),RR=APRELO                                           
*&&DO                                                                           
         L     R3,AIOAREA2                                                      
         XC    TSARREC(TBUYRECL),TSARREC                                        
         MVC   TRECL,=Y(TBUYRECL)                                               
         MVI   TRECTYP,TRECBUY          WE GOT A REGULAR BUYLINE                
*                                                                               
         TM    LFLAG2,LF2MKGDB     THIS BUY A MAKEGOOD?                         
         BZ    *+8                                                              
         OI    TBUYFLG1,TBF1MKGD   YES, NEED TO SHOW A LOWER CASE 'M'           
         TM    MISCFLG1,MF1MANUL   MANUALLY ADDED BUY (NOT XFR BUY)?            
         BZ    *+8                                                              
         OI    TBUYFLG1,TBF1MANL   YES                                          
*                                                                               
         MVC   TBUYSTA,BUYMSTA+L'BMKT   BINARY STATION CODE                     
         MVC   TBUYBUYS,IOKEY+BUYKBUY-BUYKEY     BUY DETAILS IN KEY             
         MVC   TBUYLINE,BUYKBUY    BUY LINE #                                   
         MVC   TBUYDAYS,BDDAY                    DAYS                           
         MVC   TBUYTIMS(L'TBUYTIMS*2),BDTIMST    TIMES                          
         MVC   TBUYPROG,BDPROGRM                 PROGRAM                        
         MVC   TBUYADJC,BDPROGT                  ADJACENCY CODE                 
         MVC   TBUYDYPT,BDDAYPT                  DAYPART                        
         MVC   TBUYSLN,BDSEC                     SPOT LENGTH                    
*&&                                                                             
         J     DISS0A              NOW WE CAN DISPLAY THE CHANGES               
*                                                                               
DISACX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT RECORD                                                    
* INPUT : APPARM+0(4) = A(TWA DISPLAY LINE)                                     
*         APRECKEY = RECORD KEY                                                 
***********************************************************************         
DISSEL   L     R8,APPARM           A(TWA LINE)                                  
         ST    R8,COMATWAL                                                      
         NI    MISCFLG1,X'FF'-MF1DSNBR RESET THE NBR FLAG!!                     
*                                                                               
         LA    R3,APRECKEY                                                      
         USING BUYKEY,R3                                                        
         LA    R4,TSARREC          LOOK AT TSAR RECORD                          
         USING TRECD,R4                                                         
*                                                                               
         BRAS  RE,VALSTDTE         VALIDATE THE START DATE OPTION               
         BNE   DISSX                                                            
*                                                                               
         CLI   APRECKEY,NBRKTYPQ   BUY REVISION RECORD?                         
         BE    DISS000              - YES IT IS!                                
***************                                                                 
* CONVERT BUY INTO BUY REVISION FORMAT ALSO CHECK IF REVISION FOR THIS          
* BUYLINE EXISTS ALREADY                                                        
***************                                                                 
         GOTO1 ASTUPNBR,APPARM,AIOAREA2,AIOAREA3,DBLOCK                         
         L     R0,AIOAREA2                                                      
****     LA    R1,4000                                                          
         LHI   R1,6000                                                          
         L     RE,AIOAREA3                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   APWORK(L'IOKEY),IOKEY   SAVE COPY OF BUY KEY WE'RE ON            
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING NBRKEY,R2           LOOK FOR NWS BUY REVISION RECORD             
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ      BINARY CAMPAIGN/MARKET SEQ #                 
         MVC   NBRKSTA,TBUYSTA                                                  
         MVC   NBRKKBUY,BUYKBUY-BUYKEY+APRECKEY                                 
*                                                                               
         GOTO1 AIO,DIRHI+IO3                                                    
         BNE   DISS00                                                           
*                                                                               
         CLC   NBRKEY,IOKEYSAV      FIND OUR BUY REVISION RECORD?               
         BNE   DISS00                                                           
         DROP  R2                                                               
*                                                                               
         GOTO1 AIO,FILGET3                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R0,AIOAREA2                                                      
****     LA    R1,4000                                                          
         LHI   R1,6000                                                          
         L     RE,AIOAREA3                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
*****  NEW BIT TO SIGNIFY THAT A BUY REVISION RECORD IS PRESENT                 
DISS000  OI    MISCFLG1,MF1DSNBR   WE GOT OURSELVES A BUY REVISION              
*****                                     MHC  07/17/03                         
*                                                                               
DISS00   DS    0H                                                               
*&&DO                                                                           
         CLI   APMODE,APMDISS2     TEST POST RECORD CHANGE DISPLAY MODE         
         BNE   DISS0A                                                           
*                                                                               
         CLC   BUYKAM,BAGYMD       YES-ONLY FOR BUY RECORDS                     
         BNE   DISSX                                                            
         CLC   BUYKCLT,BCLT                                                     
         BNE   DISSX                                                            
         CLC   BUYKPRD,BPRD                                                     
         BNE   DISSX                                                            
         CLC   BUYMSTA(L'BMKT+L'BSTA),BMKT                                      
         BNE   DISSX                                                            
*****    CLC   BUYMSTA+L'BMKT(L'BSTA),TBUYSTA                                   
*****    BNE   DISSX                                                            
         CLC   BUYKEST,BEST                                                     
         BNE   DISSX                                                            
         CLC   BUYKBUY,TBUYBUYS                                                 
         BNE   DISSX                                                            
         DROP  R4                                                               
*                                                                               
         MVC   IOKEY(13),APRECKEY  READ THE RECORD                              
         GOTO1 AIO,DIRRD+IO2                                                    
         BE    DISS0                                                            
         MVC   FVMSGNO,=AL2(FVFOK)  NOT FOUND-MUST BE FOLLOWING DELETE          
         B     DISSX                                                            
*                                                                               
DISS0    GOTO1 AIO,FILGET2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
DISS0A   L     R3,AIOAREA2         R3=A(RECORD)                                 
         LA    R4,TSARREC                                                       
         USING TRECD,R4                                                         
         XC    APWORK,APWORK                                                    
         MVC   APWORK+L'BMKT(L'BSTA),TBUYSTA                                    
         GOTO1 VMSUNPK,APPARM,(X'80',APWORK),QMKT,QSTA                          
*                                                                               
         CLI   APACTN,ACTSSK       TEST ACTION=SUPER-SKED                       
         BNE   DISS2                                                            
         USING SSKL1H,R8                                                        
         NI    SSKSKDH+1,X'FF'-X'20'  TAKE OFF PROTECTED                        
         OI    SSKSKDH+6,X'80'        TRANSMIT                                  
         TM    TBUYFLG1,TBF1NTCM      BUY IS NOT PART OF THIS CAMP?             
         BZ    DISS0B                                                           
         OI    SSKSKDH+1,X'20'        MAKE PROTECTED                            
*                                                                               
DISS0B   MVC   SSKLSP(5),QSTA      STATION                                      
         CLI   SSKLSP+4,C'T'                                                    
         BNE   *+8                                                              
         MVI   SSKLSP+4,C' '                                                    
         MVC   SSKLSP+5(L'SSKLSP-5),SPACES  CLEAR THE REST                      
*                                                                               
         MVC   SSKLSP+10(9),TBUYPROG   PROGRAM                                  
         CLI   QSTA,C'0'               TEST CABLE STATION                       
         BL    DISS1                                                            
         MVC   SSKLSP+10(3),QSTA+5      YES, 1ST 3 CHARS ARE NTWRK              
         LA    R1,SSKLSP+13                                                     
         LA    RE,4                                                             
         CLI   QSTA+7,C' '                                                      
         BH    *+10                                                             
         BCTR  R1,0                                                             
         LA    RE,5                                                             
         MVI   0(R1),C':'                                                       
         MVC   1(0,R1),TBUYPROG                                                 
         EX    RE,*-6                                                           
*                                                                               
DISS1    OI    SSKLSPH+6,FVOXMT                                                 
         MVC   SSKSKD,SPACES       INIT SCHEDULE TO SPACES                      
         LA    R2,SSKLSP+1 <=== RISKY, BUT WE CAN USE THE SAME DSECT            
         B     DISS3                                                            
         DROP  R8                                                               
*                                                                               
         USING WRKL1H,R8                                                        
DISS2    GOTO1 ADISPSEL            DISPLAY THE RECORD                           
         BNE   DISSX                                                            
         LA    R2,WRKLST                                                        
         DROP  R8                                                               
*                                                                               
         USING LIST3D,R2                                                        
DISS3    CLI   APMODE,APMDISS2     TEST POST RECORD CHANGE DISPLAY MODE         
         BE    DISS8               YES-SKIP THE PART THAT WON'T CHANGE          
         ZIC   R1,TBUYLINE         BUYLINE #                                    
         OC    TBUYBUYS,TBUYBUYS                                                
         BNZ   *+8                                                              
         IC    R1,TBUYSEQ                                                       
*                                                                               
         CVD   R1,APDUB                                                         
         UNPK  LINEDISP,APDUB                                                   
         OI    LINEDISP+2,X'F0'                                                 
         MVC   LST3LINE,LINEDISP                                                
         MVI   LIND,C' '                                                        
*&&DO                                                                           
         TM    BWDINDS,BWDITRLK    TEST FOR TRANSFER LOCK-OUT                   
         BZ    *+12                                                             
         MVI   LIND,C':'           YES-SHOW WITH A COLON                        
         B     DISS7                                                            
*&&                                                                             
         SR    R0,R0               NO-LOOK FOR BUY TRANSFER ELEMENT             
         LA    R1,BDELEM                                                        
*                                                                               
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
DISS4    TM    TBUYFLG1,TBF1MKGD                                                
         BZ    *+8                                                              
         MVI   LIND,C'm'           MAKEGOOD BUY - SHOW WITH A c'm'              
*                                                                               
         TM    TBUYFLG1,TBF1NTCM                                                
         BNZ   DISS7                                                            
         TM    TBUYFLG1,TBF1MANL                                                
         BNZ   DISS7                                                            
         OC    TBUYBUYS,TBUYBUYS                                                
         BNZ   *+12                                                             
         MVI   LIND,C'+'                                                        
         B     DISS7                                                            
         TM    TBUYFLG1,TBF1MKGD                                                
         BNZ   *+12                                                             
         MVI   LIND,C'*'           BUY TRANSFER - SHOW WITH A STAR              
         BE    *+8                                                              
         MVI   LIND,C'&&'          BUY TRANSFER AND MAKEGOOD                    
*&&DO                                                                           
         CLI   CLTBWPRO+9,C'Y'                                                  
         BNE   DISS5                                                            
         MVC   APFLAG,BTRIND-BTREL(R1)                                          
         NI    APFLAG,BTRIEC2+BTRIEC3                                           
         CLC   APFLAG,APBYTE                                                    
         BNE   DISS6                                                            
*&&                                                                             
*                                                                               
DISS7    MVC   LST3IND,LIND        LINE INDICATOR                               
         DROP  R2                                                               
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
         LA    R2,WRKL8H+SKEDDSPL  CLEAR ALL THE SCHEDULE LINES                 
         USING SKDLINED,R2                                                      
*                                                                               
DISS10   XC    SKDLSP,SKDLSP                                                    
         OI    SKDLSPH+6,FVOXMT                                                 
         NI    SKDLSPH+FVATRB-FVIHDR,FF-FVAHIGH                                 
         XC    SKDSKD,SKDSKD                                                    
         OI    SKDSKDH+FVOIND-FVIHDR,FVOXMT                                     
         OI    SKDSKDH+FVIIND-FVIHDR,FVIVAL   PREVIOUSLY VALIDATED              
         NI    SKDSKDH+FVATRB-FVIHDR,FF-FVAHIGH                                 
         LA    R2,SKDLINEL(R2)                                                  
         BCT   R0,DISS10                                                        
*                                                                               
         LA    R2,WRKL8H+SKEDDSPL                                               
         B     DISS14                                                           
*                                                                               
DISS12   SR    RE,RE                                                            
         LA    R2,WRKL8H+SKEDDSPL                                               
         LA    R1,SKDLINEL                                                      
         MR    RE,R1                                                            
         AR    R2,RF                                                            
*                                                                               
         USING SKDLINED,R2                                                      
DISS14   TM    MISCFLG1,MF1DSNBR   ARE WE ON A BUY REVISION?                    
         BNO   *+8                  - NOPE, NO HIGHLIGHT                        
***  WE'RE HIGHLIGHTING THE LINE IF IT HAS BUY REVISION!!                       
         OI    SKDLSPH+1,X'08'      - HIGH INTENSITY                            
***                                   MHC   07/17/03                            
         CLI   APMODE,APMDISS2     TEST POST RECORD CHANGE DISPLAY MODE         
         BE    DISS15              YES-SKIP PART THAT WON'T CHANGE              
         MVC   SKDSTA,QSTA         STATION                                      
         MVC   SKDIND,LIND         LINE INDICATOR                               
         MVC   SKDLIN,LINEDISP     LINE NUMBER                                  
         CLI   SKDSTA+4,C'T'                                                    
         BNE   DISS15                                                           
         MVI   SKDSTA+4,C' '                                                    
*                                                                               
DISS15   MVC   SKDPRG,TBUYPROG     PROGRAMMING                                  
         MVC   SKDSKD,SPACES       INIT SCHEDULE TO SPACES                      
         NI    SKDSKDH+1,X'FF'-X'20'   TRANSMIT FOR PROTECTED                   
         OI    SKDSKDH+6,X'80'                                                  
         TM    TBUYFLG1,TBF1NTCM   THIS BUY IS NOT PART OF CAMP?                
         BZ    *+8                                                              
         OI    SKDSKDH+1,X'20'         TRANSMIT FOR PROTECTED                   
         LA    RF,SKDSKD+2                                                      
         B     DISS16A                                                          
         DROP  R3                                                               
*                                                                               
         USING SSKL1H,R8                                                        
DISS16   LA    RF,SSKSKD+2                                                      
*                                                                               
DISS16A  SR    R3,R3                                                            
         ICM   R3,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    R3,CMPDSTDT                                                      
*                                                                               
         L     RE,AIOAREA2                                                      
         USING NBRKEY,RE                                                        
         LA    RE,NBRFSTEL                                                      
         USING NBRSELD,RE                                                       
         ICM   R0,12,NBRSWKS                                                    
         ICM   R0,3,NBRSWKS2                                                    
         ICM   R1,14,NBRSWKS2+2                                                 
         DROP  RE                                                               
         SLDL  R0,0(R3)                                                         
         STM   R0,R1,APDUB                                                      
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,12,APDUB                                                      
*                                                                               
         LR    RE,R3                                                            
         ZIC   R3,CMPNWKS          PLACE . IN OK WEEKS                          
         SR    R3,RE                                                            
*                                                                               
         LA    RE,APRECKEY+20      PLACE X IN OFFLIMIT WEEKS AND                
DISS18   SR    R0,R0                                                            
         SLDL  R0,1                                                             
         MVI   0(RF),C'.'                                                       
*                                                                               
         TM    0(RE),X'C0'                                                      
         BNZ   *+10                                                             
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         MVI   0(RF),C'X'                                                       
         LA    RE,1(RE)                                                         
         LA    RF,4(RF)                                                         
*                                                                               
         LA    R0,APRECKEY+20+14   GOING PAST 14 WEEKS?                         
         CR    RE,R0                                                            
         BNL   DISS19              IT WILL AND KILL OUR SCREEN                  
*                                                                               
         BCT   R3,DISS18                                                        
*                                                                               
DISS19   LA    R1,TBUYSKED                                                      
         SR    R0,R0                                                            
         ICM   R0,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    R0,CMPDSTDT                                                      
         AR    R1,R0                                                            
*                                                                               
******   XR    R3,R3                                                            
******   ICM   R3,3,TBUYOTOM                                                    
         SR    R5,R5               <== BLOWING ADDRESSIBILITY TO TWA            
         ICM   R5,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    R5,CMPDSTDT                                                      
         ICM   RE,15,TBUYOTOM                                                   
         XR    RF,RF                                                            
         ICM   RF,14,TBUYOTOM+4                                                 
         SLDL  RE,0(R5)                                                         
         SRL   RE,16                                                            
         LR    R3,RE                                                            
         L     R5,ATWA             LOADING THIS BACK, NEEDED A REGISTER         
*                                                                               
         LA    RF,TBUYSKED+L'TBUYSKED-1   RF = A(LAST ENTRY IN SKED)            
         LR    RE,R1               DO WE CHANGE THE END FOR BXLE?               
         AHI   RE,13                                                            
         CR    RE,RF                                                            
         BNL   *+6                                                              
         LR    RF,RE               YES WE DO, DON'T TRASH THE SCREEN            
         LA    RE,1                       SETUP FOR THE BXLE                    
*                                                                               
         LA    R2,SKDSKD+1                                                      
         CLI   APACTN,ACTSSK                                                    
         BNE   *+8                                                              
         LA    R2,SSKSKD+1                                                      
         XR    R0,R0                                                            
*                                                                               
DISS22   MVI   APBYTE,0                                                         
         ICM   R3,4,APBYTE                                                      
         SLL   R3,1                SHIFT THE OTO BIT MASK                       
         STCM  R3,4,APBYTE                                                      
*                                                                               
         CLI   1(R2),C'X'                                                       
         BE    DISS24                                                           
         CLI   1(R2),C'*'                                                       
         BE    DISS24                                                           
*                                                                               
         CLI   APBYTE,0            OTO BIT SET?                                 
         BE    DISS23                                                           
         MVI   2(R2),C'?'          YES                                          
         B     DISS23A                                                          
*                                                                               
DISS23   CLI   0(R1),0             ALREADY NOTHING IN THIS WEEK?                
         BE    DISS24              YES, NOTHING TO CHANGE                       
DISS23A  IC    R0,0(R1)                                                         
         CVD   R0,APDUB                                                         
         UNPK  0(2,R2),APDUB                                                    
         OI    1(R2),X'F0'                                                      
*                                                                               
DISS24   LA    R2,4(R2)                                                         
         BXLE  R1,RE,DISS22                                                     
*                                                                               
DISSX    NI    MISCFLG1,X'FF'-MF1DSNBR   RESET THE FLAG                         
*&&DO                                                                           
*** FOR SPBUYVAL ERRORS                                                         
         TM    MISCFLG1,MF1ERROR   DO WE HAVE AN ERROR?                         
         BZ    *+10                 - NOPE                                      
         MVC   FVMSGNO,=AL2(FVFSET)    - YUP                                    
*** FOR SPBUYVAL ERRORS                                                         
*&&                                                                             
         B     EXIT                                                             
         DROP  R2,R4,R8                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE LIST/SELECT DATA                                           *         
* INPUT : APPARM+0(4) = A(TWA DISPLAY LINE)                           *         
***********************************************************************         
VALSEL   GOTO1 =A(VLDSEL),RR=APRELO                                             
         B     EXIT                                                             
***********************************************************************         
* ERASE A RECORD                                                      *         
***********************************************************************         
         SPACE 1                                                                
ERASE    MVC   IOKEY,APRECKEY                                                   
*&&DO                                                                           
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
*&&                                                                             
ERASEX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT KEY FOR THE COMPANION CAMPAIGN TO SAVED KEY TABLE    *         
***********************************************************************         
PUTKEY   GOTO1 =A(PUTTKEY),RR=APRELO                                            
         B     EXIT                                                             
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
CAL2DSPL EQU   CAL1DSPL+56+8+22+8                                               
SKEDDSPL EQU   CAL2DSPL+56+8                                                    
CMTDSPL  EQU   SKEDDSPL+NSKDLINS*(22+8+56+8)                                    
GLDSPL   EQU   CMTDSPL+70+8+1+8                                                 
ACDSPL   EQU   GLDSPL+76+8+1+8                                                  
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
TWABELS  DC    X'013F0119382800',56X'00'    CALENDAR LINE 1                     
         DC    X'011D0102162800',CL22'-Sta- Ln  --Program---'                   
         DC    X'013F0019382800',56X'00'    CALENDAR LINE 2                     
         DC    X'011D0102162000',22X'00'    SCHEDULE LINE 1 PROT                
         DC    X'013F0019380000',56X'00'                    UNPROT              
         DC    X'011D0102162000',22X'00'    SCHEDULE LINE 2                     
         DC    X'013F0019380000',56X'00'                                        
         DC    X'011D0102162000',22X'00'                                        
         DC    X'013F0019380000',56X'00'                                        
         DC    X'011D0102162000',22X'00'                                        
         DC    X'013F0019380000',56X'00'                                        
         DC    X'011D0102162000',22X'00'                                        
         DC    X'013F0019380000',56X'00'                                        
         DC    X'011D0102162000',22X'00'                                        
         DC    X'013F0019380000',56X'00'                                        
         DC    X'011D0102162000',22X'00'                                        
         DC    X'013F0019380000',56X'00'                                        
         DC    X'014D0102462000',70X'00'    COMMENT                             
         DC    X'01080102012800',CL1'G'     GOAL TOTALS                         
         DC    X'015300044C2000',76X'00'                                        
         DC    X'01080102012800',CL1'A'     ACTUAL TOTALS                       
         DC    X'015300044C2000',76X'00'                                        
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT KEY FOR THE COMPANION CAMPAIGN TO SAVED KEY TABLE    *         
***********************************************************************         
         SPACE 1                                                                
PUTTKEY  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,APELEM                                                        
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
PUTKX    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FIRST LIST SCREEN HOOK                                              *         
***********************************************************************         
FRSTLSCR NTR1  BASE=*,LABEL=*                                                   
         MVI   SVACTIV,0           NO AVTIVITY YET                              
         CLI   APACTN,ACTSSK       TEST SUPER-SKED ACTION                       
         BE    FLSCR1                                                           
         CLI   APACTN,ACTSKD       TEST SCHEDULE ACTION                         
         BNE   FLSCRX                                                           
         NI    TWAFLAG,FF-TWAFMTPD                                              
         GOTO1 ATWABLD,TWABELS     BUILD SCHEDULE HALF OF SCREEN                
*                                                                               
FLSCR1   CLI   APPFKEY,PFK07       TEST PF07 - PF08                             
         BL    FLSCR2                                                           
         CLI   APPFKEY,PFK08                                                    
         BH    FLSCR2                                                           
         CLI   CMPNWKS,14          CAMPAIGN > 14 WEEKS?                         
         BNH   FLSCR2              NO NEED TO SCROLL                            
         OC    INOSTDTE,INOSTDTE   ONLY PFSCROLL IF NO STDATE OPTION            
         BNZ   FLSCR2                                                           
         ZIC   R1,CMPDSTDT                                                      
         LA    R0,14                                                            
         CLI   APPFKEY,PFK08       SCROLLING RIGHT?                             
         BE    *+8                                                              
         LHI   R0,-14                                                           
         AR    R1,R0                                                            
         BNM   FLSCR1A                                                          
         MVI   CMPDSTDT,0                                                       
         B     FLSCR2                                                           
*                                                                               
FLSCR1A  CLM   R1,1,CMPNWKS                                                     
         BNL   FLSCR2                                                           
         STC   R1,CMPDSTDT                                                      
*                                                                               
FLSCR2   LR    R2,R5               FORMAT THE SCHEDULE CALENDAR                 
         AHI   R2,CMPDATSD-TWAD                                                 
*                                                                               
         LA    R8,WRKL8H+CAL1DSPL                                               
         LA    R9,WRKL8H+CAL2DSPL                                               
         CLI   APACTN,ACTSSK                                                    
         BE    FLSCR3                                                           
         OI    7(R8),X'80'                                                      
         B     FLSCR4                                                           
FLSCR3   LA    R8,SSKCL1H                                                       
         LA    R9,SSKCL2H                                                       
*                                                                               
FLSCR4   MVC   8(56,R8),SPACES                                                  
         MVC   8(56,R9),SPACES                                                  
         LA    R0,8+55(R9)         RESERVING 1 BYTE FOR "|" OR ">"              
         OI    6(R8),FVOXMT                                                     
         OI    6(R9),FVOXMT                                                     
         LA    R4,L'FVIHDR(R8)                                                  
         LA    R8,L'FVIHDR-1(R8)                                                
         LA    R9,L'FVIHDR-1(R9)                                                
*                                                                               
         XR    RF,RF               DISPLACEMENT INTO CMPDATSD                   
         ICM   RF,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    RF,CMPDSTDT                                                      
*                                                                               
         LTR   RF,RF               ARE WE DISPLACED?                            
         BNZ   *+12                                                             
         MVI   1(R9),C'|'          NO, SIGNIFY NOTHING TO LEFT                  
         B     *+8                                                              
         MVI   1(R9),C'<'          YES, WE CAN SCROLL LEFT                      
*                                                                               
         MHI   RF,6                                                             
         AR    R2,RF                                                            
*                                                                               
FLSCR6   MVC   2(2,R9),4(R2)       DAY NUMBER                                   
         LA    R8,4(R8)                                                         
         LA    R9,4(R9)                                                         
*                                                                               
         CR    R9,R0               WE'RE AT THE END OF FIELD?                   
         BNL   FLSCR8              YES, STILL ONLY SHOW 14 DAYS/WEEKS           
*                                                                               
         CLI   6(R2),FF            TEST END OF CAMPAIGN PERIOD                  
         BE    FLSCR8                                                           
         CLC   2(2,R2),8(R2)       TEST CHANGE OF MONTH                         
         BNE   FLSCR8                                                           
         LA    R2,6(R2)                                                         
         B     FLSCR6                                                           
*                                                                               
FLSCR8   LR    RE,R8               FORMAT MONTH NAME TO LINE ABOVE              
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
*                                                                               
         CLI   6(R2),FF            TEST END OF CAMPAIGN PERIOD                  
         BE    FLSCR9              YES                                          
         CR    R9,R0               WE'RE AT THE END OF FIELD?                   
         BNL   FLSCR10             YES                                          
         LA    R2,6(R2)            NO - CONTINUE                                
         LA    R4,1(R8)                                                         
         B     FLSCR6                                                           
*                                                                               
FLSCR9   MVI   0(R9),C'|'          SO USER KNOWS NOTHING MORE TO RIGHT          
         B     FLSCRX                                                           
*                                                                               
FLSCR10  MVI   0(R9),C'>'          MORE INFO TO THE RIGHT                       
*                                                                               
FLSCRX   J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ACCUMULATE POINTS AND DOLLARS                                      
* INPUT  : IOAREA2 CONTAINS DETAIL RECORD                                       
*          LRATING=DEMO VALUE                                                   
* SVACPTS = VECTOR OF TOTAL POINTS BY WEEK                                      
* SVACDOL = TOTAL DOLLARS                                                       
***********************************************************************         
ACPTSDOL NTR1  BASE=*,LABEL=*                                                   
         LA    R4,TSARREC                                                       
         USING TRECD,R4                                                         
         LA    R8,TBUYSKED                                                      
         LA    RE,TBUYSKED+L'TBUYSKED                                           
         ST    RE,APFULL           APFULL = A(END OF SPOTS)                     
*                                                                               
         SR    R1,R1                                                            
         SR    R2,R2                                                            
         ICM   R2,7,TBUYDEMO+5     TEST RATING=0                                
         BZ    APD10               YES                                          
*                                                                               
         ZIC   R0,CMPNWKS          NO-ACCUMULATE TOTAL RATINGS BY WEEK          
         LA    R9,SVACPTS                                                       
*                                                                               
APD3     SR    RF,RF                                                            
         ICM   RF,1,0(R8)          GET THE # OF SPOTS FOR THIS WEEK             
         BZ    APD6                                                             
         CLI   0(R8),X'FE'         X-ED OUT                                     
         BNE   APD4                                                             
         SR    RF,RF                                                            
APD4     MR    RE,R2               SPOTS X RATING                               
***  2 DECIMAL                                                                  
         TM    TBUYDEMO+4,X'40'    IS THE RATING IN 2 DECIMAL FORMAT?           
         BZ    APD4E                - NOPE                                      
         M     RE,=F'2'                                                         
         D     RE,=F'10'                                                        
         AHI   RF,1                                                             
         SRA   RF,1                                                             
***  2 DECIMAL                                                                  
APD4E    TM    LFLAG2,LCAMP2       TEST COMPANION CAMPAIGN                      
         BZ    *+10                                                             
         AR    R1,RF               YES-ACCUMULATE EXTRA POINTS                  
         B     APD6                                                             
         L     RE,0(R9)                                                         
         AR    RE,RF                                                            
         ST    RE,0(R9)                                                         
*                                                                               
APD6     LA    R8,1(R8)                                                         
         CLM   R8,7,APFULL+1                                                    
         BNL   APD10                                                            
         LA    R9,4(R9)                                                         
         BCT   R0,APD3                                                          
*                                                                               
APD10    TM    LFLAG2,LCAMP2       ADD EXTRA POINTS FOR COMPANION CMPN          
         BZ    *+12                                                             
         A     R1,SVACXPTS                                                      
         ST    R1,SVACXPTS                                                      
*&&DO                              ACCUMULATE TOTAL COST                        
         OC    NBRSEDT2,NBRSEDT2   TEST EFFECTIVE DATES                         
         BZ    APD11                                                            
         GOTO1 VDATCON,APPARM,(3,NBRSEDT2),(2,APDUB)  YES-APDUB(2)=             
         OC    NBRSEDT3,NBRSEDT3                          EFF DATE 2            
         BZ    APD11                                      APDUB+2(2)=           
         GOTO1 VDATCON,APPARM,(3,NBRSEDT3),(2,APDUB+2)    EFF DATE 3            
*&&                                                                             
APD11    ZIC   R0,CMPNWKS                                                       
         LA    R8,TBUYSKED                                                      
         LR    R9,R5                                                            
         AHI   R9,CMPDATSP-TWAD                                                 
         L     R1,SVACDOL          R1=COST ACCUMULATOR                          
*                                                                               
APD12    SR    RF,RF                                                            
         ICM   RF,1,0(R8)                                                       
         BZ    APD15                                                            
         ICM   R2,15,TBUYCOST                                                   
*&&DO                                                                           
         OC    NBRSEDT2,NBRSEDT2   TEST EFF DATE 2                              
         BZ    APD14                                                            
         CLC   2(2,R9),APDUB       YES-TEST DATE IN THIS WEEK                   
         BL    APD14                                                            
         ICM   R2,15,NBRSCST2      YES-SWITCH TO EFF COST 2                     
         OC    NBRSEDT3,NBRSEDT3   TEST EFF DATE 3                              
         BZ    APD14                                                            
         CLC   2(2,R9),APDUB+2     YES-TEST DATE IN THIS WEEK                   
         BL    APD14                                                            
         ICM   R2,15,NBRSCST3      YES-SWITCH TO EFF COST 3                     
*&&                                                                             
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
APDX     J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET GOAL DOLLARS AND POINTS                              *         
***********************************************************************         
GOALS    NTR1  BASE=*,LABEL=*                                                   
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
         BAS   RE,ADDGOAL                                                       
*****                       AFG!!!                                              
         TM    INOIND,INOIALG      WE DOING ALL GOALS?                          
         BO    GOAL45               - YEAH, DON'T NEED MORE GETGOALS            
*****                                                                           
         LA    R8,1(R8)            NEXT SPOT LENGTH                             
         BCT   R2,GOAL34                                                        
*                                                                               
GOAL38   LA    R4,1(R4)            NEXT DAYPART                                 
         B     GOAL32                                                           
*                                                                               
GOAL40   MVI   BDPT,C'$'           TRY $GOALS ALSO                              
         MVI   BSLN,1                                                           
         GOTO1 AGETGOAL                                                         
         BAS   RE,ADDGOAL                                                       
*                                                                               
GOAL45   MVC   BSLN,APBYTE         RESTORE VALUES                               
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
         LA    R1,220(R1)                                                       
         LA    RF,SVGLPTS                                                       
         LA    R3,53                                                            
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
* VALIDATE LIST/SELECT DATA                                           *         
* INPUT : APPARM+0(4) = A(TWA DISPLAY LINE)                           *         
***********************************************************************         
VLDSEL   NTR1  BASE=*,LABEL=*                                                   
         MVC   COMATWAL,APPARM                                                  
*                                                                               
         MVC   IOKEY(13),APRECKEY  READ THE DETAIL RECORD                       
         GOTO1 AIO,DIRRD+IO2                                                    
         BE    *+14                                                             
         MVC   FVADDR,APPARM                                                    
         B     VALSX                                                            
*                                                                               
         GOTO1 AIO,FILGET2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,APRECKEY                                                      
         LA    R4,TSARREC                                                       
         USING TRECD,R4                                                         
         XC    TKEY,TKEY                                                        
         MVI   TRECTYP,TRECBUY                                                  
         MVI   TSARACT,TSARDH                                                   
***      CLI   INORNK,0                                                         
***      BE    VALSEL0                                                          
***      MVI   TSARACT,TSAGET                                                   
***      MVC   TBUYKST1,BUYMSTA+L'BMKT                                          
***      MVC   TBUYKBY1,BUYKBUY                                                 
*                                                                               
VALSEL0  GOTO1 ATSAR,TREC                                                       
*                                                                               
         L     RF,ATSARBLK                                                      
         USING TSARD,RF                                                         
         TM    TSERRS,TSEEOF       EOF?  (THEY'RE F'ING BITS)                   
         BNZ   VALSEL0M                                                         
         DROP  RF                                                               
*                                                                               
         CLI   TRECTYP,TRECBUY                                                  
         BNE   VALSEL0M                                                         
*                                                                               
         CLI   APRECKEY,NBRKTYPQ   BUY REVISION RECORD?                         
         BH    VALSEL07            NO, REGULAR BUY RECORD                       
***************                                                                 
* APRECKEY POINTING TO A BUY REVISION KEY                                       
***************                                                                 
         USING NBRKEY,R3                                                        
         CLI   INORNK,INORNKT                                                   
         BE    VALSEL0B                                                         
         CLC   TBUYKST1,NBRKSTA                                                 
         BNE   VALSEL0A                                                         
         CLC   TBUYKBY1(4),NBRKKBUY   4 BYTES = BUY DETAILS (3) +               
         BNE   VALSEL0A                         BUY REVISION SEQ (1)            
****                                            MHC  08/14/03                   
         CLC   TBUYKIND,APRECKEY+19                                             
         BE    VALSEL0C                                                         
*                                                                               
VALSEL0A MVI   TSARACT,TSANXT                                                   
         B     VALSEL0                                                          
*                                                                               
VALSEL0B CLC   TBUYKST2,NBRKSTA                                                 
         BNE   VALSEL0A                                                         
         CLC   TBUYKSQ2,NBRKNBSQ                                                
         BNE   VALSEL0A                                                         
*                                                                               
         OC    TBUYBUYS,TBUYBUYS        LOOKING AT A BUYLINE?                   
         BZ    *+14                                                             
         CLC   APRECKEY+19(1),TBUYKCOV                                          
         B     *+10                                                             
         CLC   APRECKEY+19(1),TBUYKIND                                          
         BNE   VALSEL0A                                                         
*                                                                               
VALSEL0C L     R0,AIOAREA3                                                      
         L     RE,AIOAREA2                                                      
****     LA    R1,4000                                                          
         LHI   R1,6000                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     VALS00                                                           
***************                                                                 
* APRECKEY POINTING TO A BUY KEY                                                
***************                                                                 
         USING BUYKEY,R3                                                        
VALSEL07 CLI   INORNK,INORNKT                                                   
         BE    VALSEL08                                                         
         CLC   TBUYKST1,BUYMSTA+L'BMKT                                          
         BNE   VALSEL0A                                                         
         CLC   TBUYKBY1,BUYKBUY                                                 
         BE    VALSEL09                                                         
         B     VALSEL0A                                                         
*                                                                               
VALSEL08 CLC   TBUYKST2,BUYMSTA+L'BMKT                                          
         BNE   VALSEL0A                                                         
         CLC   TBUYKBY2,BUYKBUY                                                 
         BNE   VALSEL0A                                                         
*                                                                               
VALSEL09 CLC   TBUYKCOV,APRECKEY+19                                             
         BNE   VALSEL0A                                                         
***************                                                                 
* CONVERT BUY INTO BUY REVISION FORMAT                                          
***************                                                                 
         GOTO1 ASTUPNBR,APPARM,AIOAREA2,AIOAREA3,DBLOCK                         
         MVC   APPARM,COMATWAL                                                  
*&&DO                                                                           
         L     R0,AIOAREA2                                                      
****     LA    R1,4000                                                          
         LHI   R1,6000                                                          
         L     RE,AIOAREA3                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*&&                                                                             
         MVC   APWORK(L'IOKEY),IOKEY   SAVE COPY OF BUY KEY WE'RE ON            
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING NBRKEY,R2           LOOK FOR NWS BUY REVISION RECORD             
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ      BINARY CAMPAIGN/MARKET SEQ #                 
         MVC   NBRKSTA,BUYMSTA+L'BMKT                                           
         MVC   NBRKKBUY,BUYKBUY-BUYKEY+APRECKEY                                 
*                                                                               
         GOTO1 AIO,DIRHI+IO1                                                    
         BNE   VLSEL09A                                                         
*                                                                               
         CLC   NBRKEY,IOKEYSAV      FIND OUR BUY REVISION RECORD?               
         BNE   VLSEL09A                                                         
         DROP  R2                                                               
*                                                                               
         GOTO1 AIO,FILGET1                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R0,AIOAREA1                                                      
****     LA    R1,4000                                                          
         LHI   R1,6000                                                          
         L     RE,AIOAREA3                                                      
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
VLSEL09A MVC   IOKEY,APWORK                                                     
         B     VALS00                                                           
*                                                                               
VALSEL0M MVC   FVADDR,APPARM                                                    
         B     VALSX                                                            
***************                                                                 
* GOT OUR RECORD                                                                
***************                                                                 
VALS00   CLI   APACTN,ACTSSK       TEST ACTION = SUPER-SKED                     
         BNE   VALS4                                                            
         L     RF,APPARM           YES-                                         
         CLI   APRECNUM,RECPKG     TES PACKAGE RECORD                           
         BNE   VALS2                                                            
         MVI   FVIFLD-FVIHDR(RF),C'S'   YES-SET SELECT FIELD TO 'S'             
         MVI   FVILEN-FVIHDR(RF),1          AND EXIT                            
         OI    6(RF),FVOXMT                                                     
         B     VALSX                                                            
*                                                                               
VALS2    LA    R2,SSKSKDH-SSKL1H(RF)   R4=A(SCHEDULE)                           
         GOTO1 AVALSKD,APPARM,(R2),SSKACTH   VALIDATE SKED AND BUILD            
         BNE   VALSX                         SPOTS PER WEEK ELEMENT             
         B     VALS8                                                            
*                                  NOT SUPER-SKED --                            
VALS4    GOTO1 AVALSEL1            VALIDATE PART 1                              
         BNE   VALSX                                                            
*                                                                               
         L     R3,AIOAREA2                                                      
         USING BUYKEY,R3                                                        
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
         LA    R2,WRKL8H+SKEDDSPL                                               
         LA    R1,SKDLINEL                                                      
         MR    RE,R1                                                            
         AR    R2,RF                                                            
         USING SKDLINED,R2                                                      
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
********                                                                        
* SHOULD I OVERWRITE AIO2????                                                   
*  DON'T THINK SO, AIO2 SHOULD BE THE BUYLINE IF IT ORIGINALLY WAS              
*&&DO                                                                           
         L     R0,AIOAREA3         COPY NBR RECORD OVER TO AIOAREA2             
         L     RE,AIOAREA2                                                      
****     LA    R1,4000                                                          
         LHI   R1,6000                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*&&                                                                             
* SHOULD I OVERWRITE AIO2????                                                   
*  DON'T THINK SO, AIO2 SHOULD BE THE BUYLINE IF IT ORIGINALLY WAS              
********                                                                        
*                                                                               
VALS8    CLI   COMCHG,0            ANY CHANGE WHATSOEVER?                       
         BE    VALS10              NONE WHATSOEVER                              
*                                                                               
         L     R8,COMATWAL         VALIDATE PART 2                              
         GOTO1 AVALSEL2                                                         
         BNE   VALSX                                                            
*                                                                               
         TM    COMCHG,LDEMO+LPROG  DID USER CHANGE THE DEMO/PROGRAM?            
         BZ    VALS9                                                            
*                                                                               
         L     R0,AIOAREA1         COPY NBR RECORD OVER TO AIOAREA1             
         L     RE,AIOAREA3                                                      
****     LA    R1,4000                                                          
         LHI   R1,6000                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R3,AIOAREA1                                                      
         USING NBRKEY,R3                                                        
         LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
         LR    R4,R3                                                            
VALS8A   CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),NBRDMELQ      DEMO ELEMENT (X'20')?                        
         BE    VALS8B                                                           
         XR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALS8A                                                           
*                                                                               
VALS8B   GOTO1 APUTCMPR,APPARM,(X'C0',NBRSDAYS),NBRSTIMS,NBRSSTA,(R4), X        
               NBRSPROG                                                         
*                                                                               
VALS8X   L     R0,AIOAREA1         COPY NBR RECORD BACK TO AIOAREA3             
         L     RE,AIOAREA3                                                      
****     LA    R1,4000                                                          
         LHI   R1,6000                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
VALS9    CLI   APACTN,ACTSSK       FOR ACTION NOT SUPER-SKED,                   
         BE    VALS10                                                           
         GOTO1 ADISPSEL            REDISPLAY THE RECORD                         
*                                                                               
VALS10   TM    COMCHG,LCHANGE      TEST FOR ANY CHANGE                          
         BZ    VALSX                                                            
         OI    SVACTIV,SVVALSEL    YES                                          
         B     VALSX                                                            
*                                                                               
VALSX    MVC   SCRLINPT,LSTSCROL   SAME LINE IF PFKEY AFTER ENTER KEY           
         B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SETUP THE TSAR KEY                                                            
*                                                                               
* ON ENTRY:    (R4)                A(TSAR RECORD)                               
***********************************************************************         
SETTSRKY NTR1  BASE=*,LABEL=*                                                   
         USING TRECD,R4                                                         
         L     R1,AIOAREA2                                                      
         CLI   0(R1),NBRKTYPQ                                                   
         BE    STTSRK00                                                         
         L     R1,AIOAREA3         WE MIGHT HAVE A BUY REV RECORD               
         CLI   0(R1),NBRKTYPQ                                                   
         BE    STTSRK00                                                         
         L     R1,AIOAREA2                                                      
         B     STTSRK03                                                         
*                                                                               
         USING NBRKEY,R1                                                        
STTSRK00 OC    NBRKKBUY,NBRKKBUY   MANUALLY ADDED?                              
         BZ    STTSRK01            YES                                          
         TM    MISCFLG1,MF1ECSTS   NO, DO WE HAVE EFFECTIVE COSTS?              
         BZ    STTSRK03                NOPE                                     
         CLI   CURRCOVR,3          PAST ANY EFFECTIVE COSTS?                    
         BNL   STTSRK03            YES                                          
         DROP  R1                                                               
*                                                                               
STTSRK01 MVI   APRECKEY+19,0                                                    
         CLI   CURRCOVR,0                                                       
         BE    STTSRK06                                                         
         CLI   CURRCOVR,1                                                       
         BH    *+12                                                             
         OI    TBUYKIND,TBYUICS2                                                
         B     STTSRK06                                                         
         OI    TBUYKIND,TBYUICS3                                                
         B     STTSRK06                                                         
*                                                                               
STTSRK03 MVC   TBUYKCOV,CURRCOVR                                                
*                                                                               
STTSRK06 CLI   INORNK,INORNKT      RANK BY PURE DAY/TIME?                       
         BE    STTSRK30                                                         
         CLI   INORNK,INORNKP                                                   
         BNE   STTSRK10                                                         
         MVC   TBUYKDP3,TBUYDYPT                                                
         MVC   TBUYKSL3,TBUYSLN                                                 
         B     STTSRK20                                                         
*                                                                               
STTSRK10 MVC   TBUYKDPT,TBUYDYPT                                                
         MVC   TBUYKSLN,TBUYSLN                                                 
*                                                                               
STTSRK20 MVC   TBUYKST1,TBUYSTA          BINARY STATION CODE                    
         MVC   TBUYKBY1,TBUYBUYS         BUY DETAILS IN KEY                     
         MVC   TBUYKSQ1,TBUYSEQ                                                 
***  TBUYKDMO IS STORED BELOW                                                   
***  TBUYKCPP IS CALCULATED BELOW                                               
         MVC   TBUYKDAY,TBUYDAYS                                                
**********************************                                              
* TUDAYCOD COPIED FROM SPNWS05     *                                            
**********************************                                              
         MVI   TBUYKDYC,0                                                       
         CLI   TBUYDAYS,X'7C'      M-F?                                         
         BE    STTSRK25                                                         
         MVI   TBUYKDYC,1                                                       
         CLI   TBUYDAYS,X'7F'      M-SU?                                        
         BE    STTSRK25                                                         
*                                                                               
         SR    RF,RF                                                            
         ZIC   R0,TBUYDAYS                                                      
         SRDL  R0,9                                                             
         SLDL  R0,1                                                             
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         BCT   RF,*-10                                                          
         LPR   RF,RF                                                            
         STC   RF,TBUYKDYC                                                      
*                                                                               
STTSRK25 MVC   TBUYKTIM,TBUYTIMS                                                
         B     STTSRK40                                                         
*                                                                               
STTSRK30 LA    R9,DTTAB            SET SEQUENCE CODES                           
STTSRK32 CLI   0(R9),FF            GET DAY/TIME SEQUENCING CODE                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R9),0                                                          
         BE    *+14                                                             
         CLC   TBUYDAYS,0(R9)                                                   
         BNE   STTSRK34                                                         
         CLC   TBUYTIMS(2),1(R9)                                                
         BL    STTSRK34                                                         
         CLC   TBUYTIMS(2),3(R9)                                                
         BNL   STTSRK34                                                         
         MVC   TBDTSEQ1,5(R9)                                                   
         B     STTSRK36                                                         
*                                                                               
STTSRK34 LA    R9,6(R9)                                                         
         B     STTSRK32                                                         
*                                                                               
STTSRK36 MVI   TBDTSEQ2,1          DAY CODE                                     
         CLI   TBUYDAYS,X'7C'      1=MO-FR                                      
         BE    STTSRK38                                                         
         MVI   TBDTSEQ2,7          7=MO-SU                                      
         CLI   TBUYDAYS,X'7F'                                                   
         BE    STTSRK38                                                         
         MVI   TBDTSEQ2,8          8=SA-SU                                      
         CLI   TBUYDAYS,X'03'                                                   
         BE    STTSRK38                                                         
         SR    R1,R1               2-6=MIXED DAYS                               
         SR    RF,RF               9-15=SINGLE DAY                              
         ZIC   R0,TBUYDAYS                                                      
         SRDL  R0,9                                                             
         SLDL  R0,1                                                             
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         BCT   RF,*-10                                                          
         LPR   RF,RF                                                            
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    RF,7(RF)                                                         
         STC   RF,TBDTSEQ2                                                      
*                                                                               
STTSRK38 SR    RE,RE               TIMES                                        
         ICM   RE,3,TBUYTIMS                                                    
         AHI   RE,-600             6A-MIDNIGHT,MIDNIGHT-6A                      
         BNM   *+8                                                              
         AHI   RE,2400                                                          
         STCM  RE,3,TBUYKTM2                                                    
         ICM   RE,3,TBUYTIMS+2                                                  
         AHI   RE,-600                                                          
         BNM   *+8                                                              
         AHI   RE,2400                                                          
         STCM  RE,3,TBUYKTM2+2                                                  
*                                                                               
STTSRK39 MVC   TBUYKDY2,TBUYDAYS                                                
         MVC   TBUYKST2,TBUYSTA          BINARY STATION CODE                    
         MVC   TBUYKDP2,TBUYDYPT                                                
         MVC   TBUYKSL2,TBUYSLN                                                 
         MVC   TBUYKBY2,TBUYBUYS         BUY DETAILS IN KEY                     
         MVC   TBUYKSQ2,TBUYSEQ                                                 
         B     STTSRKX                                                          
*                                                                               
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
*                                                                               
STTSRK40 CLI   INORNK,INORNKS      RANK BY STATION                              
         BE    STTSRKX                                                          
         LA    R1,TBUYDEMS                                                      
         MVC   LRATING,5(R1)       LRATING IS DEFINED AS  XL3                   
         MVC   TBUYKDMO,5(R1)                                                   
         NI    TBUYKDMO,X'FF'-X'80'    GET RID OF THE OVERRIDE BIT              
         SR    RE,RE               SORT IN CPP OR DEMO SEQ                      
         ICM   RE,7,TBUYKDMO       RE=RATING                                    
         CLI   INORNK,INORNKD      TEST RANK IN DEMO VALUE SEQ                  
         BE    STTSRK50            YES-USE INVERSE RATING                       
         LTR   RE,RE               NO-RANK IN CPP SEQ                           
         BZ    STTSRKX             ZERO RATING-CPP=ZERO                         
         XC    TBUYKCPP,TBUYKCPP                                                
         XC    TBUYKDMO,TBUYKDMO                                                
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,15,TBUYCOST      R1=COST                                      
         BNZ   *+16                TEST ZERO COST                               
         CLI   CLTBWPRO+10,C'Y'    YES-TEST RANK BONUSES FIRST                  
         BE    STTSRKX                 YES-                                     
         B     STTSRK50                NO, RANK IN DEMO SEQUENCE                
*                                                                               
         SR    R0,R0                                                            
***  2 DECIMAL                                                                  
         TM    TBUYDEMS+4,X'40'    IS IT 2 DECIMALS?                            
**       TM    APROFBTS,A00TWODC   2 DECIMAL PRECISION?                         
         BZ    STTSRK47                                                         
         M     R0,=F'10'                                                        
***  2 DECIMAL                                                                  
STTSRK47 M     R0,=F'20'                                                        
         DR    R0,RE                                                            
         LA    R1,1(R1)                                                         
         SRL   R1,1                                                             
         STCM  R1,7,TBUYKCPP                                                    
         B     STTSRKX                                                          
*                                                                               
STTSRK50 LNR   RE,RE               INVERSE RATING                               
         BCTR  RE,0                MINUS ONE FOR ZERO                           
         STCM  RE,7,TBUYKDMO                                                    
*                                                                               
STTSRKX  B     EXIT                                                             
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PUT BUY REVISION RECORD INFORMATION INTO THE TSAR RECORD                      
*                                                                               
* ON ENTRY:    (R4)                A(TSAR RECORD)                               
*              (R2)                A(BUY REVISION RECORD)                       
***********************************************************************         
NBR2TSAR NTR1  BASE=*,LABEL=*                                                   
         USING TRECD,R4                                                         
         USING NBRKEY,R2                                                        
         LA    R5,NBRFSTEL                                                      
         USING NBRSELD,R5          WE'LL SET TWAD BACK TO R5 AT THE END         
         MVC   APFULL,=4X'FF'                                                   
*                                                                               
         TM    MISCFLG1,MF1COSOV   UPTO COST OVERRIDES?                         
         BNZ   NBRTSR00                                                         
         XC    COSTABLE,COSTABLE   NO, CLEAR WHAT THE BUY RECORD HAD            
         MVI   COSTNUMB,0                                                       
         NI    MISCFLG1,X'FF'-MF1ECSTS    NO EFFECTIVE COSTS AS OF YET          
*                                                                               
NBRTSR00 OC    NBRSEDT2,NBRSEDT2   TEST EFFECTIVE DATES                         
         BZ    NBRTSRLP                                                         
         OI    MISCFLG1,MF1ECSTS   NOW WE HAVE EFFECTIVE COSTS                  
         GOTO1 VDATCON,APPARM,(3,NBRSEDT2),(2,APFULL)                           
         OC    NBRSEDT3,NBRSEDT3                                                
         BZ    NBRTSRLP                                                         
         GOTO1 (RF),(R1),(3,NBRSEDT3),(2,APFULL+2)                              
         DROP  R5                                                               
*                                                                               
NBRTSRLP CLI   0(R5),0                                                          
         BE    NBRTSRYS                                                         
         CLI   0(R5),NBRSELQ       SAVED DATA ELEMENT?                          
         BE    NBRTSRSV                                                         
         CLI   0(R5),NBRDMELQ      DEMO ELEMENT?                                
         BE    NBRTSRDM                                                         
         CLI   0(R5),NBRSPELQ      SPOTS/WEEK ELEMENT?                          
         BE    NBRTSRSP                                                         
         CLI   0(R5),NBRCOELQ      COST OVERRIDE ELEMENT?                       
         BE    NBRTSRCO                                                         
*                                                                               
NBRTSRNX XR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     NBRTSRLP                                                         
*                                                                               
         USING NBRSELD,R5                                                       
NBRTSRSV MVC   TBUYDAYS,NBRSDAYS                                                
         MVC   TBUYTIMS(L'TBUYTIMS*2),NBRSTIMS                                  
*                                                                               
         MVC   TBUYCOST,NBRSCST1                                                
         TM    MISCFLG1,MF1COSOV   UPTO COST OVERRIDES?                         
         BZ    NTSRSV05                                                         
         OC    NBRKKBUY,NBRKKBUY   YES, REVISION RECORD FROM A BUYLINE?         
         BZ    NTSRSV05                                                         
         TM    MISCFLG1,MF1ECSTS   DO WE HAVE EFFECTIVE COSTS?                  
         BNZ   NTSRSV02                                                         
NTSRSV01 MVC   TBUYCOST,CURRCOST   YES, THEN USE CURRENT COST OVERRIDE          
         B     NTSRSV10                                                         
*                                                                               
NTSRSV02 CLI   CURRCOVR,3          ARE WE BEYOND EFFECTIVE COSTS?               
         BNL   NTSRSV01            YES                                          
*                                                                               
NTSRSV05 CLI   CURRCOVR,0          BASE COST?                                   
         BE    NTSRSV10            YES                                          
         CLI   CURRCOVR,1          FIRST EFFECTIVE OCST?                        
         BH    NTSRSV09            NO, SECOND ONE                               
*                                                                               
         OC    NBRSEDT2,NBRSEDT2   REALLY HAVE AN EFFECTIVE COST?               
         BZ    NBRTSRNO                                                         
         MVC   TBUYCOST,NBRSCST2                                                
         B     NTSRSV10                                                         
*                                                                               
NTSRSV09 OC    NBRSEDT3,NBRSEDT3                                                
         BZ    NBRTSRNO                                                         
         MVC   TBUYCOST,NBRSCST3                                                
*                                                                               
NTSRSV10 MVC   TBUYPROG,NBRSPROG                                                
         MVC   TBUYADJC,NBRSADJC                                                
         MVC   TBUYDYPT,NBRSDYPT                                                
         MVC   TBUYSDPT,NBRSSBDP                                                
         MVC   TBUYSLN,NBRSSLN                                                  
         TM    NBRSINDS,NBRSIPRG   PROGRAMMING OVERRIDE?                        
         BZ    *+8                  - NOPE                                      
         OI    TBUYFLG1,TBF1PRG     - YUP                                       
         B     NBRTSRNX                                                         
*                                                                               
         USING NBRDMELD,R5                                                      
NBRTSRDM XC    TBUYDEMS,TBUYDEMS                                                
         XR    RF,RF                                                            
         IC    RF,NBRDMLEN                                                      
         SHI   RF,NBRDMDMO-NBRDMELD+1     HOW MUCH TO COPY                      
*                                                                               
         LA    R0,L'TBUYDEMS-1     MAKE SURE WE DON'T SURPASS TBUYDEMS          
         CR    RF,R0                                                            
         BNH   *+6                                                              
         LR    RF,R0                                                            
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TBUYDEMO(0),NBRDMDMO                                             
         B     NBRTSRNX                                                         
*                                                                               
         USING NBRSPELD,R5                                                      
NBRTSRSP XC    TBUYSKED,TBUYSKED                                                
         TM    MISCFLG1,MF1COSOV   UPTO COST OVERRIDES?                         
         BZ    NTSRSP05                                                         
         OC    NBRKKBUY,NBRKKBUY   MANUALLY ADDED BUY REVISION?                 
         BZ    NTSRSP05            YES                                          
         TM    MISCFLG1,MF1ECSTS   ANY EFFECTIVE COSTS?                         
         BZ    NBRTSRNX            NO, LET THE OVERRIDES FILL THIS UP           
         CLI   CURRCOVR,3          YES, ARE WE PAST EFFECTIVE COSTS?            
         BNL   NBRTSRNX                 YES                                     
*                                                                               
NTSRSP05 XR    RF,RF                                                            
         IC    RF,NBRSPLEN                                                      
         SHI   RF,NBRSPSPW-NBRSPELD+1     HOW MUCH TO COPY                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TBUYSKED(0),NBRSPSPW                                             
*                                                                               
         LA    R9,TBUYSKED                                                      
         AR    RF,R9               RF = A(LAST SPOT ENTRY)                      
***      LR    RE,R5               R5 IS NOT POINTING TO ATWA                   
         L     RE,ATWA                                                          
         AHI   RE,CMPDATSP-TWAD                                                 
*                                                                               
NTSRSP10 CR    R9,RF                                                            
         BH    NTSRSP50                                                         
*                                                                               
         CLI   CURRCOVR,0                                                       
         BH    NTSRSP20                                                         
         CLC   2(2,RE),APFULL      MAKE SURE LESS THAN 1ST EFFECTIVE            
         BL    NTSRSP45                                                         
         B     NTSRSP40                                                         
*                                                                               
NTSRSP20 CLI   CURRCOVR,1                                                       
         BH    NTSRSP30                                                         
         CLC   2(2,RE),APFULL      YES-IF EFF DATE 1 RANGE,                     
         BL    NTSRSP40                                                         
         CLC   2(2,RE),APFULL+2    MAKE SURE LESS THAN 2ND EFFECTIVE            
         BL    NTSRSP45                                                         
         B     NTSRSP40                                                         
*                                                                               
NTSRSP30 CLC   2(2,RE),APFULL+2    YES-IF EFF DATE 1 RANGE,                     
         BNL   NTSRSP45                                                         
*                                                                               
NTSRSP40 MVI   0(R9),0             ZERO OUT THIS WEEK                           
*                                                                               
NTSRSP45 LA    R9,1(R9)            NEXT SPOT WEEK                               
         LA    RE,4(RE)                                                         
         B     NTSRSP10                                                         
*                                                                               
NTSRSP50 B     NBRTSRNX                                                         
*                                                                               
         USING NBRCOELD,R5                                                      
NBRTSRCO ICM   R0,15,NBRCOCST                                                   
         TM    MISCFLG1,MF1COSOV   DOING OVERRIDES ALREADY?                     
         BZ    NTSRCO00            NO                                           
         TM    MISCFLG1,MF1ECSTS   OVERRIDES, WE HAVE EFFECTIVE COSTS?          
         BZ    NTSRCO50                       NO                                
         CLI   CURRCOVR,3                     YES, PAST THEM?                   
         BNL   NTSRCO50                            YES                          
*                                                                               
NTSRCO00 LA    RE,COSTABLE                                                      
         CLI   COSTNUMB,0                                                       
         BE    NTSRCO20                                                         
         XR    R1,R1                                                            
         IC    R1,COSTNUMB                                                      
NTSRCO10 CLM   R0,15,0(RE)                                                      
         BE    NTSRCO30                                                         
         LA    RE,L'NBRSCST1(RE)                                                
         BCT   R1,NTSRCO10                                                      
*                                                                               
NTSRCO20 XR    R1,R1                                                            
         IC    R1,COSTNUMB                                                      
         LA    R1,1(R1)                                                         
         CHI   R1,MAXCOSTS                                                      
         BNH   *+6                                                              
         DC    H'0'                                                             
         STC   R1,COSTNUMB                                                      
         STCM  R0,15,0(RE)                                                      
*                                                                               
***RCO30 LR    R1,R5                                                            
NTSRCO30 L     R1,ATWA             R5 IS NOT POINTING TO ATWA                   
         AHI   R1,CMPDATSP-TWAD                                                 
NTSRCO35 CLC   NBRCODAT,0(R1)                                                   
         BL    *+14                                                             
         CLC   NBRCODAT,2(R1)                                                   
         BNH   *+12                                                             
         LA    R1,4(R1)                                                         
         B     NTSRCO35                                                         
*                                                                               
         LR    RE,R1                                                            
         LR    RF,R5                                                            
         AHI   RF,CMPDATSP-TWAD                                                 
         SR    RE,RF                                                            
         SRL   RE,2                                                             
         LA    RF,TBUYSKED(RE)                                                  
*                                                                               
         CLI   0(RF),0                                                          
         BE    NBRTSRNX                                                         
*                                                                               
         XR    RE,RE               NEED TO DECREMENT # OF SPOTS BY # OF         
         IC    RE,0(RF)              OVERRIDE SPOTS TO GET # OF SPOTS           
         BCTR  RE,0                  BUY COST                                   
         STC   RE,0(RF)                                                         
         B     NBRTSRNX                                                         
*                                                                               
NTSRCO50 CLM   R0,15,CURRCOST                                                   
         BNE   NBRTSRNX                                                         
***      LR    R1,R5                                                            
         L     R1,ATWA             R5 IS NOT POINTING TO ATWA                   
         AHI   R1,CMPDATSP-TWAD                                                 
NTSRCO60 CLC   NBRCODAT,0(R1)                                                   
         BL    *+14                                                             
         CLC   NBRCODAT,2(R1)                                                   
         BNH   NTSRCO70                                                         
         LA    R1,4(R1)                                                         
         CLI   0(R1),X'FF'         EOT?                                         
         BE    NBRTSRNX            YES, NO MATCH                                
         B     NTSRCO60                                                         
*                                                                               
NTSRCO70 LR    RE,R1                                                            
***      LR    RF,R5                                                            
         L     RF,ATWA             R5 IS NOT POINTING TO ATWA                   
         AHI   RF,CMPDATSP-TWAD                                                 
         SR    RE,RF                                                            
         SRL   RE,2                                                             
         LA    RF,TBUYSKED(RE)                                                  
         XR    RE,RE               NEED TO DECREMENT # OF SPOTS BY # OF         
         IC    RE,0(RF)              OVERRIDE SPOTS TO GET # OF SPOTS           
         LA    RE,1(RE)              BUY COST                                   
         STC   RE,0(RF)                                                         
         B     NBRTSRNX                                                         
*                                                                               
NBRTSRYS B     YES                                                              
*                                                                               
NBRTSRNO B     NO                                                               
         DROP  R2,R4                                                            
         USING TWAD,R5                                                          
         LTORG                                                                  
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
VALP2    CLI   APACTN,ACTSSK       FOR SCHEDULE ACTIONS -                       
         BE    *+12                                                             
         CLI   APACTN,ACTSKD                                                    
         BNE   VALPX                                                            
         CLI   APPFKEY,PFK01       TEST PF1 - PF5                               
         BL    VALP2A                                                           
         CLI   APPFKEY,PFK05                                                    
         BH    VALP2A                                                           
         MVI   RCPDSTDT,0          FORCE RECAP TO START FROM 1ST WK/DAY         
         GOTO1 VDMGR,APPARM,DMWRITE,TEMPSTR,(4,0),TWAD                          
         MVI   APMODE,APMSWP       YES-SWAP TO RECAP SCREEN                     
         MVI   APPARM,RECBUY                                                    
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
***********************************                                             
* STILL IN SCHEDULING ACTIONS:  SCROLLING LEFT OR RIGHT?                        
***********************************                                             
VALP2A   CLI   APPFKEY,PFK07       TEST PF07 - PF08                             
         BL    VALP2G                                                           
         CLI   APPFKEY,PFK08                                                    
         BH    VALP2G                                                           
         MVC   ACPFFST,APPFKEY     GETSEL FROM FIRST PAGE                       
         MVC   SCPFKEY,ACPFFST       SO THAT TOTALS ARE CORRECT                 
*** GETTING INVALID INPUT ON FIRST SCROLL AFTER COMBIN/ADD                      
         CLI   SCRLINPT,C'A'                                                    
         BNL   *+8                                                              
         MVI   SCRLINPT,C'A'                                                    
         CLI   SCRLINPT+1,C'1'                                                  
         BNL   *+8                                                              
         MVI   SCRLINPT+1,C'1'                                                  
*** GETTING INVALID INPUT ON FIRST SCROLL AFTER COMBIN/ADD                      
         MVC   WRKSCR(2),SCRLINPT  NEED THIS TO REMAIN ON THE SAME PAGE         
         MVI   WRKSCRH+5,2                                                      
         B     VALP3                                                            
*                                                                               
VALP2G   DS    0H                                                               
         MVC   LSTSCROL,SCRLINPT                                                
*                                                                               
         MVC   SCRLINPT(1),SVPAGE   SAVE THE NEW PAGE                           
         MVC   SCRLINPT+1(1),SVLINE                                             
         OI    SCRLINPT+1,X'F0'                                                 
         CLC   SVTUNUM,SVTUMAX     WILL BE GOING TO BEGINNING?                  
         BNE   VALP3                                                            
         MVC   SCRLINPT(2),=C'A1'  YES                                          
*                                                                               
VALP3    CLI   APPFKEY,PFK10        TEST PF10 KEY                               
         BNE   VALP5                                                            
         MVI   APPFKEY,0                                                        
*&&DO                                                                           
         OC    CMPCCAM,CMPCCAM     AND THERE'S A COMPANION CAMPAIGN             
         BZ    VALP5                                                            
         TM    TWAINDS,TWAICCSW    YES-TEST ALREADY IN COMPANION                
         BZ    VALP4                                                            
         MVI   APMODE,APMRET       YES-RETURN TO ORIGINAL CAMPAIGN              
         MVI   APLSMSEL,C' '       WITH INVISIBLE COMPLETION CHARACTER          
         NI    APINDS2,255-APIMDIS2                                             
         NI    TWAINDS,255-TWAICCSW-TWAICCS1                                    
         NI    TWAINDS2,255-TWAIBYSW-TWAIBYS1                                   
         NI    TWALSCTL,255-TWALSHSL                                            
*                                                                               
         LA    R0,SVGVA                                                         
         LA    R1,SVGVAL                                                        
         LA    RE,SVCCGVA                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE               RESTORE GOAL VS ACTUAL AREA                  
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
*                                                                               
         LA    R0,SVGVA                                                         
         LA    R1,SVGVAL                                                        
         LA    RE,SVCCGVA                                                       
         LR    RF,R1                                                            
         MVCL  RE,R0               SAVE GOAL VS ACTUAL AREA                     
*&&                                                                             
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
VALPX    B     EXIT                                                             
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMMON ROUTINES                                                     *         
***********************************************************************         
         SPACE 1                                                                
COMMON   NMOD1 0,**BW35**,RA,R9                                                 
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
COMXIT   J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT PARAMETERS -- PART 1                                *         
*                                                                     *         
* INPUT  : R1=A(SECOND KEY FIELD FOR MKT AND DPT/LEN) OR 0            *         
* OUTPUT : IF ERROR, FVMSGNO SET AND FVINDX SET                       *         
*          APRECKEY                                                             
***********************************************************************         
VALPARM  BRAS  RE,VLPARM                                                        
         B     COMMONX                                                          
***********************************************************************         
* DISPLAY LIST/SELECT RECORD -- PART 1                                          
*                                                                               
* INPUT  : R8 = A(TWA DISPLAY LINE)                                             
*          IOAREA2 CONTAINS NBR RECORD                                          
***********************************************************************         
         SPACE 1                                                                
DISPSEL  LA    R3,TSARREC                                                       
         USING TRECD,R3                                                         
         USING WRKL1H,R8                                                        
         XC    WRKDLM,WRKDLM                                                    
         OI    WRKDLMH+6,FVOXMT                                                 
         LA    R4,WRKLST                                                        
         USING LIST3D,R4                                                        
         MVC   APWORK(L'BMKT),BMKT                                              
         MVC   APWORK+L'BMKT(L'TBUYSTA),TBUYSTA                                 
         GOTO1 VMSUNPK,APPARM,APWORK,QMKT,APWORK+5                              
         MVC   LST3STA,APWORK+5    STATION COULD BE CABLE WITH NTWRK            
*                                                                               
         TM    MISCFLG1,MF1DSNBR   ARE WE ON A BUY REVISION?                    
         BNO   *+8                  - NOPE, NO HIGHLIGHT                        
***  WE'RE HIGHLIGHTING THE LINE IF IT HAS BUY REVISION!!                       
         OI    WRKLSTH+1,X'08'      - HIGH INTENSITY                            
***                                   MHC   07/17/03                            
*                                                                               
         OI    WRKLSTH+6,FVOXMT                                                 
         OI    WRKDAYH+6,FVOXMT    DAYS/TIMES                                   
         OI    WRKTIMH+6,FVOXMT                                                 
*                                                                               
         NI    WRKRATH+1,X'FF'-X'20'                                            
         NI    WRKDAYH+1,X'FF'-X'20'                                            
         NI    WRKTIMH+1,X'FF'-X'20'                                            
         NI    WRKCSTH+1,X'FF'-X'20'                                            
         NI    WRKPRGH+1,X'FF'-X'20'                                            
         OI    WRKRATH+6,X'80'         CHANGE TO PROTECTED                      
         OI    WRKDAYH+6,X'80'                                                  
         OI    WRKTIMH+6,X'80'                                                  
         OI    WRKCSTH+6,X'80'                                                  
         OI    WRKPRGH+6,X'80'                                                  
         TM    TBUYFLG1,TBF1NTCM   THIS BUY IS NOT PART OF THIS CAMP?           
         BZ    DSEL4               NO                                           
         OI    WRKRATH+1,X'20'                                                  
         OI    WRKDAYH+1,X'20'                                                  
         OI    WRKTIMH+1,X'20'                                                  
         OI    WRKCSTH+1,X'20'                                                  
         OI    WRKPRGH+1,X'20'                                                  
*&&DO                                                                           
         CLI   BDDAY,0             TEST DAYS PRESENT                            
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
*&&                                                                             
DSEL4    MVC   APHALF,BDPT         SAVE DPT & SLN FILTERS                       
         GOTO1 AGETDAY,TBUYDAYS    DAYS                                         
         MVC   WRKDAY,QDAYS                                                     
*                                                                               
         GOTO1 AGETTIM,TBUYTIMS    TIMES                                        
         MVC   WRKTIM,QTIMES                                                    
         MVC   BDPT(2),APHALF      RESTORE FILTERS                              
*                                                                               
DSEL6    XC    WRKCST,WRKCST       COST                                         
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBTIN,C'B'             BINARY VALUES                             
         MVI   EBLIN,L'TBUYCOST                                                 
         MVI   EBFLOAT,C'$'                                                     
         MVI   EBDECS,2                                                         
*                                                                               
         LA    RF,WRKCST                                                        
         ST    RF,EBAOUT                                                        
         MVI   EBLOUT,L'WRKCST                                                  
*                                                                               
         L     RF,AIOAREA2                                                      
         USING NBRKEY,RF                                                        
         LA    RF,NBRFSTEL                                                      
         USING NBRSELD,RF                                                       
         TM    APRECID,RIEFFDT2                                                 
         BZ    *+12                                                             
         LA    RF,NBRSCST2                                                      
         B     DSEL8                                                            
         TM    APRECID,RIEFFDT3                                                 
         BZ    *+12                                                             
         LA    RF,NBRSCST3                                                      
         B     DSEL8                                                            
         DROP  RF                                                               
*                                                                               
         LA    RF,TBUYCOST                                                      
*                                                                               
DSEL8    ST    RF,EBAIN                                                         
         MVC   COMDETCS,0(RF)                                                   
*                                                                               
         TM    0(RF),X'80'         NEGATIVE AMOUNT?                             
         BZ    *+8                                                              
         MVI   EBFLOAT,C'-'        SHOW MINUS SIGN                              
*                                                                               
         SR    RE,RE                                                            
         ICM   RF,15,0(RF)                                                      
         BNZ   *+14                                                             
         MVC   WRKCST+4(2),=C'$0'                                               
         B     DSEL16                                                           
*                                                                               
         D     RE,=F'100'                                                       
         LTR   RE,RE               RE = REMAINDER                               
         BZ    DSEL12              RF = QUOTIENT                                
*                                                                               
DSEL10   LA    RE,L'WRKCST-4                                                    
         LTR   RE,RE                                                            
         BNP   DSEL12                                                           
         LA    R1,1                                                             
         MHI   R1,10                                                            
         BCT   RE,*-4                                                           
         CR    RF,R1                                                            
         BL    DSEL14                                                           
*                                                                               
DSEL12   MVI   EBSCIN,X'82'        SO THAT '12.34' IS SHOWN AS '12'             
         MVI   EBDECS,0                                                         
*                                                                               
DSEL14   GOTO1 VEDITOR,APPARM,EBLOCK   FORMAT THE COST                          
*                                                                               
DSEL16   OI    WRKCSTH+6,FVOXMT                                                 
*                                                                               
         MVC   WRKPRG,TBUYPROG     PROGRAMMING                                  
         OI    WRKPRGH+6,FVOXMT                                                 
*                                                                               
         L     RF,AIOAREA2                                                      
         USING NBRKEY,RF                                                        
         LA    RF,NBRFSTEL                                                      
         USING NBRSELD,RF                                                       
         TM    TBUYKIND,TBYUICS2   EFFECTIVE DATE                               
         BZ    *+14                                                             
         MVC   APFULL(3),NBRSEDT2                                               
         B     *+18                                                             
         TM    TBUYKIND,TBYUICS3                                                
         BZ    DSEL18                                                           
         MVC   APFULL(3),NBRSEDT3                                               
         DROP  RF                                                               
*                                                                               
         LA    R4,WRKDLM                                                        
         USING LIST2D,R4                                                        
         GOTO1 VDATCON,APPARM,(3,APFULL),(4,LISTMISC)                           
*                                                                               
DSEL18   LA    R4,TBUYDEMS                                                      
         LR    R1,R4               FIND TARGET RATING AND IMPRESSION            
*                                                                               
         LA    RE,L'TBUYDEMO       SET UP BXLE LOOP                             
         LA    RF,L'TBUYDEMS-1(R4)                                              
         XC    APDUB,APDUB                                                      
         XC    APFULL,APFULL                                                    
         MVI   EBFLOAT,0                                                        
*                                                                               
DSEL22   CLC   1(2,R1),SVDEMO1+1                                                
         BNE   DSEL22E                                                          
*****  WE NEED THIS WHEN IT'S CALLED IN VSEL!!!!                                
***  2 DECIMAL                                                                  
         LR    R0,RE               SAVE OFF RE                                  
         LR    R2,RF               SAVE OFF RF                                  
         ST    R1,APPARM                                                        
         GOTO1 AADJPREC,APPARM                                                  
         L     R1,APPARM                                                        
         LR    RE,R0               RESTORE RE                                   
         LR    RF,R2               RESTORE RF                                   
***  2 DECIMAL                                                                  
*****  WE NEED THIS WHEN IT'S CALLED IN VSEL!!!!                                
         MVC   APFULL,4(R1)        APFULL = TARGET DEMO                         
DSEL22E  CLC   1(2,R1),SVDEMO2+1                                                
         BNE   *+10                                                             
         MVC   APDUB(4),4(R1)      APDUB(4) = TARGET IMPRESSION/RATING          
         BXLE  R1,RE,DSEL22                                                     
*                                                                               
         TM    APFULL,DMODEMOV     MANUAL OVERRIDE?                             
         BZ    *+12                                                             
         MVI   EBFLOAT,C'*'        YES, SHOW AS *999.9                          
*                                                                               
         NI    APFULL,X'FF'-DMODEMOV                                            
         LA    RF,APFULL                                                        
         ST    RF,EBAIN                                                         
         MVI   EBLIN,4                                                          
         LA    RF,WRKRAT                                                        
         ST    RF,EBAOUT                                                        
         MVI   EBLOUT,L'WRKRAT                                                  
         MVI   EBDECS,1                                                         
***  2 DECIMAL                                                                  
         TM    APFULL,DMODEM2D     X'40' 2 DECIMAL PRECISION?                   
         BZ    *+12                 - NOPE                                      
         MVI   EBDECS,2             - YUP                                       
         NI    APFULL,FF-DMODEM2D                                               
***  2 DECIMAL                                                                  
         MVI   EBSCIN,0                                                         
         CLI   SVDEMO1+1,C'I'      TEST FOR IMPRESSION                          
         BNE   *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
**                                                                              
         OC    APFULL,APFULL       IS IT 0?                                     
         BNZ   DSEL22R              - NOPE                                      
         MVC   WRKRAT,=C'   0.0'                                                
         CLI   EBFLOAT,C'*'        DO WE NEED AN ASTERISK?                      
         BNE   *+10                 - NOPE                                      
         MVC   WRKRAT,=C'  *0.0'                                                
***  2 DECIMAL  ***                                                             
         CLI   EBDECS,2            ARE WE DOING 2 DECIMALS?                     
         BNE   DSEL22X              - NOPE                                      
         MVC   WRKRAT,=C'  0.00'   YES-EDIT MYSELF                              
         CLI   EBFLOAT,C'*'        DO WE NEED AN ASTERISK?                      
         BNE   *+10                 - NOPE                                      
         MVC   WRKRAT,=C' *0.00'   YES-EDIT MYSELF                              
         B     DSEL22X                                                          
***  2 DECIMAL  ***                                                             
*                                                                               
DSEL22R  GOTO1 VEDITOR,APPARM,EBLOCK                                            
DSEL22X  OI    WRKRATH+6,FVOXMT                                                 
***  2 DECIMAL                                                                  
         CLI   EBDECS,2            WAS IT 2 DECIMALS?                           
         BNE   *+8                  - NOPE                                      
         OI    APFULL,DMODEM2D      - YES, X'40' 2 DECIMAL PRECISION ON         
***  2 DECIMAL                                                                  
*                                                                               
***  NEED TO PAD WRKCPP WITH SPACES JUST IN CASE RATING IS 0                    
         MVC   WRKCPP,=C'      '                                                
         OI    WRKCPPH+6,FVOXMT    TRANSMIT                                     
***  NEED TO TRANSMIT AS WELL     07/22/02  MHC                                 
*                                                                               
         OC    COMDETCS,COMDETCS   TEST COST=0                                  
         BZ    DSEL24              YES                                          
         OC    APFULL,APFULL       NO-TEST RATING=0                             
         BZ    DSEL24              YES                                          
*                                                                               
         L     R1,COMDETCS         NO-CALCULATE CPP                             
         M     R0,=F'20'                                                        
***  2 DECIMAL                                                                  
         TM    APFULL,DMODEM2D     X'40' 2 DECIMAL PRECISION?                   
         BZ    DSEL23               - NOPE                                      
         NI    APFULL,FF-DMODEM2D                                               
         OC    APFULL,APFULL       TEST RATING=0                                
         BZ    DSEL24              YES                                          
         M     R0,=F'10'            - YUP                                       
***  2 DECIMAL                                                                  
DSEL23   D     R0,APFULL                                                        
         LA    R1,1(R1)                                                         
         SRL   R1,1                                                             
         STCM  R1,7,APRECID+1                                                   
*                                                                               
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
         NI    APDUB,X'FF'-DMODEMOV  NO-DISPLAY CPM                             
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
         MVC   LISTCPM(1),TBUYDYPT                                              
         ZIC   RE,TBUYSLN                                                       
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
*                                                                               
         CLI   TBUYSDPT,0          DISPLAY SUB-DAYPART IF POSS                  
         BE    DSEL30                                                           
         CLC   LISTMISC,BLANKS                                                  
         BH    DSEL30                                                           
         MVC   LISTMISC(4),=C'DPT='                                             
         MVC   LISTMISC+4(1),TBUYSDPT                                           
*                                                                               
DSEL30   CLI   TBUYADJC,0          TEST PROGRAM ADJACENCY CODE                  
         BE    DSEL32                                                           
         MVC   LISTMISC(3),=C'AJ=' YES-DISPLAY IT IN MISC FIELD                 
         MVC   LISTMISC+3(1),TBUYADJC                                           
         MVI   LISTMISC+4,C' '                                                  
         CLI   TBUYADJC,C'A'       TEST ALPHA                                   
         BNL   DSEL32                                                           
         UNPK  APFULL(3),TBUYADJC  NO-THEN NUMERIC                              
         MVC   LISTMISC+3(2),APFULL                                             
*                                                                               
DSEL32   MVC   APWORK(L'BMKT),BMKT                                              
         MVC   APWORK+L'BMKT(L'TBUYSTA),TBUYSTA                                 
         GOTO1 VMSUNPK,APPARM,(X'80',APWORK),QMKT,APWORK+5                      
         CLI   APWORK+5,C'0'           TEST CABLE STATION                       
         BL    DSELX                                                            
         MVC   LISTMISC,BLANKS     YES-DISPLAY NETWORK IN MISC FIELD            
         MVC   LISTMISC(3),APWORK+5+5                                           
*                                                                               
DSELX    B     COMMONX                                                          
         DROP  R3,R4,R8                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE LIST/SELECT DATA -- PART 1                                           
* INPUT  : COMATWAL   = A(TWA DISPLAY LINE)                                     
*          APRECID(1) = RECORD IDENTIFICATION BYTE                              
*          IOAREA2 CONTAINS BUY RECORD                                          
* OUTPUT : COMCHG = RECORD CHANGE INDICATORS                                    
***********************************************************************         
VALSEL1  L     R2,COMATWAL                                                      
         USING WRKL1H,R2                                                        
         L     R3,AIOAREA2                                                      
         USING BUYKEY,R3                                                        
         LA    R4,TSARREC                                                       
         USING TRECD,R4                                                         
*&&DO                                                                           
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
*&&                                                                             
VSEL1    MVI   COMCHG,0                                                         
         XC    COMCPPC,COMCPPC     CLEAR COST FOR CPP CALCULATION               
         XC    COMCPPD,COMCPPD           DEMO                                   
         XC    COMSLNS,COMSLNS     CLEAR MULTIPLE SPOT LENGTHS                  
*                                                                               
         TM    WRKPRGH+FVIIND-FVIHDR,FVIVAL   PROGRAMMING                       
         BO    VSEL6                                                            
*                                                                               
         GOTO1 ACHKBPPR,WRKPRGH                                                 
         BNE   VSELX                                                            
*                                                                               
         GOTO1 AFVAL,WRKPRGH                                                    
         BH    VSELX                                                            
         BL    VSEL6               PROGRAM BLANK - IGNORE IT                    
*                                                                               
         CLI   FVIFLD,C'='         TEST PROGRAM ADJACENCY CODE                  
         BNE   VSEL2                                                            
         GOTO1 AVALADJ,FVIFLD+1    VALIDATE PROGRAM ADJACENCY CODE              
         BNE   VSELX                                                            
         CLC   TBUYADJC,QADJCD      VALID - TEST CHANGE                         
         BE    VSEL6                                                            
         MVC   TBUYADJC,QADJCD      YES - SET ADJACENCY CODE                    
         OI    COMCHG,LPROG              INDICATE CHANGE (LIKE PROGRAM)         
         B     VSEL6                                                            
*                                                                               
VSEL2    CLC   FVIFLD(4),=C'SLN='    TEST SPOT LENGTHS FOR SID TRANSFER         
         BNE   VSEL4                                                            
         BAS   RE,VALSLNS            YES-VALIDATE                               
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
VSEL4    BAS   RE,VALCAMPS         VALIDATE FOR CAMPAIGNS=                      
         BNE   VSELX                                                            
         OC    COMCOLNS,COMCOLNS                                                
         BZ    VSEL5                                                            
         OI    COMCHG,LCOPY        YES-INDICATE COPY TO OTHER CAMPAIGNS         
         B     VSEL6                                                            
*                                                                               
VSEL5    CLC   TBUYPROG(L'WRKPRG),FVIFLD  TEST CHANGE IN PROGRAM                
         BE    VSEL6                                                            
         MVC   TBUYPROG,FVIFLD     YES - CHANGE THE PROGRAM                     
         OI    COMCHG,LPROG              INDICATE PROGRAM CHANGE                
         OI    TBUYFLG1,TBF1PRG          PROGRAM OVERRIDE                       
******   OI    BWDINDS,BWDIPRG           PROGRAM OVERRIDE                       
***************                                                                 
* VALIDATE THE RATING FILED                                                     
***************                                                                 
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
***  2 DECIMAL                                                                  
         TM    APROFBTS,A00TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    VSEL8E                                                           
         CLI   WRKDCP,C'R'                                                      
         BE    *+12                                                             
         CLI   WRKDCP,C'E'                                                      
         BNE   VSEL8E              IT IS IMPRESSION                             
         OI    MISCFLG1,MF1DEM2D   MAIN DEMO CATEGORY NEEDS 2 DECIMALS          
         GOTO1 VCASHVAL,APPARM,(2,FVIFLD)   2 DECIMAL CASHVAL CALL              
         B     VSEL8G                                                           
***  2 DECIMAL                                                                  
VSEL8E   GOTO1 VCASHVAL,APPARM,(1,FVIFLD)                                       
VSEL8G   CLI   APPARM,FF                                                        
         BE    VSEL99                                                           
         MVC   LNEWRTG,APPARM+4    SAVE NEW RATING                              
         DROP  R2                                                               
***  2 DECIMAL                                                                  
         TM    MISCFLG1,MF1DEM2D   WE DID 2 DECIMALS?                           
         BNO   VSEL9                - NOPE                                      
         OI    LNEWRTG,DMODEM2D    PUT ON THE X'40' 2 DECIMAL FLAG              
***  2 DECIMAL                                                                  
*                                                                               
VSEL9    LA    R4,TBUYDEMS         LOOK FOR DEMO LIST                           
         DROP  R4                                                               
*                                                                               
         ST    R4,LADEMEL     NOTE A(DEMO ELEMENT) IS POINTING TO DEMOS         
         LR    R1,R4                                                            
******** OC    INORTG,INORTG       TEST FOR OVERRIDE TARGET DEMO                
******** BZ    VSEL12                                                           
*                                  ** ALWAYS FIND CORRECT DEMO NOW **           
         LA    RE,L'TBUYDEMO       YES - GET OVERRIDE DEMO VALUE                
         LA    RF,L'TBUYDEMS-1(R1)  RF = A(LAST BYTE OF DEMOS)                  
VSEL09A  OC    0(8,R4),0(R4)       ANY DEMO HERE?                               
         BZ    VSEL10              NONE                                         
         CLC   0(3,R4),SVDEMO1                                                  
         BE    VSEL12                                                           
         BXLE  R4,RE,VSEL09A                                                    
         DC    H'0'                DIE IF NOT MORE ROOM                         
*                                                                               
VSEL10   MVC   0(3,R4),SVDEMO1     DEMO NOT FOUND, SO PUT IT IN                 
*                                                                               
VSEL12   MVC   APFULL,4(R4)        SAVE OLD DEMO VALUE IN APFULL                
         NI    APFULL,FF-DMODEMOV                                               
***      NI    APFULL,FF-DMODEM2D   NEED TO KEEP 2 DECIMAL FLAG ON              
         TM    LFLAG,LDEMUP        TEST DEMO UPGRADE REQUIRED                   
         BZ    VSEL28                                                           
         MVC   LDEMS(3),0(R4)      YES - DO THE UPGRADE                         
         MVI   LDEMS+3,FF                                                       
         BAS   RE,DEMUP                                                         
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
         LA    R0,L'TBUYDEMO                                                    
         LA    R1,L'TBUYDEMS-1(R8)                                              
         LA    RE,LDEMFRZ          BUILD LIST OF IMPS TO BE FROZEN              
*                                                                               
VSEL14   TM    4(R8),DMODEMOV                                                   
         BZ    VSEL16                                                           
         CLC   0(3,R8),0(R4)                                                    
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
         BAS   RE,DEMUP            YES - DO THE UPGRADE                         
         BNE   VSELX                                                            
         L     R8,LADEMEL                                                       
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
**       OC    APFULL,APFULL              YES - TEST OLD RATING = 0             
**       BZ    VSEL97                           YES - ERROR                     
****  APWORK+64 WILL BE WIPED OUT IN DEMADJ!!                                   
         BAS   RE,DEMADJ                        NO  - AUTO ADJUST               
****  APWORK+64 WILL BE WIPED OUT IN DEMADJ!!                                   
         BNE   VSEL97              OLD RATING = 0                               
*                                                                               
VSEL30   CLI   APACTN,ACTSKD       TEST FOR SCHEDULE                            
         BNE   VSEL36                                                           
***  2 DECIMAL                                                                  
         TM    APROFBTS,A00TWODC   WE DOING 2 DECIMAL?                          
         BZ    VSEL31               - NOPE                                      
         L     RF,LNEWRTG          YES - CALCULATE NEW ACTUAL POINTS            
         TM    LNEWRTG,X'40'       IS IT ALREADY 2 DECIMAL?                     
         BO    VSEL30G                                                          
         MHI   RF,10               WE'RE GONNA MAKE IT 2 DECIMAL                
         STCM  RF,8,APWORK+12                                                   
         OI    APWORK+12,X'40'                                                  
         ICM   RF,8,APWORK+12                                                   
*                                                                               
VSEL30G  L     R8,APFULL                                                        
         TM    APFULL,X'40'        IS IT ALREADY 2 DECIMAL?                     
         BO    VSEL30J                                                          
         MHI   R8,10                                                            
         STCM  R8,8,APWORK+12                                                   
         OI    APWORK+12,X'40'                                                  
         ICM   R8,8,APWORK+12                                                   
*                                                                               
VSEL30J  SR    RF,R8               WE'RE MAKING IT 2 DECIMAL                    
         XR    RE,RE                                                            
         M     RE,=F'2'            FOR ROUNDING OFF                             
         D     RE,=F'10'           NEED TO MAKE IT 1 DECIMAL                    
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         LR    RE,RF               NEED THE DIFFERENCE IN RE                    
         B     VSEL31G                                                          
***  2 DECIMAL                                                                  
*                                                                               
VSEL31   DS    0H                                                               
         L     RE,LNEWRTG          YES - CALCULATE NEW ACTUAL POINTS            
         S     RE,APFULL                                                        
VSEL31G  LA    R8,TSARREC                                                       
         USING TRECD,R8                                                         
         LA    R8,TBUYSKED                                                      
         DROP  R8                                                               
*                                                                               
         LA    R2,SVACPTS                                                       
         ZIC   RF,CMPNWKS                                                       
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
****     LA    R1,APELEM+DMODEMO-DMOEL                                          
****     CR    R1,R4               TEST ADDING NEW DEMO ELEMENT                 
****     BNE   VSEL40                                                           
****     GOTO1 AADDELS,BWDRECD                                                  
***  2 DECIMAL                                                                  
*****            THIS IS WRONG!!!!                                              
         TM    APROFBTS,A00TWODC   2 DECIMAL PRECISION?                         
         BZ    VSEL40               - NOPE                                      
**       ST    R4,APPARM                                                        
**       GOTO1 AADJPREC,APPARM                                                  
**       L     R4,APPARM                                                        
         CLI   SVDEMO1+1,C'R'      DO WE HAVE A RATING?                         
         BE    *+12                                                             
         CLI   SVDEMO1+1,C'E'      DO WE HAVE AN E-RATING?                      
         BNE   VSEL40               - NOPE, WE HAVE AN IMPRESSION               
         OI    4(R4),DMODEM2D      IT IS DEFINITELY 2 DECIMAL ALREADY           
***  2 DECIMAL                                                                  
*                                                                               
VSEL40   LA    R4,TSARREC                                                       
         USING TRECD,R4                                                         
         NI    LFLAG,FF-LDEMUP                                                  
         L     R2,COMATWAL                                                      
         USING WRKL1H,R2                                                        
         TM    WRKDAYH+FVIIND-FVIHDR,FVIVAL   DAYS                              
         BO    VSEL42                                                           
****     CLI   BWDKELPO,0          DAYS NOT VALID FOR PACKAGE/ORBIT             
****     BNE   VSEL42                                                           
         MVC   APHALF,BDPT         SAVE DPT/SLN                                 
         GOTO1 AVALDAY,WRKDAYH                                                  
         BNE   VSELX                                                            
         MVC   BDPT(2),APHALF      RESTORE DPT/SLN                              
         CLC   TBUYDAYS,BDAYS       TEST CHANGE OF DAYS                         
         BE    VSEL42                                                           
         OI    LFLAG,LDEMUP        YES - UPGRADE REQUIRED                       
         MVC   TBUYDAYS,BDAYS                                                   
         OI    COMCHG,LDAYTIM+LDEMO      ASSUME DEMO CHANGE                     
*                                                                               
VSEL42   TM    WRKTIMH+FVIIND-FVIHDR,FVIVAL   TIMES                             
         BO    VSEL44                                                           
****     CLI   BWDKELPO,0          TIMES NOT VALID FOR PACKAGE/ORBIT            
****     BNE   VSEL44                                                           
         MVC   APHALF,BDPT         SAVE DPT/SLN                                 
         GOTO1 AVALTIM,WRKTIMH                                                  
         BNE   VSELX                                                            
         MVC   BDPT(2),APHALF      RESTORE DPT/SLN                              
         CLC   TBUYTIMS(L'BTIMES),BTIMES     TEST FOR TIMES CHANGE              
         BE    VSEL44                                                           
         OI    LFLAG,LDEMUP        YES - UPGRADE REQUIRED                       
         MVC   TBUYTIMS(L'BTIMES),BTIMES                                        
         OI    COMCHG,LDAYTIM+LDEMO      ASSUME DEMO CHANGE                     
*                                                                               
VSEL44   TM    LFLAG,LDEMUP        TEST UPGRADE REQUIRED                        
         BZ    VSEL70                                                           
*                                                                               
****     TM    BWDINDS,BWDIPRG     YES-TEST FOR PROGRAM OVERRIDE                
****     BO    *+8                                                              
**       BAS   RE,GETPROG          NO-GET PROGRAM NAME                          
*                                                                               
         LA    R8,TBUYDEMS                                                      
         LA    R1,L'TBUYDEMS-1(R8)                                              
         LA    R0,L'TBUYDEMO                                                    
         LA    RE,LDEMFRZ                                                       
         XC    LDEMFRZ,LDEMFRZ                                                  
         TM    CMPOPTS,CAMOAIMP+CAMOAALL+CAMOATGT TEST AUTO-ADJUSTMENTS         
         BZ    VSEL52                                                           
*                                                                               
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
         LA    R8,TBUYDEMS                                                      
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
         BAS   RE,DEMUP            YES - DO THE UPGRADES                        
         BNE   VSELX                                                            
         LA    R8,TBUYDEMS         MOVE DEMO VALUES INTO DEMO ELEMENT           
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
VSEL70   MVC   LNEWCOST,TBUYCOST   DEFAULT WILL BE WHAT IS IN TSAR              
         TM    WRKCSTH+FVIIND-FVIHDR,FVIVAL   COST                              
         BO    VSEL85                                                           
         XC    LNEWCOST,LNEWCOST                                                
         GOTO1 AFVAL,WRKCSTH                                                    
         BH    VSELX                                                            
         BL    VSEL73                                                           
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
VSEL73   CLC   TBUYCOST,LNEWCOST    TEST CHANGE IN COST                         
         BNE   VSEL74                                                           
         CLI   APRECNUM,RECSID     NO-TEST NSID RECORD                          
         BE    VSEL82              YES-FORCE COST CHANGE FOR TRANSFER           
         B     VSEL85                                                           
*                                                                               
VSEL74   CLI   APACTN,ACTSKD       YES - TEST FOR SCHEDULE                      
         BNE   VSEL80                                                           
         L     R1,LNEWCOST               YES - CALCULATE NEW COST               
         ICM   R0,15,TBUYCOST                                                   
         SR    R1,R0                                                            
         ST    R1,APFULL           APFULL = DIFFERENCE IN COST                  
         LA    R8,TBUYSKED                                                      
         LA    R4,L'TBUYSKED(R8)                                                
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
VSEL80   LA    R4,TSARREC                                                       
*******  MVC   TBUYCOST,LNEWCOST                                                
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
         DROP  R3,R4                                                            
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
*&&DO                                                                           
VSLN6    CLI   0(R1),0                                                          
         BE    VSLN9                                                            
         CLC   0(1,R1),3(RE)                                                    
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     VSLN6                                                            
*&&                                                                             
VSLN6    DS    0H                                                               
         L     R1,VSLNTAB          POINT TO SLNTAB                              
         LH    RE,0(R1)            GET ENTRY LENGTH                             
         L     RF,2(R1)            GET DSPL TO EOT                              
         AR    RF,R1               RF POINTS TO EOT                             
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         SR    R0,R0                                                            
         LA    R0,C'T'                                                          
         CLI   QMED,C'T'                                                        
         BE    VSLN6C                                                           
         CLI   QMED,C'N'                                                        
         BE    VSLN6C                                                           
         CLI   QMED,C'C'                                                        
         BE    VSLN6C                                                           
         CLI   QMED,C'R'                                                        
         BE    VSLN6C                                                           
         CLI   QMED,C'X'                                                        
         BE    VSLN6C                                                           
         DC    H'0'                                                             
*                                                                               
VSLN6C   CLC   =C'00',0(R1)        FIND DEFAULT ENTRY                           
         BE    VSLN6G                                                           
         CLC   CUAALF,0(R1)        MATCH AGY ALPHA                              
         BNE   *+14                                                             
VSLN6G   CLM   R0,1,2(R1)          MATCH MEDIA                                  
         BE    VSLN6K                                                           
*                                                                               
         BXLE  R1,RE,VSLN6C        NEXT ENTRY                                   
         DC    H'0'                                                             
*                                                                               
VSLN6K   AHI   R1,4                POINT BEYOND TABLE ID                        
         SR    RE,RE                                                            
         IC    RE,3(R3)            GET SLN                                      
         AR    RE,RE               X 2                                          
         AR    RE,R1               POINT TO ENTRY                               
         CLI   1(RE),0             TEST SLN VALID                               
         BE    VSLN9                                                            
*                                                                               
         MVC   0(1,R4),3(R3)                                                    
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
       ++INCLUDE SPSLNTAB                                                       
         DC    AL1(0)                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE PROGRAM FIELD FOR 'CAMPAIGNS='                             *         
* OUTPUT : COMCOLNS=TABLE OF 3-BYTE ENTRIES: CAMP-MKT SEQ NO (2)      *         
*                                            STATION CODE (1)         *         
***********************************************************************         
         SPACE 1                                                                
VALCAMPS NTR1  ,                                                                
         LA    R4,TSARREC                                                       
         USING TRECD,R4                                                         
         XC    APWORK,APWORK                                                    
         MVC   APWORK+L'BMKT(L'BSTA),TBUYSTA                                    
         GOTO1 VMSUNPK,APPARM,(X'80',APWORK),QMKT,APWORK+8                      
*                                                                               
         MVC   APWORK(8),APWORK+8  SAVE THE STATION IN APWORK                   
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
****     CLI   BWDKELPO-BWDKEY(R1),0                                            
****     BNE   VCAM96                                                           
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
         OC    CAMKAGMD,BBYRMASK                                                
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
         OC    BWHKAGMD,BBYRMASK                                                
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
         DROP  R1,R4,R5                                                         
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
VALSEL2  L     R2,AIOAREA2                                                      
         USING BUYKEY,R2                                                        
*                                                                               
         TM    COMCHG,LDET         TEST FOR DETAIL CHANGE                       
         BZ    VASL10                                                           
         NI    MISCFLG1,X'FF'-MF1ADDRC                                          
         LA    R4,TSARREC                                                       
         USING TRECD,R4                                                         
         MVC   APWORK(L'IOKEY),IOKEY   SAVE COPY OF BUY KEY WE'RE ON            
         XC    IOKEY,IOKEY                                                      
         LA    R3,IOKEY                                                         
         USING NBRKEY,R3           LOOK FOR NWS BUY REVISION RECORD             
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ      BINARY CAMPAIGN/MARKET SEQ #                 
         MVC   NBRKSTA,TBUYSTA                                                  
         MVC   NBRKKBUY,TBUYBUYS                                                
         MVC   NBRKNBSQ,TBUYSEQ                                                 
*                                                                               
         GOTO1 AIO,DIRHIU+IO3      READ FOR DELETE/UPDATE                       
         BE    VASL00A             FOUND A NON-DELETED RECORD                   
         CLI   IOERR,IOEDEL                                                     
         BNE   VASL00B             NEED TO ADD A BUY REVISION REC               
VASL00A  CLC   NBRKEY,IOKEYSAV     FOUND OUR BUY REVISION RECORD?               
         BNE   VASL00B                                                          
         MVC   IODA,NBRKDA                                                      
         TM    NBRKCNTL,X'80'      DELETED KEY?                                 
         BZ    VASL01                                                           
         NI    NBRKCNTL,X'FF'-X'80'   NOT DELETED ANYMORE                       
         GOTO1 AIO,DIRWRT                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         B     VASL01              NO                                           
***********************************                                             
* THIS WILL ADD THE BUY REVISION RECORD                                         
***********************************                                             
VASL00B  OI    MISCFLG1,MF1ADDRC   NEED TO ADD THE RECORD                       
*&&DO                                                                           
VASL00C  GOTO1 =A(SETUPNBR),RR=APRELO                                           
*&&                                                                             
VASL00C  GOTO1 ASTUPNBR,APPARM,AIOAREA2,AIOAREA3,DBLOCK                         
         B     VASL01A                                                          
***********************************                                             
* THIS WILL CHANGE OUR BUY REVISION RECORD                                      
***********************************                                             
VASL01   GOTO1 AIO,FILGETU3+IORDEL                                              
         BE    VASL01A                                                          
         CLI   IOERR,X'02'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
******   GOTO1 =A(SETUPNBR),RR=APRELO                                           
         CLC   0(2,R2),=X'0D6B'    DO WE HAVE BUY REVISION IN AIO2?             
         BNH   VASL01A              - YUP, SKIP SETUPNBR CALL                   
         GOTO1 ASTUPNBR,APPARM,AIOAREA2,AIOAREA3,DBLOCK                         
*                                                                               
VASL01A  L     R3,AIOAREA3         R3=A(RECORD)                                 
         NI    NBRCNTL,X'FF'-X'80'   TAKE OFF DELETED BIT IF ANY                
         LA    R1,NBRFSTEL                                                      
VASL02   CLI   0(R1),0                                                          
         BE    VASL10                                                           
         CLI   0(R1),NBRSELQ       SAVED DATA ELEMENT?                          
         BE    VASL04                                                           
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VASL02                                                           
*                                                                               
         USING NBRSELD,R1                                                       
VASL04   CLI   NBRSLN,NBRSLNQ                                                   
         BH    VASL00C                                                          
*                                                                               
         MVC   NBRSDAYS,TBUYDAYS                                                
         MVC   NBRSTIMS(L'NBRSTIMS*2),TBUYTIMS                                  
         MVC   NBRSDYPT,TBUYDYPT                                                
         MVC   NBRSSBDP,TBUYSDPT                                                
         MVC   NBRSSLN,TBUYSLN                                                  
*                                                                               
         TM    TBUYKIND,X'03'      MANUAL BUY REVISION WITH EFF COSTS?          
         BNZ   VASL06              YES                                          
         CLI   TBUYKCOV,0          NO, COST OVERRIDES?                          
         BNE   VASL05                  YES                                      
         MVC   TBUYCOST,LNEWCOST                                                
         MVC   NBRSCST1,TBUYCOST                                                
         B     VASL07                                                           
*                                                                               
VASL05   BAS   RE,CHGOLCST         CHANGE THE OLD COSTS                         
         MVC   TBUYCOST,LNEWCOST                                                
         B     VASL07                                                           
*                                                                               
VASL06   TM    TBUYKIND,X'01'                                                   
         BZ    *+14                                                             
         MVC   NBRSCST2,TBUYCOST                                                
         B     VASL07                                                           
         MVC   NBRSCST3,TBUYCOST                                                
*                                                                               
VASL07   MVC   NBRSPROG,TBUYPROG                                                
         MVC   NBRSADJC,TBUYADJC                                                
         TM    TBUYFLG1,TBF1PRG    PROGRAMMING OVERRIDE?                        
         BZ    VASL10                                                           
         OI    NBRSINDS,NBRSIPRG    - YUP                                       
         DROP  R1                                                               
*                                                                               
VASL10   MVI   APELEM,NBRDMELQ     DEMO ELEMENT                                 
         GOTO1 ADELELS,NBRRECD                                                  
*                                                                               
         LA    R8,APELEM                                                        
         XC    APELEM,APELEM                                                    
         USING NBRDMELD,R8                                                      
         MVI   NBRDMEL,NBRDMELQ                                                 
         LA    RE,TBUYDEMO                                                      
         LA    RF,NBRDMDMO                                                      
         LA    R1,ESTDEMS          MAX OF 8 DEMOS                               
         LA    R0,TBUYDEMS+L'TBUYDEMS                                           
*                                                                               
VASL12   OC    0(3,R1),0(R1)       ANY MORE DEMOS?                              
         BZ    VASL10X                                                          
         MVC   0(L'NBRDMDMO,RF),0(RE)                                           
         LA    RF,L'NBRDMDMO(RF)                                                
         LA    RE,L'TBUYDEMO(RE)                                                
         CR    RE,R0               CAN WE FIT THIS MANY DEMOS?                  
         BNL   VASL10X             NO                                           
         LA    R1,3(R1)                                                         
         B     VASL12                                                           
*                                                                               
VASL10X  SR    RF,R8                                                            
         STC   RF,NBRDMLEN                                                      
         DROP  R8                                                               
*                                                                               
         GOTO1 AADDELS,NBRRECD                                                  
*                                                                               
         LA    RE,NBRFSTEL                                                      
         XC    APELEM,APELEM                                                    
         XR    R0,R0                                                            
VASL20   CLI   0(RE),0                                                          
         BE    VASL30                                                           
         CLI   0(RE),NBRSPELQ                                                   
         BNE   VASL25                                                           
         USING NBRSPELD,RE                                                      
         XR    R1,R1                                                            
         IC    R1,NBRSPLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(RE)     COPY ELEMENT SO WE DON'T LOSE INFO           
         DROP  RE                                                               
         GOTO1 ADELELS,NBRRECD                                                  
         B     VASL30                                                           
*                                                                               
VASL25   IC    R0,1(RE)            BUMP TO THE NEXT ELEMENT                     
         AR    RE,R0                                                            
         B     VASL20                                                           
***************                                                                 
* MODIFY THE SPOTS PER WEEK WATCHING OUT FOR EFFECTIVE OR OVERRIDES             
***************                                                                 
**SL30   XC    APELEM,APELEM                                                    
VASL30   LA    R1,APELEM                                                        
         USING NBRSPELD,R1                                                      
         CLI   NBRSPLEN,0                                                       
         BNE   VASL35                                                           
         XC    APELEM,APELEM       MOVED FROM VASL30, WHY?                      
         MVI   NBRSPEL,NBRSPELQ                                                 
         XR    RE,RE                                                            
         IC    RE,CMPNWKS                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   NBRSPSPW(0),TBUYSKED                                             
         AHI   RE,NBRSPSPW-NBRSPELD+1                                           
         STC   RE,NBRSPLEN                                                      
         B     VASL60                                                           
*                                                                               
VASL35   XR    RE,RE               MODFIY ONLY THOSE WEEKS THAT ARE             
         IC    RE,CMPNWKS              VALID WEEKS                              
         XR    R8,R8                                                            
         ICM   R8,1,INOSTDTD       STD'S  YO!                                   
         BNZ   *+8                                                              
         IC    R8,CMPDSTDT         MORE STD'S   YO!                             
         SR    RE,R8               REAL NUMBER OF WEEKS ON SCREEN               
         LA    RF,TBUYSKED                                                      
         LA    RF,0(R8,RF)         RF NOW POINTS TO THE RIGHT WEEK              
         LA    R5,APRECKEY+20                                                   
         LA    R1,NBRSPSPW                                                      
         LA    R1,0(R8,R1)         R1 NOW POINTS TO THE RIGHT WEEK              
***      LR    R8,R5                                                            
         L     R8,ATWA             R5 IS NOT POINTING TO ATWA                   
         AHI   R8,CMPDATSP-TWAD                                                 
         DROP  R1                                                               
VASL35A  TM    0(R5),X'C0'         INVALID WEEK?                                
         BNZ   VASL35Z             YES, NEXT WEEK                               
*                                                                               
         TM    TSARREC+TBUYKIND-TRECD,X'03'     ONE OF THE EFF COSTS?           
         BZ    *+14                                                             
         MVC   0(1,R1),0(RF)       YES, NO OTHER EFF COSTS THIS WK              
         B     VASL35Z                  CHECK NEXT WEEK                         
***********************************                                             
* NUMBER IN 0(RF),0(R5),0(R1) CAN BE A MIX OF BUY/OVERRIDE COSTS                
***********************************                                             
         DROP  R4                  SO WE HAVE A REGISTER TO USE                 
         LA    R4,NBRFSTEL         FIGURE OUT THE NUMBER FOR THIS COST          
         XR    R0,R0                                                            
VASL35B  CLI   0(R4),0                                                          
         BE    VASL35D             NO OVERRIDES FOR THIS WEEK                   
*                                                                               
         CLI   0(R4),NBRCOELQ                                                   
         BL    VASL35C                                                          
         BH    VASL35D             NO OVERRIDES FOR THIS WEEK                   
         USING NBRCOELD,R4                                                      
         CLC   NBRCODAT,0(R8)                                                   
         BL    VASL35C                                                          
         CLC   NBRCODAT,2(R8)                                                   
         BNH   VASL35O             WE HAVE AN OVERRIDE FOR THIS WEEK            
*                                                                               
VASL35C  IC    R0,1(R4)            NEXT OVERRIDE ELEM                           
         AR    R4,R0                                                            
         B     VASL35B                                                          
***************                                                                 
* NO OVERRIDES FOR THIS WEEK                                                    
***************                                                                 
VASL35D  CLI   TSARREC+TBUYKCOV-TRECD,0  IS THIS AN OVERRIDE COST?              
         BNE   VASLTMP             YES                                          
         MVC   0(1,R1),0(RF)       NO, STRAIGHT BUY COST                        
         B     VASL35Z                                                          
*********                                                                       
* BUT WE'RE ON A OVERRIDE LINE                                                  
*********                                                                       
VASLTMP  CLI   0(RF),0             ANY OVERRIDE COST SPOTS?                     
         BE    VASL35Z             NONE, NEXT WEEK                              
         STM   RE,RF,APWORK        YES, WE NEED SPOTS NOW                       
         BAS   RE,NBROVRDE         ADD OVERRIDE COST W/ DATE OF WEEK            
         LM    RE,RF,APWORK                                                     
         B     VASL35Z                                                          
***************                                                                 
* WE HAVE AN OVERRIDE FOR THIS WEEK, COULD BE ON A REGULAR COST LINE            
***************                                                                 
VASL35O  STM   RE,RF,APWORK                                                     
         BAS   RE,NBROVRDE           THE NUMBER IN APRECKEY+20 (R5)             
         LM    RE,RF,APWORK                                                     
*                                                                               
VASL35Z  LA    RF,1(RF)                                                         
         LA    R5,1(R5)                                                         
         LA    R1,1(R1)                                                         
         LA    R8,4(R8)                                                         
         STC   RE,APBYTE                                                        
         BCT   RE,VASL35A                                                       
*                                                                               
         XR    RE,RE               NEW LENGTH AS THE NUMBER OF WKS              
         IC    RE,CMPNWKS              CAN CHANGE FROM ORIGINAL                 
         AHI   RE,NBRSPSPW-NBRSPELD                                             
         LA    R1,APELEM                                                        
         USING NBRSPELD,R1                                                      
         STC   RE,NBRSPLEN                                                      
         DROP  R1                                                               
*                                                                               
VASL60   GOTO1 AADDELS,NBRRECD                                                  
         LA    R4,TSARREC                                                       
         USING TRECD,R4                                                         
         LA    R0,NMAXWKS          BUILD SCHEDULE LINE IN APRECKEY+20           
         LA    RE,APRECKEY+20                                                   
         LA    RF,TBUYSKED                                                      
VASL65   NI    0(RE),X'C0'         CLEAR # OF SPOTS W/O CLEARING BITS           
         OC    0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,VASL65                                                        
*                                                                               
****  NEWLY ADDED CREATION DATE AND CHANGE DATE ON BUY REVISION!                
****                        MHC  02/15/05                                       
VASL70   DS    0H                                                               
         L     R3,AIOAREA3                                                      
***  R3 ALREADY HAVE A USING OF NBRKEY                                          
***      USING NBRKEY,R3                                                        
         LA    R2,NBRFSTEL                                                      
         USING NBRSELD,R2                                                       
*****  ALL SETUP TO ADD THE CREATION/CHANGE DATE VIA DATCON                     
*                                                                               
         TM    MISCFLG1,MF1ADDRC                                                
         BZ    VASL70PT                                                         
****  ADDING THE CREATION DATE                                                  
         GOTO1 VDATCON,APPARM,(5,0),(3,NBRSCRDT)   CREATION DATE                
         TM    CMPOPTS,CAMODLY     TEST DAILY SKED                              
         BZ    VASL70E              - NOPE, NOT DAILY                           
         OI    NBRSINDS,NBRSDALY   IT'S A DAILY CAMPAIGN                        
*                                                                               
VASL70E  GOTO1 AIO,FILADD3                                                      
         BE    VASL71                                                           
         DC    H'0'                                                             
*                                                                               
VASL70PT DS    0H                                                               
****  CHANGING THE CHANGE DATE                                                  
         GOTO1 VDATCON,APPARM,(5,0),(3,NBRSCHDT)   CHANGE DATE                  
         DROP  R2                                                               
******  WE NEED TO CHECK IF IOKEY HAS SAME KEY AS IN AIOAREA3!!!                
         L     R1,AIOAREA3                                                      
         CLC   IOKEY(13),0(R1)     WE NEED TO CHECK ALL 13 BYTES!!              
         BE    *+6                                                              
         DC    H'0'                THEY'RE NOT EQUAL!!  DIE!!!                  
******                                      MHC  07/24/03                       
         GOTO1 AIO,FILPUT3                                                      
         BE    VASL71                                                           
         DC    H'0'                                                             
*                                                                               
VASL71   TM    MISCFLG1,MF1BUYRV   THE CAMP/MKT DOING BUY REVISIONS?            
         BNZ   VASL80              YES, ITS DOING THEM ALREADY                  
         LA    R2,IOKEY            BUILD HEADER POINTER TO FIND                 
         USING BWHKEY,R2                                                        
         XC    BWHKEY,BWHKEY                                                    
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVC   BWHKAGMD,BAGYMD                                                  
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         MVC   BWHKCAM,BCAM                                                     
         MVC   BWHKMKT,BMKT                                                     
         DROP  R2                                                               
*                                                                               
         GOTO1 AIO,DIRHI+IO2                                                    
         CLC   IOKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,FILGETU2                                                     
         L     R2,AIOAREA2                                                      
         LA    R2,BWHFSTEL-BWHKEY(R2)                                           
         XR    R0,R0                                                            
VASL72   CLI   0(R2),0             LOOK FOR INFO ELEMENT (X'06')                
         BE    VASL76              NEED TO ADD ONE THEN                         
         CLI   0(R2),INFELCDQ                                                   
         BE    VASL74                                                           
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     VASL72                                                           
*                                                                               
         USING INFELD,R2                                                        
VASL74   TM    INFFLAG1,IFF1BYRV   ALREADY DOING REVISIONS?                     
         BNZ   VASL79              YES                                          
         OI    INFFLAG1,IFF1BYRV                                                
         B     VASL78                                                           
         DROP  R2                                                               
*                                                                               
VASL76   LA    R1,APELEM           SAVE ADDRESS WHERE TO PUT INFO ELEM          
         XC    APELEM,APELEM                                                    
         USING INFELD,R1                                                        
         MVI   INFELCD,INFELCDQ                                                 
         MVI   INFELLN,INFELLNQ                                                 
         OI    INFFLAG1,IFF1BYRV                                                
         DROP  R1                                                               
*                                                                               
         L     R2,AIOAREA2                                                      
         GOTO1 AADDELS,(R2)                                                     
*                                                                               
VASL78   LA    R1,FILPUT2          PUT NWH RECORD                               
         GOTO1 AIO                                                              
VASL79   OI    MISCFLG1,MF1BUYRV                                                
********                                                                        
*  NEED TO UPDATE THE TSAR RECORD ALSO                                          
********                                                                        
VASL80   MVI   TSARACT,TSAWRT                                                   
         GOTO1 ATSAR,TREC                                                       
         BE    VASL100                                                          
         MVC   FVADDR,APPARM                                                    
*****    B     VASLX     <=== CAUSING REVISION TO OVERWRITE HEADER REC?         
*                                                                               
VASL100  DS    0H                                                               
         MVC   IOKEY(13),APRECKEY                                               
         LA    R1,DIRRD+IO2                                                     
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,FILGET2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VASLX    B     COMMONX                                                          
         DROP  R3,R4                                                            
***********************************************************************         
* THIS ROUTINE CHANGES THE OLD OVERRIDE COSTS TO THE NEW COST                   
***********************************************************************         
CHGOLCST NTR1                                                                   
         L     R3,AIOAREA3         R3=A(RECORD)                                 
         USING NBRKEY,R3                                                        
         LA    R4,TSARREC                                                       
         USING TRECD,R4                                                         
         LA    R1,NBRFSTEL                                                      
         XR    R0,R0                                                            
COLC10   CLI   0(R1),0                                                          
         BE    COLCX                                                            
         CLI   0(R1),NBRCOELQ                                                   
         BE    COLC20                                                           
COLC15   IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     COLC10                                                           
*                                                                               
         USING NBRCOELD,R1                                                      
COLC20   CLC   NBRCOCST,TBUYCOST                                                
         BNE   COLC15                                                           
         MVC   NBRCOCST,LNEWCOST                                                
         B     COLC15                                                           
*                                                                               
COLCX    B     COMMONX                                                          
         DROP  R1,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* ADD/DELETE COST OVERRIDE ELEMENT(S) TO/FROM BUY REVISION RECORD               
*                                                                               
* ON ENTRY: AIOAREA3           A(BUY REVISION RECORD)                           
*           APWORK(4)          NUMBER OF WEEKS LEFT TO PROCESS                  
*           APWORK+4(4)        A(# SPOTS FOR WK IN THIS LINE)                   
*           R1                 A(# SPOTS FOR WK IN ENTIRE RECORD)               
*           R4                 A(1ST OVERRIDE IN RECORD FOR WEEK)               
*           R5                 A(# SPOTS FOR WK IN APRECKEY+20+X)               
*           R8                 A(START AND END DATE OF THE WEEK)                
*                                                                               
* ***WARNING***                                                                 
* DO NOT TOUCH APWORK BECAUSE THESE VALUES WILL BE NEED AFTER RETURNING         
* TO THE CALLER                                                                 
***********************************************************************         
NBROVRDE NTR1                                                                   
         LM    RE,RF,APWORK                                                     
         XR    R3,R3                                                            
         IC    R3,0(RF)            # SPOTS USER WANTS FOR THIS OVERRIDE         
         XR    R0,R0                                                            
         IC    R0,0(R5)            # SPOTS PREVIOUSLY FOR THIS OVERRIDE         
         SR    R3,R0               ADD OR DELETE?                               
         BZ    NOVRX               NEITHER, SAME NUMBER, NOTHING TO DO          
         BM    NOVR50              DELETE SOME OVERRIDES                        
***************                                                                 
* NEED TO ADD A NUMBER OF SPOTS/OVERRIDE ELEMENTS FOR THIS WEEK                 
*                                                                               
*        (R3)  NUMBER TO ADD                                                    
***************                                                                 
         IC    R0,0(R1)            UPDATE NUMBER OF SPOTS TO FREE UP            
         AR    R0,R3                  R1                                        
         STC   R0,0(R1)                                                         
*                                                                               
         CLI   TSARREC+TBUYKCOV-TRECD,0  AN OVERRIDE COST?                      
         BE    NOVRX                     NO, LEAVE NOW                          
*                                                                               
         LA    R1,APELEM+128       NBRSPEL CAN'T EVER BE THIS LONG              
         USING NBRCOELD,R1                                                      
         MVI   NBRCOEL,NBRCOELQ                                                 
         MVI   NBRCOLEN,NBRCOLNQ                                                
         MVC   NBRCODAT,0(R8)      USE BEGINNING OF THE WEEK FOR NOW            
         MVC   NBRCOCST(4),TSARREC+TBUYCOST-TRECD                               
         DROP  R1                                                               
*                                                                               
         L     R4,AIOAREA3                                                      
         LA    R4,NBRFSTEL-NBRKEY(R4)                                           
         USING NBRCOELD,R4                                                      
NOVR10   CLI   0(R4),0             HIT END OF RECORD?                           
         BE    NOVR40                                                           
         CLI   NBRCOEL,NBRCOELQ       OR GREATER THAN OVERRIDE ELEM?            
         BH    NOVR40              THEN ADD IT STARTING HERE                    
         CLC   NBRCODAT,0(R8)                                                   
         BH    NOVR40                                                           
         BE    NOVR30                                                           
NOVR20   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     NOVR10                                                           
*                                                                               
NOVR30   CLC   NBRCOCST,TSARREC+TBUYCOST-TRECD                                  
         BL    NOVR20                                                           
         DROP  R4                                                               
*                                                                               
NOVR40   GOTO1 VRECUP,APPARM,(X'00',AIOAREA3),APELEM+128,(R4)                   
         BCT   R3,NOVR40                                                        
*                                                                               
         B     NOVRX                                                            
***************                                                                 
* NEED TO DELETE A NUMBER OF OVERRIDE ELEMENTS FOR THIS WEEK                    
*                                                                               
*        (R3)  NUMBER TO DELETE                                                 
***************                                                                 
NOVR50   IC    R0,0(R1)            UPDATE NUMBER OF SPOTS TO FREE UP            
         AR    R0,R3                  R1                                        
         STC   R0,0(R1)                                                         
*                                                                               
         CLI   TSARREC+TBUYKCOV-TRECD,0  AN OVERRIDE COST?                      
         BE    NOVRX                     NO, LEAVE NOW                          
*                                                                               
         LPR   R3,R3               GET POSITIVE NUMBER                          
         XR    R0,R0                                                            
         USING NBRCOELD,R4                                                      
NOVR55   CLI   0(R4),0             HIT END OF RECORD?                           
         BE    NOVRX                                                            
         CLI   NBRCOEL,NBRCOELQ       OR GREATER THAN OVERRIDE ELEM?            
         BH    NOVRX               THEN EXIT                                    
         CLC   NBRCODAT,0(R8)                                                   
         BL    NOVR60                                                           
         CLC   NBRCODAT,2(R8)                                                   
         BNH   NOVR65                                                           
NOVR60   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     NOVR55                                                           
*                                                                               
NOVR65   CLC   NBRCOCST,TSARREC+TBUYCOST-TRECD                                  
         BNE   NOVR60                                                           
         DROP  R4                                                               
*                                                                               
         GOTO1 VRECUP,APPARM,(X'00',AIOAREA3),(R4),(R4)                         
         BCT   R3,NOVR55                                                        
*                                                                               
NOVRX    B     COMXIT                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SCHEDULE AND BUILD SPOTS PER WEEK ELEMENT                 
* INPUT  : P1 A(TWA SCHEDULE LINE)                                              
*          P2 A(TWA ACTUAL POINTS/DOLLARS LINE)                                 
*          IOAREA2 CONTAINS BUY RECORD                                          
*          IOAREA3 CONTAINS NBR RECORD                                          
***********************************************************************         
         SPACE 1                                                                
VALSKD   L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
         LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
         LA    R4,TSARREC                                                       
         USING TRECD,R4                                                         
*                                                                               
         MVC   LAACLINE,4(R1)      SAVE A(ACHIEVED LINE)                        
         MVI   LFLAG,0                                                          
         L     R1,0(R1)                                                         
         ST    R1,LASKDLN          SAVE A(SKED LINE)                            
*                                                                               
         GOTO1 AFVAL               VALIDATE SCHEDULE                            
         BH    VSKDX                                                            
         BL    VSKD90              NOTHING - EXIT                               
         XC    LNOSKED,LNOSKED                                                  
*                                                                               
         OC    NBRSWKS,NBRSWKS     TEST ANY INACTIVE WEEKS                      
         BNZ   *+14                                                             
         OC    NBRSWKS2,NBRSWKS2                                                
         BZ    VSKD2                                                            
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    R2,CMPDSTDT                                                      
*                                                                               
         ICM   R0,12,NBRSWKS                                                    
         ICM   R0,3,NBRSWKS2                                                    
         ICM   R1,14,NBRSWKS2+2                                                 
         SLDL  R0,0(R2)            SHIFT TO THE START DATE OPTION               
         STM   R0,R1,APDUB                                                      
         SR    R1,R1                                                            
         ICM   R1,12,APDUB                                                      
*                                                                               
         LA    RE,LNOSKED                                                       
         LA    RF,NMAXWKS                                                       
*                                                                               
VSKD1M   SR    R0,R0               YES-SET INACTIVE WEEK INDICATORS             
         SLDL  R0,1                                                             
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         MVI   0(RE),1                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,VSKD1M                                                        
*                                                                               
VSKD2    LA    RE,APRECKEY+20                                                   
         MVC   LSPW,EFFS        BUILD SPOTS PER WEEK                            
         LA    R0,NMAXWKS       X'FF' = NO SPOTS THIS WEEK                      
         LA    RF,LSPW          X'7F' = NO SPOTS & OUTSIDE OF EFFECTIVE         
         LA    R1,LNOSKED               DATE RANGE                              
*                                                                               
VSKD3    TM    0(RE),X'C0'                                                      
         BNZ   *+12                                                             
         CLI   0(R1),0             TEST INACTIVE WEEK                           
         BE    *+8                                                              
         NI    0(RF),FF-X'80'                                                   
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,VSKD3                                                         
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
         TRT   FVIFLD(56),APELEM                                                
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
         BAS   RE,VSKDPOS          INITIALIZE SPOTS PER WEEK PRIOR              
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
         SR    R0,R0                                                            
         ICM   R0,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    R0,CMPDSTDT                                                      
         AR    RE,R0                                                            
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
         BAS   RE,VSKDPOS                                                       
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
         BAS   RE,VSKDPOS          INITIALIZE SPOTS PER WEEK PRIOR              
         CVB   R1,APDUB            R1 = SPOTS PER WEEK                          
         CHI   R1,64               CHECK NOT TOO MANY                           
         BNL   VSKD99                                                           
         TM    CMPOPTS,CAMODLY     DAILY LIMIT IS 9                             
         BZ    *+12                                                             
         CHI   R1,9                                                             
         BH    VSKD99                                                           
*                                                                               
         LA    R3,LSPW+NMAXWKS-1   GET READY FOR BXLE                           
         LR    RE,RF                                                            
         LA    R0,LSPW                                                          
         SR    RE,R0                                                            
         SR    R0,R0                                                            
         ICM   R0,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    R0,CMPDSTDT                                                      
         AR    RE,R0                                                            
         SLL   RE,2                                                             
         LA    RE,SVGLPTS(RE)                                                   
*                                                                               
******** TM    CMPOPTS,CAMODLY     TEST DAILY SKED                              
******** BO    VSKD18A                                                          
VSKD18   OC    SVGLPTST,SVGLPTST   NO-TEST FOR ANY GOAL POINTS                  
         BZ    VSKD18A                                                          
         TM    INOIND,INOINOG      IS IT NFG OPTION?                            
         BO    VSKD18A              - YUP, DON'T NEED GOAL POINTS               
         OC    0(4,RE),0(RE)       THERE ARE GOAL POINTS - TEST GOALS           
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
VSKD20   L     R3,AIOAREA3                                                      
*                                                                               
         ZIC   R0,CMPNWKS          DISPLAY SCHEDULE IN FVIFLD                   
         SR    R1,R1                                                            
         ICM   R1,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    R1,CMPDSTDT                                                      
         SR    R0,R1                                                            
*                                                                               
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
         LA    R4,TSARREC                                                       
         USING TRECD,R4                                                         
         LA    R2,APRECKEY+20      R2 = A(CURRENT SPOTS LINE)                   
*                                                                               
         SR    RE,RE               FROM THE START DATE IF ANY                   
         ICM   RE,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    RE,CMPDSTDT                                                      
         ZIC   R0,CMPNWKS          LOOK FOR CHANGES                             
         SR    R0,RE                    FROM THE START DATE                     
*                                                                               
         LA    R8,TBUYSKED                                                      
         AR    R8,RE                                                            
*                                                                               
         LA    RE,FVIFLD-1         BEGIN WITH FIRST WEEK                        
         NI    LFLAG,255-LINWKCHG                                               
         LA    R5,LNOSKED                                                       
*                                                                               
VSKD35   CLI   0(R2),FF            TEST END OF PERIOD                           
         BE    VSKD44                                                           
         LA    R1,APRECKEY+20+14       OR END OF FIELD                          
         CR    R2,R1                                                            
         BNL   VSKD44                                                           
*                                                                               
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
         OC    NBRSDTES,NBRSDTES   YES-TEST RECORD DATES SET                    
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
         CHI   R1,64              YES-TEST N'SPOTS NOT TOO HIGH                 
         BNL   VSKD99                                                           
         TM    CMPOPTS,CAMODLY     DAILY LIMIT IS 9                             
         BZ    VSKD40                                                           
         CHI   R1,9                                                             
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
         LA    RF,14               MAX # OF WEEKS THAT CAN BE DISPLAYED         
*                                                                               
VSKD45   CLI   0(RE),0                                                          
         BE    *+6                                                              
         LR    R1,R8                                                            
         SLDL  R0,1                                                             
         LA    RE,1(RE)                                                         
         BCT   RF,VSKD45                                                        
*                                                                               
         SLDL  R0,2                SHIFT 2 MORE BITS FOR ALIGNMENT              
*                                                                               
VSKD45A  CLI   CMPNWKS,14          DO WE NEED TO WORRY ABOUT NBRSWKS2?          
         BH    *+12                                                             
         STCM  R0,3,NBRSWKS                                                     
         B     VSKD46                                                           
***********************************                                             
* BIT-WISE MANIPULATION TO STORE 14 BITS INTO SOMEWHERE OF 7 BYTES              
* BEGIN                                                                         
***********************************                                             
         STCM  R0,3,APWORK+8       YES, A LITTLE MORE COMPLICATED               
*                                                                               
         XC    APDUB,APDUB         MAKE A COPY OF WHAT THE MASK WAS B4          
         MVC   APDUB(2),NBRSWKS                                                 
         MVC   APDUB+2(5),NBRSWKS2                                              
         LM    R0,R1,APDUB                                                      
*                                                                               
         XR    RF,RF               WE'RE GOING TO CLEAR THE 14 BITS             
         ICM   RF,1,INOSTDTD         WITH AN NC INSTRUCTION TO PREPARE          
         BNZ   *+8                   FOR THE OC INSTRUCTION                     
         IC    RF,CMPDSTDT                                                      
         AHI   RF,14                                                            
         SLDL  R0,0(RF)                                                         
         SHI   RF,14               SO RF IS THE DISPLACEMENT AGAIN              
*                                                                               
         SRDL  R0,14+1             SHIFTING 1 MORE SO                           
         MVI   APBYTE,X'80'          I CAN PUT 1'S LEFT OF THE                  
         ICM   R0,8,APBYTE              14 BITS FOR SRDA INSTR                  
         SRDA  R0,0(RF)                                                         
         SLDL  R0,1                HIGH ORDER BIT NOT NEEDED ANYMORE            
         STM   R0,R1,APWORK                                                     
         NC    APDUB(7),APWORK     NOW THE 14 BITS ARE ALL 0'S                  
*                                                                               
         LM    R0,R1,APDUB                                                      
         SLDL  R0,0(RF)            BACK TO THIS AGAIN                           
         STCM  R0,12,APWORK                                                     
         OC    APWORK(2),APWORK+8  BASICALLY MOVING IN THE 14 BITS              
         ICM   R0,12,APWORK                                                     
         SRDL  R0,0(RF)                                                         
         STM   R0,R1,APWORK                                                     
         OC    APDUB(7),APWORK     14 BITS SET, OTHERS LEFT ALONE               
*                                                                               
VSKD45X  MVC   NBRSWKS,APDUB       SAVE IT INTO THE RECORD                      
         MVC   NBRSWKS2(5),APDUB+2                                              
***********************************                                             
* END                                                                           
* BIT-WISE MANIPULATION TO STORE 14 BITS INTO SOMEWHERE OF 7 BYTES              
***********************************                                             
VSKD46   TM    LFLAG,LSHRTSKD      TEST FOR SHORTHAND SCHEDULE                  
         BZ    VSKD47                                                           
         L     R1,LASKDLN          YES - TRANSMIT SCHEDULE IN                   
         OI    6(R1),FVOXMT              EXPANDED FORM                          
         MVC   L'FVIHDR(L'SKDSKD,R1),FVIFLD                                     
*                                                                               
VSKD47   TM    COMCHG,LSKED        TEST FOR NEW SCHEDULE INPUT                  
         BZ    VSKD90                                                           
***      ZIC   RE,CMPNWKS          YES - RE = NUMBER OF CAMPAIGN WEEKS          
***      LA    RF,SPWPERWK-SPWEL(RE)                                            
***      STC   RF,SPWELLN          SET ELEMENT LENGTH                           
***      ST    R4,LREGSV           SAVE ADDRESS OF ELEMENT                      
         SR    R0,R0               YES --                                       
         LA    R1,TBUYDEMS         LOOK FOR DEMOS                               
*&&DO                                                                           
VSKD48   CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),DMOELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VSKD48                                                           
*&&                                                                             
         MVI   APFULL,0                                                         
         MVC   APFULL+1(3),5(R1)   APFULL = DEMO VALUE                          
*                                                                               
         OC    INORTG,INORTG       TEST FOR OVERRIDE TARGET DEMO                
         BZ    VSKD50                                                           
         MVI   APWORK,C'R'         YES - GET THE RATING                         
         CLI   INORTG+1,C'E'                                                    
         BNE   *+8                                                              
         MVI   APWORK,C'E'                                                      
         MVC   APWORK+1(1),INORTG+2                                             
*                                                                               
         LA    RE,L'TBUYDEMO       BXLE SETUP                                   
         LA    RF,TBUYDEMS+L'TBUYDEMS-1                                         
         LA    R1,TBUYDEMS                                                      
         CLC   1(2,R1),APWORK                                                   
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     VSKD50                                                           
*                                                                               
         MVC   APFULL+1(3),5(R1)                                                
*                                                                               
VSKD50   MVC   LFULL,TBUYCOST                                                   
*&&DO                                                                           
         TM    APRECID,RIEFFDT2    FIND THE COST (LFULL)                        
         BZ    *+14                                                             
         MVC   LFULL,NBRSCST2                                                   
         B     VSKD52                                                           
         TM    APRECID,RIEFFDT3                                                 
         BZ    *+14                                                             
         MVC   LFULL,NBRSCST3                                                   
         B     VSKD52                                                           
         MVC   LFULL,NBRSCST1                                                   
*&&                                                                             
VSKD52   LA    R4,TBUYSKED         R4 = A(NEW SPOTS PER WEEK)                   
         DROP  R4                  R4 WAS POINTING TO TSARREC                   
         SR    R0,R0                                                            
         ICM   R0,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    R0,CMPDSTDT                                                      
         AR    R4,R0                                                            
*                                                                               
         LA    R8,APRECKEY+20      R8 = A(OLD SPOTS PER WEEK)                   
         LA    R2,SVACPTS          R2 = A(ACTUAL POINTS LINE)                   
         SR    R0,R0                                                            
         ICM   R0,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    R0,CMPDSTDT                                                      
         MHI   R0,4                                                             
         AR    R2,R0                                                            
         L     R3,ATWA             R3 = (ACTUAL PTS FOR PACKAGE)                
         AHI   R3,SVPKAPTS-TWAD                                                 
         ZIC   RE,CMPNWKS          RE = N'WEEKS                                 
         SR    R0,R0                                                            
         ICM   R0,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    R0,CMPDSTDT                                                      
         SR    RE,R0                                                            
*                                                                               
VSKD54   LA    R0,APRECKEY+20+14   WE DON'T WANT TO CLOBBER STUFF               
         CR    R8,R0                   AFTER APRECKEY                           
         BNL   VSKD57                                                           
*                                                                               
         TM    0(R8),X'C0'         TEST INVALID WEEK                            
         BNZ   VSKD56                                                           
         CLC   0(1,R8),0(R4)       TEST FOR CHANGE THIS WEEK                    
         BE    VSKD56                                                           
         ZIC   R0,0(R4)            YES - FIND THE DIFFERENCE                    
         ZIC   RF,0(R8)                                                         
         SR    R0,RF                                                            
         SRDL  R0,32                                                            
         STM   R0,R1,APDUB                                                      
         M     R0,APFULL           DIFFERENCE X RATING                          
***  2 DECIMAL                                                                  
         LA    RF,TSARREC                                                       
         USING TRECD,RF                                                         
         TM    TBUYDEMS+4,X'40'    IS IT 2 DECIMALS?                            
         DROP  RF                                                               
         BZ    VSKD54E              - NOPE                                      
         M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRA   R1,1                                                             
***  2 DECIMAL                                                                  
VSKD54E  L     RF,0(R2)                                                         
         AR    RF,R1              ADD THE DIFFERENCE IN POINTS TO TOTAL         
         ST    RF,0(R2)                                                         
         CLI   INREC,RECPKG       AND FOR PACKAGE                               
         BNE   *+14                                                             
         L     RF,0(R3)                                                         
         AR    RF,R1                                                            
         ST    RF,0(R3)                                                         
         LM    R0,R1,APDUB                                                      
         M     R0,LFULL            DIFFERENCE X COST                            
*                                                                               
         L     R0,SVACDOL                                                       
         AR    R0,R1               ADD TO TOTAL COST                            
         ST    R0,SVACDOL                                                       
*                                                                               
         CLI   INREC,RECPKG       AND FOR PACKAGE                               
         BNE   VSKD55                                                           
         L     RF,ATWA                                                          
         AHI   RF,SVPKADOL-TWAD                                                 
         L     R0,0(RF)                                                         
         AR    R0,R1                                                            
         ST    R0,0(RF)                                                         
*                                                                               
VSKD55   DS    0H                                                               
         MVC   0(1,R8),0(R4) <=== DO THIS PART LATER LIKE FOR TBUYCOST          
*                                                                               
VSKD56   LA    R4,1(R4)            NEXT WEEK                                    
         LA    R8,1(R8)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   RE,VSKD54           DO FOR ALL CAMPAIGN WEEKS                    
*                                                                               
VSKD57   L     R3,AIOAREA3                                                      
         USING BUYKEY,R3                                                        
         OC    LAACLINE,LAACLINE   TEST FOR ACTUAL POINTS/DOLLARS LINE          
         BZ    VSKD90              ON THIS SCREEN                               
         GOTO1 AFMACPTS,APPARM,LAACLINE,SVGVA  YES-FORMAT ACTUAL POINTS         
         GOTO1 AFMACDOL,APPARM,LAACLINE,SVGVA      FORMAT ACTUAL DOLLS          
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
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET PROGRAM NAME BY DEMO LOOK-UP                         *         
***********************************************************************         
         SPACE 1                                                                
GETPROG  NTR1                                                                   
*&&DO                                                                           
         LA    R4,TSARREC                                                       
         USING TRECD,R4                                                         
         XC    SPDEMLK,SPDEMLK                                                  
         LR    R1,R5                                                            
         AHI   R1,SVDEMXTN-TWAD                                                 
         ST    R1,SPLKXTND                                                      
         MVC   SPLKAREC,AIOAREA3                                                
         MVC   SPLKAFAC,ACOM                                                    
         MVC   SPLKAGY,CUAALF                                                   
         MVC   SPLKMED,CUDMED                                                   
         MVC   SPLKCLI,QCLT                                                     
         MVC   SPLKSRC,CLTSRC                                                   
         MVI   SPLKFIL,C'T'                                                     
*****    MVC   SPLKSTA,NBRSSTA                                                  
         MVC   APWORK(L'MKTLKUP),MKTLKUP                                        
         MVC   APWORK+L'MKTLKUP(L'TBUYSTA),TBUYSTA                              
         GOTO1 VMSUNPK,APPARM,(X'80',APWORK),APWORK+5,APWORK+9                  
*****  CABLE/FUSION DATA LOOKUP                                                 
         CLI   APWORK+9,C'0'       IS IT A NUMBER?                              
         BL    GETP00E              - NOPE, WE DON'T HAVE CABLE NETWORK         
         XC    SPLKSTA,SPLKSTA                                                  
         MVC   SPLKSTA(3),APWORK+14   MOVE THE NETWORK IN                       
         USING SPLKXTD,RE                                                       
         L     RE,SPLKXTND         NEED TO PUT THE HEADEND IN EXTENDED          
         MVC   SPXTHEAD,APWORK+9                                                
         B     GETP00G                                                          
         DROP  RE                                                               
*****  CABLE/FUSION DATA LOOKUP         MHC  04/01/05                           
*                                                                               
GETP00E  MVC   SPLKSTA,APWORK+9                                                 
         CLI   SPLKMED,C'T'                                                     
         BNE   *+8                                                              
         MVI   SPLKSTA+4,C'T'                                                   
GETP00G  MVC   SPLKUMK,MKTLKUP                                                  
         MVC   SPLKDAY,TBUYDAYS                                                 
         MVC   SPLKTIM,TBUYTIMS                                                 
         MVC   SPLKBTYP,STABKTYP                                                
         MVC   APFULL(3),ESTDEMS                                                
         MVI   APFULL+3,FF                                                      
         LA    R1,APFULL                                                        
         ST    R1,SPLKALST                                                      
         LA    R1,APWORK                                                        
         ST    R1,SPLKAVAL                                                      
         MVI   SPLKSVI,X'FF'                                                    
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   GETPRG10                                                         
         OI    SPLKOPT,SPLKOP2D    - YUP, SPECIAL 2 DECIMAL LOOKUP              
***  2 DECIMAL  ***                                                             
GETPRG10 GOTO1 VSPDEMLK,APPARM,(X'FF',SPDEMLK)   CALL SPGETDEM                  
*&&                                                                             
*&&DO                                                                           
         LA    R3,LDMUPBLK         BUILD SPDEMUP BLOCK                          
         USING SPDEMUPD,R3                                                      
         XC    SPDEMUPD(SPDEMUP2),SPDEMUPD                                      
         MVC   SPUPAREC,AIOAREA3                                                
         MVC   SPUPAFAC,ACOM                                                    
         MVC   SPUPAGY,CUAALF                                                   
         MVC   SPUPMED,CUDMED                                                   
         MVC   SPUPCLI,QCLT                                                     
         MVC   SPUPMKT,MKTLKUP                                                  
         MVC   APWORK(L'MKTLKUP),MKTLKUP                                        
         MVC   APWORK+L'MKTLKUP(L'TBUYSTA),TBUYSTA                              
         GOTO1 VMSUNPK,APPARM,(X'80',APWORK),APWORK+5,APWORK+9                  
*****  CABLE/FUSION DATE LOOKUP                                                 
         CLI   APWORK+9,C'0'       IS IT A NUMBER?                              
         BL    GETP00E              - NOPE, WE DON'T HAVE CABLE NETWORK         
         XC    SPUPSTA,SPUPSTA                                                  
         MVC   SPUPSTA(3),APWORK+14   MOVE THE NETWORK IN                       
         MVC   SPUPSYSE,APWORK+9                                                
         B     GETP00G                                                          
*****  CABLE/FUSION DATE LOOKUP                                                 
GETP00E  MVC   SPUPSTA,APWORK+9                                                 
         CLI   SPUPMED,C'T'                                                     
         BNE   *+8                                                              
         MVI   SPUPSTA+4,C'T'                                                   
GETP00G  MVC   SPUPDAY,TBUYDAYS                                                 
         MVC   SPUPTIM,TBUYTIMS                                                 
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
*                                                                               
         CLI   QBOOKTYP,C' '       ANY OVERRIDE BOOK TYPE?                      
         BNH   *+10                                                             
         MVC   SPUPBTYP,QBOOKTYP   YES, USE THAT INSTEAD                        
         TM    SPUPFBK+1,BTYBITSQ  SPECIAL BOOK?                                
         BZ    GETP14                                                           
         TM    SPUPFBK+1,BTY2CHAR  2 CHARACTER BOOKTYPE?                        
         BNO   GETP13R              - NOPE                                      
         CLI   CMPFBTP,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SPUPBTYP,CMPFBTP     - YUP                                       
         B     GETP13T                                                          
*                                                                               
GETP13R  GOTO1 AGETBKTY,APPARM,(C'B',SPUPFBK+1),SPUPBTYP                        
GETP13T  NI    SPUPFBK+1,X'FF'-BTYBITSQ                                         
*                                                                               
GETP14   DS    0H                                                               
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   GETP14G                                                          
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
GETP14G  GOTO1 ASPDEMUP,APPARM,LDMUPBLK,LDEMS,LDEMVALS  CALL SPDEMUP            
         MVC   TBUYPROG,BLANKS                                                  
         MVC   TBUYPROG(L'SPUPPRG),SPUPPRG       PROGRAM NAME RETURNED          
         B     COMMONX                                                          
         DROP  R3,R4                                                            
         EJECT                                                                  
*&&                                                                             
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
***********************************                                             
* TOTAL POINTS PORTION                                                          
***********************************                                             
         LA    R2,GACPTS                                                        
         ZIC   R0,CMPNWKS                                                       
         SR    R3,R3                                                            
FACP10   OC    0(4,R2),0(R2)       ANY POINTS IN THIS WEEK                      
         BZ    *+8                 NO                                           
         A     R3,0(R2)            ACCUMULATE TOTAL OVER ALL WEEKS              
         LA    R2,4(R2)                                                         
         BCT   R0,FACP10                                                        
***********************************                                             
* DISPLAY PORTION                                                               
***********************************                                             
         LA    R2,GACPTS           START WITH WEEK SPECIFIED WITH               
         SR    R0,R0                  STDATE OPTION                             
         ICM   R0,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    R0,CMPDSTDT                                                      
         MHI   R0,4                                                             
         AR    R2,R0                                                            
*                                                                               
         ZIC   R0,CMPNWKS                                                       
         SR    R4,R4                                                            
         ICM   R4,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    R4,CMPDSTDT                                                      
         SR    R0,R4                                                            
         CHI   R0,NMAXWKS          WE CAN ONLY DISPLAY UPTO 14 WKS              
         BNH   *+8                                                              
         LA    R0,NMAXWKS          DON'T GO OVER OTHERWISE TRASHES SCRN         
*                                                                               
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
FACP20   OC    0(4,R2),0(R2)                                                    
         BZ    FACP4                                                            
         ST    R2,EBAIN                                                         
         ST    R4,EBAOUT                                                        
FACP25   GOTO1 VEDITOR,APPARM,EBLOCK     WEEK TOTAL                             
*                                                                               
FACP4    LA    R2,4(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,FACP20                                                        
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
         BAS   RE,PERCENT                                                       
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
         BAS   RE,PERCENT                                                       
*                                                                               
FACDX    L     R1,LAACLINE                                                      
         OI    6(R1),FVOXMT                                                     
         B     COMXIT                                                           
         DROP  R2,R6                                                            
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
         SLL   RF,2                GET RID OF X'80'+X'40' BIT IF THERE          
         SRL   RF,2                PUT THE VALUE BACK IN PLACE                  
         LTR   RF,RF                                                            
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
         CHI   RF,1000                                                          
         BNL   PERCX                                                            
         CHI   RF,100                                                           
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
FMTGOAL  LR    R2,R1               R2=A(PARAMETER ADDRESS LIST)                 
         L     R6,8(R2)            R6=A(GVA INFO BLOCK)                         
         USING GVAD,R6                                                          
*                                                                               
         TM    ESTIND,ESTICS2      USING COST2?                                 
         BZ    FMTG08                                                           
         L     R1,GGLDOL           YES, ADJUST GOAL WITH C2 FACTOR              
         CVD   R1,APDUB                                                         
         ZAP   EBLOCK(16),APDUB                                                 
         SRP   EBLOCK(16),6,0      MULTIPLY BY  1,000,000                       
         XR    R1,R1                                                            
         ICM   R1,7,ESTPW                                                       
         BZ    FMTG08                                                           
         CVD   R1,APDUB                                                         
         LA    RE,8                NUMBER OF BYTES OF DIVISOR                   
         LA    RF,APDUB            1ST BYTE OF DIVISOR                          
FMTG02   CLI   0(RF),0             FIRST BYTE OF DIVISOR                        
         BNE   FMTG04                                                           
         LA    RF,1(RF)                                                         
         BCT   RE,FMTG02           LAST BYTE SHOULD NEVER BE 0                  
*                                                                               
FMTG04   BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         DP    EBLOCK(16),0(0,RF)                                               
*                                                                               
         AHI   RE,2                HIGH ORDER PORTION IS THE QUOTIENT           
         LA    RF,16                                                            
         SR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         ZAP   APDUB,EBLOCK(0)                                                  
         CVB   R1,APDUB                                                         
         ST    R1,GGLDOL                                                        
*                                                                               
FMTG08   XC    EBLOCK,EBLOCK       FORMAT GOAL DOLLARS & POINTS                 
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
         BZ    FMTG10                                                           
         LA    R0,6                YES-PRINT MINUS SIGN                         
         LA    R1,TOTDOL                                                        
         CLI   0(R1),C'$'                                                       
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         B     FMTG10                                                           
         BCTR  R1,0                                                             
         MVI   0(R1),C'-'                                                       
*                                                                               
FMTG10   MVI   EBFLOAT,C'-'        SHOW NEGATIVES                               
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
FMTG20   L     R1,0(R3)                                                         
         LPR   R1,R1                                                            
         C     R1,=F'9995'                                                      
         BL    FMTG40                                                           
         MVI   GSCALE,X'82'                                                     
         C     R1,=F'99995'                                                     
         BL    FMTG40                                                           
         MVI   GSCALE,X'83'                                                     
         B     FMTG60                                                           
*                                                                               
FMTG40   LA    R3,4(R3)                                                         
         BCT   R0,FMTG20                                                        
*                                                                               
FMTG60   MVI   EBLIN,4                                                          
         MVI   EBLOUT,4                                                         
         MVC   EBSCIN,GSCALE                                                    
*                                                                               
         LA    R4,TOTPTSWK-1                                                    
         LA    R3,GGLPTS           WE WANT THE POINTS FOR THAT                  
         SR    R0,R0                   SPECIFIC WEEKS                           
         ICM   R0,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    R0,CMPDSTDT                                                      
         MHI   R0,4                                                             
         AR    R3,R0                                                            
*                                                                               
         MVC   TOTPTSWK,BLANKS                                                  
         LA    R0,NMAXWKS          NORMALLY 14 WKS THAN WE CAN DISPLAY          
         ZIC   RE,CMPNWKS          # OF WEEKS IN CAMPAIGN                       
         SR    RF,RF               LESS THE DISPLACEMENT OF START DATE          
         ICM   RF,1,INOSTDTD                                                    
         BNZ   *+8                                                              
         IC    RF,CMPDSTDT                                                      
         SR    RE,RF                                                            
         CR    R0,RE               IS 14 MORE THAN WHAT WE CAN DISPLAY?         
         BNH   *+6                                                              
         LR    R0,RE               YES, THEN ONLY WHAT WE CAN DISPLAY           
*                                                                               
FMTG80   OC    0(4,R3),0(R3)                                                    
         BZ    FMTG100                                                          
         ST    R3,EBAIN                                                         
         ST    R4,EBAOUT                                                        
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
FMTG100  LA    R4,4(R4)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,FMTG80                                                        
*                                                                               
         L     R1,4(R2)            DISPLAY COMMENT LINE IF POINTS ARE           
         OI    6(R1),FVOXMT        ROUNDED                                      
         LA    R1,L'FVIHDR(R1)                                                  
         MVC   0(70,R1),BLANKS                                                  
         CLI   GSCALE,X'81'                                                     
         BNH   FMTG120                                                          
         MVC   0(L'COMMENT,R1),COMMENT                                          
         LA    R1,L'COMMENT(R1)                                                 
         CLI   GSCALE,X'82'                                                     
         BNH   *+12                                                             
         MVI   0(R1),C'0'                                                       
         LA    R1,1(R1)                                                         
*                                                                               
         LA    R1,1(R1)                                                         
FMTG120  CLI   CUDMED,C'C'         TEST CANADA                                  
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
***  2 DECIMAL                                                                  
         XC    APWORK+64(16),APWORK+64                                          
         MVC   APWORK+64(4),APFULL   OLD TARGET RATING                          
         MVC   APWORK+68(4),LNEWRTG   NEW TARGET RATING                         
*                                                                               
         NI    APWORK+64,X'FF'-X'40'   TAKE OFF 2D BIT IF THERE                 
         NI    APWORK+68,X'FF'-X'40'   TAKE OFF 2D BIT IF THERE                 
         OC    APWORK+64(4),APWORK+64   ANYTHING IN THE OLD RATING?             
         BZ    DEMADJNO                  - NOPE                                 
***                                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BZ    DA1                  - NOPE WE'RE NOT                            
*                                                                               
         TM    APFULL,X'40'        2 DECIMAL?                                   
         BNZ   DA00E                - YUP, NOTHING TO DO                        
         L     R1,APWORK+64                                                     
         MHI   R1,10                                                            
         ST    R1,APWORK+64                                                     
DA00E    TM    LNEWRTG,X'40'       2 DECIMAL?                                   
         BNZ   DA1                                                              
         L     R1,APWORK+68                                                     
         MHI   R1,10                                                            
         ST    R1,APWORK+68                                                     
*****  BOTH APFULL AND LNEWRTG SHOULD BE IN 2 DECIMAL MODE BY NOW               
DA1      DS    0H                                                               
***  2 DECIMAL                                                                  
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
         BNE   DEMADJYS                                                         
*                                                                               
DA4      L     R1,APWORK+64        CALCULATE PCT ADJUSTMENT                     
         SR    RE,RE                                                            
         L     RF,APWORK+68                                                     
         M     RE,=F'2000'                                                      
         DR    RE,R1                                                            
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,LDEMADJ          LDEMADJ = PCT ADJUSTMENT                     
*                                                                               
         L     R1,LADEMEL          SCAN ALL DEMOS IN tbuydems                   
         LA    RF,L'TBUYDEMS-1(R1)                                              
         LA    RE,L'TBUYDEMO                                                    
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
         AHI   R3,1                                                             
         SRA   R3,1                                                             
***  2 DECIMAL                                                                  
         MVI   APBYTE,0                                                         
         TM    4(R1),X'40'         IS IT PREVIOUSLY 2 DECIMAL?                  
         BZ    DA11                 - NOPE                                      
         OI    APBYTE,X'40'         - YUP, NEED THE 2 DECIMAL BIT ON            
***  2 DECIMAL                                                                  
DA11     ST    R3,4(R1)            STORE ADJUSTED DEMO IN ELEMENT               
         TM    APBYTE,X'40'                                                     
         BZ    DA12                                                             
         OI    4(R1),X'40'                                                      
*                                                                               
DA12     BXLE  R1,RE,DA6           NEXT DEMO                                    
DEMADJYS CR    RE,RE               SET EQUAL CONDITION                          
         B     DAX                                                              
*                                                                               
DEMADJNO XR    R1,R1                                                            
         CR    RB,R1                                                            
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
         LA    R4,TSARREC                                                       
         USING TRECD,R4                                                         
         XC    LUPVALS,LUPVALS                                                  
         OC    CMPUP,CMPUP                                                      
         BZ    DM2                                                              
         MVC   LUPFIL,CMPUF                                                     
         MVC   LUPGRD,CMPUP                                                     
         MVC   LUPFRBK,CMPFB                                                    
         TM    CMPFB+1,BTY2CHAR                                                 
         BNO   DM1C                                                             
         CLI   CMPFBTP,0           ANYTHING HERE?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LUPFRBKT,CMPFBTP                                                 
DM1C     MVC   LUPFRBKL,CMPFBLST                                                
         MVC   LUPPUT,CMPUPUT                                                   
         MVC   LUPSHR,CMPUSHR                                                   
         MVI   QBOOKTYP,0          MUST BE CLEARED BECAUSE DEFAULT              
         TM    LUPFRBK+1,BTYBITSQ  SPECIAL BOOK?                                
         BZ    DM2                                                              
         TM    LUPFRBK+1,BTY2CHAR  2 CHARACTER BOOKTYPE?                        
         BNO   DM1E                 - NOPE                                      
         CLI   LUPFRBKT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   QBOOKTYP,LUPFRBKT    - YUP                                       
         B     DM2                                                              
*                                                                               
DM1E     GOTO1 AGETBKTY,APPARM,(C'B',LUPFRBK+1),QBOOKTYP                        
*                                                                               
DM2      L     R8,AIOAREA2         AIOAREA2 HAS THE BUY RECORD                  
         USING BUYKEY,R8                                                        
         SR    R0,R0                                                            
         LA    R8,BDELEM                                                        
*                                                                               
DM4      CLI   0(R8),0                                                          
         BE    DM10                                                             
         CLI   0(R8),X'62'         UPGRADE DESCRIPTION ELEMENT                  
         BNE   DM8                                                              
         USING UPELEM,R8                                                        
         MVC   LUPFIL,UPFILE                                                    
         MVC   LUPGRD,UPTYPE                                                    
         MVC   LUPFRBK,UPFBK                                                    
*** ???                                                                         
         MVC   LUPFRBKL,UPFBKLST                                                
         MVC   LUPPUT,UP2YRP                                                    
         MVC   LUPSHR,UP2YRS                                                    
         MVC   LOVDAY(L'LOVDAY+L'LOVTIME),UPDAYTIM                              
         B     DM10                                                             
         DROP  R8                                                               
*                                                                               
DM8      IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     DM4                                                              
*                                                                               
DM10     OC    LUPGRD,LUPGRD       TEST UPGRADE EXPRESSION FOUND                
         BNZ   DM12                                                             
         CLI   QMED,C'R'                                                        
         BE    DM15                                                             
         MVC   FVMSGNO,=AL2(FVNOCNUP)   NO-ERROR EXIT                           
         B     DMX                                                              
*                                                                               
DM12     LA    R8,LDMUPBLK                                                      
         USING SPDEMUPD,R8                                                      
         XC    SPDEMUPD(SPDEMUP2),SPDEMUPD                                      
         MVC   SPUPAREC,AIOAREA3                                                
         MVC   SPUPAFAC,ACOM                                                    
         MVC   SPUPAGY,CUAALF                                                   
         MVC   SPUPMED,CUDMED                                                   
         MVC   SPUPCLI,QCLT                                                     
         MVC   SPUPMKT,MKTLKUP                                                  
****     MVC   SPUPSTA,NBRSSTA                                                  
         MVC   APWORK(L'MKTLKUP),MKTLKUP                                        
         MVC   APWORK+L'MKTLKUP(L'TBUYSTA),TBUYSTA                              
         GOTO1 VMSUNPK,APPARM,(X'80',APWORK),APWORK+5,APWORK+9                  
*****  CABLE/FUSION DATE LOOKUP                                                 
         CLI   APWORK+9,C'0'       IS IT A NUMBER?                              
         BL    DM12E                - NOPE, WE DON'T HAVE CABLE NETWORK         
         XC    SPUPSTA,SPUPSTA                                                  
         MVC   SPUPSTA(3),APWORK+14   MOVE THE NETWORK IN                       
         MVC   SPUPSYSE,APWORK+9                                                
         B     DM12G                                                            
*****  CABLE/FUSION DATE LOOKUP                                                 
DM12E    MVC   SPUPSTA,APWORK+9                                                 
         CLI   SPUPMED,C'T'                                                     
         BNE   *+8                                                              
         MVI   SPUPSTA+4,C'T'                                                   
DM12G    MVC   SPUPDAY,TBUYDAYS                                                 
         MVC   SPUPTIM,TBUYTIMS                                                 
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
*                                                                               
         CLI   QBOOKTYP,C' '       ANY OVERRIDE BOOK TYPE?                      
         BNH   *+10                                                             
         MVC   SPUPBTYP,QBOOKTYP   YES, USE THAT INSTEAD                        
         TM    SPUPFBK+1,BTYBITSQ  SPECIAL BOOK?                                
         BZ    DM14                                                             
         TM    SPUPFBK+1,BTY2CHAR  2 CHARACTER BOOKTYPE?                        
         BNO   DM13R                - NOPE                                      
         CLI   LUPFRBKT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SPUPBTYP,LUPFRBKT    - YUP                                       
         B     DM13T                                                            
*                                                                               
DM13R    GOTO1 AGETBKTY,APPARM,(C'B',SPUPFBK+1),SPUPBTYP                        
DM13T    NI    SPUPFBK+1,X'FF'-BTYBITSQ                                         
*                                                                               
DM14     DS    0H                                                               
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   DM14G                                                            
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
DM14G    GOTO1 ASPDEMUP,APPARM,LDMUPBLK,LDEMS,LDEMVALS  CALL SPDEMUP            
         TM    TBUYFLG1,TBF1PRG    TEST FOR PROGRAM OVERRIDE                    
         BO    DM15                                                             
         MVC   TBUYPROG,BLANKS                                                  
         MVC   TBUYPROG,SPUPPRG                                                 
DM15     MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
DMX      CLC   FVMSGNO,=AL2(FVFOK)   SET THE CONDITION CODE                     
         B     COMMONX                                                          
         DROP  R4,R8                                                            
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
COMMENT  DC    CL16'Wkly Pts=Imps/10'                                           
CANCMT   DC    0CL51                                                            
         DC    C'Goals combined reduced by network,spill,&& selective'          
LINKEYT  DC    AL1(KEYMED,KEYBYR,KEYCAM,KEYMKT,KEYDPL,0,0,0)                    
BLANKS   DC    100C' '                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT PARAMETERS -- PART 1                                *         
*                                                                     *         
* INPUT  : R1=A(SECOND KEY FIELD FOR MKT AND DPT/LEN) OR 0            *         
* OUTPUT : IF ERROR, FVMSGNO SET AND FVINDX SET                       *         
*          APRECKEY                                                             
***********************************************************************         
         USING TWAD,R5                                                          
         USING SAVAREA,R6                                                       
         USING WORKD,R7                                                         
*                                                                               
VLPARM   NTR1  BASE=*,LABEL=*                                                   
         ST    R1,COMSVRG1         SAVE A(SECOND KEY FIELD)                     
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
         BAS   RE,VPARSETF                                                      
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
         OC    BWHKAGMD,BBYRMASK                                                
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
*                                                                               
         BRAS  RE,VALSTDTE                                                      
         BNE   VPARX                                                            
*                                                                               
         OC    BCLT(9),BCLT                                                     
         BNZ   VPAR7                                                            
         L     RE,ALSM                                                          
         MVI   LSMUKEY-LSMD(RE),0   CLEAR OUT THE SAVED KEY                     
*                                                                               
VPAR7    OI    BWSKEYH+4,X'20'     SET PREVIOUSLY VALIDATED                     
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
         MHI   R1,6                                                             
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
         MVI   WRKDCP+12,C'p'                                                   
         CLI   0(R1),C'R'                                                       
         BE    *+16                                                             
         CLI   0(R1),C'E'                                                       
         BE    *+8                                                              
         MVI   WRKDCP+12,C'm'      FORMAT M IF IMPRESSION (CPM)                 
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
*                                                                               
         NI    MISCFLG1,X'FF'-MF1BUYRV                                          
         LA    R1,BWHFSTEL                                                      
         XR    R0,R0                                                            
VPAR44   CLI   0(R1),0                                                          
         BE    VPAR46                                                           
         CLI   0(R1),INFELCDQ                                                   
         BE    VPAR45                                                           
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VPAR44                                                           
*                                                                               
         USING INFELD,R1                                                        
VPAR45   TM    INFFLAG1,IFF1BYRV                                                
         BZ    VPAR46                                                           
         OI    MISCFLG1,MF1BUYRV                                                
         DROP  R1                                                               
*                                                                               
VPAR46   DS    0H                                                               
***********************************************************************         
* THIS CODE IS NOT NECESSARY BECAUSE WE SHOULD BE ABLE TO FILTER                
*   BY STATIONS THAT ARE NOT EVEN ON THE THE WORKSHEET HEADER RECORD.           
* A SIMPLE BUYS/SKED WILL DISPLAY STATIONS THAT ARE NOT IN THE HEADER           
*    AND THE USER WILL DEFINITELY WANT TO FILTER BY THOSE STATIONS.             
***********************************************************************         
*&&DO                                                                           
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
*&&                                                                             
VPAR48   XC    APRECKEY,APRECKEY                                                
         MVI   BWDKTYP,BWDKTYPQ    SET UP FIRST PART OF DETAIL KEY              
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,BAGYMD                                                  
         OC    BWDKAGMD,BBYRMASK                                                
         MVC   BWDKBYR,BBYR                                                     
         MVC   BWDKSEQ,BWHKSEQ     CAMPAIGN/MKT SEQ NO FROM HEADER KEY          
         MVI   BWDKELCD,BWDELCDQ                                                
         STC   RE,BWDKELST         STATION CODE                                 
*                                                                               
         XC    DTLKEY,DTLKEY       SAVE THE DETAIL RECORD KEY                   
         MVC   DTLKEY(BWDKELST-BWDKEY),BWDKEY                                   
*                                                                               
         XC    APRECKEY,APRECKEY                                                
         USING BUYKEY,R3                                                        
         MVC   BUYKAM,BAGYMD                                                    
         MVC   BUYKCLT,BCLT                                                     
         MVC   BUYKPRD,BPRD                                                     
         MVC   BUYMSTA(L'BMKT+L'BSTA),BMKT  ** MIGHT NOT HAVE STATION           
         MVC   BUYKEST,CMPESTN                                                  
         USING BWDRECD,R3                                                       
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
         MVC   APWORK(0),FVIFLD                                                 
         EX    R4,*-6                                                           
         L     R1,AKEYHDR                                                       
         GOTO1 AFVAL                                                            
         MVC   FVMSGNO,APHALF                                                   
         ZIC   RE,FVILEN                                                        
         LA    R1,FVIFLD(RE)                                                    
         MVI   0(R1),C','                                                       
         MVC   1(0,R1),APWORK                                                   
         EX    R4,*-6                                                           
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
         AHI   R8,-3                                                            
         ST    R1,FVADDR                                                        
         STC   R8,FVINDX                                                        
*                                                                               
VPARX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         J     EXIT                                                             
         SPACE 2                                                                
VPARERTG MVC   FVMSGNO,=AL2(FVIRTGOP)   INVALID RATING OPTION                   
         LA    R1,BWSOPTH                                                       
         ST    R1,FVADDR                                                        
         B     VPARX                                                            
         SPACE 2                                                                
VPARSETF XC    APWORK,APWORK                                                    
         BCTR  RF,0                                                             
         MVC   APWORK+L'FVIHDR(0),12(R4)                                        
         EX    RF,*-6                                                           
         LA    RF,1(RF)                                                         
         STC   RF,FVILEN-FVIHDR+APWORK                                          
         LA    RF,L'FVIHDR(RF)                                                  
         STC   RF,APWORK                                                        
         BR    RE                                                               
         DROP  R2,R3                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE START DATE OPTION IF ANY                                         
***********************************************************************         
VALSTDTE NTR1  BASE=*,LABEL=*                                                   
         OC    INOSTDTE,INOSTDTE   ANY START DATE OPTION?                       
         BZ    VSDTX                                                            
         MVI   CMPDSTDT,0          STDATE=  OVERRIDES PREV SCROLLING            
         TM    INOSTDTD,X'80'      YEAR WAS OMITTED?                            
         BZ    VSDT20              NO, USER ACTUALLY TYPE IN THE YEAR           
         GOTO1 VDATCON,APPARM,(2,INOSTDTE),(3,APWORK)                           
*                                                                               
         CLC   CMPST(1),CMPND      CAMP DATES IN SAME YEAR?                     
         BNE   *+14                                                             
         MVC   APWORK(1),CMPST                                                  
         B     VSDT10                                                           
***************                                                                 
* CAMP DATES ARE IN DIFFERENT YEARS                                             
***************                                                                 
         CLC   APWORK+1(2),CMPST+1   STDATE M/D BEFORE CAMP START M/D?          
         BL    *+14                                                             
         MVC   APWORK(1),CMPST       NO, COPY THE YEAR FROM CAMP START          
         B     VSDT10                                                           
*                                                                               
         MVC   APWORK(1),CMPND       YES, COPY THE YEAR FROM CAMP END           
*                                                                               
VSDT10   GOTO1 VDATCON,APPARM,(3,APWORK),(2,INOSTDTE)                           
         NI    INOSTDTD,X'FF'-X'80'                                             
*                                                                               
***T20   LR    R2,R5                                                            
VSDT20   L     R2,ATWA             R5 MAY NOT BE POINTING TO ATWA               
         AHI   R2,CMPDATSP-TWAD                                                 
         XR    R3,R3               R3 = DISPL FROM CAMP START DATE              
*                                                                               
         CLC   INOSTDTE,0(R2)      MAKE SURE START DATE W/IN CAMP DATES         
         BNL   VSDT30                                                           
VSDTERR  MVC   FVMSGNO,=AL2(FVOUTCAM)                                           
         LA    R1,BWSOPTH                                                       
         ST    R1,FVADDR                                                        
         B     VSDTNO              BELOW CAMP DATES, ERROR                      
*                                                                               
VSDT30   CLI   0(R2),FF            NEXT DATE IS EOT?                            
         BE    VSDTERR             THEN ERROR                                   
         CLC   INOSTDTE,2(R2)      START DATE SHOULD BE IN 1 OF THE WKS         
         BNH   VSDT40                                                           
         AHI   R2,4                2 COMPRESSED DATES                           
         AHI   R3,1                                                             
         B     VSDT30                                                           
*                                                                               
*   NOW WE KNOW WHERE INOSTDATE STANDS IN RELATION TO CAMP START DATE           
VSDT40   MVC   INOSTDTE,0(R2)                                                   
         STC   R3,INOSTDTD                                                      
*                                                                               
VSDTYES  SR    RC,RC                                                            
VSDTNO   LTR   RC,RC                                                            
VSDTX    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SEE IF WE NEED TO REMOVE THE GLOBBER XCTL ELEM                                
*                                                                               
* ON ENTRY:    (R5)                A(TWA)                                       
***********************************************************************         
REMVGLOB NTR1  BASE=*,LABEL=*                                                   
         L     RF,ACOM                                                          
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         BZ    RGLBX                                                            
         GOTO1 (RF),APPARM,=C'GETD',APELEM,24,GLVXCTL    XFER CTRL ELEM         
         CLI   8(R1),0                                                          
         BNE   RGLBX                                                            
*                                                                               
         LA    R1,APELEM                                                        
         USING GLVXFRSY,R1                                                      
         CLC   GLVXFRSY,=C'SPO'                                                 
         BNE   RGLBX                                                            
         CLC   GLVXFRPR,=C'SFM'                                                 
         BNE   RGLBX                                                            
         TM    GLVXFLG1,GLV1RETN       RETURNING FROM SFM?                      
         BZ    RGLBX                                                            
         DROP  R1                                                               
*                                                                               
* GET RID OF IT QUICKLY !                                                       
         GOTO1 (RF),APPARM,=C'DELE'                                             
*                                                                               
         XC    BWSSRV,BWSSRV                                                    
         MVC   BWSSRV(3),=C'=RE'   RETRANSMIT SCREEN                            
         MVI   BWSSRVH+5,3                                                      
         OI    BWSSRVH+6,X'80'                                                  
*                                                                               
RGLBX    J     EXIT                                                             
         LTORG                                                                  
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
LIST3D   DSECT                                                                  
LST3STA  DS    CL4                                                              
LST3IND  DS    CL1                                                              
LST3LINE DS    CL3                                                              
*                                                                               
SKDLINED DSECT                                                                  
SKDLSPH  DS    CL8                                                              
SKDLSP   DS    0CL22                                                            
SKDSTA   DS    CL5                                                              
SKDIND   DS    CL1                                                              
SKDLIN   DS    CL3                                                              
         DS    CL1                                                              
SKDPRG   DS    CL12                                                             
*                                                                               
SKDSKDH  DS    CL8                                                              
SKDSKD   DS    CL56                                                             
*                                                                               
SKDLINEL EQU   *-SKDLINED                                                       
         EJECT                                                                  
       ++INCLUDE SPNWSWRK                                                       
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
LINEDISP DS    CL3                                                              
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
LF2ATSRR EQU   X'04'               ADD A TSAR RECORD FOR THIS RECORD            
LF2NPOCM EQU   X'02'               BUY IS NOT PART OF THIS CAMPAIGN             
LF2MKGDB EQU   X'01'               BUY IS A MAKEGOOD BUY                        
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS                          
MF1ADDRC EQU   X'80'               - NEED TO ADD THE NBR RECORD                 
MF1MANUL EQU   X'40'               - MANUALLY ADDED BUY                         
MF1COSOV EQU   X'20'               - LOOK FOR A COST OVERRIDE NOW               
MF1BUYRV EQU   X'10'               - CAMP/MKT IS DOING BUY REVISIONS            
MF1ECSTS EQU   X'08'               - WE HAVE EFFECTIVE COSTS                    
MF1DSNBR EQU   X'04'               - WE HAVE A BUY REVISION (DISSEL)            
MF1DEM2D EQU   X'02'               - MAIN DEMO CATEGORY NEEDS 2 DECIMAL         
MF1ERROR EQU   X'01'               - WE ALREADY HAVE A BUY ERROR                
*                                                                               
COSTNUMB DS    XL1                 NUMBER OF COST OVERRIDES                     
MAXCOSTS EQU   8                                                                
COSTABLE DS    0XL(MAXCOSTS*4)                                                  
         DS    (MAXCOSTS)XL4                                                    
CURRCOVR DS    XL1                 CURRENT COST OVERRIDE NUMBER                 
CURRCOST DS    XL4                 CURRENT OVERRIDE COST                        
*                                                                               
LIND     DS    X                                                                
LPKGNUM  DS    X                                                                
LFSTLINE DS    X                                                                
OTOBITS  DS    XL7                 THE OTO BITS                                 
         DS    XL12                SPARE                                        
LSPW     DS    XL(NMAXWKS)                                                      
LDMUPBLK DS    (SPDEMUP2)X                                                      
LKEYCOMP DS    XL1                                                              
*                                                                               
LUPVALS  DS    0CL24                                                            
LUPFIL   DS    C                                                                
LUPGRD   DS    XL8                                                              
LUPFRBK  DS    XL2                                                              
LUPFRBKT DS    XL1                 LUPFRBK BOOKTYPE                             
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
TSARREC  DS    (TBUYRECL)X         TSAR RECORD                                  
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
       ++INCLUDE SPNWSD5D                                                       
         EJECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSD6D                                                       
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
* SPNWSBRV                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSBRV                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENBUY                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPDEMLKXTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPDEMLKXTD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDGLOBEQUS                                                                    
* DDGLVXCTLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
         PRINT ON                                                               
* SPBUYVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPBUYVALD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'242SPNWS35   09/24/08'                                      
         END                                                                    
