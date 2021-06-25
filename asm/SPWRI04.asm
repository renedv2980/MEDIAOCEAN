*          DATA SET SPWRI04    AT LEVEL 089 AS OF 10/15/09                      
*PHASE T20404A                                                                  
         TITLE 'T20404 - SPOTPAK WRITER SCHEDULE REPORT'                        
*                                                                               
*********************************************************************           
*                                                                   *           
*          SPWRI04 (T20404) - SPOT WRITER SCHEDULE REPORT           *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 15OCT09 89 AKT -- 2-BYTE BUYLINE SUPPORT                          *           
* 03NOV03 18 AKT -- FIX MGROUP X'40' BUGS                           *           
* 14OCT02 87 EFJ -- 2 CHAR MGR SCHEME CODES                         *           
* 11JAN01 86 EFJ -- HISTORY LOST                                    *           
*                -- CHANGE LABELS FOR CLEAN ASSEMBLY                *           
*********************************************************************           
T20404   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20404,RA                                                      
         MVC   SAVERD,4(RD)        SAVE A(CALLING PROGRAM'S SAVE AREA)          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPINPUT      INPUT                                        
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         B     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     XC    SVBUYKEY,SVBUYKEY                                                
         OI    SBQSKIP,SBQSKBIL    SKIP READING STATION BILL RECORDS            
         OI    SBQDATA,SBQDGOAL+SBQDPUR   SET DATA                              
         OI    COLIND,COLIDEM      INDICATE DEMOS IS THIS RPT                   
         OI    COLIND,COLINDR      NO DEMO ROUNDING                             
         OI    DATAIND,DIDPTLEN    DAYPART/LENGTHS REQUIRED                     
         OI    DATAIND2,DIAFFIL    GET STATION AFFIL/CHAN/REP                   
         OI    DATAIND3,DICHAN+DIREP                                            
         OI    SBQPER,SBQPWK+SBQPMN    SET PERIODS ARE WEEKS AND MONTHS         
         MVI   SBQPERLO,1          MAX 14 WEEKS                                 
         MVI   SBQPERHI,14                                                      
*                                                                               
         OI    SBQPIND,SBQPPB      DON'T SPLIT PIGGYBACKS                       
         CLC   SBQPRD,=C'POL'      TEST POL REQUEST                             
         BNE   *+12                                                             
         MVI   SBQBPRD,0           YES-SEPERATE THE PRODUCTS                    
         OI    SBQPIND,SBQPOLSP                                                 
         MVI   WIDTHOPT,C'W'       MAKE SURE REPORT IS WIDE                     
         MVI   DATESW,0                                                         
         MVI   SALESFST,C'Y'                                                    
*                                                                               
         MVI   MYFIRSTH,12         SET DRIVER'S FIRST HEADLINE                  
*                                                                               
         CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INITX                                                            
         MVI   REP1OPT,C'Y'        YES-VALIDATE OPTIONS                         
         MVI   REP2OPT,C'Y'                                                     
         LA    R2,GBRRPTH                                                       
         CLI   5(R2),0                                                          
         BE    INIT2                                                            
         CLI   GBRRPT,C'1'                                                      
         BL    EINV                                                             
         BH    *+12                                                             
         MVI   REP2OPT,C'N'        BRAND TIME SCHED & BRAND PERF ONLY           
         B     INIT1                                                            
         CLI   GBRRPT,C'2'                                                      
         BNE   *+12                                                             
         MVI   REP1OPT,C'N'        SALESPERSON'S TIME SCHEDULE ONLY             
         B     INIT1                                                            
         CLI   GBRRPT,C'3'                                                      
         BNE   EINV                                                             
*                                                                               
INIT1    CLI   REP1OPT,C'N'        TEST SALESPERSON'S REPORT ONLY               
         BNE   INIT2                                                            
         OI    SBQSKIP,SBQSKGL     YES-NO GOALS                                 
         NI    COLIND,FF-COLIDEM       NO DEMOS                                 
*                                                                               
INIT2    MVI   DOLOPT,C'Y'         DOLLAR OPTION                                
         LA    R2,GBRDOLH                                                       
         CLI   5(R2),0                                                          
         BE    INIT4                                                            
         CLI   GBRDOL,C'N'                                                      
         BE    INIT4                                                            
         MVI   DOLOPT,C'N'                                                      
         CLI   GBRDOL,C'Y'         TEST SUPPRESS COST                           
         BE    INIT4                                                            
         B     EINV                                                             
*                                                                               
INIT4    MVI   STAOPT,C'N'         STATION SEQUENCE OPTION                      
         LA    R2,GBRSSQH                                                       
         CLI   5(R2),0                                                          
         BE    INIT5                                                            
         MVC   STAOPT,GBRSSQ                                                    
         CLI   GBRSSQ,C'N'                                                      
         BE    INIT5                                                            
         CLI   GBRSSQ,C'Y'                                                      
         BNE   EINV                                                             
         CLC   SBQPRD,=C'POL'  TEST STATION SEQ AND PRD=POL                     
         BNE   INIT5                                                            
         OI    SBQPIND,SBQPOLES  YES-FORCE ESTIMATES TO POL ESTIMATES           
         CLI   SBQPGRD,C' '    PRODUCT GROUPS ARE INVALID                       
         BH    EPGR                                                             
*                                                                               
INIT5    LA    R2,GBRTITH          VALIDATE TITLE                               
         MVC   TITLE1,SPACES                                                    
         MVC   TITLE1(19),=C'BRAND TIME SCHEDULE'                               
         MVC   TITLE2,SPACES                                                    
         MVC   TITLE2(24),=C'BRAND PERFORMANCE REPORT'                          
         CLI   STAOPT,C'Y'                                                      
         BNE   *+10                                                             
         MVC   TITLE2(25),=C'MARKET PERFORMANCE REPORT'                         
         MVC   TITLE3,SPACES                                                    
         MVC   TITLE3(26),=C'SALESPERSONS TIME SCHEDULE'                        
         CLI   5(R2),0                                                          
         BE    INIT6                                                            
         GOTO1 ANY                                                              
         MVC   TITLE1,WORK                                                      
         MVC   TITLE2,WORK                                                      
         MVC   TITLE3,WORK                                                      
*                                                                               
INIT6    GOTO1 CENTER,DMCB,TITLE1,63                                            
         GOTO1 (RF),(R1),TITLE2,63                                              
         GOTO1 (RF),(R1),TITLE3,63                                              
*                                                                               
         LA    R5,LEVS1P           INITIALLY SET LEVEL TABLE FOR 1ST            
         CLI   STAOPT,C'Y'         REPORT                                       
         BNE   *+8                                                              
         LA    R5,LEVS1S                                                        
         BAS   RE,SETLEVS                                                       
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
VALID    LA    R2,GBRPERH          VALIDATE REQUEST PERIOD                      
         XC    WORK,WORK                                                        
         MVC   WORK(12),SBQSTART                                                
         CLC   WORK+4(2),=C'00'                                                 
         BNE   VAL2                                                             
         MVI   WORK+5,C'1'                                                      
         MVI   WORK+11,C'1'                                                     
         GOTO1 GETBROAD,DMCB,(1,WORK+6),WORK+12,GETDAY,ADDAY                    
         MVC   WORK+6(6),WORK+18                                                
*                                                                               
VAL2     MVC   WORK+12(4),GETBROAD                                              
         MVC   WORK+16(4),ADDAY                                                 
         MVC   WORK+20(4),GETDAY                                                
         MVC   WORK+24(4),DATCON                                                
         GOTO1 MOBILE,DMCB,(55,WORK),(4,ELEM),WORK+12,SBSPPROF                  
         CLI   0(R1),14                                                         
         BH    EPER                                                             
*                                                                               
VALX     B     XIT                                                              
         EJECT                                                                  
* ERROR EXITS AND MESSAGES                                                      
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
EPER     MVC   CONHEAD(L'EPERMSG),EPERMSG                                       
         B     MYCURSOR                                                         
*                                                                               
EPGR     MVI   ERROR,INVALID       PRODUCT GROUPS INVALID                       
         LA    R2,GBRPRDH                                                       
         B     CURSOR                                                           
*                                                                               
MYCURSOR MVI   ERROR,X'FE'                                                      
CURSOR   GOTO1 CURSERR                                                          
*                                                                               
EPERMSG  DC    C'MAXIMUM REQUEST PERIOD IS 14 WEEKS'                            
         EJECT                                                                  
* SPOTIO HOOKS                                                                  
*                                                                               
INPUT    L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCSP                                                  
         BE    PROCBUY                                                          
         B     XIT                                                              
         EJECT                                                                  
* PROCESS BUY RECORD                                                            
*                                                                               
PROCBUY  CLI   DATESW,C'Y'         TEST N'DATES SET YET                         
         BE    BUY2                YES                                          
         MVI   DATESW,C'Y'                                                      
         L     R1,SBADATE                                                       
         SR    RE,RE                                                            
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         BCT   RE,*-12                                                          
         LPR   RE,RE                                                            
         ST    RE,SBNDATES                                                      
*                                                                               
BUY2     L     R3,SBAIO1           FIND DEMO OVERRIDE INDICATORS                
         USING BUYRECD,R3                                                       
         MVC   OVDEMS,SPACES       INIT TO NO MANUAL DEMO OVERRIDES             
         XC    RTGBOOK,RTGBOOK                                                  
         SR    R0,R0                                                            
         LA    R5,BDELEM           FIND DEMO ELEMENT                            
*                                                                               
BUY4     CLI   0(R5),0                                                          
         BE    BUYX                                                             
         CLI   0(R5),2                                                          
         BE    *+14                                                             
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     BUY4                                                             
         USING NDELEM,R5                                                        
         MVC   RTGBOOK,NDBOOK      SAVE THE RATING BOOK                         
         LLC   RE,NDLEN                                                         
         SH    RE,=Y(NDEMNO-NDELEM)                                             
         BNP   BUYX                                                             
         SRL   RE,3                                                             
         ST    RE,FULL             FULL=N'DEMOS IN DEMO ELEMENT                 
*                                                                               
         LA    R0,3                3 DEMOS                                      
         LA    R1,OVDEMS                                                        
         LLC   RE,SBBPRD           FIND ENTRY IN PRD/EST TABLE                  
         CLI   SBBPRD,X'FE'                                                     
         BNE   *+8                                                              
         LA    RE,255              PRD=POL FOR UNALLOCATED                      
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         LLC   RF,BUYKEST                                                       
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         A     RE,SBAESTTB                                                      
         ICM   RF,1,0(RE)          POINTER TO ESTIMATE BUFFER ENTRY             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         MH    RF,=Y(ESTBUFFL)                                                  
         A     RF,SBAESTBF                                                      
         LA    R2,EBDEMOS-ESTBUFFD(RF)   R2=A(DEMO LIST)                        
*                                                                               
BUY5     L     RF,FULL                                                          
         LA    R6,NDEMNO                                                        
         CLC   0(3,R6),0(R2)                                                    
         BE    *+16                                                             
         LA    R6,8(R6)                                                         
         BCT   RF,*-14                                                          
         B     BUY6                                                             
         TM    4(R6),X'80'         TEST DEMO HAS MANUAL OVERRIDE                
         BZ    BUY6                                                             
         MVI   0(R1),C'*'          YES                                          
*                                                                               
BUY6     LA    R1,1(R1)                                                         
         LA    R2,3(R2)                                                         
         BCT   R0,BUY5                                                          
*                                                                               
BUYX     B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
* DRIVER INPUT ROUTINE                                                          
*                                                                               
         USING GLOBALD,R4                                                       
DRIVIN   LR    R0,RE                                                            
         MVI   GLMODE,GLINPUT                                                   
         BAS   RE,GODRIVER                                                      
         CLI   MKTIND,C'O'         TEST ORIGINATING MARKET                      
         BE    *+12                                                             
         CLI   MKTIND,C'S'         OR SPILL MARKET                              
         BNE   DRIVINX                                                          
         MVC   SVMKTIND,MKTIND                                                  
         MVC   SVMAXTLV,MAXTOTLV                                                
         MVI   MKTIND,FF           SET COMBINED MARKET                          
         CLC   MKTLEV,MAXTOTLV     YES-                                         
         BNH   *+10                                                             
         MVC   MAXTOTLV,MKTLEV     DO NOT GENERATE ANY TOTALS                   
         BAS   RE,GODRIVER         CALL DRIVER FOR COMBINED MARKET              
         MVC   MKTIND,SVMKTIND                                                  
         MVC   MAXTOTLV,SVMAXTLV                                                
*                                                                               
DRIVINX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLINIT       INITIALIZATION                               
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLRESLIT     RESOLVE LITERAL                              
         BE    RESLIT                                                           
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLPUTSRT     PUT A SORT RECORD                            
         BE    PUTSRT                                                           
         CLI   GLHOOK,GLFIRST      FIRSTS                                       
         BE    FIRSTS                                                           
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEAD                                                             
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
* DRIVER INITIALIZATION HOOK                                                    
*                                                                               
DRVINIT  MVC   GLOPTS+2(1),REP1OPT REPORT OPTIONS                               
         MVC   GLOPTS+3(1),REP2OPT                                              
         MVC   GLOPTS+4(1),DOLOPT  DOLLAR OPTION                                
         MVC   GLOPTS+5(1),STAOPT  STATION SEQUENCE OPTION                      
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  DS    0H                                                               
         LA    R1,RTNLIST          SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),FF                                                         
         BE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         B     XIT                                                              
         SPACE 1                                                                
RTNLIST  DS    0F                                                               
         DC    CL8'IPRDUSR ',A(IPRD)                                            
         DC    CL8'OMGR1USR',A(OMGR1)                                           
         DC    CL8'OMGR2USR',A(OMGR2)                                           
         DC    CL8'OMGR3USR',A(OMGR3)                                           
         DC    CL8'OMKTUSR ',A(OMKT)                                            
         DC    CL8'ISTAUSR ',A(ISTA)                                            
         DC    CL8'OSTAUSR1',A(OSTA)                                            
         DC    CL8'OSTAUSR2',A(OSTATION)                                        
         DC    CL8'IBUYUSR ',A(IBUY)                                            
         DC    CL8'OBUYUSR ',A(OBUY)                                            
         DC    CL8'HBUYUSR ',A(HBUY)                                            
         DC    CL8'IDAYUSR ',A(IDAY)                                            
         DC    CL8'ODAYUSR ',A(ODAY)                                            
         DC    CL8'IBUYDTL ',A(IBUYDTL)                                         
         DC    CL8'OBUYDTL ',A(OBUYDTL)                                         
         DC    CL8'ICOST   ',A(ICOST)                                           
         DC    CL8'OCOST   ',A(OCOST)                                           
         DC    CL8'IDPTLUSR',A(IDPTLEN)                                         
         DC    CL8'ODPTLUSR',A(ODPTLEN)                                         
         DC    CL8'IMONUSR ',A(IMON)                                            
         DC    CL8'OMONUSR ',A(OMON)                                            
         DC    CL8'ISPTUSR ',A(ISPT)                                            
         DC    CL8'HSPTUSR ',A(HSPT)                                            
         DC    CL8'IDEMUSR ',A(IDEMO)                                           
         DC    CL8'ODEMUSR ',A(ODEMO)                                           
         DC    CL8'IBYDOLU ',A(IBYDOL)                                          
         DC    CL8'OBYDOLU ',A(OBYDOL)                                          
         DC    CL8'TSTA1   ',A(TSTA1)                                           
         DC    CL8'BOXOFF  ',A(BOXOFFC)                                         
         DC    CL8'TSTA2   ',A(TSTA2)                                           
         DC    CL8'TPRD    ',A(TPRD)                                            
         DC    CL8'TEST1   ',A(TEST1)                                           
         DC    CL8'TEST2   ',A(TEST2)                                           
         DC    X'FF'                                                            
         EJECT                                                                  
* RESOLVE LITERALS                                                              
*                                                                               
RESLIT   LLC   RF,GLARGS                                                        
         BCTR  RF,0                                                             
         MVI   GLARGS+1,0                                                       
         L     R1,GLAIFLD                                                       
         LA    RE,LITTAB                                                        
         SR    R0,R0                                                            
*                                                                               
RESLIT2  CLI   0(RE),0                                                          
         BE    XIT                                                              
         CLC   GLARGS(1),1(RE)                                                  
         BNE   RESLIT4                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   3(0,RE),0(R1)                                                    
         BE    RESLIT6                                                          
*                                                                               
RESLIT4  IC    R0,0(RE)                                                         
         AR    RE,R0                                                            
         B     RESLIT2                                                          
*                                                                               
RESLIT6  LLC   RF,2(RE)                                                         
         STC   RF,GLARGS+1                                                      
         BCTR  RF,0                                                             
         LLC   R1,1(RE)                                                         
         LA    RE,3(R1,RE)                                                      
         L     R1,GLAOFLD                                                       
         EX    RF,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R1),0(RE)                                                    
*                                                                               
*                                                                               
LITTAB   DC    AL1(42),AL1(7),AL1(33),C'TPLHEAD'                                
         DC    CL33'TIMES       PROGRAMMING       LEN'                          
         DC    AL1(0)                                                           
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         CLI   GLMODE,GLINPUT      TEST INPUT MODE                              
         BNE   EXEC2                                                            
         CLI   SBMODE,SBPROCGL     YES-TEST READING GOALS NOW                   
         BNE   EXEC4                                                            
         CLI   GLRECNO,2           YES-TEST BRAND PERFORMANCE RPT               
         BNE   XIT                     NO-DON'T NEED GOALS                      
*                                                                               
EXEC2    CLI   GLMODE,GLOUTPUT     TEST OUTPUT MODE                             
         BNE   EXEC4                                                            
         MVC   OUTAREA,SPACES      YES-CLEAR OUTPUT AREA                        
*                                                                               
EXEC4    L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
* STATION I/O ROUTINES                                                          
*                                                                               
ISTA     XC    0(8,R2),0(R2)                                                    
         MVC   0(5,R2),SBSTA                                                    
         CLC   SBCBLNET,SPACES     TEST CABLE NETWORK                           
         BNH   *+10                                                             
         MVC   5(3,R2),SBCBLNET    YES                                          
         MVC   8(4,R2),SBCHAN                                                   
         MVC   12(3,R2),SBAFFIL                                                 
         MVC   15(3,R2),SBREP                                                   
         B     XIT                                                              
*                                                                               
*                                                                               
OSTATION MVC   LABLAREA(7),=C'STATION'   STATION OUTPUT ROUTINE FOR             
         MVC   CODEAREA(4),0(R2)         SALESPERSON'S TIME SCHEDULE            
         OC    5(3,R2),5(R2)             AND STATION SEQUENCE REPORTS           
         BZ    *+18                                                             
         MVI   CODEAREA+4,C'/'                                                  
         MVC   CODEAREA+5(3),5(R2)                                              
         B     OSTAT2                                                           
         LA    R6,CODEAREA+3                                                    
         CLI   3(R2),C' '                                                       
         BNH   *+8                                                              
         LA    R6,1(R6)                                                         
         MVI   0(R6),C'-'                                                       
         MVC   1(1,R6),4(R2)                                                    
         MVI   2(R6),C'M'                                                       
         CLI   4(R2),C'T'                                                       
         BE    *+12                                                             
         CLI   4(R2),C' '                                                       
         BH    OSTAT2                                                           
         MVC   1(2,R6),=C'TV'                                                   
*                                                                               
OSTAT2   MVC   SVSTA,CODEAREA      SAVE STATION CALL LETTERS                    
         LA    R6,NAMEAREA                                                      
         MVC   NAMEAREA,SPACES                                                  
         CLC   8(4,R2),SPACES                                                   
         BNH   OSTAT6                                                           
         MVC   0(3,R6),=C'CH='                                                  
         MVC   3(4,R6),8(R2)                                                    
         CLI   6(R6),C' '                                                       
         BH    OSTAT4                                                           
         BCTR  R6,0                                                             
         CLI   5(R6),C' '                                                       
         BH    OSTAT4                                                           
         BCTR  R6,0                                                             
OSTAT4   LA    R6,8(R6)                                                         
*                                                                               
OSTAT6   CLC   12(3,R2),SPACES                                                  
         BNH   OSTAT8                                                           
         MVC   0(6,R6),=C'AFFIL='                                               
         MVC   6(3,R6),12(R2)                                                   
*                                                                               
OSTAT8   MVC   0(L'OUTAREA,R3),OUTAREA                                          
         B     XIT                                                              
*                                                                               
*                                                                               
OSTA     L     R1,GLADTENT         STATION OUTPUT ROUTINE FOR TIME              
         LLC   RE,DROLEN-DROD(R1)  SCHEDULE AND BRAND PERFORMANCE RPTS          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES                                                   
         MVC   0(4,R3),0(R2)                                                    
         OC    5(3,R2),5(R2)                                                    
         BZ    OSTA2                                                            
         MVI   4(R3),C'/'                                                       
         MVC   5(3,R3),5(R2)                                                    
         LA    R6,9(R3)                                                         
         CLI   7(R2),C' '                                                       
         BH    OSTA4                                                            
         BCTR  R6,0                                                             
         B     OSTA4                                                            
*                                                                               
OSTA2    LA    R6,3(R3)                                                         
         CLI   3(R3),C' '                                                       
         BNH   *+8                                                              
         LA    R6,1(R6)                                                         
         MVI   0(R6),C'-'                                                       
         MVC   1(1,R6),4(R2)                                                    
         MVI   2(R6),C'M'                                                       
         CLI   4(R2),C'T'                                                       
         BE    *+12                                                             
         CLI   4(R2),C' '                                                       
         BH    *+10                                                             
         MVC   1(2,R6),=C'TV'                                                   
         LA    R6,4(R6)                                                         
*                                                                               
OSTA4    MVC   SVSTA,0(R3)         SAVE STATION CALL LETTERS                    
         CLC   8(4,R2),SPACES                                                   
         BNH   OSTA8                                                            
         MVC   0(3,R6),=C'CH='                                                  
         MVC   3(4,R6),8(R2)                                                    
         CLI   6(R6),C' '                                                       
         BH    OSTA6                                                            
         BCTR  R6,0                                                             
         CLI   5(R6),C' '                                                       
         BH    OSTA6                                                            
         BCTR  R6,0                                                             
OSTA6    LA    R6,8(R6)                                                         
*                                                                               
OSTA8    CLC   12(3,R2),SPACES                                                  
         BNH   OSTA10                                                           
         MVC   0(6,R6),=C'AFFIL='                                               
         MVC   6(3,R6),12(R2)                                                   
         LA    R6,10(R6)                                                        
*                                                                               
OSTA10   CLC   15(3,R2),SPACES                                                  
         BNH   XIT                                                              
         LA    R0,NREPENTS         LOOK UP REP NAME                             
         LA    R4,REPTAB                                                        
*                                                                               
OSTA12   CLI   0(R4),0                                                          
         BE    OSTA14                                                           
         CLC   0(3,R4),15(R2)                                                   
         BE    OSTA16                                                           
         LA    R4,L'REPENT(R4)                                                  
         BCT   R0,OSTA12                                                        
         DC    H'0'                                                             
*                                                                               
OSTA14   MVC   0(3,R4),15(R2)                                                   
         LA    R5,KEY              READ REP RECORD                              
         USING REPRECD,R5                                                       
         MVI   REPKEY,C'0'                                                      
         MVC   REPKEY+1(16),REPKEY                                              
         MVI   REPKTYPE,C'R'                                                    
         MVC   REPKMED,SBQMED                                                   
         MVC   REPKREP,15(R2)                                                   
         MVC   REPKAGY,AGENCY                                                   
         L     R5,AIO1                                                          
         ST    R5,AIO                                                           
         MVC   3(3,R4),15(R2)                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BNE   *+10                                                             
         MVC   3(22,R4),RNAME             EXTRACT REP NAME                      
         OC    3(22,R4),SPACES                                                  
         DROP  R5                                                               
*                                                                               
OSTA16   MVC   0(4,R6),=C'REP='                                                 
         MVC   4(22,R6),3(R4)                                                   
         B     XIT                                                              
*                                                                               
*                                                                               
TSTA1    MVC   0(4,R3),=C'*** '                                                 
         MVC   4(8,R3),SVSTA                                                    
         LA    R1,13(R3)                                                        
         CLI   SVSTA+7,C' '                                                     
         BH    TSTA12                                                           
         BCTR  R1,0                                                             
         CLI   SVSTA+6,C' '                                                     
         BH    TSTA12                                                           
         BCTR  R1,0                                                             
TSTA12   MVC   0(10,R1),=C'TOTALS ***'                                          
         B     XIT                                                              
*                                                                               
BOXOFFC  OI    GLNORBOX,X'40'      TURN BOX OFF FOR SALESPERSON'S               
         B     XIT                 DPT-LEN SUMMARY                              
*                                                                               
TSTA2    MVC   0(8,R3),SVSTA       STATION TOTAL ROUTINE FOR                    
         MVI   8(R3),C' '          SALESPERSON'S DPT-LEN SUMMARY                
         CLI   SVSTA+7,C' '                                                     
         BNH   *+14                                                             
         MVC   9(3,R3),=C'TOT'                                                  
         B     XIT                                                              
         LA    R1,8(R3)                                                         
         CLI   SVSTA+6,C' '                                                     
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVC   0(6,R1),=C'TOTALS'                                               
         B     XIT                                                              
         EJECT                                                                  
* PRODUCT INPUT ROUTINE FOR MARKET PERFORMANCE REPORT                           
*                                                                               
IPRD     MVC   0(3,R2),SBPRD                                                    
         MVC   3(3,R2),SBPRD2                                                   
         CLC   SBQPRD,=C'POL'      IF REQUEST IS FOR POL,                       
         BNE   XIT                                                              
         MVC   0(3,R2),=X'FDFDFD'  MAKE SURE ONLY POL PRINTS                    
         MVC   3(3,R2),SPACES                                                   
         B     XIT                                                              
         EJECT                                                                  
* MORE TOTAL ROUTINES                                                           
*                                                                               
TPRD     MVC   0(11,R3),=C'*** PRODUCT'                                         
         MVC   12(3,R3),SBPRD                                                   
         LA    R1,15(R3)                                                        
         CLC   SBPRD2,SPACES                                                    
         BNH   TPRD2                                                            
         CLI   SBPRD+2,C' '                                                     
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   0(R1),C'-'                                                       
         MVC   1(3,R1),SBPRD2                                                   
         LA    R1,4(R1)                                                         
         CLI   SBPRD2+2,C' '                                                    
         BH    TPRD2                                                            
         BCTR  R1,0                                                             
*                                                                               
TPRD2    MVC   1(10,R1),=C'TOTALS ***'                                          
         B     XIT                                                              
*                                                                               
*                                                                               
TEST1    MVC   0(23,R3),=C'*** ESTIMATE TOTALS ***'                             
         B     XIT                                                              
*                                                                               
TEST2    MVC   0(14,R3),=C'ESTIMATE TOTAL'                                      
         B     XIT                                                              
         EJECT                                                                  
* BUYLINE I/O ROUTINES                                                          
*                                                                               
IBUY     L     R5,SBAIO1           ADDRESS THE BUY RECORD                       
         USING BUYRECD,R5                                                       
         CLC   BUYKEY,SVBUYKEY     TEST NEW BUY KEY                             
         BE    IBUY2                                                            
         MVC   SVBUYKEY,BUYKEY                                                  
         MVC   BYEST,BUYKEST       YES-FORMAT BUY DETAILS                       
         MVC   BYLINE,BUYKBUY                                                   
         TM    BUYRCNTL,BUYRLN2    2-BYTE BUYLINE?                              
         BNZ   *+14                YES                                          
         MVI   BYLINE,0            CONVERT 1-BYTE TO 2-BYTE                     
         MVC   BYLINE+1(1),BUYKBUY                                              
         MVC   BYSTART,BDSTART                                                  
         MVC   BYEND,BDEND                                                      
         MVC   BYWKS,BDWKS                                                      
         MVC   BYDPT,BDDAYPT                                                    
         MVC   BYLEN,BDSEC                                                      
         MVC   BYDAY,BDDAY                                                      
         MVC   BYTIME,BDTIMST                                                   
         MVC   BYSEDAY,BDSEDAY                                                  
         MVC   BYPROG,BDPROGRM                                                  
         MVC   BYNOWK,BDNOWK                                                    
         MVC   BYCOST,BDCOST                                                    
         MVC   BYBOOK,RTGBOOK                                                   
         MVC   BYOVDEMS,OVDEMS                                                  
IBUY2    MVC   0(L'BUYDET,R2),BUYDET                                            
         B     XIT                                                              
         SPACE 2                                                                
OBUY     MVC   BUYDET,0(R2)                                                     
         EDIT  BYEST,(3,(R3)),FILL=0    ESTIMATE                                
         MVI   3(R3),C'-'                                                       
         EDIT  BYLINE,(5,4(R3)),FILL=0                                          
         GOTO1 DATCON,DMCB,(3,BYSTART),(4,11(R3))                               
         MVI   14(R3),C'-'                                                      
         GOTO1 (RF),(R1),(3,BYEND),(4,17(R3))                                   
         EDIT  BYWKS,(2,23(R3))                                                 
         GOTO1 DAYUNPK,DMCB,(BYSEDAY,BYDAY),(0,26(R3))                          
         EDIT  BYNOWK,(2,32(R3))                                                
         GOTO1 UNTIME,DMCB,BYTIME,35(R3)                                        
         MVC   47(1,R3),BYDPT                                                   
         LA    R4,198(R3)                                                       
         OC    BYBOOK,BYBOOK                                                    
         BZ    OBUY2                                                            
         MVC   FULL(2),BYBOOK                                                   
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(3,FULL),(6,2(R4))                                   
OBUY2    EDIT  BYLEN,(3,9(R4))                                                  
         MVC   13(17,R4),BYPROG                                                 
         EDIT  BYCOST,(10,30(R4)),2,FLOAT=$                                     
         B     XIT                                                              
         SPACE 2                                                                
HBUY     MVC   0(40,R3),=C'EST-LIN  BUY PERIOD  WKS DAY    N/W TIME'            
         MVC   47(2,R3),=C'DP'                                                  
         MVI   198(R3),C'-'                                                     
         MVC   199(48,R3),198(R3)                                               
         LA    R4,198+198(R3)                                                   
         MVC   2(4,R4),=C'BOOK'                                                 
         MVC   9(15,R4),=C'LNG PROGRAMMING'                                     
         MVC   36(4,R4),=C'COST'                                                
         B     XIT                                                              
         EJECT                                                                  
* ROUTINES FOR SALESPERSON'S TIME SCHEDULE                                      
*                                                                               
IDAY     L     R5,SBAIO1           ADDRESS THE BUY RECORD                       
         USING BUYRECD,R5                                                       
         MVC   0(1,R2),BDDAY                                                    
         MVC   1(1,R2),BDSEDAY                                                  
         B     XIT                                                              
*                                                                               
ODAY     ST    R2,DMCB                                                          
         MVC   DMCB(1),1(R2)                                                    
         GOTO1 DAYUNPK,DMCB,,(0,(R3))                                           
         B     XIT                                                              
*                                                                               
IBUYDTL  L     R5,SBAIO1                                                        
         MVC   0(4,R2),BDTIMST                                                  
         MVC   4(17,R2),BDPROGRM                                                
         MVC   21(1,R2),BDSEC                                                   
         B     XIT                                                              
*                                                                               
OBUYDTL  GOTO1 UNTIME,DMCB,(R2),(R3)                                            
         MVC   12(17,R3),4(R2)                                                  
         EDIT  (B1,21(R2)),(3,30(R3))                                           
         B     XIT                                                              
*                                                                               
ICOST    L     R5,SBAIO1                                                        
         MVC   0(3,R2),BDCOST                                                   
         MVI   3(R2),1             ENSURE SIGNIFICANT DATA                      
         B     XIT                                                              
         DROP  R5                                                               
*                                                                               
OCOST    EDIT  (3,(R2)),(10,(R3)),2,FLOAT=$                                     
         B     XIT                                                              
*                                                                               
IDPTLEN  MVC   0(4,R2),SBDPT                                                    
         MVC   4(1,R2),SBLEN                                                    
         B     XIT                                                              
*                                                                               
ODPTLEN  MVC   0(3,R3),1(R2)                                                    
         MVI   3(R3),C'-'                                                       
         EDIT  (1,4(R2)),(3,4(R3)),ALIGN=LEFT                                   
         B     XIT                                                              
*                                                                               
IMON     L     R5,SBACURCH                                                      
         USING SCHUNKD,R5                                                       
         L     R1,AMONTHS                                                       
IMON2    CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SCDATE,0(R1)                                                     
         BL    *+14                                                             
         CLC   SCDATE,2(R1)                                                     
         BNH   *+12                                                             
         LA    R1,4(R1)                                                         
         B     IMON2                                                            
         MVC   0(2,R2),2(R1)       MONTH END DATE                               
         B     XIT                                                              
         DROP  R5                                                               
*                                                                               
OMON     GOTO1 DATCON,DMCB,(2,(R2)),(6,(R3))                                    
         B     XIT                                                              
*                                                                               
IBYDOL   L     R5,SBACURCH                                                      
         USING SCHUNKD,R5                                                       
         MVC   0(4,R2),SCGROSS                                                  
         MVC   4(4,R2),SCSPOTS                                                  
         B     XIT                                                              
         DROP  R5                                                               
*                                                                               
OBYDOL   TM    GLINDS,GLTOTLIN     TEST TOTAL LINE                              
         BO    OBYDOL2             YES-FORMAT TOTAL COST                        
         SR    R0,R0                                                            
         ICM   R1,15,0(R2)         COST                                         
         BZ    OBYDOL2                                                          
         ICM   RF,15,4(R2)         SPOTS                                        
         BZ    OBYDOL2                                                          
         SLDA  R0,1                CALCULATE COST PER SPOT                      
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         ST    R1,0(R2)                                                         
OBYDOL2  MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
         EJECT                                                                  
* SPOT ROUTINES FOR BRAND TIME SCHEDULE REPORT                                  
*                                                                               
ISPT     CLI   SBMODE,SBPROCSP     CHECK READING BUYS                           
         BNE   XIT                                                              
         L     R3,SBACURCH         A(CURRENT CHUNK)                             
         USING SCHUNKD,R3                                                       
         LLC   RE,GLARGS                                                        
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         L     R1,SBADATE                                                       
         LA    R1,0(RE,R1)                                                      
         CLC   SCDATE,0(R1)                                                     
         BNE   XIT                                                              
         OC    SCSPOTS(2),SCSPOTS                                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   0(2,R2),SCSPOTS+2                                                
         B     XIT                                                              
         DROP  R3                                                               
*                                                                               
HSPT     LLC   RE,GLARGS           WEEKLY SPOT HEADLINE                         
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         L     R5,AWEEKS                                                        
         LA    R5,0(RE,R5)                                                      
         OC    0(2,R5),0(R5)                                                    
         BZ    XIT                                                              
         GOTO1 DATCON,DMCB,(2,(R5)),(4,DUB)                                     
         MVC   0(3,R3),DUB                                                      
         MVI   198(R3),C' '                                                     
         MVC   199(2,R3),DUB+3                                                  
         B     XIT                                                              
         EJECT                                                                  
* DEMO ROUTINES FOR BRAND TIME SCHEDULE                                         
*                                                                               
IDEMO    CLI   SBMODE,SBPROCSP                                                  
         BNE   XIT                                                              
         L     R3,SBACURCH                                                      
         USING SCHUNKD,R3                                                       
         LLC   RE,GLARGS                                                        
         BCTR  RE,0                                                             
         SLL   RE,3                                                             
         LA    RE,SCDEMOS(RE)                                                   
         MVC   0(4,R2),0(RE)       +0 = DEMO VALUE                              
         MVC   4(4,R2),4(RE)       +4 = EQUIV DEMO                              
         MVC   8(4,R2),SCSPOTS     +8 = SPOTS                                   
         MVC   12(4,R2),SCGROSS    +12= COST                                    
         MVC   16(4,R2),SCEGROSS   +16= EQUIV COST                              
         B     XIT                                                              
         DROP  R3                                                               
*                                                                               
ODEMO    SR    R0,R0                                                            
         L     R1,0(R2)            DEMO VALUE                                   
         XC    EBLOCK,EBLOCK                                                    
         LA    RE,FULL                                                          
         ST    RE,EBAIN                                                         
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         L     RE,GLADTENT                                                      
         LLC   RF,DROLEN-DROD(RE)                                               
         BCTR  RF,0                                                             
         STC   RF,EBLOUT                                                        
         TM    GLINDS,GLTOTLIN     TEST TOTAL LINE                              
         BZ    *+14                                                             
         MVC   FULL,0(R2)          YES-FORMAT TOTAL DEMO VALUE                  
         B     *+16                                                             
         ICM   RF,15,8(R2)         N'SPOTS                                      
         BZ    ODEMO2                                                           
         BAS   RE,DIVIDE           CALCULATE DEMO VALUE PER SPOT                
         ST    R3,EBAOUT                                                        
         MVI   EBDECS,1                                                         
         GOTO1 GLAEDITR,DMCB,EBLOCK                                             
         TM    GLINDS,GLTOTLIN     TEST TOTAL LINE                              
         BO    ODEMO2                                                           
         LLC   R1,GLARGS           NO-MOVE OVERRIDE INDICATOR (IF ANY)          
         BCTR  R1,0                                                             
         LA    R1,BYOVDEMS(R1)                                                  
         LLC   RE,EBLOUT                                                        
         LA    RE,0(RE,R3)                                                      
         MVC   0(1,RE),0(R1)                                                    
*                                                                               
ODEMO2   SR    R0,R0                                                            
         ICM   R1,15,12(R2)        TOTAL COST                                   
         BZ    XIT                                                              
         ICM   RF,15,0(R2)         DEMO VALUE                                   
         BZ    XIT                                                              
         MVI   BYTE,0                                                           
         CLI   SBSPPROF+4,C'Y'     TEST EQUIV DETAIL LINES                      
         BE    *+12                                                             
         TM    GLINDS,GLTOTLIN     OR STATION TOTAL LINE                        
         BZ    ODEMO4                                                           
         MVI   BYTE,1              YES-EQUIVALENCING                            
         CLI   SBSPPROF,C'D'       TEST EQUIV DOLLARS OR POINTS                 
         BNE   *+12                                                             
         ICM   R1,15,16(R2)                                                     
         B     ODEMO4                                                           
         ICM   RF,15,4(R2)                                                      
         BZ    XIT                                                              
*                                                                               
ODEMO4   MH    R1,=H'10'                                                        
         BAS   RE,DIVIDE           CALCULATE CPP                                
         LA    R1,198(R3)          FORMAT CPP TO NEXT LINE                      
         ST    R1,EBAOUT                                                        
         MVI   EBDECS,2                                                         
         GOTO1 GLAEDITR,DMCB,EBLOCK                                             
         CLI   BYTE,1              EQUIVALENCING INDICATOR                      
         BNE   XIT                                                              
         LLC   R1,EBLOUT                                                        
         LA    R1,198(R1,R3)                                                    
         MVI   0(R1),C'+'                                                       
         B     XIT                                                              
*                                                                               
DIVIDE   SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         ST    R1,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
* MARKET GROUP OUTPUT ROUTINES                                                  
*                                                                               
OMGR1    LA    R4,MGR1HEAD         FORMAT MGR1 HEAD TO TEMP SAVE AREA           
         MVC   0(12,R4),SBMGR1BK                                                
**NOP         MVC   13(1,R4),SBQMGRD                                            
         BAS   RE,MGRCODE                                                       
         LA    R1,SBMGR1LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLC   19(L'UNKNOWN,R4),UNKNOWN                                         
         BE    XIT                                                              
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM,SBMGR1NM                                                
         MVC   19(24,R4),SBMGR1NM                                               
         B     XIT                                                              
*                                                                               
OMGR2    LA    R4,MGR2HEAD         FORMAT MGR2 HEAD TO TEMP SAVE AREA           
         MVC   0(12,R4),SBMGR2BK                                                
**NOP         MVC   13(1,R4),SBQMGRD                                            
         BAS   RE,MGRCODE                                                       
         LA    R1,SBMGR2LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLC   19(L'UNKNOWN,R4),UNKNOWN                                         
         BE    XIT                                                              
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM,SBMGR2NM                                                
         MVC   19(24,R4),SBMGR2NM                                               
         B     XIT                                                              
*                                                                               
OMGR3    LA    R4,MGR3HEAD         FORMAT MGR3 HEAD TO TEMP SAVE AREA           
         MVC   0(12,R4),SBMGR3BK                                                
**NOP         MVC   13(1,R4),SBQMGRD                                            
         BAS   RE,MGRCODE                                                       
         LA    R1,SBMGR3LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLC   19(L'UNKNOWN,R4),UNKNOWN                                         
         BE    XIT                                                              
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM,SBMGR3NM                                                
         MVC   19(24,R4),SBMGR3NM                                               
         B     XIT                                                              
*                                                                               
OGRPCODE UNPK  DUB(5),0(3,R2)                                                   
         LLC   RF,0(R1)                                                         
         BCTR  RF,0                                                             
         LA    R1,14(R4)                                                        
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         AHI   R1,1                                                             
         EX    RF,OGRPMVC                                                       
         EX    RF,OGRPCLC                                                       
         BNE   *+10                                                             
         MVC   19(L'UNKNOWN,R4),UNKNOWN                                         
         BR    RE                                                               
*                                                                               
OGRPMVC  MVC   0(0,R1),DUB                                                      
OGRPCLC  CLC   0(0,R1),=C'9999'                                                 
UNKNOWN  DC    C'** UNKNOWN **'                                                 
*                                                                               
MGRCODE  MVI   13(R4),C'?'                                                      
         LA    RF,SPMGRTAB                                                      
         LHI   R0,(SPMGRTBX-SPMGRTAB)/L'SPMGRTAB                                
         CLC   SBQMGRD,2(RF)                                                    
         BE    MGRC10                                                           
         AHI   RF,L'SPMGRTAB                                                    
         BCT   R0,*-14                                                          
         BR    RE                                                               
*                                                                               
MGRC10   MVC   13(2,R4),0(RF)                                                   
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
* MARKET OUTPUT ROUTINE                                                         
*                                                                               
OMKT     LA    R4,MKTHEAD1         FORMAT MKT HEAD TO TEMP SAVE AREA            
         MVC   MKTHEAD1,SPACES                                                  
         MVC   0(6,R4),=C'MARKET'                                               
         MVC   7(4,R4),SBMKT                                                    
         MVC   12(L'SBMKTNM,R4),SBMKTNM                                         
         CLI   2(R2),C'O'                                                       
         BNE   *+10                                                             
         MVC   12+1+L'SBMKTNM(6,R4),=C'*ORIG*'                                  
         CLI   2(R2),C'S'                                                       
         BNE   *+10                                                             
         MVC   12+1+L'SBMKTNM(7,R4),=C'*SPILL*'                                 
         LA    R1,L'MKTHEAD1                                                    
         ST    R1,DMCB+4                                                        
         GOTO1 SQUASHER,DMCB,(R4)                                               
         MVC   MKTHEAD2,SPACES                                                  
         CLI   SBQMKTWT,C'N'                                                    
         BE    XIT                                                              
         MVC   MKTHEAD2(9),=C'COVERAGE='                                        
         ICM   R1,15,SBMKTWGT                                                   
         LA    R4,MKTHEAD2+9                                                    
         EDIT  (R1),(6,(R4)),2,ALIGN=LEFT                                       
         B     XIT                                                              
         EJECT                                                                  
* DRIVER IS ABOUT TO PUT A SORT RECORD                                          
*                                                                               
PUTSRT   CLI   REP1OPT,C'Y'        TEST BRAND PERFORMANCE REPORT                
         BNE   *+12                                                             
         CLI   GLRECNO,2                                                        
         BE    XIT                                                              
         CLI   SBMODE,SBPROCGL     NO-TEST READING GOALS NOW                    
         BE    PUTSRT2                YES-GOALS NOT NEEDED                      
         TM    ININD,INIDPTOT      TEST PROCESSING DAYPART TOTALS               
         BZ    XIT                                                              
*                                                                               
PUTSRT2  MVI   GLHOOK,GLDONT       DROP THIS SORT RECORD                        
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK FIRSTS                                                            
*                                                                               
FIRSTS   CLI   REP1OPT,C'Y'        TEST BRAND TIME SCHEDULE                     
         BNE   FIRSTS3                                                          
         CLI   GLRECNO,1                                                        
         BNE   FIRSTS2                                                          
         MVI   GLSPACE,2                                                        
         MVI   GLAUTOCH,C'N'                                                    
         OI    GLINDS,GLPALTOT     NO TOTAL SUPPRESSION                         
         MVC   TITLE,TITLE1                                                     
         LA    R5,LEVS1P                                                        
         CLI   STAOPT,C'Y'                                                      
         BNE   FIRSTS6                                                          
         LA    R5,LEVS1S                                                        
         B     FIRSTS6                                                          
*                                                                               
FIRSTS2  CLI   GLRECNO,2           TEST BRAND PERFORMANCE                       
         BNE   FIRSTS3                                                          
         MVI   GLSPACE,1                                                        
         MVI   GLAUTOCH,C'Y'                                                    
         MVC   TITLE,TITLE2                                                     
         LA    R5,LEVS2P                                                        
         CLI   STAOPT,C'Y'                                                      
         BNE   FIRSTS6                                                          
         LA    R5,LEVS2S           MARKET PERFORMANCE                           
         B     FIRSTS6                                                          
*                                                                               
FIRSTS3  MVC   TITLE,TITLE3        SALESPERSON'S TIME SCHEDULE                  
         MVI   GLSPACE,1                                                        
         MVI   GLAUTOCH,C'N'                                                    
         NI    GLINDS,FF-GLPALTOT                                               
         NI    DATAIND,FF-DIBYDEM  MAKE SURE NO DEMO TYPE HEADLINES             
         NI    DATAIND4,FF-DIDEMHED                                             
         XC    EQBASE,EQBASE                                                    
         CLI   SALESFST,C'Y'       TEST FIRST TIME                              
         BNE   *+14                                                             
         MVI   SALESFST,C'N'       YES-RESET THE PAGE TO 1                      
         MVC   PAGE,=H'1'                                                       
         MVI   BYTE,1                                                           
         CLI   REP1OPT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   BYTE,3                                                           
         CLC   GLRECNO,BYTE                                                     
         BE    FIRSTS4                                                          
         NI    GLNORBOX,FF-X'40'                                                
         LA    R5,LEVS4P                                                        
         CLI   STAOPT,C'Y'                                                      
         BNE   FIRSTS6                                                          
         LA    R5,LEVS4S                                                        
         B     FIRSTS6                                                          
*                                                                               
FIRSTS4  OI    GLNORBOX,X'40'                                                   
         LA    R5,LEVS3P                                                        
         CLI   STAOPT,C'Y'                                                      
         BNE   FIRSTS6                                                          
         LA    R5,LEVS3S                                                        
*                                                                               
FIRSTS6  CLI   GLARGS,0            TEST LEVEL 0 BREAK                           
         BNE   FIRSTSX                                                          
         BAS   RE,SETLEVS          YES-SET THE LEVEL TABLE FOR THIS RPT         
*                                                                               
FIRSTSX  B     XIT                                                              
         EJECT                                                                  
* SET THE LEVEL TABLE                                                           
* R5=A(LEVEL TABLE FOR THIS REPORT)                                             
*                                                                               
SETLEVS  LR    R0,RE                                                            
         CLI   STAOPT,C'Y'                                                      
         BE    SETLEVS4                                                         
         CLI   SBQPGRD,C' '        FOR PRODUCT SEQUENCE --                      
         BH    *+14                                                             
         XC    4(4,R5),4(R5)       SET PRODUCT GROUPS                           
         B     SETLEVS2                                                         
         CLC   SBPGR1LN,SBPGR2LN                                                
         BNE   SETLEVS2                                                         
         MVI   6(R5),0                                                          
                                                                                
SETLEVS2 CLI   SBQMGRD,0           SET MARKET GROUPS                            
         BH    *+14                                                             
         XC    12(6,R5),12(R5)                                                  
         B     SETLEVS6                                                         
         CLC   SBMGR1LN,SBMGR2LN                                                
         BNE   *+14                                                             
         XC    14(4,R5),14(R5)                                                  
         B     SETLEVS6                                                         
         CLC   SBMGR2LN,SBMGR3LN                                                
         BNE   SETLEVS6                                                         
         MVI   16(R5),0                                                         
         B     SETLEVS6                                                         
*                                                                               
SETLEVS4 CLI   SBQMGRD,0           FOR STATION SEQUENCE --                      
         BH    *+14                                                             
         XC    4(6,R5),4(R5)       SET MARKET GROUPS                            
         B     SETLEVS6                                                         
         CLC   SBMGR1LN,SBMGR2LN                                                
         BNE   *+14                                                             
         XC    6(4,R5),6(R5)                                                    
         B     SETLEVS6                                                         
         CLC   SBMGR2LN,SBMGR3LN                                                
         BNE   SETLEVS6                                                         
         MVI   8(R5),0                                                          
*                                                                               
SETLEVS6 LA    R1,LEVELS           NOW SET THE LEVEL TABLE                      
         LA    RF,1                                                             
*                                                                               
SETLEVS8 CLI   0(R5),X'FF'                                                      
         BE    SETLEVSX                                                         
         CLI   0(R5),0                                                          
         BE    SETLEVS9                                                         
         MVC   0(1,R1),0(R5)                                                    
         CLI   0(R1),QMKT                                                       
         BNE   *+8                                                              
         STC   RF,MKTLEV           MARKET LEVEL                                 
         CLI   1(R5),1                                                          
         BNE   *+8                                                              
         STC   RF,LSTHEDLV         LAST HEADLINE LEVEL                          
         CLI   1(R5),2                                                          
         BNE   *+8                                                              
         STC   RF,MIDLEV           MIDLINE LEVEL                                
         CLI   1(R5),3                                                          
         BNE   *+8                                                              
         STC   RF,INDPTLEV         LEVEL ABOVE DPTLEN                           
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
*                                                                               
SETLEVS9 LA    R5,2(R5)                                                         
         B     SETLEVS8                                                         
*                                                                               
SETLEVSX LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* LEVEL TABLES                                                                  
*                                                                               
         SPACE 1                                                                
LEVS1P   DC    AL1(QRPTSEQ,0)                                                   
         DC    AL1(QCLT,0)         HEADLINES                                    
         DC    AL1(QPRDGR1,0)                                                   
         DC    AL1(QPRDGR2,0)                                                   
         DC    AL1(QPRD,0)                                                      
         DC    AL1(QEST,0)                                                      
         DC    AL1(QMKTGR1,0)                                                   
         DC    AL1(QMKTGR2,0)                                                   
         DC    AL1(QMKTGR3,0)                                                   
         DC    AL1(QMKT,1)                                                      
         DC    AL1(QRPTSEQ,0)                                                   
         DC    AL1(QSTA,2)         MIDLINE                                      
         DC    AL1(QBUY,0)         DETAIL                                       
         DC    X'FF'                                                            
*                                                                               
LEVS1S   DC    AL1(QRPTSEQ,0)                                                   
         DC    AL1(QCLT,0)         HEADLINES                                    
         DC    AL1(QMKTGR1,0)                                                   
         DC    AL1(QMKTGR2,0)                                                   
         DC    AL1(QMKTGR3,0)                                                   
         DC    AL1(QMKT,0)                                                      
         DC    AL1(QRPTSEQ,0)                                                   
         DC    AL1(QSTA,0)                                                      
         DC    AL1(QEST,1)                                                      
         DC    AL1(QPRD,2)         MIDLINE                                      
         DC    AL1(QBUY,0)         DETAIL                                       
         DC    X'FF'                                                            
*                                                                               
LEVS2P   DC    AL1(QRPTSEQ,0)                                                   
         DC    AL1(QCLT,0)         HEADLINES                                    
         DC    AL1(QPRDGR1,0)                                                   
         DC    AL1(QPRDGR2,0)                                                   
         DC    AL1(QPRD,0)                                                      
         DC    AL1(QEST,0)                                                      
         DC    AL1(QMKTGR1,0)                                                   
         DC    AL1(QMKTGR2,0)                                                   
         DC    AL1(QMKTGR3,0)                                                   
         DC    AL1(QMKT,1)                                                      
         DC    AL1(QRPTSEQ,0)                                                   
         DC    AL1(QDPTLEN,0)      DETAIL                                       
         DC    X'FF'                                                            
*                                                                               
LEVS2S   DC    AL1(QRPTSEQ,0)                                                   
         DC    AL1(QCLT,0)         HEADLINES                                    
         DC    AL1(QMKTGR1,0)                                                   
         DC    AL1(QMKTGR2,0)                                                   
         DC    AL1(QMKTGR3,0)                                                   
         DC    AL1(QMKT,0)                                                      
         DC    AL1(QRPTSEQ,0)                                                   
         DC    AL1(QPRD,0)                                                      
         DC    AL1(QEST,1)                                                      
         DC    AL1(QDPTLEN,0)      DETAIL                                       
         DC    X'FF'                                                            
*                                                                               
LEVS3P   DC    AL1(QRPTSEQ,0)                                                   
         DC    AL1(QCLT,0)         HEADLINES                                    
         DC    AL1(QPRDGR1,0)                                                   
         DC    AL1(QPRDGR2,0)                                                   
         DC    AL1(QPRD,0)                                                      
         DC    AL1(QEST,0)                                                      
         DC    AL1(QMKTGR1,0)                                                   
         DC    AL1(QMKTGR2,0)                                                   
         DC    AL1(QMKTGR3,0)                                                   
         DC    AL1(QMKT,0)                                                      
         DC    AL1(QSTA,1)                                                      
         DC    AL1(QRPTSEQ,0)                                                   
         DC    AL1(QROT,0)         DETAIL                                       
         DC    AL1(QTIMES,0)                                                    
         DC    AL1(QBUY,0)                                                      
         DC    X'FF'                                                            
*                                                                               
LEVS3S   DC    AL1(QRPTSEQ,0)                                                   
         DC    AL1(QCLT,0)         HEADLINES                                    
         DC    AL1(QMKTGR1,0)                                                   
         DC    AL1(QMKTGR2,0)                                                   
         DC    AL1(QMKTGR3,0)                                                   
         DC    AL1(QMKT,0)                                                      
         DC    AL1(QSTA,0)                                                      
         DC    AL1(QPRD,0)                                                      
         DC    AL1(QEST,1)                                                      
         DC    AL1(QRPTSEQ,0)                                                   
         DC    AL1(QROT,0)         DETAIL                                       
         DC    AL1(QTIMES,0)                                                    
         DC    AL1(QBUY,0)                                                      
         DC    X'FF'                                                            
*                                                                               
LEVS4P   DC    AL1(QRPTSEQ,0)                                                   
         DC    AL1(QCLT,0)         HEADLINES                                    
         DC    AL1(QPRDGR1,0)                                                   
         DC    AL1(QPRDGR2,0)                                                   
         DC    AL1(QPRD,0)                                                      
         DC    AL1(QEST,0)                                                      
         DC    AL1(QMKTGR1,0)                                                   
         DC    AL1(QMKTGR2,0)                                                   
         DC    AL1(QMKTGR3,0)                                                   
         DC    AL1(QMKT,0)                                                      
         DC    AL1(QSTA,1)                                                      
         DC    AL1(QRPTSEQ,3)                                                   
         DC    AL1(QDPTLEN,0)      DETAIL                                       
         DC    AL1(QPER,0)                                                      
         DC    X'FF'                                                            
*                                                                               
LEVS4S   DC    AL1(QRPTSEQ,0)                                                   
         DC    AL1(QCLT,0)         HEADLINES                                    
         DC    AL1(QMKTGR1,0)                                                   
         DC    AL1(QMKTGR2,0)                                                   
         DC    AL1(QMKTGR3,0)                                                   
         DC    AL1(QMKT,0)                                                      
         DC    AL1(QSTA,0)                                                      
         DC    AL1(QPRD,0)                                                      
         DC    AL1(QEST,1)                                                      
         DC    AL1(QRPTSEQ,3)                                                   
         DC    AL1(QDPTLEN,0)      DETAIL                                       
         DC    AL1(QPER,0)                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
* DRIVER HEADHOOK                                                               
*                                                                               
HEAD     LA    R5,HEADTAB                                                       
         USING HEADTABD,R5                                                      
*                                                                               
HD2      CLI   0(R5),X'FF'                                                      
         BE    HD6                                                              
         CLC   HDNMGR,GLOPTS+1     N'MARKET GROUPS                              
         BE    *+12                                                             
         LA    R5,HEADTABL(R5)                                                  
         B     HD2                                                              
         SR    R2,R2                                                            
         ICM   R2,1,HDMGR1                                                      
         BZ    HD4                                                              
         LA    R3,DWIDMGR                                                       
         BAS   RE,HDPOS                                                         
         MVC   0(L'MGR1HEAD,RF),MGR1HEAD                                        
         ICM   R2,1,HDMGR2                                                      
         BZ    HD4                                                              
         BAS   RE,HDPOS                                                         
         MVC   0(L'MGR2HEAD,RF),MGR2HEAD                                        
         ICM   R2,1,HDMGR3                                                      
         BZ    HD4                                                              
         BAS   RE,HDPOS                                                         
         MVC   0(L'MGR3HEAD,RF),MGR3HEAD                                        
*                                                                               
HD4      ICM   R2,1,HDMKT                                                       
         BZ    HD6                                                              
         LA    R3,DWIDMKT1                                                      
         BAS   RE,HDPOS                                                         
         MVC   0(L'MKTHEAD1,RF),MKTHEAD1                                        
         CLI   SBQMKTWT,C'N'                                                    
         BE    HD6                                                              
         IC    R2,HDMKT                                                         
         LA    R2,1(R2)                                                         
         LA    R3,DWIDMKT2                                                      
         BAS   RE,HDPOS                                                         
         MVC   0(L'MKTHEAD2,RF),MKTHEAD2                                        
         DROP  R5                                                               
*                                                                               
HD6      B     HDX                                                              
*                                                                               
HDX      B     XIT                                                              
         SPACE 1                                                                
HDPOS    L     R1,AH4                                                           
         SH    R2,=H'4'                                                         
         BNP   *+12                                                             
         A     R1,PWIDTH                                                        
         BCT   R2,*-4                                                           
         LA    RF,0(R3,R1)                                                      
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO HOOK TO CALLING PROGRAM TO CALL DRIVER                             
*                                                                               
GODRIVER NTR1                                                                   
         L     RF,ADRIVER                                                       
         L     RE,SAVERD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* HEADLINE DISPLACEMENTS                                                        
*                                                                               
DWIDMGR  EQU   64                  WIDE-    MARKET GROUP                        
DWIDMKT1 EQU   67                           MARKET                              
DWIDMKT2 EQU   73                           MARKET COVERAGE                     
         SPACE 2                                                                
* WORKING STORAGE                                                               
*                                                                               
SAVERD   DS    F                                                                
*                                                                               
DATESW   DS    CL1                                                              
REP1OPT  DS    CL1                                                              
REP2OPT  DS    CL1                                                              
DOLOPT   DS    CL1                                                              
STAOPT   DS    CL1                                                              
SALESFST DS    CL1                                                              
OVDEMS   DS    CL3                                                              
RTGBOOK  DS    XL2                                                              
*                                                                               
TITLE1   DS    CL(L'TITLE)                                                      
TITLE2   DS    CL(L'TITLE)                                                      
TITLE3   DS    CL(L'TITLE)                                                      
*                                                                               
MGR1HEAD DS    CL44                                                             
MGR2HEAD DS    CL44                                                             
MGR3HEAD DS    CL44                                                             
MKTHEAD1 DS    CL44                                                             
MKTHEAD2 DS    CL15                                                             
*                                                                               
SVMKTIND DS    CL(L'MKTIND)                                                     
SVMAXTLV DS    CL(L'MAXTOTLV)                                                   
SVBUYKEY DS    XL(L'BUYKEY)                                                     
SVSTA    DS    CL8                                                              
*                                                                               
BUYDET   DS    0XL43                                                            
BYEST    DS    XL1                                                              
BYLINE   DS    XL2                                                              
BYSTART  DS    XL3                                                              
BYEND    DS    XL3                                                              
BYWKS    DS    XL1                                                              
BYDPT    DS    CL1                                                              
BYLEN    DS    XL1                                                              
BYDAY    DS    XL1                                                              
BYTIME   DS    XL4                                                              
BYSEDAY  DS    XL1                                                              
BYPROG   DS    XL17                                                             
BYNOWK   DS    XL1                                                              
BYBOOK   DS    XL2                                                              
BYCOST   DS    XL3                                                              
BYOVDEMS DS    CL3                                                              
*                                                                               
REPTAB   DS    0H                                                               
REPENT   DC    (NREPENTS)XL25'00'                                               
NREPENTS EQU   50                                                               
*                                                                               
PATCH    DC    XL32'00'                                                         
XFF      DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
FF       EQU   X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE DDEBLOCK                                                       
         EJECT                                                                  
* HEADLINE POSITION TABLE                                                       
*                                                                               
HEADTAB  DC    X'00',X'000000',X'05'                                            
         DC    X'01',X'050000',X'07'                                            
         DC    X'02',X'050600',X'08'                                            
         DC    X'03',X'050607',X'08'                                            
         DC    X'FF'                                                            
         SPACE 1                                                                
HEADTABD DSECT                                                                  
HDNMGR   DS    X                   N'MARKET GROUPS                              
HDMGR1   DS    X                   HEADLINE FOR MARKET GROUP 1                  
HDMGR2   DS    X                   HEADLINE FOR MARKET GROUP 2                  
HDMGR3   DS    X                   HEADLINE FOR MARKET GROUP 3                  
HDMKT    DS    X                   HEADLINE FOR MARKET                          
HEADTABL EQU   *-HEADTABD                                                       
         EJECT                                                                  
*                                                                               
T20404   CSECT                                                                  
         DS    0F                                                               
       ++INCLUDE SPMGRTAB                                                       
         EJECT                                                                  
*                                                                               
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*SPGENAGY                                                                       
*SPGENBUY                                                                       
*SPWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIE2D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'089SPWRI04   10/15/09'                                      
         END                                                                    
