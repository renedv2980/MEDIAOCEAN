*          DATA SET SPREPX502  AT LEVEL 041 AS OF 05/01/02                      
***********************************************************************         
* THIS WAS LAST CHANGED IN FEB95, AND THAT ADDED AN 'A' TO THE PHASE  *         
* CARD.  PHIL SAYS THIS JOB HASN'T RUN AT ALL THIS YEAR (AUG 2000).   *         
* I BELIEVE IT IS DEAD AND HAVEN'T RE-LINKED MY CHANGES IN.           *         
* EJOR 05SEP00                                                        *         
***********************************************************************         
*PHASE SPX502A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
         TITLE 'SPX502 - P&&G SPOT INFORMATION SYSTEM TAPE'                     
SPX502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPX502,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         LA    R8,2048(RC)                                                      
         LA    R8,2048(R8)                                                      
         LA    R7,2048(R8)                                                      
         LA    R7,2048(R7)                                                      
         USING SPX502+4096,RC,R8,R7                                             
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    SP10                                                             
         CLI   MODE,REQFRST                                                     
         BE    SP20                                                             
         CLI   MODE,CLTFRST                                                     
         BE    SP35                                                             
         CLI   MODE,ESTFRST                                                     
         BE    SP40                                                             
         CLI   MODE,MKTFRST                                                     
         BE    SP50                                                             
         CLI   MODE,STAFRST                                                     
         BE    SP55                                                             
         CLI   MODE,PROCBUY                                                     
         BE    SP60                                                             
         CLI   MODE,PROCGOAL                                                    
         BE    SP80                                                             
         CLI   MODE,RUNLAST                                                     
         BE    SP100                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DS    A                                                                
         EJECT                                                                  
* RUNFRST                                                                       
*                                                                               
SP10     XC    SPOTBUY,SPOTBUY                                                  
         XC    SPOTGOAL,SPOTGOAL                                                
         MVI   SORTOPEN,C'N'                                                    
         MVI   DATATYPE,0                                                       
         MVI   RPTOPT,0                                                         
         MVI   TAPOPT,0                                                         
*                                                                               
         L     R1,ACOMFACS         CALLOV SIMULATION                            
         LA    R1,CCALLOV-COMFACSD(R1)                                          
         MVC   0(4,R1),ACALLOV                                                  
         B     EXIT                                                             
         EJECT                                                                  
* REQFRST                                                                       
*                                                                               
SP20     LA    R1,PGAGYTAB         DETERMINE THE PG AGENCY CODE                 
*                                                                               
SP21     CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                INVALID AGENCY                               
         CLC   QAGY,0(R1)                                                       
         BE    *+12                                                             
         LA    R1,6(R1)                                                         
         B     SP21                                                             
         MVC   PGAGY,2(R1)                                                      
*                                                                               
         LA    R1,DTTABLE          QOPT2 = DATA TYPE                            
*                                                                               
SP22     CLI   0(R1),0                                                          
         BE    SP28                                                             
         CLC   QOPT2,0(R1)                                                      
         BE    *+12                                                             
         LA    R1,3(R1)                                                         
         B     SP22                                                             
         CLI   DATATYPE,0          TEST FIRST REQUEST                           
         BE    *+14                                                             
         CLC   DATATYPE,1(R1)      NO-TEST DATA TYPE SAME AS LAST REQ           
         BNE   SP26                   NO-ERROR                                  
         MVC   DATATYPE,1(R1)                                                   
         MVC   RCSUBPRG,2(R1)                                                   
*                                                                               
         MVI   FCRDBUYS,C'Y'       READ BUYS                                    
         MVI   FCRDGOAL,C'Y'       READ GOALS                                   
         CLI   DATATYPE,DTEST      TEST DATA TYPE = ESTIMATED SPENDING          
         BNE   SP23                NO                                           
         MVI   FCRDBUYS,C'N'       YES-SUPPRESS READING BUYS                    
         OC    SPOTGOAL,SPOTGOAL   TEST V(SPOTGOAL) SET YET                     
         BNZ   SP25                                                             
         LA    R3,SPOTGOAL         NO-GET ITS ADDRESS                           
         MVI   DMCB+7,QSPOTGL                                                   
         B     SP24                                                             
*                                                                               
SP23     MVI   FCRDGOAL,C'N'       SUPPRESS READING GOALS                       
         OC    SPOTBUY,SPOTBUY     TEST V(SPOTBUY) SET YET                      
         BNZ   SP25                                                             
         LA    R3,SPOTBUY          NO-GET ITS ADDRESS                           
         MVI   DMCB+7,QSPOTBUY                                                  
*                                                                               
SP24     MVC   DMCB+4(3),=X'D9000A'                                             
         GOTO1 ACALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R3),0(R1)                                                    
*                                                                               
SP25     MVI   PROCESS,PRADD       SET PROCESS TYPE TO ADD                      
         MVI   RQNOPSSV,C'Y'       DON'T READ PASSIVE POINTERS                  
         CLC   QPRD,=C'ALL'        UNLESS IT'S A SINGLE PRODUCT REQUEST         
         BE    SP25A                                                            
         CLC   QPRD,=C'POL'                                                     
         BE    SP25A                                                            
         MVI   RQNOPSSV,C'N'                                                    
*                                                                               
SP25A    CLI   DATATYPE,DTEST      EXCEPT GOT GOALS,                            
         BE    *+8                                                              
         MVI   RQALLPOL,C'Y'       INCLUDE POL IN 'ALL' REQUEST                 
         MVI   FORCEHED,C'Y'                                                    
         CLI   RPTOPT,C'Y'         REPORT OPTION                                
         BE    *+10                                                             
         MVC   RPTOPT,QOPT3                                                     
         CLI   TAPOPT,0            TAPE OPTION                                  
         BE    *+14                                                             
         CLC   TAPOPT,QOPT1                                                     
         BNE   SP27                                                             
         MVC   TAPOPT,QOPT1                                                     
         CLI   TAPOPT,C'N'         TEST SUPPRESS TAPE                           
         BE    SP30                                                             
         CLI   DCBOPEN,C'Y'        TEST ALREADY OPEN                            
         BE    SP30                                                             
*                                                                               
         MVI   DCBOPEN,C'Y'                                                     
         MVC   DSNAME+13(2),QAGY                                                
         GOTO1 DYNALLOC,DMCB,DDNAME,DSNAME                                      
         LA    R2,PGTAPE                                                        
         OPEN  ((R2),(OUTPUT))                                                  
         B     SP30                                                             
*                                                                               
DDNAME   DC    CL8'PGTAPE'                                                      
DSNAME   DC    CL20'SPTTAPE.SP0X5AA1'                                           
DCBOPEN  DC    C'N'                                                             
*                                                                               
SP26     MVC   P+33(22),=C'INCONSISTENT DATA TYPE'                              
         B     SP29                                                             
*                                                                               
SP27     MVC   P+33(24),=C'INCONSISTENT TAPE OPTION'                            
         B     SP29                                                             
*                                                                               
SP28     MVC   P+33(17),=C'INVALID DATA TYPE'                                   
*                                                                               
SP29     MVI   MODE,REQLAST                                                     
         MVC   P(19),=C'**WARNING** REQUEST'                                    
         UNPK  P+20(2),RCRQTOT                                                  
         OI    P+21,X'F0'                                                       
         MVC   P+23(9),=C'IGNORED -'                                            
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
* BUILD TABLE OF EST CODES                                                      
*                                                                               
SP30     L     R1,AESTTAB                                                       
         LA    R0,220                                                           
SP31     XC    0(256,R1),0(R1)     CLEAR 512 BYTES FOR EACH BRAND               
         XC    256(256,R1),256(R1)                                              
         LA    R1,512(R1)                                                       
         BCT   R0,SP31                                                          
*                                                                               
         L     R6,ADCLT                                                         
         MVC   KEY(13),0(R6)                                                    
         MVI   KEY+4,C'A'          FORCE PAST CLTHDR                            
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE      A-M/C                                        
         BNE   EXIT                                                             
*                                                                               
SP32     GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   SP34                                                             
*                                                                               
         CLC   =C'POL',KEY+4       SKIP POL ESTIMATES                           
         BNE   *+14                                                             
         MVC   KEY+7(5),=5X'FF'    FORCE NEXT PRD                               
         B     SP32                                                             
*                                                                               
         CLI   KEY+7,0             TEST EST                                     
         BE    SP33                NO                                           
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   SP33                YES                                          
         L     R6,ADEST                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         USING ESTHDRD,R6                                                       
*                                                                               
         CLI   EPRDCD+1,220        MAKE SURE PRD CODE VALID                     
         BNH   *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,EPRDCD+1         GET PRD NUM                                  
         BCTR  RE,0                                                             
         SLL   RE,9                                                             
         ZIC   R0,7(R6)            EST NUM                                      
         BCTR  R0,0                                                             
         AR    R0,R0               X 2                                          
         AR    RE,R0                                                            
         L     RF,AESTTAB                                                       
         LA    RE,0(RE,RF)                                                      
         MVC   0(2,RE),EDESC       SAVE FIRST 2 CHAR OF NAME                    
*                                                                               
SP33     MVC   KEY+8(5),=5X'FF'                                                 
         B     SP32                                                             
*                                                                               
SP34     B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* CLTFRST                                                                       
*                                                                               
SP35     L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         MVI   SERVICE,C'N'        SET THE RATING SERVICE                       
         CLI   CPROF+3,C'0'                                                     
         BE    *+8                                                              
         MVI   SERVICE,C'A'                                                     
*                                                                               
         LA    RE,SBLOCK           INITIALIZE SPOT BLOCK                        
         LH    RF,=Y(SBLOCKX-SBLOCK)                                            
         XCEF                                                                   
         MVC   SBUSERID,RCORIGID                                                
         MVC   SBAGY,QAGY                                                       
         MVC   SBCLT,QCLT                                                       
         MVC   SBBCLT,BCLT                                                      
         MVC   SBQPRD,QPRD                                                      
         MVC   SBAIO1,ADBUY                                                     
         CLI   DATATYPE,DTEST                                                   
         BNE   *+10                                                             
         MVC   SBAIO1,ADGOAL                                                    
         MVC   SBCOMFAC,ACOMFACS                                                
         MVC   SBDPEQTB,DPEQTAB                                                 
         MVC   SBEQTAB,EQTAB                                                    
         MVC   SBBTODAY,TODAYP                                                  
         L     R1,ADAGY                                                         
         MVC   SBAGYREC,0(R1)                                                   
         MVC   SBCPROF,CPROF                                                    
         MVC   SBCEXTRA,CEXTRA                                                  
         MVC   SBSPPROF,SPOTPROF                                                
         BAS   RE,GETPROFS                                                      
         L     R1,ASPOTTAB                                                      
         ST    R1,SBASPTTB                                                      
         MVC   SBLSPTTB,=F'12000'                                               
         L     R1,ACHUNK                                                        
         ST    R1,SBACHUNK                                                      
         L     R1,ABOOKLST                                                      
         ST    R1,SBABKLST                                                      
         MVI   NDEMOS,4            GET 4 DEMOS                                  
         CLI   DATATYPE,DTEST      TEST ESTIMATED SPOT REQUEST                  
         BE    SP36                YES-NO DEMOS                                 
         MVI   SBEDEMTY,C'P'       SPOT SCHEDULE REQUEST NEEDS                  
         CLI   DATATYPE,DTSKD      PURCHASED DEMOS                              
         BE    SP36                SPOT PERFORMANCE REQUEST NEEDS               
         MVI   SBEDEMTY,C'A'       AFFID RERATE DEMO                            
         MVI   NDEMOS,6            AND 6 DEMOS                                  
*                                                                               
SP36     MVC   SBQBOOK,QBOOK1      BOOK OPTION                                  
         CLC   SBQBOOK,SPACES                                                   
         BH    *+10                                                             
         MVC   SBQBOOK(3),=C'ACT'                                               
         CLC   QHUT1,SPACES        SVI OPTION                                   
         BNH   SP37                                                             
         MVI   SBESVI,X'FF'                                                     
         CLC   QHUT1,=C'NO'                                                     
         BE    SP37                                                             
         PACK  DUB,QHUT1                                                        
         CVB   R1,DUB                                                           
         STC   R1,SBESVI                                                        
*                                                                               
SP37     MVI   SBEBYPRD,C'Y'       BREAK OUT ALLOCATED PRODUCTS                 
         MVI   SBEBYSLN,C'Y'       BREAK OUT SPOT LENGTHS                       
*                                                                               
         MVI   SBQBPRD,0                                                        
         CLC   QPRD,=C'ALL'        TEST SINGLE PRODUCT REQUEST                  
         BE    SP39                                                             
         CLC   QPRD,=C'POL'                                                     
         BE    SP39                                                             
         LA    R1,CLIST            YES-FIND ITS CODE                            
         LA    R0,220                                                           
SP38     CLI   0(R1),C' '                                                       
         BH    *+6                                                              
         DC    H'0'                                                             
         CLC   QPRD,0(R1)                                                       
         BE    *+14                                                             
         LA    R1,4(R1)                                                         
         BCT   R0,SP38                                                          
         DC    H'0'                                                             
         MVC   SBQBPRD,3(R1)       SET PRODUCT FILTER                           
*                                                                               
SP39     B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* GET D0 AND 1W PROFILES                                                        
*                                                                               
GETPROFS NTR1                                                                   
         XC    WORK,WORK           SET UP 1W PROFILE KEY                        
         MVC   WORK(4),=C'S0D0'                                                 
         MVC   WORK+4(3),SVAGY                                                  
         MVC   WORK+7(3),QCLT                                                   
         L     RF,ADCLT            CHECK IF CLIENT HAS OFFICE                   
         USING CLTHDR,RF                                                        
         CLI   COFFICE,X'41'                                                    
         BL    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         DROP  RF                                                               
         GOTO1 GETPROF,DMCB,WORK,SBD0PROF,DATAMGR                               
         MVC   WORK(4),=C'S01W'                                                 
         GOTO1 (RF),(R1),WORK,SB1WPROF,DATAMGR                                  
         B     EXIT                                                             
         EJECT                                                                  
* ESTFRST                                                                       
*                                                                               
SP40     GOTO1 DATCON,DMCB,QSTART,(3,BQSTART)                                   
         GOTO1 (RF),(R1),,(2,BQSTARTP)                                          
         GOTO1 (RF),(R1),QEND,(3,BQEND)                                         
         GOTO1 (RF),(R1),,(2,BQENDP)                                            
*                                                                               
         MVC   SBDPTTAB(L'DPTTAB),DPTTAB   SET DAYPART TABLE                    
*                                                                               
         MVI   PGERR,0             RESET ERROR FLAG                             
*                                                                               
         L     R6,ADBUY                                                         
         GOTO1 MOBILE,(R1),(25,QSTART),(0,(R6))  GET LIST OF BROADCAST          
         GOTO1 DATCON,(R1),(2,2(R6)),(3,FULL)                MONTHS             
         ZIC   RE,FULL                                                          
         CLI   FULL+1,6            TEST START MONTH JAN-JUN                     
         BH    *+6                 NO                                           
         BCTR  RE,0                YES-BACK UP ONE YEAR FOR START YEAR          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  YEAR(2),DUB                                                      
         MVI   YEAR+2,C'-'                                                      
         GOTO1 GETBROAD,(R1),QEND,WORK  GET BROADCAST MONTH OF END DATE         
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,(R1),WORK+6,(3,FULL)  GET END MONTH                       
         ZIC   RE,FULL                                                          
         CLI   FULL+1,6            TEST END MONTH JAN-JUN                       
         BNH   *+8                 YES                                          
         LA    RE,1(RE)            NO-ADD ONE YEAR FOR END YEAR                 
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  YEAR+3(2),DUB                                                    
*                                                                               
SP41     XC    QTRTAB,QTRTAB                                                    
         XC    DATES,DATES                                                      
         LA    R2,QTRTAB                                                        
         LR    R3,R6                                                            
         LA    R4,DATES                                                         
         SR    R5,R5                                                            
         MVI   BYTE,0                                                           
*                                                                               
SP42     CLI   0(R3),X'FF'         BUILD DATES LIST                             
         BE    SP46                                                             
         GOTO1 DATCON,(R1),(2,2(R3)),(3,FULL)                                   
         ZIC   RF,FULL+1                                                        
         BCTR  RF,0                                                             
         SR    RE,RE                                                            
         D     RE,=F'3'                                                         
         CLI   BYTE,0                                                           
         BE    *+10                                                             
         LTR   RE,RE                                                            
         BNZ   SP43                                                             
         MVI   BYTE,1                                                           
         SLL   RF,2                                                             
         LA    RF,PGQTRS(RF)                                                    
         MVC   0(4,R2),0(RF)                                                    
         LA    R2,4(R2)                                                         
         MVC   0(2,R4),0(R3)                                                    
         B     SP44                                                             
*                                                                               
SP43     CH    RE,=H'2'            TEST QUARTER END                             
         BNE   SP44                                                             
         MVC   2(2,R4),2(R3)                                                    
         LA    R4,4(R4)            NEXT QTR                                     
         LA    R5,1(R5)                                                         
*                                                                               
SP44     LA    R3,4(R3)            NEXT MONTH                                   
         B     SP42                                                             
*                                                                               
SP46     OC    0(4,R4),0(R4)       TEST INCOMPLETE LAST QTR                     
         BZ    SP48                                                             
         OC    2(2,R4),2(R4)                                                    
         BNZ   SP48                                                             
         SH    R3,=H'2'                                                         
         MVC   2(2,R4),0(R3)                                                    
         LA    R5,1(R5)                                                         
*                                                                               
SP48     LA    R1,DATES                                                         
         ST    R1,SBADATE                                                       
         ST    R5,SBNDATES                                                      
*                                                                               
         CLI   DATATYPE,DTEST      TEST DEMOS NEEDED                            
         BE    SP49                                                             
         GOTO1 MEDPRDRD,DMCB,(RA)  YES                                          
*                                                                               
SP49     B     EXIT                                                             
         EJECT                                                                  
* MKTFRST                                                                       
*                                                                               
SP50     CLI   DATATYPE,DTEST      TEST READING GOAL RECORDS                    
         BNE   EXIT                                                             
         L     R6,ADMARKET         YES-GET THE RATING SERVICE MARKET            
         USING MKTRECD,R6                                                       
******** NI    PGERR,PERDEM        RESET ALL ERROS BUT INVALID DEMO             
         MVI   PGERR,0                                                          
         MVC   MKTCD,=C'000'                                                    
         MVI   BYTE,C'0'                                                        
         CLI   SERVICE,C'N'                                                     
         BE    *+8                                                              
         MVI   BYTE,C'1'                                                        
         LA    R1,MKTRSM1                                                       
         CLC   MKTRS1,BYTE                                                      
         BE    SP51                                                             
         LA    R1,MKTRSM2                                                       
         CLC   MKTRS2,BYTE                                                      
         BNE   SP52                                                             
*                                                                               
SP51     LH    R0,0(R1)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKTCD,DUB                                                        
*                                                                               
SP52     CLC   MKTCD,=C'000'                                                    
         BH    EXIT                                                             
         OI    PGERR,PERMKT                                                     
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* STAFRST                                                                       
*                                                                               
SP55     DS    0H                                                               
******** NI    PGERR,PERDEM        RESET ALL ERROS BUT INVALID DEMO             
         MVI   PGERR,0                                                          
         L     R6,ADBLOCK                                                       
         USING DBLOCKD,R6                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBFUNCT,DBVLST                                                   
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELSRC,SERVICE                                                 
         MVI   DBSELMED,C'T'                                                    
         MVC   DBAREC,ADCOMREC     PASS I/O AREA ADDRESS                        
         L     R2,ADBUY                                                         
         LA    R2,BUYMSTA-BUYREC(R2)                                            
         GOTO1 MSUNPK,DMCB,(R2),WORK,WORK+4                                     
         MVC   DBSELSTA(4),WORK+4                                               
*                                                                               
         GOTO1 DEMAND,DMCB,ADBLOCK,0                                            
*****    CLI   DBERROR,0                                                        
*****    BE    *+8                                                              
*****    OI    PGERR,PERMKT        SET ERROR FLAG                               
         LH    R0,DBACTRMK                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKTCD,DUB                                                        
         L     R1,ADMARKET         MOVE MARKET RECORD TO SBLOCK                 
         MVC   SBMKTREC,0(R1)                                                   
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* PROCBUY                                                                       
*                                                                               
SP60     L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
******** NI    PGERR,PERMKT+PERDEM RESET ALL BUT INVALID MARKET & DEMO          
         NI    PGERR,PERMKT        RESET ALL BUT INVALID MARKET                 
         OC    BDCOST,BDCOST       TEST COST=0                                  
         BNZ   *+12                                                             
         TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         BZ    SP60A                                                            
         TM    BDCIND2,BDC2NEG     TEST NEGATIVE                                
         BO    EXIT                                                             
         B     SP60B                                                            
SP60A    TM    BDCIND,X'01'        YES-TEST NEGATIVE AS WELL                    
         BO    EXIT                YES-SKIP TRAFFIC REF BUYS                    
SP60B    LA    R1,KEY                                                           
         CLC   BUYMSTA(2),BUYMSTA-BUYKEY(R1)  SKIP SPILL                        
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,GETDPT           GET THE DAYPART CODE                         
         MVC   SBBMKT,KEY+4        SET KEY MARKET                               
         MVC   SBEMKT,BUYMSTA      SET RECORD MARKET                            
         GOTO1 MSUNPK,DMCB,BUYMSTA,WORK,WORK+4                                  
         MVC   SBSTA,WORK+4        SET STATION                                  
*                                                                               
         BAS   RE,GETPRDS          GET PRODUCTS ALLOCATED TO THIS BUY           
         LA    R5,PRDLST                                                        
         MVI   NPRD,1                                                           
*                                                                               
SP62     SR    RE,RE                                                            
         ICM   RE,1,0(R5)                                                       
         BZ    EXIT                                                             
         STC   RE,SBEPRD           SET PRODUCT FILTER                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     R3,PRDBUFF                                                       
         LA    R3,28(RE,R3)        R3=A(DEMOS FOR THIS PRODUCT)                 
         BAS   RE,DEMLIST          SET THE DEMO LIST                            
         CLC   DEMNAME1,SPACES     TEST DEMO ERROR                              
         BH    *+8                                                              
         OI    PGERR,PERDEM                                                     
         CLI   NOSECDEM,C'Y'       TEST SECONDARY DEMO                          
         BE    SP64                                                             
         CLC   DEMNAME2,SPACES     YES-TEST 2ND DEMO NAME FOUND                 
         BH    SP64                                                             
         MVI   NOSECDEM,C'Y'           NO-FORCE NO 2ND DEMO                     
         ZIC   R1,SBENDEM                                                       
         BCTR  R1,0                                                             
         STC   R1,SBENDEM                                                       
         LA    R1,SBEDEMOS+3              SHUFFLE THE DEMOS DOWN                
         MVC   0(3,R1),3(R1)                                                    
         LA    R1,3(R1)                                                         
         OC    0(3,R1),0(R1)                                                    
         BNZ   *-16                                                             
*                                                                               
SP64     DS    0H                                                               
         GOTO1 SPOTBUY,DMCB,SBLOCK   ** CALL SPOTBUY **                         
*                                                                               
         CLI   DATATYPE,DTSKD      TEST SPOT SCHEDULE REQUEST                   
         BNE   SP70                                                             
         L     R2,SBACHUNK                                                      
         USING SCHUNKD,R2                                                       
*                                                                               
SP66     OC    SCNEXT,SCNEXT       TEST END OF CHUNKS                           
         BZ    SP76                                                             
         OC    SCSPOTS,SCSPOTS     TEST ANY SPOTS                               
         BZ    SP68                                                             
         CLI   SCPRD1,X'FF'        REJECT PRD=POL                               
         BE    SP68                                                             
         BAS   RE,SCHEDULE         BUILD SORT RECORD                            
*                                                                               
SP68     L     R2,SCNEXT           NEXT CHUNK                                   
         B     SP66                                                             
         DROP  R2                                                               
*                                                                               
SP70     CLI   DATATYPE,DTPER      TEST SPOT PERFORMANCE REQUEST                
         BNE   SP76                                                             
         L     R2,SBASPTTB         R2=A(SPOT TABLE)                             
         L     R3,SBNSPTEN         R3=N'ENTRIES                                 
         L     R4,SBLSPTEN         R4=L'ENTRY                                   
         LTR   R3,R3                                                            
         BZ    SP76                                                             
         USING SPTTABD,R2                                                       
*                                                                               
SP72     TM    SPTIND,SPTDUMMY     TEST EXCLUDE                                 
         BO    SP74                                                             
         OC    SPTADATE(4),SPTADATE  TEST AFFIDAVIT DATE/TIME                   
         BZ    SP74                                                             
         BAS   RE,PERFORM          BUILD A SORT RECORD                          
*                                                                               
SP74     LA    R2,0(R4,R2)                                                      
         BCT   R3,SP72                                                          
*                                                                               
SP76     LA    R5,1(R5)            NEXT PRODUCT                                 
         ZIC   R1,NPRD                                                          
         LA    R1,1(R1)                                                         
         STC   R1,NPRD                                                          
         CLI   NPRD,L'PRDLST                                                    
         BNH   SP62                                                             
         B     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
* PROCGOAL                                                                      
*                                                                               
SP80     L     R6,ADGOAL                                                        
         USING GOALREC,R6                                                       
******** NI    PGERR,PERMKT+PERDEM RESET ALL BUT INVALID MARKET & DEMO          
         NI    PGERR,PERMKT        RESET ALL BUT INVALID MARKET                 
         CLI   DATATYPE,DTEST      MAKE SURE ESTIMATED SPOT REQUEST             
         BNE   EXIT                                                             
         GOTO1 SPOTGOAL,DMCB,SBLOCK   CALL SPOTGOAL                             
         L     R2,SBACHUNK                                                      
         USING SGLCHNKD,R2                                                      
*                                                                               
SP82     OC    SGNEXT,SGNEXT       TEST END OF CHUNKS                           
         BZ    EXIT                                                             
         CLI   SGPRD1,X'FF'        REJECT PRD=POL                               
         BE    *+8                                                              
         BAS   RE,SPEND            BUILD SORT RECORD                            
         L     R2,SGNEXT           NEXT CHUNK                                   
         B     SP82                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
* BUILD A LIST OF PRODUCTS ALLOCATED TO A BUY RECORD                            
* PRODUCTS ARE RETURNED IN PRDLST                                               
*                                                                               
GETPRDS  NTR1  ,                                                                
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         XC    PRDLST,PRDLST                                                    
         LA    R1,BDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
GETPRD2  CLI   0(R1),0                                                          
         BE    GETPRDX                                                          
         CLI   0(R1),11                                                         
         BL    GETPRD8                                                          
         CLI   0(R1),13                                                         
         BH    GETPRD8                                                          
         USING REGELEM,R1                                                       
         CLI   RLEN,10                                                          
         BNH   GETPRD8                                                          
         LA    R2,1                                                             
         LA    R3,RPPRD                                                         
         CLI   RLEN,14                                                          
         BNH   GETPRD3                                                          
         LA    R2,2                                                             
*                                                                               
GETPRD3  CLI   SBQBPRD,0           TEST SINGLE PRODUCT REQUEST                  
         BE    *+14                                                             
         CLC   SBQBPRD,0(R3)       YES-FILTER ON IT                             
         BNE   GETPRD6                                                          
         LA    RE,PRDLST                                                        
         LA    RF,L'PRDLST                                                      
*                                                                               
GETPRD4  CLI   0(RE),0                                                          
         BNE   *+14                                                             
         MVC   0(1,RE),0(R3)                                                    
         B     GETPRD6                                                          
         CLC   0(1,R3),0(RE)                                                    
         BE    GETPRD6                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,GETPRD4                                                       
         DC    H'0'                                                             
*                                                                               
GETPRD6  LA    R3,4(R3)                                                         
         BCT   R2,GETPRD3                                                       
         DROP  R1                                                               
*                                                                               
GETPRD8  IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GETPRD2                                                          
*                                                                               
GETPRDX  B     EXIT                                                             
         EJECT                                                                  
* SET THE DEMO LIST                                                             
* INPUT  : R3=A(DEMOS FOR THIS PRODUCT)                                         
* OUTPUT : DEMO1 = PRIMARY IMP                                                  
*          DEMO2 = SECONDARY IMP                                                
*          DEMO3 = HOMES                                                        
*          DEMO4 = RHOMES                                                       
*          DEMO5 = W18+                                                         
*          DEMO6 = W1849                                                        
*                                                                               
DEMLIST  NTR1                                                                   
         XC    SBEDEMOS,SBEDEMOS                                                
         MVC   SBENDEM,NDEMOS                                                   
         MVI   NOSECDEM,C'N'                                                    
         LA    R2,SBEDEMOS                                                      
         LA    R0,20                                                            
*                                                                               
DEML2    OC    0(3,R3),0(R3)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   0(3,R2),0(R3)                                                    
         MVI   1(R2),C'I'                                                       
         CLI   1(R3),C'I'                                                       
         BE    DEML4                                                            
         CLI   1(R3),C'R'                                                       
         BE    DEML4                                                            
         CLI   1(R3),C'S'                                                       
         BE    DEML4                                                            
         LA    R3,3(R3)                                                         
         BCT   R0,DEML2                                                         
         DC    H'0'                                                             
*                                                                               
DEML4    LA    R2,3(R2)                                                         
         CLI   QOPT4,C'Y'          ONLY GET SECONDARY DEMO IF NCT               
         BNE   DEML10              OPTION IS ON                                 
         LR    R4,R3                                                            
         B     DEML8                                                            
*                                                                               
DEML6    OC    0(3,R4),0(R4)                                                    
         BZ    DEML10                                                           
         MVC   0(3,R2),0(R4)                                                    
         MVI   1(R2),C'I'                                                       
         CLI   1(R4),C'I'                                                       
         BE    *+12                                                             
         CLI   1(R4),C'R'                                                       
         BNE   DEML8                                                            
         CLC   2(1,R4),2(R3)       CHECK 2ND DEMO DIFFERENT FROM 1ST            
         BE    DEML8                                                            
         LA    R2,3(R2)                                                         
         B     DEML12                                                           
*                                                                               
DEML8    LA    R4,3(R4)                                                         
         BCT   R0,DEML6                                                         
*                                                                               
DEML10   MVI   NOSECDEM,C'Y'       NO SECONDARY IMP                             
         CLI   DATATYPE,DTSKD      TEST SPOT SCHEDULE REQUEST                   
         BNE   DEML12                                                           
         ZIC   RE,SBENDEM          YES-ONE LESS DEMO                            
         BCTR  RE,0                                                             
         STC   RE,SBENDEM                                                       
*                                                                               
DEML12   MVC   0(3,R2),=X'00C901'  HOMES                                        
         MVC   3(3,R2),=X'00D901'  RHOMES                                       
         CLI   DATATYPE,DTPER      TEST SPOT PERFORMANCE REQUEST                
         BNE   DEML14                                                           
         MVC   6(3,R2),=X'00C92D'  YES- W18+                                    
         MVC   9(3,R2),=X'00C92A'       W1849                                   
*                                                                               
DEML14   LA    R0,2                GET DEMO NAMES AND DESCRIPTIONS              
         CLI   NOSECDEM,C'Y'                                                    
         BNE   *+6                                                              
         BCTR  R0,0                                                             
         LA    R1,SBEDEMOS                                                      
******** CLI   DATATYPE,DTPER                                                   
******** BNE   *+8                                                              
******** LA    R1,DEMOS                                                         
         LA    R2,DEMDESCS                                                      
         LA    RF,DEMNAMES                                                      
         MVC   DEMDESCS,SPACES                                                  
         MVC   DEMNAMES,SPACES                                                  
*                                                                               
DEML16   LA    RE,DEMTAB                                                        
*                                                                               
DEML18   CLI   0(RE),0                                                          
         BE    DEML20                                                           
         CLC   2(1,R1),0(RE)                                                    
         BE    *+12                                                             
         LA    RE,20(RE)                                                        
         B     DEML18                                                           
         MVC   0(5,RF),1(RE)                                                    
         MVC   0(14,R2),6(RE)                                                   
*                                                                               
DEML20   LA    R1,3(R1)                                                         
         LA    R2,14(R2)                                                        
         LA    RF,6(RF)                                                         
         BCT   R0,DEML16                                                        
*                                                                               
DEMLX    B     EXIT                                                             
         EJECT                                                                  
* BUILD A SPOT SCHEDULE SORT RECORD FROM A SPOTBUY EXTRACT CHUNK                
* R2=A(CHUNK)                                                                   
*                                                                               
SCHEDULE NTR1                                                                   
         USING SCHUNKD,R2                                                       
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         XC    SORTREC,SORTREC                                                  
         LA    R3,SORTREC                                                       
         USING SORTRECD,R3                                                      
         MVI   SKSEQ,1                                                          
         MVC   SKYEAR,YEAR         YEAR                                         
         L     R0,SBNDATES                                                      
         LA    R1,QTRTAB           QUARTER                                      
         LA    RE,DATES                                                         
*                                                                               
SKED2    CLC   SCDATE,0(RE)                                                     
         BL    *+14                                                             
         CLC   SCDATE,2(RE)                                                     
         BNH   SKED4                                                            
         LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,SKED2                                                         
         DC    H'0'                                                             
*                                                                               
SKED4    MVC   SKQTR,0(R1)                                                      
*                                                                               
         MVC   BYTE,SCPRD1         PRODUCT                                      
         BAS   RE,GETPROD                                                       
         MVC   SKSPRD,PROD                                                      
         GOTO1 GETPURP,BUYKEST     PURPOSE CODE                                 
         MVC   SKSPUR,PURP                                                      
         MVC   BYTE,SCSLN1                                                      
         BAS   RE,GETLEN           SPOT LENGTH                                  
         MVC   SKSLEN,FULL                                                      
         MVC   SKSDPT,DAYPART      DAYPART                                      
         CLC   SKSPUR,=C'SPRT'                                                  
         BNE   *+10                                                             
         MVC   SKSDPT,=C'SPRT'                                                  
         MVC   SKSSTA,STA          STATION                                      
         MVC   SKSDEM1,DEMNAMES    DEMO NAME 1                                  
         MVC   SKSDEM2,DEMNAMES+6  DEMO NAME 2                                  
*                                                                               
         MVC   SDSPTS,SCSPOTS      N'SPOTS                                      
         L     RE,SCGROSS          GROSS DOLLARS                                
         CVD   RE,DUB                                                           
         ZAP   SDDOL,DUB                                                        
         MVC   SDIMP1,SCDEMOS      DEMO 1                                       
         LA    R1,SCDEMOS+8                                                     
         CLI   NOSECDEM,C'Y'                                                    
         BE    *+14                                                             
         MVC   SDIMP2,0(R1)        DEMO 2                                       
         LA    R1,8(R1)                                                         
         MVC   SDHOMES,0(R1)       HOMES                                        
         MVC   SDRHOMES,8(R1)      RHOMES                                       
*                                                                               
         BAS   RE,PUTSORT          PUT RECORD TO SORT                           
*                                                                               
         CLI   RPTOPT,C'Y'         TEST PRINTING REPORTS                        
         BNE   SKEDX                                                            
         XC    SORTREC,SORTREC     YES-PUT SORT RECORD FOR SUMMARY              
         MVI   SKSEQ,2                                                          
         MVC   SKRPRD,PROD                                                      
         MVC   SKRESTIM,BUYKEST                                                 
         MVC   SKRPUR,PURP                                                      
         MVC   SDSPTS,SCSPOTS      N'SPOTS                                      
         L     RE,SCGROSS          GROSS DOLLARS                                
         CVD   RE,DUB                                                           
         ZAP   SDDOL,DUB                                                        
         LA    R1,SCDEMOS+8                                                     
         CLI   NOSECDEM,C'Y'                                                    
         BE    *+8                                                              
         LA    R1,8(R1)                                                         
         MVC   SDHOMES,0(R1)       HOMES                                        
         MVC   SDRHOMES,8(R1)      RHOMES                                       
         BAS   RE,PUTSORT          PUT RECORD TO SORT                           
*                                                                               
SKEDX    B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
* BUILD A SPOT PERFORMANCE SORT RECORD FROM A SPOT TABLE ENTRY                  
* R2=A(SPOT TABLE ENTRY)                                                        
*                                                                               
PERFORM  NTR1                                                                   
         USING SPTTABD,R2                                                       
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         XC    SORTREC,SORTREC                                                  
         LA    R3,SORTREC                                                       
         USING SORTRECD,R3                                                      
         MVI   SKSEQ,1                                                          
         MVC   SKYEAR,YEAR         YEAR                                         
         L     R0,SBNDATES                                                      
         LA    R1,QTRTAB           QUARTER                                      
         LA    RE,DATES                                                         
*                                                                               
PERF2    CLC   SPTRDATE,0(RE)                                                   
         BL    *+14                                                             
         CLC   SPTRDATE,2(RE)                                                   
         BNH   PERF4                                                            
         LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,PERF2                                                         
         DC    H'0'                                                             
*                                                                               
PERF4    MVC   SKQTR,0(R1)                                                      
         MVC   SKPSTA,STA          STATION                                      
         GOTO1 DATCON,DMCB,(2,SPTADATE),(0,SKPDATE)   AFFID DATE                
         SR    RE,RE                                  AFFID TIME                
         SR    RF,RF                                                            
         ICM   RF,3,SPTATIME                                                    
         D     RE,=F'100'                                                       
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SKPTIME(2),DUB                                                   
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SKPTIME+2(2),DUB                                                 
         MVC   SKPTIME+4(2),=C'00'                                              
         MVC   SKPDEM1,DEMNAMES    DEMO NAMES                                   
         MVC   SKPDEM2,DEMNAMES+6                                               
         MVI   SKPDASHS,C' '                                                    
         MVC   SDSPTS+2(2),SPTSPOTS  N'SPOTS                                    
******** MVC   SDIMP1,DEMVALS      DEMO VALUES                                  
         LA    R1,SPTDEMOS                                                      
         MVC   SDIMP1,0(R1)        DEMO VALUES                                  
******** NI    SDIMP1,X'7F'                                                     
******** TM    DEMVALS,X'80'                                                    
******** BZ    *+8                                                              
******** MVI   SKPDASHS,C'S'                                                    
         CLI   NOSECDEM,C'Y'                                                    
         BE    PERF6                                                            
******** MVC   SDIMP2,DEMVALS+4                                                 
         LA    R1,4(R1)                                                         
         MVC   SDIMP2,0(R1)                                                     
******** NI    SDIMP2,X'7F'                                                     
******** TM    DEMVALS+4,X'80'                                                  
******** BZ    PERF6                                                            
******** MVI   SKPDASHS,C'S'                                                    
*                                                                               
**PERF6  MVC   SDHOMES,SPTDEMOS                                                 
******** MVC   SDRHOMES,SPTDEMOS+4                                              
******** MVC   SDW18P,SPTDEMOS+8                                                
******** MVC   SDW1849,SPTDEMOS+12                                              
PERF6    MVC   SDHOMES,4(R1)                                                    
         MVC   SDRHOMES,8(R1)                                                   
         MVC   SDW18P,12(R1)                                                    
         MVC   SDW1849,16(R1)                                                   
*                                                                               
         CLC   SBEPRD,SPTPRD1      TEST FIRST PRODUCT IS REQUESTED PROD         
         BNE   PERF7                                                            
         MVC   BYTE,SPTPRD1        FIRST PRODUCT                                
         BAS   RE,GETPROD                                                       
         MVC   SKPPRD,PROD                                                      
         GOTO1 GETPURP,BUYKEST                                                  
         MVC   SKPPUR,PURP         PURPOSE CODE                                 
         MVC   FULL,SPTADATE                                                    
         BAS   RE,GETADPT          DAYPART                                      
         MVC   SKPDPT,DAYPART                                                   
         CLC   SKPPUR,=C'SPRT'                                                  
         BNE   *+10                                                             
         MVC   SKPDPT,=C'SPRT'                                                  
         MVC   BYTE,SPTSLN1                                                     
         BAS   RE,GETLEN                                                        
         MVC   SKPLEN,FULL                                                      
         L     RE,SPTGRS1                                                       
         CVD   RE,DUB                                                           
         ZAP   SDDOL,DUB                                                        
         BAS   RE,PUTSORT                                                       
*                                                                               
PERF7    CLI   SPTPRD2,0                                                        
         BE    PERF8                                                            
         CLC   SBEPRD,SPTPRD2                                                   
         BNE   PERF8                                                            
         MVC   BYTE,SPTPRD2        SECOND PRODUCT                               
         BAS   RE,GETPROD                                                       
         MVC   SKPPRD,PROD                                                      
         GOTO1 GETPURP,BUYKEST                                                  
         MVC   SKPPUR,PURP         PURPOSE CODE                                 
         MVC   FULL,SPTADATE                                                    
         BAS   RE,GETADPT          DAYPART                                      
         MVC   SKPDPT,DAYPART                                                   
         CLC   SKPPUR,=C'SPRT'                                                  
         BNE   *+10                                                             
         MVC   SKPDPT,=C'SPRT'                                                  
         MVC   BYTE,SPTSLN2                                                     
         BAS   RE,GETLEN                                                        
         MVC   SKPLEN,FULL                                                      
         L     RE,SPTGRS2                                                       
         CVD   RE,DUB                                                           
         ZAP   SDDOL,DUB                                                        
         BAS   RE,PUTSORT                                                       
*                                                                               
PERF8    CLI   RPTOPT,C'Y'         TEST PRINTING REPORTS                        
         BNE   PERFX                                                            
         XC    SORTREC,SORTREC     YES-PUT SORT RECORD FOR SUMMARY              
         MVI   SKSEQ,2                                                          
         MVC   SKRESTIM,BUYKEST                                                 
         MVC   SDSPTS+2(2),SPTSPOTS  N'SPOTS                                    
         LA    R1,SPTDEMOS+4                                                    
         CLI   NOSECDEM,C'Y'                                                    
         BE    *+8                                                              
         LA    R1,4(R1)                                                         
         MVC   SDHOMES,0(R1)                                                    
         MVC   SDRHOMES,4(R1)                                                   
         MVC   SDW18P,8(R1)                                                     
         MVC   SDW1849,12(R1)                                                   
         CLC   SBEPRD,SPTPRD1                                                   
         BNE   PERF9                                                            
         MVC   BYTE,SPTPRD1        FIRST PRODUCT                                
         BAS   RE,GETPROD                                                       
         MVC   SKRPRD,PROD                                                      
         GOTO1 GETPURP,BUYKEST                                                  
         MVC   SKRPUR,PURP                                                      
         L     RE,SPTGRS1                                                       
         CVD   RE,DUB                                                           
         ZAP   SDDOL,DUB                                                        
         BAS   RE,PUTSORT          PUT RECORD TO SORT                           
*                                                                               
PERF9    CLI   SPTPRD2,0                                                        
         BE    PERFX                                                            
         CLC   SBEPRD,SPTPRD2                                                   
         BNE   PERFX                                                            
         MVC   BYTE,SPTPRD2        SECOND PRODUCT                               
         BAS   RE,GETPROD                                                       
         MVC   SKRPRD,PROD                                                      
         GOTO1 GETPURP,BUYKEST                                                  
         MVC   SKRPUR,PURP                                                      
         L     RE,SPTGRS2                                                       
         CVD   RE,DUB                                                           
         ZAP   SDDOL,DUB                                                        
         BAS   RE,PUTSORT          PUT RECORD TO SORT                           
*                                                                               
PERFX    B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
* BUILD AN ESTIMATED SPOT SPENDING SORT RECORD FROM A SPOTGOAL                  
* EXTRACT CHUNK                                                                 
* R2=A(CHUNK)                                                                   
*                                                                               
SPEND    NTR1                                                                   
         USING SGLCHNKD,R2                                                      
         L     R6,ADGOAL                                                        
         USING GOALREC,R6                                                       
         XC    SORTREC,SORTREC                                                  
         LA    R3,SORTREC                                                       
         USING SORTRECD,R3                                                      
         MVI   SKSEQ,1                                                          
         MVC   SKYEAR,YEAR         YEAR                                         
         L     R0,SBNDATES                                                      
         LA    R1,QTRTAB           QUARTER                                      
         LA    RE,DATES                                                         
*                                                                               
SPEND2   CLC   SGDATE,0(RE)                                                     
         BL    *+14                                                             
         CLC   SGDATE,2(RE)                                                     
         BNH   SPEND4                                                           
         LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,SPEND2                                                        
         DC    H'0'                                                             
*                                                                               
SPEND4   MVC   SKQTR,0(R1)                                                      
*                                                                               
         MVC   BYTE,SGPRD1         PRODUCT                                      
         BAS   RE,GETPROD                                                       
         MVC   SKEPRD,PROD                                                      
         GOTO1 GETPURP,GKEYEST                                                  
         MVC   SKEPUR,PURP         PURPOSE CODE                                 
         MVC   SKEMKT,MKTCD        MARKET                                       
         MVC   BYTE,SGSLNT                                                      
         BAS   RE,GETLEN           SPOT LENGTH                                  
         MVC   SKELEN,FULL                                                      
         MVC   SKEDEMO(5),DEMNAME1                                              
         MVI   SKEDEMO+5,C' '                                                   
         MVC   SKEDEMO+6(14),DEMDESC1                                           
         L     RE,SGDOL            GOAL DOLLARS                                 
         CVD   RE,DUB                                                           
         ZAP   SDDOL,DUB                                                        
         BAS   RE,PUTSORT          PUT SORT RECORD                              
*                                                                               
         CLI   RPTOPT,C'Y'         TEST PRINTING REPORTS                        
         BNE   SPENDX                                                           
         XC    SORTREC,SORTREC     YES-PUT SORT RECORD FOR SUMMARY              
         MVI   SKSEQ,2                                                          
         MVC   SKRPRD,PROD                                                      
         MVC   SKRESTIM,GKEYEST                                                 
         MVC   SKRPUR,PURP                                                      
         L     RE,SGDOL            GOAL DOLLARS                                 
         CVD   RE,DUB                                                           
         ZAP   SDDOL,DUB                                                        
         BAS   RE,PUTSORT          PUT RECORD TO SORT                           
*                                                                               
SPENDX   B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
* GET 2-BYTE PRODUCT CODE                                                       
* INPUT  : BYTE=1-BYTE PRODUCT CODE                                             
* OUTPUT : PROD=2-BYTE PRODUCT CODE                                             
*                                                                               
GETPROD  L     R1,ADCLT                                                         
         LA    R1,CLIST-CLTHDR(R1)                                              
         LA    R0,220                                                           
         NI    PGERR,X'FF'-PERPRD                                               
GETPROD2 CLI   0(R1),C'A'                                                       
         BL    GETPROD6                                                         
         CLC   BYTE,3(R1)                                                       
         BE    GETPROD4                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,GETPROD2                                                      
         B     GETPROD6                                                         
GETPROD4 MVC   PROD,0(R1)                                                       
         BR    RE                                                               
GETPROD6 MVC   PROD,=C'**'                                                      
         OI    PGERR,PERPRD                                                     
         BR    RE                                                               
         EJECT                                                                  
* GET 4-BYTE PURPOSE CODE                                                       
* INPUT  : BYTE=1-BYTE PRODUCT CODE                                             
*          R1=A(ESTIMATE CODE)                                                  
* OUTPUT : PURP=4-BYTE PURPOSE CODE                                             
*                                                                               
GETPURP  NI    PGERR,X'FF'-PERPUR                                               
         MVC   PURP,SPACES                                                      
         ZIC   RF,0(R1)                                                         
         BCTR  RF,0                                                             
         AR    RF,RF                                                            
         ZIC   R1,BYTE                                                          
         BCTR  R1,0                                                             
         SLL   R1,9                                                             
         AR    RF,R1                                                            
         L     R1,AESTTAB                                                       
         LA    RF,0(RF,R1)         RF=A(1ST 2 BYTES OF ESTIMATE NAME)           
         LA    R1,PURTAB                                                        
*                                                                               
GETPURP2 CLI   0(R1),0                                                          
         BE    GETPURP4                                                         
         CLC   0(2,RF),0(R1)                                                    
         BE    *+12                                                             
         LA    R1,6(R1)                                                         
         B     GETPURP2                                                         
         MVC   PURP,2(R1)                                                       
         B     GETPURPX                                                         
*                                                                               
GETPURP4 OI    PGERR,PERPUR                                                     
*                                                                               
GETPURPX BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
* GET THE P&G DAYPART CODE BASED ON BUY DAYS AND TIMES                          
*                                                                               
GETDPT   NTR1                                                                   
         NI    PGERR,X'FF'-PERDPT                                               
         MVC   DAYPART,SPACES                                                   
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         BAS   RE,TESTBNUS         TEST BONUS DAYPART                           
         BE    DPTX                                                             
         XC    BUYDAYS,BUYDAYS                                                  
         LA    R1,BUYDAYS          SET THE BUY DAYS                             
         ZIC   R0,BDDAY                                                         
         SLL   R0,25                                                            
         LA    RE,1                                                             
*                                                                               
DPT2     LTR   R0,R0               TEST BUY ACTIVE THIS DAY                     
         BZ    DPT4                                                             
         BP    *+12                                                             
         STC   RE,0(R1)                                                         
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         SLL   R0,1                                                             
         B     DPT2                                                             
*                                                                               
DPT4     MVC   BUYSTART(4),BDTIMST MOVE START/END TIMES                         
         LH    R0,BUYSTART                                                      
         CH    R0,=H'600'                                                       
         BNL   DPT5                                                             
         AH    R0,=H'2400'                                                      
         CLC   BUYEND,=H'600'      TEST PROGRAM CROSSES 6AM                     
         BL    DPT5                                                             
         LH    R0,=H'600'          YES-FORCE START TO 6AM                       
*                                                                               
DPT5     STH   R0,BUYSTART                                                      
         OC    BUYEND,BUYEND                                                    
         BNZ   *+12                                                             
         STH   R0,BUYEND                                                        
         B     DPT6                                                             
         LH    R0,BUYEND                                                        
         CH    R0,=H'600'                                                       
         BNL   *+12                                                             
         AH    R0,=H'2400'                                                      
         STH   R0,BUYEND                                                        
*                                                                               
DPT6     XC    BUYTPTAB,BUYTPTAB   CLEAR COUNTERS                               
         LA    R5,BUYDAYS          POINT TO BUY DAY LIST                        
*                                                                               
DPT7     CLI   0(R5),0             TEST END OF DAY LIST                         
         BE    DPT14                                                            
         LA    R4,TPTAB            R4=A(TIME PERIOD TABLE)                      
         USING TPTABD,R4                                                        
*                                                                               
DPT8     OC    TPSTTM,TPSTTM       TEST END OF TIME PERIOD TABLE                
         BZ    DPT12                                                            
         CLC   TPDAY,0(R5)         MATCH DAY                                    
         BNE   DPT10                                                            
         CLC   BUYSTART,TPNDTM     BUY START AFTER TIME PERIOD ENDS             
         BH    DPT10               YES                                          
         CLC   BUYEND,TPSTTM       BUY END BEFORE TIME PERIOD STARTS            
         BL    DPT10                                                            
         LH    RF,BUYSTART         CALCULATE THE MINUTES IN TIME PERIOD         
         CH    RF,TPSTTM           BUYSTART TO TIME PERIOD START                
         BH    *+8                 TAKE LATER                                   
         LH    RF,TPSTTM                                                        
         SR    RE,RE                                                            
         D     RE,=F'100'          HOURS IN RF/MIN IN RE                        
         MH    RF,=H'60'           HOURS X 60                                   
         AR    RF,RE               + MINUTES                                    
         LR    R0,RF               SAVE BUY START IN R0                         
         LH    RF,BUYEND                                                        
         CH    RF,TPNDTM           BUY END TO TIME PERIOD END                   
         BL    *+8                 TAKE EARLIER                                 
         LH    RF,TPNDTM                                                        
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         MH    RF,=H'60'           HOURS X 60                                   
         AR    RF,RE               + MIN                                        
         SR    RF,R0                                                            
         LTR   RF,RF               TEST START=END                               
         BNZ   *+8                                                              
         LA    RF,1                YES-AT LEAST ONE MINUTE                      
         ZIC   RE,TPACCUM          GET ACCUM NUM                                
         SLL   RE,2                X 4                                          
         LA    RE,BUYTPTAB(RE)                                                  
         A     RF,0(RE)            ADD TO ACCUM FOR THIS PERIOD                 
         ST    RF,0(RE)                                                         
*                                                                               
DPT10    LA    R4,TPTABLN(R4)      NEXT TIME PERIOD TABLE ENTRY                 
         B     DPT8                                                             
*                                                                               
DPT12    LA    R5,1(R5)            NEXT BUY DAY                                 
         B     DPT7                                                             
*                                                                               
DPT14    LA    R0,9                FIND DAYPART WITH HIGHEST VALUE              
         LA    R1,BUYTPTAB                                                      
         LA    R2,4(R1)                                                         
*                                                                               
DPT16    CLC   0(4,R1),0(R2)                                                    
         BNL   *+6                 R1 VALUE IS GREATER THAN OR EQ               
         LR    R1,R2               ELSE TAKE R2 VALUE                           
         LA    R2,4(R2)                                                         
         BCT   R0,DPT16                                                         
*                                                                               
         OC    0(4,R1),0(R1)                                                    
         BZ    DPT18                                                            
         LA    R0,BUYTPTAB                                                      
         SR    R1,R0                                                            
         LA    R1,DPTCDTAB(R1)     INDEX INTO DAYPART CODE TABLE                
         MVC   DAYPART,0(R1)                                                    
*                                                                               
DPT18    CLC   DAYPART,SPACES      TEST VALID DAYPART                           
         BH    DPTX                                                             
         OI    PGERR,PERDPT                                                     
*                                                                               
DPTX     B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* GET THE P&G DAYPART CODE BASED ON AFFID DATE AND TIME                         
* INPUT  : FULL=AFFID DATE AND TIME                                             
*                                                                               
GETADPT  NTR1                                                                   
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         NI    PGERR,X'FF'-PERDPT                                               
         BAS   RE,TESTBNUS         TEST BONUS DAYPART                           
         BE    ADPTX                                                            
         MVC   DAYPART,SPACES                                                   
         GOTO1 DATCON,DMCB,(2,FULL),(0,DUB)                                     
         GOTO1 GETDAY,(R1),DUB,THREE                                            
         CLC   THREE,SPACES                                                     
         BH    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,DMCB           BYTE=AFFID DAY NUMBER                        
         LH    R0,FULL+2                                                        
         CH    R0,=H'600'                                                       
         BNL   *+12                                                             
         AH    R0,=H'2400'                                                      
         STH   R0,FULL+2                                                        
         LA    R4,TPTAB            R4=A(TIME PERIOD TABLE)                      
         USING TPTABD,R4                                                        
*                                                                               
ADPT2    OC    TPSTTM,TPSTTM       TEST END OF TIME PERIOD TABLE                
         BZ    ADPT8                                                            
         CLC   TPDAY,BYTE          MATCH DAY                                    
         BNE   ADPT4                                                            
         CLC   FULL+2(2),TPSTTM    TEST AFFID TIME IN THIS TIME PERIOD          
         BL    ADPT4                                                            
         CLC   FULL+2(2),TPNDTM                                                 
         BNH   ADPT6               YES                                          
*                                                                               
ADPT4    LA    R4,TPTABLN(R4)      NEXT TIME PERIOD TABLE ENTRY                 
         B     ADPT2                                                            
*                                                                               
ADPT6    ZIC   RE,TPACCUM          GET ACCUM NUM                                
         SLL   RE,2                X 4                                          
         LA    RE,DPTCDTAB(RE)                                                  
         MVC   DAYPART,0(RE)                                                    
*                                                                               
ADPT8    CLC   DAYPART,SPACES                                                   
         BH    ADPTX                                                            
         OI    PGERR,PERDPT                                                     
*                                                                               
ADPTX    B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* TEST FOR BONUS DAYPART                                                        
* ON OUTPUT, CC EQ = YES                                                        
*            CC NE = NO                                                         
*                                                                               
TESTBNUS DS    0H                                                               
         USING BUYREC,R6                                                        
         CLC   QAGY,=C'NW'         FOR AYER,                                    
         BNE   *+16                                                             
         CLI   BDDAYPT,C'B'        BONUS DAYPART = B                            
         BE    TESTBNY                                                          
         B     TESTBNN                                                          
         CLC   QAGY,=C'DF'         FOR SAATCHI,                                 
         BNE   TESTBNN                                                          
         CLI   BDDAYPT,C'X'        BONUS DAYPART = X                            
         BNE   TESTBNN                                                          
*                                                                               
TESTBNY  MVC   DAYPART,=C'BNUS'    SET CODE FOR BONUS                           
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
TESTBNN  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
* GET SPOT LENGTH CODE                                                          
* INPUT  : BYTE=BINARY LENGTH                                                   
* OUTPUT : FULL=EBCDIC LENGTH                                                   
*                                                                               
GETLEN   NTR1                                                                   
         NI    PGERR,X'FF'-PERSLN                                               
         LA    R1,LENTAB                                                        
*                                                                               
GETLEN2  CLI   0(R1),0                                                          
         BNE   *+12                                                             
         OI    PGERR,PERSLN                                                     
         B     GETLEN4                                                          
         CLC   BYTE,0(R1)                                                       
         BE    GETLEN4                                                          
         LA    R1,1(R1)                                                         
         B     GETLEN2                                                          
*                                                                               
GETLEN4  ZIC   RE,BYTE                                                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL(4),DUB                                                      
         B     EXIT                                                             
         SPACE 2                                                                
LENTAB   DC    AL1(10,15,30,45,60,90,0)                                         
         EJECT                                                                  
* PUT RECORD TO THE SORT                                                        
*                                                                               
PUTSORT  NTR1                                                                   
         CLI   SORTOPEN,C'Y'                                                    
         BE    PUTSORT2                                                         
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         MVI   SORTOPEN,C'Y'                                                    
*                                                                               
PUTSORT2 LA    R2,SORTREC                                                       
         USING SORTRECD,R2                                                      
         MVC   SERROR,PGERR        MOVE ERROR BYTE TO SORT RECORD               
         DROP  R2                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         B     EXIT                                                             
         EJECT                                                                  
* RUNLAST                                                                       
*                                                                               
SP100    MVI   EOF,C'Y'                                                         
         MVI   FSTREC,C'Y'                                                      
         MVI   SUMSW,C'N'                                                       
         MVI   CLTTOT,C'N'                                                      
         ZAP   TOTRCNT,=P'0'                                                    
         ZAP   TPCNT,=P'0'                                                      
         ZAP   ERRCNT,=P'0'                                                     
         XC    SORTREC,SORTREC                                                  
         CLI   SORTOPEN,C'Y'                                                    
         BNE   SP108                                                            
         LA    R2,SORTREC                                                       
         USING SORTRECD,R2                                                      
         GOTO1 SORTER,DMCB,=C'GET' GET FIRST SORT RECORD                        
         ICM   RE,15,4(R1)                                                      
         BZ    SP108                                                            
         MVC   SORTREC2,0(RE)                                                   
         MVI   EOF,C'N'                                                         
*                                                                               
SP102    MVC   SORTREC,SORTREC2                                                 
         CLI   SKSEQ,1             TEST DETAIL RECORD                           
         BNE   SP103                                                            
         CLI   FSTREC,C'Y'         AND FIRST RECORD IN LOGICAL FILE             
         BNE   SP103                                                            
         BAS   RE,HEADER           YES-PUT THE HEADER                           
         MVI   FSTREC,C'N'                                                      
*                                                                               
SP103    GOTO1 SORTER,DMCB,=C'GET' GET NEXT SORT RECORD                         
         ICM   RE,15,4(R1)         TEST EOF                                     
         BNZ   *+12                                                             
         MVI   EOF,C'Y'                                                         
         B     SP106                                                            
         MVC   SORTREC2,0(RE)                                                   
         CLC   SORTKEY,SORTREC2    TEST CHANGE OF KEY                           
         BNE   SP104               YES                                          
         LH    R3,DSPTS            NO-ADD THE DATA FIELDS                       
         BAS   RE,FLDADD                                                        
         LH    R3,DIMP1                                                         
         BAS   RE,FLDADD                                                        
         LH    R3,DIMP2                                                         
         BAS   RE,FLDADD                                                        
         LH    R3,DHOMES                                                        
         BAS   RE,FLDADD                                                        
         LH    R3,DRHOMES                                                       
         BAS   RE,FLDADD                                                        
         LH    R3,DW18P                                                         
         BAS   RE,FLDADD                                                        
         LH    R3,DW1849                                                        
         BAS   RE,FLDADD                                                        
         LH    R3,DDOL                                                          
         LA    RF,SORTREC2(R3)                                                  
         ZAP   DUB,0(L'SDDOL,RF)                                                
         AP    SDDOL,DUB                                                        
         LH    R3,DERROR           PASS ON THE ERROR BYTE                       
         LA    RF,SORTREC2(R3)                                                  
         OC    SERROR,0(RF)                                                     
         B     SP103               GET NEXT RECORD                              
*                                                                               
SP104    CLI   SKSEQ,2             TEST SUMMARY RECORD                          
         BNE   SP112               NO                                           
*                                                                               
SP106    CLI   SUMSW,C'Y'          YES-TEST STARTED SUMMARY REPORT              
         BE    SP110               YES                                          
*                                                                               
SP108    MVI   FORCEHED,C'Y'       PRINT RECORD COUNTERS                        
         MVC   P(20),=C'TOTAL RECORD COUNT..'                                   
         EDIT  (P4,TOTRCNT),(6,P+24)                                            
         GOTO1 REPORT                                                           
         MVI   P2,0                                                             
         MVC   P3(20),=CL20'TAPE RECORD COUNT....'                              
         EDIT  (P4,TPCNT),(6,P3+24)                                             
         GOTO1 REPORT                                                           
         MVI   P2,0                                                             
         MVC   P3(20),=CL20'ERROR COUNT.........'                               
         EDIT  (P4,ERRCNT),(6,P3+24)                                            
         GOTO1 REPORT                                                           
         CLI   SORTOPEN,C'Y'       TEST SORT OPEN                               
         BNE   SP116                                                            
         CLI   EOF,C'Y'            YES-TEST REACHED EOF                         
         BNE   *+12                                                             
         BAS   RE,TRAILER          YES-PUT TRAILER RECORD AND EXIT              
         B     SP116                                                            
         MVI   FORCEHED,C'Y'       ELSE PRINT SUMMARY REPORT                    
*                                                                               
SP110    BAS   RE,SUMM             PRINT A SUMMARY LINE                         
         CLI   EOF,C'Y'            TEST EOF                                     
         BNE   SP102               NO-GET NEXT RECORD                           
         MVI   CLTTOT,C'Y'                                                      
         BAS   RE,SUMM             YES-PRINT CLIENT TOTAL LINE                  
         B     SP116                                                            
*                                                                               
SP112    BAS   RE,DETAIL           PUT THE DETAIL RECORD                        
         LH    R3,=Y(SKSEQ-SORTRECD)                                            
         LA    R3,SORTREC2(R3)                                                  
         CLI   0(R3),2             IF THIS IS THE LAST DETAIL RECORD,           
         BE    SP114                                                            
         LH    R3,=Y(SKQTR-SORTRECD)                                            
         LA    R3,SORTREC2(R3)                                                  
         CLC   SKQTR,0(R3)         OR CHANGE OF QUARTER,                        
         BE    SP102                                                            
*                                                                               
SP114    BAS   RE,TRAILER          YES-PUT TRAILER FOR LAST QTR                 
         MVI   FSTREC,C'Y'             INDICATE NEW LOGICAL FILE                
         B     SP102                                                            
*                                                                               
SP116    LA    R3,PGTAPE                                                        
         CLI   DCBOPEN,C'Y'        TEST TAPE OPEN                               
         BNE   EXIT                                                             
         CLOSE ((R3),)             YES-CLOSE IT                                 
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
* ROUTINE TO ADD TWO DATA FIELDS                                                
*                                                                               
FLDADD   LA    RF,SORTREC2(R3)                                                  
         ICM   R1,15,0(RF)                                                      
         BZR   RE                                                               
         LA    RF,SORTREC(R3)                                                   
         L     R0,0(RF)                                                         
         AR    R0,R1                                                            
         ST    R0,0(RF)                                                         
         BR    RE                                                               
         EJECT                                                                  
* BUILD AND PUT A HEADER RECORD                                                 
*                                                                               
HEADER   NTR1                                                                   
         LA    R2,SORTREC                                                       
         USING SORTRECD,R2                                                      
         LA    R3,PGREC                                                         
         USING PGHDRD,R3                                                        
         MVC   PGREC,SPACES                                                     
         MVI   PGHTYPE,PGHTYPEQ                                                 
         MVC   PGHPRCS,PROCESS                                                  
         MVC   PGHDTYPE,DATATYPE                                                
         MVC   PGHAGY,PGAGY                                                     
         BAS   RE,GETDTIME                                                      
         MVC   PGHDATE,DATE                                                     
         MVC   PGHTIME,TIME                                                     
         MVC   PGHYEAR,SKYEAR                                                   
         MVC   PGHQTR,SKQTR                                                     
         MVI   PGERR,0                                                          
         BAS   RE,WRITEREC                                                      
         ZAP   RECCNT,=P'0'        INIT LOGICAL FILE RECORD COUNT               
*                                                                               
HEADERX  B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
* BUILD AND PUT A TRAILER RECORD                                                
*                                                                               
TRAILER  NTR1                                                                   
         LA    R2,SORTREC                                                       
         USING SORTRECD,R2                                                      
         LA    R3,PGREC                                                         
         USING PGTRAILD,R3                                                      
         MVC   PGREC,SPACES                                                     
         MVI   PGTTYPE,PGTTYPEQ                                                 
         MVC   PGTPRCS,PROCESS                                                  
         MVC   PGTDTYPE,DATATYPE                                                
         MVC   PGTAGY,PGAGY                                                     
         MVC   PGTDATE,DATE                                                     
         MVC   PGTTIME,TIME                                                     
         MVC   PGTYEAR,SKYEAR                                                   
         MVC   PGTQTR,SKQTR                                                     
         OI    RECCNT+3,X'0F'                                                   
         UNPK  PGTNRECS,RECCNT                                                  
         MVI   PGERR,0                                                          
         BAS   RE,WRITEREC                                                      
*                                                                               
TRAILERX B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
* BUILD AND PUT A DETAIL RECORD                                                 
*                                                                               
DETAIL   NTR1                                                                   
         LA    R2,SORTREC                                                       
         USING SORTRECD,R2                                                      
         MVC   PGERR,SERROR                                                     
         LA    R3,PGREC                                                         
         MVC   PGREC,SPACES                                                     
         CLI   DATATYPE,DTEST                                                   
         BE    DETEST                                                           
         CLI   DATATYPE,DTSKD                                                   
         BE    DETSKD                                                           
         CLI   DATATYPE,DTPER                                                   
         BE    DETPER                                                           
         DC    H'0'                                                             
*                                                                               
         USING PGSRECD,R3          FORMAT SPOT SCHEDULE  RECORD                 
DETSKD   MVI   PGSTYPE,PGSTYPEQ                                                 
         MVC   PGSYEAR,SKYEAR                                                   
         MVC   PGSQTR,SKQTR                                                     
         MVC   PGSPRD,SKSPRD                                                    
         MVC   PGSPUR,SKSPUR                                                    
         MVC   PGSLEN,SKSLEN                                                    
******** CLC   PGSLEN,=C'0040'                                                  
******** BNE   *+6                                                              
******** DC    H'0'                                                             
         MVC   PGSDPT,SKSDPT                                                    
         MVC   PGSSTA,SKSSTA                                                    
         MVC   PGSAGY,PGAGY                                                     
         L     RE,SDSPTS           SPOTS                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PGSSPTS,DUB                                                      
         OI    SDDOL+7,X'0F'       DOLLARS                                      
         UNPK  PGSDOL,SDDOL                                                     
         MVC   PGSDEM1,SKSDEM1     DEMO 1                                       
         MVI   DEMOTYPE,C'I'                                                    
         L     RF,SDIMP1                                                        
         BAS   RE,AVGDEM                                                        
         UNPK  PGSIMP1,DUB                                                      
         MVC   PGSDEM2,SKSDEM2     DEMO 2                                       
         L     RF,SDIMP2                                                        
         BAS   RE,AVGDEM                                                        
         UNPK  PGSIMP2,DUB                                                      
         L     RF,SDHOMES          HOMES                                        
         BAS   RE,AVGDEM                                                        
         UNPK  PGSHOMES,DUB                                                     
         L     RF,SDRHOMES         RHOMES                                       
         MVI   DEMOTYPE,C'R'                                                    
         BAS   RE,AVGDEM                                                        
         UNPK  PGSRHOME,DUB                                                     
         MVC   PGSSVC,SERVICE                                                   
         B     DETAIL2                                                          
         DROP  R3                                                               
*                                                                               
AVGDEM   LR    R0,RE                                                            
         SR    RE,RE                                                            
         SLL   RF,1                                                             
         ICM   R1,15,SDSPTS                                                     
         BNZ   *+10                                                             
         SR    RF,RF                                                            
         B     AVGDEM2                                                          
         CLI   DEMOTYPE,C'I'                                                    
         BNE   *+8                                                              
         MH    R1,=H'10'           IMPS ARE ROUNDED                             
         DR    RE,R1                                                            
         LA    RF,1(RF)                                                         
         SRL   RF,1                                                             
AVGDEM2  CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         USING PGPRECD,R3          FORMAT SPOT PERFORMANCE RECORD               
DETPER   MVI   PGPTYPE,PGPTYPEQ                                                 
         MVC   PGPYEAR,SKYEAR                                                   
         MVC   PGPQTR,SKQTR                                                     
         MVC   PGPPRD,SKPPRD                                                    
         MVC   PGPPUR,SKPPUR                                                    
         MVC   PGPLEN,SKPLEN                                                    
         MVC   PGPDPT,SKPDPT                                                    
         MVC   PGPSTA,SKPSTA                                                    
         MVC   PGPDATE(4),SKPDATE+2    MONTH/DAY                                
         MVC   PGPDATE+4(2),SKPDATE    YEAR                                     
         MVC   PGPTIME,SKPTIME                                                  
         MVC   PGPAGY,PGAGY                                                     
         OI    SDDOL+7,X'0F'       DOLLARS                                      
         UNPK  PGPDOL,SDDOL                                                     
         MVC   PGPDEM1,SKPDEM1     DEMO 1                                       
         L     R1,SDIMP1                                                        
         BAS   RE,ROUND                                                         
         UNPK  PGPIMP1,DUB                                                      
         MVC   PGPDEM2,SKPDEM2     DEMO 2                                       
         L     R1,SDIMP2                                                        
         BAS   RE,ROUND                                                         
         UNPK  PGPIMP2,DUB                                                      
         L     R1,SDHOMES          HOMES                                        
         BAS   RE,ROUND                                                         
         UNPK  PGPHOMES,DUB                                                     
         L     RE,SDRHOMES         RHOMES                                       
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PGPRHOME,DUB                                                     
         L     R1,SDW18P           W18+                                         
         BAS   RE,ROUND                                                         
         UNPK  PGPW18P,DUB                                                      
         L     R1,SDW1849          W1849                                        
         BAS   RE,ROUND                                                         
         UNPK  PGPW1849,DUB                                                     
         MVC   PGPSVC,SERVICE                                                   
         MVC   PGPDASHS,SKPDASHS                                                
         B     DETAIL2                                                          
         DROP  R3                                                               
*                                                                               
         USING PGERECD,R3          FORMAT ESTIMATED SPOT SPENDING REC           
DETEST   MVI   PGETYPE,PGETYPEQ                                                 
         MVC   PGEYEAR,SKYEAR                                                   
         MVC   PGEQTR,SKQTR                                                     
         MVC   PGEPRD,SKEPRD                                                    
         MVC   PGEMKT,SKEMKT                                                    
         MVC   PGEPUR,SKEPUR                                                    
         MVC   PGELEN,SKELEN                                                    
         MVC   PGEAGY,PGAGY                                                     
         MVC   PGEDEMO,SKEDEMO                                                  
         OI    SDDOL+7,X'0F'       DOLLARS                                      
         UNPK  PGEDOL,SDDOL                                                     
         MVC   PGESVC,SERVICE                                                   
         B     DETAIL2                                                          
*                                                                               
DETAIL2  BAS   RE,WRITEREC         PUT DETAIL RECORD                            
         CLI   PGERR,0             TEST FOR ERRORS                              
         BNE   DETAILX             YES-RECORD WAS NOT PUT TO TAPE               
         AP    RECCNT,=P'1'        AUGMENT LOGICAL FILE RECORD COUNT            
*                                                                               
DETAILX  B     EXIT                                                             
         DROP  R2,R3                                                            
         SPACE 2                                                                
ROUND    SR    R0,R0               ROUND IMPS TO THOUSANDS                      
         SLL   R1,1                                                             
         D     R0,=F'10'                                                        
         LA    R1,1(R1)                                                         
         SRL   R1,1                                                             
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         BR    RE                                                               
         EJECT                                                                  
* PUT RECORD TO TAPE                                                            
*                                                                               
WRITEREC NTR1                                                                   
         AP    TOTRCNT,=P'1'                                                    
         CLI   RPTOPT,C'Y'                                                      
         BNE   *+8                                                              
         BAS   RE,PRTREC           PRINT THE RECORD DETAILS                     
         CLI   TAPOPT,C'N'         TEST TO SUPPRESS                             
         BE    WRIRECX                                                          
         CLI   PGERR,0                                                          
         BNE   WRIRECX                                                          
         LA    R1,PGTAPE                                                        
         LA    R0,PGREC                                                         
         PUT   (R1),(R0)                                                        
         AP    TPCNT,=P'1'                                                      
*                                                                               
WRIRECX  B     EXIT                                                             
         EJECT                                                                  
* GET CURRENT DATE AND TIME                                                     
*                                                                               
GETDTIME NTR1                                                                   
         GOTO1 DATCON,DMCB,(5,0),(0,DUB)   TODAY'S DATE YYMMDD                  
         TIME                                                                   
         LR    R1,R0                                                            
         SRL   R1,24               ISOLATE HOURS                                
         SLL   R1,4                                                             
         AH    R1,=H'12'           LOOK LIKE A PACKED FIELD                     
         ST    R1,FULL                                                          
         AP    FULL,=P'8'          OUR DAY STARTS AT 8AM                        
         CP    FULL,=P'24'         TEST 24HRS OR LATER                          
         BL    GETDT2                                                           
         SP    FULL,=P'24'         YES-GO FORWARD TO NEXT DAY                   
         GOTO1 ADDAY,DMCB,DUB,DATE,1                                            
         MVC   DUB(6),DATE                                                      
*                                                                               
GETDT2   L     R1,FULL                                                          
         SRL   R1,4                                                             
         STC   R1,BYTE                                                          
         ICM   R0,8,BYTE                                                        
         SRL   R0,8                SHIFT OUT FRACTIONS OF SECOND                
         SLL   R0,4                                                             
         AH    R0,=H'12'           LOOK LIKE A PACKED FIELD                     
         ST    R0,FULL                                                          
         UNPK  TIME,FULL                                                        
         OI    TIME+5,X'F0'                                                     
         MVC   DATE(4),DUB+2                                                    
         MVC   DATE+4(2),DUB       MMDDYY                                       
         B     EXIT                                                             
         EJECT                                                                  
* FORMAT DATA TO PRINT LINE                                                     
*                                                                               
PRTREC   NTR1                                                                   
         LA    R2,P                                                             
         LA    R3,PGREC                                                         
         CLI   0(R3),PGHTYPEQ                                                   
         BE    PRHEAD                                                           
         CLI   0(R3),PGTTYPEQ                                                   
         BE    PRTRAIL                                                          
         B     PRTREC2                                                          
*                                                                               
         USING PGHDRD,R3                                                        
PRHEAD   MVC   P(1),PGHTYPE                                                     
         MVC   P+1(1),PGHDTYPE                                                  
         MVC   P+3(5),=C'YEAR='                                                 
         MVC   P+8(5),PGHYEAR                                                   
         MVI   P+13,C','                                                        
         MVC   P+14(4),=C'QTR='                                                 
         MVC   P+18(4),PGHQTR                                                   
         MVI   P+22,C','                                                        
         MVC   P+23(7),=C'AGENCY='                                              
         MVC   P+30(4),PGHAGY                                                   
         MVI   P+34,C','                                                        
         MVC   P+35(5),=C'DATE='                                                
         MVC   P+40(6),PGHDATE                                                  
         MVI   P+46,C','                                                        
         MVC   P+47(5),=C'TIME='                                                
         MVC   P+52(6),PGHTIME                                                  
         LA    R4,P+60                                                          
         B     PRTREC10                                                         
         DROP  R3                                                               
*                                                                               
         USING PGTRAILD,R3                                                      
PRTRAIL  MVC   P(1),PGTTYPE                                                     
         MVC   P+1(1),PGTDTYPE                                                  
         MVC   P+3(5),=C'YEAR='                                                 
         MVC   P+8(5),PGTYEAR                                                   
         MVI   P+13,C','                                                        
         MVC   P+14(4),=C'QTR='                                                 
         MVC   P+18(4),PGTQTR                                                   
         MVI   P+22,C','                                                        
         MVC   P+23(7),=C'AGENCY='                                              
         MVC   P+30(4),PGTAGY                                                   
         MVI   P+34,C','                                                        
         MVC   P+35(5),=C'DATE='                                                
         MVC   P+40(6),PGTDATE                                                  
         MVI   P+46,C','                                                        
         MVC   P+47(5),=C'TIME='                                                
         MVC   P+52(6),PGTTIME                                                  
         MVI   P+58,C','                                                        
         MVC   P+59(8),=C'RECORDS='                                             
         MVC   P+67(7),PGTNRECS                                                 
         LA    R4,P+76                                                          
         B     PRTREC10                                                         
         DROP  R3                                                               
*                                                                               
PRTREC2  CLI   DATATYPE,DTEST                                                   
         BE    PREST                                                            
         CLI   DATATYPE,DTSKD                                                   
         BE    PRSKD                                                            
         CLI   DATATYPE,DTPER                                                   
         BE    PRPER                                                            
         DC    H'0'                                                             
*                                                                               
         USING PELINED,R2                                                       
         USING PGERECD,R3                                                       
PREST    MVC   PETYPE,PGETYPE                                                   
         MVC   PETYPE+1(1),DATATYPE                                             
         MVC   PEYEAR,PGEYEAR                                                   
         MVC   PEQTR,PGEQTR                                                     
         MVC   PEPRD(2),PGEPRD                                                  
         MVC   PEMKT,PGEMKT                                                     
         MVC   PEPUR,PGEPUR                                                     
         MVC   PELEN,PGELEN                                                     
         MVC   PEAGY,PGEAGY                                                     
         MVC   PEDEMOS,PGEDEMO                                                  
         MVC   PEDOL,PGEDOL                                                     
         MVC   PESVC,PGESVC                                                     
         LA    R4,PEERRS                                                        
         B     PRTREC10                                                         
         DROP  R2,R3                                                            
*                                                                               
         USING PSLINED,R2                                                       
         USING PGSRECD,R3                                                       
PRSKD    MVC   PSTYPE,PGSTYPE                                                   
         MVC   PSTYPE+1(1),DATATYPE                                             
         MVC   PSYEAR,PGSYEAR                                                   
         MVC   PSQTR,PGSQTR                                                     
         MVC   PSPRD(2),PGSPRD                                                  
         MVC   PSPUR,PGSPUR                                                     
         MVC   PSLEN,PGSLEN                                                     
         MVC   PSDPT,PGSDPT                                                     
         MVC   PSSTA,PGSSTA                                                     
         MVC   PSAGY,PGSAGY                                                     
         MVC   PSSPOTS,PGSSPTS                                                  
         MVC   PSDOL,PGSDOL                                                     
         MVC   PSDEM1,PGSDEM1                                                   
         MVC   PSIMP1,PGSIMP1                                                   
         MVC   PSDEM2,PGSDEM2                                                   
         MVC   PSIMP2,PGSIMP2                                                   
         MVC   PSHOMES,PGSHOMES                                                 
         MVC   PSRHOMES(2),PGSRHOME                                             
         MVI   PSRHOMES+2,C'.'                                                  
         MVC   PSRHOMES+3(1),PGSRHOME+2                                         
         MVC   PSSVC,PGSSVC                                                     
         LA    R4,PSERRS                                                        
         B     PRTREC10                                                         
         DROP  R2,R3                                                            
*                                                                               
         USING PPLINED,R2                                                       
         USING PGPRECD,R3                                                       
PRPER    MVC   PPTYPE,PGPTYPE                                                   
         MVC   PPTYPE+1(1),DATATYPE                                             
         MVC   PPPRD(2),PGPPRD                                                  
         MVC   PPPUR,PGPPUR                                                     
         MVC   PPLEN,PGPLEN                                                     
         MVC   PPDPT,PGPDPT                                                     
         MVC   PPSTA,PGPSTA                                                     
*                                                                               
         MVC   PPDATE,PGPDATE                                                   
         MVC   PPTIME,PGPTIME                                                   
         MVC   PPDOL,PGPDOL                                                     
         MVC   PPDEM1,PGPDEM1                                                   
         MVC   PPIMP1,PGPIMP1                                                   
         MVC   PPDEM2,PGPDEM2                                                   
         MVC   PPIMP2,PGPIMP2                                                   
         MVC   PPHOMES,PGPHOMES                                                 
         MVC   PPRHOMES(2),PGPRHOME                                             
         MVI   PPRHOMES+2,C'.'                                                  
         MVC   PPRHOMES+3(1),PGPRHOME+2                                         
         MVC   PPW18P,PGPW18P                                                   
         MVC   PPW1849,PGPW1849                                                 
         MVC   PPDASHS,PGPDASHS                                                 
         LA    R4,PPERRS                                                        
         B     PRTREC10                                                         
         DROP  R2,R3                                                            
*                                                                               
PRTREC10 CLI   PGERR,0                                                          
         BE    PRTREC20                                                         
         TM    PGERR,PERMKT                                                     
         BZ    *+14                                                             
         MVC   0(3,R4),=C'MKT'                                                  
         LA    R4,4(R4)                                                         
         TM    PGERR,PERDPT                                                     
         BZ    *+14                                                             
         MVC   0(3,R4),=C'DPT'                                                  
         LA    R4,4(R4)                                                         
         TM    PGERR,PERPRD                                                     
         BZ    *+14                                                             
         MVC   0(3,R4),=C'PRD'                                                  
         LA    R4,4(R4)                                                         
         TM    PGERR,PERDEM                                                     
         BZ    *+14                                                             
         MVC   0(3,R4),=C'DEM'                                                  
         LA    R4,4(R4)                                                         
         TM    PGERR,PERPUR                                                     
         BZ    *+14                                                             
         MVC   0(3,R4),=C'PUR'                                                  
         LA    R4,4(R4)                                                         
         TM    PGERR,PERSLN                                                     
         BZ    *+14                                                             
         MVC   0(3,R4),=C'LEN'                                                  
         LA    R4,4(R4)                                                         
*                                                                               
PRTREC20 GOTO1 REPORT                                                           
*                                                                               
         CLI   PGERR,0                                                          
         BE    PRTREC22                                                         
         AP    ERRCNT,=P'1'                                                     
         B     PRTRECX                                                          
*                                                                               
PRTREC22 B     PRTRECX                                                          
*                                                                               
PRTRECX  B     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO PRINT SUMMARY LINES                                                
*                                                                               
         SPACE 1                                                                
SUMM     NTR1  ,                                                                
         LA    R2,SORTREC                                                       
         USING SORTRECD,R2                                                      
         CLI   SUMSW,C'Y'          TEST FIRST TIME                              
         BE    SUMM1                                                            
         MVI   SUMSW,C'Y'                                                       
         ZIC   R1,RCSUBPRG                                                      
         LA    R1,3(R1)                                                         
         STC   R1,RCSUBPRG                                                      
         MVI   FORCEHED,C'Y'                                                    
         B     SUMM2                                                            
*                                                                               
SUMM1    CLC   SKRPRD,PROD         TEST NEW PRODUCT                             
         BNE   *+12                                                             
         CLI   CLTTOT,C'Y'         OR CLIENT TOTAL TIME                         
         BNE   SUMM2                                                            
         MVC   ESTIM,XFF           YES-PRINT PRODUCT TOTALS FOR                 
         MVC   SPTS,PSPOTS             PREVIOUS PRODUCT                         
         MVC   HOMES,PHOMES                                                     
         MVC   RHOMES,PRHOMES                                                   
         MVC   W18P,PW18P                                                       
         MVC   W1849,PW1849                                                     
         MVC   DOLS,PDOLS                                                       
         BAS   RE,FMTSUM                                                        
         XC    PSPOTS,PSPOTS                                                    
         XC    PHOMES,PHOMES                                                    
         XC    PRHOMES,PRHOMES                                                  
         XC    PW18P,PW18P                                                      
         XC    PW1849,PW1849                                                    
         ZAP   PDOLS,=P'0'                                                      
         CLI   CLTTOT,C'Y'         TEST CLIENT TOTAL TIME                       
         BNE   SUMM2                                                            
         MVC   PROD,XFF            YES-PRINT CLIENT TOTALS                      
         MVC   ESTIM,XFF                                                        
         MVC   SPTS,CSPOTS                                                      
         MVC   HOMES,CHOMES                                                     
         MVC   RHOMES,CRHOMES                                                   
         MVC   W18P,CW18P                                                       
         MVC   W1849,CW1849                                                     
         MVC   DOLS,CDOLS                                                       
         BAS   RE,FMTSUM                                                        
         B     SUMMX                                                            
*                                                                               
SUMM2    MVC   PROD,SKRPRD                                                      
         ZIC   RE,SKRESTIM                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTIM,DUB                                                        
         MVC   PURP,SKRPUR                                                      
         MVC   SPTS,SDSPTS                                                      
         MVC   HOMES,SDHOMES                                                    
         MVC   RHOMES,SDRHOMES                                                  
         MVC   W18P,SDW18P                                                      
         MVC   W1849,SDW1849                                                    
         MVC   DOLS,SDDOL                                                       
         BAS   RE,FMTSUM                                                        
         L     RE,SPTS                                                          
         L     R1,PSPOTS                                                        
         AR    R1,RE                                                            
         ST    R1,PSPOTS                                                        
         L     R1,CSPOTS                                                        
         AR    R1,RE                                                            
         ST    R1,CSPOTS                                                        
         L     RE,HOMES                                                         
         L     R1,PHOMES                                                        
         AR    R1,RE                                                            
         ST    R1,PHOMES                                                        
         L     R1,CHOMES                                                        
         AR    R1,RE                                                            
         ST    R1,CHOMES                                                        
         L     RE,RHOMES                                                        
         L     R1,PRHOMES                                                       
         AR    R1,RE                                                            
         ST    R1,PRHOMES                                                       
         L     R1,CRHOMES                                                       
         AR    R1,RE                                                            
         ST    R1,CRHOMES                                                       
         L     RE,W18P                                                          
         L     R1,PW18P                                                         
         AR    R1,RE                                                            
         ST    R1,PW18P                                                         
         L     R1,CW18P                                                         
         AR    R1,RE                                                            
         ST    R1,CW18P                                                         
         L     RE,W1849                                                         
         L     R1,PW1849                                                        
         AR    R1,RE                                                            
         ST    R1,PW1849                                                        
         L     R1,CW1849                                                        
         AR    R1,RE                                                            
         ST    R1,CW1849                                                        
         AP    PDOLS,DOLS                                                       
         AP    CDOLS,DOLS                                                       
*                                                                               
SUMMX    B     EXIT                                                             
         SPACE 2                                                                
FMTSUM   LR    R3,RE                                                            
         LA    R4,P                                                             
         CLI   DATATYPE,DTEST                                                   
         BNE   FMTSUM2                                                          
         USING PRELINED,R4                                                      
         MVC   PREPRD,PROD                                                      
         CLC   PROD,XFF                                                         
         BNE   *+10                                                             
         MVC   PREPRD(5),=C'*ALL*'                                              
         MVC   PREEST,ESTIM                                                     
         CLC   ESTIM,XFF                                                        
         BNE   *+14                                                             
         MVC   PREEST(5),=C'*ALL*'                                              
         B     *+10                                                             
         MVC   PREPUR,PURP                                                      
         EDIT  (P8,DOLS),(11,PREDOL),2                                          
         B     FMTSUM8                                                          
*                                                                               
FMTSUM2  CLI   DATATYPE,DTSKD                                                   
         BNE   FMTSUM4                                                          
         USING PRSLINED,R4                                                      
         MVC   PRSPRD,PROD                                                      
         CLC   PROD,XFF                                                         
         BNE   *+10                                                             
         MVC   PRSPRD(5),=C'*ALL*'                                              
         MVC   PRSEST,ESTIM                                                     
         CLC   ESTIM,XFF                                                        
         BNE   *+14                                                             
         MVC   PRSEST(5),=C'*ALL*'                                              
         B     *+10                                                             
         MVC   PRSPUR,PURP                                                      
         EDIT  SPTS,(6,PRSSPOTS)                                                
         EDIT  (P8,DOLS),(11,PRSDOL),2                                          
         EDIT  HOMES,(9,PRSHOMES),1                                             
         EDIT  RHOMES,(9,PRSRHOME),1                                            
         B     FMTSUM8                                                          
*                                                                               
FMTSUM4  CLI   DATATYPE,DTPER                                                   
         BNE   FMTSUMX                                                          
         USING PRPLINED,R4                                                      
         MVC   PRPPRD,PROD                                                      
         CLC   PROD,XFF                                                         
         BNE   *+10                                                             
         MVC   PRPPRD(5),=C'*ALL*'                                              
         MVC   PRPEST,ESTIM                                                     
         CLC   ESTIM,XFF                                                        
         BNE   *+14                                                             
         MVC   PRPEST(5),=C'*ALL*'                                              
         B     *+10                                                             
         MVC   PRPPUR,PURP                                                      
         EDIT  SPTS,(6,PRPSPOTS)                                                
         EDIT  (P8,DOLS),(11,PRPDOL),2                                          
         EDIT  HOMES,(9,PRPHOMES),1                                             
         EDIT  RHOMES,(9,PRPRHOME),1                                            
         EDIT  W18P,(9,PRPW18P),1                                               
         EDIT  W1849,(9,PRPW1849),1                                             
*                                                                               
FMTSUM8  DS    0H                                                               
         GOTO1 REPORT                                                           
         CLC   ESTIM,XFF                                                        
         BNE   FMTSUMX                                                          
         GOTO1 REPORT                                                           
*                                                                               
FMTSUMX  LR    RE,R3                                                            
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* DATA CONSTANTS                                                                
*                                                                               
SORTER   DC    V(SORTER)                                                        
ACALLOV  DC    A(CALLOV)                                                        
ABOOKLST DC    A(BOOKLIST)                                                      
AESTTAB  DC    A(ESTTAB)                                                        
ASPOTTAB DC    A(SPOTTAB)                                                       
ACHUNK   DC    A(CHNKAREA)                                                      
*                                                                               
DSPTS    DC    Y(SDSPTS-SORTRECD)                                               
DIMP1    DC    Y(SDIMP1-SORTRECD)                                               
DIMP2    DC    Y(SDIMP2-SORTRECD)                                               
DHOMES   DC    Y(SDHOMES-SORTRECD)                                              
DRHOMES  DC    Y(SDRHOMES-SORTRECD)                                             
DW18P    DC    Y(SDW18P-SORTRECD)                                               
DW1849   DC    Y(SDW1849-SORTRECD)                                              
DDOL     DC    Y(SDDOL-SORTRECD)                                                
DERROR   DC    Y(SERROR-SORTRECD)                                               
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,056,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=100'                                   
*                                                                               
*                                  DATA TYPES                                   
DTTABLE  DC    C'E',AL1(DTEST),X'01'   ESTIMATED SPOT SPENDING                  
         DC    C'S',AL1(DTSKD),X'02'   SPOT SCHEDULE                            
         DC    C'P',AL1(DTPER),X'03'   SPOT PERFORMANCE                         
         DC    X'00'                                                            
*                                                                               
*                                  PG AGENCY CODE TABLE                         
PGAGYTAB DC    CL2'NW',CL4'AYER'   AYER                                         
         DC    CL2'  ',CL4'BURR'                                                
         DC    CL2'  ',CL4'CONN'                                                
         DC    CL2'  ',CL4'DMBB'                                                
         DC    CL2'  ',CL4'FONT'                                                
         DC    CL2'  ',CL4'GREY'                                                
         DC    CL2'  ',CL4'LEOB'                                                
         DC    CL2'  ',CL4'JMCT'                                                
         DC    CL2'  ',CL4'NOBL'                                                
         DC    CL2'FM',CL4'PRA '   SFM                                          
         DC    CL2'  ',CL4'RSS '   (RS)                                         
         DC    CL2'DF',CL4'SSDC'   DANCER FITZGERALD                            
         DC    CL2'  ',CL4'SHM '                                                
         DC    CL2'  ',CL4'TLK '                                                
         DC    CL2'  ',CL4'WRG '                                                
         DC    X'00'                                                            
*                                                                               
PGQTRS   DC    CL4'3JFM'           P&G QUARTER NAMES                            
         DC    CL4'4AMJ'                                                        
         DC    CL4'1JAS'                                                        
         DC    CL4'2OND'                                                        
*                                                                               
DPTCDTAB DC    CL4'EM  '          DAYPART CODE TABLE                            
         DC    CL4'DAY '                                                        
         DC    CL4'EFR '                                                        
         DC    CL4'PACC'                                                        
         DC    CL4'PRIM'                                                        
         DC    CL4'LFR '                                                        
*                                                                               
*                                 PURPOSE CODE TABLE                            
PURTAB   DC    CL2'NS',CL4'NORM'  NORMAL SUSTAINING                             
         DC    CL2'JT',CL4'MEDA'  TEST                                          
         DC    CL2'QS',CL4'NETW'  NETWORK REPLACEMENT                           
         DC    CL2'KT',CL4'INTR'  INTRODUCTION                                  
         DC    CL2'PX',CL4'PRMO'  PROMOTION                                     
         DC    CL2'LS',CL4'LATE'  LATE SUSTAINING                               
         DC    CL2'EX',CL4'HISP'  HISPANIC                                      
         DC    CL2'BX',CL4'BLCK'                                                
         DC    CL2'WX',CL4'CATM'                                                
         DC    CL2'AX',CL4'SPRT'                                                
         DC    CL2'NO',CL4'NOR2'                                                
         DC    CL2'NE',CL4'NET2'                                                
         DC    CL2'LA',CL4'LAT2'                                                
         DC    X'00'                                                            
*                                                                               
DEMTAB   DC    X'01',CL5'HH   ',CL14'ADI HOUSEHOLDS'                            
         DC    X'7A',CL5'K211 ',CL14'KIDS 2-11     '                            
         DC    X'7B',CL5'K611 ',CL14'KIDS 6-11     '                            
         DC    X'5F',CL5'M18+ ',CL14'MEN 18+       '                            
         DC    X'5B',CL5'M1834',CL14'MEN 18-34     '                            
         DC    X'5C',CL5'M1849',CL14'MEN 18-49     '                            
         DC    X'61',CL5'M2549',CL14'MEN 25-49     '                            
         DC    X'62',CL5'M2554',CL14'MEN 25-54     '                            
         DC    X'63',CL5'M2564',CL14'MEN 25-64     '                            
         DC    X'64',CL5'M25+ ',CL14'MEN 25+       '                            
         DC    X'68',CL5'M35+ ',CL14'MEN 35+       '                            
         DC    X'6B',CL5'M50+ ',CL14'MEN 50+       '                            
         DC    X'80',CL5'P1224',CL14'PERSONS 12-24 '                            
         DC    X'81',CL5'P1234',CL14'PERSONS 12-34 '                            
         DC    X'91',CL5'P18+ ',CL14'PERSONS 18+   '                            
         DC    X'8D',CL5'P1834',CL14'PERSONS 18-34 '                            
         DC    X'8E',CL5'P1849',CL14'PERSONS 18-49 '                            
         DC    X'7F',CL5'P2+  ',CL14'PERSONS 2+    '                            
         DC    X'94',CL5'P2554',CL14'PERSONS 25-54 '                            
         DC    X'95',CL5'P2564',CL14'PERSONS 25-64 '                            
         DC    X'97',CL5'P3549',CL14'PERSONS 35-49 '                            
         DC    X'9A',CL5'P35+ ',CL14'PERSONS 35+   '                            
         DC    X'9D',CL5'P50+ ',CL14'PERSONS 50+   '                            
         DC    X'7D',CL5'T1217',CL14'TEENS 12-17   '                            
         DC    X'19',CL5'W1217',CL14'WOMEN 12-17   '                            
         DC    X'41',CL5'WW18+',CL14'WORK WOMEN 18+'                            
         DC    X'1C',CL5'W1224',CL14'WOMEN 12-24   '                            
         DC    X'1D',CL5'W1234',CL14'WOMEN 12-34   '                            
         DC    X'1E',CL5'W1249',CL14'WOMEN 12-49   '                            
         DC    X'2D',CL5'W18+ ',CL14'WOMEN 18+     '                            
         DC    X'28',CL5'W1824',CL14'WOMEN 18-24   '                            
         DC    X'29',CL5'W1834',CL14'WOMEN 18-34   '                            
         DC    X'2A',CL5'W1849',CL14'WOMEN 18-49   '                            
         DC    X'2B',CL5'W1854',CL14'WOMEN 18-54   '                            
         DC    X'2F',CL5'W2549',CL14'WOMEN 25-49   '                            
         DC    X'30',CL5'W2554',CL14'WOMEN 25-54   '                            
         DC    X'31',CL5'W2564',CL14'WOMEN 25-64   '                            
         DC    X'32',CL5'W25+ ',CL14'WOMEN 25+     '                            
         DC    X'36',CL5'W35+ ',CL14'WOMEN 35+     '                            
         DC    X'39',CL5'W50+ ',CL14'WOMEN 50+     '                            
         DC    X'00'                                                            
         EJECT                                                                  
* DSECT FOR TIME PERIOD TABLES *                                                
*                                                                               
TPTABD   DSECT                                                                  
TPSTTM   DS    H                   START TIME                                   
TPNDTM   DS    H                   END TIME                                     
TPDAY    DS    C                   DAY                                          
TPACCUM  DS    X                   CODE                                         
TPTABLN  EQU   *-TPTABD                                                         
         SPACE 2                                                                
* TIME PERIOD TABLE                                                             
*                                                                               
SPX502   CSECT                                                                  
*                                                                               
TPTAB    DS    0H                                                               
         DC    AL2(0600,0859),X'01',X'00' MON 600A-859A                         
         DC    AL2(0600,0859),X'02',X'00' TUE                                   
         DC    AL2(0600,0859),X'03',X'00' WED                                   
         DC    AL2(0600,0859),X'04',X'00' THU                                   
         DC    AL2(0600,0859),X'05',X'00' FRI                                   
         DC    AL2(0600,0859),X'06',X'00' SAT                                   
         DC    AL2(0600,0859),X'07',X'00' SUN                                   
         DC    AL2(0900,1559),X'01',X'01' MON 900A-359P                         
         DC    AL2(0900,1559),X'02',X'01' TUE                                   
         DC    AL2(0900,1559),X'03',X'01' WED                                   
         DC    AL2(0900,1559),X'04',X'01' THU                                   
         DC    AL2(0900,1559),X'05',X'01' FRI                                   
         DC    AL2(1600,1859),X'01',X'02' MON 400P-659P                         
         DC    AL2(1600,1859),X'02',X'02' TUE                                   
         DC    AL2(1600,1859),X'03',X'02' WED                                   
         DC    AL2(1600,1859),X'04',X'02' THU                                   
         DC    AL2(1600,1859),X'05',X'02' FRI                                   
         DC    AL2(0900,1959),X'06',X'02' SAT 900A-759P                         
         DC    AL2(0900,1859),X'07',X'02' SUN 900A-659P                         
         DC    AL2(1900,1959),X'01',X'03' MON 700P-759P                         
         DC    AL2(1900,1959),X'02',X'03' TUE                                   
         DC    AL2(1900,1959),X'03',X'03' WED                                   
         DC    AL2(1900,1959),X'04',X'03' THU                                   
         DC    AL2(1900,1959),X'05',X'03' FRI                                   
         DC    AL2(2000,2259),X'01',X'04' MON 800P-1059P                        
         DC    AL2(2000,2259),X'02',X'04' TUE                                   
         DC    AL2(2000,2259),X'03',X'04' WED                                   
         DC    AL2(2000,2259),X'04',X'04' THU                                   
         DC    AL2(2000,2259),X'05',X'04' FRI                                   
         DC    AL2(2000,2259),X'06',X'04' SAT                                   
         DC    AL2(1900,2259),X'07',X'04' SUN 700P-1059P                        
         DC    AL2(2300,9999),X'01',X'05' MON 1100P-CC                          
         DC    AL2(2300,9999),X'02',X'05' TUE                                   
         DC    AL2(2300,9999),X'03',X'05' WED                                   
         DC    AL2(2300,9999),X'04',X'05' THU                                   
         DC    AL2(2300,9999),X'05',X'05' FRI                                   
         DC    AL2(2300,9999),X'06',X'05' SAT                                   
         DC    AL2(2300,9999),X'07',X'05' SUN                                   
         DC    AL2(0,0)                      E-O-T                              
         EJECT                                                                  
* STORAGE                                                                       
*                                                                               
         DS    0D                                                               
SPOTBUY  DS    V                                                                
SPOTGOAL DS    V                                                                
SPTS     DC    F'0'                                                             
HOMES    DC    F'0'                                                             
RHOMES   DC    F'0'                                                             
W18P     DC    F'0'                                                             
W1849    DC    F'0'                                                             
DOLS     DC    PL8'0'                                                           
PSPOTS   DC    F'0'                                                             
PHOMES   DC    F'0'                                                             
PRHOMES  DC    F'0'                                                             
PW18P    DC    F'0'                                                             
PW1849   DC    F'0'                                                             
PDOLS    DC    PL8'0'                                                           
CSPOTS   DC    F'0'                                                             
CHOMES   DC    F'0'                                                             
CRHOMES  DC    F'0'                                                             
CW18P    DC    F'0'                                                             
CW1849   DC    F'0'                                                             
CDOLS    DC    PL8'0'                                                           
BUYSTART DS    H                                                                
BUYEND   DS    H                                                                
BUYTPTAB DS    XL40                10 4 BYTE COUNTERS                           
*                                                                               
PROCESS  DS    CL1                 PROCESS TYPE                                 
PRADD    EQU   C'A'                ADD                                          
PRREP    EQU   C'R'                REPLACE                                      
*                                                                               
DATATYPE DS    CL1                 DATA TYPE                                    
DTEST    EQU   C'E'                ESTIMATED SPOT SPENDING                      
DTSKD    EQU   C'S'                SPOT SCHEDULE                                
DTPER    EQU   C'P'                SPOT PERFORMANCE                             
*                                                                               
PGERR    DS    XL1                                                              
PERMKT   EQU   X'80'                                                            
PERDPT   EQU   X'40'                                                            
PERPRD   EQU   X'20'                                                            
PERDEM   EQU   X'10'                                                            
PERPUR   EQU   X'08'                                                            
PERSLN   EQU   X'04'                                                            
*                                                                               
PGAGY    DS    CL4                 PG AGENCY CODE                               
MKTCD    DS    CL3                                                              
FSTREC   DS    CL1                                                              
EOF      DS    CL1                                                              
DEMOTYPE DS    CL1                                                              
SERVICE  DS    CL1                                                              
NDEMOS   DS    XL1                                                              
NOSECDEM DS    CL1                                                              
SORTOPEN DS    CL1                                                              
UPGRADE  DS    CL1                                                              
RPTOPT   DS    CL1                                                              
TAPOPT   DS    CL1                                                              
SUMSW    DS    CL1                                                              
CLTTOT   DS    CL1                                                              
QTRTAB   DS    CL50                                                             
DATES    DS    CL50                                                             
YEAR     DS    CL5                                                              
DAYPART  DS    CL4                                                              
**DEMOS  DS    XL6                                                              
DEMVALS  DS    XL8                                                              
DEMNAMES DS    0CL12                                                            
DEMNAME1 DS    CL6                                                              
DEMNAME2 DS    CL6                                                              
DEMDESCS DS    0CL28                                                            
DEMDESC1 DS    CL14                                                             
DEMDESC2 DS    CL14                                                             
BUYDAYS  DS    CL8                                                              
DATE     DS    CL6                                                              
TIME     DS    CL6                                                              
PRDLST   DS    XL16                                                             
NPRD     DS    XL1                                                              
PROD     DS    CL2                                                              
PURP     DS    CL4                                                              
ESTIM    DS    CL3                                                              
*                                                                               
RECCNT   DS    PL4                                                              
TOTRCNT  DS    PL4                                                              
ERRCNT   DS    PL4                                                              
TPCNT    DS    PL4                                                              
*                                                                               
XFF      DC    XL4'FFFFFFFF'                                                    
*                                                                               
SORTREC  DS    CL100                                                            
SORTREC2 DS    CL100                                                            
PGREC    DS    CL160                                                            
*                                                                               
PGTAPE   DCB   DDNAME=PGTAPE,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00160,                                            X        
               BLKSIZE=01600,                                          X        
               MACRF=PM                                                         
*                                                                               
         DS    0D                                                               
         DC    CL8'SPOTBLOK'                                                    
         PRINT OFF                                                              
       ++INCLUDE SPOTBLOCK                                                      
         PRINT ON                                                               
         SPACE 2                                                                
SPX502   CSECT                                                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'*BKLIST*'                                                    
BOOKLIST DS    360X                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'*ESTTAB*'                                                    
ESTTAB   DS    113000X             220 PRDS X 512 BYTES/PRD = 112640            
*                                                                               
         DS    0D                                                               
         DC    CL8'SPOTTAB*'                                                    
SPOTTAB  DS    12000X                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'*CHUNKS*'                                                    
CHNKAREA DS    10000X                                                           
         EJECT                                                                  
* SIMULATE CALLOV                                                               
*                                                                               
CALLOV   NMOD1 0,**CALLOV                                                       
         LR    R2,R1                                                            
         MVC   PARAMS,0(R1)                                                     
         LA    R3,SVPGMTAB                                                      
CALLOV2  CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),0                                                          
         BE    CALLOV4                                                          
         CLC   PROGNAME,0(R3)                                                   
         BE    CALLOV6                                                          
         LA    R3,8(R3)                                                         
         B     CALLOV2                                                          
CALLOV4  MVI   LONGPROG,C' '                                                    
         MVC   LONGPROG+1(7),LONGPROG                                           
         GOTO1 =V(HEXOUT),PARM,PROGNAME+1,LONGPROG,3,=C'TOG'                    
         MVI   LONGPROG,C'T'                                                    
         GOTO1 =V(LOADER),(R1),LONGPROG,0,0                                     
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+12                                                             
         MVI   PROGNAME,X'FF'                                                   
         B     CALLOVX                                                          
         MVC   0(4,R3),PROGNAME                                                 
         MVC   4(4,R3),4(R1)                                                    
CALLOV6  MVC   APROG,4(R3)                                                      
CALLOVX  MVC   0(L'PARAMS,R2),PARAMS                                            
         XMOD1 1                                                                
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
PARAMS   DS    0CL8                                                             
APROG    DS    CL4                                                              
PROGNAME DS    CL4                                                              
LONGPROG DS    CL8                                                              
PARM     DS    6F                                                               
SVPGMTAB DC    256X'00'                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
PGHDRD   DSECT                     HEADER DSECT                                 
PGHTYPE  DS    CL1                                                              
PGHTYPEQ EQU   C'H'                                                             
PGHPRCS  DS    CL1                                                              
PGHDTYPE DS    CL1                                                              
PGHAGY   DS    CL4                                                              
PGHDATE  DS    CL6                                                              
PGHTIME  DS    CL6                                                              
PGHYEAR  DS    0CL5                                                             
PGHYR1   DS    CL2                                                              
         DS    CL1                                                              
PGHYR2   DS    CL2                                                              
PGHQTR   DS    CL4                                                              
         DS    CL132                                                            
*                                                                               
PGTRAILD DSECT                     TRAILER DSECT                                
PGTTYPE  DS    CL1                                                              
PGTTYPEQ EQU   C'T'                                                             
PGTPRCS  DS    CL1                                                              
PGTDTYPE DS    CL1                                                              
PGTAGY   DS    CL4                                                              
PGTDATE  DS    CL6                                                              
PGTTIME  DS    CL6                                                              
PGTYEAR  DS    0CL5                                                             
PGTYR1   DS    CL2                                                              
         DS    CL1                                                              
PGTYR2   DS    CL2                                                              
PGTQTR   DS    CL4                                                              
PGTNRECS DS    CL7                                                              
         DS    CL125                                                            
*                                                                               
PGERECD  DSECT                     ESTIMATED SPOT SPENDING DSECT                
PGETYPE  DS    CL1                                                              
PGETYPEQ EQU   C'D'                                                             
PGEYEAR  DS    0CL5                                                             
PGEYR1   DS    CL2                                                              
         DS    CL1                                                              
PGEYR2   DS    CL2                                                              
PGEQTR   DS    CL4                                                              
PGEPRD   DS    CL2                                                              
PGEMKT   DS    CL3                                                              
PGEPUR   DS    CL4                                                              
PGELEN   DS    CL4                                                              
PGEAGY   DS    CL4                                                              
PGEDEMO  DS    CL20                                                             
PGEDOL   DS    CL11                                                             
PGESVC   DS    CL1                                                              
         DS    CL101                                                            
*                                                                               
PGSRECD  DSECT                     SPOT SCHEDULE DSECT                          
PGSTYPE  DS    CL1                                                              
PGSTYPEQ EQU   C'D'                                                             
PGSYEAR  DS    0CL5                                                             
PGSYR1   DS    CL2                                                              
         DS    CL1                                                              
PGSYR2   DS    CL2                                                              
PGSQTR   DS    CL4                                                              
PGSPRD   DS    CL2                                                              
PGSPUR   DS    CL4                                                              
PGSLEN   DS    CL4                                                              
PGSDPT   DS    CL4                                                              
PGSSTA   DS    CL4                                                              
PGSAGY   DS    CL4                                                              
PGSSPTS  DS    CL5                                                              
PGSDOL   DS    CL11                                                             
PGSDEM1  DS    CL6                                                              
PGSIMP1  DS    CL9                                                              
PGSDEM2  DS    CL6                                                              
PGSIMP2  DS    CL9                                                              
PGSHOMES DS    CL9                                                              
PGSRHOME DS    CL3                                                              
PGSSVC   DS    CL1                                                              
         DS    CL69                                                             
*                                                                               
PGPRECD  DSECT                     SPOT PERFORMANCE DSECT                       
PGPTYPE  DS    CL1                                                              
PGPTYPEQ EQU   C'D'                                                             
PGPYEAR  DS    0CL5                                                             
PGPYR1   DS    CL2                                                              
         DS    CL1                                                              
PGPYR2   DS    CL2                                                              
PGPQTR   DS    CL4                                                              
PGPPRD   DS    CL2                                                              
PGPPUR   DS    CL4                                                              
PGPLEN   DS    CL4                                                              
PGPDPT   DS    CL4                                                              
PGPSTA   DS    CL4                                                              
PGPDATE  DS    CL6                                                              
PGPTIME  DS    CL6                                                              
PGPAGY   DS    CL4                                                              
PGPDOL   DS    CL11                                                             
PGPDEM1  DS    CL6                                                              
PGPIMP1  DS    CL9                                                              
PGPDEM2  DS    CL6                                                              
PGPIMP2  DS    CL9                                                              
PGPHOMES DS    CL9                                                              
PGPRHOME DS    CL3                                                              
PGPW18P  DS    CL9                                                              
PGPW1849 DS    CL9                                                              
PGPSVC   DS    CL1                                                              
PGPDASHS DS    CL1                                                              
         DS    CL43                                                             
         EJECT                                                                  
SORTRECD DSECT                     SORT RECORD DSECT                            
SORTKEY  DS    0CL56                                                            
SKSEQ    DS    XL1                                                              
SKYEAR   DS    CL5                                                              
SKQTR    DS    CL4                                                              
*                                                                               
SKREST   DS    CL46                                                             
         ORG   SKREST                                                           
SKEPRD   DS    CL2              ** ESTIMATED SPOT SPENDING LOGICAL KEY          
SKEMKT   DS    CL3              **                                              
SKEPUR   DS    CL4              **                                              
SKELEN   DS    CL4              **                                              
SKEDEMO  DS    CL20                                                             
         ORG   SKREST                                                           
SKSPRD   DS    CL2              ** SPOT SCHEDULE LOGICAL KEY                    
SKSPUR   DS    CL4              **                                              
SKSLEN   DS    CL4              **                                              
SKSDPT   DS    CL4              **                                              
SKSSTA   DS    CL4              **                                              
SKSDEM1  DS    CL6                                                              
SKSDEM2  DS    CL6                                                              
         ORG   SKREST                                                           
SKPSTA   DS    CL4              ** SPOT PERFORMANCE LOGICAL KEY                 
SKPPRD   DS    CL2              **                                              
SKPDATE  DS    CL6              **                                              
SKPTIME  DS    CL6              **                                              
SKPPUR   DS    CL4                                                              
SKPLEN   DS    CL4                                                              
SKPDPT   DS    CL4                                                              
SKPDEM1  DS    CL6                                                              
SKPDEM2  DS    CL6                                                              
SKPDASHS DS    CL1                                                              
         ORG   SKREST                                                           
SKRPRD   DS    CL2              ** SUMMARY REPORT                               
SKRESTIM DS    XL1                                                              
SKRPUR   DS    CL4                                                              
         ORG                                                                    
SERROR   DS    XL1                 ERROR BYTE                                   
         DS    CL6                 SPARE                                        
*                                                                               
SORTDATA DS    0CL36                                                            
SDSPTS   DS    F                                                                
SDIMP1   DS    F                                                                
SDIMP2   DS    F                                                                
SDHOMES  DS    F                                                                
SDRHOMES DS    F                                                                
SDW18P   DS    F                                                                
SDW1849  DS    F                                                                
SDDOL    DS    PL8                                                              
         EJECT                                                                  
PELINED  DSECT                                                                  
PETYPE   DS    CL2                 0                                            
         DS    CL2                                                              
PEYEAR   DS    CL5                 4                                            
         DS    CL2                                                              
PEQTR    DS    CL4                 11                                           
         DS    CL2                                                              
PEPRD    DS    CL3                 17                                           
         DS    CL2                                                              
PEMKT    DS    CL3                 22                                           
         DS    CL2                                                              
PEPUR    DS    CL4                 27                                           
         DS    CL2                                                              
PELEN    DS    CL4                 33                                           
         DS    CL2                                                              
PEAGY    DS    CL4                 39                                           
         DS    CL2                                                              
PEDEMOS  DS    CL20                45                                           
         DS    CL2                                                              
PEDOL    DS    CL11                67                                           
         DS    CL2                                                              
PESVC    DS    CL3                 80                                           
         DS    CL2                                                              
PEERRS   DS    CL47                85                                           
         EJECT                                                                  
PSLINED  DSECT                                                                  
PSTYPE   DS    CL2                 0                                            
         DS    CL1                                                              
PSYEAR   DS    CL5                 3                                            
         DS    CL1                                                              
PSQTR    DS    CL4                 9                                            
         DS    CL1                                                              
PSPRD    DS    CL3                 14                                           
         DS    CL1                                                              
PSPUR    DS    CL4                 18                                           
         DS    CL1                                                              
PSLEN    DS    CL4                 23                                           
         DS    CL1                                                              
PSDPT    DS    CL4                 28                                           
         DS    CL1                                                              
PSSTA    DS    CL4                 33                                           
         DS    CL1                                                              
PSAGY    DS    CL4                 38                                           
         DS    CL1                                                              
PSSPOTS  DS    CL5                 43                                           
         DS    CL1                                                              
PSDOL    DS    CL11                49                                           
         DS    CL1                                                              
PSDEM1   DS    CL6                 61                                           
         DS    CL1                                                              
PSIMP1   DS    CL9                 68                                           
         DS    CL1                                                              
PSDEM2   DS    CL6                 78                                           
         DS    CL1                                                              
PSIMP2   DS    CL9                 85                                           
         DS    CL1                                                              
PSHOMES  DS    CL9                 95                                           
         DS    CL1                                                              
PSRHOMES DS    CL4                 105                                          
         DS    CL1                                                              
PSSVC    DS    CL3                 110                                          
         DS    CL1                                                              
PSERRS   DS    CL18                114                                          
         EJECT                                                                  
PPLINED  DSECT                                                                  
PPTYPE   DS    CL2                 0                                            
         DS    CL1                                                              
PPPRD    DS    CL3                 3                                            
         DS    CL1                                                              
PPPUR    DS    CL4                 7                                            
         DS    CL1                                                              
PPLEN    DS    CL4                 12                                           
         DS    CL1                                                              
PPDPT    DS    CL4                 17                                           
         DS    CL1                                                              
PPSTA    DS    CL4                 22                                           
         DS    CL1                                                              
PPDATE   DS    CL6                 27                                           
         DS    CL1                                                              
PPTIME   DS    CL6                 34                                           
         DS    CL1                                                              
PPDOL    DS    CL11                41                                           
         DS    CL1                                                              
PPDEM1   DS    CL6                 53                                           
         DS    CL1                                                              
PPIMP1   DS    CL9                 60                                           
         DS    CL1                                                              
PPDEM2   DS    CL6                 70                                           
         DS    CL1                                                              
PPIMP2   DS    CL9                 77                                           
         DS    CL1                                                              
PPHOMES  DS    CL9                 87                                           
         DS    CL1                                                              
PPRHOMES DS    CL4                 97                                           
         DS    CL1                                                              
PPW18P   DS    CL9                 102                                          
         DS    CL1                                                              
PPW1849  DS    CL9                 112                                          
         DS    CL1                                                              
PPDASHS  DS    CL1                 122                                          
         DS    CL1                                                              
PPERRS   DS    CL8                 124                                          
         EJECT                                                                  
PRELINED DSECT                                                                  
         DS    CL49                                                             
PREPRD   DS    CL2                                                              
         DS    CL4                                                              
PREEST   DS    CL3                                                              
         DS    CL1                                                              
PREPUR   DS    CL4                                                              
         DS    CL4                                                              
PREDOL   DS    CL11                                                             
         SPACE 1                                                                
PRSLINED DSECT                                                                  
         DS    CL35                                                             
PRSPRD   DS    CL2                                                              
         DS    CL4                                                              
PRSEST   DS    CL3                                                              
         DS    CL1                                                              
PRSPUR   DS    CL4                                                              
         DS    CL4                                                              
PRSSPOTS DS    CL6                                                              
         DS    CL2                                                              
PRSDOL   DS    CL11                                                             
         DS    CL2                                                              
PRSHOMES DS    CL9                                                              
         DS    CL2                                                              
PRSRHOME DS    CL9                                                              
         SPACE 1                                                                
PRPLINED DSECT                                                                  
         DS    CL24                                                             
PRPPRD   DS    CL2                                                              
         DS    CL4                                                              
PRPEST   DS    CL3                                                              
         DS    CL1                                                              
PRPPUR   DS    CL4                                                              
         DS    CL4                                                              
PRPSPOTS DS    CL6                                                              
         DS    CL2                                                              
PRPDOL   DS    CL11                                                             
         DS    CL2                                                              
PRPHOMES DS    CL9                                                              
         DS    CL2                                                              
PRPRHOME DS    CL9                                                              
         DS    CL2                                                              
PRPW18P  DS    CL9                                                              
         DS    CL2                                                              
PRPW1849 DS    CL9                                                              
         SPACE 1                                                                
         EJECT                                                                  
         PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPOTTABD                                                       
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041SPREPX502 05/01/02'                                      
         END                                                                    
