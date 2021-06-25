*          DATA SET SPREPM202  AT LEVEL 217 AS OF 11/21/19                      
*          DATA SET SPREPM202  AT LEVEL 166 AS OF 01/14/99                      
*PHASE SPM202T                                                                  
*INCLUDE SPRPFOOT                                                               
*INCLUDE MEDAPRNT                                                               
*INCLUDE REPSPILL                                                               
*INCLUDE COVAIL                                                                 
         TITLE 'SPREPM202-BRAND PERFORMANCE REPORT'                             
*****************FUDGES*******************************************              
* 1. QUESTOR = '*DBT' WILL PRODUCE CPP ACCROSS BRANDS FOR A SINGLE              
*              MARKET REQUEST. THIS ALLOWS CORPORATE CPPS FOR BRAND             
*              MODE CLIENTS.                                                    
******************************************************************              
*                                                                               
* MEDIA SUMMARY BUFFALO LEVELS                                                  
*    COLUMN DEFINITION 1  (DETAIL)                                              
*        LEVEL 1 = DETAIL ITEMS                                                 
*        LEVEL 2 = MARKET GROUP 3  (MGR3)                                       
*        LEVEL 3 = MARKET GROUP 2  (MGR2)                                       
*        LEVEL 4 = MARKET GROUP 1  (MGR1)                                       
*        LEVEL 5 = PRODUCT TOTALS                                               
*    COLUMN DEFINITION 2  (PRIMARY DEMO)                                        
*        LEVEL 1 = PRODUCT GROUP 3                                              
*        LEVEL 2 = PRODUCT GROUP 2                                              
*        LEVEL 3 = PRODUCT GROUP 1                                              
*        LEVEL 4 = CLIENT                                                       
*                                                                               
*    COLUMN DEFINITION 3  (CLIENT)                                              
*        LEVEL 1 = PRODUCT GROUP 3                                              
*        LEVEL 2 = PRODUCT GROUP 2                                              
*        LEVEL 3 = PRODUCT GROUP 1                                              
*        LEVEL 4 = CLIENT                                                       
*                                                                               
*    RECORD TYPE 1 = DETAIL , 2 = PRIMARY DEMO                                  
         SPACE 4                                                                
***********************************************************************         
* CHANGE HISTORY:                                                     *         
* --------------                                                      *         
*                                                                     *         
* MAY02/91  QOPT4=Y SUPPRESSES ALL DOLLAR AMOUNTS, PASSED FROM SPD2   *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
* SPROG SETTINGS                                                                
*                                                                               
*        1 = PRIMARY DEMO = RATING                                              
*        2 = PRIMARY DEMO = IMPRESSIONS                                         
*        3 = PRODUCT SUMMARY   = RATING                                         
*        4 = PRODUCT SUMMARY   = IMPRESSIONS                                    
*        5 = PRD GROUP SUMMARY - PRIMARY DEMO = RATING                          
*        6 = PRD GROUP SUMMARY - PRIMARY DEMO = IMPRESSIONS                     
*        7 = CLIENT SUMMARY                                                     
*        8 = CLIENT SUMMARY                                                     
*                                                                               
         EJECT                                                                  
SPM202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPM202,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         LA    R6,2048(RB)                                                      
         LA    R6,2048(R6)                                                      
         USING SPM202+4096,R6                                                   
         STM   RA,RC,SPM2RA                                                     
         ST    R6,SPM2R6                                                        
         ST    R5,RELO                                                          
         OI    RQOPTS,RQOPTS_POST    FLAG TO INDICATE SPOT POSTING              
         GOTO1 =V(SPRPFOOT),DMCB,(RA),RR=RELO                                   
         MVI   SPSUPMKT,C'N'                                                    
         CLI   MODE,MKTLAST                                                     
         BH    *+8                                                              
         MVI   SPSUPMKT,C'Y'                                                    
         CLI   MODE,MKTLAST        GET WEIGHTS FROM BUFFER                      
         BL    NOW                                                              
         GOTO1 MEDADDWT,DMCB,(RA)                                               
         CLI   QOPT1,C'D'                                                       
         BNE   NOW                                                              
         CLI   FRSTTOT,C'Y'                                                     
         BNE   NOW                                                              
         MVI   SPDUPTOT,C'N'                                                    
         MVI   FRSTTOT,C'N'                                                     
NOW      DS    0H                                                               
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   M00                                                              
*                                                                               
         L     RE,ADAGY                                                         
         CLI   AGYPROF+7-AGYHDR(RE),C'C' TEST CANADIAN                          
         JNE   *+8                                                              
         OI    RQOPT2,RQOPT2_NETBUYS                                            
*                                                                               
*                                                                               
         MVI   RQGETBF,C'N'                                                     
         CLI   Q2NET,C'B'                                                       
         BNE   *+8                                                              
         MVI   RQGETBF,C'Y'                                                     
*                                                                               
         CLI   Q2NET,C'Y'                                                       
         BNE   *+8                                                              
         MVI   RQGETBF,C'X'                                                     
*                                                                               
         MVI   RQLKGLS,C'N'                                                     
         CLI   QOPT6,C'Y'          REPORT LOCKED GOALS                          
         BNE   *+8                                                              
         MVI   RQLKGLS,C'Y'                                                     
*                                                                               
         L     R5,=V(DICSECT)                                                   
         USING DICSECT,R5                                                       
         XC    DMCB,DMCB                                                        
         LA    R1,DMCB                                                          
         USING DICTATED,R1                                                      
         MVI   DDACTN,DDACTNL      TRANSLATE LIST                               
         MVI   DDRETN,DDCASEU                                                   
         MVI   DDSYS,2                                                          
         MVC   DDLANG,RCLANG                                                    
         LA    RF,DCLIST                                                        
         STCM  RF,7,DDIADR                                                      
         LA    RF,DSLIST                                                        
         STCM  RF,7,DDOADR                                                      
         GOTO1 DICTATE                                                          
         DROP  R5,R1                                                            
*                                                                               
M00      CLI   MODE,RUNFRST                                                     
         BNE   M2                                                               
         MVI   WEEKLY,C' '         RESET WTP SWITCH                             
         MVI   OVNIGHT,C' '        RESET OVN SWITCH                             
         MVI   LPMWK,C' '        RESET OVN SWITCH                               
         MVC   SVMAXLIN,MAXLINES                                                
         L     RE,MEDBUFF          CLEAR DATES                                  
         USING MEDBLOCK,RE                                                      
         L     RF,=F'1272'                                                      
         XCEF                                                                   
*                                                                               
         L     RE,MEDBUFF          SET UP FOR DATES                             
         MVC   MEDNUMMO,=F'12'                                                  
         MVC   MEDNUMPE,=F'1'                                                   
         MVI   RQDAYPT,C'Y'        AND OTHER STUFF                              
         MVI   RQEQUIV,C'Y'                                                     
         LA    RF,MYHEAD                                                        
         ST    RF,HEADHOOK                                                      
         CLI   MEDEXTAV,C'Y'                                                    
         BE    *+8                                                              
         MVI   MEDEXTDM,4                                                       
         DROP  RE                                                               
         LA     RE,MYBUFIO          DEAL WITH BUFFALO                           
         ST    RE,BUFFIO                                                        
         L     R2,=V(BUFFALOC)                                                  
         A     R2,RELO                                                          
         ST    R2,BUFFBUFF                                                      
         USING BUFFALOD,R2                                                      
         LA    RE,BFHOOK                                                        
         ST    RE,BUFFHOOK                                                      
         DROP  R2                                                               
         LA    RE,MYBUFIO                                                       
         ST    RE,BUFFIO                                                        
         MVI   SPSUPMKT,C'Y'                                                    
         CLC   4(8,R1),=C'BUFFOPEN'                                             
         BE    M0                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',(R2)                                        
         CLC   QPROG,=C'M2'        ALLOCATION ELSEWHERE IF THIS                 
         BNE   M0                  IS NOT THE REQUESTED PROGRAM                 
         L     R9,BUFFBUFF                                                      
         GOTO1 =V(COVAIL),DMCB,C'SETB',20000,300000,(R9)                        
         MVC   BUFFBUFF,12(R1)                                                  
*                                                                               
M0       GOTO1 MEDSEED,DMCB,(RA)   SET UP REPORT TABLES                         
*                                                                               
         L     RE,MEDTABLE         SET CPP ADDRESSES                            
         LA    R4,CPPCTRL                                                       
M1       SR    R5,R5                                                            
         IC    R5,0(R4)                                                         
         BCTR  R5,0                                                             
         MH    R5,=H'4'                                                         
         LA    R5,0(R5,RE)         POINT TO REPORT                              
         L     R1,0(R5)            POINT TO REPORT DEF.                         
         L     R7,4(R1)            GET COL. DEF.                                
         ST    R7,FULL                                                          
         MVC   1(3,R4),FULL+1      SAVE ORIGINAL COLUMN DEF.                    
         LA    R4,4(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   M1                                                               
         MVI   CPPSW,0             DEFAULT IS NO CROSS-DAYPART CPP/M            
         B     EXIT                                                             
         EJECT                                                                  
M2       CLI   MODE,REQFRST                                                     
         BNE   M20                                                              
         GOTO1 =V(SPRPFOOT),DMCB,(RA),RR=RELO                                   
         MVC   MAXLINES,SVMAXLIN                                                
         CLI   FOOT1,C' '                                                       
         BE    M2NOFT                                                           
         ZIC   R0,MAXLINES                                                      
         SH    R0,=H'3'                                                         
         STC   R0,MAXLINES                                                      
         MVC   FOOT1(132),SPACES                                                
M2NOFT   DS    0C                                                               
         GOTO1 =V(FIXHED),DMCB,(RA),HCAP1,RR=RELO                               
         MVI   ESTSW,C'N'          NEW REQUEST                                  
         MVI   FORCEHED,C'Y'                                                    
         MVC   RQDPOVRD,QDPTMENU   OVERRIDE DAYPART MENU                        
         B     EXIT                                                             
         EJECT                                                                  
M20      CLI   MODE,CLTFRST                                                     
         BNE   M21                                                              
         CLC   QUESTOR(4),=C'*DBT'                                              
         BNE   *+12                                                             
         MVI   SPOTPROF+1,C'N'                                                  
         MVI   SPOTPROF+14,C'N'                                                 
         MVI   FCRDGOAL,C'Y'                                                    
         MVC   MULTISW,SPOTPROF+13                                              
         CLI   QMED,C'C'           ONLY APPLIES TO COMB. REPORTS                
         BE    *+8                                                              
         MVI   MULTISW,C'N'                                                     
         CLI   MULTISW,C'A'                                                     
         BNE   *+12                                                             
         MVI   FCRDGOAL,C'A'                                                    
         MVI   MULTISW,C'Y'                                                     
         CLI   MULTISW,C'Y'                                                     
         BE    *+8                                                              
         MVI   MULTISW,C'N'        SET MULTIPLE REPORT SWITCH                   
         B     EXIT                                                             
         EJECT                                                                  
M21      CLI   MODE,ESTFRST                                                     
         BNE   M3                                                               
*                                                                               
         L     RE,ADEST                                                         
         USING ESTHDR,RE                                                        
*                                                                               
         CLC   QPROG,=C'M2'                                                     
         BNE   M21AB                                                            
         L     RF,ADCLT                                                         
         USING CLTHDRD,RF                                                       
         TM    Q2USER+17,X'01'      UDEF=E1?                                    
         BZ    M21AA                NO                                          
         MVC   H7(L'CEU1),CEU1      YES - MOVE IN UDEF DESCRIPTION              
         MVC   H7+L'CEU1+2(L'EUSER1),EUSER1                                     
*                                                                               
M21AA    TM    Q2USER+17,X'02'      UDEF=E2?                                    
         BZ    M21AB                NO                                          
         MVC   H8(L'CEU2),CEU2      YES - MOVE IN UDEF DESCRIPTION              
         MVC   H8+L'CEU2+2(L'EUSER2),EUSER2                                     
         DROP  RE,RF                                                            
*                                                                               
M21AB    CLC   QPROG(2),=C'M2'                                                  
         BNE   M21A1                                                            
         CLI   RQGETBF,C'Y'                                                     
         BNE   M21A1                                                            
         GOTO1 GETBF,DMCB,(BPRD,WORK),SVBFORM                                   
*                                                                               
M21A1    GOTO1 MEDPRDRD,DMCB,(RA)                                               
         CLI   BPRD,X'FF'          FORCE SAME DEMS FOR POL REQ                  
         BNE   M21AX                                                            
         LA    RE,219              POINT TO POL SLOT                            
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF                                                       
         LA    RE,PTDEMO-PTPRDN(RE)  POL DEMO ADDRESS                           
         L     RF,PRDBUFF                                                       
         USING PTBUFFD,RF                                                       
         LA    R1,218              MAX LOOP                                     
M21A2    CLI   0(RF),0             ONLY ACTIVE PRODUCTS                         
         BE    *+10                                                             
         MVC   PTDEMO(L'PTDEMO+L'PTWGHT),0(RE)                                  
         AH    RF,PRDBUFLN                                                      
         BCT   R1,M21A2                                                         
         DROP  RF                                                               
         SPACE 1                                                                
M21AX    CLI   ESTSW,C'N'                                                       
         BNE   M141                                                             
         MVI   ESTSW,C'Y'          LOCK MEDBLOCK DATES FOR REQUEST              
*                                                                               
         L     RE,ADEST            OUT OF WEEK ROTATOR START DAY                
         USING ESTHDR,RE                                                        
*                                                                               
         MVC   PWPREFIX,SPACES                                                  
         OC    EPWPCT,EPWPCT       TEST PW CLIENT                               
         BNZ   *+14                                                             
         OC    ECOST2,ECOST2                                                    
         BZ    M21PWX                                                           
         MVC   PWPREFIX,=C' WI '                                                
         CLC   QAGY,=C'WI'                                                      
         BE    *+10                                                             
         MVC   PWPREFIX,=C'AGY '                                                
         CLI   QPWCV,C'Y'                                                       
         BNE   M21PWX                                                           
         MVC   PWPREFIX,=C'CLT '                                                
M21PWX   CLI   EOWSDAY,0           ONLY IF THERE IS ONE INPUT                   
         BE    *+10                                                             
         MVC   SPOTPROF+8(1),EOWSDAY                                            
         B     *+8                 TESTING ONLY                                 
         MVI   SPOTPROF+8,3        FORCE A DAY                                  
         DROP  RE                                                               
*                                                                               
         MVC   RQSTAFLT(1),QAFFIL     AFFILATE FILTER                           
         MVC   RQPRGTYP,QPRGTYPE   PROGRAM TYPE FILTER                          
         CLI   SPOTPROF+5,10                                                    
         BL    *+8                                                              
         MVI   SPOTPROF+5,0                                                     
         SPACE 2                                                                
         L     RF,ADAGY                                                         
         USING AGYHDRD,RF                                                       
         L     RE,ADCLT            CHECK FOR US AGENCY AND SPILL                
         USING CLTHDRD,RE                                                       
         CLI   CEXTRA+5,C'D'                                                    
         BE    *+8                                                              
         CLI   CEXTRA+5,C'Y'                                                    
         BE    *+8                                                              
         DROP  RE                                                               
         CLI   AGYPROF+7,C'C'                                                   
         BE    *+8                 NO SPILL IF US AGENCY                        
         MVI   SPOTPROF+5,0                                                     
         DROP  RF                                                               
         CLI   QOPT5,C' '          SPILL REPORTING OPTIONS                      
         BE    *+10                                                             
         MVC   PROGPROF+4(1),QOPT5                                              
         CLI   PROGPROF+4,C'N'     SPILL OVERRIDES                              
         BNE   *+8                                                              
         MVI   PROGPROF+4,0                                                     
         CLI   PROGPROF+4,0                                                     
         BE    *+10                                                             
         MVC   SPOTPROF+5(1),PROGPROF+4                                         
         NI    SPOTPROF+5,X'0F'                                                 
         SPACE 2                                                                
         CLI   QCOMPARE,C' '                                                    
         BNE   *+10                                                             
         MVC   QCOMPARE,PROGPROF   SET DATA COMPARE                             
         CLI   QDPTDET,C' '                                                     
         BNE   *+10                                                             
         MVC   QDPTDET,PROGPROF+1  SET DAYPART CONTROL                          
         MVI   CPPSW,0                                                          
         CLI   PROGPROF+2,C'Y'                                                  
         BNE   *+8                                                              
         MVI   CPPSW,1             SET CROSS/DAYPART CPP-M                      
*                                                                               
         CLC   QPROG,=C'U3'                                                     
         BE    SUBSOK                                                           
* READ D0 PROFILE TO GET LPMWK AND OVERNIGHTS OPTION                            
         MVC   WORK(10),=CL10'S0D0'                                             
         MVC   WORK+4(3),SVAGY                                                  
         MVC   WORK+7(3),QCLT                                                   
         GOTO1 GETPROF,DMCB,WORK,D0PROF,DATAMGR                                 
         CLI   Q2LPMWK,C'N'                                                     
         BNE   *+8                                                              
         MVI   D0PROF+10,C'N'                                                   
         CLI   Q2OVNITE,C'N'                                                    
         BNE   *+8                                                              
         MVI   D0PROF+11,C'N'                                                   
*                                                                               
         CLC   OVNIGHT,D0PROF+11     MODE SAME AS PREV                          
         BE    *+14                                                             
         MVC   OVNIGHT(1),D0PROF+11                                             
         B     M22                                                              
         CLC   LPMWK,D0PROF+10       MODE SAME AS PREV                          
         BE    *+14                                                             
         MVC   LPMWK(1),D0PROF+10                                               
         B     M22                                                              
*                                                                               
         CLC   OVNIGHT,Q2OVNITE      MODE SAME AS PREV                          
         BE    *+14                                                             
         MVC   OVNIGHT(1),Q2OVNITE                                              
         B     M22                                                              
         CLC   LPMWK,Q2LPMWK         MODE SAME AS PREV                          
         BE    *+14                                                             
         MVC   LPMWK(1),Q2LPMWK                                                 
         B     M22                                                              
         CLC   WEEKLY,Q2USER+2       MODE SAME AS PREV                          
         BE    SUBSOK                                                           
         MVC   WEEKLY(1),Q2USER+2                                               
                                                                                
M22      L     RE,MEDBUFF          SET UP FOR DATES                             
         USING MEDBLOCK,RE                                                      
         L     RF,=F'1272'                                                      
         XCEF                                                                   
         L     RE,MEDBUFF          CLEAR DATES                                  
*                                                                               
         CLI   D0PROF+10,C'Y'      MAY NEED WEEKLY BUCKETS                      
         BE    *+8                                                              
         CLI   D0PROF+11,C'Y'      MAY NEED WEEKLY BUCKETS                      
         BE    *+8                                                              
         CLI   D0PROF+11,C'M'      MAY NEED WEEKLY BUCKETS                      
         BE    *+8                                                              
         CLI   Q2LPMWK,C'Y'        MAY NEED WEEKLY BUCKETS                      
         BE    *+8                                                              
         CLI   Q2OVNITE,C'Y'       MAY NEED WEEKLY BUCKETS                      
         BE    *+8                                                              
         CLI   Q2USER+2,C'W'       MAY NEED WEEKLY BUCKETS                      
         BNE   *+10                                                             
         MVC   MEDNUMWK,=F'56'                                                  
*                                                                               
         MVC   MEDNUMMO,=F'12'                                                  
         MVC   MEDNUMPE,=F'1'                                                   
         DROP  RE                                                               
         MVC   DUB,=C'SPM204  '    RESET SUB PROGRAMS FOR NEW MODE              
         L     R7,MEDTABLE                                                      
         GOTO1 LOADER,DMCB,DUB,(R7)                                             
         MVC   MEDTABLE,DMCB+4                                                  
         MVC   DUB,=C'SPM201  '                                                 
         L     R7,SPECS                                                         
         GOTO1 LOADER,DMCB,DUB,(R7)                                             
         MVC   SPECS,DMCB+4                                                     
         GOTO1 MEDSEED,DMCB,(RA)                                                
*                                                                               
SUBSOK   GOTO1 MEDCLEAR,DMCB,MEDTABLE   SUBPROGRAMS LOADED AND SEEDED           
*                                                                               
         MVC   PAGE,=H'1'                                                       
*                                                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
                                                                                
         GOTO1 MEDDATE,DMCB,(RA)                                                
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDEXTAX,SPOTPROF+12                                             
         CLI   D0PROF+10,C'Y'      LPMW PROCESSING REQ WEEKS                    
         BE    PROCWTP                                                          
         CLI   D0PROF+11,C'Y'      OVN PROCESSING REQ WEEKS                     
         BE    PROCWTP                                                          
         CLI   D0PROF+11,C'M'      OVN PROCESSING REQ WEEKS                     
         BE    PROCWTP                                                          
         CLI   Q2OVNITE,C'Y'       WTP PROCESSING REQ WEEKS                     
         BE    PROCWTP                                                          
         CLI   Q2LPMWK,C'Y'        WTP PROCESSING REQ WEEKS                     
         BE    PROCWTP                                                          
         CLI   Q2USER+2,C'W'       WTP PROCESSING REQ WEEKS                     
         BE    PROCWTP                                                          
         LA    RF,MEDMON01         OTHERWISE MONTHS OR PERIODS                  
         ST    RF,MEDAFRST                                                      
*                                                                               
SETALAST ST    RF,MEDALAST         SAVE LAST ACTIVE MONTH                       
         LA    RF,12(RF)           SCAN FOR END OF MONTHS                       
         OC    0(4,RF),0(RF)                                                    
         BZ    *+8                                                              
         B     SETALAST                                                         
*                                                                               
         CLC   QBOOK1(3),=C'ACT'                                                
         BE    *+12                                                             
         LA    RF,MEDPERD          EXTRACT TOTALS ONLY                          
         ST    RF,MEDAFRST                                                      
         DROP  RE                                                               
PROCWTP  DS    0C                                                               
         SPACE 2                                                                
* SET UP CROSS DAYPART CPP/M OPTIONS                                            
         L     RE,MEDTABLE                                                      
         LA    R4,CPPCTRL                                                       
SETCPP   SR    R5,R5                                                            
         IC    R5,4(R4)            GET CONDITIONAL REPORT NUMBER                
         BCTR  R5,0                                                             
         MH    R5,=H'4'                                                         
         LA    R5,0(R5,RE)         POINT TO CONDITIONAL REPORT                  
         L     R1,0(R5)            POINT TO CONDITIONAL ROW DEF                 
         L     R7,4(R4)                                                         
         CLI   CPPSW,0                                                          
         BE    *+8                                                              
         L     R7,0(R4)            GET CPP COL DEF.                             
         LA    R7,0(R7)            CLEAR CONTROL BYTE                           
         ST    R7,4(R1)            SET COL DEF. ADDRESS                         
         LA    R4,8(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   SETCPP                                                           
         SPACE 2                                                                
*                                                                               
* OPTIMIZE BUFFALO                                                              
         CLI   SVONEPRD,0          MULTIPLE PRODUCTS                            
         BE    CHKSL                YES-SUBREPORTS REQUIRED                     
         LA    R8,SUPONE                                                        
         BAS   R9,SUPRPTS                                                       
*                                                                               
* DAYPART SPOT LENGTH SUPPRESSION ROUTINES                                      
*                                                                               
CHKSL    CLI   QDPTDET,C'B'        SUPPRESS SPOT LENGTH                         
         BE    *+12                                                             
         CLI   QDPTDET,C'C'                                                     
         BNE   CHKDP                                                            
         LA    R8,SUPSLN                                                        
         BAS   R9,SUPRPTS                                                       
*                                                                               
CHKDP    CLI   QDPTDET,C'C'        SUPPRESS DAYPART                             
         BNE   CHKSLTOT                                                         
         LA    R8,SUPDPSLN                                                      
         BAS   R9,SUPRPTS                                                       
*                                                                               
CHKSLTOT CLI   PROGPROF+3,C'Y'                                                  
         BE    M21EX                                                            
         LA    R8,SUPSLTOT                                                      
         BAS   R9,SUPRPTS                                                       
M21EX    GOTO1 =V(SETBUF),DMCB,(RA),LCODE,LVCNTRL,RR=RELO                       
         MVC   HICODE,LCODE                                                     
         L     R2,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',(R2)                                      
         B     M141                                                             
         EJECT                                                                  
M3       CLI   MODE,PROCBUY                                                     
         BNE   M4                                                               
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         LR    RF,R5               SET UP SECONDARY POST BYTES                  
         BAS   R9,SETPOST                                                       
         CLI   QPRGTYPE,C' '       PROGRAM TYPE FILTER                          
         BE    *+14                                                             
         CLC   BDPROGT,QPRGTYPE                                                 
         BNE   EXIT                                                             
         DROP  R5                                                               
         XC    PSLIST,PSLIST                                                    
*                                                                               
*                                                                               
         GOTO1 MEDPSL,DMCB,(RA),PSLIST                                          
*                                                                               
         L     RE,ADBUY            SAVE BUY DESCRIPTION TIME                    
         USING BDELEM,RE                                                        
         LA    RE,24(RE)                                                        
         MVC   BYTE,BDSEC                                                       
         DROP  RE                                                               
*                                                                               
         LA    RE,PSLIST                                                        
M3PIG    CLI   0(RE),0                                                          
         BE    M3PIGX                                                           
         CLC   BYTE,1(RE)          CLEAR IF DIFFERENT SPOT LENGTH               
         BE    *+8                  (MUST BE A PIGGYBACK)                       
         MVI   BYTE,0                                                           
         LA    RE,2(RE)                                                         
         B     M3PIG                                                            
*                                                                               
M3PIGX   MVC   0(2,RE),=X'FFFF'                                                 
         CLI   KEY+3,X'FF'         POL                                          
         BNE   M31                                                              
         CLI   BYTE,0              INDICATES PIGGYBACK                          
         BE    M32                 PROCESS BY PRODUCT                           
         L     RE,ADBUY            OTHERWISE PROCESS AS POL                     
         USING BDELEM,RE                                                        
         LA    RE,24(RE)                                                        
         MVI   PSLIST,X'FF'                                                     
         MVC   PSLIST+1(1),BDSEC                                                
         MVC   PSLIST+2(2),=X'FFFF'  END OF LIST                                
         B     M32                                                              
         DROP  RE                                                               
M31      LA    RE,PSLIST                                                        
M31A     CLC   0(2,RE),=X'FFFF'    CHECK FOR END                                
         BE    M32                                                              
         CLC   0(1,RE),KEY+3       PRODUCT OK                                   
         BNE   *+12                 NO - DELETE                                 
         LA    RE,2(RE)             YES - TRY NEXT                              
         B     M31A                                                             
         XC    0(2,RE),0(RE)                                                    
         LA    RE,2(RE)                                                         
         B     M31A                                                             
*                                                                               
M32      LA    R3,2                SET DEMO TYPE                                
         CLI   QRERATE,C' '                                                     
         BE    M322                                                             
         CLI   QRERATE,C'A'       ADJUST ONLY                                   
         BNE   M321                                                             
         LA    R3,5                                                             
         B     M322                                                             
M321     LA    R3,3                SET FOR PURCHASED RERATED                    
         CLC   QHUT1,=C'NO'                                                     
         BE    *+8                                                              
         LA    R3,1(R3)            SET FOR ADJUSTMENT                           
         CLI   QRERATE,C'I'       RERATE BASED ON INVOICE                       
         BNE   *+8                                                              
         LA    R3,3(R3)                                                         
*                                                                               
M322     LA    R2,PSLIST                                                        
M323     CLC   0(2,R2),=X'FFFF'     END                                         
         BE    EXIT                                                             
         CLI   0(R2),0             PRODUCT DELETED                              
         BNE   *+12                                                             
         LA    R2,2(R2)                                                         
         B     M323                                                             
         L     RE,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,RE                                                      
         MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
* PW CLIENT STUFF                                                               
         MVI   MEDEXTPW,C' '                                                    
         MVI   MEDEXMMR,C' '                                                    
         CLI   QPWCV,C'A'                                                       
         BL    *+14                                                             
         MVC   MEDEXTPW,QPWCV                                                   
         MVI   MEDEXMMR,C'Y'                                                    
*                                                                               
         CLI   QCOST2,C'Y'                                                      
         BNE   *+8                                                              
         MVI   MEDEXTPW,C'Y'                                                    
*                                                                               
         MVI   MEDEXCH,0                                                        
         L     RF,ADAGY            TEST CANADIAN AGENCY                         
         CLI   AGYPROF+7-AGYHDR(RF),C'C'                                        
         BNE   M323A                                                            
         L     RF,ADBUY            YES-TEST EXCHANGE ELEMENT                    
         TM    BDCIND2-BUYREC(RF),X'40'                                         
         BZ    M323A                                                            
         MVI   MEDEXCH,C'C'        YES-SET EXCHANGE TO CANADIAN DOLLARS         
         MVI   MEDCANTX,C'Y'       CPP MUST INCLUDE C58 AND MSF                 
*                                                                               
* CHECK FOR PURCH VS ACHIEVED                                                   
M323A    CLI   QCOMPARE,C'C'                                                    
         BE    M3ACH                                                            
         CLI   QCOMPARE,C'D'                                                    
         BE    M3ACH                                                            
M323B    GOTO1 MEDGETBY,DMCB,(RA),2     ANY ORDERED                             
         BAS   RE,SETPRMY                                                       
         L     RE,MEDBUFF                NO - BYPASS                            
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD                                                
         BZ    M325                                                             
         L     RE,BUYCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,BUYCNT                                                        
         GOTO1 MEDGETBY,DMCB,(RA),(R3)                                          
         BAS   RE,SETPRMY                                                       
         MVC   ACTAREA,4(R1)       SAVE ACTIVE BOOK LIST                        
*                                                                               
         L     RE,MEDBUFF                                                       
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD                                                
         BZ    M325                                                             
         CLI   QOPT4,C'Y'          TEST SUPPRESS DOLLARS                        
         BNE   *+8                                                              
         BAS   RE,CLRDOLS          YES                                          
         DROP  R4                                                               
         SPACE 2                                                                
         CLI   MEDSPILL,C'Y'                                                    
         BNE   M323NOSP                                                         
         CLI   SPOTPROF+5,0                                                     
         BE    EXIT                                                             
         CLI   SPLPRINT,1          PUT IN ORIGINATING BUFFER                    
         BNE   M323NOSP                                                         
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'01',SPBUFMKT),0,RR=RELO                
**       MVC   SPLPRINT,2                                                       
         MVI   SPLPRINT,2                                                       
         SPACE 2                                                                
M323NOSP MVI   ACTSW,1                                                          
         MVI   STACTSW,1                                                        
         GOTO1 MEDMKTWT,DMCB,(RA)                                               
M324     MVC   WORK(2),=X'6162'    SET BUFFALO CODES                            
         GOTO1 MEDPOST,DMCB,(RA)                                                
         CLI   MULTISW,C'N'        CHECK FOR SECONDARY POST                     
         BE    M325                                                             
         MVC   WORK(2),POSTWORK                                                 
         GOTO1 (RF),DMCB,(RA)                                                   
         B     M325                                                             
*                                                                               
M325     LA    R2,2(R2)                                                         
         B     M323                IS THERE ANOTHER PRODUCT                     
         EJECT                                                                  
* GET PURCHASE AND POST IN GOAL BUCKETS                                         
M3ACH    GOTO1 MEDGETBY,DMCB,(RA),2                                             
         L     RE,MEDBUFF                                                       
         CLI   MEDSPILL,C'Y'                                                    
         BNE   *+12                                                             
         CLI   SPOTPROF+5,0                                                     
         BE    EXIT                                                             
         BAS   RE,SETPRMY                                                       
*                                                                               
         L     RE,MEDBUFF                                                       
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD   ANY ACTIVITY                                 
         BZ    M323B               NO - EXIT                                    
*                                                                               
         CLI   MEDSPILL,C'Y'       CHECK IF SPILL ACTIVE                        
         BNE   M3ACH1                FOR RERATE                                 
         GOTO1 MEDGETBY,DMCB,(RA),(R3)                                          
         OC    MEDBYD(12),MEDBYD   ANY ACTIVITY                                 
         BZ    M325                NO - EXIT                                    
         GOTO1 MEDGETBY,DMCB,(RA),2                                             
* SET GOAL BUCKETS                                                              
M3ACH1   CLI   QOPT4,C'Y'          TEST SUPPRESS DOLLARS                        
         BNE   *+8                                                              
         BAS   RE,CLRDOLS          YES                                          
         MVC   MEDGLD,MEDBYD                                                    
         MVC   MEDGLDEQ,MEDBYDEQ                                                
         MVC   MEDGL1,MEDBY1                                                    
         MVC   MEDGL1EQ,MEDBY1EQ                                                
         XC    MEDBYD(52),MEDBYD   CLEAR DOLLARS, SPOTS, AND DEMOS              
*                                                                               
         DROP  R4                                                               
         DROP  RE                                                               
         GOTO1 MEDMKTWT,DMCB,(RA)                                               
         MVC   WORK(2),=X'6162'                                                 
         GOTO1 MEDPOST,DMCB,(RA)                                                
         CLI   MULTISW,C'N'        CHECK FOR SECONDARY POST                     
         BE    M323B                                                            
         MVC   WORK(2),POSTWORK                                                 
         GOTO1 (RF),DMCB,(RA)                                                   
         B     M323B                                                            
         EJECT                                                                  
M4       CLI   MODE,PROCGOAL                                                    
         BNE   M5                                                               
         CLI   QCOMPARE,C'C'       PURCHASED ONLY                               
         BE    EXIT                                                             
         CLI   QCOMPARE,C'D'       PURCHASE ONLY                                
         BE    EXIT                                                             
         LA    RE,KEY                                                           
         USING GOALREC,RE                                                       
         LA    RF,1(RE)            SET UP SECONDARY POST                        
         BAS   R9,SETPOST                                                       
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         MVC   MEDBRAND,GKEYPRD                                                 
         MVC   MEDSPTLN,GKEYSLN                                                 
         CLI   QPWCV,C'Y'                                                       
         BE    *+12                                                             
         CLI   QCOST2,C'Y'                                                      
         BNE   *+8                                                              
         MVI   MEDEXTPW,C'Y'                                                    
* CHECK FOR LOCKIN DATA                                                         
         CLI   QCOMPARE,C'E'                                                    
         BE    M4L                                                              
         CLI   QCOMPARE,C'F'                                                    
         BE    M4L                                                              
         GOTO1 MEDGETGL,DMCB,(RA)                                               
         BAS   RE,SETPRMY                                                       
         L     RF,MEDBUFF                                                       
         LA    RE,MEDPERD                                                       
         L     R4,4(RE)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDGLD(12),MEDGLD                                                
         BZ    EXIT                                                             
         CLI   QOPT4,C'Y'          TEST SUPPRESS DOLLARS                        
         BNE   *+8                                                              
         BAS   RE,CLRDOLS          YES                                          
         MVI   ACTSW,1                                                          
M42      GOTO1 MEDMKTWT,DMCB,(RA)                                               
         MVC   WORK(2),=X'6162'    SET BUFFALO CODES                            
         GOTO1 MEDPOST,DMCB,(RA)                                                
         CLI   MULTISW,C'N'        CHECK FOR SECONDARY POST                     
         BE    EXIT                                                             
         CLI   POSTWORK,X'61'                                                   
         BE    EXIT                                                             
         MVC   WORK(2),POSTWORK                                                 
         GOTO1 (RF),DMCB,(RA)                                                   
         B     EXIT                                                             
         DROP  RE,RF                                                            
         EJECT                                                                  
*                                                                               
* EXTRACT LOCKIN DATA AND THEN REFORMAT AS GOAL                                 
*                                                                               
M4L      L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   SAVNUMWK,MEDNUMWK   MEDGETLK DOESN'T GET MONTHS IF               
         XC    MEDNUMWK,MEDNUMWK   WEEKS ARE HERE                               
         MVI   MEDSPILL,C'O'                                                    
         GOTO1 MEDGETLK,DMCB,(RA)                                               
         L     RE,MEDBUFF                                                       
         MVC   MEDNUMWK,SAVNUMWK                                                
         BAS   RE,SETPRMY                                                       
*                                                                               
         L     RE,MEDBUFF                                                       
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDLKD(12),MEDLKD   EXIT IF NO ACTIVITY                          
         BZ    M4LSP                                                            
         CLI   QOPT4,C'Y'          TEST SUPPRESS DOLLARS                        
         BNE   *+8                                                              
         BAS   RE,CLRDOLS          YES                                          
         MVC   MEDGLD,MEDLKD       SET GOAL BUCKETS                             
         MVC   MEDGLDEQ,MEDLKDEQ                                                
         MVC   MEDGL1,MEDLK1                                                    
         MVC   MEDGL1EQ,MEDLK1EQ                                                
         BAS   RE,M4PST                                                         
M4LSP    CLI   SPOTPROF+5,0        ANY SPILL REPORTING                          
         BE    EXIT                NO - ONWARD                                  
         L     RE,MEDBUFF                                                       
         MVI   MEDSPILL,C'S'                                                    
         MVC   SAVNUMWK,MEDNUMWK   MEDGETLK DOESN'T GET MONTHS IF               
         XC    MEDNUMWK,MEDNUMWK   WEEKS ARE HERE                               
         GOTO1 MEDGETLK,DMCB,(RA)                                               
         L     RE,MEDBUFF                                                       
         MVC   MEDNUMWK,SAVNUMWK                                                
         BAS   RE,SETPRMY                                                       
*                                                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDLKD(12),MEDLKD   CHECK ACTIVITY                               
         BZ    EXIT                                                             
         CLI   QOPT4,C'Y'          TEST SUPPRESS DOLLARS                        
         BNE   *+8                                                              
         BAS   RE,CLRDOLS          YES                                          
         MVC   MEDGLD,MEDLKD       SET GOAL BUCKETS                             
         MVC   MEDGLDEQ,MEDLKDEQ                                                
         MVC   MEDGL1,MEDLK1                                                    
         MVC   MEDGL1EQ,MEDLK1EQ                                                
         BAS   RE,M4PST                                                         
         B     EXIT                                                             
M4PST    NTR1                                                                   
         GOTO1 MEDMKTWT,DMCB,(RA)                                               
         MVI   ACTSW,1                                                          
         MVC   WORK(2),=X'6162'    SET BUFFALO CODES                            
         GOTO1 MEDPOST,DMCB,(RA)                                                
         CLI   MULTISW,C'N'        CHECK FOR SECONDARY POST                     
         BE    M4PSTX                                                           
         CLI   POSTWORK,X'61'                                                   
         BE    M4PSTX                                                           
         MVC   WORK(2),POSTWORK                                                 
         GOTO1 (RF),DMCB,(RA)                                                   
M4PSTX   XIT1                                                                   
         DROP  RE,R4                                                            
SPM2R6   DC    F'0'                                                             
         EJECT                                                                  
M5       CLI   MODE,MKTLAST                                                     
         BNE   M6                                                               
         GOTO1 =V(SPRPFOOT),DMCB,(RA),RR=RELO                                   
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         CLC   QPRD,=C'POL'                                                     
         BNE   *+8                                                              
         MVI   MEDBRAND,X'FF'                                                   
         DROP  RE                                                               
         MVI   RCSUBPRG,1                                                       
         CLI   RTGSW,1                                                          
         BE    *+8                                                              
         MVI   RCSUBPRG,2                                                       
         MVI   FRSTTOT,C'Y'                                                     
         CLI   ACTSW,1                                                          
         BNE   EXIT                                                             
         CLI   QOPT1,C'D'                                                       
         BE    M53SUP                                                           
         MVC   WEIGHT,SPWEIGHT                                                  
         LA    RE,MYBUFIO                                                       
         LA    RF,400                                                           
         XCEF                                                                   
         XC    MID1,MID1                                                        
         MVC   MID1(4),MKT                                                      
         MVI   MID1+5,C'-'                                                      
         MVC   MID1+7(24),MKTNM                                                 
         CLI   SPOTPROF+1,C'N'                                                  
         BE    M5AA                                                             
         L     R5,=V(DICSECT)                                                   
         USING DICSECT,R5                                                       
         MVC   MID1+32(L'SP@COV),SP@COV                                         
         DROP  R5                                                               
         LA    RF,MID1+32+L'SP@COV-1                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,1(RF)                                                         
         MVI   0(RF),C'='                                                       
         LA    RF,1(RF)                                                         
         CURED WEIGHT,(5,(RF)),2,ALIGN=LEFT                                     
*                                                                               
M5AA     DS    0H                                                               
         MVI   FORCEMID,C'Y'                                                    
         MVI   PRTSW,0                                                          
         CLI   LINE,40                                                          
         BH    *+8                                                              
         MVI   PRTSW,1                                                          
         MVC   CURRLN,LINE                                                      
         SR    RE,RE                                                            
         IC    RE,CURRLN                                                        
         LA    RE,3(RE)                                                         
         STC   RE,CURRLN                                                        
         L     R4,BUFFIO                                                        
         XC    0(20,R4),0(R4)                                                   
         MVI   BUFCDE,X'21'                                                     
         MVI   0(R4),X'21'                                                      
         L     R3,BUFFBUFF                                                      
M5A      GOTO1 BUFFALO,DMCB,=C'HIGH',(R3),(R4),1                                
         CLC   0(1,R4),BUFCDE      SAME FILE                                    
         BNE   M52CA                                                            
         TM    8(R1),X'80'                                                      
         BO    M52CA                                                            
         CLI   MULTISW,C'Y'                                                     
         BNE   M5AA1                                                            
         MVI   FORCEMID,C'Y'                                                    
         LA    RE,MID1             SET MEDIA CAPTION                            
         CLI   MID1,C' '                                                        
         BE    *+8                                                              
         LA    RE,132(RE)                                                       
         MVC   0(132,RE),SPACES                                                 
         L     R5,=V(DICSECT)                                                   
         USING DICSECT,R5                                                       
         MVC   0(L'SP@COMTV,RE),SP@COMTV                                        
         CLI   0(R4),X'60'                                                      
         BH    M5AA1                                                            
         MVC   0(L'SP@NETTV,RE),SP@NETTV                                        
         CLI   0(R4),X'40'                                                      
         BH    M5AA1                                                            
         MVC   0(L'SP@SPTTV,RE),SP@SPTTV                                        
         DROP  R5                                                               
M5AA1    L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         MVI   MEDSLCNT,0                                                       
         MVI   MEDDPCNT,0                                                       
         B     M52                                                              
M51      GOTO1 BUFFALO,DMCB,=C'SEQ',(R3),(R4),1                                 
M52      CLC   0(1,R4),BUFCDE                                                   
         BNE   M52C                                                             
         TM    DMCB+8,X'80'                                                     
         BO    M52C                                                             
         OC    MYBUFIO+12(250),MYBUFIO+12                                       
         BZ    M51                                                              
         SPACE 2                                                                
         CLI   PROGPROF+5,C'Y'     SUPPRESS CROSS DAYPART DEMOS                 
         BNE   M52EDT                                                           
         CLI   MYBUFIO+2,X'FF'     IS IT A TOTAL LINE                           
         BNE   M52EDT                                                           
         USING BUFFRECD,R4                                                      
         XC    BFDGL(8),BFDGL      CLEAR OUT THE POINTS                         
         XC    BFDBY1,BFDBY1                                                    
         XC    BFDEMS,BFDEMS                                                    
         DROP  R4                                                               
         SPACE 2                                                                
M52EDT   GOTO1 MEDEDIT,DMCB,(RA),(R7)                                           
         CLI   DMCB,0              DONT PRINT THIS LINE                         
         BE    M51                                                              
         CLI   PRTSW,0                                                          
         BE    M52A                                                             
         CLI   MYBUFIO+2,X'FD'                                                  
         BE    *+8                                                              
         CLI   MYBUFIO+2,X'FE'                                                  
         BNE   *+8                                                              
         MVI   DMCB,1                                                           
         MVC   SPACING,DMCB                                                     
         TM    P1+9,X'F0'                                                       
         BO    M521                                                             
         GOTO1 MEDSTARS,DMCB,P1                                                 
M521     DS    0H                                                               
         GOTO1 REPORT                                                           
         GOTO1 =V(SPRPFOOT),DMCB,(RA),RR=RELO                                   
         LA    RE,MYBUFIO+12                                                    
         LA    RF,380                                                           
         XCEF                                                                   
         B     M51                                                              
M52A     SR    RE,RE               CHECK FOR OVERFLOW                           
         IC    RE,CURRLN                                                        
         LA    RE,1(RE)                                                         
         STC   RE,CURRLN                                                        
         CLC   CURRLN,MAXLINES                                                  
         BL    M51                                                              
         MVI   FORCEHED,C'Y'                                                    
M52B     MVI   PRTSW,1                                                          
         B     M52C1                                                            
         SPACE 2                                                                
M52C     CLI   PRTSW,1             HAS LINE BEEN PRINTED                        
         BNE   M52B                NO SET TO PRINT AND RETURN                   
M52CA    CLI   BUFCDE,X'60'        CHECK FOR END                                
         BH    M52D                                                             
         ZIC   RE,BUFCDE                                                        
         LA    RE,32(RE)                                                        
         STC   RE,BUFCDE                                                        
M52C1    XC    0(20,R4),0(R4)                                                   
         MVC   0(1,R4),BUFCDE                                                   
         B     M5A                                                              
         SPACE 2                                                                
M52D     CLI   PRTSW,0                                                          
         BE    M52B                                                             
*                                                                               
M53      L     R8,ACTAREA                                                       
         GOTO1 =V(VMDAPRNT),DMCB,(RA),(R8),1,RR=RELO                            
         SPACE 2                                                                
         OC    SPBUFMKT(29),SPBUFMKT   CHECK FOR SPILL                          
         BZ    M5NOSP                                                           
         L     RE,=V(DICSECT)                                                   
         USING DICSECT,RE                                                       
         MVC   P1(L'SP@SPILL),SP@SPILL                                          
         DROP  RE                                                               
*                                                                               
M53NXT2  DS    0H                                                               
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'02',SPBUFMKT),P1,RR=RELO               
         GOTO1 REPORT                                                           
M53SUP   XC    SPBUFMKT(250),SPBUFMKT                                           
         XC    SPBUFMKT+250(250),SPBUFMKT+250                                   
M5NOSP   MVC   DMCB+8(20),LVCNTRL                                               
         L     R3,BUFFBUFF                                                      
         TM    LVCNTRL,X'80'                                                    
         BO    M53A                                                             
         GOTO1 BUFFALO,DMCB,=C'ADD',(X'21',(R3))                                
         MVI   DMCB+4,X'41'                                                     
         GOTO1 (RF)                                                             
         MVI   DMCB+4,X'61'                                                     
         GOTO1 (RF)                                                             
         CLI   BPRD,X'FF'                                                       
         BE    M53A                                                             
         GOTO1 BUFFALO,DMCB,=C'ADD',(X'22',(R3))                                
         MVI   DMCB+4,X'42'                                                     
         GOTO1 (RF)                                                             
         MVI   DMCB+4,X'62'                                                     
         GOTO1 (RF)                                                             
M53A     GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'21',(R3)),(X'80',1)                    
         MVI   DMCB+4,X'41'                                                     
         GOTO1 (RF)                                                             
         MVI   DMCB+4,X'61'                                                     
         GOTO1 (RF)                                                             
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'22',(R3)),(X'80',1)                    
         MVI   DMCB+4,X'42'                                                     
         GOTO1 (RF)                                                             
         MVI   DMCB+4,X'62'                                                     
         GOTO1 (RF)                                                             
         MVI   ACTSW,0                                                          
         MVI   FORCEMID,C'N'                                                    
         MVI   MID1,C' '                                                        
         MVC   MID1+1(132),MID1                                                 
         B     EXIT                                                             
M6       CLI   MODE,MGR3LAST                                                    
         BNE   M7                                                               
         MVI   BUFCDE,X'21'                                                     
         MVI   LCODE,5                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
M7       CLI   MODE,MGR2LAST                                                    
         BNE   M8                                                               
         MVI   BUFCDE,X'21'                                                     
         MVI   LCODE,4                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
M8       CLI   MODE,MGR1LAST                                                    
         BNE   M9                                                               
         MVI   BUFCDE,X'21'                                                     
         MVI   LCODE,3                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
M9       CLI   MODE,PRDLAST                                                     
         BNE   M10                                                              
         TM    QMKT,X'F0'                                                       
         BO    EXIT                                                             
         L     RF,MEDBUFF                                                       
         MVC   MEDBRAND,BPRD                                                    
         MVI   RCSUBPRG,3                                                       
         CLI   RTGSW,1                                                          
         BE    *+8                                                              
         MVI   RCSUBPRG,4                                                       
         MVI   BUFCDE,X'21'                                                     
         MVI   LCODE,2                                                          
         BAS   R9,DOSUM                                                         
         MVI   RCSUBPRG,5                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
M10      CLI   MODE,PGR3LAST                                                    
         BNE   M11                                                              
         MVI   BUFCDE,X'22'                                                     
         MVI   LCODE,5                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
M11      CLI   MODE,PGR2LAST                                                    
         BNE   M12                                                              
         MVI   BUFCDE,X'22'                                                     
         MVI   LCODE,4                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
M12      CLI   MODE,PGR1LAST                                                    
         BNE   M13                                                              
         MVI   BUFCDE,X'22'                                                     
         MVI   LCODE,3                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
M13      CLI   MODE,CLTLAST                                                     
         BNE   M14                                                              
         CLC   QUESTOR(4),=C'*DBT' FUDGE FOR JWT                                
         BE    *+12                                                             
         TM    QMKT,X'F0'                                                       
         BO    EXIT                                                             
         MVI   BUFCDE,X'22'                                                     
         MVI   LCODE,2                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
M14      CLI   MODE,PRDFRST                                                     
         BNE   M15                                                              
         MVI   FORCEHED,C'Y'                                                    
M141     SR    RE,RE                                                            
         IC    RE,BPRD                                                          
         CLI   BPRD,X'FF'         GET DEMO NAMES FOR PRODUCT                    
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RE,28(RE,RF)                                                     
         XC    DNAME1(28),DNAME1                                                
         LA    R9,4                                                             
         SPACE 2                                                                
         ST    RE,FULL             NEW FORMAT                                   
         BAS   RE,NEWDNAM                                                       
         B     M14B                                                             
         SPACE 2                                                                
M14B     MVI   RTGSW,0                                                          
         CLI   DNAME1,C'E'         EXTENDED RATING                              
         BE    *+8                                                              
         CLI   DNAME1,C'R'         REGULAR RATING                               
         BNE   *+8                                                              
         MVI   RTGSW,1                                                          
*                                                                               
M15      CLI   MODE,MKTFRST                                                     
         BNE   M16                                                              
         MVI   ACTSW,0                                                          
         XC    SPBUFMKT(250),SPBUFMKT                                           
         XC    SPBUFMKT+250(250),SPBUFMKT+250                                   
         B     EXIT                                                             
*                                                                               
M16      CLI   MODE,REQLAST                                                     
         BNE   M17                                                              
         MVI   FORCEHED,C'N'                                                    
         MVI   P,0                                                              
         MVC   P2(132),FOOT1                                                    
         GOTO1 REPORT                                                           
         MVC   FOOT1(132),SPACES                                                
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
M17      CLI   MODE,STAFRST                                                     
         BNE   M18                                                              
         MVI   STACTSW,0                                                        
         MVI   SPLPRINT,1                                                       
         B     EXIT                                                             
*                                                                               
M18      CLI   MODE,STALAST                                                     
         BNE   M19                                                              
         CLI   STACTSW,1                                                        
         BNE   EXIT                                                             
         CLI   QOPT1,C'D'                                                       
         BE    EXIT                                                             
         L     R8,ACTAREA                                                       
         GOTO1 =V(VMDAPRNT),DMCB,(RA),(R8),0,RR=RELO                            
         B     EXIT                                                             
*                                                                               
M19      DS    0H                                                               
         B     EXIT                                                             
SUPRPTS  L     RE,MEDTABLE                                                      
         SR    RF,RF                                                            
         IC    RF,0(R8)                                                         
         BCTR  RF,0                                                             
         MH    RF,=H'4'                                                         
         LA    RF,0(RE,RF)                                                      
         OI    0(RF),X'80'                                                      
         LA    R8,1(R8)                                                         
         CLI   0(R8),0                                                          
         BNE   SUPRPTS                                                          
         BR    R9                                                               
         EJECT                                                                  
* ROUTINE TO CLEAR OUT ALL DOLLAR FIELDS FROM MEDBLOCK                          
*                                                                               
CLRDOLS  NTR1  ,                                                                
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         LA    RE,MEDDATES                                                      
         LA    R1,MEDTOTAL                                                      
*                                                                               
CLRDOLS2 ICM   R4,15,4(RE)                                                      
         BZ    CLRDOLS4                                                         
         USING MEDDATA,R4                                                       
         XC    MEDGLD(8),MEDGLD                                                 
         XC    MEDLKD(8),MEDLKD                                                 
         XC    MEDBYD(8),MEDBYD                                                 
*                                                                               
CLRDOLS4 LA    RE,12(RE)                                                        
         CR    RE,R1                                                            
         BNH   CLRDOLS2                                                         
*                                                                               
CLRDOLSX B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
DOSUM    L     R4,BUFFIO           DO SUMMARY REPORTS                           
         MVI   FORCEHED,C'Y'                                                    
         MVI   WTSW,1                                                           
         CLC   HICODE,LCODE                                                     
         BLR   R9                                                               
         ST    R9,SAVE9                                                         
REDOSUM  L     R3,BUFFBUFF                                                      
         CLI   SPDUPTOT,C'Y'                                                    
         BE    DOSUM3                                                           
         MVC   WEIGHT,SPWEIGHT                                                  
         MVI   SW1,0                                                            
         LA    RE,MYBUFIO                                                       
         LA    RF,400                                                           
         XCEF                                                                   
         MVC   0(1,R4),BUFCDE                                                   
         L     R3,BUFFBUFF                                                      
         SR    R5,R5                                                            
         IC    R5,LCODE                                                         
         BCTR  R5,0                                                             
         SLL   R5,2                                                             
         SR    R8,R8                                                            
         IC    R8,LCODE                                                         
         GOTO1 BUFFALO,DMCB,=C'HIGH',(R3),(R4),(R8)                             
         CLI   MULTISW,C'Y'                                                     
         BNE   DOSUM2                                                           
         MVI   FORCEMID,C'Y'                                                    
         LA    RE,MID1             SET MEDIA CAPTION                            
         CLI   MID1,C' '                                                        
         BE    *+8                                                              
         LA    RE,132(RE)                                                       
         MVC   0(132,RE),SPACES                                                 
         L     R2,=V(DICSECT)                                                   
         USING DICSECT,R2                                                       
         MVC   0(L'SP@COMTV,RE),SP@COMTV                                        
         CLI   0(R4),X'60'                                                      
         BH    DOSUM2                                                           
         MVC   0(L'SP@NETTV,RE),SP@NETTV                                        
         CLI   0(R4),X'40'                                                      
         BH    DOSUM2                                                           
         MVC   0(L'SP@SPTTV,RE),SP@SPTTV                                        
         B     DOSUM2                                                           
         DROP  R2                                                               
*                                                                               
DOSUM1   SR    R8,R8                                                            
         IC    R8,LCODE                                                         
         GOTO1 BUFFALO,DMCB,=C'SEQ',(R3),(R4),(R8)                              
DOSUM2   CLC   0(1,R4),BUFCDE      SAME FILE                                    
         BNE   DOSUM3                                                           
         TM    DMCB+8,X'80'                                                     
         BO    DOSUM3                                                           
         MVC   SPWEIGHT,WEIGHT     RESTORE WEIGHT                               
         CLC   0(1,R4),BUFCDE                                                   
         BNE   DOSUM3                                                           
         CLI   1(R4),0                                                          
         BE    DOSUM21                                                          
         TM    1(R4),X'80'         CLIENT SUMMARY                               
         BO    DOSUM2AI                                                         
         L     RE,PRDBUFF                                                       
         LA    RF,220                                                           
         SPACE 2                                                                
DOSUM2AC ZIC   R2,1(R4)            GET PRIMARY DEMO BRAND NEW FORMAT            
         BCTR  R2,0                                                             
         MH    R2,=H'3'                                                         
         LA    R2,PRMYTAB(R2)                                                   
DOSM2AC1 CLC   28(3,RE),0(R2)                                                   
         BE    DOSUM2AD                                                         
         AH    RE,PRDBUFLN                                                      
         BCT   RF,DOSM2AC1                                                      
         B     DOSUM1              BYPASS IF NO MATCH                           
*                                  (WILL HAPPEN IF SOME BRANDS HAVE             
*                                   NO DEMOS)                                   
         SPACE 2                                                                
DOSUM2AD L     RF,MEDBUFF                                                       
         MVC   MEDBRAND,0(RE)      SET PRIMARY DEMO BRAND                       
         B     DOSUM2A                                                          
DOSUM2AI MVI   RCSUBPRG,7                                                       
         B     DOSUM21                                                          
DOSUM2A  XC    DNAME1(7),DNAME1                                                 
DSM2AND  LA    R9,1                GET NEW FORMAT DEMO NAME                     
         ZIC   RF,1(R4)            GET PRIMARY DEMO SLOT                        
         BCTR  RF,0                                                             
         MH    RF,=H'3'                                                         
         LA    RF,PRMYTAB(RF)                                                   
         ST    RF,FULL                                                          
         BAS   RE,NEWDNAM                                                       
DOSM2AX  CLI   DNAME1,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAME1,C'R'                                                      
         BNE   *+8                                                              
         MVI   RTGSW,1                                                          
         MVI   RCSUBPRG,5                                                       
         CLI   RTGSW,1                                                          
         BE    *+8                                                              
         MVI   RCSUBPRG,6                                                       
         MVC   MID1(7),DNAME1                                                   
DOSUM21  CLC   MYBUFIO+1(1),PBUFFCDE                                            
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   PBUFFCDE,MYBUFIO+1                                               
         SPACE 2                                                                
         CLI   PROGPROF+5,C'Y'     SUPPRESS CROSS DAYPART DEMOS                 
         BNE   DOSUM22                                                          
         CLI   MYBUFIO+2,X'FF'     IS IT A TOTAL LINE                           
         BNE   DOSUM22                                                          
         USING BUFFRECD,R4                                                      
         XC    BFDGL(8),BFDGL      CLEAR OUT THE POINTS                         
         XC    BFDBY1,BFDBY1                                                    
         XC    BFDEMS,BFDEMS                                                    
         DROP  R4                                                               
         SPACE 2                                                                
DOSUM22  GOTO1 MEDEDIT,DMCB,(RA),(R7)                                           
         CLI   DMCB,0                                                           
         BE    DOSUM1                                                           
         CLI   MYBUFIO+2,X'FD'                                                  
         BE    *+8                                                              
         CLI   MYBUFIO+2,X'FE'                                                  
         BNE   *+8                                                              
         MVI   DMCB,1                                                           
         MVC   SPACING,DMCB                                                     
         TM    P1+9,X'F0'                                                       
         BO    DOSUM221                                                         
         GOTO1 MEDSTARS,DMCB,P1                                                 
DOSUM221 DS    0H                                                               
         GOTO1 REPORT                                                           
         GOTO1 =V(SPRPFOOT),DMCB,(RA),RR=RELO                                   
         LA    RE,MYBUFIO+12                                                    
         LA    RF,388                                                           
         XCEF                                                                   
         B     DOSUM1                                                           
DOSUM3   SR    R5,R5                                                            
         IC    R5,LCODE                                                         
DOSUM4   GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFCDE,(R3)),(X'80',(R5))                
         CLI   BUFCDE,X'61'        CHECK FOR END                                
         BH    DOSUMX                                                           
         ZIC   RE,BUFCDE                                                        
         LA    RE,32(RE)                                                        
         STC   RE,BUFCDE                                                        
         B     REDOSUM                                                          
DOSUMX   MVI   FORCEHED,C'Y'                                                    
         L     R9,SAVE9                                                         
         BR    R9                                                               
         SPACE 2                                                                
SETPOST  MVC   POSTWORK,=X'2122'   SET FOR SECONDARY POST                       
         MVC   HALF,0(RF)                                                       
         NI    HALF,X'0F'                                                       
         CLI   HALF,X'08'                                                       
         BNE   *+12                                                             
         MVC   POSTWORK,=X'6162'                                                
         BR    R9                                                               
         CLI   HALF,1              IS IT TV                                     
         BE    *+10                                                             
         MVC   POSTWORK,=X'4142'   SET FOR NETWORK                              
         BR    R9                                                               
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
         DROP  R6                                                               
         USING *,RF                                                             
MYHEAD   NTR1  BASE=SPM2RB                                                      
         LM    RA,RC,SPM2RA                                                     
         L     R6,SPM2R6                                                        
         DROP  RF                                                               
         USING SPM202+4096,R6                                                   
         GOTO1 =V(MYHEADC),DMCB,(RA),HCAP1,RR=RELO                              
*                                                                               
         LA    R4,H1+40                                                         
         SH    R4,=H'5'                                                         
         MVC   0(4,R4),PWPREFIX                                                 
*                                                                               
         MVC   H2+49(40),SPACES                                                 
         LA    R5,62               CENTER ON H1+66                              
         GOTO1 SQUASHER,DMCB,(R4),(R5)                                          
         L     R0,4(R1)            GET SQUASHED LENGTH                          
         GOTO1 UNDERLIN,(R1),((R0),(R4)),132(R4)                                
         GOTO1 CENTER,DMCB,(R4),(R5)                                            
         LA    R4,132(R4)                                                       
         GOTO1 (RF),(R1),(R4),(R5)                                              
         XIT1                                                                   
         EJECT                                                                  
NEWDNAM  NTR1                      GET NEW FORMAT DEMO NAMES                    
         L     R2,ADBLOCK          R9 HAS NUMBER OF DEMOS                       
         USING DBLOCK,R2           FULL HAS START OF LIST                       
         XC    0(256,R2),0(R2)                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
*                                                                               
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'                                                        
         BNE   *+12                                                             
         MVI   DBSELMED,C'R'                                                    
         B     NEWDCDX                                                          
*                                                                               
         L     RF,ADCLT                                                         
         CLI   CEXTRA-CLTHDR(RF),C'U' TEST US DEMOS                             
         BE    NEWDCDX                                                          
*                                                                               
         L     RF,ADAGY                                                         
         LA    RF,AGYPROF+7-AGYHDR(RF)                                          
         CLI   0(RF),C'C'          TEST CANADIAN AGY                            
         BNE   NEWDCDX                                                          
         MVI   DBSELMED,C'C'       SET CANADIAN MEDIA                           
*                                                                               
NEWDCDX  DS    0C                                                               
         DROP  R2                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         L     R0,VNONTNMS         COMSCORE DEMOS                               
         ST    R0,DMCB+16                                                       
         LA    RF,EUSRNMS          SET FOR USER NAMES                           
         DROP  RF                                                               
         L     R2,FULL                                                          
         GOTO1 DEMOCON,DMCB,((R9),(R2)),(2,DNAME1),(C'S',ADBLOCK),     X        
               (SPOTPROF+9,(RF))                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
* SET PRIMARY DEMO IN DEMO IN MEDBLOCK                                          
SETPRMY  NTR1                                                                   
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         ZIC   RE,MEDBRAND         GET PRODUCT SLOT                             
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF                                                       
         MVC   MEDPRIMY,28(RE)     EXTRACT OLD PRIMARY DEMO                     
         MVC   FULL(3),28(RE)      EXTRACT NEW PRIMARY DEMO                     
         LA    R9,PRMYTAB                                                       
         LA    R1,1                                                             
SETPRMY2 CLC   FULL(3),0(R9)       SAVE NEW PRIMARY DEMO IN TABLE               
         BE    SETPRMY4                                                         
         CLI   1(R9),0                                                          
         BE    SETPRMY3                                                         
         LA    R9,3(R9)                                                         
         LA    R1,1(R1)                                                         
         B     SETPRMY2                                                         
SETPRMY3 MVC   0(3,R9),FULL                                                     
SETPRMY4 STC   R1,MEDPRIMY         EQUATE NEW PRIMARY DEMO IN SLOT              
SETPRMYX XIT1                                                                   
         DS    0D                                                               
         EJECT                                                                  
         DROP  R6                                                               
         USING *,RF                                                             
BFHOOK   NTR1  BASE=SPM2RB                                                      
         LM    RA,RC,SPM2RA                                                     
         L     R6,SPM2R6                                                        
         DROP  RF                                                               
         USING SPM202+4096,R6                                                   
         L     R3,0(R1)                                                         
         USING BUFFRECD,R3                                                      
*                                                                               
         L     RF,BFSP             TACK ON WEIGHTED SPOTS                       
         SR    RE,RE                                                            
         M     RE,SPWEIGHT                                                      
         ST    RF,BFSPW                                                         
         OC    SPWEIGHT,SPWEIGHT   ENSURE A WEIGHT                              
         BNZ   *+10                                                             
         MVC   BFSPW,BFSP                                                       
*                                                                               
         CLI   BFDKEY+2,X'FF'       TOTAL LINE                                  
         BE    BFHOOKA                                                          
         CLI   MODE,PROCGOAL       ALWAYS WANT GOAL DETAILS                     
         BE    BFHEXIT                                                          
         L     R2,MEDBUFF                                                       
         USING MEDBLOCK,R2                                                      
         CLI   MEDSPILL,C'Y'                                                    
         BNE   BFHOOKA                                                          
         CLI   SPOTPROF+5,0        SUPPRESS SPILL                               
         BE    *+8                                                              
         CLI   SPOTPROF+5,1        SUPPRESS DETAIL SPILL                        
         BNE   BFHOOKA                                                          
         XC    BFDDATA,BFDDATA                                                  
         B     BFHEXIT                                                          
         SPACE 2                                                                
BFHOOKA  DS    0H                                                               
         CLI   BFDKEY+2,X'FF'                                                   
         BNE   BFHEXIT                                                          
         CLI   BFDKEY+11,X'FF'                                                  
         BE    *+8                                                              
         MVI   BFDKEY+2,X'FE'                                                   
         CLI   BFDKEY+12,X'01'                                                  
         BNE   BFHOOK1                                                          
         MVI   BFDKEY+2,X'FD'                                                   
BFHOOK1  L     R2,MEDBUFF                                                       
         USING MEDBLOCK,R2                                                      
         CLI   BFDKEY+2,X'FF'      OVERALL TOTALS                               
         BE    EXIT                                                             
         CLI   MODE,PROCGOAL                                                    
         BNE   BFHOOK1A                                                         
         CLI   BFDKEY+2,X'FD'      IS THIS A SPILL TOTAL                        
         BNE   BFHOOK1A                                                         
         XC    BFDDATA,BFDDATA     CLEAR IT                                     
         B     BFHEXIT                                                          
BFHOOK1A CLI   SPOTPROF+5,1        DO WE WANT S/O TOTALS                        
         BE    *+8                                                              
         CLI   SPOTPROF+5,3                                                     
         BE    *+14                                                             
         XC    BFDDATA,BFDDATA     RESET TOTALS                                 
         B     BFHEXIT                                                          
         SPACE 2                                                                
         CLI   BFDKEY+2,X'FD'      SPILL                                        
         BE    BFHOOK2                                                          
         CLI   MODE,PROCGOAL       ORIG GOALS ARE OK                            
         BE    EXIT                                                             
         CLI   MEDSPILL,C'Y'                                                    
         BNE   BFHEXIT                                                          
         XC    BFDDATA,BFDDATA                                                  
         B     BFHEXIT                                                          
         SPACE 2                                                                
BFHOOK2  CLI   MEDSPILL,C'Y'                                                    
         BE    BFHEXIT                                                          
         XC    BFDDATA,BFDDATA                                                  
         B     BFHEXIT                                                          
         SPACE 2                                                                
BFHEXIT  XIT1                                                                   
         DROP  R2                                                               
*                                                                               
SPM2RA   DC    F'0'                                                             
SPM2RB   DC    F'0'                                                             
SPM2RC   DC    F'0'                                                             
RELO     DS    F                                                                
HCAP1    DS    CL6                                                              
HCAP2    DS    CL9                                                              
         LTORG                                                                  
         EJECT                                                                  
LVCNTRL  DC    F'1'                                                             
         DC    A(2,3,4,5)                                                       
SUPONE   DC    AL1(5,6,7,8,9,10,11,12,14,24,25,19,0)                            
SUPSLN   DC    AL1(1,5,9,18,19,20,21,22,23,24,25,0)                             
SUPDPSLN DC    AL1(1,2,3,5,6,7,9,10,11,0)                                       
SUPSLTOT DC    AL1(18,19,20,21,22,23,24,25,0)                                   
CPPCTRL  DC    X'01',AL3(0),X'04',AL3(0)                                        
         DC    X'05',AL3(0),X'08',AL3(0)                                        
         DC    X'01',AL3(0),X'17',AL3(0)                                        
         DC    X'05',AL3(0),X'18',AL3(0)                                        
         DC    X'00'                                                            
CPPSW    DC    X'00'               CROSS DAYPART CPP SW                         
         EJECT                                                                  
PWPREFIX DS    CL4                 PREFIX FOR PW HEADLINES                      
SAVNUMWK DS    XL4                                                              
PSLIST   DS    CL150              PRODUCT SPOT LENGTH LIST                      
PRMYTAB  DS    CL60                                                             
PRTSW    DS    C                                                                
FRSTTOT  DS    C                                                                
CURRLN   DS    C                                                                
SW1      DS    C                   DATA SWITCH                                  
FOOTLIN  DS    F                                                                
WEIGHT   DC    F'1'                                                             
SAVE9    DS    F                                                                
ACTAREA  DS    F                                                                
BUYCNT   DC    F'0'                                                             
POSTWORK DS    CL2                                                              
WTSW     DS    C                                                                
ACTSW    DS    C                                                                
RTGSW    DS    C                   RATING SWITCH                                
BUFCDE   DS    C                   BUFFALO CODE                                 
LCODE    DS    C                   LEVEL CODE                                   
HICODE   DS    C                   HIGHEST LEVEL                                
PBUFFCDE DS    C                                                                
STACTSW  DS    C                                                                
SVMAXLIN DS    C                                                                
MULTISW  DS    C                   SWITCH FOR MULTI MEDIA REPORTS               
WEEKLY   DS    C                                                                
OVNIGHT  DS    C                                                                
LPMWK    DS    C                                                                
DNAME1   DS    CL7                                                              
DNAME2   DS    CL7                                                              
DNAME3   DS    CL7                                                              
DNAME4   DS    CL7                                                              
SPLPRINT DS    C                                                                
D0PROF   DS    CL16                                                             
SPBUFMKT DS    CL500                                                            
MYBUFIO  DS    CL400                                                            
BUFFRECD DSECT                                                                  
BFDKEY   DS    CL20                                                             
BFDDATA  DS    0CL64                                                            
BFDGL    DS    CL16                                                             
BFDBY1   DS    CL8                                                              
BFDDL    DS    CL8                                                              
BFSP     DS    CL4                                                              
BFDEMS   DS    CL24                                                             
BFSPW    DS    CL4                                                              
         EJECT                                                                  
MYHEADC  CSECT                                                                  
         NMOD1 0,MYHEAD                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
*                                                                               
         L     R5,=V(DICSECT)                                                   
         USING DICSECT,R5                                                       
*                                                                               
* RQGETBF IS SET BASED ON LOCAL TESTS FOR Q2NET IN SPREPD202                    
*                                                                               
         CLC   =C'DOLLARS',H12+44                                               
         BNE   MYH10                                                            
         CLI   RQGETBF,C'Y'        TEST BILL FORMULA ADJ                        
         BNE   MYH4                                                             
         MVC   H12+44(7),=C'CLT DOL'                                            
         B     MYH10                                                            
*                                                                               
MYH4     CLI   RQGETBF,C'X'        TEST NET DOLLAR REQUEST                      
         BNE   MYH10                                                            
         MVC   H12+44(7),=C'NET DOL'                                            
*                                                                               
MYH10    DS    0H                                                               
*                                                                               
         L     R7,4(R1)                                                         
         USING HCAP1,R7                                                         
         CLI   QFILTER,C'F'        FILM NUMBER FILTER FOR COKE                  
         BNE   MYHCTAX                                                          
         MVC   H8(L'SP@FILM),SP@FILM                                            
         MVC   H8+9(1),QFILTER+1                                                
         ICM   RF,15,CMLPTR                                                     
         BZ    *+10                                                             
         MVC   H8+9(4),0(RF)                                                    
MYHCTAX  L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         CLI   MEDEXTAX,C'Y'                                                    
         BNE   MYHSUBSX                                                         
         MVC   H8+50(L'SP@TXEX),SP@TXEX                                         
         DROP  RE                                                               
MYHSUBSX DS    0H                                                               
MYHPGRP  CLI   MODE,PGR3LAST                                                    
         BL    MYHPGRPX                                                         
         CLI   MODE,PGR1LAST                                                    
         BH    MYHPGRPX                                                         
         MVC   H7+50(32),H6+50                                                  
         XC    H6+50(32),H6+50                                                  
         CLI   MODE,PGR1LAST                                                    
         BNE   *+10                                                             
         MVC   H6+60(12),PGR1BK                                                 
         CLI   MODE,PGR2LAST                                                    
         BNE   *+10                                                             
         MVC   H6+60(12),PGR2BK                                                 
         CLI   MODE,PGR3LAST                                                    
         BNE   *+10                                                             
         MVC   H6+60(12),PGR3BK                                                 
MYHPGRPX DS    0H                                                               
         CLI   RCSUBPRG,5          PRIMARY DEMO REPORT                          
         BE    *+8                                                              
         CLI   RCSUBPRG,6                                                       
         BNE   MYHEAD1                                                          
         MVC   H11+20(6),HCAP1                                                  
         MVC   H11+45(9),HCAP2                                                  
         B     MYHEAD3                                                          
MYHEAD1  CLI   RCSUBPRG,5          CLIENT SUMMARY                               
         BL    MYHEAD2                                                          
         MVC   H11+20(6),HCAP1                                                  
         MVC   H11+43(9),HCAP2                                                  
         B     MYHEAD3                                                          
MYHEAD2  MVC   H11+14(6),HCAP1     DETAIL LINES                                 
         MVC   H11+45(9),HCAP2                                                  
MYHEAD3  DS    0H                                                               
         CLI   MODE,PRDLAST                                                     
         BH    MYHEADX                                                          
         MVC   H11+21(7),DNAME1                                                 
         CLI   DNAME2,0                                                         
         BE    MYHEADX                                                          
         MVC   H11+89(11),DASH                                                  
         MVC   H11+91(7),DNAME2                                                 
         MVC   H12+89(L'SP@PNTS),SP@PNTS                                        
         MVC   H12+97(L'SP@CPP),SP@CPP                                          
         CLI   DNAME2,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAME2,C'R'                                                      
         BE    *+22                                                             
         MVC   H12+89(L'SP@PNTS),SPACES CLEAR ANY RESIDUE                       
         MVC   H12+89(L'SP@IMPS),SP@IMPS                                        
         MVC   H12+97(L'SP@CPM),SP@CPM                                          
         CLI   DNAME3,0                                                         
         BE    MYHEADX                                                          
         MVC   H11+104(11),DASH                                                 
         MVC   H11+106(7),DNAME3                                                
         MVC   H12+104(L'SP@PNTS),SP@PNTS                                       
         MVC   H12+112(L'SP@CPP),SP@CPP                                         
         CLI   DNAME3,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAME3,C'R'                                                      
         BE    *+22                                                             
         MVC   H12+104(L'SP@PNTS),SPACES                                        
         MVC   H12+104(L'SP@IMPS),SP@IMPS                                       
         MVC   H12+112(L'SP@CPM),SP@CPM                                         
         CLI   DNAME4,0                                                         
         BE    MYHEADX                                                          
         MVC   H11+119(11),DASH                                                 
         MVC   H11+121(7),DNAME4                                                
         MVC   H12+119(L'SP@PNTS),SP@PNTS                                       
         MVC   H12+127(L'SP@CPP),SP@CPP                                         
         CLI   DNAME4,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAME4,C'R'                                                      
         BE    *+22                                                             
         MVC   H12+119(L'SP@PNTS),SPACES                                        
         MVC   H12+119(L'SP@IMPS),SP@IMPS                                       
         MVC   H12+127(L'SP@CPM),SP@CPM                                         
         DROP  R5                                                               
*                                                                               
         CLI   RQLKGLS,C'Y'                                                     
         BNE   MYHEADX                                                          
* FIND 'GOALS' AND REPLACE WITH 'LKGLS'                                         
         LA    R1,H11                                                           
         LHI   R0,32                                                            
MYHEAD10 CLC   0(5,R1),=C'GOALS'                                                
         BE    MYHEAD12                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,MYHEAD10                                                      
         B     MYHEADX                                                          
MYHEAD12 MVC   0(5,R1),=C'LKGLS'                                                
*                                                                               
MYHEADX  XIT1                                                                   
DASH     DC    11C'-'                                                           
         LTORG                                                                  
         EJECT                                                                  
SETBUF   CSECT                                                                  
         NMOD1 0,SETBUF                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R4,4(R1)            LOCODE                                       
         L     R5,8(R1)            LVCNTRL                                      
         MVI   0(R4),2             DETERMINE NUMBER OF LEVELS REQUIRED          
         CLI   QOPT1,C'S'                                                       
         BE    M2A                                                              
         CLI   MGR1LEN,0                                                        
         BNE   *+8                                                              
         CLI   PGR1LEN,0                                                        
         BE    M2A                                                              
         MVI   0(R4),3                                                          
         CLC   MGR1LEN,MGR2LEN                                                  
         BNE   *+10                                                             
         CLC   PGR1LEN,PGR2LEN                                                  
         BE    M2A                                                              
         MVI   0(R4),4                                                          
         CLC   MGR2LEN,MGR3LEN                                                  
         BNE   *+10                                                             
         CLC   PGR2LEN,PGR3LEN                                                  
         BE    M2A                                                              
         MVI   0(R4),5                                                          
*                                                                               
M2A      LA    RE,5                SET BUFFALO CONTROLS                         
         LR    RF,R5                                                            
         NI    0(RF),X'7F'         CLEAR STOP CHARACTER                         
         LA    RF,4(RF)                                                         
         BCT   RE,*-8                                                           
         IC    RE,0(R4)                                                         
         BCTR  RE,0                                                             
         SLL   RE,2                TIMES 4                                      
         LA    RE,0(R5,RE)                                                      
         OI    0(RE),X'80'                                                      
*                                                                               
* SET BUFFALO LEVELS                                                            
         L     R2,BUFFBUFF                                                      
         USING BUFFALOD,R2                                                      
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),0(R4)                                                  
         MVC   BUFFROWS+2(2),HALF                                               
         L     R9,BUFFLALL         GET MAXIMUM CORE AVILABLE                    
         SR    R8,R8                                                            
         M     R8,BUFFCRMX                                                      
         L     R7,BUFFWROW         GET NEW DATA LENGTH                          
         MH    R7,HALF                                                          
         ST    R7,BUFFLDTA                                                      
         A     R7,BUFFLKEY         GET NEW RECORD LENGTH                        
         ST    R7,BUFFLALL                                                      
         DR    R8,R7               GET NEW MAXIMUM RECORDS                      
         ST    R9,BUFFCRMX                                                      
         XMOD1 1                                                                
         LTORG                                                                  
FIXHED   CSECT                                                                  
         NMOD1 0,M2FIXHED                                                       
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
*                                                                               
         L     RE,=V(DICSECT)                                                   
         USING DICSECT,RE                                                       
*                                                                               
         L     R7,4(R1)                                                         
         USING HCAP1,R7                                                         
         MVC   HCAP1,SPACES                                                     
         MVC   HCAP2,SPACES                                                     
         MVC   HCAP1(L'SP@GOAL),SP@GOAL                                         
         CLI   QOPT6,C'Y'                                                       
         BNE   *+10                                                             
         MVC   HCAP1(6),=C' LKGL '                                              
         MVC   HCAP2(L'SP9PURCH),SP9PURCH                                       
         CLI   QRERATE,C'P'                                                     
         BNE   *+10                                                             
         MVC   HCAP2(L'SP@ACHVD),SP@ACHVD                                       
         CLI   QRERATE,C'I'                                                     
         BNE   *+10                                                             
         MVC   HCAP2(L'SP@AFFDV),SP@AFFDV                                       
         CLI   QCOMPARE,C'B'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'D'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'F'                                                    
         BNE   *+14                                                             
         MVC   HCAP2(L'SP@AFFDV),SP@AFFDV                                       
         MVI   QRERATE,C'I'                                                     
         CLI   QCOMPARE,C'C'                                                    
         BNE   *+10                                                             
         MVC   HCAP2(L'SP@ACHVD),SP@ACHVD                                       
         CLI   QCOMPARE,C'C'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'D'                                                    
         BNE   *+10                                                             
         MVC   HCAP1(L'SP6PURCH),SP6PURCH                                       
         CLI   QCOMPARE,C'E'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'F'                                                    
         BNE   *+10                                                             
         MVC   HCAP1(L'SP@ORDER),SP@ORDER                                       
         DROP  RE                                                               
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
DICSECT  CSECT                                                                  
         PRINT GEN                                                              
DCLIST   DS    0C                                                               
         DCDDL SP#COV,10                                                        
         DCDDL SP#COMTV,16                                                      
         DCDDL SP#NETTV,11                                                      
         DCDDL SP#SPTTV,11                                                      
         DCDDL SP#SPILL,11                                                      
         DCDDL SP#FILM,8,C                                                      
         DCDDL SP#TXEX,18,C                                                     
         DCDDL SP#PNTS,5                                                        
         DCDDL SP#CPP,3                                                         
         DCDDL SP#IMPS,4                                                        
         DCDDL SP#CPM,3                                                         
         DCDDL SP#GOAL,6,F                                                      
         DCDDL SP#PURCH,9,L,LABEL=SP9PURCH                                      
         DCDDL SP#PURCH,6,R,LABEL=SP6PURCH                                      
         DCDDL SP#ACHVD,9                                                       
         DCDDL SP#AFFDV,9                                                       
         DCDDL SP#ORDER,6,R                                                     
DCLISTX  DC    X'00'                                                            
*                                                                               
DSLIST   DS    0C                                                               
         DSDDL PRINT=YES                                                        
DSLISTX  EQU   *                                                                
         PRINT NOGEN                                                            
*                                                                               
         EJECT                                                                  
         BUFF  LINES=120,ROWS=5,COLUMNS=16,FLAVOR=BINARY,KEYLIST=(20,A)         
         EJECT                                                                  
         PRINT OFF                                                              
* SPREPWORKD                                                                    
       ++INCLUDE SPREPWORKD                                                     
         ORG   QGRP                                                             
QOPT6    DS    C            REPORT LOCKED GOALS                                 
*                                                                               
         ORG   Q2USER+16    SET TO COL 37                                       
Q2NET    DS    C                                                                
         ORG                                                                    
         EJECT                                                                  
* MEDPRTOPT                                                                     
*       +INCLUDE MEDRPTOPT                                                      
* SPREPPTBUF                                                                    
       ++INCLUDE SPREPPTBUF                                                     
         EJECT                                                                  
* SPREPMODES                                                                    
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
* SPMEDBLOCK                                                                    
       ++INCLUDE SPMEDBLOCK                                                     
         EJECT                                                                  
       ++INCLUDE DDBUFFALOD                                                     
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
* SPGENBUY                                                                      
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
* SPGENGOAL                                                                     
       ++INCLUDE SPGENGOAL                                                      
* SPGENEST                                                                      
       ++INCLUDE SPGENEST                                                       
* SPGENCLT                                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
* SPGENMKT                                                                      
       ++INCLUDE SPGENMKT                                                       
* DEDBLOCK                                                                      
       ++INCLUDE DEDBLOCK                                                       
* DDDICTATED                                                                    
       ++INCLUDE DDDICTATED                                                     
* SPDDEQUS                                                                      
       ++INCLUDE SPDDEQUS                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'217SPREPM202 11/21/19'                                      
         END                                                                    
EJECT                                                                           
*                                                                               
* SPGENMKT                                                                      
