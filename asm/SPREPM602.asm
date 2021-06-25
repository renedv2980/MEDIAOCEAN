*          DATA SET SPREPM602  AT LEVEL 063 AS OF 11/21/19                      
*PHASE SPM602T                                                                  
*INCLUDE SPRPFOOT                                                               
*INCLUDE MEDAPRNT                                                               
*INCLUDE REPSPILL                                                               
         TITLE 'SPREPM602-PB BRAND PERFORMANCE'                                 
*                                                                               
* MEDIA SUMMARY BUFFALO LEVELS                                                  
*    COLUMN DEFINITION 1-2  (DETAIL)                                            
*        LEVEL 1 = DETAIL ITEMS                                                 
*        LEVEL 2 = MARKET GROUP 3  (MGR3)                                       
*        LEVEL 3 = MARKET GROUP 2  (MGR2)                                       
*        LEVEL 4 = MARKET GROUP 1  (MGR1)                                       
*        LEVEL 5 = PRODUCT TOTALS                                               
*    COLUMN DEFINITION 4-5  (PRIMARY DEMO)                                      
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
SPM602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPM602,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING SPM602+4096,R2                                                   
         ST    R2,SPM6R2                                                        
         STM   RA,RC,SPM6RA                                                     
         ST    R5,RELO                                                          
         GOTO1 =V(SPRPFOOT),DMCB,(RA),RR=RELO                                   
**       MVC   SPSUPMKT,C'N'                                                    
         MVI   SPSUPMKT,C'N'                                                    
         CLI   MODE,MKTLAST                                                     
         BH    *+8                                                              
         MVI   SPSUPMKT,C'Y'                                                    
         CLI   MODE,MKTLAST                                                     
         BL    BYPW                                                             
*                                                                               
         CLC   QPRD,=C'POL'        FOR POL REQUEST                              
         BNE   *+12                                                             
         L     RE,MEDBUFF          NEED TO RESET PRODUCT TO GET CORRECT         
         USING MEDBLOCK,RE         DEMO LIST FOR MEDEDIT                        
         MVI   MEDBRAND,X'FF'                                                   
*                                                                               
         GOTO1 MEDADDWT,DMCB,(RA)                                               
         CLI   QOPT1,C'D'                                                       
         BNE   BYPW                                                             
         CLI   FRSTTOT,C'Y'                                                     
         BNE   BYPW                                                             
         MVI   SPDUPTOT,C'N'                                                    
         MVI   FRSTTOT,C'Y'                                                     
BYPW     DS    0H                                                               
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   M2                                                               
         MVC   SVMAXLIN,MAXLINES                                                
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDNUMWK,=F'56'     FOR WTP                                      
         MVC   MEDNUMMO,=F'12'     FOR WTP                                      
         MVC   MEDNUMPE,=F'1'                                                   
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         MVI   SPSUPMKT,C'Y'                                                    
         MVI   MEDEXTDM,2                                                       
         LA    RE,MYBUFIO                                                       
         ST    RE,BUFFIO                                                        
         L     R9,=V(BUFFALOC)                                                  
         A     R9,RELO                                                          
         ST    R9,BUFFBUFF                                                      
         USING BUFFALOD,R9                                                      
         LA    RE,BFHOOK                                                        
         ST    RE,BUFFHOOK                                                      
         DROP  R9                                                               
         LA    RE,MYBUFIO                                                       
         ST    RE,BUFFIO                                                        
         GOTO1 BUFFALO,DMCB,=C'SET',(R9)                                        
         GOTO1 MEDSEED,DMCB,(RA)                                                
         MVI   CPPSW,0             DEFAULT IS NO CROSS-DAYPART CPP/M            
         LA    RF,MYHEAD                                                        
         ST    RF,HEADHOOK                                                      
         DROP  RE                                                               
*                                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
M2       CLI   MODE,REQFRST                                                     
         BNE   M21                                                              
*                                                                               
         L     RE,ADAGY                                                         
         CLI   AGYPROF+7-AGYHDR(RE),C'C' TEST CANADIAN                          
         JNE   *+8                                                              
         OI    RQOPT2,RQOPT2_NETBUYS                                            
*                                                                               
         GOTO1 =V(SPRPFOOT),DMCB,(RA),RR=RELO                                   
         MVC   MAXLINES,SVMAXLIN                                                
         CLI   FOOT1,C' '                                                       
         BE    M2NOFT                                                           
         ZIC   R0,MAXLINES                                                      
         SH    R0,=H'3'                                                         
         STC   R0,MAXLINES                                                      
         MVC   FOOT1(132),SPACES                                                
M2NOFT   DS    0H                                                               
         GOTO1 =V(RQFRSTC),DMCB,(RA),HCAP1,RR=RELO                              
         B     EXIT                                                             
         EJECT                                                                  
M21      CLI   MODE,ESTFRST                                                     
         BNE   M3                                                               
         CLI   ESTSW,C'N'                                                       
         BNE   M141                                                             
         MVI   ESTSW,C'Y'          LOCK MEDBLOCK DATES FOR REQUEST              
         MVI   FCRDGOAL,C'Y'                                                    
*                                                                               
         MVI   RQLKGLS,C'N'                                                     
         CLI   QOPT6,C'Y'          TEST TO REPORT LOCKED GOALS                  
         BNE   *+8                                                              
         MVI   RQLKGLS,C'Y'                                                     
*                                                                               
         L     RE,ADEST            OUT OF WEEK ROTATOR START DAY                
         USING ESTHDR,RE                                                        
         CLI   EOWSDAY,0           ONLY IF THERE IS ONE INPUT                   
         BE    *+10                                                             
         MVC   SPOTPROF+8(1),EOWSDAY                                            
         B     *+8                 TESTING ONLY                                 
         MVI   SPOTPROF+8,3        FORCE A DAY                                  
         DROP  RE                                                               
*                                                                               
         MVC   MULTISW,SPOTPROF+13                                              
         CLI   QMED,C'C'           OPTION APPLIES TO COMB. ONLY                 
         BE    *+8                                                              
         MVI   MULTISW,C'N'                                                     
         CLI   MULTISW,C'A'                                                     
         BNE   *+12                                                             
         MVI   FCRDGOAL,C'A'                                                    
         MVI   MULTISW,C'Y'                                                     
         CLI   MULTISW,C'Y'                                                     
         BE    *+8                                                              
         MVI   MULTISW,C'N'                                                     
         MVC   RQSTAFLT(1),QAFFIL     AFFILATE FILTER                           
         MVC   RQPRGTYP,QPRGTYPE   PROGRAM TYPE FILTER                          
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDEXTAX,SPOTPROF+12                                             
         DROP  RE                                                               
         SPACE 2                                                                
         L     RF,ADAGY                                                         
         USING AGYHDRD,RF                                                       
         L     RE,ADCLT            CHECK FOR US AGENCY AND SPILL                
         USING CLTHDR,RE                                                        
         CLI   CEXTRA+5,C'D'                                                    
         BE    *+8                                                              
         CLI   CEXTRA+5,C'Y'                                                    
         BE    *+8                                                              
         DROP  RE                                                               
         CLI   AGYPROF+7,C'C'                                                   
         BE    *+8                                                              
         MVI   SPOTPROF+5,0                                                     
         CLI   SPOTPROF+5,10       CHECK FOR VALID OPTION                       
         BL    *+8                                                              
         MVI   SPOTPROF+5,0        KILL IF INVALID                              
         DROP  RF                                                               
*                                                                               
         CLI   QOPT5,C' '          SET SPILL REPORTING OPTIONS                  
         BE    *+10                                                             
         MVC   PROGPROF+5(1),QOPT5 REQUEST OVERRRIDE                            
         CLI   PROGPROF+5,C'N'     USER FREINDLY CODEING FOR NO SPILL           
         BNE   *+8                                                              
         MVI   PROGPROF+5,0                                                     
         CLI   PROGPROF+5,0        PROGRAM OR REQUEST OVERRIDE                  
         BE    *+10                                                             
         MVC   SPOTPROF+5(1),PROGPROF+5                                         
         NI    SPOTPROF+5,X'0F'                                                 
*                                                                               
* READ D0 PROFILE TO GET LPMWK AND OVERNIGHTS OPTION                            
         MVC   WORK(10),=CL10'S0D0'                                             
         MVC   WORK+4(3),SVAGY                                                  
         MVC   WORK+7(3),QCLT                                                   
         GOTO1 GETPROF,DMCB,WORK,D0PROF,DATAMGR                                 
         XC    WORK,WORK                                                        
*                                                                               
         L     RE,MEDBUFF          CLEAR DATES                                  
         USING MEDBLOCK,RE                                                      
         L     RF,=F'1272'                                                      
         XCEF                                                                   
*                                                                               
         L     RE,MEDBUFF                                                       
*                                                                               
         CLI   Q2LPMWK,C'N'                                                     
         BNE   *+8                                                              
         MVI   D0PROF+10,C'N'                                                   
         CLI   Q2OVNITE,C'N'                                                    
         BNE   *+8                                                              
         MVI   D0PROF+11,C'N'                                                   
*                                                                               
         CLI   D0PROF+10,C'Y'      LPMWK=Y NEED WEEKLY DATES                    
         BE    *+8                                                              
         CLI   D0PROF+11,C'Y'      OVN NEED WEEKLY                              
         BE    *+8                                                              
         CLI   D0PROF+11,C'M'      OVN NEED WEEKLY                              
         BE    *+8                                                              
         CLI   Q2LPMWK,C'Y'        LPMWK=Y NEED WEEKLY DATES                    
         BE    *+8                                                              
         CLI   Q2OVNITE,C'Y'       OVN NEED WEEKLY                              
         BE    *+8                                                              
         CLI   Q2USER+2,C'W'       WTP                                          
         BNE   *+10                                                             
         MVC   MEDNUMWK,=F'56'     FOR WTP                                      
         OI    RQOPTS,RQOPTS_POST                                               
*                                                                               
         MVC   MEDNUMMO,=F'12'                                                  
         MVC   MEDNUMPE,=F'1'                                                   
*                                                                               
         MVC   DUB,=C'SPM604  '                                                 
         CLI   RPTNUM,1                                                         
         BNE   *+10                                                             
         MVC   DUB,=C'SPM640  '                                                 
         L     R6,MEDTABLE                                                      
         GOTO1 LOADER,DMCB,DUB,(R6)                                             
         MVC   MEDTABLE,DMCB+4                                                  
         MVC   DUB,=C'SPM601  '                                                 
         CLI   RPTNUM,1                                                         
         BNE   *+10                                                             
         MVC   DUB,=C'SPM610  '                                                 
         L     R6,SPECS                                                         
         GOTO1 LOADER,DMCB,DUB,(R6)                                             
         MVC   SPECS,DMCB+4                                                     
         GOTO1 MEDSEED,DMCB,(RA)                                                
         L     RE,MEDTABLE                                                      
         LA    R4,CPPCTRL                                                       
         CLI   RPTNUM,1                                                         
         BNE   *+8                                                              
         LA    R4,CPPCTRL1                                                      
M1       SR    R5,R5                                                            
         IC    R5,0(R4)                                                         
         BCTR  R5,0                                                             
         MH    R5,=H'4'                                                         
         LA    R5,0(R5,RE)         POINT TO REPORT                              
         L     R6,0(R5)            POINT TO REPORT DEF.                         
         L     R7,4(R6)            GET COL. DEF                                 
         ST    R7,FULL                                                          
         MVC   1(3,R4),FULL+1      SAVE ORIGINAL COLUMN DEFINITIONS             
         LA    R4,4(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   M1                                                               
*                                                                               
         GOTO1 MEDCLEAR,DMCB,MEDTABLE                                           
*                                                                               
         MVC   PAGE,=H'1'                                                       
*                                                                               
         GOTO1 MEDDATE,DMCB,(RA)                                                
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         CLI   D0PROF+10,C'Y'      LPMWK=Y NEED WEEKLY DATES                    
         BE    *+8                                                              
         CLI   D0PROF+11,C'Y'      OVN NEED WEEKLY                              
         BE    *+8                                                              
         CLI   D0PROF+11,C'M'      OVN NEED WEEKLY                              
         BE    *+8                                                              
         CLI   Q2LPMWK,C'Y'        LPMWK=Y NEED WEEKLY DATES                    
         BE    *+8                                                              
         CLI   Q2OVNITE,C'Y'       OVN NEED WEEKLY                              
         BE    *+8                                                              
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
*                                                                               
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
*                                                                               
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
M21AX    DS    0H                                                               
         SPACE 2                                                                
* SET UP CROSS DAYPART CPP/M OPTIONS                                            
         L     RE,MEDTABLE                                                      
         LA    R4,CPPCTRL                                                       
         CLI   RPTNUM,1                                                         
         BNE   *+8                                                              
         LA    R4,CPPCTRL1                                                      
SETCPP   SR    R5,R5                                                            
         IC    R5,4(R4)            GET CONDITIONAL REPORT NUMBER                
         BCTR  R5,0                                                             
         MH    R5,=H'4'                                                         
         LA    R5,0(R5,RE)         POINT TO CONDITIONAL REPORT                  
         L     R6,0(R5)            POINT TO CONDITIONAL ROW DEF                 
         L     R7,4(R4)            GET NORMAL COLUMN DEF.                       
         CLI   CPPSW,0                                                          
         BE    *+8                                                              
         L     R7,0(R4)            GET CPP COL DEF.                             
         LA    R7,0(R7)            CLEAR CONTROL BYTE                           
         ST    R7,4(R6)            SET COL DEF ADDRESS                          
         LA    R4,8(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   SETCPP                                                           
*                                                                               
* OPTIMIZE BUFFALO                                                              
         CLI   SVONEPRD,0          MULTIPLE PRODUCTS                            
         BE    CHKSL                YES-SUBREPORTS REQUIRED                     
         LA    R8,SUPONEP                                                       
         BAS   R9,SUPRPTS                                                       
*                                                                               
* DAYPART SPOT LENGTH SUPPRESSION ROUTINES                                      
*                                                                               
CHKSL    CLI   QDPTDET,C'B'        SUPPRESS SPOT LENGTH                         
         BE    *+12                                                             
         CLI   QDPTDET,C'C'                                                     
         BNE   CHKDP                                                            
         LA    R8,SUPDPSL                                                       
         BAS   R9,SUPRPTS                                                       
*                                                                               
CHKDP    CLI   QDPTDET,C'C'        SUPPRESS DAYPART                             
         BNE   CHKSLTOT                                                         
         LA    R8,SUPDPT                                                        
         BAS   R9,SUPRPTS                                                       
*                                                                               
CHKSLTOT CLI   PROGPROF+4,C'Y'                                                  
         BE    M21EX                                                            
         LA    R8,SUPSLTOT                                                      
         BAS   R9,SUPRPTS                                                       
*                                                                               
M21EX    GOTO1 =V(SETBUF),DMCB,(RA),LCODE,LVCNTRL,RR=RELO                       
         MVC   HICODE,LCODE                                                     
         L     R9,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',(R9)                                      
         B     M141                                                             
*                                                                               
* SUPPRESS REPORT - A(SUPPRESSION TABLE) IN R8                                  
SUPRPTS  L     RE,MEDTABLE                                                      
         SR    RF,RF                                                            
         IC    RF,0(R8)                                                         
         BCTR  RF,0                                                             
         MH    RF,=H'4'                                                         
         LA    RF,0(RE,RF)                                                      
         OI    0(RF),X'80'         DELETE REPORT                                
         LA    R8,1(R8)                                                         
         CLI   0(R8),0                                                          
         BNE   SUPRPTS                                                          
         BR    R9                                                               
         LTORG                                                                  
         EJECT                                                                  
M3       CLI   MODE,PROCBUY                                                     
         BNE   M4                                                               
         L     RE,ADBUY                                                         
         USING BUYREC,RE                                                        
         LR    RF,RE                                                            
         BAS   R9,SETPOST                                                       
         CLI   QPRGTYPE,C' '       PROGRAM TYPE FILTER                          
         BE    *+14                                                             
         CLC   BDPROGT,QPRGTYPE                                                 
         BNE   EXIT                                                             
         DROP  RE                                                               
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
         LA    RE,PSLIST                                                        
         CLI   0(RE),0                                                          
         BE    *+12                                                             
         LA    RE,2(RE)                                                         
         B     *-12                                                             
         MVC   0(2,RE),=X'FFFF'                                                 
         CLI   KEY+3,X'FF'         POL                                          
         BNE   M31                                                              
         L     RE,ADBUY             YES - SUPPRESS PIGGYBACK LENGTHS            
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
M322     LA    R9,PSLIST                                                        
M323     CLC   0(2,R9),=X'FFFF'     END                                         
         BE    EXIT                                                             
         CLI   0(R9),0             PRODUCT DELETED                              
         BNE   *+12                                                             
         LA    R9,2(R9)                                                         
         B     M323                                                             
         L     RE,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,RE                                                      
         MVC   MEDBRAND,0(R9)                                                   
         MVC   MEDSPTLN,1(R9)                                                   
* CHECK FOR PURCH VS ACHIEVED                                                   
* CHECK FOR PURCH VS PURCH-RERATED OR AFFID                                     
         CLI   QCOMPARE,C'H'       PURCH VS PURCH-RERATED                       
         BE    M3ACH                                                            
         CLI   QCOMPARE,C'I'       PURCH VS AFFIDAVIT                           
         BE    M3ACH                                                            
M323A    GOTO1 MEDGETBY,DMCB,(RA),2     ANY ORDERED                             
         BAS   RE,SETPRMY                                                       
         L     RE,MEDBUFF                NO - BYPASS                            
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD                                                
         BZ    M325                                                             
         SPACE 2                                                                
         CLI   MEDSPILL,C'Y'                                                    
         BNE   M323NOSP                                                         
         CLI   SPOTPROF+5,0        BYPASS SPILL                                 
         BE    EXIT                                                             
         CLI   SPLPRINT,1          PUT ORIGINATING IN BUFFER                    
         BNE   M323NOSP                                                         
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'01',SPBUFMKT),0,RR=RELO                
         MVI   SPLPRINT,2                                                       
M323NOSP GOTO1 MEDGETBY,DMCB,(RA),(R3)                                          
         BAS   RE,SETPRMY                                                       
         MVC   ACTAREA,4(R1)                                                    
*                                                                               
         L     RE,MEDBUFF                                                       
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD                                                
         BZ    M325                                                             
         DROP  R4                                                               
         MVI   STACTSW,1                                                        
         MVI   ACTSW,1                                                          
         GOTO1 MEDMKTWT,DMCB,(RA)                                               
M324     MVC   WORK(2),=X'6162'                                                 
         GOTO1 MEDPOST,DMCB,(RA)                                                
         CLI   MULTISW,C'N'                                                     
         BE    M325                                                             
         MVC   WORK(2),POSTWORK                                                 
         GOTO1 (RF),DMCB,(RA)                                                   
         B     M325                                                             
*                                                                               
M325     LA    R9,2(R9)                                                         
         B     M323                                                             
         EJECT                                                                  
* GET PURCHASED AND POST IN LOCKIN BUCKETS                                      
M3ACH    GOTO1 MEDGETBY,DMCB,(RA),2                                             
         L     RE,MEDBUFF                                                       
         CLI   MEDSPILL,C'Y'                                                    
         BNE   *+12                                                             
         CLI   SPOTPROF+5,0        BYPASS SPILL                                 
         BE    EXIT                                                             
         BAS   RE,SETPRMY                                                       
*                                                                               
         L     RE,MEDBUFF                                                       
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
* SET GOAL BUCKETS                                                              
         OC    MEDBYD(12),MEDBYD                                                
         BZ    M323A                                                            
         CLI   MEDSPILL,C'Y'                                                    
         BNE   M3ACH1                                                           
         GOTO1 MEDGETBY,DMCB,(RA),(R3)                                          
         OC    MEDBYD(12),MEDBYD                                                
         BZ    M325                                                             
         GOTO1 MEDGETBY,DMCB,(RA),2                                             
M3ACH1   MVI   ACTSW,1                                                          
         MVC   MEDLKD,MEDBYD       SET LOCKIN DATA FROM BUY DATA                
         MVC   MEDLKDEQ,MEDBYDEQ                                                
         MVC   MEDLKSPT,MEDBYSPT                                                
         MVC   MEDLK1,MEDBY1                                                    
         MVC   MEDLK1EQ,MEDBY1EQ                                                
         XC    MEDBYD(42),MEDBYD   CLEAR DOLLARS, SPOTS, AND DEMOS              
*                                                                               
         DROP  R4                                                               
         DROP  RE                                                               
         GOTO1 MEDMKTWT,DMCB,(RA)                                               
         MVC   WORK(2),=X'6162'                                                 
         GOTO1 MEDPOST,DMCB,(RA)                                                
         CLI   MULTISW,C'N'                                                     
         BE    M323A                                                            
         MVC   WORK(2),POSTWORK                                                 
         GOTO1 (RF),DMCB,(RA)                                                   
         B     M323A                                                            
         EJECT                                                                  
M4       CLI   MODE,PROCGOAL                                                    
         BNE   M5                                                               
         LA    RE,KEY                                                           
         USING GOALREC,RE                                                       
         LA    RF,1(RE)                                                         
         BAS   R9,SETPOST                                                       
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         MVC   MEDBRAND,GKEYPRD                                                 
         MVC   MEDSPTLN,GKEYSLN                                                 
         DROP  RE,RF                                                            
* CHECK FOR GOAL DATA                                                           
         GOTO1 MEDGETGL,DMCB,(RA)                                               
         BAS   RE,SETPRMY                                                       
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDGLD(12),MEDGLD                                                
         BZ    M43                                                              
*                                                                               
         DROP  R4                                                               
*                                                                               
         MVI   ACTSW,1                                                          
M42      GOTO1 MEDMKTWT,DMCB,(RA)                                               
         MVC   WORK(2),=X'6162'                                                 
         GOTO1 MEDPOST,DMCB,(RA)                                                
         CLI   MULTISW,C'N'                                                     
         BE    M43                                                              
         CLI   POSTWORK,X'61'                                                   
         BE    M43                 BYPASS IF COMBINED                           
         MVC   WORK(2),POSTWORK                                                 
         GOTO1 (RF),DMCB,(RA)                                                   
* CHECK IF LOCKIN REQUIRED                                                      
M43      CLI   QCOMPARE,C'J'                                                    
         BE    M4L                                                              
         CLI   QCOMPARE,C'K'                                                    
         BE    M4L                                                              
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* EXTRACT LOCKIN DATA                                                           
*                                                                               
M4L      L     RE,MEDBUFF                                                       
         MVI   MODE,PROCBUY        FUDGE FOR SPILL                              
         MVI   MEDSPILL,C'O'       GET ORIGINATING ONLY                         
         MVC   SAVNUMWK,MEDNUMWK   GETLK DOESN'T LIKE WEEKS HERE                
         XC    MEDNUMWK,MEDNUMWK   KILL AND RESTORE                             
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
         OC    MEDLKD(12),MEDLKD   NO ORIGINATING                               
         BZ    M4LSP               TRY FOR SPILL                                
*                                                                               
         DROP  R4                                                               
*                                                                               
         MVI   ACTSW,1                                                          
*                                                                               
         GOTO1 MEDMKTWT,DMCB,(RA)                                               
         MVC   WORK(2),=X'6162'                                                 
         GOTO1 MEDPOST,DMCB,(RA)                                                
         CLI   MULTISW,C'N'                                                     
         BE    M4LSP                                                            
         MVC   WORK(2),POSTWORK                                                 
         GOTO1 (RF),DMCB,(RA)                                                   
M4LSP    DS    0H                                                               
         CLI   SPOTPROF+5,0        EXIT IF NO SPILL REQUESTED                   
         BE    M4LX                                                             
         L     RE,MEDBUFF                                                       
         MVI   MEDSPILL,C'S'       GET SPILL ONLY                               
         MVC   SAVNUMWK,MEDNUMWK   GETLK DOESN'T LIKE WEEKS HERE                
         XC    MEDNUMWK,MEDNUMWK   KILL AND RESTORE                             
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
         OC    MEDLKD(12),MEDLKD                                                
         BZ    M4LX                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
         MVI   ACTSW,1                                                          
         MVI   MEDSPILL,C'Y'                                                    
*                                                                               
         GOTO1 MEDMKTWT,DMCB,(RA)                                               
         MVC   WORK(2),=X'6162'                                                 
         GOTO1 MEDPOST,DMCB,(RA)                                                
         CLI   MULTISW,C'N'                                                     
         BE    M4LX                                                             
         MVC   WORK(2),POSTWORK                                                 
         GOTO1 (RF),DMCB,(RA)                                                   
*                                                                               
M4LX     MVI   MODE,PROCGOAL       RESET MODE                                   
         B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
M5       CLI   MODE,MKTLAST                                                     
         BNE   M6                                                               
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVI   RCSUBPRG,1                                                       
         CLI   RTGSW,1                                                          
         BE    *+8                                                              
         MVI   RCSUBPRG,2                                                       
         CLI   QOPT1,C'D'          SUMMARIES ONLY                               
         BNE   *+8                                                              
         MVI   FRSTTOT,C'Y'        FORCE FIRST SUMMARY                          
         CLI   ACTSW,1                                                          
         BNE   EXIT                                                             
         MVC   WEIGHT,SPWEIGHT                                                  
         LA    RE,MYBUFIO                                                       
         LA    RF,400                                                           
         XCEF                                                                   
         CLI   QOPT1,C'D'                                                       
         BE    M53                                                              
         XC    MID1,MID1                                                        
         MVC   MID1(4),MKT                                                      
         MVI   MID1+5,C'-'                                                      
         MVC   MID1+7(24),MKTNM                                                 
         CLI   SPOTPROF+1,C'N'                                                  
         BE    M5AA                                                             
         MVC   MID1+32(9),=C'COVERAGE='                                         
         EDIT  WEIGHT,(5,MID1+41),2,ALIGN=LEFT                                  
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
         CLC   0(1,R4),BUFCDE                                                   
         BNE   M52C                                                             
         CLI   MULTISW,C'Y'                                                     
         BNE   M5A2                                                             
         MVI   FORCEMID,C'Y'                                                    
         LA    RE,MID1             SET MEDIA CAPTION                            
         CLI   MID1,C' '                                                        
         BE    *+8                                                              
         LA    RE,132(RE)                                                       
         MVC   0(132,RE),SPACES                                                 
         MVC   0(11,RE),=C'COMBINED TV'                                         
         CLI   0(R4),X'60'                                                      
         BH    M5A2                                                             
         MVC   0(11,RE),=C'NETWORK TV '                                         
         CLI   0(R4),X'40'                                                      
         BH    M5A2                                                             
         MVC   0(11,RE),=C'SPOT TV    '                                         
M5A2     L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         MVI   MEDSLCNT,0                                                       
         MVI   MEDDPCNT,0                                                       
         B     M52                                                              
M51      GOTO1 BUFFALO,DMCB,=C'SEQ',(R3),(R4),1                                 
M52      MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         CLC   0(1,R4),BUFCDE                                                   
         BNE   M52C                                                             
         TM    DMCB+8,X'80'                                                     
         BO    M52C                                                             
         OC    MYBUFIO+12(250),MYBUFIO+12                                       
         BZ    M51                                                              
         SPACE 2                                                                
         CLI   PROGPROF+6,C'Y'     SUPPRESS CROSS DAYPART DEMOS                 
         BNE   M52EDT                                                           
         CLI   MYBUFIO+2,X'FF'     IS IT A TOTAL LINE                           
         BNE   M52EDT                                                           
         USING BUFFRECD,R4                                                      
         XC    BFDGL(8),BFDGL      CLEAR OUT THE POINTS                         
         XC    BFDLK(8),BFDLK                                                   
         XC    BFDBY1,BFDBY1                                                    
         XC    BFDBY2,BFDBY2                                                    
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
         CLI   RPTNUM,1                                                         
         BE    M52R01                                                           
         MVC   P+71(7),P2                                                       
         MVC   P+79(4),P2+24                                                    
         MVC   P+83(9),P2+7                                                     
         MVC   P+93(4),P2+28                                                    
         MVC   P+97(8),P2+16                                                    
         B     M52REX                                                           
         SPACE 2                                                                
M52R01   MVC   P1+35(7),P2         POINTS                                       
         MVC   P1+47(9),P2+7       DOLLARS                                      
         MVC   P1+61(8),P2+16      CPP/M                                        
         MVC   P1+73(7),P2+24      POINTS                                       
         MVC   P1+90(9),P2+31      DOLLARS                                      
         MVC   P1+109(8),P2+40     CPP/M                                        
         SPACE 2                                                                
M52REX   DS    0H                                                               
         MVC   P2,SPACES                                                        
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
M52C     CLI   BUFCDE,X'60'        CHECK FOR END                                
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
         L     R3,ACTAREA                                                       
         GOTO1 =V(VMDAPRNT),DMCB,(RA),(R3),1,RR=RELO                            
         OC    SPBUFMKT(20),SPBUFMKT                                            
         BZ    M53                                                              
         MVC   P1(11),=C'***SPILL***'                                           
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'02',SPBUFMKT),P1,RR=RELO               
         GOTO1 REPORT                                                           
*                                                                               
M53      MVC   DMCB+8(20),LVCNTRL                                               
         LA    RE,SPBUFMKT                                                      
         L     RF,=F'600'                                                       
         XCEF                                                                   
*        XC    SPBUFMKT,SPBUFMKT                                                
         L     R3,BUFFBUFF                                                      
         TM    LVCNTRL,X'80'                                                    
         BO    M53A                                                             
         GOTO1 BUFFALO,DMCB,=C'ADD',(X'21',(R3))                                
         GOTO1 BUFFALO,DMCB,=C'ADD',(X'41',(R3))                                
         GOTO1 BUFFALO,DMCB,=C'ADD',(X'61',(R3))                                
         CLI   BPRD,X'FF'                                                       
         BE    M53A                                                             
         GOTO1 BUFFALO,DMCB,=C'ADD',(X'22',(R3))                                
         GOTO1 BUFFALO,DMCB,=C'ADD',(X'42',(R3))                                
         GOTO1 BUFFALO,DMCB,=C'ADD',(X'62',(R3))                                
M53A     GOTO1 BUFFALO,DMCB,=C'CLEAR',(R3),(X'80',1)                            
         MVI   FORCEMID,C'N'                                                    
         MVI   MID1,C' '                                                        
         MVC   MID1+1(132),MID1                                                 
         B     EXIT                                                             
         SPACE 2                                                                
M6       CLI   MODE,MGR3LAST                                                    
         BNE   M7                                                               
         MVI   BUFCDE,X'21'                                                     
         MVI   LCODE,5                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
         SPACE 2                                                                
M7       CLI   MODE,MGR2LAST                                                    
         BNE   M8                                                               
         MVI   BUFCDE,X'21'                                                     
         MVI   LCODE,4                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
         SPACE 2                                                                
M8       CLI   MODE,MGR1LAST                                                    
         BNE   M9                                                               
         MVI   BUFCDE,X'21'                                                     
         MVI   LCODE,3                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
         SPACE 2                                                                
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
         B     EXIT                                                             
         SPACE 2                                                                
M10      CLI   MODE,PGR3LAST                                                    
         BNE   M11                                                              
         MVI   BUFCDE,X'22'                                                     
         MVI   LCODE,5                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
         SPACE 2                                                                
M11      CLI   MODE,PGR2LAST                                                    
         BNE   M12                                                              
         MVI   BUFCDE,X'22'                                                     
         MVI   LCODE,4                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
         SPACE 2                                                                
M12      CLI   MODE,PGR1LAST                                                    
         BNE   M13                                                              
         MVI   BUFCDE,X'22'                                                     
         MVI   LCODE,3                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
         SPACE 2                                                                
M13      CLI   MODE,CLTLAST                                                     
         BNE   M14                                                              
         TM    QMKT,X'F0'                                                       
         BO    EXIT                                                             
         MVI   BUFCDE,X'22'                                                     
         MVI   LCODE,2                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
         SPACE 2                                                                
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
         LA    R9,2                                                             
         SPACE 2                                                                
         CLC   PRDBUFLN,=H'56'     OLD FORMAT                                   
         BE    M14AOLD                                                          
         ST    RE,FULL                                                          
         BAS   RE,NEWDNAM                                                       
         B     M14B                                                             
         SPACE 2                                                                
M14AOLD  L     R8,DEMTABLE                                                      
         SH    R8,=H'7'                                                         
         LA    R5,DNAME1                                                        
M14A     SR    R6,R6                                                            
         IC    R6,0(RE)                                                         
         LTR   R6,R6                                                            
         BZ    M14B                                                             
         MH    R6,=H'7'                                                         
         LA    R7,0(R6,R8)                                                      
         MVC   0(7,R5),0(R7)                                                    
         LA    R5,7(R5)                                                         
         LA    RE,1(RE)                                                         
         BCT   R9,M14A                                                          
M14B     MVI   RTGSW,0                                                          
         CLI   DNAME1,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAME1,C'R'                                                      
         BNE   *+8                                                              
         MVI   RTGSW,1                                                          
*                                                                               
         SPACE 2                                                                
M15      CLI   MODE,MKTFRST                                                     
         BNE   M16                                                              
         LA    RE,SPBUFMKT                                                      
         L     RF,=F'600'                                                       
         XCEF                                                                   
*        XC    SPBUFMKT,SPBUFMKT                                                
         MVI   ACTSW,0                                                          
         B     EXIT                                                             
         SPACE 2                                                                
M16      CLI   MODE,REQLAST                                                     
         BNE   M17                                                              
         MVI   FORCEHED,C'N'                                                    
         MVI   P,0                                                              
         MVC   P2(132),FOOT1                                                    
         GOTO1 REPORT                                                           
         MVC   FOOT1(132),SPACES                                                
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
*                                                                               
M17      CLI   MODE,STAFRST                                                     
         BNE   M18                                                              
         MVI   SPLPRINT,1                                                       
         MVI   STACTSW,0                                                        
         B     EXIT                                                             
*                                                                               
M18      CLI   MODE,STALAST                                                     
         BNE   M19                                                              
         CLI   STACTSW,1                                                        
         BNE   EXIT                                                             
         CLI   QOPT1,C'D'          SUPPRESS DETAILS                             
         BE    EXIT                                                             
         L     R8,ACTAREA                                                       
         GOTO1 =V(VMDAPRNT),DMCB,(RA),(R8),0,RR=RELO                            
         B     EXIT                                                             
*                                                                               
M19      B     EXIT                                                             
         EJECT                                                                  
DOSUM    L     R4,BUFFIO           DO SUMMARY REPORTS                           
         MVI   FORCEHED,C'Y'                                                    
         MVI   FRSTTOT,C'N'                                                     
         MVI   WTSW,1                                                           
         CLC   HICODE,LCODE                                                     
         BLR   R9                                                               
         ST    R9,SAVE9                                                         
         L     R3,BUFFBUFF                                                      
         CLI   SPDUPTOT,C'Y'                                                    
         BE    DOSUM3                                                           
         MVI   SW1,0                                                            
         MVI   FORCEHED,C'Y'                                                    
REDOSUM  LA    RE,MYBUFIO                                                       
         LA    RF,400                                                           
         XCEF                                                                   
         MVC   0(1,R4),BUFCDE                                                   
         SR    R5,R5                                                            
         IC    R5,LCODE                                                         
         BCTR  R5,0                                                             
         SLL   R5,2                                                             
         SR    R8,R8                                                            
         IC    R8,LCODE                                                         
         GOTO1 BUFFALO,DMCB,=C'HIGH',(R3),(R4),(R8)                             
         B     DOSUM2                                                           
*                                                                               
DOSUM1   SR    R8,R8                                                            
         IC    R8,LCODE                                                         
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         GOTO1 BUFFALO,DMCB,=C'SEQ',(R3),(R4),(R8)                              
DOSUM2   TM    DMCB+8,X'80'                                                     
         BO    DOSUM3                                                           
         CLC   0(1,R4),BUFCDE                                                   
         BNE   DOSUM3                                                           
         CLI   MULTISW,C'Y'                                                     
         BNE   DOSM2                                                            
         MVI   FORCEMID,C'Y'                                                    
         LA    RE,MID1             SET MEDIA CAPTION                            
         CLI   MID1,C' '                                                        
         BE    *+8                                                              
         LA    RE,132(RE)                                                       
         MVC   0(132,RE),SPACES                                                 
         MVC   0(11,RE),=C'COMBINED TV'                                         
         CLI   0(R4),X'60'                                                      
         BH    DOSM2                                                            
         MVC   0(11,RE),=C'NETWORK TV '                                         
         CLI   0(R4),X'40'                                                      
         BH    DOSM2                                                            
         MVC   0(11,RE),=C'SPOT TV    '                                         
DOSM2    CLI   1(R4),0                                                          
         BE    DOSUM21                                                          
         TM    1(R4),X'80'         CLIENT SUMMARY                               
         BO    DOSUM2AI                                                         
         L     RE,PRDBUFF                                                       
         LA    RF,220                                                           
         SPACE 2                                                                
         CLC   PRDBUFLN,=H'56'                                                  
         BE    DOSUM2AB            OLD FORMAT DEMOS                             
         B     DOSUM2AC            NEW FORMAT DEMOS                             
         SPACE 2                                                                
DOSUM2AB CLC   28(1,RE),1(R4)      GET PRIMARY DEMO OLD FORMAT                  
         BE    DOSUM2AD                                                         
         AH    RE,PRDBUFLN                                                      
         BCT   RF,DOSUM2AB                                                      
         DC    H'0'                                                             
         SPACE 2                                                                
DOSUM2AC ZIC   R6,1(R4)            GET PRIMARY DEMO BRAND NEW FORMAT            
         BCTR  R6,0                                                             
         MH    R6,=H'3'                                                         
         LA    R6,PRMYTAB(R6)                                                   
DOSM2AC1 CLC   28(3,RE),0(R6)                                                   
         BE    DOSUM2AD                                                         
         AH    RE,PRDBUFLN                                                      
         BCT   RF,DOSM2AC1                                                      
         DC    H'0'                                                             
         SPACE 2                                                                
DOSUM2AD L     RF,MEDBUFF                                                       
         MVC   MEDBRAND,0(RE)      SET PRIMARY DEMO BRAND                       
         B     DOSUM2A                                                          
DOSUM2AI MVI   RCSUBPRG,7                                                       
         B     DOSUM21                                                          
DOSUM2A  XC    DNAME1(7),DNAME1                                                 
         CLC   PRDBUFLN,=H'56'                                                  
         BNE   DSM2AND                                                          
         SR    R6,R6                                                            
         IC    R6,1(R4)                                                         
         MH    R6,=H'7'                                                         
         A     R6,DEMTABLE                                                      
         SH    R6,=H'7'                                                         
         MVC   DNAME1,0(R6)                                                     
         B     DOSM2AX                                                          
         SPACE 2                                                                
DSM2AND  LA    R9,1                GET NEW FORMAT DEMO NAME                     
         ZIC   RF,1(R4)            GET PRIMARY DEMO SLOT                        
         BCTR  RF,0                                                             
         MH    RF,=H'3'                                                         
         LA    RF,PRMYTAB(RF)                                                   
         ST    RF,FULL                                                          
         BAS   RE,NEWDNAM                                                       
         SPACE 2                                                                
DOSM2AX  MVI   RTGSW,0                                                          
         CLI   DNAME1,C'E'                                                      
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
         CLI   WTSW,0                                                           
         SPACE 2                                                                
         CLI   PROGPROF+6,C'Y'     SUPPRESS CROSS DAYPART DEMOS                 
         BNE   DOSUM22                                                          
         CLI   MYBUFIO+2,X'FF'     IS IT A TOTAL LINE                           
         BNE   DOSUM22                                                          
         USING BUFFRECD,R4                                                      
         XC    BFDGL(8),BFDGL      CLEAR OUT THE POINTS                         
         XC    BFDLK(8),BFDLK                                                   
         XC    BFDBY1,BFDBY1                                                    
         XC    BFDBY2,BFDBY2                                                    
         DROP  R4                                                               
         SPACE 2                                                                
DOSUM22  GOTO1 MEDEDIT,DMCB,(RA)                                                
         CLI   DMCB,0                                                           
         BE    DOSUM1                                                           
         CLI   MYBUFIO+2,X'FD'                                                  
         BE    *+8                                                              
         CLI   MYBUFIO+2,X'FE'                                                  
         BNE   *+8                                                              
         MVI   DMCB,1                                                           
         MVC   SPACING,DMCB                                                     
         CLI   RPTNUM,1                                                         
         BE    DS2201                                                           
         MVC   P+71(7),P2          REFORMAT PRINT LINE                          
         MVC   P+79(4),P2+24                                                    
         MVC   P+83(9),P2+7                                                     
         MVC   P+93(4),P2+28                                                    
         MVC   P+97(8),P2+16                                                    
         B     DS22EX                                                           
         SPACE 2                                                                
DS2201   MVC   P1+35(7),P2         POINTS                                       
         MVC   P1+47(9),P2+7       DOLLARS                                      
         MVC   P1+61(8),P2+16      CPP/M                                        
         MVC   P1+73(7),P2+24      POINTS                                       
         MVC   P1+90(9),P2+31      DOLLARS                                      
         MVC   P1+109(8),P2+40     CPP/M                                        
         SPACE 2                                                                
DS22EX   DS    0H                                                               
         MVC   P2,SPACES                                                        
         TM    P1+9,X'F0'                                                       
         BO    DOSUM21A                                                         
         GOTO1 MEDSTARS,DMCB,P1                                                 
DOSUM21A DS    0H                                                               
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
SETPOST  MVC   POSTWORK,=X'2122'   SETUP FOR SECONDARY POST                     
         MVC   HALF,0(RF)                                                       
         NI    HALF,X'0F'                                                       
         CLI   HALF,8              CHECK FOR COMBINED                           
         BNE   *+12                                                             
         MVC   POSTWORK,=X'6162'                                                
         BR    R9                                                               
         CLI   HALF,1              IS IT SPOT                                   
         BE    *+10                                                             
         MVC   POSTWORK,=X'4142'   SET FOR NETWORK                              
         BR    R9                                                               
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
***      DROP  RE                                                               
         EJECT                                                                  
NEWDNAM  NTR1                      GET NEW FORMAT DEMO NAMES                    
         L     R6,ADBLOCK          R9 HAS NUMBER OF DEMO NAMES                  
         USING DBLOCK,R6                                                        
         XC    0(256,R6),0(R6)                                                  
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
         MVI   DBSELMED,C'C'                                                    
*                                                                               
NEWDCDX  DS    0C                                                               
         DROP  R6                                                               
         L     R6,FULL                                                          
         L     RF,ADEST            SET FOR USER NAMES                           
         USING ESTHDR,RF                                                        
         LA    R0,ENONTDMS                                                      
         ST    R0,DMCB+16                                                       
         LA    RF,EUSRNMS                                                       
         DROP  RF                                                               
         GOTO1 DEMOCON,DMCB,((R9),(R6)),(2,DNAME1),(C'S',ADBLOCK),     X        
               (SPOTPROF+9,(RF))                                                
         XIT1                                                                   
         EJECT                                                                  
*SET PRIMARY DEMO IN MEDBLOCK                                                   
SETPRMY  NTR1                                                                   
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         ZIC   RE,MEDBRAND         GET PRODUCT SLOT                             
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF                                                       
         MVC   MEDPRIMY,28(RE)     EXTRACT OLD PRIMARY DEMO                     
         CLC   PRDBUFLN,=H'56'     OLD FORMAT - EXIT                            
         BE    SETPRMYX                                                         
         MVC   FULL(3),28(RE)      EXTRACT NEW PRIMARY DEMO                     
         LA    R9,PRMYTAB                                                       
         LA    R1,1                                                             
SETPRMY2 CLC   FULL(3),0(R9)       SET NEW PRIMARY DEMO IN TABLE                
         BE    SETPRMY4                                                         
         CLI   1(R9),0                                                          
         BE    SETPRMY3                                                         
         LA    R9,3(R9)                                                         
         LA    R1,1(R1)                                                         
         B     SETPRMY2                                                         
SETPRMY3 MVC   0(3,R9),FULL                                                     
SETPRMY4 STC   R1,MEDPRIMY         EQUATE NEW PRIMARY DEMO IN SLOT              
SETPRMYX XIT1                                                                   
         EJECT                                                                  
         DROP  R2                                                               
         USING *,RF                                                             
MYHEAD   NTR1  BASE=SPM6RB                                                      
         LM    RA,RC,SPM6RA                                                     
         L     R2,SPM6R2                                                        
         USING SPM602+4096,R2                                                   
         DROP  RF                                                               
         GOTO1 =V(HEADC),DMCB,(RA),HCAP1,RR=RELO                                
         XIT1                                                                   
         EJECT                                                                  
         DROP  R2                                                               
         DS    0D                                                               
         USING *,RF                                                             
BFHOOK   NTR1  BASE=SPM6RB                                                      
         LM    RA,RC,SPM6RA                                                     
         L     R2,SPM6R2                                                        
         USING SPM602+4096,R2                                                   
         DROP  RF                                                               
         L     R3,0(R1)                                                         
         CLI   2(R3),X'FF'         TOTAL LINE                                   
         BE    BFHOOKA                                                          
         CLI   MODE,PROCGOAL       ALWAYS WANT GOAL DETAILS                     
         BE    BFHEXIT                                                          
         L     R9,MEDBUFF                                                       
         USING MEDBLOCK,R9                                                      
         CLI   MEDSPILL,C'Y'                                                    
         BNE   BFHOOKA                                                          
         CLI   SPOTPROF+5,0        SUPPRESS SPILL                               
         BE    *+8                                                              
         CLI   SPOTPROF+5,1        SUPPRESS DETAIL SPILL                        
         BNE   BFHOOKA                                                          
         XC    13(64,R3),13(R3)                                                 
         B     BFHEXIT                                                          
         SPACE 2                                                                
BFHOOKA  DS    0H                                                               
         CLI   2(R3),X'FF'                                                      
         BNE   BFHEXIT                                                          
         CLI   11(R3),X'FF'        ORIG+SPILL                                   
         BE    *+8                                                              
         MVI   2(R3),X'FE'                                                      
         CLI   11(R3),X'FE'                                                     
         BNE   *+8                                                              
         MVI   2(R3),X'FD'                                                      
BFHOOK1  L     R9,MEDBUFF                                                       
         CLI   2(R3),X'FF'         OVERALL TOTALS                               
         BE    BFHEXIT                                                          
         CLI   MODE,PROCGOAL                                                    
         BNE   BFHOOK1A                                                         
         CLI   2(R3),X'FD'         IS THIS A SPILL TOTAL                        
         BNE   BFHOOK1A                                                         
         XC    13(64,R3),13(R3)    CLEAR IT                                     
         B     BFHEXIT                                                          
BFHOOK1A CLI   SPOTPROF+5,1        DO WE WANT S/O TOTALS                        
         BE    *+8                                                              
         CLI   SPOTPROF+5,3                                                     
         BE    *+14                                                             
         XC    13(64,R3),13(R3)    RESET TOTALS                                 
         B     BFHEXIT                                                          
         SPACE 2                                                                
         CLI   2(R3),X'FD'         SPILL                                        
         BE    BFHOOK2                                                          
         CLI   MODE,PROCGOAL       ORIG GOALS ARE OK                            
         BE    EXIT                                                             
         CLI   MEDSPILL,C'Y'                                                    
         BNE   BFHEXIT                                                          
         XC    13(64,R3),13(R3)                                                 
         B     BFHEXIT                                                          
         SPACE 2                                                                
BFHOOK2  CLI   MEDSPILL,C'Y'                                                    
         BE    BFHEXIT                                                          
         XC    13(64,R3),13(R3)                                                 
         B     BFHEXIT                                                          
         SPACE 2                                                                
BFHEXIT  XIT1                                                                   
         DROP  R9                                                               
         EJECT                                                                  
SPM6RA   DC    F'0'                                                             
SPM6RB   DC    F'0'                                                             
SPM6RC   DC    F'0'                                                             
SPM6R2   DC    F'0'                                                             
RELO     DC    F'0'                                                             
         LTORG                                                                  
HCAP1    DS    CL6                                                              
HCAP2    DS    CL9                                                              
HCAP3    DS    CL21                                                             
LVCNTRL  DC    F'1'                                                             
         DC    A(2,3,4,5)                                                       
CPPCTRL1 DS    0C                                                               
CPPCTRL  DC    X'01',AL3(0),X'04',AL3(0)                                        
         DC    X'05',AL3(0),X'08',AL3(0)                                        
         DC    X'01',AL3(0),X'0D',AL3(0)                                        
         DC    X'05',AL3(0),X'0E',AL3(0)                                        
         DC    X'01',AL3(0),X'11',AL3(0)                                        
         DC    X'00'                                                            
CPPSW    DC    X'00'               CROSS DAYPART CPP SW                         
SUPONEP  DC    AL1(5,6,7,8,9,10,11,12,14,16,18,19,21,23,0)                      
SUPDPSL  DC    AL1(1,5,9,0)                                                     
SUPDPT   DC    AL1(2,3,6,7,10,11,0)                                             
SUPSLTOT DC    AL1(17,18,19,20,21,22,23,0)                                      
         EJECT                                                                  
SAVNUMWK DS    XL4                 SAVE NUMBER OF WEEKS                         
PRMYTAB  DS    CL90                                                             
PSLIST   DS    CL100               PRODUCT SPOT LENGTH LIST                     
MULTISW  DS    C                                                                
POSTWORK DS    CL2                                                              
PRTSW    DS    C                                                                
CURRLN   DS    C                                                                
SW1      DS    C                   DATA SWITCH                                  
FRSTTOT  DS    C                                                                
RPTNUM   DC    X'00'                                                            
WEIGHT   DC    F'1'                                                             
         DC    F'1'                                                             
ACTAREA  DS    F                                                                
SAVE9    DS    F                                                                
WTSW     DS    0C                                                               
RTGSW    DS    C                   RATING SWITCH                                
BUFCDE   DS    C                   BUFFALO CODE                                 
LCODE    DS    C                   LEVEL CODE                                   
PBUFFCDE DS    C                                                                
HICODE   DS    C                                                                
ACTSW    DS    C                                                                
SVMAXLIN DS    C                                                                
STACTSW  DS    C                                                                
DNAME1   DS    CL7                                                              
DNAME2   DS    CL7                                                              
DNAME3   DS    CL7                                                              
DNAME4   DS    CL7                                                              
SPLPRINT DS    C                                                                
D0PROF   DS    CL16                                                             
SPBUFMKT DS    CL600                                                            
MYBUFIO  DS    CL400                                                            
         SPACE 2                                                                
BUFFRECD DSECT                                                                  
BFDKEY   DS    CL13                                                             
BFDDATA  DS    0CL64                                                            
BFDGL    DS    CL16                                                             
BFDLK    DS    CL16                                                             
BFDLKSP  DS    CL4                                                              
BFDBY1   DS    CL8                                                              
BFDBYD   DS    CL8                                                              
BFDBYSP  DS    CL4                                                              
BFDBY2   DS    CL8                                                              
         EJECT                                                                  
         BUFF  LINES=200,ROWS=5,COLUMNS=16,FLAVOR=BINARY,KEYLIST=(13,A)         
         EJECT                                                                  
RQFRSTC  CSECT                                                                  
         NMOD1 0,RQFRST                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R3,4(R1)                                                         
         USING HCAP1,R3                                                         
         CLI   QCOMPARE,C' '                                                    
         BNE   *+10                                                             
         MVC   QCOMPARE,PROGPROF   SET DATA COMPARE                             
         CLI   QDPTDET,C' '                                                     
         BNE   *+10                                                             
         MVC   QDPTDET,PROGPROF+1  SET DAYPART CONTROL                          
         MVI   CPPSW,0                                                          
         CLI   PROGPROF+2,C'Y'                                                  
         BNE   *+8                                                              
         MVI   CPPSW,1             SET CROSS DAYPART CPP-M                      
         MVC   RPTNUM,PROGPROF+3   SET UP REPORT NUMBER                         
         XC    HCAP1,HCAP1                                                      
         XC    HCAP2,HCAP2                                                      
         XC    HCAP3,HCAP3                                                      
         MVC   HCAP1,=C'GOAL('                                                  
         MVC   HCAP2,=C'PURCHASED'                                              
         CLI   QCOMPARE,C'J'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'K'                                                    
         BNE   *+10                                                             
         MVC   HCAP2,=C' ORDERED '                                              
         MVC   HCAP3,=C'PURCHASED ACHIEVEMENT'                                  
         CLI   QCOMPARE,C'I'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'K'                                                    
         BNE   *+10                                                             
         MVC   HCAP3,=C'INVOICED  ACHIEVEMENT'                                  
         MVI   ESTSW,C'N'          NEW REQUEST                                  
         MVC   RQDPOVRD,QDPTMENU   OVERRIDE DAYPART MENU                        
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
HEADC    CSECT                                                                  
         NMOD1 0,M6HEADC                                                        
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R3,4(R1)                                                         
         USING HCAP1,R3                                                         
         CLI   SPOTPROF+12,C'Y'                                                 
         BNE   *+10                                                             
         MVC   H8+50(18),=C'***TAX EXCLUDED***'                                 
         CLI   QFILTER,C'F'        FILM NUMBER FOR COKE                         
         BNE   MYHPGRP                                                          
         MVC   H8(8),=C'**FILM**'                                               
         MVC   H8+9(1),QFILTER+1                                                
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
         CLI   RPTNUM,1                                                         
         BE    MYHR1                                                            
         CLI   RCSUBPRG,5          PRIMARY DEMO REPORT                          
         BE    *+8                                                              
         CLI   RCSUBPRG,6                                                       
         BNE   MYHEAD1                                                          
         CLI   RCSUBPRG,4                                                       
         BH    *+10                                                             
         MVC   H11+17(6),HCAP1                                                  
         MVC   H11+47(9),HCAP2                                                  
         MVC   H11+80(21),HCAP3                                                 
         B     MYHEAD3                                                          
MYHEAD1  CLI   RCSUBPRG,5          CLIENT SUMMARY                               
         BL    MYHEAD2                                                          
         MVC   H11+53(9),HCAP2                                                  
         MVC   H11+90(21),HCAP3                                                 
         MVC   H11+85(5),=C'-----'                                              
         MVC   H11+111(5),=C'-----'                                             
         B     MYHEAD3                                                          
MYHEAD2  MVC   H11+17(6),HCAP1     DETAIL LINES                                 
         MVC   H11+47(9),HCAP2                                                  
         MVC   H11+80(21),HCAP3                                                 
MYHEAD3  DS    0H                                                               
         CLI   MODE,PRDLAST                                                     
         BH    MYHEADX                                                          
         MVC   H11+22(7),DNAME1                                                 
         CLI   DNAME2,0                                                         
         BE    MYHEADX                                                          
         MVC   H12+117(7),DNAME2                                                
         MVC   H12+127(3),=C'CPP'                                               
         CLI   DNAME2,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAME2,C'R'                                                      
         BE    *+10                                                             
         MVC   H12+127(3),=C'CPM'                                               
*                                                                               
         CLI   QOPT6,C'Y'          TEST REPORT LOCKED GOALS                     
         BNE   LKGLX                                                            
         LA    R1,H11+17                                                        
         LHI   R0,10                                                            
*                                                                               
LKGL2    CLC   =C'GOAL',0(R1)                                                   
         BE    LKGL4                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,LKGL2                                                         
         B     LKGLX                                                            
*                                                                               
LKGL4    MVC   0(4,R1),=C'LKGL'                                                 
*                                                                               
LKGLX    DS    0H                                                               
MYHEADX  XIT1                                                                   
         SPACE 2                                                                
* HEADLINES FOR REPORT NUMBER 1                                                 
MYHR1    CLI   RCSUBPRG,5                                                       
         BE    *+8                                                              
         CLI   RCSUBPRG,6                                                       
         BNE   MYHR11                                                           
         CLI   RCSUBPRG,4                                                       
         BH    *+10                                                             
         MVC   H11+17(6),HCAP1                                                  
         MVC   H10+50(9),HCAP2                                                  
         MVC   H10+89(21),HCAP3                                                 
         B     MYHEAD3                                                          
MYHR11   CLI   RCSUBPRG,5          CLIENT SUMMARY                               
         BL    MHR12                                                            
         MVC   H10+50(9),HCAP2                                                  
         MVC   H10+89(21),HCAP3                                                 
         B     MYHEAD3                                                          
MHR12    MVC   H11+17(6),HCAP1     DETAIL LINES                                 
         MVC   H10+50(9),HCAP2                                                  
         MVC   H10+89(21),HCAP3                                                 
         B     MYHEAD3                                                          
         LTORG                                                                  
         EJECT                                                                  
SETBUF   CSECT                                                                  
         NMOD1 0,SETBUF                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R4,4(R1)            LOCDE                                        
         L     R5,8(R1)            LVCNTRL                                      
         MVI   0(R4),2             DETERMINE NUMBER OF LEVELS REQUIRED          
         CLI   QOPT1,C'S'                                                       
         BE    M2A                                                              
         TM    QMKT,X'F0'                                                       
         BNO   *+8                                                              
         MVI   0(R4),1                                                          
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
         L     R3,BUFFBUFF                                                      
         USING BUFFALOD,R3                                                      
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),0(R4)                                                  
         MVC   BUFFROWS+2(2),HALF                                               
         L     R9,BUFFLALL         GET MAXIMUM CORE AVAILABLE                   
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
         EJECT                                                                  
         PRINT GEN                                                              
         PRINT ON                                                               
* SPREPWORKD                                                                    
       ++INCLUDE SPREPWORKD                                                     
         ORG   QGRP                                                             
QOPT6    DS    C                                                                
         ORG                                                                    
       ++INCLUDE SPREPPTBUF                                                     
         EJECT                                                                  
* MEDPRTOPT                        NO LONGER INCLUDED                           
*      ++INCLUDE MEDRPTOPT                                                      
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
* SPGENMKT                                                                      
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
* DEDBLOCK                                                                      
       ++INCLUDE DEDBLOCK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063SPREPM602 11/21/19'                                      
         END                                                                    
