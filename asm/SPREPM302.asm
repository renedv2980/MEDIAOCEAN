*          DATA SET SPREPM302  AT LEVEL 147 AS OF 11/21/19                      
*PHASE SPM302T                                                                  
*INCLUDE COVAIL                                                                 
*INCLUDE MEDMOVER                                                               
*INCLUDE MEDAPRNT                                                               
*INCLUDE SPRPFOOT                                                               
*INCLUDE REPSPILL                                                               
         TITLE 'SPREPM302-BRAND WEEKLY SUMMARY'                                 
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
* NOV03/98  CHANGE CLT SUMMARY TITLES FOR NET DOLLARS           MHER  *         
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
SPM302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPM302                                                         
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING SPM302+4096,R2                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         STM   RA,RC,SPM3RA                                                     
         ST    R2,SPM3R2                                                        
         OI    RQOPTS,RQOPTS_POST    FLAG TO INDICATE SPOT POSTING              
         GOTO1 =V(SPRPFOOT),DMCB,(RA)                                           
*                                                                               
         CLI   MODE,MKTLAST                                                     
         BL    BYPW                                                             
         GOTO1 MEDADDWT,DMCB,(RA)                                               
         CLI   QOPT2,C'D'                                                       
         BNE   BYPW                                                             
         CLI   FRSTTOT,C'Y'                                                     
         BNE   BYPW                                                             
         MVI   SPDUPTOT,C'N'                                                    
         MVI   FRSTTOT,C'N'                                                     
BYPW     DS    0H                                                               
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   M2                                                               
*                                                                               
         MVC   SVMAXLIN,MAXLINES                                                
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDNUMWK,=F'56'                                                  
         MVC   MEDNUMMO,=F'12'                                                  
         MVI   BUFFACT,0                                                        
         MVI   RQEQUIV,C'Y'                                                     
         MVI   RQDAYPT,C'Y'                                                     
         CLI   MEDEXTAV,C'Y'                                                    
         BE    *+8                                                              
         MVI   MEDEXTDM,4                                                       
         L     RE,=A(FLTRTAB)                                                   
         ST    RE,AFLTRTAB                                                      
         LA    RE,MYBUFIO                                                       
         ST    RE,BUFFIO                                                        
         L     RE,=V(BUFFALOC)                                                  
         ST    RE,BUFFBUFF                                                      
         LR    RF,RE                                                            
         GOTO1 BUFFALO,DMCB,=C'SET',(RF)                                        
         GOTO1 MEDSEED,DMCB,(RA)   SET UP REPORT TABLES                         
         L     RE,MEDTABLE                                                      
         LA    R4,CPPCTRL                                                       
M1       SR    R5,R5                                                            
         IC    R5,0(R4)                                                         
         BCTR  R5,0                                                             
         MH    R5,=H'4'                                                         
         LA    R5,0(R5,RE)         POINT TO REPORT                              
         L     R6,0(R5)            POINT TO REPORT DEF.                         
         L     R7,4(R6)            GET COL. DEF.                                
         ST    R7,FULL                                                          
         MVC   1(3,R4),FULL+1      SAVE ORIGINAL COLUMN DEF.                    
         LA    R4,4(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   M1                                                               
         MVI   CPPSW,0             DEFAULT IS NO CROSS-DAYPART CPP/M            
         LA    RF,MYHEAD                                                        
         ST    RF,HEADHOOK                                                      
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         LA    RE,BFHOOK                                                        
         ST    RE,BUFFHOOK                                                      
         DROP  RF                                                               
         DROP  RE                                                               
*                                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
M2       CLI   MODE,REQFRST                                                     
         BNE   M20                                                              
*                                                                               
         L     RE,ADAGY                                                         
         CLI   AGYPROF+7-AGYHDR(RE),C'C' TEST CANADIAN                          
         JNE   *+8                                                              
         OI    RQOPT2,RQOPT2_NETBUYS                                            
*                                                                               
         L     RF,=V(DICSECT)                                                   
         USING DICSECT,RF                                                       
         XC    DMCB,DMCB                                                        
         LA    R1,DMCB                                                          
         USING DICTATED,R1                                                      
         MVI   DDACTN,DDACTNL      TRANSLATE LIST                               
         MVI   DDRETN,DDCASEU                                                   
         MVI   DDSYS,2             SPOT                                         
         MVC   DDLANG,RCLANG                                                    
         LA    RE,DCLIST                                                        
         STCM  RE,7,DDIADR                                                      
         LA    RE,DSLIST                                                        
         STCM  RE,7,DDOADR                                                      
         GOTO1 DICTATE                                                          
         DROP  R1,RF                                                            
*                                                                               
         L     RE,AFLTRTAB                                                      
         L     RF,=F'500'                                                       
         XCEF                                                                   
         MVI   MERGESW,C'N'                                                     
         XC    FLTRCNT,FLTRCNT                                                  
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDNUMWK,=F'56'                                                  
         MVC   MEDNUMMO,=F'12'                                                  
         DROP  RE                                                               
         GOTO1 =V(SPRPFOOT),DMCB,(RA)                                           
         MVC   MAXLINES,SVMAXLIN                                                
         CLI   FOOT1,C' '                                                       
         BE    M2NOFT                                                           
         ZIC   R0,MAXLINES                                                      
         SH    R0,=H'3'                                                         
         STC   R0,MAXLINES                                                      
         MVC   FOOT1(132),SPACES                                                
M2NOFT   DS    0H                                                               
         GOTO1 =V(M3HEAD),DMCB,HCAP1                                            
         MVI   FORCEHED,C'Y'                                                    
         MVI   ESTSW,C'N'                                                       
         B     EXIT                                                             
         EJECT                                                                  
M20      CLI   MODE,CLTFRST                                                     
         BNE   M21                                                              
         MVI   FCRDGOAL,C'Y'                                                    
         MVI   RQLKGLS,C'N'                                                     
         CLI   QOPT6,C'Y'          TEST TO READ LOCKED GOALS                    
         BNE   *+8                                                              
         MVI   RQLKGLS,C'Y'                                                     
         MVC   MULTISW,SPOTPROF+13                                              
         CLI   QMED,C'C'           FORCE TO REG IF NOT COMBINED                 
         BE    *+8                                                              
         MVI   MULTISW,C'N'                                                     
         CLI   MULTISW,C'A'                                                     
         BNE   *+12                                                             
         MVI   FCRDGOAL,C'A'       SET TO READ SPOT/NETWORK GOALS               
         MVI   MULTISW,C'Y'                                                     
         CLI   MULTISW,C'Y'                                                     
         BE    *+8                                                              
         MVI   MULTISW,C'N'                                                     
         MVC   MERGESW,MULTISW                                                  
         B     EXIT                                                             
         EJECT                                                                  
M21      CLI   MODE,ESTFRST                                                     
         BNE   M3                                                               
         CLI   ESTSW,C'N'                                                       
         BNE   M141                                                             
         MVI   ESTSW,C'Y'          LOCK MEDBLOCK DATES FOR REQUEST              
*                                                                               
         L     RE,ADEST            OUT OF WEEK ROTATOR START DAY                
         USING ESTHDR,RE                                                        
*                                                                               
         MVC   PWPREFIX,SPACES                                                  
         OC    EPWPCT,EPWPCT       TEST PW CLIENT                               
         BZ    M21PWX                                                           
         MVC   PWPREFIX,=C'WIM '                                                
         CLI   QPWCV,C'Y'                                                       
         BNE   M21PWX                                                           
         MVC   PWPREFIX,=C'CLT '                                                
*                                                                               
M21PWX   CLI   EOWSDAY,0           ONLY IF THERE IS ONE INPUT                   
         BE    *+10                                                             
         MVC   SPOTPROF+8(1),EOWSDAY                                            
         DROP  RE                                                               
*                                                                               
         MVC   RQSTAFLT(1),QAFFIL     AFFILATE FILTER                           
         MVC   RQPRGTYP,QPRGTYPE   PROGRAM TYPE FILTER                          
         SPACE 2                                                                
         L     RF,ADAGY                                                         
         USING AGYHDRD,RF                                                       
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         CLI   CEXTRA+5,C'D'                                                    
         BE    *+8                                                              
         CLI   CEXTRA+5,C'Y'                                                    
         BE    *+8                                                              
         CLI   AGYPROF+7,C'C'      NO SPILL IF US AGENCY                        
         BE    *+8                                                              
         MVI   SPOTPROF+5,0                                                     
         DROP  RE                                                               
         DROP  RF                                                               
         CLI   QOPT5,C' '          SET SPILL REPORTING OPTION                   
         BE    *+10                                                             
         MVC   PROGPROF+5(1),QOPT5                                              
         CLI   PROGPROF+5,C'N'                                                  
         BNE   *+8                                                              
         MVI   PROGPROF+5,0                                                     
         CLI   PROGPROF+5,0                                                     
         BE    *+10                                                             
         MVC   SPOTPROF+5(1),PROGPROF+5                                         
         NI    SPOTPROF+5,X'0F'                                                 
         SPACE 2                                                                
*                                                                               
         GOTO1 MEDCLEAR,DMCB,MEDTABLE                                           
*                                                                               
         MVC   PAGE,=H'1'                                                       
*                                                                               
* READ D0 PROFILE TO GET LPMWK AND OVERNIGHTS OPTION                            
         MVC   WORK(10),=CL10'S0D0'                                             
         MVC   WORK+4(3),SVAGY                                                  
         MVC   WORK+7(3),QCLT                                                   
         GOTO1 GETPROF,DMCB,WORK,D0PROF,DATAMGR                                 
*                                                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDEXTAX,SPOTPROF+12                                             
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
         CLI   Q2USER+2,C'W'       WTP NEED WEEKLY                              
         BE    *+8                                                              
         CLI   QOPT1,C'Y'                                                       
         BE    *+10                                                             
         XC    MEDNUMWK,MEDNUMWK                                                
         GOTO1 MEDDATE,DMCB,(RA),0,0,1                                          
*                                                                               
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
*                                                                               
         CLI   BPRD,X'FF'          FORCE SAME DEMS FOR POL REQ                  
         BNE   M21A4                                                            
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
*                                                                               
M21A4    CLC   =C'ALL',QPRD                                                     
         BNE   M21AX                                                            
*                                                                               
         SR    RE,RE                                                            
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
         ST    RE,FULL             SAVE ADDRESS OF LIST                         
         BAS   RE,NEWDNAM          GET NEW NAMES                                
         MVI   RTGSW,0                                                          
         CLI   DNAME1,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAME1,C'R'                                                      
         BNE   *+8                                                              
         MVI   RTGSW,1                                                          
M21AX    DS    0H                                                               
*                                                                               
         SPACE 2                                                                
* SET UP CROSS DAYPART CPP/M OPTIONS                                            
         L     RE,MEDTABLE                                                      
         LA    R4,CPPCTRL                                                       
SETCPP   SR    R5,R5                                                            
         IC    R5,4(R4)            GET CONDITIONAL REPORT NUMBER                
         BCTR  R5,0                                                             
         MH    R5,=H'4'                                                         
         LA    R5,0(R5,RE)         POINT TO CONDITIONAL REPORT                  
         L     R6,0(R5)            POINT TO CONDITIONAL ROW DEF                 
         L     R7,4(R4)                                                         
         CLI   CPPSW,0                                                          
         BE    *+8                                                              
         L     R7,0(R4)            GET CPP COL DEF.                             
         LA    R7,0(R7)            CLEAR CONTROL BYTE                           
         ST    R7,4(R6)            SET COL DEF. ADDRESS                         
         LA    R4,8(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   SETCPP                                                           
         SPACE 2                                                                
* OPTIMIZE BUFFALO                                                              
         CLI   SVONEPRD,0          MULTIPLE PRODUCTS                            
         B     *+8                 ***TEMPORARY                                 
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
CHKSLTOT CLI   PROGPROF+4,C'Y'                                                  
         BE    M21EX                                                            
         LA    R8,SUPSLTOT                                                      
         BAS   R9,SUPRPTS                                                       
*                                                                               
M21EX    GOTO1 =V(SETBUF),DMCB,(RA),LCODE,LVCNTRL                               
         MVC   HIGROUP,LCODE                                                    
         L     RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',(RF)                                      
         B     M141                                                             
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
         L     RE,ADBUY            OTHEWISE PROCESS AS POL                      
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
M322     LA    R6,PSLIST                                                        
M323     CLC   0(2,R6),=X'FFFF'     END                                         
         BE    EXIT                                                             
         CLI   0(R6),0             PRODUCT DELETED                              
         BNE   *+12                                                             
         LA    R6,2(R6)                                                         
         B     M323                                                             
         L     RE,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,RE                                                      
         MVC   MEDBRAND,0(R6)                                                   
         MVC   MEDSPTLN,1(R6)                                                   
* PW CLIENT STUFF                                                               
         MVI   MEDEXTPW,C' '                                                    
         MVI   MEDEXMMR,C' '                                                    
         CLI   QPWCV,C'A'                                                       
         BL    *+14                                                             
         MVC   MEDEXTPW,QPWCV                                                   
         MVI   MEDEXMMR,C'Y'                                                    
*                                                                               
* CHECK FOR PURCH VS ACHIEVED                                                   
         CLI   QCOMPARE,C'C'                                                    
         BE    M3ACH                                                            
         CLI   QCOMPARE,C'D'                                                    
         BE    M3ACH                                                            
M323A    GOTO1 MEDGETBY,DMCB,(RA),2     ANY ORDERED                             
         BAS   RE,SETPRMY          SET PRIMARY DEMO                             
         L     RE,MEDBUFF                NO - BYPASS                            
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD                                                
         BZ    M325                                                             
         CLI   MEDSPILL,C'Y'                                                    
         BNE   M323NOSP                                                         
         CLI   SPOTPROF+5,0                                                     
         BE    EXIT                                                             
         CLI   SPLPRINT,1          PUT ORIGINATING IN BUFFER                    
         BNE   M323NOSP                                                         
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'01',SPBUFMKT),0                        
         MVI   SPLPRINT,2                                                       
M323NOSP GOTO1 MEDGETBY,DMCB,(RA),(R3)                                          
         BAS   RE,SETPRMY          SET PRIMARY DEMO                             
         DROP  R4                                                               
         MVI   STACTSW,1                                                        
         MVC   ACTAREA,4(R1)       SAVE ACTUAL BOOK AREA                        
*                                                                               
M324     BAS   RE,POSTER                                                        
         B     M325                                                             
*                                                                               
M325     LA    R6,2(R6)            BUMP TO NEXT PRODUCT                         
         B     M323                                                             
         EJECT                                                                  
* GET PURCHASE AND POST IN GOAL BUCKETS                                         
M3ACH    GOTO1 MEDGETBY,DMCB,(RA),2                                             
         L     RE,MEDBUFF                                                       
         CLI   MEDSPILL,C'Y'                                                    
         BNE   *+12                                                             
         CLI   SPOTPROF+5,0                                                     
         BE    EXIT                                                             
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD   EXIT IF NOT ACTIVE                           
         BZ    M323A                                                            
*                                                                               
         CLI   MEDSPILL,C'Y'       CHECK IF SPILL ACTIVE                        
         BNE   M3ACH1                FOR RERATE                                 
         GOTO1 MEDGETBY,DMCB,(RA),(R3)                                          
         OC    MEDBYD(12),MEDBYD   ANY ACTIVITY                                 
         BZ    M325                NO - EXIT                                    
         GOTO1 MEDGETBY,DMCB,(RA),2                                             
*                                                                               
M3ACH1   BAS   RE,SETPRMY          SET PRIMARY DEMO                             
*                                                                               
         GOTO1 =V(VMDMOVER),DMCB,(RA)                                           
*                                                                               
         DROP  R4,RE                                                            
         BAS   RE,POSTER                                                        
         B     M323A                                                            
         EJECT                                                                  
M4       CLI   MODE,PROCGOAL                                                    
         BNE   M5                                                               
         CLI   QCOMPARE,C'C'       PURCHASED ONLY                               
         BE    EXIT                                                             
         CLI   QCOMPARE,C'D'       PURCHASE ONLY                                
         BE    EXIT                                                             
         LA    RE,KEY                                                           
         USING GOALREC,RE                                                       
         LR    RF,RE                                                            
         LA    RF,1(RF)                                                         
         BAS   R9,SETPOST                                                       
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         MVC   MEDBRAND,GKEYPRD                                                 
         MVC   MEDSPTLN,GKEYSLN                                                 
         CLI   QPWCV,C'Y'                                                       
         BNE   *+8                                                              
         MVI   MEDEXTPW,C'Y'                                                    
* CHECK FOR LOCKIN DATA                                                         
         CLI   QCOMPARE,C'E'                                                    
         BE    M4L                                                              
         CLI   QCOMPARE,C'F'                                                    
         BE    M4L                                                              
         GOTO1 MEDGETGL,DMCB,(RA)                                               
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         LA    RE,MEDPERD                                                       
         L     RE,4(RE)                                                         
         USING MEDDATA,RE                                                       
         OC    MEDGLD(12),MEDGLD                                                
         BZ    EXIT                                                             
         DROP  RE,RF                                                            
         BAS   RE,SETPRMY          SET PRIMARY DEMO                             
M42      DS    0H                                                               
         BAS   RE,POSTER                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* EXTRACT LOCKIN DATA AND THEN REFORMAT AS GOAL                                 
*                                                                               
M4L      L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         MVI   MEDSPILL,C'O'       GET ORINATING                                
         GOTO1 MEDGETLK,DMCB,(RA)                                               
         BAS   RE,SETPRMY          SET PRIMARY DEMO                             
*                                                                               
         GOTO1 =V(VMDMOVER),DMCB,(RA)                                           
         BAS   RE,POSTER                                                        
*                                                                               
         CLI   SPOTPROF+5,0        ANY SPILL REQUIRED ?                         
         BE    EXIT                 NO  - JUST EXIT                             
         L     RF,MEDBUFF           YES - DO SPILL NOW                          
         MVI   MEDSPILL,C'S'                                                    
         GOTO1 MEDGETLK,DMCB,(RA)                                               
         BAS   RE,SETPRMY          SET PRIMARY DEMO                             
*                                                                               
         GOTO1 =V(VMDMOVER),DMCB,(RA)                                           
         BAS   RE,POSTER                                                        
         B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
M5       CLI   MODE,MKTLAST                                                     
         BNE   M6                                                               
*                                                                               
         CLC   QPRD,=C'POL'        FOR 'POL' REQUESTS RESET THE BRAND           
         BNE   *+12                                                             
         L     RE,MEDBUFF       TO SOMETHING WHICH MAKES MEDEDIT HAPPY          
         USING MEDBLOCK,RE         OTHERWISE WEIGHTING ROUTINES                 
         MVI   MEDBRAND,X'FF'      DO FUNNY THINGS                              
         DROP  RE                                                               
*                                                                               
         LA    RE,MYBUFIO                                                       
         LA    RF,400                                                           
         XCEF                                                                   
         XC    LASTDATE,LASTDATE                                                
         CLI   MERGESW,C'Y'                                                     
         BNE   M5NOMRG                                                          
         L     R4,FLTRCNT                                                       
         L     R9,AFLTRTAB                                                      
         GOTO1 XSORT,DMCB,(R9),(R4),5,5,0                                       
         MVI   FRSTBCDE,X'21'      SET UP FOR FILTERED REPORTS                  
         L     RF,AFLTRTAB                                                      
         ST    RF,LASTDATE                                                      
M5NOMRG  CLI   QOPT2,C'D'                                                       
         BE    M53                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         CLI   RTGSW,1                                                          
         BE    *+8                                                              
         MVI   RCSUBPRG,2                                                       
         L     R4,ACTAREA                                                       
         GOTO1 =V(VMDAPRNT),DMCB,(RA),(R4),1                                    
         SPACE 2                                                                
         OC    SPBUFMKT(2),SPBUFMKT CHECK FOR SPILL                             
         BZ    M5NOSP                                                           
         MVC   P1(11),=C'***SPILL***'                                           
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'02',SPBUFMKT),P1                       
         GOTO1 REPORT                                                           
         SPACE 2                                                                
M5NOSP   XC    PREVD,PREVD                                                      
         MVI   PRTSW,0                                                          
         CLI   LINE,40                                                          
         BH    *+8                                                              
         MVI   PRTSW,1                                                          
         MVC   CURRLN,LINE                                                      
         SR    RE,RE                                                            
         IC    RE,CURRLN                                                        
         LA    RE,3(RE)                                                         
         STC   RE,CURRLN                                                        
         MVC   WEIGHT,SPWEIGHT                                                  
         L     R4,BUFFIO                                                        
         XC    0(20,R4),0(R4)                                                   
         MVI   BUFCDE,X'21'                                                     
         MVI   0(R4),X'21'                                                      
         MVI   RCSUBPRG,1                                                       
         CLI   RTGSW,1                                                          
         BE    *+8                                                              
         MVI   RCSUBPRG,2                                                       
         L     R3,BUFFBUFF                                                      
M5A      GOTO1 BUFFALO,DMCB,=C'HIGH',(R3),(R4),1                                
         BAS   R9,FLTRRECA                                                      
         CLC   0(1,R4),BUFCDE      SAME FILE                                    
         BNE   M52C                                                             
         CLI   MULTISW,C'Y'                                                     
         BNE   M5AA1                                                            
         MVI   MEDCAPSW,C'Y'                                                    
         MVI   FORCEMID,C'Y'                                                    
         LA    RE,MID1             SET MEDIA CAPTION                            
         CLI   MID1,C' '                                                        
         BE    *+8                                                              
         LA    RE,132(RE)                                                       
         MVC   0(132,RE),SPACES                                                 
         MVC   0(11,RE),=C'COMBINED TV'                                         
         CLI   0(R4),X'60'                                                      
         BH    M5AA1                                                            
         MVC   0(11,RE),=C'NETWORK TV '                                         
         CLI   0(R4),X'40'                                                      
         BH    M5AA1                                                            
         MVC   0(11,RE),=C'SPOT TV    '                                         
M5AA1    L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         MVI   MEDSLCNT,0                                                       
         MVI   MEDDPCNT,0                                                       
         B     M52                                                              
M51      GOTO1 BUFFALO,DMCB,=C'SEQ',(R3),(R4),1                                 
         BAS   R9,FLTRRECA                                                      
M52      CLC   0(1,R4),BUFCDE                                                   
         BNE   M52C                                                             
         TM    DMCB+8,X'80'                                                     
         BO    M52C                                                             
         OC    MYBUFIO+12(250),MYBUFIO+12                                       
         BZ    M51                                                              
         SPACE 2                                                                
         USING BUFFRECD,R4                                                      
         CLI   PROGPROF+6,C'Y'     SUPPRESS CROSS DAYPART DEMOS                 
         BNE   M51EDT                                                           
         CLI   MYBUFIO+7,X'FF'     IS IT A TOTAL LINE                           
         BNE   M51EDT                                                           
         XC    BFDGL(8),BFDGL      CLEAR OUT THE POINTS                         
         XC    BFDBY1,BFDBY1                                                    
         XC    BFDEMS,BFDEMS                                                    
         DROP  R4                                                               
         SPACE 2                                                                
M51EDT   GOTO1 MEDEDIT,DMCB,(RA),(R7)                                           
         CLI   DMCB,0              DONT PRINT THIS LINE                         
         BE    M51                                                              
         CLI   PRTSW,0                                                          
         BE    M52A                                                             
         CLI   MYBUFIO+7,X'FD'                                                  
         BE    *+8                                                              
         CLI   MYBUFIO+7,X'FE'                                                  
         BNE   *+8                                                              
         MVI   DMCB,1                                                           
         MVC   SPACING,DMCB                                                     
         BAS   RE,GDTE                                                          
         TM    P1+9,X'F0'                                                       
         BO    M521                                                             
         GOTO1 MEDSTARS,DMCB,P1                                                 
M521     DS    0H                                                               
         GOTO1 REPORT                                                           
         GOTO1 =V(SPRPFOOT),DMCB,(RA)                                           
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
         OC    LASTDATE,LASTDATE   ARE FILTERS ACTIVE                           
         BZ    M53                                                              
         L     RF,LASTDATE         BUMP TO NEXT FILTER                          
         LA    RF,5(RF)                                                         
         OC    0(5,RF),0(RF)                                                    
         BZ    M53                                                              
         ST    RF,LASTDATE                                                      
         MVC   BUFCDE,FRSTBCDE     RESET BUFFER CODE                            
         B     M52C1                                                            
*                                                                               
M53      MVC   DMCB+8(20),LVCNTRL                                               
         XC    SPBUFMKT(250),SPBUFMKT                                           
         XC    SPBUFMKT+250(250),SPBUFMKT+250                                   
         L     R3,BUFFBUFF                                                      
         MVI   FRSTTOT,C'N'                                                     
         TM    LVCNTRL,X'80'                                                    
         BO    M53A                                                             
         GOTO1 BUFFALO,DMCB,=C'ADD',(X'21',(R3))                                
         MVI   DMCB+4,X'41'                                                     
         GOTO1 (RF)                                                             
         MVI   DMCB+4,X'61'                                                     
         GOTO1 (RF)                                                             
         CLC   QPRD,=C'ALL'                                                     
         BE    *+12                                                             
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
         MVI   FORCEMID,C'N'                                                    
         MVI   MID1,C' '                                                        
         MVC   MID1+1(132),MID1                                                 
         XC    PREVD,PREVD                                                      
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
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
         SPACE 2                                                                
M9       CLI   MODE,PRDLAST                                                     
         BNE   M10                                                              
         TM    QMKT,X'F0'                                                       
         BO    EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
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
         B     EXIT                                                             
         TM    QSTA,X'F0'                                                       
         BO    EXIT                                                             
         TM    QMKT,X'F0'                                                       
         BO    EXIT                                                             
         CLC   QPRD,=C'ALL'                                                     
         BE    *+14                                                             
         CLC   QPRD,=C'POL'                                                     
         BNE   EXIT                                                             
         MVI   BUFCDE,X'22'                                                     
         MVI   LCODE,2                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
         EJECT                                                                  
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
         ST    RE,FULL             SAVE ADDRESS OF LIST                         
         BAS   RE,NEWDNAM          GET NEW NAMES                                
         MVI   RTGSW,0                                                          
         CLI   DNAME1,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAME1,C'R'                                                      
         BNE   *+8                                                              
         MVI   RTGSW,1                                                          
         EJECT                                                                  
*                                                                               
M15      CLI   MODE,MKTFRST                                                     
         BNE   M16                                                              
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
         B     EXIT                                                             
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
         CLI   QOPT2,C'D'          SUPPRESS DETAILS                             
         BE    EXIT                DONT PRINT ACT. BOOK                         
         L     R8,ACTAREA                                                       
         GOTO1 =V(VMDAPRNT),DMCB,(RA),(R8),0                                    
         B     EXIT                                                             
*                                                                               
M19      B     EXIT                                                             
         EJECT                                                                  
DOSUM    L     R4,BUFFIO           DO SUMMARY REPORTS                           
         CLC   HIGROUP,LCODE       BYPASS IF NOT ACTIVE                         
         BLR   R9                                                               
         CLI   BUFFACT,0                                                        
         BER   R9                                                               
         ST    R9,SAVE9                                                         
         MVI   DSEXSW,0                                                         
         MVI   FORCEHED,C'Y'                                                    
         MVC   FRSTBCDE,BUFCDE     SET UP FOR FILTERED REPORTS                  
         L     RF,AFLTRTAB                                                      
         ST    RF,LASTDATE                                                      
         CLI   MERGESW,C'Y'                                                     
         BE    *+10                                                             
         XC    LASTDATE,LASTDATE   RESET FOR NORMAL REPORTS                     
         MVI   SW1,0                                                            
REDOSUM  L     R3,BUFFBUFF                                                      
         CLI   MULTISW,C'Y'                                                     
         BE    REDODUP                                                          
         MVI   FORCEHED,C'Y'                                                    
         ZIC   RF,BUFCDE                                                        
         LA    RF,64(RF)                                                        
         STC   RF,BUFCDE                                                        
REDODUP  CLI   SPDUPTOT,C'Y'                                                    
         BE    DOSUM3                                                           
REDO1    LA    RE,MYBUFIO                                                       
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
         BAS   R9,FLTRREC                                                       
         CLC   0(1,R4),BUFCDE      SAME FILE                                    
         BNE   DOSUM4A                                                          
         CLI   MULTISW,C'Y'                                                     
         BNE   DOSUM2                                                           
         MVI   MEDCAPSW,C'Y'                                                    
         MVI   FORCEMID,C'Y'                                                    
         LA    RE,MID1             SET MEDIA CAPTION                            
         CLI   MID1,C' '                                                        
         BE    *+8                                                              
         LA    RE,132(RE)                                                       
         MVC   0(132,RE),SPACES                                                 
         MVC   0(11,RE),=C'COMBINED TV'                                         
         CLI   0(R4),X'60'                                                      
         BH    DOSUM2                                                           
         MVC   0(11,RE),=C'NETWORK TV '                                         
         CLI   0(R4),X'40'                                                      
         BH    DOSUM2                                                           
         MVC   0(11,RE),=C'SPOT TV    '                                         
         B     DOSUM2                                                           
*                                                                               
DOSUM1   SR    R8,R8                                                            
         IC    R8,LCODE                                                         
         GOTO1 BUFFALO,DMCB,=C'SEQ',(R3),(R4),(R8)                              
         BAS   R9,FLTRREC                                                       
DOSUM2   CLC   0(1,R4),BUFCDE                                                   
         BNE   DOSUM4A                                                          
         TM    DMCB+8,X'80'                                                     
         BO    DOSUM3                                                           
         CLI   1(R4),0                                                          
         BE    DOSUM21                                                          
         TM    1(R4),X'80'         CLIENT SUMMARY                               
         BO    DOSUM2AI                                                         
         L     RE,PRDBUFF                                                       
         LA    RF,220                                                           
DOSUM2AC ZIC   R6,1(R4)            FIND FIRST BRAND FOR THIS PRIMARY            
         BCTR  R6,0 DEMO                                                        
         MH    R6,=H'3'                                                         
         LA    R6,PRMYTAB(R6)                                                   
DOSM2AC1 CLC   28(3,RE),0(R6)      CHECK AGAINST PRIMARY                        
         BE    DOSUM2AD                                                         
         AH    RE,PRDBUFLN                                                      
         BCT   RF,DOSM2AC1                                                      
         DC    H'0'                                                             
DOSUM2AD L     RF,MEDBUFF                                                       
         MVC   MEDBRAND,0(RE)      SET PRIMARY DEMO BRAND                       
         B     DOSUM2A                                                          
DOSUM2AI MVI   RCSUBPRG,7                                                       
         B     DOSUM21                                                          
DOSUM2A  XC    DNAME1(7),DNAME1                                                 
         LA    R9,1                                                             
         ZIC   RF,1(R4)            GET PRIMARY DEMO SLOT                        
         BCTR  RF,0                                                             
         MH    RF,=H'3'                                                         
         LA    RF,PRMYTAB(RF)                                                   
         ST    RF,FULL                                                          
         BAS   RE,NEWDNAM                                                       
DS2ANEW  MVI   RTGSW,0                                                          
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
         CLC   LCODE,HIGROUP                                                    
         BH    DOSUM3                                                           
         SPACE 2                                                                
         USING BUFFRECD,R4                                                      
         CLI   PROGPROF+6,C'Y'     SUPPRESS CROSS DAYPART DEMOS                 
         BNE   DS2EDT                                                           
         CLI   MYBUFIO+7,X'FF'     IS IT A TOTAL LINE                           
         BNE   DS2EDT                                                           
         XC    BFDGL(8),BFDGL      CLEAR OUT THE POINTS                         
         XC    BFDBY1,BFDBY1                                                    
         XC    BFDEMS,BFDEMS                                                    
         DROP  R4                                                               
         SPACE 2                                                                
DS2EDT   GOTO1 MEDEDIT,DMCB,(RA),(R7)                                           
         CLI   DMCB,0                                                           
         BE    DOSUM1                                                           
         CLI   MYBUFIO+7,X'FD'                                                  
         BE    *+8                                                              
         CLI   MYBUFIO+7,X'FE'                                                  
         BNE   *+8                                                              
         MVI   DMCB,1                                                           
         MVC   SPACING,DMCB                                                     
         BAS   RE,GDTE                                                          
         TM    P1+9,X'F0'                                                       
         BO    DOSUM211                                                         
         GOTO1 MEDSTARS,DMCB,P1                                                 
DOSUM211 DS    0H                                                               
         GOTO1 REPORT                                                           
         GOTO1 =V(SPRPFOOT),DMCB,(RA)                                           
         LA    RE,MYBUFIO+12                                                    
         LA    RF,388                                                           
         XCEF                                                                   
         B     DOSUM1                                                           
DOSUM3   SR    R5,R5                                                            
         IC    R5,LCODE                                                         
DOSUM4   GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFCDE,(R3)),(X'80',(R5))                
         OC    LASTDATE,LASTDATE                                                
         BZ    DOSUMX2                                                          
         CLI   DSEXSW,1                                                         
         BE    DOSUMX2                                                          
DOSUM4A  OC    LASTDATE,LASTDATE                                                
         BZ    DOSUM3                                                           
         CLI   BUFCDE,X'61'        CHECK END                                    
         BH    DOSUMX                                                           
         ZIC   RE,BUFCDE                                                        
         LA    RE,32(RE)                                                        
         STC   RE,BUFCDE                                                        
         B     REDOSUM                                                          
DOSUMX   OC    LASTDATE,LASTDATE   ARE FILTERS ACTIVE                           
         BZ    DOSUM3                                                           
         L     RF,LASTDATE         BUMP TO NEXT FILTER                          
         LA    RF,5(RF)                                                         
         OC    0(5,RF),0(RF)                                                    
         BZ    DOSUMXA                                                          
         ST    RF,LASTDATE                                                      
         MVC   BUFCDE,FRSTBCDE     RESET BUFFER CODE                            
         B     REDOSUM                                                          
DOSUMXA  MVI   DSEXSW,1                                                         
         MVI   BUFCDE,0                                                         
         B     DOSUM3                                                           
         SPACE 2                                                                
DOSUMX2  MVI   FORCEHED,C'Y'                                                    
         L     R9,SAVE9                                                         
         BR    R9                                                               
         SPACE 2                                                                
SETPOST  MVC   POSTWORK,=X'2122'   SET FOR SECONDARY POST                       
         MVC   HALF(1),0(RF)                                                    
         NI    HALF,X'0F'                                                       
         CLI   HALF,1              IS IT TV                                     
         BE    *+10                                                             
         MVC   POSTWORK,=X'4142'   SET FOR NETWORK                              
         BR    R9                                                               
FLTRRECA LA    R8,1                                                             
FLTRREC  L     RF,LASTDATE                                                      
         LTR   RF,RF                                                            
         BZR   R9                                                               
         CLC   0(5,RF),2(R4)                                                    
         BER   R9                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',(R3),(R4),(R8)                              
         TM    DMCB+8,X'80'                                                     
         BZ    *+8                                                              
         MVI   0(R4),X'FF'                                                      
         CLC   BUFCDE,0(R4)                                                     
         BNER  R9                                                               
         B     FLTRREC                                                          
         EJECT                                                                  
GDTE     NTR1                                                                   
         GOTO1 =V(VGDTE),DMCB,(RA),HCAP1                                        
         XIT1                                                                   
         LTORG                                                                  
         DROP  RF                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
POSTER   NTR1                                                                   
         MVI   BUFFACT,1                                                        
         GOTO1 =V(POSTR),DMCB,PSLIST                                            
         XIT1                                                                   
         SPACE 2                                                                
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
         SPACE 2                                                                
HEADGO   NTR1                                                                   
         GOTO1 =V(HEADC),DMCB,(RA),HCAP1                                        
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
         DS    0D                                                               
         DROP  R2                                                               
         USING *,RF                                                             
MYHEAD   NTR1  BASE=SPM3RB                                                      
         LM    RA,RC,SPM3RA                                                     
         L     R2,SPM3R2                                                        
         DROP  RF                                                               
         USING SPM302+4096,R2                                                   
         BAS   RE,HEADGO                                                        
* RQGETBF IS SET BASED ON LOCAL TESTS FOR Q2NET IN SPREPD202                    
         CLC   =C'DOLLARS',H12+44                                               
         BNE   MYH10                                                            
         CLI   RQGETBF,C'Y'        TEST BILL FORMULA ADJ                        
         BNE   MYH2                                                             
         MVC   H12+44(7),=C'CLT DOL'                                            
         B     MYH10                                                            
*                                                                               
MYH2     CLI   RQGETBF,C'X'        TEST NET DOLLAR REQUEST                      
         BNE   MYH10                                                            
         MVC   H12+44(7),=C'NET DOL'                                            
*                                                                               
MYH10    CLI   QFILTER,C'F'        FILM NUMBER FOR COKE                         
         BNE   MHFILMX                                                          
         MVC   H8(8),=C'**FILM**'                                               
         MVC   H8+9(1),QFILTER+1                                                
MHFILMX  DS    0H'0'                                                            
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
         DS    0D                                                               
         USING *,RF                                                             
BFHOOK   NTR1  BASE=SPM3RB                                                      
         LM    RA,RC,SPM3RA                                                     
         L     R2,SPM3R2                                                        
         DROP  RF                                                               
         USING SPM302+4096,R2                                                   
         L     R3,0(R1)                                                         
         USING BUFFRECD,R3                                                      
         ICM   RF,15,BFSP                                                       
         SR    RE,RE                                                            
         M     RE,SPWEIGHT                                                      
         STCM  RF,15,BFSPW                                                      
         OC    SPWEIGHT,SPWEIGHT   FORCE IF ZERO WEIGHT                         
         BNZ   *+10                                                             
         MVC   BFSPW,BFSP                                                       
         DROP  R3                                                               
         CLI   MERGESW,C'Y'                                                     
         BNE   FLTRSETX                                                         
         L     RF,AFLTRTAB                                                      
FLTRSET1 OC    0(5,RF),0(RF)       SAVE DATES FOR FILTERING                     
         BZ    FLTRSET2                                                         
         CLC   2(5,R3),0(RF)                                                    
         BE    FLTRSETX                                                         
         LA    RF,5(RF)                                                         
         B     FLTRSET1                                                         
FLTRSET2 MVC   0(5,RF),2(R3)                                                    
         L     RF,FLTRCNT                                                       
         LA    RF,1(RF)                                                         
         ST    RF,FLTRCNT                                                       
FLTRSETX DS    0C                                                               
         SPACE 2                                                                
         CLI   7(R3),X'FF'         TOTAL LINE                                   
         BE    BFHOOKA                                                          
         CLI   MODE,PROCGOAL                                                    
         BE    BFHEXIT                                                          
         L     R9,MEDBUFF          SPILL DATA                                   
         USING MEDBLOCK,R9                                                      
         CLI   MEDSPILL,C'Y'                                                    
         BNE   BFHOOKA                                                          
         CLI   SPOTPROF+5,0        SUPPRESS ALL SPILL                           
         BE    *+8                                                              
         CLI   SPOTPROF+5,1        SUPPRESS DETAIL SPILL                        
         BNE   BFHOOKA                                                          
         XC    18(64,R3),18(R3)    CLEAR IT                                     
         B     BFHEXIT                                                          
         SPACE 2                                                                
BFHOOKA  CLI   7(R3),X'FF'         TOTAL LINE                                   
         BNE   BFHEXIT                                                          
         CLI   16(R3),X'FF'        ORIG + SPILL                                 
         BE    *+8                                                              
         MVI   7(R3),X'FE'         SET FOR ORIG                                 
         CLI   16(R3),X'FE'        SPILL LINE                                   
         BNE   *+8                                                              
         MVI   7(R3),X'FD'                                                      
BFHOOK1  L     R9,MEDBUFF                                                       
         CLI   7(R3),X'FF'         OVERALL TOTALS                               
         BE    BFHEXIT                                                          
         CLI   MODE,PROCGOAL                                                    
         BNE   BFHOOK1A                                                         
         CLI   7(R3),X'FD'         IS THIS A SPILL LINE                         
         BNE   BFHOOK1A                                                         
         XC    18(64,R3),18(R3)    CLEAR IT                                     
         B     BFHEXIT                                                          
BFHOOK1A CLI   SPOTPROF+5,1        DO WE WANT S/O TOTALS                        
         BE    *+8                                                              
         CLI   SPOTPROF+5,3                                                     
         BE    *+14                                                             
         XC    18(64,R3),18(R3)                                                 
         B     BFHEXIT                                                          
         SPACE 2                                                                
         CLI   7(R3),X'FD'         SPILL                                        
         BE    BFHOOK2                                                          
         CLI   MODE,PROCGOAL       ORIG GOALS ARE OK                            
         BE    BFHEXIT                                                          
         CLI   MEDSPILL,C'Y'                                                    
         BNE   BFHEXIT                                                          
         XC    18(64,R3),18(R3)                                                 
         B     BFHEXIT                                                          
         SPACE 2                                                                
BFHOOK2  CLI   MEDSPILL,C'Y'                                                    
         BE    BFHEXIT                                                          
         XC    18(64,R3),18(R3)                                                 
         B     BFHEXIT                                                          
         SPACE 2                                                                
BFHEXIT  XIT1                                                                   
         DROP  R9                                                               
         EJECT                                                                  
SPM3RA   DC    F'0'                                                             
SPM3RB   DC    F'0'                                                             
SPM3RC   DC    F'0'                                                             
SPM3R2   DC    F'0'                                                             
         EJECT                                                                  
NEWDNAM  NTR1  GET NEW FORMAT DEMO NAMES                                        
         L     R6,ADBLOCK          R9 HAS NUMBER OF DEMOS                       
         USING DBLOCK,R6           FULL HAS START OF LIST                       
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
         MVI   DBSELMED,C'C'       SET CANADIAN MEDIA                           
*                                                                               
NEWDCDX  DS    0C                                                               
         DROP  R6                                                               
         L     R6,FULL                                                          
         L     RF,ADEST            SET FOR USER NAMES                           
         AHI   RF,EUSRNMS-ESTHDR  POINT TO USERNAMES                            
         ST    RF,DMCB+12                                                       
*                                                                               
         L     RF,VNONTNMS                                                      
         ST    RF,DMCB+16                                                       
*                                                                               
         GOTO1 DEMOCON,DMCB,((R9),(R6)),(2,DNAME1),(C'S',ADBLOCK),,,0           
         XIT1                                                                   
         EJECT                                                                  
* SET PRIMARY DEMO IN MEDBLOCK                                                  
SETPRMY  NTR1                                                                   
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         ZIC   RE,MEDBRAND        GET PRODUCT SLOT                              
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
SETPRMY4 STC   R1,MEDPRIMY          EQUATE NEW PRIMARY DEMO TO SLOT             
SETPRMYX XIT1                                                                   
         DROP  RF                                                               
         LTORG                                                                  
         EJECT                                                                  
PWPREFIX DS    CL4                                                              
HCAP1    DS    CL6                                                              
HCAP2    DS    CL9                                                              
LVCNTRL  DC    F'1'                                                             
         DC    A(2,3,4,5)                                                       
CPPCTRL  DC    X'01',AL3(0),X'04',AL3(0)                                        
         DC    X'05',AL3(0),X'08',AL3(0)                                        
         DC    X'01',AL3(0),X'11',AL3(0)                                        
         DC    X'05',AL3(0),X'12',AL3(0)                                        
         DC    X'00'                                                            
SUPONE   DC    AL1(9,10,11,12,19,0)                                             
SUPSLN   DC    AL1(1,5,9,18,19,20,21,22,23,0)                                   
SUPDPSLN DC    AL1(1,2,3,5,6,7,9,10,11,0)                                       
SUPSLTOT DC    AL1(17,18,19,20,21,22,23,0)                                      
CPPSW    DC    X'00'               CROSS DAYPART CPP SW                         
BUFFACT  DC    X'00'               BUFFALO ACTIVITY SWITCH                      
         EJECT                                                                  
PRMYTAB  DS    CL60                PRIMARY DEMO TABLE                           
PSLIST   DS    CL150              PRODUCT SPOT LENGTH LIST                      
PREVD    DS    CL5                                                              
POSTWORK DS    CL2                                                              
SVHALF   DS    CL2                                                              
MULTISW  DS    C                                                                
MEDCAPSW DS    C                                                                
FRSTTOT  DS    C                                                                
PRTSW    DS    C                                                                
CURRLN   DS    C                                                                
SW1      DS    C                   DATA SWITCH                                  
DSEXSW   DS    C                                                                
HIGROUP  DS    CL1                                                              
FLTRCNT  DC    F'0'                                                             
AFLTRTAB DC    F'0'                                                             
LASTDATE DC    F'0'                                                             
WEIGHT   DC    F'1'                                                             
SAVE9    DS    F                                                                
ACTAREA  DS    F                                                                
MERGESW  DS    C                                                                
FRSTBCDE DS    C                                                                
STACTSW  DS    C                                                                
SVMAXLIN DS    C                                                                
RTGSW    DS    C                   RATING SWITCH                                
BUFCDE   DS    C                   BUFFALO CODE                                 
LCODE    DS    C                   LEVEL CODE                                   
PBUFFCDE DS    C                                                                
DNAME1   DS    CL7                                                              
DNAME2   DS    CL7                                                              
DNAME3   DS    CL7                                                              
DNAME4   DS    CL7                                                              
SPLPRINT DS    C                                                                
D0PROF   DS    CL16                                                             
SPBUFMKT DS    CL500                                                            
MYBUFIO  DS    CL400                                                            
FLTRTAB  DS    CL500                                                            
         SPACE 2                                                                
BUFFRECD DSECT                                                                  
BFDKEY   DS    CL18                                                             
BFDDATA  DS    0CL64                                                            
BFDGL    DS    CL16                                                             
BFDBY1   DS    CL8                                                              
BFDDL    DS    CL8                                                              
BFSP     DS    CL4                                                              
BFDEMS   DS    CL24                                                             
BFSPW    DS    CL4                                                              
         EJECT                                                                  
M3HEAD   CSECT                                                                  
         NMOD1 0,M3HEAD                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R7,0(R1)                                                         
         USING HCAP1,R7                                                         
         L     R5,=V(DICSECT)                                                   
         USING DICSECT,R5                                                       
         CLI   QCOMPARE,C' '                                                    
         BNE   *+10                                                             
         MVC   QCOMPARE,PROGPROF                                                
         CLI   QDPTDET,C' '                                                     
         BNE   *+10                                                             
         MVC   QDPTDET,PROGPROF+1                                               
         MVI   CPPSW,0                                                          
         CLI   PROGPROF+2,C'Y'                                                  
         BNE   *+8                                                              
         MVI   CPPSW,1                                                          
         CLI   QOPT1,C' '                                                       
         BNE   *+10                                                             
         MVC   QOPT1,PROGPROF+3                                                 
         XC    HCAP1,HCAP1                                                      
         XC    HCAP2,HCAP2                                                      
*---->   MVC   HCAP1,=C' GOAL '                                                 
         MVC   HCAP1(L'SP@GOAL),SP@GOAL                                         
         CLI   QOPT6,C'Y'                                                       
         BNE   *+10                                                             
         MVC   HCAP1(6),=C' LKGL '                                              
*---->   MVC   HCAP2,=C'PURCHASED'                                              
         MVC   HCAP2(L'SP9PURCH),SP9PURCH                                       
         CLI   QRERATE,C' '                                                     
         BE    *+10                                                             
*---->   MVC   HCAP2(9),=C'ACHIEVED '                                           
         MVC   HCAP2(L'SP@ACHVD),SP@ACHVD                                       
         CLI   QRERATE,C'I'                                                     
         BNE   *+10                                                             
*---->   MVC   HCAP2(9),=C'AFFIDAVIT'                                           
         MVC   HCAP2(L'SP@AFFDV),SP@AFFDV                                       
         CLI   QCOMPARE,C'B'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'D'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'F'                                                    
         BNE   M3HEAD1                                                          
*---->   MVC   HCAP2(9),=C'AFFIDAVIT'                                           
         MVC   HCAP2(L'SP@AFFDV),SP@AFFDV                                       
         CLI   QRERATE,C'A'                                                     
         BE    *+8                                                              
         MVI   QRERATE,C'I'                                                     
M3HEAD1  CLI   QCOMPARE,C'C'                                                    
         BNE   *+10                                                             
*---->   MVC   HCAP2(9),=C'ACHIEVED '                                           
         MVC   HCAP2(L'SP@ACHVD),SP@ACHVD                                       
         CLI   QCOMPARE,C'C'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'D'                                                    
         BNE   *+10                                                             
*---->   MVC   HCAP1(6),=C' PURCH'                                              
         MVC   HCAP1(L'SP6PURCH),SP6PURCH                                       
         CLI   QCOMPARE,C'E'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'F'                                                    
         BNE   *+10                                                             
*---->   MVC   HCAP1(6),=C' ORDER'                                              
         MVC   HCAP1(L'SP6ORDER),SP6ORDER                                       
         MVI   ESTSW,C'N'          NEW REQUEST                                  
         MVC   RQDPOVRD,QDPTMENU   OVERRIDE DAYPART MENU                        
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
HEADC    CSECT                                                                  
         NMOD1 0,M3HEADC                                                        
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R7,4(R1)                                                         
         USING HCAP1,R7                                                         
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         L     R5,=V(DICSECT)                                                   
         USING DICSECT,R5                                                       
         CLI   SPOTPROF+12,C'Y'                                                 
         BNE   MYHEADA                                                          
*---->   MVC   H8+50(15),=C'***TAX EXCLUDED'                                    
         MVC   H8+50(L'SP@TXEX),SP@TXEX                                         
         DROP  RE                                                               
MYHEADA  CLI   RCSUBPRG,5          PRIMARY DEMO REPORT                          
         BE    *+8                                                              
         CLI   RCSUBPRG,6                                                       
         BNE   MYHEAD1                                                          
         MVC   H11+18(6),HCAP1                                                  
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
         MVC   H12+86(7),DNAME2                                                 
*---->   MVC   H12+97(3),=C'CPP'                                                
         MVC   H12+97(L'SP@CPP),SP@CPP                                          
         CLI   DNAME2,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAME2,C'R'                                                      
         BE    *+10                                                             
*---->   MVC   H12+97(3),=C'CPM'                                                
         MVC   H12+97(L'SP@CPM),SP@CPM                                          
         CLI   DNAME3,0                                                         
         BE    MYHEADX                                                          
         MVC   H12+101(7),DNAME3                                                
*---->   MVC   H12+112(3),=C'CPP'                                               
         MVC   H12+112(L'SP@CPP),SP@CPP                                         
         CLI   DNAME3,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAME3,C'R'                                                      
         BE    *+10                                                             
*---->   MVC   H12+112(3),=C'CPM'                                               
         MVC   H12+112(L'SP@CPM),SP@CPM                                         
         CLI   DNAME4,0                                                         
         BE    MYHEADX                                                          
         MVC   H12+116(7),DNAME4                                                
*---->   MVC   H12+127(3),=C'CPP'                                               
         MVC   H12+127(L'SP@CPP),SP@CPP                                         
         CLI   DNAME4,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAME4,C'R'                                                      
         BE    *+10                                                             
*---->   MVC   H12+127(3),=C'CPM'                                               
         MVC   H12+127(L'SP@CPM),SP@CPM                                         
         DROP  R5                                                               
MYHEADX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
VGDTE    CSECT                                                                  
         NMOD1 0,M3GDTE                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R7,4(R1)                                                         
         USING HCAP1,R7                                                         
         L     R5,=V(DICSECT)                                                   
         USING DICSECT,R5                                                       
         CLC   LINE,MAXLINES                                                    
         BE    *+14                                                             
         CLC   MYBUFIO+2(5),PREVD                                               
         BE    GDTEX3                                                           
         MVC   PREVD,MYBUFIO+2                                                  
         MVC   P2,P1                                                            
         XC    P,P                                                              
         CLI   PREVD,1             WEEKLYS                                      
         BNE   GDTEM                                                            
*---->   MVC   P(07),=C'WEEK OF'                                                
         MVC   P(L'SP@WKOF),SP@WKOF                                             
         LA    RF,P                                                             
         LA    RF,L'SP@WKOF-1(RF)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         GOTO1 DATCON,DMCB,(X'02',PREVD+1),(X'05',2(RF))                        
         B     GDTEX                                                            
*                                                                               
GDTEM    CLI   PREVD,2             MONTHLYS                                     
         BNE   GDTEP                                                            
         GOTO1 DATCON,DMCB,(X'02',PREVD+3),(X'05',P+3)                          
         MVC   P(3),=C'***'                                                     
         MVC   P+6(6),=C'***   '                                                
         CLI   SPOTPROF+2,0        NORMAL MONTHS                                
         BE    GDTEX                YES                                         
         GOTO1 DATCON,DMCB,(X'02',PREVD+1),(X'05',P+3)                          
         MVI   P+11,C'-'                                                        
         GOTO1 DATCON,DMCB,(X'02',PREVD+3),(X'05',P+12)                         
         B     GDTEX                                                            
*                                                                               
*---->   MVC   P(9),=C'**TOTAL**'                                               
GDTEP    MVC   P(L'SP@TOTAL),SP@TOTAL                                           
GDTEX    CLI   MEDCAPSW,C'Y'                                                    
         BNE   GDTEX2                                                           
         MVC   P3,P2                                                            
         MVC   P2,MID1                                                          
         MVC   MID1,SPACES                                                      
         MVI   FORCEMID,C'N'                                                    
GDTEX2   MVI   MEDCAPSW,C'N'                                                    
         DROP  R5                                                               
GDTEX3   XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
POSTR    CSECT                                                                  
         NMOD1 0,POSTR                                                          
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R7,0(R1)                                                         
         USING PSLIST,R7                                                        
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
         CLI   QOPT4,C'Y'          TEST OPTION TO SUPPRESS DOLLARS              
         BNE   POSTER4                                                          
         LA    RE,MEDDATES         YES-CLEAR THEM ALL OUT                       
         LA    R1,MEDTOTAL                                                      
*                                                                               
POSTER2  ICM   R8,15,4(RE)                                                      
         BZ    POSTER3                                                          
         USING MEDDATA,R8                                                       
         XC    MEDGLD(8),MEDGLD                                                 
         XC    MEDLKD(8),MEDLKD                                                 
         XC    MEDBYD(8),MEDBYD                                                 
*                                                                               
POSTER3  LA    RE,12(RE)                                                        
         CR    RE,R1                                                            
         BNH   POSTER2                                                          
         DROP  R8                                                               
*                                                                               
POSTER4  DS    0H                                                               
         GOTO1 MEDMKTWT,DMCB,(RA)                                               
         LA    R4,MEDTOTAL         MOVE DATA TO WEEK 1 SLOT                     
         L     R5,MEDAFRST                                                      
         L     R6,4(R5)                                                         
*                                                                               
POSTER6  CR    R5,R4                                                            
         BH    POSTERX                                                          
         CLI   2(R5),0                                                          
         BE    POSTER8                                                          
         L     R8,4(R5)                                                         
         MVI   WORK,1              SET FOR WEEKLY                               
         LA    RF,MEDMON01                                                      
         CR    R5,RF                                                            
         BL    *+8                                                              
         MVI   WORK,2              SET FOR MONTHLY                              
         LA    RF,MEDPERD                                                       
         CR    R5,RF                                                            
         BL    *+8                                                              
         MVI   WORK,4              SET FOR PERIOD                               
         CLI   QOPT1,C'Y'                                                       
         BE    *+12                                                             
         CLI   WORK,1                                                           
         BE    POSTER8                                                          
         L     R9,MEDLCHNK                                                      
         BCTR  R9,0                                                             
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R8)       MOVE DATA                                    
         MVC   WORK+1(4),0(R5)     MOVE DATE                                    
         MVC   SVHALF,HALF                                                      
         MVC   HALF,=X'6162'                                                    
         GOTO1 MEDPOST,DMCB,(RA)                                                
         CLI   MULTISW,C'N'                                                     
         BE    POSTER8                                                          
         MVC   HALF,POSTWORK       SET FOR THIS MEDIA                           
         GOTO1 MEDPOST,DMCB,(RA)                                                
*                                                                               
POSTER8  LA    R5,12(R5)           GET NEXT SLOT                                
         B     POSTER6                                                          
POSTERX  XMOD1 1                                                                
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
         CLI   QOPT2,C'S'                                                       
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
* SET BUFFALO LEVELS                                                            
*                                                                               
         L     R2,BUFFBUFF                                                      
         USING BUFFALOD,R2                                                      
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),0(R4)                                                  
         MVC   BUFFROWS+2(2),HALF                                               
         CLC   QPROG,=C'M3'        IS IT AN M3 REQUEST                          
         BNE   ALLOCX              NO ALLOCATION DONE ELSEWHERE                 
         OC    ABUFF,ABUFF                                                      
         BNZ   ALLOCX                                                           
         GOTO1 =V(COVAIL),DMCB,C'LOOK'                                          
         L     R9,8(R1)                                                         
         C     R9,=F'550000'       ENOUGH FOR ALLOCATION                        
         BL    ALLOCX              NO-LEAVE BUFFER AS IS                        
         S     R9,=F'500000'                                                    
         ST    R9,LNBUFF                                                        
         MVC   ABUFF,=F'50000'                                                  
         GOTO1 =V(COVAIL),DMCB,C'GET',ABUFF,LNBUFF                              
         OC    4(8,R1),4(R1)       ALLOCATE OK                                  
         BNZ   *+6                                                              
         DC    H'0'                NO                                           
         MVC   ABUFF,4(R1)                                                      
         L     R9,4(R1)            SHIFT BUFFFALO TO NEW AREA                   
         MVC   0(255,R9),0(R2)                                                  
         ST    R9,BUFFBUFF                                                      
         LR    R2,R9                                                            
         MVC   BUFFADDR,4(R1)      SET BUFFER ADDRESS                           
         L     R9,8(R1)            GET LENGTH OF BUFFER                         
         SR    R8,R8                                                            
         D     R8,BUFFLALL         DIVIDE BY RECORD LENGTH                      
         ST    R9,BUFFCRMX         SAVE NUMBER OF ENTRIES                       
ALLOCX   L     R9,BUFFLALL         GET MAXIMUM CORE AVAILABLE                   
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
ABUFF    DC    A(0)                ADDRESS OF BUFFER                            
LNBUFF   DC    A(0)                LENGTH OF BUFFER                             
         LTORG                                                                  
         EJECT                                                                  
DICSECT  CSECT                                                                  
*        PRINT GEN                                                              
DCLIST   DS    0C                                                               
         DCDDL SP#ACHVD,9                                                       
         DCDDL SP#AFFDV,9                                                       
         DCDDL SP#COMTV,11                                                      
         DCDDL SP#CPM,3                                                         
         DCDDL SP#CPP,3                                                         
         DCDDL SP#FILM,8,C                                                      
         DCDDL SP#GOAL,6,C                                                      
         DCDDL SP#NETTV,11                                                      
         DCDDL SP#ORDER,6,R,LABEL=SP6ORDER                                      
         DCDDL SP#PURCH,9,LABEL=SP9PURCH                                        
         DCDDL SP#PURCH,6,R,LABEL=SP6PURCH                                      
         DCDDL SP#SPILL,11,C                                                    
         DCDDL SP#SPTTV,11                                                      
         DCDDL SP#TOTAL,9,C                                                     
         DCDDL SP#TXEX,18,C                                                     
         DCDDL SP#WKOF,10                                                       
DCLISTX  DC    X'00'                                                            
*                                                                               
DSLIST   DS    0C                                                               
         DSDDL PRINT=YES                                                        
DSLISTX  EQU   *                                                                
         PRINT NOGEN                                                            
*                                                                               
         EJECT                                                                  
         BUFF  LINES=050,ROWS=5,COLUMNS=16,FLAVOR=BINARY,KEYLIST=(18,A)         
         PRINT OFF                                                              
* SPREPWORKD                                                                    
       ++INCLUDE SPREPWORKD                                                     
         ORG   QGRP                                                             
QOPT6    DS    C            REPORT LOCKED GOALS                                 
       ++INCLUDE SPREPPTBUF                                                     
         EJECT                                                                  
* MEDPRTOPT                                                                     
*       +INCLUDE MEDRPTOPT                                                      
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
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPDDEQUS                                                       
         EJECT                                                                  
* DDDICTATED                                                                    
       ++INCLUDE DDDICTATED                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'147SPREPM302 11/21/19'                                      
         END                                                                    
