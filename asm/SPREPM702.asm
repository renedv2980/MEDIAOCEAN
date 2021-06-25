*          DATA SET SPREPM702  AT LEVEL 085 AS OF 11/21/19                      
*PHASE SPM702T                                                                  
*INCLUDE SPM7TP                                                                 
*INCLUDE SPRPFOOT                                                               
*INCLUDE MEDAPRNT                                                               
*INCLUDE REPSPILL                                                               
*INCLUDE REPCALOV                                                               
*INCLUDE COVAIL                                                                 
         TITLE 'SPREPM702-PB MARKET PERFORMANCE'                                
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
SPM702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPM702,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING SPM702+4096,R2                                                   
         ST    R2,SPM7R2                                                        
         STM   RA,RC,SPM7RA                                                     
         ST    R5,RELO                                                          
         GOTO1 =V(SPRPFOOT),DMCB,(RA)                                           
         CLI   MODE,MKTLAST        GT. MKTLAST-GET WEIGHTS                      
         BL    BYPW                                                             
         GOTO1 MEDADDWT,DMCB,(RA)                                               
         CLI   QOPT3,C'D'                                                       
         BNE   BYPW                                                             
         CLI   FRSTTOT,C'Y'                                                     
         BNE   BYPW                                                             
         MVI   SPDUPTOT,C'N'                                                    
         MVI   FRSTTOT,C'N'                                                     
BYPW     DS    0H                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   M2                                                               
         MVC   SVMAXLIN,MAXLINES                                                
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDNUMWK,=F'56'                                                  
         MVC   MEDNUMPE,=F'1'                                                   
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         LA    RF,MYHEAD                                                        
         ST    RF,HEADHOOK                                                      
         MVI   MEDEXTDM,2                                                       
         DROP  RE                                                               
         L     RE,=V(SETBUF)                                                    
         A     RE,RELO                                                          
         ST    RE,VSETBUF                                                       
         L     RE,=V(BLDPDEM)                                                   
         A     RE,RELO                                                          
         ST    RE,VBLDPDEM                                                      
         LA    RE,MYBUFIO                                                       
         ST    RE,BUFFIO                                                        
         L     R6,=V(BUFFALOC)                                                  
         A     R6,RELO                                                          
         ST    R6,BUFFBUFF                                                      
         L     RE,=V(SPM710C)                                                   
         A     RE,RELO                                                          
         ST    RE,ASPM701                                                       
         L     RE,=V(SPM740C)                                                   
         A     RE,RELO                                                          
         ST    RE,ASPM704                                                       
         L     RE,=V(SPM7401C)                                                  
         A     RE,RELO                                                          
         ST    RE,ASPM7041                                                      
         L     R6,=V(XTRMEDT)                                                   
         A     R6,RELO                                                          
         ST    R6,VXTRMEDT                                                      
         L     R6,VXTRMEDT                                                      
         GOTO1 LOADER,DMCB,SUBPROG,(R6)                                         
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   VXTRMEDT,DMCB+4                                                  
         MVC   DUB,=CL8'SPM710'                                                 
         L     R6,ASPM710                                                       
         GOTO1 LOADER,DMCB,DUB,(R6)                                             
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   ASPM710,DMCB+4                                                   
         MVC   DUB,=CL8'SPM740'                                                 
         L     R6,ASPM740                                                       
         GOTO1 LOADER,DMCB,DUB,(R6)                                             
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   ASPM740,DMCB+4                                                   
         MVC   DUB,=CL8'SPM7401'                                                
         L     R6,ASPM7401                                                      
         GOTO1 LOADER,DMCB,DUB,(R6)                                             
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   ASPM7401,DMCB+4                                                  
         MVC   ASPM701,SPECS                                                    
         MVC   ASPM704,MEDTABLE                                                 
         MVC   ASPM7041,VXTRMEDT                                                
         LA    RE,IEEQU                                                         
         ST    RE,AIEEQU                                                        
         LA    RE,EIEQU                                                         
         ST    RE,AEIEQU                                                        
         LA    RE,NEWDNAM                                                       
         ST    RE,ANEWDNAM                                                      
         LA    RE,MYBUFIO                                                       
         ST    RE,BUFFIO                                                        
         LA    RE,RFMTBUFF                                                      
         L     R7,BUFFBUFF                                                      
         USING BUFFALOD,R7                                                      
         ST    RE,BUFFHOOK                                                      
         DROP  R7                                                               
         L     R6,BUFFBUFF                                                      
*                                                                               
         CLC   RCPROG,=CL2'MJ'     CODE NOT = TO MJ DONT PRODUCE TAPE           
         BNE   BY7TP                                                            
         GOTO1 =V(SPM7TP),DMCB,(C'I',(RA)),AIEEQU,ANEWDNAM,(R4),RR=RB           
*                                                                               
BY7TP    GOTO1 BUFFALO,DMCB,=C'SET',(R6)                                        
         GOTO1 =V(COVAIL),DMCB,C'SETB',1400000,1800000,(R6)                     
         MVC   BUFFBUFF,12(R1)                                                  
*                                                                               
         GOTO1 MEDSEED,DMCB,(RA)   SET UP REPORT TABLES                         
         MVC   SVMEDTAB,MEDTABLE   SEED SPOT LENGTH TABLES                      
         MVC   MEDTABLE,VXTRMEDT                                                
         GOTO1 MEDSEED,DMCB,(RA)                                                
         MVC   MEDTABLE,SVMEDTAB                                                
*                                                                               
         MVC   SVMEDTAB,MEDTABLE   SEED SPOT LENGTH TABLES                      
         MVC   MEDTABLE,ASPM740                                                 
         GOTO1 MEDSEED,DMCB,(RA)   SET UP REPORT TABLES                         
         MVC   MEDTABLE,ASPM7401                                                
         GOTO1 MEDSEED,DMCB,(RA)                                                
         MVC   MEDTABLE,SVMEDTAB                                                
*                                                                               
         L     RE,=V(VAGYLIST)                                                  
         A     RE,RELO                                                          
         ST    RE,AGYLIST          CLEAR AGENCY LIST                            
         LA    RF,360                                                           
         XCEF                                                                   
         L     RE,MEDTABLE         SET CPP ADDRESS 04 PHASE                     
         LA    R4,CPPCTRL                                                       
         BAS   R9,M1                                                            
         L     RE,ASPM740          SET CPP ADDRESS 40 PHASE                     
         LA    R4,CPPCTRL1                                                      
         BAS   R9,M1                                                            
*                                                                               
         MVI   CPPSW,0             DEFAULT IS NO CROSS-DAYPART CPP/M            
         B     EXIT                                                             
*                                                                               
M1       SR    R5,R5                                                            
         IC    R5,0(R4)                                                         
         BCTR  R5,0                                                             
         MH    R5,=H'4'                                                         
         LA    R5,0(R5,RE)         POINT TO REPORT                              
         L     R6,0(R5)            POINT TO REPORT DEF                          
         L     R7,4(R6)            GET COL. DEF                                 
         ST    R7,FULL                                                          
         MVC   1(3,R4),FULL+1      SAVE ORIGINAL COLUMN DEFINITIONS             
         LA    R4,4(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   M1                                                               
         BR    R9                                                               
         EJECT                                                                  
*                                                                               
M2       CLC   QCLT,=C'ALL'     SIMULATE MULTIPLE REQUESTS IF CLT=ALL           
         BNE   M2NORMAL                                                         
         CLI   MODE,CLTFRST                                                     
         BNE   M2NORMAL                                                         
         MVC   QSTART(12),ORIGDATE RESTORE REQ DATES                            
         MVI   SPOTPROF+2,0        FORCE NORMAL DATES                           
         XC    SPOTPROF+6(3),SPOTPROF+6                                         
         B     M2NORM01            PROCESS AS REQFRST                           
*                                                                               
M2NORMAL CLI   MODE,REQFRST                                                     
         BNE   M21                                                              
*                                                                               
         OI    RQOPTS,RQOPTS_POST    FLAG TO INDICATE SPOT POSTING              
*                                                                               
         L     RE,ADAGY                                                         
         CLI   AGYPROF+7-AGYHDR(RE),C'C' TEST CANADIAN                          
         JNE   *+8                                                              
         OI    RQOPT2,RQOPT2_NETBUYS                                            
*                                                                               
M2NORM01 MVC   ORIGDATE,QSTART     SAVE REQUEST DATES                           
         GOTO1 =V(SPRPFOOT),DMCB,(RA)                                           
         GOTO1 =V(RQFRSTC),DMCB,(RA),HCAP1                                      
         XC    PRMYTAB,PRMYTAB                                                  
         B     EXIT                                                             
         EJECT                                                                  
M21      CLI   MODE,ESTFRST                                                     
         BNE   M3                                                               
         CLC   QEST,=C'ALL'        ALWAYS REDO IF EST = ALL                     
         BE    *+12                                                             
         CLI   ESTSW,C'N'                                                       
         BNE   EXIT                                                             
         GOTO1 =V(EFRSTC),DMCB,(RA),HCAP1                                       
         MVI   FCRDGOAL,C'Y'                                                    
*                                                                               
         MVI   RQLKGLS,C'N'                                                     
         CLI   QOPT6,C'Y'          TEST TO REPORT LOCKED GOALS                  
         BNE   *+8                                                              
         MVI   RQLKGLS,C'Y'                                                     
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
         B     EXIT                                                             
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
         LA    RE,PSLIST                                                        
         CLI   0(RE),0                                                          
         BE    *+12                                                             
         LA    RE,2(RE)                                                         
         B     *-12                                                             
         MVC   0(2,RE),=X'FFFF'                                                 
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
* CHECK FOR PURCH VS ACHIEVED                                                   
* CHECK FOR PURCH VS PURCH-RERATED OR AFFID                                     
         CLI   QCOMPARE,C'H'       PURCH VS PURCH-RERATED                       
         BE    M3ACH                                                            
         CLI   QCOMPARE,C'I'       PURCH VS AFFIDAVIT                           
         BE    M3ACH                                                            
M323A    GOTO1 MEDGETBY,DMCB,(RA),2     ANY ORDERED                             
         GOTO1 SETPRMY                                                          
         L     RE,MEDBUFF                NO - BYPASS                            
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD                                                
         BZ    M323B                                                            
         CLI   MEDSPILL,C'Y'       SPILL LINE                                   
         BNE   M323NOSP                                                         
         CLI   SPOTPROF+5,0        BYPASS SPILL                                 
         BE    EXIT                                                             
         CLI   SPLPRINT,1          PUT ORIGINATING IN BUFFER                    
         BNE   M323NOSP                                                         
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'01',SPBUFMKT),0                        
         MVI   SPLPRINT,2                                                       
M323NOSP GOTO1 MEDGETBY,DMCB,(RA),(R3)                                          
         GOTO1 SETPRMY                                                          
         L     RE,MEDBUFF                                                       
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD                                                
         BZ    M323B                                                            
         MVI   STACTSW,1                                                        
         MVC   ACTAREA,4(R1)                                                    
*                                                                               
         GOTO1 MEDMKTWT,DMCB,(RA)                                               
         BAS   RE,POST                                                          
M323B    LA    R6,2(R6)                                                         
         B     M323                                                             
         EJECT                                                                  
* GET PURCHASED AND POST IN LOCKIN BUCKETS                                      
M3ACH    GOTO1 MEDGETBY,DMCB,(RA),2                                             
         L     RE,MEDBUFF                                                       
         CLI   MEDSPILL,C'Y'       SPILL LINE                                   
         BNE   *+12                                                             
         CLI   SPOTPROF+5,0        BYPASS SPILL                                 
         BE    EXIT                                                             
         GOTO1 SETPRMY                                                          
*                                                                               
         L     RE,MEDBUFF                                                       
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD                                                
         BZ    M323A                                                            
* SET GOAL BUCKETS                                                              
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
         BAS   RE,POST                                                          
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
* CHECK FOR LOCKIN DATA                                                         
         GOTO1 MEDGETGL,DMCB,(RA)                                               
         GOTO1 SETPRMY                                                          
*                                                                               
         L     RE,MEDBUFF          CHECK FOR ACTIVITY                           
         USING MEDBLOCK,RE                                                      
         LA    R4,MEDPERD                                                       
         L     R4,4(R4)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDGLD(12),MEDGLD                                                
         BZ    M43                                                              
*                                                                               
M42      GOTO1 MEDMKTWT,DMCB,(RA)                                               
         BAS   RE,POST                                                          
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
         USING MEDBLOCK,RE                                                      
         MVI   MEDSPILL,C'O'                                                    
*                                                                               
         MVI   MODE,PROCBUY        FUDGE TO FORCE OUT TOTALS                    
         MVC   SAVNUMWK,MEDNUMWK   GETLK DOESNT LIKE WEEKS HERE                 
         XC    MEDNUMWK,MEDNUMWK   KILL AND RESTORE                             
         GOTO1 MEDGETLK,DMCB,(RA)                                               
*                                                                               
         L     RE,MEDBUFF          CHECK FOR ACTIVITY                           
         MVC   MEDNUMWK,SAVNUMWK                                                
         USING MEDBLOCK,RE                                                      
         LA    R4,MEDPERD                                                       
         L     R4,4(R4)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDLKD(12),MEDLKD                                                
         BZ    M4LSP                                                            
*                                                                               
         GOTO1 SETPRMY                                                          
*                                                                               
         GOTO1 MEDMKTWT,DMCB,(RA)                                               
         BAS   RE,POST                                                          
M4LSP    CLI   SPOTPROF+5,0                                                     
         BE    M4LX                                                             
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVI   MEDSPILL,C'S'                                                    
*                                                                               
         MVC   SAVNUMWK,MEDNUMWK   GETLK DOESNT LIKE WEEKS HERE                 
         XC    MEDNUMWK,MEDNUMWK   KILL AND RESTORE                             
         GOTO1 MEDGETLK,DMCB,(RA)                                               
*                                                                               
         L     RE,MEDBUFF          CHECK FOR ACTIVITY                           
         USING MEDBLOCK,RE                                                      
         MVC   MEDNUMWK,SAVNUMWK                                                
         LA    R4,MEDPERD                                                       
         L     R4,4(R4)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDLKD(12),MEDLKD                                                
         BZ    M4LX                                                             
*                                                                               
         GOTO1 SETPRMY                                                          
*                                                                               
         GOTO1 MEDMKTWT,DMCB,(RA)                                               
         L     RE,MEDBUFF                                                       
         MVI   MEDSPILL,C'Y'                                                    
         BAS   RE,POST                                                          
M4LX     MVI   MODE,PROCGOAL       RESET TO REAL MODE                           
         B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
M5       CLI   MODE,MKTLAST                                                     
         BNE   M6                                                               
         MVI   RCSUBPRG,1                                                       
         CLI   RTGSW,1                                                          
         BE    *+8                                                              
         MVI   RCSUBPRG,2                                                       
         MVI   LCODE,1                                                          
         MVI   BUFCDE,X'21'                                                     
         MVI   FRSTTOT,C'Y'                                                     
         CLI   QOPT3,C'D'                                                       
         BE    M53                                                              
         BAS   R9,DOSUM                                                         
*                                                                               
         MVI   FORCEHED,C'N'                                                    
         XC    P1(132),P1                                                       
         L     R3,ACTAREA                                                       
         GOTO1 =V(VMDAPRNT),DMCB,(RA),(R3),1                                    
         SPACE 2                                                                
         OC    SPBUFMKT(2),SPBUFMKT CHECK FOR SPILL                             
         BZ    M5NOSP                                                           
         MVC   P1(11),=C'***SPILL***'                                           
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'02',SPBUFMKT),P1                       
         GOTO1 REPORT                                                           
M5NOSP   DS    0H'0'                                                            
         MVI   FORCEHED,C'Y'                                                    
         SPACE 2                                                                
M53      MVC   DMCB+8(20),LVCNTRL                                               
         XC    SPBUFMKT,SPBUFMKT                                                
         L     R3,BUFFBUFF                                                      
         TM    DMCB+8,X'80'                                                     
         BO    M53A                                                             
         CLC   QPROG,=C'MJ'        NO SUMMARY FOR JWT TAPE                      
         BE    M53A                                                             
         GOTO1 BUFFALO,DMCB,=C'ADD',(R3)                                        
M53A     GOTO1 BUFFALO,DMCB,=C'CLEAR',(R3),(X'80',1)                            
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
M9       DS    0H                                                               
M10      DS    0H                                                               
M12      DS    0C                                                               
M13      CLI   MODE,PRDLAST                                                     
         BNE   M14                                                              
         TM    QMKT,X'F0'          NUMERIC MARKET                               
         BO    EXIT                                                             
         MVI   BUFCDE,X'21'                                                     
         MVI   LCODE,2                                                          
         MVI   RCSUBPRG,7                                                       
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
M14      CLI   MODE,PRDFRST                                                     
         BNE   M15                                                              
M141     SR    RE,RE                                                            
         IC    RE,BPRD                                                          
         CLI   BPRD,X'FF'         GET DEMO NAMES FOR PRODUCT                    
         BNE   *+8                                                              
         MVI   BUFCDE,X'21'                                                     
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RE,28(RE,RF)                                                     
         XC    DNAME1(28),DNAME1                                                
         LA    R6,28(RE)           GET DEMO NAMES                               
         GOTO1 ANEWDNAM,DMCB,(2,(R6)),DNAME1                                    
M14B     MVI   RTGSW,0                                                          
         CLI   DNAME1,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAME1,C'R'                                                      
         BNE   *+8                                                              
         MVI   RTGSW,1                                                          
*                                                                               
M15      CLI   MODE,MKTFRST                                                     
         BNE   M16                                                              
         XC    SPBUFMKT,SPBUFMKT                                                
         B     EXIT                                                             
*                                                                               
M16      CLI   MODE,REQLAST                                                     
         BNE   M17                                                              
         MVI   FORCEHED,C'N'                                                    
         MVI   P,0                                                              
         MVC   P2(132),FOOT1                                                    
         GOTO1 REPORT                                                           
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
         CLI   QOPT3,C'D'                                                       
         BE    EXIT                                                             
         L     R8,ACTAREA                                                       
         GOTO1 =V(VMDAPRNT),DMCB,(RA),(R8),0                                    
         B     EXIT                                                             
*                                                                               
M19      CLI   MODE,RUNLAST                                                     
         BNE   M20                                                              
         CLC   QCODE,=CL2'MJ'      CODE NOT = TO MJ DONT PRODUCE TAPE           
         BNE   M20                                                              
         GOTO1 =V(SPM7TP),DMCB,(C'C',(RA)),AIEEQU,ANEWDNAM,(R4),RR=RB           
*                                                                               
M20      B     EXIT                                                             
         EJECT                                                                  
DOSUM    L     R4,BUFFIO           DO SUMMARY REPORTS                           
         ST    R9,SAVE9                                                         
         MVI   SW1,0                                                            
         MVI   SW2,0                                                            
         MVC   WEIGHT,SPWEIGHT                                                  
         MVI   FORCEHED,C'Y'                                                    
         MVI   HEADSW,C'M'                                                      
         MVI   PBCDE1,0                                                         
         XC    PRIMDEM,PRIMDEM                                                  
         MVI   PBUFFCDE,0                                                       
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   PLCNTR,0                                                         
         XC    PREVIO,PREVIO                                                    
REDOSUM  L     R3,BUFFBUFF                                                      
         CLI   SPDUPTOT,C'Y'                                                    
         BE    DOSUM3                                                           
         LA    RE,MYBUFIO                                                       
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
         CLC   0(1,R4),BUFCDE                                                   
         BNE   DOSUM3                                                           
         CLI   MULTISW,C'Y'                                                     
         BNE   DOSUM2                                                           
         LA    RE,P1                                                            
         MVC   0(11,RE),=C'COMBINED TV'                                         
         CLI   0(R4),X'60'                                                      
         BH    DS2PRT                                                           
         MVC   0(11,RE),=C'NETWORK TV '                                         
         CLI   0(R4),X'40'                                                      
         BH    DS2PRT                                                           
         MVC   0(11,RE),=C'SPOT TV    '                                         
DS2PRT   GOTO1 REPORT                                                           
         B     DOSUM2                                                           
*                                                                               
DOSUM1   SR    R8,R8                                                            
         IC    R8,LCODE                                                         
         L     R3,BUFFBUFF                                                      
         L     R4,BUFFIO                                                        
         MVC   P2,SPACES                                                        
         GOTO1 BUFFALO,DMCB,=C'SEQ',(R3),(R4),(R8)                              
DOSUM2   TM    DMCB+8,X'80'                                                     
         BO    DOSUM3                                                           
         CLC   0(1,R4),BUFCDE                                                   
         BNE   DOSUM3                                                           
*                                                                               
         CLC   QCODE,=CL2'MJ'      CODE NOT = TO MJ DONT PRODUCE TAPE           
         BNE   DOSUM2B                                                          
*                                                                               
         CLI   MODE,MKTLAST        GT. MKTLAST-BYPASS TAPE WRITE                
         BNE   DOSUM2B                                                          
*                                                                               
         GOTO1 =V(SPM7TP),DMCB,(C'A',(RA)),AIEEQU,ANEWDNAM,(R4),       X        
               RR=RB                                                            
DOSUM2B  L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDBRAND,MYBUFIO+9                                               
         DROP  RE                                                               
         MVC   SPWEIGHT,WEIGHT                                                  
         CLC   MYBUFIO(10),PREVIO                                               
         BE    DOSUM2X                                                          
         MVI   ALLOWLIN,7                                                       
         MVI   PLCNTR,0            RESET PRINT LINE COUNTER                     
         CLC   MYBUFIO+2(2),PREVIO+2   AGENCY BREAK                             
         BE    *+8                                                              
         BAS   R9,AGYML                                                         
         CLC   MYBUFIO+4(2),PREVIO+4    PRIMARY DEMO BREAK                      
         BE    *+8                                                              
         BAS   R9,PDML                                                          
         CLC   MYBUFIO+9(1),PREVIO+9   BRAND BREAK                              
         BE    *+8                                                              
         BAS   R9,BRNDML                                                        
         MVC   PREVIO,MYBUFIO                                                   
DOSUM2X  DS    0H                                                               
         MVC   PRIMDEM,MYBUFIO+4                                                
DOSUM20  CLI   PRIMDEM+1,0         SECONDARY DEMO LINE                          
         BE    DOSUM201                                                         
         GOTO1 =V(UNWGHT),DMCB,(RA),PSLIST                                      
         MVC   SPWEIGHT,=F'1'                                                   
DOSUM201 DS    0H                                                               
         TM    MYBUFIO+20,X'80'                                                 
         BZ    *+14                                                             
         NI    MYBUFIO+20,X'7F'                                                 
         MVC   MEDTABLE,VXTRMEDT   FORCE SPOT LENGTH TABLE                      
         GOTO1 MEDEDIT,DMCB,(RA)                                                
         MVC   SPWEIGHT,WEIGHT                                                  
         MVC   MEDTABLE,SVMEDTAB   RESTORE NORMAL TABLE                         
         CLI   DMCB,0                                                           
         BE    DOSUM1                                                           
         MVC   SPACING,DMCB                                                     
         CLI   PRIMDEM+1,0                                                      
         BNE   DOSUM22                                                          
         CLI   RPTNUM,1            DIFFERENT FORMAT                             
         BE    DSOPTR1                                                          
         MVC   P+71(7),P2          REFORMAT PRINT LINE                          
         MVC   P+79(4),P2+24                                                    
         MVC   P+83(9),P2+7                                                     
         MVC   P+93(4),P2+28                                                    
         MVC   P+97(8),P2+16                                                    
         B     DSOPTRX                                                          
*                                                                               
DSOPTR1  MVC   P1+35(7),P2         POINTS                                       
         MVC   P1+47(9),P2+7       DOLLARS                                      
         MVC   P1+61(8),P2+16      CPP/M                                        
         MVC   P1+73(7),P2+24      POINTS                                       
         MVC   P1+90(9),P2+31      DOLLARS                                      
         MVC   P1+109(8),P2+40      CPP/M                                       
*                                                                               
DSOPTRX  MVC   P2,SPACES                                                        
         CLI   PLCNTR,0                                                         
         BE    DOSUM22                                                          
         SR    RE,RE                                                            
         IC    RE,PLCNTR                                                        
         LA    RE,1(RE)                                                         
         MH    RE,=H'132'                                                       
         LA    RE,P1(RE)                                                        
         MVC   0(132,RE),P1        MOVE DATA LINE TO CORRECT SLOT               
         XC    P1,P1               MOVE IN MIDLINES                             
         LA    RF,P2                                                            
         LA    R9,MID1                                                          
         SR    RE,RE                                                            
         IC    RE,PLCNTR                                                        
DOSUM21  MVC   0(132,RF),0(R9)                                                  
         MVC   0(132,R9),SPACES                                                 
         LA    R9,132(R9)                                                       
         LA    RF,132(RF)                                                       
         BCT   RE,DOSUM21                                                       
         MVI   FORCEMID,C'N'       RESET MIDLINES                               
DOSUM22  TM    P1+9,X'F0'                                                       
         BO    DOSUM22A                                                         
         GOTO1 MEDSTARS,DMCB,P1                                                 
DOSUM22A DS    0H                                                               
         GOTO1 REPORT                                                           
         GOTO1 =V(SPRPFOOT),DMCB,(RA)                                           
         MVI   ALLOWLIN,0                                                       
         MVI   PLCNTR,0                                                         
         LA    RE,MYBUFIO+12                                                    
         LA    RF,388                                                           
         XCEF                                                                   
         B     DOSUM1                                                           
*                                                                               
DOSUM3   SR    R5,R5                                                            
         IC    R5,LCODE                                                         
         CLI   LCODE,1                                                          
         BE    DOSUM4A                                                          
DOSUM4   GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFCDE,(R3)),(X'80',(R5))                
DOSUM4A  CLI   BUFCDE,X'61'        CHECK FOR END                                
         BH    DOSUMX                                                           
         ZIC   RE,BUFCDE                                                        
         LA    RE,32(RE)                                                        
         STC   RE,BUFCDE                                                        
         B     REDOSUM                                                          
DOSUMX   MVI   FORCEHED,C'Y'                                                    
DOSUM5   L     R9,SAVE9                                                         
         BR    R9                                                               
         SPACE 2                                                                
SETPOST  MVC   POSTWORK,=X'21'     SET UP FOR SECONDARY POST                    
         MVC   HALF,0(RF)                                                       
         NI    HALF,X'0F'                                                       
         CLI   HALF,8              IS IT COMBINED                               
         BNE   *+12                                                             
         MVC   POSTWORK,=X'6162'                                                
         BR    R9                                                               
         CLI   HALF,1              IS IT SPOT                                   
         BE    *+10                                                             
         MVC   POSTWORK,=X'41'     SET UP FOR NETWORK                           
         BR    R9                                                               
         LTORG                                                                  
         EJECT                                                                  
POST     NTR1                                                                   
         GOTO1 =V(SETPDEM),DMCB,(RA),PSLIST                                     
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         ZIC   RE,MEDBRAND                                                      
         DROP  RF                                                               
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF                                                       
         MVC   WORK+2(3),1(RE)                                                  
         BAS   R9,GETAGYN                                                       
         MVC   SVHALF,HALF                                                      
         MVC   HALF(1),=X'61'                                                   
         GOTO1 MEDPOST,DMCB,(RA)                                                
         MVC   MEDTABLE,VXTRMEDT   SET SPOT LENGTH TABLE                        
         GOTO1 MEDPOST,DMCB,(RA)                                                
         MVC   MEDTABLE,SVMEDTAB                                                
         SPACE 2                                                                
         CLI   MULTISW,C'Y'        DOING COMBINED BREAKOUTS                     
         BNE   POSTX                                                            
         CLI   POSTWORK,X'61'                                                   
         BE    POSTX                                                            
         MVC   HALF(1),POSTWORK    POST SPOT OR NETWORK                         
         GOTO1 MEDPOST,DMCB,(RA)                                                
         MVC   MEDTABLE,VXTRMEDT   SET SPOT LENGTH TABLE                        
         GOTO1 MEDPOST,DMCB,(RA)                                                
         MVC   MEDTABLE,SVMEDTAB                                                
         MVC   HALF,SVHALF                                                      
POSTX    XIT1                                                                   
         EJECT                                                                  
GETAGYN  XC    WORK(2),WORK                                                     
         L     RE,ADBUY                                                         
         CLI   AGYRECAP,1          AGENCY RECAP REQUIRED                        
         BNER  R9                                                               
         MVC   WORK(2),20(RE)                                                   
         BR    R9                                                               
*                                                                               
* SET UP AGENCY MIDLINES                                                        
AGYML    MVI   PREVIO+4,0                                                       
         CLC   MYBUFIO+2(2),=X'FFFF'                                            
         BNE   AGYML10                                                          
         MVC   MID1(12),=C'ALL AGENCIES'                                        
         MVI   ALLOWLIN,15                                                      
         B     AGYML20                                                          
AGYML10  MVC   MID1(2),MYBUFIO+2   AGENCY NAME ROUTINE GOES HERE                
         L     R8,AGYLIST                                                       
AGYML11  CLI   0(R8),0                                                          
         BE    AGYML12                                                          
         CLC   MYBUFIO+2(2),0(R8)                                               
         BE    AGYML13                                                          
         LA    R8,35(R8)                                                        
         B     AGYML11                                                          
AGYML12  XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),MYBUFIO+2                                               
         L     RE,ADBUY                                                         
         ST    RE,AREC                                                          
         GOTO1 READ                                                             
         GOTO1 GET                                                              
         L     RE,ADBUY                                                         
         USING AGYHDR,RE                                                        
         MVC   0(2,R8),MYBUFIO+2                                                
         MVC   2(33,R8),AGYNAME                                                 
         DROP  RE                                                               
AGYML13  LA    R1,MID1                                                          
         CLI   PLCNTR,0                                                         
         BE    *+8                                                              
         LA    R1,132(R1)                                                       
         MVC   0(33,R1),2(R8)                                                   
         B     PDML20                                                           
AGYML20  MVI   PLCNTR,1                                                         
         MVI   FORCEMID,C'Y'                                                    
         BR    R9                                                               
         EJECT                                                                  
* SET UP PRIMARY DEMO MIDLINES                                                  
PDML     CLI   MYBUFIO+9,X'FF'                                                  
         BNER  R9                                                               
         MVC   PREVIO,MYBUFIO                                                   
         CLI   MYBUFIO+5,0                                                      
         BNE   SETPDD                                                           
         CLI   AGYRECAP,0                                                       
         BE    PDML02                                                           
         CLI   RCSUBPRG,5                                                       
         BE    PDML02                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,5                                                       
PDML02   DS    0H                                                               
         MVI   FORCEMID,C'Y'                                                    
         LA    R1,MID1                                                          
         CLI   PLCNTR,0                                                         
         BE    *+8                                                              
         LA    R1,132(R1)                                                       
         CLI   MYBUFIO+4,X'FF'                                                  
         BNE   PDML10                                                           
         MVC   0(22,R1),=C'******ALL BRANDS******'                              
         B     PDML20                                                           
*                                                                               
PDML10   ST    R1,SAVER1                                                        
         LA    R6,MYBUFIO+4                                                     
         GOTO1 AIEEQU,DMCB,(1,(R6)),BYTE                                        
         GOTO1 ANEWDNAM,DMCB,(1,BYTE),WORK                                      
         L     R1,SAVER1                                                        
         MVC   0(7,R1),WORK                                                     
         CLI   MYBUFIO+9,X'FF'                                                  
         BNE   PDML20                                                           
         MVC   8(6,R1),=C'BRANDS'                                               
PDML20   SR    R1,R1                                                            
         IC    R1,PLCNTR                                                        
         LA    R1,1(R1)                                                         
         STC   R1,PLCNTR                                                        
         MVI   FORCEMID,C'Y'                                                    
         BR    R9                                                               
* SET UP BRAND MIDLINES                                                         
BRNDML   CLI   MYBUFIO+9,X'FF'                                                  
         BE    PDML                                                             
         MVI   FORCEMID,C'Y'                                                    
         LA    R1,MID1                                                          
         CLI   PLCNTR,0                                                         
         BE    *+8                                                              
         LA    R1,132(R1)                                                       
         SR    R6,R6                                                            
         IC    R6,MYBUFIO+9                                                     
         BCTR  R6,0                                                             
         MH    R6,PRDBUFLN                                                      
         A     R6,PRDBUFF                                                       
         MVC   0(3,R1),1(R6)                                                    
         MVI   3(R1),C'-'                                                       
         MVC   4(20,R1),4(R6)                                                   
         MVC   24(9,R1),=C'(       )'                                           
         ST    R1,SAVER1           GET DEMO NAME                                
         LA    R7,29(R6)           SET TO SECONDARY DEMO                        
         CLC   PRDBUFLN,=H'56'                                                  
         BE    *+8                                                              
         LA    R7,31(R6)           ****NEW FORMAT****                           
         GOTO1 ANEWDNAM,DMCB,(1,(R7)),WORK                                      
         L     R1,SAVER1                                                        
         MVC   122(7,R1),WORK                                                   
         LA    R1,25(R1)                                                        
         B     PDML10                                                           
         SPACE 2                                                                
* SET MIDLINES FOR SECONDARY DEMO SUMMARYS                                      
SETPDD   L     R4,APDTCNT          GET PRIMARY DEMO LIST                        
         L     R4,0(R4)                                                         
         L     R5,APDTAB                                                        
         GOTO1 BINSRCH,DMCB,(0,MYBUFIO+4),(R5),(R4),16,(0,1),100                
         CLI   0(R1),1                                                          
         BNE   *+6                                                              
         DC    H'0'                PRIMARY DEMO IS NOT IN TABLE                 
*                                   SHOULD HAVE BEEN PUT THERE                  
*                                   BY BLDPDEM                                  
         GOTO1 =V(PDMLINE),DUB,(RA),PSLIST                                      
         MVC   PRIMDEM,MYBUFIO+4                                                
         MVC   PBCDE1,MYBUFIO                                                   
         BR    R9                                                               
         EJECT                                                                  
EXIT     XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
         DS    0H                                                               
         USING *,RF                                                             
         DROP  R2                                                               
MYHEAD   NTR1  BASE=SPM7RB                                                      
         LM    RA,RC,SPM7RA                                                     
         L     R2,SPM7R2                                                        
         USING SPM702+4096,R2                                                   
         DROP  RF                                                               
         CLI   SPOTPROF+12,C'Y'                                                 
         BNE   *+10                                                             
         MVC   H8+50(18),=C'***TAX EXCLUDED***'                                 
         CLI   QFILTER,C'F'        FILM NUMBER FILTER FOR COKE                  
         BNE   MYHEAD2                                                          
         MVC   H8(8),=C'**FILM**'                                               
         MVC   H8+9(1),QFILTER+1                                                
*                                                                               
MYHEAD2  CLI   RPTNUM,1                                                         
         BE    MYHEAD3                                                          
         MVC   H11+21(6),HCAP1     DETAIL LINES                                 
         MVC   H11+47(9),HCAP2                                                  
         MVC   H11+80(21),HCAP3                                                 
         B     MYHEAD4                                                          
*                                                                               
MYHEAD3  DS    0H                                                               
         MVC   H10+21(6),HCAP1     DETAIL LINES                                 
         MVC   H10+54(9),HCAP2                                                  
         MVC   H10+93(21),HCAP3                                                 
         B     MYHEAD4                                                          
*                                                                               
MYHEAD4  DS    0H                                                               
*                                                                               
         CLI   QOPT6,C'Y'          TEST REPORT LOCKED GOALS                     
         BNE   MYHEADX                                                          
*                                                                               
         CLC   H11+22(4),=C'GOAL'                                               
         BNE   *+10                                                             
         MVC   H11+22(4),=C'LKGL'                                               
*                                                                               
MYHEADX  XIT1                                                                   
         EJECT                                                                  
         DS    0D                                                               
         USING *,RF                                                             
         DROP  R2                                                               
RFMTBUFF NTR1  BASE=SPM7RB                                                      
         LM    RA,RC,SPM7RA                                                     
         L     R2,SPM7R2                                                        
         USING SPM702+4096,R2                                                   
         DROP  RF                                                               
         L     R3,0(R1)            A(INPUT RECORD)                              
         L     RF,4(R1)            A(BUFFALOC)                                  
         USING BUFFALOD,RF                                                      
         CLI   5(R3),0                                                          
         BE    RFMTBUFX                                                         
         A     R3,BUFFLKEY         SET TO DATA                                  
         LA    R4,4                                                             
RFMTBUF1 OC    8(8,R3),8(R3)       CHECK FOR DEMO                               
         BNZ   *+10                                                             
         XC    0(8,R3),0(R3)        NO DEMO-ZERO DOLLARS                        
         CLI   12(R3),X'FF'                                                     
         BNE   *+10                                                             
         XC    12(4,R3),12(R3)                                                  
         LA    R3,16(R3)                                                        
         BCT   R4,RFMTBUF1                                                      
RFMTBUFX L     R3,0(R1)                                                         
         CLI   10(R3),X'FF'        TOTAL LINE                                   
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
         XC    21(64,R3),21(R3)                                                 
         B     BFHEXIT                                                          
         SPACE 2                                                                
BFHOOKA  DS    0H                                                               
         CLI   10(R3),X'FF'                                                     
         BNE   BFHEXIT                                                          
         CLI   19(R3),X'FF'                                                     
         BE    *+8                                                              
         MVI   10(R3),X'FE'                                                     
         CLI   19(R3),X'FE'                                                     
         BNE   *+8                                                              
         MVI   10(R3),X'FD'                                                     
         SPACE 2                                                                
BFHOOK1  L     R9,MEDBUFF                                                       
         CLI   10(R3),X'FF'        OVERALL TOTALS                               
         BE    EXIT                                                             
         CLI   MODE,PROCGOAL                                                    
         BNE   BFHOOK1A                                                         
         CLI   10(R3),X'FD'        IS THIS A SPILL TOTAL                        
         BNE   BFHOOK1A                                                         
         XC    21(64,R3),21(R3)                                                 
         B     BFHEXIT                                                          
         SPACE 2                                                                
BFHOOK1A CLI   SPOTPROF+5,1        DO WE WANT S/0 TOTALS                        
         BE    *+8                                                              
         CLI   SPOTPROF+5,3                                                     
         BE    *+14                                                             
         XC    21(64,R3),21(R3)    RESET TOTALS                                 
         B     BFHEXIT                                                          
         SPACE 2                                                                
         CLI   10(R3),X'FD'        SPILL                                        
         BE    BFHOOK2                                                          
         CLI   MODE,PROCGOAL       ORIG GOALS ARE OK                            
         BE    EXIT                                                             
         CLI   MEDSPILL,C'Y'                                                    
         BNE   BFHEXIT                                                          
         XC    21(64,R3),21(R3)                                                 
         B     BFHEXIT                                                          
         SPACE 2                                                                
BFHOOK2  CLI   MEDSPILL,C'Y'                                                    
         BE    BFHEXIT                                                          
         XC    21(64,R3),21(R3)                                                 
         B     BFHEXIT                                                          
BFHEXIT  XIT1                                                                   
         DROP  R9                                                               
         EJECT                                                                  
* ROUTINE TO GET NEW FORMAT DEMO NAMES                                          
         DS    0D                                                               
         USING *,RF                                                             
         DROP  R2                                                               
NEWDNAM  NTR1  BASE=SPM7RB                                                      
         LM    RA,RC,SPM7RA                                                     
         L     R2,SPM7R2                                                        
         USING SPM702+4096,R2                                                   
         DROP  RF                                                               
         ZIC   R9,0(R1)            NUMBER OF DEMOS                              
         L     R8,4(R1)            OUTPUT AREA                                  
         MH    R9,=H'7'            CLEAR OUTPUT AREA                            
         BCTR  R9,0                                                             
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),SPACES                                                   
         LA    R9,1(R9)                                                         
         CLC   PRDBUFLN,=H'56'     OLD FORMAT DEMOS                             
         BE    OLDNAM                                                           
         L     R7,ADBLOCK                                                       
         USING DBLOCK,R7                                                        
         XC    0(256,R7),0(R7)                                                  
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
         DROP  R7                                                               
         LLC   R9,0(R1)            NUMBER OF DEMOS                              
         L     R7,0(R1)            START OF INPUT                               
         L     R8,4(R1)            START OF OUTPUT                              
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         LA    RF,ENONTDMS                                                      
         ST    RF,DMCB+16                                                       
         LA    RF,EUSRNMS                                                       
         DROP  RF                                                               
         GOTO1 DEMOCON,DMCB,((R9),(R7)),(2,(R8)),(C'S',ADBLOCK),       X        
               (SPOTPROF+9,(RF))                                                
         B     EXIT                                                             
         SPACE 2                                                                
OLDNAM   LLC   R9,0(R1)            NUMBER OF DEMOS                              
         L     R7,0(R1)            START OF INPUT                               
         L     R8,4(R1)            START OF OUTPUT                              
OLDNAM1  LLC   R6,0(R7)                                                         
         LTR   R6,R6                                                            
         BZ    EXIT                                                             
         BCTR  R6,0                                                             
         MHI   R6,7                                                             
         A     R6,DEMTABLE                                                      
         MVC   0(7,R8),0(R6)                                                    
         LA    R7,1(R7)                                                         
         LA    R8,7(R8)                                                         
         BCT   R9,OLDNAM1                                                       
         B     EXIT                                                             
         EJECT                                                                  
* SET PRIMARY DEMO IN MEDPRIMY                                                  
         DROP  R2                                                               
         USING *,RF                                                             
SETPRMY  NTR1  BASE=SPM7RB                                                      
         LM    RA,RC,SPM7RA                                                     
         L     R2,SPM7R2                                                        
         USING SPM702+4096,R2                                                   
         DROP  RF                                                               
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         LLC   RE,MEDBRAND         GET PRODUCT SLOT                             
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF                                                       
         MVC   MEDPRIMY,28(RE)     EXTRACT OLD PRIMARY DEMO                     
         CLC   PRDBUFLN,=H'56'     OLD FORMAT CAN EXIT NOW                      
         BE    SETPRMYX                                                         
         MVC   DEMFULL(3),28(RE)   EXTRACT NEW PRIMARY DEMO                     
         LA    R9,PRMYTAB                                                       
         LA    R1,1                                                             
SETPRMY2 CLC   DEMFULL(3),0(R9)    SAVE NEW PRIMARY DEMO IN TABLE               
         BE    SETPRMY4                                                         
         CLI   1(R9),0                                                          
         BE    SETPRMY3                                                         
         LA    R9,3(R9)                                                         
         LA    R1,1(R1)                                                         
         B     SETPRMY2                                                         
SETPRMY3 MVC   0(3,R9),DEMFULL                                                  
SETPRMY4 STC   R1,MEDPRIMY         EQUATE NEW PRIMARY DEMO TO SLOT              
SETPRMYX B     EXIT                                                             
         EJECT                                                                  
* EQUATE DEMOS FROM INTERNAL TO EXTERNAL FORMAT                                 
         DS    0D                  P1  0     =NUMBER OF 1 BYTE DEMOS            
         DROP  R2                      1-3   =A(1 BYTE DEMOS)                   
         USING *,RF                                                             
IEEQU    NTR1  BASE=SPM7RB         P2  0-3   =A(3 BYTE DEMOS)                   
         LM    RA,RC,SPM7RA                                                     
         L     R2,SPM7R2                                                        
         DROP  RF                                                               
         USING SPM702+4096,R2                                                   
         ZIC   R9,0(R1)            NUMBER OF DEMOS                              
         L     R8,0(R1)                                                         
         L     R7,4(R1)                                                         
         CLC   PRDBUFLN,=H'56'                                                  
         BE    IEOLD                                                            
IEEQU1   ZIC   R1,0(R8)            SET TO SLOT IN EQUATE TABLE                  
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,PRMYTAB(R1)                                                   
         MVC   0(3,R7),0(R1)                                                    
         LA    R8,1(R8)                                                         
         LA    R7,3(R7)                                                         
         BCT   R9,IEEQU1                                                        
         B     EXIT                                                             
         SPACE 2                                                                
IEOLD    MVC   0(1,R7),0(R8)       SET TO OLD DEMOS                             
         LA    R7,1(R7)                                                         
         LA    R8,1(R8)                                                         
         BCT   R9,IEOLD                                                         
         B     EXIT                                                             
         EJECT                                                                  
* EQUATE DEMOS FROM EXTERNAL TO INTERNAL FORMAT                                 
         DS    0D                  P1  0  =NUMBER OF DEMOS                      
         DROP  R2                     1-3 =A(INPUT LIST)                        
         USING *,RF                P2 0-3 =A(OUTPUT LIST)                       
EIEQU    NTR1  BASE=SPM7RB                                                      
         LM    RA,RC,SPM7RA                                                     
         L     R2,SPM7R2                                                        
         DROP  RF                                                               
         USING SPM702+4096,R2                                                   
         ZIC   R9,0(R1)                                                         
         L     R8,0(R1)                                                         
         L     R7,4(R1)                                                         
         CLC   PRDBUFLN,=H'56'                                                  
         BE    EIOLD                                                            
         BCTR  R9,0                                                             
         EX    R9,*+8                                                           
         B     *+10                                                             
         XC    0(0,R7),0(R7)                                                    
         LA    R9,1(R9)                                                         
EIEQU1   LA    R1,1                                                             
         MVC   DEMFULL(3),0(R8)                                                 
         CLI   1(R8),0                                                          
         BE    EXIT                                                             
         LA    R6,PRMYTAB                                                       
EIEQU2   CLC   DEMFULL(3),0(R6)                                                 
         BE    EIEQU4                                                           
         CLI   1(R6),0                                                          
         BE    EIEQU3                                                           
         LA    R6,3(R6)                                                         
         LA    R1,1(R1)                                                         
         B     EIEQU2                                                           
EIEQU3   MVC   0(3,R6),DEMFULL                                                  
EIEQU4   STC   R1,0(R7)                                                         
         LA    R8,3(R8)                                                         
         LA    R7,1(R7)                                                         
         BCT   R9,EIEQU1                                                        
         B     EXIT                                                             
         SPACE 2                                                                
EIOLD    MVC   0(1,R7),0(R8)                                                    
         LA    R7,1(R7)                                                         
         LA    R8,1(R8)                                                         
         BCT   R9,EIOLD                                                         
         B     EXIT                                                             
         SPACE 2                                                                
         LTORG                                                                  
SPM7RA   DC    F'0'                                                             
SPM7RB   DC    F'0'                                                             
SPM7RC   DC    F'0'                                                             
SPM7R2   DC    F'0'                                                             
         EJECT                                                                  
HCAP1    DS    CL6                                                              
HCAP2    DS    CL9                                                              
HCAP3    DS    CL21                                                             
LVCNTRL  DC    F'1'                                                             
         DC    A(2,3,4,5)                                                       
SUPREG   DC    AL1(5,6,7,8,9,10,11,12,29,30,31,32,33,34,35,36,54,57)            
         DC    AL1(58,64,67,68,130,131,136,137,143,146,147,153)                 
         DC    AL1(156,157)                                                     
         DC    AL1(0)                                                           
SUPAGY   DC    AL1(13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)             
         DC    AL1(59,60,61,62)                                                 
         DC    AL1(55,56)                                                       
         DC    AL1(65,66,69,70,71,72)                                           
         DC    AL1(37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52)             
         DC    AL1(132,133,134,135,148,149,150,151,144,145,154)                 
         DC    AL1(155,158,159,160,161)                                         
         DC    AL1(138,139,140,141,0)                                           
SUPBDET  DC    AL1(1,2,3,4,53,129,142,0)                                        
SUPSL    DC    AL1(1,5,9,13,17,21,25,33,37,41,45,49)                            
         DC    AL1(129,130,131,132,133,134,135,136,137,138,139,140)             
         DC    AL1(141,142,143,144,145,146,147,148,149,150,151,152)             
         DC    AL1(153,154,155,156,157,158,159,160,161)                         
         DC    AL1(0)                                                           
SUPDPSL  DC    AL1(1,2,3,5,6,7,9,10,11,13,14,15,17,18,19)                       
         DC    AL1(21,22,23,25,26,27,29,30,31,33,34,35,37,38,39)                
         DC    AL1(41,42,43,45,46,47,49,50,51,0)                                
SUPONEDM DC    AL1(9,10,11,12,17,18,19,20,25,26,27,28)                          
         DC    AL1(131,133,135,0)                                               
SUPSLTOT DC    AL1(129,130,131,132,133,134,135,136,137,138,139,140)             
         DC    AL1(141,142,143,144,145,146,147,148,149,150,151,152)             
         DC    AL1(153,154,155,156,157,158,159,160,161)                         
         DC    AL1(0)                                                           
PLCNTR   DS    C                                                                
AGYRECAP DS    C                                                                
PREVIO   DS    CL10                                                             
CPPCTRL  DC    X'01',AL3(0),X'04',AL3(0)      04 PHASE                          
         DC    X'05',AL3(0),X'08',AL3(0)                                        
         DC    X'0D',AL3(0),X'10',AL3(0)                                        
         DC    X'15',AL3(0),X'18',AL3(0)                                        
         DC    AL1(1),AL3(0),AL1(53),AL3(0)                                     
         DC    AL1(5),AL3(0),AL1(54),AL3(0)                                     
         DC    X'00'                                                            
*                                                                               
CPPCTRL1 DC    X'01',AL3(0),X'04',AL3(0)      40 PHASE                          
         DC    X'05',AL3(0),X'08',AL3(0)                                        
         DC    X'0D',AL3(0),X'10',AL3(0)                                        
         DC    X'15',AL3(0),X'18',AL3(0)                                        
         DC    AL1(1),AL3(0),AL1(53),AL3(0)                                     
         DC    AL1(5),AL3(0),AL1(54),AL3(0)                                     
         DC    X'00'                                                            
*                                                                               
CPPSW    DC    X'00'               CROSS DAYPART CPP SW                         
         EJECT                                                                  
SAVNUMWK DS    XL4                                                              
PRMYTAB  DS    CL150                                                            
PSLIST   DS    CL150               PRODUCT SPOT LENGTH LIST                     
ORIGDATE DS    CL12                                                             
MULTISW  DS    C                                                                
POSTWORK DS    CL2                                                              
SVHALF   DS    CL2                                                              
PRTSW    DS    C                                                                
FRSTTOT  DS    C                                                                
CURRLN   DS    C                                                                
SW1      DS    C                   DATA SWITCH                                  
RPTNUM   DS    C                                                                
         DS    0F                                                               
ACTSW    DS    C                   ACTIVITY SWITCH                              
RELO     DS    F                                                                
SAVER1   DS    F                                                                
SAVERF   DS    F                                                                
DEMFULL  DS    F                                                                
AIEEQU   DS    A(0)                                                             
AEIEQU   DS    A(0)                                                             
ANEWDNAM DS    A(0)                                                             
UNIVERSE DS    F                                                                
ACTAREA  DS    F                                                                
WEIGHT   DC    F'1'                                                             
         DS    0F                                                               
SAVPOL   DS    F                                                                
SAVPOLA  DS    F                                                                
APDTAB   DS    F                                                                
APDTCNT  DS    F                                                                
VSETPDEM DC    F'0'                                                             
VMDADDWT DS    F                                                                
VSETBUF  DS    F                                                                
VBLDPDEM DS    F                                                                
REPCALOV DS    F                                                                
ASPM701  DS    F                                                                
ASPM704  DS    F                                                                
ASPM7041 DS    F                                                                
ASPM710  DS    F                                                                
ASPM740  DS    F                                                                
ASPM7401 DS    F                                                                
SAVPOLND DS    CL12                                                             
HEADSW   DS    C                                                                
PBCDE1   DS    C                                                                
PRIMDEM  DS    CL2                                                              
SW2      DS    C                                                                
WTSW     DS    C                                                                
WTLIST   DS    CL4                                                              
VXTRMEDT DC    F'0'                                                             
SVMEDTAB DC    F'0'                                                             
SUBPROG  DC    C'SPM7041 '                                                      
SAVE9    DS    F                                                                
AGYLIST  DS    F                                                                
RTGSW    DS    C                   RATING SWITCH                                
BUFCDE   DS    C                   BUFFALO CODE                                 
LCODE    DS    C                   LEVEL CODE                                   
PBUFFCDE DS    C                                                                
SVMAXLIN DS    C                                                                
STACTSW  DS    C                                                                
DNAME1   DS    CL7                                                              
DNAME2   DS    CL7                                                              
DNAME3   DS    CL7                                                              
DNAME4   DS    CL7                                                              
SPLPRINT DS    C                                                                
SPBUFMKT DS    CL240                                                            
MYBUFIO  DS    CL400                                                            
VAGYLIST CSECT                                                                  
         DS    CL360                                                            
XTRMEDT  CSECT                                                                  
         DS    6000C                                                            
SPM710C  CSECT                                                                  
         DS    16000C                                                           
SPM740C  CSECT                                                                  
         DS    16000C                                                           
SPM7401C CSECT                                                                  
         DS    16000C                                                           
         EJECT                                                                  
         BUFF  LINES=1,ROWS=5,COLUMNS=16,FLAVOR=BINARY,KEYLIST=(21,A)           
         EJECT                                                                  
RQFRSTC  CSECT                                                                  
         NMOD1 0,M7RQFRST                                                       
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R2,4(R1)                                                         
         USING HCAP1,R2                                                         
         MVC   MAXLINES,SVMAXLIN                                                
         CLI   FOOT1,C' '                                                       
         BE    M2NOFT                                                           
         ZIC   R0,MAXLINES                                                      
         SH    R0,=H'3'                                                         
         STC   R0,MAXLINES                                                      
         MVC   FOOT1(132),SPACES                                                
M2NOFT   DS    0H                                                               
         MVI   ESTSW,C'N'          NEW REQUEST                                  
         MVC   RQDPOVRD,QDPTMENU   OVERRIDE DAYPART MENU                        
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
EFRSTC   CSECT                                                                  
         NMOD1 0,M7EFRST                                                        
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R2,4(R1)                                                         
         USING HCAP1,R2                                                         
         CLC   QPROG,=C'MJ'                                                     
         BE    BLDPRF                                                           
         CLC   QPROG,=C'Q7'                                                     
         BNE   GPROFX                                                           
BLDPRF   MVC   WORK(12),=CL12'S000'   READ MS PROFILE                           
         MVC   WORK+4(3),SVAGY                                                  
         MVC   WORK+7(3),QCLT                                                   
         MVC   WORK+2(2),=C'M7'                                                 
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         DROP  R6                                                               
         GOTO1 GETPROF,DMCB,WORK,PROGPROF,DATAMGR                               
GPROFX   MVI   ESTSW,C'Y'          LOCK MEDBLOCK DATES FOR REQUEST              
         MVC   RQSTAFLT(1),QAFFIL     AFFILATE FILTER                           
         MVC   RQPRGTYP,QPRGTYPE   PROGRAM TYPE FILTER                          
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
         SPACE 2                                                                
         MVC   RPTNUM,PROGPROF+8                                                
*                                                                               
         CLI   QCOMPARE,C' '                                                    
         BNE   *+10                                                             
         MVC   QCOMPARE,PROGPROF   SET DATA COMPARE                             
         CLI   QDPTDET,C' '                                                     
         BNE   *+10                                                             
         MVC   QDPTDET,PROGPROF+1  SET DAYPART CONTROL                          
         MVI   CPPSW,0                                                          
         CLI   PROGPROF+2,C'Y'                                                  
         BNE   *+8                                                              
         MVI   CPPSW,1                                                          
         CLI   QOPT1,C' '                                                       
         BNE   *+10                                                             
         MVC   QOPT1,PROGPROF+3                                                 
         MVI   AGYRECAP,0                                                       
         CLI   PROGPROF+4,C'Y'                                                  
         BNE   *+8                                                              
         MVI   AGYRECAP,0                                                       
         XC    HCAP1,HCAP1                                                      
         XC    HCAP2,HCAP2                                                      
         XC    HCAP3,HCAP3                                                      
         MVC   HCAP1,=C' GOAL '                                                 
         CLI   QOPT6,C'Y'                                                       
         BNE   *+10                                                             
         MVC   HCAP1,=C' LKGL '                                                 
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
         MVC   HCAP3,=C'AFFIDAVIT ACHIEVEMENT'                                  
         SPACE 2                                                                
         L     RF,ADAGY                                                         
         USING AGYHDR,RF                                                        
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         CLI   CEXTRA+5,C'D'       US AGENCY WITH SPILL                         
         BE    *+8                                                              
         CLI   CEXTRA+5,C'Y'       US AGENCY WITH SPILL                         
         BE    *+8                                                              
         CLI   AGYPROF+7,C'C'      NO SPILL IF US AGENCY                        
         BE    *+8                                                              
         MVI   SPOTPROF+5,0                                                     
         DROP  RF                                                               
         DROP  RE                                                               
*                                                                               
         CLC   QPROG,=C'M7'                                                     
         BNE   LDRBYP                                                           
         MVC   SPECS,ASPM701                                                    
         MVC   MEDTABLE,ASPM704                                                 
         MVC   VXTRMEDT,ASPM7041                                                
         CLI   RPTNUM,1                                                         
         BNE   LDRBYP                                                           
         MVC   SPECS,ASPM710                                                    
         MVC   MEDTABLE,ASPM740                                                 
         MVC   VXTRMEDT,ASPM7401                                                
*                                                                               
LDRBYP   MVC   SVMEDTAB,MEDTABLE   SET FOR POST/REPORT SWITCHING                
         CLI   QOPT5,C' '          SET SPILL REPORTING OPTIONS                  
         BE    *+10                                                             
         MVC   PROGPROF+6(1),QOPT5                                              
         CLI   PROGPROF+6,C'N'                                                  
         BNE   *+8                                                              
         MVI   PROGPROF+6,0                                                     
         CLI   PROGPROF+6,0                                                     
         BE    *+10                                                             
         MVC   SPOTPROF+5(1),PROGPROF+6                                         
         NI    SPOTPROF+5,X'0F'                                                 
         SPACE 2                                                                
         MVI   AGYRECAP,0                                                       
*                                                                               
         GOTO1 MEDCLEAR,DMCB,MEDTABLE                                           
         MVC   MEDTABLE,VXTRMEDT   CLEAR SPOT LENGTH TABLES                     
         GOTO1 MEDCLEAR,DMCB,MEDTABLE                                           
         MVC   MEDTABLE,SVMEDTAB                                                
*                                                                               
M21A     LA    R8,SUPAGY           SUPPRESS AGENCY RECAP                        
         BAS   R9,SUPRPTS                                                       
*                                                                               
M21B     CLI   QOPT1,C'Y'          BRAND DETAIL WANTED                          
         BE    *+12                                                             
         LA    R8,SUPBDET                                                       
         BAS   R9,SUPRPTS                                                       
*                                                                               
         MVC   PAGE,=H'1'                                                       
         GOTO1 VSETBUF,DMCB,(RA),LCODE,LVCNTRL                                  
         L     R9,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',(R9)                                      
*                                                                               
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
         GOTO1 VBLDPDEM,DMCB,(RA),PSLIST                                        
*                                                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         CLC   QCLT,=C'ALL'        ALL CLIENT REQ                               
         BNE   *+8                 STANDARDIZE THE DATE FORMULA                 
         MVI   SPOTPROF+2,0                                                     
         MVC   MEDEXTAX,SPOTPROF+12                                             
*                                                                               
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDNUMMO,=F'0'                                                   
         DROP  RE                                                               
         GOTO1 MEDDATE,DMCB,(RA),0,0,1                                          
         SPACE 2                                                                
* SET UP CROSS DAYPART CPP/M OPTIONS                                            
         L     RE,MEDTABLE                                                      
         LA    R4,CPPCTRL                                                       
         CLI   RPTNUM,1                                                         
         BNE   SETCPP                                                           
         LA    R4,CPPCTRL1                                                      
SETCPP   SR    R5,R5                                                            
         IC    R5,4(R4)            GET CONDITIONAL REPORT NUMBER                
         BCTR  R5,0                                                             
         MH    R5,=H'4'                                                         
         LA    R5,0(R5,RE)         POINT TO CONDITIONAL REPORT                  
         L     R6,0(R5)            POINT TO CONDITIONAL ROW DEF.                
         L     R7,4(R4)            GET NORMAL COLUMN DEF.                       
         CLI   CPPSW,0                                                          
         BE    *+8                                                              
         L     R7,0(R4)            GET CPP COL DEF.                             
         LA    R7,0(R7)            CLEAR CONTROL BYTE                           
         ST    R7,4(R6)            SET COL DEF ADDRESS                          
         LA    R4,8(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   SETCPP                                                           
         SPACE 2                                                                
* CHECK FOR ONLY ONE PRIMARY DEMO - OLD FORMAT                                  
         CLC   PRDBUFLN,=H'56'                                                  
         BNE   NDM21C0                                                          
         LA    RE,220                                                           
         L     RF,PRDBUFF                                                       
         MVI   BYTE,0                                                           
M21C     CLI   0(RF),0                                                          
         BE    M21C2                                                            
         CLI   BYTE,0                                                           
         BE    M21C1                                                            
         CLC   28(1,RF),BYTE                                                    
         BNE   M21C3                                                            
M21C1    MVC   BYTE,28(RF)                                                      
M21C2    AH    RF,PRDBUFLN                                                      
         BCT   RE,M21C                                                          
         LA    R8,SUPONEDM         SUPPRESS CLIENT REPORTS                      
         BAS   R9,SUPRPTS                                                       
         B     M21C3                                                            
         SPACE 2                                                                
* CHECK FOR ONLY ONE PRIMARY DEMO - NEW FORMAT                                  
NDM21C0  LA    RE,220                                                           
         L     RF,PRDBUFF                                                       
         XC    FULL,FULL                                                        
NDM21C   CLI   1(RF),0             NEXT PRODUCT IF SLOT NOT ACTIVE              
         BE    NDM21C2                                                          
         CLI   FULL+1,0            SAVE PRIMARY ON FIRST PRODUCT                
         BE    NDM21C1                                                          
         CLC   28(3,RF),FULL       ALLOW CLIENT REPORTS IF DIFFERENT            
         BNE   M21C3               PRIMARY DEMOS                                
NDM21C1  MVC   FULL(3),28(RF)                                                   
NDM21C2  AH    RF,PRDBUFLN                                                      
         BCT   RE,NDM21C                                                        
         LA    R8,SUPONEDM         SUPPRESS CLIENT REPORTS BECAUSE              
         BAS   R9,SUPRPTS          THEY DUPLICATE PRIMARY DEMO REPORTS          
         B     M21C3                                                            
M21C3    DS    0H                                                               
*                                                                               
*                                                                               
* DAYPART SPOT LENGTH SUPPRESSION ROUTINES                                      
*                                                                               
CHKSL    CLI   QDPTDET,C'B'        SUPPRESS SPOT LENGTH                         
         BE    *+12                                                             
         CLI   QDPTDET,C'C'                                                     
         BNE   CHKDP                                                            
         LA    R8,SUPSL                                                         
         BAS   R9,SUPRPTS                                                       
*                                                                               
CHKDP    CLI   QDPTDET,C'C'        SUPPRESS DAYPART                             
         BNE   CHKSLTOT                                                         
         LA    R8,SUPDPSL                                                       
         BAS   R9,SUPRPTS                                                       
*                                                                               
CHKSLTOT CLI   PROGPROF+5,C'Y'                                                  
         BE    CHKMJ                                                            
         LA    R8,SUPSLTOT                                                      
         BAS   R9,SUPRPTS                                                       
*                                                                               
CHKMJ    CLC   QPROG,=C'MJ'                                                     
         BNE   M21EX                                                            
         LA    R8,SUPSLTOT         SUPPRESS SPOT LENGTH TOTALS                  
         BAS   R9,SUPRPTS                                                       
         LA    R8,SUPONEDM         SUPPRESS CLIENT REPORTS                      
         BAS   R9,SUPRPTS                                                       
         LA    R8,SUPREG           SUPPRESS PRIMARY/SECONDARY                   
         BAS   R9,SUPRPTS                                                       
M21EX    XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
* SUPPRESS REPORT - A(SUPPRESSION TABLE) IN R8                                  
SUPRPTS  L     RE,MEDTABLE                                                      
         SR    RF,RF                                                            
         MVC   BYTE,0(R8)                                                       
         TM    BYTE,X'80'                                                       
         BZ    *+8                                                              
         L     RE,VXTRMEDT         SET TO SPOT LENGTH TABLE                     
         NI    BYTE,X'7F'          CLEAR SPOT LENGTH INDICATOR                  
         ZIC   RF,BYTE                                                          
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
BLDPDEM  CSECT                     BUILD PRIMARY DEMO LISTS                     
         NMOD1 0,BLDPDEM                                                        
         L     RA,0(R1)                                                         
         ST    R1,SAVE1                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R2,4(R1)                                                         
         USING PSLIST,R2                                                        
         LA    RE,PDTABCNT                                                      
         ST    RE,APDTCNT                                                       
         LA    RE,PDTAB                                                         
         ST    RE,APDTAB                                                        
         XC    PDTABCNT,PDTABCNT                                                
         LA    R7,219                                                           
         L     R6,PRDBUFF                                                       
BLDPDM1  CLI   0(R6),0                                                          
         BE    BLDPDM4                                                          
         LA    RF,28(R6)           EQUATE DEMO NUMBERS                          
         GOTO1 AEIEQU,DMCB,(2,(RF)),FULL                                        
         XC    WORK,WORK                                                        
         MVC   WORK(2),FULL                                                     
         L     R4,PDTABCNT                                                      
         GOTO1 BINSRCH,DMCB,(X'01',WORK),PDTAB,(R4),16,(0,1),100                
         MVC   PDTABCNT,DMCB+8                                                  
         CLI   0(R1),1                                                          
         BE    BLDPDM4             RECORD INSERTED                              
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                PRIMARY DEMO TABLE IS FULL                   
         LA    R8,FULL+1           SET PRIMARY DEMOS                            
         LA    R9,3                                                             
         LA    R9,1                ONE SECONADRY DEMO                           
         L     R4,0(R1)                                                         
BLDPDM2  CLI   1(R4),0             DEMO NOT IN LIST                             
         BE    BLDPDM3                                                          
         CLC   0(1,R8),1(R4)       ALREADY THERE                                
         BE    BLDPDM3                                                          
         LA    R4,1(R4)            TRY NEXT                                     
         B     BLDPDM2                                                          
BLDPDM3  MVC   1(1,R4),0(R8)       INSERT DEMO INTO LIST                        
         LA    R8,1(R8)            NEXT DEMO                                    
         L     R4,0(R1)                                                         
         BCT   R9,BLDPDM2                                                       
         SPACE 2                                                                
BLDPDM4  AH    R6,PRDBUFLN         GET NEXT PRODUCT                             
         BCT   R7,BLDPDM1                                                       
         XMOD1 1                                                                
         LTORG                                                                  
SAVE1    DC    F'0'                                                             
PDTABCNT DC    F'0'                                                             
PDTAB    DS    1600C                                                            
         DROP  R2                                                               
         EJECT                                                                  
SETPDEM  CSECT                     SET PRIMARY DEMO IN TOTAL AREA               
         NMOD1 0,SETPDEM                                                        
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R9,MEDBUFF                                                       
         USING MEDBLOCK,R9                                                      
         L     R3,4(R1)                                                         
         USING PSLIST,R3                                                        
         ZIC   RE,MEDBRAND                                                      
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF                                                       
         LA    R6,28(RE)                                                        
         XC    FULL,FULL                                                        
         GOTO1 AEIEQU,DMCB,(4,(R6)),FULL                                        
         L     R5,APDTCNT                                                       
         L     R4,0(R5)                                                         
         L     R5,APDTAB                                                        
         GOTO1 BINSRCH,DMCB,(X'00',FULL),(R5),(R4),16,(0,1),100                 
         CLI   0(R1),1                                                          
         BNE   *+6                                                              
         DC    H'0'                PRIMARY DEMO NOT IN TABLE                    
*                                   ERROR - SHOULD HAVE BEEN PUT THERE          
*                                           BY BLDPDEM                          
*       IF YOU DIE HERE IT'S BECAUSE PRODUCT IN MEDBRAND IS MISSING             
*       IT'S ESTIMATE HEADER                                                    
*                                                                               
         L     R4,0(R1)                                                         
         MVC   WORK,1(R4)          SAVE DEMOS                                   
         LA    R4,FULL+1                                                        
         LA    R5,WORK                                                          
         LA    R6,3                                                             
         LA    R6,1                                                             
         LA    R7,DUB                                                           
         XC    DUB,DUB                                                          
SETPDEM1 LA    R8,1                                                             
SETPDEM2 CLC   0(1,R4),0(R5)                                                    
         BE    SETPDEM3                                                         
         CLI   0(R4),0                                                          
         BE    SETPDM3A                                                         
         LA    R8,1(R8)                                                         
         LA    R5,1(R5)                                                         
         CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                DEMO MISSING                                 
         B     SETPDEM2                                                         
SETPDEM3 STC   R8,0(R7)            STORE DEMO SLOT                              
         LA    R7,1(R7)                                                         
         LA    R4,1(R4)                                                         
         LA    R5,WORK                                                          
         BCT   R6,SETPDEM1                                                      
SETPDM3A LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         A     R4,MEDLCHNK                                                      
         ST    R4,FULL                                                          
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         MVC   HLDDEM,MEDBY2       GET SECONDARY DEMOS                          
         MVC   HLDDOL,MEDBYD                                                    
         LA    RF,MEDTOTAL                                                      
         MVC   4(4,RF),FULL                                                     
         L     R4,4(RF)                                                         
         LA    R7,HLDDEM                                                        
         LR    RE,R4                                                            
         LA    RF,200                                                           
         XCEF                                                                   
         LA    RE,MEDBY1                                                        
         LA    RF,DUB                                                           
         MVC   MEDBYD(8),HLDDOL                                                 
SETPDEM4 CLI   0(RF),0                                                          
         BE    SETPDX                                                           
         ZIC   R6,0(RF)            GET SLOT NUMBER                              
         BCTR  R6,0                                                             
         SLA   R6,3                X 8                                          
         L     R5,0(R7)                                                         
         A     R5,0(RE,R6)         ACCUMULATE DEMOS                             
         ST    R5,0(RE,R6)                                                      
         L     R5,4(R7)                                                         
         A     R5,4(RE,R6)                                                      
         ST    R5,4(RE,R6)                                                      
         LTR   R5,R5                                                            
         BNZ   *+12                                                             
         L     R5,=F'-1'                                                        
         ST    R5,4(RE,R6)                                                      
         LA    R7,8(R7)                                                         
         LA    RF,1(RF)                                                         
         B     SETPDEM4                                                         
SETPDX   XMOD1 1                                                                
         LTORG                                                                  
HLDDEM   DS    CL32                                                             
HLDDOL   DS    CL8                                                              
         DROP  R3                                                               
         EJECT                                                                  
PDMLINE  CSECT                                                                  
         NMOD1 0,PDMLINE                                                        
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R2,4(R1)                                                         
         USING PSLIST,R2                                                        
         MVI   P2,0                                                             
         MVI   FORCEHED,C'N'                                                    
         ZIC   RE,LINE                                                          
         AH    RE,=H'20'                                                        
         STC   RE,ALLOWLIN                                                      
         MVC   SVDMCB,DMCB                                                      
         CLC   ALLOWLIN,MAXLINES                                                
         BL    PDMLA                                                            
         MVI   FORCEHED,C'Y'                                                    
*        GOTO1 REPORT                                                           
         B     *+12                                                             
PDMLA    CLI   PRIMDEM+1,0                                                      
         BNE   PDML1                                                            
         ZIC   R6,PRIMDEM                                                       
         BAS   R9,PDMGTN           GET PRIMARY DEMO NAME                        
         MVC   SECHEAD+4(7),0(RE)                                               
         MVC   P2(38),SECHEAD                                                   
         MVI   P2,C' '                                                          
         CLI   PRIMDEM+1,0                                                      
         BE    PDML1                                                            
         MVC   P2+39(9),=C'CONTINUED'                                           
         SPACE 2                                                                
PDML1    LA    R5,P2             SET MIDLINE                                    
         CLI   P2,0                                                             
         BE    *+8                                                              
         LA    R5,P3                                                            
         LR    RE,R5               FORCE A SPACE                                
         AH    RE,=H'264'                                                       
         MVI   0(RE),0                                                          
         L     RE,SVDMCB           RESTORE PRIMARY TABLE SLOT                   
         ZIC   RF,MYBUFIO+5        GET DEMOS FOR THIS LINE                      
         BCTR  RF,0                                                             
         SLL   RF,2                X4                                           
         LA    RE,1(RE,RF)                                                      
         MVC   FULL,0(RE)                                                       
         LA    RF,WTLIST                                                        
         XC    WTLIST,WTLIST                                                    
         LA    R5,12(R5)                                                        
         LA    R7,FULL                                                          
         LA    R0,4                                                             
PDML2    CLI   0(R7),0                                                          
         BE    PDMLX                                                            
         ZIC   R6,0(R7)                                                         
         MVI   FORCEMID,C'Y'                                                    
         ST    RF,SAVERF                                                        
         BAS   R9,PDMGTN                                                        
         L     RF,SAVERF                                                        
         MVC   0(22,R5),DASH2                                                   
         MVC   7(7,R5),0(RE)                                                    
         MVC   133(4,R5),=C'DEMO'                                               
         MVC   140(7,R5),=C'DOLLARS'                                            
         MVC   150(3,R5),=C'CPM'                                                
         CLI   0(RE),C'E'                                                       
         BE    *+8                                                              
         CLI   0(RE),C'R'                                                       
         BNE   *+10                                                             
         MVC   150(3,R5),=C'CPP'                                                
         MVC   0(1,RF),0(RE)                                                    
         CLI   SPOTPROF+1,C'D'                                                  
         BNE   *+8                                                              
         MVI   0(RF),C'R'                                                       
         LA    RF,1(RF)                                                         
         LA    R7,1(R7)                                                         
         LA    R5,25(R5)                                                        
         BCT   R0,PDML2                                                         
PDMLX    MVI   P1,0                                                             
         GOTO1 REPORT                                                           
         MVI   ALLOWLIN,1                                                       
         XMOD1 1                                                                
         SPACE 2                                                                
PDMGTN   STC   R6,BYTE             GET DEMO NAMES                               
         GOTO1 AIEEQU,DMCB,(1,BYTE),DUB                                         
         GOTO1 ANEWDNAM,DMCB,(1,DUB),WORK                                       
         LA    RE,WORK                                                          
         BR    R9                                                               
         DROP  R2                                                               
         LTORG                                                                  
SVDMCB   DS    F                                                                
DASH2    DC    30C'-'                                                           
SECHEAD  DC    C'****        SECONDARY DEMO SUMMARY****'                        
         EJECT                                                                  
UNWGHT   CSECT                                                                  
         NMOD1 0,UNWGHT                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R2,4(R1)                                                         
         USING PSLIST,R2                                                        
         LA    RE,MYBUFIO                                                       
         USING SPM7BUF,RE                                                       
         LA    R7,BDS1D                                                         
         LA    R6,WTLIST                                                        
UNWGHT2  CLI   0(R6),0                                                          
         BE    UNWGHTX                                                          
         CLI   0(R6),C'R'                                                       
         BE    UNWGHT4                                                          
UNWGHT3  LA    R7,16(R7)                                                        
         LA    R6,1(R6)                                                         
         B     UNWGHT2                                                          
         SPACE 2                                                                
UNWGHT4  CLI   SPOTPROF+1,C'N'                                                  
         BE    UNWGHT3                                                          
         OC    SPWEIGHT,SPWEIGHT                                                
         BNZ   UNWGHT4A                                                         
         XC    0(8,R7),0(R7)                                                    
         B     UNWGHT3                                                          
UNWGHT4A DS    0H                                                               
         L     RE,0(R7)                                                         
         SRDA  RE,32                                                            
         SLDA  RE,1                                                             
         D     RE,SPWEIGHT                                                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R7)                                                         
         L     RE,4(R7)                                                         
         SRDA  RE,32                                                            
         SLDA  RE,1                                                             
         D     RE,SPWEIGHT                                                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,4(R7)                                                         
         B     UNWGHT3                                                          
UNWGHTX  XMOD1 1                                                                
         LTORG                                                                  
         DROP  R2                                                               
         DROP  RE                                                               
         EJECT                                                                  
SETBUF   CSECT                                                                  
         NMOD1 0,SETBUF                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R4,4(R1)            LOW CODE                                     
         L     R5,8(R1)                                                         
         MVI   0(R4),2             DETERMINE NO. OF LEVELS REQUIRED             
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
         SLL   RE,2                X 4                                          
         LA    RE,0(R5,RE)                                                      
         OI    0(RE),X'80'                                                      
* SET BUFFALO LEVELS                                                            
*                                                                               
         L     R2,BUFFBUFF                                                      
         USING BUFFALOD,R2                                                      
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
         DR    R8,R7                                                            
         ST    R9,BUFFCRMX                                                      
         XMOD1 1                                                                
         EJECT                                                                  
SPM7BUF  DSECT                                                                  
BPKEY    DS    CL21                                                             
BDATA    DS    0C                  PERIOD RECORDS                               
BPGD1    DS    CL4                                                              
BPGD1E   DS    CL4                                                              
BPGDL    DS    CL4                                                              
BPGDLE   DS    CL4                                                              
BPBD1    DS    CL4                                                              
BPBD1E   DS    CL4                                                              
BPBDL    DS    CL4                                                              
BPBDLE   DS    CL4                                                              
BPBSPT   DS    CL4                                                              
BPBD2    DS    CL4                                                              
BPBD2E   DS    CL4                                                              
BPBD3    DS    CL4                                                              
BPBD3E   DS    CL4                                                              
BPBD4    DS    CL4                                                              
BPBD4E   DS    CL4                                                              
         ORG   BDATA                                                            
BDS1DL   DS    CL4                                                              
BDS1DLE  DS    CL4                                                              
BDS1D    DS    CL4                                                              
BDS1DE   DS    CL4                                                              
BDS2DL   DS    CL4                                                              
BDS2DLE  DS    CL4                                                              
BDS2D    DS    CL4                                                              
BDS2DE   DS    CL4                                                              
BDS3DL   DS    CL4                                                              
BDS3DLE  DS    CL4                                                              
BDS3D    DS    CL4                                                              
BDS3DE   DS    CL4                                                              
BDS4DL   DS    CL4                                                              
BDS4DLE  DS    CL4                                                              
BDS4D    DS    CL4                                                              
BDS4DE   DS    CL4                                                              
         ORG   BDATA                                                            
BM1GD1   DS    CL4                 MONTHLY RECORDS                              
BM1GD1E  DS    CL4                                                              
BM1GDL   DS    CL4                                                              
BM1GDLE  DS    CL4                                                              
BM1BD1   DS    CL4                                                              
BM1BD1E  DS    CL4                                                              
BM1BDL   DS    CL4                                                              
BM1BDLE  DS    CL4                                                              
BM1BSPT  DS    CL4                                                              
BM2GD1   DS    CL4                                                              
BM2GD1E  DS    CL4                                                              
BM2GDL   DS    CL4                                                              
BM2GDLE  DS    CL4                                                              
BM2BD1   DS    CL4                                                              
BM2BD1E  DS    CL4                                                              
BM2BDL   DS    CL4                                                              
BM2BDLE  DS    CL4                                                              
BM2BSPT  DS    CL4                                                              
BM3GD1   DS    CL4                                                              
BM3GD1E  DS    CL4                                                              
BM3GDL   DS    CL4                                                              
BM3GDLE  DS    CL4                                                              
BM3BD1   DS    CL4                                                              
BM3BD1E  DS    CL4                                                              
BM3BDL   DS    CL4                                                              
BM3BDLE  DS    CL4                                                              
BM3BSPT  DS    CL4                                                              
*        PRINT OFF                                                              
* SPREPWORKD                                                                    
       ++INCLUDE SPREPWORKD                                                     
         ORG   QGRP                                                             
QOPT6    DS    C                                                                
         ORG                                                                    
         EJECT                                                                  
* SPREPMODES                                                                    
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
* SPMEDBLOCK                                                                    
       ++INCLUDE SPMEDBLOCK                                                     
         EJECT                                                                  
       ++INCLUDE DDBUFFALOD                                                     
         EJECT                                                                  
* SPGENBUY                                                                      
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
* SPGENGOAL                                                                     
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'085SPREPM702 11/21/19'                                      
         END                                                                    
