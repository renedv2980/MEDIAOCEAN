*          DATA SET SPREPF402  AT LEVEL 088 AS OF 05/01/02                      
*PHASE SPF402T,+0,NOAUTO                                                        
*INCLUDE SPRPFOOT                                                               
*INCLUDE MEDAPRNT                                                               
*INCLUDE REPSPILL                                                               
*INCLUDE MEDFLMBY                                                               
*INCLUDE GETRECAP                                                               
         TITLE 'SPREPF402-COMMERCIAL PERFORMANCE REPORTS'                       
         PRINT NOGEN                                                            
*                                                                               
* MEDIA SUMMARY BUFFALO LEVELS                                                  
*    COLUMN DEFINITION 1  (DETAIL)                                              
*        LEVEL 1 = DETAIL ITEMS                                                 
*                                                                               
********************GOAL ALLOCATION IS AS FOLLOWS********************           
*  1. SORT THE STATION TABLE INTO DATE ORDER                        *           
*  2. CALCULATE PERCENTAGE OF DOLLARS IN EACH CATAGORY FOR STATION  *           
*  3. GET COMMERCIALS FOR EACH STATION/COPY CODE FROM PATTERN RECORD*           
*  4. CALCULATE COST PERCENTAGE OF EACH COMMERCIAL                  *           
*  5. A. MULTIPLY COST PERCENTAGE BY COMMERCIAL PERCENTAGE          *           
*     B. MULTIPLY DOLLARS BY 5A                                     *           
*     C. ADD INTO FILM/DATE/COPY TABLE                              *           
*  6. READ BUFFALO RECORD FOR GOAL/DATE/COPY                        *           
*  7. ALLOCATE POINTS AND DOLLARS BASED ON FILM/DATE/COPY TABLE     *           
*  8. ADD ALLOCATED POINTS AND DOLLARS INTO FILM BUFFALO DETAILS    *           
*********************************************************************           
         EJECT                                                                  
SPF402   CSECT                                                                  
         NMOD1 0,SPF402,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         LA    R6,2048(RB)                                                      
         LA    R6,2048(R6)                                                      
         USING SPF402+4096,R6                                                   
         STM   RA,RC,SPF4RA                                                     
         ST    R6,SPF4R6                                                        
         ST    R5,RELO                                                          
         GOTO1 =V(SPRPFOOT),DMCB,(RA),RR=RELO                                   
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
         CLI   MODE,RUNFRST                                                     
         BNE   M2                                                               
         XC    SPBUFMKT,SPBUFMKT                                                
         MVC   SVMAXLIN,MAXLINES                                                
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         L     RF,=F'1272'                                                      
         XCEF                                                                   
         L     RE,MEDBUFF                                                       
         XC    MEDNUMMO,MEDNUMMO                                                
         MVC   MEDNUMWK,=F'56'                                                  
         MVC   MEDNUMPE,=F'1'                                                   
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         LA    RF,MYHEAD                                                        
         ST    RF,HEADHOOK                                                      
         CLI   MEDEXTAV,C'Y'                                                    
         BE    *+8                                                              
         MVI   MEDEXTDM,4                                                       
         DROP  RE                                                               
         LA    RE,MYBUFIO                                                       
         ST    RE,BUFFIO                                                        
         L     R2,=V(BUFFALOC)                                                  
         A     R2,RELO                                                          
         ST    R2,BUFFBUFF                                                      
         LA    RE,MYBUFIO                                                       
         ST    RE,BUFFIO                                                        
         CLC   4(8,R1),=C'BUFFOPEN'                                             
         BE    M0                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',(R2)                                        
*                                                                               
M0       GOTO1 MEDSEED,DMCB,(RA)   SET UP REPORT TABLES                         
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
M2       CLI   MODE,REQFRST                                                     
         BNE   M20                                                              
         MVI   SPSUPMKT,C'Y'       SUPPRESS MARKET NAME IN HEADS                
         CLC   QPROG,=C'F4'        MARKET REPORT                                
         BNE   *+8                                                              
         MVI   SPSUPMKT,C'N'       ACTIVATE MARKET PRINT                        
         MVC   SVQCMRCL,QCMRCL     SAVE THE COMMERCIAL CODE                     
         MVC   ORIGCML,QCMRCL                                                   
         MVC   QCMRCL(8),SPACES                                                 
         MVC   QDPTDET,QOPT5                                                    
         MVI   QOPT5,C' '                                                       
         LA    RE,FILMTAB                                                       
         L     RF,LNFLMTAB                                                      
         XCEF                                                                   
         L     RE,=A(SUMAREA)                                                   
         L     RF,=F'2600'                                                      
         XCEF                                                                   
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
*                                                                               
         LA    RE,F4TAB1           SET UP FOR F4 REPORT                         
         ST    RE,CURRRPT                                                       
         LA    RE,2(RE)            BUMP PAST KEY LENGTH                         
         LA    R1,0                COUNT THE NUMBER OF KEY ENTRIES              
         CLI   0(RE),X'FF'                                                      
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         LA    RE,4(RE)                                                         
         B     *-16                                                             
         STC   R1,MAXLVL           SAVE THE LEVEL COUNT                         
         B     EXIT                                                             
         EJECT                                                                  
M20      CLI   MODE,CLTFRST                                                     
         BNE   M21                                                              
         CLC   QPROG,=C'F2'                                                     
         B     *+8                                                              
         MVI   SPOTPROF+1,C'N'                                                  
         L     RE,=A(MKTBRK)       CLEAR BREAK TABLE                            
         L     RF,=F'4000'                                                      
         XCEF                                                                   
         MVC   CURRBRK,=A(MKTBRK)                                               
         MVC   BRKNUM,=H'1'        AND BREAK NUMBER                             
*                                                                               
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
         L     RE,=A(CMLEQU)       CLEAR CML EQUATE TABLE                       
         LA    RF,LNCMLEQU                                                      
         XCEF                                                                   
         CLI   ESTSW,C'N'                                                       
         BNE   M141                                                             
         MVI   ESTSW,C'Y'          LOCK MEDBLOCK DATES FOR REQUEST              
         MVC   RQSTAFLT(1),QAFFIL     AFFILATE FILTER                           
         MVC   RQPRGTYP,QPRGTYPE   PROGRAM TYPE FILTER                          
         CLI   SPOTPROF+5,10                                                    
         BL    *+8                                                              
         MVI   SPOTPROF+5,0                                                     
         SPACE 2                                                                
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
         L     RF,ADAGY                                                         
         USING AGYHDRD,RF                                                       
         L     RE,ADCLT            CHECK FOR US AGENCY AND SPILL                
         USING CLTHDR,RE                                                        
         CLI   CEXTRA+5,C'Y'                                                    
         BE    *+8                                                              
         DROP  RE                                                               
         CLI   AGYPROF+7,C'C'                                                   
         BE    *+8                 NO SPILL IF US AGENCY                        
         MVI   SPOTPROF+5,2                                                     
         DROP  RF                                                               
*                                                                               
         GOTO1 MEDCLEAR,DMCB,MEDTABLE                                           
*                                                                               
         MVC   PAGE,=H'1'                                                       
*                                                                               
         GOTO1 MEDDATE,DMCB,(RA)                                                
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDEXTAX,SPOTPROF+12                                             
         DROP  RE                                                               
         SPACE 2                                                                
M21EX    GOTO1 =V(SETBUF),DMCB,(RA),LCODE,LVCNTRL,RR=RELO                       
         MVC   HICODE,LCODE                                                     
         L     R2,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',(R2)                                      
         B     M141                                                             
         EJECT                                                                  
M3       CLI   MODE,PROCBUY                                                     
         BNE   M4                                                               
         LA    R1,FLTRSPT                                                       
         ST    R1,SPOTHOOK                                                      
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         LR    RF,R5               SET UP SECONDARY POST BYTES                  
         BAS   R9,SETPOST                                                       
         MVC   COPY,BDDAYPT        SET COPY CODE                                
         CLI   QOPT5+1,C'Y'        IGNORE DAYPARTS                              
         BNE   *+8                                                              
         MVI   COPY,C'X'                                                        
         CLI   QPRGTYPE,C' '       PROGRAM TYPE FILTER                          
         BE    *+14                                                             
         CLC   BDPROGT,QPRGTYPE                                                 
         BNE   EXIT                                                             
         DROP  R5                                                               
         XC    PSLIST,PSLIST                                                    
*                                                                               
*                                                                               
         GOTO1 =V(VMDFLMBY),DMCB,(RA),PSLIST                                    
*                                                                               
         LA    RE,PSLIST                                                        
         CLI   0(RE),0                                                          
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         B     *-12                                                             
         MVC   0(2,RE),=X'FFFF'                                                 
         CLI   KEY+3,X'FF'         POL                                          
         BE    M32                                                              
M31      LA    RE,PSLIST                                                        
M31A     CLC   0(2,RE),=X'FFFF'    CHECK FOR END                                
         BE    M32                                                              
         CLC   0(1,RE),KEY+3       PRODUCT OK                                   
         BNE   *+12                 NO - DELETE                                 
         LA    RE,4(RE)             YES - TRY NEXT                              
         B     M31A                                                             
         XC    0(4,RE),0(RE)                                                    
         LA    RE,4(RE)                                                         
         B     M31A                                                             
*                                                                               
M32      LA    R3,2                SET DEMO TYPE                                
         CLI   QBOOK1,C' '                                                      
         BE    M320                                                             
         CLI   QRERATE,C' '                                                     
         BNE   *+8                                                              
         MVI   QRERATE,C'I'                                                     
M320     CLI   QRERATE,C' '                                                     
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
M3231A   LA    R2,4(R2)                                                         
         B     M323                                                             
         L     RE,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,RE                                                      
         MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
         MVC   FILMSEQ,2(R2)                                                    
* CHECK FOR PURCH VS ACHIEVED                                                   
         CLI   QCOMPARE,C'C'                                                    
         BE    M3ACH                                                            
         CLI   QCOMPARE,C'D'                                                    
         BE    M3ACH                                                            
M323A    MVC   FILMSEQ,2(R2)                                                    
         BAS   RE,GETNFLM                                                       
         CLI   SVQCMRCL,C' '                                                    
         BE    *+14                                                             
         CLC   CANISTER,SVQCMRCL                                                
         BNE   M3231A                                                           
         GOTO1 MEDGETBY,DMCB,(RA),2     ANY ORDERED                             
         BAS   RE,SETPRMY                                                       
         L     RE,MEDBUFF                NO - BYPASS                            
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD                                                
         BZ    M3231A                                                           
         GOTO1 MEDGETBY,DMCB,(RA),(R3)                                          
         BAS   RE,SETPRMY                                                       
         MVC   ACTAREA,4(R1)       SAVE ACTIVE BOOK LIST                        
*                                                                               
M323B    L     RE,MEDBUFF                                                       
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD                                                
         BZ    M3231A                                                           
         DROP  R4                                                               
         SPACE 2                                                                
         CLI   MEDSPILL,C'Y'                                                    
         BE    EXIT                                                             
         SPACE 2                                                                
M323NOSP MVI   ACTSW,1                                                          
         MVI   STACTSW,1                                                        
         GOTO1 MEDMKTWT,DMCB,(RA)                                               
         GOTO1 =V(BSWC),DMCB,(RA)  SUM WEEKLY DOLLARS                           
M324     MVC   WORK(2),=X'6162'    SET BUFFALO CODES                            
         MVC   WORK+2(8),CANISTER                                               
         MVC   WORK+10(2),BRKNUM                                                
*                                                                               
         LA    RE,FILMTAB          SAVE IN FILTER TABLE                         
SVCAN    CLI   8(RE),X'FF'         END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),0             OPEN SLOT                                    
         BE    SVCAN2                                                           
         CLC   0(8,RE),CANISTER    ALREADY THERE                                
         BE    SVCANX              YES - BYPASS                                 
         LA    RE,8(RE)                                                         
         B     SVCAN                                                            
SVCAN2   MVC   0(8,RE),CANISTER                                                 
SVCANX   DC    0H'0'                                                            
*                                                                               
         GOTO1 MEDPOST,DMCB,(RA)                                                
         B     M3231A                                                           
         EJECT                                                                  
* GET PURCHASE AND POST IN GOAL BUCKETS                                         
M3ACH    GOTO1 MEDGETBY,DMCB,(RA),2                                             
         BAS   RE,SETPRMY                                                       
*                                                                               
         L     RE,MEDBUFF                                                       
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD   ANY ACTIVITY                                 
         BZ    M323A               NO - EXIT                                    
* SET GOAL BUCKETS                                                              
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
         BE    M323A                                                            
         MVC   WORK(2),POSTWORK                                                 
         GOTO1 (RF),DMCB,(RA)                                                   
         B     M323A                                                            
         EJECT                                                                  
M4       CLI   MODE,PROCGOAL                                                    
         BNE   M5                                                               
         CLI   QCOMPARE,C'C'       PURCHASED ONLY                               
         BE    EXIT                                                             
         CLI   QCOMPARE,C'D'       PURCHASE ONLY                                
         BE    EXIT                                                             
         CLI   FPCTSW,1            GET FILM PERCENTS                            
         BNE   M41                                                              
         MVC   SVKEY,KEY                                                        
         MVC   SVKEYSV,KEYSAVE                                                  
         GOTO1 =V(FLMPCT),DMCB,(RA)                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         MVC   KEYSAVE,SVKEYSV                                                  
         MVI   FPCTSW,0                                                         
M41      DS    0C                                                               
         LA    RE,KEY                                                           
         USING GOALREC,RE                                                       
         LA    RF,1(RE)            SET UP SECONDARY POST                        
         BAS   R9,SETPOST                                                       
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         MVC   MEDBRAND,GKEYPRD                                                 
         MVC   MEDSPTLN,GKEYSLN                                                 
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
         MVI   ACTSW,1                                                          
M42      GOTO1 MEDMKTWT,DMCB,(RA)                                               
         MVC   WORK(2),=X'6162'    SET BUFFALO CODES                            
         MVC   WORK+2(8),=CL8'**GOAL**'                                         
         MVC   WORK+10(2),BRKNUM                                                
         GOTO1 MEDPOST,DMCB,(RA)                                                
         GOTO1 =V(ALLOCG),DMCB,(RA)                                             
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
M4L      GOTO1 MEDGETLK,DMCB,(RA)                                               
         BAS   RE,SETPRMY                                                       
*                                                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         MVC   MEDGLD,MEDLKD       SET GOAL BUCKETS                             
         MVC   MEDGLDEQ,MEDLKDEQ                                                
         MVC   MEDGL1,MEDLK1                                                    
         MVC   MEDGL1EQ,MEDLK1EQ                                                
         B     M42                                                              
         DROP  RE,R4                                                            
SPF4R6   DC    F'0'                                                             
         EJECT                                                                  
M5       CLI   MODE,MKTLAST                                                     
         BNE   M6                                                               
         CLC   QPROG,=C'F2'        REPORT F2 PRINTS AT END OF REPORT            
         BNE   *+12                OTHERS PRINT NOW                             
         CLI   PASS,1              EXIT IF NO PRINT                             
         BE    EXIT                                                             
         CLI   PASS,3              SUMMATION PASS FOR HIGHER LEVELS             
         BE    M5NOSP                                                           
* SORT INTO FILM NUMBER ORDER                                                   
         LA    R7,0                                                             
         LA    R9,FILMTAB                                                       
         CLI   0(R9),0             COUNT THE FILMS                              
         BE    *+16                                                             
         LA    R7,1(R7)                                                         
         LA    R9,8(R9)                                                         
         B     *-16                                                             
         GOTO1 XSORT,DMCB,(X'00',FILMTAB),(R7),8,8,0                            
* PROCESS BUFFALO RECORDS AND PRINT                                             
         MVI   GSUMFRST,1          SET TO INITIALIZE SUMMARY                    
         L     RE,=A(SUMAREA)                                                   
         L     RF,=F'2600'                                                      
         XCEF                                                                   
         GOTO1 =V(SPRPFOOT),DMCB,(RA),RR=RELO                                   
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         CLC   QPRD,=C'POL'                                                     
         BNE   *+8                                                              
         MVI   MEDBRAND,X'FF'                                                   
         DROP  RE                                                               
         CLI   ACTSW,1                                                          
         BNE   EXIT                                                             
         CLI   PASS,1              COMMERCIALS ARE TO BE BUMPED                 
         BNE   *+12                ELSEWHERE                                    
         LA    R9,FILMTAB          SET TO START OF FILM TABLE                   
         ST    R9,NEXTFLM          AND SAVE ADDRESS                             
         MVI   RCSUBPRG,1                                                       
         CLI   RTGSW,1                                                          
         BE    *+8                                                              
         MVI   RCSUBPRG,2                                                       
         MVI   FRSTTOT,C'Y'                                                     
         CLI   QOPT1,C'D'                                                       
         BE    M53                                                              
         MVC   WEIGHT,SPWEIGHT                                                  
M5NXTFLM LA    RE,MYBUFIO                                                       
         LA    RF,400                                                           
         XCEF                                                                   
         XC    MID1,MID1                                                        
         XC    MID2,MID2                                                        
         L     R9,NEXTFLM                                                       
         CLC   ORIGCML(8),SPACES   COMMERCIAL FILTER ACTIVE                     
         BE    *+14                NO - PROCESS ALL CMLS                        
         CLC   0(8,R9),ORIGCML     YES - FILTER ONLY THIS CML                   
         BNE   M52E                                                             
*                                                                               
         CLC   SVQCMRCL(8),SPACES  COMMERCIAL FILTER ACTIVE                     
         BE    *+14                NO - PROCESS ALL CMLS                        
         CLC   0(8,R9),SVQCMRCL    YES - FILTER ONLY THIS CML                   
         BNE   M52E                                                             
*                                                                               
         MVC   MID2(8),0(R9)                                                    
         CLI   MID2,0                                                           
         BNE   *+10                                                             
         MVC   MID2(9),=C'ALL FILMS'                                            
         CLC   SVQCMRCL(8),SPACES  MARKET REPORT                                
         BE    M5AA                                                             
         XC    MID2,MID2           NO SET UP MARKET NAME                        
         MVC   MID2(4),MKT                                                      
         MVI   MID2+5,C'-'                                                      
         MVC   MID2+7(24),MKTNM                                                 
         CLI   SPOTPROF+1,C'N'                                                  
         BE    M5AA                                                             
         MVC   MID2+32(9),=C'COVERAGE='                                         
         EDIT  SPWEIGHT,(5,MID2+41),2,ALIGN=LEFT                                
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
         MVI   BUFCDE,X'61'                                                     
M5A      MVC   FLTR1+2(1),BUFCDE   SET TO FILTER ON CURRENT CODE                
         MVI   FLTR1+3,X'FF'       SET END OF FILTERS                           
*                                                                               
         L     R9,NEXTFLM                                                       
         CLI   0(R9),0             END OF TABLE DONT FILTER FILMS               
         BE    M5A2                                                             
         MVI   FLTR1+3,11          SET UP FILM FILTER                           
         MVI   FLTR1+4,8                                                        
         MVC   FLTR1+5(8),0(R9)                                                 
         MVI   FLTR1+13,X'FF'                                                   
*                                                                               
         CLC   QPROG,=C'F2'        F2 NEEDS BREAK FILTER                        
         BNE   M5A2                                                             
         L     RE,CURRBRK                                                       
         OC    0(2,RE),0(RE)       ALL MARKET TOTAL                             
         BZ    M5A2                                                             
         MVC   FLTR1+15(2),0(RE)   MOVE IN FILTER VALUE                         
         MVI   FLTR1+13,19         SET UP BREAK FILTER                          
         MVI   FLTR1+14,2                                                       
         MVI   FLTR1+17,X'FF'                                                   
*                                                                               
M5A2     LA    R9,FLTR1                                                         
         L     R3,BUFFBUFF                                                      
         MVI   BSUMFRST,1                                                       
         GOTO1 BUFFSUM,DMCB,BUFFBUFF,(0,11),(R9),GENSUM                         
         CLI   BSUMFRST,1          EOF ON FIRST READ                            
         BE    M52CA                                                            
         XC    PSLIST,PSLIST       SEND A ZERO KEY                              
         LA    R1,PSLIST                                                        
         BAS   RE,GENSUM                                                        
         L     RE,=A(SUMAREA)                                                   
         L     RF,=F'2600'                                                      
         XCEF                                                                   
         B     M52C                                                             
         SPACE 2                                                                
M52B     MVI   PRTSW,1             SET FOR PRINT PASS                           
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
M52E     L     RF,NEXTFLM          SELECT NEXT CML                              
         CLI   PASS,1              BUMP ONLY FOR PASS 1 REPORTS                 
         BNE   EXIT                                                             
         CLI   0(RF),0                                                          
         BE    M53                                                              
         LA    RF,8(RF)                                                         
         ST    RF,NEXTFLM                                                       
         B     M5NXTFLM                                                         
*                                                                               
M53      L     R8,ACTAREA                                                       
         CLC   QPROG,=C'F2'        CANT PRINT ACT TABLE                         
         BE    M5NOSP                                                           
         GOTO1 =V(VMDAPRNT),DMCB,(RA),(R8),1,RR=RELO                            
         SPACE 2                                                                
*        OC    SPBUFMKT,SPBUFMKT   CHECK FOR SPILL                              
*        BZ    M5NOSP                                                           
*        MVC   P1(11),=C'***SPILL***'                                           
*        GOTO1 =V(REPSPILL),DMCB,(RA),(X'02',SPBUFMKT),P1,RR=RELO               
*        GOTO1 REPORT                                                           
*        XC    SPBUFMKT,SPBUFMKT                                                
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
         MVI   DMCB+4,X'22'                                                     
         GOTO1 (RF)                                                             
         MVI   DMCB+4,X'42'                                                     
         GOTO1 (RF)                                                             
         MVI   DMCB+4,X'62'                                                     
         GOTO1 (RF)                                                             
         MVI   ACTSW,0                                                          
         MVI   FORCEMID,C'N'                                                    
         MVI   MID1,C' '                                                        
         MVC   MID1+1(250),MID1                                                 
         B     EXIT                                                             
M6       CLI   MODE,MGR3LAST                                                    
         BNE   M7                                                               
         MVI   BUFCDE,X'61'                                                     
         MVI   LCODE,5                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
M7       CLI   MODE,MGR2LAST                                                    
         BNE   M8                                                               
         MVI   BUFCDE,X'61'                                                     
         MVI   LCODE,4                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
M8       CLI   MODE,MGR1LAST                                                    
         BNE   M9                                                               
         MVI   BUFCDE,X'61'                                                     
         MVI   LCODE,3                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
M9       CLI   MODE,PRDLAST                                                     
         BNE   M10                                                              
         CLC   QPROG,=C'F2'                                                     
         BNE   M9NOR                                                            
         GOTO1 =V(CMLRPT),DMCB,(RA)                                             
         MVI   MODE,PRDLAST                                                     
M9NOR    TM    QMKT,X'F0'                                                       
         BO    EXIT                                                             
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         MVC   MEDBRAND,BPRD                                                    
         MVI   RCSUBPRG,3                                                       
         CLI   RTGSW,1                                                          
         BE    *+8                                                              
         MVI   RCSUBPRG,4                                                       
         MVI   BUFCDE,X'61'                                                     
         MVI   LCODE,2                                                          
         BAS   R9,DOSUM                                                         
         MVI   RCSUBPRG,5                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
M10      CLI   MODE,PGR3LAST                                                    
         BNE   M11                                                              
         MVI   BUFCDE,X'62'                                                     
         MVI   LCODE,5                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
M11      CLI   MODE,PGR2LAST                                                    
         BNE   M12                                                              
         MVI   BUFCDE,X'62'                                                     
         MVI   LCODE,4                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
M12      CLI   MODE,PGR1LAST                                                    
         BNE   M13                                                              
         MVI   BUFCDE,X'62'                                                     
         MVI   LCODE,3                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
M13      CLI   MODE,CLTLAST                                                     
         BNE   M14                                                              
         TM    QMKT,X'F0'                                                       
         BO    EXIT                                                             
         MVI   BUFCDE,X'62'                                                     
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
         MVI   PASS,1                                                           
         CLC   QPROG,=C'F4'        MARKET REPORT                                
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVI   FPCTSW,1            SET TO CALCULATE FILM PERCENTS               
         MVI   ACTSW,0                                                          
         XC    SPBUFMKT,SPBUFMKT                                                
         XC    SWCCNT,SWCCNT                                                    
         L     RE,=V(SWCTABC)                                                   
         L     RF,=F'40000'                                                     
         XCEF                                                                   
         CLC   QPROG,=C'F2'                                                     
         BNE   EXIT                                                             
         SR    RE,RE                                                            
         ICM   RE,3,BRKNUM                                                      
         LA    RE,1(RE)                                                         
         STCM  RE,3,BRKNUM                                                      
         L     RE,CURRBRK          SAVE UP MARKET AND BREAK NUMBERS             
         MVC   0(2,RE),BRKNUM                                                   
         PACK  DUB,MKT                                                          
         CVB   RF,DUB                                                           
         STCM  RF,3,2(RE)                                                       
         LA    RE,4(RE)                                                         
         L     RF,=A(MKTBRKEN)                                                  
         CR    RE,RF               MKTBRK TABLE END                             
         BL    *+6                                                              
         DC    H'0'                                                             
         ST    RE,CURRBRK                                                       
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
         CLC   QPROG,=C'F2'                                                     
         BE    EXIT                                                             
         CLI   ACTSW,1                                                          
         BNE   EXIT                                                             
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
DOSUM    L     R4,BUFFIO           DO SUMMARY REPORTS                           
         MVI   FORCEHED,C'Y'                                                    
         MVI   WTSW,1                                                           
         CLC   HICODE,LCODE                                                     
         BLR   R9                                                               
         ST    R9,SAVE9                                                         
         LA    R9,FILMTAB                                                       
         ST    R9,NEXTFLM                                                       
         MVI   BUFCDE,X'61'                                                     
REDOSUM  L     R3,BUFFBUFF                                                      
         CLI   SPDUPTOT,C'Y'                                                    
         BE    DOSUM3                                                           
         MVC   WEIGHT,SPWEIGHT                                                  
         MVI   SW1,0                                                            
DSNXTFLM LA    RE,MYBUFIO                                                       
         LA    RF,400                                                           
         XCEF                                                                   
         L     R4,BUFFIO                                                        
         XC    0(20,R4),0(R4)                                                   
         XC    MID1,MID1                                                        
         XC    MID2,MID2                                                        
         L     R9,NEXTFLM                                                       
         CLC   SVQCMRCL(8),SPACES  COMMERCIAL FILTER ACTIVE                     
         BE    *+14                NO - PROCESS ALL CMLS                        
         CLC   0(8,R9),SVQCMRCL    YES - FILTER ONLY THIS CML                   
         BNE   DS2D                                                             
         MVC   MID2(8),0(R9)                                                    
         CLI   MID2,0                                                           
         BNE   DSAA                                                             
         MVC   MID2(9),=C'ALL FILMS'                                            
DSAA     DS    0H                                                               
         MVI   FORCEMID,C'Y'                                                    
DSA      MVC   FLTR1+2(1),BUFCDE   SET TO FILTER ON CURRENT CODE                
         MVI   FLTR1+3,X'FF'       SET END OF FILTERS                           
*                                                                               
         L     R9,NEXTFLM                                                       
         CLI   0(R9),0             END OF TABLE DONT FILTER FILMS               
         BE    DSA2                                                             
         MVI   FLTR1+3,11          SET UP FILM FILTER                           
         MVI   FLTR1+4,8                                                        
         MVC   FLTR1+5(8),0(R9)                                                 
         MVI   FLTR1+13,X'FF'                                                   
*                                                                               
DSA2     LA    R9,FLTR1                                                         
         L     R3,BUFFBUFF                                                      
         MVI   BSUMFRST,1                                                       
         ZIC   R8,LCODE                                                         
         GOTO1 BUFFSUM,DMCB,((R8),BUFFBUFF),(0,11),(R9),GENSUM                  
         CLI   BSUMFRST,1          EOF ON FIRST READ                            
         BE    DS2CA                                                            
         XC    PSLIST,PSLIST       SEND A ZERO KEY                              
         LA    R1,PSLIST                                                        
         BAS   RE,GENSUM                                                        
         L     RE,=A(SUMAREA)                                                   
         L     RF,=F'2600'                                                      
         XCEF                                                                   
DS2CA    CLI   BUFCDE,X'60'        CHECK FOR END                                
         BH    DS2D                                                             
         ZIC   RE,BUFCDE                                                        
         LA    RE,32(RE)                                                        
         STC   RE,BUFCDE                                                        
DS2C1    XC    0(20,R4),0(R4)                                                   
         MVC   0(1,R4),BUFCDE                                                   
         B     DSA                                                              
         SPACE 2                                                                
DS2D     L     RF,NEXTFLM                                                       
         MVI   BUFCDE,X'61'                                                     
         CLI   0(RF),0                                                          
         BE    DOSUM3                                                           
         LA    RF,8(RF)                                                         
         ST    RF,NEXTFLM                                                       
         B     DSNXTFLM                                                         
         SPACE 2                                                                
DOSUM3   SR    R5,R5                                                            
         IC    R5,LCODE                                                         
DOSUM4   GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFCDE,(R3)),(X'80',(R5))                
         CLI   BUFCDE,X'61'        CHECK FOR END                                
         BH    DOSUMX                                                           
         ZIC   RE,BUFCDE                                                        
         LA    RE,32(RE)                                                        
         STC   RE,BUFCDE                                                        
         B     DOSUM4                                                           
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
BSUM     NTR1                                                                   
         LR    R9,R1                                                            
         L     R3,BUFFBUFF                                                      
         L     R4,BUFFIO                                                        
         MOVE  (MYBUFIO,400),0(R1)                                              
BSNODUMP LR    R1,R9                                                            
         CLI   BSUMFRST,1          IS THIS THE FIRST CALL                       
         BNE   BS2                                                              
         OC    0(10,R1),0(R1)      EOF                                          
         BZ    BSUMEX              YES EXIT                                     
         MVI   BSUMFRST,0                                                       
         CLI   MULTISW,C'Y'                                                     
         BNE   BSAA1                                                            
         MVI   FORCEMID,C'Y'                                                    
         LA    RE,MID1             SET MEDIA CAPTION                            
         CLI   MID1,C' '                                                        
         BE    *+8                                                              
         LA    RE,132(RE)                                                       
         MVC   0(132,RE),SPACES                                                 
         MVC   0(11,RE),=C'COMBINED TV'                                         
         CLI   0(R4),X'60'                                                      
         BH    BSAA1                                                            
         MVC   0(11,RE),=C'NETWORK TV '                                         
         CLI   0(R4),X'40'                                                      
         BH    BSAA1                                                            
         MVC   0(11,RE),=C'SPOT TV    '                                         
BSAA1    L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         MVI   MEDSLCNT,0                                                       
         MVI   MEDDPCNT,0                                                       
         B     BS2                                                              
BS1      B     BSUMEX              EXIT HERE FOR NEXT RECORD                    
BS2      OC    MYBUFIO(22),MYBUFIO                                              
         BZ    BSUMEX                                                           
         OC    MYBUFIO+22(120),MYBUFIO+22                                       
         BZ    BS1                                                              
         SPACE 2                                                                
         CLI   PROGPROF+5,C'Y'     SUPPRESS CROSS DAYPART DEMOS                 
         BNE   BS2EDT                                                           
         CLI   MYBUFIO+2,X'FF'     IS IT A TOTAL LINE                           
         BNE   BS2EDT                                                           
         USING BUFFRECD,R4                                                      
         XC    BFDGL(8),BFDGL      CLEAR OUT THE POINTS                         
         XC    BFDBY1,BFDBY1                                                    
         XC    BFDEMS,BFDEMS                                                    
         DROP  R4                                                               
         SPACE 2                                                                
BS2EDT   GOTO1 MEDEDIT,DMCB,(RA),(R7)                                           
         CLI   DMCB,0              DONT PRINT THIS LINE                         
         BE    BS1                                                              
         CLI   PRTSW,0                                                          
         BE    BS2A                                                             
         CLI   MYBUFIO+2,X'FD'                                                  
         BE    *+8                                                              
         CLI   MYBUFIO+2,X'FE'                                                  
         BNE   *+8                                                              
         MVI   DMCB,1                                                           
         MVC   SPACING,DMCB                                                     
         TM    P1+9,X'F0'                                                       
         BO    BS21                                                             
         GOTO1 MEDSTARS,DMCB,P1                                                 
BS21     DS    0H                                                               
         GOTO1 REPORT                                                           
         GOTO1 =V(SPRPFOOT),DMCB,(RA),RR=RELO                                   
         LA    RE,MYBUFIO+12                                                    
         LA    RF,380                                                           
         XCEF                                                                   
         B     BS1                                                              
BS2A     SR    RE,RE               CHECK FOR OVERFLOW                           
         IC    RE,CURRLN                                                        
         LA    RE,1(RE)                                                         
         STC   RE,CURRLN                                                        
         CLC   CURRLN,MAXLINES                                                  
         BL    BS1                                                              
         MVI   FORCEHED,C'Y'                                                    
BSUMEX   XIT1                                                                   
         EJECT                                                                  
* GENERATE SUMMARIES BASED ON CURRRPT ENTRY                                     
GENSUM   NTR1                                                                   
         MVI   CURRLVL,0           RESET THE LEVEL                              
         ST    R1,SVBSREC          SAVE THE BUFFSUM ADDRESS                     
         LR    RF,R1                                                            
         B     GSNODUMP                                                         
         MVC   P(32),0(R1)                                                      
         GOTO1 HEXOUT,DMCB,(RF),P2,32,0,0                                       
         GOTO1 REPORT                                                           
         L     R1,SVBSREC                                                       
GSNODUMP MVC   WORK,0(R1)                                                       
         LA    R4,CURRKEY          EXTRACT THE CURRENT KEY FIELDS               
         BAS   RE,SETKEY                                                        
         OC    CURRKEY,CURRKEY     CHECK FOR EOF                                
         BNZ   *+10                                                             
         MVC   CURRKEY,HIVALUE                                                  
         L     RE,CURRRPT                                                       
         MVC   LNSUMKEY,0(RE)                                                   
         L     R3,=V(BUFFALOC)                                                  
         USING BUFFALOD,R3                                                      
         LH    RE,LNSUMKEY         GET LENGTH OF SUMMARY KEY                    
         A     RE,BUFFLDTA         ADD LENGTH OF ACCUMULATORS                   
         STH   RE,SUMLN            SAVE SUMMARY LENGTH                          
         DROP  R3                                                               
         CLI   GSUMFRST,1          SET UP IF FIRST TIME                         
         BE    RESET                                                            
GENSUM2  CLC   CURRLVL,MAXLVL      THIS LEVEL GREATER THAN MAX                  
         BE    RESET               YES - WE ARE DONE WITH THIS PASS             
         ZIC   R5,CURRLVL          SET TO CURRENT LEVEL                         
         MH    R5,SUMLN                                                         
         L     R4,=A(SUMAREA)                                                   
         AR    R4,R5                                                            
         LH    R5,LNSUMKEY         IS CURRENT KEY HIGHER                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),CURRKEY                                                  
         BH    RESET               NO - RESET FOR NEXT                          
*                                  ADD UP LEVELS                                
         LR    R5,R4               SET UP ADD TO SLOT                           
         AH    R5,SUMLN            POSITION TO ACCUMULATORS                     
         BAS   RE,ADDSUMS          ADD TO NEXT LEVEL                            
         LR    R1,R4               SEND RECORD TO BSUM                          
         MVC   MYBUFIO(32),PREVKEY RESTORE PREVIOUS KEY                         
*                                                                               
         L     R3,=V(BUFFALOC)                                                  
         USING BUFFALOD,R3                                                      
         LA    R7,MYBUFIO          SET UP ACCUMULATERS                          
         A     R7,BUFFLKEY         BUMP PAST KEY                                
         A     R7,BUFFLCOM         BUMP PAST COMMENTS                           
         DROP  R3                                                               
         LR    RE,R4               SET TO SLOT                                  
         AH    RE,LNSUMKEY         BYPASS KEY                                   
         MOVE  ((R7),380),0(RE)                                                 
*                                                                               
         L     RE,CURRRPT          SET APPROPRIATE HIVALUES IN KEY              
         LA    RE,2(RE)            BUMP PAST KEY LENGTH                         
         ZIC   RF,CURRLVL          SET TO CURRENT LEVEL                         
         SLL   RF,2                * 4                                          
         LA    RE,0(RF,RE)                                                      
         ZIC   R7,0(RE)            DIG OUT DISPLACEMENT                         
         LA    R7,MYBUFIO(R7)                                                   
         SR    RF,RF                                                            
         ICM   RF,3,2(RE)                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),HIVALUE                                                  
*                                                                               
         LA    R1,MYBUFIO          SEND OUT RECORD                              
         BAS   RE,BSUM                                                          
*                                                                               
         LR    RE,R4               CLEAR OUT THE SLOT                           
         LH    RF,SUMLN                                                         
         XCEF                                                                   
         ZIC   R5,CURRLVL          BUMP TO NEXT LEVEL                           
         LA    R5,1(R5)                                                         
         STC   R5,CURRLVL                                                       
         B     GENSUM2             PROCESS THE NEXT LEVEL                       
         SPACE 2                                                                
RESET    MVI   CURRLVL,0           RESET LEVEL TO START                         
         SR    R5,R5                                                            
RESET2   L     RE,SVBSREC          RESTORE CURRENT KEY                          
         MVC   WORK,0(RE)                                                       
         MH    R5,SUMLN                                                         
         L     R4,=A(SUMAREA)                                                   
         AR    R4,R5                                                            
         LH    R1,LNSUMKEY         HAS THIS SLOT BEEN SENT                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R4),0(R4)                                                    
         BNZ   GENSUMX             NO - EXIT                                    
         ZIC   R5,CURRLVL          GET THE CURRENT LEVEL                        
         SLL   R5,2                * 4                                          
         A     R5,CURRRPT          SET UP THE KEY                               
         LA    R5,2(R5)                                                         
         ZIC   R9,0(R5)            GET FIELD DISPLACEMENT                       
         LA    R9,WORK(R9)                                                      
         SR    R7,R7                                                            
         ZIC   R7,3(R5)            GET LENGTH TO FILL                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R9),HIVALUE                                                  
         BAS   RE,SETKEY                                                        
         ZIC   R5,CURRLVL          BUMP THE LEVEL CODE                          
         LA    R5,1(R5)                                                         
         STC   R5,CURRLVL                                                       
         CLC   CURRLVL,MAXLVL      END OF LEVELS                                
         BNE   RESET2              NO - BUILD THE NEXT ONE                      
*                                                                               
GENSUMX  L     R1,SVBSREC          SET TO ORIGINAL RECORD                       
         MVC   PREVKEY(32),0(R1)                                                
         CLC   CURRKEY,HIVALUE     END                                          
         BNE   *+12                                                             
         MVI   GSUMFRST,1          YES - RESET AND EXIT                         
         B     GENSUMX2                                                         
*                                                                               
         BAS   RE,BSUM             THEN PROCESS IT                              
         BAS   RE,ADDDET           ADD DETAILS TO LEVEL 0                       
         MVI   CURRLVL,0           RESET CURRENT LEVEL                          
         MVI   GSUMFRST,0                                                       
GENSUMX2 XIT1                                                                   
         EJECT                                                                  
* TRANSLATE SEQUENCE NUMBERS INTO CANNISTER IDS                                 
GETNFLM  NTR1                                                                   
         MVC   CANISTER,=CL8'UNKNOWN'  DEFAULT CANNISTER                        
         OC    FILMSEQ,FILMSEQ     NO SEQUENCE NUMBER                           
         BZ    GTNFLMX2                                                         
*                                                                               
         L     R7,=A(CMLEQU)       CHECK IF ALREADY READ                        
GTNFLM2  OC    0(2,R7),0(R7)       NOT IN TABLE                                 
         BZ    GTNFLM6                                                          
*                                                                               
         CLC   0(2,R7),FILMSEQ     SAME SEQUENCE NUMBER                         
         BNE   GTNFLM4                                                          
         MVC   CANISTER(8),2(R7)   SET CANISTER AND EXIT                        
         B     GTNFLMX2                                                         
*                                                                               
GTNFLM4  LA    R7,10(R7)           TRY NEXT SEQUENCE NUMBER                     
         L     RF,=A(CMLEQUEN)                                                  
         CR    R7,RF               TEST FOR END                                 
         BL    *+6                                                              
         DC    H'0'                CMLEQU TABLE OVERFLOW                        
         B     GTNFLM2                                                          
*                                                                               
GTNFLM6  MVC   SVAREC,AREC         SAVE IO FIELDS                               
         MVC   SVKEY,KEY                                                        
         MVC   SVKEYSV,KEYSAVE                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0AA1'     READ FILM SEQUENCE NUMBER                    
         MVC   KEY+2(1),BAGYMD     TO DETERMINE CANNISTER NUMBER                
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+6(2),FILMSEQ                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETNFLMX                                                         
         L     RE,=A(SUMAREA)                                                   
         ST    RE,AREC                                                          
         GOTO1 GET                                                              
         L     RE,=A(SUMAREA)                                                   
         LA    RE,5(RE)                                                         
         MVC   CANISTER,0(RE)      SET CANNISTER NUMBER                         
         MVC   0(2,R7),FILMSEQ     SAVE EQUATED NUMBER                          
         MVC   2(8,R7),CANISTER                                                 
GETNFLMX MVC   KEY,SVKEY           RESTORE KEYS AND RECORD                      
         MVC   AREC,SVAREC                                                      
         GOTO1 HIGH                                                             
         MVC   KEYSAVE,SVKEYSV                                                  
GTNFLMX2 XIT1                                                                   
         EJECT                                                                  
SETKEY   NTR1                      SET UP CURRENT REPORT KEYS                   
         L     RE,CURRRPT           (KEY IS BUILT BACKWARDS)                    
         MVC   LNSUMKEY,0(RE)                                                   
         LH    R5,LNSUMKEY         R4 POINTS TO SLOT ON ENTRY                   
         AR    R5,R4                                                            
         XC    CURRKEY,CURRKEY                                                  
         L     RE,CURRRPT          BUILD SUMMARY KEY FROM REPORT TABLE          
         LA    RE,2(RE)                                                         
SETKEY2  CLI   0(RE),X'FF'         TABLE END                                    
         BE    SETKEYX                                                          
         ZIC   RF,0(RE)            GET FIELD DISPLACEMENT                       
         LA    RF,WORK(RF)                                                      
         ZIC   R1,1(RE)            GET FIELD LENGTH                             
         SR    R5,R1               DECREMENT FOR KEY SAVE                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(RF)                                                    
         LA    RE,4(RE)                                                         
         B     SETKEY2                                                          
SETKEYX  XIT1                                                                   
         EJECT                                                                  
ADDDET   NTR1                                                                   
         L     R3,=V(BUFFALOC)                                                  
         USING BUFFALOD,R3                                                      
         L     R4,SVBSREC          GET ORIGINAL RECORD                          
         A     R4,BUFFLKEY         SET IT TO DATA                               
         A     R4,BUFFLCOM         SET TO ACCUMULATORS                          
         MVC   NMSUMACC,BUFFCOLS+3                                              
         L     RE,CURRRPT          SET ADD TO SLOT                              
         MVC   LNSUMKEY,0(RE)                                                   
         L     R5,=A(SUMAREA)                                                   
         AH    R5,LNSUMKEY         BUMP PAST THE KEY                            
         B     ADDSUMS1                                                         
         DROP  R3                                                               
*                                                                               
ADDSUMS  NTR1                                                                   
         L     RE,CURRRPT                                                       
         MVC   LNSUMKEY,0(RE)                                                   
         AH    R4,LNSUMKEY         BUMP PAST THE KEYS                           
         AH    R5,LNSUMKEY                                                      
ADDSUMS1 ZIC   R0,NMSUMACC         GET NUMBER OF ACCUMULATORS                   
ADDSUMS2 ICM   R1,15,0(R4)                                                      
         ICM   R3,15,0(R5)                                                      
         AR    R3,R1                                                            
         STCM  R3,15,0(R5)                                                      
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,ADDSUMS2                                                      
         XIT1                                                                   
         EJECT                                                                  
         DROP  R6                                                               
         USING *,RF                                                             
MYHEAD   NTR1  BASE=SPF4RB                                                      
         LM    RA,RC,SPF4RA                                                     
         L     R6,SPF4R6                                                        
         DROP  RF                                                               
         USING SPF402+4096,R6                                                   
         GOTO1 =V(MYHEADC),DMCB,(RA),HCAP1,RR=RELO                              
         XIT1                                                                   
         EJECT                                                                  
         DROP  R6                                                               
         USING *,RF                                                             
FLTRSPT  NTR1  BASE=SPF4RB                                                      
         LM    RA,RC,SPF4RA                                                     
         L     R6,SPF4R6                                                        
         DROP  RF                                                               
         USING SPF402+4096,R6                                                   
         MVI   SPOTYORN,C'N'       SET TO BYPASS SPOT                           
         L     R7,SPOTADDR         GET SPOT ADDRESS                             
FLTRSPT2 ZIC   R8,1(R7)            GET NEXT ELEMENT                             
         AR    R7,R8                                                            
         CLI   0(R7),X'0F'         END OF SPOT                                  
         BL    FLTRSPT4                                                         
         CLI   0(R7),X'12'         FILM ELEMENT                                 
         BNE   FLTRSPT2                                                         
         USING FLMELEM,R7                                                       
         OC    FILMSEQ,FILMSEQ     IF NO FILM REQUESTED                         
         BZ    FLTRSPTX            BYPASS SPOT                                  
         CLC   FLMNUM,FILMSEQ      SAME FILM NUMBER                             
         BNE   FLTRSPT2                                                         
         MVI   SPOTYORN,C'Y'       YES - USE IT                                 
FLTRSPT4 OC    FILMSEQ,FILMSEQ                                                  
         BNZ   FLTRSPTX                                                         
         MVI   SPOTYORN,C'Y'                                                    
FLTRSPTX XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
NEWDNAM  NTR1                      GET NEW FORMAT DEMO NAMES                    
         L     R2,ADBLOCK          R9 HAS NUMBER OF DEMOS                       
         USING DBLOCK,R2           FULL HAS START OF LIST                       
         XC    0(256,R2),0(R2)                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         DROP  R2                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         LA    RF,EUSRNMS          SET FOR USER NAMES                           
         DROP  RF                                                               
         L     R2,FULL                                                          
         GOTO1 DEMOCON,DMCB,((R9),(R2)),(2,DNAME1),(C'S',ADBLOCK),(RF)          
         XIT1                                                                   
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
         DROP  RF                                                               
SPF4RA   DC    F'0'                                                             
SPF4RB   DC    F'0'                                                             
SPF4RC   DC    F'0'                                                             
RELO     DS    F                                                                
HCAP1    DS    CL6                                                              
HCAP2    DS    CL9                                                              
         LTORG                                                                  
         EJECT                                                                  
HIVALUE  DC    30X'FF'                                                          
LVCNTRL  DC    F'1'                                                             
         DC    A(2,3,4,5)                                                       
CPPSW    DC    X'00'               CROSS DAYPART CPP SW                         
FLTR1    DC    X'00',X'01',X'00'   RECORD TYPE FILTERS                          
         DC    X'00',X'00',CL8' '                                               
         DC    X'00',X'00',CL8' '                                               
         DC    X'00',X'00',CL8' '                                               
         DC    X'FF'                                                            
F4TAB1   DS    0C                                                               
         DC    AL2(L'F4KSLN+L'F4KDPT+L'F4KDGRP)                                 
         DC    AL1(F4KSLN-F4KSTRT,L'F4KSLN,0,F4KEND-F4KSLN)                     
         DC    AL1(F4KDPT-F4KSTRT,L'F4KDPT,0,F4KEND-F4KDPT)                     
         DC    AL1(F4KDGRP-F4KSTRT,L'F4KDGRP,0,F4KEND-F4KDGRP)                  
         DC    X'FF'                                                            
         EJECT                                                                  
F4KSTRT  DS    0C                                                               
F4KRTYP  DS    CL1                 RECORD TYPE                                  
F4KPDEM  DS    CL1                 PRIMARY DEMO                                 
F4KDGRP  DS    CL4                 DAYPART GROUP                                
F4KDPT   DS    CL4                 DAYPART                                      
F4KSLN   DS    CL1                 SPOT LENGTH                                  
F4KFLM   DS    CL8                 FILM NUMBER                                  
F4KBRK   DS    CL2                                                              
F4KEND   DS    0C                                                               
F4KLN    EQU   F4KEND-F4KSTRT                                                   
         SPACE 2                                                                
SWCCNT   DC    A(0)                                                             
SWCMAX   DC    A(40000/SWCLN)                                                   
SWCTAB   DC    V(SWCTABC)                                                       
FPCTCNT  DC    A(0)                                                             
FPCTMAX  DC    A(7500/FPCTCLN)                                                  
FPCTAB   DC    V(FPCTABC)                                                       
GFOTAB   DC    V(GFOTABC)                                                       
PATFREQ  DS    F                                                                
NEXTFLM  DS    F                                                                
CURRBRK  DC    F'0'                                                             
SVBSREC  DS    F                                                                
FOOTLIN  DS    F                                                                
WEIGHT   DC    F'1'                                                             
SAVE9    DS    F                                                                
ACTAREA  DS    F                                                                
MKTDOL   DS    F                                                                
SWCPTR   DS    F                   POINTER FOR STATION WEEK TABLE               
CURRRPT  DS    F                                                                
LNSUMKEY DS    H                                                                
SUMLN    DS    H                                                                
BRKNUM   DS    H                                                                
PASS     DS    C                                                                
BSUMFRST DS    C                   FIRST TIME SWITCH FOR BUFFALO                
PRTSW    DS    C                                                                
FRSTTOT  DS    C                                                                
CURRLN   DS    C                                                                
SW1      DS    C                   DATA SWITCH                                  
COPY     DS    C                                                                
FPCTSW   DS    C                                                                
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
NMSUMACC DS    C                                                                
CURRLVL  DS    C                                                                
MAXLVL   DS    C                                                                
GSUMFRST DS    C                                                                
SPLPRINT DS    C                                                                
PSLIST   DS    CL150              PRODUCT SPOT LENGTH LIST                      
PRMYTAB  DS    CL60                                                             
FILMSEQ  DS    CL2                 SAVE AREA FOR FILM SEQUENCE                  
CANISTER DS    CL8                                                              
SVQCMRCL DS    CL8                 SAVE AREA FOR REQUESTED CML                  
ORIGCML  DS    CL8                                                              
SVKEY    DS    CL13                                                             
SVKEYSV  DS    CL13                                                             
SVAREC   DS    CL4                                                              
POSTWORK DS    CL2                                                              
DNAME1   DS    CL7                                                              
DNAME2   DS    CL7                                                              
DNAME3   DS    CL7                                                              
DNAME4   DS    CL7                                                              
SPBUFMKT DS    CL240                                                            
MYBUFIO  DS    CL420                                                            
CURRKEY  DS    CL32                                                             
PREVKEY  DS    CL32                                                             
LNFLMTAB DC    A(4000)                                                          
FILMTAB  DS    CL4000                                                           
         DC    X'FF'                                                            
SUMAREA  DS    CL2600                                                           
CMLEQU   DS    2000C                                                            
CMLEQUEN DS    0C                                                               
LNCMLEQU EQU   2000                                                             
MKTBRK   DS    4000C                                                            
MKTBRKEN DS    0C                                                               
BUFFRECD DSECT                                                                  
BFDKEY   DS    CL22                                                             
BFDDATA  DS    0CL60                                                            
BFDGL    DS    CL16                                                             
BFDBY1   DS    CL8                                                              
BFDDL    DS    CL8                                                              
BFSP     DS    CL4                                                              
BFDEMS   DS    CL24                                                             
GFINP    DSECT                     GET FILM INPUT PARAMETERS                    
GFIAGYMD DS    C                                                                
GFICLT   DS    CL2                                                              
GFIPRDA  DS    C                                                                
GFISLNA  DS    C                                                                
GFIPRDB  DS    C                                                                
GFISLNB  DS    C                                                                
GFICOPY  DS    C                                                                
GFISDTE  DS    CL2                                                              
GFIEDTE  DS    CL2                                                              
GFIMSTA  DS    0CL5                                                             
GFIMKT   DS    CL2                                                              
GFISTA   DS    CL3                                                              
GFIOLEN  DS    CL4                                                              
GFOUTP   DSECT                     GET FILM OUTPUT                              
GFOSDTE  DS    CL2                                                              
GFOEDTE  DS    CL2                                                              
GFOALPH1 DS    CL8                                                              
GFOALPH2 DS    CL8                                                              
GFOFREQ  DS    CL1                                                              
GFOLN    EQU   11                                                               
SWCTABD  DSECT                    STATION/WEEK/COPY TABLE                       
SWCST    DS    0C                                                               
SWCPRD   DS    CL1                 PRODUCT                                      
SWCSLN   DS    CL1                 SPOT LENGTH                                  
SWCDAT   DS    CL2                 WEEK DATE                                    
SWCSTA   DS    CL5                 MARKET/STATION                               
SWCCPY   DS    CL1                 COPY                                         
SWCDOL   DS    CL4                 DOLLARS                                      
SWCPDOL  DS    CL2                 PERCENT OF DOLLARS                           
SWCEN    DS    0C                                                               
SWCLN    EQU   SWCEN-SWCST                                                      
SWCKLN   EQU   SWCDOL-SWCST                                                     
         SPACE 2                                                                
FPCTABD  DSECT                                                                  
FPCTFLM  DS    CL8                                                              
FPCTDAT  DS    CL2                                                              
FPCTCPY  DS    CL1                                                              
FPCTPCT  DS    CL4                                                              
FPCTCLN  EQU   15                                                               
FPCTKLN  EQU   11                                                               
         EJECT                                                                  
MYHEADC  CSECT                                                                  
         NMOD1 0,MYHEAD                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R7,4(R1)                                                         
         USING HCAP1,R7                                                         
         CLC   QPROG,=C'F2'                                                     
         BNE   MYHMKRPT                                                         
         XC    H1+45(42),H1+45                                                  
         XC    H2+45(42),H2+45                                                  
         MVC   H1+47(8),SVQCMRCL                                                
         MVC   H2+47(8),=C'--------'                                            
         MVC   H1+56(29),=C'COMMERCIAL PERFORMANCE REPORT'                      
         MVC   H2+56(29),=C'-----------------------------'                      
         MVC   H11(10),SPACES                                                   
         MVC   H11(6),=C'MARKET'                                                
MYHMKRPT CLI   QFILTER,C'F'        FILM NUMBER FILTER FOR COKE                  
         BNE   MYHCTAX                                                          
         MVC   H8(8),=C'**FILM**'                                               
         MVC   H8+9(1),QFILTER+1                                                
MYHCTAX  L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         CLI   MEDEXTAX,C'Y'                                                    
         BNE   MYHSUBSX                                                         
         MVC   H8+50(18),=C'***TAX EXCLUDED***'                                 
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
         MVC   H12+89(4),=C'PNTS'                                               
         MVC   H12+97(3),=C'CPP'                                                
         CLI   DNAME2,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAME2,C'R'                                                      
         BE    *+16                                                             
         MVC   H12+89(4),=C'IMPS'                                               
         MVC   H12+97(3),=C'CPM'                                                
         CLI   DNAME3,0                                                         
         BE    MYHEADX                                                          
         MVC   H11+104(11),DASH                                                 
         MVC   H11+106(7),DNAME3                                                
         MVC   H12+104(4),=C'PNTS'                                              
         MVC   H12+112(3),=C'CPP'                                               
         CLI   DNAME3,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAME3,C'R'                                                      
         BE    *+16                                                             
         MVC   H12+104(4),=C'IMPS'                                              
         MVC   H12+112(3),=C'CPM'                                               
         CLI   DNAME4,0                                                         
         BE    MYHEADX                                                          
         MVC   H11+119(11),DASH                                                 
         MVC   H11+121(7),DNAME4                                                
         MVC   H12+119(4),=C'PNTS'                                              
         MVC   H12+127(3),=C'CPP'                                               
         CLI   DNAME4,C'E'                                                      
         BE    *+8                                                              
         CLI   DNAME4,C'R'                                                      
         BE    *+16                                                             
         MVC   H12+119(4),=C'IMPS'                                              
         MVC   H12+127(3),=C'CPM'                                               
MYHEADX  XIT1                                                                   
DASH     DC    11C'-'                                                           
         DROP  R7                                                               
         LTORG                                                                  
         EJECT                                                                  
* SAVE STATION DOLLARS BY WEEK AND COPY                                         
BSWC     CSECT                                                                  
         NMOD1 0,BSWC                                                           
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPF402+4096,R6                                                   
         XC    CURRKEY,CURRKEY     BUILD BINSRCH KEY                            
         LA    R7,CURRKEY                                                       
         USING SWCTABD,R7                                                       
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
         MVC   SWCPRD,MEDBRAND                                                  
         MVC   SWCSLN,MEDSPTLN                                                  
         MVC   SWCCPY,COPY                                                      
         L     R2,MEDAFRST         EXTRACT WEEKLY DOLLARS                       
BSWC2    C     R2,MEDALAST                                                      
         BH    BSWCXIT                                                          
         OC    4(4,R2),4(R2)       CHECK FOR EMPTY SLOT                         
         BNZ   *+12                                                             
BSWC4    LA    R2,12(R2)           GET NEXT ENTRY                               
         B     BSWC2                                                            
         L     R4,4(R2)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD   ANY DATA                                     
         BZ    BSWC4               NO NEXT SLOT                                 
         L     R5,ADBUY                                                         
         USING BUYREC,R5                                                        
         MVC   SWCSTA(L'SWCSTA),BUYMSTA                                         
         MVC   SWCDAT,0(R2)                                                     
         XC    SWCDOL,SWCDOL       ZERO FOR INSERT                              
         DROP  R5                                                               
BSWC6    GOTO1 BINSRCH,DMCB,(X'01',CURRKEY),SWCTAB,SWCCNT,(0,SWCLN),   X        
               (0,SWCKLN),SWCMAX                                                
         OC    1(3,R1),1(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                OVERFLOW                                     
         MVC   SWCCNT,8(R1)                                                     
         MVC   SWCDOL,MEDBYD                                                    
         OC    SWCDOL,SWCDOL                                                    
         BNZ   *+10                                                             
         MVC   SWCDOL,=F'1'                                                     
         L     R7,0(R1)            P0INT TO RECORD                              
         ICM   RF,15,SWCDOL        ADD UP DOLLARS                               
         LA    R7,CURRKEY                                                       
         ICM   RE,15,SWCDOL                                                     
         AR    RE,RF                                                            
         L     R7,0(R1)                                                         
         STCM  RE,15,SWCDOL        SAVE IN RECORD                               
         LA    R7,CURRKEY                                                       
BSWC8    OC    SWCSTA,SWCSTA       ALL STATION BEEN SENT                        
         BZ    BSWC4                YES - NEXT                                  
         XC    SWCSTA,SWCSTA        NO - SEND IT NOW                            
         XC    SWCDOL,SWCDOL                                                    
         B     BSWC6                                                            
BSWCXIT  XMOD1 1                                                                
         LTORG                                                                  
         DROP  R3                                                               
         DROP  R7                                                               
         EJECT                                                                  
* CALCULATE FILM PERCENTAGES                                                    
FLMPCT   CSECT                                                                  
         NMOD1 0,FLMPCT                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPF402+4096,R6                                                   
*                                                                               
         XC    FPCTCNT,FPCTCNT     RESET FILM PERCENTS                          
         SR    RE,RE                                                            
         LA    RF,FPCTCLN                                                       
         M     RE,FPCTMAX                                                       
         L     RE,FPCTAB                                                        
         XCEF                                                                   
*                                                                               
         GOTO1 XSORT,DMCB,(X'00',SWCTAB),SWCCNT,SWCLN,SWCKLN,0                  
         GOTO1 XSORT,DMCB,(X'00',SWCTAB),SWCCNT,SWCLN,1,9                       
         L     R7,SWCTAB                                                        
         USING SWCTABD,R7                                                       
FLMPCT2  OC    0(2,R7),0(R7)                                                    
         BZ    FLMPCTX                                                          
         OC    SWCSTA,SWCSTA                                                    
         BZ    FLMPCT4                                                          
         ICM   RE,15,SWCDOL        GET STATION DOLLARS                          
         SRDA  RE,32                                                            
         M     RE,=F'10000'                                                     
         SLDA  RE,1                                                             
         D     RE,MKTDOL           DIVID BY MARKET DOLLARS                      
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         STCM  RF,3,SWCPDOL        SAVE PCNT                                    
*        MVC   P(7),=C'STATION'                                                 
*        EDIT  SWCDOL,(10,P+10),2                                               
*        EDIT  MKTDOL,(10,P+22),2                                               
*        EDIT  SWCPDOL,(8,P+34),2                                               
*        GOTO1 REPORT                                                           
         LA    R7,SWCLN(R7)                                                     
         B     FLMPCT2                                                          
FLMPCT4  MVC   MKTDOL,SWCDOL                                                    
         MVC   SWCPDOL,=H'10000'                                                
         LA    R7,SWCLN(R7)                                                     
         B     FLMPCT2                                                          
FLMPCTX  DS    0H                                                               
         L     R7,SWCTAB           RESTORE STA/WEEK POINTER                     
         ST    R7,SWCPTR                                                        
GETCML   XC    WORK,WORK           GET ROTATION FREQ.                           
         LA    R5,WORK                                                          
         USING GFINP,R5                                                         
         L     R7,SWCPTR           BUILD GETRECAP BLOCK                         
         CLI   0(R7),0             END OF INPUT                                 
         BE    GETCMLX                                                          
         OC    SWCSTA,SWCSTA       BYPASS MARKET LEVEL                          
         BZ    GETCML30                                                         
         MVC   GFIAGYMD,BAGYMD                                                  
         MVC   GFICLT,BCLT                                                      
         MVC   GFIPRDA,SWCPRD                                                   
         MVC   GFISLNA,SWCSLN                                                   
         MVC   GFISDTE,SWCDAT                                                   
         SR    RF,RF                                                            
         ICM   RF,3,SWCDAT                                                      
         LA    RF,6(RF)                                                         
         STCM  RF,3,GFIEDTE                                                     
         MVC   GFIMSTA,SWCSTA                                                   
         MVC   GFICOPY,SWCCPY                                                   
         MVC   GFIOLEN,=F'3000'                                                 
         GOTO1 =V(GETRECAP),DMCB,WORK,GFOTAB,ACOMFACS                           
         SPACE 2                                                                
*   PRINT THE ALLOCATIONS IN TEST MODE                                          
*        GOTO1 HEXOUT,DMCB,GFOTAB,P1,60,0,0                                     
*        MVC   P2(130),GFOTAB                                                   
*        GOTO1 REPORT                                                           
         SPACE 2                                                                
         SR    RF,RF               SET UP FILM PERCENTAGES                      
         L     R9,GFOTAB                                                        
         USING GFOUTP,R9                                                        
         LR    R1,R9                                                            
FLMFRE   OC    GFOALPH1,GFOALPH1   GET TOTAL FREQ FOT THIS PATTERN              
         BZ    FLMFRE2                                                          
         ZIC   R0,GFOFREQ                                                       
         AR    RF,R0                                                            
         LA    R9,17(R9)                                                        
         B     FLMFRE                                                           
FLMFRE2  LA    R9,21(R9)           SET TO NEXT PATTERN                          
         OC    0(2,R9),0(R9)       END                                          
         BNZ   FLMFRE               NO - CONTINUE COUNTING                      
         L     R9,GFOTAB            YES - CALCULATE FILM PERCENTAGES            
         ST    RF,PATFREQ          SAVE TOTAL FREQUENCY                         
FLMFRE4  OC    GFOALPH1,GFOALPH1                                                
         BZ    FLMFRE6                                                          
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING FPCTABD,R3                                                       
         MVC   FPCTFLM,GFOALPH1                                                 
         MVC   FPCTDAT,SWCDAT                                                   
         MVC   FPCTCPY,SWCCPY                                                   
         XC    FPCTPCT,FPCTPCT                                                  
         GOTO1 BINSRCH,DMCB,(X'01',WORK),FPCTAB,FPCTCNT,FPCTCLN,       X        
               FPCTKLN,FPCTMAX                                                  
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE OVERFLOW                               
         MVC   FPCTCNT,8(R1)                                                    
         L     R3,0(R1)            GET TABLE ADDRESS                            
         ZIC   RF,GFOFREQ          GET FREQUENCY OF THIS CML                    
         SR    RE,RE                                                            
         M     RE,=F'10000'                                                     
         SLDA  RE,1                                                             
         D     RE,PATFREQ          GET PERCENT FOR CML                          
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         SR    RE,RE                                                            
         ICM   RE,3,SWCPDOL                                                     
         MR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,=F'10000'                                                     
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         SR    RE,RE                                                            
         ICM   RE,15,FPCTPCT                                                    
         AR    RF,RE                                                            
         STCM  RF,15,FPCTPCT                                                    
         LA    R9,17(R9)                                                        
         B     FLMFRE4                                                          
FLMFRE6  LA    R9,21(R9)                                                        
         OC    0(2,R9),0(R9)                                                    
         BNZ   FLMFRE4                                                          
GETCML30 L     R7,SWCPTR                                                        
         LA    R7,SWCLN(R7)                                                     
         ST    R7,SWCPTR                                                        
         B     GETCML                                                           
GETCMLX  XMOD1 1                                                                
         DROP  R7                                                               
         LTORG                                                                  
         EJECT                                                                  
* ALLOCATE GOALS BY FILM AND ADD TO BUFFALO RECORDS                             
ALLOCG   CSECT                                                                  
         NMOD1 0,ALLOCG                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPF402+4096,R6                                                   
         LA    RE,KEY                                                           
         USING GOALREC,RE                                                       
         MVC   COPY,GKEYDPT                                                     
         CLI   QOPT5+1,C'Y'                                                     
         BNE   *+8                                                              
         MVI   COPY,C'X'                                                        
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         LA    RE,MEDPERD                                                       
         L     R5,4(RE)            SET TO PERIOD                                
         L     R2,MEDAFRST                                                      
AG2      L     RF,MEDBUFF                                                       
         C     R2,MEDALAST                                                      
         BH    AGEXIT                                                           
         OC    4(4,R2),4(R2)       CHECK FOR EMPTY SLOT                         
         BNZ   *+12                                                             
AG4      LA    R2,12(R2)                                                        
         B     AG2                                                              
         L     R4,4(R2)            POINT TO DATA SLOT                           
         USING MEDDATA,R4                                                       
         OC    MEDGLD(12),MEDGLD                                                
         BZ    AG4                                                              
         L     R7,FPCTAB                                                        
         USING FPCTABD,R7                                                       
         MVC   FULL(2),0(R2)                                                    
         MVC   FULL+2(1),COPY                                                   
AG6      MVC   0(16,R5),0(R4)                                                   
AG8      OC    0(8,R7),0(R7)        END OF CMLS                                 
         BZ    AG4                  GET NEXT WEEK                               
         CLC   FPCTDAT(3),FULL                                                  
         BE    *+12                                                             
AG9      LA    R7,FPCTCLN(R7)                                                   
         B     AG8                                                              
         LA    R0,4                                                             
         ST    R5,DUB                                                           
         LA    R9,0                                                             
AGA      L     RF,0(R9,R5)         PRORATE DOLLARS AND DEMOS                    
         SR    RE,RE                                                            
         ICM   RE,15,FPCTPCT                                                    
         MR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,=F'10000'                                                     
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R9,R5)                                                      
         LA    R9,4(R9)                                                         
         BCT   R0,AGA                                                           
         MVC   WORK(2),=X'6162'                                                 
         MVC   WORK+2(8),FPCTFLM                                                
         MVC   WORK+10(2),BRKNUM                                                
         GOTO1 MEDPOST,DMCB,(RA)                                                
*                                                                               
* BACK OUT ALLOCATED GOALS FROM TOTAL GOALS                                     
         LA    R0,4                                                             
         LA    R9,0                                                             
AGB      L     RF,0(R9,R5)         GET ORIGINAL VALUE                           
         LNR   RF,RF               CHANGE THE SIGN                              
         ST    RF,0(R9,R5)         SAVE IT                                      
         LA    R9,4(R9)                                                         
         BCT   R0,AGB                                                           
         MVC   WORK+2(8),=C'--GOAL--'                                           
         MVC   WORK+10(2),BRKNUM                                                
         GOTO1 MEDPOST,DMCB,(RA)   SEND REVERSAL RECORD                         
         MVC   0(16,R5),0(R4)                                                   
         B     AG9                                                              
AGEXIT   XMOD1 1                                                                
         DROP  R7                                                               
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
         NMOD1 0,F4FIXHED                                                       
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R7,4(R1)                                                         
         USING HCAP1,R7                                                         
         XC    HCAP1,HCAP1                                                      
         XC    HCAP2,HCAP2                                                      
         MVC   HCAP1,=C' GOAL '                                                 
         MVC   HCAP2,=C'PURCHASED'                                              
         CLI   QRERATE,C'P'                                                     
         BNE   *+10                                                             
         MVC   HCAP2,=C'ACHIEVED '                                              
         CLI   QRERATE,C'I'                                                     
         BNE   *+10                                                             
         MVC   HCAP2,=C'AFFIDAVIT'                                              
         CLI   QCOMPARE,C'B'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'D'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'F'                                                    
         BNE   *+14                                                             
         MVC   HCAP2(9),=C'AFFIDAVIT'                                           
         MVI   QRERATE,C'I'                                                     
         CLI   QCOMPARE,C'C'                                                    
         BNE   *+10                                                             
         MVC   HCAP2(9),=C'ACHIEVED '                                           
         CLI   QCOMPARE,C'C'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'D'                                                    
         BNE   *+10                                                             
         MVC   HCAP1(6),=C' PURCH'                                              
         CLI   QCOMPARE,C'E'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'F'                                                    
         BNE   *+10                                                             
         MVC   HCAP1(6),=C' ORDER'                                              
         XMOD1 1                                                                
         DROP  R7                                                               
         LTORG                                                                  
         EJECT                                                                  
* COMMERCIAL REPORT CONTROLLER                                                  
CMLRPT   CSECT                                                                  
         NMOD1 0,CMLRPT                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPF402+4096,R6                                                   
         XC    FLMWGHT,FLMWGHT                                                  
         LA    R9,FILMTAB          POINT TO START OF REPORT                     
         ST    R9,NEXTFLM                                                       
         MVI   PASS,2                                                           
         MVC   CURRBRK,=A(MKTBRK)                                               
         L     RE,CURRBRK                                                       
         OC    2(2,RE),2(RE)                                                    
         BZ    CMLRPT3                                                          
CMLRPT2  L     RE,CURRBRK          GET RECORDS FOR THIS BREAK                   
         LH    R0,2(RE)            GET MARKET NUMBER                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL,DUB                                                         
         MVI   KEY,C'0'            SET TO READ MARKET RECORD                    
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),FULL                                                    
         MVC   KEY+6(2),QAGY                                                    
         GOTO1 READMKT                                                          
         L     RE,ADMARKET                                                      
         USING MKTREC,RE                                                        
         OC    MKTWT,=X'F0F0F0F0'                                               
         CLC   MKTWT,=C'0000'                                                   
         BNE   *+10                                                             
         MVC   MKTWT,=C'0001'                                                   
         DROP  RE                                                               
         MVI   MODE,MKTFRST                                                     
         GOTO1 MEDADDWT,DMCB,(RA)                                               
         MVI   MODE,MKTLAST                                                     
         MVI   ACTSW,1                                                          
         L     R9,NEXTFLM                                                       
         MVC   SVQCMRCL(8),0(R9)                                                
         GOTO1 =V(SPF402),DMCB,(RA)                                             
*                                                                               
         CLI   BSUMFRST,0          SUM X-MARKET WEIGHTS                         
         BNE   *+16                                                             
         L     RE,SPWEIGHT                                                      
         A     RE,FLMWGHT                                                       
         ST    RE,FLMWGHT                                                       
*                                                                               
         L     RE,CURRBRK          GET THE NEXT BREAK FOR CURRENT CML           
         LA    RE,4(RE)                                                         
         ST    RE,CURRBRK                                                       
         OC    0(2,RE),0(RE)                                                    
         BNZ   CMLRPT2                                                          
         MVI   MODE,PRDLAST                                                     
         GOTO1 MEDADDWT,DMCB,(RA)                                               
         L     RE,ADMARKET                                                      
         USING MKTREC,RE                                                        
         MVC   SPWEIGHT,FLMWGHT                                                 
         L     R1,SPWEIGHT                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKTWT,DUB                                                        
         DROP  RE                                                               
         MVI   MODE,MKTLAST                                                     
         MVI   ACTSW,1                                                          
         MVC   MKT,=C'    '                                                     
         MVC   MKTNM(24),=CL24'ALL MARKETS'                                     
         GOTO1 =V(SPF402),DMCB,(RA)                                             
         XC    FLMWGHT,FLMWGHT                                                  
         MVC   CURRBRK,=A(MKTBRK)  RESET MARKET LIST                            
         L     R9,NEXTFLM          SET TO NEXT COMMERCIAL                       
         LA    R9,8(R9)                                                         
         ST    R9,NEXTFLM                                                       
         MVI   FORCEHED,C'Y'                                                    
         CLI   0(R9),X'00'         END OF CML/MKT PHASE                         
         BNE   CMLRPT2                                                          
         MVI   PASS,3                                                           
         GOTO1 =V(SPF402),DMCB,(RA)                                             
CMLRPT3  XMOD1 1                                                                
*                                                                               
FLMWGHT  DC    F'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
SWCTABC  CSECT                                                                  
         DS    40000C                                                           
GFOTABC  CSECT                                                                  
         DS    3000C                                                            
FPCTABC  CSECT                                                                  
         DS    7500C                                                            
         EJECT                                                                  
         BUFF LINES=2400,ROWS=5,COLUMNS=15,FLAVOR=BINARY,KEYLIST=(22,A)         
         EJECT                                                                  
         PRINT OFF                                                              
* SPREPWORKD                                                                    
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
* MEDPRTOPT                                                                     
       ++INCLUDE MEDRPTOPT                                                      
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
       ++INCLUDE SPGENCLT                                                       
* SPGENMKT                                                                      
       ++INCLUDE SPGENMKT                                                       
* DEDBLOCK                                                                      
       ++INCLUDE DEDBLOCK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'088SPREPF402 05/01/02'                                      
         END                                                                    
EJECT                                                                           
*                                                                               
* SPGENMKT                                                                      
