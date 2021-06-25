*          DATA SET SPREPM402  AT LEVEL 171 AS OF 09/18/19                      
*PHASE SPM402A                                                                  
*INCLUDE MEDMOVER                                                               
*INCLUDE SPRPFOOT                                                               
*INCLUDE MEDAPRNT                                                               
*INCLUDE REPSPILL                                                               
*INCLUDE COVAIL                                                                 
         TITLE 'SPREPM402-MARKET PERFORMANCE'                                   
*                                                                               
* REPORT TYPES                                                                  
*        01 - MONTHLY/QUARTERLY  SPOT LENGTH                                    
*        02 - MONTHLY/QUARTERLY  DAYPART                                        
*        03 - MONTHLY/QUARTERLY  DAYPART GROUP                                  
*        04 - MONTHLY/QUARTERLY  TOTAL                                          
*        05 - MONTHLY/QUARTERLY  PRIMARY DEMO SPOT LENGTH                       
*        06 - MONTHLY/QUARTERLY  PRIMARY DEMO DAYPART                           
*        07 - MONTHLY/QUARTERLY  PRIMARY DEMO DAYPART GROUP                     
*        08 - MONTHLY/QUARTERLY  PRIMARY DEMO TOTAL                             
*        09 - PERIOD SPOT LENGTH                                                
*        10 - PERIOD DAYPART                                                    
*        11 - PERIOD DAYPART GROUP                                              
*        12 - PERIOD TOTAL                                                      
*        13 - PERIOD PRIMARY DEMO SPOT LENGTH                                   
*        14 - PERIOD PRIMARY DEMO DAYPART                                       
*        15 - PERIOD PRIMARY DEMO DAYPART GROUP                                 
*        16 - PERIOD PRIMARY DEMO TOTAL                                         
*        17 - PERIOD CLIENT SPOT LENGTH                                         
*        18 - PERIOD CLIENT DAYPART                                             
*        19 - PERIOD CLIENT DAYPART GROUP                                       
*        20 - PERIOD CLIENT TOTAL                                               
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
*        1 = ALL REPORTS                                                        
*                                                                               
         EJECT                                                                  
SPM402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPM402,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         LA    R2,2048(RB)                                                      
         LA    R2,2048(R2)                                                      
         USING SPM402+4096,R2                                                   
         ST    R2,SPM4R2                                                        
         STM   RA,RC,SPM4RA                                                     
         ST    R5,RELO                                                          
         GOTO1 =V(SPRPFOOT),DMCB,(RA),RR=RELO                                   
         CLI   MODE,MKTLAST        GT. MKTLAST-GET WEIGHT                       
         BL    M1                                                               
         GOTO1 MEDADDWT,DMCB,(RA)                                               
         CLI   QOPT3,C'D'                                                       
         BNE   M1                                                               
         CLI   FRSTTOT,C'Y'                                                     
         BNE   M1                                                               
         MVI   SPDUPTOT,C'N'                                                    
         MVI   FRSTTOT,C'N'                                                     
M1       DS    0H                                                               
         SPACE 2                                                                
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
         CLI   MODE,RUNFRST                                                     
         BNE   M2                                                               
         MVC   FOOT1(132),SPACES                                                
         L     RE,MEDBUFF                                                       
         L     RF,=F'1272'                                                      
         XCEF                                                                   
*                                                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDNUMQT,=F'5'                                                   
         MVC   MEDNUMMO,=F'13'                                                  
         MVC   MEDNUMWK,=F'56'                                                  
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         LA    RF,MYHEAD                                                        
         ST    RF,HEADHOOK                                                      
         MVI   MEDEXTDM,4                                                       
         CLI   MEDEXTAV,C'Y'                                                    
         BNE   *+8                                                              
         MVI   MEDEXTDM,14                                                      
         DROP  RE                                                               
         SPACE 2                                                                
* SET UP ADDRESS CONSTANTS                                                      
         LA    RE,IEEQU                                                         
         ST    RE,AIEEQU                                                        
         LA    RE,EIEQU                                                         
         ST    RE,AEIEQU                                                        
         LA    RE,NEWDNAM                                                       
         ST    RE,ANEWDNAM                                                      
         LA    RE,MYBUFIO                                                       
         ST    RE,BUFFIO                                                        
         L     RE,=V(SETPDEM)                                                   
         A     RE,RELO                                                          
         ST    RE,VSETPDEM                                                      
         L     R7,BUFFBUFF                                                      
         L     R7,=V(BUFFALOC)                                                  
         A     R7,RELO                                                          
         ST    R7,BUFFBUFF                                                      
         USING BUFFALOD,R7                                                      
         LA    RE,RFMTBUFF                                                      
         ST    RE,BUFFHOOK                                                      
         DROP  R7                                                               
         LA    RE,MYBUFIO                                                       
         ST    RE,BUFFIO                                                        
         GOTO1 BUFFALO,DMCB,=C'SET',(R7)                                        
*                                                                               
         GOTO1 MEDSEED,DMCB,(RA)   SET UP REPORT TABLES                         
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   SVMEDFRS,MEDAFRST   SAVE SEEDED VALUES                           
         MVC   SVMEDMON,MEDMON01                                                
         MVC   SVMEDQTR,MEDQRT01                                                
         DROP  RE                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
M2       CLI   MODE,REQFRST                                                     
         BNE   M21                                                              
*                                                                               
         OI    RQOPTS,RQOPTS_POST    FLAG TO INDICATE SPOT POSTING              
*                                                                               
         L     RE,ADAGY                                                         
         CLI   AGYPROF+7-AGYHDR(RE),C'C' TEST CANADIAN                          
         JNE   *+8                                                              
         OI    RQOPT2,RQOPT2_NETBUYS                                            
*                                                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVI   MEDSPQRT,C'N'                                                    
         DROP  RE                                                               
         GOTO1 =V(SPRPFOOT),DMCB,(RA),RR=RELO                                   
         GOTO1 =V(RQFRST),DMCB,(RA),PSLIST,RR=RELO                              
         MVI   FCRDGOAL,C'Y'                                                    
         MVI   RQLKGLS,C'N'                                                     
         CLI   QOPT6,C'Y'          TEST READ LOCKED GOALS                       
         BNE   *+8                                                              
         MVI   RQLKGLS,C'Y'                                                     
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
M21      CLI   MODE,ESTFRST                                                     
         BNE   M3                                                               
         GOTO1 MEDPRDRD,DMCB,(RA)                                               
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
         B     *+8                 TESTING ONLY                                 
         MVI   SPOTPROF+8,3        FORCE A DAY                                  
         DROP  RE                                                               
*                                                                               
         MVC   RQSTAFLT(1),QAFFIL     AFFILATE FILTER                           
         MVC   RQPRGTYP,QPRGTYPE   PROGRAM TYPE FILTER                          
         L     RF,ADAGY                                                         
         USING AGYHDRD,RF                                                       
         L     RE,ADCLT            CHECK FOR US SPILL                           
         USING CLTHDR,RE                                                        
         CLI   CEXTRA+5,C'D'                                                    
         BE    *+8                                                              
         CLI   CEXTRA+5,C'Y'                                                    
         BE    *+8                                                              
         DROP  RE                                                               
         CLI   AGYPROF+7,C'C'      NO SPILL IF US AGENCY                        
         BE    *+8                                                              
         MVI   SPOTPROF+5,0                                                     
         DROP  RF                                                               
         CLI   QOPT5,C' '          SPILL REPORTING OVERRIDE                     
         BE    *+10                                                             
         MVC   PROGPROF+6(1),QOPT5                                              
         CLI   PROGPROF+6,C'N'                                                  
         BNE   *+8                                                              
         MVI   PROGPROF+6,0                                                     
         CLI   PROGPROF+6,0                                                     
         BE    *+10                                                             
         MVC   SPOTPROF+5(1),PROGPROF+6                                         
         NI    SPOTPROF+5,X'0F'                                                 
*                                                                               
         GOTO1 MEDCLEAR,DMCB,MEDTABLE                                           
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDEXTAX,SPOTPROF+12  SET TAX SWITCH                             
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         CLI   QOPT1,C'N'          SUPPRESS MONTHLY ANALYSIS                    
         BNE   M21A                                                             
*        MVC   BUFFCOLS,=F'16'     TAKEN OUT TO IMPLEMENT WAVE                  
*        MVC   BUFFWROW,=F'64'                                                  
         LA    R8,SUPMON                                                        
         BAS   R9,SUPRPTS                                                       
M21A     DS    0H                                                               
         L     RE,MEDBUFF                                                       
         L     RF,BUFFBUFF                                                      
         CLI   MEDEXTAV,C'Y'                                                    
         BNE   M21B                                                             
         MVC   BUFFCOLS,=F'32'                                                  
         MVC   BUFFWROW,=F'128'                                                 
         DROP  RE                                                               
         DROP  RF                                                               
M21B     CLI   QOPT2,C'N'          SUPPRESS BRAND DETAILS                       
         BNE   *+12                                                             
         LA    R8,SUPDET                                                        
         BAS   R9,SUPRPTS                                                       
         CLI   QDPTDET,C'C'                                                     
         BNE   *+8                                                              
         MVI   CPPSW,1                                                          
*                                                                               
         MVC   PAGE,=H'1'                                                       
*                                                                               
         BAS   RE,INITPD                                                        
         GOTO1 =V(BLDPDEM),DMCB,(RA),PSLIST,RR=RELO                             
         SPACE 2                                                                
         CLI   QCOMPARE,C'S'       SPECIAL VS REG DOES 1 DEMO                   
         BNE   M21C                                                             
         LA    RE,220                                                           
         L     RF,PRDBUFF                                                       
         XC    31(57,RF),31(RF)                                                 
         AH    RF,PRDBUFLN                                                      
         BCT   RE,*-10                                                          
         SPACE 2                                                                
M21C     LA    RE,219              SAVE POL DEMOS                               
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RF,0(RE,RF)                                                      
         ST    RF,SAVPOLA                                                       
         MVC   SAVPOL,28(RF)                                                    
         MVC   SAVPOLND,28(RF)                                                  
         SPACE 2                                                                
* CHECK FOR SECONDARY DEMO REPORTS - OLD FORMAT                                 
         MVI   BYTE,0              CHECK FOR 1 DEMO ONLY                        
NOSD     L     RE,PRDBUFF          CHECK FOR SECONDARY DEMOS                    
         LA    RF,220                                                           
         CLC   PRDBUFLN,=H'56'                                                  
         BNE   NOSDA               USE NEW LOGIC FOR NEW FORMAT                 
NOSD2    CLI   29(RE),0                                                         
         BNE   NOSD3                                                            
         LA    RE,56(RE)                                                        
         BCT   RF,NOSD2                                                         
         LA    R8,SUPSD            DELETE SECONDARY REPORTS                     
         BAS   R9,SUPRPTS                                                       
NOSD3    DS    0C                                                               
         L     RE,PRDBUFF                                                       
         LA    RF,220                                                           
ONEDM    CLI   28(RE),0                                                         
         BNE   ONEDM4                                                           
ONEDM2   LA    RE,56(RE)                                                        
         BCT   RF,ONEDM                                                         
         B     ONEDM10                                                          
ONEDM4   CLI   BYTE,0                                                           
         BNE   ONEDM6                                                           
         MVC   BYTE,28(RE)                                                      
         B     ONEDM2                                                           
*                                                                               
ONEDM6   CLC   BYTE(1),28(RE)                                                   
         BNE   ONEDMX                                                           
         B     ONEDM2                                                           
ONEDM10  LA    R8,SUPONEDM         SUPPRESS PRIMARY DEMO REPORTS                
         BAS   R9,SUPRPTS                                                       
*                                                                               
ONEDMX   DS    0H                                                               
         SPACE 2                                                                
* CHECK FOR SECONDARY DEMO REPORTS - NEW FORMAT                                 
NOSDA    XC    FULL,FULL                                                        
NOSD2A   CLI   32(RE),0            CHECK FOR SECONDARY DEMO                     
         BNE   NOSD3A                                                           
         AH    RE,PRDBUFLN                                                      
         BCT   RF,NOSD2A                                                        
         LA    R8,SUPSD            DELETE SECONDARY DEMO REPORTS                
         BAS   R9,SUPRPTS                                                       
NOSD3A   DS    0C                                                               
         L     RE,PRDBUFF                                                       
         LA    RF,220                                                           
ONEDMA   CLI   29(RE),0            CHECK FOR ANY PRIMARY DEMOS                  
         BNE   ONEDM4A                                                          
ONEDM2A  AH    RE,PRDBUFLN                                                      
         BCT   RF,ONEDMA                                                        
         B     ONEDM10A                                                         
ONEDM4A  OC    FULL,FULL                                                        
         BNZ   ONEDM6A                                                          
         MVC   FULL(3),28(RE)                                                   
         B     ONEDM2A                                                          
*                                                                               
ONEDM6A  CLC   FULL(3),28(RE)                                                   
         BNE   ONEDMXA                                                          
         B     ONEDM2A                                                          
ONEDM10A LA    R8,SUPONEDM         ALL PRIMARIES ARE THE SAME                   
         BAS   R9,SUPRPTS          DELETE PRIMARY DEMO REPORTS                  
*                                                                               
ONEDMXA  DS    0H                                                               
         CLI   ESTSW,C'N'                                                       
         BNE   EXIT                                                             
         MVI   ESTSW,C'Y'          LOCK MEDBLOCK DATES FOR REQUEST              
         SPACE 2                                                                
*                                                                               
         MVI   WORK,1                                                           
         L     R1,MEDBUFF                                                       
         USING MEDBLOCK,R1                                                      
         CLI   QOPT1,C'S'          SPECIAL QUARTERS                             
         BNE   *+8                                                              
         MVI   MEDSPQRT,C'Y'                                                    
         MVC   MEDAFRST(L'SVMEDFRS),SVMEDFRS     RESTORE SLOTS                  
         MVC   MEDMON01(L'SVMEDMON),SVMEDMON                                    
         MVC   MEDQRT01(L'SVMEDQTR),SVMEDQTR                                    
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDNUMQT,=F'5'                                                   
         MVC   MEDNUMMO,=F'13'                                                  
         GOTO1 MEDDATE,DMCB,(RA),0,0,1                                          
         L     R1,MEDBUFF                                                       
*                                                                               
         CLI   Q2LPMWK,C'Y'        LPMWK=Y NEED WEEKLY DATES                    
         BE    *+8                                                              
         CLI   Q2OVNITE,C'Y'       OVN NEED WEEKLY                              
         BE    *+8                                                              
         CLI   Q2USER+2,C'W'       WEEKLY TIME PERIOD                           
         BE    *+12                                                             
         LA    RE,MEDMON01         NEEDS WEEKS                                  
         ST    RE,MEDAFRST                                                      
*                                                                               
         MVC   SVMEDFRS,MEDAFRST   SAVE SEEDED VALUES                           
         MVC   SVMEDMON,MEDMON01                                                
         MVC   SVMEDQTR,MEDQRT01                                                
         SPACE 2                                                                
         LA    RE,MEDQRT01         SET MONTHS TO ADD TO TOTALS                  
         MVC   FULL,8(RE)                                                       
         LA    RE,MEDMON01                                                      
         LA    RF,13                                                            
SETMTOT  MVC   8(4,RE),FULL                                                     
         LA    RE,12(RE)                                                        
         BCT   RF,SETMTOT                                                       
         SPACE 2                                                                
         CLI   QOPT1,C'S'          JWT SPECIAL QUARTERS                         
         BE    *+8                                                              
         CLI   QOPT1,C'J'          TEST JWT FORMAT QUARTERLY                    
         BE    *+8                                                              
         CLI   QOPT1,C'Q'          PUT QTR BUCKETS IN MONTHS                    
         BNE   NOQTR               IF QUARTERLY REPORT REQUESTED                
         LA    RE,MEDMON01                                                      
         LA    RF,13                                                            
CLRMON   XC    0(4,RE),0(RE)       CLEAR OUT DATES ONLY                         
         LA    RE,12(RE)                                                        
         BCT   RF,CLRMON                                                        
         LA    RE,MEDMON01                                                      
         LA    R9,MEDQRT01                                                      
         LA    RF,5                                                             
MOVQTR   MVC   0(4,RE),0(R9)       FORCE MONTH SLOTS TO QUARTERS                
         MVC   8(4,RE),8(R9)       SET ADD TO SLOT                              
         LA    RE,12(RE)                                                        
         LA    R9,12(R9)                                                        
         BCT   RF,MOVQTR                                                        
NOQTR    LA    RF,13               CLEAR INACTIVE MONTH POINTERS                
         LA    RE,MEDMON01                                                      
CLRMSLOT CLI   0(RE),0                                                          
         BNE   *+10                                                             
         XC    0(12,RE),0(RE)                                                   
         LA    RE,12(RE)                                                        
         BCT   RF,CLRMSLOT                                                      
         LA    RE,MEDQRT01         CLEAR OUT QUARTERS                           
         LA    RF,5                                                             
QTRCLR   XC    0(12,RE),0(RE)                                                   
         LA    RE,12(RE)                                                        
         BCT   RF,QTRCLR                                                        
         XC    MEDNUMQT,MEDNUMQT                                                
         L     RE,MEDAFRST                                                      
QTRLAST  CLI   0(RE),0             RESET MEDALAST                               
         BE    ENDQTR                                                           
         ST    RE,MEDALAST                                                      
         LA    RE,12(RE)                                                        
         B     QTRLAST                                                          
         DROP  R1                                                               
ENDQTR   DS    0H'0'                                                            
*                                                                               
* OPTIMIZE BUFFALO                                                              
*                                                                               
* DAYPART SPOT LENGTH SUPPRESSION ROUTINES                                      
*                                                                               
CHKSL    CLI   QDPTDET,C'B'        SUPPRESS SPOT LENGTH                         
         BE    *+12                                                             
         CLI   QDPTDET,C'C'                                                     
         BNE   CHKDP                                                            
         LA    R8,SUPSL            DEACTIVATE SPOT LENGTH REPORTS               
         BAS   R9,SUPRPTS                                                       
*                                                                               
CHKDP    CLI   QDPTDET,C'C'        SUPPRESS DAYPART                             
         BNE   CHKSLTOT                                                         
         LA    R8,SUPDPSL          DEACTIVATE DAYPART REPORTS                   
         BAS   R9,SUPRPTS                                                       
*                                                                               
CHKSLTOT CLI   PROGPROF+5,C'Y'                                                  
         BE    M21EX                                                            
         LA    R8,SUPSLTOT                                                      
         BAS   R9,SUPRPTS                                                       
*                                                                               
M21EX    GOTO1 =V(SETBUF),DMCB,(RA),LCODE,LVCNTRL,RR=RELO                       
         L     R7,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'RESET',(R7)                                      
         MVC   HIGROUP,LCODE                                                    
         B     EXIT                                                             
         EJECT                                                                  
M3       CLI   MODE,PROCBUY                                                     
         BNE   M4                                                               
         L     RE,ADBUY                                                         
         USING BUYREC,RE                                                        
         CLI   QPRGTYPE,C' '       PROGRAM TYPE FILTER                          
         BE    *+14                                                             
         CLC   BDPROGT,QPRGTYPE                                                 
         BNE   EXIT                                                             
         DROP  RE                                                               
         LR    RF,RE                                                            
         BAS   R9,SETPOST                                                       
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
M322     LA    R7,PSLIST                                                        
M323     CLC   0(2,R7),=X'FFFF'     END                                         
         BE    EXIT                                                             
         CLI   0(R7),0             PRODUCT DELETED                              
         BNE   *+12                                                             
         LA    R7,2(R7)                                                         
         B     M323                                                             
         ZIC   RE,0(R7)                                                         
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF                                                       
         CLI   0(RE),0                                                          
         BNE   *+12                                                             
         LA    R7,2(R7)                                                         
         B     M323                                                             
         L     RE,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,RE                                                      
         MVC   MEDBRAND,0(R7)                                                   
         MVC   MEDSPTLN,1(R7)                                                   
* PW CLIENT STUFF                                                               
         MVI   MEDEXTPW,C' '                                                    
         MVI   MEDEXMMR,C' '                                                    
         CLI   QPWCV,C'A'                                                       
         BL    *+14                                                             
         MVC   MEDEXTPW,QPWCV                                                   
         MVI   MEDEXMMR,C'Y'                                                    
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
M323A    CLI   BPRD,X'FF'          PROCESSING POL                               
         BE    M3POK                YES - ACCEPT ALL PRODUCTS                   
         CLC   BPRD,MEDBRAND       SAME PRODUCT                                 
         BE    M3POK                YES - PROCESS                               
         LA    R7,2(R7)            NO - BYPASS                                  
         B     M323                                                             
M3POK    DS    0C                                                               
* CHECK FOR PURCH VS ACHIEVED                                                   
         CLI   QCOMPARE,C'C'                                                    
         BE    M3ACH                                                            
         CLI   QCOMPARE,C'D'                                                    
         BE    M3ACH                                                            
M323B    GOTO1 MEDGETBY,DMCB,(RA),2     ANY ORDERED                             
         GOTO1 SETPRMY                                                          
         L     RE,MEDBUFF                NO - BYPASS                            
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD                                                
         BZ    M323C                                                            
         SPACE 2                                                                
         L     R1,ADBUY                                                         
         USING BUYREC,R1                                                        
         CLI   QCOMPARE,C'S'       SPECIAL VS REG ANALYSIS                      
         BNE   M323B1                                                           
         CLI   BDPROGT-1,0         POST SPECIALS IN GOAL BUCKETS                
         BE    M3ACH                                                            
         DROP  R1                                                               
         SPACE 2                                                                
M323B1   CLI   MEDSPILL,C'Y'                                                    
         BNE   M323NOSP                                                         
         CLI   SPOTPROF+5,0                                                     
         BE    EXIT                                                             
         CLI   SPLPRINT,1          PUT ORIGINATING IN BUFFER                    
         BNE   M323NOSP                                                         
         L     RE,=A(SPBUFMKT)                                                  
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'01',(RE)),0,RR=RELO                    
         MVI   SPLPRINT,2                                                       
M323NOSP GOTO1 MEDGETBY,DMCB,(RA),(R3)                                          
         GOTO1 SETPRMY                                                          
         MVC   ACTAREA,4(R1)                                                    
         L     RE,MEDBUFF          CHECK FOR ACTIVITY                           
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD                                                
         BZ    M323C                                                            
         CLI   QOPT4,C'Y'          OPTION TO SUPPRESS DOLLARS                   
         BNE   *+8                                                              
         BAS   RE,CLRDOLS                                                       
         MVI   STACTSW,1                                                        
         MVI   ACTSW,1                                                          
*                                                                               
         BAS   R9,POST                                                          
M323C    LA    R7,2(R7)                                                         
         B     M323                                                             
         EJECT                                                                  
* GET PURCHASED AND POST IN GOAL BUCKETS                                        
M3ACH    GOTO1 MEDGETBY,DMCB,(RA),2                                             
         L     RE,MEDBUFF                                                       
         LA    RF,MEDPERD                                                       
         L     R4,4(RF)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD   ANY ACTIVITY                                 
         BZ    M323C               NO - NEXT BRAND                              
         CLI   MEDSPILL,C'Y'                                                    
         BNE   *+12                                                             
         CLI   SPOTPROF+5,0        SUPPRESS SPILL                               
         BE    EXIT                                                             
*                                                                               
         CLI   MEDSPILL,C'Y'       CHECK IF SPILL ACTIVE                        
         BNE   M3ACH1                FOR RERATE                                 
         GOTO1 MEDGETBY,DMCB,(RA),(R3)                                          
         OC    MEDBYD(12),MEDBYD   ANY ACTIVITY                                 
         BZ    M323C               NO - NEXT BRAND                              
         GOTO1 MEDGETBY,DMCB,(RA),2                                             
*                                                                               
M3ACH1   GOTO1 SETPRMY                                                          
         CLI   QOPT4,C'Y'          OPTION TO SUPPRESS DOLLARS                   
         BNE   *+8                                                              
         BAS   RE,CLRDOLS                                                       
         GOTO1 =V(VMDMOVER),DMCB,(RA),RR=RELO                                   
         BAS   R9,POST                                                          
         B     M323B                                                            
         EJECT                                                                  
M4       CLI   MODE,PROCGOAL                                                    
         BNE   M5                                                               
         CLI   QCOMPARE,C'C'       PURCHASED ONLY                               
         BE    EXIT                                                             
         CLI   QCOMPARE,C'D'       PURCHASED ONLY                               
         BE    EXIT                                                             
         LA    RE,KEY                                                           
         USING GOALREC,RE                                                       
         LA    RF,1(RE)                                                         
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
*                                                                               
         CLI   QPWCV,C'Y'                                                       
         BNE   *+8                                                              
         MVI   MEDEXTPW,C'Y'                                                    
*                                                                               
         GOTO1 MEDGETGL,DMCB,(RA)                                               
         GOTO1 SETPRMY                                                          
         L     RF,MEDBUFF          CHECK FOR ACTIVITY                           
         LA    RE,MEDPERD                                                       
         L     R4,4(RE)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDGLD(12),MEDGLD                                                
         BZ    EXIT                                                             
         CLI   QFILTER,C'F'        DOING FILM TYPE FILTERING                    
         BNE   *+12                NO - SET ACTIVE                              
         CLI   ACTSW,0             FILM TYPE AND NO BUYS                        
         BE    EXIT                                                             
         MVI   ACTSW,1                                                          
         CLI   QOPT4,C'Y'          OPTION TO SUPPRESS DOLLARS                   
         BNE   *+8                                                              
         BAS   RE,CLRDOLS                                                       
M42      BAS   R9,POST                                                          
         B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
*                                                                               
* EXTRACT LOCKIN DATA AND THEN REFORMAT AS GOAL                                 
*                                                                               
M4L      L     RF,MEDBUFF                                                       
         MVI   MEDSPILL,C'O'                                                    
         MVC   SAVNUMWK,MEDNUMWK   GETLK DOESN'T LIKE OR NEED WEEKS             
         XC    MEDNUMWK,MEDNUMWK   HERE - SO KILL THEM                          
         GOTO1 MEDGETLK,DMCB,(RA)                                               
         L     RF,MEDBUFF                                                       
         MVC   MEDNUMWK,SAVNUMWK                                                
         GOTO1 SETPRMY                                                          
         L     RF,MEDBUFF          CHECK FOR ACTIVITY                           
         LA    RE,MEDPERD                                                       
         L     R4,4(RE)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDLKSPT,MEDLKSPT                                                
         BZ    M4L2                                                             
         MVI   ACTSW,1                                                          
         CLI   QOPT4,C'Y'          OPTION TO SUPPRESS DOLLARS                   
         BNE   *+8                                                              
         BAS   RE,CLRDOLS                                                       
*                                                                               
         GOTO1 =V(VMDMOVER),DMCB,(RA),RR=RELO                                   
         BAS   R9,POST                                                          
M4L2     CLI   SPOTPROF+5,0                                                     
         BE    EXIT                                                             
         L     RF,MEDBUFF                                                       
         MVI   MEDSPILL,C'S'                                                    
         MVC   SAVNUMWK,MEDNUMWK   GETLK DOESN'T LIKE OR NEED WEEKS             
         XC    MEDNUMWK,MEDNUMWK   HERE - SO KILL THEM                          
         GOTO1 MEDGETLK,DMCB,(RA)                                               
         L     RF,MEDBUFF                                                       
         MVC   MEDNUMWK,SAVNUMWK                                                
         GOTO1 SETPRMY                                                          
         L     RF,MEDBUFF          CHECK FOR ACTIVITY                           
         LA    RE,MEDPERD                                                       
         L     R4,4(RE)                                                         
         USING MEDDATA,R4                                                       
         OC    MEDLKSPT,MEDLKSPT                                                
         BZ    EXIT                                                             
         MVI   ACTSW,1                                                          
         CLI   QOPT4,C'Y'          OPTION TO SUPPRESS DOLLARS                   
         BNE   *+8                                                              
         BAS   RE,CLRDOLS                                                       
*                                                                               
         GOTO1 =V(VMDMOVER),DMCB,(RA),RR=RELO                                   
         B     M42                                                              
         DROP  RF                                                               
         EJECT                                                                  
POST     MVC   FULL,WEIGHT                                                      
         GOTO1 =V(POSTER),DMCB,(RA),PSLIST,RR=RELO                              
         BR    R9                                                               
         EJECT                                                                  
M5       MVI   BUFCDE,X'21'                                                     
         CLI   MODE,MKTLAST                                                     
         BNE   M6                                                               
         CLI   ACTSW,1                                                          
         BNE   EXIT                                                             
         MVI   BUFCDE,X'21'                                                     
         MVI   RCSUBPRG,1                                                       
         MVI   LCODE,1                                                          
         CLI   QOPT3,C'D'                                                       
         BE    *+12                                                             
         BAS   R9,DOSUM                                                         
         B     M53                                                              
         SPACE 2                                                                
         L     RE,=A(SPBUFMKT)                                                  
         LA    RF,750                                                           
         XCEF                                                                   
*                                                                               
M53      L     R3,ACTAREA                                                       
         MVI   FORCEHED,C'N'                                                    
         GOTO1 =V(VMDAPRNT),DMCB,(RA),(R3),1,RR=RELO                            
         SPACE 2                                                                
         L     RE,=A(SPBUFMKT)                                                  
         OC    0(8,RE),0(RE)    CHECK FOR SPILL                                 
         BZ    M5NOSP                                                           
         L     RF,=V(DICSECT)                                                   
         USING DICSECT,RF                                                       
         MVC   P1(L'SP@SPILL),SP@SPILL                                          
         DROP  RF                                                               
*                                                                               
         L     RE,=A(SPBUFMKT)                                                  
         GOTO1 =V(REPSPILL),DMCB,(RA),(X'02',(RE)),P1,RR=RELO                   
         GOTO1 REPORT                                                           
         SPACE 2                                                                
M5NOSP   MVC   DMCB+8(20),LVCNTRL                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   FRSTTOT,C'Y'                                                     
         L     R3,BUFFBUFF                                                      
         TM    DMCB+8,X'80'        ONLY 1 LEVEL                                 
         BO    M53A                 YES - DONT ADD                              
         MVI   BUFCDE,X'21'                                                     
         BAS   R9,ADDBUFF                                                       
         MVI   BUFCDE,X'22'                                                     
         BAS   R9,ADDBUFF                                                       
         MVI   BUFCDE,X'41'                                                     
         BAS   R9,ADDBUFF                                                       
         MVI   BUFCDE,X'42'                                                     
         BAS   R9,ADDBUFF                                                       
         MVI   BUFCDE,X'61'                                                     
         BAS   R9,ADDBUFF                                                       
         MVI   BUFCDE,X'62'                                                     
         BAS   R9,ADDBUFF                                                       
*                                                                               
M53A     LA    R5,1                                                             
         MVI   BUFCDE,X'21'                                                     
         BAS   R9,CLEARBUF                                                      
         MVI   BUFCDE,X'22'                                                     
         BAS   R9,CLEARBUF                                                      
         MVI   BUFCDE,X'41'                                                     
         BAS   R9,CLEARBUF                                                      
         MVI   BUFCDE,X'42'                                                     
         BAS   R9,CLEARBUF                                                      
         MVI   BUFCDE,X'61'                                                     
         BAS   R9,CLEARBUF                                                      
         MVI   BUFCDE,X'62'                                                     
         BAS   R9,CLEARBUF                                                      
         B     EXIT                                                             
         SPACE 2                                                                
M6       CLI   QOPT3,C'S'          SUPPRESS MGR SUMMARIES                       
         BE    M13                                                              
         CLI   MODE,MGR3LAST                                                    
         BNE   M7                                                               
         MVI   LCODE,5                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
M7       CLI   MODE,MGR2LAST                                                    
         BNE   M8                                                               
         MVI   LCODE,4                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
M8       CLI   MODE,MGR1LAST                                                    
         BNE   M13                                                              
         MVI   LCODE,3                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
M12      DS    0C                                                               
M13      CLI   MODE,PRDLAST                                                     
         BNE   M14                                                              
         TM    QMKT,X'F0'                                                       
         BO    EXIT                                                             
         MVI   RCSUBPRG,7                                                       
         MVI   LCODE,2                                                          
         BAS   R9,DOSUM                                                         
         B     EXIT                                                             
M14      CLI   MODE,PRDFRST                                                     
         BNE   M15                                                              
*                                                                               
M15      CLI   MODE,MKTFRST                                                     
         BNE   M16                                                              
         MVI   ACTSW,0                                                          
         L     RE,=A(SPBUFMKT)                                                  
         LA    RF,750                                                           
         XCEF                                                                   
         XC    SPWEIGHT,SPWEIGHT                                                
*                                                                               
M16      CLI   MODE,REQLAST                                                     
         BNE   M17                                                              
         MVI   P,0                                                              
         MVC   P2,FOOT1                                                         
         MVI   FORCEHED,C'N'                                                    
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
M17      CLI   MODE,STAFRST                                                     
         BNE   M18                                                              
         MVI   STACTSW,0                                                        
         MVI   SPLPRINT,1                                                       
*                                                                               
M18      CLI   MODE,STALAST                                                     
         BNE   M19                                                              
         CLI   STACTSW,1                                                        
         BNE   EXIT                                                             
         CLI   QOPT3,C'D'          DONT DO ACTUAL BOOKS IF SUMMARY ONLY         
         BE    EXIT                                                             
         L     R8,ACTAREA                                                       
         GOTO1 =V(VMDAPRNT),DMCB,(RA),(R8),0,RR=RELO                            
*                                                                               
M19      B     EXIT                                                             
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
         DROP  R4,RF                                                            
         EJECT                                                                  
DOSUM    L     R4,BUFFIO           DO SUMMARY REPORTS                           
         OC    SAVPOLA,SAVPOLA                                                  
         BZR   R9                                                               
         ST    R9,SAVE9                                                         
         MVI   PDEMCNT,0                                                        
         MVI   SECDMSW,0                                                        
         MVC   WEIGHT,SPWEIGHT                                                  
         MVI   SW1,0                                                            
         MVI   SW2,0                                                            
         MVI   WTSW,0                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVI   HEADSW,C'M'                                                      
         MVI   PBCDE1,0                                                         
         XC    PRIMDEM,PRIMDEM                                                  
         MVI   PBUFFCDE,0                                                       
         CLI   QOPT1,C'N'                                                       
         BNE   *+8                                                              
         MVI   HEADSW,C'P'                                                      
         CLC   HIGROUP,LCODE                                                    
         BLR   R9                                                               
REDOSUM  L     R3,BUFFBUFF         RESTORE POINTERS                             
         L     R4,BUFFIO                                                        
         L     R9,SAVE9                                                         
         CLI   SPDUPTOT,C'Y'                                                    
         BE    DOSUM31                                                          
         LA    RE,MYBUFIO                                                       
         LA    RF,600                                                           
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
         MVI   PREVMO,0                                                         
         CLC   0(1,R4),BUFCDE                                                   
         BNE   DS2NEXT                                                          
         TM    8(R1),X'80'                                                      
         BO    DS2NEXT                                                          
         CLI   MULTISW,C'Y'                                                     
         BNE   DOSUM2                                                           
         LA    RE,P1                                                            
         L     RF,=V(DICSECT)                                                   
         USING DICSECT,RF                                                       
         MVC   0(L'SP@COMTV,RE),SP@COMTV                                        
         CLI   0(R4),X'60'                                                      
         BH    DS2PRT                                                           
         MVC   0(L'SP@NETTV,RE),SP@NETTV                                        
         CLI   0(R4),X'40'                                                      
         BH    DS2PRT                                                           
         MVC   0(L'SP@SPTTV,RE),SP@SPTTV                                        
         DROP  RF                                                               
DS2PRT   GOTO1 REPORT                                                           
         B     DOSUM2                                                           
DS2NEXT  MVC   BUFCDE,0(R4)                                                     
         CLI   BUFCDE,X'21'        CHECK FOR END WHEN SUBORDINATE               
         BL    DOSUMX               REPORT                                      
         CLI   BUFCDE,X'62'                                                     
         BH    DOSUMX                                                           
         TM    DMCB+8,X'80'                                                     
         BO    DOSUMX                                                           
         B     REDOSUM                                                          
*                                                                               
DOSUM1   SR    R8,R8                                                            
         IC    R8,LCODE                                                         
         L     R4,BUFFIO                                                        
         GOTO1 BUFFALO,DMCB,=C'SEQ',(R3),(R4),(R8)                              
DOSUM2   TM    DMCB+8,X'80'                                                     
         BO    DOSUM31                                                          
         MVC   SPWEIGHT,WEIGHT                                                  
         CLC   0(1,R4),BUFCDE                                                   
         BH    DOSUM31                                                          
         LA    RE,MYBUFIO+20                                                    
         LA    RF,27                                                            
DOS2NEG  CLI   0(RE),X'FF'         WIPE OUT NEGATIVE DEMOS                      
         BNE   *+10                                                             
         XC    0(4,RE),0(RE)                                                    
         LA    RE,4(RE)                                                         
         BCT   RF,DOS2NEG                                                       
*                                                                               
DOSUM2A1 L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDBRAND,MYBUFIO+7                                               
         CLC   MYBUFIO+1(1),PBCDE2                                              
         BE    DS2                                                              
         MVI   PREVBRND,0                                                       
         MVI   BRNDCNTR,0                                                       
         MVI   PBUFFCDE,0                                                       
DS2      DS    0H                                                               
         MVC   PBCDE2,MYBUFIO+1                                                 
         CLC   PRIMDEM(1),MYBUFIO+2                                             
         BE    DS2A                                                             
         MVI   PREVBRND,0                                                       
         MVI   BRNDCNTR,0                                                       
*        CLI   SECDMSW,1                                                        
*        BNE   *+8                                                              
*        MVI   HEADSW,C'X'                                                      
         MVI   SECDMSW,0                                                        
         CLI   MYBUFIO+2,X'FF'                                                  
         BE    DS2A                                                             
         ZIC   RE,PDEMCNT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,PDEMCNT                                                       
DS2A     DS    0H                                                               
         CLC   PREVBRND,MYBUFIO+7  SAME BRAND                                   
         BE    DOSUM2A2                                                         
         CLI   MYBUFIO+7,X'FF'                                                  
         BE    DOSUM2A2                                                         
         CLI   MYBUFIO+3,0         BRAND LINE                                   
         BNE   DOSUM2A2                                                         
         ZIC   RE,BRNDCNTR                                                      
         LA    RE,1(RE)                                                         
         STC   RE,BRNDCNTR                                                      
DOSUM2A2 MVC   PREVBRND,MYBUFIO+7                                               
         CLI   BRNDCNTR,1                                                       
         BNE   *+12                                                             
         CLI   MYBUFIO+7,X'FF'                                                  
         BE    DOSUM1                                                           
         CLI   PDEMCNT,1                                                        
         BNE   *+12                                                             
         CLI   MYBUFIO+2,X'FF'                                                  
         BE    DOSUM1                                                           
         DROP  RE                                                               
         CLC   MYBUFIO(1),PBCDE1                                                
         BNE   DOSUM2A                                                          
         CLC   MYBUFIO+2(2),PRIMDEM                                             
         BNE   DOSUM2A                                                          
         CLC   MYBUFIO+7(1),PBUFFCDE                                            
         BE    DOSUM23                                                          
*                                                                               
* GET PRODUCT AND DEMO NAMES                                                    
DOSUM2A  MVC   PBUFFCDE,MYBUFIO+7                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVI   MEDDPCNT,0                                                       
         MVI   MEDSLCNT,0                                                       
         DROP  RE                                                               
         MVI   ALLOWLIN,15                                                      
         TM    MYBUFIO,2                                                        
         BZ    *+16                                                             
         CLI   HEADSW,C'P'                                                      
         BE    *+8                                                              
         MVI   HEADSW,C'X'                                                      
         SR    RE,RE                                                            
         IC    RE,PBUFFCDE                                                      
         CLI   PBUFFCDE,X'FF'                                                   
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         LA    RE,0(RE,RF)                                                      
         MVI   SW2,0               SET FOR NORMAL                               
         CLI   PBUFFCDE,X'FF'                                                   
         BE    SETPD                                                            
         MVC   MID1(3),1(RE)       SET PRODUCT CODE                             
         MVI   MID1+3,C'-'                                                      
         MVC   MID1+4(20),4(RE)    SET PRODUCT NAME                             
         MVI   MID1+24,C'('                                                     
         MVI   MID1+32,C')'                                                     
* GET DEMO NAMES                                                                
         LA    R6,28(RE)                                                        
         OC    0(3,R6),0(R6)                                                    
         BZ    DOSUM22                                                          
         GOTO1 ANEWDNAM,DMCB,(4,(R6)),WORK                                      
         MVC   MID1+25(7),WORK                                                  
         MVC   SVPNAME(7),WORK                                                  
         TM    MYBUFIO,1           IS THIS A MONTHLY RECORD                     
         BO    DOSUM22                                                          
         CLI   HEADSW,C'P'                                                      
         BE    *+8                                                              
         MVI   HEADSW,C'X'                                                      
         LA    RF,MID1+91          ADD DEMO NAMES FOR PERIOD                    
         LA    R9,3                                                             
         LA    RE,WORK+7                                                        
DOSUM21  MVC   0(7,RF),0(RE)                                                    
         LA    RF,15(RF)                                                        
         LA    RE,7(RE)                                                         
         BCT   R9,DOSUM21                                                       
DOSUM22  MVI   FORCEMID,C'Y'                                                    
         LA    RE,MID1                                                          
         LA    RF,34                                                            
DOS22A   CLI   0(RE),0                                                          
         BNE   *+8                                                              
         MVI   0(RE),C' '                                                       
         LA    RE,1(RE)                                                         
         BCT   RF,DOS22A                                                        
         GOTO1 SQUASHER,DMCB,MID1,34                                            
DOSUM23  DS    0H                                                               
         MVC   PRIMDEM,MYBUFIO+2                                                
         MVC   PBCDE1,MYBUFIO                                                   
         TM    MYBUFIO,2                                                        
         BZ    DOSUM23A                                                         
         CLI   MYBUFIO+7,X'FF'                                                  
         BNE   DOSUM23A                                                         
         CLI   MYBUFIO+3,0         SECONDARY DEMO SUMMARY                       
         BNE   DOSUM23A                                                         
         LA    RE,MYBUFIO                                                       
         USING SP14BUF,RE                                                       
         XC    BPBD2(24),BPBD2     SUPPRESS SUBORDINATE DEMOS                   
DOSUM23A DS    0H                                                               
DOSUM24  DS    0C                                                               
         CLI   MYBUFIO+8,X'FF'     IS IT A TOTAL LINE                           
         BNE   DOSUM25                                                          
         GOTO1 =V(SUPCPP),DMCB,(RA),CPPSW,MYBUFIO,RR=RELO                       
DOSUM25  CLI   PRIMDEM+1,0         SECONDARY DEMO LINE                          
         BE    DOSUM25A            NO                                           
         MVI   BUFRECSW,C'T'       SET TYPE SWITCH                              
         GOTO1 =V(UNWGHT),DMCB,(RA),PSLIST,RR=RELO                              
         MVC   SPWEIGHT,=F'1'                                                   
DOSUM25A DS    0H                                                               
         GOTO1 MEDEDIT,DMCB,(RA)                                                
         CLI   DMCB,0                                                           
         BE    DOSUM1                                                           
         MVC   SPACING,DMCB                                                     
         CLI   QOPT1,C'J'          TEST FOR JWT QUARTERLY                       
         BNE   DOSUM25B                                                         
         SPACE 2                                                                
         TM    MYBUFIO,X'01'       CHECK FOR MONTHLY DATA                       
         BZ    DOSUM25B                                                         
         MVC   WTLIST(1),SVPNAME                                                
         MVI   BUFRECSW,C'M'       SET TYPE SWICTH                              
         GOTO1 =V(UNWGHT),DMCB,(RA),PSLIST,RR=RELO                              
         LA    RE,MYBUFIO                                                       
         LA    R8,BM1BDL                                                        
         LA    R9,BM1BD1                                                        
         LA    R1,P1+42                                                         
         BAS   RE,CALCPP                                                        
         SPACE 2                                                                
         LA    RE,MYBUFIO                                                       
         LA    R8,BM2BDL                                                        
         LA    R9,BM2BD1                                                        
         LA    R1,P1+82                                                         
         BAS   RE,CALCPP                                                        
         SPACE 2                                                                
         LA    RE,MYBUFIO                                                       
         LA    R8,BM3BDL                                                        
         LA    R9,BM3BD1                                                        
         LA    R1,P1+122                                                        
         BAS   RE,CALCPP                                                        
         SPACE 2                                                                
DOSUM25B CLI   HEADSW,C'X'                                                      
         BNE   DOSUM26                                                          
         ZIC   RE,LINE                                                          
         ZIC   RF,ALLOWLIN                                                      
         AR    RE,RF                                                            
         ZIC   RF,MAXLINES                                                      
         CR    RE,RF                                                            
         BL    *+12                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     DOSUM26                                                          
         BAS   RE,PERHED                                                        
         B     DOSUM27                                                          
DOSUM26  CLI   MYBUFIO+1,4                                                      
         BH    DOSUM261                                                         
         CLC   MYBUFIO+1(1),PREVMO                                              
         BE    DOSUM261                                                         
         MVC   PREVMO,MYBUFIO+1                                                 
         CLI   FORCEHED,C'Y'       SET HEAD FOR HEAD HOOK                       
         BE    DOSUM27                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,LINE                                                          
         IC    RF,MAXLINES                                                      
         SR    RF,RE                                                            
         CH    RF,=H'15'                                                        
         BH    *+12                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     DOSUM27                                                          
         MVC   PREVMO,MYBUFIO+1                                                 
         MVI   HEADSW,C'M'                                                      
         BAS   RE,MONHED                                                        
         B     DOSUM27                                                          
DOSUM261 CLI   FORCEMID,C'Y'       SET PRODUCT LINE                             
         BNE   DOSUM27                                                          
         CLI   FORCEHED,C'Y'                                                    
         BE    DOSUM27                                                          
         MVI   SW2,1               PUT PRODUCT LINE IN P2                       
         MVC   P3,P1                                                            
         XC    P1,P1                                                            
         MVC   P2,MID1                                                          
         MVI   FORCEMID,C'N'                                                    
DOSUM27  DS    0H                                                               
         TM    P1+9,X'F0'                                                       
         BO    DOSUM3A                                                          
         GOTO1 MEDSTARS,DMCB,P1                                                 
DOSUM3A  DS    0H                                                               
         GOTO1 REPORT                                                           
         GOTO1 =V(SPRPFOOT),DMCB,(RA),RR=RELO                                   
         MVI   SW2,0                                                            
         MVI   ALLOWLIN,0                                                       
         LA    RE,MYBUFIO+16                                                    
         LA    RF,388                                                           
         XCEF                                                                   
         B     DOSUM1                                                           
DOSUM3   MVI   P,X'00'                                                          
         GOTO1 REPORT                                                           
DOSUM31  SR    R5,R5                                                            
         IC    R5,LCODE                                                         
         CLI   LCODE,1                                                          
         BE    DOSUM4A                                                          
DOSUM4   BAS   R9,CLEARBUF                                                      
DOSUM4A  CLI   BUFCDE,X'62'                                                     
         BE    DOSUMX                                                           
         TM    BUFCDE,X'02'                                                     
         BZ    DOSUM4B                                                          
         ZIC   RE,BUFCDE                                                        
         LA    RE,31(RE)                                                        
         STC   RE,BUFCDE                                                        
         B     REDOSUM                                                          
         SPACE 2                                                                
DOSUM4B  ZIC   RE,BUFCDE                                                        
         LA    RE,1(RE)                                                         
         STC   RE,BUFCDE                                                        
         B     REDOSUM                                                          
         SPACE 2                                                                
DOSUMX   MVI   FORCEHED,C'Y'                                                    
DOSUM5   L     R9,SAVE9                                                         
         L     RF,SAVPOLA                                                       
         MVC   28(4,RF),SAVPOL                                                  
         CLC   PRDBUFLN,=H'56'                                                  
         BNER  R9                                                               
         MVC   28(12,RF),SAVPOLND  RESET POL - NEW FORMAT DEMOS                 
         BR    R9                                                               
         SPACE 2                                                                
SETPOST  MVC   POSTWORK,=X'2122'   SET UP FOR SECONDARY POST                    
         MVC   HALF,0(RF)                                                       
         NI    HALF,X'0F'                                                       
         CLI   HALF,X'08'          CHECK FOR COMBINED                           
         BNE   *+12                                                             
         MVC   POSTWORK,=X'6162'                                                
         BR    R9                                                               
         SPACE 2                                                                
         CLI   HALF,1              IS IT SPOT                                   
         BE    *+10                                                             
         MVC   POSTWORK,=X'4142'   SET UP FOR NETWORK                           
         BR    R9                                                               
         EJECT                                                                  
* CALCULATE MONTHLY CPP FOR JWT                                                 
CALCPP   ST    RE,SAVERE                                                        
         OC    0(4,R9),0(R9)                                                    
         BZR   RE                                                               
         CLI   MYBUFIO+16,X'FF'    EQUIVALENCED CPP/M                           
         BNE   CALCPP2                                                          
         SR    RE,RE                                                            
         L     RF,0(R8)                                                         
         M     RE,=F'10'                                                        
         D     RE,4(R9)                                                         
         CLC   0(4,R9),4(R9)       CHECK FOR EQUIVALENCE                        
         BE    *+8                                                              
         MVI   9(R1),C'+'                                                       
         B     CALCPP10                                                         
         SPACE 2                                                                
CALCPP2  SR    RE,RE               UNEQUIVALENCED CPP/M                         
         L     RF,0(R8)                                                         
         M     RE,=F'10'                                                        
         D     RE,0(R9)                                                         
CALCPP10 LR    R9,R1                                                            
         MVI   CURTAB+3,2                                                       
         CURED (RF),(09,(R9)),CURTAB                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
CLEARBUF GOTO1 BUFFALO,DMCB,=C'CLEAR',(BUFCDE,(R3)),(X'80',(R5))                
         BR    R9                                                               
         SPACE 2                                                                
ADDBUFF  GOTO1 BUFFALO,DMCB,=C'ADD',(BUFCDE,(R3))                               
         BR    R9                                                               
         EJECT                                                                  
SETPD    SR    R6,R6                                                            
         CLI   MYBUFIO+2,X'FF'                                                  
         BNE   SETPD1                                                           
         L     RF,=V(DICSECT)                                                   
         USING DICSECT,RF                                                       
         MVC   MID1(L'SP@ALBRN),SP@ALBRN                                        
         DROP  RF                                                               
         B     SETPD2                                                           
SETPD1   DS    0H                                                               
         CLI   MYBUFIO+3,0                                                      
         BNE   SETPDD                                                           
         LA    R6,MYBUFIO+2        GET PRIMARY DEMO NAME                        
         ST    RE,SAVERE                                                        
         GOTO1 AIEEQU,DMCB,(1,(R6)),FULL                                        
         GOTO1 ANEWDNAM,DMCB,(1,FULL),WORK                                      
         L     RE,SAVERE                                                        
         MVC   MID1(7),WORK                                                     
         MVC   SVPNAME(7),WORK                                                  
         L     RF,=V(DICSECT)                                                   
         USING DICSECT,RF                                                       
         MVC   MID1+8(L'SP@BRAND),SP@BRAND                                      
         DROP  RF                                                               
SETPD2   XC    28(4,RE),28(RE)                                                  
         MVC   28(3,RE),FULL                                                    
         MVC   0(1,RE),MYBUFIO+2                                                
         ST    RE,SAVPOLA                                                       
         B     DOSUM22                                                          
         SPACE 2                                                                
* SET MIDLINES FOR SECONDARY DEMO SUMMARYS                                      
SETPDD   L     R4,APDTCNT          GET PRIMARY DEMO LIST                        
         L     R4,0(R4)                                                         
         L     R5,APDTAB                                                        
         GOTO1 BINSRCH,DMCB,(0,MYBUFIO+2),(R5),(R4),24,(0,1),25                 
*                                   SHOULD HAVE BEEN PUT THERE                  
*                                   BY BLDPDEM                                  
         GOTO1 =V(PDMLINE),DUB,(RA),PSLIST,RR=RELO                              
         MVC   PRIMDEM,MYBUFIO+2                                                
         MVC   PBCDE1,MYBUFIO                                                   
         B     DOSUM25                                                          
         LTORG                                                                  
EXIT     XMOD1 1                                                                
         EJECT                                                                  
MONHED   NTR1                                                                   
         GOTO1 =V(MONHEDC),DMCB,(RA),PSLIST,RR=RELO                             
         XIT1                                                                   
         EJECT                                                                  
PERHED   NTR1                      PERIOD HEADINGS                              
         GOTO1 =V(PERHEDC),DMCB,(RA),PSLIST,RR=RELO                             
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* FORCE PRIMARY DEMOS INTO SEQUENCE                                             
INITPD   NTR1                                                                   
         L     R6,PRDBUFF                                                       
         LA    R7,218                                                           
*                                                                               
INITPD2  CLI   0(R6),0             CHECK FOR ACTIVE PRODUCT                     
         BE    INITPD6                                                          
         LA    R4,PRMYTAB                                                       
         MVC   FULL,28(R6)         SET PRIMARY DEMO                             
*                                                                               
INITPD3  CLI   1(R4),0             END OF DEMO SLOTS                            
         BE    INITPD5                                                          
         CLC   0(3,R4),FULL                                                     
         BE    INITPD5             ALREADY THERE                                
         BL    INITPD4             ADD LATER                                    
*                                                                               
         MVC   DUB(3),0(R4)        SORT INTO ORDER                              
         MVC   0(3,R4),FULL                                                     
         MVC   FULL(3),DUB                                                      
         B     INITPD3                                                          
*                                                                               
INITPD4  LA    R4,3(R4)            TRY NEXT SLOT                                
         B     INITPD3                                                          
*                                                                               
INITPD5  MVC   0(3,R4),FULL        INSERT INTO TABLE                            
*                                                                               
INITPD6  AH    R6,PRDBUFLN         GET NEXT PRODUCT                             
         BCT   R7,INITPD2                                                       
         B     EXIT                                                             
         EJECT                                                                  
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
         EJECT                                                                  
         DS    0D                                                               
         DROP  R2                                                               
         USING *,RF                                                             
RFMTBUFF NTR1  BASE=SPM4RB                                                      
         LM    RA,RC,SPM4RA                                                     
         L     R2,SPM4R2                                                        
         USING SPM402+4096,R2                                                   
         DROP  RF                                                               
         LR    R6,R1                                                            
         GOTO1 =V(RFCSECT),RFPARAM,(RA),(R6),PSLIST,RR=RELO                     
         XIT1                                                                   
RFPARAM  DS    3F                                                               
         XIT1                                                                   
         EJECT                                                                  
         DS    0D                                                               
         DROP  R2                                                               
         USING *,RF                                                             
MYHEAD   NTR1  BASE=SPM4RB                                                      
         LM    RA,RC,SPM4RA                                                     
         L     R2,SPM4R2                                                        
         USING SPM402+4096,R2                                                   
         DROP  RF                                                               
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
*                                                                               
         L     RE,=V(DICSECT)                                                   
         USING DICSECT,RE                                                       
         CLI   SPOTPROF+12,C'Y'                                                 
         BNE   *+10                                                             
         MVC   H8+50(L'SP@TXEX),SP@TXEX                                         
         CLI   QFILTER,C'F'        FILM NUMBER FILTER FOR COKE                  
         BNE   MYHNOF                                                           
         MVC   H8(L'SP@FILM),SP@FILM                                            
         MVC   H8+9(1),QFILTER+1                                                
         ICM   RF,15,CMLPTR                                                     
         BZ    *+10                                                             
         MVC   H8+9(4),0(RF)       MOVE FILM CLASS                              
         DROP  RE                                                               
MYHNOF   DS    0H                                                               
         CLI   HEADSW,C'X'                                                      
         BNE   *+8                                                              
         MVI   HEADSW,C'P'                                                      
         CLI   HEADSW,C'M'                                                      
         BNE   MH1                                                              
         MVI   HEADSW,C'H'                                                      
         BAS   RE,MONHED                                                        
         B     MYHEADX                                                          
MH1      CLI   HEADSW,C'P'                                                      
         BNE   MYHEADX                                                          
         BAS   RE,PERHED                                                        
MYHEADX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* ROUTINE TO GET NEW FORMAT DEMO NAMES                                          
         DS    0D                                                               
         USING *,RF                                                             
NEWDNAM  NTR1  BASE=SPM4RB                                                      
         LM    RA,RC,SPM4RA                                                     
         L     R2,SPM4R2                                                        
         USING SPM402+4096,R2                                                   
         DROP  RF                                                               
         ZIC   R9,0(R1)            NUMBER OF DEMOS                              
         L     R8,4(R1)            OUTPUT AREA                                  
         MH    R9,=H'7'            CLEAR OUTPUT AREA                            
         BCTR  R9,0                                                             
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),SPACES                                                   
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
         ZIC   R9,0(R1)            NUMBER OF DEMOS                              
         L     R7,0(R1)            START OF INPUT                               
         L     R8,4(R1)            START OF OUTPUT                              
         L     R6,ADEST            SET TO USER NAMES                            
         USING ESTHDR,R6                                                        
         LA    RF,ENONTDMS                                                      
         ST    RF,DMCB+16                                                       
         LA    R6,EUSRNMS                                                       
         DROP  R6                                                               
         GOTO1 DEMOCON,DMCB,((R9),(R7)),(2,(R8)),(C'S',ADBLOCK),       X        
               (SPOTPROF+9,(R6))                                                
         B     EXIT                                                             
         SPACE 2                                                                
OLDNAM   ZIC   R9,0(R1)            NUMBER OF DEMOS                              
         L     R7,0(R1)            START OF INPUT                               
         L     R8,4(R1)            START OF OUTPUT                              
OLDNAM1  ZIC   R6,0(R7)                                                         
         LTR   R6,R6                                                            
         BZ    EXIT                                                             
         BCTR  R6,0                                                             
         MH    R6,=H'7'                                                         
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
SETPRMY  NTR1  BASE=SPM4RB                                                      
         LM    RA,RC,SPM4RA                                                     
         L     R2,SPM4R2                                                        
         USING SPM402+4096,R2                                                   
         DROP  RF                                                               
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         ZIC   RE,MEDBRAND         GET PRODUCT SLOT                             
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF                                                       
         MVC   MEDPRIMY,28(RE)     EXTRACT OLD PRIMARY DEMO                     
         CLC   PRDBUFLN,=H'56'     OLD FORMAT CAN EXIT NOW                      
         BE    SETPRMYX                                                         
         MVC   NDFULL(3),28(RE)    EXTRACT NEW PRIMARY DEMO                     
         LA    R9,PRMYTAB                                                       
         LA    R1,1                                                             
SETPRMY2 CLC   NDFULL(3),0(R9)     SAVE NEW PRIMARY DEMO IN TABLE               
         BE    SETPRMY4                                                         
         CLI   1(R9),0                                                          
         BE    SETPRMY3                                                         
         LA    R9,3(R9)                                                         
         LA    R1,1(R1)                                                         
         B     SETPRMY2                                                         
SETPRMY3 MVC   0(3,R9),NDFULL                                                   
SETPRMY4 STC   R1,MEDPRIMY         EQUATE NEW PRIMARY DEMO TO SLOT              
SETPRMYX B     EXIT                                                             
         EJECT                                                                  
* EQUATE DEMOS FROM INTERNAL TO EXTERNAL FORMAT                                 
         DS    0D                  P1  0     =NUMBER OF 1 BYTE DEMOS            
         DROP  R2                      1-3   =A(1 BYTE DEMOS)                   
         USING *,RF                                                             
IEEQU    NTR1  BASE=SPM4RB         P2  0-3   =A(3 BYTE DEMOS)                   
         LM    RA,RC,SPM4RA                                                     
         L     R2,SPM4R2                                                        
         DROP  RF                                                               
         USING SPM402+4096,R2                                                   
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
EIEQU    NTR1  BASE=SPM4RB                                                      
         LM    RA,RC,SPM4RA                                                     
         L     R2,SPM4R2                                                        
         DROP  RF                                                               
         USING SPM402+4096,R2                                                   
         ZIC   R9,0(R1)                                                         
         L     R8,0(R1)                                                         
         L     R7,4(R1)                                                         
         CLC   PRDBUFLN,=H'56'                                                  
         BE    EIOLD                                                            
         BCTR  R9,0                                                             
         EX    R9,*+8                                                           
         B     *+10                                                             
         XC    0(0,R7),0(R7)       CLEAR OUTPUT AREA                            
         LA    R9,1(R9)                                                         
EIEQU1   LA    R1,1                                                             
         MVC   NDFULL(3),0(R8)                                                  
         CLI   1(R8),0             IS THIS THE END                              
         BE    EXIT                                                             
         LA    R6,PRMYTAB                                                       
EIEQU2   CLC   NDFULL(3),0(R6)                                                  
         BE    EIEQU4                                                           
         CLI   1(R6),0                                                          
         BE    EIEQU3                                                           
         LA    R6,3(R6)                                                         
         LA    R1,1(R1)                                                         
         B     EIEQU2                                                           
EIEQU3   MVC   0(3,R6),NDFULL                                                   
EIEQU4   STC   R1,0(R7)                                                         
         LA    R7,1(R7)                                                         
         LA    R8,3(R8)                                                         
         BCT   R9,EIEQU1                                                        
         B     EXIT                                                             
         SPACE 2                                                                
EIOLD    MVC   0(1,R7),0(R8)                                                    
         LA    R7,1(R7)                                                         
         LA    R8,1(R8)                                                         
         BCT   R9,EIOLD                                                         
         B     EXIT                                                             
         SPACE 2                                                                
SPM4RA   DC    F'0'                                                             
SPM4RB   DC    F'0'                                                             
SPM4RC   DC    F'0'                                                             
SPM4R2   DC    F'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
* REPORT SUPPRESSION TABLES                                                     
SUPSL    DC    AL1(1,5,9,13,17,53,54,55,56,57,58,59,60,61,0)                    
SUPDPSL  DC    AL1(1,2,3,5,6,7,9,10,11,13,14,15,17,18,19)                       
         DC    AL1(53,54,55,56,57,58,59,60,61,0)                                
SUPSD    DC    AL1(21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36)             
         DC    AL1(0)                                                           
SUPDET   DC    AL1(1,2,3,4,9,10,11,12,37,39,45,47,53,55,0)                      
SUPONEDM DC    AL1(17,18,19,20,57,0)                                            
SUPMON   DC    AL1(1,2,3,4,5,6,7,8,37,38,53,54,45,46,0)                         
SUPSLTOT DC    AL1(53,54,55,56,57,58,59,60,61,0)                                
LVCNTRL  DC    F'1'                                                             
         DC    A(2,3,4,5)                                                       
         EJECT                                                                  
SAVNUMWK DS    XL4                                                              
PWPREFIX DS    CL5                                                              
PSLIST   DS    CL200               PRODUCT SPOT LENGTH LIST                     
PRMYTAB  DS    CL190                                                            
MULTISW  DS    C                                                                
POSTWORK DS    CL2                                                              
HEADSW   DS    C                                                                
FRSTTOT  DS    C                                                                
HCAP1    DS    CL6                                                              
HCAP2    DS    CL9                                                              
SVPNAME  DS    CL7                                                              
PRTSW    DS    C                                                                
CURRLN   DS    C                                                                
SW1      DS    C                   DATA SWITCH                                  
CPPSW    DS    C                   CPP SWITCH                                   
HIGROUP  DS    CL1                                                              
ACTSW    DS    C                   ACTIVITY SWITCH                              
STACTSW  DS    CL1                                                              
UNIVERSE DS    F                                                                
WEIGHT   DC    F'1'                                                             
ACTAREA  DS    F                                                                
         DS    0F                                                               
RELO     DS    F                                                                
SAVPOL   DS    F                                                                
SAVPOLND DS    CL12                                                             
SAVPOLA  DS    F                                                                
APDTAB   DS    F                                                                
APDTCNT  DS    F                                                                
VSETPDEM DC    F'0'                                                             
ANEWDNAM DC    F'0'                                                             
AIEEQU   DC    F'0'                                                             
AEIEQU   DC    F'0'                                                             
SAVE9    DS    F                                                                
SAVERE   DS    F                                                                
NDFULL   DS    F                                                                
BUFRECSW DS    C                   TYPE OF BUFFALO RECORD                       
RTGSW    DS    C                   RATING SWITCH                                
BUFCDE   DS    C                   BUFFALO CODE                                 
LCODE    DS    C                   LEVEL CODE                                   
PREVBRND DS    C                                                                
BRNDCNTR DS    C                                                                
PDEMCNT  DS    C                                                                
SECDMSW  DS    C                                                                
PBCDE1   DS    C                                                                
PBCDE2   DS    C                                                                
PREVMO   DS    C                                                                
PBUFFCDE DS    C                                                                
PRIMDEM  DS    CL2                                                              
SW2      DS    C                                                                
WTSW     DS    C                                                                
WTLIST   DS    CL4                                                              
SPLPRINT DS    C                                                                
SVMEDFRS DS    CL12                                                             
SVMEDMON DS    CL156                                                            
SVMEDQTR DS    CL60                                                             
MYBUFIO  DS    CL600                                                            
SPBUFMKT DS    CL750                                                            
         EJECT                                                                  
RQFRST   CSECT                                                                  
         NMOD1 0,M4RQFRST                                                       
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R2,4(R1)                                                         
         USING PSLIST,R2                                                        
         MVI   MAXLINES,59                                                      
         CLI   QPROG,C'U'                                                       
         BNE   *+12                                                             
         MVI   QDPTDET,C' '                                                     
         MVI   QCOMPARE,C' '                                                    
         CLC   FOOT1,SPACES                                                     
         BE    *+8                                                              
         MVI   MAXLINES,56                                                      
         MVC   FOOT1,SPACES                                                     
         L     RE,=V(DICSECT)                                                   
         USING DICSECT,RE                                                       
         L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         MVC   BUFFCOLS,=F'28'                                                  
         MVC   BUFFWROW,=F'112'                                                 
         DROP  RF                                                               
         CLI   QBOOK1,C' '                                                      
         BNE   *+8                                                              
         MVI   QRERATE,C' '                                                     
         CLI   QCOMPARE,C'B'                                                    
         BNE   *+8                                                              
         MVI   QRERATE,C'P'                                                     
         CLI   QBOOK1,C' '                                                      
         BNE   *+8                                                              
         MVI   QRERATE,C' '                                                     
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
         CLI   QOPT2,C' '                                                       
         BNE   *+10                                                             
         MVC   QOPT2,PROGPROF+4                                                 
         XC    HCAP1,HCAP1                                                      
         XC    HCAP2,HCAP2                                                      
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
         CLI   QCOMPARE,C'S'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'B'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'D'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'F'                                                    
         BNE   *+14                                                             
         MVI   QRERATE,C'I'                                                     
         MVC   HCAP2(L'SP@AFFDV),SP@AFFDV                                       
         CLI   QCOMPARE,C'S'                                                    
         BNE   *+10                                                             
         MVC   HCAP2(L'SP@AFFRG),SP@AFFRG                                       
         CLI   QCOMPARE,C'C'                                                    
         BNE   *+10                                                             
         MVC   HCAP2(L'SP@ACHVD),SP@ACHVD                                       
         CLI   QCOMPARE,C'C'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'D'                                                    
         BNE   *+10                                                             
         MVC   HCAP1(L'SP6PURCH),SP6PURCH                                       
         CLI   QCOMPARE,C'S'                                                    
         BNE   *+10                                                             
         MVC   HCAP1(L'SP@SPCL),SP@SPCL                                         
         CLI   QCOMPARE,C'E'                                                    
         BE    *+8                                                              
         CLI   QCOMPARE,C'F'                                                    
         BNE   *+10                                                             
         MVC   HCAP1(L'SP6ORDER),SP6ORDER                                       
         MVI   ESTSW,C'N'          NEW REQUEST                                  
         MVC   RQDPOVRD,QDPTMENU   OVERRIDE DAYPART MENU                        
         MVI   FORCEHED,C'Y'                                                    
         DROP  RE                                                               
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
RFCSECT  CSECT                                                                  
         NMOD1 0,RFCSECT                                                        
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R2,8(R1)                                                         
         USING PSLIST,R2                                                        
         L     R6,4(R1)                                                         
         L     R3,0(R6)            A(INPUT RECORD)                              
         USING SP14BUF,R3                                                       
         ICM   RF,15,BPBSPT                                                     
         SR    RE,RE                                                            
         M     RE,SPWEIGHT                                                      
         STCM  RF,15,BPBSPW                                                     
         OC    SPWEIGHT,SPWEIGHT   ENSURE A WEIGHT                              
         BNZ   *+10                                                             
         MVC   BPBSPW,BPBSPT                                                    
         L     RF,4(R6)                                                         
         USING BUFFALOD,RF                                                      
         CLI   3(R3),0                                                          
         BE    RFMTBUFX                                                         
         A     R3,BUFFLKEY         SET TO DATA                                  
         LA    R4,4                                                             
RFMTBUF1 OC    8(8,R3),8(R3)       CHECK FOR DEMO                               
         BNZ   *+10                                                             
         XC    0(8,R3),0(R3)        NO DEMO-ZERO DOLLARS                        
         CLI   8(R3),X'FF'                                                      
         BNE   *+10                                                             
         XC    8(8,R3),8(R3)                                                    
         LA    R3,16(R3)                                                        
         BCT   R4,RFMTBUF1                                                      
         L     R3,0(R6)                                                         
         A     R3,BUFFLKEY                                                      
         OC    0(108,R3),0(R3)                                                  
         BNZ   *+10                                                             
         XC    108(4,R3),108(R3)                                                
RFMTBUFX L     R3,0(R6)                                                         
         CLI   8(R3),X'FF'         TOTAL LINE                                   
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
         XC    20(112,R3),20(R3)                                                
         B     BFHEXIT                                                          
         SPACE 2                                                                
BFHOOKA  DS    0H                                                               
         CLI   8(R3),X'FF'                                                      
         BNE   BFHEXIT                                                          
         CLI   19(R3),20                                                        
         BE    BFHEXIT                                                          
         CLI   17(R3),X'FF'                                                     
         BE    *+8                                                              
         MVI   8(R3),X'FE'         SET ORIG KEY                                 
         CLI   18(R3),X'01'                                                     
         BNE   BFHOOK1                                                          
         MVI   8(R3),X'FD'         SET SPILL KEY                                
BFHOOK1  L     R9,MEDBUFF                                                       
         CLI   8(R3),X'FF'         OVERALL TOTALS                               
         BE    BFHEXIT                                                          
         CLI   MODE,PROCGOAL                                                    
         BNE   BFHOOK1A                                                         
         CLI   8(R3),X'FD'         IS THIS A SPILL LINE                         
         BNE   BFHOOK1A                                                         
         XC    20(112,R3),20(R3)                                                
         B     BFHEXIT                                                          
BFHOOK1A CLI   SPOTPROF+5,1        DO WE WANT S/O TOTALS                        
         BE    *+8                                                              
         CLI   SPOTPROF+5,3                                                     
         BE    *+14                                                             
         XC    20(112,R3),20(R3)                                                
         B     BFHEXIT                                                          
         SPACE 2                                                                
         CLI   8(R3),X'FD'         SPILL                                        
         BE    BFHOOK2                                                          
         CLI   MODE,PROCGOAL       ORIG GOALS ARE OK                            
         BE    BFHEXIT                                                          
         CLI   MEDSPILL,C'Y'                                                    
         BNE   BFHEXIT                                                          
         XC    20(112,R3),20(R3)                                                
         B     BFHEXIT                                                          
         SPACE 2                                                                
BFHOOK2  CLI   MEDSPILL,C'Y'                                                    
         BE    BFHEXIT                                                          
         XC    20(112,R3),20(R3)                                                
         B     BFHEXIT                                                          
BFHEXIT  XMOD1 1                                                                
         DROP  R9                                                               
         LTORG                                                                  
         DROP  RF                                                               
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
         GOTO1 AEIEQU,DMCB,(4,28(R6)),FULL                                      
         XC    WORK,WORK                                                        
         MVC   WORK(4),FULL                                                     
         L     R4,PDTABCNT                                                      
         GOTO1 BINSRCH,DMCB,(X'01',WORK),PDTAB,(R4),24,(0,1),25                 
         MVC   PDTABCNT,DMCB+8                                                  
         CLI   0(R1),1                                                          
         BE    BLDPDM4             RECORD INSERTED                              
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                PRIMARY DEMO TABLE IS FULL                   
         LA    R8,FULL+1           SET PRIMARY DEMOS                            
         LA    R9,3                                                             
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
PDTAB    DS    600C                                                             
         DROP  R2                                                               
         EJECT                                                                  
SETPDEM  CSECT                     SET PRIMARY DEMO IN TOTAL AREA               
         NMOD1 0,M4SPDEM                                                        
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R2,MEDBUFF                                                       
         USING MEDBLOCK,R2                                                      
         L     R3,4(R1)                                                         
         USING PSLIST,R3                                                        
         ZIC   RE,MEDBRAND                                                      
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF                                                       
         MVC   FULL,28(RE)                                                      
         LA    R6,28(RE)                                                        
         GOTO1 AEIEQU,DMCB,(4,(R6)),FULL                                        
         L     R5,APDTCNT                                                       
         L     R4,0(R5)                                                         
         L     R5,APDTAB                                                        
         GOTO1 BINSRCH,DMCB,(X'00',FULL),(R5),(R4),24,(0,1),25                  
         CLI   0(R1),1                                                          
         BNE   *+6                                                              
         DC    H'0'                PRIMARY DEMO NOT IN TABLE                    
*                                   ERROR - SHOULD HAVE BEEN PUT THERE          
*                                           BY BLDPDEM                          
         L     R4,0(R1)                                                         
         MVC   WORK,1(R4)          SAVE DEMOS                                   
         LA    R4,FULL+1                                                        
         LA    R5,WORK                                                          
         LA    R6,3                                                             
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
         LPR   R5,R5                                                            
         LTR   R5,R5                                                            
         BNZ   *+8                                                              
         L     R5,=F'-1'                                                        
         A     R5,0(RE,R6)         ACCUMULATE DEMOS                             
         ST    R5,0(RE,R6)                                                      
         L     R5,4(R7)                                                         
         LPR   R5,R5                                                            
         LTR   R5,R5                                                            
         BNZ   *+8                                                              
         L     R5,=F'-1'                                                        
         A     R5,4(RE,R6)                                                      
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
         L     R3,4(R1)                                                         
         USING PSLIST,R3                                                        
         MVI   SECDMSW,1                                                        
         MVI   P2,0                                                             
         ZIC   RE,LINE                                                          
         AH    RE,=H'22'                                                        
         STC   RE,ALLOWLIN                                                      
         MVC   SVDMCB,DMCB                                                      
         L     R4,=V(DICSECT)                                                   
         USING DICSECT,R4                                                       
         CLC   ALLOWLIN,MAXLINES                                                
         BL    PDMLA                                                            
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 REPORT                                                           
         MVC   DMCB,SVDMCB                                                      
         B     *+12                                                             
PDMLA    CLI   PRIMDEM+1,0                                                      
         BNE   PDML1                                                            
         ZIC   R6,PRIMDEM                                                       
         BAS   R9,PDMGTN           GET PRIMARY DEMO NAME                        
         MVC   SECHEAD(L'SP@SECDS),SP@SECDS                                     
         MVC   SECHEAD+4(7),0(RE)                                               
         MVC   P2(L'SECHEAD),SECHEAD                                            
         MVI   P2,C' '                                                          
         CLI   PRIMDEM+1,0                                                      
         BE    PDML1                                                            
         MVC   P2+39(L'SP@CONTI),SP@CONTI                                       
         SPACE 2                                                                
PDML1    LA    R5,P2             SET MIDLINE                                    
         CLI   P2,0                                                             
         BE    *+8                                                              
         LA    R5,P3                                                            
         LR    RE,R5               FORCE A SPACE                                
         AH    RE,=H'264'                                                       
         MVI   0(RE),0                                                          
         L     RE,DMCB                                                          
         ZIC   RF,MYBUFIO+3        GET DEMOS FOR THIS LINE                      
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
         BAS   R9,PDMGTN                                                        
         MVC   0(22,R5),DASH2                                                   
         MVC   7(7,R5),0(RE)                                                    
         MVC   133(L'SP@DEMO,R5),SP@DEMO                                        
         MVC   140(L'SP7DOLLA,R5),SP7DOLLA                                      
         MVC   150(L'SP@CPM,R5),SP@CPM                                          
         CLI   0(RE),C'E'                                                       
         BE    *+8                                                              
         CLI   0(RE),C'R'                                                       
         BNE   *+10                                                             
         MVC   150(L'SP@CPP,R5),SP@CPP                                          
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
PDMGTN   CLC   PRDBUFLN,=H'56'                                                  
         BNE   PGDMNEW                                                          
         L     R8,DEMTABLE                                                      
         SH    R8,=H'7'                                                         
         MH    R6,=H'7'                                                         
         LA    RE,0(R6,R8)                                                      
         BR    R9                                                               
         SPACE 2                                                                
PGDMNEW  XC    SVDEMR6,SVDEMR6     SAVE DEMO NUMBER                             
         STC   R6,SVDEMR6                                                       
         LA    R6,SVDEMR6                                                       
         ST    RF,NDSVRF                                                        
         GOTO1 AIEEQU,DMCB,(1,(R6)),DUB                                         
         GOTO1 ANEWDNAM,DMCB,(1,DUB),WORK                                       
         LA    RE,WORK                                                          
         ZIC   R6,SVDEMR6                                                       
         MVC   DMCB,SVDMCB                                                      
         L     RF,NDSVRF                                                        
         BR    R9                                                               
         DROP  R3                                                               
         LTORG                                                                  
SVDMCB   DS    F                                                                
SVDEMR6  DS    F                                                                
NDSVRF   DS    F                                                                
DASH2    DC    30C'-'                                                           
SECHEAD  DC    CL38' '                                                          
         EJECT                                                                  
UNWGHT   CSECT                                                                  
         NMOD1 0,UNWGHT                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R2,4(R1)                                                         
         USING PSLIST,R2                                                        
         LA    RE,MYBUFIO                                                       
         USING SP14BUF,RE                                                       
         CLI   BUFRECSW,C'M'                                                    
         BE    UNWMON                                                           
         SPACE 2                                                                
         LA    R7,BDS1D                                                         
         LA    R6,WTLIST                                                        
UNWGHT2  CLI   0(R6),0                                                          
         BE    UNWGHTX                                                          
         CLI   0(R6),C'E'                                                       
         BE    *+12                                                             
         CLI   0(R6),C'R'                                                       
         BNE   UNWGHT3                                                          
         BAS   R9,UNWGHT4                                                       
UNWGHT3  LA    R7,16(R7)                                                        
         LA    R6,1(R6)                                                         
         B     UNWGHT2                                                          
         SPACE 2                                                                
UNWMON   LA    R7,BM1BD1           UNWEIGHT MONTHLY RECORDS                     
         LA    R6,WTLIST                                                        
         LA    R0,3                                                             
UNWMON2  CLI   0(R6),0                                                          
         BE    UNWGHTX                                                          
         CLI   0(R6),C'E'                                                       
         BE    *+12                                                             
         CLI   0(R6),C'R'                                                       
         BNE   UNWGHTX                                                          
UNWMON4  BAS   R9,UNWGHT4                                                       
         LA    R7,36(R7)                                                        
         BCT   R0,UNWMON4                                                       
         B     UNWGHTX                                                          
         SPACE 2                                                                
UNWGHT4  CLI   SPOTPROF+1,C'N'                                                  
         BER   R9                                                               
         OC    SPWEIGHT,SPWEIGHT                                                
         BNZ   *+12                                                             
         XC    0(8,R7),0(R7)                                                    
         BR    R9                                                               
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
         BR    R9                                                               
UNWGHTX  XMOD1 1                                                                
         LTORG                                                                  
         DROP  R2                                                               
         DROP  RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
MONHEDC  CSECT                                                                  
         NMOD1 0,MONHEDC                                                        
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R2,4(R1)                                                         
         USING PSLIST,R2                                                        
         L     RF,=V(DICSECT)                                                   
         USING DICSECT,RF                                                       
         CLI   SW2,1               REFORMAT BRAND NAME/DATA LINES               
         BNE   MONHED01                                                         
         MVC   MID1,P2             BRAND NAME                                   
         MVC   P1,P3               DATA LINE                                    
MONHED01 DS    0H                                                               
         MVC   P3,MID1                                                          
         MVC   P4,P1                                                            
         MVC   MID1,SPACES                                                      
         MVI   MID1,0              FORCE A MIDLINE                              
         MVI   FORCEMID,C'Y'                                                    
         CLI   HEADSW,C'M'                                                      
         BE    *+10                                                             
         MVC   MID1+56(L'SP@MTHLY),SP@MTHLY                                     
         CLI   QOPT1,C'J'          TEST JWT QUARTERLY                           
         BE    *+8                                                              
         CLI   QOPT1,C'Q'          CHECK FOR QUARTERLY REPORT                   
         BNE   *+10                                                             
         MVC   MID1+56(L'SP@QRTLY),SP@QRTLY                                     
         CLI   QOPT1,C'S'          CHECK FOR QUARTERLY REPORT                   
         BNE   *+10                                                             
         MVC   MID1+56(L'SP@CUSQR),SP@CUSQR                                     
         MVI   P1,C'-'                                                          
         MVC   P1+1(50),P1                                                      
         MVI   P1+11,C' '                                                       
         MVC   P6,P3                                                            
         CLI   P6,C' '             FORCE LINE TO PRINTER                        
         BNE   *+8                                                              
         MVI   P6,0                                                             
         MVC   P7,P4                                                            
         MVC   P4,P1                                                            
         MVI   P5,0                                                             
         MVC   P3(132),SPACES                                                   
         MVC   P2(132),SPACES                                                   
         MVC   P2(L'SP@PRDTA),SP@PRDTA                                          
         MVC   P3(L'SP@DAPLN),SP@DAPLN                                          
         MVI   P2+13,C'-'                                                       
         MVC   P2+14(12),P2+13                                                  
         MVC   P2+28(13),P2+13                                                  
         MVC   P2+15(6),HCAP1                                                   
         MVC   P2+30(9),HCAP2                                                   
         MVC   P2+42(L'SP@PCNT2),SP@PCNT2                                       
         MVC   P3+13(L'SP@DMDOL),SP@DMDOL                                       
         MVC   P3+28(L'SP@DMDOL),SP@DMDOL                                       
         MVC   P3+42(L'SP9DMDOL),SP9DMDOL                                       
         CLI   QOPT1,C'J'                                                       
         BNE   MONHED10                                                         
         MVC   P2+42(9),=C'         '                                           
         MVC   P3+46(L'SP@CPPM),SP@CPPM                                         
MONHED10 L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         LA    R6,MEDMON01         SET TO APPROPRIATE MONTH SLOT                
         CLI   MYBUFIO+1,1                                                      
         BE    MONHED20                                                         
         LA    R6,MEDMON04                                                      
         CLI   MYBUFIO+1,2                                                      
         BE    MONHED20                                                         
         LA    R6,MEDMON07                                                      
         CLI   MYBUFIO+1,3                                                      
         BE    MONHED20                                                         
         LA    R6,MEDMON10                                                      
         DROP  RE                                                               
*                                                                               
MONHED20 LA    R7,P1+26                                                         
         BAS   R9,MONHED40         SET MONTH DATES IN HEADLINE                  
         LA    R6,10(R6)                                                        
         CLI   0(R6),0                                                          
         BE    MONHEDX                                                          
         LA    R7,P1+66                                                         
         MVC   P1+52(39),P1+12                                                  
         MVC   P2+52(39),P2+12                                                  
         MVC   P3+52(39),P3+12                                                  
         MVC   P4+52(39),P4+12                                                  
MONHED21 BAS   R9,MONHED40                                                      
         LA    R6,10(R6)                                                        
         CLI   0(R6),0                                                          
         BE    MONHEDX                                                          
         LA    R7,P1+106                                                        
         MVC   P1+92(39),P1+12                                                  
         MVC   P2+92(39),P2+12                                                  
         MVC   P3+92(39),P3+12                                                  
         MVC   P4+92(39),P4+12                                                  
MONHED22 BAS   R9,MONHED40                                                      
         B     MONHEDX                                                          
*                                                                               
MONHED40 GOTO1 DATCON,DMCB,(X'02',(R6)),(X'07',(R7))                            
         MVI   5(R7),C'-'                                                       
         LA    R6,2(R6)                                                         
         LA    R7,6(R7)                                                         
         GOTO1 DATCON,DMCB,(X'02',(R6)),(X'07',(R7))                            
         BR    R9                                                               
*                                                                               
MONHEDX  MVI   HEADSW,C'M'                                                      
         XMOD1 1                                                                
         DROP  R2                                                               
         DROP  RF                                                               
         LTORG                                                                  
         EJECT                                                                  
PERHEDC  CSECT                                                                  
         NMOD1 0,PERHEDC                                                        
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     R6,4(R1)                                                         
         USING PSLIST,R6                                                        
         L     R5,=V(DICSECT)                                                   
         USING DICSECT,R5                                                       
         CLI   SW2,1                                                            
         BNE   PERHED01                                                         
         MVC   MID1,P2                                                          
         MVC   P1,P3                                                            
PERHED01 MVC   P6,MID1                                                          
         MVI   ALLOWLIN,21                                                      
         CLI   P5,C' '                                                          
         CLI   P6,C' '                                                          
         BNE   *+8                                                              
         MVI   P6,0                                                             
         MVC   P7,P1                                                            
         MVI   P5,0                                                             
         XC    MID1,MID1                                                        
         XC    MID2,MID2                                                        
         MVC   MID2+55(L'SP@MKTPE),SP@MKTPE                                     
         XC    P1,P1                                                            
         MVC   P1(11),DASH                                                      
         MVC   P1+11(23),DASH                                                   
         MVC   P1+38(35),DASH                                                   
         MVC   P1+75(9),DASH                                                    
         MVC   P1+86(44),DASH                                                   
*                                                                               
         MVC   P2(132),SPACES                                                   
         MVC   P3(132),SPACES                                                   
         MVC   P2+70(L'SP@AVPCT),SP@AVPCT                                       
         MVC   P2+79(L'SP@ACHMT),SP@ACHMT                                       
         MVC   P2(L'SP@PRDTA),SP@PRDTA                                          
         MVC   P2+101(L'SP@DMGRP),SP@DMGRP                                      
         MVC   P2+12(21),DASH                                                   
         MVC   P2+41(4),DASH                                                    
         MVC   P2+53(4),DASH                                                    
         MVC   P2+20(6),HCAP1                                                   
         MVI   HEADSW,C'P'                                                      
         MVC   P2+45(9),HCAP2                                                   
*                                                                               
         MVC   P3(L'SP@DAPLN),SP@DAPLN                                          
         MVC   P3+19(L'SP7DOLLA),SP7DOLLA                                       
         MVC   P3+44(L'SP7DOLLA),SP7DOLLA                                       
         MVC   P3+63(L'SP@SPOTS),SP@SPOTS                                       
         MVC   P3+80(L'SP4DOLLA),SP4DOLLA                                       
         MVC   P3+89(L'SP@DEMO),SP@DEMO                                         
         MVC   P3+104(L'SP@DEMO),SP@DEMO                                        
         MVC   P3+119(L'SP@DEMO),SP@DEMO                                        
         MVC   P3+95(L'SP@CPPM),SP@CPPM                                         
         MVC   P3+110(L'SP@CPPM),SP@CPPM                                        
         MVC   P3+125(L'SP@CPPM),SP@CPPM                                        
         MVC   P3+13(L'SP@DEMO),SP@DEMO                                         
         MVC   P3+28(L'SP@CPPM),SP@CPPM                                         
         MVC   P3+38(L'SP@DEMO),SP@DEMO                                         
         MVC   P3+53(L'SP@CPPM),SP@CPPM                                         
         MVC   P3+70(L'SP@DEMO2),SP@DEMO2                                       
         MVC   P4(11),DASH                                                      
         MVC   P4+11(23),DASH                                                   
         MVC   P4+38(35),DASH                                                   
         MVC   P4+75(9),DASH                                                    
         MVC   P4+86(44),DASH                                                   
         XMOD1 1                                                                
DASH     DC    50C'-'                                                           
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
SUPCPP   CSECT                                                                  
         NMOD1 0,SUPCPP                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         L     RE,8(R1)                                                         
         USING SP14BUF,RE                                                       
         L     R2,4(R1)                                                         
         CLI   0(R2),1                                                          
         BE    SUPCPPX             CPP WANTED                                   
         TM    0(RE),1             MONTHLY DATA                                 
         BZ    SUPCPER                                                          
         XC    BM1GD1E,BM1GD1E     SUPPRESS MONTHLY CPP                         
         XC    BM1GDLE,BM1GDLE                                                  
         XC    BM1GD1E,BM1BD1E                                                  
         XC    BM1GDLE,BM1BDLE                                                  
         XC    BM2GD1E,BM2GD1E                                                  
         XC    BM2GDLE,BM2GDLE                                                  
         XC    BM2GD1E,BM2BD1E                                                  
         XC    BM2GDLE,BM2BDLE                                                  
         XC    BM3GD1E,BM3GD1E                                                  
         XC    BM3GDLE,BM3GDLE                                                  
         XC    BM3GD1E,BM3BD1E                                                  
         XC    BM3GDLE,BM3BDLE                                                  
         B     SUPCPPX                                                          
*                                                                               
SUPCPER  TM    0(RE),2                                                          
         BZ    SUPCPPX                                                          
         CLI   3(RE),0                                                          
         BNE   SUPSDPER                                                         
         XC    BPGD1E,BPGD1E       SUPPRESS PERIOD CPP                          
         XC    BPGDLE,BPGDLE                                                    
         XC    BPBD1E,BPBD1E                                                    
         XC    BPBDLE,BPBDLE                                                    
         XC    BPBD2E,BPBD2E                                                    
         XC    BPBD3E,BPBD3E                                                    
         XC    BPBD4E,BPBD4E                                                    
         B     SUPCPPX                                                          
         SPACE 2                                                                
SUPSDPER XC    BDS1DLE,BDS1DLE                                                  
         XC    BDS2DLE,BDS2DLE                                                  
         XC    BDS2DLE,BDS3DLE                                                  
         XC    BDS4DLE,BDS4DLE                                                  
         XC    BDS1DE,BDS1DE                                                    
         XC    BDS2DE,BDS2DE                                                    
         XC    BDS3DE,BDS3DE                                                    
         XC    BDS4DE,BDS4DE                                                    
SUPCPPX  DS    0C                                                               
         EJECT                                                                  
         CLI   PROGPROF+7,C'Y'     SUPPRESS CROSS DAYPART DEMOS                 
         BNE   SUPDEMX                                                          
         TM    0(RE),1             MONTHLY DATA                                 
         BZ    SUPDPER                                                          
         XC    BM1GD1(8),BM1GD1    SUPPRESS MONTHLY DEMOS                       
         XC    BM1GDLE,BM1GDLE                                                  
         XC    BM1BD1(8),BM1BD1                                                 
         XC    BM1BDLE,BM1BDLE                                                  
         XC    BM2GD1(8),BM2GD1                                                 
         XC    BM2GDLE,BM2GDLE                                                  
         XC    BM2BD1(8),BM2BD1                                                 
         XC    BM2GDLE,BM2BDLE                                                  
         XC    BM3GD1(8),BM3GD1                                                 
         XC    BM3GDLE,BM3GDLE                                                  
         XC    BM3BD1(8),BM3BD1                                                 
         XC    BM3BDLE,BM3BDLE                                                  
         B     SUPDEMX                                                          
*                                                                               
SUPDPER  TM    0(RE),2                                                          
         BZ    SUPDEMX                                                          
         CLI   3(RE),0                                                          
         BNE   SUPDDPER                                                         
         XC    BPGD1(8),BPGD1       SUPPRESS PERIOD DEMOS                       
         XC    BPGDLE,BPGDLE                                                    
         XC    BPBD1(8),BPBD1                                                   
         XC    BPBDLE,BPBDLE                                                    
         XC    BPBD2(8),BPBD2                                                   
         XC    BPBD3(8),BPBD3                                                   
         XC    BPBD4(8),BPBD4                                                   
         B     SUPDEMX                                                          
         SPACE 2                                                                
SUPDDPER XC    BDS1DLE,BDS1DLE                                                  
         XC    BDS2DLE,BDS2DLE                                                  
         XC    BDS2DLE,BDS3DLE                                                  
         XC    BDS4DLE,BDS4DLE                                                  
         XC    BDS1D(8),BDS1D                                                   
         XC    BDS2D(8),BDS2D                                                   
         XC    BDS3D(8),BDS3D                                                   
         XC    BDS4D(8),BDS4D                                                   
SUPDEMX  XMOD1 1                                                                
         DROP  RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
POSTER   CSECT                                                                  
         NMOD1 0,POSTER                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         L     R2,MEDBUFF                                                       
         USING MEDBLOCK,R2                                                      
         L     R8,4(R1)                                                         
         USING PSLIST,R8                                                        
         GOTO1 MEDMKTWT,DMCB,(RA)                                               
         GOTO1 VSETPDEM,DMCB,(RA),PSLIST                                        
         LA    R3,100                                                           
         L     R3,FULL                                                          
         ZIC   RE,MEDBRAND                                                      
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF                                                       
         MVC   WORK+1(3),1(RE)     MOVE IN PRODUCT CODE                         
         MVC   WORK+4(3),29(RE)    MOVE IN SECONDARY DEMO CODES                 
         LA    R4,31(RE)                                                        
         GOTO1 AEIEQU,DMCB,(3,(R4)),DUB                                         
         MVC   WORK+4(3),DUB                                                    
         MVI   WORK,1                                                           
         MVC   HALF,=X'6162'                                                    
         GOTO1 MEDPOST,DMCB,(RA)                                                
         CLI   MULTISW,C'Y'                                                     
         BNE   MONPOST                                                          
         CLI   POSTWORK,X'61'                                                   
         BE    MONPOST                                                          
         MVC   HALF,POSTWORK                                                    
         GOTO1 MEDPOST,DMCB,(RA)                                                
MONPOST  L     R2,MEDBUFF                                                       
         USING MEDBLOCK,R2                                                      
         OC    MEDMON04(4),MEDMON04                                             
         BZ    POSTERX                                                          
         L     RE,MEDPERD+4        CLEAR PERIOD TOTALS                          
         L     R4,MEDLCHNK                                                      
         BCTR  R4,0                                                             
         EX    R4,CLRCHNK                                                       
         L     RE,MEDTOTAL+4       CLEAR SECONDARY DEMOS                        
         LA    RF,200                                                           
         XCEF                                                                   
         LA    R5,MEDMON04                                                      
         BAS   R9,SLDCHNK                                                       
         MVI   WORK,2                                                           
         MVC   HALF,=X'6162'                                                    
         GOTO1 MEDPOST,DMCB,(RA)                                                
         CLI   MULTISW,C'Y'                                                     
         BNE   MON7POST                                                         
         CLI   POSTWORK,X'61'                                                   
         BE    MON7POST                                                         
         MVC   HALF,POSTWORK                                                    
         GOTO1 MEDPOST,DMCB,(RA)                                                
*                                                                               
MON7POST OC    MEDMON07(4),MEDMON07                                             
         BZ    POSTERX                                                          
         LA    R5,MEDMON07                                                      
         BAS   R9,SLDCHNK                                                       
         MVI   WORK,3                                                           
         MVC   HALF,=X'6162'                                                    
         GOTO1 MEDPOST,DMCB,(RA)                                                
         CLI   MULTISW,C'Y'                                                     
         BNE   MONAPOST                                                         
         CLI   POSTWORK,X'61'                                                   
         BE    MONAPOST                                                         
         MVC   HALF,POSTWORK                                                    
         GOTO1 MEDPOST,DMCB,(RA)                                                
*                                                                               
MONAPOST OC    MEDMON10(4),MEDMON10                                             
         BZ    POSTERX                                                          
         LA    R5,MEDMON10                                                      
         BAS   R9,SLDCHNK                                                       
         MVI   WORK,4                                                           
         MVC   HALF,=X'6162'                                                    
         GOTO1 MEDPOST,DMCB,(RA)                                                
         CLI   MULTISW,C'Y'                                                     
         BNE   POSTERX                                                          
         CLI   POSTWORK,X'61'                                                   
         BE    POSTERX                                                          
         MVC   HALF,POSTWORK                                                    
         GOTO1 MEDPOST,DMCB,(RA)                                                
*                                                                               
POSTERX  XMOD1 1                                                                
*                                                                               
CLRCHNK  XC    0(0,RE),0(RE)                                                    
* SLIDE MONTH CHUNK TO MONTH SET 1                                              
SLDCHNK  LA    R7,MEDMON01                                                      
         LA    R1,3                                                             
SLDCHNK1 L     RE,4(R7)                                                         
         L     RF,4(R5)                                                         
         LTR   RF,RF                                                            
         BNZ   SLDCHNK2                                                         
         EX    R4,CLRCHNK                                                       
         B     SLDCHNK3                                                         
SLDCHNK2 EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
SLDCHNK3 LA    R7,12(R7)                                                        
         LA    R5,12(R5)                                                        
         BCT   R1,SLDCHNK1                                                      
         BR    R9                                                               
         XMOD1 1                                                                
         DROP  R8                                                               
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
         CLI   QOPT3,C'S'                                                       
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
         CLC   QPROG,=C'M4'        IS IT AN M4 REQUEST                          
         BNE   ALLOCX              NO ALLOCATION DONE ELSEWHERE                 
         OC    ABUFF,ABUFF                                                      
         BNZ   ALLOCX                                                           
         GOTO1 =V(COVAIL),DMCB,C'LOOK'                                          
         L     R9,8(R1)                                                         
         C     R9,=F'550000'       ENOUGH FOR ALLOCATION                        
         BL    ALLOCX              NO- LEAVE BUFFER AS IS                       
         S     R9,=F'500000'                                                    
         ST    R9,LNBUFF           SET UPPER LIMIT                              
         MVC   ABUFF,=F'50000'     SET LOWER LIMIT                              
         GOTO1 =V(COVAIL),DMCB,C'GET',ABUFF,LNBUFF                              
         OC    4(8,R1),4(R1)       ALLOCATE OK                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   ABUFF,4(R1)                                                      
         L     R9,4(R1)            SHIFT BUFFALO TO NEW AREA                    
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
ABUFF    DC    A(0)                                                             
LNBUFF   DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
DICSECT  CSECT                                                                  
*                                                                               
DCLIST   DS    0C                                                               
         DCDDL SP#ACHMT,5                                                       
         DCDDL SP#ACHVD,9                                                       
         DCDDL SP#AFFDV,9                                                       
         DCDDL SP#AFFRG,9                                                       
         DCDDL SP#ALBRN,18,C                                                    
         DCDDL SP#AVPCT,8                                                       
         DCDDL SP#BRAND,8                                                       
         DCDDL SP#COMTV,11                                                      
         DCDDL SP#CONTI,9                                                       
         DCDDL SP#CPM,3                                                         
         DCDDL SP#CPP,3                                                         
         DCDDL SP#CPPM,5                                                        
         DCDDL SP#CUSQR,21,C                                                    
         DCDDL SP#DAPLN,10                                                      
         DCDDL SP#DEMO,4                                                        
         DCDDL SP#DEMO2,9                                                       
         DCDDL SP#DMDOL,12,LABEL=SP@DMDOL                                       
         DCDDL SP#DMDOL,9,LABEL=SP9DMDOL                                        
         DCDDL SP#DMGRP,16,C                                                    
         DCDDL SP#DOLLA,7,LABEL=SP7DOLLA                                        
         DCDDL SP#DOLLA,4,LABEL=SP4DOLLA                                        
         DCDDL SP#FILM,8,C                                                      
         DCDDL SP#GOAL,6,C                                                      
         DCDDL SP#MKTPE,22,C                                                    
         DCDDL SP#MTHLY,19,C                                                    
         DCDDL SP#NETTV,11                                                      
         DCDDL SP#ORDER,6,LABEL=SP6ORDER                                        
         DCDDL SP#PCNT2,9                                                       
         DCDDL SP#PRDTA,11                                                      
         DCDDL SP#PURCH,9,LABEL=SP9PURCH                                        
         DCDDL SP#PURCH,6,R,LABEL=SP6PURCH                                      
         DCDDL SP#QRTLY,21,C                                                    
         DCDDL SP#SECDS,38,C                                                    
         DCDDL SP#SPCL,6,C                                                      
         DCDDL SP#SPILL,11                                                      
         DCDDL SP#SPOTS,5                                                       
         DCDDL SP#SPTTV,11                                                      
         DCDDL SP#TXEX,18,C                                                     
DCLISTX  DC    X'00'                                                            
*                                                                               
DSLIST   DS    0C                                                               
         DSDDL PRINT=YES                                                        
         PRINT GEN                                                              
DSLISTX  EQU   *                                                                
         EJECT                                                                  
         BUFF  LINES=050,ROWS=5,COLUMNS=28,FLAVOR=BINARY,KEYLIST=(20,A)         
         EJECT                                                                  
SP14BUF  DSECT                                                                  
BPKEY    DS    CL20                                                             
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
BPBSPW   DS    CL4                                                              
         EJECT                                                                  
         PRINT OFF                                                              
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
* SPGENCLT                                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
* SPGENAGY                                                                      
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
* DEDBLOCK                                                                      
       ++INCLUDE DEDBLOCK                                                       
* SPGENEST                                                                      
       ++INCLUDE SPGENEST                                                       
*                                                                               
       ++INCLUDE SPDDEQUS                                                       
* DDDICTATED                                                                    
       ++INCLUDE DDDICTATED                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'171SPREPM402 09/18/19'                                      
         END                                                                    
