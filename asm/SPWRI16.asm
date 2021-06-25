*          DATA SET SPWRI16    AT LEVEL 057 AS OF 10/16/06                      
*PHASE T20416A,*                                                                
         SPACE 2                                                                
         TITLE 'T20416 - MEDIA SCHEDULES'                                       
*                                                                               
*********************************************************************           
************                                             ************           
************    THIS MODULE IS DEAD AS OF OCT 11 2006    ************           
************                                             ************           
************                *************                ************           
************                * HERE LIES *                ************           
************                *    BRS    *                ************           
************                *           *                ************           
************                * 1980-2006 *                ************           
************                *           *                ************           
************                * WAS LOVED *                ************           
************                * BY NOBODY *                ************           
************                * & KILLED  *                ************           
************                * BY ALLEN  *                ************           
************                *   R.I.P   *                ************           
************                *************                ************           
*********************************************************************           
*                                                                   *           
*          SPWRI16 (T20416) - MEDIA SCHEDULES                       *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 19AUG05 56 EFJ -- MOVE STORAGE TO NMOD FROM CSECT                 *           
* 03NOV03 18 AKT -- FIX MGROUP X'40' BUGS                           *           
* 14OCT02 53 EFJ -- 2 CHAR MGR SCHEME CODES                         *           
* 17MAY02 52 EFJ -- SUPPORT RCPACK                                  *           
* 12MAR02 50 EFJ -- HISTORY LOST                                    *           
*                -- FIX ESTHDRL                                     *           
*********************************************************************           
T20416   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20416,RA,RR=R2,CLEAR=Y                                    
         LR    R6,RC                                                            
         USING WORKD,R6                                                         
         ST    R2,RELO                                                          
         MVC   SAVERD,4(RD)        SAVE A(CALLING PROGRAM'S SAVE AREA)          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         ST    RC,AGEND                                                         
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         BASR  R8,0                                                             
         AHI   R8,T20416W-*                                                     
         USING T20416W,R8          SEPARATE BASE REG FOR STORAGE                
*                                                                               
         LA    R0,AXTRAN           SET EXTENTION ROUTINES                       
         LTR   R0,R0                                                            
         BZ    BRS2                                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         L     R1,=A(T20416X)                                                   
         A     R1,RELO                                                          
         ST    R1,AXTRA(RE)                                                     
         STC   RF,AXTRA(RE)                                                     
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                  REPORT CALLING MODE                          
BRS2     CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPINPUT      INPUT                                        
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         J     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
*&&DO                                                                           
INIT     XC    SVMGR1,SVMGR1                                                    
         XC    SVMGR2,SVMGR2                                                    
         XC    SVMGR3,SVMGR3                                                    
         XC    SVSGR1,SVSGR1                                                    
         XC    SVSGR2,SVSGR2                                                    
         XC    SVBMKT,SVBMKT                                                    
         XC    SVSTA,SVSTA                                                      
*&&                                                                             
INIT     MVC   SVREP,=C'000'                                                    
         LA    RE,WKS              PRE-FILL WKS WITH BLANKS                     
         LHI   RF,56*5             L'WEEKS                                      
         XR    R0,R0                                                            
         LA    R1,C' '                                                          
         SLL   R1,24                                                            
         MVCL  RE,R0                                                            
         MVC   WKSQ1,BLANKS                                                     
         MVC   WKSQ2,BLANKS                                                     
         MVC   WKSQ3,BLANKS                                                     
         MVC   WKSQ4,BLANKS                                                     
         OI    SBQSKIP,SBQSKBIL+SBQSKGL SKIP READING BILLS AND GOALS            
         MVI   SBQPER,SBQPWK+SBQPQT   PERIOD IN WEEKS AND QUARTERS              
         MVI   SBQPERLO,1          ALL PERIODS                                  
         MVI   SBQPERHI,255                                                     
         OI    DATAIND3,DICHAN     GET STATIONS' CHANNELS                       
         OI    DATAIND4,DIWEIGHT   WANT TO SHOW COVERAGES IF MKT WEIGHT         
         OI    COLIND,COLIDEM                                                   
*                                                                               
         MVI   PRDSUM,C'Y'         PRODUCT SUMMARIES OPTION                     
         CLC   SBQMKT(3),=C'ALL'   TEST ALL MARKET REQUEST                      
         BE    INIT1                                                            
         OC    SBQMKT,SBQMKT                                                    
         BZ    INIT1                                                            
         MVI   PRDSUM,C'N'         NO-NO PRODUCT SUMMARIES                      
*                                                                               
INIT1    LA    R2,BRSROPH          VALIDATE REPORT OPTIONS                      
         LA    R4,RPTOPT                                                        
         LA    R5,DATAIND4                                                      
         BAS   RE,VALCNTL                                                       
         BNE   CURSOR                                                           
*                                                                               
         MVI   RERATE,C'P'                                                      
         MVI   BYTE,DIDEMP                                                      
         LA    R2,BRSRRTH          VALIDATE RERATE TYPE                         
         CLI   5(R2),0                                                          
         BE    INIT2               DEFAULT IS PURCH                             
         CLI   8(R2),C'P'                                                       
         BE    INIT2                                                            
         MVI   RERATE,C'R'                                                      
         MVI   BYTE,DIDEMR                                                      
         CLI   8(R2),C'A'                                                       
         BE    INIT2                                                            
         MVI   RERATE,C'A'                                                      
         MVI   BYTE,DIDEMA                                                      
         CLI   8(R2),C'I'                                                       
         BNE   EINV                                                             
*                                                                               
INIT2    OC    DATAIND,BYTE        SET DEMO TYPE INDICATOR                      
*                                                                               
         MVI   DETIND,DETDFLT      SET DEFAULT DETAIL CONTROL VALUES            
         LA    R2,BRSDETH                                                       
         L     R4,=A(DETCNTL)                                                   
         LA    R5,DETIND                                                        
         BAS   RE,VALCNTL          VALIDATE DETAIL CONTROL                      
         BNE   CURSOR                                                           
         LA    R2,BRSDOPH                                                       
         BAS   RE,VALCNTL          VALIDATE FURTHER DETAIL OPTIONS              
         BNE   CURSOR                                                           
         TM    DETIND,DETDOL       TEST DOLLARS OR DEMOS SUPPRESSED             
         BZ    *+12                                                             
         TM    DETIND,DETDEM                                                    
         BO    *+8                                                              
         NI    DETIND,255-DETCPP   YES-SUPPRESS CPP                             
         TM    DETIND,DETREP       TEST TIME SHEET REPS REQUESTED               
         BZ    *+8                                                              
         OI    DATAIND5,DITREP     YES                                          
         TM    DETIND,DETAFF       TEST STATION AFFIL NOTATION WANTED           
         BZ    *+8                                                              
         OI    DATAIND2,DIAFFIL    YES                                          
*                                                                               
         MVI   CMTOPT,0            VALIDATE COMMENT CONTROL                     
         LA    R2,BRSCOMH                                                       
         CLI   5(R2),0                                                          
         BE    INIT4                                                            
         CLI   8(R2),C'0'                                                       
         BL    EINV                                                             
         CLI   8(R2),C'2'                                                       
         BH    EINV                                                             
         MVC   CMTOPT,8(R2)                                                     
         NI    CMTOPT,X'0F'                                                     
*                                                                               
*&&DO                                                                           
INIT4    MVI   RECAP,0             RECAPS                                       
         MVI   RCPOPT,0                                                         
         MVI   RCPIND,0                                                         
         MVI   RECRCPLO,0                                                       
         MVI   RECRCPHI,0                                                       
*&&                                                                             
INIT4    LA    R2,BRSRCPH                                                       
         CLI   5(R2),0             NO RECAPS                                    
         BE    INIT7                                                            
         CLI   8(R2),C'N'                                                       
         BE    INIT7                                                            
         CLI   8(R2),C'M'          MARKET RECAPS                                
         BE    *+12                                                             
         CLI   8(R2),C'S'          STATION RECAPS                               
         BNE   EINV                                                             
         MVC   RECAP,8(R2)                                                      
         MVI   RECRCPLO,6          SET RECAP RECORD NUMBERS                     
         MVI   RECRCPHI,6                                                       
         CLI   PRDSUM,C'Y'                                                      
         BNE   *+8                                                              
         MVI   RECRCPHI,7                                                       
*                                                                               
         MVI   RCPOPT,2            RECAP TYPE                                   
         LA    R2,BRSRCTH                                                       
         CLI   5(R2),0             MONTHLY IS THE DEFAULT                       
         BE    INIT5                                                            
         CLI   8(R2),C'1'                                                       
         BL    EINV                                                             
         CLI   8(R2),C'4'                                                       
         BH    EINV                                                             
         MVC   RCPOPT,8(R2)                                                     
         NI    RCPOPT,X'0F'                                                     
         CLI   RCPOPT,3            TEST WEEKLY WITH GOALS                       
         BNE   INIT5                                                            
         CLI   RECAP,C'M'          YES-ONLY FOR MARKET RECAPS                   
         BNE   EINV                                                             
*                                                                               
INIT5    MVI   RCPIND,RCPDFLT      SET DEFAULT RECAP CONTROL VALUES             
         LA    R2,BRSRCCH                                                       
         LA    R4,RCPCNTL                                                       
         LA    R5,RCPIND                                                        
         BAS   RE,VALCNTL          VALIDATE RECAP CONTROL                       
         BNE   CURSOR                                                           
         CLI   RCPOPT,3            MIGHT HAVE TO SUPPRESS RECAPS                
         BNL   *+16                ALTOGETHER IF NO COLUMNS TO PRINT            
         TM    RCPIND,RCPSPT+RCPDOL+RCPDEM                                      
         BNZ   INIT6                                                            
         B     *+12                                                             
         TM    RCPIND,RCPDOL+RCPDEM                                             
         BNZ   INIT6                                                            
         MVI   RCPOPT,0            FORGET ABOUT RECAPS                          
         MVI   RECAP,0                                                          
         MVI   RCPIND,0                                                         
         B     INIT7                                                            
*                                                                               
INIT6    TM    RCPIND,RCPDPT       TEST SUBTOTAL BY DAYPART/LENGTH              
         BZ    *+8                                                              
         OI    DATAIND4,DISLN      YES-EXTRACT BY LENGTH                        
         TM    RCPIND,RCPDOL       TEST DOLLARS OR DEMOS SUPPRESSED             
         BZ    *+12                                                             
         TM    RCPIND,RCPDEM                                                    
         BO    *+8                                                              
         NI    RCPIND,255-RCPCPP   YES-SUPPRESS CPP                             
         CLI   RCPOPT,3            TEST WEEKLY RECAPS WITH GOALS                
         BNE   INIT7                                                            
         NI    SBQSKIP,255-SBQSKGL YES-TELL SPOTIO TO READ GOALS                
         TM    RCPIND,RCPDEM       TEST GOAL DEMOS NEEDED                       
         BZ    INIT7                                                            
         OI    DATAIND2,DIGLDEM                                                 
*                                                                               
*&&DO                                                                           
INIT7    MVI   SUMMY,0             SUMMARIES                                    
         MVI   SUMDPT,0                                                         
         MVI   SUMIND,0                                                         
         MVI   RECSUMLO,0                                                       
         MVI   RECSUMHI,0                                                       
*&&                                                                             
INIT7    LA    R2,BRSSUMH                                                       
         CLI   5(R2),0             NO SUMMARIES                                 
         BE    INIT10                                                           
         CLI   8(R2),C'N'                                                       
         BE    INIT10                                                           
         CLI   8(R2),C'0'                                                       
         BE    INIT10                                                           
         CLI   8(R2),C'1'                                                       
         BL    EINV                                                             
         CLI   8(R2),C'2'          NO MARKET PERFORMANCE YET                    
         BH    EINV                                                             
         MVC   SUMMY,8(R2)                                                      
         NI    SUMMY,X'0F'                                                      
         LA    R1,6                SET SUMMARY RECORD NUMBER                    
         CLI   RECAP,0                                                          
         BE    *+12                                                             
         IC    R1,RECRCPHI                                                      
         LA    R1,1(R1)                                                         
         STC   R1,RECSUMLO                                                      
         CLI   PRDSUM,C'Y'                                                      
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
         STC   R1,RECSUMHI                                                      
         CLI   SUMMY,2             BRAND WEEKLY SUMMARY ONLY VALID              
         BNE   *+14                FOR NON-POL REQUEST                          
         CLC   SBQPRD,=C'POL'                                                   
         BE    EINV                                                             
         CLI   SUMMY,3             TEST MARKET PERFORMANCE SUMMARY              
         BNE   INIT8                                                            
         CLC   SBQPRD,=C'POL'      YES-ONLY VALID FOR POL REQUEST               
         BNE   EINV                                                             
         MVI   SBQBPRD,0           MAKE SURE PRODUCTS ARE SEPARATED             
         OI    DATAIND5,DITARGET   AND GET PRODUCTS' TARGET DEMOS               
*                                                                               
INIT8    NI    SBQSKIP,255-SBQSKGL READ GOALS                                   
         OI    DATAIND2,DIGLDEM    GOAL DEMOS NEEDED                            
*                                                                               
         MVI   SUMDPT,1            DAYPART DETAIL CONTROL                       
         LA    R2,BRSSUDH                                                       
         CLI   5(R2),0                                                          
         BE    INIT9                                                            
         CLI   8(R2),C'1'                                                       
         BL    EINV                                                             
         CLI   8(R2),C'3'                                                       
         BH    EINV                                                             
         MVC   SUMDPT,8(R2)                                                     
         NI    SUMDPT,X'0F'                                                     
*                                                                               
INIT9    CLI   SUMDPT,1            TEST DAYPART/LENGTH BREAKOUT                 
         BNE   *+12                                                             
         OI    DATAIND4,DISLN      YES-EXTRACT BY LENGTH                        
         OI    DATAIND,DIDPTLEN                                                 
*                                                                               
         MVI   SUMIND,SUMDFLT                                                   
         LA    R2,BRSSUCH                                                       
         LA    R4,SUMCNTL                                                       
         LA    R5,SUMIND                                                        
         BAS   RE,VALCNTL          VALIDATE SUMMARY CONTROL                     
         BNE   CURSOR                                                           
         CLI   SUMMY,3             SET RECORD NUMBERS FOR MARKET                
         BNE   INIT9A              PERFORMANCE REPORT                           
         ZIC   R1,RECSUMLO                                                      
         STC   R1,RECMPRLO                                                      
         LA    R1,1(R1)                                                         
         TM    SUMIND,SUMMTH                                                    
         BZ    *+8                                                              
         LA    R1,4(R1)                                                         
         STC   R1,RECMPRHI                                                      
*                                                                               
INIT9A   CLI   SUMMY,2             TEST BRAND WEEKLY SUMMARY                    
         BE    *+8                                                              
         NI    SUMIND,255-SUMWKY   NO-SUPPRESS WEEKLY OPTION                    
         CLI   SUMMY,3             TEST MARKET PERFORMANCE SUMMARY              
         BE    *+8                                                              
         NI    SUMIND,255-SUMMTH-SUMBRD  NO-SUPPRESS MNTHLY & BRAND OPS         
         TM    SUMIND,SUMDEM       TEST PRINT DEMOS ON DPT TOTAL LINES          
         BO    *+8                                                              
         NI    SUMIND,255-SUMCPP   NO-SUPPRESS CPP ALSO                         
         TM    SUMIND,SUMCPP       TEST PRINT CPP ON TOTAL LINES                
         BO    *+8                                                              
         NI    SBQDPTLN,255-SBQDLCPP    NO-SUPPRESS                             
         TM    SUMIND,SUMLEN       TEST PRINT SPOT LENGTH TOTALS                
         BO    *+8                                                              
         NI    SBQDPTLN,255-SBQDLTOT    NO-SUPPRESS THEM                        
*                                                                               
INIT10   MVI   NDEMOS,0                                                         
         TM    DETIND,DETDEM       TEST DEMOS WILL PRINT ANYWHERE               
         BO    *+12                                                             
         TM    RCPIND,RCPDEM                                                    
         BZ    *+12                                                             
         MVI   NDEMOS,MAXDEMS      YES - 4 DEMOS                                
         OI    DATAIND4,DIDEMHED   DEMO NAMES REQUIRED                          
*                                                                               
         TM    DETIND,DETCPP       TEST CPP'S WILL PRINT ANYWHERE               
         BO    INIT10A                                                          
         TM    RCPIND,RCPCPP                                                    
         BO    INIT10A                                                          
         TM    SUMMY,0                                                          
         BE    *+8                                                              
INIT10A  OI    DATAIND,DICPP       YES                                          
*                                                                               
         MVI   RTOTIND,0           SET THE TOTAL INDICATORS                     
         CLI   RCPOPT,1            TEST PERIOD RECAP                            
         BE    *+12                                                             
         CLI   RCPOPT,2            OR MONTHLY RECAP                             
         BNE   INIT11                                                           
         OI    RTOTIND,RTRCP       YES-BUY TOTALS FOR RECAP                     
         TM    RCPIND,RCPDPT       TEST DAYPART/LENGTH BREAKOUT                 
         BZ    INIT11                                                           
         OI    RTOTIND,RTRCPL      YES-BUY TOTALS BY LENGTH                     
*                                                                               
INIT11   CLI   SUMMY,0             TEST SUMMARIES                               
         BE    INIT12                                                           
         CLI   SUMDPT,1            YES-TEST DAYPART/LENGTH BREAKOUT             
         BE    *+12                                                             
         OI    RTOTIND,RTSUM       NO-BUY TOTALS FOR SUMMARY                    
         B     INIT12                                                           
         OI    RTOTIND,RTSUML      YES-BUY TOTALS BY LENGTH                     
*                                                                               
*&&DO                                                                           
INIT12   MVI   MTOTIND,0                                                        
         MVI   WTOTIND,0                                                        
*&&                                                                             
INIT12   CLI   RCPOPT,2            TEST MONTH RECAP                             
         BNE   INIT13                                                           
         OI    MTOTIND,MTRCP       YES-GET MONTH TOTALS                         
         TM    RCPIND,RCPDPT       TEST DPTLEN BREAKOUT                         
         BZ    INIT13                                                           
         OI    MTOTIND,MTRCPL      YES-MONTH TOTALS BY LENGTH                   
*                                                                               
INIT13   CLI   SUMMY,2             TEST BRAND WEEKLY SUMMARY                    
         BNE   INIT14                                                           
         CLI   SUMDPT,1            YES-TEST DPTLEN BREAKOUT                     
         BE    *+12                                                             
         OI    MTOTIND,MTSUM       NO-MONTH TOTALS FOR SUMMARY                  
         B     *+8                                                              
         OI    MTOTIND,MTSUML      YES-MONTH TOTALS FOR SUMMARY BY LEN          
         TM    SUMIND,SUMWKY       TEST WEEKLY BREAKOUT                         
         BZ    INIT14                                                           
         OI    WTOTIND,WTSUM       YES-WEEKLY TOTALS FOR SUMMARY                
*                                                                               
INIT14   CLI   SUMMY,3             TEST MARKET PERFORMANCE REPORT               
         BNE   INIT15                                                           
         TM    SUMIND,SUMMTH       WITH MONTHLY BREAKOUT                        
         BZ    INIT15                                                           
         CLI   SUMDPT,1            YES-TEST DPTLEN BREAKOUT                     
         BE    *+12                                                             
         OI    MTOTIND,MTSUM       NO-MONTH TOTALS FOR SUUMARY                  
         B     INIT15                                                           
         OI    MTOTIND,MTSUML      YES-MONTH TOTALS FOR SUMMARY BY LEN          
*                                                                               
INIT15   CLI   RCPOPT,3            TEST WEEKLY RECAPS                           
         BL    *+8                                                              
         OI    WTOTIND,WTRCP       YES-WEEK TOTALS FOR RECAPS                   
*                                                                               
INIT15A  LA    R2,BRSTITH          TITLE                                        
         MVC   TITLE,BLANKS                                                     
         MVC   TITLE(19),=C'BRAND TIME SCHEDULE'                                
         CLI   5(R2),0                                                          
         BE    INIT16                                                           
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
*                                                                               
INIT16   GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
         LA    R4,RPTLEVS          SET LEVELS                                   
         CLI   SBQPGRD,C' '                                                     
         BH    *+14                                                             
         XC    2(2,R4),2(R4)       SET PRODUCT GROUPS                           
         B     INIT17                                                           
         CLC   SBPGR1LN,SBPGR2LN                                                
         BNE   INIT17                                                           
         MVI   3(R4),0                                                          
*                                                                               
INIT17   CLI   SBQMGRD,0           SET MARKET GROUPS                            
         BH    *+14                                                             
         XC    7(3,R4),7(R4)                                                    
         B     INIT18                                                           
         CLC   SBMGR1LN,SBMGR2LN                                                
         BNE   *+14                                                             
         XC    8(2,R4),8(R4)                                                    
         B     INIT18                                                           
         CLC   SBMGR2LN,SBMGR3LN                                                
         BNE   INIT18                                                           
         MVI   9(R4),0                                                          
*                                                                               
INIT18   CLI   SBQSGRD,C' '        SET STATION GROUPS                           
         BH    *+14                                                             
         XC    15(2,R4),15(R4)                                                  
         B     INIT18A                                                          
         CLC   SBSGR1LN,SBSGR2LN                                                
         BNE   INIT18A                                                          
         MVI   16(R4),0                                                         
*                                                                               
INIT18A  LA    R1,LEVELS           SET THE LEVELS                               
         SR    RF,RF                                                            
*                                                                               
INIT19   CLI   0(R4),X'FF'                                                      
         BE    INIT22                                                           
         CLI   0(R4),0                                                          
         BE    INIT20                                                           
         MVC   0(1,R1),0(R4)                                                    
         LA    RF,1(RF)                                                         
         CLI   0(R1),QMKT                                                       
         BNE   *+12                                                             
         STC   RF,MKTLEV           MARKET LEVEL                                 
         STC   RF,SVMKTLEV                                                      
         CLI   0(R1),QSTA                                                       
         BNE   *+8                                                              
         STC   RF,LSTHEDLV         LAST HEADLINE LEVEL                          
         LA    R1,1(R1)                                                         
*                                                                               
INIT20   LA    R4,1(R4)                                                         
         B     INIT19                                                           
*                                                                               
INIT22   MVI   MYFIRSTH,11         SET DRIVER'S FIRST HEADLINE                  
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE,                                  
         BNE   INITX                                                            
         LR    RF,R6                                                            
         AHI   RF,4096                                                          
         USING WORKD+4096,RF                                                    
         LA    R1,WKTOTS           SET SOME ADDRESSES                           
         ST    R1,AWKTOT                                                        
         LA    R1,WKTOTSX                                                       
         ST    R1,AWKTOTX                                                       
         LA    R1,WKTOTSDL                                                      
         ST    R1,AWKTOTDL                                                      
         LA    R1,WKTOTSDX                                                      
         ST    R1,AWKTOTDX                                                      
         LA    R1,RECTOTS                                                       
         ST    R1,ARECTOTS                                                      
         LA    R1,RECTOTS1                                                      
         ST    R1,ARECTOT1                                                      
         DROP  RF                                                               
*                                                                               
         CLI   LANG,LANGFRE        TEST CANADIAN FRENCH                         
         BNE   INITX                                                            
         MVI   CURTAB+4,X'11'      YES-DOLLAR IS A SUFFIX                       
*                                                                               
INITX    J     XIT                                                              
         SPACE 2                                                                
* REPORT LEVELS                                                                 
*                                                                               
RPTLEVS  DC    AL1(QMED)           HEADLINES                                    
         DC    AL1(QCLT)                                                        
         DC    AL1(QPRDGR1)                                                     
         DC    AL1(QPRDGR2)                                                     
         DC    AL1(QPRD)                                                        
         DC    AL1(QEST)                                                        
         DC    AL1(QRPTSEQ)                                                     
         DC    AL1(QMKTGR1)                                                     
         DC    AL1(QMKTGR2)                                                     
         DC    AL1(QMKTGR3)                                                     
         DC    AL1(QMKT)                                                        
         DC    AL1(QRPTSEQ)                                                     
         DC    AL1(QSTA)                                                        
         DC    AL1(QRPTSEQ)                                                     
         DC    AL1(QBUY)           DETAIL                                       
         DC    AL1(QSTAGR1)                                                     
         DC    AL1(QSTAGR2)                                                     
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE REPORT CONTROLS                                            *         
* INPUT  : R2=A(FIELD HEADER)                                         *         
*          R4=A(CONTROL VALIDATION TABLE)                             *         
*          R5=A(CONTROL BYTE)                                         *         
* OUTPUT : CC EQ - OK                                                 *         
*          CC NE - ERROR                                              *         
***********************************************************************         
         SPACE 1                                                                
VALCNTL  NTR1  ,                                                                
         MVI   ERROR,0                                                          
         CLI   5(R2),0                                                          
         BE    VALCX                                                            
         L     R3,AIO1                                                          
         GOTO1 SCANNER,DMCB,(R2),(5,(R3)),C',=,='                               
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BZ    VALC99                                                           
         ST    R4,FULL             SAVE A(VALIDATION TABLE)                     
*                                                                               
VALC2    L     R4,FULL             SCAN TABLE FOR KEYWORD MATCH                 
*                                                                               
VALC4    CLI   0(R4),0                                                          
         BE    VALC99                                                           
         CLC   0(1,R3),1(R4)       MINIMUM LENGTH                               
         BL    VALC6                                                            
         CLC   0(1,R3),2(R4)       MAXIMUM LENGTH                               
         BH    VALC6                                                            
         ZIC   RE,0(R3)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+12                                                          
         BE    VALC8               FOUND                                        
         B     VALC6                                                            
         CLC   12(0,R3),3(R4)                                                   
*                                                                               
VALC6    ZIC   RF,2(R4)                                                         
         LA    R4,3(RF,R4)                                                      
         B     VALC4                                                            
*                                                                               
VALC8    CLI   1(R3),3             TEST YES/NO                                  
         BH    VALC99                                                           
         SR    RE,RE                                                            
         ICM   RE,1,1(R3)                                                       
         BZ    VALC99                                                           
         BCTR  RE,0                                                             
         EX    RE,CLCYES                                                        
         BE    VALC10                                                           
         CLI   1(R3),2                                                          
         BH    VALC99                                                           
         EX    RE,CLCNO                                                         
         BNE   VALC99                                                           
*                                                                               
VALC10   MVC   BYTE,0(R4)          OPTION'S BIT SETTING                         
         CLI   22(R3),C'Y'                                                      
         BNE   *+14                                                             
         OC    0(1,R5),BYTE        TURN OPTION ON                               
         B     VALC12                                                           
         XI    BYTE,X'FF'                                                       
         NC    0(1,R5),BYTE        TURN OPTION OFF                              
*                                                                               
VALC12   LA    R3,32(R3)           NEXT KEYWORD                                 
         BCT   R0,VALC2                                                         
         B     VALCX                                                            
*                                                                               
VALC99   MVI   ERROR,INVALID                                                    
*                                                                               
VALCX    CLI   ERROR,0             SET RETURN CODE                              
         J     XIT                                                              
         SPACE 2                                                                
CLCYES   CLC   22(0,R3),=C'YES'    EXECUTED INSTRUCTIONS                        
CLCNO    CLC   22(0,R3),=C'NO'                                                  
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* FURTHER REQUEST VALIDATION                                          *         
***********************************************************************         
         SPACE 1                                                                
VALID    OC    SBBCLT,SBBCLT       ALL CLIENTS IS INVALID                       
         BZ    ECLT                                                             
*                                                                               
VALX     J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS AND MESSAGES                                            *         
***********************************************************************         
         SPACE 1                                                                
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
ECLT     MVI   ERROR,INVCLT                                                     
         LA    R2,BRSCLTH                                                       
         B     CURSOR                                                           
*                                                                               
MYCURSOR MVI   ERROR,X'FE'                                                      
CURSOR   GOTO1 CURSERR                                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
* SPOTIO'S RECORD HOOK                                                *         
***********************************************************************         
         SPACE 1                                                                
INPUT    L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCSP     HOOK MODE PASSED FROM SPOTIO                 
         BE    PROCBUY                                                          
         CLI   SBMODE,SBPROCGL                                                  
         BE    PROCGOAL                                                         
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS BUY RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
PROCBUY  L     R3,SBAIO1                                                        
         USING BUYRECD,R3                                                       
         OC    WKDSPLS,WKDSPLS     TEST WEEK TABLES SET YET                     
         BNZ   BUY1                                                             
         GOTO1 ASETWKS             NO-DO THEM NOW                               
         CLI   NUMWKS,14           TEST PERIOD GREATER THAN 14 WEEKS            
         BNH   BUY1                                                             
         OI    RTOTIND,RTDET       YES-NEED BUY TOTALS FOR DETAIL RPT           
*                                                                               
BUY1     MVI   SBEUNALL,C'N'       MAKE SURE NO UNALLOCATED                     
         MVI   MKTIND,FF                                                        
         MVC   SBEMKT,SBBMKT       EXTRACT MARKET IN DIRECTORY POINTER          
         CLI   SBQSPILL,C'N'       TEST SPILL REQUESTED                         
         BNE   BUY2                                                             
         CLC   SBBMKT,BUYMSTA      NO-REJECT SPILL MARKET                       
         BNE   BUYX                                                             
         B     BUY3                                                             
*                                  YES -                                        
BUY2     MVC   SBBMKT,BUYMSTA      SET MARKET FOR SPOTBUY                       
         CLI   SBQSPILL,C'C'       TEST COMBINED ORIG + SPILL                   
         BE    BUY3                                                             
         CLI   SBQSPILL,C'S'       TEST SEPERATE ORIG + SPILL                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MKTIND,C'O'         YES -                                        
         CLC   SBEMKT,SBBMKT       TEST SPILL OR ORIG MARKET                    
         BE    BUY3                                                             
         MVI   MKTIND,C'S'                                                      
*                                                                               
BUY3     GOTO1 AGETDPTB            GET DAYPART TABLE                            
         TM    RCPIND,RCPDPT       TEST DAYPART BREAKOUT                        
         BO    *+20                                                             
         CLI   SUMDPT,1                                                         
         BE    *+12                                                             
         CLI   SUMDPT,2                                                         
         BNE   BUY4                                                             
         GOTO1 SETDPT,BDDAYPT      YES-GET DPT DETAILS                          
         MVC   SVDPT,SBDPT         SAVE DAYPART                                 
*                                                                               
BUY4     MVI   SBBPRD,0                                                         
         MVI   SBBPRD2,0                                                        
         XC    SBPRD,SBPRD                                                      
         XC    SBPRD2,SBPRD2                                                    
         MVC   SBEDEMTY,RERATE     SET RERATE TYPE                              
         CLI   NDEMOS,0            UNLESS DEMOS ARE SUPPRESSED                  
         BNE   *+8                                                              
         MVI   SBEDEMTY,0                                                       
         XC    SBSPHOOK,SBSPHOOK                                                
         XC    PURVALS,PURVALS                                                  
         LA    R1,PURVALS                                                       
         LA    RE,SBLOCK                                                        
         AH    RE,=Y(SBAPURVL-SBLOCK)                                           
         ST    R1,0(RE)                                                         
         GOTO1 SPOTBUY,DMCB,SBLOCK     ** CALL SPOTBUY **                       
*                                                                               
         MVI   FSTPASS,C'Y'                                                     
         XC    DEMVALS,DEMVALS                                                  
         L     R5,SBACHUNK                                                      
         USING SCHUNKD,R5                                                       
         MVI   SPILL,C'N'                                                       
         CLC   SBEMKT,SBBMKT       TEST SPILL MARKET                            
         BE    BUY5                                                             
         MVI   SPILL,C'Y'          YES-                                         
         MVC   SBBMKT,SBEMKT       RESTORE MARKET FOR DRIVER                    
         OC    SCNEXT,SCNEXT       EXIT NOW IF ALL SPOTS EXCLUDED               
         BZ    BUY60                                                            
*                                                                               
BUY5     L     R5,SBACHUNK         LOOP THROUGH THE CHUNKS                      
         XC    SVPRDS,SVPRDS                                                    
         L     R1,ARECTOTS                                                      
         XC    0(RECTOTL,R1),0(R1)  CLEAR BUYLINE TOTALS                        
         TM    RTOTIND,RTLENS                                                   
         BZ    BUY5A                                                            
         LA    R0,MAXLENS          CLEAR BUYLINE TOTALS BY SPOT LENGTH          
         L     R1,ARECTOT1                                                      
         XC    0(RECTOTL,R1),0(R1)                                              
         LA    R1,RECTOTL(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
BUY5A    LA    R1,GRID             CLEAR GRID                                   
         LA    R0,MAXQTRS                                                       
         XC    0(GRIDL,R1),0(R1)                                                
         LA    R1,GRIDL(R1)                                                     
         BCT   R0,*-10                                                          
         LA    R1,MONTOTS          CLEAR MONTHLY TOTALS                         
         LA    R0,MAXMONS                                                       
         XC    0(MONTOTL,R1),0(R1)                                              
         LA    R1,MONTOTL(R1)                                                   
         BCT   R0,*-10                                                          
         TM    MTOTIND,MTLENS                                                   
         BZ    BUY6                                                             
         LA    RF,MAXLENS          CLEAR MONTHLY TOTALS BY SPOT LENGTH          
         LA    R1,MONTOTS1                                                      
         LA    R0,MAXMONS                                                       
         XC    0(MONTOTL,R1),0(R1)                                              
         LA    R1,MONTOTL(R1)                                                   
         BCT   R0,*-10                                                          
         BCT   RF,*-18                                                          
*                                                                               
BUY6     OC    SCNEXT,SCNEXT       TEST END OF CHUNKS                           
         BZ    BUY40                                                            
         OC    SCDATE,SCDATE       TEST USED THIS CHUNK ALREADY                 
         BZ    BUY38               YES-SKIP                                     
         CLI   SBQBPRD,0           TEST ALL PRODUCT REQUEST                     
         BNE   BUY7                                                             
         CLI   SCPRD1,FF           YES-TEST THIS CHUNK IS FOR POL               
         BNE   BUY8                NO-OK                                        
         CLI   SUMMY,3             YES-ONLY ACCEPT IF THERE'S A MKT             
         BE    BUY8                    PERFORMANCE REPORT                       
         B     BUY38                                                            
*                                                                               
BUY7     CLC   SCPRD1,SBQBPRD      NO-MATCH THE PRODUCT                         
         BE    BUY8                                                             
         CLI   SCPRD2,0            TEST SECOND PRODUCT                          
         BE    BUY38                                                            
         CLC   SCPRD2,SBQBPRD      YES-PRD CAN MATCH EITHER ONE                 
         BNE   BUY38                                                            
*                                                                               
BUY8     CLI   FSTPASS,C'Y'        TEST FIRST PASS OF CHUNKS                    
         BNE   BUY18                                                            
         CLI   SCPRD2,0            YES-TEST SECOND PRODUCT                      
         BE    BUY18                                                            
         CLI   SBEPRD,0            YES-TEST SINGLE PRODUCT REQUESTED            
         BE    BUY10                   THIS TIME                                
         CLI   SBEPRD,X'FF'                                                     
         BE    BUY10                                                            
         CLC   SCPRD1,SBEPRD       YES-REQUESTED PRD COMES FIRST                
         BNE   BUY16                                                            
         B     BUY18                                                            
*                                                                               
BUY10    LA    R0,2                GET THE PRODUCTS' ALPHA CODES                
         LA    R1,WORK                                                          
         LA    RF,SCPRD1                                                        
*                                                                               
BUY12    ZIC   RE,0(RF)                                                         
         BCTR  RE,0                                                             
         MH    RE,=Y(PRDBUFFL)                                                  
         L     RF,SBAPRDBF                                                      
         AR    RF,RE                                                            
         MVC   0(3,R1),PBALPH-PRDBUFFD(RF)                                      
*                                                                               
BUY14    LA    R1,3(R1)                                                         
         LA    RF,SCPRD2                                                        
         BCT   R0,BUY12                                                         
         CLC   WORK(3),WORK+3      TEST 1ST PRD GREATER THAN 2ND PRD            
         BNH   BUY18               NO                                           
*                                                                               
BUY16    MVC   WORK(L'SCKEY),SCKEY   SWITCH THE PRODUCTS                        
         MVC   SCPRD1,SCPRD2                                                    
         MVC   SCSLN1,SCSLN2                                                    
         MVC   SCPRD2,WORK+SCPRD1-SCKEY                                         
         MVC   SCSLN2,WORK+SCSLN1-SCKEY                                         
*                                                                               
BUY18    OC    SVPRDS,SVPRDS       TEST CURRENT PRODUCTS SET                    
         BNZ   *+14                                                             
         MVC   SVPRDS,SCPRD1       NO-SET THEM NOW                              
         B     *+14                                                             
         CLC   SVPRDS,SCPRD1       YES-MATCH AGAINST CURRENT PRDS               
         BNE   BUY38                                                            
         CLC   SBBPRD,SCPRD1       TEST CHANGE OF PRODUCT                       
         BE    BUY21                                                            
         MVC   SBBPRD,SCPRD1       YES - SET PRODUCT CODE FOR DRIVER            
         ZIC   RE,SBBPRD                                                        
         BCTR  RE,0                                                             
         MH    RE,=Y(PRDBUFFL)                                                  
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRD,PBALPH                                                     
         MVC   SBBPGR,PBGROUP      SET PRODUCT GROUP                            
         OC    SBBPGR,SBBPGR                                                    
         BNZ   BUY20                                                            
         CLC   SBQPGRF,BLANKS      PRODUCT GROUP UNKNOWN                        
         BH    BUY38               IF PRDGRP FILTER, THEN IGNORE PRD            
         MVC   SBBPGR,=X'9999'                                                  
*                                                                               
BUY20    CLI   SBQMGR,0            TEST MARKET GROUPS                           
         BE    BUY21                                                            
         CLC   SBQPGRF,BLANKS      AND NOT PRODUCT GROUP FILTERING              
         BH    BUY21                                                            
         CLI   BUYKPRD,X'FF'       AND PRD=POL                                  
         BNE   BUY21                                                            
         OC    SBPGRPEX(2),SBPGRPEX  AND THERE ARE PRDGRP EXCEPTIONS            
         BZ    BUY21                                                            
         GOTO1 AGETMGR             YES-GET THE MARKET GROUP                     
*                                                                               
BUY21    CLC   SBBPRD2,SCPRD2      TEST CHANGE OF SECOND PRD                    
         BE    BUY22                                                            
         MVC   SBBPRD2,SCPRD2                                                   
         XC    SBPRD2,SBPRD2                                                    
         CLI   SBBPRD2,0                                                        
         BE    BUY22                                                            
         ZIC   RE,SBBPRD2                                                       
         BCTR  RE,0                                                             
         MH    RE,=Y(PRDBUFFL)                                                  
         L     R1,SBAPRDBF                                                      
         AR    R1,RE                                                            
         USING PRDBUFFD,R1                                                      
         MVC   SBPRD2,PBALPH                                                    
         DROP  R1                                                               
*                                                                               
BUY22    CLC   SBBPRD,SVBPRD       TEST PRODUCT/ESTIMATE CHANGE                 
         BNE   *+14                                                             
         CLC   SBBEST,SVBEST                                                    
         BE    BUY22B                                                           
         MVC   SVBPRD,SBBPRD       YES-GET A(DEMOS)                             
         MVC   SVBEST,SBBEST                                                    
         LA    RE,SBEDEMOS                                                      
         OC    SBEDEMOS,SBEDEMOS   TEST DEMO LIST OVERRIDE                      
         BNZ   BUY22A                                                           
         ZIC   RE,SBBPRD           FIND ENTRY IN PRD/EST TABLE                  
         CLI   SBBPRD,X'FE'                                                     
         BNE   *+8                                                              
         LA    RE,255              PRD=POL FOR UNALLOCATED                      
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         ZIC   RF,SBBEST                                                        
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         A     RE,SBAESTTB                                                      
         SR    R1,R1                                                            
         ICM   R1,1,0(RE)          POINTER TO ESTIMATE BUFFER ENTRY             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         MH    R1,=Y(ESTBUFFL)                                                  
         A     R1,SBAESTBF                                                      
         USING ESTBUFFD,R1                                                      
         LA    RE,EBDEMOS           EXTRACT A(DEMOS)                            
         DROP  R1                                                               
*                                                                               
BUY22A   ST    RE,ADEMLST                                                       
*                                                                               
BUY22B   CLI   SPILL,C'Y'          TEST SPILL                                   
         BNE   *+18                                                             
         L     R1,SCSPOTS          YES-CLEAR ALL EXCEPT DEMOS AND SPOTS         
         XC    SCDATA(SCDEMOS-SCDATA),SCDATA                                    
         ST    R1,SCSPOTS                                                       
*                                                                               
         ZIC   RE,SCSLN1           GET SPOT LENGTH                              
         ZIC   RF,SCSLN2                                                        
         AR    RE,RF                                                            
         STC   RE,SBLEN                                                         
*                                                                               
         CLI   SUMMY,0             TEST SUMMARY                                 
         BE    BUY23                                                            
         NI    ININD,255-INIEQUIV  YES-SET EQUIVALENCING INDICATOR              
         CLI   SBSPPROF+4,C'Y'     EQUIV ALL DETAIL LINES                       
         BE    *+12                                                             
         CLI   SUMDPT,1            TEST DPTLEN BREAKOUT                         
         BE    BUY23               YES                                          
         OI    ININD,INIEQUIV                                                   
*                                                                               
BUY23    MVI   MPRPRD,C'N'         SET MKT PERFORMANCE RPT PRODUCT IND          
         CLI   SUMMY,3                                                          
         BNE   BUY24                                                            
         MVI   MPRPRD,C'P'         POL                                          
         CLI   SCPRD1,FF                                                        
         BE    BUY24                                                            
         MVI   MPRPRD,C'B'         BRAND                                        
         MVC   SVPRD,SBPRD         SAVE ACTUAL PRODUCT                          
         MVC   SVPRD2,SBPRD2                                                    
         MVC   SBPRD,=C'POL'       AND FUDGE HEADLINE PRODUCT TO POL            
         MVC   SBPRD2,BLANKS                                                    
*                                                                               
BUY24    CLI   WTOTIND,0           TEST WEEKLY TOTALS NEEDED                    
         BE    BUY25                                                            
         CLI   WTOTIND,WTSUM       YES-IF NEED WEEKS ONLY FOR SUMMARY,          
         BNE   *+12                                                             
         CLI   MPRPRD,C'P'         DON'T ALLOW PRD=POL                          
         BE    BUY25                                                            
         TM    SUMIND,SUMWKY       TEST BRAND WEEKLY SUMMARY                    
         BZ    *+8                                                              
         MVI   SUMSEQ,1            YES-SET SEQUENCE ABOVE WEEKS TO 1            
         MVI   PERTYP,PERWKS       CALL DRIVER WITH WEEKLY CHUNKS               
         ST    R5,SBACURCH                                                      
         BAS   RE,DRIVIN                                                        
*                                                                               
BUY25    TM    RTOTIND,RTTOT       TEST BUYLINE TOTALS NEEDED                   
         BZ    *+12                                                             
         L     R3,ARECTOTS         YES-                                         
         USING RECTOTD,R3                                                       
         BAS   RE,ADDBTOT          ADD UP BUYLINE TOTALS                        
         TM    RTOTIND,RTLENS      TEST NEED SPOT LENGTH TOTALS                 
         BZ    BUY28                                                            
         CLI   SBLEN,0             YES-CHECK NON-ZERO LENGTH                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R0,MAXLENS                                                       
         L     R3,ARECTOT1                                                      
*                                                                               
BUY26    CLI   RECSLN,0                                                         
         BE    BUY27                                                            
         CLC   RECSLN,SBLEN                                                     
         BE    BUY27                                                            
         LA    R3,RECTOTL(R3)                                                   
         BCT   R0,BUY26                                                         
         DC    H'0'                                                             
*                                                                               
BUY27    MVC   RECSLN,SBLEN                                                     
         BAS   RE,ADDBTOT          ADD UP TOTALS FOR SPOT LENGTH                
*                                                                               
BUY28    L     R2,AWEEKS           FIND WHICH WEEK WE'RE IN                     
         LA    R4,WKDSPLS                                                       
*                                                                               
BUY29    CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SCDATE,0(R2)                                                     
         BE    *+16                                                             
         LA    R2,4(R2)                                                         
         LA    R4,3(R4)                                                         
         B     BUY29                                                            
         XC    SCDATE,SCDATE       INDICATE THIS CHUNK'S BEEN PROCESSED         
*                                                                               
         TM    MTOTIND,MTTOT       TEST NEED MONTHLY TOTALS                     
         BZ    BUY30                                                            
         ZIC   R3,2(R4)            YES-ACCUMULATE MONTHLY TOTALS                
         BCTR  R3,0                                                             
         MH    R3,=Y(MONTOTL)                                                   
         LA    R3,MONTOTS(R3)                                                   
         USING MONTOTD,R3                                                       
         BAS   RE,ADDMTOT                                                       
*                                                                               
BUY30    TM    MTOTIND,MTLENS      TEST NEED MONTHLY LENGTH TOTALS              
         BZ    BUY32                                                            
         CLI   SBLEN,0             YES-CHECK NON-ZERO LENGTH                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R0,MAXLENS                                                       
         LA    R1,MONTSLN1                                                      
*                                                                               
BUY30A   CLI   0(R1),0                                                          
         BE    BUY31                                                            
         CLC   SBLEN,0(R1)                                                      
         BE    BUY31                                                            
         LA    R1,MAXMONS*MONTOTL+1(R1)                                         
         BCT   R0,BUY30A                                                        
         DC    H'0'                                                             
*                                                                               
BUY31    MVC   0(1,R1),SBLEN                                                    
         ZIC   R3,2(R4)                                                         
         BCTR  R3,0                                                             
         MH    R3,=Y(MONTOTL)                                                   
         LA    R3,1(R3,R1)                                                      
         BAS   RE,ADDMTOT          ADD UP TOTALS FOR SPOT LENGTH                
*                                                                               
BUY32    DS    0H                                                               
         CLI   MPRPRD,C'B'         DO WE NEED GRID FOR THIS PRODUCT             
         BE    BUY38                                                            
         ZIC   R3,0(R4)            YES-EXTRACT QUARTER NUMBER                   
         BCTR  R3,0                                                             
         MH    R3,=Y(GRIDL)                                                     
         LA    R3,GRID(R3)                                                      
         USING GRIDD,R3                                                         
         ZIC   RF,1(R4)            WEEK NUMBER WITHIN QUARTER                   
         SLL   RF,2                                                             
         LA    RF,GRSPW-4(RF)                                                   
         MVC   0(2,RF),SCSPOTS+2                                                
         L     R1,GRCOST           ACCUMULATE QUARTERLY COST                    
         A     R1,SCGROSS                                                       
         ST    R1,GRCOST                                                        
         L     R1,GRCOST+4         AND EQUIVALENCED COST                        
         A     R1,SCEGROSS                                                      
         ST    R1,GRCOST+4                                                      
         L     R1,GRSPTS           AND QUARTERLY N'SPOTS                        
         A     R1,SCSPOTS                                                       
         ST    R1,GRSPTS                                                        
*                                                                               
         SR    R0,R0               PUT DEMOS TO GRID                            
         ICM   R0,1,NDEMOS                                                      
         BZ    BUY38                                                            
         LA    R1,SCDEMOS                                                       
         LA    R2,PURVALS          R2=A(PURCH DEMO VALUES)                      
         LA    R4,GRDEMS           R4=A(DEMO GRID)                              
         LA    R7,DEMVALS          R7=A(SINGLE SPOT DEMO VALUES)                
*                                                                               
BUY33    L     RF,0(R1)            ACCUMULATE QUARTERLY DEMO VALUE              
         A     RF,0(R4)                                                         
         ST    RF,0(R4)                                                         
         L     RF,4(R1)            AND EQUIVALENCED VALUE                       
         A     RF,4(R4)                                                         
         ST    RF,4(R4)                                                         
         OC    0(4,R7),0(R7)       TEST SINGLE SPOT DEMO SET YET                
         BNZ   BUY36                                                            
         ICM   RF,15,0(R1)         NO-                                          
         BZ    BUY36                                                            
         SR    RE,RE               CALCULATE IT                                 
         SLL   RF,1                                                             
         D     RE,SCSPOTS          DEMO VALUE / N' SPOTS                        
         LA    RF,1(RF)                                                         
         SRL   RF,1                                                             
         SLL   RF,1                STORE VALUE X 2                              
         CLI   SBEDEMTY,C'P'       TEST PURCHASED                               
         BNE   BUY34                                                            
         TM    DETIND,DETOVR       AND OVERRIDES SHOULD BE FLAGGED              
         BZ    BUY34                                                            
         TM    0(R2),X'80'         AND BUYER HAS OVERRIDE                       
         BZ    BUY34                                                            
         LA    RF,1(RF)            YES-LOW ORDER BIT INDICATES OVERRIDE         
*                                                                               
BUY34    ST    RF,0(R7)                                                         
*                                                                               
BUY36    LA    R1,8(R1)                                                         
         LA    R2,4(R2)                                                         
         LA    R4,L'GRDEMS(R4)                                                  
         LA    R7,4(R7)                                                         
         BCT   R0,BUY33                                                         
*                                                                               
BUY38    L     R4,AGLOBAL          RESTORE A(GLOBAL)                            
         L     R5,SCNEXT           NEXT CHUNK                                   
         B     BUY6                                                             
*                                                                               
BUY40    OC    SVPRDS,SVPRDS       TEST PICKED UP PRD(S) ON THIS PASS           
         BZ    BUY60                                                            
         MVI   PERTYP,PERGRD       YES-CALL DRIVER FOR BUYLINE DETAIL           
         LA    R2,1                PASS EACH QUARTER TO DRIVIN                  
         LA    R3,GRID                                                          
         LA    RF,MAXQTRS                                                       
*                                                                               
BUY42    OC    0(GRIDL,R3),0(R3)                                                
         BZ    *+12                                                             
         STC   R2,QTRNUM                                                        
         BAS   RE,DRIVIN                                                        
         LA    R2,1(R2)                                                         
         LA    R3,GRIDL(R3)                                                     
         BCT   RF,BUY42                                                         
*                                                                               
         MVC   MONTH,XFF                                                        
         MVI   SUMSEQ,2                                                         
         L     R1,ARECTOTS                                                      
         ST    R1,ATOTS                                                         
         CLI   MPRPRD,C'B'         EXCEPT IF PRODUCT ISN'T NEEDED,              
         BE    BUY43                                                            
         TM    RTOTIND,RTDET       TEST PERIOD GT 14 WEEKS                      
         BZ    BUY43                                                            
         MVI   QTRNUM,0            YES-CALL DRIVER FOR WHOLE PERIOD             
         BAS   RE,DRIVIN                                                        
*                                                                               
BUY43    TM    RTOTIND,RTLENS      TEST NEED BUY TOTS BY SPOT LENGTH            
         BZ    BUY46                                                            
         TM    RTOTIND,RTRCPL      YES-IF ONLY FOR SUMMARY,                     
         BO    *+12                                                             
         CLI   MPRPRD,C'P'         CHECK PRODUCT ISN'T NEEDED                   
         BE    BUY46                                                            
         MVI   PERTYP,PERTOTL                                                   
         LA    RF,MAXLENS          CALL DRIVIN ONCE FOR EACH LENGTH             
         L     R3,ARECTOT1                                                      
         USING RECTOTD,R3                                                       
*                                                                               
BUY44    CLI   RECSLN,0                                                         
         BE    BUY46                                                            
         MVC   SBLEN,RECSLN                                                     
         ST    R3,ATOTS                                                         
         BAS   RE,DRIVIN                                                        
         LA    R3,RECTOTL(R3)                                                   
         BCT   RF,BUY44                                                         
*                                                                               
BUY46    TM    RTOTIND,RTTOT       TEST NEED BUY TOTS                           
         BZ    BUY48                                                            
         TM    RTOTIND,RTRCP       YES-IF ONLY FOR SUMMARY,                     
         BO    *+12                                                             
         CLI   MPRPRD,C'P'         CHECK PRODUCT ISN'T NEEDED                   
         BE    BUY48                                                            
         MVI   PERTYP,PERTOT       YES                                          
         MVI   SBLEN,255           ACROSS DAYPART/LENGTHS                       
         L     R1,ARECTOTS                                                      
         ST    R1,ATOTS                                                         
         BAS   RE,DRIVIN                                                        
*                                                                               
BUY48    TM    MTOTIND,MTTOT       TEST MONTHLY TOTALS                          
         BZ    BUY49                                                            
         TM    MTOTIND,MTRCP       YES-IF ONLY FOR SUMMARY,                     
         BO    *+12                                                             
         CLI   MPRPRD,C'P'         CHECK PRODUCT ISN'T NEEDED                   
         BE    BUY49                                                            
         MVI   SBLEN,255           ACROSS DAYPART/LENGTHS                       
         TM    MTOTIND,MTRCP       PASS MONTHS FOR RECAP                        
         BO    *+12                                                             
         CLI   SUMMY,2                 AND WEEKLY SUMMARY                       
         BNE   *+16                                                             
         MVI   PERTYP,PERMTH                                                    
         LA    R3,MONTOTS                                                       
         BAS   RE,GENMONS                                                       
         TM    SUMIND,SUMMTH       PASS QUARTERS FOR MONTHLY MARKET             
         BZ    BUY49               PERFORMANCE                                  
         MVI   PERTYP,PERSUM                                                    
         LA    R3,MONTOTS                                                       
         BAS   RE,GENMONS                                                       
*                                                                               
BUY49    TM    MTOTIND,MTLENS      TEST MONTHLY TOTALS BY LENGTH                
         BZ    BUY52                                                            
         TM    MTOTIND,MTRCPL      YES-IF ONLY FOR SUMMARY,                     
         BO    *+12                                                             
         CLI   MPRPRD,C'P'         CHECK PRODUCT ISN'T NEEDED                   
         BE    BUY52                                                            
         LA    R1,MAXLENS          GENERATE MONTHLY RECS FOR EACH               
         LA    R3,MONTSLN1         SPOT LENGTH                                  
         MVC   SBDPT,SVDPT                                                      
*                                                                               
BUY50    CLI   0(R3),0                                                          
         BE    BUY52                                                            
         MVC   SBLEN,0(R3)                                                      
         LA    R3,1(R3)                                                         
         ST    R3,FULL                                                          
         TM    MTOTIND,MTRCPL      PASS MONTHS FOR RECAP                        
         BO    *+12                                                             
         CLI   SUMMY,2             AND WEEKLY SUMMARY                           
         BNE   *+12                                                             
         MVI   PERTYP,PERMTHL                                                   
         BAS   RE,GENMONS                                                       
         TM    SUMIND,SUMMTH       PASS QUARTERS FOR MONTHLY MARKET             
         BZ    *+16                PERFORMANCE                                  
         MVI   PERTYP,PERSUM                                                    
         L     R3,FULL                                                          
         BAS   RE,GENMONS                                                       
         BCT   R1,BUY50                                                         
*                                                                               
BUY52    MVI   FSTPASS,C'N'        AGAIN FOR MORE PRODUCTS                      
         B     BUY5                                                             
*                                                                               
BUY60    XC    SBEMKT,SBEMKT                                                    
         MVI   RPMODE,RPSKIOHK     SKIP SPWRI01'S PROCBUY                       
*                                                                               
BUYX     J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* MISC PROCBUY ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
ADDBTOT  LR    R0,RE                                                            
         USING RECTOTD,R3                                                       
         USING SCHUNKD,R5                                                       
         L     R1,RECCOST          ACCUMULATE TOTAL BUYLINE COST                
         A     R1,SCGROSS                                                       
         ST    R1,RECCOST                                                       
         L     R1,RECCOST+4        AND EQUIV COST                               
         A     R1,SCEGROSS                                                      
         ST    R1,RECCOST+4                                                     
         L     R1,RECSPTS          AND SPOTS                                    
         A     R1,SCSPOTS                                                       
         ST    R1,RECSPTS                                                       
         SR    R2,R2               AND DEMOS                                    
         ICM   R2,1,NDEMOS                                                      
         BZ    ADDBTOTX                                                         
         LA    RE,SCDEMOS                                                       
         LA    RF,RECDEMS                                                       
*                                                                               
ADDBTOT2 L     R1,0(RF)                                                         
         A     R1,0(RE)                                                         
         ST    R1,0(RF)                                                         
         L     R1,4(RF)                                                         
         A     R1,4(RE)                                                         
         ST    R1,4(RF)                                                         
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R2,ADDBTOT2                                                      
*                                                                               
ADDBTOTX LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
ADDMTOT  LR    R0,RE                                                            
         USING MONTOTD,R3                                                       
         USING SCHUNKD,R5                                                       
         L     R1,MTCOST           ACCUMULATE MONTHLY COST                      
         A     R1,SCGROSS                                                       
         ST    R1,MTCOST                                                        
         L     R1,MTCOST+4         AND EQUIV COST                               
         A     R1,SCEGROSS                                                      
         ST    R1,MTCOST+4                                                      
         L     R1,MTSPTS           AND SPOTS                                    
         A     R1,SCSPOTS                                                       
         ST    R1,MTSPTS                                                        
         SR    R2,R2               AND DEMOS                                    
         ICM   R2,1,NDEMOS                                                      
         BZ    ADDMTOTX                                                         
         LA    RE,SCDEMOS                                                       
         LA    RF,MTDEMS                                                        
*                                                                               
ADDMTOT2 L     R1,0(RF)                                                         
         A     R1,0(RE)                                                         
         ST    R1,0(RF)                                                         
         L     R1,4(RF)                                                         
         A     R1,4(RE)                                                         
         ST    R1,4(RF)                                                         
         LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R2,ADDMTOT2                                                      
*                                                                               
ADDMTOTX LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
GENMONS  ST    RE,SAVERE                                                        
         CLI   PERTYP,PERSUM                                                    
         BE    GENMONS6                                                         
         MVI   SUMSEQ,2            WEEKLY SUMMARY SEQUENCE NUM                  
         L     R2,AMONTHS                                                       
         LA    RF,MAXMONS                                                       
*                                                                               
GENMONS2 OC    0(MONTOTL,R3),0(R3)                                              
         BZ    GENMONS4                                                         
         MVC   MONTH,0(R2)                                                      
         ST    R3,ATOTS                                                         
         BAS   RE,DRIVIN                                                        
*                                                                               
GENMONS4 LA    R2,4(R2)                                                         
         LA    R3,MONTOTL(R3)                                                   
         BCT   RF,GENMONS2                                                      
         B     GENMONSX                                                         
*                                                                               
GENMONS6 LA    R2,1                                                             
         LA    RF,4                                                             
*                                                                               
GENMONS8 OC    0(3*MONTOTL,R3),0(R3)                                            
         BZ    *+16                                                             
         STC   R2,QTRNUM                                                        
         ST    R3,ATOTS                                                         
         BAS   RE,DRIVIN                                                        
         LA    R2,1(R2)                                                         
         LA    R3,3*MONTOTL(R3)                                                 
         BCT   RF,GENMONS8                                                      
*                                                                               
GENMONSX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS GOAL RECORD                                                 *         
***********************************************************************         
         SPACE 1                                                                
PROCGOAL L     R3,SBAIO1                                                        
         USING GOALRECD,R3                                                      
         MVI   MKTLEV,0            SUPPRESS COMBINED MARKET FOR SPILL           
         OC    WKDSPLS,WKDSPLS     TEST WEEK TABLES SET YET                     
         BNZ   GOAL2                                                            
         GOTO1 ASETWKS             NO-DO THEM NOW                               
*                                                                               
GOAL2    CLC   SBBPRD,GKEYPRD      TEST PASSIVE POINTER                         
         BNE   GOALX               YES - IGNORE THIS RECORD                     
         GOTO1 AGETDPTB            GET DAYPART TABLE                            
         GOTO1 SETDPT,GKEYDPT      SET DAYPART DETAILS                          
         MVI   SBBPRD,0            INITIALIZE PRODUCTS                          
         MVI   SBBPRD2,0                                                        
         XC    SBPRD,SBPRD                                                      
         XC    SBPRD2,SBPRD2                                                    
         MVI   SVBPRD,0                                                         
         MVI   SVBEST,0                                                         
         MVI   SBEDEMTY,0                                                       
         TM    DATAIND2,DIGLDEM    TEST GOAL DEMOS NEEDED                       
         BZ    *+8                                                              
         MVI   SBEDEMTY,C'G'       YES - SET DEMO TYPE TO GOAL                  
         MVI   SBEPRD,0                                                         
         CLI   SBQBPRD,0                                                        
         BE    GOAL4                                                            
         CLI   SBQBPRD,X'FF'                                                    
         BE    GOAL4                                                            
         MVC   SBEPRD,SBQBPRD                                                   
*                                                                               
GOAL4    GOTO1 SPOTGOAL,PARAS,SBLOCK     CALL SPOTGOAL                          
*                                                                               
         MVI   MKTIND,FF                                                        
         CLI   SBQSPILL,C'S'       TEST SPILL=YES                               
         BNE   *+8                                                              
         MVI   MKTIND,C'O'         YES-SET MARKET TO ORIGINATING                
         L     R5,SBACHUNK         R5=A(CHUNK)                                  
         USING SGLCHNKD,R5                                                      
*                                                                               
GOAL6    OC    SGNEXT,SGNEXT       TEST END OF CHUNKS                           
         BZ    GOAL40                                                           
         CLI   SBQBPRD,0           TEST PRD=ALL                                 
         BNE   GOAL8                                                            
         CLI   SGPRD1,FF           YES-REJECT PRD=POL                           
         BE    GOAL30                                                           
         B     GOAL10                                                           
*                                                                               
GOAL8    CLC   SGPRD1,SBQBPRD      NO-CHECK PRODUCT                             
         BNE   GOAL30                                                           
*                                                                               
GOAL10   CLC   SBBPRD,SGPRD1       TEST CHANGE OF PRODUCT                       
         BE    GOAL11                                                           
         MVC   SBBPRD,SGPRD1       YES - SET PRODUCT CODE FOR DRIVER            
         ZIC   RE,SBBPRD                                                        
         BCTR  RE,0                                                             
         MH    RE,=Y(PRDBUFFL)                                                  
         L     R1,SBAPRDBF                                                      
         AR    RE,R1                                                            
         USING PRDBUFFD,RE                                                      
         MVC   SBPRD,PBALPH                                                     
         MVC   SBBPGR,PBGROUP      SET PRODUCT GROUP                            
         OC    SBBPGR,SBBPGR                                                    
         BNZ   GOAL12                                                           
         CLC   SBQPGRF,BLANKS      PRODUCT GROUP UNKNOWN                        
         BH    GOAL30              IF PRDGRP FILTER, THEN IGNORE PRD            
         MVC   SBBPGR,=X'9999'                                                  
         DROP  RE                                                               
*                                                                               
GOAL11   CLC   SVBPRD,SBBPRD       TEST CHANGE OF PRODUCT/ESTIMATE              
         BNE   *+14                                                             
         CLC   SVBEST,GKEYEST                                                   
         BE    GOAL12                                                           
         MVC   SVBPRD,SBBPRD       YES                                          
         MVC   SVBEST,GKEYEST                                                   
         TM    DATAIND2,DIGLDEM    TEST GOAL DEMOS                              
         BZ    GOAL12                                                           
         OC    SBPDEMOS,SBPDEMOS   YES-TEST DEMO MENU OR DEMO OPTION            
         BZ    GOAL12                                                           
         GOTO1 AGETEST,SBPRD       YES-GET ESTIMATE DEMOS                       
*                                                                               
GOAL12   CLC   SBBPRD2,SGPRD2      TEST CHANGE OF SECOND PRD                    
         BE    GOAL14                                                           
         MVC   SBBPRD2,SGPRD2                                                   
         XC    SBPRD2,SBPRD2                                                    
         CLI   SBBPRD2,0                                                        
         BE    GOAL14                                                           
         ZIC   RE,SBBPRD2                                                       
         BCTR  RE,0                                                             
         MH    RE,=Y(PRDBUFFL)                                                  
         L     R1,SBAPRDBF                                                      
         AR    RE,R1                                                            
         MVC   SBPRD2,PBALPH-PRDBUFFD(RE)                                       
*                                                                               
GOAL14   CLI   SUMMY,0             TEST SUMMARY                                 
         BE    GOAL16                                                           
         NI    ININD,FF-INIEQUIV   YES-TURN OFF EQUIVALENCING                   
         CLI   SBSPPROF+4,C'Y'     EQUIV DETAIL LINES                           
         BE    *+12                                                             
         CLI   SUMDPT,1            TEST DPTLEN BREAKOUT                         
         BE    GOAL16              YES                                          
         OI    ININD,INIEQUIV                                                   
*                                                                               
GOAL16   MVC   SBLEN,SGSLNT        SET THE SPOT LENGTH                          
         MVI   PERTYP,PERWKS       SET PERIOD TYPE TO WEEKS                     
         ST    R5,SBACURCH         A(CURRENT EXTRACT CHUNK)                     
         CLI   WTOTIND,0           TEST WEEKLY TOTALS NEEDED                    
         BE    GOAL18                                                           
         TM    SUMIND,SUMWKY       TEST BRAND WEEKLY SUMMARY                    
         BZ    *+8                                                              
         MVI   SUMSEQ,1            YES-SET SEQUENCE ABOVE WEEKS TO 1            
         MVI   GOALIND,GIWKS       SET GOALS INDICATOR TO WEEKS                 
         BAS   RE,DRIVIN           CALL DRIVER FOR INPUT                        
*                                                                               
GOAL18   TM    MTOTIND,MTSUM+MTSUML   TEST SUMMARY MONTH BREAKOUT               
         BZ    GOAL20                                                           
         MVI   SUMSEQ,2            YES-                                         
         MVI   GOALIND,GIMNTHS                                                  
         BAS   RE,DRIVIN           CALL DRIVER FOR MONTHS                       
*                                                                               
GOAL20   TM    RTOTIND,RTSUM+RTSUML   TEST TOTAL PERIOD FOR SUMMARIES           
         BZ    GOAL30                                                           
         MVI   SUMSEQ,2            YES-                                         
         MVI   GOALIND,GITOTAL                                                  
         BAS   RE,DRIVIN           CALL DRIVER FOR TOTAL PERIOD                 
*                                                                               
GOAL30   L     R5,SGNEXT           NEXT CHUNK                                   
         B     GOAL6                                                            
*                                                                               
GOAL40   MVI   RPMODE,RPSKIOHK     SKIP SPWRI01'S PROCGOAL                      
*                                                                               
GOALX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER INPUT ROUTINE                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING GLOBALD,R4                                                       
DRIVIN   ST    RE,DRIVINRE                                                      
         MVI   GLMODE,GLINPUT                                                   
         MVI   DPLTOT,C'N'                                                      
         BAS   RE,GODRIVER                                                      
         MVC   SVDPTLEN,SBDPTLEN                                                
         MVC   SVININD,ININD                                                    
         MVI   DPLTOT,C'Y'                                                      
         BAS   RE,GENDPTOT                                                      
         CLI   MPRPRD,C'B'         TEST MARKET PERFORMANCE REPORT BRAND         
         BNE   DRIVINX                                                          
         MVC   SBDPTLEN,SVDPTLEN                                                
         MVC   ININD,SVININD                                                    
         MVC   MYWORK(6),SVPRD     YES-CALL DRIVER AGAIN WITH                   
         MVC   SVPRD(6),XFF            ALL BRANDS                               
         MVI   DPLTOT,C'N'                                                      
         BAS   RE,GODRIVER                                                      
         MVI   DPLTOT,C'Y'                                                      
         BAS   RE,GENDPTOT                                                      
         MVC   SVPRD(6),MYWORK                                                  
DRIVINX  MVC   SBDPTLEN,SVDPTLEN                                                
         L     RE,DRIVINRE                                                      
         BR    RE                                                               
         SPACE 2                                                                
GENDPTOT LR    R0,RE                                                            
         CLI   SUMDPT,1            TEST SUMMARY WITH DPT(LEN) BREAKOUT          
         BE    *+10                                                             
         CLI   SUMDPT,2                                                         
         BNER  RE                                                               
         CLI   PERTYP,PERGRD       YES-GENERATE DPTLEN TOTAL RECORDS            
         BER   RE                      IF APPROPRIATE                           
         CLI   PERTYP,PERSUM                                                    
         BE    GENDPT2                                                          
         CLI   PERTYP,PERWKS       TEST WEEKS                                   
         BNE   GENDPT1                                                          
         CLI   SBMODE,SBPROCGL     YES-ALWAYS OK FOR GOALS                      
         BE    GENDPT2                                                          
         TM    WTOTIND,WTSUM       BUYS-ONLY IF NEED WKS FOR SUMMARIES          
         BO    GENDPT2                                                          
         BR    RE                                                               
*                                                                               
GENDPT1  CLI   PERTYP,PERMTH                                                    
         BE    *+12                                                             
         CLI   PERTYP,PERMTHL                                                   
         BNE   *+12                                                             
         TM    MTOTIND,MTSUM+MTSUML                                             
         BNZ   GENDPT2                                                          
         CLI   PERTYP,PERTOT                                                    
         BE    *+10                                                             
         CLI   PERTYP,PERTOTL                                                   
         BNER  RE                                                               
         TM    RTOTIND,RTSUM+RTSUML                                             
         BZR   RE                                                               
*                                                                               
GENDPT2  OI    ININD,INIEQUIV      TURN ON EQUIVALENCING                        
         CLI   SUMDPT,1            IF DPTLEN BREAKOUT,                          
         BNE   *+12                                                             
         MVI   SBLEN,FF            PUT DAYPART TOTAL                            
         BAS   RE,GODRIVER                                                      
         MVC   SBDPT,XFF                                                        
         OC    SBDPTGRP+1(3),SBDPTGRP+1 TEST FOR DAYPART GROUP                  
         BZ    *+8                                                              
         BAS   RE,GODRIVER              YES-PUT GROUP TOTAL                     
         MVC   SBDPTGRP,XFF                                                     
         CLI   SUMDPT,1            TEST DAYPART/LENGTH                          
         BNE   GENDPT4                                                          
         TM    SBQDPTLN,SBQDLTOT   YES-TEST SPOT LENGTH TOTALS                  
         BZ    GENDPT4                                                          
         MVC   SBLEN,SVDPTLEN+SBLEN-SBDPTLEN                                    
         BAS   RE,GODRIVER         YES                                          
         MVI   SBLEN,FF                                                         
*                                                                               
GENDPT4  BAS   RE,GODRIVER         ALL TOTAL                                    
*                                                                               
GENDPTX  LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
DRIVINRE DS    F                                                                
         EJECT                                                                  
***********************************************************************         
* DRIVER HOOK                                                         *         
***********************************************************************         
         SPACE 1                                                                
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLINIT       INITIALIZATION                               
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLINREC      ABOUT TO PROCESS A REC FOR INPUT             
         BE    INREC                                                            
         CLI   GLHOOK,GLOUTPUT     OUTPUT STAGE                                 
         BE    OUTPUT                                                           
         CLI   GLHOOK,GLINCOMP     INTERNAL COMPUTES                            
         BE    INTCOMP                                                          
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
         CLI   GLHOOK,GLFIRST      FIRSTS                                       
         BE    FIRSTS                                                           
         CLI   GLHOOK,GLLAST       LASTS                                        
         BE    LASTS                                                            
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BNE   DRHOOKX                                                          
         GOTO1 AHEAD                                                            
*                                                                               
DRHOOKX  J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER INITIALIZATION                                               *         
***********************************************************************         
         SPACE 1                                                                
DRVINIT  DS    0H                  SET GLOPTS HERE                              
         MVC   GLOPTS+4(1),RECAP   RECAP OPTION                                 
         MVC   GLOPTS+5(1),RERATE  RERATE TYPE                                  
         MVI   GLOPTS+6,C'N'       DAYPART/LENGTH RECAP BREAKOUT                
         TM    RCPIND,RCPDPT                                                    
         BZ    *+8                                                              
         MVI   GLOPTS+6,C'Y'                                                    
         MVC   GLOPTS+7(1),RCPOPT  RECAP TOTAL REPORTING OPTION                 
         MVC   GLOPTS+8(1),PRDSUM  PRODUCT SUMMARIES OPTION                     
*                                                                               
DRVINIT2 MVI   GLOPTS+9,C'N'       DETAIL DOLLARS                               
         TM    DETIND,DETDOL                                                    
         BZ    *+8                                                              
         MVI   GLOPTS+9,C'Y'                                                    
         MVI   GLOPTS+10,C'N'      DETAIL DEMOS                                 
         TM    DETIND,DETDEM                                                    
         BZ    *+8                                                              
         MVI   GLOPTS+10,C'Y'                                                   
         MVI   GLOPTS+11,C'N'      RECAP SPOTS                                  
         TM    RCPIND,RCPSPT                                                    
         BZ    *+8                                                              
         MVI   GLOPTS+11,C'Y'                                                   
         MVI   GLOPTS+12,C'N'      RECAP DOLLARS                                
         TM    RCPIND,RCPDOL                                                    
         BZ    *+8                                                              
         MVI   GLOPTS+12,C'Y'                                                   
         MVI   GLOPTS+13,C'N'      RECAP DEMOS                                  
         TM    RCPIND,RCPDEM                                                    
         BZ    *+8                                                              
         MVI   GLOPTS+13,C'Y'                                                   
         MVC   GLOPTS+14(1),SUMMY  SUMMARY TYPE                                 
         MVC   GLOPTS+15(1),SUMDPT SUMMARY DAYPART DETAIL CONTROL               
         TM    SUMIND,SUMMTH       MKT PERFORMANCE MONTHLY BREAKOUT             
         BZ    *+8                                                              
         MVI   GLOPTS+16,C'M'                                                   
         TM    SUMIND,SUMBRD       MKT PERFORMANCE BRAND BREAKOUT               
         BZ    *+8                                                              
         MVI   GLOPTS+17,C'B'                                                   
*                                                                               
         CLI   SBQSGRD,C' '                                                     
         BNH   DRI10                                                            
         MVI   GLOPTS+18,1         NUMBER OF STATION GROUP LEVELS               
         CLC   SBSGR1LN,SBSGR2LN                                                
         BE    DRI10                                                            
         MVI   GLOPTS+18,2                                                      
*                                                                               
DRI10    OI    GLINDS,GLPALTOT     PRINT ALL TOTALS                             
         OI    GLINDS2,GLMIDHED    MIDHEAD ON CHANGE OF REPORT                  
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                            *         
***********************************************************************         
         SPACE 1                                                                
RESOLVE  L     R1,=A(RTNLIST)     SEARCH LIST FOR ROUTINE NAME                  
*                                                                               
RESOLVE2 CLI   0(R1),FF                                                         
         JE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INTERNAL COMPUTES HOOK                                              *         
***********************************************************************         
         SPACE 1                                                                
INTCOMP  CLC   GLLABEL,BLANKS                                                   
         BE    EXEC                                                             
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER HOOK TO EXECUTE ROUTINES                                     *         
***********************************************************************         
         SPACE 1                                                                
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         CLI   GLMODE,GLINPUT      TEST INPUT MODE                              
         BNE   *+8                                                              
         L     R5,SBACURCH         YES-R5=A(CURRENT CHUNK)                      
         L     RF,GLAROUT          RF=A(ROUTINE)                                
         L     RE,=A(T20416E)                                                   
         LA    RE,0(RE)                                                         
         LA    RF,0(RF)                                                         
         CR    RF,RE               IS ROUTINE IN MAIN SECTION                   
         BLR   RF                  YES-GO THERE                                 
         BR    RE                  NO-GO TO T20416E                             
         EJECT                                                                  
***********************************************************************         
* DRIVER'S ABOUT TO PROCESS A RECORD FOR INPUT                        *         
***********************************************************************         
         SPACE 1                                                                
INREC    CLI   GLARGS,6            TEST IT'S A RECAP OR SUMMARY RECORD          
         JL    XIT                                                              
         CLI   PERTYP,PERGRD       YES-REJECT IF WE'RE PROCESSING BUY           
         BE    INREC9                  DETAILS NOW                              
         CLI   RECRCPLO,0          TEST THIS IS A RECAP RECORD                  
         BE    INREC1                                                           
         CLC   GLARGS(1),RECRCPLO                                               
         BL    INREC1                                                           
         CLC   GLARGS(1),RECRCPHI                                               
         BH    INREC1                                                           
         CLI   DPLTOT,C'Y'         YES-REJECT DPT/LEN TOTALS                    
         BE    INREC9                                                           
         CLI   MPRPRD,C'B'         REJECT IF PRODUCT'S NOT POL FOR              
         BE    INREC9              POL REQUEST                                  
         CLI   RCPOPT,3            TEST WEEKLY RECAPS                           
         BL    INREC0                                                           
         CLI   PERTYP,PERWKS       YES-ONLY ACCEPT WEEKS                        
         BNE   INREC9                                                           
         CLI   SBMODE,SBPROCGL     TEST PASSING GOAL RECORD                     
         JNE   XIT                                                              
         CLI   GOALIND,GIWKS       YES-ONLY ACCEPT WEEKLY PASS                  
         BNE   INREC9                                                           
         J     XIT                                                              
*                                                                               
INREC0   CLI   PERTYP,PERTOT       BUY PERIOD OK                                
         JE    XIT                                                              
         CLI   PERTYP,PERTOTL      BUY PERIOD BY LENGTH INVALID IF NOT          
         BNE   *+16                REPORTING DPTLEN                             
         TM    RCPIND,RCPDPT                                                    
         BZ    INREC9                                                           
         J     XIT                                                              
         CLI   RCPOPT,2                                                         
         BNE   INREC9                                                           
         CLI   PERTYP,PERMTH       MONTHS OK FOR MONTHLY RECAP                  
         JE    XIT                                                              
         CLI   PERTYP,PERMTHL      MONTHS BY LENGTH INVALID IF NOT              
         BNE   INREC9              REPORTING DPTLEN                             
         TM    RCPIND,RCPDPT                                                    
         BZ    INREC9                                                           
         J     XIT                                                              
*                                                                               
INREC1   CLI   RECSUMLO,0                                                       
         BE    INREC8                                                           
         CLI   SUMMY,3                                                          
         BE    INREC4                                                           
         CLC   GLARGS(1),RECSUMLO  TEST THIS IS A NON MKT PERFORMANCE           
         BL    INREC8              SUMMARY                                      
         CLC   GLARGS(1),RECSUMHI                                               
         BH    INREC8                                                           
         CLI   PERTYP,PERTOTL      YES-BUY PERIOD BY LENGTH ONLY IF             
         BNE   *+16                    SUMMARY'S BY DPTLEN                      
         CLI   SUMDPT,1                                                         
         BNE   INREC9                                                           
         B     INREC1A                                                          
         CLI   PERTYP,PERTOT       TEST BUY PERIOD                              
         BNE   INREC2                                                           
         CLI   SBLEN,255           YES-NOT FOR 'CROSS LENGTHS' AND              
         BNE   INREC1A                 DPTLEN BREAKOUT                          
         CLI   SUMDPT,1                                                         
         BE    INREC9                                                           
*                                                                               
INREC1A  CLI   SUMMY,2             BRAND WEEKLY SUMMARY ONLY IF                 
         JNE   XIT                 ALL MONTHS                                   
         CLC   MONTH,XFF                                                        
         BNE   INREC9                                                           
         J     XIT                                                              
*                                                                               
INREC2   CLI   SUMMY,2             BRAND WEEKLY SUMMARY GETS MONTHS             
         BNE   INREC3                                                           
         CLI   PERTYP,PERMTH       TEST ACROSS LENGTHS                          
         BNE   *+16                                                             
         CLI   SUMDPT,1            AND DPTLEN BREAKOUT                          
         BE    INREC9              YES-REJECT (DPTLEN HAS OWN TOTALS)           
         J     XIT                                                              
         CLI   PERTYP,PERMTHL      MONTHS BY LENGTH ONLY IF DPTLEN              
         BNE   INREC3              BREAKOUT                                     
         CLI   SUMDPT,1                                                         
         BNE   INREC9                                                           
         J     XIT                                                              
*                                                                               
INREC3   CLI   PERTYP,PERWKS       TEST WEEKS PASSED                            
         BNE   INREC9                                                           
         CLI   SBMODE,SBPROCGL     YES-TEST PASSING GOALS NOW                   
         BNE   *+12                                                             
         CLI   GOALIND,GIWKS       YES-ANTHING BUT PASSING WEEKS IS OK          
         JNE   XIT                                                              
         TM    SUMIND,SUMWKY       ELSE WEEKLY OPTION MUST BE SET               
         BZ    INREC9                                                           
         J     XIT                                                              
*                                                                               
INREC4   CLC   GLARGS(1),RECMPRLO  TEST MARKET PERFORMANCE RECORD               
         BL    INREC8                                                           
         CLC   GLARGS(1),RECMPRHI                                               
         BH    INREC8                                                           
         CLI   MPRPRD,C'P'         YES-REJECT IF PRODUCT'S POL                  
         BE    INREC9                                                           
         CLC   SVPRD,XFF           TEST ALL PRODUCTS                            
         BNE   *+14                                                             
         CLC   GLARGS(1),RECMPRHI  AND SECONDARY DEMO SUMMARY                   
         BE    INREC9              YES-REJECT                                   
         CLI   SBMODE,SBPROCSP     IF PROCESSING BUYS NOW,                      
         BNE   INREC6                                                           
         TM    SUMIND,SUMMTH       TEST MONTHLY BREAKOUT                        
         BZ    INREC6                                                           
         ZIC   R1,RECMPRLO         YES-                                         
         LA    RE,1                                                             
         LA    RF,4                                                             
*                                                                               
INREC5   CLM   R1,1,GLARGS         MATCH RECORD TO QUARTERLY REC NUMBER         
         BNE   *+16                                                             
         CLM   RE,1,GLOPTS+3       YES-TEST QUARTER NUMBER IS CORRECT           
         BNE   INREC9                  NO-REJECT                                
         B     INREC6                  YES-OK                                   
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,INREC5                                                        
*                                                                               
INREC6   CLI   PERTYP,PERTOTL      BUY PERIOD BY LENGTH ONLY IF                 
         BNE   *+16                DPTLEN BREAKOUT                              
         CLI   SUMDPT,1                                                         
         BNE   INREC9                                                           
         B     *+12                                                             
         CLI   PERTYP,PERTOT       TEST BUY PERIOD                              
         BNE   INREC7                                                           
         TM    SUMIND,SUMMTH       YES-                                         
         JZ    XIT                                                              
         ZIC   R1,RECMPRLO                                                      
         LA    R1,4(R1)                                                         
         CLM   R1,1,GLARGS                                                      
         BH    INREC9                                                           
         J     XIT                                                              
*                                                                               
INREC7   TM    SUMIND,SUMMTH       MONTHS ONLY FOR MONTHLY BREAKOUT             
         BZ    INREC9                                                           
         CLI   PERTYP,PERSUM                                                    
         BNE   INREC9                                                           
         ZIC   R1,RECMPRLO                                                      
         LA    R1,4(R1)                                                         
         CLM   R1,1,GLARGS                                                      
         BNH   INREC9                                                           
         J     XIT                                                              
*                                                                               
INREC8   B     INREC9                                                           
*                                                                               
INREC9   MVI   GLHOOK,GLDONT                                                    
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER ABOUT TO EXECUTE OUTPUT STAGE                                *         
***********************************************************************         
         SPACE 1                                                                
OUTPUT   L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         MVI   MAXLINES,61         ALLOW DEEP PAGE                              
         DROP  R1                                                               
         MVC   MKTLEV,SVMKTLEV     RESTORE MARKET LEVEL                         
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* I/O ROUTINES FOR PRODUCT                                            *         
***********************************************************************         
         SPACE 1                                                                
IPRD     MVC   0(3,R2),SVPRD                                                    
         MVC   3(3,R2),SVPRD2                                                   
         J     XIT                                                              
*                                                                               
OPRD     CLC   0(3,R2),XFF                                                      
         BNE   *+14                                                             
         MVC   0(5,R3),=C'*ALL PRODUCTS*'                                       
         J     XIT                                                              
         MVC   0(3,R3),0(R2)                                                    
         CLC   3(3,R2),BLANKS                                                   
         BNH   *+18                                                             
         MVI   3(R3),C'-'                                                       
         MVC   4(3,R3),3(R2)                                                    
         LA    R3,4(R3)                                                         
         LA    R3,4(R3)                                                         
         L     R1,SBAPRDBF                                                      
         USING PRDBUFFD,R1                                                      
         LH    RE,=Y(PRDBUFFL)                                                  
         LA    R0,256                                                           
         CLC   PBALPH,0(R2)                                                     
         BE    *+14                                                             
         LA    R1,0(RE,R1)                                                      
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
         MVC   0(L'PBNAME,R3),PBNAME                                            
         CLC   3(3,R2),BLANKS                                                   
         JNH   XIT                                                              
         L     R1,SBAPRDBF                                                      
         USING PRDBUFFD,R1                                                      
         LH    RE,=Y(PRDBUFFL)                                                  
         LA    R0,256                                                           
         CLC   PBALPH,3(R2)                                                     
         BE    *+14                                                             
         LA    R1,0(RE,R1)                                                      
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
         LA    RF,10(R3)                                                        
         MVI   0(RF),C'-'                                                       
         MVC   1(10,RF),PBNAME                                                  
         J     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* MISC ROW INPUT ROUTINES                                             *         
***********************************************************************         
         SPACE 1                                                                
IDP      MVC   0(4,R2),SBDPTGRP    DAYPART                                      
         MVC   4(4,R2),SBDPT                                                    
         J     XIT                                                              
*                                                                               
IDPL     MVC   0(4,R2),SBDPT       DAYPART/LENGTH                               
         CLI   SBLEN,255           TEST LENGTH=255                              
         BNE   *+10                                                             
         MVC   0(4,R2),XFF         YES-THEN IT'S FOR ALL DAYPARTS               
         MVC   4(1,R2),SBLEN                                                    
         J     XIT                                                              
*                                                                               
IMON     MVC   0(4,R2),MONTH       MONTH                                        
         J     XIT                                                              
*                                                                               
         USING SCHUNKD,R5                                                       
IWEEK    LA    R1,SCDATE           WEEK                                         
         CLI   SBMODE,SBPROCSP                                                  
         BE    IWEEK2                                                           
         USING SGLCHNKD,R5                                                      
         LA    R1,SGDATE                                                        
         CLI   SBMODE,SBPROCGL                                                  
         BE    IWEEK2                                                           
         DC    H'0'                                                             
IWEEK2   MVC   0(4,R2),0(R1)                                                    
         J     XIT                                                              
*                                                                               
INBUY    GOTO1 AIBUY               BUY DESCRIPTION                              
         J     XIT                                                              
*                                                                               
ISUMSEQ  MVC   0(1,R2),SUMSEQ      BRAND WEEKLY SUMMARY SEQUENCE                
         J     XIT                                                              
*                                                                               
ISPER    CLI   SBMODE,SBPROCSP     PERIOD FOR BRAND WEEKLY SUMMARY              
         BNE   ISPER2                                                           
         CLI   SUMSEQ,1            BUY WEEK                                     
         BNE   *+14                                                             
         USING SCHUNKD,R5                                                       
         MVC   0(4,R2),SCDATE                                                   
         J     XIT                                                              
         MVC   0(4,R2),MONTH       BUY MONTH                                    
         J     XIT                                                              
ISPER2   CLI   SBMODE,SBPROCGL                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         USING SGLCHNKD,R5                                                      
         CLI   GOALIND,GIWKS       GOAL WEEK                                    
         BNE   *+14                                                             
         MVC   0(4,R2),SGDATE                                                   
         J     XIT                                                              
         CLI   GOALIND,GITOTAL     GOAL TOTAL PERIOD                            
         BNE   *+14                                                             
         MVC   0(4,R2),XFF                                                      
         J     XIT                                                              
         CLI   GOALIND,GIMNTHS     GOAL MONTH                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AMONTHS                                                       
         LA    R0,MAXMONS                                                       
ISPER4   CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SGDATE,0(R1)                                                     
         BL    *+14                                                             
         CLC   SGDATE,2(R1)                                                     
         BNH   *+14                                                             
         LA    R1,4(R1)                                                         
         BCT   R0,ISPER4                                                        
         DC    H'0'                                                             
         MVC   0(4,R2),0(R1)                                                    
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* MISC ROW OUTPUT ROUTINES                                            *         
***********************************************************************         
         SPACE 1                                                                
ODP      CLC   0(8,R2),XFF         DAYPART                                      
         BNE   *+14                                                             
         MVC   0(7,R3),=C'*TOTAL*'                                              
         J     XIT                                                              
         CLC   4(4,R2),XFF         TEST DAYPART GROUP TOTAL                     
         BNE   ODP2                                                             
         CP    DPTCNTR,=P'1'       YES-ONLY PRINT IF MORE THAN ONE              
         BNH   ODP9                    DAYPART IN THE GROUP                     
         MVC   0(3,R3),1(R2)                                                    
         MVI   3(R3),C'-'                                                       
         MVC   4(3,R3),=C'TOT'                                                  
         J     XIT                                                              
ODP2     OC    1(3,R2),1(R2)       TEST DAYPART GROUP                           
         BZ    ODP4                                                             
         CLC   DPTGRPSV,1(R2)      YES-TEST NEW GROUP                           
         BE    ODP4                                                             
         ZAP   DPTCNTR,=P'0'       YES-RESET DAYPART COUNTER                    
         MVC   DPTGRPSV,1(R2)                                                   
ODP4     MVC   0(3,R3),5(R2)                                                    
         CP    DPTCNTR,=P'2'       AUGMENT DAYPART COUNTER                      
         JNL   XIT                                                              
         AP    DPTCNTR,=P'1'                                                    
         J     XIT                                                              
ODP9     MVI   PRTSW,C'N'          DON'T PRINT                                  
         J     XIT                                                              
*                                                                               
ODPL     DS    0H                  DAYPART/LENGTH                               
         MVC   SBLEN,4(R2)                                                      
         TM    GLINDS3,GLLSTLIN    IF FORMATTING A 'LAST' LINE                  
         BZ    *+12                                                             
         CLI   FMTWKS,C'A'         AND ALL DAYPARTS                             
         BE    *+14                                                             
         CLC   0(4,R2),XFF         OR, ROW SAYS ALL DAYPARTS                    
         BNE   *+14                                                             
         MVC   0(5,R3),=C'*ALL*'   YES-PRINT ALL                                
         J     XIT                                                              
         MVC   0(3,R3),1(R2)                                                    
         MVI   3(R3),C'-'                                                       
         EDIT  (1,4(R2)),(3,4(R3)),ALIGN=LEFT                                   
         J     XIT                                                              
*                                                                               
OMON     CLC   0(4,R2),XFF         TEST ALL MONTHS                              
         BNE   *+14                                                             
         MVC   0(5,R3),=C'*ALL*'                                                
         J     XIT                                                              
         CLI   DATEFORM,4          TEST CALLENDAR OR BROADCAST MONTHS           
         BNL   OMON2                                                            
         CLI   SBSPPROF+8,2        AND WEEK STARTS ON MONDAY                    
         BNL   OMON2                                                            
         GOTO1 DATCON,DMCB,(2,2(R2)),(6,(R3))  YES-PRINT MMM/YY                 
         J     XIT                                                              
OMON2    GOTO1 DATCON,DMCB,(2,0(R2)),(4,(R3))  NO-PRINT START/END DATES         
         MVI   5(R3),C'-'                                                       
         GOTO1 (RF),(R1),(2,2(R2)),(4,6(R3))                                    
         J     XIT                                                              
*                                                                               
OWEEK    TM    GLINDS3,GLLSTLIN    TEST PRINTING 'LAST' LINE                    
         JO    XIT                                                              
         L     R1,AWEEKS           YES-LOCATE WEEK                              
         SR    RE,RE                                                            
OWEEK2   OC    0(2,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R2),0(R1)                                                    
         BE    *+16                                                             
         AH    RE,=Y(WKTOTL)                                                    
         LA    R1,4(R1)                                                         
         B     OWEEK2                                                           
         ST    RE,WKTOTDSP         SET DISPLACEMENT INTO WEEK TOTALS            
         MVI   PRTSW,C'N'          SUPPRESS PRINTING THIS LINE                  
         J     XIT                                                              
*                                                                               
OUTBUY   GOTO1 AOBUY               BUY DESCRIPTION                              
         J     XIT                                                              
*                                                                               
OSUMSEQ  MVC   SUMSEQ,0(R2)        BRAND WEEKLY SUMMARY SEQUENCE                
         J     XIT                                                              
*                                                                               
OTGT     MVC   THTGT,0(R2)         TARGET DEMO                                  
         J     XIT                                                              
*                                                                               
OSPER    MVC   0(16,R3),BLANKS     BRAND WEEKLY SUMMARY PERIOD                  
         CLI   SUMSEQ,1                                                         
         BNE   OSPER2                                                           
         MVC   0(8,R3),=C'WEEK OF '                                             
         GOTO1 DATCON,DMCB,(2,(R2)),(5,8(R3))                                   
         J     XIT                                                              
OSPER2   MVC   0(3,R3),=C'***'                                                  
         CLC   0(4,R2),XFF                                                      
         BNE   *+14                                                             
         MVC   3(8,R3),=C'TOTAL***'                                             
         J     XIT                                                              
         GOTO1 DATCON,DMCB,(2,2(R2)),(4,3(R3))                                  
         CLI   DATEFORM,4                                                       
         BNL   *+14                                                             
         MVC   6(3,R3),=C'***'                                                  
         J     XIT                                                              
         MVC   9(5,R3),3(R3)                                                    
         MVC   14(3,R3),=C'***'                                                 
         MVI   8(R3),C'-'                                                       
         GOTO1 (RF),(R1),(2,(R2)),(4,3(R3))                                     
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FIRST AND LAST ROUTINES                                             *         
***********************************************************************         
         SPACE 1                                                                
FEST     XC    NMKTS,NMKTS         ESTIMATE FIRST - CLEAR MARKET WEIGHT         
         XC    TOTMKTWT,TOTMKTWT                    TOTALS                      
         XC    SVBMKT,SVBMKT                                                    
         J     XIT                                                              
*                                                                               
FMKT     DS    0H                  MARKET AND STATION FIRST                     
FSTA     L     R1,AWKTOT           CLEAR WEEKLY TOTALS                          
         LA    R0,MAXWKS                                                        
         XC    0(WKTOTL,R1),0(R1)                                               
         LA    R1,WKTOTL(R1)                                                    
         BCT   R0,*-10                                                          
         J     XIT                                                              
*                                                                               
FDPTLEN  L     R1,AWKTOTDL         DPTLEN FIRST - CLEAR WEEKLY TOTALS           
         LA    R0,MAXWKS                          FOR DPTLEN                    
         XC    0(WKTOTL,R1),0(R1)                                               
         LA    R1,WKTOTL(R1)                                                    
         BCT   R0,*-10                                                          
         J     XIT                                                              
*                                                                               
LMKT     DS    0H                  MARKET AND STATION LAST                      
LSTA     OI    GLINDS3,GLLSTLIN    TELL DRIVER TO FORMAT A 'LAST' LINE          
         MVI   FMTWKS,C'A'                                                      
         XC    ANXTNTRY,ANXTNTRY                                                
         J     XIT                                                              
*                                                                               
LDPTLEN  OI    GLINDS3,GLLSTLIN    DPTLEN LAST - TELL DRIVER TO FORMAT          
         MVI   FMTWKS,C'D'                       A 'LAST' LINE                  
         XC    ANXTNTRY,ANXTNTRY                                                
         J     XIT                                                              
*                                                                               
LRECORD  OI    GLINDS2,GLMIDHED    NEXT REPORT PRINTS ON SAME PAGE              
         J     XIT                                                              
*                                                                               
CLRDPLEN CLI   SUMDPT,1            TEST DPTLEN BREAKOUT IN SUMMARIES            
         JNE   XIT                                                              
         XC    DPTGRPSV,DPTGRPSV   YES-INITIALIZE DPTLEN RELATED FIELDS         
         MVI   LENSV,0                                                          
         XC    SLCNTRS,SLCNTRS                                                  
         ZAP   DLCNTR,=P'0'                                                     
         ZAP   DPTCNTR,=P'0'                                                    
         ZAP   LENCNTR,=P'0'                                                    
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* HEADING ROUTINES                                                    *         
***********************************************************************         
         SPACE 1                                                                
HMON     ZIC   R1,GLARGS           MONTH DATES HEADING                          
         BCTR  R1,0                                                             
         MH    R1,=H'30'                                                        
         LA    R1,MONDATES(R1)                                                  
         ZIC   RE,GLARGS+1                                                      
         BCTR  RE,0                                                             
         MH    RE,=H'10'                                                        
         LR    R1,RE                                                            
         MVC   0(39,R3),BLANKS                                                  
         MVC   14(5,R3),0(R1)                                                   
         MVI   19(R3),C'-'                                                      
         MVC   20(5,R3),5(R1)                                                   
         J     XIT                                                              
*                                                                               
HBUY     MVC   0(L'BUYHED1,R3),BUYHED1      BUY DETAIL HEADING                  
         MVC   198(BUYHEDL,R3),DASHES                                           
         MVC   198+198(L'BUYHED2,R3),BUYHED2                                    
         TM    DETIND,DETDOL                                                    
         JO    XIT                                                              
         MVC   198+198+BH2COST-BUYHED2(L'BH2COST,R3),BLANKS                     
         J     XIT                                                              
*                                                                               
HSPW     ZIC   R2,GLARGS           SPOTS PER WEEK                               
         BCTR  R2,0                                                             
         MH    R2,=Y(L'WKSQ1)                                                   
         LA    R2,WKSQ1(R2)                                                     
         MVC   0(17,R3),DASHES                                                  
         MVC   18(19,R3),=C'NUMBER OF TELECASTS'                                
         MVC   38(17,R3),DASHES                                                 
         LA    R3,198(R3)                                                       
         LA    R0,14                                                            
*                                                                               
HSPW2    OC    0(5,R2),0(R2)                                                    
         BZ    HSPWX                                                            
         MVC   0(3,R3),0(R2)                                                    
         MVC   198+1(2,R3),3(R2)                                                
         LA    R2,5(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,HSPW2                                                         
*                                                                               
HSPWX    J     XIT                                                              
         SPACE 2                                                                
HDEMTY   MVC   0(5,R3),=C'PURCH'   DEMO TYPE                                    
         CLI   RERATE,C'P'                                                      
         JE    XIT                                                              
         MVC   0(5,R3),=C'ACHVD'                                                
         CLI   RERATE,C'R'                                                      
         JE    XIT                                                              
         MVC   0(5,R3),=C'AFDVT'                                                
         CLI   RERATE,C'A'                                                      
         JE    XIT                                                              
         DC    H'0'                                                             
*                                                                               
HDEM     BAS   RE,GETDEMNM         DEMOS                                        
         JZ    XIT                                                              
         LR    RE,R3                                                            
         CLI   GLARGS,1                                                         
         BE    HDEM2                                                            
         BCTR  RE,0                                                             
         CLI   GLARGS,3                                                         
         BE    HDEM2                                                            
         LA    RE,198(RE)                                                       
HDEM2    MVC   0(7,RE),0(R1)                                                    
         J     XIT                                                              
*                                                                               
HTDEM    BAS   RE,GETDEMNM         TOTAL DEMOS                                  
         JZ    XIT                                                              
         MVC   0(7,R3),0(R1)                                                    
         J     XIT                                                              
*                                                                               
HCPP     MVC   0(3,R3),=C'CPP'     CPP/M HEADING                                
         CLI   DEMONAME,C'R'                                                    
         JE    XIT                                                              
         CLI   DEMONAME,C'E'                                                    
         JE    XIT                                                              
         MVI   2(R3),C'M'                                                       
         J     XIT                                                              
*                                                                               
GETDEMNM DS    0H                                                               
         ZIC   R1,GLARGS                                                        
         BCTR  R1,0                                                             
         MH    R1,=H'7'                                                         
         LA    R1,DEMNAMES(R1)                                                  
         MVC   DEMONAME,0(R1)                                                   
         OC    0(7,R1),0(R1)                                                    
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT A LINE                                                        *         
***********************************************************************         
         SPACE 1                                                                
PRINT    CLI   GLHOOK,GLDONT       TEST LINE ALREADY SUPPRESSED                 
         BE    PRINTX              YES                                          
         B     PRINTX                                                           
*                                                                               
PRINT9   MVI   GLHOOK,GLDONT                                                    
*                                                                               
PRINTX   J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER FIRSTS                                                       *         
***********************************************************************         
         SPACE 1                                                                
FIRSTS   B     FIRX                                                             
*                                                                               
FIRX     J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER LASTS                                                        *         
***********************************************************************         
         SPACE 1                                                                
LASTS    CLI   GLARGS,1            TEST LEVEL 1 LAST (LAST FOR REPORT)          
         BNE   LAS4                                                             
         OI    GLINDS2,GLMIDHED    YES-NEXT REPORT PRINTS ON SAME PAGE          
         CLI   GLRECNO,5           EXCEPT WHERE PAGE BRKS ARE REQUIRED          
         BH    LAS2                                                             
         CLI   RECAP,C'S'                                                       
         BE    LAS4                                                             
         CLI   GLRECNO,5                                                        
         BE    LAS2                                                             
         CLI   GLRECNO,1                                                        
         BNE   LAS4                                                             
         CLI   NUMWKS,14                                                        
         BH    LAS4                                                             
*                                                                               
LAS2     NI    GLINDS2,255-GLMIDHED                                             
*                                                                               
LAS4     DS    0H                                                               
*                                                                               
LASX     J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HOOK TO CALLING PROGRAM TO CALL DRIVER                   *         
***********************************************************************         
         SPACE 1                                                                
GODRIVER NTR1                                                                   
         L     RF,ADRIVER                                                       
         L     RE,SAVERD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         J     XIT                                                              
         EJECT                                                                  
RPTOPT   DC    AL1(DINAMES),AL1(1,5),CL5'BUYER'                                 
         DC    AL1(0)                                                           
         SPACE 2                                                                
SUMCNTL  DC    AL1(SUMDEM),AL1(1,5),CL5'DEMOS'                                  
         DC    AL1(SUMWKY),AL1(1,4),CL4'WKLY'                                   
         DC    AL1(SUMWKY),AL1(1,6),CL6'WEEKLY'                                 
         DC    AL1(SUMMTH),AL1(1,6),CL6'MNTHLY'                                 
         DC    AL1(SUMMTH),AL1(1,7),CL7'MONTHLY'                                
         DC    AL1(SUMBRD),AL1(1,5),CL5'BRAND'                                  
         DC    AL1(SUMBRD),AL1(1,4),CL4'BRND'                                   
         DC    AL1(SUMCPP),AL1(1,3),CL3'CPP'                                    
         DC    AL1(SUMLEN),AL1(1,3),CL3'LEN'                                    
         DC    AL1(0)                                                           
         SPACE 2                                                                
RCPCNTL  DC    AL1(RCPSPT),AL1(1,5),CL5'SPOTS'                                  
         DC    AL1(RCPSPT),AL1(1,4),CL4'SPTS'                                   
         DC    AL1(RCPDOL),AL1(2,4),CL4'COST'                                   
         DC    AL1(RCPDOL),AL1(2,7),CL7'DOLLARS'                                
         DC    AL1(RCPDEM),AL1(2,5),CL5'DEMOS'                                  
         DC    AL1(RCPCPP),AL1(2,3),CL3'CPP'                                    
         DC    AL1(RCPCPP),AL1(2,3),CL3'CPM'                                    
         DC    AL1(RCPDPT),AL1(2,7),CL7'DPTLENS'                                
         DC    AL1(RCPDPT),AL1(2,4),CL4'DPTS'                                   
         DC    AL1(0)                                                           
         SPACE 2                                                                
DETCNTL  DC    AL1(DETDOL),AL1(2,4),CL4'COST'                                   
         DC    AL1(DETDOL),AL1(2,7),CL7'DOLLARS'                                
         DC    AL1(DETDEM),AL1(2,5),CL5'DEMOS'                                  
         DC    AL1(DETCPP),AL1(2,3),CL3'CPP'                                    
         DC    AL1(DETCPP),AL1(2,3),CL3'CPM'                                    
         DC    AL1(DETOVR),AL1(1,4),CL4'OVRD'                                   
         DC    AL1(DETOVR),AL1(1,9),CL9'OVERRIDES'                              
         DC    AL1(DETREP),AL1(1,3),CL3'REP'                                    
         DC    AL1(DETSRP),AL1(1,4),CL4'SREP'                                   
         DC    AL1(DETMKG),AL1(1,9),CL9'MAKEGOODS'                              
         DC    AL1(DETAFF),AL1(1,10),CL10'AFFILIATES'                           
         DC    AL1(0)                                                           
         EJECT                                                                  
RTNLIST  DS    0F                                                               
         DC    CL8'IPRDUSR ',A(IPRD)                                            
         DC    CL8'OPRDUSR ',A(OPRD)                                            
         DC    CL8'OMGR1   ',A(OMGR1)                                           
         DC    CL8'OMGR2   ',A(OMGR2)                                           
         DC    CL8'OMGR3   ',A(OMGR3)                                           
         DC    CL8'OSGR1   ',A(OSGR1)                                           
         DC    CL8'OSGR2   ',A(OSGR2)                                           
         DC    CL8'OMKT    ',A(OMKT)                                            
         DC    CL8'ISTA    ',A(ISTA)                                            
         DC    CL8'OSTA    ',A(OUTSTA)                                          
         DC    CL8'OSTATOT ',A(OSTATOT)                                         
         DC    CL8'IDP     ',A(IDP)                                             
         DC    CL8'ODP     ',A(ODP)                                             
         DC    CL8'IDPL    ',A(IDPL)                                            
         DC    CL8'ODPL    ',A(ODPL)                                            
         DC    CL8'IMON    ',A(IMON)                                            
         DC    CL8'OMON    ',A(OMON)                                            
         DC    CL8'IWEEK   ',A(IWEEK)                                           
         DC    CL8'OWEEK   ',A(OWEEK)                                           
         DC    CL8'IBUY    ',A(INBUY)                                           
         DC    CL8'OBUY    ',A(OUTBUY)                                          
         DC    CL8'ISUMSEQ ',A(ISUMSEQ)                                         
         DC    CL8'OSUMSEQ ',A(OSUMSEQ)                                         
         DC    CL8'ISPER   ',A(ISPER)                                           
         DC    CL8'OSPER   ',A(OSPER)                                           
         DC    CL8'OTGT    ',A(OTGT)                                            
         DC    CL8'ISPW    ',A(ISPW)                                            
         DC    CL8'OSPW    ',A(OSPW)                                            
         DC    CL8'IDEM    ',A(IDEM)                                            
         DC    CL8'ODEM    ',A(ODEM)                                            
         DC    CL8'ITDEM   ',A(ITDEM)                                           
         DC    CL8'OTDEM   ',A(OTDEM)                                           
         DC    CL8'IRDEM   ',A(IRDEM)                                           
         DC    CL8'ORDEM   ',A(ORDEM)                                           
         DC    CL8'IWDEM   ',A(IWDEM)                                           
         DC    CL8'OWDEM   ',A(OWDEM)                                           
         DC    CL8'IGDEM   ',A(IGDEM)                                           
         DC    CL8'OWGDEM  ',A(OWGDEM)                                          
         DC    CL8'IGMDEM  ',A(IGMDEM)                                          
         DC    CL8'IBMDEM  ',A(IBMDEM)                                          
         DC    CL8'IBSDOL  ',A(IBSDOL)                                          
         DC    CL8'IBSDEM  ',A(IBSDEM)                                          
         DC    CL8'IBSCPP  ',A(IBSCPP)                                          
         DC    CL8'IBSSPT  ',A(IBSSPT)                                          
         DC    CL8'OSDEM   ',A(OSDEM)                                           
         DC    CL8'OAVEDEM ',A(OAVEDEM)                                         
         DC    CL8'ONDXDEM ',A(ONDXDEM)                                         
         DC    CL8'ICOST   ',A(ICOST)                                           
         DC    CL8'OCOST   ',A(OCOST)                                           
         DC    CL8'ITCOST  ',A(ITCOST)                                          
         DC    CL8'IRCOST  ',A(IRCOST)                                          
         DC    CL8'IWCOST  ',A(IWCOST)                                          
         DC    CL8'OWCOST  ',A(OWCOST)                                          
         DC    CL8'IGDOL   ',A(IGDOL)                                           
         DC    CL8'OWGDOL  ',A(OWGDOL)                                          
         DC    CL8'IGMDOL  ',A(IGMDOL)                                          
         DC    CL8'IBMDOL  ',A(IBMDOL)                                          
         DC    CL8'ODOL    ',A(ODOL)                                            
         DC    CL8'ONDXDOL ',A(ONDXDOL)                                         
         DC    CL8'IGCPP   ',A(IGCPP)                                           
         DC    CL8'OCPP    ',A(OCPP)                                            
         DC    CL8'ISPTS   ',A(ISPTS)                                           
         DC    CL8'OSPTS   ',A(OSPTS)                                           
         DC    CL8'ITSPTS  ',A(ITSPTS)                                          
         DC    CL8'IRSPTS  ',A(IRSPTS)                                          
         DC    CL8'OBSSPT  ',A(OBSSPT)                                          
         DC    CL8'OWSTACK ',A(OUTWSTK)                                         
         DC    CL8'FEST    ',A(FEST)                                            
         DC    CL8'FMKT    ',A(FMKT)                                            
         DC    CL8'FSTA    ',A(FSTA)                                            
         DC    CL8'LMKT    ',A(LMKT)                                            
         DC    CL8'LSTA    ',A(LSTA)                                            
         DC    CL8'TSTA    ',A(TSTA)                                            
         DC    CL8'FDPTLEN ',A(FDPTLEN)                                         
         DC    CL8'LDPTLEN ',A(LDPTLEN)                                         
         DC    CL8'LRECORD ',A(LRECORD)                                         
         DC    CL8'CLRDPLEN',A(CLRDPLEN)                                        
         DC    CL8'HBUY    ',A(HBUY)                                            
         DC    CL8'HSPW    ',A(HSPW)                                            
         DC    CL8'HDEM    ',A(HDEM)                                            
         DC    CL8'HDEMTY  ',A(HDEMTY)                                          
         DC    CL8'HTDEM   ',A(HTDEM)                                           
         DC    CL8'HCPP    ',A(HCPP)                                            
         DC    CL8'HMON    ',A(HMON)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* FURTHER I/O ROUTINES FOR DRIVER                                     *         
* INPUT  : R2=GLAIFLD=A(SORT RECORD FIELD)                            *         
*          R3=GLAOFLD=A(PRINT LINE POSITION FOR O/P ROUTINES)         *         
*          R5=SBACURCH                                                *         
***********************************************************************         
         SPACE 1                                                                
         ENTRY T20416E                                                          
         DS    0D                                                               
T20416E  LR    RB,RE                                                            
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING T20416E,RB,RA                                                    
*                                                                               
         BR    RF                  BRANCH TO DRIVER ROUTINE                     
*                                                                               
         EJECT                                                                  
***********************************************************************         
* MARKET GROUP OUTPUT ROUTINES                                        *         
***********************************************************************         
         SPACE 1                                                                
OMGR1    CLC   SVMGR1,0(R2)        FORMAT MGR1 HEAD TO TEMP SAVE AREA           
         JE    XIT                                                              
         MVC   SVMGR1,0(R2)                                                     
         LA    R4,MGR1HEAD                                                      
         MVC   0(12,R4),SBMGR1BK                                                
**NOP         MVC   13(1,R4),SBQMGRD                                            
         BAS   RE,MGRCODE                                                       
         LA    R1,SBMGR1LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLC   19(L'UNKNOWN,R4),UNKNOWN                                         
         JE    XIT                                                              
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM,SBMGR1NM                                                
         MVC   19(24,R4),SBMGR1NM                                               
         J     XIT                                                              
*                                                                               
OMGR2    CLC   SVMGR2,0(R2)        FORMAT MGR2 HEAD TO TEMP SAVE AREA           
         JE    XIT                                                              
         MVC   SVMGR2,0(R2)                                                     
         LA    R4,MGR2HEAD                                                      
         MVC   0(12,R4),SBMGR2BK                                                
**NOP         MVC   13(1,R4),SBQMGRD                                            
         BAS   RE,MGRCODE                                                       
         LA    R1,SBMGR2LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLC   19(L'UNKNOWN,R4),UNKNOWN                                         
         JE    XIT                                                              
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM,SBMGR2NM                                                
         MVC   19(24,R4),SBMGR2NM                                               
         J     XIT                                                              
*                                                                               
OMGR3    CLC   SVMGR3,0(R2)        FORMAT MGR3 HEAD TO TEMP SAVE AREA           
         JE    XIT                                                              
         MVC   SVMGR3,0(R2)                                                     
         LA    R4,MGR3HEAD                                                      
         MVC   0(12,R4),SBMGR3BK                                                
**NOP         MVC   13(1,R4),SBQMGRD                                            
         BAS   RE,MGRCODE                                                       
         LA    R1,SBMGR3LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLC   19(L'UNKNOWN,R4),UNKNOWN                                         
         JE    XIT                                                              
         MVC   SBBMGR,0(R2)                                                     
         GOTO1 GETMGRNM,SBMGR3NM                                                
         MVC   19(24,R4),SBMGR3NM                                               
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* STATION GROUP OUTPUT ROUTINES                                       *         
***********************************************************************         
         SPACE 1                                                                
OSGR1    CLC   SVSGR1,0(R2)        FORMAT SGR1 HEAD TO TEMP SAVE AREA           
         JE    XIT                                                              
         MVC   SVSGR1,0(R2)                                                     
         LA    R4,SGR1HEAD                                                      
         MVC   0(12,R4),SBSGR1BK                                                
         MVC   13(1,R4),SBQSGRD                                                 
         LA    R1,SBSGR1LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLC   19(L'UNKNOWN,R4),UNKNOWN                                         
         JE    XIT                                                              
         MVC   SBBSGR,0(R2)                                                     
         GOTO1 GETSGRNM,SBSGR1NM                                                
         MVC   19(24,R4),SBSGR1NM                                               
         J     XIT                                                              
*                                                                               
OSGR2    CLC   SVSGR2,0(R2)        FORMAT SGR2 HEAD TO TEMP SAVE AREA           
         JE    XIT                                                              
         MVC   SVSGR2,0(R2)                                                     
         LA    R4,SGR2HEAD                                                      
         MVC   0(12,R4),SBSGR2BK                                                
         MVC   13(1,R4),SBQSGRD                                                 
         LA    R1,SBSGR2LN                                                      
         BAS   RE,OGRPCODE                                                      
         CLC   19(L'UNKNOWN,R4),UNKNOWN                                         
         JE    XIT                                                              
         MVC   SBBSGR,0(R2)                                                     
         GOTO1 GETSGRNM,SBSGR2NM                                                
         MVC   19(24,R4),SBSGR2NM                                               
         J     XIT                                                              
*                                                                               
OGRPCODE UNPK  DUB(5),0(3,R2)                                                   
         ZIC   RF,0(R1)                                                         
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
         EJECT                                                                  
***********************************************************************         
* MARKET OUTPUT ROUTINE                                               *         
***********************************************************************         
         SPACE 1                                                                
OMKT     CLC   SVBMKT,0(R2)        FORMAT MKT HEAD TO TEMP SAVE AREA            
         BE    OMKT10                                                           
         LA    R3,MKTHEAD                                                       
         MVC   MKTHEAD,BLANKS                                                   
         MVC   0(6,R3),=C'MARKET'                                               
         MVC   SBBMKT,0(R2)                                                     
         EDIT  SBBMKT,(4,SBMKT),FILL=0                                          
         MVC   7(4,R3),SBMKT                                                    
         GOTO1 GETMKTNM                                                         
         MVC   12(L'SBMKTNM,R3),SBMKTNM                                         
         CLI   2(R2),C'O'                                                       
         BNE   *+10                                                             
         MVC   12+1+L'SBMKTNM(6,R3),=C'*ORIG*'                                  
         CLI   2(R2),C'S'                                                       
         BNE   *+10                                                             
         MVC   12+1+L'SBMKTNM(7,R3),=C'*SPILL*'                                 
         LA    R1,L'MKTHEAD                                                     
         ST    R1,DMCB+4                                                        
         GOTO1 SQUASHER,DMCB,(R3)                                               
*                                                                               
         CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    OMKT2                                                            
         MVC   CVGHEAD,BLANKS      YES-FORMAT COVERAGE HEADLINE                 
         MVC   CVGHEAD(9),=C'COVERAGE='                                         
         LA    R3,CVGHEAD+9                                                     
         ICM   R5,15,SBMKTWGT                                                   
         EDIT  (R5),(6,(R3)),2,ALIGN=LEFT                                       
*                                                                               
OMKT2    CLI   PRDSUM,C'Y'         TEST PRODUCT SUMMARIES WILL PRINT            
         BNE   OMKT4                                                            
         CLI   RECAP,0                                                          
         BE    OMKT4                                                            
         CLC   SVBMKT(2),0(R2)     AND THIS IS REALLY A NEW MARKET              
         BE    OMKT4                                                            
         L     R1,NMKTS            YES-AUGMENT MARKET COUNT                     
         LA    R1,1(R1)                                                         
         ST    R1,NMKTS                                                         
         CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    OMKT4                                                            
         L     R1,TOTMKTWT         YES-ADD MARKET WEIGHT TO TOTAL               
         ICM   RE,15,SBMKTWGT                                                   
         AR    R1,RE                                                            
         ST    R1,TOTMKTWT                                                      
*                                                                               
OMKT4    MVC   SVBMKT,0(R2)        SAVE THE MARKET                              
*                                                                               
OMKT10   DS    0H                                                               
         CLI   SBQGETNM,C'Y'       TEST GET BUYER/BILLER NAMES                  
         BNE   OMKTX               NO                                           
         MVC   DUB(2),SBBCLT       YES-                                         
         MVC   DUB+2(1),SBBPRD                                                  
         MVC   DUB+3(1),SBBEST                                                  
         MVC   DUB+4(2),SBBMKT                                                  
         CLC   SVNAME,DUB          TEST CLT/PRD/EST/MKT CHANGE                  
         BE    OMKTX               NO                                           
         MVC   SVNAME,DUB          YES-GET BUYER/BILLER NAMES                   
         GOTO1 AGETNAME                                                         
*                                                                               
OMKTX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* STATION INPUT AND OUTPUT ROUTINES                                   *         
***********************************************************************         
         SPACE 1                                                                
ISTA     MVC   0(5,R2),SBSTA                                                    
         CLI   SBMED,C'N'                                                       
         BNE   *+8                                                              
         MVI   4(R2),C'N'                                                       
         MVC   5(4,R2),SBCHAN                                                   
         MVC   9(3,R2),SBAFFIL                                                  
         MVC   12(3,R2),SBTREP                                                  
         J     XIT                                                              
*                                                                               
OUTSTA   CLC   SVSTA,0(R2)         FORMAT STATION HEADLINE TO TEMP              
         JE    XIT                 SAVE AREA                                    
         GOTO1 AOSTA                                                            
         J     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* STATION TOTAL ROUTINES                                              *         
***********************************************************************         
         SPACE 1                                                                
TSTA     MVC   0(7,R3),SVSTABIG                                                 
         MVC   8(3,R3),=C'TOT'                                                  
         LA    R2,18(R3)                                                        
         EDIT  SPTS,(4,(R2)),ALIGN=LEFT                                         
         AR    R2,R0                                                            
         MVC   1(6,R2),=C'TLCSTS'                                               
         ZIC   R1,GLARGS                                                        
         BCTR  R1,0                                                             
         MH    R1,=Y(L'QTRDATES)                                                
         LA    R1,QTRDATES(R1)                                                  
         MVC   198(17,R3),0(R1)                                                 
         TM    DETIND,DETDOL                                                    
         JZ    XIT                                                              
         LA    R2,198+18(R3)                                                    
         MVI   CURTAB+3,2                                                       
         CURED COST,(9,(R2)),CURTAB,ALIGN=LEFT,CURSYMB=YES,FLOAT=-              
         J     XIT                                                              
*                                                                               
OSTATOT  MVC   0(7,R3),SVSTABIG                                                 
         MVC   8(3,R3),=C'TOT'                                                  
         MVC   198(17,R3),PERDATES                                              
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* COLUMN INPUT ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
ICOST    BAS   RE,FINDQTR          COST                                         
         USING GRIDD,R3                                                         
         MVC   0(8,R2),GRCOST                                                   
         B     INX                                                              
*                                                                               
ISPTS    BAS   RE,FINDQTR          SPOTS                                        
         MVC   0(4,R2),GRSPTS                                                   
         B     INX                                                              
*                                                                               
ISPW     BAS   RE,FINDQTR          SPOTS PER WEEK GRID                          
         MVC   0(56,R2),GRSPW      14 WEEKS                                     
         B     INX                                                              
*                                                                               
IDEM     BAS   RE,FINDQTR          DEMOS                                        
         ZIC   RE,GLARGS+1         GLARGS+1 HAS THE DEMO NUMBER                 
         BCTR  RE,0                                                             
         LR    RF,RE                                                            
         SLL   RE,3                                                             
         LA    RE,GRDEMS(RE)                                                    
         MVC   0(4,R2),0(RE)       ACCUMULATED DEMO                             
         MVC   4(4,R2),4(RE)       EQUIVALENCED                                 
         SLL   RF,2                                                             
         LA    RF,DEMVALS(RF)                                                   
         MVC   8(4,R2),0(RF)       SINGLE SPOT DEMO VALUE                       
         B     INX                                                              
*                                                                               
ITCOST   L     R3,ATOTS            TOTAL COST                                   
         USING RECTOTD,R3                                                       
         MVC   0(4,R2),RECCOST                                                  
         B     INX                                                              
*                                                                               
IRCOST   L     R3,ATOTS            RECAP COST                                   
         MVC   0(4,R2),RECCOST                                                  
         CLI   RCPOPT,2            TEST MONTH RECAP                             
         BNE   INX                                                              
         LR    R5,R3               YES-EXTRACT FROM MONTH TABLE                 
         USING MONTOTD,R5                                                       
         MVC   0(4,R2),MTCOST                                                   
         B     INX                                                              
*                                                                               
ITSPTS   L     R3,ATOTS            TOTAL SPOTS                                  
         MVC   0(4,R2),RECSPTS                                                  
         B     INX                                                              
*                                                                               
IRSPTS   L     R3,ATOTS            RECAP SPOTS                                  
         MVC   0(4,R2),RECSPTS                                                  
         CLI   RCPOPT,1                                                         
         BE    INX                                                              
         CLI   RCPOPT,2            TEST MONTH RECAP                             
         BNE   INX                                                              
         LR    R5,R3               YES-EXTRACT FROM MONTH TABLE                 
         MVC   0(4,R2),MTSPTS                                                   
         B     INX                                                              
*                                                                               
ITDEM    L     R3,ATOTS            TOTAL DEMO                                   
         ZIC   RE,GLARGS                                                        
         BCTR  RE,0                                                             
         SLL   RE,3                                                             
         LA    RE,RECDEMS(RE)                                                   
         MVC   0(8,R2),0(RE)       POINTS / EQUIV POINTS                        
         MVC   8(4,R2),RECCOST                                                  
         CLI   SBSPPROF,C'D'       TEST EQUIV DOLLARS                           
         BNE   INX                                                              
         MVC   8(4,R2),RECCOST+4   YES-MOVE EQUIV COST                          
         B     INX                                                              
*                                                                               
IRDEM    L     R3,ATOTS            RECAP DEMO                                   
         ZIC   RE,GLARGS                                                        
         BCTR  RE,0                                                             
         SLL   RE,3                                                             
         CLI   RCPOPT,1            TEST PERIOD RECAP                            
         BNE   IRDEM2                                                           
         LA    RE,RECDEMS(RE)      YES-                                         
         MVC   0(8,R2),0(RE)       POINTS / EQUIV POINTS                        
         MVC   8(8,R2),RECCOST     COST / EQUIV COST                            
         B     IRDEM4                                                           
IRDEM2   CLI   RCPOPT,2            TEST MONTH RECAP                             
         BNE   IRDEM4                                                           
         LR    R5,R3               YES-                                         
         LA    RE,MTDEMS(RE)                                                    
         MVC   0(8,R2),0(RE)       POINTS / EQUIV POINTS                        
         MVC   8(8,R2),MTCOST      COST / EQUIV COST                            
IRDEM4   CLI   GLARGS+1,C'S'       TEST PRODUCT SUMMARY                         
         BNE   INX                                                              
         L     R1,0(R2)            YES-MAY NEED TO WEIGHT DEMOS                 
         BAS   RE,WGT                                                           
         ST    R1,0(R2)                                                         
         L     R1,4(R2)                                                         
         BAS   RE,WGT                                                           
         ST    R1,4(R2)                                                         
         B     INX                                                              
*                                                                               
IBMDEM   CLI   SBMODE,SBPROCSP     MONTHLY MKT PERFORMANCE BUY DEMO             
         BNE   INX                                                              
         BAS   RE,FINDMNTH                                                      
         MVC   0(4,R2),MTDEMS      TARGET DEMO VALUE                            
         L     RE,ADEMLST                                                       
         CLI   1(RE),C'R'          CHECK DEMO'S A RATING                        
         BE    *+12                                                             
         CLI   1(RE),C'E'                                                       
         BNE   INX                                                              
         L     R1,0(R2)            YES-MAY NEED TO WEIGHT                       
         BAS   RE,WGT                                                           
         ST    R1,4(R2)                                                         
         B     INX                                                              
*                                                                               
IBMDOL   CLI   SBMODE,SBPROCSP     MONTHLY MKT PERFORMANCE BUY DOLLARS          
         BNE   INX                                                              
         BAS   RE,FINDMNTH                                                      
         MVC   0(4,R2),MTCOST      DOLLARS                                      
         B     INX                                                              
*                                                                               
IBSDOL   CLI   SBMODE,SBPROCSP     BUY DOLLARS FOR SUMMARIES                    
         BNE   INX                                                              
         BAS   RE,FINDPER                                                       
         BNE   INX                                                              
         MVC   0(4,R2),0(R1)                                                    
         B     INX                                                              
*                                                                               
IBSDEM   CLI   SBMODE,SBPROCSP     BUY DEMOS FOR SUMMARIES                      
         BNE   INX                                                              
         BAS   RE,FINDPER                                                       
         BNE   INX                                                              
         ZIC   RE,GLARGS                                                        
         BCTR  RE,0                                                             
         SLL   RE,3                                                             
         LA    RE,0(RE,RF)                                                      
         L     R1,0(RE)                                                         
         CLI   GLARGS+1,C'S'       TEST PRODUCT SUMMARY                         
         BNE   *+8                                                              
         BAS   RE,WGT              YES-MAY NEED TO WEIGHT                       
         ST    R1,0(R2)                                                         
         B     INX                                                              
*                                                                               
IBSCPP   CLI   SBMODE,SBPROCSP     BUY CPP FOR SUMMARIES                        
         BNE   INX                                                              
         BAS   RE,FINDPER                                                       
         BNE   INX                                                              
         ZIC   RE,GLARGS                                                        
         BCTR  RE,0                                                             
         SLL   RE,3                                                             
         LA    RE,0(RE,RF)                                                      
         TM    ININD,INIEQUIV      TEST EQUIVALENCE                             
         BZ    IBSCPP2                                                          
         CLI   SBSPPROF,C'D'       YES-                                         
         BNE   *+12                                                             
         LA    R1,4(R1)                                                         
         B     IBSCPP2                                                          
         LA    RE,4(RE)                                                         
IBSCPP2  MVC   0(4,R2),0(R1)                                                    
         L     R1,0(RE)                                                         
         CLI   GLARGS+1,C'S'       TEST PRODUCT SUMMARY                         
         BNE   *+8                                                              
         BAS   RE,WGT              YES-MAY NEED TO WEIGHT                       
         ST    R1,4(R2)                                                         
         B     INX                                                              
*                                                                               
IBSSPT   CLI   SBMODE,SBPROCSP     BUY SPOTS FOR SUMMARIES                      
         BNE   INX                                                              
         BAS   RE,FINDPER                                                       
         BNE   INX                                                              
         ST    R0,0(R2)                                                         
         B     INX                                                              
*                                                                               
         USING SCHUNKD,R5                                                       
IWCOST   CLI   SBMODE,SBPROCSP     WEEKLY RECAP COST                            
         BNE   INX                                                              
         MVC   0(4,R2),SCGROSS                                                  
         B     INX                                                              
*                                                                               
IWDEM    CLI   SBMODE,SBPROCSP     WEEKLY RECAP DEMOS                           
         BNE   INX                                                              
         ZIC   R1,GLARGS                                                        
         BCTR  R1,0                                                             
         SLL   R1,3                                                             
         LA    R1,SCDEMOS(R1)                                                   
         MVC   0(4,R2),0(R1)                                                    
         CLI   GLARGS+1,C'S'       TEST PRODUCT SUMMARY                         
         BNE   INX                                                              
         L     R1,0(R2)            YES-MAY NEED TO WEIGHT                       
         BAS   RE,WGT                                                           
         ST    R1,0(R2)                                                         
         B     INX                                                              
*                                                                               
         USING SGLCHNKD,R5                                                      
IGDOL    CLI   SBMODE,SBPROCGL     GOAL DOLLARS                                 
         BNE   INX                                                              
         MVC   0(4,R2),SGDOL                                                    
         B     INX                                                              
*                                                                               
IGDEM    CLI   SBMODE,SBPROCGL     GOAL DEMO                                    
         BNE   INX                                                              
         OC    SBPDEMOS,SBPDEMOS   TEST DEMO MENU SET                           
         BZ    *+14                                                             
         CLC   SBPDEMOS(3),SBESTDEM   YES-CHECK DEMO CATEGORY                   
         BNE   INX                                                              
         MVC   0(4,R2),SGDEM                                                    
         CLI   GLARGS,C'S'         TEST PRODUCT SUMMARY                         
         BNE   INX                                                              
         L     R1,0(R2)            YES-MAY NEED TO WEIGHT                       
         BAS   RE,WGT                                                           
         ST    R1,0(R2)                                                         
         B     INX                                                              
*                                                                               
IGCPP    CLI   SBMODE,SBPROCGL     GOAL CPP                                     
         BNE   INX                                                              
         OC    SBPDEMOS,SBPDEMOS   TEST DEMO MENU SET                           
         BZ    *+14                                                             
         CLC   SBPDEMOS(3),SBESTDEM   YES-CHECK DEMO CATEGORY                   
         BNE   INX                                                              
         MVC   0(4,R2),SGDOL                                                    
         MVC   4(4,R2),SGDEM                                                    
         TM    ININD,INIEQUIV      TEST EQUIVALENCE                             
         BZ    IGCPP2                                                           
         CLI   SBSPPROF,C'D'       YES-                                         
         BNE   *+14                                                             
         MVC   0(4,R2),SGEDOL                                                   
         B     IGCPP2                                                           
         MVC   4(4,R2),SGEDEM                                                   
IGCPP2   CLI   GLARGS,C'S'         TEST PRODUCT SUMMARY                         
         BNE   INX                                                              
         L     R1,4(R2)            YES-MAY NEED TO WEIGHT                       
         BAS   RE,WGT                                                           
         ST    R1,4(R2)                                                         
         B     INX                                                              
*                                                                               
IGMDOL   CLI   SBMODE,SBPROCGL                                                  
         BNE   INX                                                              
         BAS   RE,TESTWK                                                        
         BNE   INX                                                              
         MVC   0(4,R2),SGDOL                                                    
         B     INX                                                              
*                                                                               
IGMDEM   CLI   SBMODE,SBPROCGL     MONTHLY MKT PERFORMANCE GOAL DEMO            
         BNE   INX                                                              
         BAS   RE,TESTWK                                                        
         BNE   INX                                                              
         OC    SBPDEMOS,SBPDEMOS   YES-TEST DEMO MENU SET                       
         BZ    *+14                                                             
         CLC   SBESTDEM(3),SBPDEMOS    YES-CHECK DEMO CATEGORY                  
         BNE   INX                                                              
         MVC   0(4,R2),SGDEM                                                    
         CLI   SBESTDEM+1,C'R'     TEST DEMOS'S A RATING                        
         BE    *+12                                                             
         CLI   SBESTDEM+1,C'E'                                                  
         BNE   INX                                                              
         L     R1,0(R2)            YES-MAY NEED TO WEIGHT                       
         BAS   RE,WGT                                                           
         ST    R1,4(R2)                                                         
         B     INX                                                              
*                                                                               
*                                                                               
INX      MVI   INDATA,1            SIGNIFICANT COLUMN DATA                      
         J     XIT                                                              
         SPACE 2                                                                
FINDQTR  ZIC   R3,GLARGS           LOCATE QUARTER WITHIN THE GRID               
         BCTR  R3,0                                                             
         MH    R3,=Y(GRIDL)                                                     
         LA    R3,GRID(R3)                                                      
         BR    RE                                                               
         SPACE 1                                                                
FINDMNTH L     R5,ATOTS            LOCATE MONTH WITH QUARTER                    
         ZIC   R1,GLARGS                                                        
         BCTR  R1,0                                                             
         MH    R1,=Y(MONTOTL)                                                   
         LA    R5,0(R1,R5)                                                      
         BR    RE                                                               
         SPACE 1                                                                
FINDPER  L     R3,ATOTS            FIND SPOTS/COST/DEMOS FOR PERIOD             
         CLI   PERTYP,PERTOT       RETURN: R0=SPOTS                             
         BE    *+12                        R1=A(DOLLARS)                        
         CLI   PERTYP,PERTOTL              RF=A(DEMOS)                          
         BNE   FINDPER2            TEST BUY PERIOD                              
         USING RECTOTD,R3                                                       
         L     R0,RECSPTS          YES-                                         
         LA    R1,RECCOST                                                       
         LA    RF,RECDEMS                                                       
         B     FINDPERY                                                         
FINDPER2 CLI   PERTYP,PERMTH       TEST MONTH PERIOD                            
         BE    *+12                                                             
         CLI   PERTYP,PERMTHL                                                   
         BNE   FINDPER4                                                         
         LR    R5,R3                                                            
         USING MONTOTD,R5                                                       
         L     R0,MTSPTS           YES-                                         
         LA    R1,MTCOST                                                        
         LA    RF,MTDEMS                                                        
         B     FINDPERY                                                         
FINDPER4 CLI   PERTYP,PERWKS       TEST WEEKLY CHUNK                            
         BNE   FINDPERN                                                         
         L     R5,SBACURCH         YES-                                         
         USING SCHUNKD,R5                                                       
         L     R0,SCSPOTS                                                       
         LA    R1,SCGROSS                                                       
         LA    RF,SCDEMOS                                                       
FINDPERY CR    RE,RE               PERIOD FOUND                                 
         BR    RE                                                               
FINDPERN LTR   RE,RE               PERIOD NOT FOUND                             
         BR    RE                                                               
         SPACE 1                                                                
TESTWK   LR    R0,RE               TEST GOAL WEEK IN REQUIRED MONTH             
         L     R1,AMONTHS                                                       
         ZIC   RE,GLARGS           QUARTER NUMBER                               
         BCTR  RE,0                                                             
         MH    RE,=H'12'                                                        
         LA    R1,0(RE,R1)                                                      
         IC    RE,GLARGS+1         MONTH WITHIN QUARTER                         
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         AR    R1,RE                                                            
         LR    RE,R0                                                            
         USING SGLCHNKD,R5                                                      
         CLC   SGDATE,0(R1)        TEST WEEK IS WITHIN THE MONTH                
         BL    *+14                                                             
         CLC   SGDATE,2(R1)                                                     
         BNH   *+8                                                              
         LTR   RE,RE               CC NE - WEEK OUTSIDE MONTH                   
         BR    RE                                                               
         CR    RE,RE               CC EQ - WEEK INSIDE MONTH                    
         BR    RE                                                               
         DROP  R5                                                               
         SPACE 1                                                                
WGT      CLI   SBQMKTWT,C'N'       MARKET WEIGHT ROUTINE                        
         BER   RE                                                               
         ICM   R0,15,SBMKTWGT                                                   
         MR    R0,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* COLUMN OUTPUT ROUTINES                                              *         
***********************************************************************         
         SPACE 1                                                                
OCOST    MVC   COST,0(R2)          SAVE COST                                    
         MVC   ECOST,4(R2)         AND EQUIVALENCED COST                        
         J     XIT                                                              
         SPACE 1                                                                
OSPTS    MVC   SPTS,0(R2)          SAVE SPOTS                                   
         J     XIT                                                              
         SPACE 1                                                                
OBSSPT   MVC   SPTS,0(R2)          SAVE SPOTS                                   
         MVI   GLHOOK,GLEDIT       AND EDIT                                     
         J     XIT                                                              
         SPACE 1                                                                
OSPW     LA    RF,14               SPOTS PER WEEK                               
         SR    R5,R5                                                            
OSPW2    ICM   R5,3,0(R2)                                                       
         BZ    OSPW4                                                            
         EDIT  (R5),(3,(R3))                                                    
OSPW4    LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   RF,OSPW2                                                         
         J     XIT                                                              
         SPACE 1                                                                
*                                  DEMOS                                        
ODEM     CLI   GLARGS,1            TEST FIRST DEMO COLUMN                       
         BNE   *+8                                                              
         LA    R3,1(R3)            YES-PRINT ONE TO RIGHT                       
         L     R1,0(R2)                                                         
         TM    GLINDS,GLTOTLIN     TEST TOTAL LINE                              
         BO    ODEM2                                                            
         L     R1,8(R2)            NO-PICK UP SINGLE SPOT DEMO VALUE            
         SR    R0,R0                                                            
         D     R0,=F'2'                                                         
         LTR   R0,R0               TEST BUYER'S OVERRIDE                        
         BZ    ODEM2                                                            
         MVI   5(R3),C'*'                                                       
*                                                                               
ODEM2    LR    R5,R1                                                            
         MVI   CURTAB+3,1                                                       
         CH    R5,=H'10000'        TEST NEED TO ROUND                           
         BNL   ODEM4                                                            
         CURED (R5),(5,(R3)),CURTAB                                             
         B     ODEM6                                                            
*                                                                               
ODEM4    DS    0H                                                               
         CURED (R5),(5,(R3)),CURTAB,DECS=ROUND                                  
*                                                                               
ODEM6    TM    DETIND,DETCPP       TEST TO PRINT CPP                            
         JZ    XIT                                                              
         L     RF,COST             YES-FORMAT CPP BELOW                         
         L     R5,0(R2)                                                         
         TM    GLINDS,GLTOTLIN     TEST TOTAL LINE                              
         BZ    ODEM8               YES-NO EQUIVALENCING                         
         CLI   SBSPPROF,C'D'       TEST EQUIV DOLLARS OR POINTS                 
         BNE   *+12                                                             
         L     RF,ECOST            EQUIV COST                                   
         B     ODEM8                                                            
         L     R5,4(R2)            EQUIV POINTS                                 
*                                                                               
ODEM8    BAS   RE,CPPCALC          GET CPP                                      
         JZ    XIT                                                              
         LA    R3,198(R3)                                                       
         MVI   CURTAB+3,2                                                       
         CH    R5,=H'10000'        TEST NEED TO ROUND                           
         BNL   ODEM10                                                           
         CURED (R5),(5,(R3)),CURTAB                                             
         B     ODEM12                                                           
*                                                                               
ODEM10   CURED (R5),(5,(R3)),CURTAB,DECS=ROUND                                  
*                                                                               
ODEM12   TM    GLINDS,GLTOTLIN     TEST TOTAL LINE                              
         JZ    XIT                                                              
         MVI   5(R3),C'+'          YES-INDICATE LINE'S EQUIVALENCED             
         J     XIT                                                              
         SPACE 1                                                                
ORDEM    DS    0H                                                               
         CLI   GLARGS,C'S'         TEST PRODUCT SUMMARY                         
         BNE   ORDEM0                                                           
         CLI   SBQMKTWT,C'N'       AND MARKET WEIGHTING                         
         BE    ORDEM0                                                           
         L     R1,0(R2)            YES-UNWEIGHT DEMO                            
         BAS   RE,UNWGT                                                         
         ST    R1,0(R2)                                                         
         L     R1,4(R2)            AND EQUIV DEMO                               
         BAS   RE,UNWGT                                                         
         ST    R1,4(R2)                                                         
ORDEM0   ICM   R5,15,0(R2)         TOTAL DEMOS                                  
         JZ    XIT                                                              
         EDIT  (R5),(10,(R3)),1                                                 
         TM    RCPIND,RCPCPP       TEST TO PRINT RECAP CPP                      
         JZ    XIT                                                              
         L     RF,8(R2)            YES-                                         
         MVI   BYTE,C' '                                                        
         TM    RCPIND,RCPDPT       TEST ACROSS SPOT LENGTHS                     
         BZ    *+12                                                             
         CLI   SBLEN,255                                                        
         BNE   ORDEM1                                                           
         MVI   BYTE,C'+'           YES-                                         
         CLI   SBSPPROF,C'D'       TEST EQUIV DOLLARS                           
         BNE   *+12                                                             
         L     RF,12(R2)           YES-USE EQUIV DOLLARS                        
         B     ORDEM1                                                           
         L     R5,4(R2)            NO-USE EQUIV POINTS                          
ORDEM1   BAS   RE,CPPCALC                                                       
         JZ    XIT                                                              
         MVC   19(1,R3),BYTE                                                    
         MVI   CURTAB+3,2                                                       
         C     R5,=F'1000000'                                                   
         BNL   ORDEM2                                                           
         CURED (R5),(8,11(R3)),CURTAB                                           
         J     XIT                                                              
ORDEM2   CURED (R5),(8,11(R3)),CURTAB,DECS=ROUND                                
         J     XIT                                                              
         SPACE 1                                                                
OTDEM    ICM   R5,15,0(R2)         TOTAL DEMOS                                  
         JZ    XIT                                                              
         EDIT  (R5),(10,(R3)),1                                                 
         TM    DETIND,DETCPP       TEST TO PRINT CPP                            
         JZ    XIT                                                              
         L     RF,8(R2)            YES-                                         
         CLI   SBSPPROF,C'D'       TEST EQUIV DOLLARS                           
         BE    *+8                                                              
         L     R5,4(R2)            NO-USE EQUIV POINTS                          
         BAS   RE,CPPCALC                                                       
         JZ    XIT                                                              
         LA    R3,198(R3)          PRINT CPP BELOW                              
         MVI   CURTAB+3,2                                                       
         C     R5,=F'100000000'                                                 
         BNL   OTDEM2                                                           
         CURED (R5),(10,(R3)),CURTAB                                            
         B     OTDEM4                                                           
OTDEM2   CURED (R5),(10,(R3)),CURTAB,DECS=ROUND                                 
OTDEM4   MVI   10(R3),C'+'                                                      
         J     XIT                                                              
         SPACE 1                                                                
OWCOST   TM    GLINDS3,GLLSTLIN    WEEKLY COST                                  
         JO    XIT                 (NOT WHEN FORMATTING 'LAST' LINE)            
         L     RE,0(R2)                                                         
         L     R1,WKTOTDSP                                                      
         L     R5,AWKTOT                                                        
         TM    RCPIND,RCPDPT                                                    
         BZ    *+8                                                              
         L     R5,AWKTOTDL                                                      
         LA    R5,0(R1,R5)                                                      
         USING WKTOTD,R5                                                        
         ST    RE,WTCOST           MOVE TO WEEKLY TOTALS TABLE                  
         TM    RCPIND,RCPDPT       TEST DAYPART/LENGTH DETAIL                   
         JZ    XIT                                                              
         L     R5,AWKTOT           YES-ADD TO TOTALS                            
         LA    R5,0(R1,R5)                                                      
         A     RE,WTCOST                                                        
         ST    RE,WTCOST                                                        
         J     XIT                                                              
         SPACE 1                                                                
OWGDOL   TM    GLINDS3,GLLSTLIN    WEEKLY GOAL DOLLARS                          
         JO    XIT                 (NOT WHEN FORMATTING 'LAST' LINE)            
         L     RE,0(R2)                                                         
         L     R1,WKTOTDSP                                                      
         L     R5,AWKTOT                                                        
         TM    RCPIND,RCPDPT                                                    
         BZ    *+8                                                              
         L     R5,AWKTOTDL                                                      
         LA    R5,0(R1,R5)                                                      
         USING WKTOTD,R5                                                        
         ST    RE,WTGDOL           MOVE TO WEEKLY TOTALS TABLE                  
         TM    RCPIND,RCPDPT       TEST DAYPART/LENGTH DETAIL                   
         JZ    XIT                                                              
         L     R5,AWKTOT           YES-ADD TO TOTALS                            
         LA    R5,0(R1,R5)                                                      
         A     RE,WTGDOL                                                        
         ST    RE,WTGDOL                                                        
         J     XIT                                                              
         SPACE 1                                                                
OWDEM    TM    GLINDS3,GLLSTLIN    WEEKLY DEMOS                                 
         JO    XIT                 (NOT WHEN FORMATTING 'LAST' LINE)            
         CLI   GLARGS+1,C'S'       TEST PRODUCT SUMMARY                         
         BNE   OWDEM2                                                           
         CLI   SBQMKTWT,C'N'       YES-UNWEIGHT IF MKT WEIGHTING                
         BE    OWDEM2                                                           
         L     R1,0(R2)                                                         
         BAS   RE,UNWGT                                                         
         ST    R1,0(R2)                                                         
OWDEM2   L     RE,0(R2)                                                         
         ZIC   RF,GLARGS                                                        
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         L     R1,WKTOTDSP                                                      
         L     R5,AWKTOT                                                        
         TM    RCPIND,RCPDPT                                                    
         BZ    *+8                                                              
         L     R5,AWKTOTDL                                                      
         LA    R5,0(R1,R5)                                                      
         USING WKTOTD,R5                                                        
         LA    R3,WTDEMS(RF)                                                    
         ST    RE,0(R3)            MOVE TO WEEKLY TOTALS TABLE                  
         TM    RCPIND,RCPDPT       TEST DAYPART/LENGTH DETAIL                   
         JZ    XIT                                                              
         L     R5,AWKTOT           YES-ADD TO TOTALS                            
         LA    R5,0(R1,R5)                                                      
         LA    R3,WTDEMS(RF)                                                    
         A     RE,0(R3)                                                         
         ST    RE,0(R3)                                                         
         J     XIT                                                              
         SPACE 1                                                                
OWGDEM   TM    GLINDS3,GLLSTLIN    WEEKLY GOAL DEMO                             
         JO    XIT                 (NOT WHEN FORMATTING 'LAST' LINE)            
         CLI   GLARGS,C'S'         TEST PRODUCT SUMMARY                         
         BNE   OWGDEM2                                                          
         CLI   SBQMKTWT,C'N'       YES-UNWEIGHT IF MKT WEIGHTING                
         BE    OWGDEM2                                                          
         L     R1,0(R2)                                                         
         BAS   RE,UNWGT                                                         
         ST    R1,0(R2)                                                         
OWGDEM2  L     RE,0(R2)                                                         
         L     R1,WKTOTDSP                                                      
         L     R5,AWKTOT                                                        
         TM    RCPIND,RCPDPT                                                    
         BZ    *+8                                                              
         L     R5,AWKTOTDL                                                      
         LA    R5,0(R1,R5)                                                      
         USING WKTOTD,R5                                                        
         ST    RE,WTGDEM           MOVE TO WEEKLY TOTALS TABLE                  
         TM    RCPIND,RCPDPT       TEST DAYPART/LENGTH DETAIL                   
         JZ    XIT                                                              
         L     R5,AWKTOT           YES-ADD TO TOTALS                            
         LA    R5,0(R1,R5)                                                      
         A     RE,WTGDEM                                                        
         ST    RE,WTGDEM                                                        
         J     XIT                                                              
         SPACE 1                                                                
OUTWSTK  GOTO1 AOWSTACK            WEEKLY STACK                                 
         J     XIT                                                              
         SPACE 1                                                                
OSDEM    DS    0H                  DEMO OUTPUT FOR SUMMARIES                    
         TM    OUTIND,OUTICRDP     TEST ACROSS DAYPARTS NOW                     
         BZ    *+12                                                             
         TM    SUMIND,SUMDEM       YES-TEST WHETHER TO PRINT DEMOS              
         JZ    XIT                                                              
         L     R1,0(R2)            YES-                                         
         CLI   GLARGS+1,C'S'       TEST PRODUCT SUMMARY                         
         BNE   OSDEM1                                                           
         CLI   SBQMKTWT,C'N'       AND MARKET WEIGHTING                         
         BE    OSDEM1                                                           
         BAS   RE,UNWGT            YES-UNWEIGHT DEMO                            
         ST    R1,0(R2)                                                         
OSDEM1   CLI   GLARGS,C'B'                                                      
         BE    *+12                                                             
         ST    R1,NDXGDEM                                                       
         B     *+8                                                              
         ST    R1,NDXBDEM                                                       
         LR    RF,R1                                                            
         L     R1,GLADTENT                                                      
         USING DROD,R1                                                          
         MVI   DRODEC,1                                                         
         L     RE,=F'100000'                                                    
         CLI   GLARGS+2,7                                                       
         BNE   *+8                                                              
         L     RE,=F'1000000'                                                   
         CR    RF,RE                                                            
         BL    OSDEM2                                                           
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,=F'10'                                                        
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         ST    RF,0(R2)                                                         
         MVI   DRODEC,0                                                         
OSDEM2   MVI   GLHOOK,GLEDIT                                                    
         J     XIT                                                              
         DROP  R1                                                               
         SPACE 1                                                                
ODOL     L     RF,0(R2)            COST FOR MARKET AND BRAND                    
         CLI   GLARGS,C'B'         PERFORMANCE REPORTS                          
         BE    *+12                                                             
         ST    RF,NDXGDOL                                                       
         B     *+8                                                              
         ST    RF,NDXBDOL                                                       
         SR    RE,RE               ROUND PENNIES TO DOLLARS                     
         SLDA  RE,1                                                             
         D     RE,=F'100'                                                       
         LTR   R5,RF                                                            
         BM    *+8                                                              
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
         LTR   R5,R5                                                            
         JZ    XIT                                                              
         MVI   CURTAB+3,0                                                       
         L     R1,GLADTENT                                                      
         USING DROD,R1                                                          
         ZIC   R7,DROLEN                                                        
         CLI   DROLEN,7                                                         
         BNE   ODOL1                                                            
         LTR   R5,R5                                                            
         BNM   ODOL2                                                            
         DROP  R1                                                               
ODOL1    CURED (R5),((R7),(R3)),CURTAB,MINUS=YES                                
         J     XIT                                                              
ODOL2    CURED (R5),((R7),(R3)),CURTAB                                          
         J     XIT                                                              
         SPACE 1                                                                
OCPP     TM    OUTIND,OUTICRDP     TEST ACROSS DAYPARTS NOW                     
         BZ    *+12                                                             
         TM    SUMIND,SUMCPP       YES-TEST WHETHER TO PRINT CPP                
         JZ    XIT                                                              
         L     R1,4(R2)                                                         
         CLI   GLARGS,C'S'         TEST PRODUCT SUMMARY                         
         BNE   OCPP2                                                            
         CLI   SBQMKTWT,C'N'       AND MARKET WEIGHTING                         
         BE    OCPP2                                                            
         BAS   RE,UNWGT            YES-UNWEIGHT DEMO                            
OCPP2    LR    R5,R1               R5=DEMO VALUE                                
         L     RF,0(R2)            RF=COST                                      
         BAS   RE,CPPCALC                                                       
         JZ    XIT                                                              
         CLI   SUMDPT,1            TEST NO DPTLEN BREAKOUT                      
         BNE   OCPP4                                                            
         CLI   SBSPPROF+4,C'Y'     OR ALL LINES ARE EQUIVALENCED                
         BE    OCPP4                                                            
         TM    OUTIND,OUTIDTOT     OR THIS LINE IS ACROSS SPOT LENGTHS          
         BZ    *+8                                                              
OCPP4    MVI   6(R3),C'+'          YES-MARK CPP AS EQUIVALENCED                 
         MVI   CURTAB+3,2                                                       
         CURED (R5),(6,(R3)),CURTAB                                             
         J     XIT                                                              
         SPACE 1                                                                
OAVEDEM  DS    0H                  AVERAGE DEMO FOR SUMMARIES                   
         TM    OUTIND,OUTICRDP     TEST ACROSS DAYPARTS NOW                     
         BZ    *+12                                                             
         TM    SUMIND,SUMDEM       YES-TEST WHETHER TO PRINT DEMOS              
         JZ    XIT                                                              
         SR    RE,RE                                                            
         L     RF,NDXBDEM                                                       
         ICM   R1,15,SPTS                                                       
         BZ    OAVEDEM2                                                         
         SLDA  RE,1                                                             
         DR    RE,R1                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         LR    R1,RF                                                            
OAVEDEM2 ST    R1,0(R2)                                                         
         MVI   GLHOOK,GLEDIT                                                    
         J     XIT                                                              
         SPACE 1                                                                
ONDXDEM  DS    0H                  DEMO INDEX                                   
         TM    OUTIND,OUTICRDP     TEST ACROSS DAYPARTS NOW                     
         BZ    *+12                                                             
         TM    SUMIND,SUMDEM       YES-TEST WHETHER TO PRINT DEMOS              
         JZ    XIT                                                              
         L     RF,NDXBDEM          DEMO INDEX                                   
         L     R1,NDXGDEM                                                       
         B     *+12                                                             
ONDXDOL  L     RF,NDXBDOL          DOLLAR INDEX                                 
         L     R1,NDXGDOL                                                       
         LTR   R1,R1                                                            
         JZ    XIT                                                              
         SR    RE,RE                                                            
         M     RE,=F'200'                                                       
         DR    RE,R1                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         CH    RF,=H'10000'                                                     
         BL    *+14                                                             
         MVC   2(2,R3),=C'HI'                                                   
         J     XIT                                                              
         ST    RF,0(R2)                                                         
         MVI   GLHOOK,GLEDIT                                                    
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* MISC OUTPUT RELATED ROUTINES                                        *         
***********************************************************************         
         SPACE 1                                                                
CPPCALC  LR    R0,RE               CPP CALCULATION                              
         LTR   R5,R5                                                            
         BZ    CPPCALCX                                                         
         M     RE,=F'20'           RF=COST                                      
         DR    RE,R5               R5=POINTS                                    
         LTR   R5,RF                                                            
         BM    *+8                                                              
         A     R5,=F'1'                                                         
         SRA   R5,1                RETURN R5=CPP                                
         LTR   R5,R5               AND CC = NE IF CPP=0                         
CPPCALCX LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
UNWGT    ICM   RF,15,TOTMKTWT      UNWEIGHT ROUTINE                             
         BNZ   *+8                                                              
         SR    R1,R1                                                            
         BR    RE                                                               
         SR    R0,R0                                                            
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
         EJECT                                                                  
       ++INCLUDE SPMGRTAB                                                       
         EJECT                                                                  
***********************************************************************         
* EXTENTION ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
T20416X  DS    0F                                                               
         NMOD1 0,**416X**,RA                                                    
         L     RC,AGEND                                                         
         USING GEND,RC                                                          
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     SETWKS                                                           
         B     GETDPTTB                                                         
         B     GETEST                                                           
         B     GETMGR                                                           
         B     GETREP                                                           
         B     OSTA                                                             
         B     IBUY                                                             
         B     OBUY                                                             
         B     OWSTACK                                                          
         B     HEAD                                                             
         B     GETNAME                                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
* SET WEEKS TABLES                                                    *         
***********************************************************************         
         SPACE 1                                                                
SETWKS   DS    0H                                                               
         L     R4,AWEEKS           COUNT N'WEEKS IN REQUEST PERIOD              
         SR    R1,R1                                                            
         CLI   0(R4),0                                                          
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         LA    R4,4(R4)                                                         
         B     *-16                                                             
         STC   R1,NUMWKS                                                        
*                                                                               
         LA    R0,4                SET UP RELATIONSHIPS BETWEEN                 
         LA    R2,WKSQ1            WEEKS AND QUARTERS                           
         ST    R2,FULL                                                          
         L     R3,AQTRS                                                         
         L     R4,AWEEKS                                                        
         LA    R5,WKDSPLS                                                       
         LA    R1,WKS                                                           
         ST    R1,DUB                                                           
         LA    R6,1                                                             
         LA    R7,1                                                             
*                                                                               
SETW2    CLI   0(R4),0             TEST END OF WEEKS                            
         BE    SETW8                                                            
         CLI   NUMWKS,14           TEST NO MORE THAN 14 WEEKS                   
         BNH   SETW6               YES-ALL WEEKS GO TO QTR1                     
*                                                                               
SETW4    CLC   2(2,R4),2(R3)       TEST WEEK IN THIS QTR                        
         BNH   SETW6                                                            
         LA    R3,4(R3)            NO-TRY NEXT QTR                              
         LA    R6,1(R6)            NEXT QTR NUMBER                              
         LA    R7,1                FIRST WEEK                                   
         L     R2,FULL                                                          
         LA    R2,L'WKSQ1(R2)                                                   
         ST    R2,FULL                                                          
         BCT   R0,SETW4                                                         
         DC    H'0'                4 QUARTERS MAX                               
*                                                                               
SETW6    GOTO1 DATCON,DMCB,(2,(R4)),(4,(R2))                                    
         STC   R6,0(R5)            QTR NUMBER                                   
         STC   R7,1(R5)            WEEK NUMBER WITHIN QTR                       
         LA    R7,1(R7)                                                         
         L     R1,DUB                                                           
         MVC   0(5,R1),0(R2)                                                    
         LA    R1,5(R1)            NEXT WEEK                                    
         ST    R1,DUB                                                           
         LA    R2,5(R2)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,3(R5)                                                         
         B     SETW2                                                            
*                                                                               
SETW8    LA    R2,QTRDATES         FORMAT QUARTER DATES                         
         L     R3,AQTRS                                                         
         LA    R0,4                                                             
*                                                                               
SETW10   CLI   0(R3),0                                                          
         BE    SETW12                                                           
         GOTO1 DATCON,DMCB,(2,(R3)),(5,(R2))                                    
         GOTO1 (RF),(R1),(2,2(R3)),(5,9(R2))                                    
         MVI   8(R2),C'-'                                                       
         CH    R0,=H'4'                                                         
         BNE   *+10                                                             
         MVC   PERDATES(9),0(R2)   FORMAT PERIOD DATES                          
         MVC   PERDATES+9(8),9(R2)                                              
         LA    R2,L'QTRDATES(R2)                                                
         LA    R3,4(R3)                                                         
         BCT   R0,SETW10                                                        
*                                                                               
SETW12   CLI   MTOTIND,0           TEST MONTHLY TOTALS NEEDED                   
         BE    SETW18                                                           
         LA    R0,MAXMONS          YES-SET MONTH NUMBERS                        
         L     R3,AMONTHS                                                       
         L     R4,AWEEKS                                                        
         LA    R5,WKDSPLS                                                       
         LA    R6,1                                                             
*                                                                               
SETW14   CLI   0(R4),0                                                          
         BE    SETW18                                                           
*                                                                               
SETW15   CLC   2(2,R4),2(R3)       TEST WEEK IN THIS MONTH                      
         BNH   SETW16                                                           
         LA    R3,4(R3)            NO-TRY NEXT MONTH                            
         LA    R6,1(R6)            NEXT MONTH NUMBER                            
         BCT   R0,SETW15                                                        
         DC    H'0'                                                             
*                                                                               
SETW16   STC   R6,2(R5)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,3(R5)                                                         
         B     SETW14                                                           
*                                                                               
SETW18   TM    SUMIND,SUMMTH       TEST MONTHLY MKT PERFORMANE RPT              
         BZ    SETW22                                                           
         L     R2,AMONTHS          YES-GET FORMATTED MONTH START AND            
         LA    R3,MONDATES             END DATES                                
         LA    R0,12                                                            
*                                                                               
SETW20   CLI   0(R2),0                                                          
         BE    SETW22                                                           
         GOTO1 DATCON,DMCB,(2,(R2)),(4,(R3))                                    
         GOTO1 (RF),(R1),(2,2(R2)),(4,5(R3))                                    
         LA    R2,4(R2)                                                         
         LA    R3,10(R3)                                                        
         BCT   R0,SETW20                                                        
*                                                                               
SETW22   B     SETWX                                                            
*                                                                               
SETWX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET DAYPART TABLE                                                   *         
* INPUT  : SBDPTMEN = DAYPART MENU                                    *         
* OUTPUT : SBDPTTAB SET                                               *         
***********************************************************************         
         SPACE 1                                                                
GETDPTTB DS    0H                                                               
         OC    SBADPTTB,SBADPTTB   TEST DAYPART TABLES BUFFER                   
         BZ    GDX                                                              
         CLC   SBDPTMEN,SVDPTMEN   YES-TEST DAYPART MENU CHANGE                 
         BE    GDX                                                              
         MVC   SVDPTMEN,SBDPTMEN   YES-GET DAYPART TABLE                        
         LA    R1,ALPHATAB                                                      
         SR    RE,RE                                                            
*                                                                               
GD10     CLI   0(R1),0                                                          
         BNE   *+12                                                             
         LA    RE,25                                                            
         B     GD20                                                             
         CLC   SBDPTMEN,0(R1)                                                   
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   RE,GD10                                                          
         LPR   RE,RE                                                            
*                                                                               
GD20     MH    RE,=H'180'                                                       
         L     R1,SBADPTTB                                                      
         LA    R1,0(RE,R1)                                                      
         XC    SBDPTTAB,SBDPTTAB                                                
         MVC   SBDPTTAB(180),0(R1)                                              
*                                                                               
GDX      J     XIT                                                              
         SPACE 1                                                                
ALPHATAB DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',X'00'                    
         EJECT                                                                  
***********************************************************************         
* READ ESTIMATE HEADER TO GET ESTIMATE DEMOS                          *         
* INPUT  : R1=A(PRODUCT)                                              *         
*          SVBEST=ESTIMATE                                            *         
* OUTPUT : SBESTDEM=ESTIMATE DEMOS                                    *         
***********************************************************************         
         SPACE 1                                                                
GETEST   XC    SBESTDEM,SBESTDEM                                                
         XC    KEY,KEY             READ ESTIMATE HEADER                         
         LA    R2,KEY                                                           
         USING ESTHDRD,R2                                                       
         MVC   EKEYAM,SBBAGYMD                                                  
         MVC   EKEYCLT,SBBCLT                                                   
         MVC   EKEYPRD,0(R1)                                                    
         MVC   EKEYEST,SVBEST                                                   
         GOTO1 HIGH                                                             
         CLC   EKEY(8),KEYSAVE                                                  
         BNE   GETESTX                                                          
         OC    EKEY+8(5),EKEY+8    TEST ITS AN ESTIMATE RECORD                  
         BNZ   GETESTX                                                          
         LA    R2,ESTREC                                                        
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   SBESTDEM,EDEMLST                                                 
*                                                                               
GETESTX  J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO GET MARKET GROUP FOR PRODUCT GROUP EXCEPTIONS        *         
* INPUT  : SBBMKT=MARKET CODE                                         *         
*          SBBPGR=PRODUCT GROUP                                       *         
* OUTPUT : SBBMGR=MARKET GROUP                                        *         
***********************************************************************         
         SPACE 1                                                                
GETMGR   DS    0H                                                               
         SR    RE,RE                                                            
         ICM   RE,3,SBBMKT         INDEX INTO MARKET GRP ASSIGN TABLE           
         AR    RE,RE                                                            
         MVC   HALF,SBBPGR                                                      
         OC    HALF,SBPG1MSK                                                    
         LA    R0,SBEXMAX                                                       
         LA    R1,SBPGRPEX                                                      
         LA    RF,SBAMGTB2                                                      
*                                                                               
GM2      OC    0(2,R1),0(R1)                                                    
         BZ    GM4                                                              
         MVC   FULL(2),0(R1)                                                    
         OC    FULL(2),SBPG1MSK                                                 
         CLC   HALF,FULL           TEST IT'S THE EXCEPTION GROUP                
         BNE   *+12                                                             
         A     RE,0(RF)            YES-THEN USE APPROPRIATE MGRP TABLE          
         B     GM6                                                              
         LA    R1,2(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,GM2                                                           
*                                                                               
GM4      A     RE,SBAMGTAB                                                      
*                                                                               
GM6      MVC   SBBMGR,=X'9999'                                                  
         OC    0(2,RE),0(RE)                                                    
         BZ    GMX                                                              
         MVC   SBBMGR,0(RE)                                                     
*                                                                               
GMX      J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET A REP RECORD                                         *         
* INPUT  : SVREP   = 3-CHAR REP CODE                                  *         
* OUTPUT : SBREPNM = REP NAME                                         *         
***********************************************************************         
         SPACE 1                                                                
GETREP   LA    R2,KEY              READ REP RECORD                              
         USING REPRECD,R2                                                       
         MVI   REPKEY,C'0'                                                      
         MVC   REPKEY+1(16),REPKEY                                              
         MVI   REPKTYPE,C'R'                                                    
         MVC   REPKMED,SBQMED                                                   
         MVC   REPKREP,SVREP                                                    
         MVC   REPKAGY,AGENCY                                                   
         L     R2,AIO2                                                          
         ST    R2,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BNE   *+16                                                             
         MVC   SBREPNM(L'RNAME),RNAME   EXTRACT REP NAME                        
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* STATION OUTPUT ROUTINE                                              *         
***********************************************************************         
         SPACE 1                                                                
OSTA     DS    0H                                                               
         MVC   SVSTA,0(R2)                                                      
         MVC   STAHEAD,BLANKS                                                   
         MVC   STAHEAD(7),=C'STATION'                                           
         CLI   SBQMED,C'C'                                                      
         BNE   OSTA2                                                            
         MVC   STAHEAD(7),=C' *SPOT*'                                           
         CLI   4(R2),C'N'                                                       
         BNE   OSTA2                                                            
         MVC   STAHEAD(7),=C'NETWORK'                                           
*                                                                               
OSTA2    MVC   STAHEAD+8(4),0(R2)                                               
         LA    R3,STAHEAD+10                                                    
         CLI   4(R2),C'N'                                                       
         BE    OSTA4                                                            
         LA    R3,STAHEAD+11                                                    
         CLI   0(R3),C' '                                                       
         BNH   *+8                                                              
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'-'                                                       
         MVC   1(1,R3),4(R2)                                                    
         MVI   2(R3),C'M'                                                       
         CLI   4(R2),C'T'                                                       
         BE    *+12                                                             
         CLI   4(R2),C' '                                                       
         BNE   OSTA4                                                            
         MVC   1(2,R3),=C'TV'                                                   
*                                                                               
OSTA4    MVC   SVSTABIG,STAHEAD+8                                               
         LA    R3,5(R3)            CHANNEL                                      
         CLC   5(4,R2),BLANKS                                                   
         BNH   OSTA8                                                            
         CLC   5(4,R2),=C'0000'                                                 
         BE    OSTA8                                                            
         CLI   SBQMED,C'T'                                                      
         BNE   OSTA6                                                            
         MVC   0(3,R3),=C'CH='                                                  
         MVC   3(2,R3),5(R2)                                                    
         MVI   5(R3),C','                                                       
         LA    R3,6(R3)                                                         
         B     OSTA8                                                            
*                                                                               
OSTA6    CLI   SBQMED,C'R'                                                      
         BNE   OSTA8                                                            
         MVC   0(5,R3),=C'FREQ='                                                
         MVC   5(4,R3),5(R2)                                                    
         MVI   9(R3),C','                                                       
         LA    R3,10(R3)                                                        
*                                                                               
OSTA8    TM    DETIND,DETAFF       AFFILIATE                                    
         BZ    OSTA10                                                           
         CLC   9(3,R2),BLANKS                                                   
         BNH   OSTA10                                                           
         CLC   9(3,R2),=C'000'                                                  
         BE    OSTA10                                                           
         MVC   0(6,R3),=C'AFFIL='                                               
         MVC   6(3,R3),9(R2)                                                    
         MVI   9(R3),C','                                                       
         LA    R3,10(R3)                                                        
*                                                                               
OSTA10   TM    DETIND,DETREP       REP                                          
         BZ    OSTA14                                                           
         CLC   SVREP,12(R2)                                                     
         BE    OSTA12                                                           
         MVC   SVREP,12(R2)                                                     
         MVC   SBREP,12(R2)                                                     
         MVC   SBREPNM,BLANKS                                                   
         CLC   SVREP,BLANKS                                                     
         BNH   OSTA12                                                           
         CLC   SVREP,=C'000'                                                    
         BE    OSTA12                                                           
         GOTO1 GETREPNM                                                         
         BE    OSTA12                                                           
         GOTO1 AGETREP                                                          
         GOTO1 PUTREPNM                                                         
*                                                                               
OSTA12   CLC   SBREPNM,BLANKS                                                   
         BNH   OSTA14                                                           
         OC    SBREPNM,BLANKS                                                   
         MVC   0(4,R3),=C'REP='                                                 
         MVC   4(22,R3),SBREPNM                                                 
         LA    R3,27(R3)                                                        
*                                                                               
OSTA14   BCTR  R3,0                                                             
         CLI   0(R3),C','                                                       
         BNE   *+8                                                              
         MVI   0(R3),C' '                                                       
         GOTO1 CENTER,DMCB,STAHEAD,63                                           
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUY DESCRIPTION INPUT ROUTINE                                       *         
***********************************************************************         
         SPACE 1                                                                
IBUY     DS    0H                                                               
         L     R3,SBAIO1           BUY DETAIL INPUT ROUTINE                     
         USING BUYRECD,R3                                                       
         CLC   BUYKEY,SVBUYKEY     TEST NEW BUY KEY                             
         BE    IBUY12                                                           
         MVC   SVBUYKEY,BUYKEY                                                  
         XC    BUYDET(BUYDETL),BUYDET  YES-SET BUY DETAILS                      
         MVC   BYEST,BUYKEST                                                    
         MVC   BYLINE1,BUYKBUY                                                  
         MVC   BYSTART,BDSTART                                                  
         MVC   BYEND,BDEND                                                      
         MVC   BYWKS,BDWKS                                                      
         MVC   BYWKIND,BDWKIND                                                  
         MVC   BYDAY,BDDAY                                                      
         MVC   BYNOWK,BDNOWK                                                    
         MVC   BYTIME,BDTIMST                                                   
         MVC   BYPROG,BDPROGRM                                                  
         MVC   BYPROGT,BDPROGT                                                  
         OI    BYPROGT,X'40'                                                    
         SR    R1,R1                                                            
         ICM   R1,7,BDCOST                                                      
         TM    BDSTAT,X'01'        TEST NETWORK                                 
         BZ    *+16                                                             
         TM    BDCIND2,X'01'       TEST IN PENNIES                              
         BO    *+8                                                              
         MH    R1,=H'100'          CONVERT DOLLARS TO PENNIES                   
         STCM  R1,15,BYCOST                                                     
         MVC   BYDPT,BDDAYPT                                                    
         MVC   BYLEN,BDSEC                                                      
         MVC   BYSEDAY,BDSEDAY                                                  
         MVC   BYSTAT2,BDSTAT2                                                  
         MVC   BYREP,BDREP         SPECIAL REP                                  
         MVC   BYMGDATE,BDMGDATE                                                
         MVI   BYPKGIND,0                                                       
*                                                                               
         SR    R0,R0               SCAN ELEMENTS                                
         LA    R5,BDELEM                                                        
IBUY2    CLI   0(R5),0                                                          
         BE    IBUY12                                                           
         CLI   0(R5),2             DEMO ELEMENT -                               
         BNE   *+14                                                             
         USING NDELEM,R5                                                        
         MVC   BYBOOK,NDBOOK       RATING BOOK                                  
         B     IBUY10                                                           
         CLI   0(R5),4             PIGGYBACK ELEMENT -                          
         BNE   IBUY3                                                            
         USING PBELEM,R5                                                        
         CLI   BDTIME,0            CHECK FOR PIGGYBACKS                         
         BE    IBUY10                                                           
         MVC   BYPBPRD,PBPRD       PARTNER PRODUCT                              
         MVC   BYPBTIME,PBTIME     TIME SHARE                                   
         MVC   BYPBEST,PBEST       PASSIVE ESTIMATE NUMBER                      
         B     IBUY10                                                           
*                                                                               
IBUY3    CLI   0(R5),5             PACKAGE ELEMENT -                            
         BNE   IBUY4                                                            
         USING PKGELEM,R5                                                       
         MVC   BYPKGIND,PKGIND     SET PACKAGE INDICATOR                        
         CLI   PKGIND,2            TEST PACKAGE SLAVE                           
         BE    *+12                                                             
         CLI   PKGIND,6            OR REVISION SLAVE                            
         BNE   *+16                                                             
         MVC   BYLINE1,PKGLINES    YES-SET MASTER LINE NUMBER                   
         MVC   BYLINE2,BUYKBUY         AND SLAVE                                
         CLI   PKGIND,8            TEST MAKEGOOD SLAVE                          
         BNE   IBUY10                                                           
         MVC   BYLINE2,PKGLINES    YES-SET MAKEGOOD MASTER                      
         B     IBUY10                                                           
*                                                                               
IBUY4    CLI   0(R5),X'66'         COMMENT ELEMENT -                            
         BNE   IBUY7                                                            
         USING COMELEM,R5                                                       
         CLI   CMTOPT,0            ALL COMMENTS                                 
         BE    IBUY6                                                            
         CLI   CMTOPT,1            ACCOUNTING COMMENTS ONLY                     
         BNE   IBUY5                                                            
         CLI   CMDATA,C'$'                                                      
         BE    IBUY6                                                            
         CLI   CMNUM,4                                                          
         BE    IBUY6                                                            
         CLI   CMNUM,5                                                          
         BE    IBUY6                                                            
*                                                                               
IBUY5    CLI   CMTOPT,2            'COMMENT-' COMMENTS ONLY                     
         BNE   IBUY10                                                           
         CLC   CMDATA(8),=C'COMMENT-'                                           
         BNE   IBUY10                                                           
*                                                                               
IBUY6    LH    RE,GECOMSEQ         ADD A COMMENT ELEMENT TO THE                 
         LA    RE,1(RE)            NAME POOL                                    
         STH   RE,GECOMSEQ                                                      
         OC    BYCOM1,BYCOM1                                                    
         BNZ   *+12                                                             
         STCM  RE,3,BYCOM1         FIRST COMMENT SEQUENCE NUMBER                
         B     *+8                                                              
         STCM  RE,3,BYCOM2         LAST COMMENT SEQUENCE NUMBER                 
         L     R1,SBAIO2                                                        
         MVI   0(R1),GENELCOM                                                   
         STCM  RE,3,1(R1)                                                       
         ZIC   RF,CMLEN                                                         
         SH    RF,=H'3'                                                         
         STC   RF,3(R1)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   4(0,R1),CMDATA                                                   
         GOTO1 PUTGENEL                                                         
         B     IBUY10                                                           
*                                                                               
IBUY7    CLI   0(R5),X'67'         ORBIT ELEMENT -                              
         BNE   IBUY8                                                            
         USING ORBELEM,R5                                                       
         LH    RE,GEORBSEQ         ADD AN ORBIT ELEMENT TO THE                  
         LA    RE,1(RE)            NAME POOL                                    
         STH   RE,GEORBSEQ                                                      
         STCM  RE,3,BYORB                                                       
         L     R1,SBAIO2                                                        
         MVI   0(R1),GENELORB                                                   
         STCM  RE,3,1(R1)                                                       
         ZIC   RF,ORBLEN                                                        
         SH    RF,=H'4'                                                         
         STC   RF,3(R1)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   4(0,R1),ORBDAY                                                   
         GOTO1 PUTGENEL                                                         
         B     IBUY10                                                           
*                                                                               
IBUY8    CLI   0(R5),XCHCODEQ      EXCHANGE ELEMENT -                           
         BNE   IBUY10                                                           
         USING XCHELEM,R5                                                       
         LA    RE,SBAGYREC                                                      
         CLI   AGYPROF+7-AGYHDR(RE),C'C'   TEST CANADA                          
         BNE   IBUY10                                                           
         MVC   BYXRATE,XCHRATE     YES-EXCHANGE RATE                            
         MVC   BYXC58,XCHC58           AND C58 TAX                              
         DROP  R5                                                               
*                                                                               
IBUY10   IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     IBUY2                                                            
*                                                                               
IBUY12   MVC   0(BUYDETL,R2),BUYDET                                             
*                                                                               
IBUYX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUY DESCRIPTION OUTPUT ROUTINE                                      *         
***********************************************************************         
         SPACE 1                                                                
OBUY     DS    0H                                                               
         MVC   BUYDET(BUYDETL),0(R2)                                            
         ZIC   R1,BYEST                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB                                                      
         MVI   3(R3),C'-'                                                       
         ZIC   R1,BYLINE1          BUYLINE NUMBER                               
         CLI   BYPKGIND,2          TEST PACKAGE SLAVE                           
         BE    *+12                                                             
         CLI   BYPKGIND,6          OR REVISION SLAVE                            
         BNE   *+8                                                              
         IC    R1,BYLINE2          YES-LINE NUMBER IS SLAVE LINE NUMBER         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R3),DUB                                                      
         GOTO1 DATCON,DMCB,(3,BYSTART),(4,9(R3))   START/END DATES              
         MVI   14(R3),C'-'                                                      
         GOTO1 (RF),(R1),(3,BYEND),(4,15(R3))                                   
         TM    BYSTAT2,X'02'       CONFIRMED START DATE                         
         BZ    *+8                                                              
         MVI   8(R3),C'('                                                       
         TM    BYSTAT2,X'01'       CONFIRMED END DATE                           
         BZ    *+8                                                              
         MVI   20(R3),C')'                                                      
         EDIT  BYWKS,(2,21(R3))    NUMBER OF WEEKS                              
         CLI   BYWKIND,C'O'                                                     
         BE    *+10                                                             
         MVC   23(1,R3),BYWKIND    A/T/F                                        
         GOTO1 DAYUNPK,DMCB,(BYSEDAY,BYDAY),(0,25(R3))   DAYS                   
         ZIC   R1,BYNOWK                                                        
         CVD   R1,DUB              NUMBER OF SPOTS PER WEEK                     
         OI    DUB+7,X'0F'                                                      
         UNPK  33(2,R3),DUB                                                     
         CLI   33(R3),C'0'                                                      
         BNE   *+8                                                              
         MVI   33(R3),C' '                                                      
         GOTO1 UNTIME,DMCB,BYTIME,36(R3)    TIMES                               
*                                                                               
         LA    R3,198(R3)          SECOND LINE                                  
         OC    BYBOOK,BYBOOK                                                    
         BZ    OBUY2                                                            
         MVC   FULL(2),BYBOOK      BOOK                                         
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(3,FULL),(9,2(R3))                                   
*                                                                               
OBUY2    MVC   9(17,R3),BYPROG     PROGRAMMING                                  
         MVC   27(1,R3),BYPROGT    PROGRAM TYPE                                 
         TM    DETIND,DETDOL       COST                                         
         BZ    OBUY4                                                            
         MVI   CURTAB+3,2                                                       
         CURED BYCOST,(10,30(R3)),CURTAB,CURSYMB=YES,ALIGN=LEFT,FLOAT=-         
*                                                                               
OBUY4    MVC   43(1,R3),BYDPT      DAYPART/LENGTH                               
         MVI   CURTAB+3,0                                                       
         CURED BYLEN,(3,44(R3)),CURTAB,ALIGN=LEFT                               
*                                                                               
         LA    R3,198(R3)          THIRD LINE                                   
         LA    R5,18               (MAX 18 MORE LINES)                          
         TM    DETIND,DETSRP                                                    
         BZ    OBUY6                                                            
         MVC   0(15,R3),=C'***SPECIAL REP='                                     
         GOTO1 RCPACK,DMCB,(C'U',BYREP),18(R3)                                  
*                                                                               
OBUY6    CLI   BYPKGIND,0          PACKAGE                                      
         BE    OBUY8                                                            
         CLI   BYPKGIND,7          IGNORE MAKEGOOD MASTER                       
         BE    OBUY8                                                            
         MVC   31(7,R3),=C'PKG MST'                                             
         CLI   BYPKGIND,1          PACKAGE MASTER                               
         BE    OBUY8                                                            
         MVC   31(3,R3),=C'ORB'                                                 
         CLI   BYPKGIND,3          ORBIT MASTER                                 
         BE    OBUY8                                                            
         MVC   31(3,R3),=C'REV'                                                 
         CLI   BYPKGIND,5          REVISION MASTER                              
         BE    OBUY8                                                            
         MVC   31(7,R3),BLANKS                                                  
         ZIC   R2,BYLINE1                                                       
         CLI   BYPKGIND,8                                                       
         BE    *+14                                                             
         MVC   31(4,R3),=C'MST='                                                
         B     OBUY7                                                            
         TM    DETIND,DETMKG       TEST PRINT MAKEGOOD CAPTIONS                 
         BZ    OBUY8                                                            
         IC    R2,BYLINE2                                                       
         MVC   31(4,R3),=C'*MG*'   MAKEGOOD SLAVE                               
         OC    BYMGDATE,BYMGDATE   MISSED DATE                                  
         BZ    OBUY7                                                            
         GOTO1 DATCON,DMCB,(2,BYMGDATE),(8,39(R3))                              
*                                                                               
OBUY7    CVD   R2,DUB              MASTER LINE NUMBER                           
         OI    DUB+7,X'0F'                                                      
         UNPK  35(3,R3),DUB                                                     
*                                                                               
OBUY8    CLC   BYPBPRD,BLANKS      PARTNER PRODUCT                              
         BNH   OBUY10                                                           
         CLI   0(R3),C'*'                                                       
         BNE   *+10                                                             
         LA    R3,198(R3)                                                       
         BCTR  R5,0                                                             
         MVC   1(8,R3),=C'PARTNER='                                             
         MVC   9(3,R3),BYPBPRD                                                  
         MVI   12(R3),C'/'                                                      
         ZIC   R1,BYPBTIME                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  13(2,R3),DUB                                                     
         MVI   15(R3),C'-'                                                      
         ZIC   R1,BYPBEST                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  16(3,R3),DUB                                                     
         MVI   19(R3),C'P'                                                      
*                                                                               
OBUY10   OC    BYXRATE,BYXRATE     CANADAIAN EXCHANGE RATE AND C58 TAX          
         BZ    OBUY12                                                           
         CLI   0(R3),C' '                                                       
         BNH   *+10                                                             
         LA    R3,198(R3)                                                       
         BCTR  R5,0                                                             
         MVC   0(9,R3),=C'EXCHANGE='                                            
         LA    R2,9(R3)                                                         
         MVI   CURTAB+3,4                                                       
         CURED (2,BYXRATE),(7,0(R2)),CURTAB,ALIGN=LEFT                          
         AR    R2,R0                                                            
         MVI   0(R2),C'%'                                                       
         OC    BYXC58,BYXC58                                                    
         BZ    OBUY12                                                           
         MVC   1(5,R2),=C',C58='                                                
         LA    R2,6(R2)                                                         
         MVI   CURTAB+3,2                                                       
         CURED (2,BYXC58),(6,0(R2)),CURTAB,ALIGN=LEFT                           
         AR    R2,R0                                                            
         MVI   0(R2),C'%'                                                       
*                                                                               
OBUY12   OC    BYORB,BYORB         ORBIT                                        
         BZ    OBUY16                                                           
         L     R1,SBAIO2           GET ORBIT'S ELEMENT                          
         MVI   0(R1),GENELORB                                                   
         MVC   1(2,R1),BYORB                                                    
         GOTO1 GETGENEL                                                         
         BNE   OBUY16                                                           
         ZIC   R0,3(R1)                                                         
         SRL   R0,4                                                             
         LTR   R0,R0               R0=N'ORBIT LINES                             
         BNP   OBUY16                                                           
         LA    R2,4(R1)                                                         
         CLI   0(R3),C' '                                                       
         BNH   OBUY14                                                           
         LA    R3,198(R3)                                                       
         BCTR  R5,0                                                             
*                                                                               
OBUY14   MVC   0(7,R3),=C'*ORBIT*'                                              
         GOTO1 DAYUNPK,DMCB,(BYSEDAY,(R2)),(0,9(R3))                            
         GOTO1 UNTIME,DMCB,1(R2),18(R3)                                         
         MVC   30(7,R3),5(R2)      DESCRIPTION                                  
         MVI   CURTAB+3,1                                                       
         CURED (B2,12(R2)),(6,38(R3)),CURTAB    DEMO                            
         LA    R2,16(R2)                                                        
         BCT   R0,*+8              NEXT ORBIT LINE                              
         B     OBUY16                                                           
         LA    R3,198(R3)          NEXT PRINT LINE                              
         BCT   R5,OBUY14                                                        
         DC    H'0'                                                             
*                                                                               
OBUY16   SR    R2,R2               COMMENTS                                     
         ICM   R2,3,BYCOM1                                                      
         BZ    OBUY24                                                           
*                                                                               
OBUY18   L     R1,SBAIO2           GET COMMENT'S ELEMENT                        
         MVI   0(R1),GENELCOM                                                   
         STCM  R2,3,1(R1)                                                       
         GOTO1 GETGENEL                                                         
         BNE   OBUY22                                                           
         LA    RF,4(R1)            A(COMMENT)                                   
         ST    RF,DMCB                                                          
         ZIC   RE,3(R1)            COMMENT LENGTH                               
         STC   RE,DMCB                                                          
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         OC    0(0,RF),BLANKS                                                   
         CLI   0(R3),C' '                                                       
         BNH   OBUY20                                                           
         LA    R3,198(R3)                                                       
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         BP    OBUY20                                                           
         DC    H'0'                                                             
*                                                                               
OBUY20   ST    R5,DMCB+8           MAX LINES LEFT                               
         MVI   DMCB+8,198                                                       
         GOTO1 CHOPPER,DMCB,,(47,(R3))                                          
         L     RF,8(R1)                                                         
         SR    R5,RF                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    R3,198(R3)                                                       
         BCT   RF,*-4                                                           
*                                                                               
OBUY22   OC    BYCOM2,BYCOM2       TEST MORE THAN ONE COMMENT                   
         BZ    OBUY24                                                           
         LA    R2,1(R2)            YES-                                         
         CLM   R2,3,BYCOM2                                                      
         BNH   OBUY18                                                           
*                                                                               
OBUY24   B     OBUY90                                                           
*                                                                               
OBUY90   CLC   0(198,R3),BLANKS    LAST LINE SHOULD BE BLANK                    
         BNH   *+8                                                              
         LA    R3,198(R3)                                                       
         MVI   0(R3),0                                                          
*                                                                               
OBUYX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* OUTPUT ROUTINE FOR WEEKLY STACKS (RECAPS 3 AND 4)                   *         
***********************************************************************         
         SPACE 1                                                                
         USING WKTOTD,R5                                                        
OWSTACK  DS    0H                                                               
         TM    GLINDS3,GLLSTLIN    TEST PRINTING 'LASTS' LINE NOW               
         BZ    OWSX                                                             
         L     R6,ANXTWK                                                        
         ICM   R5,15,ANXTNTRY      YES-TEST CONTINUATION MODE                   
         BNZ   OWS2                YES                                          
         L     R5,AWKTOT           NO-FORMAT FROM START                         
         CLI   FMTWKS,C'D'         TEST FORMATTING DPTLEN DETAIL                
         BNE   *+8                                                              
         L     R5,AWKTOTDL         YES-USE DPTLEN TOTALS                        
         LA    R6,WKS                                                           
         XC    TCOST,TCOST         CLEAR ACCUMULATOTS                           
         LA    RF,MAXDEMS*4                                                     
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         XC    TDEMS(0),TDEMS                                                   
         XC    TGDOL,TGDOL                                                      
         XC    TGDEM,TGDEM                                                      
*                                                                               
OWS2     L     R1,AWKTOTX          SET A(END OF TABLE)                          
         CLI   FMTWKS,C'D'                                                      
         BNE   *+8                                                              
         L     R1,AWKTOTDX                                                      
         ST    R1,FULL                                                          
         LA    R2,15               PRINT 15 WEEKS                               
         LA    R3,8(R3)                                                         
         CLI   RCPOPT,3                                                         
         BNE   *+8                                                              
         LA    R3,2(R3)                                                         
         MVI   BYTE,0                                                           
*                                                                               
OWS4     OC    0(WKTOTL,R5),0(R5)  TEST ANY DATA THIS WEEK                      
         BZ    OWS20                                                            
         MVC   1(5,R3),0(R6)       PRINT THE WEEK START DATE                    
         LA    R7,198+198(R3)                                                   
         CLI   RCPOPT,3            TEST GOALS ARE IN THE STACK                  
         BNE   OWS12                                                            
         TM    RCPIND,RCPDEM       YES-                                         
         BZ    OWS6                                                             
         L     R1,WTDEMS                                                        
         BAS   RE,EDDEM            DEMO                                         
         A     R1,TDEMS                                                         
         ST    R1,TDEMS                                                         
         LA    R7,198(R7)                                                       
*                                                                               
OWS6     TM    RCPIND,RCPDOL                                                    
         BZ    OWS8                                                             
         L     R1,WTCOST                                                        
         BAS   RE,EDDOL            COST                                         
         A     R1,TCOST                                                         
         ST    R1,TCOST                                                         
         LA    R7,198(R7)                                                       
*                                                                               
OWS8     TM    RCPIND,RCPDEM                                                    
         BZ    OWS10                                                            
         L     R1,WTGDEM                                                        
         BAS   RE,EDDEM            GOAL DEMO                                    
         A     R1,TGDEM                                                         
         ST    R1,TGDEM                                                         
         LA    R7,198(R7)                                                       
*                                                                               
OWS10    TM    RCPIND,RCPDOL                                                    
         BZ    OWS18                                                            
         L     R1,WTGDOL                                                        
         BAS   RE,EDDOL            GOAL DOLLARS                                 
         A     R1,TGDOL                                                         
         ST    R1,TGDOL                                                         
         B     OWS18                                                            
*                                                                               
OWS12    TM    RCPIND,RCPDOL                                                    
         BZ    OWS14                                                            
         L     R1,WTCOST           PRINT COST                                   
         BAS   RE,EDDOL                                                         
         A     R1,TCOST                                                         
         ST    R1,TCOST                                                         
         LA    R7,198(R7)                                                       
*                                                                               
OWS14    TM    RCPIND,RCPDEM                                                    
         BZ    OWS18                                                            
         LA    R0,MAXDEMS          PRINT DEMOS                                  
         STM   R4,R5,DUB                                                        
         LA    R4,WTDEMS                                                        
         LA    R5,TDEMS                                                         
*                                                                               
OWS16    L     R1,0(R4)                                                         
         BAS   RE,EDDEM                                                         
         A     R1,0(R5)            ACCUMULATE DEMO TOTALS                       
         ST    R1,0(R5)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         LA    R7,198(R7)                                                       
         BCT   R0,OWS16                                                         
         LM    R4,R5,DUB                                                        
*                                                                               
OWS18    LA    R3,7(R3)            ADVANCE TO NEXT PRINT POSITION               
         BCT   R2,OWS20                                                         
         B     OWS22                                                            
*                                                                               
OWS20    LA    R5,WKTOTL(R5)       NEXT WEEK                                    
         LA    R6,5(R6)                                                         
         C     R5,FULL             TEST END OF TABLE                            
         BL    OWS4                                                             
         B     OWS26                                                            
*                                                                               
OWS22    LA    R5,WKTOTL(R5)       NEXT WEEK                                    
         LA    R6,5(R6)                                                         
         CLI   BYTE,1              TEST JUST PRINTED 16TH WEEK                  
         BNE   *+16                                                             
         ST    R5,ANXTNTRY         YES-SAVE TABLE POSITIONS                     
         ST    R6,ANXTWK                                                        
         B     OWS32                   AND EXIT                                 
         MVI   BYTE,1              NO-SEE IF ANY WEEKS WITH DATA LEFT           
         LA    R2,1                                                             
*                                                                               
OWS24    OC    0(WKTOTL,R5),0(R5)                                               
         BNZ   OWS4                YES-PRINT 16TH WEEK                          
         LA    R5,WKTOTL(R5)                                                    
         LA    R6,5(R6)                                                         
         C     R5,FULL                                                          
         BL    OWS24                                                            
*                                                                               
OWS26    NI    GLINDS3,255-GLLSTLIN  TELL DRIVER WE'RE FINISHED                 
         MVC   1(5,R3),=C'TOTAL'   PRINT COST AND DEMO TOTALS                   
         LA    R7,198+198(R3)                                                   
         CLI   RCPOPT,3            TEST GOALS ARE IN THE STACK                  
         BNE   OWS28                                                            
         TM    RCPIND,RCPDEM       YES-                                         
         BZ    *+16                                                             
         L     R1,TDEMS                                                         
         BAS   RE,EDDEM            DEMO                                         
         LA    R7,198(R7)                                                       
         TM    RCPIND,RCPDOL                                                    
         BZ    *+16                                                             
         L     R1,TCOST                                                         
         BAS   RE,EDDOL            COST                                         
         LA    R7,198(R7)                                                       
         TM    RCPIND,RCPDEM                                                    
         BZ    *+16                                                             
         L     R1,TGDEM                                                         
         BAS   RE,EDDEM            GOAL DEMO                                    
         LA    R7,198(R7)                                                       
         TM    RCPIND,RCPDOL                                                    
         BZ    OWS32                                                            
         L     R1,TGDOL                                                         
         BAS   RE,EDDOL            GOAL DOLLARS                                 
         B     OWS32                                                            
*                                                                               
OWS28    TM    RCPIND,RCPDOL                                                    
         BZ    *+16                                                             
         L     R1,TCOST                                                         
         BAS   RE,EDDOL                                                         
         LA    R7,198(R7)                                                       
         TM    RCPIND,RCPDEM                                                    
         BZ    OWS32                                                            
         LA    R0,MAXDEMS                                                       
         LA    R5,TDEMS                                                         
*                                                                               
OWS30    L     R1,0(R5)                                                         
         BAS   RE,EDDEM                                                         
         LA    R5,4(R5)                                                         
         LA    R7,198(R7)                                                       
         BCT   R0,OWS30                                                         
*                                                                               
OWS32    L     R3,GLAOFLD                                                       
         CLC   0(119,R3),BLANKS    TEST ANY DATA FORMATTED                      
         BNH   OWSX                                                             
         MVI   198(R3),0           YES-PRINT THE CAPTIONS                       
         LA    R3,198+198(R3)                                                   
         CLI   RCPOPT,3                                                         
         BNE   OWS34                                                            
         MVC   0(7,R3),DEMNAMES                                                 
         MVC   198(7,R3),=C'DOLLARS'                                            
         MVC   198+198(9,R3),=C'GOAL DEMO'                                      
         MVC   198+198+198(6,R3),=C'GOAL $'                                     
         LA    R1,198+198+198+198(R3)                                           
         TM    RCPIND,RCPDEM                                                    
         BO    *+20                                                             
         MVC   0(7,R3),=C'DOLLARS'                                              
         MVC   198(7,R3),198+198+198(R3)                                        
         B     *+18                                                             
         TM    RCPIND,RCPDOL                                                    
         BO    OWS36                                                            
         MVC   198(9,R3),=C'GOAL DEMO'                                          
         LA    R1,198+198(R3)                                                   
         MVC   0(9,R1),BLANKS                                                   
         MVC   198(6,R1),BLANKS                                                 
         B     OWS36                                                            
*                                                                               
OWS34    LR    R1,R3                                                            
         TM    RCPIND,RCPDOL                                                    
         BZ    *+14                                                             
         MVC   0(4,R1),=C'COST'                                                 
         LA    R1,198(R1)                                                       
         TM    RCPIND,RCPDEM                                                    
         BZ    OWS36                                                            
         LA    R0,MAXDEMS                                                       
         LA    RE,DEMNAMES                                                      
         MVC   0(7,R1),0(RE)                                                    
         LA    R1,198(R1)                                                       
         LA    RE,7(RE)                                                         
         BCT   R0,*-14                                                          
*                                                                               
OWS36    CLI   FMTWKS,C'D'         TEST MORE TO COME                            
         BE    *+12                                                             
         TM    GLINDS3,GLLSTLIN                                                 
         BZ    OWSX                                                             
         MVI   0(R1),0             YES-LEAVE A SPACING LINE                     
*                                                                               
OWSX     J     XIT                                                              
         SPACE 2                                                                
EDDOL    ST    RE,SAVERE           EDIT DOLLARS                                 
         ST    R1,EDVAL                                                         
         MVI   CURTAB+3,2                                                       
         CURED EDVAL,(6,(R7)),CURTAB,DECS=ROUND                                 
         L     R1,EDVAL                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
EDDEM    ST    RE,SAVERE           EDIT DEMO VALUE                              
         ST    R1,EDVAL                                                         
         MVI   CURTAB+3,1                                                       
         CURED EDVAL,(6,(R7)),CURTAB,DECS=ROUND                                 
         L     R1,EDVAL                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET BUYER/BILLER NAMES                                             
* INPUT :  SVNAME=CLT(2)/PRD(1)/EST(1)/MKT(2)                                   
***********************************************************************         
         SPACE 1                                                                
GETNAME  DS    0H                                                               
         MVC   SBBYRNM,ASTERS      INIT TO STARS                                
         MVC   SBBLRNM,ASTERS                                                   
         XC    KEY,KEY             BUILD STATUS RECORD KEY                      
         LA    R2,KEY                                                           
         USING STATD,R2                                                         
         MVC   STKTYPE,=X'0D71'                                                 
         MVC   STKAGMD,SBBAGYMD                                                 
         MVC   STKCLT,SVNAME                                                    
         MVC   STKPRD,SVNAME+2                                                  
         MVC   STKEST,SVNAME+3                                                  
         MVC   STKMKT,SVNAME+4                                                  
         CLI   SBSTPROF+0,C'P'     TEST DATA BY PRODUCT                         
         BE    *+8                                                              
         MVI   STKPRD,0                                                         
         CLI   SBSTPROF+2,C'E'     TEST DATA BY ESTIMATE                        
         BE    *+8                                                              
         MVI   STKEST,0                                                         
*                                                                               
GETNAM2  DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST RECORD FOUND                            
         BE    GETNAM4             YES                                          
         MVC   KEY,KEYSAVE                                                      
*                                                                               
GETNAM3  CLI   STKPRD,0            NO-TEST NEED TO TRY PRD=POL                  
         BE    GETNAMX                                                          
         CLI   STKPRD,X'FF'                                                     
         BE    GETNAMX                                                          
         MVI   STKPRD,X'FF'        YES-TRY PRD=POL                              
         B     GETNAM2                                                          
*                                                                               
GETNAM4  L     R2,AIO2                                                          
         ST    R2,AIO                                                           
         GOTO1 GETREC              READ STATUS RECORD                           
         CLC   SBBYRNM,ASTERS                                                   
         BNE   GETNAM5                                                          
         CLC   BPBUYER,BLNKS                                                    
         BNH   GETNAM5                                                          
         MVC   SBBYRNM,BPBUYER     BUYER NAME                                   
*                                                                               
GETNAM5  CLC   SBBLRNM,ASTERS                                                   
         BNE   GETNAM6                                                          
         CLC   BPPAYER,BLNKS                                                    
         BNH   GETNAM6                                                          
         MVC   SBBLRNM,BPPAYER     BILLER NAME                                  
*                                                                               
GETNAM6  CLC   SBBYRNM,ASTERS      TEST BUYER OR BILLER NAME                    
         BE    *+14                STILL NOT SET                                
         CLC   SBBLRNM,ASTERS                                                   
         BNE   GETNAMX                                                          
         LA    R2,KEY              YES-TRY PRODUCT=POL                          
         B     GETNAM3                                                          
*                                                                               
GETNAMX  J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DRIVER HEADHOOK                                                     *         
***********************************************************************         
         SPACE 1                                                                
HEAD     DS    0H                                                               
         L     R1,AH4                                                           
         LA    R1,1(R1)                                                         
         LR    RE,R1                                                            
         A     RE,PWIDTH                                                        
         CLI   0(RE),C' '          TEST WHETHER HEADS'VE BEEN FORMATTED         
         BH    HD2                 YES                                          
         MVC   0(50,R1),BLANKS     NO-REMOVE MEDIA FROM FIRST HEADLINE          
         B     HDX                    AND EXIT                                  
*                                                                               
HD2      CLI   PRDSUM,C'Y'         TEST PRODUCT RECAP OR SUMMARY                
         BNE   HD4                                                              
         LA    R2,5                                                             
         STH   R2,HALF                                                          
         CLC   GLRECNO,RECRCPHI                                                 
         BE    HD2A                                                             
         CLC   GLRECNO,RECSUMHI                                                 
         BNE   HD4                                                              
         LA    R2,6                                                             
         STH   R2,HALF                                                          
*                                                                               
HD2A     LR    R0,R2               YES-                                         
         LA    R3,54                                                            
         BAS   RE,HDPOS                                                         
         MVC   0(19,RF),=C'* PRODUCT SUMMARY *'                                 
         LR    R2,R0                                                            
         LA    R2,2(R2)                                                         
         STH   R2,HALF                                                          
         LA    R3,59                                                            
         CLI   SBQMKTWT,C'N'                                                    
         BE    *+8                                                              
         LA    R3,51                                                            
         BAS   RE,HDPOS            PRINT NUMBER OF MARKETS                      
         MVC   0(8,RF),=C'MARKETS='                                             
         L     R5,NMKTS                                                         
         LA    RF,8(RF)                                                         
         EDIT  (R5),(3,(RF)),ALIGN=LEFT                                         
         CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    HD3                                                              
         AR    RF,R0               YES-PRINT THE COVERAGE                       
         MVC   0(10,RF),=C',COVERAGE='                                          
         L     R5,TOTMKTWT                                                      
         EDIT  (R5),(6,10(RF)),2,ALIGN=LEFT                                     
*                                                                               
HD3      CLC   GLRECNO,RECSUMHI    TEST PRODUCT SUMMARY                         
         BNE   HD20                                                             
         LA    R2,5                YES-PRINT FURTHER HEADLINE                   
         B     HD9                                                              
*                                                                               
HD4      L     R5,=A(HEADTAB)      FORMAT MKTGRPS, MKT AND STA                  
         USING HEADTABD,R5                                                      
*                                                                               
HD5      CLI   0(R5),FF                                                         
         BE    HD20                                                             
         CLC   HDNMGR,GLOPTS+1     N'MARKET GROUPS                              
         BE    *+12                                                             
         LA    R5,HEADTABL(R5)                                                  
         B     HD5                                                              
         CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    *+8                                                              
         AH    R5,=Y(HDWMGR1-HDMGR1)    YES                                     
         SR    R2,R2                                                            
         ICM   R2,1,HDMGR1                                                      
         BZ    HD6                                                              
         LA    R3,48                                                            
         BAS   RE,HDPOS                                                         
         MVC   0(L'MGR1HEAD,RF),MGR1HEAD                                        
         ICM   R2,1,HDMGR2                                                      
         BZ    HD6                                                              
         BAS   RE,HDPOS                                                         
         MVC   0(L'MGR2HEAD,RF),MGR2HEAD                                        
         ICM   R2,1,HDMGR3                                                      
         BZ    HD6                                                              
         BAS   RE,HDPOS                                                         
         MVC   0(L'MGR3HEAD,RF),MGR3HEAD                                        
*                                                                               
HD6      IC    R2,HDMKT            MARKET                                       
         LA    R3,53                                                            
         CLI   SBQSPILL,C'S'       TEST SPILL SEPARATE                          
         BNE   *+8                                                              
         LA    R3,50               YES-LEAVE ROOM FOR ORIG/SPILL                
         BAS   RE,HDPOS                                                         
         MVC   0(L'MKTHEAD,RF),MKTHEAD                                          
         CLI   SBQMKTWT,C'N'       TEST MARKET WEIGHTING                        
         BE    HD8                                                              
         IC    R2,HDMKT            YES-PRINT COVERAGE ON LINE BELOW             
         LA    R2,1(R2)                                                         
         LA    R3,58                                                            
         BAS   RE,HDPOS                                                         
         MVC   0(L'CVGHEAD,RF),CVGHEAD                                          
*                                                                               
HD8      IC    R2,HDSTA                                                         
         CH    R2,HALF                                                          
         BNH   *+8                                                              
         STH   R2,HALF                                                          
*                                                                               
HD9      CLC   GLRECNO,RECSUMLO    TEST MARKET SUMMARY                          
         BL    HD12                                                             
         CLC   GLRECNO,RECSUMHI                                                 
         BH    HD12                                                             
         CLI   SUMMY,3             YES-                                         
         BE    HD14                                                             
         CLI   SUMMY,1             TEST BRAND PERFORMANCE                       
         BNE   HD10                                                             
         LA    R3,49                                                            
         BAS   RE,HDPOS                                                         
         MVC   0(29,RF),=C'* BRAND PERFORMANCE SUMMARY *'                       
         B     HD20                                                             
*                                                                               
HD10     CLI   SUMMY,2             TEST BRAND WEEKLY                            
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,52                                                            
         BAS   RE,HDPOS                                                         
         MVC   0(24,RF),=C'* BRAND WEEKLY SUMMARY *'                            
         B     HD20                                                             
*                                                                               
HD12     CLC   GLRECNO,RECRCPLO    TEST MARKET RECAP                            
         BNE   HD14                                                             
         CLI   RECAP,C'M'                                                       
         BNE   HD16                                                             
         LA    R3,59               YES-                                         
         BAS   RE,HDPOS                                                         
         MVC   0(9,RF),=C'* TOTAL *'                                            
         B     HD20                                                             
*                                                                               
HD14     CLI   RECMPRLO,0          TEST MARKET PERFORMANCE REPORT               
         BE    HD16                                                             
         CLC   GLRECNO,RECMPRLO                                                 
         BL    HD16                                                             
         CLC   GLRECNO,RECMPRHI                                                 
         BH    HD16                                                             
         LA    R3,53               YES-PRINT TARGET DEMO                        
         BAS   RE,HDPOS                                                         
         MVC   0(L'TGTHED),TGTHED                                               
         B     HD20                                                             
*                                                                               
HD16     LA    R3,33               NO-FORMAT STATION TO HEADLINE                
         BAS   RE,HDPOS                                                         
         MVC   0(L'STAHEAD,RF),STAHEAD                                          
*                                                                               
HD20     CLI   SBQGETNM,C'Y'       PUT BUYER NAME                               
         BNE   HD30                                                             
         LH    R2,HALF             PRINT ROW                                    
         LA    R3,96               COL TO PRINT                                 
         BAS   RE,HDPOS                                                         
         MVC   0(8,RF),=C'BUYER - '                                             
         MVC   8(L'SBBYRNM,RF),SBBYRNM                                          
*                                                                               
HD30     CLI   SBSPPROF+12,C'Y'    EXCLUDE TAX ON MEDIA REPORTS                 
         BNE   HDX                                                              
         LH    R2,HALF             PRINT ROW                                    
         LA    R2,1(R2)                                                         
         LA    R3,1                                                             
         BAS   RE,HDPOS                                                         
         MVC   0(18,RF),=C'***TAX EXCLUDED***'                                  
*                                                                               
HDX      J     XIT                                                              
         SPACE 2                                                                
HDPOS    L     R1,AH4                                                           
         SH    R2,=H'4'                                                         
         BNP   *+12                                                             
         A     R1,PWIDTH                                                        
         BCT   R2,*-4                                                           
         LA    RF,0(R3,R1)                                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS AND MESSAGES                                            *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
MYCURS   MVI   ERROR,X'FE'                                                      
CURS     GOTO1 CURSERR                                                          
         EJECT                                                                  
***********************************************************************         
* STORAGE                                                             *         
***********************************************************************         
         SPACE 1                                                                
T20416W  DS    0D                                                               
*                                                                               
CURTAB   DC    CL8'00000000215B4040'                                            
ASTERS   DC    CL12'************'                                               
BLNKS    DC    CL32' '                                                          
*                                                                               
BUYHEDL  EQU   47                                                               
BUYHED1  DC    CL41'EST-LIN  BUY PERIOD  WKS DAY    N/W TIME'                   
BUYHED2  DS    0CL47                                                            
         DC    CL30'  BOOK   PROGRAMMING      PT  '                             
BH2COST  DC    CL4'COST'                                                        
         DC    CL7' '                                                           
         DC    CL6'DPTLEN'                                                      
*                                                                               
TGTHED   DS    0CL22                                                            
         DC    C'*** TARGET '                                                   
THTGT    DS    CL7                                                              
THREST   DC    C' ***'                                                          
*                                                                               
XFF      DC    16X'FF'                                                          
DASHES   DC    64C'-'                                                           
BLANKS   DC    132C' '                                                          
*                                                                               
*                                  GLOBAL OPTION EQUATES                        
PERTYP   EQU   GLOPTS+2            PERIOD TYPE                                  
PERGRD   EQU   C'G'                WEEKLY GRID                                  
PERWKS   EQU   C'W'                WEEKS                                        
PERMTH   EQU   C'M'                MONTHS                                       
PERMTHL  EQU   C'N'                MONTHS BY SPOT LENGTH                        
PERTOT   EQU   C'T'                TOTAL PERIOD                                 
PERTOTL  EQU   C'U'                TOTAL PERIOD BY SPOT LENGTH                  
PERSUM   EQU   C'S'                MONTHS BY QUARTER                            
QTRNUM   EQU   GLOPTS+3            CURRENT QUARTER NUMBER                       
*                                                                               
FF       EQU   X'FF'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
GRIDD    DSECT                     DSECT TO COVER GRID ENTRY                    
GRCOST   DS    XL8                                                              
GRSPTS   DS    XL4                                                              
GRSPW    DS    14XL4                                                            
GRDEMS   DS    (MAXDEMS)XL8                                                     
GRIDL    EQU   *-GRIDD                                                          
*                                                                               
MONTOTD  DSECT                     DSECT TO COVER MONTH TOTALS ENTRY            
MTCOST   DS    XL8                                                              
MTSPTS   DS    XL4                                                              
MTDEMS   DS    (MAXDEMS)XL8                                                     
MONTOTL  EQU   *-MONTOTD                                                        
*                                                                               
RECTOTD  DSECT                     DSECT TO COVER RECORD TOTALS ENTRY           
RECCOST  DS    XL8                                                              
RECSPTS  DS    XL4                                                              
RECDEMS  DS    (MAXDEMS)XL8                                                     
RECSLN   DS    XL1                                                              
RECTOTL  EQU   *-RECTOTD                                                        
*                                                                               
WKTOTD   DSECT                     DSECT TO COVER WEEK TOTALS ENTRY             
WTCOST   DS    XL4                                                              
WTDEMS   DS    (MAXDEMS)XL4                                                     
         ORG   WTDEMS+4                                                         
WTGDOL   DS    XL4                                                              
WTGDEM   DS    XL4                                                              
         ORG                                                                    
WKTOTL   EQU   *-WKTOTD                                                         
         SPACE 1                                                                
T20416   CSECT                                                                  
         DS    0F                                                               
         EJECT                                                                  
***********************************************************************         
* HEADLINE POSITION TABLE                                             *         
***********************************************************************         
         SPACE 1                                                                
HEADTAB  DC    X'00',X'0000000508',X'0000000508'                                
         DC    X'01',X'0500000608',X'0500000608'                                
         DC    X'02',X'0506000708',X'0506000709'                                
         DC    X'03',X'0506070809',X'0405060709'                                
         DC    X'FF'                                                            
         SPACE 1                                                                
HEADTABD DSECT                                                                  
HDNMGR   DS    X                   N'MARKET GROUPS                              
HDMGR1   DS    X                   HEADLINE FOR MARKET GROUP 1                  
HDMGR2   DS    X                   HEADLINE FOR MARKET GROUP 2                  
HDMGR3   DS    X                   HEADLINE FOR MARKET GROUP 3                  
HDMKT    DS    X                   HEADLINE FOR MARKET                          
HDSTA    DS    X                   HEADLINE FOR STATION                         
HDWMGR1  DS    X                   HEADLINE FOR MGR1 WITH MKT WEIGHTING         
HDWMGR2  DS    X                   HEADLINE FOR MGR2 WITH MKT WEIGHTING         
HDWMGR3  DS    X                   HEADLINE FOR MGR3 WITH MKT WEIGHTING         
HDWMKT   DS    X                   HEADLINE FOR MKT WITH MKT WEIGHTING          
HDWSTA   DS    X                   HEADLINE FOR STA WITH MKT WEIGHTING          
HEADTABL EQU   *-HEADTABD                                                       
         SPACE 1                                                                
         EJECT                                                                  
WORKD    DSECT                                                                  
*                                                                               
AXTRA    DS    0F               ** EXTENTION ROUTINE ADDRESSES **               
ASETWKS  DS    A                                                                
AGETDPTB DS    A                                                                
AGETEST  DS    A                                                                
AGETMGR  DS    A                                                                
AGETREP  DS    A                                                                
AOSTA    DS    A                                                                
AIBUY    DS    A                                                                
AOBUY    DS    A                                                                
AOWSTACK DS    A                                                                
AHEAD    DS    A                                                                
AGETNAME DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
SAVERD   DS    A                                                                
RELO     DS    A                                                                
AGEND    DS    A                                                                
*                                                                               
ESTREC   DS    XL(ESTHDRLN)                                                     
*                                                                               
MYWORK   DS    CL16                                                             
*                                                                               
ATOTS    DS    A                                                                
ANXTNTRY DS    A                                                                
ANXTWK   DS    A                                                                
AWKTOT   DS    A                                                                
AWKTOTX  DS    A                                                                
AWKTOTDL DS    A                                                                
AWKTOTDX DS    A                                                                
ARECTOTS DS    A                                                                
ARECTOT1 DS    A                                                                
*                                                                               
SAVERE   DS    F                                                                
COST     DS    F                                                                
ECOST    DS    F                                                                
SPTS     DS    F                                                                
WKTOTDSP DS    F                                                                
TCOST    DS    F                                                                
TDEMS    DS    (MAXDEMS)F                                                       
TGDOL    DS    F                                                                
TGDEM    DS    F                                                                
EDVAL    DS    F                                                                
NDXGDEM  DS    F                                                                
NDXBDEM  DS    F                                                                
NDXGDOL  DS    F                                                                
NDXBDOL  DS    F                                                                
NMKTS    DS    F                                                                
TOTMKTWT DS    F                                                                
GECOMSEQ DS    H                                                                
GEORBSEQ DS    H                                                                
GENELCOM EQU   X'01'                                                            
GENELORB EQU   X'02'                                                            
*                                                                               
RERATE   DS    CL1                 RERATE OPTION - P,R,A                        
CMTOPT   DS    XL1                 COMMENT CONTROL - 0,1,2                      
RECAP    DS    CL1                 RECAP OPTION  - 0,M,S                        
RCPOPT   DS    XL1                 RECAP TOTALING OPTION - 1,2,3,4              
SUMMY    DS    XL1                 SUMMARY OPTION - 0,1,2,3                     
SUMDPT   DS    XL1                 SUMMARY DPT DETAIL CONTROL - 1,2,3           
SPILL    DS    CL1                                                              
FMTWKS   DS    CL1                                                              
NUMWKS   DS    XL1                                                              
SUMSEQ   DS    XL1                                                              
MPRPRD   DS    CL1                                                              
DPLTOT   DS    CL1                                                              
PRDSUM   DS    CL1                                                              
RECRCPLO DS    XL1                                                              
RECRCPHI DS    XL1                                                              
RECSUMLO DS    XL1                                                              
RECSUMHI DS    XL1                                                              
RECMPRLO DS    X                                                                
RECMPRHI DS    X                                                                
*                                                                               
RTOTIND  DS    XL1                 RECORD TOTAL INDICATORS                      
RTRCP    EQU   X'80'               RECORD TOTALS FOR RECAPS                     
RTSUM    EQU   X'40'               RECORD TOTALS FOR SUMMARIES                  
RTRCPL   EQU   X'20'               RECORD TOTS BY SPOT LEN FOR RECAPS           
RTSUML   EQU   X'10'               RECORD TOTS BY SPOT LEN FOR SUMMYS           
RTDET    EQU   X'08'               RECORD TOTALS FOR DETAIL REPORT              
RTTOT    EQU   RTRCP+RTSUM+RTDET                                                
RTLENS   EQU   RTRCPL+RTSUML                                                    
*                                                                               
MTOTIND  DS    XL1                 MONTHLY TOTAL INDICATORS                     
MTRCP    EQU   X'80'               MONTHLY TOTALS FOR RECAPS                    
MTSUM    EQU   X'40'               MONTHLY TOTALS FOR SUMMARIES                 
MTRCPL   EQU   X'20'               MONTHLY TOTALS BY SLN FOR RECAPS             
MTSUML   EQU   X'10'               MONTHLY TOTALS BY SLN FOR SUMMARIES          
MTTOT    EQU   MTRCP+MTSUM                                                      
MTLENS   EQU   MTRCPL+MTSUML                                                    
*                                                                               
WTOTIND  DS    XL1                 WEEKLY TOTAL INDICATORS                      
WTRCP    EQU   X'80'               WEEKLY TOTALS FOR RECAPS                     
WTSUM    EQU   X'40'               WEEKLY TOTALS FOR SUMMARIES                  
*                                                                               
DETIND   DS    XL1                 DETAIL CONTROL                               
DETDEM   EQU   X'80'                                                            
DETOVR   EQU   X'40'                                                            
DETDOL   EQU   X'20'                                                            
DETCPP   EQU   X'10'                                                            
DETREP   EQU   X'08'                                                            
DETMKG   EQU   X'04'                                                            
DETAFF   EQU   X'02'                                                            
DETSRP   EQU   X'01'                                                            
DETDFLT  EQU   X'FE'                                                            
*                                                                               
RCPIND   DS    XL1                 RECAP CONTROL                                
RCPSPT   EQU   X'80'                                                            
RCPDOL   EQU   X'40'                                                            
RCPDEM   EQU   X'20'                                                            
RCPCPP   EQU   X'10'                                                            
RCPDPT   EQU   X'08'                                                            
RCPDFLT  EQU   RCPSPT+RCPDOL+RCPDEM                                             
*                                                                               
SUMIND   DS    XL1                 SUMMARY CONTROL                              
SUMDEM   EQU   X'80'                                                            
SUMWKY   EQU   X'40'                                                            
SUMMTH   EQU   X'20'                                                            
SUMBRD   EQU   X'10'                                                            
SUMCPP   EQU   X'08'                                                            
SUMLEN   EQU   X'04'                                                            
SUMDFLT  EQU   X'F0'                                                            
*                                                                               
GOALIND  DS    CL1                 GOAL PERIOD INDICATOR                        
GIWKS    EQU   C'W'                                                             
GIMNTHS  EQU   C'M'                                                             
GITOTAL  EQU   C'T'                                                             
*                                                                               
SVNAME   DS    XL6                                                              
SVMGR1   DS    XL2                                                              
SVMGR2   DS    XL2                                                              
SVMGR3   DS    XL2                                                              
SVSGR1   DS    XL2                                                              
SVSGR2   DS    XL2                                                              
SVBMKT   DS    XL3                                                              
SVSTA    DS    CL5                                                              
SVSTABIG DS    CL7                                                              
SVREP    DS    CL3                                                              
SVPRDS   DS    XL2                                                              
SVBPRD   DS    X                                                                
SVBEST   DS    X                                                                
SVPRD    DS    CL3                                                              
SVPRD2   DS    CL3                                                              
D2BPRD   DS    XL1                                                              
SVDPTLEN DS    XL(L'SBDPTLEN)                                                   
SVDPT    DS    XL(L'SBDPT)                                                      
SVMKTLEV DS    XL1                                                              
SVININD  DS    XL1                                                              
SVDPTMEN DS    XL1                                                              
SVBUYKEY DS    XL13                                                             
*                                                                               
MONDATES DS    12CL10              FORMATTED MONTH DATES                        
QTRDATES DS    4CL17               FORMATTED QTR DATES                          
PERDATES DS    CL17                FORMATTED PERIOD DATES                       
*                                                                               
SGR1HEAD DS    CL43                                                             
SGR2HEAD DS    CL43                                                             
*                                                                               
MGR1HEAD DS    CL44                                                             
MGR2HEAD DS    CL44                                                             
MGR3HEAD DS    CL44                                                             
MKTHEAD  DS    CL44                                                             
CVGHEAD  DS    CL16                                                             
STAHEAD  DS    CL63                                                             
*                                                                               
FSTPASS  DS    CL1                                                              
DEMVAL   DS    XL4                                                              
MONTH    DS    XL4                                                              
DEMONAME DS    CL7                                                              
*                                                                               
DEMVALS  DS    XL(MAXDEMS*4)                                                    
PURVALS  DS    XL(MAXDEMS*4)                                                    
*                                                                               
BUYDET   DS    0XL65               BUY DETAILS                                  
BYEST    DS    XL1                                                              
BYLINE1  DS    XL1                                                              
BYLINE2  DS    XL1                                                              
BYSTART  DS    XL3                                                              
BYEND    DS    XL3                                                              
BYWKS    DS    XL1                                                              
BYWKIND  DS    CL1                                                              
BYDAY    DS    XL1                                                              
BYNOWK   DS    XL1                                                              
BYTIME   DS    XL4                                                              
BYBOOK   DS    XL2                                                              
BYPROG   DS    XL17                                                             
BYPROGT  DS    CL1                                                              
BYCOST   DS    XL4                                                              
BYDPT    DS    CL1                                                              
BYLEN    DS    XL1                                                              
BYSTAT2  DS    XL1                                                              
BYSEDAY  DS    XL1                                                              
BYPKGIND DS    XL1                                                              
BYREP    DS    XL2                                                              
BYMGDATE DS    XL2                                                              
BYPBPRD  DS    CL3                                                              
BYPBTIME DS    XL1                                                              
BYPBEST  DS    XL1                                                              
BYXRATE  DS    XL2                                                              
BYXC58   DS    XL2                                                              
BYCOM1   DS    XL2                                                              
BYCOM2   DS    XL2                                                              
BYORB    DS    XL2                                                              
BUYDETL  EQU   *-BUYDET                                                         
*                                                                               
WKS      DS    56CL5               56 X 5-BYTE WEEKS                            
WKSQ1    DS    CL70                14 X 5-BYTE WEEKS                            
WKSQ2    DS    CL70                                                             
WKSQ3    DS    CL70                                                             
WKSQ4    DS    CL70                                                             
WKDSPLS  DS    XL(56*3)            56 WEEKS - QTRNUM/WKNUM/MONNUM               
*                                                                               
GRID     DS    (MAXQTRS)XL(GRIDL)  WEEKLY TOTALS                                
MAXQTRS  EQU   4                                                                
MAXDEMS  EQU   4                                                                
MAXLENS  EQU   3                                                                
*                                                                               
MONTOTS  DS    (MAXMONS)XL(MONTOTL)   MONTHLY TOTALS, ALL SPOT LENGTHS          
MAXMONS  EQU   13                                                               
*                                                                               
MONTSLN1 DS    XL1                                                              
MONTOTS1 DS    (MAXMONS)XL(MONTOTL)   MONTHLY TOTALS, SPOT LENGTH 1             
MONTSLN2 DS    XL1                                                              
MONTOTS2 DS    (MAXMONS)XL(MONTOTL)   MONTHLY TOTALS, SPOT LENGTH 2             
MONTSLN3 DS    XL1                                                              
MONTOTS3 DS    (MAXMONS)XL(MONTOTL)   MONTHLY TOTALS, SPOT LENGTH 3             
*                                                                               
RECTOTS  DS    XL(RECTOTL)         RECORD TOTALS, ALL SPOT LENGTHS              
RECTOTS1 DS    XL(RECTOTL)         RECORD TOTALS, SPOT LENGTH 1                 
RECTOTS2 DS    XL(RECTOTL)         RECORD TOTALS, SPOT LENGTH 2                 
RECTOTS3 DS    XL(RECTOTL)         RECORD TOTALS, SPOT LENGTH 3                 
*                                                                               
WKTOTSDL DS    (MAXWKS)XL(WKTOTL)  WEEK TOTALS FOR DPTLEN                       
WKTOTSDX EQU   *                                                                
WKTOTS   DS    (MAXWKS)XL(WKTOTL)  WEEK TOTALS                                  
WKTOTSX  EQU   *                                                                
MAXWKS   EQU   54                                                               
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*SPOTTABD                                                                       
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DDLANGEQUS                                                                     
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD2                                                                     
*SPGENSTAT                                                                      
*DEDBLOCK                                                                       
*SPGENAGY                                                                       
*SPGENEST                                                                       
*SPGENBUY                                                                       
*SPGENGOAL                                                                      
*SPGENREP                                                                       
*SPWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE SPOTTABD                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDLANGEQUS                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
       ++INCLUDE SPGENSTAT                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
**NOP    INCLUDE SPWRIECAD                                                      
       ++INCLUDE SPWRIECD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057SPWRI16   10/16/06'                                      
         END                                                                    
