*          DATA SET SPGETDEME  AT LEVEL 082 AS OF 03/04/21                      
*PHASE T00A20C                                                                  
*INCLUDE NSIWEEK                                                                
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-52654  03/01/21 TWO DEC RATINGS FOR CANADIAN RADIO        *         
* AKAT SPEC-31705  01/26/21 OPTIMIZE CANADIAN NEW POSTING METHODOLOGY *         
***********************************************************************         
*=====================================================================*         
*         SPGETDEME - SPOT DEMO LOOKUP - BUY REC INTERFACE            *         
*---------------------------------------------------------------------*         
* MOD LOG:                                                            *         
* --------                                                            *         
* 01MAR19   HWON     ADD NCM LOOKUP SUPPORT FOR STATION SV**D         *         
* 13APR18   MHER     MERGE CHANGES FROM S VERSION THAT FIXES          *         
*                    COMSCORE LOOKUPS WHEN NSI LOOKUP ERRORS OCCUR    *         
* 24JAN12   AKAT  -- DON'T CLOBBER ALPHA MKT IF X'24' ELEM HAS NONE   *         
* 03DEC08   AKAT  -- INCREASE AFDLIST                                 *         
* 27FEB08   AKAT  -- DON'T CALL STAPACK WITH MEDIA W OR O!            *         
* 08OCT07   EJOR  -- MAKE SURE NOT READING USER-ID LEVEL 00A PROFILE  *         
* 02FEB07   BPOO  -- REWRITE OVN/WEEKLY POSTING                       *         
* 23AUG06   AKAT  -- AFFID ON JAN30/06 SHOULDNT PULL OLYMPIC EXCLUSION*         
* 30OCT05   MHER  -- EXTRACT ALPHA MKT, RTGSVC, AND STATION FROM      *         
*                    DLUELEM AND NDELEM(FOR SPILL)                    *         
* 16OCT05   MHER  -- 0 FOR MARKET RTGSVC SUPPRESSES CABLE LOOKUPS     *         
* 28SEP05   MHER  -- EXPAND RADIO DEMO AREA TO MAXDEMS+3              *         
* 18MAY05   MHER  -- RETURN A(DBLOCK) IN FAC LIST (P5) AT +4          *         
* 15FEB05  (MHER) -- CHANGES FOR FUSION CABLE RATINGS                 *         
* 30AUG04  (MHER) -- PASS ALONG 2-DEC RATING REQUEST                  *         
* 06JUL04  (MHER) -- READ MARKET RECORD FOR USTV LPM DETERMINATION    *         
* 39MAR04  (EJOR) -- NEW TEST FOR CABLE                               *         
* 06JUL00  (BZEH) -- FIX STRATA UPLOAD Y2K BUG                        *         
* 20SEP99  (MHER) -- NTI CABLE DEMOS FOR SPOT TV (OFFLINE ONLY)       *         
* 30DEC97  (BZEH) -- CANADIAN MM MONTHLY SUPPORT                      *         
*=====================================================================*         
                                                                                
*=====================================================================*         
*                                                                     *         
* PARAMETER LIST AS FOLLOWS *                                         *         
*                                                                     *         
*  P1    ERROR BYTE  (NOTE 1)    A(BUYREC)                            *         
*  P2    SPILL CNTL  (NOTE 2)    A(DEMO ELEMENT) (NOTE 3)             *         
*  P3    SOURCE CNTL (NOTE 4)    A(HUTADJ BYTE) OR IF X'40' BIT IS ON *         
*        >>>>>>>> SPGETDEMD      A(GETDEMD) EXTENDED PARAMETER BLOCK  *         
*  P4                            A(1500 BYTE WORK AREA)               *         
*  P5    >>>>>>>> DEMFACSD       A(FACILITY LIST) (NOTE 5)            *         
*  P6                            A(64 BYTE AREA FOR TOTALS)           *         
*                                                                     *         
*  NOTE 1  -  ERROR CODES    X'40' = DISK ERROR                       *         
*                            X'41' = INVALID AGY/MEDIA/RTG SVC        *         
*                            X'42' = NO AFFIDAVIT ELEMENTS            *         
*                            X'43' = INVALID TIME                     *         
*                            X'45' = STATION/BOOK NOT ON FILE         *         
*                                                                     *         
*  NOTE 2  -  SPILL CONTROL  X'00' = NO SPILL                         *         
*                            X'01' = ORIGINATING + SPILL              *         
*                            X'02' = SPILL ONLY                       *         
*                                                                     *         
*  NOTE 3  -  DEMO ELEMENT   + 2 = ACTUAL BOOK (X'00' = LATEST)       *         
*                            + 4 = 14 ONE BYTE WTD DEMO WTS           *         
*                     (SPL)  + 4 = SPILL MARKET NUMBERS (AGY/RTGSVC)  *         
*                     (SPL)  + 8 = BOOK TYPE OVERRIDE                 *         
*                     (AFD)  +18 = PRODUCT CODE                       *         
*                     (AFD)  +19 = SPOT LENGTH                        *         
*                     (AFD)  +20 = REQUEST START DATE (2 BYTES)       *         
*                     (AFD)  +22 = REQUEST  END  DATE (2 BYTES)       *         
*                                                                     *         
*  NOTE 4  -  SOURCE CONTROL X'80' = CANADA                           *         
*                            X'01' = ARB  / X'00' = NSI               *         
*                            X'40' = P3 IS A(DEMLKUPD)                          
*                                                                     *         
*  NOTE 5  -  FACILITY LIST PARAMETERS  (SEE DEMFACSD)                *         
*                            + 0  X'80' = SPCL DEMO ADJ FACTORS       *         
*                                 X'40' = ACTUAL BOOK REQUEST                   
*                                 A(SP1W PROFILE)                     *         
*                            + 4  X'80' = A(NONT50EL) AT +24          *         
*                                 X'40' = COMSCORE DEMO IN REQ                  
*                                 X'20' = NSI DEMO IN REQ                       
*                                 ON EXIT A(SPGETDEM DBLOCK)          *         
*                            + 8  A(COMFACS)                          *         
*                            +12  A(DATAMGR)                          *         
*                            +16  A(CALLOV)                           *         
*                            +20  A(SPD0 PROFILE)                     *         
*                            +24  A(NONT 50EL)  SEE +4                          
***********************************************************************         
T00A20   TITLE 'SPGETDEMO - SPOT DEMO LOOK-UP BUY RECORD INTERFACE'             
GETDEMO  RSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,GETDEMO,RA,R9,CLEAR=YES,RR=R4                        
         USING WORKD,RC                                                         
*                                                                               
         ST    R4,RELO                                                          
         ST    R1,SAVER1                                                        
         ST    RD,SAVERD                                                        
*                                                                               
         OC    VGETIUN,VGETIUN                                                  
         BNZ   *+8                                                              
         BRAS  RE,SETADCON                                                      
*                                                                               
GD01     MVI   TAPEOPT,C'Y'        PATCHABLE TAPE PRECISION                     
         MVI   TAPEOPT,C'N'        NORMAL PRECISION IS BOOK                     
         MVI   WVPHOPT,C'N'        NORMAL OPT IS SWEEP                          
         MVI   OVPHOPT,C'N'        NORMAL OPT IS SWEEP                          
         MVI   AFFIDLK,C'N'                                                     
         XC    STNAMEOV,STNAMEOV                                                
         XC    ENDNAMEO,ENDNAMEO                                                
*                                                                               
         MVC   ADBUY,0(R1)                                                      
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
         TM    BDCIND,X'01'        TEST NEGATIVE RATE                           
         JO    EXIT                                                             
*                                                                               
         MVC   SVBSTART(6),BDSTART                                              
*                                                                               
         MVC   ADDEMEL,4(R1)                                                    
         L     RF,ADDEMEL                                                       
         MVC   VPHSDAT(4),NDACTL-NDELEM(RF)                                     
         MVC   DBFILE,=C'TP '                                                   
*                                                                               
         MVI   DBSELSRC,C'N'                                                    
         TM    8(R1),X'01'         TEST NSI                                     
         BZ    *+8                                                              
         MVI   DBSELSRC,C'A'                                                    
*                                                                               
         L     RE,16(R1)           GET A(FACILITIES LIST)                       
         USING DEMFACSD,RE                                                      
         MVI   COMREQ,C'N'                                                      
         MVI   NSIREQ,C'N'                                                      
*                                                                               
         TM    DEMFDBLK,DEMF_50ELQ  TEST A(50EL) PASSED FOR COMSCORE            
         JZ    *+12                IF NO, MUST BE AN NSI REQUEST                
         TM    DEMFDBLK,DEMF_NSIREQ TEST NEILSEN DEMOS                          
         JZ    *+8                                                              
         MVI   NSIREQ,C'Y'                                                      
*                                                                               
         TM    DEMFDBLK,DEMF_50ELQ  TEST A(50EL) PASSED FOR COMSCORE            
         JZ    GD01X                                                            
         MVC   AD50EL,DEMF50EL     IF YES, SAVE IT                              
         MVC   VCOMINTR,DEMFVCOM   AND A(COMINTER)                              
*                                                                               
         TM    DEMFDBLK,DEMF_COMREQ TEST COMSCORE DEMOS                         
         JZ    *+8                                                              
         MVI   COMREQ,C'Y'                                                      
*                                                                               
         MVC   COMSCSD,DEMFCOMSD    SAVE SURVEY DATES (B/S)                     
*                                                                               
         TM    DEMFSP1W,DEMF_ADJQ   TEST SPCL DEMO ADJ FACTORS                  
         BZ    *+10                                                             
         MVC   DBDAYPT,BDDAYPT     SET DAYPART CODE IN DBLOCK                   
*                                                                               
         TM    DEMFSP1W,DEMF_ACTQ   TEST ACTUAL BOOK REQUEST                    
         BZ    *+8                                                              
         MVI   ACTBOOK,C'Y'         REMEMBER THAT FACT                          
*                                                                               
GD01X    LA    RF,DBLOCK           RETURN MY DBLOCK ADDR TO CALLER              
         ST    RF,DEMFDBLK                                                      
*                                                                               
         L     RF,DEMFSP1W         GET ADDRESS OF 1W PROFILE                    
         MVC   SV1WPROF,0(RF)      AND SAVE PROFILE                             
*                                                                               
         L     RF,DEMFSPD0         GET A(D0 PROFILE)                            
         MVC   SVD0PROF,0(RF)      AND SAVE                                     
         CLI   SVD0PROF+15,C'W'                                                 
         BNE   *+8                                                              
         MVI   WVPHOPT,C'Y'                                                     
*                                                                               
         CLI   COMREQ,C'Y'                                                      
         JNE   *+16                                                             
         MVC   SVREQDTS,DEMFQSTR     SAVE REQ START/END DATES                   
         MVC   SVBUYDTS(6),DEMFBYSD  SAVE BUY START/END DATES                   
                                                                                
         MVI   SMMMONTH,C'N'                                                    
         CLI   SVD0PROF+11,C'M'    CHECK FOR OVERNIGHT POSTING                  
         BNE   *+12                M= NEW OVN OPTION                            
         MVI   OVPHOPT,C'Y'                                                     
         MVI   SMMMONTH,C'Y'       SET METERED POST TO MONTHLY                  
*                                                                               
         CLI   SVD0PROF+11,C'Y'    CHECK FOR OVERNIGHT POSTING                  
         BNE   *+8                                                              
         MVI   OVPHOPT,C'Y'                                                     
                                                                                
         MVI   LPMWKOPT,C'N'                                                    
         CLI   SVD0PROF+10,C'Y'    LPMWK OPTION                                 
         BNE   *+8                                                              
         MVI   LPMWKOPT,C'Y'                                                    
                                                                                
         CLI   DBSELMED,C'T'       USTV ALWAYS IMPRESSION BASED                 
         BE    *+8                                                              
         CLI   SV1WPROF+5,C'I'     CHECK FOR DMA IMP CALCS                      
         BNE   *+8                                                              
         MVI   TAPEOPT,C'Y'                                                     
*                                                                               
         XR    R0,R0                                                            
         IC    R0,0(R2)            GTFM                                         
         N     R0,=X'0000000F'                                                  
*                                                                               
         MVI   DBSELMED,C'R'       FORCE TO RADIO                               
         CHI   R0,2                TEST RADIO                                   
         BNE   GD02                                                             
         CLI   SV1WPROF+6,C'Y'     RADIO BUY/POST ACTIVE                        
         BE    *+8                  IT'S OK                                     
         MVI   DBSELMED,C'X'       ASSUME RADIO DISABLED                        
*                                                                               
GD02     L     RE,8(R1)                                                         
         USING GETDEMD,RE                                                       
*                                                                               
         MVC   CBLDEMOV,GTDMCBL    SAVE CABLE LOOKUP OVERRIDE                   
         MVC   ESTEOW,GTDMEOW                                                   
         TM    GTDMFLAG,X'04'      TEST SPOT POSTING                            
         BZ    *+8                                        *                     
         OI    DBVOPT,X'40'                                                     
*                                                                               
GD03     DS    0H                                                               
                                                                                
***********************************************************                     
* MAKE SURE THIS IS THE ORDER OF PRECEDENCE               *                     
* WEEKLY ALWAYS GETS PRIORITY AS THE MEDIA BECAUSE  WE    *                     
* NOW ALLOW WEEKLY AND OVERNIGHTS OPTIONS TO BE USED      *                     
* TOGETHER, ACTBKG ALWAYS DEFAULTS TO THE WEEKLY. CODE    *                     
* WAS BASED ON EITHER WEEKLY OR OVN NOT BOTH.             *                     
*                                                         *                     
* DONT TOUCH THIS UNLESS YOU KNOW WHAT YOU ARE DOING      *                     
* THERES REALLY NO REASON TO TOUCH THIS CHUNK OF CODE     *                     
* TOUCHING THIS WILL BREAK POSTING FOR OVERNIGHTS         *                     
*                                                                               
         CLI   WVPHOPT,C'Y'                               *                     
         BE    *+16                                       *                     
         TM    GTDMFLAG,X'20'      TEST OVERNITE LOOKUP   *                     
         BZ    *+8                                        *                     
         MVI   DBSELMED,C'O'                              *                     
*                                                         *                     
         TM    GTDMFLAG,X'10'      TEST WEEKLY LOOKUP     *                     
         BZ    *+8                                        *                     
         MVI   DBSELMED,C'W'                              *                     
***********************************************************                     
                                                                                
         MVI   TWODEC,0                                                         
         SR    RF,RF                                                            
         TM    GTDMFLAG,X'40'      TEST 2-DEC RTG REQUEST                       
         JZ    *+8                                                              
         LA    RF,X'40'                                                         
         TM    GTDMFLAG,X'01'      TEST 2-DEC IMP REQUEST                       
         JZ    *+8                                                              
         AHI   RF,X'01'                                                         
*                                                                               
         TM    GTDMFLAG,X'41'      TEST 2-DEC RAT OR 2-DEC IMP REQUEST          
         BZ    GD04                 NEITHER                                     
         TM    8(R1),X'80'         CANADIAN AGENCY?                             
         BNZ   *+12                YES - 2 DEC RATINGS NOW FOR RADIO            
*                                  EITHER/BOTH ALWAYS REQ 2-DEC RATINGS         
         CHI   R0,2                BUT NEVER FOR RADIO                          
         JE    GD04                                                             
         STC   RF,TWODEC           SAVE 2-DEC RTG/IMP FLAG                      
*                                                                               
         LA    RF,DBLOCKX                                                       
         USING DBXTTID,RF                                                       
         L     R0,DBEXTEND                                                      
         ST    RF,DBEXTEND         POINT TO EXTENSION                           
*                                                                               
         CR    R0,RF               MAKE SURE WE DON'T POINT TO OURSELF          
         JNE   *+6                                                              
         SR    R0,R0                                                            
*                                  ALWAYS ASK FOR 2-DEC RTG                     
         MVC   DBXTID,=C'SPOT'                                                  
         MVI   DBXTSCTL,C'2'       CHAR-YOU CAN ASK FOR 0-2DEC                  
         ST    R0,DBXTNEXT                                                      
         DROP  RF                                                               
*                                                                               
GD04     MVC   SAVEHUT,0(RE)       SAVE HUT CONTROL IF NOT EXT P3               
*                                                                               
         CLI   DBSELMED,C'O'       IF OVERNITE                                  
         BE    GD05                                                             
         CLI   DBSELMED,C'W'       OR WEEKLY                                    
         BE    GD05                                                             
*                                                                               
         TM    0(R2),X'01'         TEST NTWK OR SPOT TV                         
         BZ    GD05                 NO                                          
         MVI   DBSELMED,C'T'                                                    
*                                                                               
         TM    8(R1),X'80'         TEST CANADIAN                                
         BZ    GD05                                                             
         MVI   DBSELMED,C'C'                                                    
*                                                                               
GD05     TM    8(R1),X'40'         TEST EXTENDED P3                             
         BZ    GD05D                                                            
*                                                                               
         MVC   SAVEUID,GTDMUID                                                  
         MVC   SAVEHUT,GTDMHUT                                                  
         MVC   SAVENTI,GTDMNTI                                                  
*                                                                               
         MVC   DBSELALF,GTDMKALF                                                
         CLI   DBSELMED,C'C'                                                    
         BE    GD05A                                                            
         CLI   DBSELMED,C'R'                                                    
         BE    GD05A                                                            
         XC    DBSELALF,DBSELALF   NO ALPHA MKT FOR TV                          
*                                                                               
GD05A    CLI   GTDMSTA,C' '        TEST FOR OVERRIDE STATION                    
         BNH   GD05B                                                            
         MVC   DBSELSTA,GTDMSTA                                                 
         CLI   DBSELSTA+4,C'A'                                                  
         BNL   *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
         DROP  RE                                                               
*                                                                               
GD05B    CLI   SAVENTI,C' '        TEST NTI CABLE LOOKUP                        
         BNH   GD05D               NO                                           
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'C'                                                    
         MVC   DBSELSTA,SAVENTI                                                 
         MVI   DBSELSTA+4,C'C'                                                  
         CLC   DBSELSTA(4),=C'TEL '                                             
         BNE   GD05C                                                            
         MVI   DBSELSRC,C'H'                                                    
         MVI   DBSELSTA+4,C'H'                                                  
         B     GD05D                                                            
*                                                                               
GD05C    CLC   DBSELSTA(4),=C'WB  '                                             
         BNE   GD05D                                                            
         MVI   DBSELSRC,C'K'                                                    
         MVI   DBSELSTA+4,C'T'                                                  
         B     GD05D                                                            
*                                                                               
GD05D    MVC   DBAREC,12(R1)       SET I/O AREA ADDRESS FROM USER               
         MVI   DBFUNCT,DBGETDEM                                                 
         MVI   DBTPTT,C'T'         REQUEST TYPICAL TIME                         
         MVC   DBFRANCO,SV1WPR4    SET ANGLO/FRANCO OPTION                      
*                                                                               
*  CHECK TO SEE IF WE ARE LOOKING AT NCM CALL LETTERS                           
         CLC   DBSELSTA(2),=C'CM'  SET NCM SRCE FOR CM**D CALL LETTER           
         BE    *+10                                                             
         CLC   DBSELSTA(2),=C'SV'  SET NCM SRCE FOR SV**D CALL LETTER           
         BNE   *+8                                       -HWON 11/27/19         
         CLI   DBSELSTA+4,C'D'                                                  
         BNE   *+8                                                              
         MVI   DBSELSRC,C'C'       SET NCM SOURCE                               
*                                                                               
         L     R3,ADDEMEL          GET DEMO ELEM ADDRESS                        
NDEL     USING NDELEM,R3                                                        
*                                                                               
         MVC   DBSELBK,NDEL.NDBOOK                                              
         OC    DBSELBK,DBSELBK     TEST LATEST BOOK REQUEST                     
         BNZ   *+14                                                             
         MVC   DBSELDAT,BDEND      YES - SET LATEST BOOK LIMIT                  
         B     GD05E               LATEST IS OK                                 
* FIX STRATA UPLOAD Y2K BUG BY CONVERTING BOOK TO Y2K FORMAT                    
         MVC   FULL(2),DBSELBK                                                  
         MVI   FULL+2,1            SET VALID DAY                                
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(3,FULL),DUB  CONVERT TO EBCDIC                        
         GOTO1 (RF),(R1),DUB,(3,FULL)  THEN BACK TO BINARY                      
         MVC   DBSELBK,FULL                                                     
         L     R1,SAVER1                                                        
*                                                                               
GD05E    DS    0C                                                               
         TM    DBSELBK+1,X'80'     SCAN AM                                      
         BZ    *+8                                                              
         MVI   DBBTYPE,C'S'                                                     
         NI    DBSELBK+1,X'7F'                                                  
*                                                                               
         CLI   SV1WPR9,C'M'        CANADIAN METER MARKET CONTROL                
         BNE   *+14                                                             
         MVI   DBBEST,C'M'         DO MONTHLY POSTING                           
         MVC   DBUSEBBM,SV1WPR10   USE BBM METER MKTS                           
*                                                                               
         MVC   DBSELAGY,BUYREC+20                                               
*                                                                               
         CLI   4(R1),2             TEST SPILL-ONLY REQUEST                      
         BNE   GD1A                                                             
                                                                                
* SPILL REQUEST                                                                 
*                                                                               
         MVC   DBSELUMK,NDEL.NDAGYMKT    SET AGY MKT CODE                       
         MVC   DBSELMK,NDEL.NDRSMKT      SET RTG SVC SPILL MKT NUM              
         MVC   DBBTYPE,NDEL.NDBKTYPE                                            
         CLI   DBBTYPE,C'O'        KILL OLYMPICS FROM NDELEM                    
         BNE   *+8                                                              
         MVI   DBBTYPE,0                                                        
*                                                                               
         CLI   DBSELMED,C'C'       TEST CANADA                                  
         BNE   GD1A                                                             
         CLI   NDEL.NDRTGSVC,C'0'  TEST RATING SERVICE PRESENT                  
         BL    GD1A                NO - THEN NO OTHER OVERRIDES                 
         MVI   DBSELSRC,C'N'                                                    
         CLI   NDEL.NDRTGSVC,C'0'                                               
         BE    *+8                                                              
         MVI   DBSELSRC,C'A'                                                    
         MVC   DBSELALF,NDEL.NDMKTALF                                           
         CLI   NDEL.NDSTA,C'A'                                                  
         BL    *+10                                                             
         MVC   DBSELSTA,NDEL.NDSTA USE OVERRIDE STATION                         
         DROP  NDEL                                                             
*                                                                               
GD1A     DS    0H                  RADIO NBOOK LOOKUPS                          
         CLI   DBSELMED,C'R'                                                    
         BNE   GD1B                                                             
         TM    SAVEHUT,X'F0'       NBOOK INDICATOR IN HUT FIELD                 
         BNO   GD1B                                                             
         OC    DBSELBK,DBSELBK     LATEST-N  LEAVE BDEND AS START               
         BZ    *+10                                                             
         MVC   DBSELDAT,DBSELBK     SET TO START AT REQUESTED BOOK              
         MVI   DBSELBK,X'FF'        NBOOK FLAG                                  
         MVC   DBSELBK+1(1),SAVEHUT NBOOK NUMBER OF BOOKS                       
         MVI   SAVEHUT,0                                                        
*                                                                               
GD1B     GOTO1 VCLUNPK,DMCB,BUYREC+1,DBSELCLI                                   
                                                                                
         CLI   DBSELMED,C'C'       IF NOT CANADIAN MEDIA,                       
         BNE   STCANFGX             DON'T SET FLAG                              
                                                                                
         CLI   DBSELSRC,C'A'       ARBITRON / BBM                               
         BE    STCANFGA                                                         
         CLI   DBSELSRC,C'N'       NIELSEN                                      
         BE    STCANFGN                                                         
         B     STCANFGX            DON'T SET FLAG FOR OTHER SOURCES             
                                                                                
STCANFGA DS    0H                  ARBITRON / BBM                               
         B     STCANFGW             ALWAYS SET FLAG                             
                                                                                
STCANFGN DS    0H                  NIELSEN                                      
         OC    DBSELBK,DBSELBK                                                  
         BZ    STCANFGW             SET FLAG IF LATEST BOOK REQUESTED           
         CLC   DBSELBK,=X'6001'                                                 
         BNL   STCANFGW             SET FLAG IF BOOK >= 1ST WK OF 1996          
         B     STCANFGX            DON'T SET FLAG                               
                                                                                
STCANFGW DS    0H                  BRANCH HERE TO                               
         OI    FLAGS,CANHPT         SET CANADIAN HPT'S                          
STCANFGX EQU   *                                                                
                                                                                
*=====================================================*                         
* SEARCH FOR LOOK-UP OVERRIDE ELEMENT TO SET BOOKTYPE *                         
*=====================================================*                         
                                                                                
         L     RE,ADDEMEL                                                       
         CLI   0(RE),2             TEST ORIGINATING DEMOS                       
         BE    *+12                                                             
         CLI   0(RE),X'12'         I DONT'T KNOW WHY THIS HAPPENS               
         BNE   GD1H                BUT IF NOT 02 OR 12, IGNORE DLUEL            
         LA    R6,BDELEM                                                        
         MVI   ELCODE,X'24'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   GD1C                                                             
*                                                                               
DLUEL    USING DLUELEM,R6                                                       
*                                                                               
* DLU ELEMENT VALUES OVERRIDE RTGSVC/STATION/MARKET                             
*                                                                               
         CLI   DBSELMED,C'C'       TEST CANADIAN                                
         BNE   GD1BA                                                            
*                                                                               
         CLI   DLUEL.DLUBAMKT,C'A' HAVE ALPHA MARKET OVERRIDE?                  
         BL    *+10                NO - DON'T CLOBBER ALPHA MARKET!             
         MVC   DBSELALF,DLUEL.DLUBAMKT                                          
         MVC   SVCNFLGS,DLUEL.DLUBFLGS                                          
         CLI   DLUEL.DLUBSTOV,C'A'       TEST STATION OVERRIDE                  
         BL    *+10                                                             
         MVC   DBSELSTA,DLUEL.DLUBSTOV                                          
*                                                                               
         MVI   DBSELSRC,C'N'                                                    
         TM    SVCNFLGS,X'01'      TEST BBM                                     
         BZ    *+8                                                              
         MVI   DBSELSRC,C'A'                                                    
*                                                                               
GD1BA    MVC   DBBTYPE,DLUEL.DLUBKTYP                                           
         CLI   DBBTYPE,C'O'        KILL OLYMPIC FROM NDELEM                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0                                                        
                                                                                
*=================================================================*             
* IF DBBTYPE = S ASSUME LOOK-UP OF SRC DATA WHICH REQUIRES A      *             
* SLIGHT ADJUSTMENT TO DBSELSRC (LIKE SETTING IT TO S)            *             
*=================================================================*             
                                                                                
         CLI   DBBTYPE,C'S'                                                     
         BNE   *+12                                                             
         MVI   DBSELSRC,C'S'                                                    
         MVI   DBBTYPE,0                                                        
         DROP  DLUEL                                                            
*                                                                               
GD1C     DS    0H                                                               
         L     RF,VBKTYPTB         A(BOOKTYPE TABLE)                            
         USING SPBKTYPD,RF                                                      
*                                                                               
GD1CA    CLI   0(RF),X'FF'         EOT?                                         
         BNE   *+12                                                             
         MVI   DBBTYPE,0           YES: SUPPRESS INVALID BOOKTYPES              
         B     GD1H                                                             
*                                                                               
         CLC   DBBTYPE,SPBKTYPN    IS BOOKTYPE IN TABLE?                        
         BE    GD1H                                                             
         AH    RF,BKTYPTBL         NO: TRY NEXT                                 
         B     GD1CA                                                            
         DROP  RF                                                               
*                                                                               
*=================================================================*             
* FOR US TV (DBSELMED=T), FORCE RTGSVC=NSI                                      
*=================================================================*             
                                                                                
GD1H     CLI   DBSELMED,C'T'       TEST US DEMOS                                
         BNE   *+8                                                              
         MVI   DBSELSRC,C'N'                                                    
*                                                                               
         CLI   DBSELSRC,C'N'       DMA IMPS FOR NSI/USTV ONLY                   
         BNE   *+12                                                             
         CLI   DBSELMED,C'T'                                                    
         BE    *+8                                                              
         MVI   TAPEOPT,C'N'        OTHERWISE RESET OPTION                       
         EJECT                                                                  
*                                                                               
         BRAS  RE,SETADCON2                                                     
*&&DO                                                                           
* CREATE ADDRESS CONSTANTS FOR NON-ADDRESSABLE WORK AREA FIELDS *               
                                                                                
         LA    RE,ADCONS                                                        
         LA    RF,WKADCONS                                                      
         LA    R0,(ADCONSX-ADCONS)/4                                            
GD2      L     R1,0(RE)                                                         
         AR    R1,RC                                                            
         ST    R1,0(RF)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,GD2                                                           
         B     GD4                                                              
*                                                                               
ADCONS   DS    0A                                                               
         DC    A(DTUNVS-WORKD)                                                  
         DC    A(DTDEMS-WORKD)                                                  
         DC    A(SVIREC-WORKD)                                                  
         DC    A(DUMMY-WORKD)                                                   
         DC    A(IUNWK-WORKD)                                                   
         DC    A(IUNVS-WORKD)                                                   
         DC    A(IUNOLD-WORKD)                                                  
         DC    A(IRTGOLD-WORKD)                                                 
         DC    A(IPUTOLD-WORKD)                                                 
         DC    A(IIMPOLD-WORKD)                                                 
         DC    A(ITOTOLD-WORKD)                                                 
         DC    A(IUNOLDX-WORKD)                                                 
         DC    A(IUNNEW-WORKD)                                                  
         DC    A(IRTGNEW-WORKD)                                                 
         DC    A(IPUTNEW-WORKD)                                                 
         DC    A(IIMPNEW-WORKD)                                                 
         DC    A(ITOTNEW-WORKD)                                                 
         DC    A(IUNNEWX-WORKD)                                                 
         DC    A(IUNXTRA-WORKD)                                                 
         DC    A(DBLOCKS-WORKD)                                                 
         DC    A(RADEMSA-WORKD)                                                 
         DC    A(RADEMSU-WORKD)                                                 
         DC    A(DEMOLST2-WORKD)                                                
         DC    A(GDEXTRA1-WORKD)                                                
         DC    A(GDEXTRA2-WORKD)                                                
         DC    A(AFDLIST-WORKD)                                                 
         DC    A(AFDLISTX-WORKD)                                                
         DC    A(SPDTTAB-WORKD)                                                 
         DC    A(GDEXSPDT-WORKD)                                                
         DC    A(COMEXT-WORKD)                                                  
ADCONSX  EQU   *                                                                
*&&                                                                             
         EJECT                                                                  
* GET ADDRESSES OF REQUIRED DEMO TABLES *                                       
                                                                                
GD4      BRAS  RE,GETDEMTB                                                      
         EJECT                                                                  
*==========================================================                     
* DECODE STATION CALL LETTERS                                                   
*==========================================================                     
                                                                                
GD6      SR    RE,RE                                                            
         ICM   RE,7,BUYREC+6                                                    
         BNZ   GD8                                                              
         MVI   DBFUNCT,DBGETTOT                                                 
         MVC   DBSELRMK,BUYREC+4   NO STAT IS MKT TOTAL REQ                     
         B     GD20                                                             
*                                                                               
GD8      L     R1,SAVER1           CHECK FOR SPILL LOOKUP                       
         CLI   4(R1),2             USER MKT ALREADY SET                         
         BE    *+10                                                             
         MVC   DBSELUMK,BUYREC+4   SET USER MKT TO TEST OVRD                    
*                                                                               
         CLI   DBBTYPE,C'S'        SCAN AM                                      
         BNE   *+10                                                             
         XC    DBSELUMK,DBSELUMK                                                
*                                                                               
         CLI   SAVENTI,C' '        TEST NTI LOOKUP                              
         BH    GD15                                                             
*                                                                               
GD9      CLI   DBSELSTA,C' '       IF OVERRIDE STATION SET                      
         BH    GD12                                                             
* NEED TO READ 00A PROFILE !                                                    
         XC    WORK(16),WORK       READ 00A PROFILE                             
         MVC   WORK(4),=C'S00A'                                                 
         NI    WORK,X'BF'          MAKE 'S' LOWERCASE                           
         MVC   WORK+4(2),DBSELAGY                                               
         L     RF,DBCOMFCS                                                      
         L     R0,CDATAMGR-COMFACSD(RF)                                         
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'90',WORK),SV00APRF,(R0)                             
*                                                                               
         CLI   BUYREC+6,X'E8'      TEST CABLE LOOKUP                            
         BL    GD11                NO                                           
*                                                                               
         CLI   CBLDEMOV,C'0'       TEST SUPPRESS CABLE LOOKUPS                  
         BE    GD20X                                                            
*                                                                               
         CLI   SV00APRF,C'N'       TEST NSI DEFAULT                             
         BE    *+12                                                             
         CLI   SV00APRF,C'F'       OR FUSION                                    
         BNE   GD20X               IF NEITHER, SUPPRESS LOOKUP                  
*                                                                               
GD11     XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPACOM,DBCOMFCS                                                
         MVC   STAPAGY,DBSELAGY                                                 
         MVC   STAPMED,DBSELMED                                                 
         CLI   STAPMED,C'C'        FOR CANADA, MEDIA 'T' IS A 'C'               
         BNE   *+8                                                              
         MVI   STAPMED,C'T'                                                     
         CLI   STAPMED,C'W'        MEDIA CHANGED TO W FOR DEMO LOOKUPS?         
         BE    *+12                YES - CHANGE IT TO MEDIA T!                  
         CLI   STAPMED,C'O'        MEDIA CHANGED TO O FOR DEMO LOOKUPS?         
         BNE   *+8                 NO                                           
         MVI   STAPMED,C'T'        YES - CALL STAPACK WITH MEDIA T!             
         MVI   STAPCTRY,C'U'                                                    
         CLI   DBSELMED,C'C'                                                    
         BNE   *+8                                                              
         MVI   STAPCTRY,C'C'                                                    
         MVC   STAPMKST,BUYREC+4                                                
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DBSELSTA,STAPQSTA                                                
         CLI   BUYREC+6,X'E8'      TEST CABLE LOOKOUP                           
         BL    GD12                NO                                           
*                                                                               
         MVI   DBSELMED,C'T'                                                    
         PACK  DUB,STAPQSTA(4)     SYSCODE                                      
         CVB   R0,DUB                                                           
         STH   R0,DBSELSYC         SET SYSCODE IN BINARY                        
*                                                                               
GD11A    MVI   STAPACT,C'X'        TRANSLATE 3CHAR NET TO 4CHAR                 
         GOTO1 VSTAPACK,(R1)                                                    
*                                                                               
         MVC   DBSELSTA,STAPQSTA   MOVE 4 CHAR NETWORK CODE                     
         MVI   DBSELSTA+4,C'T'                                                  
         MVC   SVSTPSTA,DBSELSTA   SAVE OFF STAPACK NETWORK CODE                
*                                                                               
         CLI   DBSELSTA,C' '       TEST DIDN'T FIND ANY                         
         BE    GD20X               DIDN'T - GO CHECK FOR -S AND EXIT            
*                                                                               
         CHI   R0,7000             FOR 7000-7500 CHECK FOR ALTERNATE            
         BL    GD12                SYSCODE FOR DEMOS                            
         CHI   R0,7500                                                          
         BH    GD12                                                             
*                                                                               
         BRAS  RE,GETSYSC          READ CABLE STA MASTER REC                    
         OC    CBLKUP,CBLKUP       TEST FOUND ALT SYSCODE                       
         BZ    GD20X               NO - JUST EXIT                               
         MVC   DBSELSYC,CBLKUP                                                  
*                                                                               
GD12     CLI   DBSELSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
*                                                                               
*  CHECK TO SEE IF WE ARE LOOKING AT NCM CALL LETTERS                           
         CLC   DBSELSTA(2),=C'CM'  SET NCM SRCE FOR CM**D CALL LETTRS           
         BE    *+10                                                             
         CLC   DBSELSTA(2),=C'SV'  SET NCM SRCE FOR SV**D CALL LETTRS           
         BNE   *+8                                       -HWON 11/27/19         
         CLI   DBSELSTA+4,C'D'                                                  
         BNE   *+8                                                              
         MVI   DBSELSRC,C'C'       SET NCM SOURCE                               
*                                                                               
GD15     OC    DBSELMK,DBSELMK     ALREADY SET                                  
         BNZ   GD20                DON'T BOTHER LOOKING FOR IT                  
         CLI   DBSELMED,C'R'                                                    
         BNE   GD20                                                             
*                                                                               
         BRAS  RE,GETAMKT                                                       
         DROP  R1                                                               
         EJECT                                                                  
GD20     L     R1,AGDEXTR1                                                      
         XC    0(L'GDEXTRA1,R1),0(R1)                                           
         L     R0,DBEXTEND                                                      
*                                                                               
         CR    R0,R1               MAKE SURE WE DON'T POINT TO OURSELF          
         JNE   *+6                                                              
         SR    R0,R0                                                            
*                                                                               
         ST    R1,DBEXTEND                                                      
         MVC   0(4,R1),=CL4'UID '                                               
         ST    R0,4(R1)            SET THE LINK                                 
         MVC   8(2,R1),SAVEUID     SET USERID IN DBLOCK EXT1                    
                                                                                
*  RETURN HERE IF LOOKING UP MULTIPLE TIMES (SPILL ELEMENTS)                    
                                                                                
GD20LP   CLI   DBSELMED,C'O'       TEST OVERNIGHTS                              
         BE    *+8                                                              
         CLI   DBSELMED,C'W'       TEST WEEKLY                                  
         BE    *+8                                                              
         CLI   DBSELMED,C'T'       TEST USTV                                    
         BNE   GD20E               NO                                           
         OC    DBSELSYC,DBSELSYC   TEST CABLE LOOKUP                            
         BNZ   GD20A               YES - GO CHECK MARKET RECORD                 
                                                                                
         CLI   DBBTYPE,0           TEST STANDARD SPECIFIED                      
         BE    GD20A                                                            
         CLI   DBBTYPE,C'G'        TEST STANDARD SPECIFIED                      
         BE    GD20A                                                            
         CLI   DBBTYPE,C'P'        TEST PEOPLE METER SPECIFIED                  
         BE    GD20A                                                            
         CLI   DBBTYPE,C'H'        TEST HISPANIC                                
         BE    GD20A               YES - CHECK FOR PEOPLE METER                 
*                                                                               
         L     RE,VBKTYPTB         A(BOOKTYPE TABLE)                            
         USING SPBKTYPD,RE                                                      
GD20BKT  CLI   0(RE),X'FF'         EOT?                                         
         BE    GD20A               YES: CHECK FOR LPM                           
         CLC   DBBTYPE,SPBKTYPN    IS BOOKTYPE IN TABLE?                        
         BNE   GD20BKT2            YES                                          
         TM    SPBTYIND,SPBKOVN    IF WEEKLY OR OVN BOOKTYPE                    
         BO    GD20BKT1            WE NEED TO GET LPM DATE                      
         TM    SPBTYIND,SPBKWKLY   FROM MKT REC                                 
         BZ    GD20E               IF NOT WKLY OR OVN SKIP                      
GD20BKT1 MVC   BKTYPIND,SPBTYIND   SAVE BOOKTYPE INDICATOR IN TABLE             
         MVC   OVEFFBK,SPOVEFFB    OVERNIGHT EFFECTIVE BOOK FOR BKTYPE          
* PROTECTIVE CODE IN CASE DEMTABS GETS BACKED OUT                               
* COMPARE NEW ELONGATED TABLE LENGTH AGAINST TABLE LENGTH RETURNED              
* BY DEMTABS.  IF NOT EQUAL IGNORE NEW CODE.                                    
* DELETE THIS CODE NEXT TIME TABLE GETS ELONGATED.  DUPLICATE LOGIC             
* NEXT TIME TABLE GETS ELONGATED FOR NEW FIELDS.                                
         LA    R0,SPBKTYLQ                                                      
         CH    R0,BKTYPTBL                                                      
         BE    *+10                                                             
         XC    OVEFFBK,OVEFFBK                                                  
*                                                                               
         B     GD20A               READ MKT RECORD FOR WKLY OR OVN              
*                                                                               
GD20BKT2 AH    RE,BKTYPTBL         NO: TRY NEXT                                 
         B     GD20BKT                                                          
         DROP  RE                                                               
*                                                                               
GD20A    BRAS  RE,GETMKT           READ MARKET RECORD FOR LPM START             
*                                  AND RTGSVC/ALPHAMKT FOR CABLE                
         OC    DBSELSYC,DBSELSYC   TEST CABLE                                   
         BZ    GD20B                                                            
         CLI   GDMKCDEM,C'0'       TEST SUPPRESS LOOKUPS THIS MKT               
         BE    GD20X                                                            
         MVC   DBSELSRC,GDMKCDEM                                                
         MVC   DBSELALF,GDMKALPH                                                
* CHECK FOR OVERRIDES                                                           
         CLI   CBLDEMOV,C'N'       TEST FORCE TO NSI                            
         BE    *+12                                                             
         CLI   CBLDEMOV,C'F'       TEST FORCE TO FUSION                         
         BNE   GD20A1                                                           
         MVC   DBSELSRC,CBLDEMOV   SET OVERRIDE RATING SERVICE                  
*                                                                               
GD20A1   CLI   CBLDEMOV,C'L'       TEST LOOKUP FOR LPM MKT ONLY                 
         BNE   GD20E               NO - AND IGNORE LPM START DATE               
         OC    LPMDTP,LPMDTP       TEST LPM START DATE                          
         BNZ   GD20E               YES - CONTINUE LOOKUP                        
         XC    DBSELSTA,DBSELSTA   SUPPRESS DEMO LOOKUP                         
         B     GD20X                                                            
*                                                                               
GD20B    OC    LPMDTP,LPMDTP       TEST HAVE LPM START DATE                     
         BZ    GD20E               NO - SO NO PEOPLE METER                      
*                                                                               
* ONLY IF ARE ARE ASKING FOR LPM BOOKTYPES OR STANDARD SHOULD WE                
* OVERRIDE THE BOOKTYPE                                                         
* CHECK FOR BOOKTYPES THAT COULD BE USED FOR LPM                                
* IF WE DONT RECOGNIZE THE BOOKTYPE THEN DONT OVERRIDE                          
         CLI   DBBTYPE,C'G'     ALLOW GENERAL BOOKTYPE                          
         BE    *+8                                                              
         CLI   DBBTYPE,C'P'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,0                                                        
         BE    *+8                                                              
         CLI   DBBTYPE,C'H'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'I'                                                     
         BNE   GD20E                                                            
*                                                                               
         OC    DBSELBK,DBSELBK     TEST BOOK SPECIFIED                          
         BZ    GD20C               NO                                           
         CLC   DBSELBK,LPMDTB      COMPARE BOOK YYMM TO LPM YYMM                
         BL    GD20E               LOW - SO NO PEOPLE METER                     
         B     GD20D                                                            
*                                                                               
GD20C    CLC   BDEND(2),LPMDTB     BUY END YM TO LPM START                      
         BL    GD20E               LOW - SO NO PEOPLE METER                     
*                                                                               
GD20D    DS    0C                                                               
* DONT NEED TO DO THIS FOR NCM                                                  
         CLI   DBSELSRC,C'C'                                                    
         BE    GD20E                                                            
                                                                                
*                                                                               
         LA    R0,C'P'             SET TO MAKE BOOK A P                         
         CLI   DBBTYPE,C'G'        ALLOW GENERAL BOOKTYPE                       
         BE    *+8                                                              
         CLI   DBBTYPE,C'P'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,0                                                        
         BNE   *+8                                                              
         OI    BKTYPIND,SPBKWKLY   ENABLE WEEKLY POST FOR BKTYPE P              
*                                                                               
         CLI   DBBTYPE,0           ALLOW STABDARD BOOKTYPE                      
         BE    *+8                                                              
         CLI   DBBTYPE,C'P'        ALLOW BOOKTYPE P                             
         BE    *+8                                                              
         CLI   DBBTYPE,C'G'        ALLOW GENERAL BOOKTYPE                       
         BE    GD20DC                                                           
         CLI   DBBTYPE,C'I'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'H'        UNLESS IT'S AN H                             
         BNE   *+12                                                             
         LA    R0,C'I'                                                          
GD20DC   OI    BKTYPIND,SPBKOVN    ENABLE OVR POST FOR BKTYPE I                 
         STC   R0,DBBTYPE                                                       
*                                                                               
GD20E    TM    FLAGS,CANHPT        TEST CANADIAN HPT'S                          
         BNZ   GD20F                                                            
         CLI   DBSELMED,C'R'       ARB RADIO FOLLOWS CANADA,                    
         BNE   GD20X                FOR NOW                                     
         L     R1,AGDEXTR1         POINT TO EXTENSION 1                         
         MVC   4(4,R1),AGDEXTR2    SET A(EXTENSION 2)                           
         L     R1,AGDEXTR2                                                      
         USING DBEXTRAD,R1                                                      
         XC    0(L'GDEXTRA2,R1),0(R1)                                           
         MVC   DBRID,=C'RADI'      RADIO EXTEND ID                              
****     MVI   DBRCOPT,C'N'        GENERAL SPOT- NO CONDENSED SUPPORT           
         MVI   DBRCOPT,C'Y'        ALLOW CONDENSED MARKET LOOKUPS               
         DROP  R1                                                               
*                                                                               
GD20F    L     R1,ADUMMY           POINT TO AN I/O AREA                         
         XC    0(256,R1),0(R1)     AND CREATE A FAKE RECORD                     
         XC    256(256,R1),256(R1) DO THIS FOR DOUBLE WORD CANADA-BPOO          
         MVI   0(R1),C'R'                                                       
         LA    R0,DRFRSTEL+1-DRKEY                                              
         STCM  R0,3,DRRLEN-DRKEY(R1)                                            
         L     R1,SAVER1                                                        
         MVC   DBAREC,12(R1)       RESTORE DBAREC                               
                                                                                
* GET CONTROL BITS FOR THIS USER *                                              
                                                                                
GD20X    MVI   DBFUNCT,DBGETCTL                                                 
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0                                               
         TM    DBCSHR,DBOTOSHR     TEST COMPUTE SHARES                          
         BZ    *+8                                                              
         MVI   SHRSW,X'80'         INDICATE SHARE COMP REQUIRED                 
         TM    DBC7MIN,DBOE7MIN    TEST 7 MIN LAST QH AFFID OPTION              
         BZ    *+8                                                              
         OI    AFDOPTS,X'80'                                                    
                                                                                
         MVI   DBFUNCT,DBGETDEM    RESTORE DEMO LOOK-UP FUNCTION                
                                                                                
         MVC   DUB(1),0(R3)        MOVE DEMO ELEM CODE                          
         NI    DUB,X'0F'           DROP LEFT HAND BITS                          
         CLI   DUB,2               TEST NORMAL DEMOS                            
         BNE   *+10                NO - NO WEIGHT LIST FOR SPILL                
                                                                                
**********     NOTE FOLLOWING INSTRUCTION HAS LENGTH = 14 ********              
                                                                                
         MVC   SAVEWGTS(14),4(R3)  SAVE WGTD DEMO WEIGHT LIST                   
                                                                                
         MVI   IUNTYPE,1           SET DEFAULT TO RTGS/IMPS ONLY                
* BUILD LIST OF DEMO CODES *                                                    
                                                                                
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         SHI   R0,24                                                            
         JNP   EXIT                                                             
*                                                                               
         SRL   R0,3                                                             
         ST    R0,NUMDEMS          SAVE NUMBER OF DEMOS                         
         LA    R4,24(R3)                                                        
         LA    R1,DEMOLIST                                                      
                                                                                
GD22     CLI   1(R4),63            TEST WEIGHTED DEMO                           
         BE    GD28                                                             
         MVC   0(3,R1),0(R4)                                                    
         CLI   1(R4),C'R'                                                       
         BE    GD26                                                             
         CLI   1(R4),C'I'                                                       
         BE    GD26                                                             
         MVI   IUNTYPE,4                                                        
*                                                                               
GD26     LA    R1,3(R1)            NEXT LIST ENTRY                              
*                                                                               
GD28     LA    R4,8(R4)            NEXT DEMO IN ELEMENT                         
         BCT   R0,GD22                                                          
*                                                                               
         MVI   0(R1),X'FF'         SET E-O-L FLAG                               
*                                                                               
         CLI   DEMOLIST,X'FF'      TEST NO DEMOS IN LIST                        
         JE    EXIT                NONE - EXIT                                  
*                                                                               
         BRAS  RE,PBDEMOS          LOOK FOR POST BUY DEMO OVERRIDES             
*                                                                               
GD29     CLI   BDPROGT-1,0         TEST SPECIAL                                 
         BE    GD30                YES                                          
         CLI   CBLDEMOV,C'L'       TEST CABLE  LPM MKT ONLY                     
         BNE   GD29A               NO                                           
         OC    DBSELSTA,DBSELSTA   TEST DEMO LOOKUP SUPPRESSED                  
         BZ    GD30                                                             
GD29A    B     GD40                                                             
         EJECT                                                                  
* SEARCH FOR DEMO ELEMENT IN BUYREC *                                           
                                                                                
GD30     LA    R6,BDELEM                                                        
         MVC   ELCODE,0(R3)                                                     
         NI    ELCODE,X'0F'                                                     
GD31     BRAS  RE,NEXTEL                                                        
         BNE   GD40                                                             
         CLI   ELCODE,2            TEST SPILL DEMEL                             
         BE    GD32                NO                                           
         CLC   4(2,R3),4(R6)       MATCH SPILL MKT                              
         BNE   GD31                NO - CONTINUE                                
*                                                                               
GD32     CR    R6,R3               COMPARE TO LOOK-UP DEMO ELEM ADDR            
         BE    GD40                                                             
         ZIC   R0,1(R3)            GET NEW DEMEL LEN                            
         AHI   R0,-24                                                           
         BNP   GD40                IF NO DEMOS IGNORE                           
                                                                                
* MOVE OVERRIDES FROM OLD DEMO EL TO NEW (FOR -S BUYS) *                        
                                                                                
         SRL   R0,3                SET FOR BCT                                  
         LA    R1,24(R3)           POINT TO FIRST NEW DEMO                      
         BRAS  RE,GDSRCH                                                        
         LA    R1,8(R1)                                                         
         BCT   R0,*-8                                                           
         B     GD40                                                             
                                                                                
*                                                                               
GD40     DS    0H                                                               
         CLI   DBSELMED,C'R'                                                    
         BNE   *+8                                                              
         BAS   RE,BLDRDL                                                        
                                                                                
         CLI   0(R3),X'10'         TEST AFFID LOOK-UP                           
         BH    GD100                                                            
         B     GD50                                                             
         EJECT                                                                  
*                                                                               
*---------------------- BUILD DEMOLIST FOR RADIO ---------------------*         
*                                                                               
         DS    0H                                                               
BLDRDL   NTR1                                                                   
                                                                                
         XC    RDEMLIST,RDEMLIST    BUILD RADIO DEMO LIST                       
         LA    RE,DEMOLIST                                                      
         LA    RF,RDEMLIST                                                      
         USING RDEMLSTD,RF                                                      
BRDL10   MVI   0(RF),X'FF'         ASSUME END OF DEMOLIST                       
         CLI   0(RE),X'FF'                                                      
         BE    BRDL50                                                           
         MVC   RDLDMOD,1(RE)       INPUT MODIFIER                               
         MVC   RDLDNUM,2(RE)       DEMO NUMBER                                  
         CLI   RDLDMOD,C'R'        IF DEMO IS A RATING,                         
         BNE   BRDL10A                                                          
         MVI   RDLDMOD1,C'I'        GET SUB-ORDINATE MODIFIERS TOO              
         MVI   RDLDMOD2,C'U'                                                    
         MVI   RDLOPER,C'D'         OPERATION=DIVIDE                            
BRDL10A  LA    RE,3(RE)                                                         
         LA    RF,RDEMLSTQ(RF)                                                  
         B     BRDL10                                                           
         DROP  RF                                                               
*                                                                               
BRDL50   DS    0H                                                               
         L     R1,ADEMLST2         R1-->TARGET AREA FOR LIST                    
         XC    0(L'DEMOLST2,R1),0(R1)                                           
         LA    RF,RDEMLIST                                                      
         USING RDEMLSTD,RF                                                      
         LA    RE,DEMOLIST                                                      
BRDL60   MVI   0(R1),X'FF'         ASSUME FINISHED BUILDING THE LIST            
         CLI   0(RE),X'FF'                                                      
         BE    BRDLX                                                            
         MVC   0(3,R1),0(RE)                                                    
         CLI   RDLDMOD1,0          IF THERE ARE EXTRA                           
         BE    BRDL60A              DEMO MODIFIERS, TRANSFER                    
         LA    R1,3(R1)                                                         
         MVC   0(1,R1),0(RE)        THEM TO NEW DEMOLIST (DEMOLST2)             
         MVC   1(1,R1),RDLDMOD1                                                 
         MVC   2(1,R1),RDLDNUM                                                  
BRDL60A  CLI   RDLDMOD2,0          IF THERE ARE EXTRA                           
         BE    BRDL60B              DEMO MODIFIERS, TRANSFER                    
         LA    R1,3(R1)                                                         
         MVC   0(1,R1),0(RE)        THEM TO NEW DEMOLIST (DEMOLST2)             
         MVC   1(1,R1),RDLDMOD2                                                 
         MVC   2(1,R1),RDLDNUM                                                  
BRDL60B  LA    R1,3(R1)                                                         
         LA    RE,3(RE)                                                         
         LA    RF,RDEMLSTQ(RF)                                                  
         B     BRDL60                                                           
         DROP  RF                                                               
*                                                                               
BRDLX    J     EXIT                                                             
         EJECT                                                                  
*===================================================================*           
*  BUY DESCRIPTION DEMO LOOK-UP                                     *           
*===================================================================*           
                                                                                
GD50     LA    R6,BDELEM                                                        
         MVI   ELCODE,X'67'        SEARCH FOR ORBIT ELEMENT                     
         BRAS  RE,NEXTEL                                                        
         BE    GD60                                                             
                                                                                
* NO ORBIT *                                                                    
                                                                                
         MVC   DBSELDAY,BDDAY      SET DAY(S)                                   
         MVC   DBSELTIM,BDPROG     SET START/END TIMES                          
         XC    SVWTPDQH,SVWTPDQH                                                
         MVI   DAYREAD,0                                                        
*                                                                               
         CLC   BDSTART,BDEND       SET DATE FOR SINGLE DAY                      
         BE    *+12                                                             
         CLI   SV1WPR1,C'1'        SPECIAL FOR CPPRS EXTRACT                    
         BNE   GD50A               BDSTART HAS ACTUAL DAY HERE                  
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(3,BDSTART),(2,DBSEL1WK)                               
         MVI   DBSELWKN,0                                                       
*                                                                               
GD50A    MVI   DEMANDSW,0          RESET SWITCH                                 
         MVC   DBTAPEP,TAPEOPT                                                  
*                                                                               
         L     RF,VBKTYPTB         A(BOOKTYPE TABLE)                            
         USING SPBKTYPD,RF                                                      
GD50B    CLI   0(RF),X'FF'         EOT?                                         
         BNE   *+12                                                             
         MVI   DBBTYPE,0           YES: INVALID BOOKTYPE                        
         B     GD50C                                                            
*                                                                               
         MVC   BKTYPIND,SPBTYIND   SAVE BOOKTYPE INDICATOR IN TABLE             
         MVC   OVEFFBK,SPOVEFFB    OVERNIGHT EFFECTIVE BOOK FOR BKTYPE          
                                                                                
*=====================================================================          
* PROTECTIVE CODE IN CASE DEMTABS GETS BACKED OUT                               
* COMPARE NEW ELONGATED TABLE LENGTH AGAINST TABLE LENGTH RETURNED              
* BY DEMTABS.  IF NOT EQUAL IGNORE NEW CODE.                                    
* DELETE THIS CODE NEXT TIME TABLE GETS ELONGATED.  DUPLICATE LOGIC             
* NEXT TIME TABLE GETS ELONGATED FOR NEW FIELDS.                                
*=====================================================================          
                                                                                
         LA    R0,SPBKTYLQ                                                      
         CH    R0,BKTYPTBL                                                      
         BE    *+10                                                             
         XC    OVEFFBK,OVEFFBK                                                  
*                                                                               
         CLC   DBBTYPE,SPBKTYPN    IS BOOKTYPE IN TABLE?                        
         BE    GD50C               YES                                          
         AH    RF,BKTYPTBL         NO: TRY NEXT                                 
         B     GD50B                                                            
         DROP  RF                                                               
*                                                                               
GD50C    DS    0H                                                               
         CLI   DBSELALF,C' '       IF ALPHA MARKET PRESENT                      
         BNH   GD50D                                                            
         XC    DBSELMK,DBSELMK     THEN DO NOT PASS MARKET NUMBER               
         XC    DBSELRMK,DBSELRMK                                                
*                                                                               
GD50D    CLI   DBSELMED,C'R'       IF RADIO,                                    
         BNE   *+8                                                              
         MVI   DBBEST,C'P'          SUPPORT OVERNIGHT DAYPART                   
*                                                                               
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
         MVC   DSTACALL,DBSELSTA                                                
                                                                                
         MVC   SVDBBEST,DBBEST                                                  
         BRAS  RE,TOMONTHL        DEFAULT WEEKLY TO MONTHLY LOOKUPS             
*                                 AFTER MMW CUTOFF DATE.                        
* IS THIS A TRUE LPM MARKET -                                                   
         TM    DBVOPT,X'40'                                                     
         BNO   GD50DX                                                           
         CLI   DBSELMED,C'C'       TEST CANADA TV/NET                           
         JE    GD50DX                                                           
         OC    VPHSDAT,VPHSDAT     NO DATE PASSED MUST NOT BE OVN               
         JZ    GD50DX                                                           
         CLI   OVPHOPT,C'Y'                                                     
         BNE   GD50DX                                                           
         BRAS  RE,SETMKTYP         SETS STABMKT                                 
         BRAS  RE,TRUELPM          IF NOT A TRUE LPM LPMDTP WILL CLEAR          
*                                                                               
* CHECK POSTING OPTIONS                                                         
                                                                                
GD50DX   CLC   =C'YNN',LPMWKOPT   POST MONTHLY FOR NON LPM MARKETS              
         BNE   *+14               IF LPMWK OPTION IS TURNED ON                  
         OC    LPMDTP,LPMDTP                                                    
         BZ    GD51VX                                                           
                                                                                
         CLI   LPMWKOPT,C'Y'      CHECK FOR EACH POSTING OPTION                 
         BE    GD50F                                                            
         CLI   WVPHOPT,C'Y'                                                     
         BE    GD50F                                                            
         CLI   OVPHOPT,C'Y'       OVN ALWAYS FIGURE OUT THE BOOK TO             
         BE    GD50F              READ FROM DATES PASSED IN                     
*                                 DONT ASSUME ACTBKG PASSED IN IS RIGHT         
*                                                                               
* IF WE ARE POSTING MONTHLY -CHECK IF BOOK IS JUN09 FOR LPM MARKET              
* JUN09 IS THE DIGITAL TRANSITION WHERE NIELSEN HAS NOT RELEASED A              
* BOOK HENCE WE WANT TO TO OVERNIGHTS - IF NO OVERNIGHTS WE WANT TO             
* TO POST TO JUL_09                                                             
         MVI   DIGITALF,C'N'                                                    
                                                                                
         TM    DBVOPT,X'40'                                                     
         BNO   GD51VX                                                           
         CLC   DBSELBK,=AL2(JUN_09)                                             
         BNE   GD51VX                                                           
         CLI   DBSELMED,C'T'                                                    
         BNE   GD51VX                                                           
         CLI   DBSELSRC,C'N'                                                    
         BNE   GD51VX                                                           
*                                                                               
* CHECK FOR LPM MARKET                                                          
*                                                                               
         OC    LPMDTP,LPMDTP     ONLY FOR LPM MARKETS-NON LPM POST              
         BZ    GD51VX            TO MONTHLY                                     
         OC    DBSELSYC,DBSELSYC  LPM CABLE JUN09 POST TO JUL09                 
         BNZ   GD50E                                                            
         CLI   DBBTYPE,C'H'      HISPANIC/BLACK BOOKTYPES POST TO               
         BE    *+8               JUL09 BOOK                                     
         CLI   DBBTYPE,C'B'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'I'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'J'                                                     
         BNE   *+14                                                             
GD50E    MVC   DBSELBK,=AL2(JUL_09)                                             
         B     GD51VX                                                           
         MVI   DIGITALF,C'Y'     TURN ON DIGITAL TRANSITION FLAG                
*                                                                               
*===================================================================            
* YOU SHOULD ONLY BE IN HERE IF YOU ARE POSTING AGAINST            *            
* LPMWK, WTP, OVN OPTIONS                                          *            
*===================================================================            
******** CLI   DBSELMED,C'T'                                                    
******** BNE   GD51VX                                                           
* WE USED TO CHECK FOR THE MEDIA COMING IN TO DETERMINE IF WE SHOULD            
* READ MONTHLY, WEEKLY OR OVERNIGHTS.  THERE ARE INSTANCES SUCH AS              
* CROSSING WHEN A MARKET CROSSES BETWEEN SMM DARK AND LPM PERIODS               
* WHERE WE CANT REPLY ON THE BOOK COMING IN TO BE CORRECT.  WE ARE              
* NOT GOING TO ASSUME THE BOOK PASSED IN IS WHAT WE NEED WANT.                  
*                                                                               
GD50F    DS    0H                                                               
***      MVC   SVDBBEST,DBBEST                                                  
*&&DO                                                                           
* BEN IS TAKING THIS BOOKTYPE CHECK OUT.  I HAVE A MUCH BETTER CHECK            
         CLI   DBBTYPE,C'P'        LPM PRELIM IS OK                             
         BE    *+12                                                             
         CLI   DBBTYPE,0                                                        
         BNE   GD51VX                                                           
*&&                                                                             
         CLI   DBBTYPE,0           ALWAYS ALLOW STANDARD                        
         BE    GD50H                                                            
         CLI   LPMWKOPT,C'Y'       IF WE ARE POSTING WEEKLY THEN CHECK          
         BE    *+8                 BOOKTYPE INDICATOR TO SEE IF WEEKLY          
         CLI   WVPHOPT,C'Y'        IS AVAILABLE FOR THIS BOOKTYPE               
         BNE   *+12                IF ITS NOT AN AVAILABLE WEEKLY               
         TM    BKTYPIND,SPBKWKLY   BOOKTYPE THEN CHECK TO SEE IF                
         BNZ   GD50H               OVERNIGHT POSTING OPTION IS ON               
         CLI   DIGITALF,C'Y'       DIGITAL TRANSITION                           
         BE    GD50H               BOOKTYPE IS AVAILABLE FOR OVERNIGHT          
         CLI   OVPHOPT,C'Y'                                                     
         BNE   GD50G                                                            
         TM    BKTYPIND,SPBKOVN    BOOKTYPE AVAILABLE FOR OVERNIGHT?            
         BNZ   GD50H                                                            
GD50G    MVI   DBSELMED,C'T'       IF NOT AVAILABLE-GO READ MONTHLY             
         XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         B     GD51VX                                                           
                                                                                
GD50H    OC    DBSELSYC,DBSELSYC   CABLE POST AGAINST MONTHLY                   
         BNZ   GD51VX                                                           
         OC    DBSELMK,DBSELMK                                                  
         BNZ   GD51VX                                                           
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         CLI   DIGITALF,C'Y'       DIGITAL TRANSITION JUN09 BOOK ALWAYS         
         BE    GD51A               GRAB DATE FROM BDSTART- VPHSAT               
*                                  IS BROKEN OUT TO THE CORRECT WK              
         OC    VPHSDAT,VPHSDAT                                                  
         BNZ   GD51B                                                            
GD51A    GOTO1 (RF),DMCB,(3,BDSTART),(2,VPHSDAT)                                
GD51B    GOTO1 (RF),DMCB,(2,VPHSDAT),(3,DUB)                                    
                                                                                
* FOR ACHIEVED LOOKUP THE VPHSDAT COMING IN IS ALWAYS BASED                     
* ON A MONDAY DAY. ADJUST THE DATE UNTIL IT IS EQUAL TO THE                     
* ROTATION START DAY. EX, IF ROTATION IS A SAT THEN ADJUST THE DATE             
* WE TRANSLATE THE BOOK (DUB) FROM.                                             
* THIS WAY WE CAN ACCURATELY DETERMINE IF THE CURRENT ROTATION FALLS            
* AFTER THE LPM START BOOK EVEN IF THE MONDAY DAY IS RIGHT BEFORE LPM.          
         BRAS  RE,ADJVHSDT                                                      
         MVC   DUB,ADJDATE                                                      
*                                                                               
         MVC   SVSELBK,DBSELBK                                                  
*********************************************************************           
*  CONVERT THE BOOK TO GET THE CORRECT YEAR,LAST WEEK OF THE YEAR CAN           
*  ACTUALLY BE THE 1ST WEEK OF THE NEXT YEAR ON THE DEMO FILE DEPENDED          
* ON THE DAY OF THE ROTATION                                                    
**********************************************************************          
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(3,DUB),(0,DUB3)                                       
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         MVC   WORK(12),DMCB+4                                                  
         GOTO1 =V(NSIWEEK),DMCB,DUB3,RR=RELO                                    
         MVC   DBSELBK(1),DMCB+4                                                
         MVC   DBSELBK+1(1),DMCB                                                
***************************************************************                 
* CHECK FOR LPM MARKET                                                          
         OC    LPMDTP,LPMDTP       ANY LPM DATE                                 
         BZ    GD51C                NO - USE WEEKLY HOMES TO ADJUST             
         CLC   DUB(2),LPMDTB        MONTH GREATER THAT LPM                      
         BL    GD51C                YES - GET LPM DEMOS                         
         CLI   DBBTYPE,C'P'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0            IF LPM ALREADY NO BOOKTYPE                  
         CLI   DBBTYPE,C'I'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,C'H'         IF LPM ALREADY NO BOOKTYPE                  
         MVI   MKTTYPE,MKTLPM                                                   
         B     WVPHLPM              YES - GET LPM DEMOS                         
GD51C    DS    0C                                                               
         CLI    DIGITALF,C'Y'      DIGITAL TRANSITION GO TO                     
         BNE   *+12                OVERNIGHTS FOR LPM MARKETS                   
         MVI   DIGITALF,C'N'       ELSE WE WANT TO POST TO MONTHLY              
         B     GD51VX                                                           
*----------------   NON LPM SECTION -------------------------------             
* AT THIS POINT- NOT LPM - CHECK TO SEE IF WE ARE READING SET METERED           
* MARKET.  IF SET METERED THEN PROCEED ELSE WE ARE DIARY MARKET                 
* DIARY MARKETS IGNORE  WTP AND OTP OPTIONS AND PROCEED TO READ                 
* BOOK OPTION OR ACT BOOK.                                                      
         OC    LPMDTP,LPMDTP                                                    
         BZ    GD51CC                                                           
*                                                                               
         CLC   WVPHOPT(2),=C'NN'   OVN=Y OR WTP OPTION ASKED FOR?               
         BE    GD51D               NO OPTION READ MONTHLY                       
*                                                                               
         MVI   MKTTYPE,MKTSETM                                                  
         B     GD51D                                                            
GD51CC   BRAS  RE,SETMKTYP          CALL ROUTINE TO GET MKT TYPE                
GD51D    CLI   MKTTYPE,MKTSETM      SET METERED ?                               
         BE    GD51DD                                                           
         MVI   WVPHOPT,C'N'         DIARY MKT READ MONTHLY                      
         MVI   OVPHOPT,C'N'                                                     
         XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         B     GD51V2                                                           
GD51DD   MVI   MKTTYPE,MKTSETM                                                  
*                                                                               
         XC    DBACTSTA,DBACTSTA                                                
         XC    DBACTBK,DBACTBK                                                  
         MVI   DBSELMED,C'W'                                                    
*****    MVI   DBBTYPE,0           WTF IS THIS?                                 
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,VPHSDAT),(3,DUB)                                    
         MVC   SVYEAR(1),DUB                                                    
*&&DO                                                                           
* REMOVE FOR ADDRESSUBILTY.  WE DO NOT HAVE DATA PASS 5 YEARS                   
*                                                                               
         CLI   DUB,X'66'                                                        
         BNE   WVYX0                                                            
         CLC   DUB+1(2),=X'0C1B'                                                
         BL    WVYX0                                                            
         MVI   DUB,X'67'                                                        
         B     WVYX0A                                                           
WVYX0    DS    0C                                                               
         CLI   DUB,X'67'                                                        
         BNE   WVYX0A                                                           
         CLC   DUB+1(2),=X'0C1D'                                                
         BL    WVYX0A                                                           
         MVI   DUB,X'68'                                                        
*&&                                                                             
WVYX0A   DS    0C                                                               
***      MVC   DBSELBK(1),DUB      SET BOOK YEAR                                
         MVI   DBSELBK+1,0         CLEAR BOOK MONTH-READ ZERO RECORD            
         CLI   DBBTYPE,C'P'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0                                                        
                                                                                
         XC    SVWTPDQH,SVWTPDQH                                                
         MVI   DAYREAD,0                                                        
         MVI   NEXTYRLK,C'N'                                                    
         MVI   LASTYRLK,C'N'                                                    
         MVC   SVSELDAY,DBSELDAY   SAVE ROTATION                                
                                                                                
                                                                                
GD51E    MVI   AFFIDLK,C'N'                                                     
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,GETVHOME                                        
         MVI   MISCOUNT,0          CLEAR MISSING DATA COUNT AND DAY             
         MVI   MISSDAY,0                                                        
         MVC   DBSELDAY,SVSELDAY   RESTORE ROTATION                             
*                                                                               
* CHECK IF WE NEED TO READ LAST YRS RECORD                                      
* THIS CAN HAPPEN IF WE FAILED READING AN AFFID LOOKUP                          
* AND THE AFFID SPOT DATE IS ON A SAT OR SUNDAY OF THE 1ST WEEK OF              
* THE YEAR BUT THE ROTATION WE WANT TO RERATE AGAINST STARTS BEFORE             
* SAT SO BELONGS IN THE LAST WEEK OF LAST YEARS RECORD                          
*                                                                               
*                                                                               
         CLI   VPHSWK+1,0          WE NEED LAST YEARS RECORD                    
         BNE   GD51F                                                            
         CLI   NEXTYRLK,C'N'       IF DID END OF YEAR SPLIT WEEK DONT           
         BNE   GD51F               DO THIS AGAIN                                
         CLI   LASTYRLK,C'Y'       DID WE ALREADY ADJUST  TO READ LAST          
         BE    GD51F               YEAR'S RECORD?                               
         ZIC   RE,DBSELBK          BUMP TO NEXT YEAR'S 00 RECORD                
         SHI   RE,1                                                             
         STC   RE,DBSELBK                                                       
         MVI   DBSELBK+1,0                                                      
         MVI   LASTYRLK,C'Y'       SET FLAG THAT WE READ LAST YR                
         B     GD51E                                                            
*                                                                               
* END OF YEAR SPLIT WEEK WHERE THE WEEK CROSSES                                 
* THE END OF THE YEAR AND THE 1ST WEEK OF THE NEXT YR                           
* TEST END OF YEAR SPLIT  WEEK.  IF ITS A WEEK WHICH CROSSES THE END            
* OF YEAR AND WE DIDNT FIND THE SAT-SUN DATA THAT MEANS WE SHOULD TRY           
* TO READ THE NEXT YEARS RECORD FOR SAT-SUN DATA.                               
* ALSO CHECK TO SEE IF OOWR WHERE DBSELDAY IS ON THE NEXT YRS FILE              
* BECAUSE BUY WEEK WAS ON THE LAST WEEK OF THE PREVIOUS YEAR                    
*                                                                               
GD51F    CLI   NEXTYRLK,C'N'       ALREADY PROCESSED END OF                     
         BNE   GD51I               YEAR SPLIT WEEK.                             
         CLI   VPHSWK+1,52         WAS THE WEEK ASKED FOR 52ND WEEK?            
         BL    GD51I                                                            
         BRAS  RE,SMMSPLIT                                                      
         CLI   NEXTYRLK,C'Y'        SMM END OF YR SPLIT WEEK                    
         BNE   GD51I                LOOKUP                                      
         B     GD51E                                                            
*                                                                               
* CHECK TO SEE IF WE HAVE CALL LETTER LINK                                      
* IF WE HAVE A CALL LETTER LINK CHECK TO SEE IF WE HAVE                         
* A M-SU SPLIT WEEK PROBLEM WHERE WE READ M-F FROM 1 STATION                    
* BUT THE SA-SU COULD BE ON THE CALL LETTER LINKED DEMO RECORD                  
* OR OOWR SITUATION WHERE DAYS OF THE ROTATION IS ON THE LINKED                 
* STATION'S DEMO RERORD                                                         
*                                                                               
GD51I    CLC   DBSELSTA,DSTACALL   CHECK FOR DSTATION LINK                      
         BE    GD51L                                                            
         CLI   DSTASPLT,C'Y'       IF ALREADY PROCESSED CALL LETTER             
         BE    GD51L               LINK SPLIT WEEK LOOKUP                       
         BRAS  RE,DSTASPLIT                                                     
         CLI   DSTASPLT,C'Y'                                                    
         BNE   GD51L                                                            
         B     GD51E                SPLIT WEEK SITUATION                        
*                                                                               
* IF LPMWK WAS THE ONLY OPTION ENTERED THEN WE HAVE TO SET                      
* DMINDEX TO 'Y' SO THE DEMO GETS INDEXED LATER ON                              
*                                                                               
GD51L    MVI   DMINDEX,C'N'                                                     
         CLC   LPMWKOPT,=C'YNN'                                                 
         BNE  *+8                                                               
         MVI   DMINDEX,C'Y'        SET FORCE DEMO INDEX FLAG                    
                                                                                
* ********OVERNIGHT LOGIC*************************************                  
                                                                                
GD51O    CLC   WVPHOPT(2),=C'NY'   OVN=Y ONLY?                                  
         BNE   *+16                NO - JUST LEAVE VPHHOMES ALONE               
         XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
                                                                                
         OC    VPHHOMES,VPHHOMES   IF ANY HOMES                                 
         BNZ   GD51V2              IT'S OK                                      
*                                                                               
* NO VPHHOMES.  CHECK TO SEE IF WTP OPTION IS ON                                
* FOR WEEKLY RETRY TOR STATION CALL LETTER LINK                                 
*                                                                               
         CLI   WVPHOPT,C'Y'                                                     
         BNE   GD51P                                                            
         CLC   DBSELSTA,DSTACALL   DO WE HAVE A STATION LINK                    
         BE    GD51P               TO LOOK AT?                                  
         MVC   DBSELSTA,DSTACALL   LOOK FOR LINKED STATION                      
         MVC   DSTACALL,SVSELSTA   SWAP LINK AND  CALL LETTERS                  
         MVI   DBERROR,0           AND REREAD FOR THE LINKED                    
         XC    VPHHOMES,VPHHOMES   CALL LETTERS                                 
         XC    VPHFACT,VPHFACT                                                  
         B     GD51E                                                            
                                                                                
*                                                                               
GD51P    CLI   OVPHOPT,C'Y'        DO WE WANT OVERNIGHTS                        
         BNE   GD51T               NO WEEKLY HOMES BUT DONT WANT OVN            
*                                                                               
         CLI   SMMMONTH,C'Y'                                                    
         JNE   GD51R                                                            
GD51Q    XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
         XC    VPHHOMES,VPHHOMES                                                
         MVI   DAYREAD,0                                                        
         J     GD51V2                                                           
*                                                                               
GD51R    MVI   DBSELMED,C'O'                                                    
         BRAS  RE,GETOVSWK                                                      
         MVC   DBSELBK,VPHSWK      OTHERWISE - TRY FOR OVERNIGHT                
         XC    SVWTPDQH,SVWTPDQH                                                
         MVI   DAYREAD,0                                                        
         MVI   TAPEOPT,C'Y'        OVERNIGHTS IS IMPRESSION BASED               
*                                                                               
* CHECK TO SEE IF OVERNIGHT BOOKTYPE IS ONLY AVAILABLE                          
* AFTER A CERTAIN EFFECTIVE BOOK - AS DEFINED BY DEDEMTABS SPBOOKTB             
* IF BOOK IS BEFORE EFFECTIVE DATE THEN KEEP POSTING AS MONTHLY                 
* SO OLD POSTS WILL REMAIN THE SAME                                             
*                                                                               
         OC    OVEFFBK,OVEFFBK                                                  
         BZ    GD51S                                                            
         CLC   DBSELBK,OVEFFBK     READ MONTHLY !                               
         BL    GD51Q                                                            
*                                                                               
GD51S    CLI   COMPASS,C'1'        TEST COMSCORE PASS 1                         
         JE    GD51T               YES- DON'T NEED NSI DEMOS                    
         CLI   NSIREQ,C'Y'         TEST ANY NSI DEMOS                           
         JNE   GD51T                                                            
                                                                                
*=====================================================================          
* CALL DEMAND AND RETURN OVERNIGHT VALUE IN VPHHOMES                            
*=====================================================================          
                                                                                
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,GETOVHOM                                        
                                                                                
*================================================================               
* NOW DEAL WITH COMSCORE DEMOS                                                  
* IF PASS ONE THEN BYPASS ALL EXISTING DEMO SYSTEM CALLING                      
* CODE AND GO STRAIGHT TO CALL COMINTER                                         
*================================================================               
                                                                                
GD51T    CLI   COMPASS,C'1'                                                     
         JE    *+12                                                             
         CLI   DBVOPT,X'40'                                                     
         JNE   GD51U                                                            
*                                                                               
         CLI   DBSELSRC,C'N'                                                    
         JE    *+12                                                             
         CLI   DBSELSRC,C'F'                                                    
         JNE   GD51U                                                            
*                                                                               
         CLI   COMREQ,C'Y'                                                      
         JNE   GD51U                                                            
*                                                                               
         MVI   COMTYPE,C'A'        SET FOR ACHIEVED                             
         BRAS  RE,COMSCORE                                                      
         J     GD99                THAT'S ALL FOR PASS 1                        
*                                                                               
* COMSCORE PASS 2                                                               
*                                                                               
GD51U    CLI   COMPASS,C'2'                                                     
         JE    *+12                                                             
         CLI   DBVOPT,X'40'                                                     
         JNE   GD51V                                                            
*                                                                               
         CLI   DBSELSRC,C'N'                                                    
         JE    *+12                                                             
         CLI   DBSELSRC,C'F'                                                    
         JNE   GD51V                                                            
*                                                                               
         CLI   COMREQ,C'Y'                                                      
         JNE   GD51V                                                            
*                                                                               
         MVI   COMTYPE,C'A'        SET FOR ACHIEVED                             
         BRAS  RE,COMSCORE                                                      
                                                                                
GD51V    CLI   DEMANDSW,0          IF WE DIDNT GET TO HOOK                      
         BNE   GD51V2              EXIT WITH ERROR                              
*                                                                               
* IF WE HAVE AN ERROR READING POSTING AGAINST WEEKLY OR OVERNIGHTS              
* FOR AFFIDS AND THE RERATE ALSO FAILS THEN WE NEED TO                          
* CHECK IF OTHER SPOTS IN THE AFFIDS LOOKUP WAS DONE SUCCESSFULLY.              
* IF SO WE WANT TO PASS BACK THOSE DEMOS WITH BUYERS ESTIMATES                  
* AVERAGED IN.  THIS IS WHAT THE SPOT WRITER DOES SO WE NEED TO MATCH           
*                                                                               
         CLI   AFDSW,C'Y'          AFFID LOOKUP?                                
         BNE   POVPHX              IF WE FAIL TO FIND DATA                      
         OC    NOAFDCNT,NOAFDCNT   FOR WEEKLY OR OVERNIGHT POSTING              
         BZ    POVPHX              AND WE HAVE AFFID NUMBERS                    
         OC    AFDCNT,AFDCNT       FOR SPOTS THEN AVERAGE IN THE B.E            
         BZ    POVPHX              FOR THE MISSING SPOTS.                       
         LA    R1,DTDEMSU                                                       
         BRAS  RE,GETBYEST         GET BUYERS ESTIMATES                         
         B     GD56                                                             
*===================================================================            
*                                                                               
GD51V2   MVC   DBSELBK,SVSELBK                                                  
         MVI   DBSELMED,C'T'                                                    
                                                                                
GD51VX   DS    0H                                                               
         CLI   DBSELMED,C'T'                                                    
         BNE   GD51O02X                                                         
         CLI   DBSELSRC,C'F'        NO FUSION OLYMPIC EXCLUSION                 
         BE    GD51O02X                                                         
         CLI   WVPHOPT,C'Y'        IF ASKING WEEKLY NON LPM THEN                
         BE    GD51O02X            DONT BOTHER W OLY EXCLUSION                  
*****    CLI   OVPHOPT,C'Y'        IF ASKING OVERNIGHTS LPM THEN                
*****    BE    GD51O02X            DONT BOTHER W OLY EXCLUSION                  
*                                                                               
*****    MVC   SVSELBK,DBSELBK                                                  
*****    BRAS  RE,SETMKTYP          CALL ROUTINE TO GET MKT TYPE                
         CLI   MKTTYPE,MKTSETM      SET METERED ?                               
         BNE   GD51O98A                                                         
         CLI   SMMMONTH,C'Y'        IS MONTHLY OVNTS REQUESTED                  
         BE    GD51O98A             CAN GET OLYMPIC EXC                         
         CLI   OVPHOPT,C'Y'        IF ASKING OVERNIGHTS LPM THEN                
         BE    GD51O02X            DONT BOTHER W OLY EXCLUSION                  
*                                                                               
GD51O98A CLI   DBBTYPE,C'L'         ALLOW LIVE AND LIVE+SAME DAY                
         BE    *+8                  AND LIVE+3 FOR OLYMPICS                     
         CLI   DBBTYPE,BOOKTYPE_LS                                              
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_L1                                              
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_L3                                              
         BE    *+8                                                              
         CLI   DBBTYPE,0                                                        
         BNE   GD51O02X                                                         
         CLI   SVD0PROF+7,C'Y'                                                  
         BE    *+12                                                             
         CLI   SVD0PROF+7,C'F'                                                  
         BNE   GD51O02X                                                         
*                                                                               
* DATE CHECK FOR OLYMPIC EXCLUSION                                              
* OLYEXCL1 IS THE DATE TABLE                                                    
*                                                                               
GD51O98X DS    0H                                                               
                                                                                
*                                                                               
         LA    RE,OLYEXCL1                                                      
*                                                                               
GD51O10B CLI   0(RE),X'FF'                                                      
         BE    GD51O10X                                                         
         CLC   DBSELBK,0(RE)                                                    
         BE    GD51O10D                                                         
         LA    RE,8(RE)                                                         
         B     GD51O10B                                                         
*                                                                               
GD51O10D CLC   SVBSTART(3),5(RE)         STARTS AFTER SWEEP END                 
         BH    GD51O10E                                                         
         CLC   SVBSTART+3(3),2(RE)       AND ENDS BEFORE SWEEP START            
         BL    GD51O10E                                                         
         B     GD51O10X                                                         
*                                                                               
GD51O10E CLI   DBBTYPE,C'L'                                                     
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_OL                                              
         B     GD51O10X                                                         
* ADD IN WHEN _OS GETS DEFINED                                                  
         CLI   DBBTYPE,BOOKTYPE_LS                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_OS                                              
         B     GD51O10X                                                         
         CLI   DBBTYPE,BOOKTYPE_L3                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_O3                                              
         B     GD51O10X                                                         
         CLI   DBBTYPE,BOOKTYPE_L1                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_O1                                              
         B     GD51O10X                                                         
         MVI   DBBTYPE,C'O'                                                     
         B     GD51O10X                                                         
*                                                                               
*&&DO                                                                           
GD51O10F DS    0H                                                               
         CLC   SVBSTART(3),5(RE)                                                
         BNH   GD51O10X                                                         
         CLI   DBBTYPE,C'L'                                                     
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_OL                                              
         B     GD51O10X                                                         
         CLI   DBBTYPE,BOOKTYPE_LS                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_OS                                              
         B     GD51O10X                                                         
         CLI   DBBTYPE,BOOKTYPE_L3                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_O3                                              
         B     GD51O10X                                                         
         MVI   DBBTYPE,C'O'                                                     
*&&                                                                             
GD51O10X B     GD51O02X                                                         
*                                                                               
*  EXCLUSION DATE TABLE                                                         
*  BYTES 1-2 BOOK YYMM                                                          
*  BYTES 3-5 SWEEP START DATE YYMMDD                                            
*  BYTES 6-8 SWEEP END DATE YYMMDD                                              
*                                                                               
OLYEXCL1 DC    XL8'6E026E02046E0303'                                            
         DC    XL8'720272011E72021A'                                            
         DC    XL8'660266011C66021B'                                            
         DC    XL8'7602760208760219'                                            
         DC    X'FF'                                                            
                                                                                
                                                                                
GD51O02X DS    0H                                                               
*                                                                               
         XC    DTDEMSU,DTDEMSU     CLEAR DEMO ACCUMULATOR AREA                  
         CLI   MKTTYPE,MKTLPM                                                   
         BE    *+8                                                              
         CLI   WVPHOPT,C'P'        PEOPLE METER LOOKUP?                         
         BNE   PWVPHX                                                           
*================================================================               
* FIGURE OUT THE BOOK TO POST AGAINST MARKETS                                   
*================================================================               
WVPHLPM  XC    DTDEMSU,DTDEMSU                                                  
         XC    DBACTSTA,DBACTSTA                                                
         XC    DBACTBK,DBACTBK                                                  
         MVI   DBSELMED,C'W'       SET FOR WEEKLY LOOKUP                        
***      MVI   DBBTYPE,0           NO BOOK TYPES AVAILABLE                      
*                                                                               
         CLI   LPMWKOPT,C'Y'      CHECK FOR EACH POSTING OPTION                 
         BE    *+8                                                              
         CLI   WVPHOPT,C'Y'                                                     
         BE    *+8                                                              
         CLI   OVPHOPT,C'Y'                                                     
         BNE   PWVPHX                                                           
*                                                                               
         BRAS  RE,ACHWKBK                                                       
WVPHLPM4 CLI   SVD0PROF+11,C'M'              OVERNIGHTS?                        
         BE    *+8                                                              
         CLI   SVD0PROF+11,C'Y'              OVERNIGHTS?                        
         BNE   PWVPHX                                                           
         CLC   =C'NNY',LPMWKOPT              IF OVERNIGHTS ONLY                 
         BNE   PWVPHX                        READ OVERNIGHTS                    
         MVI   DBSELMED,C'O'                                                    
*=================================================================              
PWVPHX   DS    0C                                                               
*                                                                               
GD51     CLI   DBSELMED,C'W'             IF THE INPUT BOOK IS SET               
         BE    *+8                       AS P FOR WEEKLY/OVERNIGHTS             
         CLI   DBSELMED,C'O'             THEN RESET TO A STANDARD               
         BNE   *+8                       LOOKUP                                 
         CLI   DBBTYPE,C'P'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0                                                        
*                                                                               
* CHECK TO SEE IF OVERNIGHT                                                     
* IF SO CHECK BOOK AGAINST OVEFFBK AND SEE IF BOOK FOR THE BOOKTYPE             
* WE ARE POSTING AGAINST IS BEFORE THE AVAILABLE START BOOK                     
         CLI   DIGITALF,C'Y'                                                    
         BE    *+8                                                              
         CLI   OVPHOPT,C'Y'                                                     
         BNE   GD51H                                                            
         CLI   DBSELMED,C'O'                                                    
         BNE   GD51H                                                            
         OC    OVEFFBK,OVEFFBK                                                  
         BZ    GD51H                                                            
         CLC   DBSELBK,OVEFFBK       POST MONTHLY PRIOR TO EFFECTIVE BK         
         BNL   GD51H                                                            
* READ MONTHLY BOOK                                                             
         MVI   DBSELMED,C'T'                                                    
         XC    VPHHOMES,VPHHOMES                                                
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         XC    VPHFACT,VPHFACT                                                  
         MVI   NEXTYRLK,C'N'                                                    
         MVC   DBSELBK,SVSELBK                                                  
*                                                                               
GD51H    MVI   DEMANDSW,0                                                       
*                                                                               
         CLI   NSIREQ,C'Y'         ANY NSI DEMOS?                               
         JNE   GD54COMS                                                         
*                                                                               
* IF NEW METHODOLOGY PROFILE AND POSTING                                        
         MVI   DBNPMFLG,0                                                       
         IF (CLI,SVD0PROF+14,E,C'Y'),AND,                                       
            (CLI,DBVOPT,E,X'40'),AND,(CLI,DBSELMED,E,C'C')                      
         MVI   DBNPMFLG,DBNPMACH   SET FLAG TO NPM ACHEIVED                     
         BRAS  RE,FILLSPTB                                                      
         ENDIF                                                                  
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,GETDEMHK                                        
         CLI   DBSELMED,C'W'       WEEKLY JUST LOOKED UP                        
         BNE   POVPHX               NO - TREAT AS NORMAL                        
                                                                                
* CHECK TO SEE IF WE ARE POSTING  WEEKLY/LPMWK, OVERNIGHTS                      
         CLI   DIGITALF,C'Y'                                                    
         BE    GD53A                                                            
         CLI   OVPHOPT,C'Y'                                                     
         BE    GD52                                                             
         CLI   LPMWKOPT,C'Y'                                                    
         BE    GD52                                                             
         CLI   WVPHOPT,C'Y'                                                     
         BNE   POVPHX                                                           
*                                                                               
* SEE IF POSSIBLE SPLIT WEEK SCENARIO                                           
* WHERE PART OF THE SPLIT WEEK  WAS NOT READ SUCESSFULLY                        
*                                                                               
GD52     MVC   DBBEST,SVDBBEST     RESTORE DBBEST                               
         CLI   DBERROR,X'10'       IF WE GET BACK NOT FOUND ERROR               
         BNE   GD53                                                             
*                                                                               
         CLI   ESTEOW,X'01'        IF OOWR NOT M-SU WEEK JUST PASS              
         BH    GD52A               BACK WHAT WE GOT IN THE HOOK                 
*                                                                               
         TM    DBSELDAY,B'01111100' ATLEAST ONE DAY HAS TO BE ON                
         BZ    GD53                 BETWEEN M-F                                 
         CLI   DBSELDAY,X'03'       AND  EITHER SAT OR SUNDAY                   
         BZ    GD53                 ALSO ON                                     
*                                                                               
GD52A    CLI   DEMANDSW,C'Y'       PASS BACK WHAT WE GOT IN HOOK                
         BNE   GD53                                                             
         MVI   DBERROR,X'80'       RESET TO NO ERROR                            
*==================================================================             
* CHECK TO SEE IF WE WANT TO POST AGAINST OVERNIGHTS                            
*==================================================================             
                                                                                
GD53     CLI   SVD0PROF+11,C'M'    OVERNITES ACTIVE                             
         BE    *+8                                                              
         CLI   SVD0PROF+11,C'Y'    OVERNITES ACTIVE                             
         BNE   POVPHX               NO - TREAT AS NORMAL                        
         CLI   DEMANDSW,0          DID WE GET TO HOOK                           
         BNE   POVPHX                                                           
                                                                                
GD53A    MVI   DBSELMED,C'O'       NOTHING FOUND - TRY OVERNITE                 
         BRAS  RE,GETOVSWK                                                      
         MVC   DBSELBK,VPHSWK      OTHERWISE - TRY FOR OVERNIGHT                
         MVI   DEMANDSW,0          RESET SWITCH                                 
* TO REREAD FOR OVERNIGHTS - CLEAR IUN VALUES FROM  WHAT                        
* WAS STORED FROM WEEKLY READ IF ANY                                            
GD53B    LA    R0,4                RTGS/IMPS/PUTS/TOTS                          
         L     R1,ADTUNVS                                                       
         XC    0(IUNDEMS*4,R1),0(R1)                                            
         LA    R1,IUNDEMS*4(R1)                                                 
         BCT   R0,*-10                                                          
         B     PWVPHX                                                           
*==================================================================             
                                                                                
POVPHX   MVC   DBBEST,SVDBBEST     RESTORE DBBEST                               
*                                                                               
         CLI   DIGITALF,C'Y'       DIGITAL TRANSITION IF OVERNIGHT              
         BNE   POVPHXX             NOT FOUND - WE WANT JUL09                    
         CLI   DBSELMED,C'O'                                                    
         BNE   POVPHXX                                                          
         CLI   DBERROR,X'80'                                                    
         BNE   *+12                                                             
         CLI   DEMANDSW,0          DID WE GET TO HOOK                           
         BNE   GD54                                                             
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELBK,=AL2(JUL_09)                                             
         B     GD53B                                                            
*                                                                               
POVPHXX  DS    0C                                                               
                                                                                
* IF NEW METHODOLOGY PROFILE AND POSTING                                        
* AS LONG AS WE HAVE PARTIAL WEEKLY DATA FOUND- WE WANT TO GIVE BACK            
* A DEMO VALUE.                                                                 
                                                                                
         IF (CLI,SVD0PROF+14,E,C'Y'),AND,                                       
            (CLI,DBVOPT,E,X'40'),AND,(CLI,DBSELMED,E,C'C')                      
         IF (CLI,DEMANDSW,E,C'Y')                                               
         OC    DBDIVSOR,DBDIVSOR                                                
* SOMETIMES DEMAND DOESNT RETURN X'80 RETURN CODE.  SHOULD PROCEED              
* FOR NEW METHODLOGY IF WE GOT INTO HOOK                                        
* AND RETURN WHAT WE ACCUMULATED FOR MIDFLIGHT LOOKUPS WHERE SOME WEEKS         
* ARE NOT ON FILE                                                               
***      BNZ   GD53C                                                            
         BZ    GD53C                                                            
         CLI   DEMANDSW,0                                                       
         BNE   GD53D                                                            
         ENDIF                                                                  
         ENDIF                                                                  
*                                                                               
GD53C    CLI   DBERROR,X'80'       TEST EOF RETURN                              
         BNE   *+12                NO - ERROR!!                                 
         CLI   DEMANDSW,0          DID WE GET TO HOOK                           
         BNE   GD53D               YES                                          
*                                                                               
         L     RE,DBCOMFCS         IF ONLINE, RETURN ERROR!                     
         ICM   RE,15,CSWITCH-COMFACSD(RE)  NON-ZERO ONLINE ONLY                 
         JNZ   GDERR2                                                           
                                                                                
* WE GOT SOME SORT OF ERROR - GET BUYER ESTS ON PASS2 ONLY                      
                                                                                
         CLI   COMREQ,C'Y'         TEST COMSCORE REQUEST                        
         JNE   *+12                NO - RETURN BUYER ESTIMATES                  
         CLI   COMPASS,C'2'        TEST PASS 2                                  
         JNE   GD53D                                                            
GD53BYR  BRAS  RE,SETBYEST                                                      
         J     GD54COMS                                                         
*                                                                               
GD53D    CLI   DBBEST,C'M'                                                      
         BNE   *+10                                                             
         MVC   SVACTBK,DBACTBK                                                  
                                                                                
* EXTRACT REQUESTED DEMOS *   =SPGETDEMF GD69                                   
                                                                                
GD54     BAS   RE,GETIUN                                                        
*                                                                               
         MVC   LPMWKOPT,SVLPMWKOPT RESTORE ORIGINAL OPTIONS                     
         MVC   WVPHOPT,SVWVPHOPT                                                
*                                                                               
GD54COMS DS    0C                                                               
         CLI   COMPASS,C'1'        TEST COMSCORE REQUEST                        
         JL    GD56                                                             
         CLI   DBSELSRC,C'N'                                                    
         JE    *+12                                                             
         CLI   DBSELSRC,C'F'                                                    
         JNE   GD56                                                             
         CLI   DBVOPT,X'40'                                                     
         JNE   GD56                                                             
*                                                                               
         CLI   COMREQ,C'Y'         TEST ANY COMSCORE DEMOS                      
         JNE   GD56                                                             
*                                                                               
         CLI   AFDSW,C'Y'          ARE WE DOING AFFIDS                          
         JNE   *+14                NO                                           
         OC    NOAFDCNT,NOAFDCNT   ARE WE HERE DUE TO AFFID ERRORS              
         JZ    GD56                NO                                           
*                                                                               
         MVI   COMTYPE,C'A'        SET FOR ACHIEVED                             
         BRAS  RE,COMSCORE                                                      
         CLI   COMERRF,0                                                        
         BE    GD56                                                             
         MVI   DBERROR,COMSCERR    SET COMSCORE ERROR                           
         MVI   COMERROR,COMSCERR   SET FLAG HERE TO REMEMBER LATER              
*                                                                               
GD56     CLI   COMREQ,C'Y'                                                      
         JNE   *+12                                                             
         CLI   COMPASS,C'1'                                                     
         JE    GD99                                                             
                                                                                
* SET BUY VALUES = DAY TIME VALUES *                                            
                                                                                
         MVC   BUDEMSA,DTDEMSA                                                  
         MVC   BUDEMSU,DTDEMSU                                                  
         B     GD70                                                             
         EJECT                                                                  
************************************************                                
* ORBIT PRESENT - LOOK UP VALUES FOR EACH SHOW *                                
*      EACH SHOW COUNTS EQUALLY IN AVERAGE     *                                
************************************************                                
                                                                                
GD60     DS    0H                                                               
         SR    R7,R7                                                            
         IC    R7,1(R6)                                                         
         SRL   R7,4                DIVIDE BY 16 AND TRUNCATE                    
         LA    R6,4(R6)                                                         
*                                                                               
GD62     MVC   DBSELDAY,0(R6)                                                   
         MVC   DBSELTIM,1(R6)                                                   
         MVC   DBTAPEP,TAPEOPT     TAPE/BOOK PRECISION                          
                                                                                
         CLI   DBSELMED,C'R'       IF RADIO,                                    
         BNE   *+8                                                              
         MVI   DBBEST,C'P'          SUPPORT OVERNIGHT DAYPART                   
                                                                                
         MVI   DEMANDSW,0                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,GETDEMHK                                        
         CLI   DBERROR,X'80'       TEST EOF RETURN                              
         BNE   GDERR1                                                           
         CLI   DEMANDSW,0          DID WE GET TO HOOK                           
         BE    GDERR2              NO                                           
         CLI   DBBEST,C'M'                                                      
         BNE   *+10                                                             
         MVC   SVACTBK,DBACTBK                                                  
                                                                                
* EXTRACT REQUESTED DEMOS *                                                     
                                                                                
         BAS   RE,GETIUN                                                        
         EJECT                                                                  
* ADD TO ORBIT TOTALS AND INCREASE WEIGHT BY 1 *                                
                                                                                
         L     RF,ORBWGT                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ORBWGT                                                        
*                                                                               
         LA    R0,MAXDEMS*2        SET FOR BOTH ADJ AND UNADJ                   
         SR    R1,R1                                                            
GD64     L     RE,DTDEMSA(R1)                                                   
         A     RE,ORBDEMSA(R1)                                                  
         ST    RE,ORBDEMSA(R1)                                                  
         LA    R1,4(R1)                                                         
         BCT   R0,GD64                                                          
*                                                                               
         XC    DTDEMSA,DTDEMSA                                                  
         XC    DTDEMSU,DTDEMSU                                                  
*                                                                               
         LA    R6,16(R6)           NEXT ORBIT ENTRY                             
         BCT   R7,GD62                                                          
                                                                                
* GET ADJUSTMENT FACTORS (BEFORE AVERAGING) *                                   
                                                                                
         LA    R1,ORBDEMSA                                                      
         BRAS  RE,GDSVI                                                         
                                                                                
* DIVIDE TOTALS BY NUMBER OF SHOWS TO GET AVERAGE DEMOS *                       
                                                                                
         L     R0,ORBWGT                                                        
         LA    R1,ORBDEMSA                                                      
         BRAS  RE,GDUNWGT                                                       
                                                                                
* RECALUCULATE ADJ VALUES = UNADJ VALUES X SVI'S *                              
                                                                                
         LA    R1,ORBDEMSA                                                      
         BRAS  RE,GDXSVI                                                        
                                                                                
* AND SET BUY VALUES                                                            
                                                                                
         MVC   BUDEMSA,ORBDEMSA                                                 
         MVC   BUDEMSU,ORBDEMSU                                                 
         EJECT                                                                  
GD70     DS    0H                                                               
         CLI   AFDSW,C'Y'          TEST AFFIDS LOOKED UP                        
         BNE   GD80                NO                                           
                                                                                
* WEIGHT BUY DEMOS BY NUMBER OF SPOTS AND ADD TO AFFID TOTALS *                 
* REMEMBER COMSCORE DEMOS ARE 0 IN BUDEMS                                       
                                                                                
         LA    R0,MAXDEMS*2                                                     
         SR    R1,R1                                                            
*                                                                               
GD72     L     RE,BUDEMSA(R1)                                                   
         MH    RE,NOAFDCNT                                                      
         A     RE,AFDEMSA(R1)                                                   
         ST    RE,BUDEMSA(R1)                                                   
         LA    R1,4(R1)                                                         
         BCT   R0,GD72                                                          
                                                                                
* NOW GET SVI VALUES *                                                          
                                                                                
         LA    R1,BUDEMSA                                                       
         BRAS  RE,GDSVI                                                         
         EJECT                                                                  
         L     R1,SAVER1                                                        
         CLC   20(4,R1),=F'0'      TEST ADDRESS PASSED                          
         BE    GD78                NO - IGNORE                                  
                                                                                
* FORMAT TOTAL AREA WITH DEMO NUM/VALUE/SVI *                                   
                                                                                
         L     R1,SAVER1                                                        
         L     R1,20(R1)           POINT TO TOTAL AREA                          
         LA    R4,24(R3)           POINT TO FIRST DEMO                          
         ZIC   R5,1(R3)                                                         
*        SH    R5,=H'24'                                                        
         SHI   R5,24                                                            
         LA    R6,BUDEMSU          POINT TO FIRST UNADJ DEMO VALUE              
         LA    R7,SVIS             POINT TO FIRST SVI                           
                                                                                
         SRL   R5,3                                                             
GD72A    XC    0(12,R1),0(R1)      CLEAR SLOT                                   
         MVC   0(3,R1),0(R4)       DEMO NUM                                     
         MVC   4(4,R1),4(R4)       MOVE OLD DEMO VALUE                          
         MVC   11(1,R1),3(R4)      MOVE OLD SVI                                 
*                                                                               
         CLI   1(R1),63            TEST WEIGHTED DEMO                           
         BE    GD72B               YES - NOT IN LIST                            
*                                                                               
         MVC   4(4,R1),0(R6)       DEMO VALUE                                   
         MVC   8(4,R1),0(R7)       SVI VALUE                                    
*                                                                               
         LA    R6,4(R6)            NEXT DEMO VALUE                              
         LA    R7,4(R7)            NEXT SVI VALUE                               
*                                                                               
GD72B    LA    R4,8(R4)            NEXT DEMO IN ELEM                            
         LA    R1,12(R1)           NEXT SLOT                                    
         BCT   R5,GD72A                                                         
         XC    0(12,R1),0(R1)      CLEAR NEXT TOTAL SLOT                        
         EJECT                                                                  
* FIND ORIGINAL DEMO ELEMENT (IN BUYREC) *                                      
                                                                                
         LA    R6,BDELEM                                                        
         MVC   ELCODE,0(R3)                                                     
         NI    ELCODE,X'0F'                                                     
*                                                                               
GD73     BRAS  RE,NEXTEL                                                        
         BNE   GD78                ORIG DEMEL NOT FOUND - IGNORE                
         CLI   ELCODE,2                                                         
         BE    GD73X                                                            
         CLC   4(2,R3),4(R6)       MATCH SPILL MARKET NUMBER                    
         BNE   GD73                                                             
*                                                                               
GD73X    LH    RE,AFDCNT           GET TOTAL SPOT COUNT                         
         AH    RE,NOAFDCNT                                                      
         STH   RE,WORK             SAVE TOTAL SPOTS IN WORK(2)                  
                                                                                
* R3 POINTS TO DEMO ELEM IN PROCESS/ R6 TO ORIGINAL DEMO ELEM *                 
                                                                                
         L     R1,SAVER1                                                        
         L     R1,20(R1)           POINT TO TOTAL AREA                          
         LA    R5,PBDEMVAL                                                      
         EJECT                                                                  
*=======================================================*                       
* IF '-S' OPTION AND MANUAL OVERRIDES PRESENT, MULTIPLY *                       
* ORIGINAL DEMO OVERRIDES BY NUMBER OF SPOTS AND RETURN *                       
* VALUES IN TOTAL AREA                                  *                       
* DO THIS ALSO IF USER DEMOS ARE PRESENT - STUPID       *                       
* USE POST BUY DEMO OVERRIDES IF THEY ARE PRESENT       *                       
*=======================================================*                       
                                                                                
GD74     CLI   WKLYOPT,X'80'       TEST WEEKLY OPT WITH ALL DATA                
         BE    *+12                YES                                          
         CLI   BDPROGT-1,0         TEST '-S'                                    
         BE    GD75                                                             
         CLI   1(R1),X'21'         TEST USER DEMO                               
         BNE   GD75A               NO - GO ON                                   
                                                                                
GD75     BRAS  RE,GDTOT            FIND DEMO IN ORIGINAL ELEM                   
                                                                                
GD75A    CLI   PBDEMSW,C'Y'        TEST POST BUY DEMO OVERRIDES                 
         BNE   GD75X                                                            
         CLI   PBLKERR,C'Y'        YES-TEST LOOKUP ERROR                        
         BE    *+12                YES-ALWAYS USE VALUE IN PB DEMO LIST         
         TM    1(R5),X'80'         NO-ONLY USE GENUINE OVERRIDE                 
         BZ    GD75X                                                            
         SR    R0,R0                                                            
         ICM   R0,7,2(R5)          GET VALUE WITHOUT FLAGS                      
         MH    R0,WORK             X SPOTS                                      
         ST    R0,4(R1)            STORE IN TOTAL AREA                          
         MVC   11(1,R1),0(R5)      AND SVI                                      
                                                                                
GD75X    LA    R1,12(R1)           NEXT SLOT IN TOTAL AREA                      
         LA    R5,5(R5)                                                         
         OC    0(4,R1),0(R1)                                                    
         BNZ   GD74                                                             
                                                                                
* DIVIDE TOTALS BY NUMBER OF SPOTS TO GET AVERAGE DEMOS *                       
                                                                                
GD78     LH    R0,AFDCNT                                                        
         AH    R0,NOAFDCNT                                                      
         LA    R1,BUDEMSA                                                       
         BRAS  RE,GDUNWGT                                                       
                                                                                
* RECALUCULATE ADJ VALUES = UNADJ VALUES X SVI'S *                              
                                                                                
         LA    R1,BUDEMSA                                                       
         BRAS  RE,GDXSVI                                                        
         EJECT                                                                  
* SET ACTUAL BOOK AND PROGRAM NAME *                                            
                                                                                
GD80     DS    0H                                                               
*                                                                               
         MVC   2(2,R3),SVACTBK     SET ACTUAL BOOK IN NDELEM                    
*                                                                               
GD80A    CLI   0(R3),3             TEST SPILL DEMEL                             
         BE    GD82                YES - DO NOT RETURN PROGRAM                  
         CLI   0(R3),X'13'                                                      
         BE    GD82                                                             
*                                                                               
         TM    DBVOPT,X'40'        SPOT POSTING?                                
         BZ    GD80B                                                            
         OC    STNAMEOV,STNAMEOV   USE OVERNIGHTS PROGRAM NAMES                 
         BZ    GD80B                                                            
         MVC   STNAME,STNAMEOV                                                  
         MVC   ENDNAME,ENDNAMEO                                                 
GD80B    DS    0H                                                               
*                                                                               
         MVC   4(15,R3),STNAME                                                  
         CLC   STNAME,ENDNAME      TEST START/END NAMES EQUAL                   
         BE    *+14                                                             
         MVI   11(R3),C'/'         IF NOT, RETURN HALF OF EACH                  
         MVC   12(7,R3),ENDNAME                                                 
*                                                                               
         CLI   BDPROGT-1,0         TEST '-S'                                    
         BNE   GD82                                                             
         CLI   WKLYOPT,X'80'       TEST WKLY OPT SUCCESSFUL                     
         BE    GD82                                                             
         MVC   4(15,R3),=CL15'*SPECIAL*'                                        
                                                                                
* DIVIDE ADJ DEMS BY UNADJ DEMS TO GET ACTUAL SVI VALUES *                      
                                                                                
GD82     LA    R1,BUDEMSA                                                       
         BRAS  RE,GDSVI                                                         
                                                                                
* SET ALL SVI VALUES = 100 AS DEFAULT *                                         
                                                                                
         LA    R6,24(R3)                                                        
         ZIC   R0,1(R3)                                                         
*        SH    R0,=H'24'                                                        
         SHI   R0,24                                                            
                                                                                
         SRL   R0,3                                                             
         MVI   3(R6),100                                                        
         LA    R6,8(R6)                                                         
         BCT   R0,*-8                                                           
                                                                                
* NOW RETURN DEMO VALUES IN DEMO ELEMENT *                                      
                                                                                
         LA    R1,DEMOLIST                                                      
         LA    R5,PBDEMVAL                                                      
         LA    R6,24(R3)                                                        
         LA    R7,SVIS                                                          
         LA    R8,BUDEMSU                                                       
         EJECT                                                                  
* NOTE SPECIAL CODE HERE TO HANDLE SITUATIONS WHERE DEMO 1 *                    
* IS NOT LOOKED UP WHEN IT IS A USER OR WEIGHTED DEMO      *                    
                                                                                
         CLI   1(R6),63            TEST WEIGHTED DEMO                           
         BE    GD98                YES - SKIP (NOT IN LIST)                     
**NOP    CLI   1(R6),X'21'         TEST USER DEMO                               
**NOP    BE    GD98                YES - SKIP (NOT IN LIST)                     
         LA    RE,NDACTL-NDELEM(R3)                                             
         MVC   0(2,RE),BUDEMSU+2   SET ACTUAL FIRST DEMO                        
*                                                                               
GD92     CLI   1(R6),63            TEST WEIGHTED DEMO                           
         BE    GD98                YES - SKIP (NOT IN LIST)                     
**NOP    CLI   1(R6),X'21'         TEST USER DEMO                               
**NOP    BE    GD98                YES - SKIP (NOT IN LIST)                     
*                                                                               
         CLI   PBDEMSW,C'Y'        TEST ANY POST BUY DEMO OVERRIDES             
         BNE   GD94                                                             
         CLI   PBLKERR,C'Y'        YES-TEST LOOKUP ERROR                        
         BE    *+12                YES-ALWAYS USE VALUE IN PB DEMO LIST         
         TM    1(R5),X'80'         NO-ONLY USE GENUINE OVERRIDE                 
         BZ    GD94                                                             
         MVC   3(5,R6),0(R5)       MOVE SVI AND VALUE                           
         B     GD94C                                                            
*                                                                               
GD94     DS    0H                                                               
         CLI   COMPASS,C'1'        TEST COMSCORE REQUEST                        
         JNL   GD94B               YES                                          
* NOT A COMSCORE DEMO                                                           
         CLI   0(R6),0             TEST NORMALIZED COMSCORE DEMO                
         JNE   GD96                                                             
         CLI   2(R6),0             TEST COMSCORE DEMO                           
         BE    GD96                YES - DO NOT OVERWRITE                       
*                                                                               
GD94B    TM    4(R6),X'80'         TEST OVERRIDE                                
         BO    GD96                YES - SKIP (IN LIST)                         
*                                                                               
         MVC   4(4,R6),0(R8)       SET DEMO VALUE                               
         MVC   3(1,R6),3(R7)       SET SVI                                      
                                                                                
* REMEMBER COMSCORE DEMOS MAY BE NORMALIZED!                                    
                                                                                
GD94C    CLI   0(R6),0             TEST NORMALIZED COMSCORE DEMO                
         JE    GD94D               NO                                           
         LR    RE,R6                                                            
         J     GD94E                                                            
*                                                                               
GD94D    CLI   2(R6),0             TEST COMSCORE DEMO                           
         JNE   GD94F               NO                                           
         LLC   RE,1(R6)            GET DEMO NUMBER                              
         BCTR  RE,0                                                             
         MHI   RE,9                                                             
         LA    RE,2(RE)                                                         
         A     RE,AD50EL                                                        
*                                                                               
GD94E    CLI   0(RE),C'R'                                                       
         JE    GD94G                                                            
         CLI   0(RE),C'E'                                                       
         JE    GD94G                                                            
         J     GD94H                                                            
*                                                                               
GD94F    CLI   1(R6),C'R'          TEST RATING                                  
         BE    GD94G                                                            
         CLI   1(R6),C'E'          OR EXTENDED RATING                           
         BNE   GD94H                                                            
*                                                                               
GD94G    TM    TWODEC,X'41'        TEST 2-DEC RTG OR IMP REQUEST?               
         JZ    *+8                  YES, SET 2-DEC FLAG                         
         OI    4(R6),X'40'         SET 2-DEC FLAG                               
         J     GD95                                                             
* NOT A RATING                                                                  
GD94H    TM    TWODEC,X'01'        TEST 2-DEC IMP REQ                           
         JZ    *+8                                                              
         OI    4(R6),X'40'         SET 2-DEC FLAG                               
*                                                                               
GD95     CLI   3(R6),0             CHECK SVI IS NON-ZERO                        
         BNE   *+8                                                              
         MVI   3(R6),100                                                        
*                                                                               
GD96     LA    R1,3(R1)            NEXT DEMO LIST ENTRY                         
         LA    R7,4(R7)            NEXT SVI                                     
         LA    R8,4(R8)            NEXT DEMO VALUE                              
*                                                                               
GD98     LA    R6,8(R6)            NEXT DEMO IN ELEM                            
         LA    R5,5(R5)                                                         
         CLI   0(R1),X'FF'         TEST E-O-L                                   
         BNE   GD92                                                             
         EJECT                                                                  
* CALCULATE WEIGHTED DEMO IF REQUIRED *                                         
                                                                                
         OC    SAVEWGTS,SAVEWGTS   TEST ANY WEIGHTS PRESENT                     
         BZ    GD99                NO - SKIP WEIGHTED DEMO CALC                 
         BRAS  RE,GETWD                                                         
                                                                                
GD99     L     R1,SAVER1                                                        
         CLI   4(R1),1             TEST SPILL + ORIG REQUEST                    
         JNE   EXIT                                                             
         SR    R0,R0                                                            
*                                                                               
GD99A    IC    R0,1(R3)                                                         
         AR    R3,R0               POINT TO NEXT BUYREC ELEMENT                 
         CLI   0(R3),X'22'         IGNORE POST BUY DEMO ELEMENTS                
         BE    GD99A                                                            
         CLI   0(R3),X'23'                                                      
         BE    GD99A                                                            
         CLI   0(R3),X'13'                                                      
         BE    *+12                                                             
         CLI   0(R3),3             TEST SPILL DEMEL                             
         JNE   EXIT                                                             
                                                                                
* SET UP FOR SPILL DEMO LOOK-UP *                                               
                                                                                
         ST    R3,ADDEMEL                                                       
NDEL     USING NDELEM,R3                                                        
*                                                                               
         MVI   SVISW,0             RESET SVI SWITCH                             
         MVI   DBMODE,DBMFRST      RESET DEBLOCK                                
         XC    DBDIVSOR,DBDIVSOR                                                
         XC    DBACTUAL,DBACTUAL                                                
         XC    SAVEWGTS,SAVEWGTS    NO WEIGHTED DEMO FOR SPILL                  
*                                                                               
         CLI   DBSELMED,C'R'        FOR RADIO                                   
         BE    *+10                 EVERY MARKET FOR HIMSELF                    
         MVC   DBSELBK,SVACTBK      FORCE SPILL BOOK = ORIG MKT BOOK            
*                                                                               
         MVC   DBSELUMK,NDEL.NDAGYMKT    SET AGY MKT CODE                       
         MVC   DBSELMK,NDEL.NDRSMKT      SET RTG SVC MKT CODE                   
         MVC   DBBTYPE,NDEL.NDBKTYPE     BOOKTYPE OVERRIDE                      
         CLI   DBBTYPE,C'O'        KILL OLYMPICS FROM NDELEM                    
         BNE   *+8                                                              
         MVI   DBBTYPE,0                                                        
*                                                                               
         CLI   DBSELMED,C'R'       TEST RADIO SPILL                             
         BNE   *+10                                                             
         MVC   DBSELALF,NDEL.NDMKTALF                                           
*                                                                               
         CLI   DBSELMED,C'C'       TEST CANADA                                  
         BNE   GD99B                                                            
*                                                                               
         MVI   DBSELSRC,C'N'                                                    
         TM    NDEL.NDRTGSVC,X'01' TEST BBM                                     
         BZ    *+8                                                              
         MVI   DBSELSRC,C'A'                                                    
*                                                                               
         MVC   DBSELALF,NDEL.NDMKTALF                                           
         CLI   NDEL.NDSTA,C'A'     TEST FOR STATION OVERRIDE                    
         BL    *+10                NO                                           
         MVC   DBSELSTA,NDEL.NDSTA                                              
         DROP  NDEL                                                             
*                                                                               
GD99B    XC    DTDEMSA,DTDEMSA                                                  
         XC    DTDEMSU,DTDEMSU                                                  
         XC    ORBDEMSA,ORBDEMSA                                                
         XC    ORBDEMSU,ORBDEMSU                                                
         XC    AFDEMSA,AFDEMSA                                                  
         XC    AFDEMSU,AFDEMSU                                                  
         XC    BUDEMSA,BUDEMSA                                                  
         XC    BUDEMSU,BUDEMSU                                                  
*                                                                               
         L     R0,ADTUNVS          CLEAR DTUNVS/DTDEMS                          
         LHI   R1,DTDEMX-DTUNVS                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   DBSELMED,C'R'       IF RADIO,                                    
         BNE   GD20LP              NO - GO REBUILD DEMOLIST NOW                 
         L     RE,ARADEMSA         RADIO AREAS                                  
         L     RF,ARADEMSU                                                      
         XC    0(L'RADEMSA,RE),0(RE)   CLEAR THESE FIELDS ALSO                  
         XC    0(L'RADEMSU,RF),0(RF)                                            
         B     GD20LP              GO REBUILD DEMOLIST                          
         EJECT                                                                  
*================================================================               
* AFFIDAVIT LOOK-UPS                                                            
*================================================================               
                                                                                
GD100    DS    0H                                                               
         MVI   AFDSW,C'Y'          SET AFFIDAVIT LOOK-UP SWITCH                 
         TM    AFDOPTS,X'80'       TEST 7 MINUTE OPTION                         
         BZ    GD102               NO                                           
         BRAS  RE,SET7                                                          
*                                                                               
GD102    MVC   AFDPRD,18(R3)       SET AFFID PRD CODE                           
         MVC   AFDSLN,19(R3)       SET AFFID SLN                                
         MVC   AFDSDT,20(R3)       SET PERIOD START DATE                        
         MVC   AFDEDT,22(R3)       AND END DATE                                 
*                                                                               
         CLI   BUYKEY+3,X'FF'      TEST POL BUY                                 
         BNE   GD130                                                            
                                                                                
* POL AFFIDAVIT PROCESSING *                                                    
                                                                                
         LA    R6,BDELEM                                                        
GD110    SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
GD112    CLI   0(R6),0                                                          
         BE    GD150                                                            
         CLI   0(R6),X'0B'                                                      
         BL    GD110                                                            
         CLI   0(R6),X'0D'                                                      
         BH    GD110                                                            
         CLI   1(R6),10            TEST UNALL                                   
         BNH   GD110                                                            
         TM    6(R6),X'C4'         TEST HIATUS,MINUS,MINUSSED                   
         BNZ   GD110                                                            
                                                                                
* TEST IN REQUEST PERIOD *                                                      
                                                                                
         SR    R7,R7                                                            
         IC    R7,1(R6)                                                         
         AR    R7,R6               POINT TO NEXT ELEM                           
         CLI   0(R7),X'10'         IS IT AN AFFID                               
         BE    *+6                                                              
         LR    R7,R6               ELSE POINT TO REGEL DATE                     
         CLC   2(2,R7),AFDSDT                                                   
         BL    GD110                                                            
         CLC   2(2,R7),AFDEDT                                                   
         BH    GD110                                                            
*                                                                               
         LA    R1,BDSEC                                                         
         CLI   AFDPRD,X'FF'        TEST POL REQ                                 
         BE    GD114               YES                                          
         LA    R1,11(R6)           POINT TO PRD 1 SLN                           
         CLC   AFDPRD,10(R6)       MATCH PRD 1                                  
         BE    GD114                                                            
         CLI   1(R6),18            TEST P/B SPOT                                
         BL    GD110               NO                                           
         LA    R1,15(R6)           POINT TO PRD 2 SLN                           
         CLC   AFDPRD,14(R6)                                                    
         BNE   GD110                                                            
         EJECT                                                                  
GD114    CLI   AFDSLN,0            TEST ALL SLN REQ                             
         BE    *+14                                                             
         CLC   AFDSLN,0(R1)                                                     
         BNE   GD110                                                            
                                                                                
* TEST FOR AFFIDAVIT *                                                          
                                                                                
GD116    ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    GD120                                                            
         CLI   0(R6),X'0B'         TEST REGEL (0B-0D)                           
         BL    GD116                                                            
         CLI   0(R6),X'0D'                                                      
         BNH   GD120               YES - NO AFFID                               
         CLI   0(R6),X'10'                                                      
         BNE   GD116                                                            
                                                                                
* AFFID PRESENT - ADD TO DAY/TIME LIST AND COUNTER *                            
                                                                                
GD118    BRAS  RE,AFDBLD           MOVE TO OWN BASE ROUTINE                     
         LH    RE,AFDCNT                                                        
         LA    RE,1(RE)                                                         
         STH   RE,AFDCNT                                                        
         B     GD110                                                            
                                                                                
* NO AFFID - ADD TO COUNTER *                                                   
                                                                                
GD120    CLI   SAVENTI,C' '        TEST NTI LOOKUP                              
         BH    GD118               YES - ALWAYS BUILD AFDLIST ENTRY             
*                                                                               
         LH    RE,NOAFDCNT                                                      
         LA    RE,1(RE)                                                         
         STH   RE,NOAFDCNT                                                      
         B     GD112               NOTE - POINTING AT NEXT REGEL                
         EJECT                                                                  
* NON-POL AFFIDAVIT PROCESSING *                                                
                                                                                
GD130    CLI   BDTIME,0            TEST PIGGYBACK RECORD                        
         BE    GD140               NO                                           
                                                                                
* FIND PBELEM *                                                                 
                                                                                
         LA    R6,BDELEM                                                        
         MVI   ELCODE,4                                                         
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,1(R6)                                                         
         SRDA  R0,32                                                            
         D     R0,=F'7'                                                         
         LR    R0,R1               SET R1 FOR BCT                               
         LA    R6,2(R6)            POINT TO PTR 1                               
*                                                                               
GD132    LA    R1,2(R6)            POINT TO SLN (TIME SHARE)                    
         CLC   AFDPRD,0(R6)        TEST RIGHT PRD                               
         BE    GD134                                                            
         LA    R6,7(R6)                                                         
         BCT   R0,GD132                                                         
                                                                                
* MUST BE PROCESSING ACTIVE PARTNER *                                           
                                                                                
         LA    R1,BDTIME                                                        
         CLC   AFDPRD,BUYREC+3                                                  
         BE    GD134                                                            
         DC    H'0'                                                             
*                                                                               
GD134    CLI   AFDSLN,0            TEST ALL SLN REQUEST                         
         BE    GD140                                                            
         CLC   AFDSLN,0(R1)        TEST RIGHT SLN                               
         JNE   EXIT                                                             
         EJECT                                                                  
GD140    LA    R6,BDELEM                                                        
*                                                                               
GD141    SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
GD142    CLI   0(R6),0                                                          
         BE    GD150                                                            
         CLI   0(R6),6                                                          
         BL    GD141                                                            
         CLI   0(R6),8                                                          
         BH    GD141                                                            
                                                                                
* TEST ELEMENT IN REQUESTED PERIOD *                                            
                                                                                
         MVI   INPERIOD,C'N'                                                    
         CLC   2(2,R6),AFDSDT      TEST BEFORE PERIOD START                     
         BL    GD144                                                            
         CLC   2(2,R6),AFDEDT      OR AFTER PERIOD END                          
         BH    GD144                                                            
*                                                                               
         MVI   INPERIOD,C'Y'                                                    
         ZIC   R0,7(R6)            GET NUMBER OF SPOTS                          
         TM    6(R6),X'80'         TEST MINUS SPOT                              
         BZ    *+6                                                              
         LCR   R0,R0                                                            
         AH    R0,NOAFDCNT                                                      
         STH   R0,NOAFDCNT                                                      
                                                                                
* NOW LOOK FOR AFFIDAVITS FOR THIS ELEMENT *                                    
                                                                                
GD144    ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    GD150                                                            
         CLI   0(R6),6             TEST REGEL (06-08)                           
         BL    GD144                                                            
         CLI   0(R6),8                                                          
         BNH   GD142               YES - NO MORE AFFIDS                         
         CLI   0(R6),X'10'                                                      
         BNE   GD142               END OF AFFIDS FOR THIS ELEMENT               
* TEST AFFID IN REQUESTED PERIOD                                                
         CLC   2(2,R6),AFDSDT      TEST BEFORE PERIOD START                     
         BL    GD146                                                            
         CLC   2(2,R6),AFDEDT      OR AFTER PERIOD END                          
         BH    GD146                                                            
                                                                                
* AFFID PRESENT - ADD TO DAY TIME LIST *                                        
                                                                                
****     BAS   RE,AFDBLD                                                        
         BRAS  RE,AFDBLD           MOVE TO OWN BASE ROUTINE                     
*                                                                               
         LH    RE,AFDCNT           ADD TO AFFID COUNT                           
         AHI   RE,1                                                             
         STH   RE,AFDCNT                                                        
*                                                                               
GD146    CLI   INPERIOD,C'Y'       IF NOT IN PERIOD                             
         BNE   GD144               DO NOT ADJUST NOAFDCNT                       
         LH    R0,NOAFDCNT         REDUCE MISSING AFFID COUNT                   
         BCTR  R0,0                                                             
         STH   R0,NOAFDCNT                                                      
         B     GD144                                                            
         EJECT                                                                  
* PROCESS AFFIDAVIT DAY/TIME LIST *                                             
                                                                                
GD150    SR    R0,R0                                                            
         ICM   R0,3,AFDCNT         TEST ANY AFFIDAVIT SPOTS                     
         BZ    GD160               NO                                           
                                                                                
* SORT INTO MO/TU/.../FR/SAT/SUN/MO-FR SEQUENCE *                               
                                                                                
         L     RF,DBCOMFCS                                                      
         L     RF,CXSORT-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,AAFDLST,(R0),L'AFDLIST,L'AFDLIST,0                     
*                                                                               
         L     R4,AAFDLST          POINT TO SORTED LIST                         
         CLI   0(R4),0             FIRST SORTED ITEM SHOULD NOT BE 0            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   SVD0PROF+4,C'Y'     TEST LOOK UP LOWER QH FEATURE                
         BNE   GD151A                                                           
         GOTO1 =A(LQHSET),DMCB,(RC),RR=RELO  CHANGE QH TIMES AS REQ             
*                                                                               
GD151A   MVI   WKLYOPT,X'80'       SET WEEKLY LOOK-UP FLAG                      
         CLC   SV1WPR5,BDPROGRM    NO WEEKLY FOR THIS SPCL CHAR                 
         BE    GD151C                                                           
*                                                                               
         CLI   BDPROGT-1,0         TEST -S BUY                                  
         BNE   GD151B                                                           
         CLI   SV1WPR2,C'Y'        TEST -S BUYS GET WEEKLY                      
         BE    GD152               YES                                          
         B     GD151C              NO                                           
*                                                                               
GD151B   CLC   SV1WPR3,BDPROGRM    MATCH SPECIAL CHAR TO PROG NAME              
         BE    GD152                                                            
*                                                                               
         CLI   SV1WPR1,C'Y'        TEST ALL BUYS GET WEEKLY                     
         BE    GD152                                                            
*                                                                               
GD151C   MVI   WKLYOPT,0           UNSET WEEKLY LOOK-UP FLAG                    
*                                                                               
GD152    MVC   SVAFFMED,DBSELMED   SAVE ORIGINAL AFFIDS MEDIA CODE              
         MVC   SVSELBK,DBSELBK     SAVE ORIGINAL BOOK COMING IN                 
         MVC   DBSELDAY,0(R4)                                                   
         CLI   DBSELDAY,X'95'      TEST MO-FR                                   
         BNE   *+8                                                              
         MVI   DBSELDAY,X'40'      SET TO MONDAY                                
         MVC   DBSELTIM(2),1(R4)   SET START TIME ONLY                          
*                                                                               
         CLC   DBSELTIM(2),=X'FFFF'                                             
         BNE   *+16                                                             
         MVC   DBSELTIM,BDPROG     SET START/END TIMES                          
         MVC   DBSELDAY,0(R4)      AND SET THE DAYS AGAIN                       
*                                                                               
         XC    DBSEL1WK,DBSEL1WK   CLEAR WEEKLY REQUEST FIELDS                  
         MVI   DBSELWKN,0                                                       
         CLI   SV1WPROF+15,C'Y'    TRUE WEEKLY?                                 
         BE    GD152A                                                           
         TM    WKLYOPT,X'80'       TEST WEEKLY LOOK-UP                          
         BZ    *+10                NO                                           
GD152A   MVC   DBSEL1WK(2),3(R4)   SET WEEK DATE                                
         MVC   VPHSDAT(2),3(R4)    SET FOR WEEKLY VPH                           
         MVC   VPHEDAT(2),3(R4)    SET FOR WEEKLY VPH                           
         MVC   DBTAPEP,TAPEOPT     TAPE/BOOK PRECISION                          
*                                                                               
         CLI   SAVENTI,C' '        TEST NTI LOOKUP                              
         BNH   GD152D                                                           
* PREPARE TO CALL NETWEEK                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,3(R4)),WORK                                         
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     R0,CGETDAY-COMFACSD(RF)                                          
         ST    R0,DMCB+4                                                        
         L     R0,CADDAY-COMFACSD(RF)                                           
         ST    R0,DMCB+8                                                        
         CLC   WORK(6),=C'      '  HAVE A DATE                                  
         BNE   *+10                 YES - OK                                    
         MVC   WORK,=C'840101'      NO - FORCE A MISS                           
         GOTO1 VNETWEEK,DMCB,WORK                                               
*                                                                               
         MVC   DBSELBK(1),DMCB+4   MOVE YEAR                                    
         MVC   DBSELBK+1(1),DMCB+8 MOVE WEEK NUMBER                             
         XC    DBSEL1WK,DBSEL1WK   CLEAR WEEKLY REQUEST FIELDS                  
*                                                                               
GD152D   CLI   DBSELMED,C'R'       IF RADIO,                                    
         BNE   *+8                                                              
         MVI   DBBEST,C'P'          SUPPORT OVERNIGHT DAYPART                   
                                                                                
         DS    0H                  OLYMPICS EXCLUSION TAPE                      
         CLI   DBSELMED,C'T'        IS IT TV?                                   
**       BNE   OLYEXCFX              DON'T BOTHER IF IT ISN'T                   
         BNE   OLYEXCLX              DON'T BOTHER IF IT ISN'T                   
         CLI   DBSELSRC,C'F'        NO OLYMPIC EXCLUSION                        
***      BE    OLYEXCFX             FOR FUSION                                  
         BE    OLYEXCLX             FOR FUSION                                  
         CLI   SVD0PROF+7,C'J'      USE JULY?                                   
         BE    OLYEXCLB              YES                                        
         CLI   SVD0PROF+7,C'Y'      USE BOTH (FEB & JULY)?                      
         BE    OLYEXCLB              YES                                        
         B     OLYEXCLX                                                         
OLYEXCLB MVI   DBBEST,C'O'                                                      
OLYEXCLX EQU   *                                                                
OLYEXCF  DS    0H                                                               
                                                                                
         MVC   SVDBBEST,DBBEST                                                  
         BRAS  RE,TOMONTHL        DEFAULT WEEKLY TO MONTHLY LOOKUPS             
*                                 AFTER MMW CUTOFF DATE.                        
                                                                                
         CLC   =C'YNN',LPMWKOPT     IF USER REQUESTED LPMWK=Y ONLY              
         BNE   *+14                 AND WE ARE NOT DEALING WITH AN LPM          
         OC    LPMDTP,LPMDTP        MARKET, WE WANT TO READ MONTHLY             
         BZ    OLYWKX                                                           
                                                                                
         XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         MVC   DSTACALL,DBSELSTA                                                
                                                                                
* CHECK FOR POSTING OPTIONS                                                     
                                                                                
         CLI   LPMWKOPT,C'Y'      LPMWK                                         
         BE    AFFWTP1                                                          
         CLI   OVPHOPT,C'Y'       OVN ALWAYS FIGURE OUT THE BOOK TO             
         BE    AFFWTP1            READ FROM DATES PASSED IN                     
*                                 DONT ASSUME ACTBKG PASSED IN IS RIGHT         
***      CLI   WVPHOPT,C'Y'       WTP                                           
***      BNE   OLYWKX                                                           
         CLI   WVPHOPT,C'Y'                                                     
         BE    AFFWTP1                                                          
*                                                                               
* IF WE ARE POSTING MONTHLY -CHECK IF BOOK IS JUN09 FOR LPM MARKET              
* JUN09 IS THE DIGITAL TRANSITION WHERE NIELSEN HAS NOT RELEASED A              
* BOOK HENCE WE WANT TO TO OVERNIGHTS - IF NO OVERNIGHTS WE WANT TO             
* TO POST TO JUL_09                                                             
         MVI   DIGITALF,C'N'                                                    
         TM    DBVOPT,X'40'                                                     
         BNO   OLYWKX                                                           
         CLI   DBSELMED,C'T'                                                    
         BNE   OLYWKX                                                           
         CLI   DBSELSRC,C'N'                                                    
         BNE   OLYWKX                                                           
         CLC   DBSELBK,=AL2(JUN_09)                                             
         BNE   OLYWKX                                                           
         OC    LPMDTP,LPMDTP     ONLY FOR LPM MARKETS-NON LPM POST              
         BZ    OLYWKX            TO MONTHLY                                     
         OC    DBSELSYC,DBSELSYC  LPM CABLE JUN09 POST TO JUL09                 
         BNZ   AFFWTP0                                                          
         CLI   DBBTYPE,C'H'      HISPANIC/BLACK BOOKTYPES POST TO               
         BE    *+8               JUL09 BOOK                                     
         CLI   DBBTYPE,C'B'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'I'                                                     
         BE    *+8                                                              
         CLI   DBBTYPE,C'J'                                                     
         BNE   *+14                                                             
AFFWTP0  MVC   DBSELBK,=AL2(JUL_09)                                             
         B     OLYWKX                                                           
         MVI   DIGITALF,C'Y'     TURN ON DIGITAL TRANSITION FLAG                
                                                                                
*=================================================================              
* YOU SHOULD ONLY BE HERE IF YOU ARE POSTING  WEEKLY OR OVERNIGHTS              
*=================================================================              
         CLI   DBSELMED,C'T'                                                    
         BNE   OLYWKX                                                           
AFFWTP1  DS    0C                                                               
                                                                                
         CLI   DBBTYPE,0           ALWAYS ALLOW STANDARD                        
         BE    GD152DH                                                          
         CLI   LPMWKOPT,C'Y'       IF WE ARE POSTING WEEKLY THEN CHECK          
         BE    *+8                 BOOKTYPE INDICATOR TO SEE IF WEEKLY          
         CLI   WVPHOPT,C'Y'        IS AVAILABLE FOR THIS BOOKTYPE               
         BNE   *+12                IF ITS NOT AN AVAILABLE WEEKLY               
         TM    BKTYPIND,SPBKWKLY   BOOKTYPE THEN CHECK TO SEE IF                
         BNZ   GD152DH             OVERNIGHT POSTING OPTION IS ON               
         CLI   DIGITALF,C'Y'       IF DIGITAL TRANSITION WE DONT CARE           
         BE    GD152DH             BOOKTYPE IS AVAILABLE FOR OVERNIGHT          
         CLI   OVPHOPT,C'Y'                                                     
         BNE   GD152DG                                                          
         TM    BKTYPIND,SPBKOVN    BOOKTYPE AVAILABLE FOR OVERNIGHT?            
         BNZ   GD152DH                                                          
GD152DG  MVI   DBSELMED,C'T'       IF NOT AVAILABLE-GO READ MONTHLY             
         XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         B     OLYWKX                                                           
                                                                                
GD152DH  OC    DBSELSYC,DBSELSYC    CABLE POST AGAINST MONTHLY                  
         BNZ   OLYWKX                                                           
         OC    DBSELMK,DBSELMK                                                  
         BNZ   OLYWKX                                                           
                                                                                
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         OC    VPHSDAT,VPHSDAT                                                  
         BNZ   GD152E                                                           
         GOTO1 (RF),DMCB,(3,BDSTART),(2,VPHSDAT)                                
GD152E   GOTO1 (RF),DMCB,(2,VPHSDAT),(3,DUB)                                    
         MVC   SVSELBK,DBSELBK     SAVE AORIGINAL BOOK COMING IN                
*******************************************************************             
*  CONVERT THE BOOK TO GET THE CORRECT YEAR,,COULD BE END OF YR BUT             
*  ITS STILL CAN BE 1ST WEEK OF NEXT YR ON THE FILE                             
*******************************************************************             
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(3,DUB),(0,DUB3)                                       
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         MVC   WORK(12),DMCB+4                                                  
         GOTO1 =V(NSIWEEK),DMCB,DUB3,RR=RELO                                    
         MVC   DBSELBK(1),DMCB+4                                                
         MVC   DBSELBK+1(1),DMCB                                                
***************************************************************                 
*                                                                               
         TM    DBVOPT,X'40'                                                     
         BNO   GD152EX                                                          
         OC    VPHSDAT,VPHSDAT     NO DATE PASSED MUST NOT BE OVN               
         JZ    GD152EX                                                          
         CLI   OVPHOPT,C'Y'                                                     
         BNE   GD152EX                                                          
         BRAS  RE,SETMKTYP         SETS STABMKT                                 
         BRAS  RE,TRUELPM          IF NOT A TRUE LPM LPMDTP WILL CLEAR          
* CHECK FOR LPM MARKET                                                          
GD152EX  OC    LPMDTP,LPMDTP       ANY LPM DATE                                 
         BZ    GD152F              NO - USE WEEKLY HOMES TO ADJUST              
         CLC   DUB(2),LPMDTB       MONTH GREATER THAT LPM                       
         BL    GD152F                                                           
         CLI   DBBTYPE,C'P'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0           IF LPM - NO BOOKTYPE                         
         CLI   DBBTYPE,C'I'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,C'H'        IF LPM - NO BOOKTYPE                         
         MVI   MKTTYPE,MKTLPM                                                   
         B     WVPHLPM2            GET LPM DEMOS                                
GD152F   DS    0X                                                               
*----------------   NON LPM SECTION -------------------------------             
* AT THIS POINT- NOT LPM - CHECK TO SEE IF WE ARE READING SET METERED           
* MARKET.  IF SET METERED THEN PROCEED ELSE WE ARE DIARY MARKET                 
* DIARY MARKETS IGNORE  WTP AND OTP OPTIONS AND PROCEED TO READ                 
* BOOK OPTION OR ACT BOOK.                                                      
         OC    LPMDTP,LPMDTP                                                    
         BZ    GD152FF                                                          
                                                                                
         CLI   DIGITALF,C'Y'       DIGITAL TRANSITION GO TO                     
         BNE   *+12                OVERNIGHTS FOR LPM MARKETS                   
         MVI   DIGITALF,C'N'       ELSE WE WANT TO POST TO MONTHLY              
         B     OLYWKX                                                           
*                                                                               
         CLC   WVPHOPT(2),=C'NN'   OVN=Y OR WTP OPTION ASKED FOR?               
         BE    GD152G              NOT ASKED FOR GO TO MONTHLY                  
         MVI   MKTTYPE,MKTSETM                                                  
         B     GD152G                                                           
GD152FF  BRAS  RE,SETMKTYP          CALL ROUTINE TO GET MKT TYPE                
GD152G   CLI   MKTTYPE,MKTSETM      SET METERED ?                               
         BE    GD152GG                                                          
         MVI   WVPHOPT,C'N'         DIARY MKT READ MONTHLY                      
         MVI   OVPHOPT,C'N'                                                     
         XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         B     GD152N                                                           
GD152GG  MVI   MKTTYPE,MKTSETM                                                  
*-----------------------------------------------------------------              
*                                                                               
         CLI   DBSELMED,C'T'                                                    
         BNE   OLYWKX                                                           
*&&DO                                                                           
* THIS BOOKTYPE IS NOT NECESSARY                                                
*                                                                               
         CLI   DBBTYPE,C'P'        ALLOW P FOR LPM MARKETS                      
         BE    *+8                 FOR OVN,WEEKLY LOOKUPS                       
         CLI   DBBTYPE,0                                                        
         BNE   OLYWKX                                                           
*&&                                                                             
         OC    DBSELMK,DBSELMK                                                  
         BNZ   OLYWKX                                                           
         MVI   DBMODE,DBMFRST      RESET DBLOCK                                 
         XC    DBLASTS,DBLASTS                                                  
         XC    DBACTUAL,DBACTUAL                                                
         CLI   DBBTYPE,C'P'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0                                                        
*                                                                               
         MVI   DBSELMED,C'W'                                                    
*        MVI   DBSELBK+1,0         AND WEEK 0 SUMMARY RECORD                    
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,VPHSDAT),(3,DUB)                                    
         CLI   DUB,X'66'                                                        
         BNE   WVYX1                                                            
         CLC   DUB+1(2),=X'0C1B'                                                
         BL    WVYX1                                                            
         MVI   DUB,X'67'                                                        
         B     WVYX1A                                                           
WVYX1    DS    0C                                                               
         CLI   DUB,X'67'                                                        
         BNE   WVYX1A                                                           
         CLC   DUB+1(2),=X'0C1D'                                                
         BL    WVYX1A                                                           
         MVI   DUB,X'68'                                                        
WVYX1A   DS    0C                                                               
***      MVC   DBSELBK(1),DUB      SET BOOK YEAR                                
         MVI   DBSELBK+1,0         CLEAR BOOK MONTH READ ZERO RECORD            
*                                                                               
         XC    SVWTPDQH,SVWTPDQH                                                
         MVI   DAYREAD,0                                                        
         MVI   NEXTYRLK,C'N'                                                    
GD152H   MVI   AFFIDLK,C'Y'                                                     
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,GETVHOME                                        
         MVI   MISCOUNT,0          CLEAR MISSING DATA COUNT AND DAY             
         MVI   MISSDAY,0                                                        
                                                                                
* CHECK IF WE HAVE READ WEEKLY AND IF VPHHOMES IS SET                           
* IF LPMWK IS THE ONLY OPTION THEN  WE HAVE TO SET                              
* DMINDEX TO 'Y' SO THE DEMO GETS INDEXED LATER ON                              
                                                                                
         MVI   DMINDEX,C'N'                                                     
         CLC   LPMWKOPT,=C'YNN'                                                 
         BNE  *+8                                                               
         MVI   DMINDEX,C'Y'        SET FORCE DEMO INDEX FLAG                    
*                                                                               
**********OVERNIGHT LOGIC*************************************                  
*                                                                               
                                                                                
         CLC   =C'NY',WVPHOPT      OVN=Y ONLY ALWAYS GRAB                       
         BNE   *+16                NO - JUST LEAVE VPHHOMES ALONE               
         XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
                                                                                
         OC    VPHHOMES,VPHHOMES   IF ANY HOMES                                 
         BNZ   GD152N              IT'S OK                                      
*                                                                               
* NO VPHHOMES, CHECK TO SEE IF WTP OPTION IS ON                                 
*                                                                               
         CLI   WVPHOPT,C'Y'                                                     
         BNE   GD152I                                                           
         CLC   DBSELSTA,DSTACALL   DO WE HAVE A STATION LINK                    
         BE    GD152I              TO LOOK AT?                                  
         MVC   DBSELSTA,DSTACALL   LOOK FOR LINKED STATION                      
         MVC   DSTACALL,SVSELSTA     SWAP LINK AND  CALL LETTERS                
         MVI   DBERROR,0                                                        
         XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
         B     GD152H                                                           
                                                                                
*                                                                               
GD152I   DS    0H                                                               
         CLI   OVPHOPT,C'Y'        DO WE WANT OVERNIGHTS                        
         BNE   GD152M              GO TO ERROR FOR B.E                          
*                                                                               
         CLI   SMMMONTH,C'Y'       SET METER GO TO MONTHLY?                     
         JNE   GD152K                                                           
GD152J   XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
         XC    VPHHOMES,VPHHOMES                                                
         MVI   DAYREAD,0                                                        
         J     GD152N                                                           
*                                                                               
                                                                                
GD152K   MVI   DBSELMED,C'O'                                                    
         BRAS  RE,GETOVSWK                                                      
         MVC   DBSELBK,VPHSWK      WEEKLY BOOK                                  
         MVI   DBSELMED,C'O'       SET THE OVERNIGHT FLAG                       
         XC    SVWTPDQH,SVWTPDQH                                                
         MVI   DAYREAD,0                                                        
         MVI   TAPEOPT,C'Y'        OVERNIGHTS IS IMPRESSION BASED               
*&&DO                                                                           
*SHOULD WE BREAK AVERAGE QTR HOURS ?                                            
         CLI   SV1WPROF+12,0                                                    
         JE    GD152K1                                                          
         CLC   DBSELBK(1),SV1WPROF+12                                           
         BL    *+10                                                             
         CLC   DBSELBK+1(1),SV1WPROF+14                                         
         BL    *+8                                                              
         OI    DBNPMFLG,DBNOBAVG                                                
*&&                                                                             
*SHOULD WE BREAK AVERAGE QTR HOURS ?                                            
         NI    DBNPMFLG,X'FF'-DBNOBAVG                                          
         CLI   SV1WPROF+12,0                                                    
         JE    GD152K1                                                          
         ZIC   RE,SV1WPROF+12                                                   
         AHI   RE,100                                                           
         ZIC   R1,DBSELBK                                                       
         CR    R1,RE                                                            
         BL    GD152K1                     LOWER YEAR DONT TURN ON FLAG         
         BH    *+14                        HIGHER YEAR, TURN ON FLAG            
         CLC   DBSELBK+1(1),SV1WPROF+14    SAME YEAR/ CHECK MONTH               
         BL    *+8                                                              
         OI    DBNPMFLG,DBNOBAVG                                                
*                                                                               
* CHECK TO SEE IF OVERNIGHT BOOKTYPE IS ONLY AVAILABLE                          
* AFTER A CERTAIN EFFECTIVE BOOK - AS DEFINED BY DEDEMTABS SPBOOKTB             
* IF BOOK IS BEFORE EFFECTIVE DATE THEN KEEP POSTING AS MONTHLY                 
* SO OLD POSTS WILL REMAIN THE SAME                                             
GD152K1  OC    OVEFFBK,OVEFFBK                                                  
         BZ    GD152L                                                           
         CLC   DBSELBK,OVEFFBK   READ MONTHLY IF PRIOR TO EFFECTIVE BK          
         BL    GD152J                                                           
*                                                                               
* CALL DEMAND AND RETURN OVERNIGHT VALUE IN VPHHOMES                            
*                                                                               
GD152L   L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,GETOVHOM                                        
GD152M   CLI   DEMANDSW,0                                                       
         BE    GD152R                                                           
         OC    VPHHOMES,VPHHOMES                                                
         BNZ   GD152N                                                           
         MVI   DEMANDSW,0                                                       
         B     GD152R                                                           
                                                                                
                                                                                
GD152N   MVC   DBSELBK,SVSELBK                                                  
         MVI   DBSELMED,C'T'                                                    
*============================================================                   
OLYWKX   DS    0H                                                               
                                                                                
         CLI   DBBTYPE,C'O'        BE SURE TO RESET                             
         BNE   *+8                                                              
         MVI   DBBTYPE,0                                                        
         CLI   DBBTYPE,BOOKTYPE_OL BE SURE TO RESET                             
         BNE   *+8                                                              
         MVI   DBBTYPE,C'L'                                                     
                                                                                
         CLI   DBBTYPE,BOOKTYPE_O1 BE SURE TO RESET                             
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_L1                                              
                                                                                
         CLI   DBBTYPE,BOOKTYPE_OS BE SURE TO RESET                             
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_LS                                              
                                                                                
         CLI   DBBTYPE,BOOKTYPE_O3 BE SURE TO RESET                             
         BNE   *+8                                                              
         MVI   DBBTYPE,BOOKTYPE_L3                                              
*                                                                               
         CLI   DBBTYPE,C'L'        ALLOW LIVE ONLY AND LIVE+SAME DAY            
         BE    *+8                 AND LIVE+3 FOR OLYMPICS                      
         CLI   DBBTYPE,BOOKTYPE_L3                                              
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_L1                                              
         BE    *+8                                                              
         CLI   DBBTYPE,BOOKTYPE_LS                                              
         BE    *+8                                                              
         CLI   DBBTYPE,0                                                        
         BNE   OLYEXCFX                                                         
*                                                                               
         CLI   DBSELMED,C'R'       RADIO DO NO FALL DOWN TO OLYMPIC             
         BE    OLYEXCFX            CODE                                         
*                                                                               
         CLI   WVPHOPT,C'Y'        IF ASKING WEEKLY NON LPM THEN                
         BE    OLYEXCFX            DONT BOTHER W OLY EXCLUSION                  
******   CLI   OVPHOPT,C'Y'        IF ASKING OVERNIGHTS LPM THEN                
******   BE    OLYEXCFX            DONT BOTHER W OLY EXCLUSION                  
         CLI   SVD0PROF+7,C'Y'                                                  
         BE    *+12                                                             
         CLI   SVD0PROF+7,C'F'                                                  
         BNE   OLYEXCFX                                                         
*                                                                               
*                                                                               
*****    MVC   SVSELBK,DBSELBK                                                  
*****    BRAS  RE,SETMKTYP          CALL ROUTINE TO GET MKT TYPE                
         CLI   MKTTYPE,MKTSETM      SET METERED ?                               
         BNE   *+12                                                             
         CLI   SMMMONTH,C'Y'        IS MONTHLY OVNTS REQUESTED                  
         BE    OLYEXCF2             CAN GET OLYMPIC EXC                         
         CLI   OVPHOPT,C'Y'        IF ASKING OVERNIGHTS LPM THEN                
         BE    OLYEXCFX            DONT BOTHER W OLY EXCLUSION                  
*                                                                               
*                                                                               
*&&DO                                                                           
         CLC   DBSELBK,=X'6202'                                                 
         BNE   OLYEXCF2                                                         
*        CLC   SVBSTART+3(3),=X'620204'                                         
         CLC   DBSEL1WK(2),=X'C444' 980204                                      
         BH    *+8                                                              
         MVI   DBBTYPE,C'O'                                                     
*        CLC   SVBSTART(3),=X'620304'                                           
         CLC   DBSEL1WK(2),=X'C464' 980304                                      
         BNH   *+8                                                              
         MVI   DBBTYPE,C'O'                                                     
*&&                                                                             
*Y2K                                                                            
*                                                                               
* DATE CHECK FOR OLYMPIC EXCLUSION                                              
* OLYEXCL2 IS THE DATE TABLE                                                    
*                                                                               
OLYEXCF2 DS    0H                                                               
                                                                                
*                                                                               
         LA    RE,OLYEXCL2                                                      
*                                                                               
OLYEXCF3 CLI   0(RE),X'FF'                                                      
         BE    OLYXCF6X                                                         
         CLC   DBSELBK,0(RE)                                                    
         BE    OLYEXCF4                                                         
         LA    RE,6(RE)                                                         
         B     OLYEXCF3                                                         
*                                                                               
OLYEXCF4 CLC   3(2,R4),2(RE)        START OF SWEEP                              
         BNL   OLYEXCF7                                                         
         CLI   DBBTYPE,C'L'                                                     
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_OL                                              
         B     OLYEXCF7                                                         
         CLI   DBBTYPE,BOOKTYPE_LS                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_OS                                              
         B     OLYEXCF7                                                         
         CLI   DBBTYPE,BOOKTYPE_L3                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_O3                                              
         B     OLYEXCF7                                                         
         CLI   DBBTYPE,BOOKTYPE_L1                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_O1                                              
         B     OLYEXCF7                                                         
         MVI   DBBTYPE,C'O'                                                     
OLYEXCF7 CLC   3(2,R4),4(RE)        END OF SWEEP                                
         BNH   OLYXCF6X                                                         
         CLI   DBBTYPE,C'L'                                                     
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_OL                                              
         B     OLYXCF6X                                                         
         CLI   DBBTYPE,BOOKTYPE_LS                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_OS                                              
         B     OLYXCF6X                                                         
         CLI   DBBTYPE,BOOKTYPE_L3                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_O3                                              
         B     OLYXCF6X                                                         
         CLI   DBBTYPE,BOOKTYPE_L1                                              
         BNE   *+12                                                             
         MVI   DBBTYPE,BOOKTYPE_O1                                              
         B     OLYXCF6X                                                         
         MVI   DBBTYPE,C'O'                                                     
OLYXCF6X B     OLYEXCFX                                                         
*                                                                               
*  EXCLUSION DATE TABLE                                                         
*  BYTES 1-2 BOOK YYMM                                                          
*  BYTES 3-4 SWEEP START DATE COMPRESSED                                        
*  BYTES 5-6 SWEEP END DATE COMPRESSED                                          
*                                                                               
OLYEXCL2 DC    XL6'6E02DC44DC63'                                                
         DC    XL6'7202E43EE45A'                                                
         DC    XL6'6602CC3CCC5B'                                                
         DC    XL6'7602EC48EC59'                                                
         DC    X'FF'                                                            
                                                                                
                                                                                
OLYEXCFX DS    0H                                                               
*&&DO                                                                           
* AGAIN DONT NEED THIS CHECK ANYMORE                                            
* WE SHOULD ALLOW OVERNIGHT/WEEKLY VPHS FOR ALL BOOKTYPES                       
         CLI   DBBTYPE,0                                                        
         BE    GD152P                                                           
*                                                                               
         XC    VPHHOMES,VPHHOMES                                                
         XC    VPHFACT,VPHFACT                                                  
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
*&&                                                                             
                                                                                
GD152P   MVI   DEMANDSW,0                                                       
         MVI   WKLYIND,0           RESET WEEKLY DATA FOUND                      
*=================================================================              
* CHECK TO SEE IF WE NEED TO FIGURE OUT THE DBSELBK FOR LPM MKTS                
*=================================================================              
         CLI   MKTTYPE,MKTLPM                                                   
         BE    *+8                                                              
         CLI   WVPHOPT,C'P'        PEOPLE METER LOOKUP?                         
         BNE   AWVPHX                                                           
                                                                                
WVPHLPM2 MVI   DBSELMED,C'W'       SET FOR WEEKLY LOOKUP                        
         CLI   LPMWKOPT,C'Y'      CHECK FOR EACH POSTING OPTION                 
         BE    *+8                                                              
         CLI   WVPHOPT,C'Y'                                                     
         BE    *+8                                                              
         CLI   DIGITALF,C'Y'      IF DIGITAL TRANSITION                         
         BE    *+8                                                              
         CLI   OVPHOPT,C'Y'                                                     
         BNE   AWVPHX                                                           
         BRAS  RE,AFFWKBK                                                       
AWVPHX   DS    0C                                                               
*=================================================================              
*                                                                               
         CLI   DBSELMED,C'W'                                                    
         BE    *+8                                                              
         CLI   DBSELMED,C'O'                                                    
         BNE   *+8                                                              
         CLI   DBBTYPE,C'P'                  NO BOOKTYPE P FOR                  
         BNE   *+8                           OVERNIGHTS/WEEKLY                  
         MVI   DBBTYPE,0                     LOOK FOR STANDARD                  
*                                                                               
*SHOULD WE BREAK AVERAGE QTR HOURS ?                                            
GD152Q   NI    DBNPMFLG,X'FF'-DBNOBAVG                                          
         CLI   SV1WPROF+12,0                                                    
         JE    AFFNOBAV                                                         
***      CLC   DBSELBK(1),SV1WPROF+12                                           
         ZIC   RE,SV1WPROF+12                                                   
         AHI   RE,100                                                           
         ZIC   R1,DBSELBK                                                       
         CR    R1,RE                                                            
         BH    AFFNOBA8            YR HIGHER THAN PROFILE TURN FLAG ON          
         BL    AFFNOBAV            YR LOWER - DONT TURN FLAG ON                 
         CLI   DBSELMED,C'O'                                                    
         BNE   AFFNOBA1                                                         
         CLC   DBSELBK+1(1),SV1WPROF+14                                         
         BL    AFFNOBAV                                                         
         B     AFFNOBA8                                                         
*                                                                               
AFFNOBA1 CLI   DBSELMED,C'T'                                                    
         BNE   AFFNOBAV                                                         
         CLI   DBSELSRC,C'N'                                                    
         BE    *+8                                                              
         CLI   DBSELSRC,C'F'                                                    
         BNE   AFFNOBAV                                                         
         CLC   DBSELBK+1(1),SV1WPROF+13                                         
         BL    AFFNOBAV                                                         
AFFNOBA8 OI    DBNPMFLG,DBNOBAVG                                                
AFFNOBAV DS    0C                                                               
*                                                                               
*                                                                               
*SHOULD WE LOOK UP TRUE WEEKLY NIELSEN MONTHLY DATA                             
         CLI   SV1WPROF+15,C'Y'                                                 
         JNE   AFFNOTWK                                                         
         CLI   DBSELMED,C'T'                                                    
         BNE   AFFNOTWK                                                         
         CLI   DBSELSRC,C'N'                                                    
         BE    *+8                                                              
         CLI   DBSELSRC,C'F'                                                    
         BNE   AFFNOTWK                                                         
AFFTRWK8 OI    DBNPMFLG,DBTRUEWK                                                
AFFNOTWK DS    0C                                                               
*                                                                               
* CHECK TO SEE IF OVERNIGHT                                                     
* IF SO CHECK BOOK AGAINST OVEFFBK AND SEE IF BOOK FOR THE BOOKTYPE             
* WE ARE POSTING AGAINST IS BEFORE THE AVAILABLE START BOOK                     
         CLI   OVPHOPT,C'Y'                                                     
         BNE   GD152Q1                                                          
         CLI   DIGITALF,C'Y'                                                    
         BE    *+8                                                              
         CLI   DBSELMED,C'O'                                                    
         BNE   GD152Q1                                                          
         OC    OVEFFBK,OVEFFBK                                                  
         BZ    GD152Q1                                                          
         CLC   DBSELBK,OVEFFBK       POST MONTHLY PRIOR TO EFFECTIVE BK         
         BNL   GD152Q1                                                          
* READ MONTHLY BOOK                                                             
         MVI   DBSELMED,C'T'                                                    
         XC    VPHHOMES,VPHHOMES                                                
         XC    OVPHHMES,OVPHHMES                                                
         XC    OVHOMUNV,OVHOMUNV                                                
         XC    VPHFACT,VPHFACT                                                  
         MVI   NEXTYRLK,C'N'                                                    
         MVC   DBSELBK,SVSELBK                                                  
*                                                                               
*                                                                               
GD152Q1  L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
*                                                                               
*IF PROFILE SET TO USE NEW POSTING METHODOLOGY THEN                             
*SET THE BOOK TO VPHSDAT - DEGETTP WILL FIGURE OUT THE WEEKLY BOOK              
*        MVI   DBNPMFLG,0                                                       
* IF NEW METHODLOGY PROFILE ON AND POSTING                                      
         IF (CLI,SVD0PROF+14,E,C'Y'),AND,                                       
            (CLI,DBVOPT,E,X'40'),AND,(CLI,DBSELMED,E,C'C')                      
         MVI   DBNPMFLG,DBNPMAFF   SET FLAG TO NEW POSTING METHODOLOGY          
         MVC   DBSELBK,VPHSDAT                                                  
         ENDIF                                                                  
*                                                                               
         CLI   COMPASS,C'1'        TEST COMSCORE PASS 1                         
         JE    GD154X              YES - SKIP NSI STUFF                         
*                                                                               
         CLI   NSIREQ,C'Y'         TEST ANY NSI DEMOS                           
         JNE   GD154X                                                           
*                                                                               
         MVI   DEMANDSW,0                                                       
         GOTO1 (RF),DMCB,DBLOCK,GETDEMHK                                        
         MVC   DBBEST,SVDBBEST     REST DBBEST TO ORIGINAL                      
*==============================================================                 
* SEE IF WE NEED TO REREAD FOR OVERNIGHTS                                       
* IF WE READ WEEKLY AND THE CORRECT OPTIONS ARE TURNED ON                       
* REREAD FOR OVERNIGHTS                                                         
*==============================================================                 
                                                                                
GD152Q2  CLI   DBSELMED,C'W'       WEEKLY JUST LOOKED UP                        
         BNE   GD152R               NO - TREAT AS NORMAL                        
         CLI   SVD0PROF+11,C'M'    OVERNITES ACTIVE                             
         JE    *+8                                                              
         CLI   SVD0PROF+11,C'Y'    OVERNITES ACTIVE                             
         JNE   GD152R               NO - TREAT AS NORMAL                        
                                                                                
         CLI   DEMANDSW,0          DID WE GET TO HOOK                           
         BNE   GD152R                                                           
                                                                                
GD152Q3  MVI   DBSELMED,C'O'       NOTHING FOUND - TRY OVERNITE                 
         MVI   AFFIDLK,C'Y'                                                     
         BRAS  RE,GETOVSWK                                                      
         MVC   DBSELBK,VPHSWK      OTHERWISE - TRY FOR OVERNIGHT                
         MVI   DEMANDSW,0          RESET SWITCH                                 
* TO REREAD FOR OVERNIGHTS - CLEAR IUN VALUES FROM  WHAT                        
* WAS STORED FROM WEEKLY READ IF ANY                                            
GD152Q4  LA    R0,4                RTGS/IMPS/PUTS/TOTS                          
         L     R1,ADTUNVS                                                       
         XC    0(IUNDEMS*4,R1),0(R1)                                            
         LA    R1,IUNDEMS*4(R1)                                                 
         BCT   R0,*-10                                                          
         B     GD152Q                                                           
*==============================================================                 
                                                                                
GD152R   CLI   DIGITALF,C'Y'                                                    
         BNE   GD152S                                                           
         CLI   DBSELMED,C'O'                                                    
         BNE   GD152S                                                           
         MVC   DBSELMED,SVAFFMED                                                
         CLI   DBERROR,X'80'       CHECK FOR EOF RETURN                         
         BNE   *+12                                                             
         CLI   DEMANDSW,0                                                       
         BNE   GD152T              NO                                           
         MVC   DBSELBK,=AL2(JUL_09)                                             
         MVI   DBSELMED,C'T'                                                    
         B     GD152Q4                                                          
*                                                                               
GD152S   MVC   DBSELMED,SVAFFMED                                                
         CLI   DBERROR,X'80'                                                    
         BNE   GD153                                                            
         CLI   DEMANDSW,0                                                       
         BE    GD153                                                            
         CLI   DBBEST,C'M'                                                      
         BNE   *+10                                                             
         MVC   SVACTBK,DBACTBK                                                  
*                                                                               
GD152T   CLI   BDPROGT-1,0         TEST -S                                      
         BNE   *+12                NO                                           
         TM    WKLYIND,X'40'       TEST NON-WEEKLY DATA                         
         BO    GD153               YES - TREAT AS MISSING DATA                  
*                                                                               
         MVC   DBSELRMK,DBACTRMK   SET RTG SVC MKT TO PREVENT REREAD            
                                                                                
* DIVIDE RESULTS BY NUMBER OF QH'S IF NOT 1 *                                   
                                                                                
         LH    R0,DBDIVSOR                                                      
         CH    R0,=H'1'                                                         
         BE    GD154                                                            
         LA    R1,DTDEMSA                                                       
         BRAS  RE,GDUNWGT                                                       
         B     GD154                                                            
                                                                                
* IF ERROR OCCURS ON AFFID LOOK-UP TREAT AS NOAFD SPOT                          
                                                                                
GD153    DS    0H                                                               
         LH    RE,NOAFDCNT                                                      
         LA    RE,1(RE)                                                         
         STH   RE,NOAFDCNT                                                      
         OI    WKLYOPT,X'40'       INDICATE WEEKLY MISSING                      
         LH    RE,AFDCNT                                                        
         BCTR  RE,0                                                             
         STH   RE,AFDCNT                                                        
         B      GD154X                                                          
         EJECT                                                                  
GD154    CLI   NSIREQ,C'Y'         ANY NSI DEMOS                                
         JNE   *+8                                                              
         BAS   RE,GETIUN           EXTRACT REQUESTED DEMOS                      
                                                                                
*===================================================================            
* SEE IF NEED ANY COMSCORE DEMOS                                                
*===================================================================            
                                                                                
GD154X   CLI   COMPASS,C'1'        TEST PASS 1 OR 2                             
         JL    GD156               NO                                           
         CLI   DBVOPT,X'40'                                                     
         JNE   GD156                                                            
*                                                                               
         CLI   DBSELSRC,C'N'                                                    
         JE    *+12                                                             
         CLI   DBSELSRC,C'F'                                                    
         JNE   GD156                                                            
*                                                                               
         CLI   COMREQ,C'Y'                                                      
         JNE   GD156                                                            
*                                                                               
         MVI   COMTYPE,C'I'                                                     
         BRAS  RE,COMSCORE                                                      
*                                                                               
         CLI   COMERRF,0                                                        
         JE    *+12                                                             
         OI    DBERROR,COMSCERR    SET COMSCORE ERROR OCCURRED                  
         MVI   COMERROR,COMSCERR   AND SET HERE TOO                             
*                                                                               
         CLI   COMPASS,C'1'        TEST PASS 1                                  
         JE    GD159                                                            
*                                                                               
* ADD TO AFD TOTALS *                                                           
*                                                                               
GD156    LA    R0,MAXDEMS*2                                                     
         SR    R1,R1                                                            
*                                                                               
GD157    L     RE,AFDEMSA(R1)        GET TOTAL SO FAR                           
         L     RF,DTDEMSA(R1)        GET NEW VALUE                              
         C     RF,=F'-1'             TEST NO VALUE FOUND                        
         JNE   GD157A                                                           
*                                                                               
         L     RF,ADDEMEL                                                       
         LA    RF,NDEMNO-NDELEM(RF)  POINT TO FIRST DEMO                        
         AR    RF,R1                 R1*2 IS DSPL OF DEMO IN DEMEL              
         AR    RF,R1                 SO ADD DSPL TWICE                          
*                                                                               
         L     RF,4(RF)              GET VALUE FOR THIS DEMO                    
         N     RF,=X'3FFFFFFF'       DROP FLAGS                                 
         L     RE,AFDEMSU(R1)                                                   
         AR    RE,RF                 ADD NEW VALUE                              
         ST    RE,AFDEMSU(R1)        ADD TO UNADJ VALUE                         
*                                                                               
         L     RE,AFDEMSA(R1)        RESTORE PREVIOUS VALUE                     
         MHI   RF,100                X SVI FACTOR                               
*                                                                               
GD157A   AR    RE,RF                                                            
         ST    RE,AFDEMSA(R1)                                                   
*                                                                               
         LA    R1,4(R1)                                                         
         BCT   R0,GD157                                                         
*                                                                               
GD158    XC    DTDEMSA,DTDEMSA     CLEAR FOR NEXT LOOK-UP                       
         XC    DTDEMSU,DTDEMSU                                                  
         L     RE,ARADEMSA                                                      
         L     RF,ARADEMSU                                                      
         CLI   DBSELMED,C'R'       IF RADIO,                                    
         BNE   *+16                                                             
         XC    0(L'RADEMSA,RE),0(RE)   CLEAR THESE FIELDS ALSO                  
         XC    0(L'RADEMSU,RF),0(RF)                                            
*                                                                               
         MVC   LPMWKOPT,SVLPMWKOPT RESTORE ORIGINAL OPTIONS                     
         MVC   WVPHOPT,SVWVPHOPT                                                
*                                                                               
         TM    WKLYOPT,X'80'       TEST WEEKLY LOOKUP                           
         BZ    GD159               NO                                           
         CLI   SV1WPR1,C'Y'        TEST ALL BUY OPTION                          
         BE    GD159                                                            
         TM    WKLYIND,X'40'       TEST NON-WEEKLY DATA FOUND                   
         BZ    GD159                                                            
         OI    WKLYOPT,X'40'       SET MISSING DATA FLAG                        
*                                                                               
GD159    LA    R4,L'AFDLIST(R4)    NEXT AFFID                                   
         MVC   DBSELMED,SVAFFMED   RESTORE MEDIA                                
         MVC   DBSELBK,SVSELBK     RESTORE ORFINAL SELBK                        
         MVI   DEMANDSW,0          RESET                                        
         CLI   0(R4),0                                                          
         BNE   GD152                                                            
                                                                                
* ALL AFFIDS COMPLETE - TEST FOR MISSING AFFIDS *                               
                                                                                
GD160    DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R0,3,NOAFDCNT                                                    
         BZ    GD70                NO - CONTINUE                                
         XC    DBSEL1WK,DBSEL1WK   ENOUGH WEEKLY DATA ALREADY                   
         MVI   DBSELWKN,0          THIS BYTE TOO !!                             
         OI    WKLYOPT,X'40'       SET MISSING WEEKLY DATA                      
*                                                                               
         CLI   ACTBOOK,C'Y'                                                     
         JNE   *+10                                                             
         XC    DBACTBK,DBACTBK                                                  
*                                                                               
         B     GD50                AND GO DO BUY LOOKUP                         
         EJECT                                                                  
******************************************************************              
* DEMAND RECORD PROCESSING HOOK                                  *              
******************************************************************              
                                                                                
GETDEMHK NTR1                                                                   
         BRAS  RE,GETDMHK                                                       
         J     EXIT                                                             
******************************************************************              
GETVHOME NTR1                                                                   
         BRAS  RE,GETWKHK                                                       
         J     EXIT                                                             
         EJECT                                                                  
GETOVHOM NTR1                                                                   
         BRAS  RE,GETOVHK                                                       
         J     EXIT                                                             
HOMDEM   DC    X'00',C'J',X'01',X'00',C'L',X'01',X'FF'                          
         EJECT                                                                  
**************************************************************                  
         EJECT                                                                  
*                                                                               
GDERR1   DS    0H                                                               
*                                                                               
GDERR2   CLI   PBDEMSW,C'Y'        TEST ANY POST BUY DEMO OVERRIDES             
         BNE   GDERR2A             NO                                           
         MVI   PBLKERR,C'Y'        YES-INDICATE ERROR                           
         XC    BUDEMSA,BUDEMSA                                                  
         XC    BUDEMSU,BUDEMSU                                                  
         XC    AFDEMSA,AFDEMSA                                                  
         XC    AFDEMSU,AFDEMSU                                                  
         B     GD70                GO BACK AS IF NO ERROR                       
*                                                                               
GDERR2A  L     R1,SAVER1                                                        
         CLI   4(R1),1             TEST ORIGINATING + SPILL REQUEST             
         BNE   GDERRX              NO                                           
         CLI   0(R3),3             TEST SPILL ELEMENT                           
         BE    GD99                YES - IGNORE ERROR                           
*                                                                               
GDERRX   L     R3,ADDEMEL                                                       
         MVC   22(1,R3),DBERRMOD   FAILING MODULE ID                            
         L     R1,SAVER1                                                        
         MVC   0(1,R1),DBERROR     RETURN ERROR CODE                            
*                                                                               
         CLI   DBERROR,NOTFOUND    TEST NOT FOUND ERROR                         
         BNE   *+12                                                             
         MVI   0(R1),X'45'         TRANSLATE TO OLD ERROR CODE                  
         J     EXIT                                                             
*                                                                               
         CLI   DBERROR,INVFM       TEST INVALID FILE/MEDIA                      
         BL    GDERRX2                                                          
         CLI   DBERROR,INVFMS                                                   
         BH    GDERRX2                                                          
         MVI   0(R1),X'45'         TRANSLATE TO OLD ERROR CODE                  
         J     EXIT                                                             
*                                                                               
GDERRX2  J     EXIT                                                             
*                                                                               
         EJECT                                                                  
**************************************************************                  
*                                                            *                  
* SUBROUTINE TO BUILD IUN RECORD AND EXTRACT REQUESTED DEMOS *                  
*                                                            *                  
**************************************************************                  
                                                                                
GETIUN   NTR1                                                                   
         TM    FLAGS,CANHPT        TEST CANADIA HPT'S                           
         BNZ   GETCN                                                            
         CLI   DBSELMED,C'R'       TEST FOR RADIO                               
         BE    GETRAD                                                           
* COMMENT OUT . NCM WILL ROLL UP ALL DAYS TIMES IN A WEEK IN EACH               
* QTR HOUR                                                                      
*&&DO                                                                           
* NCM DONT UNWEIGHT FOR NOW                                                     
         CLI   DBSELSRC,C'C'                                                    
         BNE   *+10                                                             
         MVC   DBDIVSOR,=H'1'                                                   
*&&                                                                             
         LH    R0,DBDIVSOR         UNWEIGHT DEMOS                               
         ST    R0,DUB              GET DIVISOR ON WORD BOUNDARY                 
                                                                                
         CLC   =F'0',VPHHOMES      NO WEEKLY - BYPASS WVPH                      
         BZ    GETIUNV1                                                         
         CLI   DMINDEX,C'Y'        FORCE TO DO INDEX                            
         BE    *+8                                                              
         CLI   OVPHOPT,C'Y'                                                     
         BE    *+8                                                              
         CLI   WVPHOPT,C'Y'                                                     
         BNE   GETIUNV1                                                         
         L     R1,ADTUNVS          POINT TO EXPLODED DATA                       
         L     R7,IUNHMDSP(R1)                                                  
         ST    R7,HOMUNIV                                                       
         LA    R1,IUNDEMS*4(R1)    POINT TO DEMOS                               
         L     R7,IUNHMDSP(R1)       GET HOMES VALUE                            
         LA    R0,IUNDEMS*4        TOTAL NUMBER OF VALUES                       
*                                                                               
         SR    RE,RE                                                            
         LR    RF,R7               UNWEIGHT THE HOMES                           
         SLDA  RE,1                                                             
         D     RE,DUB              UNWEIGHT IT                                  
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         LR    R7,RF               AND SAVE IN R7                               
         LTR   R7,R7                                                            
         BZ    GETIUNV1                                                         
*                                                                               
* CHECK TO SEE IF WE READ THE OVERNIGHTS HOMES IMPRESSION                       
* IF SO UNWEIGHT IT AND USE IT                                                  
         CLI   OVPHOPT,C'Y'                                                     
         BNE   GETIUNWV                                                         
         OC    OVPHHMES,OVPHHMES                                                
         BNZ   GETIUNOV                                                         
*                                                                               
GETIUNWV SR    RE,RE                                                            
         L     RF,VPHHOMES         UNWEIGHT THE WEEKLY RATINGS                  
         SLDA  RE,1                                                             
         D     RE,VPHFACT          UNWEIGHT IT                                  
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         SR    RE,RE                                                            
         M     RE,HOMUNIV          CONVERT TO DMA IMPS UNIV IN (00)             
         SLDA  RE,1                                                             
         D     RE,=F'10'                                                        
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,VPHHOMES                                                      
         B     GETIUNOVX                                                        
* OVERNIGHTS IMPRESSIONS                                                        
GETIUNOV SR    RE,RE                                                            
         L     RF,OVPHHMES         UNWEIGHT THE OVERNIGHTS IMP                  
         SLDA  RE,1                                                             
         D     RE,VPHFACT          UNWEIGHT IT                                  
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,VPHHOMES                                                      
*                                                                               
* INDEX THE TOTAL HOME VIEWERS BY OV UNIV AND MONTHLY UNIV                      
*                                                                               
         XC    DUB3,DUB3             PACK INPUT  VALUE                          
         CVD   RF,DUB3              RF= IMPRESSION                              
         OI    DUB3+7,X'0C'                                                     
         L     RF,HOMUNIV           MONTHLY UNIV                                
         XC    DUB2,DUB2                                                        
         CVD   RF,DUB2                                                          
         OI    DUB2+7,X'0C'                                                     
         MP    DUB3(8),DUB2+4(4)     DUB HAS PACKED NUMBER1                     
*                                                                               
         L     RF,OVHOMUNV          OVERNIGHT HOME UNIV                         
         XC    DUB2,DUB2                                                        
         CVD   RF,DUB2                                                          
         OI    DUB2+7,X'0C'                                                     
         XC    WORK,WORK                                                        
         MVC   WORK+4(8),DUB3                                                   
*                                                                               
         DP    WORK(12),DUB2+4(4)                                               
         MVC   DUB3(8),WORK                                                     
         CVB   RF,DUB3                                                          
         ST    RF,VPHHOMES         UNIV INDEXED IMPRESSION                      
GETIUNOVX DS    0H                                                              
*                                                                               
         CLI   VPWKSTPT,0                                                       
         BE    GETIUNV                                                          
         SR    RE,RE                                                            
         L     RF,VPHHOMES         WEIGHT FOR WEEKS FOUND                       
*                                                                               
         SR    RE,RE                                                            
         M     RE,VPNWKS                                                        
         ST    RF,VPHHOMES                                                      
         ZIC   RE,VPWKSTPT         UNCOVERED WEEKS                              
         LR    RF,R7               RATING RECORD HOMES                          
         A     RF,VPHHOMES         ADJUST VPHHOMES                              
         ZIC   RE,VPWKSTPT                                                      
         A     RE,VPNWKS                                                        
         ST    RE,VPNWKS                                                        
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,VPNWKS                                                        
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,VPHHOMES                                                      
*                                                                               
*        CALCULATE THE VPHS                                                     
GETIUNV  L     RE,0(R1)            GET THE DEMO VALUE                           
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         D     RE,DUB                                                           
         SR    RE,RE                                                            
         M     RE,=F'10000'        X 10000                                      
         DR    RE,R7               / HOMES                                      
         AHI   RF,1                ROUND IT                                     
         SRL   RF,1                                                             
         ST    RF,0(R1)            GIVES VPH                                    
         LA    R1,4(R1)            NEXT DEMO                                    
         BCT   R0,GETIUNV          DO FOR ALL                                   
         MVC   DUB,=F'1'           STOP THE UNWEIGHTING FOR DEMS                
*                                                                               
GETIUNV1 L     RE,AIUNVS                                                        
         L     RF,ADTUNVS                                                       
         MVC   0(4*IUNDEMS,RE),0(RF)                                            
*                                                                               
         LA    R0,IUNDEMS*4                                                     
         L     R4,ADTDEMS                                                       
         L     R5,AIUNOLD                                                       
         SR    R7,R7                                                            
*                                                                               
GETIUN2  SR    RE,RE                                                            
         L     RF,0(R4,R7)                                                      
         SLDA  RE,1                                                             
         CLC   =F'0',DUB          IF DBDIVSOR=ZERO - DONT                       
         BNE   *+10               CARRY OVER VALUE                              
         SR    RF,RF              CLEAR OUT DEMO VALUE                          
         B     GETIUN2V                                                         
*                                                                               
         D     RE,DUB                                                           
         AHI   RF,1                                                             
         SRA   RF,1                                                             
*                                                                               
         CLC   =F'0',VPHHOMES                                                   
         BE    GETIUN2V                                                         
         CLI   DMINDEX,C'Y'                                                     
         BE    *+8                                                              
         CLI   OVPHOPT,C'Y'                                                     
         BE    *+8                                                              
         CLI   WVPHOPT,C'Y'        DOING WEEKLY VPH                             
         BNE   GETIUN2V                                                         
         SR    RE,RE                                                            
         M     RE,VPHHOMES                                                      
         SLDA  RE,1                                                             
         D     RE,=F'10000'                                                     
         AHI   RF,1                                                             
         SRA   RF,1                                                             
*                                                                               
GETIUN2V DS    0H                                                               
         ST    RF,0(R5,R7)           SET AS OLD VALUE                           
         ST    RF,IUNDEMS*16(R5,R7)  AND AS NEW                                 
         LA    R7,4(R7)                                                         
         BCT   R0,GETIUN2                                                       
                                                                                
* UNWEIGHT THE VUTS *                                                           
                                                                                
         LA    R0,3                                                             
         LA    R4,DTVUTS                                                        
*                                                                               
GETIUN4  SR    RE,RE                                                            
         L     RF,0(R4)                                                         
* DONT DIE BY DIVIDE BY ZERO- BPOO - DUB=DBDIVSOR                               
         CLC   =F'0',DUB           IF DBDIVSOR = ZERO                           
         BNE   *+10                CLEAR OUT DEMO VALUE                         
         SR    RF,RF               DOESNT MAKE SENSE TO CARRY IT OVER           
         B     GETIUN5                                                          
* NEW YORK EXCEEDS BINARY LIMIT - MUST DO IN PACKED                             
         CVD   RF,DUBP             RATING TO PACKED                             
         ZAP   DPWORK(16),DUBP                                                  
         L     RE,DUB              GET THE WEIGHT                               
         CVD   RE,DUBP             INDEX TO PACKED                              
         MP    DPWORK,=P'10'       ADJ FOR ROUND                                
         DP    DPWORK,DUBP                                                      
         ZAP   DUBP,DPWORK(8)                                                   
         AP    DUBP,=P'5'          ROUND IT                                     
         ZAP   DPWORK(16),DUBP                                                  
         DP    DPWORK,=PL8'10'                                                  
         ZAP   DUBP,DPWORK(8)                                                   
         CVB   RF,DUBP             GET IT IN BINARY                             
GETIUN5  ST    RF,0(R4)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,GETIUN4                                                       
                                                                                
* NOW COMPUTE THE HOME SHARES = RATING/VUT                                      
                                                                                
         LA    R0,3                R0=N'HOMES SHARE VALUES                      
         SR    R7,R7               R7=INDEX REGISTER                            
*                                                                               
GETIUN6  SR    RF,RF                                                            
         CLI   TAPEOPT,C'Y'        DONT SAVE SHARES FOR IMP BASED               
         BE    GETIUN6B      NOTE- (PUTS NEED TO BE MULTIPLIED BY               
*                                   UNIV IF SAVED SHARES NEEDED)                
         L     RF,AIRTGOLD                                                      
         L     RF,IUNHMDSP(RF,R7)                                               
         M     RE,=F'1000'                                                      
*                                                                               
         L     R1,DTVUTS(R7)       PICK UP VUT                                  
         TM    SHRSW,X'80'         TEST TO COMPUTE SHARES                       
         BZ    *+12                                                             
         L     R1,AIPUTOLD                                                      
         L     R1,IUNHMDSP(R1,R7)  PICK UP ACTUAL PUT                           
*                                                                               
         LTR   R1,R1               TEST FOR ZERO PUT                            
         BNZ   GETIUN6A                                                         
         SR    RF,RF               YES-SET SHARE TO ZERO                        
         B     GETIUN6B                                                         
*                                                                               
GETIUN6A DR    RE,R1                                                            
*                                                                               
GETIUN6B L     RE,AIUNXTRA                                                      
         ST    RF,0(RE,R7)         SET SHARE VALUE IN IUN RECORD                
*                                                                               
         LA    R7,4(R7)                                                         
         BCT   R0,GETIUN6                                                       
*                                                                               
GETIUN8  L     RE,ADBLOCKS         USE THIS FOR DBLOCK SAVE AREA                
         MVC   0(256,RE),DBLOCK                                                 
*                                                                               
         MVC   DBFILE,=C'PAV'                                                   
         L     RE,ADUMMY                                                        
         ST    RE,DBAREC                                                        
         LA    RE,23(RE)           POINT TO FIRST DEMO ELEMENT                  
         MVI   0(RE),0             SET END OF RECORD                            
         ST    RE,DBAQUART         SET A(QUARTER HOUR)                          
         LA    R0,IUNDEMS*9+6                                                   
         STH   R0,DBNUMVLS                                                      
*                                                                               
* 03/2020 AS PART OF THE LIVE+1 REQUEST TO SLOT TSA IMPS AND TOTS FROM          
* DMA WE ARE GOING TO DISABLE CLEARING TSA TOTS AND IMPS FOR SPILL              
* LOOKUPS AS PER MARIA DASILVA. DO THIS FOR NSI TV                              
         CLI   DBSELSRC,C'N'                                                    
         BNE   *+12                                                             
         CLI   DBSELMED,C'T'                                                    
         BE    GETIUN10                                                         
*                                                                               
         CLI   0(R3),3             TEST SPILL LOOK-UP                           
         BE    *+12                                                             
         CLI   0(R3),X'13'                                                      
         BNE   GETIUN10            NO                                           
         L     RE,AIIMPOLD         CLEAR ALL IMPS/TOTS                          
         XC    0(IUNDEMS*4,RE),0(RE)                                            
         L     RE,AITOTOLD                                                      
         XC    0(IUNDEMS*4,RE),0(RE)                                            
         L     RE,AIIMPNEW                                                      
         XC    0(IUNDEMS*4,RE),0(RE)                                            
         L     RE,AITOTNEW                                                      
         XC    0(IUNDEMS*4,RE),0(RE)                                            
*                                                                               
                                                                                
* CALL DEMAINT TO BUILD IUN RECORD *                                            
                                                                                
GETIUN10 L     RF,DBCOMFCS                                                      
         L     RF,CDEMAINT-COMFACSD(RF)                                         
         MVC   WORK(10),OFORMAT                                                 
         CLC   SVACTBK,=X'5801'    SWITCH FORMULAS AT JAN/88                    
         BL    *+10                                                             
         MVC   WORK+7(2),=X'530B'                                               
         CLI   TAPEOPT,C'Y'        SWITCH FORMULAS FOR TAPE BASED               
         BNE   *+10                                                             
         MVC   WORK+7(2),=X'5A0B'                                               
         CLI   DBACTSRC,C'C'       PROCESS SAME AS NIELSEN                      
         BNE   *+8                                                              
         MVI   DBACTSRC,C'N'                                                    
         GOTO1 (RF),DMCB,REP,DBLOCK,AIUNWK,WORK                                 
         B     GETIUN20                                                         
*                                                                               
REP      DC    C'REP'                                                           
OFORMAT  DC    C'PAVUIUN',X'520B00'                                             
         EJECT                                                                  
*===============================================================                
* CALL DEMOUT TO EXTRACT DEMOS IN CALLERS LIST INTO DTDEMSU                     
*===============================================================                
                                                                                
GETIUN20 L     RF,DBCOMFCS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
*                                                                               
GETIUN21 DS    0H                                                               
         GOTO1 (RF),DMCB,(C'L',DEMOLIST),DBLOCK,DTDEMSU                         
                                                                                
*==========================================================                     
* RETURN VALUES TO USER *                                                       
*==========================================================                     
                                                                                
*                                                                               
GETIUN30 L     RE,ADBLOCKS         RESTORE ORIGINAL DBLOCK                      
         MVC   DBLOCK(256),0(RE)                                                
*                                                                               
         BRAS  RE,GETADJ           GET ADJUSTMENT FACTORS                       
*                                                                               
         LA    R1,DTDEMSA                                                       
         BRAS  RE,GDXSVI           MULTIPLY ADJ FACTORS X DEMOS                 
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
* EXTRACT RADIO DEMOS *                                                         
                                                                                
GETRAD   XC    DUB(4),DUB                                                       
         MVC   DUB+2(2),DBDIVSOR                                                
         L     RE,ADEMLST2         RE-->DEMO LIST FOR RADIO                     
         L     RF,ARADEMSU         RF-->"MAD"-ED DEMO VALUES FOR RADIO          
*                                                                               
GETRAD10 CLI   0(RE),X'FF'                                                      
         BE    GETRAD20                                                         
         SR    R0,R0                                                            
         ICM   R1,15,0(RF)                                                      
                                                                                
         SLDA  R0,1                                                             
         D     R0,DUB                                                           
         LTR   R1,R1                                                            
         BM    *+8                                                              
*        AH    R1,=H'1'                                                         
         AHI   R1,1                                                             
         SRA   R1,1                                                             
                                                                                
         STCM  R1,15,0(RF)                                                      
         LA    RE,3(RE)                                                         
         LA    RF,4(RF)                                                         
         B     GETRAD10                                                         
*                                                                               
** NOW CALCULATE DEMOS GIVEN IN ORIGINAL DEMO LIST **                           
*                                                                               
GETRAD20 LA    RE,RDEMLIST                                                      
         USING RDEMLSTD,RE                                                      
         LA    R3,DTDEMSU          R3-->TARGET AREA FOR DEMO VALUES             
         L     RF,ARADEMSU         RF-->ACCUMULATED DEMO VALUES                 
         SR    R0,R0                                                            
*                                                                               
GETRAD22 CLI   0(RE),X'FF'                                                      
         BE    GETRADX                                                          
         ICM   R1,15,0(RF)         R1=DEMO REQUESTED IN DEMOLIST                
         CLI   RDLOPER,0           DOES IT NEED TO BE CALCULATED?               
         BE    GETRAD30             NO                                          
                                                                                
         CLI   RDLOPER,C'D'        IS THE CALC A DIVIDE OPERATION?              
         BNE   GETRAD24             NO                                          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         CLC   8(4,RF),=F'0'       IS THERE A NONZERO DIVISOR?                  
         BE    GTRAD22A             NO, RETURN VALUE OF ZERO                    
         ICM   R1,15,4(RF)         R1=DIVIDEND VALUE                            
         CLI   RDLDMOD1,C'I'        IF IT IS AN IMPRESSION,                     
         BNE   *+8                                                              
         M     R0,=F'1000'           MULTIPLY IT BY 1000                        
         MVC   DUB(4),8(RF)        DUB=DIVISOR VALUE                            
                                                                                
         SLDA  R0,1                                                             
         D     R0,DUB                                                           
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
                                                                                
GTRAD22A LA    RF,4+4(RF)          BUMP 2X FOR DIVIDEND & DIVISOR               
         B     GETRAD30                                                         
*                                                                               
GETRAD24 DC    H'0'                                                             
*                                                                               
GETRAD30 STCM  R1,15,0(R3)                                                      
         LA    R3,4(R3)                                                         
         LA    RF,4(RF)                                                         
         LA    RE,RDEMLSTQ(RE)                                                  
         B     GETRAD22                                                         
         DROP  RE                                                               
*                                                                               
GETRADX  DS    0H                                                               
         B     GETCN10             FOLLOW CANADIEN AT THIS POINT                
                                                                                
* EXTRACT CANADIAN DEMOS *                                                      
                                                                                
GETCN    L     R1,ADUMMY           POINT TO I/O AREA                            
* BPOO                                                                          
* CANADIAN NON POSTING CALLS ADUMMY IS FULL WORD ARTHMETIC WHICH STILL          
* HOLDS THE ACTUALY DEMO RECORD WHILE                                           
* CANADIAN SPOT POSTING USES DOUBLE WORD ARITHMETIC WHERE                       
* ADUMMY POINTS TO DOUBLE WORD ACCUMLATOR BUFFERS, DBAREC STILL HOLDS           
* THE REAL DEMO RECORD LAST READ.                                               
* SO SET DBAQUART OFF THE REAL DEMO RECORD IN DBAREC NOT ADUMMY WHICH           
* HOLDS THE REAL ACCUMULATED DEMO RECORD FOR FULL WORD ARTHMETIC                
* MATH BUT HOLDS DOUBLE WORD BUFFERS FOR DOUBLE WORD ARTHMETIC.                 
*                                                                               
         TM    DBVOPT,X'40'                                                     
         BNO   *+8                                                              
         L     R1,DBAREC                                                        
*                                                                               
         STCM  R1,15,DBAREC                                                     
         LA    R1,DRFRSTEL-DRKEY(R1)                                            
         STCM  R1,15,DBAQUART                                                   
         XC    WORK,WORK                                                        
         LA    R0,DBLOCK                                                        
         STCM  R0,15,WORK                                                       
         MVC   WORK+06(2),DBDIVSOR                                              
* IF NEW POSTING METHODOLOGY FOR CANADA USE THE FULL WORD                       
* DBDIVSR2 FOR ACHIEVED POSTING                                                 
* IN THEORY WE CAN ALWAYS USE DBDIVSR2 INSTEAD OF DBDIVSOR                      
* BUT WE WANT TO LIMIT THE TESTING TO ONLY TO CANADIAN POSTING                  
*                                                                               
         TM    DBNPMFLG,DBNPMACH                                                
         BNO   GETCN06                                                          
* ONLY IF GETTTP SETS DBDIVSR2 SHOULD WE USE IT                                 
* THIS IS TO PREVENT ISSUES WHERE DEGETTP GETS BACKED OUT WITHOUT               
* SETTING DBDIVSR2.                                                             
         OC    DBDIVSR2,DBDIVSR2                                                
         BZ    GETCN06                                                          
         MVC   WORK+04(4),DBDIVSR2                                              
*                                                                               
GETCN06  MVC   WORK+08(3),DBFILE                                                
         MVC   WORK+11(3),DBFILE                                                
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMOMTH-COMFACSD(RF)                                         
         TM    DBVOPT,X'40'   DOUBLE WORD ARITHMETIC - BPOO                     
         BO    GETCN06B                                                         
         GOTO1 (RF),DMCB,=C'DIV',ADUMMY,ADUMMY,WORK                             
* BPOO                                                                          
         B     GETCN06C                                                         
*                                                                               
* DOUBLE WORD DIVIDE FOR CANADIAN POSTING                                       
GETCN06B GOTO1 (RF),DMCB,=C'DDI',ADUMMY,DBAREC,WORK                             
GETCN06C DS    0C                                                               
*                                                                               
*===========================================================                    
* NOW EXTRACT VALUES FROM RECORD JUST CREATED                                   
* AND CLEAR IMPRESSIONS IF FLAG IS SET                                          
*===========================================================                    
                                                                                
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(C'L',DEMOLIST),DBLOCK,DTDEMSU                         
*                                                                               
         L     R1,SAVER1                                                        
         L     RE,8(R1)                                                         
         USING GETDEMD,RE                                                       
         TM    SVCNFLGS,X'80'      TEST SUPPRESS IMPS                           
         BZ    GETCN10                                                          
         DROP  RE                                                               
*                                                                               
         LA    R4,DEMOLIST                                                      
         LA    R5,DTDEMSU                                                       
*                                                                               
GETCN2   CLI   1(R4),C'R'          TEST RATING                                  
         BE    GETCN4                                                           
         CLI   1(R4),C'E'          OR EXTENDED RATING                           
         BE    GETCN4                                                           
         XC    0(4,R5),0(R5)       CLEAR THE IMPS                               
*                                                                               
GETCN4   LA    R4,3(R4)            NEXT DEMO                                    
         LA    R5,4(R5)            NEXT ACCUM                                   
         CLI   0(R4),X'FF'                                                      
         BNE   GETCN2                                                           
*                                                                               
GETCN10  DS    0H                                                               
         BRAS  RE,GETADJ           GET ADJUSTMENT FACTORS                       
         LA    R1,DTDEMSA          GET DEMOS X ADJ FACTORS                      
         BRAS  RE,GDXSVI                                                        
                                                                                
*  THESE INSTRUCTIONS RESTORE DBAREC FOR NEXT DEMAND CALL *                     
                                                                                
         L     R1,ADUMMY           POINT TO AN I/O AREA                         
         XC    0(256,R1),0(R1)     AND CREATE A FAKE RECORD                     
         MVI   0(R1),C'R'                                                       
         LA    R0,DRFRSTEL+1-DRKEY                                              
         STCM  R0,3,DRRLEN-DRKEY(R1)                                            
         L     R1,SAVER1                                                        
         MVC   DBAREC,12(R1)       RESTORE DBAREC                               
         J     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
*================================================                               
* CREATE ADDRESS CONSTANTS FOR NON-ADDRESSABLE WORK AREA FIELDS *               
*================================================                               
SETADCON2 NTR1  BASE=*,LABEL=*                                                  
         LA    RE,ADCONS                                                        
         LA    RF,WKADCONS                                                      
         LA    R0,(ADCONSX-ADCONS)/4                                            
GD2      L     R1,0(RE)                                                         
         AR    R1,RC                                                            
         ST    R1,0(RF)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,GD2                                                           
         J     EXIT                                                             
*                                                                               
ADCONS   DS    0A                                                               
         DC    A(DTUNVS-WORKD)                                                  
         DC    A(DTDEMS-WORKD)                                                  
         DC    A(SVIREC-WORKD)                                                  
         DC    A(DUMMY-WORKD)                                                   
         DC    A(IUNWK-WORKD)                                                   
         DC    A(IUNVS-WORKD)                                                   
         DC    A(IUNOLD-WORKD)                                                  
         DC    A(IRTGOLD-WORKD)                                                 
         DC    A(IPUTOLD-WORKD)                                                 
         DC    A(IIMPOLD-WORKD)                                                 
         DC    A(ITOTOLD-WORKD)                                                 
         DC    A(IUNOLDX-WORKD)                                                 
         DC    A(IUNNEW-WORKD)                                                  
         DC    A(IRTGNEW-WORKD)                                                 
         DC    A(IPUTNEW-WORKD)                                                 
         DC    A(IIMPNEW-WORKD)                                                 
         DC    A(ITOTNEW-WORKD)                                                 
         DC    A(IUNNEWX-WORKD)                                                 
         DC    A(IUNXTRA-WORKD)                                                 
         DC    A(DBLOCKS-WORKD)                                                 
         DC    A(RADEMSA-WORKD)                                                 
         DC    A(RADEMSU-WORKD)                                                 
         DC    A(DEMOLST2-WORKD)                                                
         DC    A(GDEXTRA1-WORKD)                                                
         DC    A(GDEXTRA2-WORKD)                                                
         DC    A(AFDLIST-WORKD)                                                 
         DC    A(AFDLISTX-WORKD)                                                
         DC    A(SPDTTAB-WORKD)                                                 
         DC    A(GDEXSPDT-WORKD)                                                
         DC    A(COMEXT-WORKD)                                                  
ADCONSX  EQU   *                                                                
         EJECT                                                                  
*=================================================================              
* GET ADDRESSES OF DEMO TABLES                                                  
*=================================================================              
                                                                                
GETDEMTB NTR1  BASE=*,LABEL=*                                                   
         XC    DUB,DUB                                                          
         L     RF,DBCOMFCS                                                      
         L     RF,CSWITCH-COMFACSD(RF) USE SWITCH TO PICK UP V(SSB)             
         LTR   RF,RF                                                            
         BNZ   GD4ALET                                                          
         L     RF,DBCOMFCS                                                      
         L     RF,CMASTC-COMFACSD(RF)      RF=AMASTC                            
         ICM   RF,15,MCSSB-MASTD(RF)       RF=ASSB                              
         B     GD4ALETX                                                         
*                                                                               
GD4ALET  MVC   DUB(4),=XL4'FEFFFFFF'                                            
         GOTO1 (RF),DUB                                                         
         ICM   RF,15,0(R1)         RF=V(SYSFACS)                                
         JZ    BADGD4A                                                          
         ICM   RF,15,VSSB-SYSFACD(RF)                                           
         JNZ   *+6                                                              
BADGD4A  DC    H'0'                                                             
GD4ALETX MVC   ALET,SSBTBLET-SSBD(RF)                                           
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(5),=X'D0000000FF'                                            
         L     R4,DBCOMFCS                                                      
         L     RF,CDEMADDR-COMFACSD(R4)                                         
         GOTO1 (RF),P1,(X'FF',DUB),(R4)                                         
*                                                                               
         L     R1,DUB              POINT TO MASTER DISP TABLE                   
         USING DSPHDRD,R1                                                       
         XR    RE,RE                                                            
*                                                                               
GD4A     CLC   0(2,R1),=H'0'       TEST EOT                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   DSPFILE,C'I'        MATCH ON INTERNAL FILE CODE                  
         BE    GD4B                                                             
         ICM   RE,7,DSPAET                                                      
         LA    R1,1(RE,R1)         NEXT TABLE HEADER                            
         B     GD4A                                                             
*                                                                               
GD4B     CLI   TAPEOPT,C'Y'        TAPE BASED OPTION                            
         BE    GD4C                                                             
         CLC   DSPSBOOK,=X'B0F4'   BOOK BASE DISP TABLE                         
         BE    GD4C                                                             
         ICM   RE,7,DSPAET         NO - USE NEXT DISPLACEMENT TABLE             
         LA    R1,1(RE,R1)                                                      
         B     GD4A                                                             
*                                                                               
GD4C     AHI   R1,DSPHDRLN         BUMP PAST HEADER                             
         ST    R1,ADISPTAB         AND SAVE A(FIRST DEMO ENTRY)                 
         J     EXIT                                                             
*                                                                               
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   0(1,R6),ELCODE                                                   
         BER   RE                  EXIT WITH CC EQ                              
         J     NEXTEL                                                           
NEXTELX  LTR   RE,RE               SET CC NOT EQ                                
         BR    RE                                                               
*================================================                               
* SUBROUTINE TO GET ADJUSTMENT FACTORS                                          
* SVI FACTORS ARE NOW FORCED TO 100                                             
* CODE REMOVED MHER 1/11/17                                                     
*================================================                               
                                                                                
GETADJ   NTR1  BASE=*,LABEL=*                                                   
         L     R8,ASVIREC                                                       
         USING SVIREC,R8                                                        
         XC    SVIKEY,SVIKEY                                                    
*                                                                               
         LA    R0,100              SET ALL SVI VALUES = 100                     
         ST    R0,SVIS                                                          
         MVC   SVIS+4(L'SVIS-4),SVIS                                            
         J     EXIT                AND EXIT                                     
*                                                                               
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**************************************************************                  
GDTOT    NTR1  BASE=*,LABEL=*                                                   
         ZIC   R0,1(R6)                                                         
         SHI   R0,24                                                            
         JNP   EXIT                                                             
         SRL   R0,3                                                             
         LA    R6,24(R6)                                                        
*                                                                               
GDTOT2   CLC   1(2,R1),1(R6)       MATCH DEMO                                   
         BE    GDTOTX                                                           
         LA    R6,8(R6)                                                         
         BCT   R0,GDTOT2                                                        
         J     EXIT                                                             
*                                                                               
GDTOTX   TM    4(R6),X'80'         TEST OVRD                                    
         JZ    EXIT                                                             
         SR    R0,R0                                                            
         ICM   R0,7,5(R6)          GET VALUE                                    
         MH    R0,WORK             X SPOTS                                      
         ST    R0,4(R1)            R1 POINTS TO TOTAL AREA                      
         MVC   10(2,R1),=H'100'    FORCE SVI                                    
         J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
* FILL SPDTTAB WITH X'0B'/X'0C' SPOT DATE ELEMENTS IN BUYREC          *         
***********************************************************************         
FILLSPTB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,BDELEM           A(FIRST BUY ELEMENT)                         
         L     R2,ASPDTTAB         A(SPOT DATES TABLE)                          
         XC    PREVSPDT,PREVSPDT   INIT PREVSPDT                                
         XC    NUMSPOTS,NUMSPOTS   INIT NUMSPOTS                                
*                                                                               
         MVI   ELCDLO,X'0B'        LOOK FOR X'0B' ELEMENTS                      
         MVI   ELCDHI,X'0C'        LOOK FOR X'0C' ELEMENTS                      
*                                                                               
FILLSP10 BRAS  RE,NEXTEL3          HAVE X'0B'/X'0C' ELEMENT?                    
         BNE   FILLSP30            NO - DONE BUILDING DATE TABLE                
*                                                                               
         USING REGELEM,R6          SPOT ELEMENT DSECT                           
         TM    RSTATUS,RSMINUSQ    MINUS SPOT?                                  
         BNZ   FILLSP10            YES - SKIP THIS ELEMENT                      
         TM    RSTATUS,RSMINSDQ    SPOT HAS BEEN MINUSED?                       
         BNZ   FILLSP10            YES - SKIP THIS ELEMENT                      
         CLC   RDATE,PREVSPDT      SAME DATE AS PREVIOUS SPOT?                  
         BNE   FILLSP20            NO - ADD THIS DATE TO THE TABLE              
* SAME SPOT DATE AS PREVIOUS                                                    
         SHI   R2,1                BACK UP TABLE POINTER 1 BYTE                 
         ZIC   R0,0(R2)            CURRENT OF TIMES WE HAVE THIS DATE           
         AHI   R0,1                ADD ONE TO TIMES WE HAVE THIS DATE           
         STC   R0,0(R2)            NUM OF TIMES WE NOW HAVE THIS DATE           
         AHI   R2,1                RESTORE TABLE POINTER                        
         B     FILLSP10            GET NEXT SPOT ELEMENT                        
* NEW UNIQUE SPOT DATE                                                          
FILLSP20 MVC   PREVSPDT,RDATE      SAVE THE CURRENT SPOT DATE                   
         MVC   0(2,R2),RDATE       SPOT DATE IN TABLE                           
         MVI   2(R2),1             SO FAR, THIS DATE OCCURS ONCE                
         AHI   R2,3                BUMP TO NEXT TABLE ENTRY                     
         ZICM  R0,NUMSPOTS,(3)     R0 = NUMBER OF UNIQUE SPOT DATES             
         AHI   R0,1                ADD ONE                                      
         STCM  R0,3,NUMSPOTS       NUMBER OF UNIQUE SPOT DATES                  
         CLC   NUMSPOTS,=H'300'    HAVE UNDER 300 DATE ENTRIES?                 
         BL    FILLSP10            YES - CONTINUE BUILDING DATE TABLE           
         DROP  R6                  DROP SPOT ELEMENT USING                      
*                                                                               
FILLSP30 OC    NUMSPOTS,NUMSPOTS   DO WE HAVE ANY SPOT DATES?                   
         BZ    FILLSPTX            NO - DONE                                    
*                                                                               
         L     RF,AGDEXSPD         A(SPOT POSTING DATES TABLE)                  
         USING DBSPDTD,RF          SPOT POSTING DATES DSECT                     
         MVC   DBSPDNXT,DBEXTEND   LINK TO PREV DBEXTEND                        
         STCM  RF,15,DBEXTEND      SET A(EXTENDED DBLOCK)                       
         MVC   DBSPDTID,=C'SPDT'   SPOT DATE ID                                 
         MVC   DBSPASPT,ASPDTTAB   A(SPOT DATES TABLE)                          
         MVC   DBSPNSPT,NUMSPOTS   NUMBER OF SPOTS                              
         MVI   DBSPLEN,3           3 BYTE ENTRIES ( DATE(2),DUP(1) )            
         DROP  RF                  DROP DBSPDTD USING                           
*                                                                               
FILLSPTX XIT1                      DONE                                         
*                                                                               
NEXTEL3  XR    R0,R0               CLEAR R0                                     
         ICM   R0,1,1(R6)          HAVE ELEMENT LENGTH                          
         BZ    NEXTEL3X            NO - AVOID INFINITE LOOP                     
         AR    R6,R0               BUMP TO NEXT ELEMENT                         
         CLI   0(R6),0             END OF RECORD?                               
         BE    NEXTEL3X            YES - RETURN CC NEQ                          
         CLC   0(1,R6),ELCDLO      ELEMENT < ELCDLO?                            
         JL    NEXTEL3             YES, GET NEXT ELEMENT                        
         CLC   0(1,R6),ELCDHI      ELEMENT > ELCDHI?                            
         JH    NEXTEL3             YES, GET NEXT ELEMENT                        
         CR    RE,RE               SET CC EQU                                   
         BR    RE                  RETURN                                       
NEXTEL3X LTR   RE,RE               SET CC NEQ                                   
         BR    RE                  RETURN                                       
**************************************************************                  
* ROUTINE TO FIGURE OUT END OF YEAR SPLIT WEEK SITUATION                        
* FOR SET METERED MARKETS                                                       
* SPLIT WEEK SITUATION IS A BUY WEEK ROTATION WHICH CROSSES                     
* 2 WEEKS ON THE DEMO FILE                                                      
**************************************************************                  
SMMSPLIT NTR1  BASE=*,LABEL=*                                                   
         CLI   WKINFLAG,C'N'        AND WEEK NOT IN FLAG IS SET                 
         BNE   SMMSPLTX                                                         
         BRAS  RE,ADJVHSDT         GET DAY OF VPHSDAT                           
                                                                                
*------------  SUN TO SAT OOWR WEEK -------------------------                   
SMMSPSUN CLI   ESTEOW,X'07'                                                     
         BNE   SMMSPSAT                                                         
         TM    DBSELDAY,B'00000010' IF SAT ASKED FOR                            
         BZ    SMMSPLTX                                                         
         TM    DAYREAD,B'00000010' IF WE READ SATURDAY DATA THEN                
         BO    SMMSPLTX            WE HAD 53 WEEKS ON RECORD- GOOD              
         NI    DBSELDAY,B'00000010' NO NEED TO LOOKUP ANY SU-F ANYMORE          
         B     SMMSP500            READ NEXT YEARS 00 RECORD                    
                                                                                
*------------  SAT TO FRI OOWR WEEK --------------------------                  
SMMSPSAT CLI   ESTEOW,X'06'                                                     
         BNE   SMMSPFRI            SAT NO SPLIT                                 
         B     SMMSP500            READ NEXT YEARS 00 RECORD                    
                                                                                
*------------  FRI TO THU OOWR WEEK  --------------------------                 
SMMSPFRI CLI   ESTEOW,X'05'                                                     
         BNE   SMMSPTHU                                                         
         TM    DBSELDAY,B'01111011' IF ANY DAY SA-TH ASKED FOR                  
         BZ    SMMSPLTX                                                         
         TM    DAYREAD,B'01111011'  READ ANY SAT-SUN OR M-TH THEN               
         BNZ   SMMSPLTX             WE HAD 53 WEEKS ON THE FILE                 
         NI    DBSELDAY,B'01111011' NO NEED TO LOOKUP ANY FRI ANYMORE           
         B     SMMSP500            READ NEXT YEARS 00 RECORD                    
*                                                                               
*------------  THU TO WED OOWR WEEK  ---------------------------                
SMMSPTHU CLI   ESTEOW,X'04'                                                     
         BNE   SMMSPWED                                                         
         TM    DBSELDAY,B'01110011' IF ANY DAY SA-W ASKED FOR                   
         BZ    SMMSPLTX                                                         
         TM    DAYREAD,B'01110011'  READ ANY SA-SUN DAY OR M-WED                
         BNZ   SMMSPLTX             THEN WE HAD 53 WEEKS ON THE FILE            
         NI    DBSELDAY,B'01110011' NO NEED TO LOOKUP ANY TH-F ANYMORE          
         B     SMMSP500            READ NEXT YEARS 00 RECORD                    
                                                                                
*------------  WED TO TUE OOWR WEEK  ---------------------------                
SMMSPWED CLI   ESTEOW,X'03'                                                     
         BNE   SMMSPTUE                                                         
         TM    DBSELDAY,B'01100011' IF ANY DAY SA-TU ASKED FOR                  
         BZ    SMMSPLTX                                                         
         TM    DAYREAD,B'01100011'  READ ANY SA-SUN DAY OR M-TU                 
         BNZ   SMMSPLTX             THEN WE HAD 53 WEEKS ON THE FILE            
         NI    DBSELDAY,B'01100011' NO NEED TO LOOKUP ANY W-F ANYMORE           
         B     SMMSP500            READ NEXT YEARS 00 RECORD                    
                                                                                
*------------  TUE TO MON OOWR WEEK   ---------------------------               
SMMSPTUE CLI   ESTEOW,X'02'                                                     
         BNE   SMMSPMON                                                         
         TM    DBSELDAY,B'01000011' IF ANY DAY SA-M ASKED FOR                   
         BZ    SMMSPLTX                                                         
         TM    DAYREAD,B'01000011'  READ ANY SA-SUN DAY OR MON                  
         BNZ   SMMSPLTX             THEN WE HAD 53 WEEKS ON THE FILE            
         NI    DBSELDAY,B'01000011' NO NEED TO LOOKUP ANY TU-F ANYMORE          
         B     SMMSP500             READ NEXT YEARS 00 RECORD                   
                                                                                
*------------  MON TO FRI STANDARD WEEK --------------------------              
SMMSPMON TM    DBSELDAY,B'00000011' 1 DAY SAT OR SUNDAY OR BOTH                 
         BZ    SMMSPLTX                                                         
         TM    DAYREAD,B'00000011'  IF WE READ SAT-SUN THEN WE HAD              
         BNZ   SMMSPLTX             53 WEEKS ON THE FILE                        
         NI    DBSELDAY,B'00000011' NO NEED TO LOOKUP ANY M-F ANYMORE           
         B     SMMSP500             READ NEXT YEARS RECORD                      
*                                   ELSE                                        
SMMSP500 MVC   DBSELBK,SVYEAR       BUMP TO NEXT YEAR'S 00 RECORD               
         ZIC   RE,DBSELBK                                                       
         LA    RE,1(RE)                                                         
         STC   RE,DBSELBK                                                       
         MVI   DBSELBK+1,0                                                      
         MVI   NEXTYRLK,C'Y'        FLAG TO INDICATE NEXT YR 1ST WK             
SMMSPLTX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**************************************************************                  
* ROUTINE FOR DSTATION CALL LETTER LINK END OF YEAR                             
* SPLIT SCENARIO FOR SMM MARKETS                                                
*************************************************************                   
DSTASPLIT NTR1 BASE=*,LABEL=*                                                   
         CLI   WKINFLAG,C'N'       AND NOT WEEK IN FLAG IS SET                  
         BNE   DSTAPLTX                                                         
                                                                                
*------------  SUN TO SAT OOWR WEEK --------------------------                  
DSTASUN  CLI   ESTEOW,X'07'                                                     
         BNE   DSTASAT                                                          
         TM    DBSELDAY,B'00000010' IF SAT ASKED FOR                            
         BZ    DSTAPLTX                                                         
         TM    DAYREAD,B'00000010' IF SAT READ THEN WE HAD 53 WEEKS             
         BNZ   DSTAPLTX            ON THE RECORD                                
         NI    DBSELDAY,B'00000010' NO NEED TO LOOKUP ANY SU-F ANYMORE          
         B     DSTA200             READ NEXT YEARS 00 RECORD                    
                                                                                
*------------  SAT TO FRI OOWR WEEK ----------------------------                
DSTASAT  CLI   ESTEOW,X'06'                                                     
         BNE   DSTAFRI             SAT NO SPLIT                                 
         B     DSTA200             READ NEXT YEARS 00 RECORD                    
                                                                                
*------------  FRI TO THU OOWR WEEK ----------------------------                
DSTAFRI  CLI   ESTEOW,X'05'                                                     
         BNE   DSTATHU                                                          
         TM    DBSELDAY,B'01111011' IF ANY DAY SA-TH ASKED FOR                  
         BZ    DSTAPLTX                                                         
         TM    DAYREAD,B'01111011' IF SA-SUN PR M-TH READ THEN WE               
         BNZ   DSTAPLTX            HAD 53 WEEKS ON THE FILE                     
         NI    DBSELDAY,B'01111011' NO NEED TO LOOKUP ANY FRI ANYMORE           
         B     DSTA200             READ NEXT YEARS 00 RECORD                    
                                                                                
*------------  THU TO WED OOWR WEEK -----------------------------               
DSTATHU  CLI   ESTEOW,X'04'                                                     
         BNE   DSTAWED                                                          
         TM    DBSELDAY,B'01110011' IF ANY DAY SA-W ASKED FOR                   
         BZ    DSTAPLTX                                                         
         TM    DAYREAD,B'01110011' IF ANY DAY SA-SUN OR M-W READ                
         BNZ   DSTAPLTX            THEN WE HAD 53 WEEKS ON THE FILE             
         NI    DBSELDAY,B'01110011' NO NEED TO LOOKUP ANY TH-F ANYMORE          
         B     DSTA200             READ NEXT YEARS 00 RECORD                    
                                                                                
*------------- WED TO TUE OOWR WEEK ------------------------------              
DSTAWED  CLI   ESTEOW,X'03'                                                     
         BNE   DSTATUE                                                          
         TM    DBSELDAY,B'01100011' IF ANY DAY SA-TU ASKED FOR                  
         BZ    DSTAPLTX                                                         
         TM    DAYREAD,B'01100011' IF ANY DAY SA-SUN OR M-TU READ               
         BNZ   DSTAPLTX            THEN WE HAD 53 WEEKS ON THE FILE             
         NI    DBSELDAY,B'01100011' NO NEED TO LOOKUP ANY W-F ANYMORE           
         B     DSTA200             READ NEXT YEARS 00 RECORD                    
                                                                                
*------------  TUE TO MON OOWR WEEK --------------------------------            
DSTATUE  CLI   ESTEOW,X'02'                                                     
         BNE   DSTAMON                                                          
         TM    DBSELDAY,B'01000011' IF ANY DAY SA-M ASKED FOR                   
         BZ    DSTAPLTX                                                         
         TM    DAYREAD,B'01000011' IF ANY DAY SA-SUN OR MON READ                
         BNZ   DSTAPLTX            THEN WE HAD 53 WEEKS ON THE FILE             
         NI    DBSELDAY,B'01000011' NO NEED TO LOOKUP ANY TU-F ANYMORE          
         B     DSTA200             READ NEXT YEARS 00 RECORD                    
                                                                                
*------------  MO-FRI STANDARD WEEK ---------------------------------           
DSTAMON  TM    DBSELDAY,B'00000011' 1 DAY SAT OR SUNDAY                         
         BZ    DSTAPLTX                                                         
         TM    DAYREAD,B'00000011' IF ANY DAY SA-SUN READ                       
         BNZ   DSTAPLTX            THEN WE HAD 53 WEEKS ON THE FILE             
         NI    DBSELDAY,B'00000011'                                             
         B     DSTA200                                                          
                                                                                
DSTA200  MVC   DBSELSTA,DSTACALL    CALL LETTER LINK                            
         MVI   DSTASPLT,C'Y'        INDICATE DSTATION CALL LETTER               
         MVI   NEXTYRLK,C'N'        RESET FLAG                                  
DSTAPLTX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
**************************************************************                  
* ROUTINE TO FIGURE OUT WEEKLY DBSELBK FOR ACHIEVED LOOKUP   *                  
**************************************************************                  
ACHWKBK  NTR1  BASE=*,LABEL=*                                                   
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,VPHSDAT),(0,DUB) GET THE WEEK DATE                  
*                                                                               
* WE NEED TO ADJUST THE BOOK TO THE WEEK THE ROTATION STARTS ON                 
* BASED ON  WHAT THE BUY WEEK DEFINITION IS AND THE DAY                         
* THE SPOTDATE IS ON WE CAN DETERMINE HOW TO ADJUST THE                         
* WEEK TO THE BEGINNING OF THE ROTATION.                                        
*                                                                               
ACHWKB58 DS    0C                                                               
         CLC   =C'NNY',LPMWKOPT   OVERNIGHTS                                    
         BE    ACHWKB59                                                         
         BRAS  RE,ADJVHSDT        GET SPOTDAY (HOLDS DAY OF AFFID SPOT)         
*                                                                               
* SUNDAY TO SAT OOWR WEEK                                                       
ACHWKE07 DS    0C                                                               
         CLI   ESTEOW,X'07'        SUN-SAT ESTIMATE OOW INDICATOR               
         BNE   ACHWKE06                                                         
         MVI   DBBEST,X'60'        INDICATOR TO GETTP SUN-SAT WEEK              
         CLI   BISPTDAY,X'06'      IF SPOT IS SAT THEN BUMP BACK A WEEK         
         BNE   ACHWKB60            DBBEST=X'60' TELLS GETTP HOW TO              
         B     ACHWKSUB            ADJUST THE BOOK                              
* SAT TO FRIDAY OOWR WEEK                                                       
ACHWKE06 DS    0C                                                               
         CLI   ESTEOW,X'06'                                                     
         BNE   ACHWKE05                                                         
         MVI   DBBEST,X'65'        INDICATOR TO GETTP SAT-FRI WEEK              
         B     ACHWKB60                                                         
* FRI TO THURS OOWR WEEK                                                        
ACHWKE05 DS    0C                                                               
         CLI   ESTEOW,X'05'        FRI-THU ESTIMATE OOW INDICATOR               
         BNE   ACHWKE04                                                         
         MVI   DBBEST,X'64'        INDICATOR TO GETTP FRI-THU WEEK              
         CLI   BISPTDAY,X'04'      IF SPOT IS BETWEEN MON-THU THEN              
         BNH   ACHWKSUB            BUMP THE BOOK BACK 1 WEEK                    
         CLI   BISPTDAY,X'06'      IF SPOT IS BETWEEN SAT-SUN THEN              
         BNL   ACHWKSUB            BUMP THE BOOK BACK 1 WEEK                    
         B     ACHWKB60            ELSE FRI SPOT KEEP SAME WEEK                 
* THU TO WED OOWR WEEK                                                          
ACHWKE04 DS    0C                                                               
         CLI   ESTEOW,X'04'        THU-WED ESTIMATE OOW INDICATOR               
         BNE   ACHWKE03                                                         
         MVI   DBBEST,X'63'        INDICATOR TO GETTP THU-WED WEEK              
         CLI   BISPTDAY,X'03'      IF SPOT IS BETWEEN MON-WED THEN              
         BNH   ACHWKSUB            BUMP THE BOOK BACK 1 WEEK                    
         CLI   BISPTDAY,X'06'      IF SPOT IS BETWEEN SAT-SUN THEN              
         BNL   ACHWKSUB            BUMP THE BOOK BACK 1 WEEK                    
         B     ACHWKB60            ELSE THU-FRI SPOT KEEP SAME WEEK             
* WED TO TUES OOWR WEEK                                                         
ACHWKE03 DS    0C                                                               
         CLI   ESTEOW,X'03'        WED-TUE ESTIMATE OOW INDICATOR               
         BNE   ACHWKE02                                                         
         MVI   DBBEST,X'62'        INDICATOR TO GETTP WED-TUE WEEK              
         CLI   BISPTDAY,X'02'      IF SPOT IS BETWEEN MON-TUE THEN              
         BNH   ACHWKSUB            BUMP THE BOOK BACK 1 WEEK                    
         CLI   BISPTDAY,X'06'      IF SPOT IS BETWEEN SAT-SUN THEN              
         BNL   ACHWKSUB            BUMP THE BOOK BACK 1 WEEK                    
         B     ACHWKB60            ELSE WED-FRI SPOT KEEP SAME WEEK             
* TUE TO MONDAY OOWR WEEK                                                       
ACHWKE02 DS    0C                                                               
         CLI   ESTEOW,X'02'        TUE-MON ESTIMATE OOW INDICATOR               
         BNE   ACHWKE01                                                         
         MVI   DBBEST,X'61'        INDICATOR TO GETTP TUE-MON WEEK              
         CLI   BISPTDAY,X'01'      IF SPOT IS MONDAY THEN                       
         BE    ACHWKSUB            BUMP THE BOOK BACK 1 WEEK                    
         CLI   BISPTDAY,X'06'      IF SPOT IS BETWEEN SAT-SUN THEN              
         BNL   ACHWKSUB            BUMP THE BOOK BACK 1 WEEK                    
         B     ACHWKB60            ELSE TUE-FRI SPOT KEEP SAME WEEK             
* MON TO FRI STANDARD WEEK                                                      
ACHWKE01 DS    0C                                                               
         CLI   ESTEOW,X'00'        MON-FRI STANDARD WEEK                        
         BE    *+8                                                              
         CLI   ESTEOW,X'01'        MON-FRI                                      
         BNE   ACHWKB60                                                         
         MVI   DBBEST,0                                                         
         CLI   BISPTDAY,X'06'      SAT/SUN SPOT BUMP BACK A WEEK                
         BNL   ACHWKSUB                                                         
         B     ACHWKB60                                                         
*                                                                               
* OVERNGHTS FAILED AFFID ADJUSTMENT                                             
* ADJUST TO THE START OF THE WEEK SPECIED BY ESTEOW                             
*                                                                               
* SUNDAY TO MONDAY OOWR WEEK                                                    
ACHWKB59 DS    0C                                                               
ACHOVE07 DS    0C                                                               
         CLI   ESTEOW,X'07'        SUN-SAT ESTIMATE OOW INDICATOR               
         BNE   ACHOVE06                                                         
         MVI   DBBEST,X'16'        INDICATOR TO GETTP SUN-SAT WEEK              
         CLI   BISPTDAY,X'06'      IF SPOT IS M-SA THEN BUMP BACK A WK          
         BNH   ACHWKSUB            DBBEST=X'60' TELLS GETTP HOW TO              
         B     ACHWKB60            SUNDAY AFFID SPOT KEEP SAME WEEK             
* SAT TO FRIDAY OOWR WEEK                                                       
ACHOVE06 DS    0C                                                               
         CLI   ESTEOW,X'06'                                                     
         BNE   ACHOVE05                                                         
         MVI   DBBEST,X'15'                                                     
         CLI   BISPTDAY,5          IF SPOT IS M-F THEN BUMP BACK A WEEK         
         BNH   ACHWKSUB                                                         
         B     ACHWKB60                                                         
* FRI TO THURS OOWR WEEK                                                        
ACHOVE05 DS    0C                                                               
         CLI   ESTEOW,X'05'        FRI-THU ESTIMATE OOW INDICATOR               
         BNE   ACHOVE04                                                         
         MVI   DBBEST,X'14'        INDICATOR TO GETTP FRI-THU WEEK              
         CLI   BISPTDAY,X'04'      IF SPOT IS BETWEEN MON-THU THEN              
         BNH   ACHWKSUB            BUMP THE BOOK BACK 1 WEEK                    
         B     ACHWKB60            ELSE FRI-SUN SPOT KEEP SAME WEEK             
* THU TO WED OOWR WEEK                                                          
ACHOVE04 DS    0C                                                               
         CLI   ESTEOW,X'04'        THU-WED ESTIMATE OOW INDICATOR               
         BNE   ACHOVE03                                                         
         MVI   DBBEST,X'13'        INDICATOR TO GETTP THU-WED WEEK              
         CLI   BISPTDAY,X'03'      IF SPOT IS BETWEEN MON-WED THEN              
         BNH   ACHWKSUB            BUMP THE BOOK BACK 1 WEEK                    
         B     ACHWKB60            ELSE THU-SUN SPOT KEEP SAME WEEK             
* WED TO TUES OOWR WEEK                                                         
ACHOVE03 DS    0C                                                               
         CLI   ESTEOW,X'03'        WED-TUE ESTIMATE OOW INDICATOR               
         BNE   ACHOVE02                                                         
         MVI   DBBEST,X'12'        INDICATOR TO GETTP WED-TUE WEEK              
         CLI   BISPTDAY,X'02'      IF SPOT IS BETWEEN MON-TUE THEN              
         BNH   ACHWKSUB            BUMP THE BOOK BACK 1 WEEK                    
         B     ACHWKB60            ELSE WED-SUN SPOT KEEP SAME WEEK             
* TUE TO MONDAY OOWR WEEK                                                       
ACHOVE02 DS    0C                                                               
         CLI   ESTEOW,X'02'        TUE-MON ESTIMATE OOW INDICATOR               
         BNE   ACHOVE01                                                         
         MVI   DBBEST,X'10'        INDICATOR TO GETTP TUE-MON WEEK              
         CLI   BISPTDAY,X'01'      IF SPOT IS MONDAY THEN                       
         BE    ACHWKSUB            BUMP THE BOOK BACK 1 WEEK                    
         B     ACHWKB60            ELSE TUE-SUN SPOT KEEP SAME WEEK             
* MON TO FRI STANDARD WEEK                                                      
ACHOVE01 DS    0C                                                               
         CLI   ESTEOW,X'00'        MON-FRI STANDARD WEEK                        
         BE    *+8                                                              
         CLI   ESTEOW,X'01'        TUE-MON ESTIMATE OOW INDICATOR               
         BNE   ACHWKB60                                                         
         MVI   DBBEST,0                                                         
         B     ACHWKB60                                                         
*                                                                               
ACHWKADD DS    0X                                                               
         L     RF,DBCOMFCS                    ADJUST TO NEXT WEEK               
         L     RF,CADDAY-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,DUB,DUB,7                                              
         B     ACHWKB60                                                         
ACHWKSUB DS    0X                                                               
         L     RF,DBCOMFCS                    ADJUST A WEEK BACK                
         L     RF,CADDAY-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,DUB,DUB,-7                                             
         B     ACHWKB60                                                         
*=============================================================                  
ACHWKB60 DS    0X                                                               
*    CONVERT DBSELBK                                                            
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         MVC   WORK(12),DMCB+4                                                  
         CLI   SVD0PROF+11,C'M'              OVERNIGHTS?                        
         BE    *+8                                                              
         CLI   SVD0PROF+11,C'Y'              OVERNIGHTS?                        
         BNE   *+10                                                             
         CLC   =C'NNY',LPMWKOPT                                                 
         BNE   *+8                                                              
         MVI   DMCB+4,1                      STARTDAY=MONDAY                    
         GOTO1 =V(NSIWEEK),DMCB,DUB,RR=RELO  CONVERT TO A BOOK                  
         MVC   DBSELBK(1),DMCB+4             AND SET IT IN DBLOCK               
         MVC   DBSELBK+1(1),DMCB                                                
                                                                                
ACHWKB80 DS     0X                                                              
*                                                                               
ACHWKBKX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
**************************************************************                  
* ROUTINE TO FIGURE OUT WEEKLY DBSELBK FOR AFFIDS            *                  
**************************************************************                  
AFFWKBK  NTR1  BASE=*,LABEL=*                                                   
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,VPHSDAT),(0,DUB) GET THE WEEK DATE                  
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         MVC   WORK(12),DMCB+4                                                  
         GOTO1 =V(NSIWEEK),DMCB,DUB,RR=RELO  CONVERT TO A BOOK                  
         MVC   DBSELBK(1),DMCB+4             AND SET IT IN DBLOCK               
         MVC   DBSELBK+1(1),DMCB                                                
* SINCE GETTP DOES THE ADJUSTMENT OF SAT/SUN BY BUMPING A WEEK                  
* AHEAD FOR SAT/SUN READJUST SAT/SUN BACK A WEEK TO SAME                        
* AS M-F.  THIS HOW THE BOOK IS SET LPMWK=Y BK=ACT                              
* FOR LPMWK=Y BK=ACT                                                            
* SAT AND SUN IS THE SAME WEEK# AS M-F FOR LPMMWK=Y,BK=ACT                      
         CLI   DBSELDAY,X'03'                                                   
         BH    AFFWKBK6                                                         
         ZIC   RE,DBSELBK+1                                                     
         SHI   RE,1                                                             
         STC   RE,DBSELBK+1                                                     
         CLI   DBSELBK+1,0           CANT HAVE 0 WEEK NUMBER                    
         BNE   AFFWKBK6                                                         
         ZIC   RE,DBSELBK            SO THAT GETTP WILL KNOW TO ADJUST          
         SHI   RE,1                  THIS BACK TO 1ST WEEK OF NEXT YEAR         
         STC   RE,DBSELBK                                                       
         MVI   DBSELBK+1,53                                                     
***      MVI   DBSELBK+1,54                                                     
AFFWKBK6 DS    0H                                                               
*                                                                               
         CLI   DIGITALF,C'Y'                 DIGITIAL TRANSITION                
         BE    *+8                           POST AGAINST OVERNIGHT             
         CLI   SVD0PROF+11,C'M'              OVERNIGHTS?                        
         BE    *+8                                                              
         CLI   SVD0PROF+11,C'Y'              OVERNIGHTS?                        
         BNE   AFFWKBKX                                                         
         CLI   DIGITALF,C'Y'                 DIGITIAL TRANSITION                
         BE    *+10                          POST AGAINST OVERNIGHT             
         CLC   =C'NNY',LPMWKOPT              IF OVERNIGHTS ONLY                 
         BNE   AFFWKBKX                      READ OVERNIGHTS                    
         MVI   DBSELMED,C'O'                                                    
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,VPHSDAT),(0,DUB) GET THE WEEK DATE                  
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         MVC   WORK(12),DMCB+4                                                  
         MVI   DMCB+4,1                      STARTDAY=MONDAY                    
         GOTO1 =V(NSIWEEK),DMCB,DUB,RR=RELO  CONVERT TO A BOOK                  
         MVC   DBSELBK(1),DMCB+4             AND SET IT IN DBLOCK               
         MVC   DBSELBK+1(1),DMCB                                                
AFFWKBKX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*****************************************************************               
* ROUTINE TO GET THE BUYERS ESTIMATES INTO A(TABLE) PASSED                      
* AT 0(R1)                                                                      
* IGNORE THE 2 DECIMAL X'40' BIT - 2 DEC IS ASSUMED                             
* IF NO COMSCORE ERRORS, ON AFFID LOOKUP, JUST RETURN NSI VALS                  
*****************************************************************               
                                                                                
GETBYEST NTR1  BASE=*,LABEL=*                                                   
         L     RE,ADDEMEL                                                       
         L     R4,NUMDEMS                                                       
         LA    RF,NDEMNO-NDELEM(,RE)  1ST DEMO                                  
*                                                                               
GETBYE10 CLI   COMERROR,0             IF COMSCORE ERROR                         
         JNE   GETBYE20               RETURN ALL DEMOS                          
         CLI   2(RF),0                ELSE TEST COMSCORE DEMO                   
         JE    GETBYE22               YES - SKIP                                
*                                                                               
GETBYE20 MVC   0(4,R1),4(RF)          MOVE IN DEMO VALUES                       
         NI    0(R1),X'BF'            DROP 2 DECIMAL FLAG                       
*                                                                               
GETBYE22 LA    R1,4(R1)                                                         
         LA    RF,8(RF)                                                         
         BCT   R4,GETBYE10                                                      
*                                                                               
         LA    R1,DTDEMSA                                                       
         BRAS  RE,GDXSVI           MULTIPLY ADJ FACTORS X DEMOS                 
         J     EXIT                                                             
                                                                                
*============================================================                   
* SET DEMOS TO BUYERS ESTIMATES AND GET COMSCORE DEMOS                          
*============================================================                   
                                                                                
SETBYEST NTR1  BASE=*,LABEL=*                                                   
         LA    R1,DTDEMSU                                                       
         BRAS  RE,GETBYEST                                                      
*                                                                               
         LA    R1,SVIS                                                          
         LA    R0,MAXDEMS                                                       
         MVC   0(2,R1),=H'100'                                                  
         LA    R1,2(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         LA    R1,DTDEMSA                                                       
         BRAS  RE,GDXSVI                                                        
         MVC   BUDEMSA,DTDEMSA                                                  
         MVC   BUDEMSU,DTDEMSU                                                  
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
**************************************************************                  
* ADJUST VPHSDAT UNTIL DAY OF THE DATE EQUALS THE STARTDAY OF*                  
* THE ROTATION                                               *                  
* ENTRY VPHSDAT = DATE                                       *                  
*       DBSELDAY = ROTATION                                  *                  
* EXIT  ADJDATE =DATE ADJUSTED TO MATCH THE DAY OF ROTATION  *                  
**************************************************************                  
ADJVHSDT NTR1  BASE=*,LABEL=*                                                   
         OC    VPHSDAT,VPHSDAT                                                  
         BZ    ADJVHDTX                                                         
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,VPHSDAT),(0,DUB3)                                   
         L     RF,DBCOMFCS         CORRECTLY                                    
         L     RF,CGETDAY-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DUB3,SPOTDAY                                           
         MVC   BISPTDAY,0(R1)                                                   
*                                                                               
         ZIC   RE,0(R1)            DAY NUMBER FROM GETDAY                       
         ICM   R1,1,DBSELDAY                                                    
         BNZ   *+6                 SOMETHING IS WRONG IF DBSELDAY               
         DC    H'0'                IS NOT SET                                   
         SR    R0,R0                                                            
         SR    R4,R4                                                            
         SLL   R1,24                                                            
*                                                                               
ADJVHDT6 SLDL  R0,1                FIND DAY OF WEEK                             
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         BCT   R4,ADJVHDT6                                                      
         LPR   R4,R4               R4 HAS DAY OF ROTATION                       
         SR    R4,RE               GET THE DIFFERENCE IN # OF DAYS              
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,DUB3,ADJDATE,(R4)                                      
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(0,ADJDATE),(3,ADJDATE)                                
ADJVHDTX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**************************************************************                  
* ADJUST DATE IN DUB UNTIL TO START OF THE ROTATION.         *                  
* ENTRY DUB = DATE, YMD(BINARY)                              *                  
*       DBSELDAY = ROTATION                                  *                  
* EXIT  DUB =DATE ADJUSTED TO MATCH THE START OF ROTATION    *                  
**************************************************************                  
ADJDADAT NTR1  BASE=*,LABEL=*                                                   
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(3,DUB),(0,DUB2)                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DUB2,DUB3                                              
*                                                                               
         ZIC   RE,0(R1)            DAY NUMBER FROM GETDAY                       
         ICM   R1,1,DBSELDAY                                                    
         BNZ   *+6                 SOMETHING IS WRONG IF DBSELDAY               
         DC    H'0'                IS NOT SET                                   
         SR    R0,R0                                                            
         SR    R4,R4                                                            
         SLL   R1,24                                                            
*                                                                               
ADJADAT6 SLDL  R0,1                FIND DAY OF WEEK                             
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         BCT   R4,ADJADAT6                                                      
         LPR   R4,R4               R4 HAS DAY OF ROTATION                       
         SR    R4,RE               GET THE DIFFERENCE IN # OF DAYS              
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,DUB2,DUB3,(R4)                                         
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(0,DUB3),(3,DUB)                                       
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
************************************************************                    
* GETDEMHK HOOK ROUTINE                                    *                    
************************************************************                    
*                                                                               
GETDMHK  NTR1  BASE=*,LABEL=*                                                   
*  MAKE SURE WE ARE INDEXING OFF THE SAME ROTATION WE READ FOR WTP              
         L     RE,DBAQUART         GET ADDRESS OF QHELEM                        
         OC    DAYREAD,DAYREAD                                                  
         JZ    GDHK1                                                            
GDHKSUN  CLI   2(RE),X'70'         SUNDAY                                       
         BNE   *+16                                                             
         TM    DAYREAD,X'01'       IF SUNDAY WASNT READ FOR SMM WTP             
         BZ    GDHKEXC             THEN EXCLUDE IT                              
         B     GDHK1                                                            
                                                                                
GDHKSAT  CLI   2(RE),X'60'         SAT                                          
         BNE   *+16                                                             
         TM    DAYREAD,X'02'       IF SAT WASNT READ FOR SMM WTP                
         BZ    GDHKEXC             THEN EXCLUDE IT                              
         B     GDHK1                                                            
                                                                                
GDHKFRI  CLI   2(RE),X'50'         FRI                                          
         BNE   *+16                                                             
         TM    DAYREAD,X'04'       IF FRI WASNT READ FOR SMM WTP                
         BZ    GDHKEXC             THEN EXCLUDE IT                              
         B     GDHK1                                                            
                                                                                
GDHKTHU  CLI   2(RE),X'40'         THU                                          
         BNE   *+16                                                             
         TM    DAYREAD,X'08'       IF THU WASNT READ FOR SMM WTP                
         BZ    GDHKEXC             THEN EXCLUDE IT                              
         B     GDHK1                                                            
                                                                                
GDHKWED  CLI   2(RE),X'30'         WED                                          
         BNE   *+16                                                             
         TM    DAYREAD,X'10'       IF WED WASNT READ FOR SMM WTP                
         BZ    GDHKEXC             THEN EXCLUDE IT                              
         B     GDHK1                                                            
*                                                                               
GDHKTUE  CLI   2(RE),X'20'         TUE                                          
         BNE   *+16                                                             
         TM    DAYREAD,X'20'       IF TUE WASNT READ FOR SMM WTP                
         BZ    GDHKEXC             THEN EXCLUDE IT                              
         B     GDHK1                                                            
*                                                                               
GDHKMON  CLI   2(RE),X'10'         MON                                          
         BNE   GDHK1                                                            
         TM    DAYREAD,X'40'       IF MON WASNT READ FOR SMM WTP                
         BZ    GDHKEXC             THEN EXCLUDE IT                              
         B     GDHK1                                                            
                                                                                
GDHKEXC  LH    R0,DBDIVSOR         IF WE ARE IGNORING THIS QHR THEN             
         SHI   R0,1                ADJUST DIVSOR FOR CORRECT WEIGHT             
         STH   R0,DBDIVSOR                                                      
         J     EXIT                                                             
*                                                                               
GDHK1    CLI   DEMANDSW,0          TEST HERE PREVIOUSLY                         
         JNE   GDHK2               YES                                          
         MVI   DEMANDSW,C'Y'       SET SWITCH THAT WE WERE HERE                 
*                                                                               
         LA    R0,4                CLEAR DEMAND RTG/PUT/IMP/TOT AREAS           
         L     R1,ADTDEMS                                                       
         XC    0(4*IUNDEMS,R1),0(R1)                                            
         LA    R1,4*IUNDEMS(R1)                                                 
         BCT   R0,*-10                                                          
         XC    DTVUTS,DTVUTS                                                    
                                                                                
* MUST SAVE ACTUAL BOOK NOW (EXPLETIVE DELETED) *                               
         MVC   SVACTBK,DBACTBK     SET IT BECAUSE DEMOUT CHANGES IT             
                                                                                
* CALL DEFINE FOR MARKET NUMBER *                                               
                                                                                
         GOTO1 VDEFINE,P1,=C'MARKET',DBLOCK,SAVEMKT                             
                                                                                
         EJECT                                                                  
* EXTRACT UNIVS/RTGS/IMPS/PUTS/TOTS *                                           
                                                                                
GDHK2    TM    FLAGS,CANHPT        TEST CANADIAN HPT'S                          
         JNZ   GDHKCN                                                           
         CLI   DBSELMED,C'R'       ARB RADIO FOLLOWS CANADA,                    
         JE    GDHKCN               FOR NOW                                     
         MVC   SVTPOPT,TAPEOPT                                                  
*                                                                               
*                                                                               
         L     RE,DBAREC           MAKE SURE BOSTON GETS                        
         CLC   0(3,RE),=C'RON'     EXTENDED PRECISION FOR FILES                 
         JE    RONPREC             WITH PERSONS DEMOS                           
         CLC   0(3,RE),=C'RWN'     EXTENDED PRECISION FOR FILES                 
         JNE   BOSWTPX             WITH PERSONS DEMOS                           
         CLC   DBACTRMK,=H'106'                                                 
         JNE   GDHK03                                                           
         CLC   DBACTBK,=X'6627'                                                 
         JL    BOSWTPX                                                          
RONPREC  MVI   TAPEOPT,C'Y'        HAVE DEMOS - FORCE TO IMP BASED              
         J     BOSWTPX                                                          
*          DATA SET SPGETDEMF  AT LEVEL 083 AS OF 03/24/05                      
** OTHER WEEKLY MARKETS - CHECK IF LPM BY CHECKING '5E' ELEMENT                 
** IF TTN IN '5E' THEN WE ARE LPM                                               
GDHK03   LH    RE,DBDTADSP                                                      
         L     R0,DBAREC                                                        
         AR    RE,R0                                                            
         SR    R0,R0                                                            
GDHK034  CLI   0(RE),0            TEST E-O-R                                    
         JE    BOSWTPX                                                          
         CLI   0(RE),X'5E'                                                      
         JE    GDHK035                                                          
         ZIC   R0,1(RE)           LENGTH OF ELEMENT                             
         AR    RE,R0                                                            
         J     GDHK034                                                          
GDHK035  CLC   =C'TTN',2(RE)      LPM WEEKLY?                                   
         JNE   GDHK035X                                                         
         MVI   TAPEOPT,C'Y'       IMPRESSION BASED                              
         J     BOSWTPX                                                          
GDHK035X DS    0C                                                               
         MVI   TAPEOPT,C'N'       RATINGS BASED                                 
*                                                                               
BOSWTPX  DS    0C                                                               
*                                                                               
         CLI   WVPHOPT,C'Y'        DOING WEEKLY ADJUST                          
         JNE   *+8                                                              
         MVI   TAPEOPT,C'Y'        CAN ONLY DO FOR IMP BASED                    
*                                                                               
         CLI   DBACTSRC,C'F'       TAPEOPT FOR FUSION ALSO                      
         JE    *+8                                                              
         CLI   DBACTSRC,C'N'       TAPEOPT ONLY FOR NSI                         
         JE    *+8                                                              
         CLI   DBACTSRC,C'C'       TAPEOPT ONLY FOR NCM                         
         JE    *+8                                                              
         MVI   TAPEOPT,C'N'                                                     
*                                                                               
         MVC   DBTAPEP,TAPEOPT     FORCE DBLOCK PREC OPTION                     
*                                                                               
         TM    TWODEC,X'01'        TEST 2-DEC IMP REQUEST                       
         JZ    *+8                                                              
         OI    DBBSTFLG,DBBST2DI   THEN REQUEST THEM STUPIDO                    
*                                                                               
         GOTO1 VGETIUN,P1,(IUNTYPE,DBLOCK),ADTUNVS,DEMOLIST                     
                                                                                
* LOOK UP VUTS (HUTS USED TO DERIVE SHARES) *                                   
                                                                                
GDHK04   L     RF,DBCOMFCS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         GOTO1 (RF),P1,(C'L',AVUTLIST),DBLOCK,QHVUTS                            
                                                                                
         OC    QHVUTS(4),QHVUTS    ANY VUTS                                     
         JNZ   GDHK06                                                           
         GOTO1 (RF),P1,(C'L',APUTLIST)  NO USE PUTS                             
                                                                                
GDHK06   CLI   TAPEOPT,C'Y'                                                     
         JNE   GDHK10                                                           
         GOTO1 (RF),P1,(C'L',AUNILIST),DBLOCK,QHUNIV                            
         L     RE,QHUNIV           CONVERT TO IMP BASED                         
         L     RF,QHVUTS                                                        
         MR    RE,RE                                                            
         ST    RF,QHVUTS                                                        
         L     RE,QHUNIV+4                                                      
         L     RF,QHVUTS+4                                                      
         MR    RE,RE                                                            
         ST    RF,QHVUTS+4                                                      
         L     RE,QHUNIV+8                                                      
         L     RF,QHVUTS+8                                                      
         MR    RE,RE                                                            
         ST    RF,QHVUTS+8                                                      
*                                                                               
* SET 1 WEEK DATA INDICATOR *                                                   
* IF EXACTLY ONE OR TWO BITS ON, CONSIDER THE DATA TO BE WEEKLY *               
                                                                                
GDHK10   MVC   DUB(1),DBACT1WK     GET REQUESTED WEEK BITS                      
         L     RE,DBAQUART         GET ADDRESS OF QHELEM                        
         XC    DUB(1),5(RE)        CLEAR REQUESTED WEEK BITS                    
         MVI   DUB+1,X'80'         SET IND FOR WEEKLY DATA                      
         SR    RE,RE                                                            
         ICM   RE,1,DUB            TEST ANY UNREQUESTED WEEKS                   
         JZ    GDHK11              NO                                           
         SLL   RE,28                                                            
         J     *+8                                                              
         SLL   RE,1                                                             
         LTR   RE,RE               SHIFT UNTIL NEGATIVE                         
         BP    *-6                                                              
         SLL   RE,1                SHIFT ONE MORE TIME                          
         LTR   RE,RE               AND SEE IF ANY MORE BITS                     
         JZ    GDHK11              IF NO MORE, EXACTLY TWO WERE ON              
         MVI   DUB+1,X'40'         ELSE SET IND FOR NON-WEEKLY DATA             
*                                                                               
GDHK11   OC    WKLYIND,DUB+1       SAVE FOR LATER                               
                                                                                
* ACCUMULATE VUTS FOR HOME SHARE CALCULATIONS *                                 
                                                                                
         LA    R0,3                                                             
         LA    R4,QHVUTS                                                        
         LA    R5,DTVUTS                                                        
         SR    R7,R7                                                            
*                                                                               
GDHK12   SR    RE,RE                                                            
         L     RF,0(R4,R7)         GET VALUE                                    
         A     RF,=F'5'                                                         
         D     RE,=F'10'                                                        
         M     RE,=F'10'                                                        
         MH    RF,DBFACTOR         X NUM QH                                     
         A     RF,0(R5,R7)         ADD TO TOTAL                                 
         ST    RF,0(R5,R7)                                                      
         LA    R7,4(R7)                                                         
         BCT   R0,GDHK12                                                        
*                                                                               
GDHK28   CLI   0(R3),3             TEST SPILL DEMO EL                           
         JE    EXIT                                                             
                                                                                
* REQUEST PROGRAM NAME + WEEKS *                                                
                                                                                
         LA    R4,ENDNAME                                                       
         CLI   DBSELXQH,C'Y'       TEST EXTENDED QH LOOKUP                      
         JNE   GDHK30                                                           
         LA    R4,NEWNAME                                                       
         CLI   XQHNUM,0            TEST FIRST TIME                              
         JE    GDHK30              YES                                          
         MVC   ENDNAME,NEWNAME                                                  
*                                                                               
GDHK30   GOTO1 VDEFINE,P1,=C'PROG+',DBLOCK,(R4)                                 
         OC    STNAME,STNAME                                                    
         JNZ   *+10                                                             
         MVC   STNAME,ENDNAME                                                   
*                                                                               
         CLI   DBSELXQH,C'Y'       TEST EXTENDED QH LOOKUP                      
         JNE   GDHK32                                                           
         CLI   XQHNUM,1            TEST TO SET START NAME                       
         JH    GDHK32              NO                                           
         MVC   STNAME,NEWNAME      SET NAME                                     
         ZIC   RE,XQHNUM           AND ADJUST COUNTER                           
         LA    RE,1(RE)                                                         
         CLC   DBACTSQC,DBACTEQC   TEST REC COVERS MULT QH'S                    
         JE    *+8                                                              
         LA    RE,1(RE)                                                         
         STC   RE,XQHNUM                                                        
GDHK32   J     EXIT                                                             
*                                                                               
GDHKCN   DS    0H                                                               
         XC    WORK,WORK           MAD RECORD INTO ADUMMY                       
         LA    R0,DBLOCK                                                        
         ST    R0,WORK                                                          
         MVC   WORK+6(2),DBFACTOR                                               
         MVC   WORK+8(3),DBFILE                                                 
         MVC   WORK+11(3),DBFILE                                                
         L     RF,DBCOMFCS                                                      
         CLI   DBSELMED,C'R'       IF RADIO,                                    
         JE    GDHKRD               DO MY OWN DEMO CALCULATIONS                 
         L     RF,CDEMOMTH-COMFACSD(RF)                                         
         TM    DBVOPT,X'40'         POSTING USE DOUBLE WORD ARITHMETIC          
         BO    GDHKCN10             BPOO                                        
         GOTO1 (RF),DMCB,=C'MAD',DBAREC,ADUMMY,WORK                             
         J     GDHK28                                                           
GDHKCN10 GOTO1 (RF),DMCB,=C'DMA',DBAREC,ADUMMY,WORK   DOUBLE WORD-BPOO          
         J     GDHK28                                                           
                                                                                
GDHKRD   DS    0H                                                               
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         XC    DTDEMSA,DTDEMSA     USE FIELD TO HOLD DEMO VALUES                
         GOTO1 (RF),DMCB,(C'L',ADEMLST2),DBLOCK,DTDEMSA                         
                                                                                
         BAS   RE,RMAD             DO "MAD" FOR RADIO                           
         XC    DTDEMSA,DTDEMSA     DON'T LEAVE GARBAGE IN HERE                  
         J     GDHK28                                                           
                                                                                
RMAD     NTR1                                                                   
*                                  DEMOMATH'S "MAD" FOR RADIO                   
         LA    RE,DTDEMSA           RE-->DEMO VALUES                            
         L     RF,ARADEMSU          RF-->ACCUMULATORS                           
         L     R3,ADEMLST2          R3-->DEMO LIST FOR RADIO                    
         ZICM  R4,DBFACTOR,(3)                                                  
                                                                                
RMAD10   CLI   0(R3),X'FF'         IF NOT EOL OF DEMOLIST,                      
         JE    RMADX                                                            
         SR    R0,R0                                                            
         ICM   R1,15,0(RE)          TAKE RETURNED DEMO VALUE,                   
         MR    R0,R4                MULT BY DBFACTOR,                           
         ICM   R0,15,0(RF)                                                      
         AR    R0,R1                ADD IT TO RUNNING TOTAL                     
         STCM  R0,15,0(RF)          AND STORE NEW TOTAL                         
         LA    R3,3(R3)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         J     RMAD10                                                           
*                                                                               
RMADX    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
                                                                                
************************************************************                    
* SETMETERED  WEEKLY HOUSEHOLD LOOKUP DEMAND HOOK          *                    
************************************************************                    
GETWKHK  NTR1  BASE=*,LABEL=*                                                   
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
*        MVC   SVBSTART(6),=X'620601620607'                                     
         CLC   VPHSDAT,=X'0000'     WE NEED DATES HERE                          
         JE    EXIT                                                             
         CLC   VPHEDAT,=X'0000'                                                 
         JNE   *+10                                                             
         MVC   VPHEDAT,VPHSDAT                                                  
GETWKHK2 GOTO1 (RF),DMCB,(2,VPHSDAT),(0,DUB)                                    
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         MVC   WORK(12),DMCB+4                                                  
         GOTO1 =V(NSIWEEK),DMCB,DUB,RR=RELO                                     
         MVC   VPHSWK(1),DMCB+4                                                 
         MVC   VPHSWK+1(1),DMCB                                                 
                                                                                
*                                                                               
*-------------------------------------------------------------------            
* WE NEED TO ADJUST THE BOOK TO THE WEEK THE ROTATION STARTS ON                 
* BASED ON  WHAT THE BUY WEEK DEFINITION IS AND THE DAY                         
* THE SPOTDATE IS ON WE CAN DETERMINE HOW TO ADJUST THE                         
* WEEK TO THE BEGINNING OF THE ROTATION.                                        
                                                                                
         L     RF,DBCOMFCS         CORRECTLY                                    
         L     RF,CGETDAY-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DUB,SPOTDAY                                            
         MVC   BISPTDAY,0(R1)                                                   
*                                                                               
         CLI   AFFIDLK,C'N'        MAKE SURE COMING FROM THE RERATE             
         BNE   GETWKH30            DEMAND CALL                                  
*                                                                               
GETWKSUN CLI   ESTEOW,X'07'    SUNDAY TO SAT OOWR?                              
         BNE   GETWKSAT                                                         
         CLI   BISPTDAY,X'06'  SAT SPOT DAY BUMP BACK A WEEK                    
         BE    GETWKH20                                                         
         B     GETWKH30                                                         
                                                                                
GETWKSAT CLI   ESTEOW,X'06'    SAT TO FRI OOWR?                                 
         BE    GETWKH30        EVERYDAY STILL IN THE SAME WEEK                  
                                                                                
GETWKFRI CLI   ESTEOW,X'05'    FRI TO THU OOWR?                                 
         BNE   GETWKTHU                                                         
         CLI   BISPTDAY,X'04'  MON-THU SPOT BUMP BACK A WEEK                    
         BNH   GETWKH20                                                         
         CLI   BISPTDAY,X'06'  SAT-SUN SPOT BUMP BACK A WEEK                    
         BNL   GETWKH20                                                         
         B     GETWKH30                                                         
GETWKTHU CLI   ESTEOW,X'04'    THU TO WED OOWR?                                 
         BNE   GETWKWED                                                         
         CLI   BISPTDAY,X'03'  MON-WED SPOT BUMP BACK A WEEK                    
         BNH   GETWKH20                                                         
         CLI   BISPTDAY,X'06'  SAT-SUN SPOT BUMP BACK A WEEK                    
         BNL   GETWKH20                                                         
         B     GETWKH30                                                         
GETWKWED CLI   ESTEOW,X'03'    WED TO TUE OOWR?                                 
         BNE   GETWKTUE                                                         
         CLI   BISPTDAY,X'02'  MON-TUE SPOT BUMP BACK A WEEK                    
         BNH   GETWKH20                                                         
         CLI   BISPTDAY,X'06'  SAT-SUN SPOT BUMP BACK A WEEK                    
         BNL   GETWKH20                                                         
         B     GETWKH30                                                         
GETWKTUE CLI   ESTEOW,X'02'    TUE TO MON OOWR?                                 
         BNE   GETWKMON                                                         
         CLI   BISPTDAY,X'01'  MON SPOT BUMP BACK A WEEK                        
         BE    GETWKH20                                                         
         CLI   BISPTDAY,X'06'  SAT-SUN SPOT BUMP BACK A WEEK                    
         BNL   GETWKH20                                                         
         B     GETWKH30                                                         
* MON-SUNDAY STANDARD WEEK                                                      
* IF THE SPOT IF SAT OR SUNDAY AND THE ROTATION ASKED FOR HAS M-F               
* THEN BUMP BACK A WEEK                                                         
GETWKMON CLI   ESTEOW,X'01'    MON TO SUNDAY STANDARD BROADCAST WEEK?           
         BE    *+8                                                              
         CLI   ESTEOW,0                                                         
         BNE   GETWKH30                                                         
         CLI   BISPTDAY,X'06'  SAT, SUN SPOT BUMP BACK A WEEK                   
         BNL   GETWKH20                                                         
         B     GETWKH30                                                         
*                                                                               
GETWKH20 ZIC   RE,VPHSWK+1     READJUST BOOK BACK TO THE WEEK OF                
         SHI   RE,1            THE START OF THE ROTATION                        
         STC   RE,VPHSWK+1                                                      
*                                                                               
*  IF THE WEEK NUMBER IS ZERO WE NEED TO REREAD LAST YRS 00 REC                 
         CLI   VPHSWK+1,0      DO WE NEED TO EXIT AND REREAD FOR LAST           
         BNE   GETWKH30        YRS RECORD?                                      
         CLI   LASTYRLK,C'Y'   DID WE ALREADY EXITED AND REREAD LAST            
         BNE   GETWKH25        YEARS 00 REC?                                    
*                                                                               
* WE HAVE ALREADY EXITED AND NOW HAVE LAST YRS 00 REC                           
* FIGURE OUT THE LAST WEEK OF THE YEAR BY SUBTRACTING 7 DAYS                    
* FROM THE SPOT                                                                 
*                                                                               
****     BRAS  RE,ADJVHSDT     GET ADJUSTED DATE BASED ON ROTATION              
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         XC    DUB3,DUB3                                                        
         GOTO1 (RF),DMCB,(3,VPHSDAT),(0,DUB3)                                   
         L     RF,DBCOMFCS                    ADJUST A WEEK BACK                
         L     RF,CADDAY-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,DUB3,DUB3,-7                                           
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         MVC   WORK(12),DMCB+4                                                  
         GOTO1 =V(NSIWEEK),DMCB,DUB3,RR=RELO                                    
         MVC   VPHSWK(1),DMCB+4                                                 
         MVC   VPHSWK+1(1),DMCB                                                 
         B     GETWKH30                                                         
                                                                                
* WE NEED TO SET FLAG AND EXIT TO READ LAST YRS 00 REC                          
GETWKH25 DS    0H                                                               
         J     EXIT                                                             
GETWKH30 DS    0H                                                               
*------------------------------------------------------------------             
                                                                                
         L     RF,WORK+8                                                        
*                                                                               
         GOTO1 (RF),DMCB,(2,VPHEDAT),(0,DUB)                                    
         MVC   DMCB+4(12),WORK                                                  
         GOTO1 =V(NSIWEEK),DMCB,DUB,RR=RELO                                     
* WHATS WRONG WITH THIS CALL??                                                  
* HMM IF VPEDAT = 0000 IT DIES                                                  
*                                                                               
         MVC   VPHEWK(1),DMCB+4                                                 
         MVC   VPHEWK+1(1),DMCB                                                 
         MVC   VPHEWK,VPHSWK       FORCE IT TO DO 1 WEEK                        
*                                                                               
* CHECK FOR IN SWEEP PERIOD                                                     
*                                                                               
         LA    RF,SWPWKTAB                                                      
CHKSWK   CLI   0(RF),X'FF'                                                      
         JE    CHKSWKX                                                          
         CLC   DBSELAGY,=C'FM'     FORCE WTP LOOKUP IN SWEEP                    
         JE    CHKSWKX              FOR MPG                                     
         CLC   SVSELBK(2),0(RF)    FIND THE BASE SWEEP                          
         JE    *+12                                                             
         LA    RF,4(RF)                                                         
         J     CHKSWK                                                           
         CLC   VPHSWK(1),0(RF)     YEAR MUST BE THE SAME                        
         JNE   CHKSWKX                                                          
         CLC   VPHSWK+1(1),2(RF)    BOOK START LT CURR WEEK                     
         JL    CHKSWKX              OK TO USE                                   
         CLC   VPHSWK+1(1),3(RF)    CURR WEEK GT BOOK END                       
         JH    CHKSWKX             OK TO USE                                    
         XC    VPHHOMES,VPHHOMES   IN SWEEP - KILL IT                           
         J     EXIT                                                             
CHKSWKX  DS    0C                                                               
*                                                                               
         L     R1,DBAQUART                                                      
         ZIC   RF,VPHSWK+1                                                      
         ZIC   R0,VPHEWK+1                                                      
         SR    R0,RF               SAVE NUMBER OF WEEKS                         
         JP    *+8                                                              
         LA    R0,0                                                             
         AHI   R0,1                                                             
         ST    R0,VPNWKS                                                        
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         CLI   0(R1),X'51'                                                      
         JE    *+14                                                             
         XC    VPHHOMES,VPHHOMES   NO VALUES- KILL IT                           
         J     EXIT                                                             
*                                                                               
         ZIC   R5,2(R1)            SET FIELD WIDTH                              
         SR    R4,R4                                                            
         IC    R4,VPHMASK-1(R5)    SET UP ICM MASK                              
         STC   R4,MASK51                                                        
         ZIC   RF,1(R1)            SET ELEMENT END                              
         AR    RF,R1                                                            
         ST    RF,END51                                                         
         STH   R5,P6               SAVE FIELD LENGTH                            
         CLI   0(RF),X'53'         SET UP FOR UNIVERSE ELEMENT                  
         JE    *+14                                                             
         XC    VPHHOMES,VPHHOMES   NO VALUES- KILL IT                           
         J     EXIT                                                             
         ST    RF,ELEM53                                                        
         LR    R3,RF                                                            
         ZIC   R5,2(R3)            SET FIELD WIDTH                              
         SR    R4,R4                                                            
         IC    R4,VPHMASK-1(R5)    SET UP ICM MASK                              
         STC   R4,MASK53           SAVE MASK                                    
         STH   R5,P6+2             AND FIELD LENGTH                             
*                                                                               
         ZIC   RF,VPHSWK+1         ADJUST TO START WEEK                         
                                                                                
*                                                                               
          MVI   QHRDARK,C'N'                                                    
                                                                                
*********************************************************                       
* CHECK IF CURRENT QTR DAY PROCESSED IS SAT/SUN                                 
* IF SAT/SUN GET THE FOLLOWING START FROM THE FOLLOWING WEEK                    
* SINCE WE ARE POSTING ON A M-SU SCHEDULE AND THE WEEKLY                        
* METERED DEMO FILES ARE LOADED BASED ON A SAT-SUN SCHEDULE                     
* FOR P- PURCHASE GO THROUGH THIS CODE                                          
* FOR AFFID- WE ARE FINE                                                        
         CLI   AFFIDLK,C'Y'        LOOKING UP AFFID CURRENTLY?                  
         BE    *+8                                                              
         CLI   DSTASPLT,C'Y'       DSTATION SPLIT WEEK CHECK                    
         BE    *+14                                                             
         OC    NOAFDCNT,NOAFDCNT   NO SPOTS IN AFFIDS LOOKUP                    
         BNZ   *+12                SAME AS ACHIEVED LOOKUP                      
         CLI   AFDSW,C'Y'          TEST AFFIDS LOOKED UP                        
         JE    NOROT                                                            
*                                                                               
* OOWR  BUY SCENARIO BASED ON THE START DAY OF THE WEEK                         
* THIS NEEDS TO BE ACCOUNTED FOR BECAUSE THE DATE WE ARE CONVERTING IS          
* ALWAYS THE DATE OF THE START OF THE BUY LINE                                  
* ACHIEVED LOOKUP - CHECK THE START DAY OF VPHSDAT TO SEE WHAT THE              
* START DAY OF THE WEEK COMING IN IS AND ADJUST THE WEEK THE ROTATION           
* SHOULD BE READ FROM ACCORDINGLY.                                              
*                                                                               
         MVI   OOWRFLG,C'N'        NOT OOWR ROTATION LOOKUP                     
         CLI   AFFIDLK,C'N'        CHECK IF FROM ACHIEVED DEMAND CALL           
         BNE   NOROT                                                            
*  SAT-FRI BUY WEEK                                                             
         L     RE,DBAQUART                                                      
         CLI   ESTEOW,X'06'                                                     
         BNE   CHKDLSUN                                                         
         TM    DBSELDAY,X'03'       SAT/SUNDAY?  OR SU/SUN WITH M-F             
         BNZ   NOROT                SAT/SUN SAME WEEK                           
         B     CHKDLY60                                                         
* SUN-SAT BUY WEEK                                                              
CHKDLSUN DS    0C                                                               
         CLI   ESTEOW,X'07'                                                     
         BNE   CHKDLTUE                                                         
         CLI   2(RE),X'70'          SUN READ CURRENT WEEK SUN                   
         BE    NOROT                                                            
         CLI   2(RE),X'60'          SAT READ FROM NEXT WEEK'S SAT               
         BE    CHKDLY50                                                         
         B     CHKDLY60                                                         
* TUE-MON BUY WEEK                                                              
CHKDLTUE DS    0C                                                               
         CLI   ESTEOW,X'02'                                                     
         BNE   CHKDLWED                                                         
         CLI   2(RE),X'10'          MONDAY FROM NEXT WEEK                       
         BE    CHKDLY50                                                         
         CLI   2(RE),X'60'          SAT/SUN FROM NEXT WEEK                      
         BNL   CHKDLY50                                                         
         B     CHKDLY60                                                         
*                                                                               
* WED-TUE BUY WEEK                                                              
CHKDLWED CLI   ESTEOW,X'03'                                                     
         BNE   CHKDLTHU                                                         
         CLI   DBSELDAY,X'40'       MON FROM NEXT WEEK                          
         BE    CHKDLY50                                                         
         CLI   DBSELDAY,X'20'       TUE FROM NEXT WEEK                          
         BE    CHKDLY50                                                         
         CLI   DBSELDAY,X'02'       SAT FROM NEXT WEEK                          
         BE    CHKDLY50                                                         
         CLI   DBSELDAY,X'01'       SUN FROM NEXT WEEK                          
         BE    CHKDLY50                                                         
* ROTATION                                                                      
         CLI   2(RE),X'60'          SA-SU READ NEXT WEEK                        
         BNL   CHKDLY50             READ NEXT WEEK                              
         CLI   2(RE),X'20'          M-TU READ NEXT WEEK                         
         BNH   CHKDLY50             READ NEXT WEEK                              
         B     CHKDLY60                                                         
* THU-WED BUY WEEK                                                              
CHKDLTHU CLI   ESTEOW,X'04'                                                     
         BNE   CHKDLFRI                                                         
         CLI   DBSELDAY,X'40'       MON NEXT WEEK                               
         BE    CHKDLY50                                                         
         CLI   DBSELDAY,X'20'       TUE                                         
         BE    CHKDLY50                                                         
         CLI   DBSELDAY,X'10'       WED                                         
         BE    CHKDLY50                                                         
         CLI   DBSELDAY,X'02'       SAT FROM NEXT WEEK                          
         BE    CHKDLY50                                                         
         CLI   DBSELDAY,X'01'       SUN FROM NEXT WEEK                          
         BE    CHKDLY50                                                         
* ROTATION                                                                      
         CLI   2(RE),X'60'          SA-SUN NEXT WEEK                            
         BNL   CHKDLY50                                                         
         CLI   2(RE),X'30'          ANY ROTATION M-WED                          
         BNH   CHKDLY50             NEXT WEEK                                   
         B     CHKDLY60                                                         
* FRI-THU BUY WEEK                                                              
CHKDLFRI CLI   ESTEOW,X'05'                                                     
         BNE   CHKDLY60                                                         
         CLI   DBSELDAY,X'40'       MON NEXT WEEK                               
         BE    CHKDLY50                                                         
         CLI   DBSELDAY,X'20'       TUE NEXT WEEK                               
         BE    CHKDLY50                                                         
         CLI   DBSELDAY,X'10'       WED NEXT WEEK                               
         BE    CHKDLY50                                                         
         CLI   DBSELDAY,X'08'       THU NEXT WEEK                               
         BE    CHKDLY50                                                         
         CLI   DBSELDAY,X'02'       SAT FROM NEXT WEEK                          
         BE    CHKDLY50                                                         
         CLI   DBSELDAY,X'01'       SUN FROM NEXT WEEK                          
         BE    CHKDLY50                                                         
*                                                                               
* ROTATION                                                                      
         CLI   2(RE),X'60'          SA-SUN NEXT WEEK                            
         BNL   CHKDLY50                                                         
         CLI   2(RE),X'40'          ANY ROTATION M-TH                           
         BNH   CHKDLY50                                                         
         B     CHKDLY60                                                         
*                                                                               
CHKDLY50 DS    0X                                                               
         LA    RF,1(RF)                                                         
         STC   RF,VPHSWK+1                                                      
         MVI   OOWRFLG,C'Y'        OOWR ROTATION LOOKUP                         
         CLI   NEXTYRLK,C'N'       DOING A FORCED NEXT YR LOOKUP FOR            
         BE    NOROT                                                            
CHKDLY60 DS    0X                                                               
*                                                                               
         CLI   NEXTYRLK,C'N'       DOING A FORCED NEXT YR LOOKUP FOR            
         BE    GETVHOM0            END OF YR SPLIT WEEK SITUATION?              
         MVC   VPHSWK,DBSELBK      DBAQUART ALWAYS 60 OR 70 FOR                 
         MVI   VPHSWK+1,0          NEXTYRLK LOOKUPS                             
         LA    RF,1                1ST WEEK OF THE YEAR LOOKUP                  
         B     NOROT                                                            
                                                                                
GETVHOM0 L     RE,DBAQUART         SAT/SUN GRAB 1 WEEK LATER                    
         CLI   2(RE),X'95'         M-F ROTATION ON 00 REC                       
         JE    NOROT                                                            
         CLI   2(RE),X'50'         DONT DECREMENT THE WEEK                      
         JH    *+6                                                              
                                                                                
NOROT    BCTR  RF,0                                                             
*                                                                               
NEXTWK   STC   RF,BYTE             WEEK#-1                                      
         MH    RF,P6                                                            
*                                                                               
* CHECK IF WE ARE DEALING WITH DARKWEEK                                         
         CLC   LPMDRKWK(1),VPHSWK                                               
         BNE   NOTDARKW                                                         
         ZIC   R4,BYTE                                                          
         AHI   R4,1                                                             
         STC   R4,BYTE                                                          
         CLC   BYTE,LPMDRKWK+1    DARK WEEK?                                    
         BNE   NOTDARKW                                                         
         MVI   QHRDARK,C'Y'                                                     
NOTDARKW DS    0C                                                               
*                                                                               
*                                                                               
         LA    RF,3(RF)            GET PAST HEADER INFO                         
         AR    R1,RF               ADDR OF DMA IMP DATA                         
         SR    RF,RF                                                            
         ZIC   RF,VPHSWK+1         ADJUST TO START WEEK                         
*                                                                               
         CLI   AFFIDLK,C'Y'        LOOKING UP AFFID CURRENTLY?                  
         BE    *+8                                                              
         CLI   DSTASPLT,C'Y'                                                    
         BE    *+14                                                             
         OC    NOAFDCNT,NOAFDCNT   NO SPOTS IN AFFIDS LOOKUP                    
         BNZ   *+12                SAME AS ACHIEVED LOOKUP                      
         CLI   AFDSW,C'Y'          TEST AFFIDS LOOKED UP                        
         JE    NOROT2                                                           
*                                                                               
         CLI   NEXTYRLK,C'N'       DOING A FORCED NEXT YR LOOKUP FOR            
         BE    GETVHM0A            END OF YR SPLIT WEEK SITUATION?              
         MVC   VPHSWK,DBSELBK      DBAQUART ALWAYS 60 OR 70 FOR                 
         MVI   VPHSWK+1,0          NEXTYRLK LOOKUPS                             
         LA    RF,1                1ST WEEK OF THE YEAR LOOKUP                  
         B     NOROT2                                                           
*                                                                               
GETVHM0A L     RE,DBAQUART         SAT/SUN GRAB 1 WEEK LATER                    
         CLI   OOWRFLG,C'Y'        OUT OF WEEK ROTATION LOOKUP?                 
         BE    NOROT2                                                           
         CLI   2(RE),X'95'         M-F ROTATION ON 00 REC                       
         JE    NOROT2                                                           
         CLI   2(RE),X'50'         DONT DECREMENT THE WEEK                      
         JH    *+6                                                              
NOROT2   BCTR  RF,0                                                             
NEXTWK2  MH    RF,P6+2                                                          
         LA    RF,3(RF)            GET PAST HEADER INFO                         
         AR    R3,RF               ADDR OF UNIVERSE DATA                        
         SR    R4,R4                                                            
         SR    R7,R7                                                            
GETVHOM1 C     R1,END51            END OF DATA                                  
***      JNL   GETVHOM3                                                         
         JL    GETVHOM2                                                         
*                                                                               
         OC    SVWTPDQH,SVWTPDQH   CHECK TO SEE IF WE READ A PREVIOUS           
         BZ    GETVHM1A            QHR FOR THE SAME DAY SUCESSFULLY             
         CLC   SVWTPDQH(1),2(RE)   IF SO WE DONT HAVE NUMBER BUT DATA           
         BNE   GETVHM1A            IS IN.                                       
         B     GETVHM1B                                                         
*                                                                               
*                                                                               
GETVHM1A MVI   WKINFLAG,C'N'       WEEK IS NOT IN YET                           
*                                                                               
* SET UP MISSING DATA COUNT TO GET COUNT # OF MISSING DATA                      
* FOR A CURRENT DAY SO IF WE COULD GET DATA                                     
* FOR THAT DAY EVENTUALLY THEN WE WANT TO ADD THE COUNT TO WEIGHT               
* CORRECTLY AS ZERO VALUES                                                      
         CLI   WKINFLAG,C'N'                                                    
         BNE   GETVHM1B                                                         
         CLI   MISCOUNT,0                                                       
         BE    *+10                IF DAY IS DIFFERENT THAN SAVED               
         CLC   MISSDAY,2(RE)       IN MISSDAY THEN CLEAR COUNT                  
         BE    *+12                                                             
         MVI   MISCOUNT,0                                                       
         MVI   MISSDAY,0                                                        
*                                                                               
         MVC   MISSDAY,2(RE)       SAVE DAY OF MISSING DATA                     
         ZIC   RE,MISCOUNT         MISSING DATA COUNT                           
         AHI   RE,1                                                             
         STC   RE,MISCOUNT                                                      
*                                                                               
GETVHM1B J     GETVHM2B            NEED TO FALL THROUGH                         
GETVHOM2 SR    RE,RE                                                            
         SR    RF,RF                                                            
         SR    R5,R5                                                            
         MVI   WKINFLAG,C'Y'       WEEK IS IN                                   
         IC    R4,MASK51                                                        
         EX    R4,*+8                                                           
         J     *+8                                                              
         ICM   RF,0,0(R1)          WEEKLY VALUE                                 
***      BZ    GETVHOM3            END OF DATA                                  
         BZ    GETVHM2B            END OF DATA                                  
*                                                                               
         IC    R4,MASK53                                                        
         EX    R4,*+8                                                           
         J     *+8                                                              
         ICM   R5,0,0(R3)          WEEKLY UNIVERSE                              
         JNZ   GETVHM2A                                                         
*                                                                               
         L     RE,VPNWKS           SUBTRACT FROM FROM WEEK COUNT                
         BCTR  RE,0                TO IGNORE WEEKS WITH 0 UNIV                  
         ST    RE,VPNWKS                                                        
         J     GETVHM2B                                                         
*                                                                               
* CALCULATE THE RATING                                                          
*                                                                               
GETVHM2A M     RE,=F'2000'         XX.X                                         
         DR    RE,R5               IMP/UNIV                                     
         AHI   RF,1                                                             
         SRL   RF,1                                                             
         CHI   RF,1000                                                          
         JNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         AR    R7,RF                                                            
         AH    R1,P6                                                            
GETVHM2B AH    R3,P6+2                                                          
         BCT   R0,GETVHOM1                                                      
*                                                                               
GETVHOM3 STC   R0,VPWKSTPT         SAVE BASE WEEKS NEEDED                       
         LR    RF,R7                                                            
         L     RE,VPNWKS                                                        
         SR    RE,R0                                                            
         ST    RE,VPNWKS           ADJUST WEEKS FROM YEAR RECORD                
         LTR   RE,RE                                                            
         JZ    EXIT                                                             
*                                                                               
         M     RE,=F'2'                                                         
         D     RE,VPNWKS           ADJUST FOR THE WEEKS                         
         AHI   RF,1                                                             
         SRA   RF,1                                                             
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),DBFACTOR  AND THE FACTOR                               
         L     R1,FULL                                                          
*                                                                               
* SAVE OFF LAST DAY QHR HOUR OF WTP RECORD READ SO WE CAN CONTROL               
* WHETHER WE WANT TO USE THE SAME END DAY/QH FOR THE MONTHLY FILE               
* READ TO INDEX THE DEMOS                                                       
         CLI   WKINFLAG,C'N'      IF CURRENT QEEK IS NOT IN YET                 
         BE    *+8                OR                                            
         CLI   QHRDARK,C'Y'       IF CURRENT QTR HOUR IS DARK WEWK              
         BE    GETVHEXC           DONT WANT TO SAVE -EXCLUDE QHR                
         L     RE,DBAQUART                                                      
         MVC   SVWTPDQH,2(RE)     SAVE DAY AND QH                               
* SAVE OFF BITS TO INDICATE THE WEEKS READ                                      
         CLI   SVWTPDQH,X'95'      IF M-F RECORD                                
         BNE   *+8                                                              
         OI    DAYREAD,B'01111100'                                              
         CLI   SVWTPDQH,X'70'     SAVE OFF BITS OF DAYS READ                    
         BH    GETVHEXC                                                         
         CLI   SVWTPDQH,X'10'                                                   
         BL    GETVHEXC                                                         
         ZIC   RE,SVWTPDQH                                                      
         SRL   RE,4                                                             
         LA    R0,X'80'                                                         
         SRL   R0,1                                                             
         BCT   RE,*-4                                                           
         ZIC   RE,DAYREAD                                                       
         OR    R0,RE                                                            
         STC   R0,DAYREAD                                                       
GETVHEXC DS    0C                                                               
*                                                                               
* COMPARE SVWTPDQH TO DBAQUART                                                  
* IF ITS NOT EQUAL THAT MEANS WE ARE NOT INCLUDING THIS QTR HOUR IN             
* THE FACTOR,                                                                   
*                                                                               
         L     R5,DBAQUART                                                      
         CLC   SVWTPDQH,2(R5)                                                   
         BNE   GETVHOM5                                                         
         A     R1,VPHFACT                                                       
         ST    R1,VPHFACT                                                       
*                                                                               
         CLC   MISSDAY,2(R5)                                                    
         BNE   GETVHOM4                                                         
         ZIC   R1,MISCOUNT         GET ALL # OF ZERO DATA COUNT                 
         A     R1,VPHFACT                                                       
         ST    R1,VPHFACT                                                       
GETVHOM4 MVI   MISCOUNT,0          CLEAR MISSING DATA COUNT AND DAY             
         MVI   MISSDAY,0                                                        
*                                                                               
GETVHOM5 DS    0C                                                               
*                                                                               
         M     RE,FULL                                                          
         LTR   RF,RF                                                            
         JNP   EXIT                                                             
         A     RF,VPHHOMES                                                      
         ST    RF,VPHHOMES                                                      
         J      EXIT                                                            
VPHMASK  DC    AL1(1,3,7,15)                                                    
************************************************************                    
* NOTE CHECKING FOR SWEEP PERIODS AFTER DEC/2002 IS NOT DONE                    
* THAT'S WHY WE DON'T HAVE DATES AFTER 2002 IN THE TABLE                        
************************************************************                    
* SWEEP BOOK/START WEEK/END WEEK                                                
SWPWKTAB DS    0C                                                               
*&&DO                                                                           
         DC    AL1(98,01,03,06)                                                 
         DC    AL1(98,02,07,10)                                                 
         DC    AL1(98,03,11,14)                                                 
         DC    AL1(98,05,18,21)                                                 
         DC    AL1(98,07,29,32)                                                 
         DC    AL1(98,10,40,43)                                                 
         DC    AL1(98,11,45,48)                                                 
         DC    AL1(99,01,02,05)                                                 
         DC    AL1(99,02,06,09)                                                 
         DC    AL1(99,03,10,13)                                                 
         DC    AL1(99,05,18,21)                                                 
         DC    AL1(99,07,28,31)                                                 
         DC    AL1(99,10,40,43)                                                 
         DC    AL1(99,11,45,48)                                                 
         DC    AL1(100,01,02,05)                                                
         DC    AL1(100,02,06,09)                                                
         DC    AL1(100,03,10,13)                                                
         DC    AL1(100,05,18,21)                                                
         DC    AL1(100,07,28,31)                                                
         DC    AL1(100,10,40,43)                                                
         DC    AL1(100,11,45,48)                                                
         DC    AL1(101,01,02,05)                                                
         DC    AL1(101,02,06,09)                                                
         DC    AL1(101,03,10,13)                                                
         DC    AL1(101,05,18,21)                                                
         DC    AL1(101,07,28,31)                                                
         DC    AL1(101,10,40,43)                                                
         DC    AL1(101,11,45,48)                                                
         DC    AL1(102,01,02,05)                                                
         DC    AL1(102,02,06,09)                                                
         DC    AL1(102,03,10,13)                                                
         DC    AL1(102,05,18,21)                                                
         DC    AL1(102,07,28,31)                                                
         DC    AL1(102,10,40,43)                                                
         DC    AL1(102,11,45,48)                                                
*&&                                                                             
         DC    X'FFFF'                                                          
         LTORG                                                                  
         DROP  RB                                                               
************************************************************                    
* OVERNIGHTS DEMAND HOOK                                   *                    
************************************************************                    
GETOVHK  NTR1   BASE=*,LABEL=*                                                  
         MVI   VPWKSTPT,0           SET TO 0 - NOT WEEKLY                       
         CLI   DEMANDSW,0          TEST HERE PREVIOUSLY                         
         BNE   *+8                 YES                                          
         MVI   DEMANDSW,C'Y'       SET SWITCH THAT WE WERE HERE                 
*                                                                               
         LA    R4,ENDNAMEO                                                      
         GOTO1 VDEFINE,P1,=C'PROG+',DBLOCK,(R4)                                 
         OC    STNAMEOV,STNAMEOV                                                
         BNZ   *+10                                                             
         MVC   STNAMEOV,ENDNAMEO                                                
*                                                                               
* LOOK UP HOMES TO DERIVE INDEX *                                               
                                                                                
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         GOTO1 (RF),P1,(C'L',HOMDEM),DBLOCK,QHVUTS                              
*                                                                               
* GET UNIVERSE INTO 100'S TO MATCH IUN PRECSION                                 
*                                                                               
         SR    RE,RE                                                            
         L     RF,QHVUTS+4                                                      
         AHI   RF,50                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         ST    RF,OVHOMUNV                                                      
*                                                                               
         SR    RE,RE                                                            
         L     RF,QHVUTS                                                        
*                                                                               
         MH    RF,DBFACTOR                                                      
         A     RF,VPHHOMES                                                      
         ST    RF,VPHHOMES                                                      
         MVC   OVPHHMES,VPHHOMES                                                
         LH    RF,DBFACTOR                                                      
         A     RF,VPHFACT                                                       
         ST    RF,VPHFACT                                                       
*                                                                               
* SAVE OFF DAYS READ SO WE CAN USE THE SAME DAYS TO READ THE MONTHLY            
* SO WE DONT HAVE A MISMATCH ON THE WEIGHTING                                   
*                                                                               
         L     RF,DBAQUART                                                      
         CLI   2(RF),X'95'      IF M-F RECORD                                   
         BNE   *+8                                                              
         OI    DAYREAD,B'01111100'                                              
*                                                                               
         CLI   2(RF),X'70'      SAVE OFF BITS OF DAYS READ                      
         BH    GETOVHKX                                                         
         CLI   2(RF),X'10'                                                      
         BL    GETOVHKX                                                         
         ZIC   RE,2(RF)                                                         
         SRL   RE,4                                                             
         LA    R0,X'80'                                                         
         SRL   R0,1                                                             
         BCT   RE,*-4                                                           
         ZIC   RE,DAYREAD                                                       
         OR    R0,RE                                                            
         STC   R0,DAYREAD                                                       
*                                                                               
GETOVHKX J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
**************************************************************                  
* ROUTINE TO SET THE MARKET TYPE                                                
* CALL THIS ROUTINE ONLY TO DETERMINE WHAT TYPE OF NON LPM                      
* MKT WE ARE DEALING WITH.                                                      
* EXIT- MKTTYPE SET TO TYPE OF MARKET  (SET METERED, DIARY)                     
*                                                                               
**************************************************************                  
SETMKTYP NTR1  BASE=*,LABEL=*                                                   
         L     R4,=A(GETMKTCS)                                                  
         A     R4,RELO                                                          
         USING GETMKTCS,R4                                                      
         MVC   SVMED,DBSELMED      SAVE AWAY                                    
         MVC   SVDBFUNC,DBFUNCT                                                 
         MVC   SVDBTYPE,DBBTYPE                                                 
         MVC   SVSELAGY,DBSELAGY                                                
         XC    STABKMKT,STABKMKT                                                
         MVC   SVSELSTA,DBSELSTA                                                
         MVC   DBSELAGY,=C'SJ'       SJR ACCESS FOR TO GET MKT INFO             
         MVC   HALF,DBSELBK                                                     
         MVC   DSTACALL,DBSELSTA                                                
***      MVI   DBSELMED,C'W'                                                    
         MVI   DBSELMED,C'O'                                                    
         CLI   DBBTYPE,C'P'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,0                                                        
*                                                                               
         CLI   DBBTYPE,C'I'                                                     
         BNE   *+8                                                              
         MVI   DBBTYPE,C'H'                                                     
*                                                                               
* SEE IF  WE ARE LOOKING UP LIVE O LY BOOKTYPE                                  
* IF SO, LOOK UP THE KEYS FOR THE  LIVE PLUS INSTEAD BECAUSE                    
* WE DON T ALWAYS GET LIVE ONLY                                                 
*                                                                               
SETMKT16 L     RF,DBCOMFCS                                                      
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,LIVEBKTY                                               
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)                                                         
         USING LVBKTYD,RE                                                       
SETMKT18 CLC   =X'FFFF',0(RE)                                                   
         BE    SETMKT21                                                         
         CLC   DBBTYPE,LVONLYBT                                                 
         BE    SETMKT20                                                         
         AR    RE,RF                                                            
         B     SETMKT18                                                         
SETMKT20 MVC   DBBTYPE,LVPLUSBT                                                 
*                                                                               
         MVI   MKTTYPE,MKTDIARY    DEFAULT AS DIARY                             
SETMKT21 MVI   DBFUNCT,DBVLSTBK                                                 
*   CALL DEMAND TO VALIDATE STATION/BOOK                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0                                               
         MVC   DBFUNCT,SVDBFUNC                                                 
         MVC   DBBTYPE,SVDBTYPE                                                 
         MVC   DBSELSTA,SVSELSTA                                                
*                                                                               
         CLI   DBSELMED,C'T'                                                    
         BNE   SETMKT22                                                         
         MVC   DBSELBK,HALF                                                     
         MVI   DBSELMED,C'W'                                                    
         CLI   DBERROR,0           TEST FOR ERRORS                              
         JE    SETMKT23            IF ERROR JUST EXIT                           
**       J     SETMKTYX            IF ERROR JUST EXIT                           
* EVEN THOUGH WE CANT FIND THE PASSIVE KEYS TO VALIDATE THIS STATION            
* IN DEMO DIRECTORY THIS MIGHT BE A FAKE LOCAL CABLE CALL LETTER                
* THAT IS NOT ON THE DIRECTORY.  SEARCH DEMTABS TABLE BASED ON                  
* THE SVMKALPH IF IT IS SET                                                     
*                                                                               
         CLC   SVMKALPH,=X'404040' TRY USING THE ALPHA MARKET SAVED             
         BNH   SETMKTYX            IF NO SVMKALPH THEN EXIT                     
         B     SETMKT25                                                         
*                                                                               
SETMKT22 CLI   DBERROR,0                                                        
         BE    SETMKT23                                                         
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELBK,SVSELBK                                                  
******   J     SETMKT21                                                         
         J     SETMKT16                                                         
*                                                                               
SETMKT23 MVC   DSTACALL,DBKEY+(BSSTAT-BSKEY)    STATION READ                    
         MVC   STABKMKT,DBKEY+(BSRMKT-BSKEY)   MKT NUMBER                       
         CLC   DSTACALL,DBSELSTA      SET DBSELSTA TO                           
         BE    *+16                   THE LINKED STATION FOR THIS BOOK          
         MVC   DBSELSTA,DSTACALL                                                
         MVC   DSTACALL,SVSELSTA                                                
*                                                                               
* IF CALL LETTERS ARE THE SAME DSTACALL=DBSELSTA                                
* THEN TRY TO READ FOR THE WEEK AFTER (SPLIT WEEK)                              
* SAT/SUN IS A WEEK AHEAD ON THE DEMOS FILE                                     
* IN ORDER TO SEE IF THERE IS A DSTATION LINK SET UP                            
*                                                                               
         CLC   DSTACALL,DBSELSTA                                                
         BNE   SETMKT25                                                         
         TM    DBSELDAY,B'01111100' IF ITS A ROTATION OF ATLEAST 1              
         BZ    SETMKT25             DAY BEFORE SATURDAY AND                     
         TM    DBSELDAY,B'00000011' 1 DAY SAT OR SUNDAY                         
         BZ    SETMKT25                                                         
         MVC   HALF,DBSELBK                     SAVE BOOK                       
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,VPHSDAT),(0,DUB3)                                   
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,DUB3,DUB3,7                                            
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         GOTO1 =V(NSIWEEK),DMCB,DUB3,RR=RELO                                    
         MVC   DBSELBK(1),DMCB+4                                                
         MVC   DBSELBK+1(1),DMCB                                                
***                                                                             
         MVI   DBFUNCT,DBVLSTBK                                                 
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0                                               
         MVC   DBBTYPE,SVDBTYPE                                                 
         MVC   DBSELSTA,SVSELSTA                                                
         MVC   DBFUNCT,SVDBFUNC                                                 
         MVC   DBSELBK,HALF                     RESTORE BOOK                    
         CLI   DBERROR,0           TEST FOR ERRORS                              
         JNE   SETMKT25                                                         
         CLC   DSTACALL,DBKEY+(BSSTAT-BSKEY)                                    
         BE    *+10                                                             
         MVC   DSTACALL,DBKEY+(BSSTAT-BSKEY)    STATION READ                    
*                                                                               
* GET SET METER MKT TABLE FROM DEDEMTABS                                        
SETMKT25 L     RF,DBCOMFCS                                                      
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,SETMETER                                               
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DBSELMED,SVMED      RESTORE TO WHATEVER IT WAS                   
*                                                                               
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         LR    RF,RE               SAVE A(FIRST ENTRY)                          
         USING SETMETRD,RE                                                      
SETMKT26 CLI   0(RE),X'FF'              IN TABLE                                
         BE    SETMKTYX                 IF NOT IN LPM TABLE EXIT                
         CLC   SETMKNUM,STABKMKT                                                
         BE    SETMKT28                                                         
         CLC   SETMKALF,SVMKALPH                                                
         BE    SETMKT28                                                         
         AR    RE,R0                                                            
         B     SETMKT26                                                         
*                                                                               
SETMKT28 MVI   MKTTYPE,MKTSETM      FOUND MKT IN SET METER TAB                  
*                                                                               
SETMKTYX XC    DBKEY,DBKEY                                                      
         MVI   DBERROR,0                                                        
         MVC   DBSELMED,SVMED      RESTORE TO WHATEVER IT WAS                   
         MVC   DBSELAGY,SVSELAGY    RESTORE AGENCY CODE                         
         DROP  RE                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,R4                                                            
**********************************************************                      
* SUBROUTINE TO GET VPHSWK LOGIC TO DETERMINE WEEKLY BOOK*                      
**********************************************************                      
GETOVSWK NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
*        MVC   SVBSTART(6),=X'620601620607'                                     
         CLI   AFFIDLK,C'Y'                                                     
         BE    GETOVW05                                                         
         CLI   ESTEOW,X'01'          DONT TAKE ADJDATE DATE FOR OOWR            
         BH    GETOVW05              ADJDATE SHOULD BE FOR M-SU WK              
*                                                                               
         OC    ADJDATE,ADJDATE                                                  
         BNZ   *+10                                                             
         MVC   ADJDATE,VPHSDAT                                                  
         GOTO1 (RF),DMCB,(3,ADJDATE),(0,DUB)                                    
         B     GETOVW10                                                         
GETOVW05 GOTO1 (RF),DMCB,(2,VPHSDAT),(0,DUB)                                    
*                                                                               
GETOVW10 DS    0C                                                               
*                                                                               
* ADJUST BOOK BASED ON BUY WEEK DEFINITION AND DAY OF THE SPOTDATE              
* IS ON.                                                                        
* ----------------------------                                                  
GETOVW62 DS    0H                                                               
         CLI   AFFIDLK,C'N'       MAKE SURE COMING FROM THE ACHIEVED            
         BNE   GETOVW80            DEMAND CALL                                  
         BRAS  RE,ADJVHSDT        GET SPOTDAY (HOLDS DAY OF AFFID SPOT)         
*  SUNDAY - SAT WEEK?                                                           
         CLI   ESTEOW,X'07'        SUN-SAT ESTIMATE OOW INDICATOR               
         BNE   GETOVE06                                                         
         MVI   DBBEST,X'16'        INDICATOR TO GETTP SUN-SAT WEEK              
         CLI   BISPTDAY,X'06'      IF SPOT IS M-SA THEN BUMP BACK A WK          
         BNH   GETOVSUB            DBBEST=X'60' TELLS GETTP HOW TO              
         B     GETOVW80            SUNDAY AFFID SPOT KEEP SAME WEEK             
* SAT TO FRIDAY OOWR WEEK                                                       
GETOVE06 DS    0C                                                               
         CLI   ESTEOW,X'06'                                                     
         BNE   GETOVE05                                                         
         MVI   DBBEST,X'15'                                                     
         CLI   BISPTDAY,5          IF SPOT IS M-F THEN BUMP BACK A WEEK         
         BNH   GETOVSUB                                                         
         B     GETOVW80                                                         
* FRI TO THURS OOWR WEEK                                                        
GETOVE05 DS    0C                                                               
         CLI   ESTEOW,X'05'        FRI-THU ESTIMATE OOW INDICATOR               
         BNE   GETOVE04                                                         
         MVI   DBBEST,X'14'        INDICATOR TO GETTP FRI-THU WEEK              
         CLI   BISPTDAY,X'04'      IF SPOT IS BETWEEN MON-THU THEN              
         BNH   GETOVSUB            BUMP THE BOOK BACK 1 WEEK                    
         B     GETOVW80            ELSE FRI-SUN SPOT KEEP SAME WEEK             
* THU TO WED OOWR WEEK                                                          
GETOVE04 DS    0C                                                               
         CLI   ESTEOW,X'04'        THU-WED ESTIMATE OOW INDICATOR               
         BNE   GETOVE03                                                         
         MVI   DBBEST,X'13'        INDICATOR TO GETTP THU-WED WEEK              
         CLI   BISPTDAY,X'03'      IF SPOT IS BETWEEN MON-WED THEN              
         BNH   GETOVSUB            BUMP THE BOOK BACK 1 WEEK                    
         B     GETOVW80            ELSE THU-SUN SPOT KEEP SAME WEEK             
* WED TO TUES OOWR WEEK                                                         
GETOVE03 DS    0C                                                               
         CLI   ESTEOW,X'03'        WED-TUE ESTIMATE OOW INDICATOR               
         BNE   GETOVE02                                                         
         MVI   DBBEST,X'12'        INDICATOR TO GETTP WED-TUE WEEK              
         CLI   BISPTDAY,X'02'      IF SPOT IS BETWEEN MON-TUE THEN              
         BNH   GETOVSUB            BUMP THE BOOK BACK 1 WEEK                    
         B     GETOVW80            ELSE WED-SUN SPOT KEEP SAME WEEK             
* TUE TO MONDAY OOWR WEEK                                                       
GETOVE02 DS    0C                                                               
         CLI   ESTEOW,X'02'        TUE-MON ESTIMATE OOW INDICATOR               
         BNE   GETOVE01                                                         
         MVI   DBBEST,X'10'        INDICATOR TO GETTP TUE-MON WEEK              
         CLI   BISPTDAY,X'01'      IF SPOT IS MONDAY THEN                       
         BE    GETOVSUB            BUMP THE BOOK BACK 1 WEEK                    
         B     GETOVW80            ELSE TUE-SUN SPOT KEEP SAME WEEK             
* MON TO FRI STANDARD WEEK                                                      
GETOVE01 DS    0C                                                               
         CLI   ESTEOW,X'00'        MON-FRI STANDARD WEEK                        
         BE    *+8                                                              
         CLI   ESTEOW,X'01'        TUE-MON ESTIMATE OOW INDICATOR               
         BNE   GETOVW80                                                         
         MVI   DBBEST,0                                                         
         B     GETOVW80                                                         
*                                                                               
GETOVSUB DS    0X                                                               
         L     RF,DBCOMFCS                    ADJUST A WEEK BACK                
         L     RF,CADDAY-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,DUB,DUB,-7                                             
         B     GETOVW80                                                         
*                                                                               
GETOVADD DS    0H                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         GOTO1 (RF),DMCB,DUB,DUB,7                                              
         B     GETOVW80                                                         
GETOVW80 DS    0H                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         ST    RF,DMCB+12                                                       
         L     RF,DBCOMFCS                                                      
         L     RF,CADDAY-COMFACSD(RF)                                           
         ST    RF,DMCB+8                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         ST    RF,DMCB+4                                                        
         MVC   WORK(12),DMCB+4                                                  
         CLI   DBSELMED,C'O'         STARTDAY=MONDAY                            
         BNE   *+8                                                              
         MVI   DMCB+4,1                                                         
         GOTO1 =V(NSIWEEK),DMCB,DUB,RR=RELO                                     
         MVC   VPHSWK(1),DMCB+4                                                 
         MVC   VPHSWK+1(1),DMCB                                                 
         L     RF,WORK+8                                                        
* I ADDED THIS CODE BECAUSE IF VPHEDAT IS NOT SET WE WILL DIE ANYWAYS           
* IN NSIWEEK SO IT DOESNT HURT TO DO THIS                                       
         OC    VPHEDAT,VPHEDAT                                                  
         BNZ   *+10                                                             
         MVC   VPHEDAT,VPHSDAT                                                  
*                                                                               
         GOTO1 (RF),DMCB,(2,VPHEDAT),(0,DUB)                                    
         MVC   DMCB+4(12),WORK                                                  
         GOTO1 =V(NSIWEEK),DMCB,DUB,RR=RELO                                     
         MVC   VPHEWK(1),DMCB+4                                                 
         MVC   VPHEWK+1(1),DMCB                                                 
         MVC   VPHEWK,VPHSWK       FORCE IT TO DO 1 WEEK                        
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
******************************************************                          
* SUBROUTINE TO INSERT AFFIDAVIT DAY TIME IN AFDLIST *                          
******************************************************                          
AFDBLD   NTR1  BASE=*,LABEL=*                                                   
                                                                                
* GET YYMMDD VIA DATCON *                                                       
                                                                                
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,2(R6)),WORK                                         
                                                                                
* GET DAY VIA GETDAY *                                                          
                                                                                
         L     RF,DBCOMFCS                                                      
         L     RF,CGETDAY-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,WORK,WORK+6                                            
*                                                                               
         ICM   R4,15,AFDNEXT       GET NEXT LIST POSITION                       
         BNZ   *+8                                                              
         L     R4,AAFDLST          ELSE POINT TO START                          
* FOR NTI WEEKLY LOOKUPS, MAY NOT HAVE AFFID                                    
         CLI   0(R6),X'10'         TEST AFFID ELEMENT                           
         BE    AFDBLD1             YES                                          
         MVC   0(1,R4),BDDAY       MOVE DAY BITS                                
         MVC   3(2,R4),2(R6)       MOVE DATE                                    
         MVC   1(2,R4),=X'FFFF'    SET FLAG TO USE BDTIMST                      
         B     AFDBLD20                                                         
*                                                                               
AFDBLD1  ZIC   R5,0(R1)            GET DAY                                      
         IC    R5,DAYTBL-1(R5)                                                  
         STC   R5,0(R4)                                                         
*                                                                               
         MVC   1(2,R4),4(R6)       MOVE TIME TO LIST                            
*                                                                               
         TM    AFDOPTS,X'80'       TEST 7 MINUTE OPTION IN PLAY                 
         BZ    AFDBLD2             NO                                           
         CLC   1(2,R4),AF7QHST     COMPARE TO LAST QH START                     
         BL    AFDBLD2             PRIOR - IGNORE                               
         CLC   1(2,R4),AF7QHEND    COMPARE TO LAST QH END                       
         BH    AFDBLD2                                                          
         MVC   1(2,R4),AF7QHSET    MOVE 'SET' TIME VALUE                        
*                                                                               
AFDBLD2  NI    1(R4),X'0F'         DROP HOB                                     
         CLC   DBSELBK,=X'5002'    NO M-F FOR BOOK NOT FOUND                    
         BE    AFDBLD10                                                         
         CLC   DBSELBK,=X'6509'    NO M-F SET FOR OCT/01 AND SUBS               
         BH    AFDBLD10                                                         
         CLI   0(R1),5             TEST ACTUAL DAY MON(1) TO FRI(5)             
         BH    AFDBLD10            NO                                           
         CLC   DBSELBK,=X'6509'                                                 
         BH    AFDBLD10                                                         
         CLC   1(2,R4),=H'0600'    TEST AFTER 6AM                               
         BL    AFDBLD10            NO                                           
         MVC   DMCB(2),=H'1557'    DEFAULT ARB MAR/92 & SUBS-4PM                
         CLI   DBSELSRC,C'N'                                                    
         BNE   *+10                                                             
         MVC   DMCB(2),=H'1527'    SET FOR NSI MAR/92 & SUBS-330PM              
         CLI   DBSELMED,C'T'       MAKE SURE IT'S USTV                          
         BNE   AFDBLD3                                                          
         CLC   DBSELBK,=X'5C02'                                                 
         BH    *+10                                                             
AFDBLD3  MVC   DMCB(2),=H'1657'    SET FOR PRIOR MAR/92------5PM                
         CLC   1(2,R4),DMCB        TEST PRIOR TO 1 DAY BREAK TIME               
         BH    AFDBLD10            NO                                           
         MVI   0(R4),X'95'         FORCE DAY = MO-FR (HIGH SORT)                
*                                                                               
AFDBLD10 MVC   3(2,R4),2(R6)       MOVE DATE TO LIST ENTRY                      
*                                                                               
AFDBLD20 LA    R4,L'AFDLIST(R4)                                                 
         ST    R4,AFDNEXT                                                       
         L     R0,AAFDLSTX                                                      
         CR    R4,R0                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
DAYTBL   DC    X'40201008040201'   DAY TABLE FOR DEMAND CALLS                   
*****************************************************************               
*           SUBROUTINE TO UNWEIGHT ACCUMS AT 0(R1)              *               
*                       BY VALUE IN R0                          *               
*****************************************************************               
                                                                                
GDUNWGT  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,MAXDEMS*2        ADJ/UNADJ ACCUMS                             
*                                                                               
GDUNWGT2 SR    RE,RE                                                            
         ICM   RF,15,0(R1)                                                      
         BNP   GDUNWGT3                                                         
         SLDA  RE,1                X 2                                          
         LTR   R0,R0                                                            
         BNZ   *+10                                                             
GDUNWGT3 SR    RF,RF                                                            
         B     *+6                                                              
         DR    RE,R0                                                            
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R4,GDUNWGT2                                                      
         J     EXIT                                                             
         LTORG                                                                  
                                                                                
         EJECT                                                                  
*****************************************************************               
* SUBROUTINE TO EXTRACT VALUES FROM SVI RECORD                  *               
* MONTH WEIGHTED FACTORS FOR EACH DEMO ARE RETURNED IN WORK     *               
*****************************************************************               
                                                                                
GETSVI   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R0,100                                                           
         ST    R0,WORK                                                          
         MVC   WORK+4(76),WORK     FORCE ALL VALUES = 100                       
*                                                                               
         LA    R1,DEMOLIST         R1 = CURRENT DEMOLIST ENTRY                  
         LA    R4,SVITYPES         R4 = CORRESPONDING SVI TYPE ENTRY            
         LA    R2,WORK                                                          
*                                                                               
GETSVI2  DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    GETSVI10                                                         
         MVI   ELCODE,2                                                         
         LA    R6,SVISDEL                                                       
         BRAS  RE,NEXTEL2                                                       
         B     *+8                                                              
*                                                                               
GETSVI3  BRAS  RE,NEXTEL                                                        
         BNE   GETSVI3X                                                         
         CLC   0(1,R4),2(R6)       MATCH SVI TYPE                               
         BE    GETSVI4                                                          
         B     GETSVI3                                                          
                                                                                
* TYPE NOT FOUND - POINT TO DUMMY SVI ELEM *                                    
                                                                                
GETSVI3X LA    R6,=12AL1(100)                                                   
*        SH    R6,=H'3'                                                         
         SHI   R6,3                                                             
         B     GETSVI5                                                          
                                                                                
* SET BAD SVI FACTORS TO 100 *                                                  
                                                                                
GETSVI4  LA    R0,12                                                            
         LA    RE,3(R6)                                                         
GETSVI4A CLI   0(RE),0                                                          
         BNE   *+8                                                              
         MVI   0(RE),100                                                        
         LA    RE,1(RE)                                                         
         BCT   R0,GETSVI4A                                                      
         EJECT                                                                  
GETSVI5  ZIC   RE,SVACTBK+1        GET 'FROM' MONTH                             
         BCTR  RE,0                                                             
         IC    RE,3(RE,R6)         GET FACTOR                                   
         ST    RE,DUB              SAVE IN DUB                                  
*                                                                               
         LA    R0,12                                                            
         LA    R6,3(R6)            POINT TO MONTHLY SVIS                        
         LA    R5,SVIWGTS          POINT TO MONTHLY WEIGHTS                     
         SR    R7,R7               CLEAR TOTAL                                  
*                                                                               
GETSVI6  ZIC   RF,0(R6)            GET 'TO' MONTH FACTOR                        
         LA    RE,200              X 100 X 2                                    
         MR    RE,RE                                                            
         D     RE,DUB              DIVIDE BY 'FROM' MONTH FACTOR                
         LA    RF,1(RF)                                                         
         SRL   RF,1                ROUND                                        
                                                                                
* TEST FOR BAD SVI VALUES *                                                     
                                                                                
         CH    RF,=H'25'           LESS THAN 26 IS BAD                          
         BH    *+8                                                              
         LA    RF,100                                                           
         CH    RF,=H'200'          MORE THAN 199 IS BAD                         
         BL    *+8                                                              
         LA    RF,100                                                           
         OC    VPHHOMES,VPHHOMES   NO SVIS IF WTP                               
         BZ    *+8                                                              
         LA    RF,100                                                           
*                                                                               
         MH    RF,0(R5)            X MONTHLY WEIGHT                             
         AR    R7,RF               ADD TO TOTAL                                 
         LA    R6,1(R6)                                                         
         LA    R5,2(R5)                                                         
         BCT   R0,GETSVI6                                                       
                                                                                
         SR    R6,R6                                                            
         AR    R7,R7               X 2                                          
         D     R6,SVIWGTSM                                                      
         LA    R7,1(R7)                                                         
         SRL   R7,1                                                             
         ST    R7,0(R2)                                                         
*                                                                               
GETSVI10 LA    R2,4(R2)                                                         
         LA    R4,1(R4)                                                         
         LA    R1,3(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   GETSVI2                                                          
         J     EXIT                                                             
*                                                                               
         DROP  R8                  UP TO HERE USED FOR SVIREC                   
         EJECT                                                                  
*****************************************************************               
* SUBROUTINE TO CALCULATE ACTUAL SVI VALUES FOR ACCUMS AT 0(R1) *               
*             VALUES ARE RETURNED IN SVIS                       *               
*****************************************************************               
                                                                                
GDSVI    NTR1  BASE=*,LABEL=*                                                   
         L     R4,NUMDEMS                                                       
         LA    R5,SVIS                                                          
         XC    SVIS,SVIS                                                        
*                                                                               
         LA    R0,100                                                           
GDSVI2   ST    R0,0(R5)                                                         
*&&DO                                                                           
GDSVI2   ICM   RE,15,MAXDEMS*4(R1)                                              
         BNZ   *+10                                                             
         SR    RF,RF                                                            
         B     GDSVI4                                                           
         SR    RE,RE                                                            
         ICM   RF,15,0(R1)                                                      
         BP    *+10                                                             
         SR    RF,RF                                                            
         B     GDSVI4                                                           
         SLDA  RE,1                X 2                                          
         D     RE,MAXDEMS*4(R1)                                                 
*        AH    RF,=H'1'                                                         
         AHI   RF,1                                                             
         SRA   RF,1                                                             
GDSVI4   ST    RF,0(R5)                                                         
         LA    R1,4(R1)                                                         
*&&                                                                             
         LA    R5,4(R5)                                                         
         BCT   R4,GDSVI2                                                        
         J     EXIT                                                             
         LTORG                                                                  
*****************************************************************               
* SUBROUTINE TO CALCULATE ADJUSTED DEMO VALUES AS EQUAL TO      *               
*  UNADJUSTED VALUES X SVI'S                                    *               
* R1 POINTS TO 80 BYTE ADJ DEMOS FOLLOWED BY 80 BYTES UNADJ     *               
*****************************************************************               
                                                                                
GDXSVI   NTR1  BASE=*,LABEL=*                                                   
         L     R4,NUMDEMS                                                       
         LA    R5,SVIS                                                          
*                                                                               
GDXSVI2  L     RF,MAXDEMS*4(R1)    GET UNADJ VALUE                              
         C     RF,=F'-1'                                                        
         BE    *+8                                                              
         MH    RF,2(R5)            X SVI                                        
         ST    RF,0(R1)            STORE ADJ VALUE                              
*                                                                               
         LA    R1,4(R1)            NEXT DEMO                                    
         LA    R5,4(R5)            NEXT SVI                                     
         BCT   R4,GDXSVI2                                                       
         J     EXIT                                                             
*                                                                               
VUTLIST  DC    X'81',C'V',AL1(1)                                                
         DC    X'81',C'V',AL1(2)                                                
         DC    X'81',C'V',AL1(3)                                                
         DC    X'FF'                                                            
*                                                                               
PUTLIST  DC    X'81',C'P',AL1(1)                                                
         DC    X'81',C'P',AL1(2)                                                
         DC    X'81',C'P',AL1(3)                                                
         DC    X'FF'                                                            
*                                                                               
UNILIST  DC    X'43',C'U',AL1(1)   STRAIGHT UNIVERSE                            
         DC    X'43',C'U',AL1(2)                                                
         DC    X'43',C'U',AL1(3)                                                
         DC    X'FF'                                                            
         LTORG                                                                  
         DROP  R9,RA,RB                                                         
         EJECT                                                                  
*============================================================                   
* LOOK UP ALPHA MARKET CODE FOR RADIO                                           
*============================================================                   
                                                                                
GETAMKT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,=A(GETAMKCS)                                                  
         A     R4,RELO                                                          
         USING GETAMKCS,R4                                                      
*                                                                               
         LA    RE,=C'DMRDHI'       SET UP DMCB                                  
         ST    RE,DMCB                                                          
         LA    RE,=C'STATION'                                                   
         ST    RE,DMCB+4                                                        
         LA    RE,WORK                                                          
         ST    RE,DMCB+8                                                        
         MVC   DMCB+12(4),DBAREC                                                
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         LA    R1,DMCB                                                          
*                                                                               
         LA    R9,WORK             BUILD KEY OF STATION RECORD                  
         USING STARECD,R9                                                       
         XC    STAKEY,STAKEY                                                    
         MVI   STAKTYPE,C'S'       RECORD TYPE                                  
         MVI   STAKMED,C'R'        MEDIA                                        
         TM    BUYKAM,X'04'        TEST NETWORK RADIO                           
         BZ    *+8                                                              
         MVI   STAKMED,C'X'                                                     
         MVC   STAKCALL,DBSELSTA   STATION CALL LETTERS                         
         MVC   STAKAGY,BUYALPHA    AGENCY CODE                                  
         MVC   STAKCLT,DBSELCLI    CLIENT CODE                                  
*                                                                               
         L     RE,DBCOMFCS                                                      
         ICM   RE,15,CSWITCH-COMFACSD(RE)  NON-ZERO ONLINE ONLY                 
         BNZ   GETAMK0             AND NO SAVE AREA AVAILABLE                   
         MVC   DUB,SVMKTALF        RESTORE PREVIOUS ALPHA MKT                   
         CLC   SVSTAKEY(12),STAKEY TEST SAME STA/CLT AS PREV                    
         BE    GETAMK10                                                         
         MVC   SVSTAKEY,STAKEY     ELSE SAVE STA/CLT NOW                        
         XC    SVMKTALF,SVMKTALF   AND CLEAR SAVE AREA                          
         DROP  R9                                                               
*                                                                               
GETAMK0  XC    DBSELALF,DBSELALF                                                
         GOTO1 (RF),(R1)                                                        
*                                                                               
         L     R9,DBAREC                                                        
         USING STARECD,R9                                                       
*                                                                               
         CLC   STAKEY(12),WORK     DID WE FIND MASTER RECORD?                   
         BNE   GETAMK2             NO                                           
         CLI   SMKTALPH,C' '       ALPHA MKT IN IT                              
         BNH   GETAMK2             NO                                           
         MVC   DUB(3),SMKTALPH     MOVE CODE                                    
         B     GETAMK10                                                         
         DROP  R9                                                               
*                                                                               
GETAMK2  LA    R9,WORK             READ AGY DEFAULT STATION                     
         USING STARECD,R9                                                       
         MVC   STAKCLT,=3C'0'      CLEAR CLIENT CODE                            
         DROP  R9                                                               
         GOTO1 (RF),(R1)                                                        
*                                                                               
         L     R9,DBAREC                                                        
         USING STARECD,R9                                                       
*                                                                               
         CLC   STAKEY(12),WORK     DID WE FIND MASTER RECORD?                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   SMKTALPH,C' '       ANY ALPHA MKT IN IT?                         
         BNH   GETAMK4             NO                                           
         MVC   DUB(3),SMKTALPH     MOVE CODE                                    
         B     GETAMK10                                                         
         DROP  R9                                                               
*                                                                               
GETAMK4  LA    R9,WORK             NOT IN STATION, READ MKT REC                 
         USING MKTRECD,R9                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVI   MKTKTYPE,C'M'       RECORD TYPE                                  
         MVI   MKTKMED,C'R'        MEDIA                                        
         TM    BUYKAM,X'04'        TEST NETWORK RADIO                           
         BZ    *+8                                                              
         MVI   MKTKMED,C'X'                                                     
         SR    R0,R0                                                            
         ICM   R0,3,BUYREC+4       GET MKT NUM FROM BUY                         
         BZ    GETAMKX                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKTKMKT,DUB                                                      
         MVC   MKTKAGY,BUYALPHA    AGENCY CODE                                  
         DROP  R9                                                               
*                                                                               
         GOTO1 (RF),(R1)                                                        
*                                                                               
         L     R9,DBAREC           PULL OUT ALPHAMKT CODE                       
         USING MKTRECD,R9                                                       
*                                                                               
         CLC   WORK(8),0(R9)                                                    
         BNE   GETAMKX                                                          
         CLI   MKTALST,C' '         ANY ALPHA-MKT CODE?                         
         BNH   GETAMKX              NO                                          
         MVC   DUB(3),MKTALST                                                   
         DROP  R9                                                               
*                                                                               
GETAMK10 MVC   DBSELALF,DUB        PASS THROUGH ALPHAMKT                        
*                                                                               
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CPROTOFF-COMFACSD(RF)                                      
         JZ    *+6                                                              
         BASR  RE,RF               *** TURN STORAGE PROTECTION OFF ***          
*                                                                               
         MVC   SVMKTALF,DUB                                                     
*                                                                               
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CPROTON-COMFACSD(RF)                                       
         JZ    *+6                                                              
         BASR  RE,RF               TURN STORAGE PROTECTION BACK ON              
*                                                                               
         XC    DBSELRMK,DBSELRMK                                                
         XC    DBSELMK,DBSELMK                                                  
*                                                                               
GETAMKX  XIT1                                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
GETAMKCS CSECT                                                                  
SVSTAKEY DS    CL12                                                             
SVMKTALF DS    CL3                                                              
         EJECT                                                                  
GETDEMO  RSECT                                                                  
*======================================================================         
* READ USTV MARKET REC FOR DBSELUMK TO GET LPM START DATE                       
* FOR CABLE, GET RATING SERVICE AND ALPHA MARKET                                
*======================================================================         
                                                                                
GETMKT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CPROTOFF-COMFACSD(RF)                                      
         JZ    *+6                                                              
         BASR  RE,RF               *** TURN STORAGE PROTECTION OFF ***          
*                                                                               
         L     R4,=A(GETMKTCS)                                                  
         A     R4,RELO                                                          
         USING GETMKTCS,R4                                                      
*                                                                               
         LA    RE,=C'DMRDHI'       SET UP DMCB                                  
         ST    RE,DMCB                                                          
         LA    RE,=C'STATION'                                                   
         ST    RE,DMCB+4                                                        
         LA    RE,WORK                                                          
         ST    RE,DMCB+8                                                        
         MVC   DMCB+12(4),DBAREC                                                
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         LA    R1,DMCB                                                          
*                                                                               
         LA    R9,WORK                                                          
         USING MKTRECD,R9                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVI   MKTKTYPE,C'M'       RECORD TYPE                                  
         MVI   MKTKMED,C'T'        MEDIA                                        
         SR    R0,R0                                                            
         ICM   R0,3,DBSELUMK       MARKET NUMBER                                
         BNZ   GETMKT2                                                          
         XC    SVMKTKEY,SVMKTKEY                                                
         B     GETMKTX                                                          
*                                                                               
GETMKT2  CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKTKMKT,DUB                                                      
         MVC   MKTKAGY,BUYALPHA    AGENCY CODE                                  
*                                                                               
         L     RE,DBCOMFCS                                                      
         ICM   RE,15,CSWITCH-COMFACSD(RE)  NON-ZERO ONLINE ONLY                 
         BNZ   GETMKT4X               AND NO SAVE AREA AVAILABLE                
*                                                                               
         CLC   SVMKTKEY(8),MKTKEY    TEST SAME MARKET AS PREV                   
         BNE   GETMKT4                                                          
         MVC   GDMKCDEM,SVMKCDEM                                                
         MVC   GDMKALPH,SVMKALPH                                                
         MVC   LPMDTB,SVLPMDTB                                                  
         MVC   LPMDTP,SVLPMDTP                                                  
         B     GETMKTX                                                          
*                                                                               
GETMKT4  MVC   SVMKTKEY,MKTKEY        ELSE SAVE MKTKEY NOW                      
         DROP  R9                                                               
*                                                                               
GETMKT4X XC    GDMKCDEM,GDMKCDEM   CLEAR SAVE AREAS                             
         XC    GDMKALPH,GDMKALPH                                                
         XC    LPMDTB,LPMDTB                                                    
         XC    LPMDTP,LPMDTP                                                    
         GOTO1 (RF),(R1)           AND READ THE RECORD                          
*                                                                               
         L     R9,DBAREC                                                        
         USING MKTRECD,R9                                                       
*                                                                               
         CLC   WORK(8),0(R9)                                                    
         BNE   GETMKTX                                                          
*                                                                               
         MVC   GDMKALPH,MKTALST    MOVE ALPHA MARKET (NOT MKTAMKTC)             
         MVC   GDMKCDEM,SV00APRF   SET DFLT CBL SOURCE FROM 00A PROF            
         CLI   MKTCDEM,C'0'        TEST SUPPRESS LOOKUP                         
         BE    GETMKT6                                                          
         CLI   MKTCDEM,C'N'        TEST NSI SPECIFIED                           
         BE    GETMKT6                                                          
         CLI   MKTCDEM,C'F'        TEST FUSION SPECIFIED                        
         BNE   GETMKT8             IF NOT LEAVE 00A PROFILE AS DFLT             
*                                                                               
GETMKT6  MVC   GDMKCDEM,MKTCDEM    MOVE RATING SERVICE                          
*                                                                               
GETMKT8  MVC   LPMDTP,MKTLPMDT     SAVE 2-BYTE PACKED DATE                      
         OC    LPMDTP,LPMDTP       TEST FOR ANY 2-BYTE DATE                     
         BZ    GETMKTX             NONE - EXIT                                  
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(2,LPMDTP),(3,LPMDTB)                                  
         DROP  R9                                                               
*                                                                               
GETMKTX  MVC   SVLPMDTB,LPMDTB                                                  
         MVC   SVLPMDTP,LPMDTP                                                  
         MVC   SVMKALPH,GDMKALPH                                                
         MVC   SVMKCDEM,GDMKCDEM                                                
*                                                                               
* LOOK FOR DARK WEEK FOR LPM MARKET                                             
*                                                                               
         XC    LPMDRKWK,LPMDRKWK                                                
         OC    LPMDTP,LPMDTP       TEST FOR ANY 2-BYTE DATE                     
         BZ    GETMKTXX            NONE - EXIT                                  
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WKLYLPM                                                
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         JNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
*                                                                               
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         LR    RF,RE               SAVE A(FIRST ENTRY)                          
         USING WKLYLPMD,RE                                                      
GETMKX10 CLI   0(RE),X'FF'                                                      
         JE    GETMKTXX                                                         
         CLC   SVMKALPH,WKLYAMKT   LOOK FOR MARKET IN TABLE                     
         JE    GETMKX20                                                         
         AR    RE,R0                                                            
         J     GETMKX10                                                         
GETMKX20 MVC   LPMDRKWK,WKLYDARK    SAVE DARK WEEK                              
         DROP  RE                                                               
*                                                                               
GETMKTXX DS    0H                                                               
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CPROTON-COMFACSD(RF)                                       
         JZ    *+6                                                              
         BASR  RE,RF               TURN STORAGE PROTECTION BACK ON              
*                                                                               
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
GETMKTCS CSECT                                                                  
SVMKTKEY DS    CL8                                                              
SVLPMDTB DS    XL3                                                              
SVLPMDTP DS    XL2                                                              
SVMKCDEM DS    C                                                                
SVMKALPH DS    CL3                                                              
         EJECT                                                                  
GETDEMO  RSECT                                                                  
*============================================================                   
* READ STATION MASTER FOR ALTERNATE CABLE SYSCODE                               
*============================================================                   
                                                                                
GETSYSC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CPROTOFF-COMFACSD(RF)                                      
         JZ    *+6                                                              
         BASR  RE,RF               *** TURN STORAGE PROTECTION OFF ***          
*                                                                               
         L     R4,=A(GETSYSCC)                                                  
         A     R4,RELO                                                          
         USING GETSYSCC,R4                                                      
*                                                                               
         LA    RE,=C'DMRDHI'       SET UP DMCB                                  
         ST    RE,DMCB                                                          
         LA    RE,=C'STATION'                                                   
         ST    RE,DMCB+4                                                        
         LA    RE,WORK                                                          
         ST    RE,DMCB+8                                                        
         MVC   DMCB+12(4),DBAREC                                                
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         LA    R1,DMCB                                                          
*                                                                               
         LA    R9,WORK             BUILD KEY OF STATION RECORD                  
         USING STARECD,R9                                                       
         XC    STAKEY,STAKEY                                                    
         MVI   STAKTYPE,C'S'       RECORD TYPE                                  
         MVI   STAKMED,C'T'                                                     
         SR    R0,R0                                                            
         ICM   R0,3,DBSELSYC                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  STAKCALL(4),DUB                                                  
         MVI   STAKCALL+4,C'T'     NEED TO APPEND A T                           
         MVC   STAKAGY,BUYALPHA    AGENCY CODE                                  
         MVC   STAKCLT,=C'000'     READ AGENCY RECORD ONLY                      
*                                                                               
         L     RE,DBCOMFCS                                                      
         ICM   RE,15,CSWITCH-COMFACSD(RE)  NON-ZERO ONLINE ONLY                 
         BNZ   GETSYSC2            AND NO SAVE AREA AVAILABLE                   
         CLC   SVCBLKEY(12),STAKEY TEST SAME STA/CLT AS PREV                    
         BE    GETSYSC4                                                         
         MVC   SVCBLKEY,STAKEY     ELSE SAVE STA/CLT NOW                        
         DROP  R9                                                               
*                                                                               
GETSYSC2 GOTO1 (RF),(R1)                                                        
*                                                                               
         L     R9,DBAREC                                                        
         USING STARECD,R9                                                       
*                                                                               
         CLC   STAKEY(12),WORK     DID WE FIND MASTER RECORD?                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVCBLKUP,SCBLLKUP   MOVE CODE                                    
         DROP  R9                                                               
*                                                                               
GETSYSC4 MVC   CBLKUP,SVCBLKUP                                                  
*                                                                               
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CPROTON-COMFACSD(RF)                                       
         JZ    *+6                                                              
         BASR  RE,RF               TURN STORAGE PROTECTION BACK ON              
*                                                                               
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
GETSYSCC CSECT                                                                  
SVCBLKEY DS    CL12                                                             
SVCBLKUP DS    H                                                                
         EJECT                                                                  
GETDEMO  RSECT                                                                  
*==============================================================                 
*           SUBROUTINE TO COMPUTE WEIGHTED DEMO                                 
*==============================================================                 
                                                                                
GETWD    NTR1  BASE=*,LABEL=*                                                   
         LA    R6,24(R3)           POINT TO FIRST DEMO VALUE                    
         ZIC   R7,1(R3)                                                         
*        SH    R7,=H'24'                                                        
         SHI   R7,24                                                            
         SRL   R7,3                SET FOR BCT                                  
         LA    R8,SAVEWGTS         POINT TO WEIGHTS                             
         XC    WORK,WORK           CLEAR SAVE AREAS                             
         XC    WTDEMA(8),WTDEMA     AND ACCUMS                                  
GETWD2   CLI   1(R6),63            TEST WEIGHTED DEMO                           
         JNE   GETWD4              NO                                           
         TM    4(R6),X'80'         TEST OVERRIDE                                
         JO    EXIT                YES - DONE                                   
         ST    R6,WORK             ELSE SAVE SLOT ADDRESS                       
         MVC   WORK+7(1),0(R8)      AND WEIGHT (IF ANY)                         
         J     GETWD6                                                           
*                                                                               
GETWD4   SR    R0,R0                                                            
         ICM   R0,1,0(R8)          GET WEIGHT VALUE                             
         JZ    GETWD6                                                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,5(R6)          UNADJ DEMO VALUE                             
         MR    RE,R0               X WEIGHT                                     
         A     RF,WTDEMU                                                        
         ST    RF,WTDEMU                                                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,5(R6)          UNADJ DEMO VALUE                             
         SR    RE,RE                                                            
         IC    RE,3(R6)            SVI FACTOR                                   
         MR    RE,RE               GIVES ADJ DEM                                
         MR    RE,R0               X WEIGHT                                     
         A     RF,WTDEMA                                                        
         ST    RF,WTDEMA                                                        
*                                                                               
GETWD6   LA    R6,8(R6)                                                         
         LA    R8,1(R8)                                                         
         BCT   R7,GETWD2                                                        
         EJECT                                                                  
                                                                                
* DIVIDE ADJ BY UNADJ VALUE TO GET SVI *                                        
                                                                                
         ICM   R6,15,WORK          POINT TO WEIGHTED DEMO SLOT                  
         JZ    EXIT                                                             
         SR    RE,RE                                                            
         L     RF,WTDEMA           GET ADJ VALUE                                
         SLDA  RE,1                X 2                                          
         ICM   R0,15,WTDEMU                                                     
         BZ    *+6                                                              
         DR    RE,R0                                                            
*        AH    RF,=H'1'                                                         
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         STC   RF,3(R6)                                                         
*                                                                               
         MVC   4(4,R6),WTDEMU      WEIGHTED DEMO VALUE                          
         ICM   RF,15,4(R6)                                                      
         OC    WORK+4(4),WORK+4    TEST WEIGHT FOR WTD DEMO                     
         JZ    GETWD8                                                           
         SR    RE,RE                                                            
         L     RF,WTDEMU           -- PER MEL                                   
         SLDA  RE,1                X 2                                          
         D     RE,WORK+4           DIVIDE BY WEIGHT                             
*        AH    RF,=H'1'                                                         
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,4(R6)                                                         
         EJECT                                                                  
GETWD8   CLI   AFDSW,C'Y'          TEST AFFIDS LOOKED UP                        
         JNE   EXIT                                                             
                                                                                
* CALC DEMO VALUE X SPOTS *                                                     
                                                                                
         LH    RE,NOAFDCNT                                                      
         AH    RE,AFDCNT                                                        
         MR    RE,RE                                                            
                                                                                
* FIND SLOT IN TOTAL AREA *                                                     
                                                                                
         L     R1,SAVER1                                                        
         ICM   R1,15,20(R1)                                                     
         JZ    EXIT                                                             
*                                                                               
GETWD10  DS    0H                  SCAN TOTAL AREA FOR                          
         CLI   1(R1),63            WEIGHTED DEMO                                
         JE    GETWD12                                                          
         LA    R1,12(R1)                                                        
         CLI   1(R1),0                                                          
         JNE   GETWD10                                                          
         DC    H'0'                ERROR IF NOT THERE                           
*                                                                               
GETWD12  ST    RF,4(R1)            SET VALUE                                    
         MVC   10(2,R1),=H'100'    AND SVI                                      
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*===========================================================*                   
* SUBROUTINE SETS VALUES FOR SPECIAL 7 MINUTE AFFID OPTION  *                   
* IF PROGRAM SPANS 2 QH'S AND RUNS LESS THAN 8 MINUTES IN   *                   
* FINAL QH, SET END TIME TO 3 MINUTES PRIOR TO LAST QH      *                   
* E.G. IF PROG RUNS 11-1137, AFFIDS FROM 1128 TO 1139 WILL  *                   
* BE CHANGED TO 1127.                                       *                   
*===========================================================*                   
                                                                                
SET7     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* TEST PROG RUNS LESS THAN 8 MINUTES INTO LAST QH                               
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,BDTIMEND                                                    
         D     R0,=F'100'          HOURS IN R1/ MINS IN R0                      
         SRDL  R0,32                                                            
         D     R0,=F'15'           MINUTES INTO LAST QH IN R0                   
         LTR   R0,R0               TEST ENDS ON QH BOUNDARY                     
         BZ    SET7X                                                            
         CH    R0,=H'7'            ELSE TEST 1-7 MINUTES INTO QH                
         BH    SET7X                                                            
*                                                                               
* TEST PROG RUNS AT LEAST 2 QHS                                                 
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,BDTIMST                                                     
         BAS   RE,SET7QH                                                        
         LR    RF,R1               SAVE START QH NUMBER                         
         SR    R1,R1                                                            
         ICM   R1,3,BDTIMEND                                                    
         BAS   RE,SET7QH                                                        
         CR    R1,RF               TEST SAME START/END QH                       
         BE    SET7X               YES                                          
*                                                                               
         D     R0,=F'4'            HOURS/100 IN R1, MINS/15 IN R0               
         MH    R0,=H'15'           MINS IN R0                                   
*        SH    R0,=H'2'            BACK UP 2 MINUTES                            
         SHI   R0,2                BACK UP 2 MINUTES                            
         BP    *+10                                                             
         BCTR  R1,0                BACK UP 1 HOUR                               
         LA    R0,58               AND SET MINUTES                              
         MH    R1,=H'100'          HOUR X 100                                   
         AR    R1,R0                                                            
         STH   R1,AF7QHST          SET START OF LAST QH IN HHMM                 
         BCTR  R1,0                BACK UP 1 MORE MINUTE                        
         STH   R1,AF7QHSET         AND SET 'SET TIME' VALUE                     
         EJECT                                                                  
* REMEMBER THAT PROGRAM ENDS WITHIN 7 MINUTES OF QH SO CAN JUST ADD 2           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,BDTIMEND       GET END TIME                                 
         LA    R1,0(R1)            ADD 0 MINUTES - NO BREAK                     
         STH   R1,AF7QHEND         SET END TIME OF LAST QH IN  HHMM             
*                                                                               
SET7X    XIT1                                                                   
                                                                                
*==========================================================*                    
* CONVERT MILITARY TIME IN R1 TO QH NUMBER                 *                    
*==========================================================*                    
                                                                                
SET7QH   SR    R0,R0                                                            
         D     R0,=F'100'          HOUR IN R1/MIN IN R0                         
         SLL   R1,2                HOUR X 4                                     
         ST    R1,DUB                                                           
         SRDL  R0,32                                                            
         D     R0,=F'15'                                                        
         A     R1,DUB                                                           
         SR    R0,R0               EXIT WITH R0=0 ALWAYS                        
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*===================================================================*           
* LOOK UP FIRST DEMO FOR EACH BREAK TIME IN AFFID LIST TO DETERMINE *           
* WHICH HAS LOWEST RATING. THEN CHANGE TIME TO APPROPRIATE QH.      *           
* FEATURE CONTROLLED BY CHARACTER 5 OF SPD0 PROFILE                 *           
*===================================================================*           
                                                                                
LQHSET   NMOD1 0,**LQH***                                                       
         L     RC,0(R1)            RESTORE WORK AREA POINTER                    
*                                                                               
         XC    LQHLIST,LQHLIST     BUILD DEMO LIST (FIRST RATING)               
         MVC   LQHLIST(4),LRAD3564                                              
*                                                                               
         L     R4,AAFDLST                                                       
*                                                                               
LQH10    MVC   DBSELDAY,0(R4)                                                   
         CLI   DBSELDAY,X'95'      TEST MO-FR                                   
         BNE   *+8                                                              
         MVI   DBSELDAY,X'40'      SET TO MONDAY                                
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,1(R4)          GET TIME                                     
         BAS   RE,TIMTOQH          CONVERT TIME TO START QH                     
         STC   R1,SQHNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,EQHNUM                                                        
*                                                                               
         L     R0,DUB+4            GET MINUTES                                  
         CH    R0,=H'3'            TEST BETWEEN 00 AND 03                       
         BNH   LQH14               YES- BREAK SPOT                              
*                                                                               
         CH    R0,=H'27'           TEST BETWEEN 04 AND 27                       
         BL    LQH40               YES - NOT A BREAK SPOT                       
         CH    R0,=H'33'           TEST BETWEEN 27 AND 33                       
         BH    LQH16               NO                                           
         CH    R0,=H'30'           TEST BETWEEN 27 AND 30                       
         BL    LQH20               YES                                          
*                                                                               
LQH14    BCTR  R1,0                BACKUP 1 QH FOR 00-03 AND 30-33              
         STC   R1,EQHNUM                                                        
         BCTR  R1,0                                                             
         STC   R1,SQHNUM                                                        
         B     LQH20                                                            
*                                                                               
LQH16    CH    R0,=H'57'           TEST BETWEEN 34 AND 57                       
         BL    LQH40               YES - NOT A BREAK SPOT                       
*                                                                               
* BREAK SPOT                                                                    
*                                                                               
LQH20    L     R0,=F'-1'           SET FLAG FOR NOT FOUND                       
         ST    R0,SQHVAL                                                        
         ST    R0,EQHVAL                                                        
*                                                                               
         XC    DBSEL1WK,DBSEL1WK   CLEAR WEEKLY REQUEST FIELDS                  
         MVI   DBSELWKN,0                                                       
         TM    WKLYOPT,X'80'       TEST WEEKLY LOOK-UP                          
         BZ    *+10                NO                                           
         MVC   DBSEL1WK(2),3(R4)   SET WEEK DATE                                
*                                                                               
         ZIC   R1,SQHNUM                                                        
         BAS   RE,QHTOTIM          CONVERT START QH TO TIME                     
         STCM  R1,3,DBSELTIM       SET START TIME FOR DEMO LOOKUP               
*                                                                               
         MVI   DEMANDSW,0                                                       
         MVC   DBTAPEP,TAPEOPT     TAPE/BOOK PRECISION                          
                                                                                
         CLI   DBSELMED,C'R'       IF RADIO,                                    
         BNE   *+8                                                              
         MVI   DBBEST,C'P'          SUPPORT OVERNIGHT DAYPART                   
                                                                                
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,LQHHOOK                                         
         MVC   SQHVAL,EQHVAL       SAVE VALUE FROM EQHVAL                       
*                                                                               
         ZIC   R1,EQHNUM                                                        
         BAS   RE,QHTOTIM                                                       
         STCM  R1,3,DBSELTIM                                                    
         MVC   DBTAPEP,TAPEOPT     TAPE/BOOK PRECISION                          
         CLI   DBSELMED,C'R'       IF RADIO,                                    
         BNE   *+8                                                              
         MVI   DBBEST,C'P'          SUPPORT OVERNIGHT DAYPART                   
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB                                                        
*                                                                               
         SR    R1,R1                                                            
         IC    R1,EQHNUM                                                        
         CLC   EQHVAL,SQHVAL       COMPARE RHOMES FOR EQH TO SQH                
         BH    LQH30               IF EQH IS HIGH USE SQH                       
         BAS   RE,QHTOTIM          ELSE USE EQH                                 
         B     LQH35                                                            
*                                                                               
LQH30    IC    R1,SQHNUM           GET SQHNUM                                   
         BAS   RE,QHTOTIM                                                       
*                                                                               
LQH35    LA    R1,5(R1)            ADD 5 MINUTES SO NEVER IN BREAK              
         STCM  R1,3,1(R4)          AND OVERWRITE TIME IN AFDLIST                
*                                                                               
LQH40    LA    R4,L'AFDLIST(R4)    NEXT AFD                                     
         CLI   0(R4),0             TEST EOL                                     
         BNE   LQH10                                                            
         XIT1                                                                   
*                                                                               
LRAD3564 DC    X'81',C'R',AL1(153)                                              
         DC    X'FF'                                                            
         EJECT                                                                  
*========================================================*                      
* CONVERT TIME TO START QUARTER HOUR NUMBER              *                      
* INPUT IS TIME IN R1, OUTPUT IS QH IN R1                *                      
* RETURN HOUR IN DUB, MINUTES IN DUB+4                   *                      
*========================================================*                      
                                                                                
TIMTOQH  SR    R0,R0                                                            
         D     R0,=F'100'          HOUR IN R1/MIN IN R0                         
         SLL   R1,2                                                             
         ST    R1,DUB              STORE HOUR IN DUB                            
         ST    R0,DUB+4            STORE MINUTES IN DUB+4                       
         SRDL  R0,32                                                            
         D     R0,=F'15'                                                        
         A     R1,DUB                                                           
         BR    RE                                                               
                                                                                
QHTOTIM  SR    R0,R0                                                            
         D     R0,=F'4'            HOURS IN R1/QH IN R0                         
         MH    R1,=H'100'          GIVES HOURS X 100                            
         MH    R0,=H'15'           GIVES QH X 15                                
         AR    R1,R0                                                            
         BR    RE                                                               
                                                                                
*================================================================*              
* DEMAND HOOK FOR LQH TO EXTRACT RHOMES                          *              
*================================================================*              
                                                                                
LQHHOOK  NTR1                                                                   
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         GOTO1 (RF),P1,(C'L',LQHLIST),DBLOCK,EQHVAL                             
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* SUBR TO FIND DEMO IN OLD ELEMENT (AT R6)                                      
*===================================================================            
                                                                                
GDSRCH   NTR1  BASE=*,LABEL=*                                                   
         ZIC   R0,1(R6)                                                         
         AHI   R0,-24                                                           
         BNP   GDSRCHX                                                          
         SRL   R0,3                                                             
         LA    R6,24(R6)                                                        
*                                                                               
GDSRCH2  CLC   1(2,R1),1(R6)       MATCH DEMO                                   
         BE    GDSRCH4                                                          
         LA    R6,8(R6)                                                         
         BCT   R0,GDSRCH2                                                       
         B     GDSRCHX                                                          
*                                                                               
GDSRCH4  TM    4(R6),X'80'         TEST OVRD                                    
         BZ    GDSRCHX                                                          
         TM    4(R1),X'80'         YES-TEST ALREADY HAVE POST BUY OVRD          
         BO    GDSRCHX             YES                                          
         MVC   3(5,R1),3(R6)       NO-MOVE SVI                                  
*                                                                               
GDSRCHX  J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* SET ADDRESS CONSTANTS. ON ENTRY R1 POINTS TO CALLERS PARAM LIST               
*================================================================               
                                                                                
SETADCON NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,16(R1)                                                        
         MVC   DBCOMFCS,DEMFCOMF-DEMFACSD(RE)  SET A(COMFACS)                   
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,0,X'D9000A24'                                          
         ICM   R0,15,0(R1)                                                      
         BZ    BADLOAD                                                          
         ST    R0,VGETIUN                                                       
*                                                                               
         GOTO1 (RF),DMCB,0,X'D9000A26'                                          
         ICM   R0,15,0(R1)                                                      
         BZ    BADLOAD                                                          
         ST    R0,VDEFINE                                                       
*                                                                               
         GOTO1 (RF),DMCB,0,X'D9000A15'                                          
         ICM   R0,15,0(R1)                                                      
         BZ    BADLOAD                                                          
         ST    R0,VCLUNPK                                                       
*                                                                               
         GOTO1 (RF),DMCB,0,X'D9000A17'                                          
         ICM   R0,15,0(R1)                                                      
         BZ    BADLOAD                                                          
         ST    R0,VNETWEEK                                                      
*                                                                               
         GOTO1 (RF),DMCB,0,X'D9000A7A'                                          
         ICM   R0,15,0(R1)                                                      
         BNZ   *+6                                                              
BADLOAD  DC    H'0'                                                             
         ST    R0,VSTAPACK                                                      
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,SPBOOKTB                                               
         ICM   RE,15,0(R1)         A(BOOKTYPE TABLE) RETURNED IN P1             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,VBKTYPTB         SAVE A(BOOKTYPE TABLE)                       
         MVC   BKTYPTBL,6(R1)      SAVE L'BOOKTYPE TABLE ENTRY                  
*                                                                               
         L     R0,=A(VUTLIST)                                                   
         A     R0,RELO                                                          
         ST    R0,AVUTLIST                                                      
         L     R0,=A(PUTLIST)                                                   
         A     R0,RELO                                                          
         ST    R0,APUTLIST                                                      
         L     R0,=A(UNILIST)                                                   
         A     R0,RELO                                                          
         ST    R0,AUNILIST                                                      
*                                                                               
         MVI   COMPASS,0                                                        
         L     RF,DBCOMFCS                                                      
         L     RF,CMASTC-COMFACSD(RF)      RF=AMASTC                            
         ICM   RF,15,MCAEXTRA-MASTD(RF)    EXTRA DATA AREA                      
         BZ    *+10                                                             
         MVC   COMPASS,MCCSPASS-MCEXTRA(RF)                                     
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
       ++INCLUDE SPCNCBLTAB                                                     
         EJECT                                                                  
**************************************************************                  
* DETERMINE IF THIS MARKET IS A TRUE LPM MARKET                                 
**************************************************************                  
* INPUT : STABMKT SET TO MARKET                                                 
*                                                                               
TRUELPM  NTR1  BASE=*,LABEL=*                                                   
*&&DO                                                                           
* THIS CODE HAS BEEN REPLACED WITH NEW CODE TO LOOK UP ANOTHER TABLE            
* IN DEMTABS.  BEN 9/7/2017                                                     
         MVI   TRUELPMF,C'N'                                                    
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,FUSNENDP                                               
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)                                                         
         USING FUSENDPD,RE                                                      
TRUEL10  CLC   =X'FFFF',0(RE)                                                   
         BE    TRUELPMN                                                         
         CLC   STABKMKT,FUSNMKT                                                 
         BE    TRUELPMY                                                         
         AR    RE,RF                                                            
         B     TRUEL10                                                          
TRUELPMY MVI   TRUELPMF,C'Y'                                                    
         J     TRUELPMX                                                         
TRUELPMN DS    0C                                                               
         XC    LPMDTP,LPMDTP                                                    
*&&                                                                             
* NEW CODE LOOKS UP NSIMKTS DEMTABS TABLE.  BEN 9/7/2017                        
         L     R4,=A(GETMKTCS)                                                  
         A     R4,RELO                                                          
         USING GETMKTCS,R4                                                      
         MVI   TRUELPMF,C'N'                                                    
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,NSIMKTS                                                
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)                                                         
         USING NMKTD,RE                                                         
TRUEL10  CLI   0(RE),X'FF'                                                      
         BE    TRUELPMN                                                         
         CLC   STABKMKT,NMKTNUM                                                 
         BE    TRUEL20                                                          
         CLC   SVMKALPH,NMKTALF                                                 
         BE    TRUEL20                                                          
         AR    RE,RF                                                            
         B     TRUEL10                                                          
TRUEL20  DS    0C                                                               
         CLI   NMKTTYPE,NMKTLQ                                                  
         BNE   TRUELPMN                                                         
TRUELPMY MVI   TRUELPMF,C'Y'                                                    
         J     TRUELPMX                                                         
TRUELPMN DS    0C                                                               
         XC    LPMDTP,LPMDTP                                                    
         DROP  R4                                                               
*                                                                               
TRUELPMX XIT1                                                                   
*                                                                               
********************************************************************            
* CHECK TO SEE IF THE BUY/AFFID DATE IS BEFORE THE MMW CUTOFF DATE.             
* WE NO LONGER HAVE MMW DATA STARTING WITH SEP1/12.                             
* RESET OPTIONS FOR MONTHLY LOOKUPS STARTING WITH CUTOFF DATE.                  
********************************************************************            
TOMONTHL NTR1  BASE=*,LABEL=*                                                   
         MVC   SVLPMWKOPT,LPMWKOPT  SAVE ORIGINAL OPTIONS                       
         MVC   SVWVPHOPT,WVPHOPT                                                
*                                                                               
         CLI   LPMWKOPT,C'Y'      CHECK WEEKLY OPTIONS SET                      
         BE    TOMON10                                                          
         CLI   WVPHOPT,C'Y'                                                     
         BE    TOMON10                                                          
         B     TOMONTHX                                                         
*                                                                               
TOMON10  L     RF,DBCOMFCS        GET BUY/AFFID DATE IN DUB(BINARY YMD)         
         L     RF,CDATCON-COMFACSD(RF)                                          
         OC    VPHSDAT,VPHSDAT                                                  
         BNZ   TOMON20                                                          
         GOTO1 (RF),DMCB,(3,BDSTART),(3,DUB)                                    
         B     TOMON30                                                          
TOMON20  GOTO1 (RF),DMCB,(2,VPHSDAT),(3,DUB)                                    
                                                                                
TOMON30  CLI   AFFIDLK,C'N'       IF ACHIEVED,                                  
         BNE   *+8                ADJUST DATE TO START OF ROTATION,             
         BRAS  RE,ADJDADAT        AND RETUN IT IN DUB                           
*                                                                               
         MVC   DUB2,=C'120901'    CUTOFF DATE IS SEP1/12                        
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(0,DUB2),(3,DUB3)   CONVERT TO BINARY YMD              
         CLC   DUB(3),DUB3        COMPARE DATES                                 
         BL    TOMONTHX           PRIOR TO CUTOFF DATE, CONTINUE AS BEF         
*                               RESET TO MONTHLY LOOKUPS                        
         MVI   LPMWKOPT,C'N'      LPMWK=N                                       
         MVI   WVPHOPT,C'N'       WTP=N                                         
         MVI   DBSELMED,C'T'      MEDIA FOR MONTHLY                             
         MVI   TAPEOPT,C'Y'       MONTHLY DATA IS IMP BASED                     
         MVI   DBBEST,0                                                         
         MVI   MKTTYPE,0                                                        
*                                                                               
TOMONTHX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* ROUTINE TO GET BUY RECORD POST BUY DEMO OVERRIDES                             
*=============================================================                  
                                                                                
PBDEMOS  NTR1  BASE=*,LABEL=*                                                   
         MVI   PBDEMSW,C'N'                                                     
         MVI   PBLKERR,C'N'                                                     
         MVC   ELCODE,0(R3)        FIND BUY RECORD DEMO ELEMENTS                
         NI    ELCODE,X'0F'                                                     
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         SR    R8,R8                                                            
         LA    R6,BDELEM                                                        
*                                                                               
PBD2     CLI   0(R6),0                                                          
         BE    PBD12                                                            
         CLI   ELCODE,2                                                         
         BNE   PBD4                                                             
         CLI   0(R6),2                                                          
         BE    PBD6                                                             
         CLI   0(R6),X'22'                                                      
         BE    PBD8                                                             
         B     PBD10                                                            
*                                                                               
PBD4     CLI   0(R6),3                                                          
         BE    *+12                                                             
         CLI   0(R6),X'13'                                                      
         BNE   PBD5                                                             
         CLC   4(2,R3),4(R6)                                                    
         BE    PBD6                                                             
         B     PBD10                                                            
*                                                                               
PBD5     CLI   0(R6),X'23'                                                      
         BNE   PBD10                                                            
         CLC   4(2,R3),SDAGYMKT-SDELEM(R6)                                      
         BE    PBD8                                                             
         B     PBD10                                                            
*                                                                               
PBD6     LR    R1,R6               R1=A(OLD DEMO ELEMENT)                       
         B     PBD10                                                            
PBD8     LR    R8,R6               R8=A(POST BUY DEMO ELEMENT)                  
*                                                                               
PBD10    IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PBD2                                                             
*                                                                               
PBD12    LTR   R8,R8               TEST POST BUY DEMO ELE FOUND                 
         BZ    PBDX                                                             
         LTR   R6,R1               YES-CHECK DEMO ELEMENT FOUND                 
         BZ    PBDX                                                             
         ZIC   R7,1(R6)                                                         
*        SH    R7,=H'24'                                                        
         SHI   R7,24                                                            
         BNP   PBDX                                                             
         SRL   R7,3                R7=N'DEMOS IN OLD DEMO ELEMENT               
         LA    R6,24(R6)           R6=A(DEMOS IN OLD DEMO ELEMENT)              
         LA    R4,3                                                             
         ZIC   R5,1(R8)            R4,R5 FOR POST BUY DEMO ELE BXLE             
         AR    R5,R8                                                            
         BCTR  R5,0                                                             
         LA    R1,PDEMO-PDELEM(R8)                                              
         CLI   ELCODE,2                                                         
         BE    *+8                                                              
         LA    R1,SDEMO-SDELEM(R8)                                              
         LR    R8,R1               R8=A(DEMO VALS IN POST BUY DEMO ELE)         
         STM   R4,R8,WORK                                                       
         L     R0,NUMDEMS                                                       
         LA    R1,24(R3)           R1=A(DEMOS IN NEW DEMO ELE)                  
         XC    PBDEMVAL,PBDEMVAL                                                
         LA    RE,PBDEMVAL                                                      
*                                                                               
PBD14    MVI   0(RE),100           DEFAULT SVI = 100                            
         LM    R4,R8,WORK                                                       
*                                                                               
PBD16    CLC   0(3,R6),0(R1)       MATCH THE DEMO                               
         BNE   PBD18                                                            
         TM    0(R8),X'80'         TEST POST BUY DEMO OVERRIDE                  
         BO    PBD17                                                            
         CLI   ELCODE,3            NO-LEAVE ZERO IF SPILL AND EXCLUDE           
         BNE   *+12                   UNREPORTED SPOTS                          
         CLI   SVD0PROF+1,C'Y'                                                  
         BE    PBD18                                                            
         MVC   0(5,RE),3(R6)       ELSE SAVE PURCHASED VALUE AND SVI            
         NI    1(RE),X'3F'                                                      
         B     PBD20                                                            
*                                                                               
PBD17    STM   R0,R1,R0SAVE                                                     
         OI    1(RE),X'80'                                                      
         SR    R1,R1                                                            
         ICM   R1,7,0(R8)          GET VALUE AND FLAGS                          
         N     R1,=X'003FFFFF'     DROP FLAGS                                   
         CLI   1(R6),C'R'          TEST RATING                                  
         BE    *+12                                                             
         CLI   1(R6),C'E'          OR EXTENDED RATING                           
         BNE   PBD17A                                                           
         TM    TWODEC,X'40'        TEST 2-DEC RTG REQUEST                       
         BO    PBD17C                                                           
         B     PBD17B                                                           
*                                                                               
PBD17A   TM    TWODEC,X'01'        TEST 2-DEC IMP REQUEST                       
         BO    PBD17C                                                           
                                                                                
PBD17B   TM    0(R8),X'40'         1-DEC REQUEST, TEST 2-DEC VALUE              
         BZ    PBD17D                                                           
         M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         B     PBD17D                                                           
*                                                                               
PBD17C   TM    0(R8),X'40'         2-DEC REQ, TEST 2-DEC VALUE                  
         BO    PBD17D                                                           
         MHI   R1,10               CONVERT TO 2-DECIMAL VALUE                   
*                                                                               
PBD17D   STCM  R1,7,2(RE)                                                       
         MVI   PBDEMSW,C'Y'        INDICATE POST BUY DEMO OVERRIDES             
         LM    R0,R1,R0SAVE                                                     
         B     PBD20                                                            
*                                                                               
PBD18    LA    R6,8(R6)            NEXT DEMO IN OLD DEMO ELE                    
         BCT   R7,*+8                                                           
         B     PBD20                                                            
         BXLE  R8,R4,PBD16         NEXT DEMO VAL IN POST BUY DEMO ELE           
*                                                                               
PBD20    LA    R1,8(R1)            NEXT DEMO                                    
         LA    RE,5(RE)                                                         
         BCT   R0,PBD14                                                         
*                                                                               
PBDX     J     EXIT                                                             
         EJECT                                                                  
*===============================================================                
* CALL COMINTER  R1 HAS A(ACHVD) OR A(AFFID)                                    
* ON ENTRY, R4 POINTS TO AFFID LIST ENTRY ON AFFID CALL                         
*===============================================================                
                                                                                
COMSCORE NTR1  BASE=*,LABEL=*,WORK=(R6,32)    REG/DBLWDS                        
*                                                                               
         MVC   0(256,R6),DBLOCK    SAVE CALLERS DBLOCK                          
*                                                                               
         BRAS  RE,TRAMKT                                                        
* USE ORIGNAL CALL LETTERS BEFORE HITTING DEMO SYSTEM                           
* DEMO SYSTEM CHANGES DBSELSTA TO NUMERIC FOR FUSION                            
*                                                                               
         OC    SVSTPSTA,SVSTPSTA   ONLY RESTORE IF IT WAS SAVED!                
         JZ    *+10                                                             
         MVC   DBSELSTA,SVSTPSTA                                                
*                                                                               
         L     RE,SAVER1                                                        
         L     RE,8(RE)                                                         
         USING GETDEMD,RE                                                       
         TM    GTDMFLAG,X'02'      TEST PAR+SAT FLAG                            
         JZ    COMSC02                                                          
         CLI   DBSELMED,C'T'                                                    
         JNE   COMSC02                                                          
         MVI   DBSELSTA+4,C'+'     OVERWRITE THE TRAIILING T                    
         CLI   DBSELSTA+3,C' '     IF ONLY 3 CHAR STATION                       
         JNE   COMSC02                                                          
         MVI   DBSELSTA+3,C'+'                                                  
         MVI   DBSELSTA+4,C' '                                                  
         DROP  RE                                                               
*                                                                               
COMSC02  L     RF,ACOMEXT                                                       
         XC    0(L'COMEXT,RF),0(RF)                                             
*                                                                               
         USING DBCMINTD,RF                                                      
         MVC   DBCMID,=C'CLOC'        COMINTER                                  
         MVC   DBCMNEXT,DBEXTEND                                                
         MVC   DBEXTEND,ACOMEXT                                                 
*                                                                               
         CLI   ACTBOOK,C'Y'        IF ACTUAL BOOK REQUEST                       
         BE    COMSC10             THEN DO NOT SET BOOK                         
         CLI   OVPHOPT,C'Y'        DO NOT SET FORCE BOOK FOR                    
         BE    COMSC10             NIGHTS, ONLY FOR MONTHLY POSTING             
         MVC   DBCMFRBK,DBSELBK                                                 
*                                                                               
COMSC10  MVI   DBCMPREC,C'1'                                                    
         TM    TWODEC,X'40'        TEST 2-DEC RTG REQUEST                       
         JZ    *+8                                                              
         MVI   DBCMPREC,C'2'                                                    
*                                                                               
         TM    TWODEC,X'01'        TEST 2-DEC IMP REQUEST                       
         BE    *+8                                                              
         OI    DBBSTFLG,DBBST2DI   THEN REQUEST THEM STUPIDO                    
*                                                                               
         MVC   DBCMMKT,SVNUMMKT                                                 
         LA    R0,DEMOLIST                                                      
         ST    R0,DBDEMOL                                                       
*                                                                               
         LA    R0,DTDEMSU                                                       
         ST    R0,DBDEMVAL                                                      
*                                                                               
         MVC   DBCMAEST,AD50EL    A(NONT DEMO LIST)                             
         MVI   DBCMBTYP,C'L'      ALWAYS LIVE ONLY BOOKTYPE                     
         MVC   DBCMDATE,COMSCSD                                                 
*                                                                               
COMACHVD CLI   COMTYPE,C'A'       TEST ACHIEVED                                 
         BNE   COMAFFD                                                          
*                                                                               
         CLI   OVPHOPT,C'Y'       IF OVERNIGHTS REQUESTED                       
         BE    *+8                                                              
         CLI   OVPHOPT,C'M'                                                     
         BNE   *+14                                                             
* VPHSDAT COMES FROM START/END DATES SET BY MEDGETBY                            
         MVC   DBCMSPDT,VPHSDAT   SET SPOTDATE FIELD WITH WEEK DATE             
         B     COMPASS1                                                         
* MONTHLY POSTING - USE BUYDATES                                                
         MVC   DBCMSTDT(6),SVBUYDTS  SET 3-BYTE BUY START/END DATES             
         MVC   DBCMREQS(4),SVREQDTS  SET 2-BYTE REQ START/END DATES             
         B     COMPASS1                                                         
*                                                                               
COMAFFD  MVC   DBCMSPDT,3(R4)     SET DATE FROM AFDLIST                         
         DROP  RF                                                               
*                                                                               
COMPASS1 DS    0C                                                               
         CLI   COMPASS,C'1'                                                     
         BNE   COMPASS2                                                         
         GOTO1 VCOMINTR,DMCB,(0,=C'PUT'),DBLOCK                                 
         B     COMSCORX                                                         
*                                                                               
COMPASS2 DS    0C                                                               
         CLI   COMPASS,C'2'                                                     
         JNE   *+2                                                              
         MVI   COMERRF,0                                                        
         XC    DMCB,DMCB                                                        
         GOTO1 VCOMINTR,DMCB,(0,=C'GET'),DBLOCK                                 
         CLI   DMCB,X'02'           FAIL STATUS?                                
         BNE   *+10                                                             
         MVC   COMERRF,DMCB         SAVE ERROR CODE FROM COMINTER               
*                                                                               
         BRAS  RE,GETADJ                                                        
         LA    R1,DTDEMSA                                                       
         BRAS  RE,GDXSVI                                                        
*                                                                               
         CLI   NSIREQ,C'Y'         TEST ANY NEILSEN DEMOS                       
         JE    COMSCORX                                                         
*                                                                               
         L     RE,ACOMEXT                                                       
         USING DBCMINTD,RE                                                      
         MVC   STNAME,DBCMPNAM                                                  
         MVC   ENDNAME,DBCMPNAM                                                 
         DROP  RE                                                               
*                                                                               
COMSCORX MVC   DBLOCK(256),0(R6)     RESTORE DBLOCK                             
         XIT1                                                                   
                                                                                
*=================================================================              
* TRANSLATE SVMKALPH ALPHA MKT TO SVNUMMKT (NIELSEN NUMERIC MKT)                
*=================================================================              
                                                                                
TRAMKT   NTR1                                                                   
*                                                                               
         L     R4,=A(GETMKTCS)                                                  
         A     R4,RELO                                                          
         USING GETMKTCS,R4                                                      
*                                                                               
         MVI   DBFUNCT,DBCNVA2N                                                 
         MVC   DBSELALF,SVMKALPH                                                
         MVI   DBSELMED,C'T'         USE MEDIA/BOOK LAST READ                   
         MVI   DBSELSRC,C'N'         ALWAYS USE NIELSEN MARKETS                 
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0,0                                             
*                                                                               
         CLI   DBERROR,0           PICK UP ERROR CODE                           
         JZ    *+12                                                             
         AHI   R0,X'40'            SET COMSCORE ERROR FLAG                      
         J     TRAMKTX                                                          
*                                                                               
         MVC   SVNUMMKT,DBSELRMK                                                
         CLC   DBSELRMK,=H'168'     ATLANTA IS 168 IN OUR SYSTEM                
         BNE   *+10                 BUT IT SHOULD BE 124 TO REST OF             
         MVC   SVNUMMKT,=H'124'     THE WORKD                                   
*                                                                               
TRAMKTX  J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
*                                                                               
MAXDEMS  EQU   42                  *******************                          
*                                                                               
SAVERD   DS    F                                                                
SAVER1   DS    F                                                                
R0SAVE   DS    F                                                                
R1SAVE   DS    F                                                                
DUB      DS    D                                                                
DUBP     DS    D                                                                
DUB2     DS    D                                                                
DUB3     DS    D                                                                
DPWORK   DS    CL16                                                             
ALET     DS    A                                                                
RELO     DS    A                                                                
ADBUY    DS    A                                                                
ADDEMEL  DS    A                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
DMCB     DS    6F                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
WORK     DS    CL80                                                             
SV00APRF DS    CL16                                                             
*                                                                               
SVBDSTRT DS    XL3                 SAVE BUY START/END DATES                     
SVBDEND  DS    XL3                                                              
SVCNFLGS DS    XL1                                                              
COMSCSD  DS    XL1                 COMSCORE SURVEY DATES                        
ACTBOOK  DS    CL1                                                              
COMREQ   DS    CL1                                                              
NSIREQ   DS    CL1                                                              
COMPASS  DS    CL1                 C'1' FOR PASS 1                              
COMTYPE  DS    CL1                 C'I' FOR AFFID, C'A' FOR ACHVD               
         DS    XL1                 SPARE                                        
*                                                                               
ADISPTAB DS    A                   MASTER DSPL TABLE ADDRESS                    
ADEMLSTX DS    A                   DEMO LIST END ADDRESS                        
AD50EL   DS    A                   A(NONT DEMO NAMES ELEM)                      
NUMDEMS  DS    F                   ACTUAL NUMBER OF DEMOS IS DEMOLIST           
DEMOLIST DS    XL((MAXDEMS+1)*3)   DEMO LIST FROM BUYREC                        
*                                   USER AND OVERRIDDEN DEMOS EXCLUDED          
RDEMLIST DS    XL((MAXDEMS+1)*5)   DEMO LIST FOR USE W/ MEDIA=R                 
*                                   (SEE RDEMLSTD)                              
PBDEMVAL DS    XL(MAXDEMS*5)                                                    
SAVEHUT  DS    XL1                 USER SVI ADJUSTMENT MONTH(S)                 
SAVENTI  DS    CL4                 NTI LOOKUP CALL LETTERS                      
SAVEMKT  DS    XL2                 MARKET NUMBER FOR SVI READ                   
SAVEUID  DS    XL2                 USERID                                       
SAVEWGTS DS    XL(MAXDEMS)         WEIGHTED DEMO LIST OF WEIGHTS                
*                                                                               
AVUTLIST DS    A                                                                
APUTLIST DS    A                                                                
AUNILIST DS    A                                                                
*                                                                               
ORBWGT   DS    F                   COUNT OF ORBDEM ITEMS                        
*                                                                               
SVIS     DS    XL(MAXDEMS*4)       MONTH WEIGHTED SVI VALUES (1/DEMO)           
SVITYPES DS    XL(MAXDEMS)         SVI TYPE FOR EACH ENTRY ABOVE                
SVIWGT   DS    F                   COUNT OF SVI ITEMS                           
         DS    0F                                                               
QHUNIV   DS    XL12                                                             
QHVUTS   DS    XL12                                                             
DTVUTS   DS    XL12                                                             
                                                                                
OVHOMUNV DS    F                   OV HOM UNIV                                  
OVPHHMES DS    XL4                 OVERNIGHTS RAW HOMES VALUE                   
VPHHOMES DS    XL4                 WEEKLY HOMES VALUE                           
VPHFACT  DS    XL4                 WEEKLY HOMES DBFACTOR - SET IN HOOK          
VPWKSTPT DS    C                   NUM OF WKS COMING FROM TPT                   
MASK51   DS    C                   BIT MASK FOR 51 ELEMENT - VPH                
MASK53   DS    C                   BIT MASK FOR 51 ELEMENT - VPH                
CBLDEMOV DS    C                   CABLE DEMO OVRD (FROM SPGETDEMD)             
VPNWKS   DS    F                   NUM OF WKS COMING FROM WEEKLY                
ELEM53   DS    F                   A(UNIVERSE ELEMENT)                          
END51    DS    F                   END OF WEEKLY ELEMENT                        
HOMUNIV  DS    F                   HOMES UNIVERSE                               
*                                                                               
DTDEMSA  DS    XL(MAXDEMS*4)       ADJ DEMO TOTALS FOR DAY/TIME                 
DTDEMSU  DS    XL(MAXDEMS*4)       UNA DEMO TOTALS                              
ORBDEMSA DS    XL(MAXDEMS*4)       ADJ DEMO TOTALS FOR ORBIT                    
ORBDEMSU DS    XL(MAXDEMS*4)       UNA DEMO TOTALS                              
AFDEMSA  DS    XL(MAXDEMS*4)       ADJ DEMO TOTALS FOR AFFID                    
AFDEMSU  DS    XL(MAXDEMS*4)       UNA DEMO TOTALS                              
BUDEMSA  DS    XL(MAXDEMS*4)       ADJ DEMO TOTALS FOR BUY                      
BUDEMSU  DS    XL(MAXDEMS*4)       UNA DEMO TOTALS                              
         EJECT                                                                  
WKADCONS DS    0A                                                               
ADTUNVS  DC    A(DTUNVS-WORKD)                                                  
ADTDEMS  DC    A(DTDEMS-WORKD)                                                  
ASVIREC  DC    A(SVIREC-WORKD)                                                  
ADUMMY   DC    A(DUMMY-WORKD)                                                   
AIUNWK   DC    A(IUNWK-WORKD)                                                   
AIUNVS   DC    A(IUNVS-WORKD)                                                   
AIUNOLD  DC    A(IUNOLD-WORKD)                                                  
AIRTGOLD DC    A(IRTGOLD-WORKD)                                                 
AIPUTOLD DC    A(IPUTOLD-WORKD)                                                 
AIIMPOLD DC    A(IIMPOLD-WORKD)                                                 
AITOTOLD DC    A(ITOTOLD-WORKD)                                                 
AIUNOLDX DC    A(IUNOLDX-WORKD)                                                 
AIUNNEW  DC    A(IUNNEW-WORKD)                                                  
AIRTGNEW DC    A(IRTGNEW-WORKD)                                                 
AIPUTNEW DC    A(IPUTNEW-WORKD)                                                 
AIIMPNEW DC    A(IIMPNEW-WORKD)                                                 
AITOTNEW DC    A(ITOTNEW-WORKD)                                                 
AIUNNEWX DC    A(IUNNEWX-WORKD)                                                 
AIUNXTRA DC    A(IUNXTRA-WORKD)                                                 
ADBLOCKS DC    A(DBLOCKS-WORKD)                                                 
ARADEMSA DC    A(RADEMSA-WORKD)                                                 
ARADEMSU DC    A(RADEMSU-WORKD)                                                 
ADEMLST2 DC    A(DEMOLST2-WORKD)                                                
AGDEXTR1 DC    A(GDEXTRA1-WORKD)                                                
AGDEXTR2 DC    A(GDEXTRA2-WORKD)                                                
AAFDLST  DC    A(AFDLIST-WORKD)                                                 
AAFDLSTX DC    A(AFDLISTX-WORKD)                                                
ASPDTTAB DC    A(SPDTTAB-WORKD)                                                 
AGDEXSPD DC    A(GDEXSPDT-WORKD)                                                
ACOMEXT  DC    A(COMEXT-WORKD)                                                  
WKADCONX EQU   *                                                                
*                                                                               
VSTAPACK DS    A                                                                
VCLUNPK  DS    A                                                                
VNETWEEK DS    A                                                                
VDEFINE  DS    A                                                                
VGETIUN  DS    A                                                                
VCOMINTR DS    A                                                                
WTDEMA   DS    F                                                                
WTDEMU   DS    F                                                                
VBKTYPTB DS    A                   A(BOOKTYPE TABLE) IN DEMTABS                 
BKTYPTBL DS    H                   L'BOOKTYPE TABLE ENTRY                       
NUMSPOTS DS    H                                                                
SVNUMMKT DS    H                                                                
         EJECT                                                                  
TAPEOPT  DS    CL1                 OPTION FOR TAPE PRECISION                    
SVTPOPT  DS    CL1                 SAVE OPTION FOR TAPE PRECISION               
SVMED    DS    C                                                                
TRUELPMF DS    C                                                                
STABKMKT DS    XL2                 NSI MKT NUMBER FOR STATION/BOOK              
SVSELSTA DS    CL(L'DBSELSTA)                                                   
SVSTPSTA DS    CL(L'DBSELSTA)      SAVE STAPACK STATION                         
DSTACALL DS    CL(L'DBSELSTA)                                                   
SVSELAGY DS    CL(L'DBSELAGY)                                                   
SVDBFUNC DS    X                                                                
SVDBTYPE DS    X                                                                
SVAFFMED DS    C                                                                
SVSELDAY DS    C                                                                
SVDBBEST DS    CL(L'DBBEST)                                                     
SVYEAR   DS    X                                                                
SVLPMWKOPT DS  CL(L'LPMWKOPT)                                                   
SVWVPHOPT  DS  CL(L'WVPHOPT)                                                    
AFFIDLK  DS    C                                                                
DSTASPLT DS    C                   INDICATOR FOR DSTATION SPLIT WEEK            
SPOTDAY  DS    CL3                                                              
BISPTDAY DS    X                                                                
ROTDAY   DS    CL3                                                              
MKTTYPE  DS    X                                                                
MKTLPM   EQU   X'80'               LPM MARKET                                   
MKTSETM  EQU   X'40'               SET METERED                                  
MKTDIARY EQU   X'20'               DIARY MKT                                    
*&&DO                                                                           
* TAKEN OUT IT SEEMS LIKE WE DONT NEED THIS CODE FOR DARK 03/20/2007            
DARKFLAG DS    X                   LPM DARK WEEK FLAG                           
DARKONLY EQU   X'80'                                                            
NOTDARK  EQU   X'00'                                                            
*&&                                                                             
DMINDEX   DS    C                   DEMO INDEX FLAG                             
********************************************************************            
* DO NOT SEPERATE THESE  OPTIONS IN THIS BOX                       *            
LPMWKOPT DS    CL1                 LPMWK OPTION                    *            
WKOVOPT  DS    0CL2                DO NOT SEPERATE !!              *            
WVPHOPT  DS    CL1                 OPTION FOR WEEKLY VPH ADJUST    *            
OVPHOPT  DS    CL1                 OPTION FOR OVERNIGHTS VPH ADJUST*            
********************************************************************            
SMMMONTH DS    C                   NEW OVN OPTION- SET METER TO MONTHLY         
DIGITALF DS    C                   Y/N  DIGITAL TRANSITION FLAG                 
DEMANDSW DS    XL1                 NON-ZERO IF DEMAND HOOK CALLED               
TWODEC   DS    XL1                 X'40'=2-DEC RTG,X'01'=2-DEC IMPS             
ELCODE   DS    XL1                                                              
LPMDTB   DS    XL3                 LPM 3-BYTE START DATE                        
LPMDTP   DS    XL2                 LPM 2-BYTE START DATE                        
LPMDRKWK DS    XL2                 LPM DARKWEEK                                 
SVWTPDQH DS    XL2                 SAVE WTO 1 BYTE DAY/ 1 BYTE QH               
DAYREAD  DS    X                   BITS (0,MON,TUE,WED,THU,FRI,SAT,SUN)         
BKTYPIND DS    X                   BOOKTYPE INDICATOR                           
OVEFFBK  DS    XL2                 BOOKTYPE EFFECTIVE BOOK- OVERNIGHTS          
ESTEOW   DS    C                   ESTIMATE EOWDAY                              
MISSDAY  DS    X                                                                
MISCOUNT DS    C                   MISSING DATA COUNT                           
ADJDATE  DS    D                                                                
COMERRF  DS    X                                                                
*                                                                               
QHRDARK  DS    C                                                                
NEXTYRLK DS    C                   Y/N FORCE NEXT YEAR WKLY LOOKUP?             
OOWRFLG  DS    C                   OUT OF WEEK ROTATION LOOKUP                  
LASTYRLK DS    C                   Y/N FORCE LAST YEAR WKLY LOOKUP?             
WKINFLAG DS    C                   FLAG TO INDICATE IF WK IS IN YET             
GDMKCDEM DS    C                   CABLE DEMO RTG SERVICE OVERRIDE              
GDMKALPH DS    CL3                 MARKET ALPHA                                 
CBLKUP   DS    XL2                                                              
SVBSTART DS    XL3                 BUY DESC START                               
SVBEND   DS    XL3                 BUY DESC END                                 
SVSELBK  DS    XL2                 SAVE AREA DBSELBK                            
VPHSDAT  DS    XL2                 START DATE FOR VPHOMES                       
VPHEDAT  DS    XL2                 END DATE FOR VPHHOMES                        
VPHSWK   DS    XL2                 START WEEK FOR VPHOMES                       
VPHEWK   DS    XL2                 END WEEK FOR VPHHOMES                        
SVISW    DS    XL1                 FIRST TIME SWITCH FOR SVI SEARCH             
SVADJHUT DS    XL1                 SVI FILE CODE (IF SVISW=Y)                   
SHRSW    DS    XL1                 INDICATES SHARE COMP REQUIRED (TBC)          
SVACTBK  DS    XL2                 ACTUAL BOOK (FUCKED UP BY DEMOUT)            
SVISQH   DS    XL1                 ACTUAL SVI QH FOR CURRENT DEMO REC           
SVIEQH   DS    XL1                 END SVI QH FOR CURRENT DEMO REC              
SVIDAY   DS    XL1                 SVIFILE DAY FOR CURRENT DEMO REC             
XQHNUM   DS    XL1                 USED FOR XQH QTR HR COUNT                    
WKLYIND  DS    XL1                 X'80'=WEEKLY DATA,X'40'=UNWEEKLY             
WKLYOPT  DS    XL1                 X'80'=WEEKLY REQ, X'40'=MISSING DATA         
PBDEMSW  DS    CL1                 C'Y'=POST BUY DEMOS EXIST                    
PBLKERR  DS    CL1                 C'Y'=LOOKUP ERROR                            
STNAME   DS    CL16                START QH PROGRAM NAME                        
ENDNAME  DS    CL16                END QH PROGRAM NAME                          
STNAMEOV DS    CL16                START QH PROGRAM NAME OVERNIGHTS             
ENDNAMEO DS    CL16                END QH PROGRAM NAME OVERNIGHTS               
NEWNAME  DS    CL16                NEXT PROGRAM FOR XQH LOOKUP                  
SV1WPROF DS    CL16                WEEKLY DATA PROFILE                          
         ORG   SV1WPROF                                                         
SV1WPR1  DS    CL1                 WEEKLY DATA FOR ALL BUYS (Y/N)               
SV1WPR2  DS    CL1                 WEEKLY DATA FOR -S BUYS  (Y/N)               
SV1WPR3  DS    CL1                 WEEKLY DATA IF PROGRAM STARTS WITH X         
SV1WPR4  DS    CL1                 ANGLO/FRANCO FOR CANADA                      
SV1WPR5  DS    CL1                 NO WEEKLY DATA IF PGM STARTS WITH X          
SV1WPR6  DS    CL1                 DMA=I/R                                      
SV1WPR7  DS    CL1                 BUY/POST RADIO DEMOS                         
SV1WPR8  DS    CL1                 NORMALIZE HPT                                
SV1WPR9  DS    CL1                 CANADIAN METERED MARKET TREATMENT            
SV1WPR10 DS    CL1                 USE BBM METERED MRKT IF AVAILABLE            
         ORG                                                                    
SVD0PROF DS    CL16                SPD0 PROFILE                                 
*                                                                               
ASVIMOD  DS    A                   A(SVI MODIFIER TABLE)                        
ASVIDEM  DS    A                   A(SVI DEMO TABLE)                            
SVIWGTS  DS    XL24                MONTHLY SVI WEIGHTS                          
SVIWGTSM DS    F                   SUM OF SVIWGTS                               
SVREQDTS DS    XL4                                                              
SVBUYDTS DS    XL6                                                              
*                                                                               
SQHVAL   DS    F                                                                
EQHVAL   DS    F                                                                
SQHNUM   DS    X                                                                
EQHNUM   DS    X                                                                
LQHLIST  DS    XL4                                                              
*                                                                               
INPERIOD DS    X                                                                
COMERROR DS    X                   COMSCORE LOOKUP ERROR FLAG                   
AFDSW    DS    XL1                                                              
AFDOPTS  DS    XL1                 X'80' = AFD 7 MIN OPT                        
AFDPRD   DS    XL1                                                              
AFDSLN   DS    XL1                                                              
AFDSDT   DS    XL2                                                              
AFDEDT   DS    XL2                                                              
AF7QHST  DS    H                   7 MIN OPT - ST TIME OF LAST QH - 2           
AF7QHEND DS    H                   7 MIN OPT - END TIME OF LAST QH + 2          
AF7QHSET DS    H                   7 MIN OPT - TIME TO SET AFFID TO             
NOAFDCNT DS    H                   COUNT OF MISSING AFFIDS                      
AFDCNT   DS    H                   COUNT OF AFDLIST ENTRIES                     
AFDNEXT  DS    A                   A(NEXT AFDLIST ENTRY)                        
*                                                                               
FLAGS    DS    X                   VARIOUS FLAGS                                
CANHPT   EQU   X'80'               CANADIAN HUTS, PUTS, & TOTS                  
*        EQU   X'40'               UNUSED                                       
*        EQU   X'20'               UNUSED                                       
*        EQU   X'10'               UNUSED                                       
*        EQU   X'08'               UNUSED                                       
*        EQU   X'04'               UNUSED                                       
*        EQU   X'02'               UNUSED                                       
*        EQU   X'01'               UNUSED                                       
*                                                                               
PREVSPDT DS    XL2                 PREVIOUS SPOT DATE (FOR DUPS)                
ELCDLO   DS    XL1                 ELEMENT CODE LOW                             
ELCDHI   DS    XL1                 ELEMENT CODE HIGH                            
*                                                                               
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         DS    CL80               CHANGED FROM 8 -80 - SLOPPED OVER             
DBLOCKX  DS    CL128               EXCESS FOR DBLOCK                            
         EJECT                                                                  
       ++INCLUDE DEDEMEQUS                                                      
         EJECT                                                                  
****************** I U N   R E C O R D *****************                        
                                                                                
IUNTYPE  DS    C                   1=RTG/IMP 4=RTG/IMP/PUT/TOT                  
IUNDEMS  EQU   32                  UNVS/RTGS/IMPS/PUTS/TOTS (EACH)              
IUNHMDSP EQU   80                  DSPL TO HMS RTGS IN IUN RTGS AREA            
*                                                                               
IUNWK    DS    0F                                                               
*                                                                               
IUNVS    DS    (IUNDEMS)F                                                       
IUNVX    EQU   *                                                                
*                                                                               
IUNOLD   EQU   *                                                                
IRTGOLD  DS    (IUNDEMS)F                                                       
IIMPOLD  DS    (IUNDEMS)F                                                       
IPUTOLD  DS    (IUNDEMS)F                                                       
ITOTOLD  DS    (IUNDEMS)F                                                       
IUNOLDX  EQU   *                                                                
*                                                                               
IUNNEW   EQU   *                                                                
IRTGNEW  DS    (IUNDEMS)F                                                       
IIMPNEW  DS    (IUNDEMS)F                                                       
IPUTNEW  DS    (IUNDEMS)F                                                       
ITOTNEW  DS    (IUNDEMS)F                                                       
IUNNEWX  EQU   *                                                                
*                                                                               
IUNXTRA  EQU   *                                                                
IUNSHMS  DS    F                                                                
IUNSMETA DS    F                                                                
IUNSMETB DS    F                                                                
*                                                                               
ILUNVS   DS    (IUNDEMS)F          LOONEYVERSES                                 
ILUNVX   EQU   *                                                                
                                                                                
****************    DEMO LOOKUP AREAS ************                              
                                                                                
         DS    0F                                                               
DTUNVS   DS    (IUNDEMS*4)X        UNIVERSES                                    
*                                                                               
DTDEMS   DS    0F                  ACCUMULATORS FOR DEMAND CALL                 
DTRTGS   DS    (IUNDEMS*4)X        RATINGS                                      
DTIMPS   DS    (IUNDEMS*4)X        IMPRESSIONS                                  
DTPUTS   DS    (IUNDEMS*4)X        PUTS                                         
DTTOTS   DS    (IUNDEMS*4)X        TOTALS                                       
DTXTRA   DS    7F                  SHARES(3)                                    
DTDEMX   EQU   *                                                                
         ORG                                                                    
DBLOCKS  DS    XL256               DBLOCK SAVE AREA                             
         DS    CL20                                                             
COMEXT   DS    CL(DBCMINDX-DBCMINTD)                                            
         ORG                                                                    
         EJECT                                                                  
         DS    0D                                                               
GDEXTRA1 DS    XL64                FOR USERID                                   
GDEXTRA2 DS    CL128               RADIO EXTENDED DBLOCK AREA                   
GDEXTRA3 DS    CL128                                                            
GDEXSPDT DS    CL16                SPOT POSTING DATES                           
* 2 BYTE SPOT DATE , 1 BYTE DUPLICATION FACTOR                                  
SPDTTAB  DS    300XL3              TABLE OF SPOT DATES                          
*                                                                               
AFDLIST  DS    300XL5              AFFID DAY(1)/TIME(2)/DATE(2)                 
AFDLISTX EQU   *-1                                                              
*                                                                               
DUMMY    DS    1024C               MAXIMUM SVI RECORD SIZE                      
DUMMY2   DS     500C               DOUBLE WORD USE FOR CANADA -BPOO             
         ORG                                                                    
*                                                                               
** THERE WASN'T ENOUGH ROOM TO PUT THIS WITH THE OTHERS **                      
*                                                                               
RADEMSA  DS    XL((MAXDEMS+8)*4)   ADJ DEMO TOTALS FOR RADIO                    
RADEMSU  DS    XL((MAXDEMS+8)*4)   UNA DEMO TOTALS                              
DEMOLST2 DS    XL((MAXDEMS+8)*3)   DEMOLIST FOR RADIO                           
* THE AREA BELOW IS USED FOR READING SVI RECORD NO MATTER HOW MUCH              
* IT LOOKS LIKE SVIREC IS THE IOAREA                                            
         ORG                                                                    
         DS    XL1024                                                           
         ORG   *-1024                                                           
       ++INCLUDE DESVIREC                                                       
         ORG                                                                    
WORKX    EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
*======================== MISCELLANEOUS DSECTS =======================*         
*                                                                               
*-------------------------- RADIO DEMO LIST --------------------------*         
RDEMLSTD DSECT                                                                  
RDLDMOD  DS    CL1                                                              
RDLDNUM  DS    XL1                                                              
RDLDMOD1 DS    CL1                                                              
RDLDMOD2 DS    CL1                                                              
RDLOPER  DS    CL1                                                              
RDEMLSTQ EQU   *-RDEMLSTD                                                       
         DS    0CL(5-RDEMLSTQ+1)                                                
***********************************************************************         
         EJECT                                                                  
       ++INCLUDE SPGETDEMD                                                      
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
COMFACSD DSECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DEDBEXTRAD                                                     
       ++INCLUDE DDMONYREQU                                                     
                                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'082SPGETDEME 03/04/21'                                      
         END                                                                    
