*          DATA SET DEDEM08    AT LEVEL 030 AS OF 05/13/20                      
*PHASE T21B08C,*                                                                
         TITLE 'DEDEM08 - $DEM AFFID LOOK-UP'                                   
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                               
* -------- --- ----   ------------------------------------------------*         
* MAY15/09 026 BPOO - FIX WTP SAT/SUN                                           
* MAY15/02 019 BPOO - FIX OLYMPIC EXCLUSION                           *         
* NOV04/00 017 BPOO - SUPPORT USERID AUTHORIZATION                    *         
* NOV02/00 016 BPOO - MOVED DEM TABLES TO DEM81 PHASE                 *         
* SEP22/00 015 BPOO - falink stuff...change screen one line lower     *         
* Mar08/00 014 GLEE - Re-linked for bigger buy record support         *         
*                                                                     *         
* Nov16/95 013 GLEE - Prepatory stage for STEREO support              *         
*                                                                     *         
* 02/14/95 012 GLEE - support alpha market input                                
*                                                                               
* 10/19/94 011 GLEE - support overnight (1a-5a) daypart radio posting           
*                                                                               
* 10/12/94 010 GLEE - Pass DMA option for precision specification               
*                                                                               
* 07/21/94 008 GLEE - Pass spill mkt# in case of radio lookups  (GLEE)*         
*                                                                               
* 08/10/94 009 ZEN    Support book type affid lookups                           
*                                                                               
*  ??????   ?   ??  - HISTORY UNKNOWN                                           
***********************************************************************         
         EJECT                                                                  
DEM08    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**DEM8**                                                       
         USING DEMWRKD,R9          R9=A(GLOBAL W/S)                             
         USING DEMTWAD,R8          R8=A(TWA)                                    
         L     R7,AAPWORK                                                       
         USING DEMTMPD,R7          R7=A(TEMP W/S)                               
*                                  HANDLE CONTROLLER MODE SETTINGS              
         CLI   APMODE,FORMHEAD     FORMAT HEADLINES & INITIALISE                
         BE    DEMHEAD                                                          
         CLI   APMODE,PROCESS      READ & POST RECORDS TO BUFFER                
         BE    DEMPROC                                                          
         CLI   APMODE,FORMLINE     FORMAT & PRINT A BUFFER RECORD               
         BE    DEMLINE                                                          
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* FORMAT HEADLINES & INITIALIZE FOR DEMO LOOK-UPS                               
*                                                                               
DEMHEAD  ZIC   R1,NDEMS                                                         
         LA    RE,DEMWIDTH                                                      
         MR    R0,RE                                                            
         BCTR  R1,0                                                             
         STH   R1,HEADLEN          HEADLEN=L'STATION HEADING                    
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         STH   R1,EXECLEN          EXECLEN=L'STATION HEADING-2                  
         LH    R1,HEADLEN                                                       
         LA    R0,L'BUYSTA                                                      
         SR    R1,R0                                                            
         SRL   R1,1                                                             
         STH   R1,DUB              STADISP=DISP. TO STATION IN HEADING          
*                                                                               
         LA    R2,DEMHD1+31        R2=A(START OF STATION HEADINGS)              
         LR    R0,R2               CALCULATE DATA DISPLACEMENT                  
         LA    R1,DEMHD1                                                        
         SR    R0,R1                                                            
         STH   R0,DATADISP         SET DATA DISPLACEMENT                        
         MVI   0(R2),C'-'                                                       
         LH    R1,EXECLEN                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),0(R2)       FILL HEADLINE WITH DASHES                    
         LR    R1,R2                                                            
         AH    R1,DUB              CENTRALIZE STATION IN HEADING                
         MVC   0(L'BUYSTA,R1),BUYSTA                                            
         CLI   4(R1),C'T'          TEST PARENT STATION                          
         BNE   *+8                                                              
         MVI   4(R1),C'-'          YES DON'T SHOW THE 'T'                       
         CLI   3(R1),C' '          TEST 3 BYTE STATION NAME                     
         BNE   *+8                                                              
         MVI   3(R1),C'-'                                                       
*                                  MOVE DEMO NAMES UNDER HEADING                
         LA    RE,L'DEMHD1H+L'DEMHD1(R2)                                        
         LA    RF,L'DEMHD1H+L'DEMHD1(RE)                                        
         LA    R1,DEMOUT75                                                      
         ZIC   R0,NDEMS                                                         
DEMHEAD2 MVC   0(7,RE),0(R1)                                                    
         MVC   1(5,RF),7(R1)                                                    
         LA    RE,DEMWIDTH(RE)                                                  
         LA    RF,DEMWIDTH(RF)                                                  
         LA    R1,L'DEMOUT75(R1)                                                
         BCT   R0,DEMHEAD2                                                      
*                                                                               
         MVC   DEMHD2(L'HEADDAY1),HEADDAY1                                      
         MVC   DEMHD3(L'HEADDAY2),HEADDAY2                                      
*                                                                               
         LA    RE,BINKEYL                                                       
         ST    RE,BINLKEY          SET L'BINSRCH TABLE KEY                      
         ZIC   R1,NDEMS                                                         
         LA    R0,L'BINDEMS                                                     
         MR    R0,R0               R1=L'DEMO ACCUMULATORS                       
         LA    R1,L'BINPROG(R1,RE)                                              
         ST    R1,BINLREC          SET TOTAL BINSRCH RECORD LENGTH              
*                                                                               
         XC    WORK,WORK           SET UP 1W PROFILE KEY                        
         MVC   WORK+0(4),=C'S01W'                                               
         MVC   WORK+4(2),AGYALPH                                                
         MVI   WORK+6,C'T'                                                      
         MVC   WORK+7(3),BUYCLT                                                 
         CLI   BUYCLTO,X'41'                                                    
         BL    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),BUYCLTO                                               
         GOTO1 VGETPROF,DMCB,WORK,BUY1WEEK,VDATAMGR                             
                                                                                
         CLI   OPTDMA,0              WAS PRECISION SPECIFIED?                   
         BE    *+10                                                             
         MVC   BUY1WEEK+5(1),OPTDMA   YES, PASS IT TO THE PROFILE               
*                                                                               
         MVC   WORK+2(2),=C'D0'    GET D0 PROFILE                               
         GOTO1 (RF),(R1),WORK,D0PROF,VDATAMGR                                   
*                                                                               
DEMHEADX B     EXIT                                                             
         EJECT                                                                  
* READ DEMO FILES VIA T00A21 & POST VALUES TO TABLE                             
*                                                                               
DEMPROC  BAS   RE,PBDEMOS          LOOK FOR POST BUY DEMO OVERRIDES             
         LA    R6,DBLOCK1          INITIALIZE DEMLOOK BLOCK                     
         USING SPDEMLKD,R6         R6=A(DEMLOOK BLOCK)                          
         XC    SPDEMLK(SPDEMLKL),SPDEMLK                                        
         MVC   SPLKAREC,AIOAREA1                                                
         MVC   SPLKAFAC,AFAC                                                    
         LA    R0,DEMS                                                          
         ST    R0,SPLKALST                                                      
         LA    R0,THISDEMS                                                      
         ST    R0,SPLKAVAL                                                      
         LA    R0,DEMLKHK                                                       
         ST    R0,SPLKHOOK                                                      
         L     R0,AEBREC                                                        
         ST    R0,SPLKABUY                                                      
         LA    R0,BUY1WEEK                                                      
         ST    R0,SPLKA1W                                                       
         MVI   SPLKOPT,SPLKOAFD                                                 
         CLI   D0PROF+4,C'Y'       LOWER QH FEATURE                             
         BNE   *+8                                                              
         OI    SPLKOPT,SPLKOLOW                                                 
         MVC   SPLKFIL,DBFIL                                                    
         MVC   SPLKMED,DBMED                                                    
                                                                                
         CLI   DBMED,C'R'          IF RADIO,                                    
         BNE   *+8                                                              
         MVI   SPLKBEST,C'P'        SUPPORT OVERNIGHT DAYPART POSTING           
                                                                                
         CLI   BKS+3,C'O'          TURN ON OLYMPIC EXLUSIONS                    
         BNE   *+8                                                              
         OI    SPLKOPT,SPLKOEXO                                                 
                                                                                
         MVC   SPLKSRC,DBSRC                                                    
         MVC   SPLKAGY,AGYALPH                                                  
         MVC   SPLKCLI,BUYCLT      SET CLIENT FOR OVERRIDES                     
         MVC   SPLKUMK,BUYSTM      SET AGENCY MARKET FOR OVR'S                  
         MVC   SPLKDBK,BKS                                                      
         MVC   SPLKBTYP,BKS+3                                                   
         MVC   SPLKSTA,BUYSTA                                                   
         MVC   SPLKSPL,DBSTABK     SPILL MKT# PUT IN DBSTABK BY T21B00          
******   MVC   SPLKSVI,OPTSVIS                                                  
******   CLI   SPLKSVI,0           TEST FOR SVI FACTOR                          
******   BNE   *+8                 YES                                          
         MVI   SPLKSVI,X'FF'       NO-SUPPRESS SVI LOOKUP                       
         MVC   SPLKAFPR,BUYPRDN                                                 
         MVC   SPLKAFST,OPTSDAT                                                 
         OC    SPLKAFND,OPTEDAT                                                 
         BNZ   *+10                                                             
         MVC   SPLKAFND,=X'FFFF'                                                
         MVC   SPLKAFLN,OPTSECS                                                 
                                                                                
         CLI   OPTSVIS,C'Y'                                                     
         BNE   *+8                                                              
         OI    SPLKOPT,SPLKOWTP       READ WTP                                  
* ALLOW DEC=2 FOR AFFIDS USTV AND FUSION                                        
         CLI   OPTDEC,C'2'                                                      
         BNE   DP15                                                             
         CLI   DBMED,C'C'                                                       
         BNE   *+12                                                             
         CLI   DBSRC,C'A'                                                       
         BE    DP14                                                             
         CLI   DBMED,C'T'                                                       
         BE    *+8                                                              
         CLI   DBSRC,C'F'                                                       
         BNE   DP15                                                             
DP14     OI    SPLKOPT,SPLKOP2D                                                 
DP15     DS    0C                                                               
*                                                                               
         OC    SPLKSPL,SPLKSPL        IF NO NUMERIC SPILL MARKET,               
         BNZ   DP20                                                             
         OC    ALFMKTS,ALFMKTS         AND THERE WAS ALPHA MKT INPUT,           
         BZ    DP20                                                             
         MVC   SPLKALF,ALFMKTS         USE IT                                   
                                                                                
DP20     DS    0H                                                               
***  MOVE SYSCODE FOR THIS STATION INTO AN AREA FOR SPDEMLK EXTENSION           
         XC    SYSCEXT,SYSCEXT                                                  
         LA    RE,SYSCEXT                                                       
         USING SPLKXTD,RE                                                       
         XC    SPLKXTND,SPLKXTND   DEFAULT NO EXTENSION                         
         XC    SPXTSYSC,SPXTSYSC                                                
         OC    SYSCODES,SYSCODES                                                
         BZ    *+10                                                             
         MVC   SPXTSYSC,SYSCODES                                                
         OI    SPXTFLAG,SPXTRAPP  SET CALLING RESEARCH APPL FLAG                
* SET NEW FLAG TO INDICATE CALLER IS SPOT DESKTOP DEMO ENGINE                   
* WE ARE SETTING THIS FLAG SO SPGETDEMF WILL TRANSLATE THE SPOT CALL            
* LETTERS TO NSI CALL LETTERS VIA STAPACK CALL.                                 
*                                                                               
         MVI   SPXTFLG2,SPXTSDEM                                                
* SET SPXTHEAD SO SPGETDEMF CAN CALL STAPACK TO LINK SPOT CABLE                 
* CALL LETTERS TO NSI CALL LETTERS                                              
         XC    SPXTHEAD,SPXTHEAD                                                
         OC    SPXTSYSC,SPXTSYSC                                                
***      JZ    DP30                                                             
         JZ    DP28                                                             
         ZICM  R0,SPXTSYSC,(3)                                                  
         CVD   R0,DUB                                                           
         UNPK  SPXTHEAD(4),DUB                                                  
         OI    SPXTHEAD+3,X'F0'                                                 
*                                                                               
*                                                                               
DP28     ST    RE,SPLKXTND                                                      
         DROP  RE                                                               
DP30     MVC   SPLKUID,USERID                                                   
         GOTO1 VSPDEMLK,DMCB,(X'FF',SPDEMLKD)                                   
*                                                                               
         OC    SPLKTIM(2),SPLKTIM                                               
         BZ    EXIT                                                             
         MVC   THISDATE,=X'EFFF'   SET BUY TOTAL LINE                           
         MVC   THISTIME,SPLKTIM                                                 
*                                                                               
         CLI   PBDEMSW,C'Y'        TEST ANY POST BUY DEMO OVERRIDES             
         BNE   DEMP6               NO                                           
         ZIC   R0,NDEMS            YES-GET OVERRIDE TOTALS                      
         LA    R4,PBDEMVAL                                                      
         MVC   FULL(2),THISTIME    FULL(2)=TOTAL NUMBER OF SPOTS                
*                                                                               
DEMP2    TM    0(R4),X'80'         TEST THIS DEMO OVERRIDDEN                    
         BZ    DEMP4               NO                                           
         L     R1,0(R4)            YES-MULTIPLY BY N'SPOTS                      
         SLL   R1,1                                                             
         SRL   R1,9                                                             
         MH    R1,FULL                                                          
         STCM  R1,7,0(R4)                                                       
         OI    0(R4),X'80'                                                      
DEMP4    LA    R4,3(R4)                                                         
         BCT   R0,DEMP2                                                         
*                                                                               
DEMP6    BAS   RE,DEMPOST                                                       
         B     EXIT                                                             
         EJECT                                                                  
DEMLKHK  ST    RE,SAVERE           SAVE RETURN ADDRESS                          
         MVC   THISDATE,SPLKADAT                                                
         CLI   THISDATE,X'FF'      TEST NON-AFFIDAVIT SPOTS                     
         BNE   *+8                                                              
         MVI   THISDATE,X'8F'                                                   
         MVC   THISTIME,SPLKTIM                                                 
         MVC   THISPROG,SPLKPRG    SET PROG NAME & DEMO VALUES                  
         BAS   RE,DEMPOST                                                       
         L     RE,SAVERE           EXIT BACK TO SPDEMLK FRO NEXT RECORD         
         BR    RE                                                               
         EJECT                                                                  
* BUILD BINSRCH RECORD & CALL ROOT POST ROUTINE                                 
*                                                                               
DEMPOST  NTR1  ,                                                                
         LA    RF,DROPSVIS         DROP SVIS FROM THISDEMS                      
         CLI   OPTDTYP,C'S'        TEST TO DISPLAY SVI INDICES                  
         BNE   *+8                                                              
         LA    RF,DROPDEMS         YES-DROP THE DEMOS INSTEAD                   
         BASR  RE,RF                                                            
         LA    R1,POSTLINE                                                      
         USING BINRECD,R1                                                       
         MVC   BINKEY(BINKEYL),THISKEY                                          
         MVC   BINPROG,THISPROG                                                 
         LA    R4,THISDEMS         R4=A(DEMO VALUES)                            
         LA    RE,BINDEMS                                                       
         ZIC   R0,NDEMS                                                         
*                                                                               
DEMPOST2 MVC   0(3,RE),1(R4)                                                    
         LA    RE,3(RE)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,DEMPOST2                                                      
*                                                                               
         CLI   PBDEMSW,C'Y'        TEST ANY POST BUY DEMO OVERRIDES             
         BNE   DEMPOST8                                                         
         LA    R4,PBDEMVAL         YES-MOVE THEM IN                             
         LA    RE,BINDEMS                                                       
         ZIC   RF,NDEMS                                                         
*                                                                               
DEMPOST4 TM    0(R4),X'80'         TEST THIS DEMO IS OVERRIDDEN                 
         BZ    DEMPOST6                                                         
         CLI   OPTDTYP,C'S'        YES-TEST TO DISPLAY SVI INDICES              
         BNE   *+12                                                             
         MVI   2(R1),100           YES-FORCE SVI=100                            
         B     DEMPOST6                                                         
*                                                                               
         STM   R0,R1,DUB                                                        
         SR    R1,R1                                                            
         ICM   R1,7,0(R4)          GET DEMO OVERRIDE VALUE                      
         N     R1,=X'003FFFFF'     DROP FLAGS                                   
         TM    0(R4),X'40'         TEST 2-DEC                                   
         BZ    DEMPOS4X                                                         
         M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
DEMPOS4X STCM  R1,7,0(RE)          SAVE OVERRIDE VALUE                          
         LM    R0,R1,DUB                                                        
*                                                                               
DEMPOST6 LA    R4,3(R4)            NEXT DEMO                                    
         LA    RE,3(RE)                                                         
         BCT   RF,DEMPOST4                                                      
*                                                                               
DEMPOST8 GOTO1 APOST               GO ADD RECORD TO BINSRCH BUFFER              
         B     EXIT                                                             
         DROP  R1                                                               
         SPACE 1                                                                
* REMOVE SVI FACTORS FROM OUTPUT VALUES RETURNED FROM SPDEMLK                   
*                                                                               
DROPSVIS LA    R1,THISDEMS         R1=A(RETURNED DEMO VALUES)                   
         LA    RF,8(R1)            RF=A(2ND DEMO VALUE ENTRY)                   
         ZIC   R0,NDEMS            R0=N'OUTPUT ENTRIES                          
         MVC   4(4,R1),0(RF)                                                    
         LA    R1,4(R1)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,*-14                                                          
         BR    RE                                                               
         SPACE 1                                                                
* REMOVE DEMOS FROM OUTPUT VALUES RETURNED FROM SPDEMLK                         
*                                                                               
DROPDEMS LA    R1,THISDEMS         R1=A(RETURNED DEMO VALUES)                   
         LA    RF,4(R1)            RF=A(FIRST SVI VALUE)                        
         ZIC   R0,NDEMS            R0=N'OUTPUT VALUES                           
         MVC   0(4,R1),0(RF)       SHIFT SVI TO DEMO POSITION                   
         LA    R1,4(R1)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,*-14                                                          
         BR    RE                                                               
         EJECT                                                                  
* FORMAT PRINT LINES                                                            
*                                                                               
DEMLINE  L     R5,BINAREC                                                       
         USING BINRECD,R5          R5=A(RECORD)                                 
         CLI   BINDATE,X'FF'       TEST FOR E-O-F RECORD                        
         BE    EXIT                                                             
         MVI   DAYTIME,C' '                                                     
         MVC   DAYTIME+1(L'DAYTIME-1),DAYTIME                                   
*                                                                               
***      CLI   BINDATE,X'EF'       HANDLE BUY LINE TOTAL                        
         CLC   BINDATE,=X'EFFF'    HANDLE BUY LINE TOTAL                        
         BNE   DEMLINE2                                                         
         MVC   DAYTIME(14),=C'*** BUY TOTALS'                                   
         SR    R0,R0                                                            
         ICM   R0,3,BINTIME                                                     
         LA    R2,DAYTIME+15                                                    
         EDIT  (R0),(4,0(R2)),ALIGN=LEFT                                        
         AR    R2,R0                                                            
         MVC   1(5,R2),=C'SPOTS'                                                
         B     DEMLINE6                                                         
*                                                                               
DEMLINE2 CLI   BINDATE,X'8F'       HANDLE NON-AFFID LINE                        
         BNE   DEMLINE3                                                         
         MVC   DAYTIME(3),=C'***'                                               
         SR    R0,R0                                                            
         ICM   R0,3,BINTIME                                                     
         LA    R2,DAYTIME+4                                                     
         EDIT  (R0),(4,0(R2)),ALIGN=LEFT                                        
         AR    R2,R0                                                            
         MVC   1(5,R2),=C'NOAFD'                                                
         B     DEMLINE5                                                         
*                                  EDIT DEMO VALUES                             
DEMLINE3 GOTO1 VDATCON,DMCB,(2,BINDATE),(0,WORK)                                
         GOTO1 VGETDAY,DMCB,WORK,DAYTIME                                        
         GOTO1 VDATCON,DMCB,(2,BINDATE),(4,DAYTIME+4)                           
*                                  MILITARY TIME FORMAT                         
         SR    R0,R0                                                            
         ICM   R0,3,BINTIME                                                     
         CLI   OPTTIME,C'M'        CONVERT INPUT DAY VALUES                     
         BNE   DEMLINE4                                                         
         CVD   R0,DUB              EDIT START TIME                              
         OI    DUB+7,X'0F'                                                      
         UNPK  DAYTIME+10(4),DUB                                                
         B     DEMLINE5                                                         
*                                  STANDARD TIME FORMAT                         
DEMLINE4 SLL   R0,16                                                            
         ST    R0,DUB                                                           
         GOTO1 VUNTIME,DMCB,DUB,DAYTIME+10                                      
*                                                                               
DEMLINE5 MVC   DAYTIME+16(L'BINPROG),BINPROG                                    
*                                                                               
DEMLINE6 MVC   LINE1(30),DAYTIME                                                
         LA    R2,LINE1                                                         
         AH    R2,DATADISP         R2=A(EDITTED DEMOS)                          
         LA    R3,DEMS             R3=A(DEMO EXPRESSIONS)                       
         LA    R4,BINDEMS          R4=A(DEMO VALUES)                            
         ZIC   R0,NDEMS            R0=N'DEMOS                                   
*                                                                               
DEMLINE7 BAS   RE,GETDEM           EDIT DEMO VALUE                              
         LA    R2,DEMWIDTH(R2)     BUMP TO NEXT DEMO                            
         LA    R3,L'DEMS(R3)                                                    
         LA    R4,L'BINDEMS(R4)                                                 
         BCT   R0,DEMLINE7         DO FOR NUMBER OF DEMOS                       
         B     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO EDIT A DEMO VALUE                                                  
* NTRY - R3=A(3 BYTE DEMO EXPRESSION)                                           
*        R4=A(3 BYTE DEMO VALUE)                                                
*        R2=A(7 BYTE OUTPUT DEMO VALUE)                                         
*                                                                               
GETDEM   NTR1                                                                   
         SR    R0,R0                                                            
         ICM   R0,7,0(R4)          R0=DEMO VALUE                                
         CLI   OPTDTYP,C'S'        TEST TO OUTPUT SVI INDICES                   
         BE    GETDEM5             YES-EDIT INTEGER VALUE                       
         MVC   DUB(1),DBFIL                                                     
         MVC   DUB+1(1),1(R3)                                                   
         MVI   DUB+2,0                                                          
         LA    R1,EDITTAB          R1=A(EDIT TABLE)                             
* CHECK IF 2 DECIMAL OPTION                                                     
         CLI   OPTDEC,C'2'                                                      
         BNE   GETDEM2                                                          
         CLI   DBMED,C'C'                                                       
         BNE   *+12                                                             
         CLI   DBSRC,C'A'                                                       
         BE    GETDEM1                                                          
         CLI   DBMED,C'T'                                                       
         BE    *+8                                                              
         CLI   DBSRC,C'F'                                                       
         BNE   GETDEM2                                                          
GETDEM1  LA    R1,EDITTAB2         R1=A(EDIT TABLE) 2 DECIMAL                   
*                                  SEARCH TABLE FOR DEMO                        
GETDEM2  CLI   0(R1),EOT           TEST E-O-T                                   
         BE    GETDEM4                                                          
         CLC   0(2,R1),DUB         MATCH FILE/DEMO MODIFIER                     
         BE    *+12                                                             
         LA    R1,L'EDITTAB(R1)                                                 
         B     GETDEM2                                                          
         MVC   DUB+2(1),2(R1)      EXTRACT EDIT VALUES                          
*                                  EDIT DEMO VALUE                              
GETDEM4  TM    DUB+2,X'80'         TEST DEMO NEEDS SCALING                      
         BZ    *+8                                                              
         MH    R0,=H'10'                                                        
         TM    DUB+2,X'02'         TEST EDIT TO 2 DECIMALS                      
         BO    GETDEM6                                                          
         TM    DUB+2,X'01'         TEST EDIT TO 1 DECIMAL                       
         BO    GETDEM8                                                          
GETDEM5  EDIT  (R0),(7,0(R2))                                                   
         B     GETDEMX                                                          
GETDEM6  EDIT  (R0),(7,0(R2)),2,ZERO=BLANK                                      
         B     GETDEMX                                                          
GETDEM8  EDIT  (R0),(7,0(R2)),1,ZERO=BLANK                                      
         B     GETDEMX                                                          
GETDEMX  B     EXIT                                                             
         EJECT                                                                  
* SUB-ROUTINE TO GET BUY RECORD POST BUY DEMO OVERRIDES                         
*                                                                               
PBDEMOS  NTR1                                                                   
         MVI   PBDEMSW,C'N'                                                     
         XC    PBDEMVAL,PBDEMVAL                                                
         SR    R0,R0                                                            
         SR    R2,R2                                                            
         SR    R6,R6                                                            
         L     R1,AEBREC                                                        
         LA    R1,BDELEM-BUYREC(R1)                                             
*                                                                               
PBD2     CLI   0(R1),0             SCAN BUY RECORD FOR DEMO ELEMENTS            
         BE    PBD10                                                            
         CLI   0(R1),2             TEST ORIGINAL MARKET DEMO ELE                
         BE    PBD6                                                             
         CLI   0(R1),X'22'            OR ORIG MKT POST BUY DEMO ELE             
         BNE   PBD8                                                             
*                                                                               
PBD4     LR    R6,R1               R6=A(POST BUY DEMO ELEMENT)                  
         B     PBD8                                                             
PBD6     LR    R2,R1               R2=A(DEMO ELEMENT)                           
*                                                                               
PBD8     IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     PBD2                                                             
*                                                                               
PBD10    LTR   R6,R6               TEST POST BUY DEMO ELE FOUND                 
         BZ    PBDX                                                             
         LTR   R2,R2               YES-CHECK DEMO ELEMENT FOUND                 
         BZ    PBDX                                                             
         ZIC   R3,1(R2)                                                         
         SH    R3,=Y(NDEMNO-NDELEM)                                             
         BNP   PBDX                                                             
         SRL   R3,3                   R3=N'DEMOS IN DEMO ELEMENT                
         LA    R2,NDEMNO-NDELEM(R2)   R2=A(DEMOS IN DEMO ELEMENT)               
         LA    R4,3                                                             
         ZIC   R5,1(R6)            R4,R5 FOR POST BUY DEMO ELE BXLE             
         AR    R5,R6                                                            
         BCTR  R5,0                                                             
         LA    R6,PDEMO-PDELEM(R6) R6=A(DEMO VALS IN POST BUY DEMO ELE)         
         STM   R2,R6,SAVEREGS                                                   
         LA    R1,DEMS             R1=A(DEMO LIST)                              
         LA    RE,PBDEMVAL         RE=A(OUTPUT AREA)                            
         ZIC   R0,NDEMS            R0=N'DEMOS                                   
*                                                                               
PBD12    LM    R2,R6,SAVEREGS                                                   
*                                                                               
PBD16    CLC   0(3,R2),0(R1)       MATCH THE DEMO                               
         BNE   PBD18                                                            
         TM    0(R6),X'80'         TEST POST BUY DEMO OVERRIDE                  
         BZ    PBD20                                                            
         MVC   0(3,RE),0(R6)       YES-MOVE IN POST DEMO VALUE                  
         MVI   PBDEMSW,C'Y'        INDICATE POST BUY DEMOS EXIST                
         B     PBD20                                                            
*                                                                               
PBD18    LA    R2,8(R2)            NEXT DEMO IN DEMO ELE                        
         BCT   R3,*+8                                                           
         B     PBD20                                                            
         BXLE  R6,R4,PBD16         NEXT DEMO VAL IN POST BUY DEMO ELE           
*                                                                               
PBD20    LA    R1,3(R1)            NEXT LOOKUP DEMO                             
         LA    RE,3(RE)                                                         
         BCT   R0,PBD12                                                         
*                                                                               
PBDX     B     EXIT                                                             
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
HEADDAY1 DC    C'DAY DATE  TIME  PROGRAM'                                       
HEADDAY2 DC    C'-----------------------'                                       
         SPACE 1                                                                
*                                  TABLE FOR DEMO EDITTING ROUTINE              
EDITTAB  DS    0XL3                FILE/DEMO MODIFIER/EDIT RULES                
         DC    C'TI',X'01'                                                      
         DC    C'TR',X'01'                                                      
         DC    C'TP',X'01'                                                      
         DC    C'TS',X'01'                                                      
         DC    C'TQ',X'01'                                                      
         DC    C'TX',X'01'                                                      
         DC    C'TE',X'01'                                                      
         DC    C'PI',X'01'                                                      
         DC    C'PR',X'01'                                                      
         DC    C'PP',X'81'                                                      
         DC    C'PS',X'01'                                                      
         DC    C'PQ',X'01'                                                      
         DC    C'PX',X'01'                                                      
         DC    AL1(EOT)                                                         
EDITTAB2 DS    0XL3                FILE/DEMO MODIFIER/EDIT RULES                
         DC    C'TI',X'01'                                                      
         DC    C'TR',X'02'                                                      
         DC    C'TP',X'01'                                                      
         DC    C'TS',X'01'                                                      
         DC    C'TQ',X'01'                                                      
         DC    C'TX',X'01'                                                      
         DC    C'TE',X'02'                                                      
         DC    C'PI',X'01'                                                      
         DC    C'PR',X'01'                                                      
         DC    C'PP',X'81'                                                      
         DC    C'PS',X'01'                                                      
         DC    C'PQ',X'01'                                                      
         DC    C'PX',X'01'                                                      
         DC    AL1(EOT)                                                         
         EJECT                                                                  
* DEDEMWRK                                                                      
       ++INCLUDE DEDEMWRK                                                       
         ORG   APSAVE                                                           
         SPACE 1                                                                
* SAVE STORAGE FOR OVERLAY                                                      
*                                                                               
DEMWIDTH EQU   8                   WIDTH FOR ONE DEMO ENTRY                     
HEADLEN  DS    H                   L'STATION HEADINGS                           
EXECLEN  DS    H                   L'STATION HEADINGS-2 (FOR EX)                
DATADISP DS    H                   DISPLACEMENT TO START OF DISPLAY             
         ORG                                                                    
         EJECT                                                                  
* DSECT TO COVER TEMPORARY WORKING STORAGE                                      
*                                                                               
DEMTMPD  DSECT                                                                  
*                                                                               
SAVERE   DS    A                   SAVED RE VALUE IN DEMHOOK                    
SAVEREGS DS    5A                                                               
DAYTIME  DS    CL33                OUTPUT DAY/TIME VALUE                        
BUY1WEEK DS    XL16                1 WEEK LOOK-UP PROFILE                       
D0PROF   DS    XL16                D0 PROFILE                                   
*                                                                               
THISKEY  DS    0X                  CURRENT KEY VALUES                           
THISDATE DS    XL2                 CURRENT DAY/TIME ENTRY NUMBER                
THISTIME DS    XL2                 CURRENT START QUARTER HOUR                   
THISKEYX EQU   *                                                                
THISPROG DS    CL16                CURRENT PROGRAM NAME                         
THISDEMS DS    6XL4                CURRENT DEMOUT DEMO VALUES                   
*                                                                               
PBDEMSW  DS    CL1                 Y=POST BUY DEMO OVERRIDES PRESENT            
PBDEMVAL DS    XL(MAXDEMS*3)       POST BUY DEMO OVERRIDE VALUES                
*                                                                               
         DS    0F                  EXTENSION AREA FOR SPDEMLK                   
SYSCEXT  DS    0X                                                               
         DS    XL(L'SPXTAREA)                                                   
*                                                                               
*                                                                               
         SPACE 1                                                                
* DSECT TO COVER BINSRCH RECORD                                                 
*                                                                               
BINRECD  DSECT                                                                  
BINKEY   DS    0X                  BINSRCH KEY                                  
BINDATE  DS    XL2                 DAY NUMBER                                   
BINTIME  DS    XL2                 START QUARTER HOUR                           
BINKEYL  EQU   *-BINKEY                                                         
BINDATA  DS    0X                  BINSRCH DATA                                 
BINPROG  DS    CL16                PROGRAM NAME(S)                              
BINDEMS  DS    0XL3                VARIABLE NUMBER OF DEMO VALUES               
         SPACE 1                                                                
* BUYREC                                                                        
         SPACE 1                                                                
         PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030DEDEM08   05/13/20'                                      
         END                                                                    
