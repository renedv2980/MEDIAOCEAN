*          DATA SET DEDEM0A    AT LEVEL 122 AS OF 05/16/11                      
*PHASE T21B0AC                                                                  
                                                                                
****************************** UPDATE LOG *****************************         
*                                                                     *         
*   DATE  LVL USER DESCRIPTION                                        *         
* ------- --- ---- -------------------------------------------------- *         
* 22APR09 121 SKUI SUPPORT FOR NEW INVENTORY KEY                      *         
* 21MAR02 115 BPOO CHANGE MARKET NAME LENGTH TO 15 FROM DEM32         *         
* 06MAR01 113 BPOO CHANGE CPP LABEL TO CPP/M                          *         
* 02NOV00 078 BPOO MOVE DEM TABLES TO DEM81 PHASE                     *         
* 22SEP00 077 BPOO falink stuff...change screen one line lower        *         
* 20Jun00 075 GLEE Change versioning scheme to support PC vrsn stamp  *         
*                                                                     *         
* 08Mar00 074 GLEE Use new TSAR I/O area for TSAR routines            *         
*             GLEE Re-linked for bigger buy record support            *         
*                                                                     *         
* 13Sep99 073 GLEE Set CC upon exiting phase                          *         
*                                                                     *         
* 01Sep99 072 GLEE Don't "hardcode" A(demo list) in PVLK# routine     *         
*                                                                     *         
* 30Aug99 071 GLEE Handle more than MAXDEMS demos in DEM32 code       *         
*                                                                     *         
* 18Aug99 070 GLEE Set DBBEST to ALL for PAV lookups                  *         
*                                                                     *         
* 14May99 069 GLEE Fit Day/Time+Demo info on mainframe screen         *         
*                                                                     *         
* 18Jan99 067 GLEE Implement version (extract) dates for DEM32        *         
*                                                                     *         
* 23Dec98 066 GLEE Exits setting CC to HIGH, LOW, & EQUAL             *         
*                                                                     *         
* 14Dec98 065 GLEE Get S01W profile at top of DEMLINE routine         *         
*                                                                     *         
* 11Dec98 064 GLEE Bug fixes                                          *         
*                                                                     *         
* 20Nov98 063 GLEE Support for FALINK break & resume                  *         
*                                                                     *         
* 10Nov98 062 GLEE Support two different keys lengths for TSAR recds  *         
*                                                                     *         
* 28Oct98 061 GLEE Support for PAV in DEM32                           *         
*                                                                     *         
* 28Oct98 060 GLEE Get RMP profile if in REP system                   *         
*                                                                     *         
* 28Oct98 059 GLEE Set up SUBR03                                      *         
*                                                                     *         
* 31JUL98 006 BPOO SUPPORT FALINK AND DEM32                           *         
*                                                                     *         
* 09Jan97 005 GLEE Return 2400 instead of 0000 military time in QTM#  *         
*                                                                     *         
* 25Jul96 004 GLEE Removed alphamkt & mkt name from Tsar Demo Record  *         
*                                                                     *         
* 09Jul96 003 GLEE Enhanced program to capture all stations running   *         
*                   the program (increased table size)                *         
*                                                                     *         
* 20Nov95 002 GLEE Support for STEREO                                 *         
*                                                                     *         
* 14Nov95 001 GLEE Enhanced program and re-leveled to 001             *         
*                                                                     *         
* 01Dec94 001 GLEE New program to list those markets/stations which   *         
*                   ran a certain program and the times (demos) which *         
*                   the program ran                                   *         
***********************************************************************         
T21B0A   TITLE 'DEDEM0A - $DEM MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM'           
DEM0A    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LOCWORKL,**DM0A**,RA,RR=RE,CLEAR=YES                             
         USING DEMWRKD,R9          R9=A(GLOBAL W/S)                             
         USING DEMTWAD,R8          R8=A(TWA)                                    
         L     R7,AAPWORK                                                       
         USING DEMTMPD,R7          R7=A(TEMP W/S)                               
                                                                                
         ST    RC,ASDQTAB          A(STATION/DAY/QH TABLE)                      
         AH    RC,=Y(SDQMXSZQ)                                                  
         ST    RC,AIUNWRK          A(IUN WORK AREA)                             
         ST    RE,RELO                                                          
*                                                                               
         AHI   RC,IUNWRKL                                                       
         USING DM0AWRKD,RC                                                      
*                                                                               
         STM   RA,RB,MYBASES                                                    
*                                                                               
         DS    0H                  SET UP ADCONS                                
         LH    R1,=Y(DISPTAB-DEM0A)                                             
         LA    R1,DEM0A(R1)                                                     
         LA    R0,DISPTABQ                                                      
DEM0A_10 SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         LA    RE,DEM0A(RE)        RE=A(TABLE OR ROUTINE)                       
         SR    RF,RF                                                            
         ICM   RF,3,2(R1)                                                       
         LA    RF,DEMTMPD(RF)      RF-->PLACE TO STORE A(TABLE)                 
         ST    RE,0(RF)                                                         
         LA    R1,L'DISPTAB(R1)                                                 
         BCT   R0,DEM0A_10                                                      
*                                                                               
         LA    R0,DEMOD256                                                      
         ST    R0,ADEMD256                                                      
         LA    R0,DMVLI256                                                      
         ST    R0,ADVLI256                                                      
         LA    R0,DMVPG256                                                      
         ST    R0,ADVPG256                                                      
         LA    R0,DMVLO256                                                      
         ST    R0,ADVLO256                                                      
         LA    R0,DMVAL512                                                      
         ST    R0,ADMVL512                                                      
                                                                                
*                                  HANDLE CONTROLLER MODE SETTINGS              
         CLI   APMODE,FORMHEAD     FORMAT HEADLINES & INITIALISE                
         BE    DEMHEAD                                                          
         CLI   APMODE,PROCESS      READ & POST RECORDS TO BUFFER                
         BE    DEMPROC                                                          
         CLI   APMODE,FORMLINE     FORMAT A PRINT A BUFFER RECORD               
         BE    DEMLINE                                                          
         CLI   APMODE,STERTASK     STEREO SESSION TASKS                         
         BE    DEMSTTSK                                                         
         CLI   APMODE,APMBREAK     MAINFRAME TRANSACTION BREAK TASKS            
         BE    DEMBREAK                                                         
*                                                                               
EXITH    DS    0H                  EXIT W/ CC HIGH                              
         LA    R0,1                                                             
         J     EXITCR                                                           
                                                                                
EXITL    DS    0H                  EXIT W/ CC LOW                               
         LHI   R0,-1                                                            
         J     EXITCR                                                           
                                                                                
EXITE    DS    0H                  EXIT W/ CC EQUAL                             
         SR    R0,R0                                                            
         J     EXITCR                                                           
                                                                                
EXITCR   DS    0H                                                               
         AR    R0,R9                                                            
         CR    R0,R9               SET CC                                       
         J     EXIT                 AND EXIT                                    
                                                                                
YES      SR    R9,R9                                                            
NO       LTR   R9,R9                                                            
EXIT     XMOD1 1                                                                
         TITLE 'DEDEM0A - $DEM MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (A+        
               PMODE=FORMHEAD)'                                                 
***********************************************************************         
*=================== FORMAT HEADLINES & INITIALIZE ===================*         
                                                                                
DEMHEAD  DS    0H                                                               
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION                            
         BO    DEMHD10              GO TO STEREO LOGIC                          
*                                                                               
*                                                                               
         LA    RF,BINEND-BINRECD   L'RECORD                                     
                                                                                
         LA    R2,LMDHMNUM-LSTMKTD ASSUME SEQUENCING BY MARKET                  
         LA    R3,LMDHALFM-LSTMKTD                                              
         LA    R4,LMDHMNAM-LSTMKTD                                              
         LA    R6,LMDHSTTN-LSTMKTD                                              
         LA    R0,BINMNUM-BINRECD                                               
         LA    RE,L'BINMNUM+L'BINSTAT+L'BINDAY                                  
         CLI   OPTLIST,C'S'        TEST LIST IN STATION SEQUENCE                
         BNE   DEMHEAD2             NOPE                                        
         LA    R2,LSDHMNUM-LSTSTAD  YES, SEQUENCE BY NAME                       
         LA    R3,LSDHALFM-LSTSTAD                                              
         LA    R4,LSDHMNAM-LSTSTAD                                              
         LA    R6,LSDHSTTN-LSTSTAD                                              
         LA    R0,BINSTAT-BINRECD                                               
         LA    RE,L'BINSTAT+L'BINDAY                                            
*                                                                               
DEMHEAD2 DS    0H                                                               
         ST    RF,BINLREC          SET BINSRCH PARMS                            
         ST    RE,BINLKEY                                                       
         STC   R0,BINDKEY                                                       
                                                                                
         LA    RE,DEMHD2(R2)       FORMAT HEADING - MARKET NUMBER               
         MVC   0(L'LMDHMNUM,RE),HEADNUM2                                        
         LA    RE,DEMHD3(R2)                                                    
         MVC   0(L'LMDHMNUM,RE),HEADNUM3                                        
                                                                                
         LA    RE,DEMHD2(R3)       FORMAT HEADING - ALPHA MARKET                
         MVC   0(L'LMDHALFM,RE),HEADALF2                                        
         LA    RE,DEMHD3+(LMDALFM-LMDHALFM)(R3)                                 
         MVC   0(L'HEADALF3,RE),HEADALF3                                        
                                                                                
         LA    RE,DEMHD2(R4)       FORMAT HEADING - MARKET NAME                 
         MVC   0(LMNAM,RE),HEADNAM2                                             
         LA    RE,DEMHD3(R4)                                                    
         MVC   0(LMNAM,RE),HEADNAM3                                             
                                                                                
         LA    RE,DEMHD2(R6)       FORMAT HEADING - STATION                     
         MVC   0(L'LMDHSTTN,RE),HEADSTA2                                        
         LA    RE,DEMHD3(R6)                                                    
         MVC   0(L'LMDHSTTN,RE),HEADSTA3                                        
                                                                                
         DS    0H                  FORMAT HEADING - DAY                         
         LA    RE,DEMHD2+(LDHDAY-LDSECT)                                        
         MVC   0(L'LDHDAY,RE),HEADDAY2                                          
         LA    RE,DEMHD3+(LDHDAY-LDSECT)                                        
         MVC   0(L'LDHDAY,RE),HEADDAY3                                          
                                                                                
         DS    0H                  FORMAT HEADING - START TIME                  
         LA    RE,DEMHD2+(LDHSTM-LDSECT)                                        
         MVC   0(L'HEADSTM2,RE),HEADSTM2                                        
         LA    RE,DEMHD3+(LDHSTM-LDSECT)+1                                      
         MVC   0(L'HEADSTM3,RE),HEADSTM3                                        
                                                                                
         DS    0H                  FORMAT HEADING - END TIME                    
         LA    RE,DEMHD2+(LDHETM-LDSECT)                                        
         MVC   0(L'HEADETM2,RE),HEADETM2                                        
         LA    RE,DEMHD3+(LDHETM-LDSECT)+1                                      
         MVC   0(L'HEADETM3,RE),HEADETM3                                        
*                                                                               
         CLI   NDEMS,0             ANY DEMOS SPECIFIED?                         
         BE    DEMHEAD6             NOPE                                        
                                                                                
         DS    0H                  FORMAT HEADING - LEAD IN                     
         LA    RE,DEMHD2+(LDHLDIN-LDSECT)+1                                     
         MVC   0(L'HEADLEAD,RE),HEADLEAD                                        
         LA    RE,DEMHD3+(LDHLDIN-LDSECT)+2                                     
         MVC   0(L'HEADLIN3,RE),HEADLIN3                                        
                                                                                
         DS    0H                  FORMAT HEADING - DEMO NAME                   
         LA    RE,DEMHD2+(LDHDEMO-LDSECT)                                       
         MVC   0(7,RE),DEMOUT75                                                 
         LA    RE,DEMHD3+(LDHDEMO-LDSECT)+1                                     
         MVC   0(5,RE),DEMOUT75+7                                               
                                                                                
         DS    0H                  FORMAT HEADING - LEAD OUT                    
         LA    RE,DEMHD2+(LDHLDOUT-LDSECT)+1                                    
         MVC   0(L'HEADLEAD,RE),HEADLEAD                                        
         LA    RE,DEMHD3+(LDHLDOUT-LDSECT)+2                                    
         MVC   0(L'HEADLOT3,RE),HEADLOT3                                        
                                                                                
DEMHEAD6 DS    0H                                                               
         B     DEMHEADX                                                         
                                                                                
*                                                                               
DEMHD10  DS    0H                  THIS PART FOR STEREO SESSION ONLY            
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BNO   DEMHD15                                                          
*                                                                               
*********************************************                                   
         XC    PFLAG,PFLAG                                                      
         CLC   =C'PROGMRNK',DUMACT                                              
         BNE   DEMHD15                                                          
         LA    RE,REQCTLTB    IF MORE THAN ONE PROGRAM REQUESTED                
         LH    RF,DSPRCTST    FOR MKT RANK                                      
         AR    RE,RF                                                            
         USING RCTDSECT,RE                                                      
         OI    PFLAG,PMKTRNK                                                    
         CLI   RCTDCNT,1                                                        
*        BNH   DEMHEADX                                                         
         BNH   DEMHD15                                                          
*                                                                               
         CLI   RCTUPTO,1                                                        
         BNH   *+8                                                              
         OI    PFLAG,PPASSONE    PASSED FIRST PROGRAM NUMBER                    
*                                                                               
         OI    PFLAG,PMULTPRG                                                   
* ===================   MKT DEMHEAD  =====================                      
         DS    0H                  THIS PART FOR STEREO SESSION ONLY            
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         CLI   TSKEYL,0            SEE IF WE NEED TO SET LENGTHS AGAIN          
         BNE   DEMHEADX             NOPE, THEY WERE SET BEFORE                  
*                                                                               
         MVI   TSKEYL,L'TDRAMNAM   ASSUME LIST IN ALPHA SEQUENCE                
         CLI   OPTLIST,C'N'                                                     
         BNE   *+8                                                              
         MVI   TSKEYL,L'TDRNMRKT    WRONG, LIST IN NUMERIC SEQUENCE             
*                                                                               
         LA    R0,TDRLEN3Q+2                                                    
         STH   R0,TSRECL           SET MAX LENGTH OF RECORD                     
         MVI   ANYDATA,C'N'        ASSUME NO DATA FOR THIS REQUEST              
*                                                                               
         DROP  RE                                                               
*        B     DEMHD20                                                          
         B     DEMHEADX                                                         
*                                                                               
****************************************************                            
DEMHD15  LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         CLI   TSKEYL,0            SEE IF WE NEED TO SET LENGTHS AGAIN          
         BNE   DEMHD20              NOPE, THEY WERE SET BEFORE                  
*                                                                               
         MVI   TSKEYL,TDRKEYL      SET  LIST IN NUMERIC SEQUENCE                
         MVC   TSRECL,LTSIOREC     SET MAX RECORD LENGTH                        
         MVI   STDOPROC,C'Y'       EXECUTE DEM PROC ROUTINE                     
         MVI   ANYDATA,C'N'        ASSUME NO DATA FOR THIS REQUEST              
         DROP  R4                                                               
******************************************                                      
*                                                                               
DEMHD20  DS    0H                  SET UP DEMO LIST                             
         ZICM  R1,NDEMS,(1)                                                     
         BZ    DEMHD30              DON'T DO ANYTHING IF NO DEMOS               
                                                                                
         L     RE,ASTDMEXP                                                      
         ZIC   RF,0(RE)            UPDATE STEREO'S COUNT OF # OF DEMOS          
         AR    RF,R1                                                            
         STC   RF,0(RE)                                                         
         STC   R1,DEMONDEM         STORE CURRENT # OF DEMOS ENTERRED            
                                                                                
         SR    RF,R1                                                            
         MH    RF,=H'3'                                                         
         LA    RE,1(RE,RF)         RE = A(PLACE TO MOVE DEMOS TO)               
         MH    R1,=H'3'            R1 = L(DEMOS TO MOVE)                        
         EXMVC R1,0(RE),DEMS       INCLUDES LIST TERMINATOR                     
*                                                                               
DEMHD30  DS    0H                                                               
*                                                                               
DEMHEADX DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
         TITLE 'DEDEM0A - $DEM MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (A+        
               PMODE=PROCESS)'                                                  
***********************************************************************         
*============== READ DEMO FILES & POST TO BINSRCH BUFFER =============*         
                                                                                
DEMPROC  DS    0H                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BZ    DEMPRC05                                                         
         TM    PFLAG,PMKTRNK+PMULTPRG+PPASSONE PASSED 1ST PROGRAM               
         BO    EXIT                                                             
         TM    PFLAG,PMKTRNK+PMULTPRG   MKT RANK AND MORE THAN 1 PRG#           
         BO    DEMPR200                                                         
*                                                                               
DEMPRC05 TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BZ    *+12                                                             
         CLI   STDOPROC,C'Y'        SEE IF WE SHOULD EXECUTE  DEMPROC           
         BNE   DMPR90                NO, SKIP ALL THE DEMO FILE READS           
                                                                                
         MVI   STDOPROC,C'N'        IF YES, DON'T DO IT NEXT TIME               
*                                                                               
         L     RE,ASDQTAB                                                       
         LH    RF,=Y(SDQMXSZQ)                                                  
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR AREA FOR SDQTABD TABLE                 
         BCTR  RE,0                                                             
         ST    RE,ASDQTABX          AND SET A(END OF SDQTABD TABLE)             
*                                                                               
** GET ALL STATIONS, DAYS, AND START- & END- QHS FOR PROGRAM **                 
*                                                                               
         MVI   GOSUBN,IDB#         INITIALIZE DBLOCK                            
         GOTO1 AGOSUB                                                           
                                                                                
         LA    R5,DBLOCK1                                                       
         USING DBLOCKD,R5                                                       
         MVI   DBFUNCT,DBGETISI    SET DEMAND FUNCTION                          
         MVC   DBSELPR4,STAS       STAS(4) = PROGRAM NUMBER                     
         XC    SDQCNT,SDQCNT       CLEAR # OF ENTRIES IN SDQTABD TABLE          
                                                                                
         DS    0H                  CALL DEMAND TO READ RECORDS                  
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,DEMHOOK1,0,0,0,0                            
         OC    SDQCNT,SDQCNT                                                    
         BZ    DEMPROCX                                                         
         DROP  R5                                                               
                                                                                
*                                                                               
** PROCESS EACH ENTRY IN STATION/DAY/QH TABLE **                                
*                                                                               
         MVI   ANYDATA,C'Y'        THERE IS DATA FOR THIS REQUEST               
         MVI   GOSUBN,IDB#         INITIALIZE DBLOCK                            
         GOTO1 AGOSUB                                                           
                                                                                
         LA    R5,DBLOCK1                                                       
         USING DBLOCKD,R5                                                       
         MVI   DBFUNCT,DBGETSM     GET MKT NUMBER FOR EACH STATION              
         MVC   DBBTYPE,BKS+3                                                    
*                                                                               
         L     R2,ASDQTAB                                                       
         USING SDQTABD,R2                                                       
         ZAP   COUNTER,=P'1'                                                    
                                                                                
DMPR50   DS    0H                  START OF LOOP                                
*                                                                               
*** GET MARKET INFORMATION FOR STATIONS IN SDQTABD ***                          
*                                                                               
         DS    0H                                                               
         MVC   DBSELSTA,SDQSTA        SET STATION                               
         XC    THISMNUM,THISMNUM                                                
                                                                                
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,DEMHOOK2,0,0                                
         DROP  R5                                                               
*                                                                               
*** POST TO BUFFER ***                                                          
*                                                                               
         OC    THISMNUM,THISMNUM                                                
         BZ    DMPR70                                                           
                                                                                
         MVC   THISSTA,SDQSTA                                                   
         MVC   THISKDAY,SDQDAY                                                  
         MVC   THISSQH,SDQSQH                                                   
         MVC   THISEQH,SDQEQH                                                   
         MVI   POSTRTYP,TDRRTPGI   POSTING PROGRAM INFORMATION                  
         BAS   RE,DEMPOST                                                       
         ST    R2,ASDQNTRY                                                      
*                                                                               
*                                                                               
                                                                                
DMPR70   DS    0H                                                               
         ZAP   DUB,COUNTER                                                      
         CVB   R0,DUB                                                           
         CH    R0,SDQCNT           PROCESSED ALL ENTRIES IN SDQTAB YET?         
         BNL   DMPR80               YEP, GET OUT OFF LOOP                       
         AP    COUNTER,=P'1'        NOPE, BUMP COUNTERS                         
         LA    R2,SDQLENQ(R2)        AND POINTERS                               
         B     DMPR50                AND LOOP BACK TO DO SOME MORE              
                                                                                
         DROP  R2                                                               
                                                                                
*                                                                               
** GET PROGRAM INFORMATION **                                                   
*                                                                               
DMPR80   DS    0H                  BUILD DBLOCK                                 
         MVI   GOSUBN,IDB#         INITIALIZE DBLOCK                            
         GOTO1 AGOSUB                                                           
                                                                                
         LA    R5,DBLOCK1                                                       
         USING DBLOCKD,R5                                                       
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBBTYPE,BKS+3                                                    
         MVI   DBSELWKN,X'FF'      WANT ALL WEEKS                               
                                                                                
         L     R2,ASDQNTRY         USE A "GOOD" SDQTABD ENTRY                   
         USING SDQTABD,R2                                                       
         MVC   DBSELSTA,SDQSTA        SET STATION                               
                                                                                
         MVI   DBSELDAY,0                                                       
         L     R1,ADAYTAB                                                       
DMPR82A  CLI   0(R1),X'FF'                                                      
         BE    DMPR82X                                                          
         CLC   SDQDAY,1(R1)                                                     
         BE    DMPR82C                                                          
         LA    R1,L'DAYTAB(R1)                                                  
         B     DMPR82A                                                          
DMPR82C  MVC   DBSELDAY,2(R1)         SET DAY                                   
DMPR82X  EQU   *                                                                
                                                                                
         MVI   GOSUBN,QTM#         CONVERTING QHRS TO MIL TIMES                 
         MVC   TEMPQHR,SDQSQH                                                   
         GOTO1 AGOSUB                                                           
         MVC   DBSELTIM(2),TEMPMTIM   SET START TIME                            
         ZIC   R1,SDQEQH                                                        
         LA    R1,1(R1)                                                         
         STC   R1,TEMPQHR                                                       
         GOTO1 AGOSUB                                                           
         MVC   DBSELTIM+2(2),TEMPMTIM  AND END TIME                             
         DROP  R2                                                               
                                                                                
         XC    DFPGNUM,DFPGNUM     INITIALIZE PROGRAM VALUES                    
         XC    DFPGNAM,DFPGNAM                                                  
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,DEMHOOK3,0,0,0,0                            
         DROP  R5                                                               
                                                                                
*                                                                               
** COUNT # OF MARKETS AND STATIONS **                                           
*                                                                               
         DS    0H                                                               
         MVI   GOSUBN,CMS#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
** FORMAT 1ST HEADLINE **                                                       
*                                                                               
DMPR90   DS    0H                                                               
         MVI   GOSUBN,BMSG#                                                     
         GOTO1 AGOSUB                                                           
                                                                                
         TM    DEMFLAG1,DF1STERO                                                
         BO    DMPR95                                                           
         MVC   DEMHD1,SCRNLINE                                                  
         B     DMPR100                                                          
                                                                                
DMPR95   DS    0H                  STEREO SESSION,                              
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    *+12                                                             
         MVI   POSTRTYP,TDRRTMSG    POST INFO MSG RECD                          
         BAS   RE,DEMPOST                                                       
                                                                                
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    *+12                                                             
         MVI   POSTRTYP,TDRRTHDR    POST COLUMN HEADER RECD                     
         BAS   RE,DEMPOST                                                       
                                                                                
         B     DMPR100                                                          
                                                                                
DMPR100  DS    0H                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32 A STEREO SESSION,                     
         BNO   DMPR109              DON'T EXECUTE LOGIC BELOW                   
         CLI   NDEMS,0              DONT CREATE IF DEMOS REQUESTED              
         BE    DMPR109                                                          
         MVI   POSTRTYP,TDRRTDML                                                
         XC    THISKEY(THISKEYL),THISKEY                                        
         BAS   RE,DEMPOST                                                       
DMPR109  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         B     DEMPROCX                                                         
                                                                                
**    PROCESS FOR MKT RANK WITH MORE THAN ONE PROGRAM # ***                     
DEMPR200 DS    0X                                                               
         MVI   GOSUBN,IDB#         INITIALIZE DBLOCK                            
         GOTO1 AGOSUB                                                           
*                                                                               
         LA    R5,DBLOCK1          INITIALIZE DBLOCK FOR MARKET READS           
         USING DBLOCKD,R5          R5=A(DBLOCK)                                 
         MVI   DBFUNCT,DBGETMKB                                                 
         MVC   DBBTYPE,BKS+3                                                    
         XC    MARKETN,MARKETN     CLEAR N'MARKETS                              
*                                  CALL DEMAND TO READ RECORDS                  
*                                                                               
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,MYDEMHK,0,0,0,0                             
*                                                                               
DMPRC10  DS    0H                  SPECIAL POSTING FOR STEREO                   
         CLI   ANYDATA,C'Y'         IF THERE WERE NO DATA,                      
         BNE   DEMPROCX              SKIP THIS PART                             
*                                                                               
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         SR    R2,R2                                                            
         ICM   R2,7,TSAREC+1                                                    
                                                                                
         MVC   0(2,R2),TSRECL              MOVE IN LENGTH OF RECORD             
         LA    R2,2(R2)                                                         
         USING TSDEMRCD,R2                                                      
         XC    0(TDRLENQ,R2),0(R2)                                              
         MVI   TDRRTYP,TDRRTMSG                                                 
         LA    R1,TDRAMNAM+L'TDRAMNAM-L'MARKETN                                 
         CLI   OPTLIST,C'N'                                                     
         BNE   *+8                                                              
         LA    R1,TDRNMNAM+L'TDRNMNAM-L'MARKETN                                 
         MVC   0(L'MARKETN,R1),MARKETN     MOVE IN # OF MARKETS                 
         GOTO1 APOST                                                            
*                                                                               
         DROP  R2,R4                                                            
                                                                                
*                                                                               
DEMPROCX DS    0H                                                               
         B     EXITE                                                            
         EJECT                                                                  
***************************************************************                 
* ROUTINE TO PROCESS A DEMO RECORD RETURNED FROM DEMAND                         
*                                                                               
MYDEMHK  DS    0H                                                               
         ST    RE,SAVERE           SAVE RETURN ADDRESS                          
*                                  SEARCH RECORD FOR NAME ELEMENT               
         L     R1,DBAREC                                                        
         LA    R1,DMFRSTEL-DMKEY(R1)                                            
         USING DMELEM,R1           R1=A(FIRST ELEMENT)                          
         SR    RE,RE                                                            
MYDEMHK2 CLI   DMELEM,0            TEST E-O-R                                   
         BE    MYDEMHK6                                                         
         CLI   DMELEM,DMECODEQ     TEST MARKET NAME ELEMENT                     
         BE    *+14                                                             
         IC    RE,DMLEN                                                         
         AR    R1,RE                                                            
         B     MYDEMHK2                                                         
*                                  EXTRACT MARKET NAME FROM ELEMENT             
         MVC   MARNAME,SPACES                                                   
         MVC   MARKET,DMMNO                                                     
         IC    RE,DMLEN                                                         
         SH    RE,=H'5'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MARNAME(0),DMMNAME                                               
         DROP  R1                                                               
                                                                                
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,DMCB,=C'NAMKT',DBLOCK,WORK                               
         MVC   MARALF,WORK                                                      
*                                  BUILD TSAR DEMO RECORD & POST                
MYDEMHK4 DS    0H                                                               
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         SR    R2,R2                                                            
         ICM   R2,7,TSAREC+1                                                    
         MVC   0(2,R2),TSRECL      MOVE IN LENGTH OF RECORD                     
         LA    R3,2(R2)                                                         
         USING TSDEMRCD,R3                                                      
                                                                                
         MVC   TDRNMNAM,MARNAME    MOVE MARKET NAME AND NUMBER                  
         MVC   TDRNMRKT,MARKET      INTO TSAR DEMO RECORD                       
         MVC   TDRALF,MARALF       MOVE IN ALPHA MARKET CODE                    
         MVI   TDRRTYP,TDRRTMKT                                                 
                                                                                
         GOTO1 APOST                                                            
         MVI   ANYDATA,C'Y'        THERE IS DATA FOR THIS REQUEST               
                                                                                
         B     MYDEMHK6                                                         
         DROP  R3,R4                                                            
*                                  BUMP N'ACTIVE MARKETS                        
MYDEMHK6 SR    R1,R1                                                            
         ICM   R1,3,MARKETN                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,MARKETN                                                     
*                                  RETURN TO DEMAND                             
MYDEMHKX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*======= ROUTINE TO PROCESS A DEMO RECORD RETURNED FROM DEMAND =======*         
                                                                                
* Builds entries for SDQTABD table and inserts them into it.  Since             
*  the records are passed into this hook in order, the entries in               
*  the table will be in order as well.                                          
* At entry,                                                                     
*  R5-->DBLOCK                                                                  
* At exit,                                                                      
*  SDQCNT = # of entries in table.                                              
                                                                                
DEMHOOK1 DS    0H                                                               
HK1      NTR1                                                                   
         USING DBLOCKD,R5                                                       
                                                                                
         CLI   DBRECTYP,DBRECPRG   MAKE SURE CORRECT RECD TYPE                  
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         DS    0H                  UPDATE # OF ENTRIES IN TABLE                 
         LH    RE,SDQCNT                                                        
         CH    RE,=Y(SDQMXSZQ/SDQLENQ)  CAN ANY MORE FIT INTO TABLE?            
         BNL   DMHK1X                    NO, EXIT NOW                           
         LA    R1,1(RE)                                                         
         STH   R1,SDQCNT                 YES, UPDATE COUNT                      
*                                                                               
         DS    0H                                                               
         MH    RE,=Y(SDQLENQ)      RE=DISPL TO PUT NEXT ENTRY                   
         A     RE,ASDQTAB          RE-->LOCATION TO PUT NEXT ENTRY              
         USING SDQTABD,RE                                                       
         LA    R2,DBKEY                                                         
         USING PIKEY,R2                                                         
         MVC   SDQSTA,PISTA                                                     
         MVC   SDQDAY,PIDAY                                                     
         MVC   SDQSQH,PISQH                                                     
         MVC   SDQEQH,PIEQH                                                     
         DROP  R2,RE                                                            
*                                                                               
DMHK1X   DS    0H                  RETURN TO DEMAND                             
         B     EXIT                                                             
                                                                                
         DROP  R5                                                               
         EJECT                                                                  
*====== ROUTINE TO PROCESS A STATION RECORD RETURNED FROM DEMAND =====*         
                                                                                
* Hook is used to get market number corresponding to the station.               
* At entry,                                                                     
*  R5-->DBLOCKD.                                                                
                                                                                
DEMHOOK2 DS    0H                                                               
HK2      NTR1                                                                   
         USING DBLOCKD,R5                                                       
*                                                                               
         CLI   DBRECTYP,DBRECSM    MAKE SURE WE HAVE CORRECT RECD TYPE          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,DBKEY                                                         
         USING BSKEY,R2            R2=A(KEY)                                    
         CLC   DBBTYPE,BSBTYP       MATCH ON BOOKTYPE                           
         BNE   DMHK214X                                                         
         OC    BSKMKT,BSKMKT        NO SPILL MARKETS                            
         BNZ   DMHK214X                                                         
         MVC   THISMNUM,BSRMKT                                                  
DMHK214X EQU   *                                                                
         DROP  R2                                                               
*                                                                               
         B     EXIT                                                             
                                                                                
         DROP  R5                                                               
         EJECT                                                                  
*======= ROUTINE TO PROCESS A DEMO RECORD RETURNED FROM DEMAND =======*         
                                                                                
* Hook is used to extract program info.                                         
* At entry,                                                                     
*  R5-->DBLOCKD.                                                                
                                                                                
DEMHOOK3 DS    0H                                                               
HK3      NTR1                                                                   
         USING DBLOCKD,R5                                                       
*                                                                               
         OC    DFPGNUM,DFPGNUM     DID WE GET PROGRAM INFO ALREADY?             
         BNZ   DMHK3X               YES, DON'T WASTE ANY TIME                   
*                                                                               
         CLI   DBRECTYP,DBRECDEM   MAKE SURE WE HAVE CORRECT RECD TYPE          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         DS    0H                   AND QUARTER-HOUR ELEMENT                    
         BAS   RE,MTCHPRGN                                                      
         BNE   DMHK3X                                                           
*                                                                               
         DS    0H                  EXTRACT PROGRAM INFORMATION                  
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'TPNO',DBLOCKD,WORK,0,0,0                       
         MVC   DFPGNUM,WORK         PROGRAM NUMBER                              
                                                                                
         MVC   WORK,SPACES                                                      
         GOTO1 (RF),(R1),=C'PROGRA',,WORK,0,0,0                                 
         MVC   DFPGNAM,WORK         PROGRAM NAME                                
*                                                                               
*                                                                               
         B     DMHK3X                                                           
*                                                                               
DMHK3X   DS    0H                  RETURN TO DEMAND                             
         B     EXIT                                                             
                                                                                
         DROP  R5                                                               
         EJECT                                                                  
*======================== POST TO SORT BUFFER ========================*         
                                                                                
         DS    0H                                                               
DEMPOST  NTR1                                                                   
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BO    DMPST100             POST THE STEREO WAY                         
*                                                                               
         LA    R1,POSTLINE                                                      
         USING BINRECD,R1                                                       
         XC    BINRECD(BINRECL),BINRECD                                         
         MVC   BINKEY(BINKEYL),THISKEY                                          
         DROP  R1                                                               
                                                                                
         GOTO1 APOST                                                            
                                                                                
         DS    0H                                                               
         B     DEMPOSTX                                                         
                                                                                
                                                                                
DMPST100 DS    0H                  POSTING FOR STEREO                           
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         SR    R2,R2                                                            
         ICM   R2,7,TSAREC+1                                                    
         BZ    DEMPOSTX                                                         
         DROP  R4                                                               
*                                                                               
         CLI   POSTRTYP,TDRRTPGI    RECORD IS TO CONTAIN PROGRAM INFO           
         BE    DMPST110                                                         
         CLI   POSTRTYP,TDRRTDML    RECORD IS TO CONTAIN DEMOLIST               
         BE    DMPST170                                                         
         CLI   POSTRTYP,TDRRTMSG    INFO MESSAGE RECORD                         
         BE    DMPST130                                                         
         CLI   POSTRTYP,TDRRTHDR    COLUMN HEADER RECORD                        
         BE    DMPST150                                                         
         DC    H'0'                                                             
                                                                                
*                                                                               
** POST PROGRAM INFORMATION **                                                  
*                                                                               
DMPST110 DS    0H                  POSTING PROGRAM INFORMATION                  
                                                                                
         LA    R0,TDRFIXL+2                                                     
         STCM  R0,3,0(R2)          SET LENGTH OF TSAR DEMO RECD                 
*                                                                               
         DS    0H                  BUILD KEY OF TSAR DEMO RECORD                
         LA    R2,2(R2)                                                         
         USING TSDEMRCD,R2                                                      
         XC    TDRKEY(TDRKEYL),TDRKEY                                           
         MVI   TDRRTYP,TDRRTPGI                                                 
                                                                                
         LH    RE,DSPRCTST                                                      
         LA    RE,REQCTLTB(RE)                                                  
         MVC   TDRPNUM,(RCTFROM-RCTDSECT)(RE)   NTH PROGRAM OF REQUEST          
                                                                                
         MVC   TDRMMNUM,THISMNUM   SEQUENCE BY MARKET                           
         MVC   TDRMSTAT,THISSTA                                                 
         CLI   OPTLIST,C'S'                                                     
         BNE   *+16                                                             
         MVC   TDRSSTAT,THISSTA    SEQUENCE BY STATION                          
         MVC   TDRSMNUM,THISMNUM                                                
                                                                                
         MVC   TDRDAY,THISKDAY                                                  
         MVC   TDRSQH,THISSQH                                                   
         MVC   TDREQH,THISEQH                                                   
*                                                                               
         DS    0H                  MOVE DATA INTO RECORD                        
         MVI   TDRDUMMY,0                                                       
*                                                                               
         DROP  R2                                                               
                                                                                
         DS    0H                                                               
         B     DMPST200            PUT RECORD IN (SORT) BUFFER                  
                                                                                
*                                                                               
** POST INFO MESSAGE **                                                         
*                                                                               
DMPST130 DS    0H                  POSTING INFO MESSAGE RECORD                  
         CLI   ANYDATA,C'Y'                                                     
         BNE   DEMPROC                                                          
                                                                                
         DS    0H                  CALCULATE LENGTH OF MESSAGE                  
         LA    R0,SCRNLINE                                                      
         LA    R1,SCRNLINE+L'SCRNLINE-1                                         
DMPST133 CR    R0,R1                                                            
         BE    DMPST136                                                         
         CLI   0(R1),C' '                                                       
         BH    DMPST136                                                         
         BCT   R1,DMPST133                                                      
DMPST136 SR    R1,R0               R1 = L(MESSAGE) - 1                          
         LA    R0,TDRKEYL+2+1(R1)                                               
         STCM  R0,3,0(R2)          SET LENGTH OF TSAR DEMO RECD                 
                                                                                
         DS    0H                  BUILD KEY OF TSAR DEMO RECORD                
         LA    R2,2(R2)                                                         
         USING TSDEMRCD,R2                                                      
         XC    TDRKEY(TDRKEYL),TDRKEY                                           
         MVI   TDRRTYP,TDRRTMSG                                                 
         EXMVC R1,TDRDATA,SCRNLINE                                              
                                                                                
         DS    0H                                                               
*                                                                               
         B     DMPST200            PUT RECORD IN (SORT) BUFFER                  
                                                                                
*                                                                               
** POST COLUMN HEADER **                                                        
*                                                                               
DMPST150 DS    0H                  POSTING COLUMN HEADER RECORD                 
         CLI   ANYDATA,C'Y'                                                     
         BNE   DEMPROC                                                          
                                                                                
         LA    R0,2+TDRKEYL+1                                                   
         STCM  R0,3,0(R2)          SET LENGTH OF TSAR DEMO RECD                 
                                                                                
         DS    0H                  BUILD KEY OF TSAR DEMO RECORD                
         LA    R2,2(R2)                                                         
         USING TSDEMRCD,R2                                                      
         XC    TDRKEY(TDRKEYL),TDRKEY                                           
         MVI   TDRRTYP,TDRRTHDR                                                 
         MVI   TDRDATA,0            MOVE IN DUMMY DATA                          
                                                                                
         MVI   TDRRTYP+1,2         "2ND" HEADLINE INFO                          
         GOTO1 APOST                                                            
         MVI   TDRRTYP+1,3         "3RD" HEADLINE INFO                          
         GOTO1 APOST                                                            
                                                                                
         DROP  R2                                                               
                                                                                
         DS    0H                                                               
         B     DEMPOSTX            EXIT                                         
                                                                                
*                                                                               
** POST DEMOLIST **                                                             
*                                                                               
DMPST170 DS    0H                                                               
         LA    R3,TSARKEY                                                       
         USING TSDEMRCD,R3                                                      
         XC    TDRKEY(TDRKEYL),TDRKEY                                           
         MVI   TDRRTYP,TDRRTDML                                                 
         GOTO1 AGETTDR                                                          
         DROP  R3                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         LA    R3,2(R2)            R2 = A(TSAR RECORD)                          
         USING TSDEMRCD,R3                                                      
         LH    RF,DSPRCTDM                                                      
         LA    RF,REQCTLTB(RF)                                                  
         ZIC   RE,(RCTFROM-RCTDSECT)(RF)                                        
         BCTR  RE,0                                                             
         MH    RE,=Y(L'DEMS)                                                    
         LA    RE,TDRDEMS(RE)                                                   
                                                                                
         ZIC   R1,NDEMS                                                         
         MH    R1,=Y(L'DEMS)                                                    
                                                                                
         EXMVC R1,0(RE),DEMS       MOVES IN DELIMITER TOO!                      
         LA    RE,1(R1,RE)                                                      
         SR    RE,R2                                                            
         STH   RE,0(R2)                                                         
         DROP  R3                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         B     DMPST200                                                         
                                                                                
*                                                                               
DMPST200 DS    0H                  PUT RECORD IN (SORT) BUFFER                  
         GOTO1 APOST                                                            
*                                                                               
DEMPOSTX DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
         TITLE 'DEDEM0A - $DEM MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (A+        
               PMODE=FORMLINE)'                                                 
***********************************************************************         
*========================= FORMAT PRINT LINES ========================*         
                                                                                
DEMLINE  DS    0H                                                               
*                                                                               
         DS    0H                                                               
         MVC   DUB(4),=C'S01W'             ALWAYS GET SPOT 1W PROFILE           
         MVI   GOSUBN,GPRF#                                                     
         GOTO1 AGOSUB                                                           
         MVC   PRECPROF,PROF1W+5           SET PRECISN TO PROFL SETTING         
*                                                                               
         L     RF,ASYSNTRY                                                      
         CLI   (SYSOVSYS-SYSTABD)(RF),8    IF REP SYSTEM,                       
         BNE   DMLN012RX                                                        
         MVC   DUB(4),=C'RRMP'                                                  
         MVI   GOSUBN,GPRF#                                                     
         GOTO1 AGOSUB                       GO GET REP/RMP PROFILE              
***      MVI   PRECPROF,C'R'               SET PRECISION                        
****     TM    PROFRRMP+RMPIMPSB,RMPIMPSA                                       
****     BZ    *+8                                                              
* FORCE TO BE IMPRESSIONS BASED                                                 
***                                                                             
****     MVI   PRECPROF,C'I'                ACCORDING TO PROFILE                
***      B     DMLN012X                                                         
DMLN012RX EQU  *                                                                
         MVI   PRECPROF,C'I'                FORCE TO BE IMP BASED               
*                                                                               
DMLN012X EQU   *                                                                
*&&DO                                                                           
         CLI   OPTDMA,0            WAS PRECISION SPECIFIED?                     
         BE    *+10                                                             
         MVC   PRECPROF,OPTDMA      YES, PASS IT TO THE PROFILE                 
*&&                                                                             
*                                                                               
         DS    0H                  SET TAPE/BOOK BASED DEMO CALC                
         MVI   TAPEOPT,C'Y'         (PATCHABLE TAPE BASED OPTION)               
******   MVI   TAPEOPT,C'N'         BOOK BASED                                  
                                                                                
         CLI   DBMED,C'N'                                                       
         BE    DMLN014M                                                         
         CLI   DBSRC,C'N'           TAPE BASED FOR NIELSEN                      
         BNE   DMLN014X                                                         
         CLI   DBMED,C'T'            USTV ONLY                                  
         BNE   DMLN014X                                                         
DMLN014M CLI   PRECPROF,C'I'        IMP BASED <==> TAPE BASED                   
         BNE   *+8                                                              
         MVI   TAPEOPT,C'Y'         TAPE BASED                                  
DMLN014X EQU   *                                                                
         EJECT                                                                  
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BO    DMLN100              GO TO STEREO LOGIC                          
*                                                                               
*                                                                               
         L     R5,BINAREC                                                       
         USING BINRECD,R5                                                       
         CLI   BINKEY,X'FF'                                                     
         BE    DEMLINEX                                                         
         MVC   THISKEY(BINKEYL),BINKEY                                          
         B     DMLN40                                                           
         DROP  R5                                                               
                                                                                
DMLN40   DS    0H                  SET DISPLACEMENTS W/IN DISPLAY LINE          
         MVI   GOSUBN,SDLD#                                                     
         GOTO1 AGOSUB                                                           
         B     DMLN50                                                           
*                                                                               
** FORMAT DATA ONTO DISPLAY LINE **                                             
*                                                                               
DMLN50   DS    0H                  MARKET STUFF                                 
         CLI   NLINE,0             IF NOT @ TOP OF PAGE,                        
         BE    DMLN52                                                           
         CLC   PVMNUM,THISMNUM      SUPPRESS DISPLAY SAME MARKET                
         BE    DMLN60                                                           
                                                                                
DMLN52   DS    0H                                                               
         MVI   GOSUBN,IDB#                                                      
         GOTO1 AGOSUB                                                           
         LA    R6,DBLOCK1                                                       
         USING DBLOCKD,R6                                                       
         MVI   DBFUNCT,DBGETMK     GET MARKET INFORMATION                       
         MVC   DBSELRMK,THISMNUM                                                
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,DEMHOOK5,0,0                                
         DROP  R6                                                               
*                                                                               
         MVC   PVMNUM,THISMNUM                                                  
                                                                                
         DS    0H                   MARKET NUMBER                               
         LH    R3,DLMNUM                                                        
         LA    R3,LINE1(R3)                                                     
         ZICM  R1,THISMNUM,(3)                                                  
         EDIT  (R1),(LMNUM,(R3)),ALIGN=LEFT                                     
                                                                                
         DS    0H                   ALPHA MARKET CODE                           
         LH    R3,DLALFM                                                        
         LA    R3,LINE1(R3)                                                     
         MVC   0(LALFM,R3),THISALFM                                             
                                                                                
         DS    0H                   MARKET NAME                                 
         LH    R3,DLMNAM                                                        
         LA    R3,LINE1(R3)                                                     
         MVC   0(LMNAM,R3),THISMNAM                                             
*                                                                               
DMLN60   DS    0H                  STATION                                      
         CLI   NLINE,0             IF NOT @ TOP OF PAGE,                        
         BE    DMLN62                                                           
         CLC   PVSTTN,THISSTA       SUPPRESS DISPLAY SAME STATION               
         BE    DMLN70                                                           
                                                                                
DMLN62   DS    0H                                                               
         MVC   PVSTTN,THISSTA                                                   
         LH    R3,DLSTTN                                                        
         LA    R3,LINE1(R3)                                                     
         MVC   0(LSTTN,R3),THISSTA                                              
*                                                                               
DMLN70   DS    0H                  DAY & TIMES / DEMOS                          
         MVC   TEMPKSE,THISKDAY                                                 
         MVI   GOSUBN,GDAY#                                                     
         GOTO1 AGOSUB                                                           
                                                                                
         LH    R3,DLDAY                                                         
         LA    R3,LINE1(R3)                                                     
         MVC   0(LDAY,R3),DAYTIME       DAY                                     
                                                                                
         LH    R3,DLSTM                                                         
         LA    R3,LINE1(R3)                                                     
         MVC   0(LTIME,R3),DAYTIME+3    EARLIEST START TIME                     
                                                                                
         LH    R3,DLETM                                                         
         LA    R3,LINE1(R3)                                                     
         MVC   0(LTIME,R3),DAYTIME+8    LATEST   END   TIME                     
                                                                                
*                                                                               
*** DEMOS ***                                                                   
*                                                                               
DMLN80   DS    0H                                                               
         CLI   NDEMS,0              ANY DEMOS SPECIFIED?                        
         BE    DMLN90                NOPE                                       
                                                                                
         ZIC   R1,NDEMS                                                         
         STC   R1,DEMONDEM                                                      
         LA    R0,L'DEMS                                                        
         MR    R0,R0                                                            
         EXMVC R1,DEMODEMS,DEMS    MOVE DEMO REQUEST LIST TO MY AREA            
*                                                                               
         DS    0H                  INITIALIZE SPDEMLK BLOCK                     
         MVI   GOSUBN,IDML#                                                     
         GOTO1 AGOSUB                                                           
                                                                                
         LA    R6,SPDMLK_1                                                      
         USING SPDEMLKD,R6                                                      
         LA    R0,DEMHOOK4                                                      
         ST    R0,SPLKHOOK         SET LOOK-UP HOOK                             
*                                                                               
**** CALCULATE LEAD-IN DEMO ****                                                
*                                                                               
         BAS   RE,DOLINDEM                                                      
*                                                                               
**** CALCULATE PROGRAM'S DEMO ****                                              
*                                                                               
******                                                                          
* SET UP THE SPOT LINK FOR PRECISION                                            
         CLI   OPTDEC,C'0'                                                      
         BL    DMLN82                                                           
         XC    DUB,DUB                                                          
         LA    R0,WORK2                                                         
         ST    R0,DUB+4                                                         
         MVC   DUB+0(4),=C'SPOT'                                                
*****    MVI   SDBXF,C'S'                                                       
         MVI   GOSUBN,SDBX#                                                     
         GOTO1 AGOSUB                                                           
                                                                                
         ICM   R1,15,FULL                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBXTTID,R1                                                       
                                                                                
*   FOR NOW JUST FOR ZEN'S PRECISION SUPPORT FUDGE THE DISPLACEMENT             
*   UNTIL DBEXTRAD DSECT IS LIVE                                                
         LA    RE,DBXTRC2T                GIVE ZEN HIS DECMIAL                  
         SHI   RE,1                        SUPPORT                              
         MVC   0(1,RE),OPTDEC                                                   
*****    MVC   DBXTSCTL,OPTDEC                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         DROP  R1                                                               
******                                                                          
*                                                                               
DMLN82   DS    0H                                                               
         BAS   RE,DOPRGDEM                                                      
*                                                                               
**** CALCULATE LEAD-OUT DEMO ****                                               
*                                                                               
* SET UP THE SPOT LINK FOR PRECISION                                            
         CLI   OPTDEC,C'0'                                                      
         BL    DMLN86                                                           
         XC    DUB,DUB                                                          
         LA    R0,WORK2                                                         
         ST    R0,DUB+4                                                         
         MVC   DUB+0(4),=C'SPOT'                                                
*****    MVI   SDBXF,C'S'                                                       
         MVI   GOSUBN,SDBX#                                                     
         GOTO1 AGOSUB                                                           
                                                                                
         ICM   R1,15,FULL                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBXTTID,R1                                                       
                                                                                
*   FOR NOW JUST FOR ZEN'S PRECISION SUPPORT FUDGE THE DISPLACEMENT             
*   UNTIL DBEXTRAD DSECT IS LIVE                                                
         LA    RE,DBXTRC2T                GIVE ZEN HIS DECMIAL                  
         SHI   RE,1                        SUPPORT                              
         MVC   0(1,RE),OPTDEC                                                   
*****    MVC   DBXTSCTL,OPTDEC                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         DROP  R1                                                               
*                                                                               
DMLN86   DS    0H                                                               
         BAS   RE,DOLOTDEM                                                      
                                                                                
         DROP  R6                                                               
*                                                                               
**** DISPLAY THE DEMOS ****                                                     
*                                                                               
         LA    R3,DEMODEMS                                                      
         ZIC   R0,NDEMS                                                         
         SR    R1,R1                                                            
                                                                                
DMLN88A  DS    0H                                                               
         LH    R2,DLLID                                                         
         LA    R2,LINE1(R2)        R2-->LEAD-IN OUTPUT AREA                     
         LA    R4,DEMVLIN(R1)      R4-->LEAD-IN DEMO VALUE                      
*        BAS   RE,GETDEM                                                        
         MVI   GOSUBN,GETDEM#                                                   
         GOTO1 AGOSUB                                                           
                                                                                
         LH    R2,DLLOD                                                         
         LA    R2,LINE1(R2)        R2-->LEAD-OUT OUTPUT AREA                    
         LA    R4,DEMVLOUT(R1)     R4-->LEAD-OUT DEMO VALUE                     
*        BAS   RE,GETDEM                                                        
         MVI   GOSUBN,GETDEM#                                                   
         GOTO1 AGOSUB                                                           
                                                                                
         LH    R2,DLDEM                                                         
         LA    R2,LINE1(R2)        R2-->PROGRAM'S OUTPUT AREA                   
         LA    R4,DEMVPRG(R1)      R4-->PROGRAM'S DEMO VALUE                    
*        BAS   RE,GETDEM                                                        
         MVI   GOSUBN,GETDEM#                                                   
         GOTO1 AGOSUB                                                           
                                                                                
         DS    0H                                                               
         LA    R1,4(R1)                                                         
         LA    R3,3(R3)                                                         
         BCT   R0,DMLN88A                                                       
*                                                                               
DMLN90   DS    0H                                                               
         B     DEMLINEX                                                         
         EJECT                                                                  
DMLN100  DS    0H                  STEREO SESSION - USE SPECIAL FORMAT          
                                                                                
* First record is always a message record telling # of programs.                
                                                                                
         L     R2,ASTIOBUF                                                      
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         SR    R5,R5                                                            
         ICM   R5,7,TSAREC+1                                                    
         BZ    DMLN290                                                          
         DROP  R4                                                               
*                                                                               
         LA    R5,2(R5)                                                         
         USING TSDEMRCD,R5                                                      
         L     RE,ASTDMEXP                                                      
         MVC   STNDEMS,0(RE)                                                    
*                                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32   DEM32?                              
         BO    DMLN400                                                          
*                                                                               
         CLI   TDRRTYP,TDRRTPGI                                                 
         BE    DMLN110                                                          
         CLI   TDRRTYP,TDRRTMSG                                                 
         BE    DMLN200                                                          
         CLI   TDRRTYP,TDRRTHDR                                                 
         BE    DMLN220                                                          
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
** RECORD CONTAINS PROGRAM INFORMATION **                                       
*                                                                               
DMLN110  DS    0H                                                               
         USING TSDEMRCD,R5                                                      
         MVC   THISMNUM,TDRMMNUM                                                
         MVC   THISSTA,TDRMSTAT                                                 
         CLI   OPTLIST,C'S'                                                     
         BNE   *+16                                                             
         MVC   THISSTA,TDRSSTAT                                                 
         MVC   THISMNUM,TDRSMNUM                                                
                                                                                
         MVC   THISKDAY,TDRDAY                                                  
         MVC   THISSQH,TDRSQH                                                   
         MVC   THISEQH,TDREQH                                                   
         DROP  R5                                                               
*                                                                               
         DS    0H                                                               
         MVC   TEMPKSE,THISKDAY    SET DAY, EARLIEST START, AND                 
         MVI   GOSUBN,GDAY#                                                     
         GOTO1 AGOSUB               LATEST END IN  DAYTIME                      
*                                                                               
         LA    R3,STPRVOVL                                                      
         USING SPVOVALD,R3                                                      
*                                                                               
         DS    0H                  OUTPUT MARKET NUMBER                         
         CLC   SPOVMNUM,THISMNUM                                                
         BE    DMLN122                                                          
                                                                                
         MVI   GOSUBN,IDB#                                                      
         GOTO1 AGOSUB                                                           
         LA    R6,DBLOCK1                                                       
         USING DBLOCKD,R6                                                       
         MVI   DBFUNCT,DBGETMK     GET MARKET INFORMATION                       
         MVC   DBSELRMK,THISMNUM                                                
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,DEMHOOK5,0,0                                
         DROP  R6                                                               
                                                                                
DMLN122  DS    0H                                                               
         CLC   SPOVMNUM,THISMNUM                                                
         BC    0,DMLN125                                                        
                                                                                
         ZICM  R1,THISMNUM,(3)                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STCM  R1,3,SPOVMNUM                                                    
         EDIT  (R1),(LMNUM,(R2)),ALIGN=LEFT                                     
         AR    R2,R0                                                            
                                                                                
DMLN125  DS    0H                  OUTPUT ALPHA MARKET                          
         BAS   RE,INSSEP                                                        
         CLC   SPOVALFM,THISALFM                                                
         BC    0,DMLN130                                                        
                                                                                
         MVC   SPOVALFM,THISALFM                                                
         MVC   0(LALFM,R2),THISALFM                                             
         LR    R0,R2                                                            
         LA    R2,LALFM-1(R2)                                                   
         BAS   RE,TRUNCSPC                                                      
         LA    R2,1(R2)                                                         
                                                                                
DMLN130  DS    0H                  OUTPUT MARKET NAME                           
         BAS   RE,INSSEP                                                        
         CLC   SPOVMNAM,THISMNAM                                                
         BC    0,DMLN135                                                        
                                                                                
         MVC   SPOVMNAM,THISMNAM                                                
         MVC   0(LMNAM,R2),THISMNAM                                             
         LR    R0,R2                                                            
         LA    R2,LMNAM-1(R2)                                                   
         BAS   RE,TRUNCSPC                                                      
         LA    R2,1(R2)                                                         
                                                                                
DMLN135  DS    0H                  OUTPUT STATION                               
         BAS   RE,INSSEP                                                        
         CLC   SPOVSTA,THISSTA                                                  
         BC    0,DMLN140                                                        
                                                                                
         MVC   SPOVSTA,THISSTA                                                  
         MVC   0(LSTTN,R2),THISSTA                                              
         LA    R2,LSTTN(R2)                                                     
                                                                                
DMLN140  DS    0H                  OUTPUT DAY                                   
         BAS   RE,INSSEP                                                        
         CLC   SPOVDAY,THISKDAY                                                 
         BC    0,DMLN143                                                        
                                                                                
         MVC   SPOVDAY,THISKDAY                                                 
         MVC   0(3,R2),DAYTIME                                                  
         LA    R2,3(R2)                                                         
                                                                                
DMLN143  DS    0H                  OUTPUT EARLIEST START TIME                   
         BAS   RE,INSSEP                                                        
         CLC   SPOVSQH,THISSQH                                                  
         BC    0,DMLN146                                                        
                                                                                
         MVC   SPOVSQH,THISSQH                                                  
         MVC   0(5,R2),DAYTIME+3                                                
         LR    R0,R2                                                            
         LA    R2,5-1(R2)                                                       
         BAS   RE,TRUNCSPC                                                      
         LA    R2,1(R2)                                                         
                                                                                
DMLN146  DS    0H                  OUTPUT LATEST END TIME                       
         BAS   RE,INSSEP                                                        
         CLC   SPOVEQH,THISEQH                                                  
         BC    0,DMLN150                                                        
                                                                                
         MVC   SPOVEQH,THISEQH                                                  
         MVC   0(5,R2),DAYTIME+8                                                
         LR    R0,R2                                                            
         LA    R2,5-1(R2)                                                       
         BAS   RE,TRUNCSPC                                                      
         LA    R2,1(R2)                                                         
         DROP  R3                                                               
                                                                                
DMLN150  DS    0H                  OUTPUT DEMO VALUES,                          
         L     R3,ASTDMEXP                                                      
         CLI   0(R3),0              IF THERE ANY TO OUTPUT                      
         BE    DMLN190                                                          
                                                                                
         DS    0H                                                               
         LA    R3,1(R3)            R3-->"MASTER" DEMO LIST                      
                                                                                
         MVI   GOSUBN,IDML#                                                     
         GOTO1 AGOSUB                                                           
         LA    R6,SPDMLK_1         R6-->SPDEMLK BLOCK                           
         USING SPDEMLKD,R6                                                      
         LA    R0,DEMHOOK4                                                      
         ST    R0,SPLKHOOK         SET HOOK ROUTINE FOR LOOK-UP                 
                                                                                
DMLN155  DS    0H                  R3-->NEXT "SET" OF DEMOS TO GET              
         LA    R0,MAXDEMS                                                       
         SR    R1,R1                                                            
         L     RF,SPLKALST                                                      
         XC    0(MAXDEMS*3+1,RF),0(RF)                                          
                                                                                
DMLN157  DS    0H                  BUILD DEMO LIST FOR SPDEMLK                  
         CLI   0(R3),XFF                                                        
         BE    DMLN157X                                                         
         MVC   0(L'DEMODEMS,RF),0(R3)                                           
         LA    R1,1(R1)                                                         
         LA    R3,3(R3)                                                         
         LA    RF,L'DEMODEMS(RF)                                                
         BCT   R0,DMLN157                                                       
DMLN157X MVI   0(RF),XFF           PUT DELIMITER @ END OF LIST                  
         STC   R1,DEMONDEM                                                      
*                                                                               
         DS    0H                  GET DEMOS                                    
         BAS   RE,DOLINDEM          DO LEAD-IN DEMOS                            
         BAS   RE,DOPRGDEM          DO PROGRAM'S DEMOS                          
         BAS   RE,DOLOTDEM          DO LEAD-OUT DEMOS                           
         DROP  R6                                                               
*                                                                               
         DS    0H                  MOVE DEMO VALUES TO OUTPUT BUFFER            
         LR    R0,R3                SAVE R3 IN R0                               
         LA    R3,DEMODEMS                                                      
         SR    R1,R1               R1 INDEXES INTO DEMO VALUES                  
                                                                                
DMLN162  DS    0H                                                               
         CLI   0(R3),XFF                                                        
         BE    DMLN169                                                          
                                                                                
         BAS   RE,INSSEP                                                        
         LA    R4,DEMVLIN(R1)       MOVE LEAD-IN DEMO VALUE                     
*        BAS   RE,GETDEM                                                        
         MVI   GOSUBN,GETDEM#                                                   
         GOTO1 AGOSUB                                                           
         A     R2,FULL               FULL=L(FORMATTED DEMO VALUE)               
                                                                                
         BAS   RE,INSSEP                                                        
         LA    R4,DEMVPRG(R1)       MOVE PROGRAM DEMO VALUE                     
*        BAS   RE,GETDEM                                                        
         MVI   GOSUBN,GETDEM#                                                   
         GOTO1 AGOSUB                                                           
         A     R2,FULL               FULL=L(FORMATTED DEMO VALUE)               
                                                                                
         BAS   RE,INSSEP                                                        
         LA    R4,DEMVLOUT(R1)      MOVE LEAD-OUT DEMO VALUE                    
*        BAS   RE,GETDEM                                                        
         MVI   GOSUBN,GETDEM#                                                   
         GOTO1 AGOSUB                                                           
         A     R2,FULL               FULL=L(FORMATTED DEMO VALUE)               
                                                                                
         LA    R1,4(R1)                                                         
         LA    R3,L'DEMODEMS(R3)                                                
         B     DMLN162                                                          
                                                                                
DMLN169  DS    0H                                                               
         LR    R3,R0               RESTORE R3                                   
*                                                                               
         DS    0H                                                               
         CLI   0(R3),XFF                                                        
         BNE   DMLN155                                                          
         B     DMLN190                                                          
                                                                                
DMLN190  DS    0H                                                               
         B     DMLN280                                                          
*                                                                               
** OUTPUT INFORMATION MESSAGE **                                                
*                                                                               
DMLN200  DS    0H                                                               
         SH    R5,=H'2'                                                         
         ZICM  R1,0(R5),(3)        R1 = L(TSAR DEMO RECORD)                     
         AH    R5,=H'2'                                                         
                                                                                
         USING TSDEMRCD,R5                                                      
         LA    R0,TDRKEYL+2+1                                                   
         SR    R1,R0                                                            
         EXMVC R1,0(R2),TDRDATA                                                 
         LA    R2,1(R2,R1)                                                      
         DROP  R5                                                               
                                                                                
         B     DMLN280                                                          
*                                                                               
** OUTPUT COLUMN HEADERS **                                                     
*                                                                               
DMLN220  DS    0H                                                               
                                                                                
         DS    0H                  GET DEMO NAMES FIRST                         
         L     R3,ASTDMEXP                                                      
         ZICM  R0,0(R3),(1)        ANY DEMOS INPUTTED?                          
         BZ    DMLN225              NOPE, DON'T OUTPUT HDRS FOR DEMOS           
         STC   R0,STNDEMS                                                       
                                                                                
         DS    0H                  GET DEMO NAMES                               
         MVI   GOSUBN,IDB#          INITIALIZE DBLOCK (FOR DEMOCON)             
         GOTO1 AGOSUB                                                           
                                                                                
         LA    R4,DBLOCK1                                                       
         USING DBLOCKD,R4                                                       
         MVC   DBFILE,DBFIL         OVERRIDE DEFAULT FILE                       
         XC    DBAREC,DBAREC        DEMOCON DOESN'T NEED THIS,                  
         XC    DBSELBK,DBSELBK       NOR THIS                                   
         LA    R3,1(R3)             R3-->START OF DEMO LIST                     
         GOTO1 VDEMOCON,DMCB,((R0),(R3)),(7,AIOAREA),(C'S',DBLOCKD),0           
         DROP  R4                                                               
                                                                                
DMLN225  DS    0H                                                               
         USING TSDEMRCD,R5                                                      
         CLI   TDRRTYP+1,2                                                      
         BE    DMLN230                                                          
         CLI   TDRRTYP+1,3                                                      
         BE    DMLN235                                                          
         DC    H'0'                                                             
*                                                                               
DMLN230  DS    0H                  OUTPUT HEADLINE #2                           
                                                                                
         DS    0H                  MARKET NUMBER                                
         MVC   0(L'HEADNUM2,R2),HEADNUM2                                        
         LA    R2,L'HEADNUM2(R2)                                                
                                                                                
         DS    0H                  ALPHA MARKET                                 
         BAS   RE,INSSEP                                                        
         MVC   0(L'HEADALF2,R2),HEADALF2                                        
         LA    R2,L'HEADALF2(R2)                                                
                                                                                
         DS    0H                  MARKET NAME                                  
         BAS   RE,INSSEP                                                        
         MVC   0(L'HEADNAM2A,R2),HEADNAM2A                                      
         LA    R2,L'HEADNAM2A(R2)                                               
                                                                                
         DS    0H                  STATION                                      
         BAS   RE,INSSEP                                                        
         MVC   0(L'HEADSTA2,R2),HEADSTA2                                        
         LA    R2,L'HEADSTA2(R2)                                                
                                                                                
         DS    0H                  DAY                                          
         BAS   RE,INSSEP                                                        
         MVC   0(L'HEADDAY2,R2),HEADDAY2                                        
         LA    R2,L'HEADDAY2(R2)                                                
                                                                                
         DS    0H                  EARLIEST START (TIME)                        
         BAS   RE,INSSEP                                                        
         MVC   0(L'HEADSTM2,R2),HEADSTM2                                        
         LA    R2,L'HEADSTM2(R2)                                                
                                                                                
         DS    0H                  LATEST END (TIME)                            
         BAS   RE,INSSEP                                                        
         MVC   0(L'HEADETM2,R2),HEADETM2                                        
         LA    R2,L'HEADETM2(R2)                                                
                                                                                
         DS    0H                  DEMO NAMES                                   
         ZICM  R0,STNDEMS,(1)      ANY DEMOS INPUTTED?                          
         BZ    DMLN234              NOPE, DON'T OUTPUT HDRS FOR DEMOS           
                                                                                
         DS    0H                  MOVE NAMES TO STEREO I/O BUFFER              
         L     RF,AIOAREA           RF-->DEMO NAMES                             
DMLN232  BAS   RE,INSSEP                                                        
         MVC   0(L'HEADLEAD,R2),HEADLEAD                                        
         LA    R2,L'HEADLEAD(R2)                                                
                                                                                
         BAS   RE,INSSEP                                                        
         MVC   0(7,R2),0(RF)                                                    
         LA    R2,7(R2)                                                         
         LA    RF,12(RF)                                                        
                                                                                
         BAS   RE,INSSEP                                                        
         MVC   0(L'HEADLEAD,R2),HEADLEAD                                        
         LA    R2,L'HEADLEAD(R2)                                                
         BCT   R0,DMLN232                                                       
*                                                                               
DMLN234  DS    0H                                                               
         B     DMLN280                                                          
*                                                                               
DMLN235  DS    0H                  OUTPUT HEADLINE #3                           
                                                                                
         DS    0H                  MARKET NUMBER                                
         BAS   RE,INSSPC                                                        
                                                                                
         DS    0H                  ALPHA MARKET                                 
         BAS   RE,INSSEP                                                        
         MVC   1(L'HEADALF3,R2),HEADALF3                                        
         LA    R2,L'HEADALF3+1(R2)                                              
                                                                                
         DS    0H                  MARKET NAME                                  
         BAS   RE,INSSEP                                                        
         BAS   RE,INSSPC                                                        
                                                                                
         DS    0H                  STATION                                      
         BAS   RE,INSSEP                                                        
         BAS   RE,INSSPC                                                        
                                                                                
         DS    0H                  DAY                                          
         BAS   RE,INSSEP                                                        
         BAS   RE,INSSPC                                                        
                                                                                
         DS    0H                  EARLIEST START (TIME)                        
         BAS   RE,INSSEP                                                        
         MVC   1(L'HEADSTM3,R2),HEADSTM3                                        
         LA    R2,L'HEADSTM3+1(R2)                                              
                                                                                
         DS    0H                  LATEST END (TIME)                            
         BAS   RE,INSSEP                                                        
         MVC   1(L'HEADETM3,R2),HEADETM3                                        
         LA    R2,L'HEADETM3+1(R2)                                              
                                                                                
         DS    0H                  DEMO NAMES                                   
         ZICM  R0,STNDEMS,(1)      ANY DEMOS INPUTTED?                          
         BZ    DMLN239              NOPE, DON'T OUTPUT HDRS FOR DEMOS           
                                                                                
         DS    0H                  MOVE NAMES TO STEREO I/O BUFFER              
         L     RF,AIOAREA           RF-->DEMO NAMES                             
DMLN237  BAS   RE,INSSEP                                                        
         MVC   1(L'HEADLIN3,R2),HEADLIN3                                        
         LA    R2,L'HEADLIN3+1(R2)                                              
                                                                                
         BAS   RE,INSSEP                                                        
         MVC   1(5,R2),7(RF)                                                    
         LA    R2,5+1(R2)                                                       
         LA    RF,12(RF)                                                        
                                                                                
         BAS   RE,INSSEP                                                        
         MVC   1(L'HEADLOT3,R2),HEADLOT3                                        
         LA    R2,L'HEADLOT3+1(R2)                                              
         BCT   R0,DMLN237                                                       
*                                                                               
DMLN239  DS    0H                                                               
         B     DMLN280                                                          
*                                                                               
DMLN280  DS    0H                                                               
         MVI   0(R2),STSPIKEY      DELIMIT STUFF IN OUTPUT BUFFER               
         LA    R2,1(R2)                                                         
         B     DMLN290                                                          
*                                                                               
DMLN290  DS    0H                                                               
         S     R2,ASTIOBUF                                                      
         STH   R2,IODATALN                                                      
         B     DEMLINEX                                                         
*                                                                               
DMLN400  DS    0H                                                               
         MVI   GOSUBN,D32DL#                                                    
         GOTO1 AGOSUB                                                           
         B     DEMLINEX                                                         
*                                                                               
DEMLINEX B     EXIT                                                             
                                                                                
                                                                                
* Little routine to insert field separator into output buffer.                  
*  R2-->next output location in the output buffer.                              
                                                                                
INSSEP   DS    0H                                                               
         MVI   0(R2),STSPOLST                                                   
         LA    R2,1(R2)                                                         
         BR    RE                                                               
                                                                                
                                                                                
* Little routine to insert a space into output buffer.                          
*  R2-->next output location in the output buffer.                              
                                                                                
INSSPC   DS    0H                                                               
         MVI   0(R2),C' '                                                       
         LA    R2,1(R2)                                                         
         BR    RE                                                               
                                                                                
                                                                                
* Little routine to help truncate right-padded spaces.                          
* At entry, R0-->1st byte of string,                                            
*           R2-->last byte of string.                                           
* At exit,  R2-->last "printable" character in string.                          
                                                                                
TRUNCSPC DS    0H                                                               
         CR    R0,R2                                                            
         BER   RE                                                               
         CLI   0(R2),C' '                                                       
         BHR   RE                                                               
         BCT   R2,*-10                                                          
         BR    RE                                                               
         EJECT                                                                  
*========================== DO LEAD-IN DEMOS =========================*         
                                                                                
* At entry,                                                                     
*   R6-->SPDEMLKD.                                                              
                                                                                
         DS    0H                                                               
DOLINDEM NTR1                                                                   
         USING SPDEMLKD,R6                                                      
                                                                                
         MVI   CDMODE,C'I'         SET MODE FOR CALC'NG LEAD-IN DEMO            
         LA    R0,DEMVLIN                                                       
         ST    R0,ADEMVAL          SET STORAGE TO HOLD DEMO VALUES              
                                                                                
         MVI   GOSUBN,QTM#         CONVERTING QHRS TO MIL TIMES                 
         ZIC   R1,THISSQH                                                       
         STC   R1,TEMPQHR                                                       
         GOTO1 AGOSUB                                                           
         MVC   SPLKTIM+2(2),TEMPMTIM  SET END  TIME                             
                                                                                
         BCTR  R1,0                                                             
         STC   R1,TEMPQHR                                                       
         GOTO1 AGOSUB                                                           
         MVC   SPLKTIM(2),TEMPMTIM    SET START TIME                            
                                                                                
         MVC   SPLKWKN,BKS+2          SET WEEKS                                 
                                                                                
         DS    0H                                                               
         BAS   RE,CALCDEMO         GO GET LEAD-IN DEMOS                         
                                                                                
         B     EXIT                                                             
         DROP  R6                                                               
                                                                                
                                                                                
*                                                                               
*========================= DO PROGRAM'S DEMOS ========================*         
                                                                                
* At entry,                                                                     
*   R6-->SPDEMLKD.                                                              
                                                                                
         DS    0H                                                               
DOPRGDEM NTR1                                                                   
         USING SPDEMLKD,R6                                                      
                                                                                
         MVI   CDMODE,C'P'                                                      
         LA    R0,DEMVPRG          USE THIS TO ACCUMULATE DEMO VALUES           
         ST    R0,ADEMVAL                                                       
*                                                                               
         MVC   SPLKTIM(2),SPLKTIM+2   START TIME IS LEAD-IN'S END TIME          
                                                                                
         ZIC   R1,THISEQH                                                       
         LA    R1,1(R1)                                                         
         STC   R1,TEMPQHR                                                       
         MVI   GOSUBN,QTM#                                                      
         GOTO1 AGOSUB                                                           
         MVC   SPLKTIM+2(2),TEMPMTIM  SET END   TIME                            
*                                                                               
         CLI   BKS+2,0             IF NO WEEKS SPECIFIED IN REQUEST,            
         BNE   *+8                                                              
         MVI   SPLKWKN,X'FF'        DEFAULT TO ALL WEEKS                        
*                                                                               
         DS    0H                                                               
         BAS   RE,CALCDEMO         GO GET PROGRAM'S DEMOS                       
                                                                                
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
                                                                                
                                                                                
*========================= DO LEAD-OUT DEMOS =========================*         
                                                                                
* At entry,                                                                     
*   R6-->SPDEMLKD.                                                              
                                                                                
         DS    0H                                                               
DOLOTDEM NTR1                                                                   
         USING SPDEMLKD,R6                                                      
                                                                                
         MVI   CDMODE,C'O'                                                      
         LA    R0,DEMVLOUT         USE THIS TO ACCUMULATE DEMO VALUES           
         ST    R0,ADEMVAL                                                       
*                                                                               
         MVC   SPLKTIM(2),SPLKTIM+2   START TIME IS PROG'S LATEST END           
                                                                                
         ZIC   R1,THISEQH                                                       
         LA    R1,2(R1)                                                         
         STC   R1,TEMPQHR                                                       
         MVI   GOSUBN,QTM#                                                      
         GOTO1 AGOSUB                                                           
         MVC   SPLKTIM+2(2),TEMPMTIM  SET END   TIME                            
*                                                                               
         MVC   SPLKWKN,BKS+2          SET WEEKS                                 
*                                                                               
         DS    0H                                                               
         BAS   RE,CALCDEMO         GO GET PROGRAM'S DEMOS                       
                                                                                
         B     EXIT                                                             
         DROP  R6                                                               
                                                                                
                                                                                
         EJECT                                                                  
*========================== CALCULATE DEMOS ==========================*         
                                                                                
* At entry,                                                                     
*  R6-->SPDEMLKD,                                                               
*  ADEMVAL = A(to put demo values),                                             
*  CDMODE is set to the set of demos to calculate.                              
                                                                                
         DS    0H                                                               
CALCDEMO NTR1                                                                   
         USING SPDEMLKD,R6                                                      
*                                                                               
         L     R0,ADEMVAL                                                       
         LA    R1,MAXDEMS*4                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR AREA FOR DEMO VALUES                   
         L     R0,SPLKAVAL                                                      
         LA    R1,(MAXDEMS*2)*4                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR AREA FOR DEMO VALUES                   
         XC    ACCUFCTR,ACCUFCTR   CLEAR FACTOR AS WELL                         
*                                                                               
         DS    0H                  GO LOOK-UP DEMOS                             
         MVC   SPLKUID,USERID                                                   
         GOTO1 VSPDEMLK,DMCB,(X'FF',SPDEMLKD),0,0,0,0,0                         
         CLI   0(R1),0                                                          
         BNE   CDEMOX                                                           
*                                                                               
         DS    0H                  UN-FACTOR ACCUMULATED DEMO VALUES            
         OC    ACCUFCTR,ACCUFCTR                                                
         BZ    CDEMOX                                                           
                                                                                
         ZIC   R0,DEMONDEM                                                      
         L     R1,ADEMVAL                                                       
         MVI   GOSUBN,DIV#                                                      
         MVC   DIVISOR,ACCUFCTR    SET DIVISOR                                  
                                                                                
CDEMO22  DS    0H                                                               
         XC    DIVIDEND,DIVIDEND                                                
         MVC   DIVIDEND+4(4),0(R1) SET DIVIDEND                                 
         GOTO1 AGOSUB                                                           
         MVC   0(4,R1),QUOTIENT                                                 
         LA    R1,4(R1)                                                         
         BCT   R0,CDEMO22                                                       
         B     CDEMOX                                                           
*                                                                               
CDEMOX   DS    0H                                                               
         B     EXIT                                                             
                                                                                
         DROP  R6                                                               
         EJECT                                                                  
*======= ROUTINE TO PROCESS A DEMO RECORD RETURNED FROM DEMAND =======*         
                                                                                
* At entry,                                                                     
*   R6-->SPDEMLK.                                                               
                                                                                
DEMHOOK4 DS    0H                                                               
D32HK4   NTR1                                                                   
         USING SPDEMLKD,R6                                                      
                                                                                
         L     R5,SPLKDBLK                                                      
         USING DBLOCKD,R5                                                       
*                                                                               
         CLI   CDMODE,C'P'         IF CALCULATING PROGRAM'S DEMOS,              
         BNE   *+12                                                             
         BAS   RE,MTCHPRGN          MAKE SURE QH-ELEM HAS PROG NUM              
         BNE   DMHK4X                                                           
*                                                                               
         DS    0H                  DROP SVI FACTORS                             
         L     R1,SPLKAVAL          R1-->RETURNED DEMO VALUES                   
         LA    RF,8(R1)             RF-->SECOND DEMO VALUE ENTRY                
         ZIC   R0,DEMONDEM                                                      
         MVC   4(4,R1),0(RF)                                                    
         LA    R1,4(R1)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,*-14                                                          
*                                                                               
         L     RE,SPLKAVAL                                                      
         L     RF,ADEMVAL                                                       
         ZIC   R0,NDEMS                                                         
                                                                                
DMHK410  DS    0H                                                               
         L     R1,0(RE)                                                         
         A     R1,0(RF)                                                         
         ST    R1,0(RF)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,DMHK410                                                       
                                                                                
         ZICM  R1,DBFACTOR,(3)                                                  
         A     R1,ACCUFCTR                                                      
         ST    R1,ACCUFCTR                                                      
                                                                                
         B     DMHK4X                                                           
*                                                                               
DMHK4X   DS    0H                  RETURN TO DEMAND                             
         B     EXIT                                                             
                                                                                
         DROP  R5,R6                                                            
         EJECT                                                                  
*===== ROUTINE TO PROCESS A MKT NAME RECORD RETURNED FROM DEMAND =====*         
                                                                                
* Hook is used to extract market info.                                          
* At entry,                                                                     
*  R6-->DBLOCKD.                                                                
                                                                                
DEMHOOK5 DS    0H                                                               
HK5      NTR1                                                                   
         USING DBLOCKD,R6                                                       
*                                                                               
         CLI   DBRECTYP,DBRECMK    MAKE SURE WE HAVE CORRECT RECD TYPE          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DS    0H                  EXTRACT MARKET INFORMATION                   
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'MNA',DBLOCKD,WORK,0,0,0                        
         MVC   THISMNAM,WORK+2      MARKET NAME                                 
                                                                                
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'NAMKT',DBLOCKD,WORK,0,0,0                      
         MVC   THISALFM,WORK        ALPHA MARKET                                
*                                                                               
DMHK2X   DS    0H                  RETURN TO DEMAND                             
*                                                                               
         B     EXIT                                                             
                                                                                
         DROP  R6                                                               
         EJECT                                                                  
*&&DO                                                                           
*=================== ROUTINE TO EDIT A DEMO VALUE ====================*         
                                                                                
* At entry,                                                                     
*   R3=A(3-byte demo expression),                                               
*   R4=A(4-byte demo value),                                                    
*   R2=A(7-byte output demo value).                                             
                                                                                
GETDEM   NTR1                                                                   
         L     R0,0(R4)            R0=DEMO VALUE                                
                                                                                
         MVC   DUB(1),DBFIL                                                     
         MVC   DUB+1(1),1(R3)                                                   
         MVI   DUB+2,0                                                          
         L     R1,AEDITTAB         R1=A(EDIT TABLE) 1 DECIMAL DEFAULT           
*                                                                               
*                                                                               
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
*                                                                               
         TM    DEMFLAG1,DF1STERO   IS THIS A STEREO SESSION?                    
         BO    GETDEM9              YES, USE ANOTHER EDIT FORMAT                
                                                                                
         TM    DUB+2,X'02'         TEST EDIT TO 2 DECIMALS                      
         BO    GETDEM6                                                          
         TM    DUB+2,X'01'         TEST EDIT TO 1 DECIMAL                       
         BO    GETDEM8                                                          
         EDIT  (R0),(7,0(R2))                                                   
         B     GETDEMX                                                          
GETDEM6  EDIT  (R0),(7,0(R2)),2,ZERO=BLANK                                      
         B     GETDEMX                                                          
GETDEM8  EDIT  (R0),(7,0(R2)),1,ZERO=BLANK                                      
         B     GETDEMX                                                          
*                                                                               
GETDEM9  DS    0H                  EDITTING DEMOS FOR STEREO                    
         TM    DUB+2,X'02'          TEST EDIT TO 2 DECIMALS                     
         BO    GETDEM9B                                                         
         TM    DUB+2,X'01'          TEST EDIT TO 1 DECIMAL                      
         BO    GETDEM9C                                                         
GETDEM9A EDIT  (R0),(7,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                           
         B     GETDEM9X                                                         
GETDEM9B EDIT  (R0),(7,0(R2)),2,ALIGN=LEFT,ZERO=NOBLANK                         
         B     GETDEM9X                                                         
GETDEM9C EDIT  (R0),(7,0(R2)),1,ALIGN=LEFT,ZERO=NOBLANK                         
         B     GETDEM9X                                                         
GETDEM9X ST    R0,FULL                                                          
         B     GETDEMX                                                          
*                                                                               
GETDEMX  B     EXIT                                                             
*&&                                                                             
***********************************************************************         
         TITLE 'DEDEM0A - $DEM MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (A+        
               PMODE=STERTASK)'                                                 
***********************************************************************         
*=============== MISCELLANEOUS TASKS FOR STEREO SESSION ==============*         
                                                                                
DEMSTTSK DS    0H                                                               
         CLI   STMODE,STMMIDQ                                                   
         BE    DMSTSK20                                                         
         CLI   STMODE,STMOUTQ                                                   
         BE    DMSTSK50                                                         
         B     DMSTSKX                                                          
*                                                                               
DMSTSK20 DS    0H                                                               
         MVI   BYTE,OF1RELSE+OF1ITSR   RELEASE TDRs & RE-INIT TSAR              
         L     RE,ASTDMEXP                                                      
         CLI   0(RE),0             CHECK IF ANY DEMOS INPUTTED                  
         BE    DMSTSK25            IF NONE, RELEASE AND INIT                    
                                                                                
         LH    R2,DSPRCTDM         WANT DEMOS ENTRY                             
         LA    R2,REQCTLTB(R2)                                                  
         USING RCTDSECT,R2                                                      
         CLI   RCTFROM,1           IF NOT FORMATTING 1ST DEMO AGAIN,            
         BE    DMSTSK25                                                         
         MVI   BYTE,0               DON'T RELEASE AND INIT YET                  
         DROP  R2                                                               
                                                                                
DMSTSK25 DS    0H                                                               
         OC    OUPTFLG1,BYTE                                                    
         B     DMSTSKX                                                          
                                                                                
                                                                                
DMSTSK50 DS    0H                  STMODE = STMOUTQ                             
         TM    OUPTFLG1,OF1RELSE                                                
         BO    DMSTSK60                                                         
         B     DMSTSKX                                                          
*                                                                               
DMSTSK60 DS    0H                                                               
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         TM    TSERRS,TSEEOF                                                    
         BO    DMSTSK70                                                         
         B     DMSTSKX                                                          
         DROP  R1                                                               
*                                                                               
DMSTSK70 DS    0H                  ALL TSAR DEMO RECDS JUST RELEASED            
         L     RE,ASTDMEXP         CLEAR DEMO EXPRESSIONS BUFFER                
         CLI   0(RE),0              IF THERE WERE DEMOS INPUTTED                
         BE    DMSTSK75                                                         
                                                                                
         LH    RF,=Y(STDMEXPX-STDEMEXP)  RF = L(DESTINATION)                    
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                     RE-->DESTINATION                       
         B     DMSTSK75                                                         
                                                                                
DMSTSK75 DS    0H                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BNO   DMSTSK75X                                                        
         CLI   RESUMAPP,C'Y'       IF RESUMING FROM APPLIC BREAK,               
         BNE   *+12                                                             
         MVI   RESUMAPP,0           RESET FLAG AND                              
         B     DMSTSK75X            DON'T ASK FOR MF TRNSCTN BREAK              
         TM    OUPTFLG1,OF1RCTX    IF ALL COMBO OF INPUT EXHAUSTED,             
         BNZ   DMSTSK75X            DON'T ASK FOR MF TRNSCTN BREAK              
                                                                                
         LH    R2,DSPRCTBK         WANT BOOKS ENTRY                             
         LA    R2,REQCTLTB(R2)                                                  
         USING RCTDSECT,R2                                                      
         CLI   RCTFROM,1           IF FORMATTING 1ST BOOK AGAIN,                
         BH    *+8                                                              
         OI    BRKFLAG1,BF1APPLC    BREAK MAINFRAME TRANSACTION                 
         DROP  R2                                                               
DMSTSK75X EQU  *                                                                
*                                                                               
         B     DMSTSKX                                                          
                                                                                
                                                                                
DMSTSKX  DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
         TITLE 'DEDEM0A - $DEM MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (A+        
               PMODE=APMBREAK)'                                                 
***********************************************************************         
*============ TASKS TO DO FOR MAINFRAME TRANSACTION BREAK ============*         
                                                                                
DEMBREAK DS    0H                                                               
         DS    0H                  SEE WHO CALLED FOR A BREAK                   
         TM    BRKFLAG1,BF1FALNK    FALINK?                                     
         BO    DMBRKF                YEP                                        
         TM    BRKFLAG1,BF1APPLC    APPLICATION?                                
         BO    DMBRKA                YEP                                        
         B     DMBRKXE                                                          
                                                                                
                                                                                
*                                                                               
** BREAK CALLED BY FALINK **                                                    
*                                                                               
DMBRKF   DS    0H                                                               
         B     DMBRKXE                                                          
                                                                                
                                                                                
*                                                                               
** BREAK CALLED BY APPLICATION **                                               
*                                                                               
DMBRKA   DS    0H                                                               
*^^GYL 12/17/98 - This code is put here because the PC FALINK can not           
*^^                handle breaks yet                                            
*^^TEMP                                                                         
         MVI   FERN,36             PLEASE REQUEST 1 PROGRAM AT A TIME           
         LA    RE,DMBRKA19                                                      
         NTR1                                                                   
         L     RB,ABASE                                                         
         L     RA,BBASE                                                         
         L     R7,CBASE                                                         
         L     R6,DBASE                                                         
         L     RF,AERROR0                                                       
         BR    RF                                                               
DMBRKA19 EQU   *                                                                
         OI    MISCFLG1,MF1ERROR                                                
         B     DMBRKXL                                                          
         B     DMBRKXE                                                          
                                                                                
                                                                                
*                                                                               
DMBRKXH  DS    0H                                                               
         J     EXITH                                                            
*                                                                               
DMBRKXL  DS    0H                                                               
         J     EXITL                                                            
*                                                                               
DMBRKXE  DS    0H                                                               
         J     EXITE                                                            
                                                                                
*                                                                               
DMBRKX   DS    0H                                                               
         J     EXIT                                                             
***********************************************************************         
         TITLE 'DEDEM0A - $DEM MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (A+        
               PMODE=APMRESUM)'                                                 
***********************************************************************         
*============ TASKS TO DO FOR MAINFRAME TRANSACTION RESUME ===========*         
                                                                                
DEMRESUM DS    0H                                                               
         TM    RSMFLAG1,RF1APPLC   RESUMING FOR APPLIC BREAK?                   
         BO    DMRSMA                                                           
         B     DMRSMX                                                           
                                                                                
*                                                                               
** RESUME NEEDED FOR APPLICATION BREAK                                          
*                                                                               
DMRSMA   DS    0H                                                               
         MVI   RESUMAPP,C'Y'                                                    
         B     DMRSMX                                                           
                                                                                
*                                                                               
DMRSMX   DS    0H                                                               
         J     EXIT                                                             
***********************************************************************         
         TITLE 'DEDEM0A - $DEM MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM'           
***********************************************************************         
*================== MATCH TO REQUEST PROGRAM NUMBER ==================*         
                                                                                
* Routine is used to see if DBAQUART points to a quarter-hour element           
*  corresponding to the requested program number.                               
* At entry,                                                                     
*   R5     -->DBLOCKD,                                                          
*   STAS(4) = requested program number.                                         
* At exit,                                                                      
*   CC equal if it matches,                                                     
*   CC not equal if it doesn't match.                                           
                                                                                
         DS    0H                                                               
MTCHPRGN NTR1                                                                   
         USING DBLOCKD,R5                                                       
                                                                                
         L     RF,DBAQUART                                                      
         USING QHELEM,RF                                                        
         CLI   QHCODE,QHCODEQ      CHECK IF QH-ELEMENT FIRST                    
         BNE   MPNX                 IF NOT, EXIT W/ CC SET TO NOT EQUAL         
         ZIC   R0,QHELN                                                         
         DROP  RF                                                               
                                                                                
         AR    RF,R0                                                            
         USING QIELEM,RF                                                        
         CLI   QICODE,QICODEQ      CHECK IF PROG-INFO ELEMENT                   
         BNE   MPNX                 IF NOT, EXIT W/ CC SET TO NOT EQUAL         
         CLI   QIELN,4             IS IT A BACKWARD POINTER?                    
         BL    MPNX                                                             
         BH    MPN15                                                            
         ZICM  R0,2(RF),(3)         YES, ADJUST FOR IT                          
         SLL   R0,17                                                            
         SRL   R0,17                                                            
         LR    RF,R0                                                            
         A     RF,DBAREC                                                        
MPN15    CLC   QIPNUM,STAS+1       MATCH ON REQUESTED PROG NUMBER               
         B     MPNX                 EXIT W/ CC SET                              
         DROP  RF                                                               
*                                                                               
MPNX     DS    0H                                                               
         B     EXIT                                                             
                                                                                
         DROP  R5                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*===================== SUBROUTINE POOL INTERFACE =====================*         
                                                                                
         DS    0H                                                               
GOSUB    NTR1  BASE=MYBASE1,LABEL=N                                             
         L     RA,MYBASE2                                                       
                                                                                
         L     RE,4(RD)            PUT EYECATCHER IN MYSELF                     
         MVC   0(3,RE),=C'+GO'                                                  
         MVC   3(1,RE),GOSUBN                                                   
         SR    RE,RE                AND CLEAR  RE  JUST TO BE SAFE              
                                                                                
         MVC   ASUBRTN,ASUBR01                                                  
         CLI   GOSUBN,R01#                                                      
         BNH   GOSUBGO                                                          
         MVC   ASUBRTN,ASUBR02                                                  
         CLI   GOSUBN,R02#                                                      
         BL    GOSUBGO                                                          
         MVC   ASUBRTN,ASUBR03                                                  
         CLI   GOSUBN,R03#                                                      
         BL    GOSUBGO                                                          
         DC    H'0'                                                             
*                                                                               
GOSUBGO  DS    0H                                                               
         ST    RC,ALOCWRK                                                       
         SR    R1,R1                                                            
         IC    R1,GOSUBN                                                        
         GOTO1 ASUBRTN,(R1)                                                     
*                                                                               
         DS    0H                                                               
         XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=============================== GETEL ===============================*         
                                                                                
         GETEL R3,MYDATDSP,MYELCODE                                             
***********************************************************************         
         TITLE 'DEDEM0A - $DEM MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (L+        
               TORG && CONSTANTS)'                                              
***********************************************************************         
*======================== LITERALS & CONSTANTS =======================*         
         LTORG                                                                  
         SPACE 2                                                                
HEADNUM2 DC    CL(L'LMDHMNUM)'MKT #'                                            
HEADNUM3 DC    CL(L'LMDHMNUM)'-----'                                            
HEADALF2 DC    CL(L'LMDHALFM)'ALPHA'                                            
HEADALF3 DC    CL3'MKT'                                                         
HEADNAM2 DC    CL(LMNAM)'MARKETNAME'                                            
HEADNAM3 DC    CL(LMNAM)'----------'                                            
HEADNAM2A DC    CL30'MARKET NAME'                                               
HEADSTA2 DC    CL(L'LMDHSTTN)'STATION'                                          
HEADSTA3 DC    CL(L'LMDHSTTN)'-------'                                          
HEADDAY2 DC    CL(L'LDHDAY)'DAY'                                                
HEADDAY3 DC    CL(L'LDHDAY)'---'                                                
HEADSTM2 DC    CL(L'LDHSTM)'EARLIEST'                                           
HEADSTM3 DC    CL5'START'                                                       
HEADETM2 DC    CL(L'LDHETM)'LATEST'                                             
HEADETM3 DC    CL3'END'                                                         
HEADLEAD DC    CL4'LEAD'                                                        
HEADLIN3 DC    CL2'IN'                                                          
HEADLOT3 DC    CL3'OUT'                                                         
                                                                                
                                                                                
***********************************************************************         
                                                                                
         DROP  R7,R8,R9,RA,RB,RC                                                
         TITLE 'DEDEM0A - $DEM MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (S+        
               UBR01)'                                                          
***********************************************************************         
*======================== SUBROUTINE POOL ONE ========================*         
                                                                                
* At entry,                                                                     
*   R9-->DEMWRKD,                                                               
*   R8-->TWA,                                                                   
*   R7-->DEMTMPD,                                                               
*   R1 = equated sub-routine number.                                            
                                                                                
*&&DO                                                                           
SUBR01Q  EQU   (((*-DEM0A+X'0FFF')/X'1000')*X'1000')                            
*&&                                                                             
SUBR01Q  EQU   (((*-DEM0A+X'07FF')/X'0800')*X'0800')                            
                                                                                
         ORG   DEM0A+SUBR01Q                                                    
SUBR01   NMOD1 0,**0A01**                                                       
         USING DEMWRKD,R9          R9=A(GLOBAL WORK AREA)                       
         USING DEMTWAD,R8          R8=A(TWA)                                    
         USING DEMTMPD,R7          R7=A(TEMP W/S)                               
                                                                                
         L     RC,ALOCWRK                                                       
         USING DM0AWRKD,RC                                                      
                                                                                
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R01_00(R1)                                                       
                                                                                
QTM#     EQU   (R01_01-*)/4+1      CONVERT QUARTER HR TO MILITARY TIME          
DIV#     EQU   (R01_02-*)/4+1      DIVIDE                                       
CMS#     EQU   (R01_03-*)/4+1      COUNT MARKETS AND STATIONS                   
BMSG#    EQU   (R01_04-*)/4+1      BUILD MESSAGE                                
SDLD#    EQU   (R01_05-*)/4+1      SET DISPLAY LINE DISPLACEMENTS               
GDAY#    EQU   (R01_06-*)/4+1      GET DAY (AND TIMES)                          
IDB#     EQU   (R01_07-*)/4+1      INITIALIZE DBLOCK                            
IDML#    EQU   (R01_08-*)/4+1      INITIALIZE SPDEMLK BLOCK                     
                                                                                
                                                                                
R01_00   DS    0H                                                               
R01_01   B     QHRTOMIL            CONVERT QUARTER OUR TO MILITARY TIME         
R01_02   B     DIVIDE              DIVIDE                                       
R01_03   B     CNTMKSTA            COUNT MARKETS AND STATIONS                   
R01_04   B     BLDMSG              BUIL MESSAGE                                 
R01_05   B     SDLD                SET DISPLAY LINE DISPLACEMENTS               
R01_06   B     GETDAY              GET DAY (AND TIMES)                          
R01_07   B     INITDBLK            INITIALIZE DBLOCK                            
R01_08   B     INITDMLK            INITIALIZE SPDEMLK BLOCK                     
R01#     EQU   (*-R01_00)/4                                                     
         DC    H'0'                                                             
                                                                                
YES_01   SR    R9,R9                                                            
NO_01    LTR   R9,R9                                                            
XIT_01   XIT1                                                                   
         TITLE 'DEDEM0A - MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (SUBR01+        
               --QTM#)'                                                         
*---------- ROUTINE TO CONVERT QUARTER HOUR TO MILITARY TIME ---------*         
                                                                                
* At entry,                                                                     
*   TEMPQHR  = input quarter hour.                                              
* At exit,                                                                      
*   TEMPMTIM = output military time.                                            
                                                                                
QHRTOMIL DS    0H                                                               
         SR    R0,R0                                                            
         ZIC   R1,TEMPQHR                                                       
         D     R0,=F'4'                                                         
         LA    R1,5(R1)            BASE = 5AM IF RADIO,                         
         CLI   DBMED,C'R'                                                       
         BE    *+8                                                              
         LA    R1,1(R1)             ELSE, BASE = 6AM                            
         CH    R1,=H'24'           TEST AFTER MIDNIGHT                          
         BL    *+8                                                              
         SH    R1,=H'24'           YES - GO BACK ONE DAY                        
         MH    R1,=H'100'                                                       
         MH    R0,=H'15'                                                        
         AR    R1,R0               R1 CONTAINS MILITARY TIME                    
         OR    R1,R1                                                            
         BNZ   *+8                                                              
         LH    R1,=H'2400'                                                      
         STCM  R1,3,TEMPMTIM                                                    
*                                                                               
         B     XIT_01                                                           
         TITLE 'DEDEM0A - MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (SUBR01+        
               --DIV#)'                                                         
*---------------------------- DIVIDE LOGIC ---------------------------*         
*                                                                               
                                                                                
* At entry, DIVIDEND & DIVISOR are set.                                         
* At exit,  QUOTIENT & RMAINDER will be set.                                    
                                                                                
DIVIDE   DS    0H                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
                                                                                
         OC    DIVISOR,DIVISOR                                                  
         BZ    DIV10                                                            
         LM    R0,R1,DIVIDEND                                                   
         SLDA  R0,1                 ****** START OF DIVIDE LOGIC ******         
         D     R0,DIVISOR                                                       
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                 ******* END OF DIVIDE LOGIC *******         
                                                                                
DIV10    DS    0H                                                               
         ST    R0,RMAINDER                                                      
         ST    R1,QUOTIENT                                                      
*                                                                               
DIVX     DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'DEDEM0A - MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (SUBR01+        
               --CMS#)'                                                         
*------------------- COUNT # OF MARKETS & STATIONS -------------------*         
                                                                                
* Goes through the (sort) buffer and count the number of markets and            
*  stations for the requested program.                                          
* At exit,                                                                      
*  MKTCNT = # of markets,                                                       
*  STACNT = # of stations.                                                      
                                                                                
CNTMKSTA DS    0H                                                               
                                                                                
         DS    0H                  INITIALIZATION                               
         XC    PVMNUM,PVMNUM                                                    
         XC    PVSTTN,PVSTTN                                                    
         ZAP   MKTCNT,=P'0'                                                     
         ZAP   STACNT,=P'0'                                                     
                                                                                
         DS    0H                  INITIALIZE PARAMS FOR MY BINSRCH             
         XC    MBNPARMS(MBNPARML),MBNPARMS                                      
         MVC   MBNATAB,AIOAREA1     MY BINSRCH TABLE                            
         LA    R0,L'THISMNUM                                                    
         ST    R0,MBNLREC           LENGTH OF RECORD                            
         ST    R0,MBNLKEY           LENGTH OF KEY                               
         LA    R0,1000/L'THISMNUM                                               
         ST    R0,MBNMAXN           MAX # ENTRIES                               
*                                                                               
         L     R0,AIOAREA1                                                      
         LA    R1,1000                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR AREA FOR MY BINSRCH TABLE              
*                                                                               
         DS    0H                                                               
         L     R5,BINATAB          START FROM BEGINNING (SORT) BUFFER           
         L     R0,BINSOFAR                                                      
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BZ    CMS25                                                            
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4             GET COUNT FROM TSAR BLOCK                   
         LH    R0,TSPRECN                                                       
         XC    TSRNUM,TSRNUM                                                    
         DROP  R4                                                               
                                                                                
CMS25    DS    0H                                                               
         LTR   R0,R0               MAKE SURE THERE ARE THINGS TO COUNT          
         BZ    CMSX                                                             
         CVD   R0,DUB                                                           
         ZAP   COUNTER,DUB                                                      
         B     CMS30                                                            
                                                                                
CMS30    DS    0H                                                               
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BO    CMS35                GET RECORD FROM TSAR BUFFER                 
                                                                                
         USING BINRECD,R5                                                       
         CLI   BINKEY,X'FF'                                                     
         BE    CMSX                                                             
         MVC   THISMNUM,BINMNUM                                                 
         MVC   THISSTA,BINSTAT                                                  
         DROP  R5                                                               
         B     CMS40                                                            
                                                                                
CMS35    DS    0H                                                               
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         LA    R1,1                                                             
         AH    R1,TSRNUM           BUMP TO NEXT RECORD NUMBER                   
         STH   R1,TSRNUM                                                        
         MVI   TSACTN,TSAGET                                                    
         GOTO1 VTSAR,TSARD                                                      
         TM    TSERRS,TSEEOF                                                    
         BO    CMSX                                                             
         ZICM  R5,TSAREC+1,(7)                                                  
         DROP  R4                                                               
                                                                                
         LA    R5,2(R5)                                                         
         USING TSDEMRCD,R5                                                      
         CLI   TDRRTYP,TDRRTXFF    IGNORE ANY RECORD WHICH                      
         BE    CMSX                                                             
         CLI   TDRRTYP,TDRRTPGI     IS NOT A PROGRAM INFO RECORD                
         BNE   CMS70                                                            
         MVC   THISMNUM,TDRMMNUM   EXTRACT MARKET & STATION                     
         MVC   THISSTA,TDRMSTAT                                                 
         CLI   OPTLIST,C'S'                                                     
         BNE   *+16                                                             
         MVC   THISSTA,TDRSSTAT                                                 
         MVC   THISMNUM,TDRSMNUM                                                
         DROP  R5                                                               
         B     CMS40                                                            
                                                                                
CMS40    DS    0H                                                               
         CLC   PVMNUM,THISMNUM     IF SAME AS PREVIOUS                          
         BE    CMS46                THEN DON'T COUNT IT                         
         MVC   PVMNUM,THISMNUM                                                  
                                                                                
         MVI   MBNPARMS+0,X'01'    INSERT MKT# IF NOT FOUND                     
         LA    R0,THISMNUM                                                      
         STCM  R0,7,MBNAREC+1                                                   
         GOTO1 VBINSRCH,MBNPARMS                                                
         OC    MBNAREC+1(3),MBNAREC+1   TABLE FULL?                             
         BZ    CMS46                     YES, LEAVE MKT COUNT ALONE             
         CLI   MBNPARMS+0,X'01'         WAS RECORD FOUND?                       
         BNE   CMS46                                                            
         AP    MKTCNT,=P'1'              NOPE, INCREMENT MARKET CNTER           
                                                                                
CMS46    DS    0H                                                               
         CLC   PVSTTN,THISSTA                                                   
         BE    CMS50                                                            
         MVC   PVSTTN,THISSTA                                                   
         AP    STACNT,=P'1'                                                     
                                                                                
CMS50    DS    0H                                                               
         B     CMS70                                                            
*                                                                               
CMS70    DS    0H                  GET NEXT RECORD IN BUFFER                    
         TM    DEMFLAG1,DF1STERO                                                
         BO    CMS75                                                            
         A     R5,BINLREC                                                       
                                                                                
CMS75    DS    0H                                                               
         SP    COUNTER,=P'1'                                                    
         BNZ   CMS30                                                            
*                                                                               
CMSX     DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'DEDEM0A - MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (SUBR01+        
               --BMSG#)'                                                        
*--------------------------- BUILD MESSAGE ---------------------------*         
                                                                                
* Builds # of markets/stations message in SCRNLINE.                             
                                                                                
BLDMSG   DS    0H                                                               
         MVC   SCRNLINE,SPACES                                                  
         LA    R2,SCRNLINE+2                                                    
                                                                                
         OC    DFPGNAM,DFPGNAM     DID WE GET A PROGRAM NAME?                   
         BZ    BMSGX                NOPE                                        
         LA    R0,DFPGNAM           YES, FIND ITS LENGTH                        
         LA    R1,DFPGNAM+L'DFPGNAM-1                                           
BMSG10   CR    R0,R1               IF R0>R1,                                    
         BNH   *+8                                                              
         BAS   RE,BMSGDIE           THEN SOMETHING WRONG                        
         CLI   0(R1),C' '                                                       
         BNE   *+12                                                             
         BCT   R1,BMSG10                                                        
         BAS   RE,BMSGDIE          SHIT IF R1=0!                                
         SR    R1,R0                                                            
         EXMVC R1,0(R2),DFPGNAM                                                 
         LA    R2,2(R1,R2)                                                      
                                                                                
         DS    0H                  FORMAT PROGRAM NUMBER                        
         MVI   0(R2),C'('           SURROUND IT IN PARENTHESIS                  
         LA    R2,1(R2)                                                         
         PACK  DUB,DFPGNUM                                                      
         CVB   R1,DUB                                                           
         EDIT  (R1),(10,0(R2)),ALIGN=LEFT                                       
         AR    R2,R0                                                            
         MVI   0(R2),C')'           CLOSE THE PARENTHESIS                       
         LA    R2,2(R2)                                                         
                                                                                
         DS    0H                                                               
         MVC   0(2,R2),=C'in'                                                   
         LA    R2,3(R2)                                                         
                                                                                
         DS    0H                                                               
         ZAP   DUB,MKTCNT                                                       
         CVB   R1,DUB                                                           
         EDIT  (R1),(3,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                           
         AR    R2,R0                                                            
         MVC   1(10,R2),=C'market(s),'                                          
         LA    R2,12(R2)                                                        
                                                                                
         DS    0H                                                               
         ZAP   DUB,STACNT                                                       
         CVB   R1,DUB                                                           
         EDIT  (R1),(3,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                           
         AR    R2,R0                                                            
         MVC   1(10,R2),=C'station(s)'                                          
         LA    R2,12(R2)                                                        
                                                                                
         MVC   0(3,R2),=C'for'                                                  
         LA    R2,3+1(R2)                                                       
                                                                                
         XC    DUB,DUB                                                          
         MVC   DUB(2),BKS                                                       
         GOTO1 VDATCON,DMCB,(X'83',DUB),(6,(R2))                                
         ZIC   R0,DMCB+4                                                        
         AR    R2,R0                                                            
                                                                                
         CLI   BKS+3,0                                                          
         BE    BMSGX                                                            
         MVI   0(R2),C'('                                                       
*******  MVC   1(1,R2),BKS+3       COMMENTED OUT -USE 2 CHAR BT CODE            
* DISPLAY 2 CHARACTER BOOKTYPE                                                  
         XC    DMCB2,DMCB2                                                      
         MVI   DMCB2,12                                                         
         MVC   DMCB2+1(1),BKS+3                                                 
         GOTO1 ATRANSBT                                                         
         MVC   1(2,R2),=C'??'                                                   
         CLI   DMCB2+4,X'FF'                                                    
         BE    *+10                                                             
         MVC   1(2,R2),DMCB2+4                                                  
*                                                                               
         CLI   DMCB2+5,0           1 CHARACTER OR 2 CHARACTER BOOKTYPE          
         BE    BMSG60                                                           
         CLI   DMCB2+5,X'40'       1 CHARACTER OR 2 CHARACTER BOOKTYPE          
         BE    BMSG60                                                           
*                                                                               
         MVI   3(R2),C')'          2 CHARACTER BOOKTYPE                         
         AHI   R2,4                ADJUST FOR L(BOOKTYPE)                       
         B     BMSGX                                                            
BMSG60   MVI   2(R2),C')'                                                       
*                                                                               
         LA    R2,3(R2)                                                         
*                                                                               
BMSGX    DS    0H                                                               
         B     XIT_01                                                           
*                                                                               
BMSGDIE  DC    H'0'                COME HERE VIA   BAS RE,BMSGDIE               
         TITLE 'DEDEM0A - MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (SUBR01+        
               --SDLD#)'                                                        
*------------------- SET DISPLAY LINE DISPLACEMENTS ------------------*         
                                                                                
* Routine is used to set displacements found in DLNMTB, DLNSTB                  
*  tables.                                                                      
* At entry,                                                                     
*   R1  -->table of displacements,                                              
*   R0   = number of entries in table,                                          
                                                                                
SDLD     DS    0H                                                               
         L     R1,ADLNMTB          R1-->TABLE                                   
         LA    R0,DLNMTBQ          R0 = # OF ENTRIES IN TABLE                   
         CLI   OPTLIST,C'S'        TEST LIST IN STATION SEQUENCE                
         BNE   SDLD10                                                           
         L     R1,ADLNSTB          R1-->TABLE                                   
         LA    R0,DLNSTBQ          R0 = # OF ENTRIES IN TABLE                   
*                                                                               
SDLD10   DS    0H                                                               
         SR    RF,RF                                                            
         ICM   RF,3,2(R1)                                                       
         LA    RF,DEMTMPD(RF)      RF = A(HALF-WORD TO STORE DSPLCEMNT)         
         MVC   0(2,RF),0(R1)       STORE DISPLACEMENT                           
         LA    R1,4(R1)                                                         
         BCT   R0,SDLD10                                                        
*                                                                               
         B     XIT_01                                                           
         TITLE 'DEDEM0A - MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (SUBR01+        
               --GDAY#)'                                                        
*------- ROUTINE TO CONVERT DAY/TIME VALUES INTO OUTPUT FORMAT -------*         
                                                                                
* At entry,                                                                     
*  TEMPKSE = key day, start- and end- quarter hours.                            
                                                                                
GETDAY   DS    0H                                                               
         L     R1,ADAYTAB                                                       
         MVC   DAYTIME,SPACES                                                   
*                                  CONVERT KEY DAY VALUES                       
GETDAY2  CLI   0(R1),X'FF'         TEST E-O-T                                   
         BE    GETDAY6                                                          
         CLI   0(R1),0             TEST IF AN ALL FILE VALUE                    
*&&DO                                                                           
         BE    *+14                                                             
         CLC   0(1,R1),DBFIL       NO - MATCH ON FILE NAME                      
*&&                                                                             
         BE    *+12                 NO - MATCH ON FILE NAME                     
         CLI   0(R1),C'T'           ONLY HAVE KEY DAYS FOR TP FOR NOW           
         BNE   GETDAY4                                                          
         CLI   1(R1),X'FF'         TEST FOR FUNNIES                             
         BE    GETDAY6                                                          
         CLC   1(1,R1),TEMPKDAY    MATCH ON KEY DAY VALUE                       
         BE    GETDAY6                                                          
GETDAY4  LA    R1,L'DAYTAB(R1)                                                  
         B     GETDAY2                                                          
GETDAY6  MVC   DAYTIME(3),3(R1)    SET OUTPUT VALUE FROM TABLE                  
         MVC   DAYBIN(1),2(R1)     GET BINARY VALUE OF DAY OF WEEK              
*                                                                               
                                                                                
         DS    0H                  CONVERT QHRS TO MILITARY TIME                
         MVI   GOSUBN,QTM#                                                      
         XC    DUB,DUB                                                          
         MVC   TEMPQHR,TEMPSQH                                                  
         GOTO1 AGOSUB               CONVERT QHR TO MIL TIME                     
         MVC   DUB(2),TEMPMTIM                                                  
         ZIC   R1,TEMPEQH                                                       
         LA    R1,1(R1)                                                         
         STC   R1,TEMPQHR                                                       
         GOTO1 AGOSUB               CONVERT QHR TO MIL TIME                     
         MVC   DUB+4(2),TEMPMTIM                                                
                                                                                
         DS    0H                  ASSUME STANDARD TIME FORMAT                  
         GOTO1 VUNTIME,DMCB,DUB,DAYTIME+3                                       
         GOTO1 (RF),(R1),DUB+4,DAYTIME+8                                        
***                                                                             
         TM    DEMFLAG1,DF1STERO+DF1DEM32   DEM32 ALWAYS MILITARY TIME          
         BO    *+12                                                             
         CLI   OPTTIME,C'M'        EDIT TIME VALUES                             
         BNE   GETDAYX                                                          
***                                                                             
         DS    0H                  MILITARY TIME FORMAT                         
         MVC   DUB+2(2),TEMPMTIM                                                
         XC    DAYTIME,DAYTIME                                                  
         L     R0,DUB                                                           
         SRDL  R0,16               R0=START TIME                                
         SRL   R1,16               R1=END TIME                                  
         CVD   R0,DUB              EDIT START TIME                              
         OI    DUB+7,X'0F'                                                      
         UNPK  DAYTIME+3(4),DUB                                                 
         CR    R0,R1               TEST START TIME EQ END TIME                  
         BE    GETDAYX                                                          
         CVD   R1,DUB              EDIT END TIME                                
         OI    DUB+7,X'0F'                                                      
         UNPK  DAYTIME+8(4),DUB                                                 
         B     GETDAYX                                                          
*                                                                               
GETDAYX  DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'DEDEM0A - MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (SUBR01+        
               --IDB#)'                                                         
*------------------------- INITIALIZE DBLOCK -------------------------*         
                                                                                
* Initializes a DEMO BLOCK in DBLOCK1.                                          
                                                                                
INITDBLK DS    0H                                                               
         LA    R5,DBLOCK1                                                       
         USING DBLOCKD,R5                                                       
                                                                                
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,AIOAREA1     I/O AREA                                     
         MVC   DBCOMFCS,AFAC       COMFACS                                      
         MVC   DBFILE,=C'TP '      DEFAULT TO TIME PERIOD FILE                  
***      CLI   TAPEOPT,C'Y'                                                     
***      BNE   *+8                                                              
         MVI   DBTAPEP,C'Y'        FORCE IMPRESSION BASED                       
         MVC   DBSELMED,DBMED      MEDIA                                        
         MVC   DBSELSRC,DBSRC      SOURCE                                       
         MVC   DBSELAGY,AGYALPH    AGENCY ALPHA                                 
         MVC   DBSELBK,BKS         BOOK                                         
                                                                                
         DROP  R5                                                               
*                                                                               
IDBX     DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'DEDEM0A - MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (SUBR01+        
               --IDML#)'                                                        
*------------------------- INITIALIZE SPDEMLK ------------------------*         
                                                                                
* Initializes a SPDEMLK BLOCK in SPDMLK_1.                                      
* At entry,                                                                     
*   THISSTA  = station for demo lookup,                                         
*   THISKDAY = day for demo lookup.                                             
                                                                                
INITDMLK DS    0H                                                               
         LA    R6,SPDMLK_1                                                      
         USING SPDEMLKD,R6                                                      
                                                                                
         XC    SPDEMLK(SPDEMLKL),SPDEMLK                                        
         MVC   SPLKAREC,AIOAREA1                                                
         MVC   SPLKAFAC,AFAC                                                    
         LA    R0,DEMODEMS                                                      
         ST    R0,SPLKALST                                                      
         LA    R0,THISDEMS                                                      
         ST    R0,SPLKAVAL                                                      
                                                                                
         MVC   MYPROF1W,PROF1W                                                  
         MVC   MYPROF1W+5(1),PRECPROF  SET PRECISION ACCORDING TO PROFL         
         LA    R0,MYPROF1W                                                      
         ST    R0,SPLKA1W                                                       
                                                                                
         MVC   SPLKFIL,DBFIL                                                    
         MVC   SPLKMED,DBMED                                                    
         MVC   SPLKSRC,DBSRC                                                    
         MVC   SPLKAGY,AGYALPH                                                  
         MVC   SPLKDBK,BKS                                                      
         MVC   SPLKBTYP,BKS+3                                                   
         MVI   SPLKSVI,X'FF'       SUPPRESS SVI LOOK-UP                         
         MVC   SPLKSTA,THISSTA     SET STATION                                  
*        MVC   SPLKRMK,THISMNUM                                                 
*                                                                               
         MVI   SPLKDAY,0                                                        
         L     R1,ADAYTAB                                                       
IDML22A  CLI   0(R1),X'FF'                                                      
         BE    IDML22X                                                          
         CLC   THISKDAY,1(R1)                                                   
         BE    IDML22C                                                          
         LA    R1,L'DAYTAB(R1)                                                  
         B     IDML22A                                                          
IDML22C  MVC   SPLKDAY,2(R1)          SET DAY                                   
IDML22X  EQU   *                                                                
*                                                                               
* SET UP THE SPOT LINK FOR PRECISION                                            
         CLI   OPTDEC,C'0'                                                      
         BL    IDMLX                                                            
         XC    DUB,DUB                                                          
         LA    R0,WORK2                                                         
         ST    R0,DUB+4                                                         
         MVC   DUB+0(4),=C'SPOT'                                                
*****    MVI   SDBXF,C'S'                                                       
         MVI   GOSUBN,SDBX#                                                     
         GOTO1 AGOSUB                                                           
                                                                                
         ICM   R1,15,FULL                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBXTTID,R1                                                       
                                                                                
*   FOR NOW JUST FOR ZEN'S PRECISION SUPPORT FUDGE THE DISPLACEMENT             
*   UNTIL DBEXTRAD DSECT IS LIVE                                                
         LA    RE,DBXTRC2T                GIVE ZEN HIS DECMIAL                  
         SHI   RE,1                        SUPPORT                              
         MVC   0(1,RE),OPTDEC                                                   
****     MVC   DBXTSCTL,OPTDEC                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         DROP  R1                                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
IDMLX    DS    0H                                                               
         B     XIT_01                                                           
                                                                                
         TITLE 'DEDEM0A - MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (SUBR01+        
               --LTORG && CONSTANTS)'                                           
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
         TITLE 'DEDEM0A - MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (SUBR01+        
               --MISC STUFF)'                                                   
*--------------------- SUBR01 MISCELLANEOUS STUFF --------------------*         
                                                                                
SUBR01L  EQU   *-SUBR01                                                         
         DS    0CL(4096-SUBR01L+1)                                              
***********************************************************************         
                                                                                
         DROP  R7,R8,R9,RB,RC                                                   
*                                                                               
***********************************************************************         
         TITLE 'DEDEM0A - $DEM MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (S+        
               UBR02)'                                                          
***********************************************************************         
*======================== SUBROUTINE POOL TWO ========================*         
                                                                                
* At entry,                                                                     
*   R9-->DEMWRKD,                                                               
*   R8-->TWA,                                                                   
*   R7-->DEMTMPD,                                                               
*   R1 = equated sub-routine number.                                            
                                                                                
SUBR02Q  EQU   (((*-DEM0A+4095)/4096)*4096)                                     
                                                                                
         ORG   DEM0A+SUBR02Q                                                    
SUBR02   NMOD1 0,**0A02**                                                       
         USING DEMWRKD,R9          R9=A(GLOBAL WORK AREA)                       
         USING DEMTWAD,R8          R8=A(TWA)                                    
         USING DEMTMPD,R7          R7=A(TEMP W/S)                               
                                                                                
         L     RC,ALOCWRK                                                       
         USING DM0AWRKD,RC                                                      
                                                                                
         SH    R1,=Y(R01#)         SUBTRACT FOR SUB-RTN # 2                     
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R02_00(R1)                                                       
                                                                                
GADLD#   EQU   (R02_01-*)/4+R01#+1 GET A(DOWNLOAD DATA TABLES)                  
OKDL#    EQU   (R02_02-*)/4+R01#+1 OKAY TO DOWNLOAD DATA?                       
D32DL#   EQU   (R02_03-*)/4+R01#+1 DEMLINE PROCESSING FOR DEM32                 
D32PGMK# EQU   (R02_04-*)/4+R01#+1 DOWNLOAD PROG ACROSS MRKT                    
GETDEM#  EQU   (R02_05-*)/4+R01#+1 4+R01#+1 GETDEM                              
                                                                                
R02_00   DS    0H                                                               
R02_01   B     GADLDTAB            GET A(DOWNLOAD DATA TABLES)                  
R02_02   B     OKDOWNLD            OKAY TO DOWNLOAD DATA?                       
R02_03   B     D32DEMLN            DEMLINE PROCESSING FOR DEM32                 
R02_04   B     D32PGMK             DOWNLOAD PROG ACROSS MRKT                    
R02_05   B     GETDEM              DOWNLOAD PROG ACROSS MRKT                    
R02#     EQU   (*-R02_00)/4+R01#+1                                              
         DC    H'0'                                                             
                                                                                
YES_02   SR    R9,R9                                                            
NO_02    LTR   R9,R9                                                            
XIT_02   XIT1                                                                   
*                                                                               
         TITLE 'DEDEM0A - $DEM MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (S+        
               UBR02--GADLD#)'                                                  
*-------------------- GET A(DOWNLOAD DATA TABLES) --------------------*         
                                                                                
* Gets address of corresponding download data entry                             
* At entry,                                                                     
*   TMPRCTYP = TSAR demo record type                                            
* At exit,                                                                      
*   ADLDNTRY = A(download data table entry)                                     
                                                                                
GADLDTAB DS    0H                                                               
         ICM   R2,15,ADLDTABS      R2-->DOWNLOAD DATA TABLES                    
         BZ    GADLDX                                                           
         USING DLDTABD,R2                                                       
                                                                                
*                                                                               
GADLD012 DS    0H                                                               
         CLI   DLDTLEN,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DLDTFIL,DBFIL                                                    
         BNE   GADLD018                                                         
         CLC   DLDTSRC,DBSRC                                                    
         BNE   GADLD018                                                         
         CLC   DLDTMED,DBMED                                                    
         BNE   GADLD018                                                         
         CLC   DLDTRTYP,TMPRTYP                                                 
         BNE   GADLD018                                                         
         CLC   DLDTVRSN,D32PCVER                                                
         BH    GADLD018                                                         
         B     GADLD019                                                         
*                                                                               
GADLD018 DS    0H                  BUMP TO NEXT DOWNLOAD DATA ENTRY             
         ZIC   R0,DLDTLEN                                                       
         AR    R2,R0                                                            
         B     GADLD012                                                         
GADLD019 EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         B     GADLDX                                                           
                                                                                
*                                                                               
GADLDX   DS    0H                                                               
         ST    R2,ADLDNTRY                                                      
         J     EXIT                                                             
         DROP  R2                                                               
         TITLE 'DEDEM0A - $DEM MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (S+        
               UBR02--OKDL#)'                                                   
*------------------------- OKAY TO DOWNLOAD? -------------------------*         
                                                                                
* See if data can be downloaded to PC                                           
* At entry,                                                                     
*   TMPRCTYP = TSAR demo record type                                            
*   FALEMPC  = element map code                                                 
*   FALDMPC  = data    map code                                                 
* At exit,                                                                      
*   CC set to eql if data can be downloaded                                     
*   CC set to neq if otherwise                                                  
                                                                                
OKDOWNLD DS    0H                                                               
         MVI   GOSUBN,GADLD#                                                    
         GOTO1 AGOSUB                                                           
         ICM   R2,15,ADLDNTRY                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
         DS    0H                  R2-->DOWNLOAD DATA TABLE ENTRY               
         LA    R3,DLDTFIXL(R2)                                                  
         SR    R0,R0                                                            
*                                                                               
OKDL022  DS    0H                  BUMP TO SECTION FOR ELEMENT MAP CODE         
         CLI   0(R3),0                                                          
         BE    OKDLXN                                                           
         CLC   FALEMPC,1(R3)                                                    
         BE    *+14                                                             
         IC    R0,0(R3)                                                         
         AR    R3,R0                                                            
         B     OKDL022                                                          
*                                                                               
         LA    R4,3(R3)                                                         
OKDL025  DS    0H                                                               
         OC    0(L'FALDMPC,R4),0(R4)                                            
         BZ    OKDLXN                                                           
         CLC   0(L'FALDMPC,R4),FALDMPC                                          
         BE    *+12                                                             
         LA    R4,2(R4)                                                         
         B     OKDL025                                                          
                                                                                
*                                                                               
         DS    0H                                                               
         B     OKDLXY                                                           
                                                                                
*                                                                               
OKDLXN   DS    0H                                                               
         J     NO                                                               
OKDLXY   DS    0H                                                               
         J     YES                                                              
         TITLE 'DEDEM0A - $DEM MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (S+        
               UBR02--D32DL#)'                                                  
*--------------------------- DEM32 DOWNLOAD --------------------------*         
                                                                                
D32DEMLN  DS    0H                                                              
          USING TSDEMRCD,R5                                                     
          MVC   TMPRTYP,TDRRTYP                                                 
          CLI   TDRRTYP,TDRRTPGI                                                
          BE    D32DL100                                                        
          CLI   TDRRTYP,TDRRTDML                                                
          BE    D32DL200                                                        
          B     D32DLX                                                          
                                                                                
*                                                                               
** DOWNLOAD PROGRAM INFORMATION **                                              
*                                                                               
D32DL100  DS    0H                                                              
          TM    PFLAG,PMKTRNK+PMULTPRG+PPASSONE PASSED 1ST PROGRAM              
          BO    D32DLX                                                          
          MVI   GOSUBN,D32PGMK#                                                 
          GOTO1 AGOSUB                                                          
          B     D32DLX                                                          
          EJECT                                                                 
*                                                                               
** DOWNLOAD DEMO NAMES **                                                       
*                                                                               
D32DL200 DS    0H                  RECORD CONTAINS DEMO LIST                    
         CLC   =C'PROGMRNK',DUMACT MARKET RANK DOESN'T NEED TO SEND             
         BE    D32DLX              DEMO MODIFIERS                               
*                                                                               
         L     RE,ASTDMEXP                                                      
         LA    RE,1(RE)            RE-->DESTINATION                             
         ZIC   RF,STNDEMS                                                       
         MH    RF,=Y(L'DEMS)                                                    
         LA    RF,1(RF)            RF = LENGTH TO MOVE (INCL DELIMITER)         
         LA    R0,TDRDEMS          R0-->SOURCE                                  
         LR    R1,RF                                                            
         MVCL  RE,R0               MOVE DEMO EXPRSNS INTO SAVE AREA             
                                                                                
*                                                                               
         DS    0H                                                               
         TM    STFLAG1,SF1MDFRL    DEMO MODIFIERS RELEASED YET?                 
         BO    D32DL299             YEP, EXIT NOW                               
         OI    STFLAG1,SF1MDFRL     ELSE, RELEASE THEM ONCE ONLY                
                                                                                
*                                                                               
         DS    0H                  GET DEMO NAMES                               
         LA    R4,DBLOCK1          BUILD DBLOCK FOR DEMOCON                     
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,DBFIL                                                     
         MVC   DBCOMFCS,AFAC                                                    
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         L     R0,AIOAREA                                                       
         L     R3,ASTDMEXP                                                      
         LA    R3,1(R3)                                                         
                                                                                
         DS    0H                                                               
         CLC   DBFIL,=C'PAV'                                                    
         BNE   D32DL229                                                         
         LR    R1,R3                                                            
D32DL227 CLI   0(R1),XFF                                                        
         BE    D32DL229                                                         
         CLI   1(R1),C'T'                                                       
         BNE   *+8                                                              
         MVI   1(R1),C'I'                                                       
         LA    R1,L'DEMS(R1)                                                    
         B     D32DL227                                                         
D32DL229 EQU   *                                                                
*                                                                               
         DS    0H                                                               
         MVI   BYTE,C'S'            ASSUME SPOTPAK VALIDATION                   
         CLC   DBFIL,=C'PAV'        (TYPE OF VALIDATION BASED ON FILE)          
         BNE   *+8                                                              
         MVI   BYTE,0                REMOVE SPOTPAK VALIDATION                  
         GOTO1 VDEMOCON,DMCB,(STNDEMS,(R3)),(6,(R0)),(BYTE,DBLOCKD),0           
         DROP  R4                                                               
                                                                                
                                                                                
         DS    0H                                                               
         MVC   FALEMPC,=Y(FMHDMLS)                                              
         GOTO1 ADM32SET            DO SETELEM FOR FALINK FIRST                  
                                                                                
*                                                                               
         DS    0H                  DOWNLOAD DEMO NAMES                          
         ZIC   R5,STNDEMS                                                       
         L     R6,AIOAREA          R6-->DEMO NAMES                              
*                                                                               
D32DL242 DS    0H                  START OF LOOP                                
         LA    R2,WORK              R2-->OUTPUT AREA FOR DEMO VALUE             
         ZIC   RF,STNDEMS                                                       
         SR    RF,R5                                                            
         LR    RE,RF                                                            
         MH    RF,=H'3'                                                         
         L     R3,ASTDMEXP                                                      
         LA    R3,1(RF,R3)          R3-->CURRENT DEMO EXPRESSION                
                                                                                
         SLL   RE,2                                                             
         LA    R4,TDRDEMS(RE)       R4-->DEMO VALUES                            
         MVI   GOSUBN,GETDEM#       GET DEMO PRECISION                          
         GOTO1 AGOSUB                                                           
         PRINT OFF                                                              
*&&DO                                                                           
                                                                                
*                                                                               
** "LEAD IN" LABEL **                                                           
*                                                                               
         MVC   FALDMPC,=Y(FMDDMLSFDNM)   "DEMO NAME"                            
         MVC   WORK(7),=C'Lead In'                                              
         LA    R0,WORK                                                          
         LA    R1,7                                                             
         BAS   RE,D32TPAD          ADD DATA TO DOWNLOAD                         
*                                                                               
         MVC   FALDMPC,=Y(FMDDMLSPREC)   PRECISION                              
         LA    R1,1                                                             
         LA    R0,DEMOPRE                                                       
         BAS   RE,D32TPAD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMDDMLSCPP)    CPP APPLICABILITY                      
         MVI   WORK2,C'N'                 CPP DOES NOT APPLY FOR LEADIN         
         LA    R0,WORK2                                                         
         LA    R1,1                                                             
         BAS   RE,D32TPAD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMDDMLSCPLB)   CPP/CPM CAPTION                        
         MVI   WORK2,C' '                 CPP DOES NOT APPLY FOR LEADIN         
         LA    R0,WORK2                                                         
         LA    R1,1                                                             
         BAS   RE,D32TPAD                                                       
*&&                                                                             
         PRINT ON                                                               
                                                                                
*                                                                               
** PROGRAM'S DEMO NAME LABEL **                                                 
*                                                                               
         MVC   FALDMPC,=Y(FMDDMLSFDNM)   DEMO NAME                              
         LR    R0,R6                      R0-->FORMATTED DEMO NAME              
         LA    R1,6                                                             
         BAS   RE,D32TPAD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMDDMLSPREC)   PRECISION                              
         LA    R1,1                                                             
         LA    R0,DEMOPRE                                                       
         BAS   RE,D32TPAD                                                       
*                                                                               
         DS    0H                  CPP APPLICABILITY FLAG                       
         MVC   FALDMPC,=Y(FMDDMLSCPP)                                           
                                                                                
         ZIC   RF,STNDEMS                                                       
         SR    RF,R5                                                            
         MH    RF,=H'3'                                                         
         L     R3,ASTDMEXP                                                      
         LA    R3,1(RF,R3)          R3-->CURRENT DEMO EXPRESSION                
         MVI   WORK2,C'Y'           ASSUME CPP APPLIES                          
         CLI   1(R3),C'R'            IF MODIFIER IS RATINGS                     
         BE    D32DL248G                                                        
         CLI   1(R3),C'I'            IF MODIFIER IS TSA IMPRESS                 
         BE    D32DL248G                                                        
         CLI   1(R3),C'A'            IF MODIFIER IS DMA IMPRESS                 
         BE    D32DL248G                                                        
         MVI   WORK2,C'N'           CPP DOES NOT APPLY                          
D32DL248G EQU  *                                                                
                                                                                
         LA    R0,WORK2                                                         
         LA    R1,1                                                             
         BAS   RE,D32TPAD                                                       
D32DL248X EQU  *                                                                
*                                                                               
         DS    0H                  CPP LABEL                                    
         MVC   FALDMPC,=Y(FMDDMLSCPLB)                                          
                                                                                
         ZIC   RF,STNDEMS                                                       
         SR    RF,R5                                                            
         MH    RF,=H'3'                                                         
         L     R3,ASTDMEXP                                                      
         LA    R3,1(RF,R3)          R3-->CURRENT DEMO EXPRESSION                
                                                                                
         MVC   WORK2(3),=C'CPP'     ASSUME CPP APPLIES                          
         CLC   D32PCVER,=AL4(XTRCVER7)                                          
         BL    *+10                                                             
         MVC   WORK2(5),=C'CPP/M'   ASSUME CPP APPLIES                          
         CLI   1(R3),C'R'            IF MODIFIER IS RATINGS                     
         BE    D32DL249G                                                        
         CLC   D32PCVER,=AL4(XTRCVER7)                                          
         BNL   *+10                                                             
         MVC   WORK2(3),=C'CPM'     ASSUME CPM APPLIES                          
         CLI   1(R3),C'I'            IF MODIFIER IS TSA IMPRESSIONS             
         BE    D32DL249G                                                        
         CLI   1(R3),C'A'            IF MODIFIER IS DMA IMPRESSIONS             
         BE    D32DL249G                                                        
         B     D32DL249X            CPP LABEL DOES NOT APPLY                    
D32DL249G EQU  *                                                                
                                                                                
         LA    R1,3                                                             
         LA    R0,WORK2                                                         
         CLC   D32PCVER,=AL4(XTRCVER7)                                          
         BL    *+8                                                              
         LA    R1,5                                                             
         BAS   RE,D32DLAD                                                       
D32DL249X EQU  *                                                                
                                                                                
*                                                                               
** "LEAD IN" LABEL **                                                           
*                                                                               
         MVC   FALDMPC,=Y(FMDDMLSFDNM)   "DEMO NAME"                            
         MVC   WORK(7),=C'Lead In'                                              
         LA    R0,WORK                                                          
         LA    R1,7                                                             
         BAS   RE,D32TPAD          ADD DATA TO DOWNLOAD                         
*                                                                               
         MVC   FALDMPC,=Y(FMDDMLSPREC)   PRECISION                              
         LA    R1,1                                                             
         LA    R0,DEMOPRE                                                       
         BAS   RE,D32TPAD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMDDMLSCPP)    CPP APPLICABILITY                      
         MVI   WORK2,C'N'                 CPP DOES NOT APPLY FOR LEADIN         
         LA    R0,WORK2                                                         
         LA    R1,1                                                             
         BAS   RE,D32TPAD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMDDMLSCPLB)   CPP/CPM CAPTION                        
         MVI   WORK2,C' '                 CPP DOES NOT APPLY FOR LEADIN         
         LA    R0,WORK2                                                         
         LA    R1,1                                                             
         BAS   RE,D32TPAD                                                       
                                                                                
*                                                                               
** "LEAD OUT" LABEL **                                                          
*                                                                               
         MVC   FALDMPC,=Y(FMDDMLSFDNM)   "DEMO NAME"                            
         XC    WORK,WORK                                                        
         MVC   WORK(8),=C'Lead Out'                                             
         LA    R0,WORK                                                          
         LA    R1,8                                                             
         BAS   RE,D32TPAD          ADD DATA TO DOWNLOAD                         
*                                                                               
         MVC   FALDMPC,=Y(FMDDMLSPREC)                                          
         LA    R1,1                                                             
         LA    R0,DEMOPRE                                                       
         BAS   RE,D32TPAD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMDDMLSCPP)    CPP APPLICABILITY FLAG                 
         MVI   WORK2,C'N'                 CPP DOES NOT APPLY TO LEADOUT         
         LA    R0,WORK2                                                         
         LA    R1,1                                                             
         BAS   RE,D32TPAD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMDDMLSCPLB)   CPP/CPM CAPTION                        
         MVI   WORK2,C' '                 CPP DOES NOT APPLY TO LEADOUT         
         LA    R0,WORK2                                                         
         LA    R1,1                                                             
         BAS   RE,D32TPAD                                                       
                                                                                
*                                                                               
** BUMP TO NEXT DEMO **                                                         
*                                                                               
         LA    R6,6(R6)             BUMP TO NEXT DEMO NAME                      
         BCT   R5,D32DL242                                                      
                                                                                
*                                                                               
         DS    0H                                                               
         CLC   DBFIL,=C'PAV'       IF PAV FILE,                                 
         BNE   *+8                                                              
         MVI   APMODE,SAMEREC       GET SAME REC TO RSTORE FUDGED DMLST         
         B     D32DL299                                                         
D32DL299 EQU   *                                                                
                                                                                
*                                                                               
         B     D32DLX                                                           
*                                                                               
D32DLX   DS    0H                                                               
         J     EXIT                                                             
         TITLE 'DEDEM0A - $DEM MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (S+        
               UBR02--D32PGMK#)'                                                
*--------------------- DEM32 DOWNLOAD MARKET LIST --------------------*         
                                                                                
D32PGMK   DS    0H                                                              
*                                                                               
         TM    PFLAG,PMKTRNK+PMULTPRG   MARKET RNK AND MORE THAN 1              
         BO    RNKDMLN                  PROGRAM NUMBER REQUESTED                
D32PGMK2 DS    0H                                                               
         USING TSDEMRCD,R5                                                      
         MVC   FALEMPC,=Y(FMHPGMK)                                              
         SR    R1,R1                                                            
         GOTO1 ADM32SET                                                         
*                                                                               
         MVC   FALDMPC,=Y(FMDPGPNM)       PROGRAM DEMO                          
         MVC   BYTE,TDRPNUM                                                     
         LA    R1,1                                                             
         LA    R0,BYTE                                                          
         BAS   RE,D32TPAD                                                       
*                                                                               
          DS    0H                                                              
          MVC   FALDMPC,=Y(FMDPGMK)                                             
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRMMNUM),TDRMMNUM  ASSUME LIST BY MKT SEQ               
          CLI   OPTLIST,C'S'                                                    
          BNE   *+10                                                            
          MVC   WORK(L'TDRSMNUM),TDRSMNUM                                       
          ZICM  R1,WORK,2                                                       
          MVC   THISMNUM,WORK                                                   
          EDIT  (R1),(4,WORK),ALIGN=LEFT                                        
          LA    R0,WORK                                                         
          LA    R1,4                                                            
          CLC   =C'PROGMRNK',DUMACT                                             
          BNE   *+20                                                            
          CLC   MYNMKT,WORK        SAME NUMERIC MKT                             
          BE    *+14               DONT NEED TO PASS SAME MKT                   
          MVC   MYNMKT,WORK        FOR MKT RANK                                 
          BAS   RE,D32TPAD                                                      
*                                                                               
          MVI   GOSUBN,IDB#        INIT DEM BLOCK SINCE CLEARED BEFORE          
          GOTO1 AGOSUB                                                          
          LA    R6,DBLOCK1                                                      
          USING DBLOCKD,R6                                                      
          MVI   DBFUNCT,DBGETMK     GET MARKET INFORMATION                      
          MVC   DBSELRMK,THISMNUM                                               
          GOTO1 ASETUSID                                                        
          GOTO1 VDEMAND,DMCB,DBLOCKD,D32HOOK5,0,0                               
          DROP  R6                                                              
*                                                                               
         MVC   THISMNUM,TDRMMNUM                                                
         MVC   THISSTA,TDRMSTAT                                                 
         CLI   OPTLIST,C'S'                                                     
         BNE   *+16                                                             
         MVC   THISSTA,TDRSSTAT                                                 
         MVC   THISMNUM,TDRSMNUM                                                
                                                                                
         MVC   THISKDAY,TDRDAY                                                  
         MVC   THISSQH,TDRSQH                                                   
         MVC   THISEQH,TDREQH                                                   
*                                                                               
          CLC   =C'PROGMRNK',DUMACT                                             
          BE    DODEMS                                                          
*                                                                               
          MVC   FALDMPC,=Y(FMDPGAM)                                             
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'THISALFM),THISALFM  ALPHA MARKET CODE                    
          LA    R1,L'THISALFM                                                   
          LA    R0,WORK                                                         
          BAS   RE,D32TPAD                                                      
*                                                                               
          MVC   FALDMPC,=Y(FMDPGMN)                                             
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'THISMNAM),THISMNAM  MARKET CODE                          
          LA    R1,L'THISMNAM                                                   
          LA    R0,WORK                                                         
          BAS   RE,D32TPAD                                                      
*                                                                               
         MVC   FALDMPC,=Y(FMDPGST)       STATION CODE                           
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'THISSTA),THISSTA                                          
         CLI   WORK+L'THISSTA-1,C'T'     DON'T SHOW PARENT INDICATORS           
         BNE   *+8                                                              
         MVI   WORK+L'THISSTA-1,C' '                                            
         LA    R1,L'THISSTA                                                     
         LA    R0,WORK                                                          
         BAS   RE,D32TPAD                                                       
                                                                                
*                                                                               
         MVC   FALDMPC,=Y(FMDPGDY)       DAY                                    
         MVC   TEMPKSE,THISKDAY                                                 
         MVI   GOSUBN,GDAY#                                                     
         GOTO1 AGOSUB                                                           
         MVC   WORK,SPACES                                                      
         MVC   WORK(1),DAYBIN                                                   
         LA    R1,1                                                             
         LA    R0,WORK                                                          
         BAS   RE,D32TPAD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMDPGES)       EARLIEST START TIME                    
         GOTO1 AGOSUB                                                           
         MVC   WORK,SPACES                                                      
         MVC   WORK(4),DAYTIME+3                                                
         LA    R1,4                                                             
         LA    R0,WORK                                                          
         BAS   RE,D32TPAD                                                       
*                                                                               
         MVC   FALDMPC,=Y(FMDPGLS)       LATEST END TIME                        
         GOTO1 AGOSUB                                                           
         MVC   WORK,SPACES                                                      
         MVC   WORK(4),DAYTIME+8                                                
         LA    R1,4                                                             
         LA    R0,WORK                                                          
         BAS   RE,D32TPAD                                                       
                                                                                
*                                                                               
DODEMS   DS    0H                                                               
         L     RE,ASTDMEXP                                                      
         ZIC   RF,0(RE)                                                         
         STC   RF,DEMONDEM                                                      
         AHI   RE,1                RE-->DEMOS TO BE COPIED                      
         MHI   RF,L'DEMS           RF = L(DEMOS TO BE COPIED)                   
         AHI   RF,1                   + DELIMITER                               
         L     R0,ADEMD256         R0-->DESTINATION FOR DEMOLIST                
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
*                                                                               
         MVI   GOSUBN,IDML#                                                     
         GOTO1 AGOSUB                                                           
*                                                                               
         LA    R6,SPDMLK_1         R6-->SPDEMLK BLOCK                           
         USING SPDEMLKD,R6                                                      
         LA    R0,D32HOOK4                                                      
         ST    R0,SPLKHOOK         SET HOOK ROUTINE FOR LOOK-UP                 
         MVC   SPLKALST,ADEMD256                                                
         MVC   SPLKAVAL,ADMVL512                                                
*                                                                               
         MVI   PGDEMFLG,C'N'        SET DOING PROGRAM DEMOS FLAG                
         BAS   RE,DLIDEM32          DO LEAD-IN DEMOS                            
         MVI   PGDEMFLG,C'Y'        SET DOING PROGRAM DEMOS FLAG                
***                                                                             
**** RESET THE SPDEMLK EXTEND FOR 2 DECIMAL                                     
         CLI   OPTDEC,C'0'                                                      
         BL    D32PG100                                                         
         XC    DUB,DUB                                                          
         LA    R0,WORK2                                                         
         ST    R0,DUB+4                                                         
         MVC   DUB+0(4),=C'SPOT'                                                
*****    MVI   SDBXF,C'S'                                                       
         MVI   GOSUBN,SDBX#                                                     
         GOTO1 AGOSUB                                                           
                                                                                
         ICM   R1,15,FULL                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBXTTID,R1                                                       
                                                                                
*   FOR NOW JUST FOR ZEN'S PRECISION SUPPORT FUDGE THE DISPLACEMENT             
*   UNTIL DBEXTRAD DSECT IS LIVE                                                
         LA    RE,DBXTRC2T                GIVE ZEN HIS DECMIAL                  
         SHI   RE,1                        SUPPORT                              
         MVC   0(1,RE),OPTDEC                                                   
****     MVC   DBXTSCTL,OPTDEC                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         DROP  R1                                                               
***                                                                             
D32PG100 BAS   RE,DOPRGD32          DO PROGRAM'S DEMOS                          
         MVI   PGDEMFLG,C'N'        SET DOING PROGRAM DEMOS FLAG                
**** RESET THE SPDEMLK EXTEND FOR 2 DECIMAL                                     
         CLI   OPTDEC,C'0'                                                      
         BL    D32PG110                                                         
         XC    DUB,DUB                                                          
         LA    R0,WORK2                                                         
         ST    R0,DUB+4                                                         
         MVC   DUB+0(4),=C'SPOT'                                                
*****    MVI   SDBXF,C'S'                                                       
         MVI   GOSUBN,SDBX#                                                     
         GOTO1 AGOSUB                                                           
                                                                                
         ICM   R1,15,FULL                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBXTTID,R1                                                       
                                                                                
*   FOR NOW JUST FOR ZEN'S PRECISION SUPPORT FUDGE THE DISPLACEMENT             
*   UNTIL DBEXTRAD DSECT IS LIVE                                                
         LA    RE,DBXTRC2T                GIVE ZEN HIS DECMIAL                  
         SHI   RE,1                        SUPPORT                              
         MVC   0(1,RE),OPTDEC                                                   
****     MVC   DBXTSCTL,OPTDEC                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         DROP  R1                                                               
****                                                                            
D32PG110 BAS   RE,DOLOTD32          DO LEAD-OUT DEMOS                           
* MAKE SURE PROGRAM DEMOS READ IS THE LAST DEMO READ IN THIS ORDER              
* BECAUSE OUR FLAG BEING SET CORRECTLY DEPENDS ON THIS                          
****     BAS   RE,DOPRGD32          DO PROGRAM'S DEMOS                          
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
         L     R3,ADEMD256                                                      
         SR    R6,R6                                                            
*                                                                               
D32PM112 DS    0H                                                               
         CLI   0(R3),XFF                                                        
         BE    D32_190                                                          
                                                                                
*                                                                               
** PROGRAM'S DEMO VALUE **                                                      
*                                                                               
         DS    0H                                                               
         LA    R2,WORK             R2---> OUTPUT AREA                           
         MVC   WORK,SPACES                                                      
         L     R4,ADVPG256                                                      
         AR    R4,R6               R4-->DEMO VALUE                              
         MVI   GOSUBN,GETDEM#                                                   
         GOTO1 AGOSUB                                                           
                                                                                
         CLI   ANYREAD,C'Y'                                                     
         BE    D32PM115                                                         
****     MVC   WORK,SPACES                                                      
****     MVC   WORK(3),=C'N/A'                                                  
         B     D32PGX                                                           
                                                                                
D32PM115 MVC   FALDMPC,=Y(FMDPGPDM)       PROGRAM DEMO                          
         LA    R1,7                 PICK UP L(FORMATTED DEMO VALUE)             
         LA    R0,WORK              POINT IT TO DEMO VALUE FROM GETDEM          
         BAS   RE,D32TPAD           ADD DATA TO DOWNLOAD                        
                                                                                
*                                                                               
         DS    0H                                                               
         CLC   =C'PROGMRNK',DUMACT       MARKET RANK NEEDS ONLY THE             
         BE    D32PM149                  PROGRAM DEMOS                          
                                                                                
*                                                                               
** "LEAD IN" DEMO VALUE **                                                      
*                                                                               
         LA    R2,WORK                                                          
         MVC   WORK,SPACES                                                      
         L     R4,ADVLI256                                                      
         AR    R4,R6               R4-->DEMO VALUE                              
         MVI   GOSUBN,GETDEM#                                                   
         GOTO1 AGOSUB                                                           
*                                                                               
         MVC   FALDMPC,=Y(FMDPGLID)       LEAD IN DEMO                          
         LA    R1,7                 PICK UP L(FORMATTED DEMO VALUE)             
         LA    R0,WORK              POINT IT TO DEMO VALUE FROM GETDEM          
         BAS   RE,D32TPAD           ADD DATA TO DOWNLOAD                        
                                                                                
*                                                                               
** "LEAD OUT" DEMO VALUE **                                                     
*                                                                               
         DS    0H                                                               
         LA    R2,WORK             R2---> OUTPUT AREA                           
         MVC   WORK,SPACES                                                      
         L     R4,ADVLO256                                                      
         AR    R4,R6               R4-->DEMO VALUE                              
         MVI   GOSUBN,GETDEM#                                                   
         GOTO1 AGOSUB                                                           
*                                                                               
         MVC   FALDMPC,=Y(FMDPGLOD)       LEAD OUT DEMO                         
         LA    R1,7                 PICK UP L(FORMATTED DEMO VALUE)             
         LA    R0,WORK              POINT IT TO DEMO VALUE FROM GETDEM          
         BAS   RE,D32TPAD           ADD DATA TO DOWNLOAD                        
                                                                                
*                                                                               
D32PM149 EQU   *                                                                
         LA    R6,4(R6)                                                         
         LA    R3,3(R3)                                                         
         B     D32PM112                                                         
                                                                                
*                                                                               
D32_190  DS    0H                                                               
         MVC   FALDMPC,=Y(FMDPGAFF)      AFFILIATION                            
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'AFFILS),AFFILS                                            
         LA    R1,L'AFFILS                                                      
         LA    R0,WORK                                                          
         BAS   RE,D32TPAD                                                       
         B     D32PGX                                                           
         EJECT                                                                  
********************************************************                        
RNKDMLN   DS    0H                                                              
******************* DEM32 DOWNLOAD MARKET LIST *******                          
RNKDLMK   DS    0H                                                              
          USING TSDEMRCD,R5                                                     
          MVC   FALEMPC,=Y(FMHBKMK)                                             
          SR    R1,R1                                                           
          GOTO1 ADM32SET                                                        
*                                                                               
          DS    0H                  ALPHA MARKET                                
          MVC   FALDMPC,=Y(FMDBKMKAMKT)                                         
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRALF),TDRALF                                           
          LA    R0,WORK                                                         
          LA    R1,3                                                            
          BAS   RE,D32TPAD                                                      
*                                                                               
          DS    0H                  NUMERIC MARKET                              
          MVC   FALDMPC,=Y(FMDBKMKNMKT)                                         
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRNMRKT),TDRNMRKT                                       
          ZICM  R1,WORK,2                                                       
          EDIT  (R1),(4,WORK),ALIGN=LEFT                                        
          LA    R0,WORK                                                         
          LA    R1,4                                                            
          BAS   RE,D32TPAD                                                      
*                                                                               
          DS    0H                  MARKET NAME                                 
          MVC   FALDMPC,=Y(FMDBKMKMNAM)                                         
          MVC   WORK,SPACES                                                     
          MVC   WORK(L'TDRNMNAM),TDRNMNAM                                       
          LA    R0,WORK                                                         
          LA    R1,30                                                           
          BAS   RE,D32TPAD                                                      
*                                                                               
          MVC   FALEMPC,=Y(FMHPGMK)                                             
          SR    R1,R1                                                           
          GOTO1 ADM32SET                                                        
          MVI   THISSQH,1                                                       
          MVI   THISEQH,2                                                       
          MVI   THISKDAY,X'10'                                                  
          EDIT  (B2,MARKET),(4,THISSTA),ZERO=NOBLANK,FILL=0                     
          MVC   THISSTA+L'THISSTA-1(L'DBMED),DBMED                              
          MVC   THISMNUM,MARKET                                                 
*         MVI   GOSUBN,IDML#                                                    
*         GOTO1 AGOSUB                                                          
          B     DODEMS                                                          
*                                                                               
********************************************************                        
*                                                                               
*                                                                               
D32PGX    J     EXIT                                                            
*                                                                               
          DROP  R5                                                              
         EJECT                                                                  
DLIDEM32 NTR1                                                                   
         USING SPDEMLKD,R6                                                      
                                                                                
         MVI   CDMODE,C'I'         SET MODE FOR CALC'NG LEAD-IN DEMO            
         MVI   SPLKFIL,C'T'        ALWAYS LOOK UP LEAD-IN FROM TP FILE          
         MVC   ADEMVAL,ADVLI256    SET STORAGE TO HOLD DEMO VALUES              
                                                                                
         MVI   GOSUBN,QTM#         CONVERTING QHRS TO MIL TIMES                 
         ZIC   R1,THISSQH                                                       
         STC   R1,TEMPQHR                                                       
         GOTO1 AGOSUB                                                           
         MVC   SPLKTIM+2(2),TEMPMTIM  SET END  TIME                             
                                                                                
         BCTR  R1,0                                                             
         STC   R1,TEMPQHR                                                       
         GOTO1 AGOSUB                                                           
         MVC   SPLKTIM(2),TEMPMTIM    SET START TIME                            
                                                                                
         MVC   SPLKWKN,BKS+2          SET WEEKS                                 
                                                                                
         DS    0H                                                               
         BAS   RE,CLCDM32          GO GET LEAD-IN DEMOS                         
                                                                                
         J     EXIT                                                             
         EJECT                                                                  
*========================= DO PROGRAM'S DEMOS ========================*         
                                                                                
* At entry,                                                                     
*   R6-->SPDEMLKD.                                                              
                                                                                
         DS    0H                                                               
DOPRGD32 NTR1                                                                   
         USING SPDEMLKD,R6                                                      
                                                                                
         MVI   CDMODE,C'P'                                                      
         MVC   SPLKFIL,DBFIL       SET FILE FOR PROGRAM DEMO LOOKUP             
         MVC   ADEMVAL,ADVPG256    USE THIS TO ACCUMULATE DEMO VALUES           
*                                                                               
         MVC   SPLKTIM(2),SPLKTIM+2   START TIME IS LEAD-IN'S END TIME          
                                                                                
         ZIC   R1,THISEQH                                                       
         LA    R1,1(R1)                                                         
         STC   R1,TEMPQHR                                                       
         MVI   GOSUBN,QTM#                                                      
         GOTO1 AGOSUB                                                           
         MVC   SPLKTIM+2(2),TEMPMTIM  SET END   TIME                            
*                                                                               
         CLI   BKS+2,0             IF NO WEEKS SPECIFIED IN REQUEST,            
         BNE   *+8                                                              
         MVI   SPLKWKN,X'FF'        DEFAULT TO ALL WEEKS                        
*                                                                               
         DS    0H                                                               
         BAS   RE,CLCDM32          GO GET PROGRAM'S DEMOS                       
                                                                                
         J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*========================= DO LEAD-OUT DEMOS =========================*         
                                                                                
* At entry,                                                                     
*   R6-->SPDEMLKD.                                                              
                                                                                
         DS    0H                                                               
DOLOTD32 NTR1                                                                   
         USING SPDEMLKD,R6                                                      
                                                                                
         MVI   CDMODE,C'O'                                                      
         MVI   SPLKFIL,C'T'        ALWAYS LOOK UP LEAD-OUT FROM TP FILE         
         MVC   ADEMVAL,ADVLO256    USE THIS TO ACCUMULATE DEMO VALUES           
*                                                                               
         MVC   SPLKTIM(2),SPLKTIM+2   START TIME IS PROG'S LATEST END           
                                                                                
         ZIC   R1,THISEQH                                                       
         LA    R1,2(R1)                                                         
         STC   R1,TEMPQHR                                                       
         MVI   GOSUBN,QTM#                                                      
         GOTO1 AGOSUB                                                           
         MVC   SPLKTIM+2(2),TEMPMTIM  SET END   TIME                            
*                                                                               
         MVC   SPLKWKN,BKS+2          SET WEEKS                                 
*                                                                               
         DS    0H                                                               
         BAS   RE,CLCDM32          GO GET PROGRAM'S DEMOS                       
*                                                                               
         J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*========================== CALCULATE DEMOS ==========================*         
                                                                                
* At entry,                                                                     
*  R6-->SPDEMLKD,                                                               
*  ADEMVAL = A(to put demo values),                                             
*  CDMODE is set to the set of demos to calculate.                              
                                                                                
         DS    0H                                                               
CLCDM32  NTR1                                                                   
         TM    PFLAG,PMKTRNK+PMULTPRG                                           
         BO    CLCDMRNK                                                         
*                                                                               
         USING SPDEMLKD,R6                                                      
*                                                                               
         L     R0,ADEMVAL                                                       
         LA    R1,256*4                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR AREA FOR DEMO VALUES                   
         L     R0,SPLKAVAL                                                      
         LA    R1,(256*2)*4                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR AREA FOR DEMO VALUES                   
         XC    ACCUFCTR,ACCUFCTR   CLEAR FACTOR AS WELL                         
                                                                                
*                                                                               
DOSPDEM  DS    0H                                                               
         CLI   SPLKFIL,C'T'        LOOK UP TIME PERIOD FILE                     
         BE    DOSPDEMT                                                         
         CLI   SPLKFIL,C'P'        LOOK UP PROGRAM AVG FILE                     
         BE    DOSPDEMP                                                         
         DC    H'0'                                                             
*                                                                               
DOSPDEMT DS    0H                  TIME PERIOD FILE                             
         MVC   SPLKUID,USERID                                                   
         CLI   PGDEMFLG,C'Y'                                                    
         BNE   *+8                                                              
         MVI   ANYREAD,C'N'        SET FLAG TO FOUND DEMO RECORDS               
         GOTO1 VSPDEMLK,DMCB,(X'FF',SPDEMLKD),0,0,0,0,0                         
         CLI   0(R1),0                                                          
         BNE   CDMX                                                             
         OC    ACCUFCTR,ACCUFCTR                                                
         BZ    CDMX                                                             
                                                                                
         ZIC   R0,DEMONDEM                                                      
         L     R1,ADEMVAL                                                       
         MVI   GOSUBN,DIV#                                                      
         MVC   DIVISOR,ACCUFCTR    SET DIVISOR                                  
                                                                                
CDMO22   DS    0H                                                               
         XC    DIVIDEND,DIVIDEND                                                
         MVC   DIVIDEND+4(4),0(R1) SET DIVIDEND                                 
         GOTO1 AGOSUB                                                           
         MVC   0(4,R1),QUOTIENT                                                 
         LA    R1,4(R1)                                                         
         BCT   R0,CDMO22                                                        
         B     DOSPDEMX                                                         
                                                                                
*                                                                               
DOSPDEMP DS    0H                  PROGRAM AVERAGE FILE                         
         CLI   PGDEMFLG,C'Y'                                                    
         BNE   *+8                                                              
         MVI   ANYREAD,C'N'        SET FLAG TO FOUND DEMO RECORDS               
         MVI   GOSUBN,PVLK#                                                     
         GOTO1 AGOSUB                                                           
         B     DOSPDEMX                                                         
*                                                                               
DOSPDEMX DS    0H                                                               
         B     CDMX                                                             
         EJECT                                                                  
CLCDMRNK DS    0H                                                               
         LA    R5,DBLOCK1                                                       
         USING DBLOCKD,R5                                                       
         MVC   DBSELSTA,THISSTA                                                 
         MVC   DBSELDAY,THISKDAY                                                
         MVC   DBSELTIM,=X'02580273'                                            
         MVI   DBFUNCT,DBGETDEM                                                 
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,RNKDMHK                                     
*                                                                               
CDMX     DS    0H                                                               
         J     EXIT                                                             
                                                                                
         DROP  R6                                                               
         EJECT                                                                  
*======= ROUTINE TO PROCESS A DEMO RECORD RETURNED FROM DEMAND =======*         
RNKDMHK  DS    0H                                                               
         NTR1                                                                   
         LA    R0,MYDBLOCK                                                      
         LA    R1,DBLOCK1X-DBLOCK1                                              
         LA    RE,DBLOCK1                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         GOTO1 VDEMOUT,DMCB,(C'L',DEMODEMS),DBLOCKD,THISDEMS                    
         LA    R0,MYDBLOCK                                                      
         LA    R1,DBLOCK1X-DBLOCK1                                              
         LA    RE,DBLOCK1                                                       
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
*======= ROUTINE TO PROCESS A DEMO RECORD RETURNED FROM DEMAND =======*         
                                                                                
* At entry,                                                                     
*   R6-->SPDEMLK.                                                               
                                                                                
D32HOOK4 DS    0H                                                               
HK4      NTR1                                                                   
         USING SPDEMLKD,R6                                                      
                                                                                
         L     R5,SPLKDBLK                                                      
         USING DBLOCKD,R5                                                       
*                                                                               
         CLI   CDMODE,C'P'         IF CALCULATING PROGRAM'S DEMOS,              
         BNE   D32HK409                                                         
         BAS   RE,M32HPRGN          MAKE SURE QH-ELEM HAS PROG NUM              
         BNE   D32K4X                                                           
         CLI   PGDEMFLG,C'Y'                                                    
         BNE   *+8                                                              
         MVI   ANYREAD,C'Y'        YES DEMO READ FOUND                          
                                                                                
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'AFFL',DBLOCKD,WORK                             
         MVC   AFFILS,WORK                                                      
D32HK409 EQU   *                                                                
*                                                                               
         DS    0H                  DROP SVI FACTORS                             
         L     R1,SPLKAVAL          R1-->RETURNED DEMO VALUES                   
         LA    RF,8(R1)             RF-->SECOND DEMO VALUE ENTRY                
         ZIC   R0,DEMONDEM                                                      
         MVC   4(4,R1),0(RF)                                                    
         LA    R1,4(R1)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,*-14                                                          
*                                                                               
         L     RE,SPLKAVAL                                                      
         L     RF,ADEMVAL                                                       
         ZIC   R0,DEMONDEM                                                      
                                                                                
D32K410  DS    0H                                                               
         L     R1,0(RE)                                                         
         A     R1,0(RF)                                                         
         ST    R1,0(RF)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,D32K410                                                       
                                                                                
         ZICM  R1,DBFACTOR,(3)                                                  
         A     R1,ACCUFCTR                                                      
         ST    R1,ACCUFCTR                                                      
                                                                                
         B     D32K4X                                                           
*                                                                               
D32K4X   DS    0H                  RETURN TO DEMAND                             
         J   EXIT                                                               
                                                                                
         DROP  R5,R6                                                            
         EJECT                                                                  
D32HOOK5 NTR1                                                                   
*                                                                               
         USING DBLOCKD,R6                                                       
         CLI   DBRECTYP,DBRECMK    MAKE SURE WE HAVE CORRECT RECD TYPE          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DS    0H                  EXTRACT MARKET INFORMATION                   
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'MNA',DBLOCKD,WORK,0,0,0                        
         MVC   THISMNAM,WORK+2      MARKET NAME                                 
                                                                                
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'NAMKT',DBLOCKD,WORK,0,0,0                      
         MVC   THISALFM,WORK        ALPHA MARKET                                
*                                                                               
*                                                                               
D32K2X   DS    0H                  RETURN TO DEMAND                             
         J     EXIT                                                             
*                                                                               
         DROP  R6                                                               
                                                                                
                                                                                
                                                                                
D32DLAD   DS    0H                                                              
D32TPAD   NTR1                                                                  
          DS    0XL((D32TPAD-D32DLAD)+1)                                        
*                                                                               
          ST    R0,ADLDATA                                                      
          ST    R1,LDLDATA                                                      
          MVI   ADMODE,ADMADD                                                   
          GOTO1 ADM32ADD                                                        
          XIT1                                                                  
*                                                                               
***********************************************************************         
         TITLE 'DEDEM0A - $DEM MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM'           
***********************************************************************         
*================== MATCH TO REQUEST PROGRAM NUMBER ==================*         
                                                                                
* Routine is used to see if DBAQUART points to a quarter-hour element           
*  corresponding to the requested program number.                               
* At entry,                                                                     
*   R5     -->DBLOCKD,                                                          
*   STAS(4) = requested program number.                                         
* At exit,                                                                      
*   CC equal if it matches,                                                     
*   CC not equal if it doesn't match.                                           
                                                                                
         DS    0H                                                               
M32HPRGN NTR1  BASE=*                                                           
         USING DBLOCKD,R5                                                       
                                                                                
         L     RF,DBAQUART                                                      
         USING QHELEM,RF                                                        
         CLI   QHCODE,QHCODEQ      CHECK IF QH-ELEMENT FIRST                    
         BNE   M32X                                                             
*                                    CC SET TO NOT EQUAL                        
         ZIC   R0,QHELN                                                         
         DROP  RF                                                               
                                                                                
         AR    RF,R0                                                            
         USING QIELEM,RF                                                        
         CLI   QICODE,QICODEQ      CHECK IF PROG-INFO ELEMENT                   
         BNE   M32X                 IF NOT, EXIT W/ CC SET TO NOT EQUAL         
         CLI   QIELN,4             IS IT A BACKWARD POINTER?                    
         BL    M32X                                                             
         BH    M3215                                                            
         ZICM  R0,2(RF),(3)         YES, ADJUST FOR IT                          
         SLL   R0,17                                                            
         SRL   R0,17                                                            
         LR    RF,R0                                                            
         A     RF,DBAREC                                                        
M3215    CLC   QIPNUM,STAS+1       MATCH ON REQUESTED PROG NUMBER               
         B     M32X                 EXIT W/ CC SET                              
         DROP  RF                                                               
*                                                                               
M32X     DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  R5                                                               
         DROP  RB                                                               
*                                                                               
*=================== ROUTINE TO EDIT A DEMO VALUE ====================*         
                                                                                
* At entry,                                                                     
*   R3=A(3-byte demo expression),                                               
*   R4=A(4-byte demo value),                                                    
*   R2=A(7-byte output demo value).                                             
                                                                                
GETDEM   NTR1  BASE=*,LABEL=YES                                                 
         L     R0,0(R4)            R0=DEMO VALUE                                
                                                                                
         MVC   DUB(1),DBFIL                                                     
         MVC   DUB+1(1),1(R3)                                                   
         MVI   DUB+2,0                                                          
         L     R1,AEDITTAB         R1=A(EDIT TABLE) 1 DEC DEFAULT               
         CLI   OPTDEC,C'0'                                                      
         BNE   *+8                                                              
         L     R1,AEDITTA3         R1=A(EDIT TABLE) 1 DECIMAL DEFAULT           
         CLI   OPTDEC,C'2'                                                      
         BNE   *+8                                                              
         L     R1,AEDITTA2         R1=A(EDIT TABLE) 1 DECIMAL DEFAULT           
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
*                                                                               
         TM    DEMFLAG1,DF1STERO   IS THIS A STEREO SESSION?                    
         BO    GETDEM9              YES, USE ANOTHER EDIT FORMAT                
                                                                                
         TM    DUB+2,X'02'         TEST EDIT TO 2 DECIMALS                      
         BO    GETDEM6                                                          
         TM    DUB+2,X'01'         TEST EDIT TO 1 DECIMAL                       
         BO    GETDEM8                                                          
         EDIT  (R0),(7,0(R2))                                                   
         B     GETDEMX                                                          
GETDEM6  EDIT  (R0),(7,0(R2)),2,ZERO=BLANK                                      
         B     GETDEMX                                                          
GETDEM8  EDIT  (R0),(7,0(R2)),1,ZERO=BLANK                                      
         B     GETDEMX                                                          
*                                                                               
GETDEM9  DS    0H                  EDITTING DEMOS FOR STEREO                    
*                                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    GETDEM10                                                         
*                                                                               
         TM    DUB+2,X'02'          TEST EDIT TO 2 DECIMALS                     
         BO    GETDEM9B                                                         
         TM    DUB+2,X'01'          TEST EDIT TO 1 DECIMAL                      
         BO    GETDEM9C                                                         
GETDEM9A EDIT  (R0),(7,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                           
         B     GETDEM9X                                                         
GETDEM9B EDIT  (R0),(7,0(R2)),2,ALIGN=LEFT,ZERO=NOBLANK                         
         B     GETDEM9X                                                         
GETDEM9C EDIT  (R0),(7,0(R2)),1,ALIGN=LEFT,ZERO=NOBLANK                         
         B     GETDEM9X                                                         
GETDEM9X ST    R0,FULL                                                          
         B     GETDEMX                                                          
*                                                                               
*------------ DEM32 DEMO EDITING   BPOO -------------*                          
GETDEM10 DS    0H                  EDITTING DEMOS FOR DEM32                     
         MVI   PRECFLAG,C'N'                                                    
         TM    DUB+2,X'02'          TEST EDIT TO 2 DECIMALS                     
         BO    GETDEM11                                                         
         TM    DUB+2,X'01'          TEST EDIT TO 1 DECIMAL                      
         BO    GETDEM12                                                         
*                                                                               
         CLI   DEMOPRE,0           IF NO PRECISION ALREADY                      
         BE    *+8                                                              
         MVI   PRECFLAG,C'Y'                                                    
         MVI   DEMOPRE,0                                                        
         B     GETDEM13                                                         
GETDEM11 DS    0H                                                               
         CLI   DEMOPRE,2           IF 2 DECIMAL ALREADY                         
         BE    *+8                                                              
         MVI   PRECFLAG,C'Y'                                                    
         MVI  DEMOPRE,2                                                         
         B     GETDEM13                                                         
GETDEM12 DS    0H                                                               
         CLI   DEMOPRE,1           IF 1 DECIMAL PLACE ALREADY                   
         BE    *+8                                                              
         MVI   PRECFLAG,C'Y'                                                    
         MVI   DEMOPRE,1                                                        
         B     GETDEM13                                                         
GETDEM13 DS    0H                                                               
         EDIT  (R0),(7,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                           
         ST    R0,FULL                                                          
         B     GETDEMX                                                          
*                                                                               
GETDEMX  DS    0H                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,RC                                                            
***********************************************************************         
         TITLE 'DEDEM0A - $DEM MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (S+        
               UBR03)'                                                          
***********************************************************************         
*======================= SUBROUTINE POOL THREE =======================*         
                                                                                
* At entry,                                                                     
*   R9-->DEMWRKD,                                                               
*   R8-->TWA,                                                                   
*   R7-->DEMTMPD,                                                               
*   R1 = equated sub-routine number.                                            
                                                                                
SUBR03Q  EQU   (((*-DEM0A+X'0FFF')/X'1000')*X'1000')                            
                                                                                
         ORG   DEM0A+SUBR03Q                                                    
SUBR03   NMOD1 0,**0A03**                                                       
         USING DEMWRKD,R9          R9=A(GLOBAL WORK AREA)                       
         USING DEMTWAD,R8          R8=A(TWA)                                    
         USING DEMTMPD,R7          R7=A(TEMP W/S)                               
                                                                                
         L     RC,ALOCWRK                                                       
         USING DM0AWRKD,RC                                                      
                                                                                
         SH    R1,=Y(R02#)         SUBTRACT FOR SUB-RTN #2                      
         SLL   R1,2                                                             
         B     R03_00(R1)                                                       
                                                                                
GPRF#    EQU   (R03_01-*)/4+R02#   GET PROFILE                                  
PVLK#    EQU   (R03_02-R03_00)/4+R02#   PAV FILE LOOK UP                        
PIUN#    EQU   (R03_03-R03_00)/4+R02#   PROCESS IUN                             
XDV#     EQU   (R03_04-R03_00)/4+R02#   EXTRACT DEMO VALUES                     
SDBX#    EQU   (R03_05-R03_00)/4+R02#   SET UP A LINK IN DBEXTEND               
                                                                                
                                                                                
R03_00   DS    0H                                                               
R03_01   B     GETPROFL            GET PROFILE                                  
R03_02   B     PAVLOOK                  PAV FILE LOOK UP                        
R03_03   B     PROCIUN                  PROCESS IUN                             
R03_04   B     XTRCTDMV                 EXTRACT DEMO VALUES                     
R03_05   B     STDBXLNK            SET UP DBEXTEND LINK                         
R03#     EQU   (*-R03_00)/4+R02#                                                
         DC    H'0'                                                             
                                                                                
         TITLE 'DEDEM0A - MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (SUBR03+        
               --GPRF#)'                                                        
*---------------------------- GET PROFILE ----------------------------*         
                                                                                
* At entry,                                                                     
*   DUB(4) = profile id                                                         
                                                                                
GETPROFL DS    0H                                                               
*                                                                               
         DS    0H                  FIND ENTRY FOR PROFILE                       
         L     RE,APROFTAB                                                      
         USING PROFTAB,RE                                                       
GPRF010  CLI   0(RE),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   PRFTBID,DUB                                                      
         BE    *+12                                                             
         LA    RE,PRFTBQ(RE)                                                    
         B     GPRF010                                                          
                                                                                
         ZICM  RF,PRFTBASV,(3)                                                  
         LA    RF,DEMWRKD(RF)       RF-->AREA TO HOLD PROFILE                   
         ST    RF,APROFILE                                                      
         ZIC   R1,PRFTBLSV                                                      
         STC   R1,LPROFILE          R1 = L(AREA TO HOLD PROFILE)                
                                                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,RF),0(RF)        CLEAR AREA USED TO HOLD PROFILE             
                                                                                
         ZICM  RF,PRFTBRTN,(3)                                                  
         LA    RF,GETPROFL(RF)                                                  
         BR    RF                   GO TO APPROPRIATE GET-PROFILE RTN           
         DC    H'0'                                                             
         DROP  RE                                                               
                                                                                
*                                                                               
** REP RMP PROFILE **                                                           
*                                                                               
GPRFRRMP00 DS  0H                                                               
         XC    WORK,WORK            BUILD KEY OF REP RECD IN WORK               
         LA    R6,WORK                                                          
         USING RREPREC,R6                                                       
         MVI   RREPKTYP,X01                                                     
         MVC   RREPKREP,AGYALPH                                                 
         DROP  R6                                                               
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'REPDIR',WORK,WORK                    
         CLI   DMCB+08,X'10'                                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 (RF),(R1),=C'GETREC',=C'REPFILE',WORK+28,AIOAREA,       +        
               MYDMWORK                                                         
         CLI   DMCB+08,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
         DS    0H                  LOOK FOR PROFILE IN RECORD                   
         L     R3,AIOAREA           R3-->REP RECORD                             
         MVI   MYELCODE,X'04'                                                   
         LA    R0,RREPELEM-RREPREC                                              
         STH   R0,MYDATDSP                                                      
         BRAS  RE,GETEL                                                         
         BNE   GPRFRRMPX                                                        
*                                                                               
         ZIC   R0,(RREPPGM#-RREPPGMP)(R3)  R0 = # OF PROGRAM PROFILES           
         LA    R6,(RREPPGM1-RREPPGMP)(R3)  R6-->PROGRAM PROFILES LIST           
                                                                                
GPRFRRMP40 DS  0H                                                               
         CLI   0(R6),RREPQRMP       LOOK FOR RMP PROGRAM PROFILE                
         BE    *+16                                                             
         LA    R6,RREPPGML(R6)                                                  
         BCT   R0,GPRFRRMP40                                                    
         B     GPRFRRMPX                                                        
*                                                                               
         L     RF,APROFILE                                                      
         ZIC   R1,LPROFILE                                                      
         BCTR  R1,0                                                             
         EXMVC R1,0(RF),2(R6)       MOVE PROFILE INTO STORAGE AREA              
                                                                                
*                                                                               
GPRFRRMPX EQU  *                                                                
         B     GETPRFLX                                                         
                                                                                
*                                                                               
** SPOT 1W PROFILE **                                                           
*                                                                               
GPRFS01W00 DS  0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK+0(4),=C'S01W'                                               
         MVC   WORK+4(2),AGYALPH                                                
         MVI   WORK+6,C'T'                                                      
         MVC   WORK+7(3),OPTCLI                                                 
         GOTO1 VGETPROF,DMCB,WORK,PROF1W,VDATAMGR                               
                                                                                
*                                                                               
         DS    0H                                                               
         B     GETPRFLX                                                         
                                                                                
*                                                                               
GETPRFLX DS    0H                                                               
         J     EXIT                                                             
         TITLE 'DEDEM0A - MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (SUBR03+        
               --PVLK#)'                                                        
*----------------------------- PAV LOOKUP ----------------------------*         
                                                                                
* At entry,                                                                     
*   R6-->SPDEMLKD                                                               
                                                                                
PAVLOOK  DS    0H                                                               
         LA    R5,DBLOCK1                                                       
         USING DBLOCKD,R5                                                       
*                                                                               
         DS    0H                 CLEAR DBLOCK                                  
         LR    R0,R5               R0-->DESTINATION                             
         LA    R1,DBLOCK1X-DBLOCK1 R1 = L(DESTINATION)                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R0,AIOAREA2         R0-->DESTINATION                             
         LR    R4,R0                                                            
         LH    R1,LIOAREA2         R1 = L(DESTINATION)                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR BUFFER FOR ACCUMULATING RECDS          
         MVI   0(R4),RINVKTYQ       AND MAKE IT AN INVENTORY RECD               
                                                                                
         XC    CUMFCTR,CUMFCTR                                                  
         XC    CUMSHR(CUMSHRL),CUMSHR                                           
                                                                                
*                                                                               
** BUILD DBLOCK **                                                              
*                                                                               
         DS    0H                 BUILD DBLOCK                                  
         MVI   GOSUBN,IDB#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
         MVC   DBFILE,DBFIL                                                     
         MVI   DBFUNCT,DBGETDEM                                                 
                                                                                
         USING SPDEMLKD,R6                                                      
         MVC   DBSELBK,SPLKDBK                                                  
         MVC   DBBTYPE,SPLKBTYP                                                 
         MVC   DBSELSTA,SPLKSTA                                                 
         MVC   DBSELDAY,SPLKDAY                                                 
         MVC   DBSELTIM,SPLKTIM                                                 
         MVC   ADEMLIST,SPLKALST                                                
         DROP  R6                                                               
                                                                                
         MVI   DBBEST,C'A'         GET EVERYTHING--WILL FILTER IN HOOK          
                                                                                
** CALL DEMAND **                                                               
*                                                                               
         DS    0H                                                               
         NI    MYFLAG1,XFF-MYF1RDOK                                             
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCK,PAVHOOK,0                                    
         TM    MYFLAG1,MYF1RDOK                                                 
         BZ    PVLK149                                                          
                                                                                
*                                                                               
** PROCESS CUMULATIVE RECORD **                                                 
*                                                                               
         DS    0H                  UNWEIGH RECORD                               
         LA    R0,DBLOCKD                                                       
         ST    R0,MTHCFACS                                                      
         MVC   MTHFCTR,CUMFCTR                                                  
         MVC   MTHIFIL,=C'INV'                                                  
         MVC   MTHOFIL,=C'INV'                                                  
         MVC   MTHOSRC,=C'NSI'                                                  
                                                                                
         L     RF,AFAC                                                          
         L     RF,(CDEMOMTH-COMFACSD)(RF)                                       
         GOTO1 (RF),DMCB,=C'DIVIDE',AIOAREA2,AIOAREA2,MATHFAC                   
         DROP  R5                                                               
                                                                                
*                                                                               
         DS    0H                  EXTRACT DEMO VALUES                          
         L     R6,AIOAREA2                                                      
*&&DO                                                                           
         LA    R0,DEMODEMS                                                      
         ST    R0,ADEMLIST                                                      
*&&                                                                             
         MVI   GOSUBN,XDV#                                                      
         GOTO1 AGOSUB                                                           
PVLK149  EQU   *                                                                
                                                                                
*                                                                               
PVLKX    DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
*-------------------------- PAV DEMAND HOOK --------------------------*         
                                                                                
* At entry,                                                                     
*   R5-->DBLOCK                                                                 
                                                                                
PAVHOOK  DS    0H                                                               
         USING DBLOCKD,R5                                                       
                                                                                
*                                                                               
** TEST PROCESS RECORD OR NOT **                                                
*                                                                               
         DS    0H                  TEST FOR CORRECT PROGRAM NUMBER              
*^^GLEE - Should call DEFINE to get program number, but it isn't                
*          coded in DEFINE yet.                                                 
         L     RF,DBAQUART                                                      
         USING PHELEM,RF                                                        
         CLI   PHCODE,PHCODEQ                                                   
         BNE   PVHK012N                                                         
                                                                                
         CLI   PHELN,4                                                          
         BL    PVHK012N                                                         
         BH    PVHK012D                                                         
         ZICM  R0,2(RF),(3)                                                     
         SLL   R0,17                                                            
         SRL   R0,17                                                            
         LR    RF,R0                                                            
         A     RF,DBAREC                                                        
PVHK012D EQU   *                                                                
                                                                                
         CLI   PHREVID,XFF                                                      
         BNE   PVHK012N                                                         
                                                                                
         SR    R0,R0                                                            
         ICM   R0,7,PHPNUM3                                                     
*&&DO                                                                           
* THE FOLLOWING 5 LINES OF CODE SHOULD NOT BE NEEDED AS FAR AS                  
* WE CAN UNDERSTAND BECAUSE IF PHREVID IS SET TO X'FF' THEN THE                 
* REVISION NUMBER SHOULD ALWAYS BE 1 OR HIGHER                                  
                                                                                
         CLI   PHREV,1                                                          
         BE    PVHK012G                                                         
         CLI   PHREV,2                                                          
         BNL   PVHK012G                                                         
         B     PVHK012N                                                         
*&&                                                                             
         CLI   PHREV,1         SHOULD ALWAYS BE 1 OR HIGHER!!!                  
         BNL   *+6                                                              
         DC    H'0'                                                             
PVHK012G EQU   *                                                                
         CLM   R0,15,STAS                                                       
         BNE   PVHK012N                                                         
                                                                                
         B     PVHK012X                                                         
*                                                                               
PVHK012N DS    0H                  CAN'T MATCH ON PROGRAM NUMBER                
         B     PVHKX                                                            
PVHK012X EQU   *                                                                
         DROP  RF                                                               
         CLI   PGDEMFLG,C'Y'                                                    
         BNE   *+8                                                              
         MVI   ANYREAD,C'Y'        YES DEMO READ FOUND                          
         EJECT                                                                  
*                                                                               
** EXTRACT DATA FOR PAV FILE **                                                 
*                                                                               
         OI    MYFLAG1,MYF1RDOK    DEMAND READ OK--GOT SOME DEMO RECDS          
                                                                                
*                                                                               
         DS    0H                  GET AFFILIATION                              
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'AFFL',DBLOCKD,WORK                             
         MVC   AFFILS,WORK                                                      
                                                                                
*                                                                               
         DS    0H                  LOOK UP VUTS (TO DERIVE SHARES)              
         GOTO1 VDEMOUT,MYDMCB,(C'L',VUTLIST),DBLOCK,QHVUTS                      
         OC    QHVUTS(4),QHVUTS    IF NO VUTS,                                  
         BNZ   DMLKPH10                                                         
         GOTO1 VDEMOUT,MYDMCB,(C'L',PUTLIST),DBLOCK,QHVUTS                      
*                                                                               
DMLKPH10 DS    0H                  CALL REGETIUN FOR DEMO VALUES                
         LA    R0,QHVUTS                                                        
         ST    R0,AVUTS             SET A(VUTS TO USE)                          
         MVI   GOSUBN,PIUN#                                                     
         GOTO1 AGOSUB                                                           
*                                                                               
         DS    0H                                                               
         LA    R3,CUMSHR                                                        
         BAS   RE,GETSHR           ACCUMULATE SHARES IN CUMSHR                  
*                                                                               
         DS    0H                  MAD IUNWRK RECD TO CUMULATIVE AREA           
         LA    R0,DBLOCK                                                        
         ST    R0,MTHCFACS          INITIALIZE MATH BLOCK: A(DBLOCK)            
         ZICM  R0,DBFACTOR,(3)                                                  
         ST    R0,MTHFCTR           MATH FACTOR                                 
         A     R0,CUMFCTR                                                       
         ST    R0,CUMFCTR           UPDATE CUMULATIVE FACTOR AS WELL            
         MVC   MTHIFIL,=C'INV'                                                  
         MVC   MTHOFIL,=C'INV'                                                  
         MVC   MTHOSRC,=C'NSI'                                                  
                                                                                
         L     RF,DBCOMFCS                                                      
         L     RF,(CDEMOMTH-COMFACSD)(RF)                                       
         GOTO1 (RF),MYDMCB,=C'MAD',AIUNWRK,AIOAREA2,MATHFAC                     
         DROP  R5                                                               
                                                                                
*                                                                               
PVHKX    DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
* Routine to get shares and update shares accumulators.                         
* At entry,                                                                     
*   R3-->output area                                                            
*   R5-->DBLOCK.                                                                
*   MTHFCTR+2 = half-word aligned factor to use in weighting                    
                                                                                
         DS    0H                                                               
GETSHR   NTR1                                                                   
         USING DBLOCKD,R5                                                       
         MVC   HALF,DBACTBK        SAVE ACTUAL BOOK                             
         XC    HOMSHR(HOMSHRL),HOMSHR                                           
                                                                                
         OC    DBAQUART,DBAQUART   IF NO DBAQUART,                              
         BZ    GETSHR05X            THERE BE NO SHARES                          
         GOTO1 VDEMOUT,MYDMCB,(C'P',DEMOSHR),DBLOCKD,HOMSHR                     
GETSHR05X EQU  *                                                                
                                                                                
         MVC   DBACTBK,HALF        RESTORE ACTUAL BOOK VALUE                    
         DROP  R5                                                               
                                                                                
*                                                                               
         DS    0H                  ACCUMULATE THE SHARES                        
         LA    R0,3                 R0 = COUNTER                                
         SR    R1,R1                R1 = INDEX INTO VALUES                      
                                                                                
GETSHR10 DS    0H                                                               
         L     RE,HOMSHR(R1)                                                    
         MH    RE,MTHFCTR+2                                                     
         LR    RF,RE                                                            
         A     RE,0(R1,R3)                                                      
         ST    RE,0(R1,R3)         UPDATE SHARES                                
         LA    R1,4(R1)                                                         
         BCT   R0,GETSHR10                                                      
*                                                                               
         J     EXIT                                                             
         TITLE 'DEDEM0A - MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (SUBR03+        
               --PIUN#)'                                                        
*------------------------- PROCESS IUN STUFF -------------------------*         
                                                                                
* At entry,                                                                     
*   R5-->DBLOCK,                                                                
*   AVUTS-->VUT values to use in computing home shares,                         
*   DEMODEMS = list of requested demos.                                         
* At exit,                                                                      
*   IUNWRK contains demo values in IUN format                                   
                                                                                
PROCIUN  DS    0H                                                               
         USING DBLOCKD,R5                                                       
                                                                                
*                                                                               
* SAVE DBLOCK *                                                                 
*                                                                               
         LA    R0,MYDBLOCK          DESTINATION                                 
         LA    R1,DBLOCK1X-DBLOCK1                                              
         LA    RE,DBLOCK            SOURCE                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
*                                                                               
* SET UP IUN AREAS *                                                            
*                                                                               
         L     R4,AIUNWRK                                                       
         LR    R0,R4                                                            
         LA    R1,IUNWRKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR IUN WORK AREA                          
*                                                                               
         MVI   0(R4),RINVKTYQ                                                   
         LA    R4,500(R4)                                                       
         USING IUNRECD,R4                                                       
                                                                                
*                                                                               
* EXPLODE DEMO RECORD INTO IUN BUCKETS *                                        
*                                                                               
         DS    0H                                                               
         GOTO1 VRGETIUN,MYDMCB,(10,DBLOCK),IUNRECD                              
                                                                                
*                                                                               
* COPY OLD TO NEW TO COMPLETE IUN RECORD *                                      
*                                                                               
         LA    R0,IUNNEW                                                        
         LA    R1,IUNNEWX-IUNNEW                                                
         LA    RE,IUNOLD                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
*                                                                               
* GET HOME SHARES *                                                             
*                                                                               
         GOTO1 VDEMOUT,MYDMCB,(C'L',SHRLIST),DBLOCKD,ISHOMES,0                  
                                                                                
*                                                                               
* BUILD IUN RECORD *                                                            
*                                                                               
         DS    0H                  SET DBLOCK VALUES                            
         L     RE,AIUNWRK                                                       
         ST    RE,DBAREC            SET A(RECORD)                               
         LA    RE,(RINVPEL-RINVREC)(RE)                                         
         MVI   0(RE),0                                                          
         ST    RE,DBAQUART          SET A(QH ELEM)                              
         LA    R0,IUNRECL/4                                                     
         STH   R0,DBNUMVLS          SET NUMBER OF VALUES                        
                                                                                
         L     RF,DBCOMFCS                                                      
         L     RF,(CDEMAINT-COMFACSD)(RF)                                       
         MVC   WORK(10),OFORMAT                                                 
         CLI   TAPEOPT,C'Y'        SWITCH FORMULAE FOR TAPE BASED OPT           
         BNE   *+10                                                             
         MVC   WORK+7(2),=X'5A0B'                                               
         GOTO1 (RF),MYDMCB,=C'REP',DBLOCK,IUNRECD,WORK                          
                                                                                
*                                                                               
* RESTORE DBLOCK *                                                              
*                                                                               
         LA    R0,DBLOCK            DESTINATION                                 
         LA    R1,DBLOCK1X-DBLOCK1                                              
         LA    RE,MYDBLOCK          SOURCE                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         DS    0H                                                               
         J     EXIT                                                             
         DROP  R4,R5                                                            
         TITLE 'DEDEM0A - MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (SUBR03+        
               --XDV#)'                                                         
*------------------------ EXTRACT DEMO VALUES ------------------------*         
                                                                                
* Extracts demo values from an inventory track record.                          
* At entry,                                                                     
*   ADEMLIST = list of demos to get,                                            
*   ADEMVAL  = A(output area for demo values)                                   
*   R6       = A(record to extract demos from)                                  
* NOTE:  MYDBLOCK area will get creamed here.                                   
                                                                                
XTRCTDMV DS    0H                                                               
         L     R0,ADEMVAL                                                       
         LA    R1,MAXDEMS*4                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR OUTPUT AREA FOR DEMO VALUES            
*                                                                               
         DS    0H                  CLEAR DBLOCK                                 
         LA    R0,MYDBLOCK                                                      
         LA    R1,L'MYDBLOCK                                                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
*                                                                               
         DS    0H                  SET UP DBLOCK                                
         LA    R5,MYDBLOCK                                                      
         USING DBLOCKD,R5                                                       
         ST    R6,DBAREC                 A(RECORD)                              
         LA    R0,(RINVPEL-RINVREC)(R6)  A(1ST ELEMENT)                         
         ST    R0,DBAQUART                                                      
         MVC   DBCOMFCS,AFAC             A(COMFACS)                             
*                                                                               
         DS    0H                  SET UP PRECISION IN DBLOCK EXTNSN            
                                                                                
         CLI   OPTDEC,C'0'                                                      
         BL    XTRCT40                                                          
         XC    MYDBXTRA,MYDBXTRA                                                
         LA    R1,MYDBXTRA                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
*                                                                               
**** FOR NOW JUST FOR Zen's                                                     
**** PRECISION SUPPORT FUDGE THE DISPLACEMENT                                   
**** UNTIL DBEXTRAD DSECT IS LIVE                                               
*                                                                               
         LA    RE,DBXTRC2T                GIVE ZEN HIS DECMIAL                  
         SHI   RE,1                        SUPPORT                              
         MVC   0(1,RE),OPTDEC                                                   
****     MVC   DBXTSCTL,OPTDEC                                                  
*                                                                               
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         DROP  R1                                                               
                                                                                
*                                                                               
*                                                                               
         DS    0H                                                               
XTRCT40  GOTO1 VDEMOUT,MYDMCB,(C'L',ADEMLIST),DBLOCKD,ADEMVAL                   
         DROP  R5                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         J     EXIT                                                             
*------------------------- SET DBEXTEND AREA -------------------------*         
                                                                                
* Sets up a link in the DBEXTEND area.                                          
* At entry,                                                                     
*   DUB(4)   = link identification if want to match to an existing link         
*            = x'00000000' if link must be added,                               
*   DUB+4(4) = A(link),                                                         
*   R6       = A(SPDEMLK BLOCK).                                                
* At exit,                                                                      
*   FULL     = address of link, zeroes if no link set up.                       
                                                                                
STDBXLNK DS    0H                                                               
         XC    FULL,FULL                                                        
*                                                                               
         DS    0H                  GET R2 TO START OF EXTENSION AREA            
         USING SPDEMLKD,R6                                                      
         L     RF,SPLKAREC                                                      
         ICM   R2,15,8(RF)                                                      
         CLC   0(8,RF),=C'DBEXTEND'                                             
         BE    SDBX022                                                          
                                                                                
         MVC   0(8,RF),=C'DBEXTEND'                                             
         L     R2,DUB+4            USE A(CALLER'S LINK) IF DBEXTEND=0           
         STCM  R2,15,8(RF)                                                      
         B     SDBX040              AND MAKE IT THE LAST LINK                   
         DROP  R6                                                               
                                                                                
*                                                                               
SDBX020  DS    0H                  BUMP TO APPROPRIATE LINK                     
         ICM   R0,15,4(R2)          GET ADDRESS OF NEXT LINK                    
         BZ    SDBX030               IF ZERO, ADD CALLER'S LINK                 
                                                                                
SDBX022  DS    0H                                                               
         LR    R2,R0                "BUMPED" TO NEXT LINK                       
         OC    DUB(4),DUB           IF LOOKING FOR MATCH,                       
         BZ    SDBX020                                                          
         CLC   DUB(4),0(R2)          AND LINKS' IDS MATCH,                      
         BNE   SDBX020                                                          
         B     SDBX050               PASS BACK ADDR OF CURRENT LINK             
                                                                                
*                                                                               
SDBX030  DS    0H                  ADD CALLER'S LINK TO END                     
         MVC   4(4,R2),DUB+4        SET THE NEXT ADDR INTO LAST LINK            
         ICM   R2,15,4(R2)          BUMP TO THE NEW LAST LINK                   
         B     SDBX040                                                          
                                                                                
*                                                                               
SDBX040  DS    0H                  R2-->LAST LINK                               
         XC    4(4,R2),4(R2)        ZERO OUT THE NEXT ADDRESS                   
         B     SDBX050               NOPE                                       
                                                                                
*                                                                               
SDBX050  DS    0H                                                               
         ST    R2,FULL             RETURN ADDRESS OF MATCHED/ADDED LINK         
         B     SDBXX                                                            
                                                                                
*                                                                               
SDBXX    DS    0H                                                               
         J     EXIT                                                             
*                                                                               
         TITLE 'DEDEM0A - MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (SUBR03+        
               --LTORG && CONSTANTS)'                                           
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
VUTLIST  DC    X'81',C'V',AL1(1)   RATING TIMES SHARE                           
         DC    X'81',C'V',AL1(2)                                                
         DC    X'81',C'V',AL1(3)                                                
         DC    X'FF'                                                            
                                                                                
PUTLIST  DC    X'81',C'P',AL1(1)   STRAIGHT PUT                                 
         DC    X'81',C'P',AL1(2)                                                
         DC    X'81',C'P',AL1(3)                                                
         DC    X'FF'                                                            
                                                                                
SHRLIST  DC    X'00',C'S',AL1(1)   SHARES                                       
         DC    X'00',C'S',AL1(2)                                                
         DC    X'00',C'S',AL1(3)                                                
         DC    X'FF'                                                            
                                                                                
DEMOSHR  DS    0XL3                                                             
         DC    X'81',C'S',AL1(1)                                                
         DC    X'81',C'S',AL1(2)                                                
         DC    X'81',C'S',AL1(3)                                                
         DC    X'FF'                                                            
*                                                                               
OFORMAT  DC    C'INVUIUN',X'530B00'                                             
         TITLE 'DEDEM0A - MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (SUBR03+        
               --MISC STUFF)'                                                   
*--------------------- SUBR03 MISCELLANEOUS STUFF --------------------*         
                                                                                
SUBR03L  EQU   *-SUBR03                                                         
         DS    0CL(X'1000'-SUBR03L+1)                                           
***********************************************************************         
                                                                                
         DROP  R7,R8,R9,RB,RC                                                   
         TITLE 'DEDEM0A - MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM'                
DLDATTAB  DS  0D                                                                
*                                                                               
***********************************************************************         
*========================== T21B0A's EQUATES =========================*         
                                                                                
LINESQ   EQU   (DEMLAST-DEMLN1H)/(L'DEMLN1+L'DEMLN1H)-1                         
LMNUM    EQU   4                                                                
LALFM    EQU   3                                                                
LMNAM    EQU   10                                                               
LSTTN    EQU   5                                                                
LDAY     EQU   3                                                                
LTIME    EQU   5                                                                
LDEMO    EQU   7                                                                
SDQMXSZQ EQU   12000               MAX SIZE OF SDQTABD TABLE                    
IUNWRKL  EQU   IUNRECL+500                                                      
LOCWORKL EQU   SDQMXSZQ+IUNWRKL+DM0AWRKL                                        
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=============================== TABLES ==============================*         
                                                                                
*                                  TABLE OF DISPLACEMENTS                       
DISPTAB  DS    0AL(2+2)                                                         
         DC    AL2(DAYTAB-DEM0A),AL2(ADAYTAB-DEMTMPD)                           
         DC    AL2(EDITTAB-DEM0A),AL2(AEDITTAB-DEMTMPD)                         
         DC    AL2(EDITTAB2-DEM0A),AL2(AEDITTA2-DEMTMPD)                        
         DC    AL2(EDITTAB3-DEM0A),AL2(AEDITTA3-DEMTMPD)                        
         DC    AL2(DLNMTB-DEM0A),AL2(ADLNMTB-DEMTMPD)                           
         DC    AL2(DLNSTB-DEM0A),AL2(ADLNSTB-DEMTMPD)                           
         DC    AL2(GOSUB-DEM0A),AL2(AGOSUB-DEMTMPD)                             
         DC    AL2(SUBR01-DEM0A),AL2(ASUBR01-DEMTMPD)                           
         DC    AL2(SUBR02-DEM0A),AL2(ASUBR02-DEMTMPD)                           
         DC    AL2(SUBR03-DEM0A),AL2(ASUBR03-DEMTMPD)                           
         DC    AL2(DLDATTAB-DEM0A),AL2(ADLDTABS-DEMTMPD)                        
         DC    AL2(GETDEM-DEM0A),AL2(AGETDEM-DEMTMPD)                           
         DC    AL2(PROFTAB-DEM0A),AL2(APROFTAB-DEMTMPD)                         
DISPTABQ EQU   (*-DISPTAB)/(L'DISPTAB)                                          
                                                                                
                                                                                
*                                  DSPL TO DISPLAY LINE IN MKT SEQUENCE         
DLNMTB   DS    0AL(2+2)                                                         
         DC    AL2(LMDMNUM-LSTMKTD),AL2(DLMNUM-DEMTMPD)                         
         DC    AL2(LMDALFM-LSTMKTD),AL2(DLALFM-DEMTMPD)                         
         DC    AL2(LMDMNAM-LSTMKTD),AL2(DLMNAM-DEMTMPD)                         
         DC    AL2(LMDSTTN-LSTMKTD),AL2(DLSTTN-DEMTMPD)                         
         DC    AL2(LDDAY-LDSECT),AL2(DLDAY-DEMTMPD)                             
         DC    AL2(LDSTM-LDSECT),AL2(DLSTM-DEMTMPD)                             
         DC    AL2(LDETM-LDSECT),AL2(DLETM-DEMTMPD)                             
         DC    AL2(LDLIDEMO-LDSECT),AL2(DLLID-DEMTMPD)                          
         DC    AL2(LDDEMO-LDSECT),AL2(DLDEM-DEMTMPD)                            
         DC    AL2(LDLODEMO-LDSECT),AL2(DLLOD-DEMTMPD)                          
DLNMTBQ  EQU   (*-DLNMTB)/(L'DLNMTB)                                            
                                                                                
                                                                                
*                                  DSPL TO DISPLAY LINE IN STA SEQUENCE         
DLNSTB   DS    0AL(2+2)                                                         
         DC    AL2(LSDMNUM-LSTSTAD),AL2(DLMNUM-DEMTMPD)                         
         DC    AL2(LSDALFM-LSTSTAD),AL2(DLALFM-DEMTMPD)                         
         DC    AL2(LSDMNAM-LSTSTAD),AL2(DLMNAM-DEMTMPD)                         
         DC    AL2(LSDSTTN-LSTSTAD),AL2(DLSTTN-DEMTMPD)                         
         DC    AL2(LDDAY-LDSECT),AL2(DLDAY-DEMTMPD)                             
         DC    AL2(LDSTM-LDSECT),AL2(DLSTM-DEMTMPD)                             
         DC    AL2(LDETM-LDSECT),AL2(DLETM-DEMTMPD)                             
         DC    AL2(LDLIDEMO-LDSECT),AL2(DLLID-DEMTMPD)                          
         DC    AL2(LDDEMO-LDSECT),AL2(DLDEM-DEMTMPD)                            
         DC    AL2(LDLODEMO-LDSECT),AL2(DLLOD-DEMTMPD)                          
DLNSTBQ  EQU   (*-DLNSTB)/(L'DLNSTB)                                            
                                                                                
                                                                                
*                                  TABLE TO CONVERT KEY DAY VALUES              
DAYTAB   DS    0XL6                                                             
         DC    X'0',X'10',X'40',C'MON'                                          
         DC    X'0',X'20',X'20',C'TUE'                                          
         DC    X'0',X'30',X'10',C'WED'                                          
         DC    X'0',X'40',X'08',C'THU'                                          
         DC    X'0',X'50',X'04',C'FRI'                                          
         DC    X'0',X'60',X'02',C'SAT'                                          
         DC    X'0',X'70',X'01',C'SUN'                                          
         DC    C'T',X'95',X'7C',C'M-F'                                          
         DC    C'T',X'FF',X'FF',C'VAR'                                          
         DC    X'FF',X'FF',X'FF',C'???'                                         
*                                  TABLE FOR DEMO EDITTING ROUTINE              
EDITTAB  DS    0XL3                FILE/DEMO MODIFIER/EDIT RULES                
         DC    C'TC',X'01'                                                      
         DC    C'TI',X'01'                                                      
         DC    C'TR',X'01'                                                      
         DC    C'TP',X'01'                                                      
         DC    C'TS',X'01'                                                      
         DC    C'TQ',X'01'                                                      
         DC    C'TX',X'01'                                                      
         DC    C'TE',X'01'                                                      
         DC    C'TD',X'01'                                                      
         DC    C'PI',X'01'                                                      
         DC    C'PT',X'01'         (IS ACTUALLY TSA IMP, NOT TSA SHR)           
         DC    C'PR',X'01'                                                      
         DC    C'PP',X'01'                                                      
         DC    C'PS',X'01'                                                      
         DC    C'PQ',X'01'                                                      
         DC    C'PX',X'01'                                                      
         DC    C'PD',X'01'                                                      
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                  TABLE FOR DEMO EDITTING ROUTINE              
EDITTAB2 DS    0XL3                FILE/DEMO MODIFIER/EDIT RULES                
         DC    C'TC',X'01'         FOR DEC=2                                    
         DC    C'TI',X'01'                                                      
         DC    C'TR',X'02'                                                      
         DC    C'TP',X'01'                                                      
         DC    C'TS',X'01'                                                      
         DC    C'TQ',X'01'                                                      
         DC    C'TX',X'01'                                                      
         DC    C'TE',X'01'                                                      
         DC    C'TD',X'01'                                                      
         DC    C'PI',X'01'                                                      
         DC    C'PT',X'01'         (IS ACTUALLY TSA IMP, NOT TSA SHR)           
         DC    C'PR',X'02'                                                      
         DC    C'PP',X'01'                                                      
         DC    C'PS',X'01'                                                      
         DC    C'PQ',X'01'                                                      
         DC    C'PX',X'01'                                                      
         DC    C'PD',X'01'                                                      
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                  TABLE FOR DEMO EDITTING ROUTINE              
EDITTAB3 DS    0XL3                FILE/DEMO MODIFIER/EDIT RULES                
         DC    C'TC',X'00'         FOR DEC=0                                    
         DC    C'TI',X'00'                                                      
         DC    C'TR',X'00'                                                      
         DC    C'TP',X'00'                                                      
         DC    C'TS',X'00'                                                      
         DC    C'TQ',X'00'                                                      
         DC    C'TX',X'00'                                                      
         DC    C'TE',X'00'                                                      
         DC    C'TD',X'00'                                                      
         DC    C'PI',X'00'                                                      
         DC    C'PT',X'00'         (IS ACTUALLY TSA IMP, NOT TSA SHR)           
         DC    C'PR',X'00'                                                      
         DC    C'PP',X'00'                                                      
         DC    C'PS',X'00'                                                      
         DC    C'PQ',X'00'                                                      
         DC    C'PX',X'00'                                                      
         DC    C'PD',X'00'                                                      
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                  TABLE OF PROFILE INFORMATION                 
PROFTAB  DS    0X                                                               
PRFTBID  DS     CL4                 PROFILE ID                                  
PRFTBRTN DS     AL2                 ROUTINE TO GET PROFILE                      
PRFTBASV DS     AL2                 A(FIELD) TO STORE PROFILE                   
PRFTBLSV DS     XL1                 L(FIELD) TO STORE PROFILE                   
PRFTBQ   EQU   *-PROFTAB           L(PROFILE INFO ENTRY)                        
         ORG   PROFTAB                                                          
         DC    C'RRMP',AL2(GPRFRRMP00-GETPROFL),AL2(PROFRRMP-DEMWRKD)           
         DC    AL1(L'PROFRRMP)                                                  
         DC    C'S01W',AL2(GPRFS01W00-GETPROFL),AL2(PROF1W-DEMWRKD)             
         DC    AL1(L'PROF1W)                                                    
         DC    AL1(EOT)                                                         
***********************************************************************         
         TITLE 'DEDEM0A - $DEM MKT/STTNS DAY&&TIME/DEMOS FOR PROGRAM (D+        
               EDEMWRK)'                                                        
***********************************************************************         
*============================== DEDEMWRK =============================*         
       ++INCLUDE DEDEMWRK                                                       
         ORG   APSAVE                                                           
         SPACE 2                                                                
* SAVE STORAGE FOR OVERLAY                                                      
*                                                                               
PVMNUM   DS    XL2                                                              
PVSTTN   DS    CL5                                                              
STDOPROC DS    CL1                 Y=EXECUTE DEMPROC RTN, N=DON'T EXEC          
ANYDATA  DS    CL1                 ANY DATA FOR THIS REQUEST? (Y/N)             
RESUMAPP DS    CL1                 RESUMING FROM APPLC BREAK? (Y/X'00')         
                                                                                
         DS    0CL(L'APSAVE-(*-APSAVE)+1)                                       
         SPACE 2                                                                
         ORG                                                                    
*                                                                               
TWAUSED  EQU   *-DEMTWAD                                                        
TWAREMN  EQU   TPSTRPGL-TWAUSED                                                 
         DS    0CL(TWAREMN+1)      DON'T EXCEED LIMIT                           
***********************************************************************         
         EJECT                                                                  
       ++INCLUDE DEDBEXTRAD                                                     
***********************************************************************         
*======================== MISCELLANEOUS DSECTS =======================*         
                                                                                
* DSECT TO COVER TEMPORARY WORKING STORAGE                                      
*                                                                               
DEMTMPD  DSECT                                                                  
SAVERE   DS    A                   SAVED RE VALUE IN DEMHOOK                    
ALOCWRK  DS    A                   A(LOC WRK AREA)-USED B/W SUBRTN POOL         
ADEMVAL  DS    A                   A(AREA TO USE TO STORE DEMO VALUES)          
ASDQTAB  DS    A                   A(SDQTABD TABLE)                             
ASDQTABX DS    A                   A(LAST BYTE OF AREA FOR SDQTABD TBL)         
ASDQNTRY DS    A                   A(AN SDQTABD TABLE ENTRY)                    
ASUBRTN  DS    A                                                                
AVUTS    DS    A                   A(VUT VALUES PASSED TO PROCIUN)              
ADEMLIST DS    A                   A(DEMO LIST)                                 
                                                                                
MYBASES  DS    0A                  BASE REGS FOR MAIN DEM0A NMOD                
MYBASE2  DS    A                    A(2ND 4096 BYTES OF THIS PROGRAM)           
MYBASE1  DS    A                    A(1ST 4096 BYTES OF THIS PROGRAM)           
                                                                                
AIUNWRK  DS    A                   A(WORK AREA FOR IUN STUFF)                   
APROFILE DS    A                   A(PROFILE STORAGE AREA)                      
LPROFILE DS    XL1                 L(PROFILE STORAGE AREA)                      
                                                                                
ADEMD256 DS    A                   A(256-ENTRIES DEMO LIST)                     
ADVLI256 DS    A                   A(256-ENTRIES LEAD-IN  DEMOS)                
ADVPG256 DS    A                   A(256-ENTRIES PROGRAM  DEMOS)                
ADVLO256 DS    A                   A(256-ENTRIES LEAD-OUT DEMOS)                
ADMVL512 DS    A                   A(512-ENTRIES DEMO VALUES)                   
*                                                                               
DEMOFROM DS    XL(L'RCTFROM)                                                    
DEMOUPTO DS    XL(L'RCTUPTO)                                                    
RELO     DS    F                                                                
MYDMWORK DS    12D                                                              
MYDMCB   DS    6F                                                               
DEMOVALU DS    F                   HOLDS DEMO VALUE                             
MYDIVSOR DS    H                                                                
DIVIDEND DS    D                                                                
DIVISOR  DS    F                                                                
RMAINDER DS    F                                                                
QUOTIENT DS    F                                                                
TMPRTYP  DS    XL(L'TDRRTYP)      TEMP STORAGE FOR TSAR DEMR ECD TYPE           
PRECFLAG DS    CL1                PRECISION CHANGE FLAG                         
DEMOPRE  DS    CL1                DEMO PRECISON NUMBER                          
MYNMKT   DS    CL4                TEMP NUMERIC MKT STORAGE                      
PFLAG    DS    CL1                PROG FLAG                                     
PMKTRNK  EQU   X80                MARKET RANK                                   
PMULTPRG EQU   X40                MORE THAN ONE PROGRAM REQUESTED               
PPASSONE EQU  X20                DONE WITH FIRST PROGRAM- END                   
MARKETN  DS    XL2                 N'ACTIVE MARKETS FOR BOOK                    
MARKET   DS    XL2                 MARKET NUMBER                                
MARNAME  DS    CL30                MARKET NAME                                  
MARALF   DS    CL3                 ALPHA MARKET CODE                            
AFFILS   DS    CL5                 AFFILIATION CODE                             
MYPROF1W DS    XL(L'PROF1W)                                                     
MYDBLOCK DS    CL(DBLOCK1X-DBLOCK1)                                             
*                                                                               
ADLDNTRY DS    A                                                                
         DS    0A                 *************** ADCONS **************         
ADAYTAB  DS    A                   A(DAYTAB)                                    
AEDITTAB DS    A                   A(EDITTAB)                                   
AEDITTA2 DS    A                   A(EDITTAB2)                                  
AEDITTA3 DS    A                   A(EDITTAB3)                                  
ADLNMTB  DS    A                   A(DLNMTB)                                    
ADLNSTB  DS    A                   A(DLNSTB)                                    
AGOSUB   DS    A                   A(SUBROUTINE POOL INTERFACE)                 
ASUBR01  DS    A                   A(SUB-ROUTINE POOL ONE)                      
ASUBR02  DS    A                   A(SUBR02 POOL INTERFACE)                     
ASUBR03  DS    A                   A(SUBR03 POOL INTERFACE)                     
ADLDTABS DS    A                                                                
AGETDEM  DS    A                                                                
APROFTAB DS    A                   A(PROFTAB)                                   
                                                                                
         DS    0H                 ******* DSPL W/IN DISPLAY LINE ******         
DLMNUM   DS    H                   MARKET #                                     
DLALFM   DS    H                   ALPHA MARKET                                 
DLMNAM   DS    H                   MARKET NAME                                  
DLSTTN   DS    H                   STATION                                      
DLDAY    DS    H                   DAY                                          
DLSTM    DS    H                   EARLIEST START TIME                          
DLETM    DS    H                   LATEST   END   TIME                          
DLLID    DS    H                   LEAD-IN  DEMO                                
DLDEM    DS    H                   ACTUAL DEMO VALUE                            
DLLOD    DS    H                   LEAD-OUT DEMO                                
                                                                                
MBNPARMS DS    0F                 ******* MY BINSRCH PARAMETERS *******         
MBNAREC  DS    A                   A(BINSRCH RECORD)                            
MBNATAB  DS    A                   A(BINSRCH TABLE)                             
MBNNREC  DS    F                   NUMBER OF RECORDS                            
MBNLREC  DS    F                   L(RECORD)                                    
MBNDSPK  DS    0X                  DISPLACEMENT OF KEY INTO RECORD              
MBNLKEY  DS    F                   L(KEY)                                       
MBNMAXN  DS    F                   MAX # OF RECORDS                             
MBNPARML EQU   *-MBNPARMS                                                       
                                                                                
SDQCNT   DS    H                   N'ENTRIES IN SDQTABD TABLE                   
MKTCNT   DS    PL4                 N'MARKETS FOR PROGRAM                        
STACNT   DS    PL4                 N'STATIONS FOR PROGRAM                       
COUNTER  DS    PL4                 GENERAL COUNTER                              
STNDEMS  DS    XL1                 N'DEMOS FROM INPUTTED FROM STEREO            
                                                                                
MRKT     DS    XL2                                                              
STTN     DS    CL5                                                              
DAYTIME  DS    CL13                OUTPUT DAY/TIME VALUE                        
DAYBIN   DS    X                                                                
CDMODE   DS    CL1                 CALC DEMO MODE (I/P/O)                       
GOSUBN   DS    XL1                 SUB-ROUTINE NUMBER                           
POSTRTYP DS    XL1                 RECORD TYPE                                  
MYDATDSP DS    H                                                                
MYELCODE DS    XL1                                                              
PRECPROF DS    CL1                 PRECISION SET IN PROFILE                     
TAPEOPT  DS    CL1                 Y=TAPE BASED, N=BOOK BASED DEMO CALC         
ANYREAD  DS    CL1                                                              
PGDEMFLG DS    CL1                                                              
                                                                                
TEMPQHR  DS    XL1                 TEMP STORAGE FOR QUARTER HOUR                
TEMPMTIM DS    XL2                  "      "     "  MILITARY TIME               
TEMPKSE  DS    0XL(L'THISKDAY+L'THISSQH+L'THISEQH)                              
TEMPKDAY DS    XL(L'THISKDAY)                                                   
TEMPSQH  DS    XL(L'THISSQH)                                                    
TEMPEQH  DS    XL(L'THISEQH)                                                    
                                                                                
THISKEY  DS    0X                                                               
THISMNUM DS    XL2                                                              
THISSTA  DS    CL(L'SDQSTA)                                                     
THISKDAY DS    CL(L'SDQDAY)                                                     
THISSQH  DS    CL(L'SDQSQH)                                                     
THISEQH  DS    CL(L'SDQEQH)                                                     
THISKEYL EQU   *-THISKEY                                                        
THISALFM DS    CL3                                                              
*THISMNAM DS    CL(LMNAM)                                                       
THISMNAM DS    CL(LMNAM+5)         15 CHARS SHOULDB BE ENOUGH                   
THISDEMS DS    (MAXDEMS*2)F        THIS TIME'S DEMO VALUES                      
ACCUDEMS DS    (MAXDEMS)F          ACCUMULATED DEMO VALUES                      
ACCUFCTR DS    F                   ACCUMULATED DBFACTOR                         
                                                                                
DEMONDEM DS    XL(L'NDEMS)                                                      
DEMODEMS DS    (MAXDEMS)XL3,XL1     DEMO LIST + EOLIST                          
*                                                                               
MYFLAG1  DS    XL1                                                              
MYF1RDOK EQU    X80                 DEMAND READ OKAY--GOT DEMO RECDS            
                                                                                
         DS    0C                 ******** DEFINE RETURN VALUES *******         
DFPGNUM  DS    XL8                 PROGRAM NUMBER                               
DFPGNAM  DS    CL16                PROGRAM NAME                                 
DFMKNUM  DS    XL2                 MKT NUMBER                                   
DFMKNAM  DS    XL(LMNAM)           MKT NAME                                     
DFTIME   DS    0XL7                TIME                                         
DFTSQH   DS    XL(L'PISQH)          START QH                                    
DFTEQH   DS    XL(L'PIEQH)          END   QH                                    
         DS    XL(L'DFTIME-(*-DFTIME))  MILITARY TIMES & DURATION               
DFDAY    DS    XL(L'PIDAY)         DAY                                          
                                                                                
*                                 ******** AREA FOR DEMO VALUES *******         
DEMVLIN  DS    (MAXDEMS)F          LEAD-IN DEMO VALUES                          
DEMVPRG  DS    (MAXDEMS)F          PROGRAM'S DEMO VALUES                        
DEMVLOUT DS    (MAXDEMS)F          LEAD-OUT DEMO VALUES                         
                                                                                
QHVUTS   DS    3F                  VUT VALUES                                   
                                                                                
*                                 ************* MATH BLOCK ************         
HOMSHR   DS    3F                  SHARE VALUES                                 
HOMSHRL  EQU   *-HOMSHR                                                         
CUMSHR   DS    3F                  CUMULATIVE SHARE                             
CUMSHRL  EQU   *-CUMSHR                                                         
                                                                                
         DS    0F                                                               
MATHFAC  DS    0XL17                                                            
MTHCFACS DS     A                   A(DBLOCK)                                   
MTHFCTR  DS     F                   WEIGHTING FACTOR FOR MULT & DIVIDE          
MTHIFIL  DS     CL3                 INPUT FILE                                  
MTHOFIL  DS     CL3                 OUTPUT FILE                                 
MTHOSRC  DS     CL3                 OUTPUT SOURCE                               
*                                                                               
CUMFCTR  DS    F                   CUMULATIVE MATH FACTOR                       
*                                   X'80' = MULTIPLE PROGRAMS WITH              
*                                    DIFFERING ACTIVE WEEKS                     
*                                                                               
GKSPARM  DS    XL5                                                              
                                                                                
*                                 ************ SPDEMLK BLOCK **********         
SPDMLK_1 DS    XL(SPDEMLKL)                                                     
                                                                                
*                                 ************* BIG BUFFERS ***********         
SCRNLINE DS    CL(L'DEMHD1)                                                     
                                                                                
MYDBXTRA DS    XL128               DBLOCK EXTNSION (HOPE 128 IS ENOUGH)         
*                                                                               
DEMTMPQ  EQU   *-DEMTMPD                                                        
         EJECT                                                                  
*==================== THIS PHASE'S LOCAL WORK AREA ===================*         
                                                                                
DM0AWRKD DSECT                                                                  
DEMOD256 DS    256XL3,XL1          256-ENTRIES DEMOLIST                         
DMVLI256 DS    256F                256-ENTRIES LEAD-IN  DEMOS                   
DMVPG256 DS    256F                256-ENTRIES PROGRAM  DEMOS                   
DMVLO256 DS    256F                256-ENTRIES LEAD-OUT DEMOS                   
DMVAL512 DS    (256*2)F            512-ENTRIES DEMO VALUES                      
DM0AWRKX EQU   *                                                                
DM0AWRKL EQU   DM0AWRKX-DM0AWRKD                                                
         EJECT                                                                  
* DSECT TO COVER BINSRCH RECORD                                                 
*                                                                               
BINRECD  DSECT                                                                  
BINKEY   DS    0X                  BINSRCH KEY                                  
BINMNUM  DS    XL2                  MARKET NUMBER                               
BINSTAT  DS    CL(LSTTN)            STATION                                     
BINDAY   DS    XL(L'PIDAY)          DAY                                         
BINSQH   DS    XL(L'PISQH)          START QH                                    
BINEQH   DS    XL(L'PIEQH)          END   QH                                    
BINKEYL  EQU   *-BINKEY            L(BINSRCH KEY)                               
BINEND   EQU   *                                                                
BINRECL  EQU   BINEND-BINRECD                                                   
         SPACE 1                                                                
* DSECT TO COVER STATION/DAY/QHs TABLE                                          
*                                                                               
SDQTABD  DSECT                                                                  
SDQSTA   DS    CL(L'PISTA)         STATION                                      
SDQDAY   DS    XL(L'PIDAY)         DAY                                          
SDQSQH   DS    XL(L'PISQH)         START QH                                     
SDQEQH   DS    XL(L'PIEQH)         END   QH                                     
SDQLENQ  EQU   *-SDQTABD                                                        
                                                                                
                                                                                
* DSECT TO COVER PREVIOUS VALUES OUTPUTTED TO STEREO                            
*                                                                               
SPVOVALD DSECT                                                                  
SPOVMNUM DS    XL2                 MARKET NUMBER                                
SPOVSTA  DS    CL(LSTTN)           STATION CALL LETTERS                         
SPOVDAY  DS    XL(L'PIDAY)         DAY                                          
SPOVSQH  DS    XL(L'PISQH)         START QH                                     
SPOVEQH  DS    XL(L'PIEQH)         END   QH                                     
SPOVALFM DS    CL3                 ALPHA MARKET                                 
SPOVMNAM DS    CL(LMNAM)           MARKET NAME                                  
SPVOVALQ EQU   *-SPVOVALD                                                       
         DS    0CL(L'STPRVOVL-SPVOVALQ+1)                                       
         EJECT                                                                  
* DSECT TO COVER TSAR DEMO RECORD                                               
*                                                                               
TSDEMRCD DSECT                                                                  
TDRKEY   DS    0X                  TSAR DEMO RECORD KEY                         
TDRRTYP  DS    XL1                  RECORD TYPE (SEE BELOW)                     
TDRPNUM  DS    XL1                  NTH PROGRAM                                 
                                                                                
TDRMKEY  DS    0X                   LIST BY MARKET SEQUENCE                     
TDRMMNUM DS    XL2                   MARKET NUMBER                              
TDRMSTAT DS    CL(LSTTN)             STATION                                    
         ORG   TDRMKEY                                                          
TDRSKEY  DS    0X                   LIST BY STATION SEQUENCE                    
TDRSSTAT DS    CL(LSTTN)             STATION                                    
TDRSMNUM DS    XL2                   MARKET NUMBER                              
         ORG                                                                    
TDRDAY   DS    XL(L'PIDAY)          DAY                                         
TDRSQH   DS    XL(L'PISQH)          START QH                                    
TDREQH   DS    XL(L'PIEQH)          END   QH                                    
TDRKEYL  EQU   *-TDRKEY                                                         
         DS    0XL((L'TSARKEY-TDRKEYL)+1)                                       
                                                                                
TDRDATA  DS    0X                                                               
TDRDUMMY DS    XL1                  DUMMY DATA (FOR TSAR TO WORK)               
TDRFIXL  EQU   *-TSDEMRCD                                                       
*                                                                               
         ORG   TDRDATA                                                          
TDRDATA2 DS    0X                                                               
TDRDEMS  DS    0XL3                 DEMO VALUES                                 
TDRLENQ  EQU   *-TSDEMRCD                                                       
                                                                                
* TDRRTYP can have the following values:                                        
TDRRTDML EQU   X'10'               RECORD CONTAINS DEMO LIST                    
TDRRTMSG EQU   X'40'               RECORD CONTAINS INFO MESSAGE                 
TDRRTHDR EQU   X'80'               RECORD SIGNIFIES TO OUTPUT COL HDR           
TDRRTPGI EQU   X'C0'               RECORD CONTAINS PROGRAM INFORMATION          
TDRRTXFF EQU   XFF                 (RESERVED FOR DEM00)                         
                                                                                
                                                                                
                                                                                
* TSAR DEMO RECORD FOR ACTION=PROGMRNK                                          
         ORG   TDRSKEY                                                          
TDRAKEY  DS    0C                  LIST BY ALPHA (MKT NAME) SEQUENCE            
TDRAMNAM DS    CL(L'MARNAME)        MARKET NAME                                 
TDRAMRKT DS    XL(L'MARKET)         MARKET NUMBER                               
*                                                                               
         ORG   TDRAKEY                                                          
TDRNKEY  DS    0C                  LIST BY NUMERIC (MKT #) SEQUENCE             
TDRNMRKT DS    XL2                  MARKET NUMBER                               
TDRNMNAM DS    CL30                 MARKET NAME                                 
         ORG                                                                    
TDRKEY3L EQU   *-TDRKEY                                                         
         DS    0XL((L'TSARKEY-TDRKEY3L)+1)                                      
                                                                                
TDRDATA3 DS    0X                                                               
TDRALF   DS     CL3                                                             
                                                                                
TDRLEN3Q EQU   *-TSDEMRCD                                                       
* TDRRTYP CAN HAVE THE FOLLOWING VALUES:                                        
TDRRTMKT EQU   X'C0'               RECORD CONTAINS MARKET INFORMATION           
*                                                                               
         EJECT                                                                  
* DSECT TO COVER AN ENTRY ON DISPLAY LINE (MARKET SEQUENCE)                     
*                                                                               
LSTMKTD  DSECT                                                                  
LMDHMNUM DS    0CL5                'MKT #' HEADING                              
         DS    CL1                                                              
LMDMNUM  DS    CL(LMNUM)                                                        
         DS    0CL((L'LMDHMNUM-(*-LMDHMNUM))+1)                                 
         DS    0CL(((*-LMDHMNUM)-L'LMDHMNUM)+1)                                 
                                                                                
         DS    CL1                                                              
                                                                                
LMDHALFM DS    0CL5                'ALPHA' HEADING                              
         DS    CL1                                                              
LMDALFM  DS    CL(LALFM)                                                        
         DS    CL1                                                              
         DS    0CL((L'LMDHALFM-(*-LMDHALFM))+1)                                 
         DS    0CL(((*-LMDHALFM)-L'LMDHALFM)+1)                                 
                                                                                
         DS    CL1                                                              
                                                                                
LMDHMNAM DS    0CL(LMNAM)          MARKET NAME                                  
LMDMNAM  DS    CL(LMNAM)                                                        
                                                                                
         DS    CL2                                                              
                                                                                
LMDHSTTN DS    0CL7                STATION                                      
         DS    CL1                                                              
LMDSTTN  DS    CL(LSTTN)                                                        
         DS    CL1                                                              
         DS    0CL((L'LMDHSTTN-(*-LMDHSTTN))+1)                                 
         DS    0CL(((*-LMDHSTTN)-L'LMDHSTTN)+1)                                 
*                                                                               
LSTMKTQ  EQU   *-LSTMKTD                                                        
         EJECT                                                                  
* DSECT TO COVER AN ENTRY ON DISPLAY LINE (STATION SEQUENCE)                    
*                                                                               
LSTSTAD  DSECT                                                                  
LSDHSTTN DS    0CL7                'STATION' HEADING                            
         DS    CL1                                                              
LSDSTTN  DS    CL(LSTTN)                                                        
         DS    CL1                                                              
         DS    0CL((L'LSDHSTTN-(*-LSDHSTTN))+1)                                 
         DS    0CL(((*-LSDHSTTN)-L'LSDHSTTN)+1)                                 
                                                                                
         DS    CL2                                                              
                                                                                
LSDHMNUM DS    0CL5                'MKT #' HEADING                              
         DS    CL1                                                              
LSDMNUM  DS    CL(LMNUM)                                                        
         DS    0CL((L'LSDHMNUM-(*-LSDHMNUM))+1)                                 
         DS    0CL(((*-LSDHMNUM)-L'LSDHMNUM)+1)                                 
                                                                                
         DS    CL1                                                              
                                                                                
LSDHALFM DS    0CL5                'ALPHA' HEADING                              
         DS    CL1                                                              
LSDALFM  DS    CL(LALFM)                                                        
         DS    CL1                                                              
         DS    0CL((L'LSDHALFM-(*-LSDHALFM))+1)                                 
         DS    0CL(((*-LSDHALFM)-L'LSDHALFM)+1)                                 
                                                                                
         DS    CL1                                                              
                                                                                
LSDHMNAM DS    0CL(LMNAM)          'MARKET NAME' HEADING                        
LSDMNAM  DS    CL(LMNAM)                                                        
*                                                                               
LSTSTAQ  EQU   *-LSTSTAD                                                        
                                                                                
         DS    0CL((LSTMKTQ-LSTSTAQ)+1)                                         
         DS    0CL((LSTSTAQ-LSTMKTQ)+1)                                         
         EJECT                                                                  
* DSECT TO COVER DAY/TIMES OR DEMOS ON DISPLAY LINE                             
*                                                                               
LDSECT   DSECT                                                                  
         DS    XL(LSTMKTQ)                                                      
         DS    XL2                                                              
                                                                                
LDHDAY   DS    0CL(LDAY)           'DAY' HEADING                                
LDDAY    DS    CL(LDAY)                                                         
                                                                                
         DS    XL1                                                              
                                                                                
LDHSTM   DS    0CL8                'EARLIEST' HEADING                           
         DS    CL1                                                              
LDSTM    DS    CL(LTIME)                                                        
         DS    CL2                                                              
                                                                                
         DS    XL1                                                              
                                                                                
LDHETM   DS    0CL6                'LATEST' HEADING                             
         DS    CL1                                                              
LDETM    DS    CL(LTIME)                                                        
                                                                                
         DS    CL2                                                              
                                                                                
LDHLDIN  DS    0CL(LDEMO)          'LEAD IN' HEADING                            
LDLIDEMO DS    CL(LDEMO)                                                        
                                                                                
         DS    XL1                                                              
                                                                                
LDHDEMO  DS    0CL(LDEMO)          HEADING FOR DEMO NAME                        
LDDEMO   DS    CL(LDEMO)                                                        
                                                                                
         DS    XL1                                                              
                                                                                
LDHLDOUT DS    0CL(LDEMO)          'LEAD OUT' HEADING                           
LDLODEMO DS    CL(LDEMO)                                                        
LDLINEQ  EQU   *-LDSECT                                                         
         DS    0CL(L'DEMLN1-LDLINEQ+1)                                          
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================== DDCOMFACS ============================*         
COMFACSD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================= REP DSECTS ============================*         
                                                                                
*------------------------------ REGENREP -----------------------------*         
         PRINT OFF                                                              
       ++INCLUDE REGENREPA                                                      
         PRINT ON                                                               
                                                                                
                                                                                
*----------------------------- RERMPPROF -----------------------------*         
         PRINT OFF                                                              
       ++INCLUDE RERMPPROF                                                      
         PRINT ON                                                               
                                                                                
                                                                                
*------------------------------ REGENINV -----------------------------*         
REINVRCD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENINVA                                                      
         PRINT ON                                                               
                                                                                
                                                                                
*------------------------------ REGENAVL -----------------------------*         
         PRINT OFF                                                              
       ++INCLUDE REGENAVL                                                       
         PRINT ON                                                               
***********************************************************************         
         EJECT                                                                  
DLDTABD  DSECT                                                                  
DLDTLEN  DS    XL1                 L(ENTRY)                                     
DLDTFMS  DS    0CL(L'DBFIL+L'DBSRC+L'DBMED)                                     
DLDTFIL  DS     CL(L'DBFIL)         FILE                                        
DLDTSRC  DS     CL(L'DBSRC)         SOURCE                                      
DLDTMED  DS     CL(L'DBMED)         MEDIA                                       
DLDTRTYP DS    XL(L'TDRRTYP)      TSAR DEMO RECORD TYPE                         
DLDTVRSN DS    XL(L'D32PCVER)      STEREO DEM EXTRACT VERSION                   
DLDTFIXL EQU   *-DLDTABD                                                        
                                                                                
DLDTDATA DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
*========================== IUN RECORD DSECT =========================*         
                                                                                
IUNRECD  DSECT                                                                  
*                                                                               
IUNIVS   DS    (IUNNVALS)F        UNIVERSES                                     
*                                                                               
IUNOLD   DS    0F                 ORIGINAL (OLD) BOOK VALUES                    
IOLDRTG  DS    (IUNNVALS)F         RATINGS                                      
         ORG   IOLDRTG+IUNHMDSP                                                 
IUORHOME DS    F                                                                
         ORG                                                                    
IOLDIMP  DS    (IUNNVALS)F         IMPRESSIONS                                  
IOLDHPT  DS    (IUNNVALS)F         HUTS/PUTS                                    
         ORG   IOLDHPT+IUNHMDSP                                                 
IUOPHOME DS    F                                                                
         ORG                                                                    
IOLDTOT  DS    (IUNNVALS)F         TSA TOTALS                                   
         ORG   IOLDTOT+IUNHMDSP                                                 
IUOQHOME DS    F                                                                
         ORG                                                                    
IUNOLDX  EQU   *                                                                
*                                                                               
IUNNEW   DS    0F                 NEW VALUES                                    
INEWRTG  DS    (IUNNVALS)F         RATINGS                                      
         ORG   INEWRTG+IUNHMDSP                                                 
IUNRHOME DS    F                                                                
         ORG                                                                    
INEWIMP  DS    (IUNNVALS)F         IMPRESSIONS                                  
INEWHPT  DS    (IUNNVALS)F         HUTS/PUTS                                    
         ORG   INEWHPT+IUNHMDSP                                                 
IUNPHOME DS    F                                                                
         ORG                                                                    
INEWTOT  DS    (IUNNVALS)F         TSA TOTALS                                   
         ORG   INEWTOT+IUNHMDSP                                                 
IUNQHOME DS    F                                                                
         ORG                                                                    
IUNNEWX  EQU   *                                                                
                                                                                
         DS    0CL((IUNOLDX-IUNOLD)-(IUNNEWX-IUNNEW)+1)                         
         DS    0CL((IUNNEWX-IUNNEW)-(IUNOLDX-IUNOLD)+1)                         
*                                                                               
IUNOTH   DS    0F                 OTHER VALUES                                  
ISHOMES  DS    F                                                                
ISMETA   DS    F                                                                
ISMETB   DS    F                                                                
                                                                                
ILUNVS   DS    (IUNNVALS)F         LOONEYVERSES                                 
ILUNVX   EQU   *                                                                
*                                                                               
IUNRECL  EQU   *-IUNRECD                                                        
                                                                                
                                                                                
IUNNVALS EQU   32                  # OF IUN VALUES                              
IUNLVALS EQU   IUNNVALS*4          LENGTH OF IUN VALUES                         
IUNHMNDX EQU   20                  INDEX TO HOMES RTGS IN IUN RTGS AREA         
IUNHMDSP EQU   IUNHMNDX*4          DISPL TO HOMES RTGS IN IUN RTGS AREA         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*====================== APPLICATION SAVE AREA #2 =====================*         
                                                                                
APSAV2D  DSECT                                                                  
APSAPMOD DS    XL(L'APMODE)                                                     
*                                                                               
APSAV2Q  EQU   *-APSAV2D                                                        
                                                                                
                                                                                
         DS    0XL(APSAV2L-APSAV2Q+1)   DON'T EXCEED STORAGE ALLOTTED           
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'122DEDEM0A   05/16/11'                                      
         END                                                                    
