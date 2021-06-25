*          DATA SET DEDEM0C    AT LEVEL 161 AS OF 10/14/20                      
*PHASE T21B0CE,*                                                                
*INCLUDE NETUNBK                                                                
*INCLUDE DEMTIME                                                                
T21B0C   TITLE 'DEDEM0C - $DEM LILO REPORT                      '               
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                               
* -------- --- ----   ------------------------------------------------*         
* SEP02/03 148 BPOO - RELINKED WITH NEW WORK AREA TO SUPPORT          *         
*                     WTP MBKS AND IMPRESSION BASED MBK CALCULATION   *         
*                     ADDED CODE TO CHECK IOCOUNT TO PREVENT          *         
*                     DEATH IN TASKNEXT                               *         
* NOV04/00 144 BPOO - SUPPORT USERID AUTHORIZATION                    *         
* NOV02/00 143 BPOO - MOVE DEM TABLES TO DEM81 PHASE                  *         
* SEP22/00 142 BPOO - falink stuff...change screen one line lower     *         
* Jun20/00 141 GLEE - Change versioning scheme to support PC vrsn stmp*         
*                                                                     *         
* Mar22/00 140 GLEE - Adjust QHs referenced thru DBACTSQH & DBACTEQH  *         
*                                                                     *         
* Mar08/00 137 GLEE - Use new TSAR I/O area for TSAR routines         *         
*              GLEE - Relinked for bigger buy record I/O area         *         
*                                                                     *         
* Sep13/99 135 GLEE - Set CC upon exiting DEMPROC mode                *         
*                                                                     *         
* Aug05/99 134 GLEE - Deal with start/end times changes with QHs      *         
*                                                                     *         
* Jul29/99 133 GLEE - Add DLDATTAB entry for NHTI TP                  *         
*                                                                     *         
* Feb11/99 130 GLEE - Implement version (extract) dates for DEM32     *         
*                                                                     *         
* JUL23/98 094 BPOO - NO DEMO DOWNLOADS DUMMY DEMO HEADER FOR TP                
*                     NO DEMOS (INV BOOK) DOWNLOAD NOTHING FOR PAV              
***********************************************************************         
         EJECT                                                                  
DEM0C    CSECT                                                                  
         ENTRY TIMVAL                                                           
         ENTRY UNTIME                                                           
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 LOCWORKL,**DEMC**,RA,CLEAR=YES,RR=RE                             
         USING DEMWRKD,R9          R9=A(GLOBAL W/S)                             
         USING DEMTWAD,R8          R8=A(TWA)                                    
         L     R7,AAPWORK                                                       
         USING DEMTMPD,R7          R7=A(TEMP W/S)                               
         ST    RC,AIUNWRK                                                       
         AHI   RC,IUNWRKL                                                       
         USING DM0CWRKD,RC                                                      
*                                                                               
         STM   RA,RB,MYBASES                                                    
         ST    RE,RELO0C                                                        
                                                                                
         ST    RD,DM0CW_RD                                                      
*                                                                               
         DS    0H                  SET UP ADCONS                                
         L     R0,=V(QHTOHR)                                                    
         LH    R1,=Y(DISPTAB-DEM0C)                                             
         LA    R1,DEM0C(R1)                                                     
         LA    R0,DISPTABQ                                                      
DEM0C_10 SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         LA    RE,DEM0C(RE)        RE=A(TABLE)                                  
         SR    RF,RF                                                            
         ICM   RF,3,2(R1)                                                       
         LA    RF,DEMTMPD(RF)      RF-->PLACE TO STORE A(TABLE)                 
         ST    RE,0(RF)                                                         
         LA    R1,L'DISPTAB(R1)                                                 
         BCT   R0,DEM0C_10                                                      
*                                                                               
         DS    0H                                                               
         L     R0,=V(HRTOQH)                                                    
         A     R0,RELO0C                                                        
         ST    R0,VHRTOQH                                                       
         L     R0,=V(QHTOHR)                                                    
         A     R0,RELO0C                                                        
         ST    R0,VQHTOHR                                                       
*                                  HANDLE CONTROLLER MODE SETTINGS              
         CLI   APMODE,FORMHEAD     FORMAT HEADLINES & INITIALISE                
         BE    DEMHEAD                                                          
         CLI   APMODE,PROCESS      READ & POST RECORDS TO BUFFER                
         BE    DEMPROC                                                          
         CLI   APMODE,FORMLINE     FORMAT & PRINT A BUFFER RECORD               
         BE    DEMLINE                                                          
         CLI   APMODE,PROCREC      CONTINUATION RECORD                          
         BE    DEMLINE                                                          
         CLI   APMODE,STERTASK     STEREO SESSION TASKS                         
         BE    DEMSTTSK                                                         
         CLI   APMODE,APMBREAK     MAINFRAME TRANSACTION BREAK TASKS            
         BE    DEMBREAK                                                         
         CLI   APMODE,APMRESUM     MAINFRAME TRANSACTION RESUME TASKS           
         BE    DEMRESUM                                                         
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
                                                                                
*                                                                               
XITMODH  DS    0H                  EXIT W/ CC HIGH                              
         LA    R0,1                                                             
         J     XITMODCR                                                         
                                                                                
XITMODL  DS    0H                  EXIT W/ CC LOW                               
         LHI   R0,-1                                                            
         J     XITMODCR                                                         
                                                                                
XITMODE  DS    0H                  EXIT W/ CC EQ                                
         SR    R0,R0                                                            
         J     XITMODCR                                                         
                                                                                
XITMODCR DS    0H                                                               
         AR    R0,R9                                                            
         CR    R0,R9               SET CC                                       
         J     XITMOD               AND EXIT                                    
                                                                                
XITMOD   DS    0H                  EXITS TO DEM02 CALLER                        
         L     RD,DM0CW_RD                                                      
         XMOD1                                                                  
         EJECT                                                                  
* FORMAT HEADLINES & INITIALIZE FOR DEMO LOOK-UPS                               
*                                                                               
DEMHEAD  DS    0H                                                               
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BO    DEMHD10              GO TO STEREO LOGIC                          
*                                                                               
DEMHD10  DS    0H                  THIS PART FOR STEREO SESSION ONLY            
         LA    R2,TSARBLCK                                                      
         USING TSARD,R2                                                         
         CLI   TSKEYL,0            SEE IF WE NEED TO SET LENGTHS AGAIN          
         BNE   DEMHD20              NOPE, THEY WERE SET BEFORE                  
                                                                                
         MVI   TSKEYL,TDRKEYL      SET L(TSAR DEMO RECD KEY)                    
         MVC   TSRECL,LTSIOREC     SET MAX RECORD LENGTH                        
         DROP  R2                                                               
*                                                                               
DEMHD20  DS    0H                  SET UP DEMO LIST                             
         L     RE,ASTDMEXP                                                      
         ZIC   RF,0(RE)            UPDATE STEREO'S COUNT OF # OF DEMOS          
         ZIC   R1,NDEMS                                                         
         AR    RF,R1                                                            
         STC   RF,0(RE)                                                         
         STC   R1,DEMONDEM                                                      
                                                                                
         SR    R0,R0                                                            
         LA    RF,L'DEMS                                                        
         MR    R0,RF                                                            
         EXMVC R1,DEMODEMS,DEMS    INCLUDES LIST TERMINATOR                     
                                                                                
         B     DEMHEADX                                                         
*                                                                               
DEMHEADX B     EXIT                                                             
         EJECT                                                                  
* READ DEMO FILES & POST TO BINSRCH BUFFER                                      
* SEQUENCE IS DAY/TIME WITHIN BOOK WITHIN STATION                               
*                                                                               
DEMPROC  LA    R4,DBSTABK-STABKL                                                
         USING STABKD,R4                                                        
         MVI   THISSTA,0                                                        
*                                                                               
         XC    WORK,WORK           SET UP 1W PROFILE KEY                        
         MVC   WORK+0(4),=C'S01W'                                               
         MVC   WORK+4(2),AGYALPH                                                
         MVI   WORK+6,C'T'                                                      
         MVC   WORK+7(3),OPTCLI                                                 
         GOTO1 VGETPROF,DMCB,WORK,PROF1W,VDATAMGR                               
         MVC   PRECPROF,PROF1W+5   GET PRECISION FROM (SPOT) 1W PROFILE         
         MVC   CANMMBK,PROF1W+8    GET CANADIAN MTR MKT POSTING PROFILE         
                                                                                
         DS    0H                  GET REP PROFILE IF SYSTEM IS REP             
         L     RF,ASYSNTRY                                                      
         CLI   (SYSOVSYS-SYSTABD)(RF),8                                         
         BNE   DP012X                                                           
         MVC   DUB(4),=C'RRMP'                                                  
         MVI   GOSUBN,GPRF#                                                     
         GOTO1 AGOSUB                                                           
*****    MVI   PRECPROF,C'R'        RESET PRECISION                             
*****    TM    PROFRRMP+RMPIMPSB,RMPIMPSA                                       
*****    BZ    *+8                                                              
*****    MVI   PRECPROF,C'I'         ACCORDING TO THE REP RMP PROFILE           
DP012X   EQU   *                                                                
* EVERYTHING FORCED TO BE IMPRESSION BASED                                      
         MVI   PRECPROF,C'I'         ACCORDING TO THE REP RMP PROFILE           
*&&DO                                                                           
         CLI   OPTDMA,0            WAS PRECISION SPECIFIED?                     
         BE    *+10                                                             
         MVC   PRECPROF,OPTDMA      YES, PASS IT TO THE PROFILE                 
*&&                                                                             
         DS    0H                  SET CANADIAN MTR MKT BK (WK/MTH)             
         NI    MYFLAG1,XFF-MYF1CNT  ASSUME IT'S NOT CSI TP                      
         CLI   DBMED,C'C'           CANADIAN MEDIA                              
         BNE   DP016X                                                           
         CLI   DBSRC,C'N'           NIELSEN                                     
         BNE   DP016X                                                           
         CLC   DBFIL,=C'TP '        TIME PERIOD                                 
         BNE   DP016X                                                           
         OI    MYFLAG1,MYF1CNT      IT IS CSI TP!                               
                                                                                
         MVI   CANMMBK,C'N'         ASSUME BOOK IS A WEEK                       
         TM    MISCFLG1,MF1BKVWK    WAS IT VALIDATED AS A WEEK?                 
         BNZ   *+8                                                              
         MVI   CANMMBK,C'M'          NOPE, A MONTH BOOK WAS ENTERRED            
DP016X   EQU   *                                                                
*                                                                               
         DS    0H                  SET TAPE/BOOK BASED DEMO CALC                
         MVI   TAPEOPT,C'Y'         (PATCHABLE TAPE BASED OPTION)               
******** MVI   TAPEOPT,C'N'         BOOK BASED                                  
**  EVERYTHING WILL BE IMPRESSIONS BASED                                        
         CLI   DBMED,C'N'                                                       
         BE    DP017                                                            
         CLI   DBSRC,C'F'           FUSION                                      
         BE    *+8                                                              
         CLI   DBSRC,C'N'           TAPE BASED FOR NIELSEN                      
         BNE   DP029                                                            
         CLI   DBMED,C'O'            OVERNIGHTS                                 
         BE    *+8                                                              
         CLI   DBMED,C'T'            USTV ONLY                                  
         BNE   DP029                                                            
DP017    DS    0H                                                               
******   CLI   PRECPROF,C'I'        IMP BASED <==> TAPE BASED                   
*******  BNE   DP029                                                            
* EVERYTHING WILL BE IMPRESSIONS BASED                                          
         MVI   TAPEOPT,C'Y'                                                     
DP029    EQU   *                                                                
*                                  NEXT STATION                                 
         MVC   SVSETIME,1(R2)                                                   
         MVI   THISRTYP,FMDTPDPRTYP_RO                                          
DEMPROC2 ZIC   R1,THISSTA                                                       
         LA    R1,1(R1)                                                         
         STC   R1,THISSTA                                                       
         LA    R4,STABKL(R4)                                                    
         CLC   THISSTA,NSTAS       TEST ALL STATIONS PROCESSED                  
         BH    DMPROC10                                                         
         MVI   THISBK,0                                                         
         LA    R3,STABKS-L'STABKS                                               
                                                                                
         LA    R0,L'ALFMKTS        ALPHA MKT INPUT (IN CASE THERE IS)           
         ZIC   R1,THISSTA                                                       
         BCTR  R1,0                                                             
         MR    R0,R0                                                            
         LA    R1,ALFMKTS(R1)                                                   
         MVC   ALPHAMKT,0(R1)                                                   
*                                                                               
         TM    DEMFLAG1,DF1STERO                                                
         BZ    DEMPROC2G                                                        
         ZIC   RF,THISSTA                                                       
         LH    RE,DSPRCTST                                                      
         LA    RE,REQCTLTB(RE)                                                  
         ZIC   R0,(RCTFROM-RCTDSECT)(RE)                                        
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
         CLM   RF,1,SVTHSSTN                                                    
         BE    *+12                                                             
         MVI   STBKDATA,C'N'                                                    
         STC   RF,SVTHSSTN                                                      
DEMPROC2G EQU  *                                                                
*                                  NEXT BOOK FOR STATION                        
DEMPROC4 ZIC   R1,THISBK                                                        
         LA    R1,1(R1)                                                         
         STC   R1,THISBK                                                        
         LA    R3,L'STABKS(R3)                                                  
         CLC   THISBK,NBKS         TEST ALL BOOKS PROCESSED                     
         BH    DEMPROC2                                                         
         CLC   0(L'INVBOOK,R3),INVBOOK                                          
         BE    DEMPROC4                                                         
         MVI   THISDAY,0                                                        
         LA    R2,DAYS-L'DAYS                                                   
                                                                                
         NI    MYFLAG1,XFF-MYF1LNBK  IF PROCESSING A LATEST BOOK RQST,          
         CLI   0(R3),XFF                                                        
         BNE   *+8                                                              
         OI    MYFLAG1,MYF1LNBK       FLAG IT AS SUCH                           
                                                                                
         TM    DEMFLAG1,DF1STERO                                                
         BZ    DEMPROC4G                                                        
         ZIC   RF,THISBK                                                        
         LH    RE,DSPRCTBK                                                      
         LA    RE,REQCTLTB(RE)                                                  
         ZIC   R0,(RCTFROM-RCTDSECT)(RE)                                        
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
         CLM   RF,1,SVTHSBK                                                     
         BE    *+12                                                             
         MVI   STBKDATA,C'N'                                                    
         STC   RF,SVTHSBK                                                       
DEMPROC4G EQU  *                                                                
                                                                                
*                                  NEXT DAY/TIME FOR STATION/BOOK               
DEMPROC6 ZIC   R1,THISDAY                                                       
         LA    R1,1(R1)                                                         
         STC   R1,THISDAY                                                       
         LA    R2,L'DAYS(R2)                                                    
         CLC   THISDAY,NDAYS       TEST ALL DAY/TIMES PROCESSED                 
         BH    DEMPROC4                                                         
                                                                                
         TM    DEMFLAG1,DF1STERO+DF1DEM32  IF DEM32 SESSION,                    
         BNO   DMPRC07G                                                         
         GOTO1 AIOCHECK                     CHECK I/O COUNT FOR TRNSCTN         
         BH    DMPRC07G                                                         
         MVI   FERN,120                    "RQST TOO BIG" ERROR MSG             
         MVI   APMODE,FORCEEND                                                  
         J     XITMODL                                                          
DMPRC07G EQU   *                                                                
                                                                                
         DS    0H                  LATEST  N  BOOKS STUFF                       
         ZAP   THISLBK,=P'0'                                                    
         XC    PVACTBK,PVACTBK                                                  
         XC    LNBKLIST(L'LNBKLIST*4),LNBKLIST                                  
                                                                                
         DS    0H                  OTHERS                                       
         XC    THISSWKN(2),THISSWKN  START/END WEEKS OF MONTH                   
                                                                                
*                                                                               
         DS    0H                  GET REQUEST START/END TIME IN QHs            
         XC    SVSEQH,SVSEQH        INITIALIZE THEM FIRST                       
         MVI   THISRTYP,XFF         SPECIAL CALL TO DEMLOOK                     
         BAS   RE,DEMLOOK                                                       
                                                                                
         ZIC   R1,SVSEQHS                                                       
         BCTR  R1,0                                                             
         STC   R1,SVSEQHS           MAKE START QH ZERO-BASED                    
         ZIC   R1,SVSEQHE                                                       
         BCTR  R1,0                                                             
         STC   R1,SVSEQHE           MAKE END   QH ZERO-BASED                    
*                                                                               
         MVC   SVSETIME,1(R2)      SAVE REQUEST'S START/END TIMES               
                                                                                
*                                                                               
         DS    0H                                                               
         L     R6,AROTQHTB                                                      
         USING ROQHTABD,R6                                                      
*                                                                               
DP039G   DS    0H                                                               
         CLI   0(R6),EOT                                                        
         BE    DP039X                                                           
*                                                                               
         DS    0H                  ROTATION TYPE                                
         MVC   THISRTYP,ROQHRTYP                                                
                                                                                
         DS    0H                  ROTATION'S START TIME                        
         ZICM  RF,ROQHSQH,(3)                                                   
         LA    RF,DEMTMPD(RF)                                                   
         ZIC   RE,0(RF)                                                         
         AH    RE,ROQHSQHA                                                      
         BM    DP039P               SKIP THIS ROTATION IF START < 0             
         STC   RE,THISSQH                                                       
         GOTO1 VQHTOHR,DMCB,THISSQH,(0,HALF)                                    
         MVC   THISSETM+0(2),HALF                                               
                                                                                
         DS    0H                  ROTATION'S END   TIME                        
         ZICM  RF,ROQHEQH,(3)                                                   
         LA    RF,DEMTMPD(RF)                                                   
         ZIC   RE,0(RF)                                                         
         AH    RE,ROQHEQHA                                                      
         CHI   RE,95                                                            
         BH    DP039P               SKIP THIS ROTATION IF END > 95              
         STC   RE,THISEQH                                                       
         GOTO1 VQHTOHR,DMCB,THISEQH,(0,HALF)                                    
         MVC   THISSETM+2(2),HALF                                               
*                                                                               
         MVC   1(4,R2),THISSETM                                                 
         BAS   RE,DEMLOOK                                                       
*                                                                               
DP039P   DS    0H                                                               
         AHI   R6,L'ROTQHTAB                                                    
         B     DP039G                                                           
DP039X   EQU   *                                                                
         DROP  R6                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   1(4,R2),SVSETIME    RESTORE REQUEST'S START/END TIMES            
         B     DEMPROC6                                                         
                                                                                
*                                                                               
         EJECT                                                                  
DMPROC10 DS    0H                  ALL STTN/BOOK/DAY&TIME PROCESSED             
         MVI   THISSTA,0                                                        
         MVI   THISDAY,0                                                        
         MVI   THISBK,0                                                         
*                                                                               
* This code special for STEREO session only.  We'll make a TSAR DEMO            
*  RECORD out of the demo expressions and make it the first record              
*  of the TSAR buffer.  This way, we'll know what the numerical demo            
*  values represent when we get them back in for APMODE=FORMLINE.               
         TM    DEMFLAG1,DF1STERO   IF NOT A STEREO SESSION,                     
         BZ    EXIT                 DON'T EXECUTE LOGIC BELOW                   
         MVI   RECTYPE,TDRRTDML                                                 
         MVI   THISSTA,0                                                        
         MVI   THISBK,0                                                         
         MVI   THISDAY,0                                                        
         MVI   THISDAYR,XFF                                                     
         MVI   THISKDAY,XFF                                                     
         MVI   THISSQH,XFF                                                      
         MVI   THISEQH,XFF                                                      
         MVI   THISTYPE,0                                                       
         MVI   THISPROG,C' '                                                    
         MVC   THISPROG+1(L'THISPROG-1),THISPROG                                
         BAS   RE,DEMPOST                                                       
                                                                                
*        MVI   GOSUBN,DDT#         POST DUMMY RECORDS (IF NEEDED)               
*        GOTO1 AGOSUB                                                           
*                                                                               
DEMPROCX DS    0H                                                               
         B     EXITE                                                            
         EJECT                                                                  
* INTERFACE TO DEMUP FOR UPGRADES                                               
*                                                                               
DEMUPGD  NTR1                                                                   
         LA    R5,SPDEMUP1         INITIALIZE UPGRADE BLOCK                     
         USING SPDEMUPD,R5         R5=A(UPGRADE BLOCK)                          
         XC    SPDEMUPD(SPDEMUPL),SPDEMUPD                                      
         MVC   SPUPAREC,AIOAREA1                                                
         MVC   SPUPAFAC,AFAC                                                    
         MVC   SPUPAGY,AGYALPH                                                  
         MVC   SPUPMED,DBMED                                                    
         MVC   SPUPSTA,STASTA                                                   
         MVC   SPUPDAY,0(R2)                                                    
         MVC   SPUPTIM,1(R2)                                                    
         MVC   SPUPSPL,STASPILL                                                 
         MVC   SPUPFIL,OPTUPFIL                                                 
         MVC   SPUPBTYP,OPTUPBT                                                 
         CLI   SPUPFIL,0                                                        
         BNE   *+8                                                              
         MVI   SPUPFIL,C'T'                                                     
         MVC   SPUPSRC,DBSRC                                                    
         MVC   SPUPTPTT,DBTPTTS                                                 
         MVC   SPUPFBK,OPTUPBK                                                  
         MVC   SPUP2YRP,OPT2YRP                                                 
         MVC   SPUP2YRR,OPT2YRR                                                 
         MVC   SPUPUDAY,OPTUPDT                                                 
         MVC   SPUPUTIM,OPTUPDT+1                                               
         MVC   SPUPTYPE(L'OPTUPGD),OPTUPGD                                      
         MVC   SPUPCLI,OPTCLI                                                   
         CLI   PROF1W+3,C'Y'                                                    
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOANGFR                                                
                                                                                
*&&DO                                                                           
         CLI   PROF1W+5,C'I'       'I' PRECISION REQUESTED?                     
*&&                                                                             
*****    CLI   PRECPROF,C'I'       'I' PRECISION REQUESTED?                     
*****    BNE   *+8                  YES, PASS                                   
** FORCE TO BE IMPRESSION BASED                                                 
         OI    SPUPOPTS,SPOPDMAI     THROUGH PRECISION FOR UPGRADES             
                                                                                
         CLI   PROF1W+7,C'Y'       normalize huts/puts requested                
         BNE   *+8                  YES, PASS                                   
         OI    SPUPOPTS,SPOPNORM     THROUGH FOR UPGRADES                       
                                                                                
         CLI   PROF1W+8,C'M'       FOR CSI/BBM WEEKLY                           
         BNE   SETOP2X              CALC MONTH                                  
         OI    SPUPOPT2,SPO2CMON                                                
         CLI   PROF1W+9,C'Y'                                                    
         BNE   SETOP2X                                                          
         OI    SPUPOPT2,SPO2UBBM                                                
SETOP2X  DS    0C                                                               
                                                                                
         L     RF,ASYSNTRY                                                      
         CLI   (SYSOVSYS-SYSTABD)(RF),8  IF REP SYSTEM,                         
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM          ALWAYS NORMALIZE HUT/PUT              
                                                                                
         CLI   OPTSPRT,0                                                        
         BE    DEMUPGD2                                                         
         OI    SPUPOPTS,SPOSPRTY                                                
         CLI   OPTSPRT,C'Y'                                                     
         BE    DEMUPGD2                                                         
         NI    SPUPOPTS,X'FF'-SPOSPRTY                                          
         OI    SPUPOPTS,SPOSPRTN                                                
         CLI   OPTSPRT,C'N'                                                     
         BE    DEMUPGD2                                                         
         DC    H'0'                                                             
DEMUPGD2 DS    0H                                                               
                                                                                
         CLC   0(L'MBKBOOK,R3),MBKBOOK                                          
         BNE   DMUPG4X                                                          
         MVC   SPUPFBK,OPTMBK                                                   
         MVC   SPUPFBKL,OPTMBK+L'SPUPFBK                                        
         MVC   SPUPBTYP,OPTMBKBT                                                
         MVI   SPUPFIL,C'T'                                                     
         MVC   SPUPTYPE(8),=XL8'0400006400000000'                               
DMUPG4X  EQU   *                                                                
                                                                                
*                                  CALL DEMUP TO CALCULATE DEMOS                
         GOTO1 VSPDEMUP,DMCB,SPDEMUPD,DEMODEMS,THISDEMS                         
         CLI   0(R1),0             TEST FOR ERRORS                              
         BNE   EXIT                                                             
*                                  SET POSTING KEY VALUES & PROG NAME           
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BZ    *+8                                                              
         MVI   RECTYPE,TDRRTDMV     INDICATE TYPE OF RECD TO POST               
         MVC   THISPROG,SPUPPRG                                                 
         MVI   THISDAYR,XFF        FORCE HIGHEST DAY # W/IN ROTATION            
         MVC   THISKDAY,THISDAY                                                 
         XI    THISKDAY,X'FF'      SORT TO DAY/TIME END                         
         MVI   THISSQH,X'FF'                                                    
         MVI   THISEQH,X'FF'                                                    
         BAS   RE,DEMPOST          GO AND POST DEMO VALUES                      
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
* INTERFACE TO SPDEMLK FOR SVI PROCESSING                                       
*                                                                               
DEMLOOK  NTR1                                                                   
         CLC   DBFIL,=C'PAV'       IF PAV FILE SELECTED,                        
         BE    DEMLK100             IT GETS PROCESSED DIFFERENTLY               
*                                                                               
         LA    R6,DBLOCK1          DEMLOOK BLOCK                                
         USING SPDEMLKD,R6         R6=A(DEMLOOK BLOCK)                          
         XC    SPDEMLK(SPDEMLKL),SPDEMLK                                        
         MVC   SPLKAREC,AIOAREA1                                                
         MVC   SPLKAFAC,AFAC                                                    
         LA    R0,DEMODEMS                                                      
         ST    R0,SPLKALST                                                      
         LA    R0,THISDEMS                                                      
         ST    R0,SPLKAVAL                                                      
*                                                                               
         CLI   DBMED,C'N'          NETWORK FILE?                                
         BNE   DEMLK5                                                           
         CLI   DBSRC,C'D'          NO TP DATA AVAIL FOR NAD                     
         BE    EXIT                  JUST EXIT W/NO DATA                        
*                                                                               
DEMLK5   DS    0H                                                               
*                                                                               
*&&DO                                                                           
         LA    R0,PROF1W                                                        
*&&                                                                             
         MVC   MYPROF1W,PROF1W         THIS IS TO INCORPORATE THE               
         MVC   MYPROF1W+5(1),PRECPROF   PRECISION PROFILE IF $DEM IS            
         LA    R0,MYPROF1W              USED VIA THE REP SYSTEM                 
         ST    R0,SPLKA1W                                                       
         MVC   MYPROF1W+8(1),CANMMBK    CAN MTR MKT POSTING PROFILE             
*&&DO                                                                           
* MAY26  99-GLEE - AS PER ZEN, THE CLIENTS DECIDED THEY DIDN'T WANT TO          
*                  USE THE CANADIAN METERED MARKET PROFILE.  HOWEVER,           
*                  SINCE SUPPRESSING IT CHANGES THE BEHAVIOR OF NSI             
*                  LOOK-UPS, THE PROFILE IS SUPPRESSED FOR BBM ONLY.            
         CLI   DBMED,C'C'              IF CANADIAN MEDIA,                       
         BNE   DEMLK05C                                                         
         CLI   DBSRC,C'A'                AND BBM SOURCE,                        
         BNE   DEMLK05C                                                         
         MVI   MYPROF1W+8,0             REMOVE CAN MTR MKT PROFILE              
*&&                                                                             
DEMLK05C EQU   *                                                                
*                                                                               
*                                                                               
         MVC   SPLKFIL,DBFIL                                                    
         MVC   SPLKMED,DBMED                                                    
         CLI   DBMED,C'N'          IF NETWORK                                   
         BNE   *+8                                                              
         MVI   SPLKBEST,C'L'       EXACT DAY MATCH LK UP/NOT BIT MASK           
         CLI   DBMED,C'R'          IF RADIO,                                    
         BNE   *+8                                                              
         MVI   SPLKBEST,C'P'        SUPPORT OVERNIGHT DAYPART POSTING           
         MVC   SPLKSRC,DBSRC                                                    
         MVC   SPLKTPTT,DBTPTTS                                                 
         MVC   SPLKAGY,AGYALPH                                                  
         MVC   SPLKDBK,0(R3)                                                    
         MVC   SPLKWKN,2(R3)       WEEK NUMBER                                  
         MVC   SPLKBTYP,3(R3)      BOOK TYPE                                    
*  IF BBM WEEKLY USE BOOKTYPE W FOR BOOKTYP TO GET LATEST WEEK                  
         L     RE,ADEMFIL                                                       
         CLC   =C'WTP',8(RE)                                                    
         BNE   DEMLK05CX                                                        
         OC    SPLKDBK,SPLKDBK                                                  
         BNZ   DEMLK05CX                                                        
         CLI   SPLKMED,C'C'                                                     
         BNE   *+8                                                              
         CLI   SPLKSRC,C'A'                                                     
         BNE   *+8                                                              
         MVI   SPLKBTYP,C'W'                                                    
DEMLK05CX DS   0C                                                               
*                                                                               
         CLI   DBMED,C'N'          FOR NETWK FORCE TO NUMERIC                   
         BNE   *+16                                                             
         CLI   3(R3),C'Z'                                                       
         BNH   *+8                                                              
         NI    SPLKBTYP,X'0F'      SET 'F0' - 'F9' TO NUMERIC 1-9               
         MVC   SPLKSTA,STASTA                                                   
         MVC   SPLKSPL,STASPILL                                                 
*                                                                               
* CANADIAN CODE TO SET MONTHLY FLAG                                             
*                                                                               
         CLI   DBMED,C'C'                                                       
         BNE   DEMLK05D                                                         
         L     RF,ADEMFIL                                                       
         CLC   =C'WTP',8(RF)                                                    
**       BE    DEMLK05D                                                         
         BNE   *+12                                                             
         MVI   SPLKBEST,C'W'                                                    
         B     DEMLK05D                                                         
**       MVI   DBBEST,C'M'                                                      
         MVI   SPLKBEST,C'M'                                                    
**       MVI   UBUSEBBM,C'Y'                                                    
DEMLK05D DS    0H                                                               
*                                                                               
         CLC   0(3,R2),=C'ROT'                                                  
         BE    *+20                                                             
         MVC   SPLKDAY,0(R2)                                                    
         MVC   SPLKTIM,1(R2)                                                    
         B     DEMLK05DT                                                        
                                                                                
         DS    0H                  BUILD DAY/TIME LINK                          
         XC    DUB,DUB                                                          
         LA    R0,MYDBXTRA                                                      
         ST    R0,DUB+4                                                         
         MVI   GOSUBN,SDBX#                                                     
         GOTO1 AGOSUB                                                           
                                                                                
         ICM   RF,15,FULL                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBXTLD,RF                                                        
         MVC   DBXTLID,=C'DYTM'                                                 
         ZICM  RE,3(R2),(3)                                                     
         LA    RE,LDTROTN(RE)                                                   
         ZIC   R1,0(RE)                                                         
         SH    R1,=H'2'                                                         
         EXMVC R1,DBXTLIST,1(RE)                                                
         LA    R1,DBXTLIST+1(R1)                                                
         MVI   0(R1),0                                                          
         DROP  RF                                                               
DEMLK05DT EQU  *                                                                
***  MOVE SYSCODE FOR THSI STATION INTO AN AREA FOR SPDEMLK EXTENSION           
         LA    R0,L'SYSCODES       ALPHA MKT INPUT (IN CASE THERE IS)           
         ZIC   R1,THISSTA                                                       
         BCTR  R1,0                                                             
         MR    R0,R0                                                            
         LA    R1,SYSCODES(R1)    R1 POINTS TO CURR SYSCODE FOR STATION         
         XC    SYSCEXT,SYSCEXT                                                  
         LA    RE,SYSCEXT                                                       
         USING SPLKXTD,RE                                                       
         XC    SPLKXTND,SPLKXTND   DEFAULT NO EXTENSION                         
         OC    0(L'SYSCODES,R1),0(R1)                                           
***      BZ    *+14                                                             
         BZ    *+10                                                             
         MVC   SPXTSYSC,0(R1)                                                   
         OI    SPXTFLAG,SPXTRAPP                                                
         ST    RE,SPLKXTND                                                      
*                                                                               
         CLI   OPTDEC,C'2'             IF OPTION OF MAINFRAME INVOKED *         
         BNE   *+8                                                    *         
         OI    SPXTFLG2,SPXTOP2I   2 DEC IMPRESSIONS                            
         DROP  RE                                                               
*                                                                               
         MVC   SPLKCLI,OPTCLI                                                   
         MVC   SPLKSVI,OPTSVIS                                                  
         CLI   SPLKSVI,0           TEST FOR SVI FACTOR                          
         BNE   *+8                 YES                                          
         MVI   SPLKSVI,X'FF'       NO-SUPPRESS SVI LOOKUP                       
         MVC   SPLKSPRT,OPTSPRT                                                 
         CLI   DBMED,C'C'                                                       
         BE    *+14                                                             
         OC    STASPILL,STASPILL   IF NO NUMERIC SPILL MARKET,                  
         BNZ   DEMLK10                                                          
         OC    ALPHAMKT,ALPHAMKT    AND THERE WAS ALPHA MKT INPUT,              
         BZ    DEMLK10                                                          
         MVC   SPLKALF,ALPHAMKT     USE IT                                      
         CLI   DBMED,C'C'                                                       
         BNE   *+10                                                             
         XC    SPLKSPL,SPLKSPL                                                  
                                                                                
DEMLK10  DS    0H                                                               
****                                                                            
         CLI   OPTDEC,C'0'                                                      
         BL    DEMLK11                                                          
         CLI   DBMED,C'W'           DISABLE WEEKLIES FOR NOW                    
         BE    DEMLK11                                                          
                                                                                
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
                                                                                
*   FOR NOW JUST FOR ZEN'S PRECISION SUPPORT DFUDGE THE DISPLACEMENT            
*   UNTIL DBEXTRAD DSECT IS LIVE                                                
**       LA    RE,DBXTRC2T                GIVE ZEN HIS DECMIAL                  
**       SHI   RE,1                        SUPPORT                              
**       MVC   0(1,RE),OPTDEC                                                   
         MVC   DBXTSCTL,OPTDEC                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
*&&DO                                                                           
         MVI   TMPWHOLE,WHOLERTG                                                
         OI    TMPWHOLE,WHOLESHR                                                
         OI    TMPWHOLE,WHOLEPUT                                                
         TM    TMPWHOLE,WHOLERTG           ROUND RATINGS TO WHOLE #S?           
         BZ    *+8                                                              
         MVI   DBXTRC2T,C'Y'                YEP                                 
         TM    TMPWHOLE,WHOLESHR           ROUND SHARES  TO WHOLE #S?           
         BZ    *+8                                                              
         MVI   DBXTSC2T,C'Y'                YEP                                 
         TM    TMPWHOLE,WHOLEPUT           ROUND PUTS    TO WHOLE #S?           
         BZ    *+8                                                              
         MVI   DBXTPC2T,C'Y'                YEP                                 
*&&                                                                             
         DROP  R1                                                               
* EOT  11/16/2000                                                               
*******                                                                         
DEMLK11  DS    0H                                                               
****                                                                            
*                                  CALL SPDEMLK TO GET DEMOS                    
         MVC   SPLKTIM,1(R2)                                                    
*                                                                               
         DS    0H                                                               
         CLI   THISRTYP,XFF        SPECIAL CALL TO GET REQ'S S/E QHs?           
         BNE   DMLK022X                                                         
         LA    R0,DEMLKHK                                                       
         ST    R0,SPLKHOOK          USE HOOK TO EXTRACT QHs                     
         ST    RD,SAVERD            REMEMBER  RD  FOR QUICK EXIT                
DMLK022X EQU   *                                                                
*                                                                               
         DS    0H                                                               
         MVC   SPLKUID,USERID                                                   
         GOTO1 VSPDEMLK,DMCB,(X'FF',SPDEMLKD)                                   
         CLI   0(R1),0             TEST FOR ERRORS                              
         BNE   EXIT                                                             
         CLI   THISRTYP,XFF        SPECIAL CALL TO GET REQ'S S/E QHs?           
         JE    EXIT                 YEP, EXIT NOW                               
                                                                                
*                                  SET POSTING VALUES FOR PERIOD TOTS           
         MVC   THISPROG,SPLKPRG                                                 
         TM    MYFLAG1,MYF1LNBK    IF PROCESSING LATEST N BOOKS,                
         BZ    *+10                                                             
         MVC   THISBOOK,0(R3)       GET ACTUAL BOOK VALUE                       
         LA    RF,DROPSVIS         REMOVE SVIS FROM THISDEMS                    
         CLI   OPTDTYP,C'S'        TEST TO DISPLAY SVIS                         
         BNE   *+8                                                              
         LA    RF,DROPDEMS         YES-REMOVE THE DEMOS                         
         BASR  RE,RF                                                            
                                                                                
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BZ    *+8                                                              
         MVI   RECTYPE,TDRRTDMV     INDICATE TYPE OF RECD TO POST               
         TM    MYFLAG1,MYF1CNT     IF PROCESSING CSI TP,                        
         BZ    *+10                                                             
         MVC   THISBOOK,THISSWKN    REMEMBER START/END WEEKS OF MONTH           
         TM    MYFLAG1,MYF1LNBK    IF PROCESSING LATEST N BOOKS,                
         BZ    *+14                                                             
         MVC   MYNLBK,THISLBK       REMEMBER # OF LATEST BOOKS                  
         MVI   THISLBK,X'9C'        FORCE HIGHEST (PACKED) LATEST BK #          
         MVI   THISDAYR,XFF        FORCE HIGHEST DAY # W/IN ROTATION            
         MVC   THISKDAY,THISDAY                                                 
         XI    THISKDAY,X'FF'                                                   
         LA    R1,2                                                             
         MVI   THISSQH,X'FF'                                                    
         MVI   THISEQH,X'FF'                                                    
         BAS   RE,DEMPOST          GO AND POST DEMO VALUES                      
*                                                                               
         DS    0H                  INDIVIDUAL SUMMARY LINES                     
         TM    MYFLAG1,MYF1LNBK     APPLICABLE FOR LATEST N BOOKS               
         BZ    DEMLK29                                                          
         CLI   ACTN,DEMOPER         NOT FOR ACTN=PERIOD                         
         BE    DEMLK29                                                          
         CLI   ACTN,DEMOAVL         NOT FOR ACTN=AVAIL                          
         BE    DEMLK29                                                          
         MVI   GOSUBN,GIS#                                                      
         GOTO1 AGOSUB                                                           
DEMLK29  EQU   *                                                                
                                                                                
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
                                                                                
*                                                                               
** PAV FILE **                                                                  
*                                                                               
DEMLK100 DS    0H                                                               
         LA    R6,DBLOCK1                                                       
         USING DBLOCKD,R6                                                       
*                                                                               
         DS    0H                 CLEAR DBLOCK                                  
         LR    R0,R6               R0-->DESTINATION                             
         LA    R1,DBLOCK1X-DBLOCK1 R1 = L(DESTINATION)                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         DS    0H                 BUILD DBLOCK                                  
         MVC   DBAREC,AIOAREA1     A(I/O AREA)                                  
         MVC   DBCOMFCS,AFAC       A(COMFACS)                                   
         MVC   DBFILE,DBFIL        FILE                                         
         MVI   DBFUNCT,DBGETDEM    DEMAND REQUEST FUNCTION                      
         MVC   DBSELSRC,DBSRC       SELECTED SOURCE                             
         MVC   DBSELBK,0(R3)           "     BOOK                               
         MVC   DBSELMED,DBMED          "     MEDIA                              
         MVC   DBSELSTA,STASTA         "     STATION                            
         MVC   DBSELMK,STASPILL        "     STATION                            
         MVC   DBSELCLI,OPTCLI         "     CLIENT                             
         MVC   DBSELDAY,0(R2)          "     DAY                                
         MVC   DBSELTIM(2),=H'600'     "     START TIME                         
         MVC   DBSELTIM+2(2),=H'545'   "     END TIME                           
         MVC   DBSELWKN,2(R3)          "     WEEK NUMBER                        
         MVC   DBSELSPO,OPTSPRT        "     SPORT OPTION                       
         MVC   DBSELAGY,AGYALPH     AGENCY                                      
         MVC   DBBTYPE,3(R3)        BOOK TYPE                                   
         MVI   DBBEST,C'A'          ASK FOR ALL RECORDS                         
         CLI   OPTBEST,C'B'                                                     
         BNE   *+8                                                              
         MVI   DBBEST,C'B'           UNLESS BEST REQUESTED                      
*                                                                               
         CLI   DBSELMED,C'N'       FOR NETW ALLOW NUMERIC BKTYP                 
         BNE   DEMLK120                                                         
         CLI   DBBTYPE,C'Z'                                                     
         BNH   *+8                                                              
         NI    DBBTYPE,X'0F'                                                    
         MVI   DBBEST,C'L'         EXACT MATCH LK UP (NOT BIT MATCH)            
*                                                                               
DEMLK120 CLI   TAPEOPT,C'Y'        IF IMP BASED CALCULATIONS REQUESTED,         
         BNE   *+8                                                              
         MVI   DBTAPEP,C'Y'         WE WANT TAPE PRECISION                      
         OC    STASPILL,STASPILL   IF NO NUMERIC SPILL MARKET,                  
         BNZ   *+20                                                             
         OC    ALPHAMKT,ALPHAMKT    AND THERE WAS ALPHA MKT INPUT,              
         BZ    *+10                                                             
         MVC   DBSELALF,ALPHAMKT    USE IT                                      
                                                                                
         DS    0H                  SET UP PRECISION IN DBLOCK EXTNSN            
         XC    MYDBXTRA,MYDBXTRA                                                
         LA    R1,MYDBXTRA                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
*                                                                               
**** FOR NOW JUST FOR Zen's                                                     
**** PRECISION SUPPORT FUDGE THE DISPLACEMENT                                   
**** UNTIL DBEXTRAD DSECT IS LIVE                                               
*                                                                               
**       LA    RE,DBXTRC2T                GIVE ZEN HIS DECMIAL                  
**       SHI   RE,1                        SUPPORT                              
**       MVC   0(1,RE),OPTDEC                                                   
         MVC   DBXTSCTL,OPTDEC                                                  
*                                                                               
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         DROP  R1                                                               
                                                                                
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCK,DMLKPHK,0                                    
         DROP  R6                                                               
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ROUTINE TO PROCESS VALUES RETURNED FROM SPDEMLK                               
** BE CAREFUL NOT TO CLOBBER DMCB, SINCE SPDEMLK'S CALLER DEPENDS ON IT         
                                                                                
DEMLKHK  NTR1                                                                   
         LA    R6,DBLOCK1                                                       
         USING SPDEMLKD,R6                                                      
                                                                                
*                                                                               
         DS    0H                                                               
         L     R5,SPLKDBLK                                                      
         USING DBLOCKD,R5                                                       
         ST    R5,ADBLOCK                                                       
         MVI   GOSUBN,GADQT#       GO AND GET ADBDQD                            
         GOTO1 AGOSUB                                                           
                                                                                
         MVC   SVSEQH,DBACTSQH     GET ACTUAL START/END QHs                     
*                                                                               
         DS    0H                  ADJUST QHs IF NECESSARY                      
         CLC   =AL2(DBDQUXTD),DBDQTAB+0 USING XTND FOR D/Q ENTRIES?             
         BNE   DMLKHK29                  NOPE, DON'T ADJUST QHs                 
         L     RF,ADBDQD                 YES, QH ADJ FCTR IS B4 D/QHs           
         SHI   RF,(DBDQXFXL-(DBDQADJF-DBDQXTND))  RF-->ADJ FACTOR               
         ZICM  R0,0(RF),(3)                       R0 = ADJ FACTOR,              
         BZ    DMLKHK29                                                         
         LCR   R0,R0                                                            
                                                                                
         DS    0H                   ADJUST START QH                             
         ZIC   R1,SVSEQHS                                                       
         AR    R1,R0                 ADD ADJUSTMENT FACTOR TO QH                
         BNM   *+8                    IF NEGATIVE,                              
         AHI   R1,96                   ADD 96 TO RESULT                         
         CHI   R1,96                  IF EQ TO OR GREATER END-OF-DAY,           
         BL    *+8                                                              
         SHI   R1,96                   SUBTRACT 96 FROM RESULT                  
         STC   R1,SVSEQHS                                                       
                                                                                
         DS    0H                   ADJUST END   QH                             
         ZIC   R1,SVSEQHE                                                       
         AR    R1,R0                 ADD ADJUSTMENT FACTOR TO QH                
         BNM   *+8                    IF NEGATIVE,                              
         AHI   R1,96                   ADD 96 TO RESULT                         
         CHI   R1,96                  IF EQ TO OR GREATER END-OF-DAY,           
         BL    *+8                                                              
         SHI   R1,96                   SUBTRACT 96 FROM RESULT                  
         STC   R1,SVSEQHE                                                       
DMLKHK29 EQU   *                                                                
         DROP  R5                                                               
                                                                                
         L     R5,SPLKDBLK                                                      
         USING DBLOCKD,R5                                                       
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'AFFL',SPLKDBLK,WORK                            
         MVC   THISAFFL,WORK                                                    
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'MAR',SPLKDBLK,WORK                             
         MVC   THISMNUM,WORK                                                    
         DROP  R5                                                               
                                                                                
         DROP  R6                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   THISRTYP,XFF        IF SPECIAL CALL,                             
         BNE   *+12                 EXIT BACK TO  DEMLOOK  ROUTINE              
         L     RD,SAVERD                                                        
         L     RD,8(RD)                                                         
*                                                                               
         DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
* At entry,                                                                     
*   R2-->requested day/time entry,                                              
*   R3-->requested book entry,                                                  
*   R4-->requested station entry.                                               
                                                                                
DMLKPHK  NTR1                                                                   
         LA    R6,DBLOCK1                                                       
         USING DBLOCKD,R6                                                       
*                                                                               
** TEST PROCESS RECORD OR NOT **                                                
         CLI   ACTN,DEMOPER        TEST PERIOD TOTALS ONLY                      
         BE    DMLKPHKX                                                         
         CLI   ACTN,DEMOAVL        TEST AVAILS                                  
         BE    DMLKPHKX                                                         
*                                                                               
         DS    0H                  TEST FOR CORRECT WEEK                        
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'WEEK',DBLOCK,WORK                              
         SR    R1,R1                                                            
         ICM   R1,1,2(R3)          R1 = WEEK REQUESTED                          
         BZ    *+22                 OK IF NONE REQUESTED                        
         LA    R1,WORK(R1)         R1-->ACTIVE WEEK                             
         NI    0(R1),X'0F'          (REMOVE ZONE)                               
         CLC   0(1,R1),2(R3)       MATCH WEEK TO CHECK FOR ACTIVE-NESS          
         BNE   DMLKPHKX             EXIT HOOK IF WEEK WASN'T ACTIVE             
         DS    0H                  WEEK IN RECORD IS CORRECT                    
*                                                                               
         DS    0H                  TEST FOR CORRECT TIMES                       
         MVC   TEMPMTIM,1(R2)       FIRST CONVERT REQUEST TIMES                 
         MVI   GOSUBN,MTQ#                                                      
         GOTO1 AGOSUB                                                           
         MVC   RQSTSQHR,TEMPQHR                                                 
         MVC   TEMPMTIM,3(R2)        TO QUARTER HOURS                           
         MVI   GOSUBN,S1M#                                                      
         GOTO1 (RF)                                                             
         MVI   GOSUBN,MTQ#                                                      
         GOTO1 (RF)                                                             
         MVC   RQSTEQHR,TEMPQHR                                                 
                                                                                
         CLC   DBACTEQC,RQSTSQHR    PROG'S END  VS.  REQUEST'S START            
         BNH   DMLKPHKX                                                         
         CLC   DBACTSQC,RQSTEQHR    PROG'S START  VS.  REQUEST'S END            
         BH    DMLKPHKX                                                         
         DS    0H                  WE WANT TIMES TO OVERLAP                     
*                                                                               
         DS    0H                  TEST FOR CORRECT PROGRAM TYPE                
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'PTYPE',DBLOCK,WORK                             
         CLI   OPTSPRT,C'Y'        IF USER WANT SPORTS                          
         BNE   *+12                                                             
         CLI   WORK,C'S'            BUT PROGTYPE ISN'T,                         
         BNE   DMLKPHKX             IT'S NO GOOD                                
         CLI   OPTSPRT,C'N'        IF USER DON'T WANT SPORTS                    
         BNE   *+12                                                             
         CLI   WORK,C'S'            BUT PROGTYPE IS,                            
         BE    DMLKPHKX             IT'S NO GOOD                                
         DS    0H                  RECORD CAN BE PROCESSED                      
*                                                                               
** EXTRACT DATA FOR PAV FILE **                                                 
*                                                                               
         MVI   THISDAYR,0          N/A FOR PAV                                  
                                                                                
*                                                                               
         CLI   DBSELMED,C'N'       LET IT GO THRU FOR NETWORK                   
         BE    *+8                                                              
         CLI   DBACTSRC,C'N'       TAPEOPT - NSI ONLY                           
         BE    *+12                                                             
         MVI   TAPEOPT,C'N'         OTHERWISE RESET                             
         MVI   DBTAPEP,0                                                        
*                                                                               
         MVI   THISFLG1,0          CLEAR FLAG BYTE                              
*                                                                               
         MVC   THISASQC,DBACTSQC   GET ACTUAL START                             
         MVC   THISAEQC,DBACTEQC    AND END QH'S                                
*                                                                               
         L     RF,DBAQUART                                                      
         MVI   THISNOR,0           ASSUME NOT NORMAL PROGRAM                    
         CLI   DBSELMED,C'N'       NETWORK?                                     
         BNE   DMLKPH03                                                         
         USING PHELEM,RF                                                        
         TM    PHDTYPE,X'01'       REG PROGRAM?                                 
         BNO   *+8                                                              
         MVI   THISNOR,1           SET TO NORMAL                                
         B     DMLKPH05            DEFAULT IS TO KEEP NORMAL FOR NETWK          
*                                                                               
         USING PHELEM,RF                                                        
DMLKPH03 CLI   PHDTYPE,1           0=NORMAL, 1=FULL CYCLE                       
         BE    *+8                                                              
         MVI   THISNOR,1            IT'S A NORMAL PROGRAM                       
         CLI   OPTNOR,C'N'         IF ASKED TO INCLUDE NORMAL PROGRAMS,         
         BE    *+12                 LET IT PASS                                 
         CLI   THISNOR,1           ELSE, SUPPRESS THEM BY DEFAULT               
         BE    DMLKPHKX                                                         
                                                                                
DMLKPH05 XC    THISPNUM,THISPNUM   ASSUME NO PROGRAM NUMBER                     
         CLI   PHREVID,X'FF'                                                    
         BNE   *+18                                                             
         CLI   PHREV,1                                                          
         BNE   *+10                                                             
         MVC   THISPNUM,PHPNUM3    PROGRAM NUMBER                               
         DROP  RF                                                               
*                                                                               
         DS    0H                  SET PROGRAM NAMES                            
         MVC   WORK,SPACES                                                      
         GOTO1 VDEFINE,MYDMCB,=C'PROG+',DBLOCK,WORK                             
         MVC   THISPROG,WORK        SET THIS PROGRAM'S NAME                     
                                                                                
         DS    0H                  GET PURE NUMBER                              
         XC    WORK,WORK                                                        
         GOTO1 (RF),MYDMCB,=C'PURE',DBLOCK,WORK                                 
         MVC   THISPURE,WORK        SET THIS PROGRAM'S PURE NUMBER              
                                                                                
         DS    0H                  GET TOTAL DURATION                           
         MVI   WORK,0                                                           
         GOTO1 (RF),MYDMCB,=C'TOTDUR',DBLOCK,WORK                               
         MVC   THISNQH,WORK         SET THIS PROGRAM'S DURATION                 
                                                                                
         DS    0H                  GET NUMBER OF WEEKS                          
         MVI   WORK,0                                                           
         GOTO1 (RF),MYDMCB,=C'WEEK',DBLOCK,WORK                                 
         ZICM  RE,WORK,(1)                                                      
         BNZ   DMLKPH07                                                         
         CLI   DBSELMED,C'N'       ZERO=5 WEEKS FOR NETWORK                     
         BE    *+6                                                              
         DC    H'0'                NON-NETWORK -> 0 WKS IS INVALID              
         LA    RE,X'10'            SET A 5TH WK BIT                             
*                                                                               
DMLKPH07 STC   RE,THISWKS                                                       
         LR    RF,RE                                                            
         A     RF,A1STWKTB                                                      
         MVC   THIS1WK,0(RF)        SET THIS PROGRAM'S 1ST WEEK                 
         A     RE,ANUMWKTB                                                      
         MVC   THISNWK,0(RE)        SET THIS PROGRAM'S # OF WEEKS               
*                                                                               
         DS    0H                  LOOK UP VUTS (TO DERIVE SHARES)              
         GOTO1 VDEMOUT,MYDMCB,(C'L',AVUTLIST),DBLOCK,QHVUTS                     
         OC    QHVUTS(4),QHVUTS    IF NO VUTS,                                  
         BNZ   DMLKPH10                                                         
         GOTO1 VDEMOUT,MYDMCB,(C'L',APUTLIST),DBLOCK,QHVUTS                     
*                                                                               
DMLKPH10 DS    0H                  CALL REGETIUN FOR DEMO VALUES                
         L     R4,AIUNWRK                                                       
         LA    R4,500(R4)                                                       
         USING IUNRECD,R4                                                       
                                                                                
         GOTO1 VRGETIUN,MYDMCB,(9,DBLOCK),(R4)                                  
                                                                                
         DS    0H                  GO PROCESS IUN                               
         LA    R0,QHVUTS                                                        
         ST    R0,AVUTS             SET A(VUTS TO USE)                          
         MVI   GOSUBN,PIUN#                                                     
         GOTO1 AGOSUB                                                           
*                                                                               
         DS    0H                  POST INTO SORT BUFFER                        
         TM    DEMFLAG1,DF1STERO   IF STEREO SESSION,                           
         BZ    *+8                                                              
         MVI   RECTYPE,TDRRTDMV     INDICATE TYPE OF RECD TO POST               
                                                                                
         ZIC   RF,DBDQPTR                                                       
*        CLI   DBSELMED,C'N'                                                    
*        BNE   *+8                                                              
*        LA    RF,X'01'                                                         
         LA    RE,DBDQLEN                                                       
         MR    RE,RE                                                            
         LA    RF,DBDQD-DBDQLEN(RF)                                             
         MVC   THISKDAY,DBDQKDAY-DBDQD(RF)                                      
                                                                                
         ZIC   R1,DBACTSQC                                                      
         CLM   R1,1,RQSTSQHR                                                    
         BNL   *+8                                                              
         IC    R1,RQSTSQHR         R1=START QHR TO PUT IN BINSRCH KEY           
                                                                                
         ZIC   R0,DBACTEQC                                                      
         CLM   R0,1,RQSTEQHR                                                    
         BNH   *+8                                                              
         IC    R0,RQSTEQHR         R0 = LAST QHR (END CONDITION)                
                                                                                
         OI    THISFLG1,BF11HHR    1ST ENTRY IS 1ST HALF HOUR DSPLYED           
DMLKPH22 DS    0H                  POST ON HALF-HOUR SEGMENTS                   
         LA    RF,1(R1)            RF=POTENTIAL END QH OF ENTRY                 
         CR    RF,R0               IF IT SURPASSES END,                         
         BNH   *+6                                                              
         BCTR  RF,0                 BACK UP ONE QH                              
         LA    RF,1(RF)            ADD 1 TO SHOW CORRECT END TIME               
         STC   R1,THISSQH                                                       
         STC   RF,THISEQH                                                       
         CR    RF,R0               IF LAST ENTRY FOR PROGRAM,                   
         BNH   *+8                                                              
         OI    THISFLG1,BF1XHHR     FLAG IT'S LAST 1/2-HOUR                     
         BAS   RE,DEMPOST                                                       
         CR    RF,R0               KEEP POSTING UNTIL END PASSED                
         BH    DMLKPH30                                                         
         LA    R1,2(R1)                                                         
         NI    THISFLG1,X'FF'-BF11HHR                                           
         B     DMLKPH22                                                         
                                                                                
*                                                                               
DMLKPH26 DS    0H                  POST ENTIRE PROGRAM                          
         MVC   THISSQH,DBACTSQC                                                 
         MVC   THISEQH,DBACTEQC                                                 
         BAS   RE,DEMPOST                                                       
         B     DMLKPH30                                                         
*                                                                               
DMLKPH30 DS    0H                                                               
*                                                                               
DMLKPHKX DS    0H                                                               
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* REMOVE SVI FACTORS FROM OUTPUT VALUES RETURNED FROM SPDEMLK                   
*                                                                               
DROPSVIS LA    R1,THISDEMS         R1=A(RETURNED DEMO VALUES)                   
         LA    RF,8(R1)            RF=A(2ND DEMO VALUE ENTRY)                   
         ZIC   R0,DEMONDEM         R0=N'OUTPUT ENTRIES                          
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
         ZIC   R0,DEMONDEM         R0=N'OUTPUT VALUES                           
         MVC   0(4,R1),0(RF)       SHIFT SVI TO DEMO POSITION                   
         LA    R1,4(R1)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,*-14                                                          
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* BUILD BINSRCH RECORD & CALL ROOT POST ROUTINE                                 
*                                                                               
DEMPOST  NTR1                                                                   
         MVI   GOSUBN,DPST#        CALL ADDRESSABLE ROUTINE                     
         GOTO1 AGOSUB                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* FORMAT PRINT LINES                                                            
*                                                                               
         USING STABKD,R4                                                        
DEMLINE  L     R5,BINAREC                                                       
         USING BINRECD,R5          R5=A(RECORD)                                 
                                                                                
         TM    DEMFLAG1,DF1STERO   IS THIS A STEREO SESSION?                    
         BO    DEMLIN20             YES, DO STEREO LOGIC                        
         DC    H'0'                ONLY DEM32 SUPPORTED                         
                                                                                
*                                                                               
         DS    0H                  DISPLAY DISCLAIMER IF EXPLODING WKS          
DEMLIN20 DS    0H                  STEREO SESSION - USE SPECIAL FORMAT          
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         SR    R5,R5                                                            
         ICM   R5,7,TSAREC+1                                                    
         DROP  R1                                                               
                                                                                
         LA    R5,2(R5)            R5-->TSAR DEMO RECORD                        
         USING TSDEMRCD,R5                                                      
         L     R2,ASTIOBUF         R2-->STEREO'S I/O BUFFER                     
         L     RE,ASTDMEXP                                                      
         MVC   STNDEMS,0(RE)                                                    
*                                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    DMLN200                                                          
*                                                                               
         DC    H'0'                                                             
         EJECT                                                                  
DMLN200  DS    0H                  STEREO DEM32 SESSION                         
         MVI   GOSUBN,D32DL#                                                    
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
DEMLINX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
                                                                                
                                                                                
* Little routine to insert field separator into output buffer.                  
*  R2-->next output location in the output buffer.                              
                                                                                
INSSEP   DS    0H                                                               
         MVI   0(R2),STSPIFLD                                                   
         LA    R2,1(R2)                                                         
         BR    RE                                                               
         EJECT                                                                  
* MISCELLANEOUS TASKS FOR STEREO SESSION                                        
*                                                                               
DEMSTTSK DS    0H                                                               
         CLI   STMODE,STMMIDQ                                                   
         BE    DMSTSK20                                                         
         CLI   STMODE,STMOUTQ                                                   
         BE    DMSTSK50                                                         
         B     DMSTSKX                                                          
*                                                                               
DMSTSK20 DS    0H                                                               
         LH    R2,DSPRCTDT         WANT DAY/TIME ENTRY                          
         LA    R2,REQCTLTB(R2)                                                  
         USING RCTDSECT,R2                                                      
*        CLI   RCTFROM,1           IF WE ARE GOING TO FORMAT THE FIRST          
*        BNE   DMSTSK25X                                                        
*        CLI   STBKDATA,C'Y'        DAY/TIME AGAIN, AND NO DATA FOR             
*        BE    DMSTSK25X                                                        
*        MVI   GOSUBN,PDDR#         STN/BK COMBO, POST DUMMY DATA RECD          
*        GOTO1 AGOSUB                                                           
DMSTSK25X EQU  *                                                                
         DROP  R2                                                               
                                                                                
*                                                                               
         LH    R2,DSPRCTDM         WANT DEMOS ENTRY                             
         LA    R2,REQCTLTB(R2)                                                  
         USING RCTDSECT,R2                                                      
                                                                                
         CLI   RCTFROM,1           IF WE ARE GOING TO FORMAT THE FIRST          
         BNE   DMSTSK30             DEMO AGAIN, RELEASE TSAR DEMO RECDS         
         OI    OUPTFLG1,OF1RELSE+OF1ITSR     AND RE-INIT TSAR                   
*                                                                               
DMSTSK30 DS    0H                                                               
         TM    OUPTFLG1,OF1RELSE+OF1RCTX                                        
         BNO   DMSTSK35                                                         
         CLC   DBFIL,=C'PAV'                                                    
         BNE   DMSTSK32                                                         
         MVI   GOSUBN,PDT#         POST DAY/TIME LIST                           
         GOTO1 AGOSUB                                                           
                                                                                
DMSTSK32 DS    0H                                                               
*                                                                               
DMSTSK35 DS    0H                                                               
         B     DMSTSKX                                                          
         DROP  R2                                                               
                                                                                
                                                                                
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
         LH    RF,=Y(STDMEXPX-STDEMEXP)  RF = L(DESTINATION)                    
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                     RE-->DESTINATION                       
                                                                                
         B     DMSTSKX                                                          
                                                                                
                                                                                
DMSTSKX  DS    0H                                                               
         B     EXIT                                                             
         TITLE 'DEDEM0C - $DEM LILO REPORT (APMODE=APMBREAK)'                   
***********************************************************************         
*============ TASKS TO DO FOR MAINFRAME TRANSACTION BREAK ============*         
                                                                                
DEMBREAK DS    0H                                                               
*^^GYL 12/17/98 - This code is put here because the PC FALINK can not           
*^^                handle breaks yet                                            
*^^TEMP                                                                         
         MVI   FERN,120            ENQUIRY TOO LARGE TO PROCESS                 
         LA    RE,DMBRKF19                                                      
         NTR1                                                                   
         L     RB,ABASE                                                         
         L     RA,BBASE                                                         
         L     R7,CBASE                                                         
         L     R6,DBASE                                                         
         L     RF,AERROR0                                                       
         BR    RF                                                               
DMBRKF19 EQU   *                                                                
         OI    MISCFLG1,MF1ERROR                                                
         B     DMBRKXL                                                          
*^^EOTEMP                                                                       
         DS    0H                  SEE WHO CALLED FOR A BREAK                   
         TM    BRKFLAG1,BF1FALNK    FALINK?                                     
         BO    DMBRKF                YEP                                        
         TM    BRKFLAG1,BF1APPLC    APPLICATION?                                
         BO    DMBRKA                YEP                                        
         B     DMBRKX                                                           
                                                                                
                                                                                
*                                                                               
** BREAK CALLED BY FALINK **                                                    
*                                                                               
DMBRKF   DS    0H                                                               
         L     RF,AAPSAV2                                                       
         USING APSAV2D,RF                                                       
         MVC   AS2EMPC,FALEMPC                                                  
         MVC   AS2DMPC,FALDMPC                                                  
         MVC   AS2STMOD,STMODE                                                  
         MVI   AS2DEMON,0          THIS WILL GET SET LATER, IF NEEDED           
         DROP  RF                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         BE    DMBRKXE                                                          
                                                                                
                                                                                
*                                                                               
** BREAK CALLED BY APPLICATION **                                               
*                                                                               
DMBRKA   DS    0H                                                               
         L     RF,AAPSAV2                                                       
         USING APSAV2D,RF                                                       
         MVC   AS2THSST,THISSTA                                                 
         MVC   AS2THSBK,THISBK                                                  
         MVC   AS2THSDY,THISDAY                                                 
         DROP  RF                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         B     DMBRKXE                                                          
                                                                                
*                                                                               
DMBRKX   DS    0H                                                               
         J     EXIT                                                             
*                                                                               
DMBRKXL  DS    0H                                                               
         J     EXITL                                                            
*                                                                               
DMBRKXE  DS    0H                                                               
         J     EXITE                                                            
***********************************************************************         
         TITLE 'DEDEM0C - $DEM LILO REPORT (APMODE=APMRESUM)'                   
***********************************************************************         
*============ TASKS TO DO FOR MAINFRAME TRANSACTION RESUME ===========*         
                                                                                
DEMRESUM DS    0H                                                               
         TM    RSMFLAG1,RF1APPLC   RESUMING FOR APPLIC BREAK?                   
         BO    DMRSMA               YEP                                         
         TM    RSMFLAG1,RF1FALNK   RESUMING FOR FALINK BREAK?                   
         BO    DMRSMF               YEP                                         
         B     DMRSMX                                                           
                                                                                
                                                                                
*                                                                               
** RESUME NEEDED FOR APPLICATION BREAK **                                       
*                                                                               
DMRSMA   DS    0H                                                               
         L     R6,AAPSAV2                                                       
         USING APSAV2D,R6                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         OI    AS2RSMF,AS2RFAPP                                                 
*                                                                               
         DS    0H                                                               
         CLI   AS2STMOD,STMOUTQ                                                 
         BE    DMRSMAO                                                          
         B     DMRSMAX                                                          
                                                                                
*                                                                               
*** STMODE=STMOUTQ WHEN BREAK CALLED BY APPLIC ***                              
*                                                                               
DMRSMAO  DS    0H                                                               
         MVI   RESUMAPP,C'Y'                                                    
         B     DMRSMAX                                                          
                                                                                
         DROP  R6                                                               
                                                                                
*                                                                               
DMRSMAX  DS    0H                                                               
         B     DMRSMX                                                           
                                                                                
                                                                                
*                                                                               
** RESUME CALLED BY FALINK **                                                   
*                                                                               
DMRSMF   DS    0H                                                               
         L     R6,AAPSAV2                                                       
         USING APSAV2D,R6                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   AS2STMOD,STMOUTQ                                                 
         BE    DMRSMFO                                                          
         B     DMRSMFX                                                          
                                                                                
*                                                                               
*** STMODE=STMOUTQ WHEN BREAK CALLED BY FALINK ***                              
*                                                                               
DMRSMFO  DS    0H                                                               
         OI    AS2RSMF,AS2RFFLK                                                 
         DROP  R6                                                               
                                                                                
*                                                                               
DMRSMFX  DS    0H                                                               
         B     DMRSMX                                                           
                                                                                
                                                                                
*                                                                               
** EXIT RESUME TASKS **                                                         
*                                                                               
DMRSMX   DS    0H                                                               
         J     EXIT                                                             
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
         BNH   GOSUBGO                                                          
         MVC   ASUBRTN,ASUBR03                                                  
         CLI   GOSUBN,R03#                                                      
         BNH   GOSUBGO                                                          
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
*============== EXTERNAL ROUTINES CALLED WITHIN  DEMTIME =============*         
                                                                                
TIMVAL   NTR1                                                                   
         GOTO1 VTIMVAL,(R1)                                                     
         J     EXIT                                                             
                                                                                
                                                                                
UNTIME   NTR1                                                                   
         GOTO1 VUNTIME,(R1)                                                     
         J     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
HEADDAY1 DC    C'DAY/TIME'                                                      
HEADDAY2 DC    C'--------'                                                      
         SPACE 1                                                                
PROG     DC    C'PROGRAM'                                                       
                                                                                
WKDSCLMR DC    C'If pgm runs 2+ weeks, multi-week avg is repeated in ac+        
               tive weeks'                                                      
WKDSCLML EQU   *-WKDSCLMR                                                       
WKDSCLMD EQU   (L'DEMLN1-WKDSCLML)/2                                            
         DS    0CL(L'DEMLN1-WKDSCLML+1)                                         
                                                                                
IUNWRKL  EQU   IUNRECL+500                                                      
         DROP  R7,R8,R9,RA,RB                                                   
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR01)'            
***********************************************************************         
*======================== SUBROUTINE POOL ONE ========================*         
                                                                                
* At entry,                                                                     
*   R9-->DEMWRKD,                                                               
*   R8-->TWA,                                                                   
*   R7-->DEMTMPD,                                                               
*   R1 = equated sub-routine number.                                            
                                                                                
SUBR01Q  EQU   (((*-DEM0C+4095)/4096)*4096)                                     
                                                                                
         ORG   DEM0C+SUBR01Q                                                    
SUBR01   NMOD1 0,**0C01**                                                       
         USING DEMWRKD,R9          R9=A(GLOBAL WORK AREA)                       
         USING DEMTWAD,R8          R8=A(TWA)                                    
         USING DEMTMPD,R7          R7=A(TEMP W/S)                               
                                                                                
         L     RC,ALOCWRK                                                       
         USING DM0CWRKD,RC                                                      
*                                                                               
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R01_00(R1)                                                       
                                                                                
DTL#     EQU   (R01_01-*)/4+1      BUILD  DAY/TIME LIST                         
PDT#     EQU   (R01_02-*)/4+1      POST   DAY/TIME LIST                         
ODT#     EQU   (R01_03-*)/4+1      OUTPUT DAY/TIME LIST TO STEREO               
GDY#     EQU   (R01_04-*)/4+1      TRANSLATE DAY & TIME                         
CMD#     EQU   (R01_05-*)/4+1      CALC MAX # DTNTRYs                           
DDT#     EQU   (R01_06-*)/4+1      DO DUMMY TSAR DEMO RECDS                     
GWK#     EQU   (R01_07-*)/4+1      GET WEEKS                                    
MTQ#     EQU   (R01_08-*)/4+1      MILITARY TO QUARTER HOUR                     
S1M#     EQU   (R01_09-*)/4+1      SUBTRACT ONE MINUTE                          
GIS#     EQU   (R01_10-*)/4+1      GET INDIVIDUAL (BOOK) SUMMARY LINE           
*DM#     EQU   (R01_11-*)/4+1      GET DEMOS                                    
GPRF#    EQU   (R01_12-*)/4+1      GET PROFILE                                  
PIUN#    EQU   (R01_13-*)/4+1      PROCESS IUN                                  
                                                                                
R01_00   DS    0H                                                               
R01_01   B     DAYTMLST            BUILD  DAY/TIME LIST                         
R01_02   B     POSTDT              POST   DAY/TIME LIST                         
R01_03   B     OUTDT               OUTPUT DAY/TIME LIST TO STEREO               
R01_04   B     GET_DAY             TRANSLATE DAY & TIME                         
R01_05   B     CMXNDT              CALC MAX # DTNTRYs                           
R01_06   B     DODUMTDR            DO DUMMY TSAR DEMO RECDS                     
R01_07   B     GETWKS              GET WEEKS                                    
R01_08   B     MILTOQHR            MILITARY TO QUARTER HOUR                     
R01_09   B     SUB1MIN             SUBTRACT ONE MINUTE                          
R01_10   B     GTINDSUM            GET INDIVIDUAL (BOOK) SUMMARY LINE           
*01_11   B     GETDEM              GET DEMOS                                    
R01_12   B     GETPROFL            GET PROFILE                                  
R01_13   B     PROCIUN             PROCESS IUN                                  
R01#     EQU   (*-R01_00)/4                                                     
         DC    H'0'                                                             
                                                                                
YES_01   SR    R9,R9                                                            
NO_01    LTR   R9,R9                                                            
XIT_01   XIT1                                                                   
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR01--DTL+        
               #)'                                                              
*------------------------ BUILD DAY/TIME LIST ------------------------*         
                                                                                
* Routine to add day/time combo to list for PAV requests.                       
                                                                                
DAYTMLST DS    0H                                                               
         LA    RF,TSARBLCK                                                      
         USING TSARD,RF                                                         
         ZICM  R2,TSAREC+1,(7)                                                  
         DROP  RF                                                               
         LA    R2,2(R2)                                                         
         USING TSDEMRCD,R2                                                      
                                                                                
         LA    R5,WORK             USE WORK TO BUILD DAY/TIME COMBO             
         USING DTNTRYD,R5                                                       
         XC    DTNTRYD(DTNTRYQ),DTNTRYD                                         
                                                                                
         DS    0H                  CALC NTH DAY/TIME INPUT FROM STEREO          
         LH    RF,DSPRCTDT                                                      
         LA    RF,REQCTLTB(RF)                                                  
         USING RCTDSECT,RF                                                      
         ZIC   R1,RCTFROM                                                       
         DROP  RF                                                               
         ZIC   R0,TDRDAY                                                        
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
                                                                                
         DS    0H                  BUILD DAY/TIME ENTRY                         
         STC   R1,DTNTHINP          nth INPUT FROM STEREO                       
         MVC   DTKDAY,TDRKDAY       KEY DAY                                     
         MVC   DTSQH,TDRSQH         START QUARTER-HOUR                          
         MVC   DTEQH,TDREQH         END   QUARTER HOUR                          
         DROP  R2                                                               
*                                                                               
         DS    0H                  PUT ENTRY INTO LIST VIA BINSRCH              
         L     R6,ASTBUFFR                                                      
         MVI   GOSUBN,CMD#         CALC MAX # OF DTNTRYs INTO MAXNDT            
         GOTO1 AGOSUB                                                           
         XC    DMCB(6*4),DMCB                                                   
         ST    R5,DMCB             P1      = ADDR OF RECORD                     
         MVI   DMCB,X'01'          P1+0(1) = INSRT RECD IF NOT FOUND            
         LA    RE,4(R6)                                                         
         ST    RE,DMCB+4           P2      = ADDR OF TABLE (LIST)               
         LH    RE,0(R6)                                                         
         ST    RE,DMCB+8           P3      = # OF RECDS SO FAR                  
         MVI   DMCB+15,DTNTRYQ     P4      = L(RECORD)                          
         MVI   DMCB+16,0           P5+0(1) = DISPL OF KEY INTO RECD             
         MVI   DMCB+19,DTNTRYQ     P5+1(3) = L(KEY)                             
         LH    RE,MAXNDT                                                        
         ST    RE,DMCB+20          P6      = MAX # OF RECDS                     
         DROP  R5                                                               
                                                                                
         GOTO1 VBINSRCH,DMCB                                                    
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,DMCB+8                                                        
         STH   RE,0(R6)            UPDATE THE # OF RECDS SO FAR                 
*                                                                               
         B     XIT_01                                                           
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR01--PDT+        
               #)'                                                              
*------------------------- POST DAY/TIME LIST ------------------------*         
                                                                                
* Routine to post day/time list to TSAR buffer for PAV requests.                
                                                                                
POSTDT   DS    0H                                                               
         LA    RF,TSARBLCK                                                      
         USING TSARD,RF                                                         
         ZICM  R2,TSAREC+1,(7)                                                  
         DROP  RF                                                               
*                                                                               
         DS    0H                  BUILD TSAR DEMO RECORD                       
         LA    R3,2(R2)                                                         
         USING TSDEMRCD,R3                                                      
         XC    TDRKEY(TDRKEYL),TDRKEY                                           
         MVI   TDRRCTYP,TDRRTDTL                                                
         XC    TDREBCDT,TDREBCDT                                                
                                                                                
         L     RE,ASTBUFFR         RE = A(SOURCE)--DAY/TIME LIST                
         LH    RF,0(RE)            RF = TOTAL # OF DAY/TIME COMBOS              
         STCM  RF,3,TDRNUMDT                                                    
         MH    RF,=Y(DTNTRYQ)         * DTNTRYQ                                 
         LA    RF,4(RF)               + 4 = TOTAL LENGTH OF SOURCE              
         LH    R1,LTSIOREC                                                      
         SH    R1,=Y(TDRFIXL2+2)                                                
         CR    RF,R1               R1=MAX AMT WE HAVE TO STORE LIST             
         BNH   *+6                                                              
         DC    H'0'                DESTINATION CAN'T FIT ENTIRE LIST            
         LA    R0,TDRDYTMS         R0 = A(DESTINATION)                          
         LR    R1,RF               R1 = L(DESTINATION)                          
         MVCL  R0,RE               MOVE LIST INTO TSAR DEMO RECORD              
         DROP  R3                                                               
                                                                                
         DS    0H                  CALC L(TSAR DEMO RECORD)                     
         SR    R0,R2               R0-->END, R2-->START                         
         STH   R0,0(R2)                                                         
                                                                                
         GOTO1 APOST                                                            
*                                                                               
         B     XIT_01                                                           
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR01--ODT+        
               #)'                                                              
*------------------------ OUTPUT DAY/TIME LIST -----------------------*         
                                                                                
* Routine to translate & pass day/time list to STEREO for PAV requests.         
                                                                                
OUTDT    DS    0H                                                               
         L     R2,ASTIOBUF         R2-->OUTPUT BUFFER                           
                                                                                
         LA    RF,TSARBLCK                                                      
         USING TSARD,RF                                                         
         ZICM  R3,TSAREC+1,(7)     R3-->TSAR I/O AREA                           
         DROP  RF                                                               
         LA    R4,2(R3)            R4-->TSAR DEMO RECORD                        
         USING TSDEMRCD,R4                                                      
                                                                                
         CLI   TDREBCDT,0          SEE IF ANY UNFINISHED BUSINESS               
         BE    ODT30                NOPE                                        
                                                                                
         DS    0H                   YES, COULDN'T FIT ALL IN PREVIOUSLY         
         ZIC   R1,TDREBCDT                                                      
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),TDREBCDT+1   MOVE THEM INTO OUTPUT BUFFER NOW           
         XC    TDREBCDT,TDREBCDT                                                
         LA    R2,1(R2,R1)                                                      
*                                                                               
ODT30    DS    0H                  OUTPUT NEXT DAY/TIME COMBO                   
         LA    R5,TDRDYTMS+4       R5-->LIST OF DAY/TIME COMBOS                 
         USING DTNTRYD,R5                                                       
                                                                                
         SR    R1,R1                                                            
         ICM   R1,3,TDRNUMDT       NEED TO PASS COUNT TO STEREO?                
         BZ    ODT40                NOPE, NOT ANY MORE                          
         MVI   0(R2),ORTDYTIM       YES, INDICATE OUTPUT (RECORD) TYPE          
         LA    R2,1(R2)                                                         
         EDIT  (R1),(3,(R2)),ALIGN=LEFT                                         
         AR    R2,R0                R2-->NEXT OUTPUT LOCATION                   
         XC    TDRNUMDT,TDRNUMDT   DON'T PASS COUNT NEXT TIME AROUND            
                                                                                
*                                                                               
ODT40    DS    0H                                                               
         OC    TDRDYTMS(2),TDRDYTMS  ANY MORE DAY/TIME COMBOS?                  
         BNZ   ODT50                  YES, KEEP PROCESSING                      
                                                                                
         DS    0H                     NO, NEED TO INSERT "END OF KEY"           
         LH    R1,LSTIOBUF                                                      
         A     R1,ASTIOBUF                                                      
         SR    R1,R2               R1 = AMT LEFT IN I/O BUFFER                  
         BP    ODT42                                                            
         BZ    ODT44                                                            
         DC    H'0'                                                             
                                                                                
ODT42    DS    0H                  ROOM LEFT IN I/O BUFFER                      
         MVI   0(R2),STSPIKEY      MOVE IN "END OF KEY" SEPARATOR               
         LA    R2,1(R2)                                                         
         B     ODT100               AND WE'RE DONE!                             
                                                                                
ODT44    DS    0H                  NO ROOM LEFT IN I/O BUFFER                   
         GOTO1 APOST                WRITE RECORD BACK TO TSAR                   
         MVI   APMODE,SAMEREC       WANT THIS RECORD NEXT TIME AROUND           
         B     ODT100               GO RELEASE STUFF IN I/O BUFFER              
                                                                                
*                                                                               
ODT50    DS    0H                  INSERT SEPARATOR BEFORE NEXT ENTRY           
         LH    R1,LSTIOBUF                                                      
         A     R1,ASTIOBUF                                                      
         SR    R1,R2               ANY ROOM LEFT TO INSERT SEPARATOR?           
         BP    ODT52                YEP                                         
         BZ    ODT54                NOPE                                        
         DC    H'0'                                                             
                                                                                
ODT52    DS    0H                  INSERT SEPARATOR                             
         MVI   0(R2),STSPIFLD                                                   
         LA    R2,1(R2)                                                         
         B     ODT60                AND MOVE ON                                 
                                                                                
ODT54    DS    0H                  NO ROOM LEFT FOR SEPARATOR                   
         B     ODT80                GO SHIFT REMNING DAY/TIMES OVER             
                                                                                
*                                                                               
ODT60    DS    0H                                                               
         MVC   TMPKSE,DTKDAY       GET KEY DAY, START & END QHRS                
         MVI   GOSUBN,GDY#          AND TRANSLATE THEM TO DAY & TIME            
         GOTO1 AGOSUB                                                           
         ZICM  R1,TDRDYTMS,(3)                                                  
         BCTR  R1,0                                                             
         STCM  R1,3,TDRDYTMS       ONE LESS DAY/TIME TO PROCESS                 
         LA    R5,DTNTRYQ(R5)      BUMP TO NEXT DAY/TIME COMBO                  
                                                                                
         DS    0H                  PUT EBCDIC DAY/TIME IN OUTPUT BUFFER         
         LH    R1,LSTIOBUF                                                      
         A     R1,ASTIOBUF                                                      
         SR    R1,R2               R1 = AMT LEFT IN I/O BUFFER                  
         LA    RE,DAYTIME                                                       
         LA    RF,L'DAYTIME-1(RE)                                               
         CR    RE,RF                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-16                                                          
         SR    RF,RE                                                            
         LA    RF,1(RF)            RF = L(EBCDIC DAY/TIME OUTPUT)               
                                                                                
         CR    RF,R1               CAN DAY/TIME OUTPUT FIT IN I/O BUFF?         
         BH    *+6                  NO, FIT AS MUCH AS POSSIBLE                 
         LR    R1,RF                YES, MOVE ENTIRE OUTPUT TO I/O BUFF         
                                                                                
         LR    R0,R2               SET DESTINATION                              
         MVCL  R0,RE                AND MOVE OUTPUT TO I/O BUFFER               
         LR    R2,R0               SET NEXT OUTPUT LOCATION                     
         LTR   RF,RF               MOVED ENTIRE DAY/TIME OUTPUT?                
         BZ    ODT40                YES, GO BACK & PROCESS NXT DAY/TIME         
                                                                                
         DS    0H                   NO, CAN'T FIT THE DAMN THING                
         STC   RF,TDREBCDT         REMEMBER HOW MUCH WAS LEFT OVER              
         BCTR  RF,0                                                             
         EXMVC RF,TDREBCDT+1,0(RE)  AND REMEMBER WHAT WAS LEFT OVER             
                                                                                
ODT80    DS    0H                  SHFT REMNING PORTION OF DT LIST OVER         
         LA    R0,TDRDYTMS+4        A(DESTINATION)                              
         ZICM  R1,TDRDYTMS,(3)                                                  
         MH    R1,=Y(DTNTRYQ)       L(DESTINATION)                              
         LA    RE,DTNTRYD           A(SOURCE)                                   
         LR    RF,R1                L(SOURCE)                                   
         MVCL  R0,RE                                                            
         GOTO1 APOST               WRITE RECORD BACK TO BUFFER                  
         MVI   APMODE,SAMEREC      WE WANT SAME RECD BACK                       
         B     ODT100                                                           
         DROP  R4,R5                                                            
*                                                                               
ODT100   DS    0H                  CALCULATE L(OUTPUT RECORD)                   
         S     R2,ASTIOBUF                                                      
         STH   R2,IODATALN                                                      
         B     ODTX                 AND EXIT                                    
*                                                                               
ODTX     DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR01--GDY+        
               #)'                                                              
*--------------------------- GET DAY & TIME --------------------------*         
                                                                                
         PRINT OFF                                                              
* Links to the GETDAY routine in the main NMOD to translate the day             
*  and time in TMPKSE.                                                          
         PRINT ON                                                               
* At entry,                                                                     
*  TMPKSE = day, start and end quarter hours.                                   
* At exit,                                                                      
*  DAYTIME = EBCDIC day and times.                                              
*  WORK2   = secondary output area.                                             
*  TEMPIDAY = internal day                                                      
*  TEMPSTIM = start time in military                                            
*  TEMPETIM = end   time in military                                            
                                                                                
GET_DAY  DS    0H                                                               
                                                                                
         DS    0H                                                               
         L     R1,AKDAYTAB                                                      
         MVC   DAYTIME,SPACES                                                   
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(L'WORK2-1),WORK2                                         
         MVI   TEMPIDAY,0                                                       
         XC    TEMPSTIM,TEMPSTIM                                                
         XC    TEMPETIM,TEMPETIM                                                
                                                                                
         LA    R2,WORK2                                                         
         NI    MYFLAG1,XFF-MYF1DTRO  ASSUME NOT A ROTATION                      
         MVI   FNDX,1                USE  FNDX  AS A COUNTER                    
         CLI   TMPSQH,X'FF'        TEST FOR COMPOSITE DAY/TIME                  
         BE    GETDAY8                                                          
*                                  CONVERT KEY DAY VALUES                       
GETDAY2  CLI   0(R1),X'FF'         TEST E-O-T                                   
         BE    GETDAY8                                                          
         CLI   0(R1),0             TEST IF AN ALL FILE VALUE                    
         BE    *+14                                                             
         CLC   0(1,R1),DBFIL       NO - MATCH ON FILE NAME                      
         BNE   GETDAY4                                                          
         CLI   1(R1),X'FF'         TEST FOR FUNNIES                             
         BE    GETDAY6                                                          
         CLC   1(1,R1),TMPKDAY     MATCH ON KEY DAY VALUE                       
         BE    GETDAY6                                                          
GETDAY4  LA    R1,L'KDAYTAB(R1)                                                 
         B     GETDAY2                                                          
GETDAY6  MVC   0(3,R2),2(R1)       SET OUTPUT VALUE FROM TABLE                  
         MVC   TEMPIDAY,5(R1)                                                   
*                                  CONVERT QHRS TO MILITARY TIME                
         CLI   DBMED,C'N'          NETWORK?                                     
         BNE   *+8                                                              
         CLI   TMPKDAY,0           FOR NETWORK, X'00'=M-F                       
         BNE   *+10                                                             
         MVC   0(3,R2),=C'M-F'     SET CORRECT DAY                              
         ZIC   R1,TMPSQH                                                        
         BAS   RE,QHRTOMIL                                                      
         STH   R1,DUB                                                           
         STCM  R1,3,TEMPSTIM                                                    
         ZIC   R1,TMPEQH                                                        
         BAS   RE,QHRTOMIL                                                      
         STH   R1,DUB+2                                                         
         STCM  R1,3,TEMPETIM                                                    
         B     GETDAY16                                                         
*                                  CONVERT INPUT DAY VALUES                     
GETDAY8  MVC   DUB(1),TMPKDAY                                                   
         XI    DUB,X'FF'           INVERT DAY VALUE                             
         ZIC   R1,DUB              GET DAY/TIME TABLE ENTRY                     
         LA    R0,L'DAYS                                                        
         MR    R0,R0                                                            
         LA    R1,DAYS-L'DAYS(R1)                                               
                                                                                
         MVC   WORK(L'DAYS),0(R1)                                               
         CLC   0(3,R1),=C'ROT'                                                  
         BNE   GETDAY8R                                                         
         OI    MYFLAG1,MYF1DTRO      IT IS A ROTATION                           
         ZICM  RF,3(R1),(3)                                                     
         LA    RF,LDTROTN(RF)                                                   
         ZIC   R0,FNDX               PICK UP COUNTER                            
         CLM   R0,1,0(RF)                                                       
         BNL   GTDAY30R                                                         
         AR    RF,R0                                                            
         MVC   WORK(L'DAYS),0(RF)                                               
GETDAY8R EQU   *                                                                
                                                                                
         IC    RF,WORK             RF=DAY EXPRESSION                            
         L     R1,AIDAYTAB                                                      
         MVC   DUB(1),WORK                                                      
         MVI   DUB+1,0                                                          
         MVC   TEMPIDAY,WORK+0                                                  
         MVC   TEMPSTIM,WORK+1                                                  
         MVC   TEMPETIM,WORK+3                                                  
*                                                                               
GETDAY10 CLI   0(R1),X'FF'         TEST E-O-T                                   
         BE    GETDAY14                                                         
         CLI   0(R2),C' '          TEST START DAY SET                           
         BE    *+10                                                             
         OC    DUB+1(1),0(R1)      YES - SET THIS DAY ON                        
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    0(R1),0             TEST IF DAY BIT ON                           
         BZ    GETDAY12                                                         
         MVC   3(3,R2),1(R1)       SET START & END DAY                          
         CLC   0(3,R2),SPACES                                                   
         BNE   *+10                                                             
         MVC   0(3,R2),3(R2)                                                    
         OC    DUB+1(1),0(R1)                                                   
         XC    DUB(1),0(R1)        TURN OFF THIS DAY IN MASK                    
         CLI   DUB,0               TEST ALL DAYS PROCESSED                      
         BE    GETDAY14            YES - GO FORMAT OUTPUT                       
GETDAY12 LA    R1,L'IDAYTAB(R1)                                                 
         B     GETDAY10                                                         
GETDAY14 CLC   DUB+1(1),WORK       TEST REGULAR ROTATOR (M-W,M-F ETC.)          
         BE    *+8                                                              
         MVI   3(R2),C'+'          NO - INDICATE IRREGULAR                      
         CLC   0(3,R2),3(R2)       TEST START EQ END DAY                        
         BE    *+14                                                             
         MVI   1(R2),C'-'          NO - SHOW FIRST CHAR OF EACH DAY             
         MVC   2(1,R2),3(R2)                                                    
         MVC   3(3,R2),SPACES                                                   
         MVC   DUB(4),WORK+1       SET TIME VALUES                              
*                                  EDIT TIME VALUES                             
GETDAY16 CLI   OPTTIME,C'M'                                                     
         BNE   GETDAY18                                                         
*                                  MILITARY TIME FORMAT                         
         MVI   3(R2),C'/'                                                       
         L     R0,DUB                                                           
         SRDL  R0,16               R0=START TIME                                
         SRL   R1,16               R1=END TIME                                  
         CVD   R0,DUB              EDIT START TIME                              
         OI    DUB+7,X'0F'                                                      
         UNPK  4(4,R2),DUB                                                      
         CR    R0,R1               TEST START TIME EQ END TIME                  
         B     GETDAY30                                                         
         MVI   8(R2),C'-'                                                       
         CVD   R1,DUB              EDIT END TIME                                
         OI    DUB+7,X'0F'                                                      
         UNPK  9(4,R2),DUB                                                      
         B     GETDAY30                                                         
*                                  STANDARD TIME FORMAT                         
GETDAY18 DS    0H                                                               
         CLI   1(R2),C'-'          TEST M-F TYPE                                
         BNE   *+10                                                             
         MVC   1(1,R2),2(R2)                                                    
         MVI   2(R2),C'/'                                                       
         CLC   DUB(2),DUB+2        TEST START TIME EQ END TIME                  
         BNE   *+10                                                             
         XC    DUB+2(2),DUB+2                                                   
         GOTO1 VUNTIME,DMCB,DUB,3(R2)                                           
                                                                                
*                                                                               
GETDAY30 DS    0H                                                               
         TM    MYFLAG1,MYF1DTRO    IS THIS A ROTATION?                          
         BZ    GTDAY30R                                                         
         ZIC   R1,FNDX              YES, TAKE COUNTER                           
         LA    R1,L'DAYS(R1)         AND BUMP IT TO GET NEXT COMPONENT          
         STC   R1,FNDX                                                          
                                                                                
         CLI   0(R2),C' '           BUMP OUTPUT POINTER TO NEXT                 
         BNH   *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-12                                                             
         MVI   0(R2),C'+'            AVAILABLE SLOT IN OUTPUT AREA              
         LA    R2,1(R2)                                                         
                                                                                
         B     GETDAY8                                                          
GTDAY30R EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  DONE FORMATTING DAYS/TIMES                   
         MVC   DAYTIME,WORK2                                                    
         TM    MYFLAG1,MYF1DTRO     IS IT A ROTATION?                           
         BZ    *+10                                                             
         MVC   DAYTIME(3),=C'ROT'    YES, FLAG IT                               
                                                                                
*                                                                               
GDYX     DS    0H                                                               
         B     XIT_01                                                           
         EJECT                                                                  
* ROUTINE TO CONVERT QUARTER HOUR (IN R1) TO MILITARY TIME                      
*                                                                               
QHRTOMIL SR    R0,R0                                                            
         D     R0,=F'4'                                                         
         LA    R1,5(R1)            BASE = 5AM IF RADIO,                         
         CLI   DBMED,C'R'                                                       
         BE    *+8                                                              
         LA    R1,1(R1)             ELSE, BASE = 6AM                            
         CH    R1,=H'24'           TEST AFTER MIDNIGHT                          
         BNH   *+8                                                              
         SH    R1,=H'24'           YES - GO BACK ONE DAY                        
         MH    R1,=H'100'                                                       
         MH    R0,=H'15'                                                        
         AR    R1,R0               R1 CONTAINS MILITARY TIME                    
         BR    RE                                                               
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR01--CMD+        
               #)'                                                              
*------------------------- CALC MAX # DTNTRYs ------------------------*         
                                                                                
* Calculates the max number of day/time combos that can fit into                
*  STBUFFER.  It is used in BINSRCH and other places.                           
* At exit, MAXNDT = the max number of day/time entries possible                 
                                                                                
CMXNDT   DS    0H                                                               
         SR    RE,RE                                                            
         LH    RF,LSTBUFFR                                                      
         SH    RF,=H'4'            LEAVE 4 BYTES FOR "HEADER" INFO              
         LA    R0,DTNTRYQ                                                       
         DR    RE,R0                                                            
         STH   RF,MAXNDT                                                        
*                                                                               
         B     XIT_01                                                           
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR01--DDT+        
               #)'                                                              
*---------------------- DO DUMMY TSAR DEMO RECDS ---------------------*         
                                                                                
* Goes through all combinations of this time's STATION/BOOK/DAY/TIME            
*  input and creates dummy TSAR DEMO RECORDs those combinations not in          
*  the sort buffer yet.                                                         
                                                                                
DODUMTDR DS    0H                                                               
         LA    R6,DBLOCK1                                                       
         USING DBLOCKD,R6                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,DBFIL                                                     
         MVC   DBAREC,AIOAREA                                                   
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   DBCOMFCS,AFAC                                                    
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELAGY,AGYALPH                                                 
                                                                                
         MVI   THISSTA,0                                                        
*                                                                               
DDT12    DS    0H                                                               
         ZIC   R1,THISSTA                                                       
         LA    R1,1(R1)                                                         
         STC   R1,THISSTA                                                       
         CLC   THISSTA,NSTAS                                                    
         BH    DDT70                                                            
         MVC   DBSELSTA,STAS       SET STATION FOR DEMAND CALL                  
         MVI   THISBK,0                                                         
                                                                                
DDT14    DS    0H                                                               
         ZIC   R1,THISBK                                                        
         LA    R1,1(R1)                                                         
         STC   R1,THISBK                                                        
         CLC   THISBK,NBKS                                                      
         BH    DDT12                                                            
         MVC   DBSELBK,BKS         SET BOOK                                     
         MVC   DBBTYPE,BKS+2        AND BOOK TYPE FOR DEMAND CALL               
         MVI   THISDAY,0                                                        
         LA    R2,DAYS-L'DAYS                                                   
                                                                                
DDT16    DS    0H                                                               
         ZIC   R1,THISDAY                                                       
         LA    R1,1(R1)                                                         
         STC   R1,THISDAY                                                       
         LA    R2,L'DAYS(R2)                                                    
         CLC   THISDAY,NDAYS                                                    
         BH    DDT14                                                            
                                                                                
         MVI   THISDAYR,0          ASSUME NO ROTATION                           
         NI    MYFLAG1,XFF-MYF1DTRO                                             
*                                                                               
DDT16M   DS    0H                  CHECK FOR ROTATIONS                          
         MVC   DUB(L'DAYS),0(R2)    ASSUME IT'S NOT A ROTATION                  
         CLC   0(3,R2),=C'ROT'                                                  
         BNE   DDT16R                                                           
         OI    MYFLAG1,MYF1DTRO     IT IS A ROTATION                            
         ZICM  RF,3(R2),(3)                                                     
         LA    RF,LDTROTN(RF)       RF = A(COMPONENTS OF ROTATION) - 1          
         ZIC   R1,THISDAYR          PICK UP COMPONENT COUNTER                   
         MH    R1,=Y(L'DAYS)        GET DISPLACEMENT TO COMPONENT               
         LA    R1,1(R1)                                                         
         AR    RF,R1                RF-->COMPONENT TO PROCESS NOW               
         MVC   DUB(L'DAYS),0(RF)                                                
DDT16R   EQU   *                                                                
                                                                                
         CLC   DBFIL,=C'PAV'                                                    
         BE    DDT100                                                           
*                                                                               
         CLI   ACTN,DEMOPER        FOR ACTION=PERIOD,                           
         BE    DDT30                NO DETAIL RECORDS NEEDED                    
*                                                                               
         DS    0H                  RESET DBLOCK FOR ANOTHER DEMAND CALL         
         MVC   DBSELDAY,DUB+0       SET DAY                                     
         MVC   DBSELTIM,DUB+1        AND TIMES FOR DEMAND CALL                  
         MVI   DBERROR,0                                                        
         MVI   DBDQPTR,0                                                        
         XC    DBLASTS,DBLASTS                                                  
         XC    DBDQTAB,DBDQTAB                                                  
         XC    DBACTUAL,DBACTUAL                                                
                                                                                
         DS    0H                  HAVE STATION/BOOK/DAY/TIME COMBO             
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0,0,0,0                                   
         LA    R4,DBDQD                                                         
         DROP  R6                                                               
                                                                                
         USING DBDQD,R4                                                         
*                                                                               
DDT20    DS    0H                  SET LOOPING PARAMETERS                       
         CLI   0(R4),XFF           IF NO MORE DBDQD ENTRIES,                    
         BE    DDT54                GO POST SUMMARY RECORD                      
         MVC   THISKDAY,DBDQKDAY                                                
         MVC   RQSTSQHR,DBDQSQH                                                 
         MVC   THISSQH,DBDQSQH                                                  
         MVC   THISEQH,DBDQSQH                                                  
         MVC   RQSTEQHR,DBDQEQH                                                 
         DROP  R4                                                               
                                                                                
         B     DDT30                                                            
*                                                                               
DDT30    DS    0H                  BUILD TSAR DEMO RECD KEY                     
         XC    TSARKEY,TSARKEY                                                  
         LA    R3,TSARKEY                                                       
         USING TSDEMRCD,R3                                                      
         MVI   TDRRCTYP,TDRRTDMV                                                
                                                                                
         ZICM  RF,THISSTA,(1)                                                   
         BZ    DDT32                                                            
         LH    RE,DSPRCTST                                                      
         LA    RE,REQCTLTB(RE)                                                  
         ZIC   R0,(RCTFROM-RCTDSECT)(RE)                                        
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
DDT32    STC   RF,TDRSTA                                                        
                                                                                
         ZICM  RF,THISBK,(1)                                                    
         BZ    DDT34                                                            
         LH    RE,DSPRCTBK                                                      
         LA    RE,REQCTLTB(RE)                                                  
         ZIC   R0,(RCTFROM-RCTDSECT)(RE)                                        
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
DDT34    STC   RF,TDRBK                                                         
                                                                                
         ZICM  RF,THISDAY,(1)                                                   
         BZ    DDT36                                                            
         LH    RE,DSPRCTDT                                                      
         LA    RE,REQCTLTB(RE)                                                  
         ZIC   R0,(RCTFROM-RCTDSECT)(RE)                                        
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
DDT36    STC   RF,TDRDAY                                                        
*                                                                               
         CLI   ACTN,DEMOPER        FOR ACTION=PERIOD,                           
         BE    DDT54                POST SUMMARY RECORD ONLY                    
                                                                                
DDT40    DS    0H                  R3 MUST BE POINTING TO TSARKEY               
         MVC   TDRDAYR,THISDAYR                                                 
         MVC   TDRKDAY,THISKDAY                                                 
                                                                                
DDT42    DS    0H                  R3 MUST BE POINTING TO TSARKEY               
         MVC   TDRSQH,THISSQH                                                   
         MVC   TDREQH,THISEQH                                                   
         DROP  R3                                                               
                                                                                
         GOTO1 AGETTDR             GO GET TSAR DEMO RECORD                      
         BE    DDT50               RECORD ALREADY EXISTS                        
*                                                                               
         DS    0H                  RECORD JUST ADDED TO TSAR BUFFER             
         LA    RF,TSARBLCK                                                      
         USING TSARD,RF                                                         
         ZICM  RE,TSAREC+1,(7)                                                  
         DROP  RF                                                               
                                                                                
         DS    0H                  FILL IT UP W/ DUMMY DATA                     
         LA    R3,2(RE)                                                         
         USING TSDEMRCD,R3                                                      
         MVC   TDRPROG,SPACES                                                   
         OI    TDRFLAG1,TF1DUMDV                                                
         XC    TDRDEMS,TDRDEMS                                                  
         DROP  R3                                                               
         LA    R0,TDRFIXL+L'TDRDEMS+2                                           
         STCM  R0,3,0(RE)                                                       
                                                                                
         GOTO1 APOST               POST DUMMY RECD W/ DUMMY VALUES              
*                                                                               
DDT50    DS    0H                  BUMP TO NEXT QUARTER-HOUR                    
         CLI   THISSQH,XFF          POSTED DUMMY SUMMARY RECORD?                
         BE    DDT16                 YEP, GET NEXT DAY TIME                     
         ZIC   R1,THISSQH                                                       
         CLM   R1,1,RQSTEQHR        REACHED END QUARTER-HOUR YET?               
         BNL   DDT52                 YEP, GO BUMP TO NEXT DBDQD ENTRY           
         LA    R1,1(R1)              NOPE, POST RECD FOR NEXT QHR               
         STC   R1,THISSQH                                                       
         STC   R1,THISEQH                                                       
         LA    R3,TSARKEY           POINT R3 BACK TO TSARKEY                    
         B     DDT42                 AND SET NEW KEY VALUES                     
                                                                                
DDT52    DS    0H                  BUMP TO NEXT DBDQD ENTRY                     
         LA    R4,DBDQLEN(R4)                                                   
         B     DDT20                                                            
                                                                                
DDT54    DS    0H                  SET FOR POSTING SUMMARY RECORD               
         TM    MYFLAG1,MYF1DTRO     CHECK FIRST IF IT'S A ROTATION              
         BZ    DDT59                                                            
         ZICM  RF,3(R2),(3)         IT IS A ROTATION                            
         LA    RF,LDTROTN(RF)       RF-->L(STORAGE SPACE FOR COMPNENTS)         
         ZIC   R1,THISDAYR          PICK UP COMPONENT COUNTER                   
         LA    R1,1(R1)              INCREMENT IT,                              
         STC   R1,THISDAYR           AND UPDATE TO NEW COUNT                    
         MH    R1,=Y(L'DAYS)        GET DISPLACEMENT TO NEXT COMPONENT          
         LA    R1,1(R1)              ADJUST FOR HEADER BYTE                     
         CLM   R1,1,0(RF)           DONE W/ ALL COMPONENTS?                     
         BL    DDT16M                NO, GO BACK & PROCESS NEXT ONE             
DDT59    EQU   *                                                                
*                                                                               
                                                                                
         DS    0H                  SET VALUES TO POST SUMMARY RECORD            
         MVI   THISDAYR,XFF                                                     
         MVC   THISKDAY,THISDAY                                                 
         XI    THISKDAY,XFF                                                     
         MVI   THISSQH,XFF                                                      
         MVI   THISEQH,XFF                                                      
         LA    R3,TSARKEY           POINT R3 BACK TO TSARKEY                    
         B     DDT40                 AND SET NEW KEY VALUES                     
*                                                                               
DDT70    DS    0H                                                               
         B     DDTX                                                             
                                                                                
                                                                                
DDT100   DS    0H                  PAV FILE                                     
         XC    TSARKEY,TSARKEY                                                  
         LA    R3,TSARKEY                                                       
         USING TSDEMRCD,R3                                                      
         MVI   TDRRCTYP,TDRRTDMV                                                
                                                                                
         ZICM  RF,THISSTA,(1)                                                   
         BZ    DDT112                                                           
         LH    RE,DSPRCTST                                                      
         LA    RE,REQCTLTB(RE)                                                  
         ZIC   R0,(RCTFROM-RCTDSECT)(RE)                                        
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
DDT112   STC   RF,TDRSTA                                                        
                                                                                
         ZICM  RF,THISBK,(1)                                                    
         BZ    DDT114                                                           
         LH    RE,DSPRCTBK                                                      
         LA    RE,REQCTLTB(RE)                                                  
         ZIC   R0,(RCTFROM-RCTDSECT)(RE)                                        
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
DDT114   STC   RF,TDRBK                                                         
         DROP  R3                                                               
*                                                                               
         DS    0H                  MAKE A COPY OF TSAR BLOCK AND USE IT         
         MVC   WORK(TSARDL),TSARBLCK                                            
         LA    R4,WORK                                                          
         USING TSARD,R4                                                         
         ZICM  R2,TSAREC+1,(7)                                                  
                                                                                
         LA    R3,2(R2)                                                         
         USING TSDEMRCD,R3                                                      
         MVC   TDRKEY(TDRKEYL),TSARKEY  PUT KEY IN I/O AREA                     
         MVI   TSACTN,TSARDH            READ HIGH FOR RECORD                    
         XC    TSRNUM,TSRNUM                                                    
         GOTO1 VTSAR,(R4)                                                       
         CLI   TSERRS,0                                                         
         BE    DDT150                                                           
         TM    TSERRS,TSEEOF                                                    
         BO    DDT120                                                           
         CLC   TDRKEY(TKYBKL),TSARKEY                                           
         BE    DDT150                                                           
                                                                                
                                                                                
DDT120   DS    0H                  NOTHING FOR STATION/BOOK COMBO               
         B     DDTX                                                             
         DROP  R3                                                               
         DROP  R4                                                               
                                                                                
*                                                                               
DDT150   DS    0H                                                               
         MVI   STBKDATA,C'Y'       THERE IS DATA FOR STTN/BOOK COMBO            
         B     DDTX                                                             
*                                                                               
DDTX     DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR01--GWK+        
               #)'                                                              
*---------------------GET ACTIVE WEEKS INDICATORS --------------------*         
                                                                                
* Translates the week bits in TMPWKS into printable format in ACTVWKS.          
                                                                                
GETWKS   DS    0H                                                               
         MVI   ACTVWKS,C'.'                                                     
         MVC   ACTVWKS+1(L'ACTVWKS-1),ACTVWKS                                   
         CLI   DBMED,C'N'                                                       
         BNE   *+22                                                             
         TM    TMPWKS,X'0F'                                                     
         BNZ   *+14                                                             
         MVC   ACTVWKS,=C' N/A'                                                 
         B     GETWKSX                                                          
*                                                                               
         TM    TMPWKS,X'08'                                                     
         BZ    *+8                                                              
         MVI   ACTVWKS+0,C'1'                                                   
         TM    TMPWKS,X'04'                                                     
         BZ    *+8                                                              
         MVI   ACTVWKS+1,C'2'                                                   
         TM    TMPWKS,X'02'                                                     
         BZ    *+8                                                              
         MVI   ACTVWKS+2,C'3'                                                   
         TM    TMPWKS,X'01'                                                     
         BZ    *+8                                                              
         MVI   ACTVWKS+3,C'4'                                                   
*                                                                               
         DS    0H                                                               
GETWKSX  B     XIT_01                                                           
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR01--MTQ+        
               #)'                                                              
*--------------------- MILITARY TO QUARTER HOURS ---------------------*         
                                                                                
* ROUTINE TO CONVERT MILITARY TIME TO QUARTER HOUR                              
* At entry,                                                                     
*   TEMPMTIM = input military time,                                             
* At exit,                                                                      
*   TEMPQHR  = output quarter hour.                                             
                                                                                
MILTOQHR DS    0H                                                               
         SR    R0,R0               R0 WILL CONTAIN QUARTER HOUR                 
         ZICM  RE,TEMPMTIM,(3)                                                  
         CH    RE,=H'600'                                                       
         BNL   *+8                                                              
         AH    RE,=H'2400'                                                      
         SH    RE,=H'600'                                                       
         BZ    MTQ20                                                            
         SRDA  RE,32                                                            
         D     RE,=F'100'                                                       
         SLA   RF,2                MULTIPLY BY 4                                
         AR    R0,RF                                                            
         SRDA  RE,32                                                            
         D     RE,=F'15'                                                        
         AR    R0,RF                                                            
MTQ20    STC   R0,TEMPQHR                                                       
         B     XIT_01                                                           
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR01--S1M+        
               #)'                                                              
*------------------------ SUBTRACT ONE MINUTE ------------------------*         
                                                                                
* ROUTINE TO SUBTRACT 1 MINUTE FROM A GIVEN TIME                                
* At entry,                                                                     
*   TEMPMTIM = input military time,                                             
* At exit,                                                                      
*   TEMPMTIM = output military time.                                            
                                                                                
SUB1MIN  DS    0H                                                               
         LA    RF,59               59TH MINUTE                                  
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,TEMPMTIM                                                    
         BNZ   *+8                                                              
         LA    R1,2400             FORCE MIDNIGHT TO 2400 HOUR                  
         BCTR  R1,0                SUBTRACTING THE 1 MINUTE                     
         D     R0,=F'100'                                                       
         CR    R0,RF               IF REMAINDER > 59, USE 59 MINUTES,           
         BH    *+6                                                              
         LR    RF,R0                ELSE, USE THE REMAINDER                     
         SR    R0,R0                                                            
         M     R0,=F'100'          GET TIME BACK TO MILITARY                    
         AR    R1,RF                                                            
         STCM  R1,3,TEMPMTIM                                                    
         B     XIT_01                                                           
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR01--GIS+        
               #)'                                                              
*----------------- GET INDIVIDUAL (BOOK) SUMMARY LINE ----------------*         
                                                                                
* Routine to get the summary line for individual books in a latest  n           
*  books request and post it to the buffer.  It calls SPDEMLK with              
*  the specific book that is currently being processed.                         
* At entry,                                                                     
*   DBLOCK1  = the main SPDEMLKD block                                          
*   MYNLBK   = number of latest books in packed decimal                         
*   R2      -->current day/time in process                                      
                                                                                
GTINDSUM DS    0H                                                               
         LA    R5,LNBKLIST                                                      
         ZAP   THISLBK,=P'1'                                                    
*                                                                               
         LA    R6,SPDMLK2          SET UP A SECOND SPDEMLKD BLOCK               
         USING SPDEMLKD,R6                                                      
         MVC   SPDEMLK(SPDEMLKL),DBLOCK1                                        
         XC    SPLKHOOK,SPLKHOOK    NO NEED FOR HOOKS HERE                      
                                                                                
*                                                                               
GIS010   DS    0H                                                               
         MVC   SPLKDBK,0(R5)        GET SPECIFIC BOOK                           
*                                                                               
         DS    0H                   SET DAYS & TIMES                            
         CLC   0(3,R2),=C'ROT'                                                  
         BE    *+20                                                             
         MVC   SPLKDAY,0(R2)         DIRECTLY INTO SPDEMLK BLOCK                
         MVC   SPLKTIM,1(R2)                                                    
         B     GIS015X                                                          
                                                                                
         DS    0H                    SET UP DAY/TIME LINK IN SPDEMLK            
         XC    DUB,DUB                                                          
         LA    R0,MYDBXTRA                                                      
         ST    R0,DUB+4                                                         
         MVI   GOSUBN,SDBX#                                                     
         GOTO1 AGOSUB                                                           
                                                                                
         ICM   RF,15,FULL                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBXTLD,RF                                                        
         MVC   DBXTLID,=C'DYTM'                                                 
         ZICM  RE,3(R2),(3)                                                     
         LA    RE,LDTROTN(RE)                                                   
         ZIC   R1,0(RE)                                                         
         SH    R1,=H'2'                                                         
         EXMVC R1,DBXTLIST,1(RE)                                                
         LA    R1,DBXTLIST+1(R1)                                                
         MVI   0(R1),0                                                          
         DROP  RF                                                               
GIS015X  EQU  *                                                                 
*                                                                               
         XC    SPLKPRG,SPLKPRG                                                  
         XC    SPLKDBLK,SPLKDBLK                                                
                                                                                
         MVC   SPLKUID,USERID                                                   
         GOTO1 VSPDEMLK,MYDMCB,(X'FF',SPDEMLKD)                                 
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         BNE   GIS099                                                           
                                                                                
         DS    0H                  DROP SVI OR DEMO VALUES FROM LOOKUP          
         LA    RF,THISDEMS          DROPPING SVI                                
         CLI   OPTDTYP,C'S'                                                     
         BNE   *+8                                                              
         LA    RF,THISDEMS+4        DROPPING DEMO VALUES                        
         ZIC   R0,DEMONDEM                                                      
         LA    R1,THISDEMS                                                      
         MVC   0(4,R1),0(RF)                                                    
         LA    R1,4(R1)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,*-14                                                          
                                                                                
*                                                                               
         DS    0H                  POST TO BUFFER                               
         XC    POSTLINE,POSTLINE    PUTTING STUFF ON POSTLINE DIRECTLY          
         LA    R1,POSTLINE                                                      
         USING BINRECD,R1                                                       
         MVC   BINKEY(BINKEYL),THISKEY                                          
         MVC   BINKDAY,THISDAY                                                  
         XI    BINKDAY,XFF                                                      
         MVI   BINSQH,XFF                                                       
         MVI   BINEQH,XFF                                                       
         MVI   BINTYPE,0                                                        
                                                                                
         MVC   BINPROG,SPLKPRG                                                  
         MVC   BINBOOK,0(R5)                                                    
         LA    RE,BINDEMS                                                       
         LA    RF,THISDEMS                                                      
         ZIC   R0,NDEMS                                                         
         MVC   0(3,RE),1(RF)                                                    
         LA    RE,3(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,*-14                                                          
                                                                                
         GOTO1 APOST                                                            
         DROP  R1,R6                                                            
GIS099   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  BUMP TO NEXT LATEST BOOK IN LIST             
         SP    MYNLBK,=P'1'                                                     
         BZ    GISX                                                             
         LA    R5,L'LNBKLIST(R5)                                                
         AP    THISLBK,=P'1'                                                    
         B     GIS010                                                           
                                                                                
*                                                                               
GISX     DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR01--GPR+        
               F#)'                                                             
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
         BAS   RE,GETEL                                                         
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
GETPRFLX DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR01--PIU+        
               N#)'                                                             
*------------------------- PROCESS IUN STUFF -------------------------*         
                                                                                
* At entry,                                                                     
*   R6-->DBLOCK,                                                                
*   IUNOLD values are set,                                                      
*   AVUTS-->VUT values to use in computing home shares,                         
*   DEMODEMS = list of requested demos.                                         
* At exit,                                                                      
*   THISDEMS = output demo values.                                              
                                                                                
PROCIUN  DS    0H                                                               
         USING DBLOCKD,R6                                                       
*                                                                               
         L     R4,AIUNWRK                                                       
         LA    R4,500(R4)                                                       
         USING IUNRECD,R4                                                       
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
         GOTO1 VDEMOUT,DMCB,(C'L',ASHRLIST),DBLOCKD,ISHOMES,0                   
                                                                                
*                                                                               
* SAVE DBLOCK *                                                                 
*                                                                               
         LA    R0,SVDBLOCK          DESTINATION                                 
         LA    R1,DBLOCK1X-DBLOCK1                                              
         LA    RE,DBLOCK            SOURCE                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
* BUILD IUN RECORD *                                                            
*                                                                               
         DS    0H                  SET DBLOCK VALUES                            
         L     RE,AIUNWRK                                                       
         ST    RE,DBAREC            SET A(RECORD)                               
         LA    RE,23(RE)                                                        
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
* CALL DEMOUT TO EXTRACT REQUESTED DEMOS *                                      
*                                                                               
         GOTO1 VDEMOUT,MYDMCB,(C'L',DEMODEMS),DBLOCK,THISDEMS                   
*                                                                               
* RESTORE DBLOCK *                                                              
*                                                                               
         LA    R0,DBLOCK            DESTINATION                                 
         LA    R1,DBLOCK1X-DBLOCK1                                              
         LA    RE,SVDBLOCK          SOURCE                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         B     XIT_01                                                           
         DROP  R4,R6                                                            
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR01--MIS+        
               C STUFF)'                                                        
*--------------------- SUBR01 MISCELLANEOUS STUFF --------------------*         
                                                                                
         GETEL R3,MYDATDSP,MYELCODE                                             
                                                                                
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR01--LTO+        
               R && CONSTANTS)'                                                 
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
         SPACE 1                                                                
OFORMAT  DC    C'IUNUIUN',X'530B00'                                             
                                                                                
SUBR01L  EQU   *-SUBR01                                                         
         DS    0CL(4096-SUBR01L+1)                                              
                                                                                
         DROP  R7,R8,R9,RB,RC                                                   
***********************************************************************         
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR02)'            
***********************************************************************         
*======================== SUBROUTINE POOL TWO ========================*         
                                                                                
* At entry,                                                                     
*   R9-->DEMWRKD,                                                               
*   R8-->TWA,                                                                   
*   R7-->DEMTMPD,                                                               
*   R1 = equated sub-routine number.                                            
                                                                                
SUBR02Q  EQU   (((*-DEM0C+4095)/4096)*4096)                                     
                                                                                
         ORG   DEM0C+SUBR02Q                                                    
SUBR02   NMOD1 0,**0C02**                                                       
         USING DEMWRKD,R9          R9=A(GLOBAL WORK AREA)                       
         USING DEMTWAD,R8          R8=A(TWA)                                    
         USING DEMTMPD,R7          R7=A(TEMP W/S)                               
                                                                                
         L     RC,ALOCWRK                                                       
         USING DM0CWRKD,RC                                                      
*                                                                               
         SH    R1,=Y(R01#)         SUBTRACT FOR SUB-RTN # 2                     
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R02_00(R1)                                                       
                                                                                
DPST#    EQU   (R02_01-*)/4+R01#+1 CALLS ROUTE POST ROUTINE                     
SDBX#    EQU   (R02_02-*)/4+R01#+1 SET UP A LINK IN DBEXTEND AREA               
PDDR#    EQU   (R02_03-*)/4+R01#+1 POST DUMMY DATA RECORD                       
GADLD#   EQU   (R02_04-*)/4+R01#+1 GET A(DOWNLOAD DATA TABLES)                  
OKDL#    EQU   (R02_05-*)/4+R01#+1 OKAY TO DOWNLOAD DATA?                       
D32DL#   EQU   (R02_06-*)/4+R01#+1 DEMLINE PROCESSING FOR DEM32                 
D32TP#   EQU   (R02_07-*)/4+R01#+1 DOWNLOAD TP DATA                             
D32PV#   EQU   (R02_08-*)/4+R01#+1 DOWNLOAD PAV DATA                            
                                                                                
R02_00   DS    0H                                                               
R02_01   B     DEMPST              POST INFO TO BUFFER                          
R02_02   B     STDBXLNK            SET UP A LINK IN DBEXTEND AREA               
R02_03   B     PDUMDREC            POST DUMMY DATA RECORD                       
R02_04   B     GADLDTAB            GET A(DOWNLOAD DATA TABLES)                  
R02_05   B     OKDOWNLD            OKAY TO DOWNLOAD DATA?                       
R02_06   B     D32DEMLN            DEMLINE PROCESSING FOR DEM32                 
R02_07   B     D32DLTP             DOWNLOAD TP DATA                             
R02_08   B     D32DLPAV            DOWNLOAD PAV DATA                            
R02#     EQU   (*-R02_00)/4+R01#                                                
         DC    H'0'                                                             
                                                                                
YES_02   SR    R9,R9                                                            
NO_02    LTR   R9,R9                                                            
XIT_02   XIT1                                                                   
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR02--DPS+        
               T#)'                                                             
*----------- BUILD BINSRCH RECORD & CALL ROOT POST ROUTINE -----------*         
                                                                                
DEMPST   DS    0H                                                               
         TM    DEMFLAG1,DF1STERO   IS THIS A STEREO SESSION?                    
         BO    DMPOST50             YES, POST DIFFERENTLY                       
         B     DPSTX                                                            
                                                                                
*                                                                               
** THIS CODE FOR STEREO POSTING **                                              
*                                                                               
DMPOST50 DS    0H                                                               
         CLI   RECTYPE,TDRRTDMV    IF POSTING DEMO VALUES, CHECK                
         BNE   *+20                                                             
         TM    MYFLAG1,MYF1LNBK     IF THIS IS A LATEST N BK RQST,              
         BZ    *+12                                                             
         CLI   THISLBK,X'9C'         ONLY POST THE SUMMARY ONE                  
         BNE   XIT_02                                                           
                                                                                
         XC    TSARKEY,TSARKEY                                                  
         LA    R1,TSARKEY                                                       
         USING TSDEMRCD,R1                                                      
         MVC   TDRRCTYP,RECTYPE                                                 
                                                                                
         ZICM  RF,THISSTA,(1)                                                   
         BZ    DMPST51C                                                         
         LH    RE,DSPRCTST                                                      
         LA    RE,REQCTLTB(RE)                                                  
         ZIC   R0,(RCTFROM-RCTDSECT)(RE)                                        
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
DMPST51C STC   RF,TDRSTA                                                        
                                                                                
         ZICM  RF,THISBK,(1)                                                    
         BZ    DMPST52C                                                         
         LH    RE,DSPRCTBK                                                      
         LA    RE,REQCTLTB(RE)                                                  
         ZIC   R0,(RCTFROM-RCTDSECT)(RE)                                        
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
DMPST52C STC   RF,TDRBK                                                         
                                                                                
         ZICM  RF,THISDAY,(1)                                                   
         BZ    DMPST53C                                                         
         LH    RE,DSPRCTDT                                                      
         LA    RE,REQCTLTB(RE)                                                  
         ZIC   R0,(RCTFROM-RCTDSECT)(RE)                                        
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
DMPST53C STC   RF,TDRDAY                                                        
                                                                                
         MVC   TDRDAYR,THISDAYR                                                 
         MVC   TDRKDAY,THISKDAY                                                 
         MVC   TDRSQH,THISSQH                                                   
         MVC   TDREQH,THISEQH                                                   
         MVC   TDRTYPE,THISTYPE                                                 
         MVC   TDR1WK,THIS1WK                                                   
         MVC   TDRNOR,THISNOR                                                   
         MVC   TDRASQC,THISASQC                                                 
         MVC   TDRAEQC,THISAEQC                                                 
         MVC   TDRSETM,THISSETM                                                 
         MVC   TDRRTYPE,THISRTYP                                                
         DROP  R1                                                               
         GOTO1 AGETTDR             GO GET TSAR DEMO RECORD                      
         BE    DMPOST60                                                         
                                                                                
         DS    0H                  RECORD JUST ADDED TO TSAR BUFFER             
         LA    RF,TSARBLCK                                                      
         USING TSARD,RF                                                         
         ZICM  R1,TSAREC+1,(7)                                                  
         DROP  RF                                                               
         LA    R1,2(R1)                                                         
         USING TSDEMRCD,R1                                                      
         CLI   TDRRCTYP,TDRRTDML   IS THIS RECD TO CONTAIN DEMOS?               
         BE    DMPOST60             YES                                         
         DROP  R1                                                               
                                                                                
         CLC   DBFIL,=C'PAV'        NO, AND IF PAV FILE                         
         BNE   DMPOST60                                                         
         MVI   GOSUBN,DTL#           PROCSS A DAY/TIME LST (FOR STEREO)         
         GOTO1 AGOSUB                                                           
*                                                                               
DMPOST60 DS    0H                                                               
         LH    R2,DSPRCTDM         WANT THE DEMOS ENTRY                         
         LA    R2,REQCTLTB(R2)                                                  
         USING RCTDSECT,R2                                                      
                                                                                
         L     RE,ASTDMEXP                                                      
         ZIC   RF,0(RE)            RF = TOTAL # OF DEMOS TO PROCESS             
         ZIC   R0,NDEMS                                                         
         SR    RF,R0                                                            
         STC   RF,DEMOFROM                                                      
         ZIC   RF,RCTUPTO                                                       
         STC   RF,DEMOUPTO                                                      
         DROP  R2                                                               
*                                                                               
DMPOST70 DS    0H                                                               
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         ZICM  R2,TSAREC+1,(7)                                                  
         LA    R3,2(R2)                                                         
         USING TSDEMRCD,R3                                                      
                                                                                
         DS    0H                                                               
         MVC   TDRFLAG1,THISFLG1   TRANSFER FLAG INTO RECORD                    
         MVC   TDRWKS,THISWKS      MOVE ACTIVE WEEKS INTO RECORD                
         MVC   TDRAFFL,THISAFFL                                                 
                                                                                
         DS    0H                  MOVE PROGRAM NAME INTO RECORD,               
         MVC   TDRPROG,SPACES                                                   
         LA    R0,L'TDRPROG                                                     
         LA    RE,TDRPROG                                                       
         LA    RF,THISPROG                                                      
DMPOST71 CLI   0(RF),STSPIFLD       REMOVING ALL FIELD SEPARATORS               
         BE    *+14                                                             
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,DMPOST71                                                      
                                                                                
         MVC   TDRPURE,THISPURE                                                 
                                                                                
         DS    0H                  APPEND DATA ONTO RECORD                      
         ZIC   RE,DEMOFROM                                                      
         MH    RE,=Y(L'TDRDEMS)                                                 
         LA    RE,TDRDEMS(RE)      RE-->NXT PLACE TO PUT DEMOS                  
                                                                                
         LA    RF,THISDEMS         ASSUME POSTING FOR DEMO VALUES               
         CLI   RECTYPE,TDRRTDML                                                 
         BNE   *+8                                                              
         LA    RF,DEMS              POSTING DEMO EXPRESSIONS                    
                                                                                
         ZIC   R0,NDEMS            R0=# OF DATA VALUES TO APPEND                
DMPOST74 CLI   RECTYPE,TDRRTDML    IF POSTING DEMO VALUES,                      
         BE    *+8                                                              
         LA    RF,1(RF)             BUMP SOURCE BY ONE BYTE                     
         MVC   0(L'TDRDEMS,RE),0(RF)                                            
         LA    RE,L'TDRDEMS(RE)    RE-->NEW END OF TSAR DEMO RECORD             
         LA    RF,3(RF)            RF-->NEXT SOURCE                             
         BCT   R0,DMPOST74                                                      
                                                                                
         CLI   RECTYPE,TDRRTDML    AND IF POSTING DEMO EXPRESSIONS,             
         BNE   *+12                                                             
         MVI   0(RE),XFF            MOVE IN END-OF-LIST DELIMITER               
         LA    RE,1(RE)                                                         
                                                                                
         SR    RE,R2               RE=NEW L(TSAR DEMO RECORD)                   
         STH   RE,0(R2)                                                         
         GOTO1 APOST                                                            
         DROP  R3,R4                                                            
                                                                                
*                                                                               
DPSTX    DS    0H                                                               
         B     XIT_02                                                           
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR02--SDB+        
               X#)'                                                             
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
         B     XIT_02                                                           
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR02--PDD+        
               R#)'                                                             
*----------------------- POST DUMMY DATA RECORD ----------------------*         
                                                                                
* Posts dummy records to be used as "space-fillers" when data is                
*  passed back to STEREO.                                                       
                                                                                
PDUMDREC DS    0H                                                               
         CLC   DBFIL,=C'PAV'                                                    
         BE    PDDRPAV                                                          
         B     PDDRX                                                            
                                                                                
*                                                                               
** PAV FILE **                                                                  
*                                                                               
PDDRPAV  DS    0H                                                               
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         ZICM  R2,TSAREC+1,(7)                                                  
         LA    R3,2(R2)                                                         
         USING TSDEMRCD,R3                                                      
         XC    0(TDRFIXL,R3),0(R3)                                              
         MVI   TDRRCTYP,TDRRTDMV    RECORD TYPE = DEMO VALUE                    
         MVC   TDRSTA,SVTHSSTN      STATION                                     
         MVC   TDRBK,SVTHSBK         & BOOK COMBO                               
         MVI   TDRKDAY,X01          DUMMY KEY DAY                               
         MVI   TDRWKS,X'0F'         MIGHT AS WELL PUT ALL WEEKS                 
         OI    TDRFLAG1,TF1DUMDV    DUMMY DEMO VALUE                            
         DROP  R3                                                               
         LA    R0,TDRFIXL+2                                                     
         STCM  R0,3,0(R2)                                                       
         DROP  R4                                                               
*                                                                               
         DS    0H                                                               
         GOTO1 APOST                POST DUMMY DATA RECORD                      
         B     PDDRX                                                            
                                                                                
*                                                                               
PDDRX    DS    0H                                                               
         B     XIT_02                                                           
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR02--GAD+        
               LD#)'                                                            
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
         CLC   DLDTRTYP,TMPRCTYP                                                
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
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR02--OKD+        
               L#)'                                                             
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
         CLI   PUFLLOO,C'N'        OKAY FROM LAST LEFT OFF?                     
         BNE   OKDL010X             EITHER "YES" OR "N/A"                       
         L     RF,AAPSAV2                                                       
         USING APSAV2D,RF                                                       
         CLC   AS2EMPC,FALEMPC      MATCH ON ELEM MAP CODE AT BREAK             
         BNE   OKDLXN                                                           
         CLC   AS2DMPC,FALDMPC      MATCH ON DATA MAP CODE AT BREAK             
         BNE   OKDLXN                                                           
         DROP  RF                                                               
         MVI   PUFLLOO,C'Y'        NOW WE'RE OKAY FROM LAST LEFT OFF            
         B     OKDLXN               BUT DON'T DOWNLOAD THIS DATA TWICE          
OKDL010X EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
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
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR02--D32+        
               DL#)'                                                            
*-------------------- DEMLINE PROCESSING FOR DEM32 -------------------*         
                                                                                
* At entry,                                                                     
*   R5-->TSAR demo record                                                       
                                                                                
D32DEMLN DS    0H                                                               
         USING TSDEMRCD,R5                                                      
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   PUFLLOO,0           INITIALIZE "LAST LEFT OFF" FLAGS             
         MVI   PUFLLOO3,0                                                       
                                                                                
         L     RF,AAPSAV2                                                       
         USING APSAV2D,RF                                                       
         TM    AS2RSMF,AS2RFFLK    IF FALINK RESUME,                            
         BZ    D32DL019                                                         
         MVI   PUFLLOO,C'N'         NEED TO RE-ESTABLISH MAP CODES              
         CLI   AS2DEMON,0           IF BREAK OCCURRED ADDING DEMO,              
         BE    *+8                                                              
         MVI   PUFLLOO3,C'N'         NEED TO RE-ESTABLISH NTH DEMO              
D32DL019 EQU   *                                                                
         DROP  RF                                                               
                                                                                
         DS    0H                  GO TO LOGIC CORRESPONDING TO RCD TYP         
         MVC   TMPRCTYP,TDRRCTYP                                                
         CLI   TDRRCTYP,TDRRTDMV                                                
         BE    D32DL100                                                         
         CLI   TDRRCTYP,TDRRTDML    DEMO LIST                                   
         BE    D32DL200                                                         
         B     D32DLX                                                           
         EJECT                                                                  
D32DL100 DS    0H                  RECORD CONTAINS PROGRAM & DEMO INFO          
         MVI   GOSUBN,D32TP#                                                    
         CLC   DBFIL,=C'TP '                                                    
         BE    D32DL115X                                                        
         MVI   GOSUBN,D32PV#                                                    
         CLC   DBFIL,=C'PAV'                                                    
         BE    D32DL115X                                                        
         DC    H'0'                                                             
D32DL115X EQU  *                                                                
         GOTO1 AGOSUB                                                           
         B     D32DLX                                                           
         EJECT                                                                  
D32DL200 DS    0H                  RECORD CONTAINS DEMO LIST                    
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
         MVC   FALEMPC,=Y(FMHDMLS)                                              
         MVC   FALDMPC,=Y(FMDDMLSFDNM)                                          
         BAS   RE,D32DLOKD         OKAY TO RELEASE DEMO NAME?                   
         BNE   D32DL299             NO, EXIT NOW                                
                                                                                
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
*        GOTO1 VDEMOCON,DMCB,(STNDEMS,(R3)),(7,(R0)),(BYTE,DBLOCKD),0           
         GOTO1 VDEMOCON,DMCB,(STNDEMS,(R3)),(6,(R0)),(BYTE,DBLOCKD),0           
         DROP  R4                                                               
                                                                                
                                                                                
         DS    0H                                                               
         MVC   FALEMPC,=Y(FMHDMLS)                                              
                                                                                
         CLI   PUFLLOO,C'N'        IS "P/U FROM LAST LEFT OFF" NOT OK?          
         BNE   D32DL232K                                                        
         L     RF,AAPSAV2                                                       
         USING APSAV2D,RF                                                       
         CLC   AS2EMPC,FALEMPC      TRY TO RESUME FROM LAST OFF?                
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
         B     D32DL232M                                                        
D32DL232K EQU  *                                                                
                                                                                
         SR    R1,R1                                                            
         GOTO1 ADM32SET            DO SETELEM FOR FALINK FIRST                  
D32DL232M EQU  *                                                                
                                                                                
*                                                                               
         DS    0H                  DOWNLOAD DEMO NAMES                          
         ZIC   R5,STNDEMS                                                       
         L     R6,AIOAREA          R4-->DEMO NAMES                              
*                                                                               
D32DL242 DS    0H                                                               
         MVC   FALDMPC,=Y(FMDDMLSFDNM)                                          
         LR    R0,R6               R0-->FORMATTED DEMO NAME                     
         LA    R1,6                                                             
         BAS   RE,D32DLAD                                                       
*                                                                               
         DS    0H                  DEMO PRECISION                               
         LA    R2,WORK              R2-->OUTPUT AREA FOR DEMO VALUE             
                                                                                
         ZIC   RF,STNDEMS                                                       
         SR    RF,R5                                                            
         LR    RE,RF                                                            
         MH    RF,=H'3'                                                         
         L     R3,ASTDMEXP                                                      
         LA    R3,1(RF,R3)          R3-->CURRENT DEMO EXPRESSION                
                                                                                
         SLL   RE,2                                                             
         LA    R4,TDRDEMS(RE)       R4-->DEMO VALUES                            
                                                                                
         MVI   GOSUBN,GDM#          GET DEMO PRECISION                          
         GOTO1 AGOSUB                                                           
         MVC   FALDMPC,=Y(FMDDMLSPREC)                                          
         LA    R1,1                                                             
         LA    R0,DEMOPRE                                                       
         BAS   RE,D32DLAD                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         LA    R6,6(R6)                                                         
                                                                                
         BCT   R5,D32DL242                                                      
*                                                                               
                                                                                
                                                                                
         DS    0H                                                               
         CLC   DBFIL,=C'PAV'       IF PAV FILE,                                 
         BNE   *+8                                                              
         MVI   APMODE,SAMEREC       GET SAME REC TO RSTORE FUDGED DMLST         
         B     D32DL299                                                         
D32DL299 EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         B     D32DLX                                                           
                                                                                
                                                                                
*                                                                               
D32DLX   DS    0H                                                               
         DS    0H                  ENSURE WE'VE P/UP FROM LAST LEFT OFF         
         L     R6,AAPSAV2                                                       
         USING APSAV2D,R6                                                       
         TM    AS2RSMF,AS2RFFLK                                                 
         BZ    D32DLPUX                                                         
         CLI   PUFLLOO,C'N'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         NI    AS2RSMF,XFF-AS2RFFLK                                             
         DROP  R6                                                               
D32DLPUX EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         J     EXIT                                                             
         DROP  R5                                                               
                                                                                
                                                                                
D32DLOKD NTR1                                                                   
         MVI   GOSUBN,OKDL#                                                     
         GOTO1 AGOSUB                                                           
         J     EXIT                                                             
                                                                                
                                                                                
* Little helper routine to add data to FALINK buffer                            
* At entry,                                                                     
*   FALDMPC = map code of data to be added                                      
*   R0      = A(data to be added)                                               
                                                                                
D32DLAD  NTR1                                                                   
         ST    R0,ADLDATA          SET A(DATA TO DOWNLOAD)                      
         ST    R1,LDLDATA          SET L(DATA TO DOWNLOAD)                      
         MVI   ADMODE,ADMADD                                                    
         GOTO1 ADM32ADD                                                         
         J     EXIT                                                             
         TITLE 'DEDEM0C - $DEM LILO REPORT (SUBR02--D32TP#)'                    
*--------------------- DEM32 DOWLOAD TIME PERIOD ---------------------*         
                                                                                
* At entry,                                                                     
*   R5-->TSAR demo record containing TP demo information                        
                                                                                
D32DLTP  DS    0H                                                               
         USING TSDEMRCD,R5                                                      
                                                                                
                                                                                
         DS    0H                  DO SETELEM FOR FALINK FIRST                  
         MVC   FALEMPC,=Y(FMHTPDP)                                              
                                                                                
         CLI   PUFLLOO,C'N'        IS "P/U FROM LAST LEFT OFF" NOT OK?          
         BNE   D32TP012K                                                        
         L     RF,AAPSAV2                                                       
         USING APSAV2D,RF                                                       
         CLC   AS2EMPC,FALEMPC      TRY TO RESUME FROM LAST OFF?                
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
         B     D32TP012M                                                        
D32TP012K EQU  *                                                                
                                                                                
         SR    R1,R1                                                            
         GOTO1 ADM32SET                                                         
D32TP012M EQU  *                                                                
                                                                                
                                                                                
         DS    0H                  "RECORD" TYPE                                
         MVC   FALDMPC,=Y(FMDTPDPRTYP)                                          
         BAS   RE,D32TPOKD                                                      
         BNE   D32TP029                                                         
*                                                                               
         LA    R0,TDRRTYPE                                                      
         LA    R1,1                                                             
         BAS   RE,D32TPAD                ADD DATA TO DOWNLOAD                   
D32TP029 EQU   *                                                                
                                                                                
                                                                                
         DS    0H                  STATION NUMBER                               
         MVC   FALDMPC,=Y(FMDTPDPSTNN)                                          
         BAS   RE,D32TPOKD                                                      
         BNE   D32TP034                                                         
*                                                                               
         ZICM  RF,TDRSTA,(1)              GET STATION NUMBER                    
         BZ    D32TP034                                                         
         STC   RF,BYTE                                                          
                                                                                
         LA    R0,BYTE                                                          
         SR    R1,R1                                                            
         BAS   RE,D32TPAD                  ADD DATA TO DOWNLOAD                 
D32TP034 EQU   *                                                                
                                                                                
                                                                                
         DS    0H                  BOOK NUMBER                                  
         MVC   FALDMPC,=Y(FMDTPDPBKN)                                           
         BAS   RE,D32TPOKD                                                      
         BNE   D32TP039                                                         
*                                                                               
         ZICM  RF,TDRBK,(1)                GET BOOK NUMBER                      
         OR    RF,RF                                                            
         BZ    D32TP039                                                         
         STC   RF,BYTE                                                          
                                                                                
         LA    R0,BYTE                                                          
         SR    R1,R1                                                            
         BAS   RE,D32TPAD                  ADD DATA TO DOWNLOAD                 
D32TP039 EQU   *                                                                
                                                                                
                                                                                
         DS    0H                  DAY/TIME NUMBER                              
         MVC   FALDMPC,=Y(FMDTPDPDTN)                                           
         BAS   RE,D32TPOKD                                                      
         BNE   D32TP049                                                         
*                                                                               
         ZICM  RF,TDRDAY,(1)               GET DAY/TIME NUMBER                  
         BZ    D32TP049                                                         
         STC   RF,BYTE                                                          
                                                                                
         LA    R0,BYTE                                                          
         SR    R1,R1                                                            
         BAS   RE,D32TPAD                  ADD DATA TO DOWNLOAD                 
D32TP049 EQU   *                                                                
                                                                                
                                                                                
         DS    0H                  STATION                                      
         MVC   FALDMPC,=Y(FMDTPDPSTTN)                                          
         BAS   RE,D32TPOKD                                                      
         BNE   D32TP059                                                         
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(5),STAS                                                     
         MVC   MYSTAS,STAS                                                      
* ATTACH THE  SYSCODE TO STATION FOR FUSION                                     
*                                                                               
*                                                                               
         DS    0H                    CHECK FOR ANY SPILL MARKET                 
         MVI   FLDH+5,0               (ASSUME THERE IS NONE)                    
         OC    MYSTAS+5(2),MYSTAS+5     VIA NUMERIC MKT CODE                    
         BNZ   D32TP050                                                         
         OC    ALPHAMKT,ALPHAMKT                                                
         BZ    D32TP051                                                         
         CLC   ALFMKTS,SPACES           VIA ALPHA MKTS                          
         BE    D32TP051                                                         
         B     D32TP50B                                                         
                                                                                
D32TP050 DS    0H                  NUMERIC CODE SPILL MARKET                    
         MVC   DUB(2),MYSTAS+5                                                  
         MVI   GOSUBN,TNMKT#                                                    
         GOTO1 AGOSUB                                                           
         LH    R1,HALF                                                          
         STC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EXMVC R1,FLD,WORK         FLD HAS MARKET                               
         B     D32TP051                                                         
*                                                                               
D32TP50B DS    0H                  ALPHA CODE SPILL MARKET                      
         MVC   FLD(L'ALPHAMKT),ALPHAMKT                                         
         LA    R1,L'ALPHAMKT                                                    
         LA    RF,FLD+L'ALPHAMKT                                                
         BCTR  RF,0                                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-10                                                          
         STC   R1,FLDH+5                                                        
         B     D32TP051                                                         
*                                                                               
D32TP051 DS    0H                                                               
*                                                                               
         DS    0H                                                               
         MVC   WORK(L'SPOVSTA),SPACES  BUILD STATON OUTPUT IN  WORK             
         LA    R4,WORK                                                          
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   0(4,R4),MYSTAS                                                   
         LA    R4,4(R4)                                                         
*  take out any trailing spaces and adjust length                               
         LR    RF,R4                                                            
         LA    R0,4                                                             
D32TP052 SHI   RF,1                GO BACKWARDS TO FIND LAST CHARACTER          
         CLI   0(RF),X'40'                                                      
         BNE   D32TP053                                                         
         BCT   R0,D32TP052                                                      
*                                                                               
D32TP053 DS    0H                                                               
         LA    R4,1(RF)            READJUST TO ONE PAST LAST CHAR               
*                                                                               
         CLI   DBMED,C'T'                                                       
         BE    D32TP054                                                         
         CLI   DBMED,C'W'                                                       
         BE    D32TP054                                                         
         CLI   DBMED,C'C'                                                       
         BE    D32TP054                                                         
         B     D32TP055                                                         
D32TP054 DS    0H                                                               
         CLI   MYSTAS+4,C'T'                                                    
         BNE   D32TP055                                                         
         B     D32TP056                                                         
D32TP055 DS    0H                                                               
         MVC   0(1,R4),MYSTAS+4                                                 
         LA    R4,1(R4)                                                         
D32TP056 EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                    CHECK FOR ANY SPILL MARKET                 
         ZICM  R1,FLDH+5,(1)                                                    
         BZ    D32TP058                                                         
         MVI   0(R4),C'/'                                                       
         BCTR  R1,0                                                             
         EXMVC R1,1(R4),FLD                                                     
         LA    R4,1+1(R1,R4)                                                    
********************   MOVE IN SYCODE TO THE END                                
         CLI   DBSRC,C'F'        FUSION                                         
         BNE   D32TP058                                                         
         OC    SYSCODES,SYSCODES                                                
         BZ    D32TP058                                                         
         LA    R1,SYSCODES                                                      
         MVI   0(R4),C'-'                                                       
         AHI   R4,1                                                             
         EDIT  (B2,0(R1)),(5,0(R4)),WRK=WORK2,ALIGN=LEFT                        
         AHI   R4,5                                                             
D32TP058 DS    0H                                                               
         LA    R0,WORK                                                          
         SR    R4,R0                                                            
         LR    R1,R4                                                            
********************                                                            
                                                                                
*                                                                               
         LA    R0,WORK                                                          
****     LA    R1,4                                                             
         BAS   RE,D32TPAD           ADD DATA TO DOWNLOAD                        
D32TP059 EQU   *                                                                
                                                                                
                                                                                
         DS    0H                  BOOK                                         
         MVC   FALDMPC,=Y(FMDTPDPBOOK)                                          
         BAS   RE,D32TPOKD                                                      
         BNE   D32TP069                                                         
*                                                                               
         LA    RF,DBSTABK                                                       
         USING STABKD,RF                                                        
         MVC   DUB(L'STABKS),STABKS    GET BOOK/WEEK#/BOOKTYPE                  
         DROP  RF                                                               
                                                                                
         MVI   GOSUBN,TBK#             TRANSLATE BOOK                           
         GOTO1 AGOSUB                                                           
*                                                                               
         LA    R0,WORK                                                          
         LH    R1,HALF                                                          
         BAS   RE,D32TPAD           ADD DATA TO DOWNLOAD                        
D32TP069 EQU   *                                                                
                                                                                
                                                                                
         DS    0H                  DAY/START TIME/END TIME                      
         MVC   TMPKSE,TDRKDAY                                                   
         MVI   GOSUBN,GDY#          GET DAY AND TIMES                           
         GOTO1 AGOSUB                                                           
*                                                                               
         DS    0H                                                               
         MVC   FALDMPC,=Y(FMDTPDPIDAY)                                          
         BAS   RE,D32TPOKD                                                      
         BNE   D32TP073                                                         
                                                                                
         LA    R0,TEMPIDAY                                                      
         SR    R1,R1                                                            
         BAS   RE,D32TPAD           ADD DATA TO DOWNLOAD                        
D32TP073 EQU   *                                                                
*                                                                               
         DS    0H                                                               
         MVC   FALDMPC,=Y(FMDTPDPSTIM)                                          
         BAS   RE,D32TPOKD                                                      
         BNE   D32TP076                                                         
                                                                                
*        LA    R0,TEMPSTIM                                                      
         LA    R0,TDRSETM                                                       
*        SR    R1,R1                                                            
         LA    R1,2                                                             
         BAS   RE,D32TPAD           ADD DATA TO DOWNLOAD                        
D32TP076 EQU   *                                                                
*                                                                               
         DS    0H                                                               
         MVC   FALDMPC,=Y(FMDTPDPETIM)                                          
         BAS   RE,D32TPOKD                                                      
         BNE   D32TP079                                                         
                                                                                
         LA    R0,TDRSETM+2                                                     
         LA    R1,2                                                             
         BAS   RE,D32TPAD           ADD DATA TO DOWNLOAD                        
D32TP079 EQU   *                                                                
                                                                                
                                                                                
         DS    0H                  PROGRAM NAME                                 
         MVC   FALDMPC,=Y(FMDTPDPPNAM)                                          
         BAS   RE,D32TPOKD                                                      
         BNE   D32TP089                                                         
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(16),TDRPROG                                                 
         LA    RE,WORK                                                          
         LA    RF,15(RE)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,RE                                                            
         LA    RF,1(RF)                                                         
         STH   RF,HALF                                                          
*                                                                               
         LA    R0,WORK                                                          
         LH    R1,HALF                                                          
         BAS   RE,D32TPAD           ADD DATA TO DOWNLOAD                        
D32TP089 EQU   *                                                                
* SEND DOWN AFFILIATION AND MARKET NAME                                         
         MVC   FALDMPC,=Y(FMDTPDPAFFL)                                          
         BAS   RE,D32TPOKD                                                      
         BNE   D32TP080                                                         
         MVC   WORK(16),TDRAFFL                                                 
         LA    R0,WORK                                                          
**       LH    R1,HALF                                                          
         LA    R1,3                                                             
         BAS   RE,D32TPAD                                                       
D32TP080 EQU   *                                                                
         MVI   GOSUBN,IDB#                                                      
         GOTO1 AGOSUB                                                           
         LA    R6,DBLOCK1                                                       
         USING DBLOCKD,R6                                                       
         MVI   DBFUNCT,DBGETMK     GET MARKET INFORMATION                       
         MVC   DBSELRMK,THISMNUM                                                
         XC    THISMKNM,THISMKNM                                                
         CLC   =C'N/A',THISMNUM                                                 
         BNE   *+14                                                             
         MVC   THISMKNM(3),=C'N/A'                                              
         B     D32TP081                                                         
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,DEMHOOK5,0,0                                
         DROP  R6                                                               
*                                                                               
D32TP081 EQU   *                                                                
         MVC   FALDMPC,=Y(FMDTPDPMNAM)                                          
         BAS   RE,D32TPOKD                                                      
         BNE   D32TP082                                                         
         MVC   WORK(16),THISMKNM                                                
         LA    R0,WORK                                                          
**       LH    R1,HALF                LEN OF FIELD                              
         LA    R1,L'THISMKNM                                                    
         BAS   RE,D32TPAD                                                       
D32TP082 EQU   *                                                                
                                                                                
                                                                                
         DS    0H                  DEMO VALUES                                  
         MVI   PRECFLAG,C'Y'       SET DEMO PREC FLAG TO YES                    
         MVI   DEMOPRE,0           SET DEFAULT PREC TO ZERO PLACES              
         MVC   FALDMPC,=Y(FMDTPDPDMVL)                                          
         BAS   RE,D32TPOKD                                                      
         BNE   D32TP099                                                         
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,1,STNDEMS         R6 = # OF DEMOS (ACTUALLY) RQUESTED         
         BZ    D32TP099                                                         
                                                                                
         LA    R2,WORK              R2-->OUTPUT AREA FOR DEMO VALUE             
         L     R3,ASTDMEXP                                                      
         LA    R3,1(R3)             R3-->DEMO EXPRESSIONS                       
         LA    R4,TDRDEMS           R4-->DEMO VALUES                            
*                                                                               
D32TP083 DS    0H                                                               
         CLI   PUFLLOO3,C'N'        RE-ESTABLISHED NTH DEMO YET?                
         BNE   D32TP083G             EITHER YES OR N/A                          
         L     RF,AAPSAV2                                                       
         CLM   R6,1,(AS2DEMON-APSAV2D)(RF)                                      
         BNE   D32TP095                                                         
         MVI   PUFLLOO3,C'Y'        RE-ESTABLISHED NTH DEMO,                    
         B     D32TP095              BUT DON'T ADD TO FALINK AGAIN              
D32TP083G EQU  *                                                                
*                                                                               
         MVC   WORK,SPACES          INITIALIZE OUTPUT AREA                      
         OC    0(L'TDRDEMS,R4),0(R4)   IF DEMO VALUE IS ZERO,                   
         BNZ   D32TP085                                                         
         CLC   BKS(2),INVBOOK           AND IT'S AN INVALID BOOK,               
         BNE   D32TP085                                                         
         LA    R1,1                     JUST PASS BACK DUMMY HEADER             
         B     D32TP088                                                         
                                                                                
D32TP085 DS    0H                                                               
         MVI   GOSUBN,GDM#          FORMAT DEMO VALUE                           
         GOTO1 AGOSUB                                                           
*                                                                               
D32TP088 DS    0H                   R1 = L(FORMATTED DEMO VALUE)                
*                                                                               
D32TP090 DS    0H                                                               
         LA    RF,DBSTABK           CHECK TO SEE IF VALID BOOK                  
         USING STABKD,RF                                                        
         MVC   DUB(L'STABKS),STABKS                                             
         DROP  RF                                                               
         MVC   FALDMPC,=Y(FMDTPDPDMVL)                                          
         CLC   DUB(2),INVBOOK                                                   
         BNE   *+10                                                             
         MVC   FALDMPC,=Y(FMDTPDPDUMV)                                          
         L     R1,FULL              PICK UP L(FORMATTED DEMO VALUE)             
         LA    R0,WORK              POINT IT TO DEMO VALUE FROM GETDEM          
         BAS   RE,D32TPAD           ADD DATA TO DOWNLOAD                        
*                                                                               
D32TP095 LA    R3,L'DEMS(R3)                                                    
         LA    R4,L'TDRDEMS(R4)                                                 
         BCT   R6,D32TP083                                                      
D32TP099 EQU   *                                                                
                                                                                
                                                                                
*                                                                               
D32TPX   DS    0H                                                               
         J     EXIT                                                             
         DROP  R5                                                               
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
         MVC   THISMKNM,WORK+2      MARKET NAME                                 
*                                                                               
DMHK5X   DS    0H                  RETURN TO DEMAND                             
         J     EXIT                                                             
         DROP  R6                                                               
                                                                                
                                                                                
* Little helper routine to see if data can be downloaded                        
* At entry,                                                                     
*   FALDMPC = map code of data to check                                         
                                                                                
D32TPOKD NTR1                                                                   
         MVI   GOSUBN,OKDL#                                                     
         GOTO1 AGOSUB                                                           
         J     EXIT                                                             
                                                                                
                                                                                
* Little helper routine to add data to FALINK buffer                            
* At entry,                                                                     
*   FALDMPC = map code of data to be added                                      
*   R0      = A(data to be added)                                               
                                                                                
D32TPAD  NTR1                                                                   
         ST    R0,ADLDATA          SET A(DATA TO DOWNLOAD)                      
         ST    R1,LDLDATA          SET L(DATA TO DOWNLOAD)                      
         MVI   ADMODE,ADMADD                                                    
         GOTO1 ADM32ADD                                                         
         JL    XITMODL                                                          
                                                                                
         TM    BRKFLAG1,BF1FALNK  DID FALINK CALL FOR A BREAK?                  
         BZ    D32TPADG            NOPE                                         
         L     RF,AAPSAV2          YES, NEED TO SAVE LOCAL VARIABLES            
         USING APSAV2D,RF                                                       
         CLC   FALDMPC,=Y(FMDTPDPDMVL)  IF ADDING DEMO DATA,                    
         BNE   *+8                                                              
         STCM  R6,1,AS2DEMON             SAVE NTH DEMO TO ADDDATA CALL          
         DROP  RF                                                               
         J     XITMOD              EXIT DEM03 NOW                               
D32TPADG EQU   *                                                                
*                                                                               
         J     EXIT                                                             
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR02--D32+        
               PV#)'                                                            
*--------------------- DEM32 DOWLOAD TIME PERIOD ---------------------*         
                                                                                
* At entry,                                                                     
*   R5-->TSAR demo record containing TP demo information                        
                                                                                
D32DLPAV DS    0H                                                               
         USING TSDEMRCD,R5                                                      
                                                                                
         LA    RF,DBSTABK             CHECK TO SEE IF VALID BOOK                
         USING STABKD,RF              IF NO VALID DONT DWNLOAD                  
         MVC   DUB(L'STABKS),STABKS   ANY DATA                                  
         DROP  RF                                                               
         CLC   DUB(2),INVBOOK                                                   
         BE    D32PVX                                                           
                                                                                
         DS    0H                  DO SETELEM FOR FALINK FIRST                  
         MVC   FALEMPC,=Y(FMHPVDP)                                              
         GOTO1 ADM32SET                                                         
                                                                                
                                                                                
         DS    0H                  "RECORD" TYPE                                
         MVC   FALDMPC,=Y(FMDPVDPRTYP)                                          
         BAS   RE,D32PVOKD                                                      
         BNE   D32PV029                                                         
*                                                                               
         LA    R0,=AL1(FMDPVDPRTYP_DE)   ASSUME DETAIL LINE                     
         CLI   TDRSQH,XFF                TEST IF SUMMARY LINE                   
         BNE   *+8                                                              
         LA    R0,=AL1(FMDPVDPRTYP_RO)    IF SO, INDICATE IT                    
         SR    R1,R1                                                            
         BAS   RE,D32PVAD                ADD DATA TO DOWNLOAD                   
D32PV029 EQU   *                                                                
                                                                                
                                                                                
         DS    0H                  STATION NUMBER                               
         MVC   FALDMPC,=Y(FMDPVDPSTNN)                                          
         BAS   RE,D32PVOKD                                                      
         BNE   D32PV034                                                         
*                                                                               
         ZICM  RF,TDRSTA,(1)               GET STATION NUMBER                   
         BZ    D32PV034                                                         
*        LH    RE,DSPRCTST                                                      
*        LA    RE,REQCTLTB(RE)                                                  
*        ZIC   R0,(RCTFROM-RCTDSECT)(RE)                                        
*        AR    RF,R0                       ADJUST STATION NUMBER                
*        BCTR  RF,0                                                             
         STC   RF,BYTE                                                          
                                                                                
         LA    R0,BYTE                                                          
         SR    R1,R1                                                            
         BAS   RE,D32PVAD                  ADD DATA TO DOWNLOAD                 
D32PV034 EQU   *                                                                
                                                                                
                                                                                
         DS    0H                  BOOK NUMBER                                  
         MVC   FALDMPC,=Y(FMDPVDPBKN)                                           
         BAS   RE,D32PVOKD                                                      
         BNE   D32PV039                                                         
*                                                                               
         ZICM  RF,TDRBK,(1)                GET BOOK NUMBER                      
         BZ    D32PV039                                                         
*        LH    RE,DSPRCTBK                                                      
*        LA    RE,REQCTLTB(RE)                                                  
*        ZIC   R0,(RCTFROM-RCTDSECT)(RE)                                        
*        AR    RF,R0                       ADJUST BOOK NUMBER                   
*        BCTR  RF,0                                                             
         STC   RF,BYTE                                                          
                                                                                
         LA    R0,BYTE                                                          
         SR    R1,R1                                                            
         BAS   RE,D32PVAD                  ADD DATA TO DOWNLOAD                 
D32PV039 EQU   *                                                                
                                                                                
                                                                                
         DS    0H                  DAY/TIME NUMBER                              
         MVC   FALDMPC,=Y(FMDPVDPDTN)                                           
         BAS   RE,D32PVOKD                                                      
         BNE   D32PV049                                                         
*                                                                               
         ZICM  RF,TDRDAY,(1)               GET DAY/TIME NUMBER                  
         BZ    D32PV049                                                         
*        LH    RE,DSPRCTDT                                                      
*        LA    RE,REQCTLTB(RE)                                                  
*        ZIC   R0,(RCTFROM-RCTDSECT)(RE)                                        
*        AR    RF,R0                       ADJUST DAY/TIME NUMBER               
*        BCTR  RF,0                                                             
         STC   RF,BYTE                                                          
                                                                                
         LA    R0,BYTE                                                          
         SR    R1,R1                                                            
         BAS   RE,D32PVAD                  ADD DATA TO DOWNLOAD                 
D32PV049 EQU   *                                                                
                                                                                
                                                                                
         DS    0H                  STATION                                      
         MVC   FALDMPC,=Y(FMDPVDPSTTN)                                          
         BAS   RE,D32PVOKD                                                      
         BNE   D32PV059                                                         
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(5),STAS                                                     
*                                                                               
         LA    R0,WORK                                                          
         LA    R1,4                                                             
         BAS   RE,D32PVAD           ADD DATA TO DOWNLOAD                        
D32PV059 EQU   *                                                                
                                                                                
                                                                                
         DS    0H                  BOOK                                         
         MVC   FALDMPC,=Y(FMDPVDPBOOK)                                          
         BAS   RE,D32PVOKD                                                      
         BNE   D32PV069                                                         
*                                                                               
         LA    RF,DBSTABK                                                       
         USING STABKD,RF                                                        
         MVC   DUB(L'STABKS),STABKS    GET BOOK/WEEK#/BOOKTYPE                  
         DROP  RF                                                               
                                                                                
         MVI   GOSUBN,TBK#             TRANSLATE BOOK                           
         GOTO1 AGOSUB                                                           
*                                                                               
         LA    R0,WORK                                                          
         LH    R1,HALF                                                          
         BAS   RE,D32PVAD           ADD DATA TO DOWNLOAD                        
D32PV069 EQU   *                                                                
                                                                                
                                                                                
         DS    0H                  DAY/START TIME/END TIME                      
         MVC   TMPKSE,TDRKDAY                                                   
         MVI   GOSUBN,GDY#          GET DAY AND TIMES                           
         GOTO1 AGOSUB                                                           
*                                                                               
         DS    0H                                                               
         MVC   FALDMPC,=Y(FMDPVDPIDAY)                                          
         BAS   RE,D32PVOKD                                                      
         BNE   D32PV073                                                         
                                                                                
         LA    R0,TEMPIDAY                                                      
         SR    R1,R1                                                            
         BAS   RE,D32PVAD           ADD DATA TO DOWNLOAD                        
D32PV073 EQU   *                                                                
*                                                                               
         DS    0H                                                               
         MVC   FALDMPC,=Y(FMDPVDPSTIM)                                          
         BAS   RE,D32PVOKD                                                      
         BNE   D32PV076                                                         
                                                                                
         LA    R0,TEMPSTIM                                                      
         SR    R1,R1                                                            
         BAS   RE,D32PVAD           ADD DATA TO DOWNLOAD                        
D32PV076 EQU   *                                                                
*                                                                               
         DS    0H                                                               
         MVC   FALDMPC,=Y(FMDPVDPETIM)                                          
         BAS   RE,D32PVOKD                                                      
         BNE   D32PV079                                                         
                                                                                
         LA    R0,TEMPETIM                                                      
         SR    R1,R1                                                            
         BAS   RE,D32PVAD           ADD DATA TO DOWNLOAD                        
D32PV079 EQU   *                                                                
                                                                                
                                                                                
         DS    0H                  ACTIVE WEEKS                                 
         MVC   FALDMPC,=Y(FMDPVDPWKS)                                           
         BAS   RE,D32PVOKD                                                      
         BNE   D32PV084                                                         
*                                                                               
         MVC   TMPWKS,TDRWKS                                                    
         MVI   GOSUBN,TWK#                                                      
         GOTO1 AGOSUB                                                           
*                                                                               
         LA    R0,WORK                                                          
         LH    R1,HALF                                                          
         BAS   RE,D32PVAD           ADD DATA TO DOWNLOAD                        
D32PV084 EQU   *                                                                
                                                                                
                                                                                
         DS    0H                  PROGRAM NAME                                 
         MVC   FALDMPC,=Y(FMDPVDPPNAM)                                          
         BAS   RE,D32PVOKD                                                      
         BNE   D32PV089                                                         
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(16),TDRPROG                                                 
         LA    RE,WORK                                                          
         LA    RF,15(RE)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,RE                                                            
         LA    RF,1(RF)                                                         
         STH   RF,HALF                                                          
*                                                                               
         LA    R0,WORK                                                          
         LH    R1,HALF                                                          
         BAS   RE,D32PVAD           ADD DATA TO DOWNLOAD                        
D32PV089 EQU   *                                                                
                                                                                
                                                                                
         DS    0H                  DEMO VALUES                                  
         MVC   FALDMPC,=Y(FMDPVDPDMVL)                                          
         BAS   RE,D32PVOKD                                                      
         BNE   D32PV099                                                         
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,1,STNDEMS         R6 = # OF DEMOS (ACTUALLY) RQUESTED         
         BZ    D32PV099                                                         
                                                                                
         LA    R2,WORK              R2-->OUTPUT AREA FOR DEMO VALUE             
         L     R3,ASTDMEXP                                                      
         LA    R3,1(R3)             R3-->DEMO EXPRESSIONS                       
         LA    R4,TDRDEMS           R4-->DEMO VALUES                            
         MVI   PRECFLAG,C'Y'       SET DEMO PREC FLAG TO YES                    
         MVI   DEMOPRE,0           SET DEFAULT PREC TO ZERO PLACES              
*                                                                               
D32PV083 DS    0H                                                               
         MVC   WORK,SPACES          INITIALIZE OUTPUT AREA                      
                                                                                
         OC    0(L'TDRDEMS,R4),0(R4)   IF DEMO VALUE IS ZERO,                   
         BNZ   D32PV085                                                         
         CLC   BKS(2),INVBOOK           AND IT'S AN INVALID BOOK,               
         BNE   D32PV085                                                         
         LA    R1,1                     JUST PASS BACK A BLANK                  
         B     D32PV088                                                         
                                                                                
D32PV085 DS    0H                                                               
         MVI   GOSUBN,GDM#         FORMAT DEMO VALUE                            
         GOTO1 AGOSUB                                                           
         B     D32PV088                                                         
*                                                                               
D32PV088 DS    0H                   R1 = L(FORMATTED DEMO VALUE)                
*                                                                               
D32PV090 MVC   FALDMPC,=Y(FMDTPDPDMVL)                                          
         L     R1,FULL              PICK UP L(FORMATTED DEMO VALUE)             
         LA    R0,WORK                                                          
         BAS   RE,D32PVAD                                                       
*                                                                               
D32PV095 LA    R3,L'DEMS(R3)                                                    
         LA    R4,L'TDRDEMS(R4)                                                 
         BCT   R6,D32PV083                                                      
D32PV099 EQU   *                                                                
                                                                                
                                                                                
         DS    0H                  PURE NUMBER                                  
         MVC   FALDMPC,=Y(FMDPVDPPURE)                                          
         BAS   RE,D32PVOKD                                                      
         BNE   D32PV104                                                         
*                                                                               
         MVC   TMPPURE,TDRPURE                                                  
         MVI   GOSUBN,TPUR#                                                     
         GOTO1 AGOSUB                                                           
*                                                                               
         LA    R0,WORK                                                          
         LH    R1,HALF                                                          
         BAS   RE,D32PVAD           ADD DATA TO DOWNLOAD                        
D32PV104 EQU   *                                                                
                                                                                
                                                                                
*                                                                               
D32PVX   DS    0H                                                               
         J     EXIT                                                             
         DROP  R5                                                               
                                                                                
                                                                                
* Little helper routine to see if data can be downloaded                        
* At entry,                                                                     
*   FALDMPC = map code of data to check                                         
                                                                                
D32PVOKD NTR1                                                                   
         MVI   GOSUBN,OKDL#                                                     
         GOTO1 AGOSUB                                                           
         J     EXIT                                                             
                                                                                
                                                                                
* Little helper routine to add data to FALINK buffer                            
* At entry,                                                                     
*   FALDMPC = map code of data to be added                                      
*   R0      = A(data to be added)                                               
                                                                                
D32PVAD  NTR1                                                                   
         ST    R0,ADLDATA          SET A(DATA TO DOWNLOAD)                      
         ST    R1,LDLDATA          SET L(DATA TO DOWNLOAD)                      
         MVI   ADMODE,ADMADD                                                    
         GOTO1 ADM32ADD                                                         
         J     EXIT                                                             
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR02--MIS+        
               C STUFF)'                                                        
*--------------------- SUBR02 MISCELLANEOUS STUFF --------------------*         
                                                                                
         GETEL2 R3,MYDATDSP,MYELCODE                                            
                                                                                
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR02--LTO+        
               R && CONSTANTS)'                                                 
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
SUBR02L  EQU   *-SUBR02                                                         
         DS    0CL(4096-SUBR02L+1)                                              
                                                                                
         DROP  R7,R8,R9,RB,RC                                                   
***********************************************************************         
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR03)'            
***********************************************************************         
*======================= SUBROUTINE POOL THREE =======================*         
                                                                                
* At entry,                                                                     
*   R9-->DEMWRKD,                                                               
*   R8-->TWA,                                                                   
*   R7-->DEMTMPD,                                                               
*   R1 = equated sub-routine number.                                            
                                                                                
SUBR03Q  EQU   (((*-DEM0C+4095)/4096)*4096)                                     
                                                                                
         ORG   DEM0C+SUBR03Q                                                    
SUBR03   NMOD1 0,**0C03**                                                       
         USING DEMWRKD,R9          R9=A(GLOBAL WORK AREA)                       
         USING DEMTWAD,R8          R8=A(TWA)                                    
         USING DEMTMPD,R7          R7=A(TEMP W/S)                               
                                                                                
         L     RC,ALOCWRK                                                       
         USING DM0CWRKD,RC                                                      
*                                                                               
         SH    R1,=Y(R02#)         SUBTRACT FOR SUB-RTN # 2                     
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R03_00(R1)                                                       
                                                                                
TBK#     EQU   (R03_01-*)/4+R02#+1 TRANSLATE BOOK                               
TWK#     EQU   (R03_02-*)/4+R02#+1 TRANSLATE WEEKS                              
TPUR#    EQU   (R03_03-*)/4+R02#+1 TRANSLATE PURE NUMBER                        
GDM#     EQU   (R03_04-*)/4+R02#+1 TRANSLATE PURE NUMBER                        
GADQT#   EQU   (R03_05-*)/4+R02#+1 GET A(DBDQD ENTRIES)                         
TNMKT#   EQU   (R03_06-*)/4+R02#+1 TRANSLATE NUMERIC TO ALPHA MKT               
IDB#     EQU   (R03_07-*)/4+R02#+1                                              
                                                                                
R03_00   DS    0H                                                               
R03_01   B     TRSLTBK             TRANSLATE BOOK                               
R03_02   B     TRSLTWKS            TRANSLATE WEEKS                              
R03_03   B     TRSLTPUR            TRANSLATE PURE NUMBER                        
R03_04   B     GETDEM              TRANSLATE PURE NUMBER                        
R03_05   B     GETADQTB            GET A(DBDQD ENTRIES)                         
R03_06   B     TRSLTNMK            TRANSLATE NUM TO ALPHA MKT                   
R03_07   B     INITDBLK            INIT DBLOCK                                  
R03#     EQU   (*-R03_00)/4+R02#                                                
         DC    H'0'                                                             
                                                                                
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR03--TBK+        
               #)'                                                              
*--------------------------- TRANSLATE BOOK --------------------------*         
                                                                                
* Translates book into a printable format                                       
* At entry,                                                                     
*   DUB+0(4) = book/week#/booktype                                              
* At exit,                                                                      
*   WORK     = formatted book                                                   
*   HALF     = length of formatted book                                         
                                                                                
TRSLTBK  DS    0H                                                               
         MVC   WORK,SPACES                                                      
         LA    R4,WORK                                                          
                                                                                
*                                                                               
         DS    0H                  FORMAT BOOK                                  
         CLC   DUB(2),INVBOOK       CHECK VALIDITY OF BOOK                      
         BE    TBK020                                                           
         CLC   DUB(2),ESTBOOK       CHECK IF ESTIMATE BOOK                      
         BE    TBK030                                                           
         CLC   DUB(2),MBKBOOK       CHECK IF MULTBOOK BOOK                      
         BE    TBK030                                                           
         B     TBK050                                                           
                                                                                
TBK020   DS    0H                  INVALID BOOK                                 
         MVC   DUB(L'BKS),BKS       STILL NEED TO OUTPUT ACT BK INPTTED         
         B     TBK050                                                           
                                                                                
*                                                                               
TBK030   DS    0H                  ESTIMATE BOOK                                
         TM    DEMFLAG1,DF1STERO                                                
         BZ    TBK039                                                           
*                                                                               
         DS    0H                   GET ACTUAL DATA OF NTH BOOK INPUT           
         LA    R1,TSARBLCK                                                      
         USING TSARD,R1                                                         
         SR    R5,R5                                                            
*****    ICM   R5,7,((TSAREC+1)-TSARD)                                          
         ICM   R5,7,TSAREC+1                                                    
         DROP  R1                                                               
         LA    R5,2(R5)                                                         
         USING TSDEMRCD,R5                                                      
                                                                                
         L     R1,ASTINCHK                                                      
         USING STICKEYD,R1                                                      
TBK032   CLI   STICKKNU,IKNBOO       LOCATE ENTRIES FOR BOOK INPUT              
         BH    TBK037                                                           
         BL    TBK034                                                           
         CLC   STICKNTH,TDRBK        FIND NTH INPUT OF BOOK                     
         BH    TBK037                                                           
         BE    TBK035                                                           
TBK034   ZIC   R0,STICLEN                                                       
         AR    R1,R0                                                            
         B     TBK032                                                           
TBK035   LA    RE,STICDATA         EXTRACT DATA                                 
TBK036   CLI   0(RE),STSPSOPI       UNTIL START-NEST INDICATOR REACHED          
         BE    TBK038               AND EXIT                                    
         MVC   0(1,R4),0(RE)                                                    
         LA    R4,1(R4)                                                         
         LA    RE,1(RE)                                                         
         B     TBK036                                                           
TBK037   LA    R4,1(R4)            ELSE, BUMP 1 FOR DUMMY DATA INPUT            
TBK038   EQU   *                                                                
         B     TBKX                                                             
         DROP  R1,R5                                                            
TBK039   EQU   *                                                                
                                                                                
*                                                                               
TBK050   DS    0H                                                               
         CLI   DBMED,C'W'                                                       
         BE    TBK060                                                           
         CLI   DBMED,C'O'                                                       
         BE    TBK060                                                           
*                                                                               
         DS    0H                                                               
         MVI   STARTDAY,0          USE DEFAULT START DAY IN NSIWEEK             
         CLI   DBMED,C'C'           CHECK FOR CANADIEN                          
         BNE   TBK053X                                                          
         CLI   DBSRC,C'N'            NIELSEN                                    
         BNE   TBK053X                                                          
         CLC   DBFIL,=C'TP '         TIME PERIOD                                
         BNE   TBK053X                                                          
         CLC   DUB(2),=X'6001'       ON OR AFTER JAN/96                         
         BL    TBK053X                                                          
* NEW CODE WTP ASSUME WEEKLYTP ASSUME MONTHLY                                   
         L     RE,ADEMFIL                                                       
         CLC   =C'WTP',8(RE)                                                    
         BNE   TBK054X                                                          
*                                                                               
         MVI   STARTDAY,1           OVERRIDE START DAY TO MONDAY                
         B     TBK060                                                           
TBK053X  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   STARTDAY,0          USE DEFAULT START DAY IN NSIWEEK             
         CLI   DBMED,C'C'           CHECK FOR CANADIEN                          
         BNE   TBK054X                                                          
         CLI   DBSRC,C'A'            BBM                                        
         BNE   TBK054X                                                          
         CLI   DUB+3,C'W'            BOOKTYPE "W"                               
         BNE   TBK054X                                                          
         CLC   DBFIL,=C'TP '         TIME PERIOD                                
         BNE   TBK054X                                                          
         CLC   DUB(2),=X'6301'       ON OR AFTER JAN99                          
         BL    TBK054X                                                          
* NEW CODE WTP ASSUME WEEKLY TP ASSUME MONTHLY                                  
         L     RE,ADEMFIL                                                       
         CLC   =C'WTP',8(RE)                                                    
         BNE   TBK054X                                                          
         MVI   STARTDAY,1           OVERRIDE START DAY TO MONDAY                
         B     TBK060                                                           
*                                                                               
TBK054X  DS    0H                                                               
         B     TBK080              GO FORMAT AS MONTHLY BOOK                    
                                                                                
*                                                                               
TBK060   DS    0H                  FORMAT WEEKLY BOOK                           
         CLI   DBMED,C'O'          OVERNIGHTS START ON MONDAY                   
         BNE   *+8                                                              
         MVI   STARTDAY,1           OVERRIDE START DAY TO MONDAY                
         GOTO1 VNSIWEEK,DMCB,(C'D',DUB),(STARTDAY,VGETDAY),VADDAY,     +        
               VDATCON                                                          
         ZICM  R1,DMCB+1,(7)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DUB2(6),0(R1)                                                    
         GOTO1 VDATCON,DMCB,(X'80',DUB2),(5,(R4))                               
         ZIC   R0,DMCB+4                                                        
         AR    R4,R0                                                            
         B     TBK100                                                           
                                                                                
*                                                                               
TBK080   DS    0H                  FORMAT MONTHLY BOOK                          
         CLC   BKS(L'LATBOOK),LATBOOK  CHECK IF LATEST BOOK REQUESTED           
         BE    TBK085                                                           
         CLI   DUB,XFF              CHECK IF LATEST N BOOKS REQUESTED           
         BE    TBK085                                                           
         B     TBK093                                                           
*                                                                               
TBK085   DS    0H                                                               
         MVC   0(6,R4),=C'LATEST'    YES, FORMAT AS LATEST N BOOKS              
         LA    R4,6(R4)                                                         
                                                                                
         CLC   BKS(L'LATBOOK),LATBOOK                                           
         BNE   *+22                                                             
         MVC   0(3,R4),SPACES        A SPACE BEFORE AND AFTER                   
         MVI   1(R4),C'-'             THIS DASH                                 
         LA    R4,3(R4)                                                         
         B     TBK088                                                           
                                                                                
         CLI   DUB,XFF               LATEST N                                   
         BNE   *+18                                                             
         MVC   1(1,R4),DUB+1                                                    
         LA    R4,2(R4)                                                         
         B     TBK088                                                           
                                                                                
         DC    H'0'                                                             
                                                                                
TBK088   DS    0H                                                               
         CLC   BKS(L'LATBOOK),LATBOOK                                           
         BE    TBK093                                                           
         B     TBK100                                                           
*                                                                               
TBK093   DS    0H                                                               
         GOTO1 VDATCON,DMCB,(X'83',DUB),(6,(R4))                                
         CLI   3(R4),C'/'          IF OUTPUT IS MMM/YY,                         
         BNE   *+12                                                             
         MVC   3(2,R4),4(R4)        THEN REMOVE THE '/'                         
         BCTR  R4,0                                                             
         ZIC   R0,DMCB+4                                                        
         AR    R4,R0                                                            
         MVI   0(R4),C' '           AND BLANK OUT "GARBAGE"                     
         B     TBK100                                                           
                                                                                
*                                                                               
TBK100   DS    0H                  DO BOOK TYPE                                 
         CLI   DBMED,C'C'                                                       
         BNE   TBK101                                                           
         CLI   DBSRC,C'A'                                                       
         BNE   TBK101               DONT PRINT OUT THE BOOKTYPE W               
         L     RF,ADEMFIL           FOR WTP  (BBM)                              
         CLC   =C'WTP',8(RF)                                                    
         BNE   TBK101                                                           
         CLI   DUB+3,C'W'                                                       
         BE    TBK109                                                           
TBK101   DS    0H                                                               
*                                                                               
         CLI   DUB+3,0              ANY BOOK TYPE?                              
         BE    TBK109                NOPE                                       
         MVI   0(R4),C'('                                                       
*&&DO                                                                           
         MVC   1(1,R4),DUB+3                                                    
         CLI   DBMED,C'T'                                                       
         BNE   TBK104                                                           
         CLI   1(R4),X'81'                                                      
         BL    *+16                                                             
         CLI   1(R4),X'B9'                                                      
         BH    *+8                                                              
         OI    1(R4),X'40'         CAPITALIZE                                   
*&&                                                                             
* DISPLAY 2 CHARACTER BOOKTYPE                                                  
         XC    DMCB2,DMCB2                                                      
         MVI   DMCB2,12                                                         
         MVC   DMCB2+1(1),DUB+3                                                 
         GOTO1 ATRANSBT                                                         
         MVC   1(2,R4),=C'??'                                                   
         CLI   DMCB2+4,X'FF'                                                    
         BE    *+10                                                             
         MVC   1(2,R4),DMCB2+4                                                  
*                                                                               
         CLI   DMCB2+5,0           1 CHARACTER OR 2 CHARACTER BOOKTYPE?         
         BE    TBK104                                                           
         CLI   DMCB2+5,X'40'       1 CHARACTER OR 2 CHARACTER BOOKTYPE?         
         BE    TBK104                                                           
*                                                                               
         MVI   3(R4),C')'          2 CHARACTER BOOKTYPE                         
         AHI   R4,4                ADJUST FOR L(BOOKTYPE)                       
         B     TBK109                                                           
*                                                                               
TBK104   DS    0H                                                               
         MVI   2(R4),C')'                                                       
         LA    R4,3(R4)                                                         
TBK109   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  DO WEEK NUMBER                               
         CLI   DUB+2,0              ANY WEEK NUMBER                             
         BE    TBK119                NOPE                                       
         MVC   2(4,R4),=C'WEEK'                                                 
         MVC   6+1(1,R4),DUB+2                                                  
         OI    6+1(R4),X'F0'                                                    
         LA    R4,8(R4)                                                         
TBK119   EQU   *                                                                
                                                                                
*                                                                               
TBKX     DS    0H                                                               
         LA    R0,WORK                                                          
         SR    R4,R0                                                            
         STH   R4,HALF             SET LENGTH INTO HALF                         
*                                                                               
         J     EXIT                                                             
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR03--TWK+        
               #)'                                                              
*-------------------------- TRANSLATE WEEKS --------------------------*         
                                                                                
* Translates active week bits into printable format                             
* At entry,                                                                     
*   TMPWKS   = active week bits                                                 
* At exit,                                                                      
*   WORK     = formatted active weeks                                           
*   HALF     = length of formatted active weeks                                 
                                                                                
TRSLTWKS DS    0H                                                               
         MVC   WORK,SPACES                                                      
         LA    R4,WORK                                                          
                                                                                
*                                                                               
         DS    0H                  FORMAT BOOK                                  
         LA    RE,B'00001000'      TRANSLATE BITS (STARTING W/ X'08')           
         LA    RF,C'1'              INTO EBCDIC DIGITS                          
TWK012   EX    RE,*+8              JUST PASS THOSE WEEK NOS. OF                 
         B     *+8                  THOSE WEEKS THAT WERE ACTIVE                
         TM    TMPWKS,0             (eg. X'0B' ==> C'134' )                     
         BZ    *+12                                                             
         STC   RF,0(R4)                                                         
         LA    R4,1(R4)                                                         
         LA    RF,1(RF)                                                         
         SRL   RE,1                                                             
         OR    RE,RE                                                            
         BNZ   TWK012                                                           
                                                                                
*                                                                               
TWKX     DS    0H                                                               
         LA    R0,WORK                                                          
         SR    R4,R0                                                            
         STH   R4,HALF             SET LENGTH INTO HALF                         
*                                                                               
         J     EXIT                                                             
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR03--TPU+        
               R#)'                                                             
*----------------------- TRANSLATE PURE NUMBER -----------------------*         
                                                                                
* At entry,                                                                     
*   TMPPURE = pure number to translate.                                         
* At exit,                                                                      
*   WORK    = printable pure number.                                            
*   HALF    = L(printable pure number).                                         
                                                                                
TRSLTPUR DS    0H                                                               
         LA    R5,DBLOCK1                                                       
         USING DBLOCKD,R5                                                       
         LR    R0,R5                 CLEAR DBLOCK                               
         LA    R1,DBLOCK1X-DBLOCK1                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   DBFILE,=C'PAV'        SET UP DBLOCK : FILE (PAV<==>PURE)         
         L     RF,AIOAREA1                                                      
         ST    RF,DBAREC             SET UP DBLOCK : A(RECORD)                  
         USING PRKEY,RF                                                         
         XC    PRKEY(24),PRKEY                                                  
         LA    R0,PRFRSTEL                                                      
         ST    R0,DBAQUART           SET UP DBLOCK : A(1ST ELEMENT)             
         MVC   PRKMINOR,TMPPURE      SET PURE NUMBER IN PAV RECORD              
         DROP  RF                                                               
*                                                                               
         MVC   WORK2,SPACES                                                     
         GOTO1 VDEFINE,DMCB,=C'PURE',DBLOCKD,WORK2                              
         MVC   WORK(4),WORK2+3                                                  
         DROP  R5                                                               
*                                                                               
         LA    R4,WORK+3                                                        
         CLI   0(R4),C' '          POINT R4 TO LAST NON-SPACE                   
         BH    *+8                                                              
         BCT   R4,*-8                                                           
*                                                                               
         B     TPURX                                                            
                                                                                
*                                                                               
TPURX    DS    0H                                                               
         LA    R0,WORK                                                          
         SR    R4,R0                                                            
         LA    R4,1(R4)                                                         
         STH   R4,HALF             SET LENGTH INTO HALF                         
         J     EXIT                                                             
**                                                                              
*                                                                               
*----------------------------- GET DEMOS -----------------------------*         
                                                                                
* ROUTINE TO EDIT A DEMO VALUE                                                  
* ON ENTRY R3=A(3 BYTE DEMO EXPRESSION)                                         
*          R4=A(3 BYTE DEMO VALUE)                                              
*          R2=A(7 BYTE OUTPUT DEMO VALUE)                                       
*                                                                               
GETDEM   DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R0,7,0(R4)          R0=DEMO VALUE                                
         CLI   OPTDTYP,C'S'        TEST TO OUTPUT SVI INDICES                   
         BE    GETDEM5             YES-EDIT INTEGER VALUE                       
         MVC   DUB(1),DBFIL                                                     
         MVC   DUB+1(1),1(R3)                                                   
         MVI   DUB+2,0                                                          
         L     R1,AEDITTAB         R1=A(EDIT TABLE)                             
         CLI   DBMED,C'W'          WEEKLIES DONT HAVE DEC OPTION FOR            
         BE    GETDEM2             ALWAYS GIVE BACK 1                           
*                                  SEARCH TABLE FOR DEMO                        
         CLI   OPTDEC,C'0'                                                      
         BL    GETDEM2                                                          
         CLI   OPTDEC,C'2'                                                      
         BNE   GETDEM1                                                          
         L     R1,AEDITTA3                                                      
         CLI   DBMED,C'C'          CANADA?                                      
         JNE   GETDEM1                                                          
         CLI   DBSRC,C'A'                                                       
         JNE   *+8                                                              
         L     R1,AEDITTA4                                                      
*                                                                               
GETDEM1  CLI   OPTDEC,C'0'                                                      
         BNE   *+8                                                              
         L     R1,AEDITTA2                                                      
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
                                                                                
         TM    DEMFLAG1,DF1STERO   IS THIS A STEREO SESSION?                    
         BO    GETDEM9              YES, USE ANOTHER EDIT FORMAT                
                                                                                
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
*                                                                               
GETDEM9  DS    0H                  EDITTING DEMOS FOR STEREO                    
         TM    DEMFLAG1,DF1STERO+DF1DEM32   DEM32 ?                             
         BO    GETDEM10                                                         
         TM    DUB+2,X'02'          TEST EDIT TO 2 DECIMALS                     
         BO    GETDEM9B                                                         
         TM    DUB+2,X'01'          TEST EDIT TO 1 DECIMAL                      
         BO    GETDEM9C                                                         
*                                                                               
GETDEM9A EDIT  (R0),(7,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                           
         B     GETDEM9X                                                         
GETDEM9B EDIT  (R0),(7,0(R2)),2,ALIGN=LEFT,ZERO=NOBLANK                         
         B     GETDEM9X                                                         
GETDEM9C EDIT  (R0),(7,0(R2)),1,ALIGN=LEFT,ZERO=NOBLANK                         
         B     GETDEM9X                                                         
GETDEM9X ST    R0,FULL                                                          
         B     GETDEMX                                                          
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
         MVI   DEMOPRE,2                                                        
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
**                                                                              
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR03--GAD+        
               QT#)'                                                            
*------------------------ GET A(DAY/QH TABLE) ------------------------*         
                                                                                
* At entry,                                                                     
*   ADBLOCK  = A(DBLOCK)                                                        
* At exit,                                                                      
*   ADBDQD   = A(Internal day/qhr table)                                        
                                                                                
GETADQTB DS    0H                                                               
         L     R5,ADBLOCK                                                       
         USING DBLOCKD,R5                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         LA    R3,DBDQD             PICK UP ADDR DIRECTLY FROM HERE             
         CLC   =AL2(DBDQUXTD),DBDQTAB+0   DO WE NEED TO LOOK IN XTND?           
         BNE   GADQT12X                    NOPE                                 
*                                                                               
         DS    0H                                                               
         LA    RF,DBEXTEND-4                                                    
GADQT12G DS    0H                   LOOK THROUGH DBEXTEND                       
         ICM   RF,15,4(RF)                                                      
         BZ    GADQT12X                                                         
         CLC   0(4,RF),DBDQTAB+2     FOR NAME GIVEN HERE                        
         BNE   GADQT12G                                                         
         LA    R3,DBDQXFXL(RF)                                                  
GADQT12X EQU   *                                                                
*                                                                               
         DS    0H                                                               
         ST    R3,ADBDQD                                                        
         DROP  R5                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         J     EXIT                                                             
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR03--MIS+        
               C STUFF)'                                                        
*                                                                               
*---------------------- TRANSLATE NUMERIC MARKET ---------------------*         
                                                                                
* Translates numeric market into an alpha market.  If no alpha market           
*  found, routine passes back the numeric market in printable format.           
* At entry,                                                                     
*   DUB(2)   = numeric market                                                   
* At exit,                                                                      
*   WORK     = alpha or nuermice market code                                    
*   HALF     = length of alpha or numeric market code                           
                                                                                
TRSLTNMK DS    0H                                                               
         MVC   WORK,SPACES                                                      
         XC    HALF,HALF                                                        
                                                                                
*                                                                               
         DS    0H                  ASSUME NO ALPHA MKT                          
         ZICM  R1,DUB,(3)                                                       
         LR    RE,R1                                                            
*        EDIT  (R1),(3,WORK),ALIGN=LEFT,WRK=WORK2                               
         EDIT  (R1),(4,WORK),ALIGN=LEFT,WRK=WORK2                               
         STH   R0,HALF                                                          
         STCM  RE,3,DUB            RESTORE DUB...EDIT MACRO DESTROYS IT         
                                                                                
*                                                                               
         DS    0H                                                               
         LA    R5,DBLOCK1                                                       
         USING DBLOCKD,R5          R5=A(DBLOCK)                                 
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,DBFIL                                                     
         MVC   DBAREC,AIOAREA2                                                  
         MVI   DBFUNCT,DBCNVN2A                                                 
         MVC   DBCOMFCS,AFAC                                                    
         MVC   DBBTYPE,BKS+3                                                    
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELBK,BKS                                                      
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         MVC   DBSELRMK,DUB                                                     
                                                                                
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
         CLI   DBERROR,0                                                        
         BNE   TNMKTX                                                           
*                                                                               
         OC    DBSELALF,DBSELALF                                                
         BZ    TNMKTX                                                           
         OC    DBSELALF,SPACES                                                  
         BE    TNMKTX                                                           
*                                                                               
         LA    R1,DBSELALF+L'DBSELALF-1                                         
         CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         B     *-10                                                             
         LA    R0,DBSELALF                                                      
         SR    R1,R0                                                            
         EXMVC R1,WORK,DBSELALF                                                 
         LA    R1,1(R1)                                                         
         STH   R1,HALF                                                          
         DROP  R5                                                               
                                                                                
*                                                                               
TNMKTX   DS    0H                                                               
         J     EXIT                                                             
INITDBLK DS    0H                                                               
         LA    R1,DBLOCK1                                                       
         USING DBLOCKD,R1                                                       
                                                                                
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,AIOAREA1     I/O AREA                                     
         MVC   DBCOMFCS,AFAC       COMFACS                                      
         MVC   DBFILE,=C'TP '      DEFAULT TO TIME PERIOD FILE                  
         MVI   DBTAPEP,C'Y'        FORCE IMPRESSION BASED                       
         MVC   DBSELMED,DBMED      MEDIA                                        
         MVC   DBSELSRC,DBSRC      SOURCE                                       
         MVC   DBSELAGY,AGYALPH    AGENCY ALPHA                                 
**       MVC   DBSELBK,BKS         BOOK                                         
         LA    RF,DBSTABK                                                       
         USING STABKD,RF                                                        
         MVC   DBSELBK,STABKS                                                   
         J     EXIT                                                             
                                                                                
         DROP  R1                                                               
*--------------------- SUBR03 MISCELLANEOUS STUFF --------------------*         
                                                                                
                                                                                
         TITLE 'DEDEM0C - TP && PAV DEMO LOOK-UPS/UPGRADES (SUBR03--LTO+        
               R && CONSTANTS)'                                                 
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
SUBR03L  EQU   *-SUBR03                                                         
         DS    0CL(X'1000'-SUBR03L+1)                                           
                                                                                
         DROP  R7,R8,R9,RB,RC                                                   
***********************************************************************         
         TITLE 'DEDEM0C - $DEM LILO REPORT (EQUATES)'                           
***********************************************************************         
*========================== T21B0C's EQUATES =========================*         
                                                                                
LOCWORKL EQU   IUNWRKL+DM0CWRKL                                                 
***********************************************************************         
         TITLE 'DEDEM0C - $DEM T/P && PAV DEMO LOOK-UPS/UPGRADES (TABLE+        
               S)'                                                              
***********************************************************************         
*=============================== TABLES ==============================*         
                                                                                
*                                  TABLE OF DISPLACEMENTS                       
DISPTAB  DS    0AL(2+2)                                                         
         DC    AL2(GOSUB-DEM0C),AL2(AGOSUB-DEMTMPD)                             
         DC    AL2(SUBR01-DEM0C),AL2(ASUBR01-DEMTMPD)                           
         DC    AL2(SUBR02-DEM0C),AL2(ASUBR02-DEMTMPD)                           
         DC    AL2(SUBR03-DEM0C),AL2(ASUBR03-DEMTMPD)                           
         DC    AL2(EDITTAB-DEM0C),AL2(AEDITTAB-DEMTMPD)                         
         DC    AL2(EDITTAB2-DEM0C),AL2(AEDITTA2-DEMTMPD)                        
         DC    AL2(EDITTAB3-DEM0C),AL2(AEDITTA3-DEMTMPD)                        
         DC    AL2(EDITTAB4-DEM0C),AL2(AEDITTA4-DEMTMPD)                        
         DC    AL2(KDAYTAB-DEM0C),AL2(AKDAYTAB-DEMTMPD)                         
         DC    AL2(IDAYTAB-DEM0C),AL2(AIDAYTAB-DEMTMPD)                         
         DC    AL2(VUTLIST-DEM0C),AL2(AVUTLIST-DEMTMPD)                         
         DC    AL2(PUTLIST-DEM0C),AL2(APUTLIST-DEMTMPD)                         
         DC    AL2(SHRLIST-DEM0C),AL2(ASHRLIST-DEMTMPD)                         
         DC    AL2(NUMWKTAB-DEM0C),AL2(ANUMWKTB-DEMTMPD)                        
         DC    AL2(FRSTWKTB-DEM0C),AL2(A1STWKTB-DEMTMPD)                        
         DC    AL2(PROFTAB-DEM0C),AL2(APROFTAB-DEMTMPD)                         
         DC    AL2(ROTQHTAB-DEM0C),AL2(AROTQHTB-DEMTMPD)                        
         DC    AL2(DLDATTAB-DEM0C),AL2(ADLDTABS-DEMTMPD)                        
DISPTABQ EQU   (*-DISPTAB)/(L'DISPTAB)                                          
                                                                                
*                                  TABLE FOR DEMO EDITTING ROUTINE              
EDITTAB  DS    0XL3                FILE/DEMO MODIFIER/EDIT RULES                
         DC    C'TC',X'01'                                                      
         DC    C'TI',X'01'                                                      
         DC    C'TR',X'01'                                                      
         DC    C'TP',X'01'                                                      
         DC    C'TS',X'01'                                                      
         DC    C'TQ',X'01'                                                      
         DC    C'TX',X'01'                                                      
         DC    C'TD',X'01'                                                      
         DC    C'TE',X'01'                                                      
         DC    C'PI',X'01'                                                      
         DC    C'PT',X'01'         (IS ACTUALLY TSA IMP, NOT TSA SHR)           
         DC    C'PR',X'01'                                                      
         DC    C'PP',X'01'                                                      
         DC    C'PS',X'01'                                                      
         DC    C'PQ',X'01'                                                      
         DC    C'PX',X'01'                                                      
         DC    C'PD',X'01'                                                      
         DC    AL1(EOT)                                                         
*                                  TABLE FOR DEMO EDITTING ROUTINE              
EDITTAB2 DS    0XL3                FILE/DEMO MODIFIER/EDIT RULES                
         DC    C'TC',X'00'                                                      
         DC    C'TI',X'00'         THIS TABLE IS FOR DEC=0 CARD                 
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
*                                  TABLE FOR DEMO EDITTING ROUTINE              
EDITTAB3 DS    0XL3                FILE/DEMO MODIFIER/EDIT RULES                
         DC    C'TC',X'01'                                                      
         DC    C'TI',X'02'         THIS TABLE IS FOR DEC=2 CARD                 
         DC    C'TR',X'02'                                                      
         DC    C'TP',X'01'                                                      
         DC    C'TS',X'01'                                                      
         DC    C'TQ',X'01'                                                      
         DC    C'TX',X'01'                                                      
         DC    C'TE',X'02'                                                      
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
* FOR CANADA                       TABLE FOR DEMO EDITTING ROUTINE              
EDITTAB4 DS    0XL3                FILE/DEMO MODIFIER/EDIT RULES                
         DC    C'TC',X'01'                                                      
         DC    C'TI',X'01'         THIS TABLE IS FOR DEC=2 CARD                 
         DC    C'TR',X'02'                                                      
         DC    C'TP',X'01'                                                      
         DC    C'TS',X'01'                                                      
         DC    C'TQ',X'01'                                                      
         DC    C'TX',X'01'                                                      
         DC    C'TE',X'02'                                                      
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
STAWIDTH EQU   64                  TOTAL WIDTH OF STATION HEADINGS              
DEMWIDTH EQU   8                   WIDTH FOR ONE DEMO ENTRY                     
         SPACE 1                                                                
*                                  TABLE TO CONVERT KEY DAY VALUES              
KDAYTAB  DS    0XL6                                                             
         DC    X'0',X'10',C'MON',X'40'                                          
         DC    X'0',X'20',C'TUE',X'20'                                          
         DC    X'0',X'30',C'WED',X'10'                                          
         DC    X'0',X'40',C'THU',X'08'                                          
         DC    X'0',X'50',C'FRI',X'04'                                          
         DC    X'0',X'60',C'SAT',X'02'                                          
         DC    X'0',X'70',C'SUN',X'01'                                          
         DC    C'T',X'95',C'M-F',X'7C'                                          
         DC    C'T',X'FF',C'VAR',X'00'                                          
         DC    C'P',X'00',C'M-F',X'7C'                                          
         DC    C'P',X'80',C'M-S',X'7F'                                          
         DC    C'P',X'FF',C'VAR',X'00'                                          
         DC    X'FF',X'FF',C'???',X'00'                                         
         SPACE 1                                                                
*                                  TABLE TO CONVERT INPUT DAY VALUES            
IDAYTAB  DS    0XL4                                                             
         DC    X'40',C'MON'                                                     
         DC    X'20',C'TUE'                                                     
         DC    X'10',C'WED'                                                     
         DC    X'08',C'THU'                                                     
         DC    X'04',C'FRI'                                                     
         DC    X'02',C'SAT'                                                     
         DC    X'01',C'SUN'                                                     
         DC    X'FF'                                                            
*                                                                               
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
*                                                                               
NUMWKTAB DC    AL1(0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4)                             
FRSTWKTB DC    AL1(0,4,3,3,2,2,2,2,1,1,1,1,1,1,1,1)                             
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
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*                                  TABLE OF START/END QHs FOR ROTATIONS         
         DS    0F                                                               
ROTQHTAB DS    0XL(1+1+2+2+2+2)                                                 
         DC     AL1(FMDTPDPRTYP_RO),AL1(0)    ROTATION                          
         DC      Y(SVSEQHS-DEMTMPD),Y(00)                                       
         DC      Y(SVSEQHE-DEMTMPD),Y(+1)                                       
         DC     AL1(FMDTPDPRTYP_LI),AL1(0)    LEAD-IN                           
         DC      Y(SVSEQHS-DEMTMPD),Y(-1)                                       
         DC      Y(SVSEQHS-DEMTMPD),Y(00)                                       
         DC     AL1(FMDTPDPRTYP_LO),AL1(0)    LEAD-OUT                          
         DC      Y(SVSEQHE-DEMTMPD),Y(+1)                                       
         DC      Y(SVSEQHE-DEMTMPD),Y(+2)                                       
         DC     AL1(FMDTPDPRTYP_RP),AL1(0)    ROTATION PLUS                     
         DC      Y(SVSEQHS-DEMTMPD),Y(-1)                                       
         DC      Y(SVSEQHE-DEMTMPD),Y(+2)                                       
         DC     AL1(FMDTPDPRTYP_IB),AL1(0)    IN-BREAK                          
         DC      Y(SVSEQHS-DEMTMPD),Y(-1)                                       
         DC      Y(SVSEQHS-DEMTMPD),Y(+1)                                       
         DC     AL1(FMDTPDPRTYP_OB),AL1(0)    OUT-BREAK                         
         DC      Y(SVSEQHE-DEMTMPD),Y(00)                                       
         DC      Y(SVSEQHE-DEMTMPD),Y(+2)                                       
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*======================== DOWNLOAD DATA TABLES =======================*         
                                                                                
DLDATTAB    DS  0D                                                              
                                                                                
                                                                                
*---------------------------- NSI TP & T4 ----------------------------*         
                                                                                
TNT098175A  DS  0X                                                              
            DC   AL1(TNT098175AX-TNT098175A)                                    
            DC   C'TP NT'                                                       
            DC   AL1(TDRRTDMV)                                                  
            DC   AL1(00,98,06,24)                                               
TNT098175AA DS   0X                                                             
            DC    AL1(TNT098175AAX-TNT098175AA),AL2(FMHTPDP)                    
            DC    AL2(FMDTPDPRTYP)                                              
            DC    AL2(FMDTPDPSTNN)                                              
            DC    AL2(FMDTPDPBKN)                                               
            DC    AL2(FMDTPDPSBKN)                                              
            DC    AL2(FMDTPDPDTN)                                               
            DC    AL2(FMDTPDPCMPN)                                              
            DC    AL2(FMDTPDPSTTN)                                              
            DC    AL2(FMDTPDPBOOK)                                              
            DC    AL2(FMDTPDPSUBK)                                              
            DC    AL2(FMDTPDPIDAY)                                              
            DC    AL2(FMDTPDPSTIM)                                              
            DC    AL2(FMDTPDPETIM)                                              
            DC    AL2(FMDTPDPWKS)                                               
            DC    AL2(FMDTPDPPNAM)                                              
            DC    AL2(FMDTPDPDMVL)                                              
            DC    AL2(FMDTPDPDUMV)                                              
            DC    AL2(FMDTPDPPREC)                                              
            DC    AL2(FMDTPDPAFFL)                                              
            DC    AL2(FMDTPDPMNAM)                                              
            DC    AL2(0)                                                        
TNT098175AAX EQU *                                                              
            DC    AL1(0)                                                        
TNT098175AX EQU *                                                               
                                                                                
                                                                                
TNT098175B  DS  0X                                                              
            DC   AL1(TNT098175BX-TNT098175B)                                    
            DC   C'TP NT'                                                       
            DC   AL1(TDRRTDML)                                                  
            DC   AL1(00,98,06,24)                                               
TNT098175BA DS   0X                                                             
            DC    AL1(TNT098175BAX-TNT098175BA),AL2(FMHDMLS)                    
            DC    AL2(FMDDMLSRCAT)                                              
            DC    AL2(FMDDMLSNCAT)                                              
            DC    AL2(FMDDMLSFDNM)                                              
            DC    AL2(0)                                                        
TNT098175BAX EQU *                                                              
            DC    AL1(0)                                                        
TNT098175BX EQU *                                                               
         EJECT                                                                  
*------------------------------ NHTI TP ------------------------------*         
                                                                                
THN098175A  DS  0X                                                              
            DC   AL1(THN098175AX-THN098175A)                                    
            DC   C'TP HN'                                                       
            DC   AL1(TDRRTDMV)                                                  
            DC   AL1(00,98,06,24)                                               
THN098175AA DS   0X                                                             
            DC    AL1(THN098175AAX-THN098175AA),AL2(FMHTPDP)                    
            DC    AL2(FMDTPDPRTYP)                                              
            DC    AL2(FMDTPDPSTNN)                                              
            DC    AL2(FMDTPDPBKN)                                               
            DC    AL2(FMDTPDPSBKN)                                              
            DC    AL2(FMDTPDPDTN)                                               
            DC    AL2(FMDTPDPCMPN)                                              
            DC    AL2(FMDTPDPSTTN)                                              
            DC    AL2(FMDTPDPBOOK)                                              
            DC    AL2(FMDTPDPSUBK)                                              
            DC    AL2(FMDTPDPIDAY)                                              
            DC    AL2(FMDTPDPSTIM)                                              
            DC    AL2(FMDTPDPETIM)                                              
            DC    AL2(FMDTPDPWKS)                                               
            DC    AL2(FMDTPDPPNAM)                                              
            DC    AL2(FMDTPDPDMVL)                                              
            DC    AL2(FMDTPDPDUMV)                                              
            DC    AL2(FMDTPDPPREC)                                              
            DC    AL2(0)                                                        
THN098175AAX EQU *                                                              
            DC    AL1(0)                                                        
THN098175AX EQU *                                                               
                                                                                
                                                                                
THN098175B  DS  0X                                                              
            DC   AL1(THN098175BX-THN098175B)                                    
            DC   C'TP HN'                                                       
            DC   AL1(TDRRTDML)                                                  
            DC   AL1(00,98,06,24)                                               
THN098175BA DS   0X                                                             
            DC    AL1(THN098175BAX-THN098175BA),AL2(FMHDMLS)                    
            DC    AL2(FMDDMLSRCAT)                                              
            DC    AL2(FMDDMLSNCAT)                                              
            DC    AL2(FMDDMLSFDNM)                                              
            DC    AL2(0)                                                        
THN098175BAX EQU *                                                              
            DC    AL1(0)                                                        
THN098175BX EQU *                                                               
         EJECT                                                                  
*------------------------------ NSI WTP ------------------------------*         
                                                                                
TNW098267A  DS  0X                                                              
            DC   AL1(TNW098267AX-TNW098267A)                                    
            DC   C'TP NW'                                                       
            DC   AL1(TDRRTDMV)                                                  
            DC   AL1(00,98,09,24)                                               
TNW098267AA DS   0X                                                             
            DC    AL1(TNW098267AAX-TNW098267AA),AL2(FMHTPDP)                    
            DC    AL2(FMDTPDPRTYP)                                              
            DC    AL2(FMDTPDPSTNN)                                              
            DC    AL2(FMDTPDPBKN)                                               
            DC    AL2(FMDTPDPSBKN)                                              
            DC    AL2(FMDTPDPDTN)                                               
            DC    AL2(FMDTPDPCMPN)                                              
            DC    AL2(FMDTPDPSTTN)                                              
            DC    AL2(FMDTPDPBOOK)                                              
            DC    AL2(FMDTPDPSUBK)                                              
            DC    AL2(FMDTPDPIDAY)                                              
            DC    AL2(FMDTPDPSTIM)                                              
            DC    AL2(FMDTPDPETIM)                                              
            DC    AL2(FMDTPDPWKS)                                               
            DC    AL2(FMDTPDPPNAM)                                              
            DC    AL2(FMDTPDPDMVL)                                              
            DC    AL2(FMDTPDPDUMV)                                              
            DC    AL2(FMDTPDPPREC)                                              
            DC    AL2(0)                                                        
TNW098267AAX EQU *                                                              
            DC    AL1(0)                                                        
TNW098267AX EQU *                                                               
                                                                                
                                                                                
TNW098267B  DS  0X                                                              
            DC   AL1(TNW098267BX-TNW098267B)                                    
            DC   C'TP NW'                                                       
            DC   AL1(TDRRTDML)                                                  
            DC   AL1(00,98,09,24)                                               
TNW098267BA DS   0X                                                             
            DC    AL1(TNW098267BAX-TNW098267BA),AL2(FMHDMLS)                    
            DC    AL2(FMDDMLSRCAT)                                              
            DC    AL2(FMDDMLSNCAT)                                              
            DC    AL2(FMDDMLSFDNM)                                              
            DC    AL2(0)                                                        
TNW098267BAX EQU *                                                              
            DC    AL1(0)                                                        
TNW098267BX EQU *                                                               
         EJECT                                                                  
*------------------------------- SRC TP ------------------------------*         
                                                                                
TST098267A  DS  0X                                                              
            DC   AL1(TST098267AX-TST098267A)                                    
            DC   C'TP ST'                                                       
            DC   AL1(TDRRTDMV)                                                  
            DC   AL1(00,98,09,24)                                               
TST098267AA DS   0X                                                             
            DC    AL1(TST098267AAX-TST098267AA),AL2(FMHTPDP)                    
            DC    AL2(FMDTPDPRTYP)                                              
            DC    AL2(FMDTPDPSTNN)                                              
            DC    AL2(FMDTPDPBKN)                                               
            DC    AL2(FMDTPDPSBKN)                                              
            DC    AL2(FMDTPDPDTN)                                               
            DC    AL2(FMDTPDPCMPN)                                              
            DC    AL2(FMDTPDPSTTN)                                              
            DC    AL2(FMDTPDPBOOK)                                              
            DC    AL2(FMDTPDPSUBK)                                              
            DC    AL2(FMDTPDPIDAY)                                              
            DC    AL2(FMDTPDPSTIM)                                              
            DC    AL2(FMDTPDPETIM)                                              
            DC    AL2(FMDTPDPWKS)                                               
            DC    AL2(FMDTPDPPNAM)                                              
            DC    AL2(FMDTPDPDMVL)                                              
            DC    AL2(FMDTPDPDUMV)                                              
            DC    AL2(FMDTPDPPREC)                                              
            DC    AL2(0)                                                        
TST098267AAX EQU *                                                              
            DC    AL1(0)                                                        
TST098267AX EQU *                                                               
                                                                                
                                                                                
TST098267B  DS  0X                                                              
            DC   AL1(TST098267BX-TST098267B)                                    
            DC   C'TP ST'                                                       
            DC   AL1(TDRRTDML)                                                  
            DC   AL1(00,98,09,24)                                               
TST098267BA DS   0X                                                             
            DC    AL1(TST098267BAX-TST098267BA),AL2(FMHDMLS)                    
            DC    AL2(FMDDMLSRCAT)                                              
            DC    AL2(FMDDMLSNCAT)                                              
            DC    AL2(FMDDMLSFDNM)                                              
            DC    AL2(0)                                                        
TST098267BAX EQU *                                                              
            DC    AL1(0)                                                        
TST098267BX EQU *                                                               
         EJECT                                                                  
*------------------------------- ARB TP ------------------------------*         
                                                                                
TAT098267A  DS  0X                                                              
            DC   AL1(TAT098267AX-TAT098267A)                                    
            DC   C'TP AT'                                                       
            DC   AL1(TDRRTDMV)                                                  
            DC   AL1(00,98,09,24)                                               
TAT098267AA DS   0X                                                             
            DC    AL1(TAT098267AAX-TAT098267AA),AL2(FMHTPDP)                    
            DC    AL2(FMDTPDPRTYP)                                              
            DC    AL2(FMDTPDPSTNN)                                              
            DC    AL2(FMDTPDPBKN)                                               
            DC    AL2(FMDTPDPSBKN)                                              
            DC    AL2(FMDTPDPDTN)                                               
            DC    AL2(FMDTPDPCMPN)                                              
            DC    AL2(FMDTPDPSTTN)                                              
            DC    AL2(FMDTPDPBOOK)                                              
            DC    AL2(FMDTPDPSUBK)                                              
            DC    AL2(FMDTPDPIDAY)                                              
            DC    AL2(FMDTPDPSTIM)                                              
            DC    AL2(FMDTPDPETIM)                                              
            DC    AL2(FMDTPDPWKS)                                               
            DC    AL2(FMDTPDPPNAM)                                              
            DC    AL2(FMDTPDPDMVL)                                              
            DC    AL2(FMDTPDPDUMV)                                              
            DC    AL2(FMDTPDPPREC)                                              
            DC    AL2(0)                                                        
TAT098267AAX EQU *                                                              
            DC    AL1(0)                                                        
TAT098267AX EQU *                                                               
                                                                                
                                                                                
TAT098267B  DS  0X                                                              
            DC   AL1(TAT098267BX-TAT098267B)                                    
            DC   C'TP AT'                                                       
            DC   AL1(TDRRTDML)                                                  
            DC   AL1(00,98,09,24)                                               
TAT098267BA DS   0X                                                             
            DC    AL1(TAT098267BAX-TAT098267BA),AL2(FMHDMLS)                    
            DC    AL2(FMDDMLSRCAT)                                              
            DC    AL2(FMDDMLSNCAT)                                              
            DC    AL2(FMDDMLSFDNM)                                              
            DC    AL2(0)                                                        
TAT098267BAX EQU *                                                              
            DC    AL1(0)                                                        
TAT098267BX EQU *                                                               
         EJECT                                                                  
*------------------------------- BBM TP ------------------------------*         
                                                                                
TAC098267A  DS  0X                                                              
            DC   AL1(TAC098267AX-TAC098267A)                                    
            DC   C'TP AC'                                                       
            DC   AL1(TDRRTDMV)                                                  
            DC   AL1(00,98,09,24)                                               
TAC098267AA DS   0X                                                             
            DC    AL1(TAC098267AAX-TAC098267AA),AL2(FMHTPDP)                    
            DC    AL2(FMDTPDPRTYP)                                              
            DC    AL2(FMDTPDPSTNN)                                              
            DC    AL2(FMDTPDPBKN)                                               
            DC    AL2(FMDTPDPSBKN)                                              
            DC    AL2(FMDTPDPDTN)                                               
            DC    AL2(FMDTPDPCMPN)                                              
            DC    AL2(FMDTPDPSTTN)                                              
            DC    AL2(FMDTPDPBOOK)                                              
            DC    AL2(FMDTPDPSUBK)                                              
            DC    AL2(FMDTPDPIDAY)                                              
            DC    AL2(FMDTPDPSTIM)                                              
            DC    AL2(FMDTPDPETIM)                                              
            DC    AL2(FMDTPDPWKS)                                               
            DC    AL2(FMDTPDPPNAM)                                              
            DC    AL2(FMDTPDPDMVL)                                              
            DC    AL2(FMDTPDPDUMV)                                              
            DC    AL2(FMDTPDPPREC)                                              
            DC    AL2(0)                                                        
TAC098267AAX EQU *                                                              
            DC    AL1(0)                                                        
TAC098267AX EQU *                                                               
                                                                                
                                                                                
TAC098267B  DS  0X                                                              
            DC   AL1(TAC098267BX-TAC098267B)                                    
            DC   C'TP AC'                                                       
            DC   AL1(TDRRTDML)                                                  
            DC   AL1(00,98,09,24)                                               
TAC098267BA DS   0X                                                             
            DC    AL1(TAC098267BAX-TAC098267BA),AL2(FMHDMLS)                    
            DC    AL2(FMDDMLSRCAT)                                              
            DC    AL2(FMDDMLSNCAT)                                              
            DC    AL2(FMDDMLSFDNM)                                              
            DC    AL2(0)                                                        
TAC098267BAX EQU *                                                              
            DC    AL1(0)                                                        
TAC098267BX EQU *                                                               
         EJECT                                                                  
*------------------------------- CSI TP ------------------------------*         
                                                                                
TNC098267A  DS  0X                                                              
            DC   AL1(TNC098267AX-TNC098267A)                                    
            DC   C'TP NC'                                                       
            DC   AL1(TDRRTDMV)                                                  
            DC   AL1(00,98,09,24)                                               
TNC098267AA DS   0X                                                             
            DC    AL1(TNC098267AAX-TNC098267AA),AL2(FMHTPDP)                    
            DC    AL2(FMDTPDPRTYP)                                              
            DC    AL2(FMDTPDPSTNN)                                              
            DC    AL2(FMDTPDPBKN)                                               
            DC    AL2(FMDTPDPSBKN)                                              
            DC    AL2(FMDTPDPDTN)                                               
            DC    AL2(FMDTPDPCMPN)                                              
            DC    AL2(FMDTPDPSTTN)                                              
            DC    AL2(FMDTPDPBOOK)                                              
            DC    AL2(FMDTPDPSUBK)                                              
            DC    AL2(FMDTPDPIDAY)                                              
            DC    AL2(FMDTPDPSTIM)                                              
            DC    AL2(FMDTPDPETIM)                                              
            DC    AL2(FMDTPDPWKS)                                               
            DC    AL2(FMDTPDPPNAM)                                              
            DC    AL2(FMDTPDPDMVL)                                              
            DC    AL2(FMDTPDPDUMV)                                              
            DC    AL2(FMDTPDPPREC)                                              
            DC    AL2(0)                                                        
TNC098267AAX EQU *                                                              
            DC    AL1(0)                                                        
TNC098267AX EQU *                                                               
                                                                                
                                                                                
TNC098267B  DS  0X                                                              
            DC   AL1(TNC098267BX-TNC098267B)                                    
            DC   C'TP NC'                                                       
            DC   AL1(TDRRTDML)                                                  
            DC   AL1(00,98,09,24)                                               
TNC098267BA DS   0X                                                             
            DC    AL1(TNC098267BAX-TNC098267BA),AL2(FMHDMLS)                    
            DC    AL2(FMDDMLSRCAT)                                              
            DC    AL2(FMDDMLSNCAT)                                              
            DC    AL2(FMDDMLSFDNM)                                              
            DC    AL2(0)                                                        
TNC098267BAX EQU *                                                              
            DC    AL1(0)                                                        
TNC098267BX EQU *                                                               
                                                                                
*                                                                               
*------------------------------- ARB RTP -----------------------------*         
                                                                                
TAR098267A  DS  0X                                                              
            DC   AL1(TAR098267AX-TAR098267A)                                    
            DC   C'TP AR'                                                       
            DC   AL1(TDRRTDMV)                                                  
            DC   AL1(00,98,09,24)                                               
TAR098267AA DS   0X                                                             
            DC    AL1(TAR098267AAX-TAR098267AA),AL2(FMHTPDP)                    
            DC    AL2(FMDTPDPRTYP)                                              
            DC    AL2(FMDTPDPSTNN)                                              
            DC    AL2(FMDTPDPBKN)                                               
            DC    AL2(FMDTPDPSBKN)                                              
            DC    AL2(FMDTPDPDTN)                                               
            DC    AL2(FMDTPDPCMPN)                                              
            DC    AL2(FMDTPDPSTTN)                                              
            DC    AL2(FMDTPDPBOOK)                                              
            DC    AL2(FMDTPDPSUBK)                                              
            DC    AL2(FMDTPDPIDAY)                                              
            DC    AL2(FMDTPDPSTIM)                                              
            DC    AL2(FMDTPDPETIM)                                              
            DC    AL2(FMDTPDPWKS)                                               
            DC    AL2(FMDTPDPPNAM)                                              
            DC    AL2(FMDTPDPDMVL)                                              
            DC    AL2(FMDTPDPDUMV)                                              
            DC    AL2(FMDTPDPPREC)                                              
            DC    AL2(0)                                                        
TAR098267AAX EQU *                                                              
            DC    AL1(0)                                                        
TAR098267AX EQU *                                                               
                                                                                
                                                                                
TAR098267B  DS  0X                                                              
            DC   AL1(TAR098267BX-TAR098267B)                                    
            DC   C'TP AR'                                                       
            DC   AL1(TDRRTDML)                                                  
            DC   AL1(00,98,09,24)                                               
TAR098267BA DS   0X                                                             
            DC    AL1(TAR098267BAX-TAR098267BA),AL2(FMHDMLS)                    
            DC    AL2(FMDDMLSRCAT)                                              
            DC    AL2(FMDDMLSNCAT)                                              
            DC    AL2(FMDDMLSFDNM)                                              
            DC    AL2(0)                                                        
TAR098267BAX EQU *                                                              
            DC    AL1(0)                                                        
TAR098267BX EQU *                                                               
         EJECT                                                                  
*---------------------------- NSI TP OVERNIGHTS-----------------------*         
                                                                                
TOT098175A  DS  0X                                                              
            DC   AL1(TOT098175AX-TOT098175A)                                    
            DC   C'TP NO'                                                       
            DC   AL1(TDRRTDMV)                                                  
            DC   AL1(00,98,06,24)                                               
TOT098175AA DS   0X                                                             
            DC    AL1(TOT098175AAX-TOT098175AA),AL2(FMHTPDP)                    
            DC    AL2(FMDTPDPRTYP)                                              
            DC    AL2(FMDTPDPSTNN)                                              
            DC    AL2(FMDTPDPBKN)                                               
            DC    AL2(FMDTPDPSBKN)                                              
            DC    AL2(FMDTPDPDTN)                                               
            DC    AL2(FMDTPDPCMPN)                                              
            DC    AL2(FMDTPDPSTTN)                                              
            DC    AL2(FMDTPDPBOOK)                                              
            DC    AL2(FMDTPDPSUBK)                                              
            DC    AL2(FMDTPDPIDAY)                                              
            DC    AL2(FMDTPDPSTIM)                                              
            DC    AL2(FMDTPDPETIM)                                              
            DC    AL2(FMDTPDPWKS)                                               
            DC    AL2(FMDTPDPPNAM)                                              
            DC    AL2(FMDTPDPDMVL)                                              
            DC    AL2(FMDTPDPDUMV)                                              
            DC    AL2(FMDTPDPPREC)                                              
            DC    AL2(0)                                                        
TOT098175AAX EQU *                                                              
            DC    AL1(0)                                                        
TOT098175AX EQU *                                                               
                                                                                
                                                                                
TOT098175B  DS  0X                                                              
            DC   AL1(TOT098175BX-TOT098175B)                                    
            DC   C'TP NO'                                                       
            DC   AL1(TDRRTDML)                                                  
            DC   AL1(00,98,06,24)                                               
TOT098175BA DS   0X                                                             
            DC    AL1(TOT098175BAX-TOT098175BA),AL2(FMHDMLS)                    
            DC    AL2(FMDDMLSRCAT)                                              
            DC    AL2(FMDDMLSNCAT)                                              
            DC    AL2(FMDDMLSFDNM)                                              
            DC    AL2(0)                                                        
TOT098175BAX EQU *                                                              
            DC    AL1(0)                                                        
TOT098175BX EQU *                                                               
         EJECT                                                                  
*                                                                               
                                                                                
*---------------------------- FUSION ---------------------------------*         
                                                                                
TTF098175A  DS  0X                                                              
            DC   AL1(TTF098175AX-TTF098175A)                                    
            DC   C'TP FT'                                                       
            DC   AL1(TDRRTDMV)                                                  
            DC   AL1(00,98,06,24)                                               
TTF098175AA DS   0X                                                             
            DC    AL1(TTF098175AAX-TTF098175AA),AL2(FMHTPDP)                    
            DC    AL2(FMDTPDPRTYP)                                              
            DC    AL2(FMDTPDPSTNN)                                              
            DC    AL2(FMDTPDPBKN)                                               
            DC    AL2(FMDTPDPSBKN)                                              
            DC    AL2(FMDTPDPDTN)                                               
            DC    AL2(FMDTPDPCMPN)                                              
            DC    AL2(FMDTPDPSTTN)                                              
            DC    AL2(FMDTPDPBOOK)                                              
            DC    AL2(FMDTPDPSUBK)                                              
            DC    AL2(FMDTPDPIDAY)                                              
            DC    AL2(FMDTPDPSTIM)                                              
            DC    AL2(FMDTPDPETIM)                                              
            DC    AL2(FMDTPDPWKS)                                               
            DC    AL2(FMDTPDPPNAM)                                              
            DC    AL2(FMDTPDPDMVL)                                              
            DC    AL2(FMDTPDPDUMV)                                              
            DC    AL2(FMDTPDPPREC)                                              
            DC    AL2(0)                                                        
TTF098175AAX EQU *                                                              
            DC    AL1(0)                                                        
TTF098175AX EQU *                                                               
                                                                                
                                                                                
TTF098175B  DS  0X                                                              
            DC   AL1(TTF098175BX-TTF098175B)                                    
            DC   C'TP FT'                                                       
            DC   AL1(TDRRTDML)                                                  
            DC   AL1(00,98,06,24)                                               
TTF098175BA DS   0X                                                             
            DC    AL1(TTF098175BAX-TTF098175BA),AL2(FMHDMLS)                    
            DC    AL2(FMDDMLSRCAT)                                              
            DC    AL2(FMDDMLSNCAT)                                              
            DC    AL2(FMDDMLSFDNM)                                              
            DC    AL2(0)                                                        
TTF098175BAX EQU *                                                              
            DC    AL1(0)                                                        
TTF098175BX EQU *                                                               
         EJECT                                                                  
            DC   AL1(0)                                                         
***********************************************************************         
         TITLE 'DEDEM0C - $DEM T/P && PAV DEMO LOOK-UPS/UPGRADES (DEDEM+        
               WRK)'                                                            
* DEDEMWRK                                                                      
       ++INCLUDE DEDEMWRK                                                       
         ORG   APSAVE                                                           
         SPACE 1                                                                
* SAVE STORAGE FOR OVERLAY                                                      
*                                                                               
CHUNKLEN DS    H                   TOTAL L'STATION CHUNK                        
HEADLEN  DS    H                   L'STATION HEADINGS                           
STADISP  DS    H                   DISP. INTO HEADING OF STATION                
EXECLEN  DS    H                   L'STATION HEADINGS-2 (FOR EX)                
DATADISP DS    (MAXSTAS)H          DATA DISPS. ONE PER STATION                  
SVTHSSTN DS    XL1                 SAVE STATION # IN STEREO SESSION             
SVTHSBK  DS    XL1                 SAVE BOOK    # IN STEREO SESSION             
STBKDATA DS    CL1                 ANY DATA FOR STTN/BOOK COMBO (Y/N)?          
PRECFLAG DS    CL1                 DID PRECISION CHANGE FOR DEMO                
DEMOPRE  DS    CL1                 PRECISION FOR DEMO                           
RESUMAPP DS    CL1                 RESUMING FROM APPLC BREAK? (Y/X'00')         
MYAPSAVL EQU   *-APSAVE                                                         
         DS    0CL(L'APSAVE-MYAPSAVL+1)                                         
         ORG                                                                    
         TITLE 'DEDEM0C - $DEM T/P && PAV DEMO LOOK-UPS/UPGRADES (DBEXT+        
               RAD)'                                                            
***********************************************************************         
*============================= DEDBEXTRAD ============================*         
                                                                                
       ++INCLUDE DEDBEXTRAD                                                     
***********************************************************************         
         TITLE 'DEDEM0C - $DEM LILO REPORT'                                     
DM0CWRKD DSECT                                                                  
DM0CW_RD DS    A                   DEM0C'S RD                                   
DM0CWRKX EQU   *                                                                
DM0CWRKL EQU   DM0CWRKX-DM0CWRKD                                                
         EJECT                                                                  
* DSECT TO COVER TEMPORARY WORKING STORAGE                                      
*                                                                               
DEMTMPD  DSECT                                                                  
*                                                                               
MYDMWORK DS    12D                                                              
MYDMCB   DS    6F                                                               
SAVEDMCB DS    6F                                                               
ALOCWRK  DS    A                   A(LOC WRK AREA)-USED B/W SUBRTN POOL         
SAVERD   DS    A                   SAVED RD VALUE                               
SAVERE   DS    A                   SAVED RE VALUE IN DEMHOOK                    
SAVERF   DS    A                   SAVED RF                                     
*                                                                               
AIUNWRK  DS    A                   A(WORK AREA FOR IUN STUFF)                   
AVUTS    DS    A                   A(VUT VALUES PASSED TO PROCIUN)              
ADBLOCK  DS    A                   A(DBLOCK)                                    
ASUBRTN  DS    A                                                                
ADLDNTRY DS    A                   A(DOWNLOAD DATA TABLE ENTRY)                 
ADBDQD   DS    A                   A(DBDQD ENTRIES)                             
*                                                                               
MYBASES  DS    0A                  BASE REGS FOR MAIN DEM0C NMOD                
MYBASE2  DS    A                    A(2ND 4096 BYTES OF THIS PROGRAM)           
MYBASE1  DS    A                    A(1ST 4096 BYTES OF THIS PROGRAM)           
*                                                                               
AGOSUB   DS    A                   A(SUBROUTINE POOL INTERFACE)                 
ASUBR01  DS    A                   A(SUB-ROUTINE POOL ONE)                      
ASUBR02  DS    A                   A(SUB-ROUTINE POOL TWO)                      
ASUBR03  DS    A                   A(SUB-ROUTINE POOL THREE)                    
                                                                                
AEDITTAB DS    A                   A(EDITTAB)                                   
AEDITTA2 DS    A                   A(EDITTAB2)                                  
AEDITTA3 DS    A                   A(EDITTAB3)                                  
AEDITTA4 DS    A                   A(EDITTAB4)                                  
AKDAYTAB DS    A                   A(KDAYTAB)                                   
AIDAYTAB DS    A                   A(IDAYTAB)                                   
AVUTLIST DS    A                   A(VUTLIST)                                   
APUTLIST DS    A                   A(PUTLIST)                                   
ASHRLIST DS    A                   A(SHRLIST)                                   
ANUMWKTB DS    A                   A(NUMWKTAB)                                  
A1STWKTB DS    A                   A(FRSTWKTB)                                  
APROFTAB DS    A                   A(PROFTAB)                                   
AROTQHTB DS    A                   A(ROTQHTAB)                                  
ADLDTABS DS    A                   A(DOWNLOAD DATA TABLES)                      
                                                                                
VHRTOQH  DS    V                   A(HRTOQH) IN DEMTIME                         
VQHTOHR  DS    V                   A(QHTOHR) IN DEMTIME                         
*                                                                               
APROFILE DS    A                   A(PROFILE STORAGE AREA)                      
LPROFILE DS    XL1                 L(PROFILE STORAGE AREA)                      
*                                                                               
MAXNDT   DS    H                   MAX # DAY/TIME COMBOS (STEREO/PAV)           
*                                                                               
MYDATDSP DS    H                                                                
*                                                                               
MYSTAS   DS    CL(L'STAS)          MY INTERNAL STATIONS FIELD                   
MYPROF1W DS    XL(L'PROF1W)                                                     
STPROG   DS    CL16                START TIME PROGRAM NAME                      
NDPROG   DS    CL16                END TIME PROGRAM NAME                        
DAYTIME  DS    CL13                OUTPUT DAY/TIME VALUE                        
ALPHAMKT DS    CL3                 ALPHA MARKET                                 
ACTVWKS  DS    CL4                 ACTIVE WEEKS (1234)                          
RQSTSQHR DS    XL1                 REQUEST'S START QUARTER OUR                  
RQSTEQHR DS    XL1                 REQUEST'S END   QUARTER OUR                  
TEMPMTIM DS    XL2                 TEMPORARY MILITARY TIME                      
TEMPQHR  DS    XL1                 TEMPORARY QUARTER HOUR                       
TEMPIDAY DS    XL1                 TEMPORARY INTERNAL DAY                       
TEMPSTIM DS    XL2                 TEMPORARY START TIME IN MILITARY             
TEMPETIM DS    XL2                 TEMPORARY END   TIME IN MILITARY             
STARTDAY DS    XL1                 NSIWEEK START DAY (1-7)                      
PVACTBK  DS    XL2                 PREVIOUS ACTUAL BOOK                         
LNBKLIST DS    4XL2                LIST OF LATEST N BOOKS (MAX 4)               
MYNLBK   DS    PL1                 NUMBER OF LATEST BOOKS                       
PRECPROF DS    CL1                 PRECISION SET IN PROFILE                     
CANMMBK  DS    CL1                 CANAD MTR MKT--C'N'=WKLY, C'M'=MTHLY         
TAPEOPT  DS    CL1                 Y=TAPE BASED, N=BOOK BASED DEMO CALC         
TMPRCTYP DS    XL(L'TDRRCTYP)      TEMP STORAGE FOR TSAR DEM RECD TYPE          
TMPPURE  DS    XL2                 TEMP STORAGE FOR PURE NUMBER                 
         DS    XL16                SPARE                                        
*                                                                               
MYFLAG1  DS    XL1                                                              
MYF1LNBK EQU    X80                 PROCESSING LATEST N BOOK                    
MYF1CNT  EQU    X40                 CANADIAN NSI TIME PERIOD                    
MYF1DTRO EQU    X20                 PROCESSING DAY/TIME ROTATION                
*                                                                               
DEMOFROM DS    XL(L'RCTFROM)                                                    
DEMOUPTO DS    XL(L'RCTUPTO)                                                    
RECTYPE  DS    XL(L'TDRRCTYP)      TYPE OF RECD TO POST (STEREO ONLY)           
STNDEMS  DS    XL1                 TOTAL # OF DEMOS REQUSTD FROM STEREO         
GOSUBN   DS    XL1                 SUB-ROUTINE NUMBER                           
MYELCODE DS    XL1                                                              
*                                 PICK-UP FROM LAST LEFT OFF FLAGS              
PUFLLOO  DS    CL1                 RE-ESTABLISHED MAP CODES ?                   
PUFLLOO2 DS    CL1                 RE-ESTABLISHED "THIS" FIELDS ?               
PUFLLOO3 DS    CL1                 RE-ESTABLISHED NTH DEMO ?                    
*                                                                               
TMPKSE   DS    0XL(L'THISKDAY+L'THISSQH+L'THISEQH)                              
TMPKDAY  DS    XL(L'THISKDAY)                                                   
TMPSQH   DS    XL(L'THISSQH)                                                    
TMPEQH   DS    XL(L'THISEQH)                                                    
TMPWKS   DS    XL(L'THISWKS)                                                    
*                                                                               
THISKEY  DS    0X                  CURRENT KEY VALUES                           
THISBK   DS    XL1                 CURRENT BOOK NUMBER                          
THISLBK  DS    PL1                 CURRENT LATEST BOOK NUMBER                   
THISDAY  DS    XL1                 CURRENT DAY/TIME ENTRY NUMBER                
THISDAYR DS    XL1                 CURRENT DAY/TIME ENTRY NUMBER IN ROT         
THISKDAY DS    XL1                 CURRENT KEY DAY                              
THISSQH  DS    XL1                 CURRENT START QUARTER HOUR                   
THISEQH  DS    XL1                 CURRENT END QUARTER HOUR                     
THISTYPE DS    XL1                 CURRENT LINE TYPE                            
THIS1WK  DS    XL1                 CURRENT 1ST WEEK OF PROGRAM                  
THISNOR  DS    XL1                 CURRENT NORMAL PROGRAM FLAG                  
THISSTA  DS    XL1                 CURRENT STATION NUMBER                       
THISASQC DS    XL1                 CURRENT DBACTSQC                             
THISAEQC DS    XL1                 CURRENT DBACTEQC                             
THISKEYX EQU   *                                                                
THISPROG DS    (MAXSTAS)CL16       CURRENT PROGRAM NAME                         
THISBOOK DS    (MAXSTAS)XL2        CURRENT BOOK                                 
THISPNUM DS    XL3                 CURRENT PROGRAM NUMBER                       
THISPURE DS    XL2                 CURRENT PURE NUMBER                          
THISWKS  DS    XL1                 CURRENT WEEK BITS                            
THISNWK  DS    XL1                 CURRENT # OF WEEKS                           
THISSWKN DS    XL1                 CURRENT START WEEK OF MONTH                  
THISEWKN DS    XL1                 CURRENT END   WEEK OF MONTH                  
THISNQH  DS    XL1                 CURRENT DURATION IN QH                       
THISFLG1 DS    XL1                 CURRENT BINFLAG1                             
THISSETM DS    XL4                                                              
THISAFFL DS    CL5                 AFFILIATION                                  
THISMNUM DS    XL2                 MARKET NUMBER                                
THISMKNM DS    CL25                MARKET NAME                                  
THISRTYP DS    CL1                                                              
THISDEMS DS    (MAXDEMS*3)XL4      CURRENT DEMOUT DEMO VALUES                   
PERIDEMS DS    (MAXDEMS*3)XL4      PERIOD TOTALS DEMO VALUES                    
DEMONDEM DS    XL1                 ACTUAL N'DEMOS TO LOOK-UP                    
DEMODEMS DS    (MAXDEMS*3)XL3,X    DEMO VALUES (+LIST TERMINATOR)               
SVSTAT   DS    XL(L'THISSTA)                                                    
SVBOOK   DS    XL(L'THISBK)                                                     
SVDAY    DS    XL(L'THISDAY)                                                    
SVSETIME DS    XL4                                                              
SVSEQH   DS    0XL(L'SVSEQHS+L'SVSEQHE)                                         
SVSEQHS  DS     XL1                                                             
SVSEQHE  DS     XL1                                                             
*                                                                               
QHVUTS   DS    3F                  VUT VALUES                                   
*                                                                               
SPDMLK2  DS    XL(SPDEMLKL)        2ND SPDEMLK BLOCK                            
*                                                                               
SVDBLOCK DS    XL(DBLOCK1X-DBLOCK1)    AREA TO SAVE DBLOCK                      
*                                                                               
MYDBXTRA DS    XL128               DBLOCK EXTNSION (HOPE 128 IS ENOUGH)         
         DS    0F                                                               
SYSCEXT  DS    0X                                                               
         DS    XL(L'SPXTAREA)                                                   
                                                                                
RELO0C   DS    F                                                                
*                                                                               
*                                                                               
                                                                                
DEMTMPL  EQU   *-DEMTMPD                                                        
         DS    0CL((APWORKX-APWORK)-DEMTMPL+1)                                  
         EJECT                                                                  
         SPACE 1                                                                
* DSECT TO COVER BINSRCH RECORD                                                 
*                                                                               
BINRECD  DSECT                                                                  
BINKEY   DS    0X                  BINSRCH KEY                                  
BINBK    DS    XL1                 BOOK NUMBER                                  
BINLBK   DS    PL1                 LATEST BOOK NUMBER                           
BINDAY   DS    XL1                 DAY NUMBER                                   
BINDAYR  DS    XL1                 DAY NUMBER W/IN ROTATION                     
BINKDAY  DS    XL1                 KEY DAY                                      
BINSQH   DS    XL1                 START QUARTER HOUR                           
BINEQH   DS    XL1                 END QUARTER HOUR                             
BINTYPE  DS    XL1                 FORMAT TYPE (SEE BELOW)                      
BINTDEM  EQU   0                   DEMOS OR RTG/IMPS                            
BINTSHR  EQU   1                   DEMO SHARES       (AVAIL FORMAT)             
BINTHPT  EQU   2                   DEMO HUT/PUT/TOTS (AVAIL FORMAT)             
BIN1WK   DS    XL1                 1ST WEEK PROGRAM AIRED (PAV ONLY)            
BINNOR   DS    XL1                 NORMAL PROGRAM (PAV ONLY)                    
BINSTA   DS    XL1                 STATION NUMBER                               
BINASQC  DS    XL1                 DBACTSQC (PAV ONLY--TO IDENTFY PROG)         
BINAEQC  DS    XL1                 DBACTEQC ( "   "     "    "     "  )         
BINKEYL  EQU   *-BINKEY                                                         
BINDATA  DS    0X                  BINSRCH DATA                                 
BINPROG  DS    CL16                PROGRAM NAME(S)                              
BINBOOK  DS    XL2                 ACTUAL BOOK                                  
*                                   SET TO ST/END WKS OF MTH FOR CSI TP         
BINPNUM  DS    XL3                 PROGRAM NUMBER                               
BINPURE  DS    XL2                 PURE NUMBER (PAV ONLY)                       
BINWKS   DS    XL1                 WEEK BITS (PAV ONLY)                         
BINNWK   DS    XL1                 NUMBER OF WEEKS (PAV ONLY)                   
BINNQH   DS    XL1                 DURATION IN QUARTER HOURS (PAV ONLY)         
BINFLAG1 DS    XL1                 FLAGS ABOUT BINSRCH ENTRY                    
BF11HHR  EQU   X'80'                ENTRY IS 1ST  HALF-HOUR OF PROG             
BF1XHHR  EQU   X'40'                ENTRY IS LAST HALF-HOUR OF PROG             
BINFXDTL EQU   *-BINDATA           BINSRCH FIXED DATA LENGTH                    
BINDEMS  DS    0XL3                VARIABLE NUMBER OF DEMO VALUES               
         EJECT                                                                  
* DSECT TO COVER TSAR DEMO RECORD                                               
*                                                                               
TSDEMRCD DSECT                                                                  
TDRKEY   DS    0X                  TSAR DEMO RECORD KEY                         
TDRRCTYP DS    CL1                  RECORD TYPE SEE BELOW                       
TDRSTA   DS    XL1                  STATION NUMBER                              
TDRBK    DS    XL1                  BOOK NUMBER                                 
TDRDAY   DS    XL1                  DAY NUMBER                                  
TDRDAYR  DS    XL1                  DAY NUMBER W/IN ROTATION                    
TDRSETM  DS    XL4                  START-ENDTIME                               
TDRKDAY  DS    XL1                  KEY DAY                                     
TDRSQH   DS    XL1                  START QUARTER HOUR                          
TDREQH   DS    XL1                  END QUARTER HOUR                            
TDRTYPE  DS    XL1                  FORMAT TYPE (SEE BELOW)                     
TDRTDEM  EQU   0                     DEMOS OR RTG/IMPS                          
TDRTSHR  EQU   1                     DEMO SHARES       (AVAIL FORMAT)           
TDRTHPT  EQU   2                     DEMO HUT/PUT/TOTS (AVAIL FORMAT)           
TDR1WK   DS    XL1                  1ST WEEK PROGRAM AIRED (PAV ONLY)           
TDRNOR   DS    XL1                  NORMAL PROGRAM (PAV ONLY)                   
TDRASQC  DS    XL1                  DBACTSQC (PAV ONLY--IDENTIFY PROG)          
TDRAEQC  DS    XL1                  DBACTEQC (PAV ONLY--IDENTIFY PROG)          
TDRRTYPE DS    CL1                  ROTATION TYPE                               
TDRKEYL  EQU   *-TDRKEY                                                         
         DS    0CL(L'TSARKEY-TDRKEYL+1)                                         
*KYBKL   EQU   TDRBK-TDRKEY+L'TDRBK                                             
TKYBKL   EQU   TDRSETM-TDRKEY+L'TDRSETM                                         
                                                                                
TDRDATA  DS    0X                  TSAR DEMO RECORD DATA                        
TDRPROG  DS    CL16                 PROGRAM NAME(S)                             
TDRPURE  DS    XL2                  PURE NUMBER (PAV ONLY)                      
TDRWKS   DS    XL1                  ACTIVE WEEKS BITS (PAV ONLY)                
TDRFLAG1 DS    XL1                  FLAGS ABOUT TSAR DEMO RECD ENTRY:           
TF11HHR  EQU   BF11HHR               ENTRY IS 1ST  HALF-HOUR OF PROG            
TF1XHHR  EQU   BF1XHHR               ENTRY IS LAST HALF-HOUR OF PROG            
TF1DUMDV EQU   X20                   RECORD CONTAINS DUMMY DEMO VALUES          
TF1ALL   EQU   TF11HHR+TF1XHHR+TF1DUMDV                                         
         DS    0CL(XFF-TF1ALL+1)     ENSURE EACH BIT IS USED ONLY ONCE          
TDRAFFL  DS    CL5                                                              
TDRFIXL  EQU   *-TSDEMRCD           L(FIXED PART OF RECORD)                     
TDRDEMS  DS    0XL3                 VARIABLE NUMBER OF DEMO VALUES              
                                                                                
         ORG   TDRDATA             AREA FOR DAY/TIME LIST                       
TDRDATA2 DS    0X                   ie. TDRRCTYP=TDRRTDTL                       
TDREBCDT DS    CL(1+L'DAYTIME)      L'DAYTIME, DAYTIME                          
TDRNUMDT DS    XL2                  TOTAL # OF DAY/TIME COMBOS                  
TDRFIXL2 EQU   *-TSDEMRCD                                                       
TDRDYTMS DS    0XL(DTNTRYQ)                                                     
                                                                                
* TDRRCTYP can have the following values:                                       
TDRRTDML EQU   X'10'               RECORD CONTAINS DEMO LIST                    
TDRRTDMV EQU   X'80'                 "       "     DEMO VALUES                  
TDRRTDTL EQU   X'F0'                 "       "     DAY/TIME LIST                
         EJECT                                                                  
* DSECT TO COVER PREVIOUS VALUES OUTPUTTED TO STEREO                            
*                                                                               
SPVOVALD DSECT                                                                  
SPOVSTA  DS    CL9                 WABCT/PHI--CALL LETTERS(/SPILL MKT)          
SPOVBOOK DS    CL17                "NOV/95(B)  WEEK 1"                          
SPOVDYQH DS    XL(L'BINKDAY+L'BINSQH+L'BINEQH)                                  
SPOVWKS  DS    XL(L'TDRWKS)                                                     
SPOVPROG DS    CL(L'BINPROG)                                                    
SPVOVALQ EQU   *-SPVOVALD                                                       
         DS    0CL(L'STPRVOVL-SPVOVALQ+1)                                       
                                                                                
* DSECT TO COVER LIST OF DIFFERENT DAY/TIME COMBOS                              
*                                                                               
DTNTRYD  DSECT                                                                  
DTNTHINP DS    XL1                 NTH DAY/TIME INPUT TO STEREO                 
DTKDAY   DS    XL(L'BINKDAY)                                                    
DTSQH    DS    XL(L'BINSQH)                                                     
DTEQH    DS    XL(L'BINEQH)                                                     
DTNTRYQ  EQU   *-DTNTRYD                                                        
                                                                                
                                                                                
* DSECT to cover list of start/end QHs for rotational analysis                  
*                                                                               
ROQHTABD DSECT                                                                  
ROQHRTYP DS    XL1                 ROTATION TYPE                                
         DS    XL1                 (SPARE)                                      
ROQHSQH  DS    S                   BASE FOR ROTATION'S START QH                 
ROQHSQHA DS    Y                   ADJUSTMENT FROM START QH BASE                
ROQHEQH  DS    S                   BASE FOR ROTATION'S START QH                 
ROQHEQHA DS    Y                   ADJUSTMENT FROM START QH BASE                
ROQHTABX EQU   *                                                                
ROQHTABQ EQU   ROQHTABX-ROQHTABD                                                
                                                                                
         DS    0XL((ROQHTABQ-L'ROTQHTAB)+1)                                     
         DS    0XL((L'ROTQHTAB-ROQHTABQ)+1)                                     
         EJECT                                                                  
* DSECT TO DOWNLOAD DATA TABLES                                                 
*                                                                               
DLDTABD  DSECT                                                                  
DLDTLEN  DS    XL1                 L(ENTRY)                                     
DLDTFMS  DS    0CL(L'DBFIL+L'DBSRC+L'DBMED)                                     
DLDTFIL  DS     CL(L'DBFIL)         FILE                                        
DLDTSRC  DS     CL(L'DBSRC)         SOURCE                                      
DLDTMED  DS     CL(L'DBMED)         MEDIA                                       
DLDTRTYP DS    XL(L'TDRRCTYP)      TSAR DEMO RECORD TYPE                        
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
*============================= REP DSECTS ============================*         
                                                                                
*------------------------------ REGENREP -----------------------------*         
         PRINT OFF                                                              
       ++INCLUDE REGENREPA                                                      
         PRINT ON                                                               
                                                                                
                                                                                
*----------------------------- RERMPPROF -----------------------------*         
         PRINT OFF                                                              
       ++INCLUDE RERMPPROF                                                      
         PRINT ON                                                               
***********************************************************************         
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         EJECT                                                                  
APSAV2D  DSECT                                                                  
AS2EMPC  DS    XL(L'FALEMPC)       REMEMBER ELEMENT MAP CODE                    
AS2DMPC  DS    XL(L'FALDMPC)       REMEMBER DATA    MAP CODE                    
AS2STMOD DS    XL(L'STMODE)        REMEMBER STEREO MODE                         
AS2DEMON DS    XL1                    "     NTH DEMO FOR ADDDATA CALL           
AS2THSST DS    XL(L'THISSTA)          "     THIS STATION                        
AS2THSBK DS    XL(L'THISBK)           "     THIS BOOK                           
AS2THSDY DS    XL(L'THISDAY)          "     THIS DAY                            
AS2RSMF  DS    XL1                 RESUME FLAG                                  
AS2RFFLK EQU    X80                 RESUME FOR FALINK BREAK                     
AS2RFAPP EQU    X40                 RESUME FOR APPLIC BREAK                     
APSAV2Q  EQU   *-APSAV2D                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'161DEDEM0C   10/14/20'                                      
         END                                                                    
