*          DATA SET ACPRF00    AT LEVEL 014 AS OF 10/01/11                      
*PHASE T63000A                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE PRORATA                                                                
T63000   TITLE 'ACPRF00 - PRESTO FALINK INTERFACE'                              
T63000   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,T63000,R9,RR=R2,CLEAR=YES                            
         USING WORKD,RC                                                         
         SPACE 1                                                                
***********************************************************************         
*                                                                     *         
* DATE    LEV WHO  DESCRIPTION                                        *         
* ------- --- ---- -------------------------------------------------- *         
* 10MAR04 06  SPRI TEMP CODE TO FORCE DUMP WHEN CMV BUG OCCURS        *         
* 29SEP04 07  SPRI CALL ACPRF06 FOR DDLINK TO PROCESS JOB UPLD SCRIPT *         
*                                                                     *         
***********************************************************************         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* INITIALIZATION CODE                                                 *         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
         ST    R2,BASERELO                                                      
         ST    RD,BASERD                                                        
         ST    RB,BASERB                                                        
         ST    R9,BASER9                                                        
         ST    R1,ASYSPARM                                                      
         MVC   ATIOB,0(R1)                                                      
         MVC   ATWA,4(R1)                                                       
         MVC   ASYSFACS,8(R1)                                                   
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
*                                                                               
         LR    RE,RC                                                            
         AHI   RE,IOAREA1-WORKD                                                 
         ST    RE,AIO                                                           
         ST    RE,AIO1                                                          
         AHI   RE,LENIO                                                         
         ST    RE,AIO2                                                          
         AHI   RE,LENIO                                                         
         ST    RE,AIO3                                                          
         AHI   RE,LENIO                                                         
         ST    RE,AIO4                                                          
*                                                                               
         L     RE,=V(TWABLD)                                                    
         A     RE,BASERELO                                                      
         ST    RE,VTWABLD                                                       
         L     RE,=V(PRORATA)                                                   
         A     RE,BASERELO                                                      
         ST    RE,VPRORATA                                                      
*                                                                               
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VDICTATE,CDICTATE                                                
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VCASHVAL,CCASHVAL                                                
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VADDAY,CADDAY                                                    
         MVC   VPERVERT,CPERVERT                                                
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VPERVAL,CPERVAL                                                  
         MVC   VGLOBBER,CGLOBBER                                                
         MVC   VGETFACT,CGETFACT                                                
         MVC   VSWITCH,CSWITCH                                                  
*&&UK*&& MVC   VTOBACCO,CTOBACCO                                                
*                                                                               
         L     RE,=V(TWABLD)                                                    
         A     RE,BASERELO                                                      
         ST    RE,VTWABLD                                                       
         DROP  R1                                                               
         SPACE 1                                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SET UP ADDRESSES FOR CORE-RESIDENT PHASES                                     
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
         L     R2,=A(PHASES)       R2=A(PHASE LIST)                             
         A     R2,BASERELO                                                      
         LA    R3,APHASES          R3=A(ADDRESS LIST)                           
         LA    R4,PHASESN          R4=MAX NUMBER OF PHASES (CORERES)            
         SR    R0,R0                                                            
         ICM   R0,14,=X'D9000A'                                                 
         LA    R1,DMCB                                                          
         L     RF,VCALLOV                                                       
*                                                                               
INIT02   ICM   R0,1,0(R2)          ANY ENTRY HERE?                              
         BZ    INIT04              NONE, SKIP TO THE NEXT ENTRY                 
*                                                                               
         GOTO1 (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
*                                                                               
INIT04   LA    R2,1(R2)            BUMP TO THE NEXT ENTRY                       
         LA    R3,4(R3)                                                         
         BCT   R4,INIT02                                                        
*                                                                               
         LR    RE,RB                                                            
         AHI   RE,VCOMMON-T63000                                                
         LA    R0,VCOMBASN                                                      
         SR    RF,RF                                                            
         LA    R1,READ                                                          
INIT10   DS    0H                                                               
         ST    RE,0(R1)                                                         
         STC   RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,INIT10                                                        
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         MVC   DATADISP,=Y(ACCRFST-ACCRECD)                                     
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(1,TODAYP)   GET TODAY'S DATE                 
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SET CUL AND OTHER VALUES FROM UTL AND COMPANY RECORD                          
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         L     R1,ASYSPARM         GET COMPANY FROM SYSPARMS                    
         MVC   CUL(1),0(R1)                                                     
*                                                                               
         DATE  DUB,DATE=NO         GET COUNTRY                                  
         MVC   CTRY,DUB+6                                                       
*                                                                               
         LA    R4,KEY              READ COMPANY RECORD                          
         USING CPYRECD,R4                                                       
         MVC   CPYKEY,BLANKS                                                    
         MVC   CPYKCPY,CUL                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'ACCKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,CPYELQ       GET COMPANY ELEMENT                          
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CPYELD,R6                                                        
*                                                                               
         MVC   CUL+1(2),CPYPROD    PRODUCTION UNIT/LEDGER                       
*                                                                               
         MVC   COMPSTA1,CPYSTAT1   COMPANY STATUS BYTE 1                        
         MVC   COMPSTA2,CPYSTAT2                                                
         MVC   COMPSTA3,CPYSTAT3                                                
         MVC   COMPSTA4,CPYSTAT4                                                
         CLI   CPYLN,CPYSTAT5-CPYELD   CHECK LENGTH OF ELEMENT                  
         BL    VSETCX                                                           
         MVC   COMPSTA5,CPYSTAT5                                                
         MVC   COMPSTA6,CPYSTAT6                                                
*                                                                               
         CLI   CPYLN,CPYLN3Q       CHECK LENGTH OF ELEMENT                      
         BL    VSETCX                                                           
         MVC   COMPSTA7,CPYSTAT7                                                
         MVC   COMPSTA8,CPYSTAT8                                                
         MVC   COMPCUR,CPYCURR                                                  
*&&UK*&& MVC   COMPCURS,CPYCURRS                                                
*                                                                               
         MVI   CMPDEPLN,2          SET DEFAULT DEPARTMENT LENGTH                
         CLI   CPYDEPTL,0                                                       
         BE    *+10                                                             
         MVC   CMPDEPLN,CPYDEPTL   SET ACTUAL DEPARTMENT LENGTH                 
*                                                                               
         MVI   CMPOFFLN,1          SET DEFAULT OFFICE CODE LENGTH               
         TM    CPYSTAT4,CPYSOFF2                                                
         BZ    *+8                                                              
         MVI   CMPOFFLN,2          NEW OFFICES                                  
         DROP  R6                                                               
*                                                                               
VSETCX   DS    0H                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* READ LEDGER HEIRARCHY SETTING VALUES IN WORK AREA                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   KEY(42),BLANKS                                                   
         MVC   KEY(3),CUL                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(42),KEYSAVE                                                  
         BE    *+14                                                             
         MVC   ERROR,=Y(QINVLDGR)  NO PRODUCTION LEDGER                         
         B     VMSG                                                             
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,SECCHECK         (SECURITY CHECK)                             
*                                                                               
         MVI   ELCODE,ACLTELQ      LEDGER ELEMENT                               
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACLEDGD,R6                                                       
         MVC   LEDGTOFF,ACLTOFF                                                 
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,ACHRELQ      GET HEIRARCHY ELEMENT                        
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACHEIRD,R6                                                       
*                                                                               
         MVC   LLEVA,ACHRLEVA       AND SET LENGTHS                             
         MVC   LLEVAB,ACHRLEVB                                                  
         MVC   LLEVABC,ACHRLEVC                                                 
         MVC   LLEVABCD,ACHRLEVD                                                
         ZIC   R1,LCLIPRO                                                       
         ZIC   R0,LCLI                                                          
         SR    R1,R0                                                            
         STC   R1,LPRO                                                          
         IC    R1,LCLIJOB                                                       
         IC    R0,LCLIPRO                                                       
         SR    R1,R0                                                            
         STC   R1,LJOB                                                          
         IC    R1,LCLIJOB                                                       
         IC    R0,LCLI                                                          
         SR    R1,R0                                                            
         STC   R1,LPROJOB                                                       
         EJECT                                                                  
INIT20   DS    0H                                                               
*                                                                               
*&&US                                                                           
         LA    R1,OFFBLK                                                        
         USING OFFALD,R1                                                        
         MVC   OFFACOMF,ACOMFACS                                                
         MVC   OFFAALPH,TWAAGY                                                  
         MVC   OFFACPY,CUL                                                      
         MVC   OFFACST1,COMPSTA1                                                
         MVC   OFFACST2,COMPSTA2                                                
         MVC   OFFACST3,COMPSTA3                                                
         MVC   OFFACST4,COMPSTA4                                                
         MVC   OFFACST5,COMPSTA5                                                
         MVC   OFFACST6,COMPSTA6                                                
         MVC   OFFACST7,COMPSTA7                                                
         MVC   OFFACST8,COMPSTA8                                                
*                                                                               
         MVC   OFFALIMA,TWAACCS                                                 
         MVC   OFFAAUTH,TWAAUTH                                                 
         MVI   OFFAINDS,OFFAIOFF                                                
         MVI   OFFAACT,OFFAINI                                                  
         GOTO1 VOFFAL                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*&&                                                                             
*                                                                               
         XC    SVRCVEL,SVRCVEL           CLEAR LAST RECEIVE ELEMENT             
         XC    CMTCOUNT,CMTCOUNT         CLEAR COMMENT COUNTER                  
*                                                                               
         BAS   RE,INIFALNK               INITIALIZE FALINK BLOCK                
         BNE   INIT60                    DDLINK SCRIPT INSTEAD                  
         GOTO1 VFALINK,DMCB,FABLK        GIVE FALINK CONTROL                    
* *****************************************************************             
* TEMPORARY BLOCK OF CODE - FORCES DUMP WHEN CMV OVERWRITES SESSION             
* BECAUSE VERSIONS OF CMV < 4.0 HAVE A BUG CAUSING INFINITE LOOPS               
         CLC   =X'000B',FAMSGBLK         DUMP IF INVALID ACTION ERROR           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =X'00D4',FAMSGBLK         DUMP IF INVALID ACTION SEQ             
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
* END OF TEMPORARY BLOCK OF CODE                                                
* *****************************************************************             
         B     EXIT                                                             
INIT60   XC    DMCB(4),DMCB                                                     
         MVC   DMCB(1),SVOLAY      MOVE OVERLAY NUMBER                          
         GOTO1 VCALLOV,DMCB,,ATWA                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         LR    R1,RC                                                            
         BASR  RE,RF                                                            
         L     R1,FABLK+(FALABLD-FALINKD)                                       
         CLI   FALCONC-FALSCRD(R1),FCZERO                                       
         BE    *+12                                                             
         CLI   FALCONC-FALSCRD(R1),FCZERO                                       
         BE    *+12                                                             
         CLI   FALCONC-FALSCRD(R1),FCDONE                                       
         BNE   EXIT                                                             
         MVI   SVOLAY,0            RESET OVERLAY IF ALL DONE                    
EXIT     XIT1                                                                   
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* INITIALIZE FALINK                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
INIFALNK NTR1                                                                   
         CLI   SVXFROV,0           TEST RETURN FROM GLOBBER                     
         BNE   INI2                                                             
         LA    R0,FABLK                                                         
         LA    R1,FALINKDL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    SVOLDRCV,SVOLDRCV                                                
         XC    SVREASON,SVREASON                                                
*                                                                               
INI2     MVC   SVRESUME,SVXFROV    SAVE THE GLOBBING OVERLAY NUMBER             
         MVI   SVXFROV,0           AND CLEAR THIS FLAG NOW !                    
*                                                                               
         OI    PRFSERVH+1,X'01'    SERVICE REQUEST ALWAYS MODIFIED              
         OI    PRFSERVH+6,X'80'                                                 
*                                                                               
         XC    FAMSGBLK,FAMSGBLK                                                
         LA    R2,FABLK                                                         
         ST    R2,AFABLK           FOR OTHER OVERLAYS                           
         USING FALINKD,R2                                                       
         LA    R1,FAMSGBLK             A(MESSAGE BLOCK)                         
         ST    R1,FALAMSG                                                       
*                                                                               
         LA    R1,FACON                A(CONTROL FIELD BUFFER)                  
         ST    R1,FALACON                                                       
         L     R1,ATWA                                                          
         AH    R1,=Y(SVFALINK-TWAD) A(FALINK SAVED STORAGE)                     
         ST    R1,FALASVE                                                       
*                                                                               
         CLI   SVRESUME,6          6 = JOB LAUNCH                               
         BE    INI6                                                             
*                                                                               
         LA    R1,PRFINPH          SET A(FIRST SCREEN POSITION)                 
         ST    R1,FALABLD                                                       
         MVC   FALTBLD,VTWABLD     A(TWABLD)                                    
*                                                                               
         L     R1,ACOMFACS             A(SWITCH)                                
         L     R1,CSWITCH-COMFACSD(R1)                                          
         ST    R1,FALASWCH                                                      
*                                                                               
         L     R0,=A(RECEIVE)                                                   
         A     R0,BASERELO                                                      
         ST    R0,FALARCV                                                       
*                                                                               
         L     R0,=A(SEND)                                                      
         A     R0,BASERELO                                                      
         ST    R0,FALASND                                                       
*                                                                               
         L     R0,=A(BREAK)                                                     
         A     R0,BASERELO                                                      
         ST    R0,FALASTP                                                       
*                                                                               
         L     R0,=A(RESUME)                                                    
         A     R0,BASERELO                                                      
         ST    R0,FALARSM                                                       
*                                                                               
         L     R0,=A(FAMAP)            A(MAP TABLE)                             
         A     R0,BASERELO                                                      
         ST    R0,FALAMAP                                                       
         ST    R0,AMAPTAB          FOR OTHER OVERLAYS                           
*                                                                               
         MVC   FALAPGS,TWAPGS                                                   
*                                                                               
         L     R1,FALABLD          POINT TO CONTROL FIELD HEADER                
         USING FALSCRD,R1                                                       
         CLI   FALCONA,C' '                                                     
         BNH   INIX                                                             
         CLI   FALCONA,FADOWN      TEST FOR FALINK ACTIONS                      
         BE    *+8                                                              
         CLI   FALCONA,FAUPLD                                                   
         BE    *+8                                                              
         CLI   FALCONA,FAVER       TEST FOR VERSION(S)                          
         BE    *+8                                                              
         CLI   FALCONA,FAVERE                                                   
         BE    *+8                                                              
         CLI   FALCONA,FANVER                                                   
         BE    *+8                                                              
         CLI   FALCONA,FANVERE                                                  
         BNE   INI6                NOT FALINK CALL ?? OVERLAY                   
         CLI   FALCONM,C'Y'        TEST MORE TO COME                            
         BE    INIX                                                             
         CLI   FALCONM,C'C'        TEST MORE PROCESSING TO DO                   
         BE    INIX                                                             
         MVI   SVOLAY,0                                                         
         LA    RE,FALCONC          LOOK FOR DDLINK HANDLED MAPS                 
*                                                                               
         GOTO1 VGETFACT,DMCB,(X'80',0),F#FAL1ST                                 
         L     RE,0(,R1)           GET RETURN BLOCK                             
INI4     CLC   =C'D=FE',0(RE)                                                   
         BNE   INIX                                                             
                                                                                
INI6     MVI   SVOLAY,6            SET TO CALL 06 OVERLAY                       
*                                                                               
INIX     CLI   SVOLAY,0            SET CC=NEQ FOR GO TO OVERLAY                 
         B     EXIT                                                             
         DROP  R1,R2                                                            
*                                                                               
TWAPGS   DC    AL4(FALATMS)                                                     
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* VALIDATE CLIENT SETTING VALUES IN WORK AREA                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
VALCLI   NTR1                                                                   
         L     RB,BASERB           RESTORE BASE REGISTERS                       
         L     R9,BASER9                                                        
*                                                                               
         MVC   ERROR,=Y(QINVCLNT)  ERROR IF INPUT TOO LONG                      
         ZIC   R1,LCLI                                                          
         CR    R5,R1                                                            
         BH    VMSG                                                             
*                                                                               
         MVC   CPJ,BLANKS          BUILD CPJ WITH CLIENT ONLY                   
         MVC   CPJ(6),QCLNT                                                     
*                                                                               
         MVC   KEY(42),BLANKS      BUILD KEY AND READ CLIENT RECORD             
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(6),QCLNT                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'ACCKEY),KEYSAVE                                            
         BNE   VMSG                INVALID CLIENT                               
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,SECCHECK         (SECURITY CHECK)                             
         GOTO1 SETCLI                                                           
*                                                                               
         CLI   TWAACCS,C'*'        SKIP ADDITIONAL SECURITY IF                  
         BE    VVALCLIX                DDS TERMINAL                             
         CLI   TWAACCS,C'$'                                                     
         BE    VVALCLIX                                                         
*                                                                               
         BAS   RE,ACCCHECK         CHECK ADDITIONAL SECURITY                    
*                                                                               
VVALCLIX GOTO1 SETOG               GET OFFICE GROUP IF AVAILABLE                
         B     XIT                                                              
*                                                                               
SETCLI   NTR1                                                                   
         L     R1,AIO                                                           
         MVI   ELCODE,RSTELQ       SET DATA FROM STATUS ELEMENT                 
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         USING RSTELD,R6                                                        
         MVC   CLIF1,RSTFILT1                                                   
         MVC   CLIF2,RSTFILT2                                                   
         MVC   CLIF4,RSTFILT4                                                   
*                                                                               
         GOTO1 GETFILTS,CLFLTS     LOAD CLFLTS FROM STATUS ELEMENT              
         BAS   RE,CLREFFS          CLEAR EFFECTIVE FILTER VALUES                
         GOTO1 SETEFFS,CLFLTS      SET EFLTS FROM CLIENT FILTERS                
*                                                                               
         MVI   CLIOFF,0                                                         
         XC    CLIOFFC,CLIOFFC                                                  
         MVI   ELCODE,ACPRELQ      SET OFFICE FROM PROFILE ELEMENT              
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   VSETCLI2            (IF AROUND)                                  
         USING ACPROFD,R6                                                       
         MVC   CLIOFF,ACPROFFC                                                  
         MVC   CLIOFFC,ACPROFFC                                                 
         MVC   CLICOST,ACPRCOST                                                 
*                                                                               
VSETCLI2 MVC   EFFOFF,CLIOFF                                                    
         MVC   EFFOFFC,CLIOFFC                                                  
         MVC   EFFF1,CLIF1                                                      
         MVC   EFFF2,CLIF2                                                      
         MVC   EFFF4,CLIF4                                                      
         B     XIT                                                              
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* VALIDATE PRODUCT SETTING VALUES IN WORK AREA                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
VALPROD  NTR1                                                                   
         L     RB,BASERB           RESTORE BASE REGISTERS                       
         L     R9,BASER9                                                        
*                                                                               
         MVC   ERROR,=Y(QINVPROD)  ERROR IF INPUT TOO LONG                      
         ZIC   R1,LPRO                                                          
         CR    R5,R1                                                            
         BH    VMSG                                                             
*                                                                               
         ZIC   R1,LCLI             ADD PROD CODE TO CPJ                         
         LA    R1,CPJ(R1)                                                       
         MVC   0(6,R1),QPROD                                                    
*                                                                               
         MVC   KEY(42),BLANKS      READ PRODUCT RECORD                          
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(12),CPJ                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'ACCKEY),KEYSAVE                                            
         BNE   VMSG                INVALID PRODUCT                              
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,SECCHECK         (SECURITY CHECK)                             
         GOTO1 SETPROD                                                          
         BAS   RE,ACCCHECK         (ACCESS CHECK)                               
         GOTO1 SETOG               GET OFFICE GROUP IF AVAILABLE                
         B     XIT                                                              
*                                                                               
SETPROD  NTR1                                                                   
         L     R1,AIO                                                           
         MVI   ELCODE,RSTELQ       SET DATA FROM STATUS ELEMENT                 
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
*                                                                               
         USING RSTELD,R6                                                        
         MVC   PRODF1,RSTFILT1                                                  
         MVC   PRODF2,RSTFILT2                                                  
         MVC   PRODF4,RSTFILT4                                                  
*                                                                               
         GOTO1 GETFILTS,PRFLTS                                                  
         BAS   RE,CLREFFS          CLEAR EFFECTIVE FILTER VALUES                
         GOTO1 SETEFFS,CLFLTS      SET EFLTS FROM CLIENT FILTERS                
         GOTO1 SETEFFS,PRFLTS      SET EFLTS FROM PRODUCT FILTERS               
*                                                                               
         MVI   PRODOFF,0                                                        
         XC    PRODOFFC,PRODOFFC                                                
         MVI   ELCODE,ACPRELQ      SET OFFICE FROM PROFILE ELEMENT              
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   VSETPRO2            (IF AROUND)                                  
         USING ACPROFD,R6                                                       
         MVC   PRODOFF,ACPROFFC                                                 
         MVC   PRODOFFC,ACPROFFC                                                
         MVC   PRDCOST,ACPRCOST                                                 
*                                                                               
VSETPRO2 MVC   EFFOFF,CLIOFF       RE-ESTABLISH EFFECTIVE FROM CLI              
         MVC   EFFOFFC,CLIOFFC                                                  
         MVC   EFFF1,CLIF1                                                      
         MVC   EFFF2,CLIF2                                                      
         MVC   EFFF4,CLIF4                                                      
         CLI   PRODOFF,X'41'       AND PRODUCT STATUS                           
         BL    *+10                                                             
         MVC   EFFOFF,PRODOFF                                                   
         CLI   PRODOFFC,X'41'                                                   
         BL    *+10                                                             
         MVC   EFFOFFC,PRODOFFC                                                 
         CLI   PRODF1,X'41'                                                     
         BL    *+10                                                             
         MVC   EFFF1,PRODF1                                                     
         CLI   PRODF2,X'41'                                                     
         BL    *+10                                                             
         MVC   EFFF2,PRODF2                                                     
         CLI   PRODF4,X'41'                                                     
         BL    *+10                                                             
         MVC   EFFF4,PRODF4                                                     
         B     XIT                                                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* VALIDATE PRODUCT SETTING VALUES IN WORK AREA                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
VALJOB   NTR1                                                                   
         L     RB,BASERB           RESTORE BASE REGISTERS                       
         L     R9,BASER9                                                        
*                                                                               
         MVC   ERROR,=Y(QINVJOB)   ERROR IF INPUT TOO LONG                      
         ZIC   R1,LJOB                                                          
         CR    R5,R1                                                            
         BH    VMSG                                                             
*                                                                               
         ZIC   R1,LCLIPRO          ADD JOB NUM TO CPJ                           
         LA    R1,CPJ(R1)                                                       
         ZIC   RF,LJOB                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),QJOB                                                     
*                                                                               
         MVC   KEY(42),BLANKS      READ JOB RECORD                              
         MVC   KEY(3),CUL                                                       
         MVC   KEY+3(12),CPJ                                                    
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         CLI   SVRCVEL+1,X'06'     READ FOR DELETED JOBS WHEN CHECKING          
         BNE   VJOB10                JOB STATUS                                 
         OI    DMINBTS,X'08'       READ FOR DELETES                             
VJOB10   GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08' TURN OFF READ FOR DELETES                    
*                                                                               
         CLC   KEY(L'ACCKEY),KEYSAVE                                            
         BNE   VMSG                INVALID JOB                                  
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,SECCHECK         (SECURITY CHECK)                             
         GOTO1 SETJOB                                                           
         B     XIT                                                              
*                                                                               
SETJOB   NTR1                                                                   
         L     R1,AIO                                                           
         MVI   ELCODE,RSTELQ       SET DATA FROM STATUS ELEMENT                 
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
*                                                                               
         USING RSTELD,R6                                                        
         MVC   JOBF1,RSTFILT1                                                   
         MVC   JOBF2,RSTFILT2                                                   
         MVC   JOBF4,RSTFILT4                                                   
         MVC   JOBSTAT,RSTSTAT     EXTRACT STATUS BYTE                          
*                                                                               
         GOTO1 GETFILTS,JBFLTS                                                  
         BAS   RE,CLREFFS          CLEAR EFFECTIVE FILTER VALUES                
         GOTO1 SETEFFS,CLFLTS      SET EFLTS FROM CLIENT FILTERS                
         GOTO1 SETEFFS,PRFLTS      SET EFLTS FROM PRODUCT FILTERS               
         GOTO1 SETEFFS,JBFLTS      SET EFLTS FROM JOB FILTERS                   
*                                  RE-ESTABLISH EFFECTIVE FROM CLI              
         MVC   EFFF1,CLIF1                                                      
         MVC   EFFF2,CLIF2                                                      
         MVC   EFFF4,CLIF4                                                      
         CLI   PRODF1,X'41'        AND FROM PRODUCT                             
         BL    *+10                                                             
         MVC   EFFF1,PRODF1                                                     
         CLI   PRODF2,X'41'                                                     
         BL    *+10                                                             
         MVC   EFFF2,PRODF2                                                     
         CLI   PRODF4,X'41'                                                     
         BL    *+10                                                             
         MVC   EFFF4,PRODF4                                                     
         CLI   JOBF1,X'41'         AND FROM JOB                                 
         BL    *+10                                                             
         MVC   EFFF1,JOBF1                                                      
         CLI   JOBF2,X'41'                                                      
         BL    *+10                                                             
         MVC   EFFF2,JOBF2                                                      
         CLI   JOBF4,X'41'                                                      
         BL    *+10                                                             
         MVC   EFFF4,JOBF4                                                      
*                                                                               
         MVI   ELCODE,ACJBELQ                                                   
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         USING ACJOBD,R6                                                        
         MVC   JOBCLOSE,ACJBCLOS                                                
         MVC   JOBJSTAT,ACJBSTAT   EXTRACT JOB ELEMENT STATUS BYTE              
         B     XIT                                                              
         EJECT                                                                  
SECCHECK NTR1                      CHECK SECURITY LEVEL AGAINST TWA             
         CLI   TWAAUTH+1,0                                                      
         BE    XIT                                                              
         MVI   ELCODE,RSTELQ                                                    
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSTELD,R6                                                        
*                                                                               
         CLC   RSTSECY+1(1),TWAAUTH+1                                           
         BNH   XIT                                                              
         MVC   ERROR,=Y(QSECLKT)   RECORD HAS HIGHER SECURITY THAN TWA          
         B     VMSG                                                             
*                                                                               
ACCCHECK NTR1                      CHECK FOR LIMIT ACCESS                       
         CLC   TWAACCS,BLANKS      TEST FOR ANY LIMIT ACCESS                    
         BNH   XIT                 NO                                           
*        BAS   RE,CHKSEC                                                        
         BE    XIT                 BYPASS SECURITY CHECKING                     
         CLC   TWAACCS(2),BLANKS   TEST FOR ANY OLD ACCESS                      
         BNH   OFFCHECK                                                         
         CLI   TWAACCS,C'*'        TEST FOR SINGLE OFFICE CONTROL               
         BE    OFFCHECK                                                         
         CLI   TWAACCS,C'$'        TEST FOR LIST CONTROL                        
         BE    OFFCHECK                                                         
         CLC   TWAACCS(2),QCLNT    APPLY CLIENT CODE SECURITY                   
         BE    XIT                                                              
         B     NOACCEX                                                          
*                                                                               
OFFCHECK B     XIT                                                              
*&&DO                                                                           
OFFCHECK L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFAOFFC,EFFOFFC                                                 
         MVC   OFFAOPOS,LEDGTOFF   LEDGER OFFICE POSITION                       
         MVC   OFFAREC,AIO                                                      
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 OFFAL                                                            
         BE    XIT                 SECURITY IS OK                               
         DROP  R1                                                               
*&&                                                                             
*                                                                               
NOACCEX  MVC   ERROR,=Y(QSECLKT)                                                
         B     VMSG                                                             
         EJECT                                                                  
*&&DO                                                                           
***********************************************************                     
* CHKSEC - CHECK IF LIMITED ACCESS/OFFCICE SECURITY       *                     
*          IS REQUIRED                                    *                     
* ON EXIT,  CC=EQ DO NOT TEST, CC=NEQ DO SECURITY TEST    *                     
***********************************************************                     
CHKSEC   L     R4,ARECSEC          R4=A(RECORD/ACTION TABLE)                    
         LA    R3,L'RECSEC         R3=L'RECORD/ACTION TABLE ENTRY               
*                                                                               
CHKSEC2  CLI   0(R4),X'FF'         TEST FOR EOT                                 
         BE    CHKSECN                                                          
         CLC   0(1,R4),RECNUM      FIND CORRECT RECORD/ACTION                   
         BNE   CHKSEC4             NO                                           
         CLC   1(1,R4),ACTNUM      MATCH ON RECORD/ACTION                       
         BE    CHKSECY                                                          
*                                                                               
CHKSEC4  LA    R4,0(R3,R4)         NEXT TABLE ENTRY                             
         B     CHKSEC2                                                          
*                                                                               
CHKSECY  CR    RB,RB               SET CC=EQ FOR 'YES'                          
         BR    RE                                                               
*                                                                               
CHKSECN  LTR   RB,RB               SET CC=NEQ FOR 'NO'                          
         BR    RE                                                               
*&&                                                                             
         EJECT                                                                  
SETOG    NTR1                                                                   
         MVI   EFFOFG,0            SET EFFOFG FROM EFFOFFC                      
         CLI   EFFOFFC,X'41'                                                    
         BL    XIT                                                              
         LA    R4,KEY                                                           
         USING ACOGKEY,R4                                                       
         XC    ACOGKEY,ACOGKEY                                                  
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOFF                                                 
         MVC   ACOGCUL,CUL                                                      
         MVC   ACOGOFC,EFFOFFC                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   ACOGKEY,KEYSAVE     WAS THERE AN OFFICE RECORD?                  
         BNE   XIT                                                              
*                                                                               
         MVI   EFFOFG,0            PRESET OFFICE GROUP TO ZERO                  
         MVI   ELCODE,ACGPELQ      GET GROUP FROM GROUP ELEMENT                 
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING ACGPD,R6                                                         
         MVC   EFFOFG,ACGPCODE                                                  
         DROP  R6                                                               
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
* SET THE STRING OF FIELDS STARING AT 0(R2)) TO THE FILTER VALUES IN            
* THE RSTEL OF THE RECORD IN AIO                                                
*----------------------------------------------------------------------         
SETFLTS  NTR1                                                                   
         MVI   ELCODE,RSTELQ                                                    
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   VSETFX                                                           
         LA    RE,FILTTAB                                                       
*                                                                               
VSETF30  XR    R1,R1                                                            
         ICM   R1,1,0(RE)                                                       
         BZ    VSETFX              END OF TABLE                                 
         LA    R1,0(R6,R1)         ADDRESS FILTER VALUE                         
         MVC   8(1,R2),0(R1)       MOVE FILTER TO SCREEN.                       
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1               R2 = A(NEXT FLDHDR)                          
         LA    RE,1(RE)            NEXT FILTER OFFSET                           
         B     VSETF30                                                          
VSETFX   B     XIT                                                              
*                                                                               
*----------------------------------------------------------------------         
* SET THE STRING OF 1 BYTE FIELDS AT 0(R1)) TO THE FILTER VALUES IN             
* THE RSTEL AT 0(R6),                                                           
* USED BY SETCLI,SETPRO, SETJOB                                                 
*----------------------------------------------------------------------         
*                                                                               
GETFILTS NTR1                                                                   
         LA    RE,FILTTAB                                                       
*                                                                               
GETF30   XR    R2,R2                                                            
         ICM   R2,1,0(RE)                                                       
         BZ    GETFX               END OF TABLE                                 
         LA    R2,0(R2,R6)         ADDRESS FILTER VALUE                         
         MVC   0(1,R1),0(R2)       MOVE FILTER TO OUTPUT AREA                   
         LA    RE,1(RE)            NEXT FILTER OFFSET                           
         LA    R1,1(R1)            NEXT OUTPUT AREA                             
         B     GETF30                                                           
*                                                                               
GETFX    B     XIT                                                              
*                                                                               
CLREFFS  LA    RF,EFLTS            CLEAR EFFECTIVE FILTER VALS                  
         LA    R0,NFLTS                                                         
         XC    0(1,RF),0(RF)                                                    
         LA    RF,1(RF)                                                         
         BCT   R0,*-10                                                          
         BR    RE                                                               
*                                                                               
* SET EFLTS FROM THE FILTER VALUES IN 0(R1)                                     
*                                                                               
SETEFFS  LA    RF,EFLTS                                                         
         LA    R0,NFLTS                                                         
SETEF30  CLI   0(R1),C' '          IS THIS FILTER VALUE DEFINED                 
         BNH   *+10                NO                                           
         MVC   0(1,RF),0(R1)       UPDATE EFFECIVE VALUE                        
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,SETEF30                                                       
         BR    RE                                                               
*                                                                               
FILTTAB  DC    AL1(RSTFILT1-RSTELD)                                             
         DC    AL1(RSTFILT2-RSTELD)                                             
         DC    AL1(RSTFILT3-RSTELD)                                             
         DC    AL1(RSTFILT4-RSTELD)                                             
         DC    AL1(RSTFILT5-RSTELD)                                             
         DC    AL1(0)                                                           
         EJECT                                                                  
VALWORK  NTR1                                                                   
         L     RB,BASERB           RESTORE BASE REGISTERS                       
         L     R9,BASER9                                                        
*                                                                               
         MVC   KEY,BLANKS                                                       
         MVI   KEY,X'0A'           TRY AND READ THE WORK RECORD                 
         MVC   KEY+1(3),CUL                                                     
         MVC   KEY+4(2),QWORK                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVC   ERROR,=Y(QBADWORK)                                               
         CLC   KEY(6),KEYSAVE                                                   
         BNE   VMSG                                                             
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,ACANELQ                                                   
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACANALD,R6                                                       
*        MVC   WORKNAME,ACANDESC                                                
*        MVC   WGROUP,ACANGRUP                                                  
         B     XIT                                                              
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* GETEL AND LTORG                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         GETEL R6,56,ELCODE                                                     
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
BREAK    NTR1  BASE=*,LABEL=*                                                   
         CR    RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* ON RETURN FROM AN OVERLAY THAT WANTS TO EXIT VIA GLOBBER,                     
* WE RETURNED TO FALINK WITH AN FAGLB, FAGLB CALL.                              
* WHEN CALLED PROGRAM RETURNS TO US, THIS EXIT IS CALLED.                       
* ON EXIT FROM HERE, AND SVRESUME IS THE OVERLAY TO BE CALLED.                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
RESUME   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CR    RB,RB               EXIT WITH CC =                               
RSMX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SEND     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   ASETELEM,0(R1)      SAVE FALINK ROUTINE ADDRESSES                
         MVC   AADDDATA,4(R1)                                                   
*                                                                               
         CLI   SVRESUME,0          IF RETURNING FROM GLOBBER THEN               
         BE    SEND10                  DON'T LOOKUP HEADER                      
         LA    R4,SVRESUME-2                                                    
         B     SEND20                                                           
*                                                                               
SEND10   LA    R4,SOVTAB           LOOKUP HEADER IN OVERLAY TABLE               
         LA    R5,(SOVTABX-SOVTAB)/L'SOVTAB                                     
*                                                                               
SEND12   CLC   SVRCVEL,0(R4)       LOOP UNTIL HEADER FOUND IN TABLE             
         BE    SEND20                                                           
         LA    R4,L'SOVTAB(R4)                                                  
         BCT   R5,SEND12                                                        
         DC    H'0'                                                             
*                                                                               
SEND20   CLI   2(R4),X'FF'         TEST NOT TO CALL ANYTHING                    
         BE    SEND40              ALAN SAYS A ZED WILL GO OUT !                
*                                                                               
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB(1),2(R4)       MOVE OVERLAY NUMBER                          
         GOTO1 VCALLOV,DMCB,,ATWA                                               
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         LR    R1,RC                                                            
         BASR  RE,RF                                                            
*                                                                               
         MVC   SVRESUME,SVXFROV    SAVE THIS OVERLAY NUMBER                     
         CLI   SVXFROV,0           TEST OVLY REQUESTED GLOBBER CALL             
         BE    SEND30                                                           
         GOTO1 AADDDATA,DMCB,AFABLK,FALAGLB,FALAGLB                             
         B     SENDX                                                            
*                                                                               
SEND30   CLI   ANYDATA,C'Y'        ANY DATA IN BUFFER?                          
         BZ    SENDX               NO - DON'T CLOSE                             
*                                                                               
SEND40   GOTO1 AADDDATA,DMCB,AFABLK,FALADNE,FALADNE,0                           
*                                                                               
SENDX    CR    RB,RB                                                            
         XIT1                                                                   
         SPACE 1                                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND OVERLAY LOOKUP TABLE                                                     
* ENTRIES ARE                                                                   
*        DS    AL2(FIRST RCV EL CODE)                                           
*        DS    XL1(SEND OVERLAY NUMBER)                                         
*        DS    XL1(SPARE)                                                       
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
SOVTAB   DS    0AL4                                                             
         DC    XL2'01',X'02',X'00' JOB DOWNLOAD                                 
         DC    XL2'02',X'01',X'00' ZOOM                                         
         DC    XL2'03',X'03',X'00' ORDER RESERVATION                            
         DC    XL2'04',X'03',X'00' ORDER DELETE                                 
         DC    XL2'05',X'01',X'00' TEST CONNECTION                              
         DC    XL2'06',X'03',X'00' TEST JOB STATUS FOR ADDING ORDERS            
         DC    XL2'09',X'03',X'00' VALIDATE EXP ACCT FOR EXP ANALYSIS           
         DC    XL2'10',X'03',X'00' VALIDATE EXPENSE ANALYSIS FIELDS             
         DC    XL2'11',X'03',X'00' DELETE ORDER RESERVATION                     
*                                                                               
         DC    XL2'FD',X'FF',X'00' FIELDSN CODES                                
         DC    XL2'FE',X'FF',X'00' VERSION CODES                                
SOVTABX  EQU   *                                                                
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* CONTROL RECEIVED HERE WHEN FALINK HAS RECEIVED DATA                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
RECEIVE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AGETDATA,0(R1)      SAVE FALINK ROUTINE ADDRESS                  
*                                                                               
RCV10    GOTO1 AGETDATA,DMCB,FABLK,FPARMS                                       
         BL    RCVX2               FALINK ERROR                                 
         BH    RCVX                END OF DATA                                  
*                                                                               
         CLI   FPARMS,0            HEADER?                                      
         BNE   RCV20                                                            
         SPACE 1                                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* PRCHDR - PROCESS HEADER ELEMENT                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
         L     R6,FPARMS                                                        
         USING MHELD,R6            R6=A(HEADER ENTRY)                           
*                                                                               
         CLC   MHCODE,=X'00FE'     IGNORE FE/FD ELEMS                           
         BE    RCV10                                                            
         CLC   MHCODE,=X'00FD'                                                  
         BE    RCV10                                                            
         MVC   SVRCVEL,MHCODE      SAVE FIRST RECEIVE ELEMENT                   
*                                                                               
         B     RCV10                                                            
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* PROCESS DATA FIELD                                                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
RCV20    L     R6,FPARMS                                                        
         USING MDELD,R6            R6=A(DATA ENTRY)                             
*                                                                               
         L     R4,FPARMS+4         GET DATA ADDRESS                             
         L     R5,FPARMS+8         GET DATA LENGTH                              
         AHI   R5,-1               SET FOR EX                                   
*                                                                               
         ICM   RF,15,MDUSER        GET PROCESS DATA ROUTINE ADDRESS             
         A     RF,BASERELO                                                      
         BASR  RE,RF               NO RETURN EXPECTED - TRACE USE ONLY          
         DC    H'0'                                                             
*                                                                               
RCVX     CR    RB,RB                                                            
RCVX2    XIT1                                                                   
*                                                                               
RCVERR   DC    H'0'                                                             
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* DATA RECEIVE ROUTINES                                                         
* ON ENTRY: R5 CONTAINS LENGTH OF DATA                                          
*           R4 CONTAINS THE ADDRESS OF THE DATA                                 
* BRANCH TO RCV10 WHEN DONE                                                     
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
INCLNT   MVC   QCLNT,0(R4)               CLIENT                                 
         OC    QCLNT,BLANKS                                                     
         L     RF,=A(VALCLI)             CALL VALCLI                            
         A     RF,BASERELO                                                      
         BASR  RE,RF                                                            
         B     RCV10                                                            
*                                                                               
INPROD   MVC   QPROD,0(R4)               PRODUCT                                
         OC    QPROD,BLANKS                                                     
         L     RF,=A(VALPROD)            CALL VALPROD                           
         A     RF,BASERELO                                                      
         BASR  RE,RF                                                            
         B     RCV10                                                            
*                                                                               
INJOB    MVC   QJOB,0(R4)                JOB                                    
         OC    QJOB,BLANKS                                                      
         L     RF,=A(VALJOB)             CALL VALJOB                            
         A     RF,BASERELO                                                      
         BASR  RE,RF                                                            
         B     RCV10                                                            
*                                                                               
INEXP    XC    SVEXPANL(LENSVEXP),SVEXPANL    CLEAR EXPENSE ANAL FIELDS         
         MVC   SVEXPACC,0(R4)      EXPENSE ACCOUNT                              
         OC    SVEXPACC,BLANKS                                                  
         B     RCV10                                                            
*                                                                               
INWRKCD  MVC   QWORK,0(R4)         W/C (WORKCODE)                               
         CLC   QWORK,=C'99'        DON'T VALIDATE W/C 99 FOR ZOOM               
         BNE   INWCD10                                                          
         CLI   SVRCVEL+1,X'02'                                                  
         BE    RCV10                                                            
INWCD10  L     RF,=A(VALWORK)      CALL VALWORK                                 
         A     RF,BASERELO                                                      
         BASR  RE,RF                                                            
         B     RCV10                                                            
*                                                                               
INORSTR  MVC   SVORSTR,0(R4)       START ORDER NUMBER TO RESERVE                
         B     RCV10                                                            
*                                                                               
INOREND  MVC   SVOREND,0(R4)       END ORDER NUMBER TO RESERVE                  
         B     RCV10                                                            
*                                                                               
INPRSN   EX    R5,EXSVPRSN         PERSON                                       
         OC    SVPRSN,SPACES                                                    
         AHI   R5,1                                                             
         STC   R5,SVPRSNLN         SAVE LENGTH                                  
         B     RCV10                                                            
*                                                                               
INORSRV  MVC   SVORSRV,0(R4)       ORIGINATING SQL SERVER ID                    
         B     RCV10                                                            
*                                                                               
INVERSN  MVC   VERSION,0(R4)       PRESTO VERSION RUNNING                       
         B     RCV10                                                            
*                                                                               
INDOF    EX    R5,EXSVDOF          DEBIT OFFICE                                 
         OC    SVDOF,SPACES                                                     
         AHI   R5,1                                                             
         STC   R5,SVDOFLN          SAVE LENGTH                                  
         B     RCV10                                                            
*                                                                               
INCOF    EX    R5,EXSVCOF          CREDIT OFFICE                                
         OC    SVCOF,SPACES                                                     
         AHI   R5,1                                                             
         STC   R5,SVCOFLN          SAVE LENGTH                                  
         B     RCV10                                                            
*                                                                               
INAOF    EX    R5,EXSVAOF          ANALYSIS OFFICE                              
         OC    SVAOF,SPACES                                                     
         AHI   R5,1                                                             
         STC   R5,SVAOFLN          SAVE LENGTH                                  
         B     RCV10                                                            
*                                                                               
INDEPT   EX    R5,EXSVDEPT         DEPARTMENT                                   
         OC    SVDEPT,SPACES                                                    
         AHI   R5,1                                                             
         STC   R5,SVDEPTLN         SAVE LENGTH                                  
         B     RCV10                                                            
         EJECT                                                                  
EXSVPRSN MVC   SVPRSN(0),0(R4)                                                  
EXSVDOF  MVC   SVDOF(0),0(R4)                                                   
EXSVCOF  MVC   SVCOF(0),0(R4)                                                   
EXSVAOF  MVC   SVAOF(0),0(R4)                                                   
EXSVDEPT MVC   SVDEPT(0),0(R4)                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                                
* ON ENTRY, CALLER MUST HAVE RC = A(WORK)                                       
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         DS    0D                                                               
VCOMMON  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         SRL   RF,24                                                            
         L     RF,VBRANCH(RF)                                                   
         AR    RF,RB                                                            
         BASR  RE,RF               *** NO RETURN EXPECTED HERE ***              
         DC    H'0'                                                             
VCOMMONX SR    RC,RC                                                            
NTEQUAL  LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 1                                                                
VBRANCH  DS    0A                                                               
         DC    A(DMREAD-VCOMMON)                                                
         DC    A(DMSEQ-VCOMMON)                                                 
         DC    A(DMHIGH-VCOMMON)                                                
         DC    A(DMADD-VCOMMON)                                                 
         DC    A(DMWRITE-VCOMMON)                                               
         DC    A(DMGETREC-VCOMMON)                                              
         DC    A(DMPUTREC-VCOMMON)                                              
         DC    A(DMADDREC-VCOMMON)                                              
         DC    A(VGETHDR-VCOMMON)                                               
         DC    A(VGETDATA-VCOMMON)                                              
         DC    A(VMSG-VCOMMON)     ERROR MESSAGES USE THIS                      
         DC    A(VGETELEM-VCOMMON)                                              
         DC    A(VSRCHGET-VCOMMON)                                              
         DC    A(VNEXTEL-VCOMMON)                                               
         DC    A(VDELELEM-VCOMMON)                                              
         DC    A(VSRCHDEL-VCOMMON)                                              
         DC    A(VADDELEM-VCOMMON)                                              
         DC    20A(0)              SPARE                                        
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* DIRECTORY AND FILE ROUTINES                                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
DMREAD   MVC   COMMAND,=C'DMREAD'                                               
         B     DIRCTRY                                                          
*                                                                               
DMSEQ    MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
*                                                                               
DMHIGH   MVC   COMMAND,=C'DMRDHI'                                               
         B     DIRCTRY                                                          
*                                                                               
DMADD    MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
*                                                                               
DMWRITE  MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
*                                                                               
DIRCTRY  CLI   RDUPDATE,C'Y'                                                    
         BNE   *+8                                                              
         OI    DMINBTS,X'80'                                                    
         MVC   KEYSAVE,KEY                                                      
         MVC   DIRECTRY,=C'ACCDIR'                                              
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),DIRECTRY,KEYSAVE,       X        
               KEY,0                                                            
*                                                                               
DMCHECK  DS    0H                                                               
         MVI   DMINBTS,X'00'                                                    
         MVI   RDUPDATE,C'N'                                                    
         MVC   DMBYTE,DMCB+8                                                    
         NC    DMBYTE,DMOUTBTS                                                  
         B     VCOMMONX                                                         
*                                                                               
DMGETREC MVC   COMMAND,=C'GETREC'                                               
         B     DMFILE                                                           
*                                                                               
DMPUTREC MVC   COMMAND,=C'PUTREC'                                               
         B     DMFILE                                                           
*                                                                               
DMADDREC MVC   COMMAND,=C'ADDREC'                                               
         B     DMFILE                                                           
*                                                                               
DMFILE   CLI   RDUPDATE,C'Y'                                                    
         BNE   *+8                                                              
         OI    DMINBTS,X'80'                                                    
         LA    R4,KEY+50                                                        
         MVC   FILE(8),=CL8'ACCMST'                                             
*                                                                               
DMFILEGO GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),FILE,(R4),              X        
               AIO,(0,DMWORK)                                                   
         MVC   DMDSKADD,0(R4)                                                   
         B     VCOMMONX                                                         
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RETURN ADDRESS OF MAP HEADER ELEMENT IN R1                                    
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
VGETHDR  LA    RE,FAMAP                                                         
         USING MHELD,RE                                                         
         SR    RF,RF                                                            
*                                                                               
GHDR2    CLM   R1,3,MHCODE         MATCH EL                                     
         BE    GHDRX                                                            
         ICM   RF,3,MHDISP                                                      
         AR    RE,RF                                                            
         CLI   MHLEN,0                                                          
         BNE   GHDR2                                                            
         DC    H'0'                                                             
*                                                                               
GHDRX    ST    RE,HDRADDR                                                       
         B     VCOMMONX                                                         
         DROP  RE                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RETURN DATA ITEM ADDRESS FOR HEADER  IN HDRADDR                               
* R1 CONTAINS DATA ITEM NUMBER                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
VGETDATA ICM   RE,15,HDRADDR                                                    
         BNZ   *+6                                                              
         DC    H'0'                TAKING NO PRISONERS                          
         USING MDELD,RE                                                         
         SR    RF,RF                                                            
         IC    RF,MHLEN-MHELD(RE)  DSPL TO FIRST DATA ITEM                      
         AR    RE,RF                                                            
*                                                                               
GDAT2    CLM   R1,3,MDCODE         MATCH EL                                     
         BE    GDATX                                                            
         ICM   RF,1,MDLEN                                                       
         AR    RE,RF                                                            
         CLI   MDLEN,0                                                          
         BNE   GDAT2                                                            
         DC    H'0'                                                             
*                                                                               
GDATX    ST    RE,DATADDR                                                       
         B     VCOMMONX                                                         
         DROP  RE                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SENDS MESSAGE TO PC IN THE FORM OF A DIALOG                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
VMSG     LA    R4,FAMSGBLK                                                      
         USING FAMSGD,R4                                                        
         OC    ERROR,ERROR         FAMSGNO CAN BE SET BY FALINK                 
         BZ    *+10                                                             
         MVC   FAMSGNO,ERROR                                                    
         CLI   *,X'FF'             SET CC LOW                                   
         L     RD,BASERD           MONITOR TO ACPRS00                           
         L     RD,8(RD)            ACPRS00 TO FALINK                            
         L     RD,8(RD)            FALINK TO ACPRS00                            
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* THESE TWO ROUTINES POINT R6 TO THE FIRST ELEMENT IN AIO WITH THE              
* ELEMENT CODE SPECIFIED IN PARAMETER ONE.  SRCHGET FURTHER RESTRICTS           
* THE SEARCH TO MATCH THE ELEMENT'S DATA (STARTING AFTER THE ELEMENT            
* LENGTH) WITH THE SEARCH ARGUMENT SPECIFIED IN PARAMETER TWO.  FOR             
* EITHER ROUTINE, IF THE ELEMENT IS FOUND, IT RETURNS 'YES'.                    
* OTHERWISE, IT RETURNS 'NO'.                                                   
*                                                                               
*        PARAMETER 1 - ELEMENT CODE                                             
*        PARAMETER 2 (FOR SRCHGET ONLY) - A(SEARCH ARGUMENT)                    
*                                         BYTE 0 IS L(SEARCH ARGUMENT)          
*                                                                               
* ADDED 6/2/93:                                                                 
*    CODE NOW SAVES FOR SRCHLEN AND SRCHDATA IF CALL IS TO SRCHGET.             
*    NEXTELEM WILL USE THESE VALUE TO FILTER ELEMENTS.                          
***********************************************************************         
VGETELEM DS    0H                                                               
         SR    R3,R3               CLEAR R3 AND R4 TO MAKE HELLO USE NO         
         SR    R4,R4                   SEARCH ARGUMENT                          
         MVI   SRCHLEN,0           CLEAR SRCHLEN SO NEXTELEM DOESN'T            
         B     ALLGET                  FILTER                                   
*                                                                               
VSRCHGET DS    0H                                                               
         L     R3,4(R1)            R3 = A(SEARCH ARGMENT)                       
         ZIC   R4,4(R1)            R4 = LENGTH OF SEARCH ARGUMENT               
*                                                                               
         MVC   SRCHLEN,4(R1)       SAVE SEARCH LENGTH AND DATA SO               
         LR    RE,R3                   NEXTELEM FILTERS                         
         LR    RF,R4                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SRCHDATA(0),0(RE)                                                
*                                                                               
ALLGET   L     R2,0(R1)            R2 = ELEMENT CODE                            
         STC   R2,CMELCODE         SAVE CODE IN CONTROLLER GLOBAL               
*                                                                               
*                                  CALL HELLO TO GET ELEMENT                    
         GOTO1 VHELLO,DMCB,(C'G',=C'ACCMST'),((R2),AIO),((R4),(R3))             
*                                                                               
         L     R6,12(R1)           RETURN A(ELEMENT) IN R6                      
*                                                                               
         CLI   12(R1),0            RETURN 'YES' IF ELEMENT FOUND AND            
         B     XIT6                    'NO' IF NOT FOUND                        
         EJECT                                                                  
***********************************************************************         
* R6 SHOULD POINT TO A VALID RECORD ELEMENT.  UPON RETURN, R6 WILL              
* POINT TO THE NEXT ELEMENT.                                                    
*                                                                               
* THIS ROUTINE MUST BE USED ONLY AFTER FIRST MAKING AN INITIAL CALL TO          
* GETELEM.  THE ELCODE PASSED TO GETELEM, WHICH IS SAVED INTERNALLY, IS         
* USED IN THIS ROUTINE TO DECIDE WHICH ELEMENT IS NEXT.  IF THE ELCODE          
* WAS ZERO, THEN THE VERY NEXT ELEMENT IS RETURNED.  OTHERWISE, IT              
* CONTINUES BUMPING THROUGH THE RECORD UNTIL AN ELEMENT WITH THE SAME           
* CODE IS FOUND.  IF THE END OF THE RECORD IS REACHED BEFORE THE                
* ELEMENT IS FOUND, THE ROUTINE RETURNS 'NO'.  OTHERWISE, IT RETURNS            
* 'YES'.                                                                        
*                                                                               
* ADDED 6/2/93:                                                                 
*    CODE NOW CHECKS FOR SRCHLEN AND SRCHDATA, VALUES SET IF ORIGINAL           
*    CALL WAS SRCHGET.  SRCHGET SETS THESE VALUE FROM THE VALUES                
*    PASSED TO IT.  THIS ROUTINE USES THEM AS A FILTER.                         
***********************************************************************         
VNEXTEL  DS    0H                                                               
         CLI   0(R6),0             IF END OF RECORD THEN RETURN 'NO'            
         BZ    NENO                                                             
*                                                                               
NE10     ZIC   R0,1(R6)            BUMP R6 TO NEXT ELEMENT                      
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0             IF END OF RECORD REACHED THEN RETURN         
         BE    NENO                    'NO'                                     
*                                                                               
         CLI   CMELCODE,0          ELSE IF ELCODE IS ZERO THEN DONE             
         BE    NEYES                                                            
*                                                                               
         CLC   CMELCODE,0(R6)      ELSE IF MATCH NOT FOUND THEN BUMP            
         BNE   NE10                    TO NEXT ELEMENT                          
*                                                                               
         CLI   SRCHLEN,0           IF ORIGINAL SEARCH WAS SRCHGET               
         BE    NEYES                                                            
         ZIC   RF,SRCHLEN          THEN SKIP ELEMENT IF DATA DOES               
         BCTR  RF,0                    NOT MATCH FILTER                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SRCHDATA(0),2(R6)                                                
         BNE   NE10                                                             
*                                                                               
NEYES    B     YES6                ELSE RETURN 'YES' AND R6                     
*                                                                               
NENO     B     NO6                                                              
         EJECT                                                                  
***********************************************************************         
* THESE TWO ROUTINES DELETE THE ELEMENT IN AIO WITH THE ELEMENT CODE            
* SPECIFIED IN PARAMETER ONE.  SRCHDEL DELETES ONLY THE ELEMENT                 
* WHOSE ELEMENT DATA (STARTING AFTER THE ELEMENT LENGTH) MATCHES THE            
* SEARCH ARGUMENT SPECIFIED IN PARAMETER TWO.  DELELEM DELETES ALL              
* ELEMENTS WITH THE SPECIFIED ELEMENT CODE.                                     
*                                                                               
*        PARAMETER 1 - ELEMENT CODE                                             
*        PARAMETER 2 (FOR SRCHDEL ONLY) - A(SEARCH ARGUMENT)                    
***********************************************************************         
VDELELEM DS    0H                                                               
         SR    R3,R3               CLEAR R3 AND R4 TO MAKE HELLO USE NO         
         SR    R4,R4                   SEARCH ARGUMENT                          
         B     ALLDEL                                                           
*                                                                               
VSRCHDEL DS    0H                                                               
         L     R3,4(R1)            R3 = A(SEARCH ARGMENT)                       
         ZIC   R4,4(R1)            R4 = LENGTH OF SEARCH ARGUMENT               
*                                                                               
ALLDEL   L     R2,0(R1)            R2 = ELEMENT CODE                            
*                                                                               
*                                  CALL HELLO TO DELETE ELEMENT                 
         GOTO1 VHELLO,DMCB,(C'D',=C'ACCMST'),((R2),AIO),((R4),(R3))             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE ADDS THE ELEMENT FOUND IN PARAMETER ONE TO THE RECORD            
* IN AIO.  IF THE RECORD OVERFLOWS, THE ROUTINE CAUSES A PROGRAM                
* FAILURE TO PREVENT ANY BAD RECORDS FROM ENDING UP ON THE FILE.                
* (IF THE ROUTINE RETURNED A CONDITION CODE, THE PROGRAMMER WOULD               
* PROBABLY IGNORE IT).                                                          
***********************************************************************         
VADDELEM DS    0H                                                               
         L     R2,0(R1)            R2 = A(ELEMENT TO ADD)                       
*                                                                               
*                                  CALL HELLO TO ADD ELEMENT                    
         GOTO1 VHELLO,DMCB,(C'P',=C'ACCMST'),AIO,(R2),0,0                       
*                                                                               
         CLI   12(R1),0            DIE IF RECORD OVERFLOWS TO PREVENT           
         BE    XIT                     ACCIDENTALLY ADDING BAD RECORDS          
         DC    H'0'                    TO THE FILE                              
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* EXITS WITH CONDITION CODES AND SAVING OF R6                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
XIT      XIT1                                                                   
*                                                                               
YES6     SR    RC,RC               RETURN CC EQUAL                              
NO6      LTR   RC,RC               RETURN CC NOT EQUAL                          
XIT6     XIT1  REGS=(R6)                                                        
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* PHASE LIST                                                                    
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
PHASES   DS    0X                  ** LOADED PHASE LIST **                      
         DC    AL1(QFALINK)                                                     
         DC    AL1(QTSAR)                                                       
         DC    AL1(QJOBBER)                                                     
         DC    AL1(QGETOPT)                                                     
         DC    AL1(QOFFAL)                                                      
PHASESN  EQU   *-PHASES                                                         
         LTORG                                                                  
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* FALINK MAP TABLE                                                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         DS    0D                                                               
         DC    C'**FAMAP*'         EYE CATCHER                                  
FAMAP    DS    0D                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 01 - JOB DOWNLOAD                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H01      DC   AL1(H01X-H01)           HEADER LENGTH                             
         DC   XL2'0001'               HEADER CODE                               
         DC   AL2(H01XX-H01)          DISP TO NEXT HEADER                       
H01X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCCLNT),CL5'CLNT ',AL1(MDTCHQ),AL1(0)                
         DC    AL4(INCLNT)                                                      
         DC    AL1(14),AL2(MCPROD),CL5'PROD ',AL1(MDTCHQ),AL1(0)                
         DC    AL4(INPROD)                                                      
         DC    AL1(14),AL2(MCJOB),CL5'JOB  ',AL1(MDTCHQ),AL1(0)                 
         DC    AL4(INJOB)                                                       
*                                                                               
         DC    AL1(10),AL2(MCJOB01),CL5'JOB01',AL1(MDTCHQ),AL1(12)              
         DC    AL1(10),AL2(MCJOB02),CL5'JOB02',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCJOB03),CL5'JOB03',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCJOB04),CL5'JOB04',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCJOB05),CL5'JOB05',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCJOB06),CL5'JOB06',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCJOB07),CL5'JOB07',AL1(MDTCHQ),AL1(0)               
*                                                                               
         DC    X'00'                                                            
H01XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 02 - ZOOM                                                                     
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H02      DC   AL1(H02X-H02)           HEADER LENGTH                             
         DC   XL2'0002'               HEADER CODE                               
         DC   AL2(H02XX-H02)          DISP TO NEXT HEADER                       
H02X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCCLNT),CL5'CLNT ',AL1(MDTCHQ),AL1(0)                
         DC    AL4(INCLNT)                                                      
         DC    AL1(14),AL2(MCPROD),CL5'PROD ',AL1(MDTCHQ),AL1(0)                
         DC    AL4(INPROD)                                                      
         DC    AL1(14),AL2(MCJOB),CL5'JOB  ',AL1(MDTCHQ),AL1(0)                 
         DC    AL4(INJOB)                                                       
         DC    AL1(14),AL2(MCWRKCD),CL5'WRKCD',AL1(MDTCHQ),AL1(2)               
         DC    AL4(INWRKCD)                                                     
*                                                                               
         DC    AL1(10),AL2(MCTRADT),CL5'TRADT',AL1(MDTCHQ),AL1(0)               
         DC    AL1(10),AL2(MCOPNPO),CL5'OPNPO',AL1(MDTCHQ),AL1(0)               
*                                                                               
         DC    X'00'                                                            
H02XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 03 - ORDER RESERVATION                                                        
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H03      DC   AL1(H03X-H03)           HEADER LENGTH                             
         DC   XL2'0003'               HEADER CODE                               
         DC   AL2(H03XX-H03)          DISP TO NEXT HEADER                       
H03X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCORSTR),CL5'ORSTR',AL1(MDTCHQ),AL1(6)               
         DC    AL4(INORSTR)                                                     
         DC    AL1(14),AL2(MCOREND),CL5'OREND',AL1(MDTCHQ),AL1(6)               
         DC    AL4(INOREND)                                                     
         DC    AL1(14),AL2(MCPRSN),CL5'PRSN',AL1(MDTCHQ),AL1(8)                 
         DC    AL4(INPRSN)                                                      
         DC    AL1(14),AL2(MCORSRV),CL5'ORSRV',AL1(MDTCHQ),AL1(16)              
         DC    AL4(INORSRV)                                                     
         DC    AL1(10),AL2(MCRTNCD),CL5'RTNCD',AL1(MDTCHQ),AL1(0)               
*                                                                               
         DC    X'00'                                                            
H03XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 04 - ORDER DELETE                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H04      DC   AL1(H04X-H04)           HEADER LENGTH                             
         DC   XL2'0004'               HEADER CODE                               
         DC   AL2(H04XX-H04)          DISP TO NEXT HEADER                       
H04X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCORSTR),CL5'ORSTR',AL1(MDTCHQ),AL1(6)               
         DC    AL4(INORSTR)                                                     
         DC    AL1(10),AL2(MCRTNCD),CL5'TRNCD',AL1(MDTCHQ),AL1(0)               
*                                                                               
         DC    X'00'                                                            
H04XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 05 - TEST CONNECTION                                                          
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H05      DC   AL1(H05X-H05)           HEADER LENGTH                             
         DC   XL2'0005'               HEADER CODE                               
         DC   AL2(H05XX-H05)          DISP TO NEXT HEADER                       
H05X     EQU  *                                                                 
*                                                                               
         DC    AL1(10),AL2(MCRTNCD),CL5'TRNCD',AL1(MDTCHQ),AL1(0)               
         DC    X'00'                                                            
H05XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 06 - CHECK JOB STATUS FOR NEW ORDER                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H06      DC   AL1(H06X-H06)           HEADER LENGTH                             
         DC   XL2'0006'               HEADER CODE                               
         DC   AL2(H06XX-H06)          DISP TO NEXT HEADER                       
H06X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCCLNT),CL5'CLNT ',AL1(MDTCHQ),AL1(0)                
         DC    AL4(INCLNT)                                                      
         DC    AL1(14),AL2(MCPROD),CL5'PROD ',AL1(MDTCHQ),AL1(0)                
         DC    AL4(INPROD)                                                      
         DC    AL1(14),AL2(MCJOB),CL5'JOB  ',AL1(MDTCHQ),AL1(0)                 
         DC    AL4(INJOB)                                                       
*                                                                               
         DC    AL1(10),AL2(MCRTNCD),CL5'TRNCD',AL1(MDTCHQ),AL1(0)               
*                                                                               
         DC    X'00'                                                            
H06XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
* 07 - FOREIGN CURRENCY LIST - USED IN UK ONLY                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
*&&UK                                                                           
H07      DC   AL1(H07X-H07)           HEADER LENGTH                             
         DC   XL2'0007'               HEADER CODE                               
         DC   AL2(H07XX-H07)          DISP TO NEXT HEADER                       
H07X     EQU  *                                                                 
*                                                                               
         DC    AL1(10),AL2(MCFCURR),CL5'FCURR',AL1(MDTCHQ),AL1(0)               
*                                                                               
         DC    X'00'                                                            
H07XX    EQU   *                                                                
*&&                                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
* 08 - ARTICLE LIST - USED IN UK ONLY                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
*&&UK                                                                           
H08      DC   AL1(H08X-H08)           HEADER LENGTH                             
         DC   XL2'0008'               HEADER CODE                               
         DC   AL2(H08XX-H08)          DISP TO NEXT HEADER                       
H08X     EQU  *                                                                 
*                                                                               
         DC    AL1(10),AL2(MCARTLS),CL5'ARTLS',AL1(MDTCHQ),AL1(0)               
*                                                                               
         DC    X'00'                                                            
H08XX    EQU   *                                                                
*&&                                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
* 09 - VALIDATE EXPENSE ACCOUNT FOR EXPENSE ANALYSIS - US ONLY                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
*&&US                                                                           
H09      DC   AL1(H09X-H09)           HEADER LENGTH                             
         DC   XL2'0009'               HEADER CODE                               
         DC   AL2(H09XX-H09)          DISP TO NEXT HEADER                       
H09X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCEXPAC),CL5'EXPAC',AL1(MDTCHQ),AL1(0)               
         DC    AL4(INEXP)                                                       
         DC    AL1(10),AL2(MCVALFL),CL5'VALFL',AL1(MDTCHQ),AL1(3)               
         DC    AL1(10),AL2(MCRTNCD),CL5'RTNCD',AL1(MDTCHQ),AL1(0)               
*                                                                               
         DC    X'00'                                                            
H09XX    EQU   *                                                                
*&&                                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
* 10 - VALIDATE EXPENSE ANALYSIS FIELDS FOR SAVING ORDER- US ONLY               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
*&&US                                                                           
H10      DC   AL1(H10X-H10)           HEADER LENGTH                             
         DC   XL2'0010'               HEADER CODE                               
         DC   AL2(H10XX-H10)          DISP TO NEXT HEADER                       
H10X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCEXPAC),CL5'EXPAC',AL1(MDTCHQ),AL1(0)               
         DC    AL4(INEXP)                                                       
         DC    AL1(14),AL2(MCCLNT),CL5'CLNT ',AL1(MDTCHQ),AL1(0)                
         DC    AL4(INCLNT)                                                      
         DC    AL1(14),AL2(MCPROD),CL5'PROD ',AL1(MDTCHQ),AL1(0)                
         DC    AL4(INPROD)                                                      
         DC    AL1(14),AL2(MCDOF),CL5'DOF  ',AL1(MDTCHQ),AL1(0)                 
         DC    AL4(INDOF)                                                       
         DC    AL1(14),AL2(MCCOF),CL5'COF  ',AL1(MDTCHQ),AL1(0)                 
         DC    AL4(INCOF)                                                       
         DC    AL1(14),AL2(MCAOF),CL5'AOF  ',AL1(MDTCHQ),AL1(0)                 
         DC    AL4(INAOF)                                                       
         DC    AL1(14),AL2(MCDEPT),CL5'DEPT ',AL1(MDTCHQ),AL1(0)                
         DC    AL4(INDEPT)                                                      
         DC    AL1(14),AL2(MCPRSN),CL5'PRSN ',AL1(MDTCHQ),AL1(0)                
         DC    AL4(INPRSN)                                                      
         DC    AL1(10),AL2(MCRTNCD),CL5'RTNCD',AL1(MDTCHQ),AL1(0)               
*                                                                               
         DC    X'00'                                                            
H10XX    EQU   *                                                                
*&&                                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* 11 - DELETE ORDER RESERVATION                                                 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
H11      DC   AL1(H11X-H11)           HEADER LENGTH                             
         DC   XL2'0011'               HEADER CODE                               
         DC   AL2(H11XX-H11)          DISP TO NEXT HEADER                       
H11X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(MCORSTR),CL5'ORSTR',AL1(MDTCHQ),AL1(6)               
         DC    AL4(INORSTR)                                                     
         DC    AL1(14),AL2(MCOREND),CL5'OREND',AL1(MDTCHQ),AL1(6)               
         DC    AL4(INOREND)                                                     
*                                                                               
         DC    X'00'                                                            
H11XX    EQU   *                                                                
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* FD - VERSION DATA                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
HFD      DC   AL1(HFDX-HFD)           HEADER LENGTH                             
         DC   XL2'00FD'               HEADER CODE                               
         DC   AL2(HFDXX-HFD)          DISP TO NEXT HEADER                       
HFDX     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(01),CL5'VERSN',AL1(MDTHXQ),AL1(4)                    
         DC    AL4(INVERSN)                                                     
         DC    X'00'                                                            
HFDXX    EQU   *                                                                
                                                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* FE - VERSION DATA                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         SPACE 1                                                                
HFE      DC   AL1(HFEX-HFE)              HEADER LENGTH                          
         DC   XL2'00FE'                  HEADER CODE                            
         DC   AL2(HFEXX-HFE)             DISP TO NEXT HEADER                    
HFEX     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(01),CL5'VERSN',AL1(MDTHXQ),AL1(4)                    
         DC    AL4(INVERSN)                                                     
         DC    X'00'                                                            
HFEXX    EQU   *                                                                
*                                                                               
         DC    X'00'                     END OF TABLE                           
         EJECT                                                                  
BLANKS   DC    CL80' '                                                          
*                                                                               
       ++INCLUDE ACPRFWRK                                                       
WORKD    DSECT                                                                  
* OVERLAY WORKING STORAGE                                                       
         ORG   BIGWORK                                                          
SVOLAY   DS    X                   SAVED OVERLAY NUMBER                         
*                                                                               
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FAUTL                                                          
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014ACPRF00   10/01/11'                                      
         END                                                                    
